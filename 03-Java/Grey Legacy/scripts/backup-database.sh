#!/bin/bash
###############################################################################
# backup-database.sh - Grey Legacy Database Backup
#
# Performs PostgreSQL backups with compression, integrity verification,
# remote replication, and retention policy enforcement.
#
# Usage: ./backup-database.sh [--type daily|monthly] [--no-remote]
# Cron:  0 1 * * * /opt/greylegacy/scripts/backup-database.sh --type daily
#        0 2 1 * * /opt/greylegacy/scripts/backup-database.sh --type monthly
#
# Maintainer: Grey Legacy DBA Team
# Last Modified: 2025-10-03
###############################################################################
set -euo pipefail
IFS=$'\n\t'

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
readonly DB_HOST="${GL_DB_HOST:-db-prod.greylegacy.internal}"
readonly DB_PORT="${GL_DB_PORT:-5432}"
readonly DB_NAME="${GL_DB_NAME:-greylegacy}"
readonly DB_USER="${GL_DB_USER:-backup_admin}"
readonly PGPASSFILE="${HOME}/.pgpass"
export PGPASSFILE

readonly BACKUP_BASE="/var/backups/greylegacy/database"
readonly BACKUP_DAILY_DIR="${BACKUP_BASE}/daily"
readonly BACKUP_MONTHLY_DIR="${BACKUP_BASE}/monthly"
readonly LOG_DIR="/var/log/greylegacy"
readonly LOG_FILE="${LOG_DIR}/db-backup-$(date +%Y%m%d).log"
readonly TIMESTAMP="$(date +%Y%m%d_%H%M%S)"

readonly DAILY_RETENTION=30    # keep last 30 daily backups
readonly MONTHLY_RETENTION=12  # keep last 12 monthly backups

readonly REMOTE_HOST="${GL_BACKUP_HOST:-backup.greylegacy.internal}"
readonly REMOTE_USER="backup_svc"
readonly REMOTE_DIR="/data/offsite/greylegacy/database"
readonly REMOTE_KEY="/opt/greylegacy/.ssh/backup_id_rsa"

readonly MAIL_TO="dba-team@greylegacy.com"

BACKUP_TYPE="daily"
NO_REMOTE=false

# ---------------------------------------------------------------------------
# Logging
# ---------------------------------------------------------------------------
mkdir -p "${LOG_DIR}" "${BACKUP_DAILY_DIR}" "${BACKUP_MONTHLY_DIR}"
exec > >(tee -a "${LOG_FILE}") 2>&1

log_info()  { echo "[$(date '+%Y-%m-%d %H:%M:%S')] [INFO]  $*"; }
log_warn()  { echo "[$(date '+%Y-%m-%d %H:%M:%S')] [WARN]  $*"; }
log_error() { echo "[$(date '+%Y-%m-%d %H:%M:%S')] [ERROR] $*"; }

# ---------------------------------------------------------------------------
# Parse arguments
# ---------------------------------------------------------------------------
while [[ $# -gt 0 ]]; do
    case "$1" in
        --type)      BACKUP_TYPE="$2"; shift 2 ;;
        --no-remote) NO_REMOTE=true;   shift ;;
        *) log_error "Unknown argument: $1"; exit 1 ;;
    esac
done

case "${BACKUP_TYPE}" in
    daily)   BACKUP_DIR="${BACKUP_DAILY_DIR}" ;;
    monthly) BACKUP_DIR="${BACKUP_MONTHLY_DIR}" ;;
    *) log_error "Invalid backup type: ${BACKUP_TYPE}"; exit 1 ;;
esac

readonly BACKUP_FILE="${BACKUP_DIR}/${DB_NAME}_${BACKUP_TYPE}_${TIMESTAMP}.sql.gz"

# ---------------------------------------------------------------------------
# Perform backup
# ---------------------------------------------------------------------------
perform_backup() {
    log_info "Starting ${BACKUP_TYPE} backup of ${DB_NAME}@${DB_HOST}..."
    local start_time
    start_time=$(date +%s)

    pg_dump -h "${DB_HOST}" -p "${DB_PORT}" -U "${DB_USER}" -d "${DB_NAME}" \
        --format=plain --no-owner --no-privileges --verbose 2>>"${LOG_FILE}" \
        | gzip -9 > "${BACKUP_FILE}"

    local end_time duration file_size
    end_time=$(date +%s)
    duration=$((end_time - start_time))
    file_size=$(du -h "${BACKUP_FILE}" | cut -f1)

    log_info "Backup completed in ${duration}s (${file_size})"
}

# ---------------------------------------------------------------------------
# Verify backup integrity
# ---------------------------------------------------------------------------
verify_backup() {
    log_info "Verifying backup integrity..."

    # Check file exists and is non-empty
    if [[ ! -s "${BACKUP_FILE}" ]]; then
        log_error "Backup file is empty or missing: ${BACKUP_FILE}"
        return 1
    fi

    # Test gzip integrity
    if ! gzip -t "${BACKUP_FILE}" 2>/dev/null; then
        log_error "Backup file failed gzip integrity check"
        return 1
    fi

    # Verify SQL content by checking for expected markers
    local table_count
    table_count=$(zcat "${BACKUP_FILE}" | grep -c "^CREATE TABLE" || true)
    if [[ "${table_count}" -lt 5 ]]; then
        log_warn "Backup contains only ${table_count} CREATE TABLE statements (expected >=5)"
    fi

    # Generate and store checksum
    sha256sum "${BACKUP_FILE}" > "${BACKUP_FILE}.sha256"

    log_info "Backup verified: ${table_count} tables, checksum stored"
}

# ---------------------------------------------------------------------------
# Copy to remote server
# ---------------------------------------------------------------------------
replicate_remote() {
    if [[ "${NO_REMOTE}" == "true" ]]; then
        log_info "Skipping remote replication (--no-remote)"
        return 0
    fi

    log_info "Replicating to ${REMOTE_HOST}:${REMOTE_DIR}..."

    ssh -i "${REMOTE_KEY}" "${REMOTE_USER}@${REMOTE_HOST}" \
        "mkdir -p ${REMOTE_DIR}/${BACKUP_TYPE}" 2>/dev/null

    rsync -az --progress -e "ssh -i ${REMOTE_KEY}" \
        "${BACKUP_FILE}" "${BACKUP_FILE}.sha256" \
        "${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_DIR}/${BACKUP_TYPE}/"

    if [[ $? -eq 0 ]]; then
        log_info "Remote replication completed"
    else
        log_warn "Remote replication failed (local backup is intact)"
    fi
}

# ---------------------------------------------------------------------------
# Retention cleanup
# ---------------------------------------------------------------------------
cleanup_old_backups() {
    log_info "Enforcing retention policy..."

    local retention dir
    case "${BACKUP_TYPE}" in
        daily)   retention="${DAILY_RETENTION}";   dir="${BACKUP_DAILY_DIR}" ;;
        monthly) retention="${MONTHLY_RETENTION}"; dir="${BACKUP_MONTHLY_DIR}" ;;
    esac

    local count
    count=$(find "${dir}" -name "*.sql.gz" -type f | wc -l)

    if [[ "${count}" -gt "${retention}" ]]; then
        local to_remove=$((count - retention))
        log_info "Removing ${to_remove} old ${BACKUP_TYPE} backup(s) (keeping ${retention})..."
        find "${dir}" -name "*.sql.gz" -type f | sort | head -n "${to_remove}" | while read -r f; do
            rm -f "${f}" "${f}.sha256"
            log_info "  Removed: $(basename "${f}")"
        done
    else
        log_info "Retention OK: ${count}/${retention} ${BACKUP_TYPE} backups"
    fi
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
main() {
    log_info "=========================================="
    log_info "Database Backup - ${BACKUP_TYPE}"
    log_info "=========================================="

    if perform_backup && verify_backup; then
        replicate_remote
        cleanup_old_backups

        log_info "=========================================="
        log_info "Backup completed successfully"
        log_info "  File: ${BACKUP_FILE}"
        log_info "  Size: $(du -h "${BACKUP_FILE}" | cut -f1)"
        log_info "=========================================="
        exit 0
    else
        log_error "Backup FAILED"
        echo "Database backup failed on $(hostname) at $(date). Check ${LOG_FILE}" \
            | mail -s "[Grey Legacy] DB Backup FAILURE" "${MAIL_TO}" 2>/dev/null || true
        exit 1
    fi
}

main "$@"
