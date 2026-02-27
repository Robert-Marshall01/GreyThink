#!/bin/bash
###############################################################################
# etl-claims-export.sh - Claims Data ETL Export
#
# Exports open claims with policy information to CSV for downstream partner
# systems (reinsurance, actuarial, TPA). Transfers via SFTP and sends
# email notification on completion.
#
# Usage: ./etl-claims-export.sh [--dry-run] [--no-transfer]
# Cron:  0 4 * * * /opt/greylegacy/scripts/etl-claims-export.sh
#
# Maintainer: Grey Legacy Data Engineering
# Last Modified: 2025-09-22
###############################################################################
set -euo pipefail
IFS=$'\n\t'

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
readonly SCRIPT_NAME="$(basename "$0")"
readonly LOCK_FILE="/var/run/greylegacy/${SCRIPT_NAME}.lock"
readonly LOG_DIR="/var/log/greylegacy/etl"
readonly LOG_FILE="${LOG_DIR}/claims-export-$(date +%Y%m%d).log"
readonly LOG_MAX_FILES=30

readonly DB_HOST="${GL_DB_HOST:-db-prod.greylegacy.internal}"
readonly DB_PORT="${GL_DB_PORT:-5432}"
readonly DB_NAME="${GL_DB_NAME:-greylegacy}"
readonly DB_USER="${GL_DB_USER:-etl_reader}"
readonly PGPASSFILE="${HOME}/.pgpass"

readonly OUTBOUND_DIR="/data/outbound/claims"
readonly TIMESTAMP="$(date +%Y%m%d_%H%M%S)"
readonly EXPORT_FILE="${OUTBOUND_DIR}/open_claims_${TIMESTAMP}.csv"
readonly CHECKSUM_FILE="${EXPORT_FILE}.sha256"
readonly CONTROL_FILE="${EXPORT_FILE}.ctl"

readonly SFTP_HOST="${GL_SFTP_HOST:-sftp.partner.example.com}"
readonly SFTP_USER="${GL_SFTP_USER:-greylegacy_etl}"
readonly SFTP_KEY="/opt/greylegacy/.ssh/partner_sftp_id_rsa"
readonly SFTP_REMOTE_DIR="/inbound/claims"

readonly MAIL_TO="data-ops@greylegacy.com"
readonly MAIL_FROM="etl-noreply@greylegacy.com"
readonly MAIL_SUBJECT_PREFIX="[Grey Legacy ETL]"

DRY_RUN=false
NO_TRANSFER=false

# ---------------------------------------------------------------------------
# Logging
# ---------------------------------------------------------------------------
mkdir -p "${LOG_DIR}" "${OUTBOUND_DIR}"
exec > >(tee -a "${LOG_FILE}") 2>&1

log() { echo "[$(date '+%Y-%m-%d %H:%M:%S')] [$1] $2"; }
log_info()  { log "INFO"  "$*"; }
log_warn()  { log "WARN"  "$*"; }
log_error() { log "ERROR" "$*"; }

# ---------------------------------------------------------------------------
# Parse arguments
# ---------------------------------------------------------------------------
while [[ $# -gt 0 ]]; do
    case "$1" in
        --dry-run)     DRY_RUN=true;     shift ;;
        --no-transfer) NO_TRANSFER=true; shift ;;
        *) log_error "Unknown argument: $1"; exit 1 ;;
    esac
done

# ---------------------------------------------------------------------------
# Lock file management
# ---------------------------------------------------------------------------
acquire_lock() {
    mkdir -p "$(dirname "${LOCK_FILE}")"
    if [[ -f "${LOCK_FILE}" ]]; then
        local lock_pid
        lock_pid=$(cat "${LOCK_FILE}")
        if kill -0 "${lock_pid}" 2>/dev/null; then
            log_error "Another instance is running (PID ${lock_pid}). Exiting."
            exit 1
        else
            log_warn "Stale lock file found (PID ${lock_pid}). Removing."
            rm -f "${LOCK_FILE}"
        fi
    fi
    echo $$ > "${LOCK_FILE}"
    trap release_lock EXIT
}

release_lock() {
    rm -f "${LOCK_FILE}"
}

# ---------------------------------------------------------------------------
# Log rotation
# ---------------------------------------------------------------------------
rotate_logs() {
    local count
    count=$(find "${LOG_DIR}" -name "claims-export-*.log" -type f | wc -l)
    if [[ "${count}" -gt "${LOG_MAX_FILES}" ]]; then
        find "${LOG_DIR}" -name "claims-export-*.log" -type f | sort | head -n -"${LOG_MAX_FILES}" | xargs rm -f
        log_info "Rotated old log files (kept last ${LOG_MAX_FILES})"
    fi
}

# ---------------------------------------------------------------------------
# Export query
# ---------------------------------------------------------------------------
export_claims() {
    log_info "Exporting open claims with policy data..."

    local sql
    sql=$(cat <<'EOSQL'
COPY (
    SELECT
        c.claim_id,
        c.claim_number,
        c.claim_type,
        c.status,
        c.date_of_loss,
        c.date_reported,
        c.description,
        c.estimated_amount,
        c.paid_amount,
        c.reserved_amount,
        c.deductible,
        p.policy_number,
        p.policy_type,
        p.effective_date,
        p.expiration_date,
        p.insured_name,
        p.insured_address,
        a.adjuster_name,
        a.adjuster_email,
        c.created_date,
        c.last_modified_date
    FROM claims c
    INNER JOIN policies p ON c.policy_id = p.policy_id
    LEFT JOIN adjusters a ON c.adjuster_id = a.adjuster_id
    WHERE c.status IN ('OPEN', 'REOPENED', 'UNDER_REVIEW', 'PENDING_APPROVAL')
      AND c.date_of_loss >= CURRENT_DATE - INTERVAL '2 years'
    ORDER BY c.date_of_loss DESC
) TO STDOUT WITH (FORMAT CSV, HEADER TRUE, DELIMITER ',', QUOTE '"', ENCODING 'UTF-8');
EOSQL
)

    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would execute export query"
        echo "claim_id,claim_number,claim_type,status,date_of_loss" > "${EXPORT_FILE}"
        echo "0,DRY-RUN,AUTO,OPEN,2025-01-01" >> "${EXPORT_FILE}"
        return 0
    fi

    PGPASSFILE="${PGPASSFILE}" psql -h "${DB_HOST}" -p "${DB_PORT}" -U "${DB_USER}" -d "${DB_NAME}" \
        --no-align --tuples-only -c "${sql}" > "${EXPORT_FILE}"

    if [[ $? -ne 0 ]]; then
        log_error "Database export failed"
        return 1
    fi

    log_info "Export written to ${EXPORT_FILE}"
}

# ---------------------------------------------------------------------------
# File validation
# ---------------------------------------------------------------------------
validate_export() {
    log_info "Validating export file..."

    if [[ ! -f "${EXPORT_FILE}" ]]; then
        log_error "Export file not found: ${EXPORT_FILE}"
        return 1
    fi

    local file_size row_count
    file_size=$(stat -c%s "${EXPORT_FILE}" 2>/dev/null || stat -f%z "${EXPORT_FILE}")
    row_count=$(wc -l < "${EXPORT_FILE}")

    # Subtract header row
    local data_rows=$((row_count - 1))

    if [[ "${file_size}" -lt 100 ]]; then
        log_error "Export file suspiciously small (${file_size} bytes)"
        return 1
    fi

    if [[ "${data_rows}" -lt 1 ]]; then
        log_warn "Export contains no data rows"
    fi

    # Generate checksum
    sha256sum "${EXPORT_FILE}" > "${CHECKSUM_FILE}"
    log_info "Checksum: $(cat "${CHECKSUM_FILE}")"

    # Write control file
    cat > "${CONTROL_FILE}" <<EOF
FILENAME=$(basename "${EXPORT_FILE}")
EXPORT_DATE=${TIMESTAMP}
ROW_COUNT=${data_rows}
FILE_SIZE=${file_size}
CHECKSUM=$(awk '{print $1}' "${CHECKSUM_FILE}")
SOURCE_SYSTEM=GREYLEGACY
ENVIRONMENT=PRODUCTION
EOF

    log_info "Validation passed: ${data_rows} data rows, ${file_size} bytes"
}

# ---------------------------------------------------------------------------
# SFTP transfer
# ---------------------------------------------------------------------------
transfer_files() {
    if [[ "${NO_TRANSFER}" == "true" ]]; then
        log_info "Skipping file transfer (--no-transfer)"
        return 0
    fi

    log_info "Transferring files to ${SFTP_HOST}:${SFTP_REMOTE_DIR}..."

    sftp -i "${SFTP_KEY}" -o StrictHostKeyChecking=yes -b - "${SFTP_USER}@${SFTP_HOST}" <<EOF
cd ${SFTP_REMOTE_DIR}
put ${EXPORT_FILE}
put ${CHECKSUM_FILE}
put ${CONTROL_FILE}
bye
EOF

    if [[ $? -eq 0 ]]; then
        log_info "Transfer completed successfully"
    else
        log_error "SFTP transfer failed"
        return 1
    fi
}

# ---------------------------------------------------------------------------
# Email notification
# ---------------------------------------------------------------------------
send_notification() {
    local status="$1"
    local row_count="${2:-N/A}"
    local subject="${MAIL_SUBJECT_PREFIX} Claims Export ${status} - $(date +%Y-%m-%d)"
    local body

    body="Grey Legacy Claims Export Report\n"
    body+="================================\n"
    body+="Status:    ${status}\n"
    body+="Date:      $(date '+%Y-%m-%d %H:%M:%S')\n"
    body+="File:      $(basename "${EXPORT_FILE}")\n"
    body+="Rows:      ${row_count}\n"
    body+="Server:    $(hostname)\n"
    body+="Log:       ${LOG_FILE}\n"

    echo -e "${body}" | mail -s "${subject}" -r "${MAIL_FROM}" "${MAIL_TO}" 2>/dev/null || \
        log_warn "Failed to send email notification"
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
main() {
    log_info "=========================================="
    log_info "Claims ETL Export starting"
    log_info "=========================================="

    acquire_lock
    rotate_logs

    if export_claims && validate_export && transfer_files; then
        local row_count
        row_count=$(( $(wc -l < "${EXPORT_FILE}") - 1 ))
        send_notification "SUCCESS" "${row_count}"
        log_info "=========================================="
        log_info "ETL export completed successfully"
        log_info "=========================================="
        exit 0
    else
        send_notification "FAILURE"
        log_error "=========================================="
        log_error "ETL export FAILED - check logs"
        log_error "=========================================="
        exit 1
    fi
}

main "$@"
