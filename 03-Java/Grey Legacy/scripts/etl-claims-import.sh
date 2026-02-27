#!/bin/bash
###############################################################################
# etl-claims-import.sh - Claims Data ETL Import
#
# Imports claims from CSV files dropped in the inbound directory.
# Loads into staging tables, validates data quality, merges valid records
# into production, and generates a reconciliation report.
#
# Designed to complement etl-claims-export.sh (outbound) with the full
# inbound ETL pipeline:
#   1. Poll inbound directory for new CSV files
#   2. Load CSV into STG_CLAIM_IMPORT staging table
#   3. Run SP_VALIDATE_STAGED_CLAIMS for data quality checks
#   4. Run SP_MERGE_STAGED_CLAIMS for valid records
#   5. Run SP_ETL_RECONCILIATION for verification
#   6. Archive processed files
#   7. Send notification with results
#
# Usage: ./etl-claims-import.sh [--dry-run] [--file <specific-file>]
# Cron:  */30 * * * * /opt/greylegacy/scripts/etl-claims-import.sh
#
# Maintainer: Grey Legacy Data Engineering
# Last Modified: 2026-03-15
###############################################################################
set -euo pipefail
IFS=$'\n\t'

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
readonly SCRIPT_NAME="$(basename "$0")"
readonly LOCK_FILE="/var/run/greylegacy/${SCRIPT_NAME}.lock"
readonly LOG_DIR="/var/log/greylegacy/etl"
readonly LOG_FILE="${LOG_DIR}/claims-import-$(date +%Y%m%d).log"
readonly LOG_MAX_FILES=30

readonly DB_HOST="${GL_DB_HOST:-db-prod.greylegacy.internal}"
readonly DB_PORT="${GL_DB_PORT:-5432}"
readonly DB_NAME="${GL_DB_NAME:-greylegacy}"
readonly DB_USER="${GL_DB_USER:-etl_service}"
readonly PGPASSFILE="${HOME}/.pgpass"

readonly INBOUND_DIR="/data/inbound/claims"
readonly ARCHIVE_DIR="/data/archive/claims"
readonly REJECT_DIR="/data/rejected/claims"
readonly PROCESSING_DIR="/data/processing/claims"
readonly FILE_PATTERN="*.csv"
readonly TIMESTAMP="$(date +%Y%m%d_%H%M%S)"

readonly MAIL_TO="data-ops@greylegacy.com"
readonly MAIL_FROM="etl-noreply@greylegacy.com"
readonly MAIL_SUBJECT_PREFIX="[Grey Legacy ETL]"

# Data quality thresholds — abort if reject rate exceeds threshold
readonly MAX_REJECT_RATE=30   # percent — if >30% of rows are bad, abort merge

DRY_RUN=false
SPECIFIC_FILE=""

# ---------------------------------------------------------------------------
# Logging
# ---------------------------------------------------------------------------
mkdir -p "${LOG_DIR}" "${INBOUND_DIR}" "${ARCHIVE_DIR}" "${REJECT_DIR}" "${PROCESSING_DIR}"
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
        --dry-run)  DRY_RUN=true;           shift ;;
        --file)     SPECIFIC_FILE="$2";     shift 2 ;;
        *) log_error "Unknown argument: $1"; exit 1 ;;
    esac
done

# ---------------------------------------------------------------------------
# Lock file management (prevent concurrent runs)
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
    count=$(find "${LOG_DIR}" -name "claims-import-*.log" -type f | wc -l)
    if [[ "${count}" -gt "${LOG_MAX_FILES}" ]]; then
        find "${LOG_DIR}" -name "claims-import-*.log" -type f | sort | head -n -"${LOG_MAX_FILES}" | xargs rm -f
        log_info "Rotated old log files (kept last ${LOG_MAX_FILES})"
    fi
}

# ---------------------------------------------------------------------------
# Validate control file (if present)
# ---------------------------------------------------------------------------
validate_control_file() {
    local csv_file="$1"
    local ctl_file="${csv_file%.csv}.ctl"

    if [[ ! -f "${ctl_file}" ]]; then
        log_warn "No control file found for $(basename "${csv_file}") — skipping checksum validation"
        return 0
    fi

    log_info "Validating control file: $(basename "${ctl_file}")"

    # Parse control file
    local expected_rows expected_checksum
    expected_rows=$(grep -i "^ROW_COUNT=" "${ctl_file}" | cut -d= -f2 | tr -d '[:space:]')
    expected_checksum=$(grep -i "^CHECKSUM=" "${ctl_file}" | cut -d= -f2 | tr -d '[:space:]')

    # Verify checksum if provided
    if [[ -n "${expected_checksum}" ]]; then
        local actual_checksum
        actual_checksum=$(sha256sum "${csv_file}" | awk '{print $1}')
        if [[ "${actual_checksum}" != "${expected_checksum}" ]]; then
            log_error "Checksum mismatch for $(basename "${csv_file}")"
            log_error "  Expected: ${expected_checksum}"
            log_error "  Actual:   ${actual_checksum}"
            return 1
        fi
        log_info "Checksum verified OK"
    fi

    # Verify row count if provided
    if [[ -n "${expected_rows}" ]]; then
        local actual_rows
        actual_rows=$(( $(wc -l < "${csv_file}") - 1 ))  # Subtract header
        if [[ "${actual_rows}" -ne "${expected_rows}" ]]; then
            log_warn "Row count mismatch: expected ${expected_rows}, found ${actual_rows}"
            # Warning only — don't abort on count mismatch
        fi
    fi

    return 0
}

# ---------------------------------------------------------------------------
# Load CSV into staging table
# ---------------------------------------------------------------------------
load_csv_to_staging() {
    local csv_file="$1"
    local filename
    filename=$(basename "${csv_file}")
    log_info "Loading ${filename} into staging..."

    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would load ${filename} into STG_CLAIM_IMPORT"
        echo "1"
        return 0
    fi

    # Create the ETL batch log entry and capture the batch ID
    local batch_id
    batch_id=$(PGPASSFILE="${PGPASSFILE}" psql -h "${DB_HOST}" -p "${DB_PORT}" \
        -U "${DB_USER}" -d "${DB_NAME}" --no-align --tuples-only -c \
        "INSERT INTO ETL_BATCH_LOG (BATCH_TYPE, SOURCE_FILENAME, STATUS, EXECUTED_BY)
         VALUES ('CLAIM_IMPORT', '${filename}', 'STARTED', '${SCRIPT_NAME}')
         RETURNING BATCH_ID;")

    if [[ -z "${batch_id}" ]]; then
        log_error "Failed to create ETL batch log entry"
        return 1
    fi
    log_info "Created batch ID: ${batch_id}"

    # Load CSV using PostgreSQL COPY (skip header row)
    # Use a temporary table approach to handle the header
    local row_count
    row_count=$(PGPASSFILE="${PGPASSFILE}" psql -h "${DB_HOST}" -p "${DB_PORT}" \
        -U "${DB_USER}" -d "${DB_NAME}" --no-align --tuples-only <<EOSQL
-- Create temp table matching CSV structure
CREATE TEMP TABLE tmp_import (
    claim_number VARCHAR(50),
    policy_number VARCHAR(50),
    claimant_first_name VARCHAR(100),
    claimant_last_name VARCHAR(100),
    claim_type VARCHAR(50),
    loss_date VARCHAR(50),
    loss_description VARCHAR(2000),
    estimated_loss VARCHAR(50),
    loss_location VARCHAR(500)
);

-- Load CSV (COPY skips header with HEADER option)
\\COPY tmp_import FROM '${csv_file}' WITH (FORMAT CSV, HEADER TRUE, ENCODING 'UTF-8');

-- Insert into staging with batch ID and line numbers
INSERT INTO STG_CLAIM_IMPORT (
    BATCH_ID, LINE_NUMBER,
    CLAIM_NUMBER, POLICY_NUMBER,
    CLAIMANT_FIRST_NAME, CLAIMANT_LAST_NAME,
    CLAIM_TYPE, LOSS_DATE, LOSS_DESCRIPTION,
    ESTIMATED_LOSS, LOSS_LOCATION,
    VALIDATION_STATUS
)
SELECT
    ${batch_id},
    ROW_NUMBER() OVER () + 1,  -- +1 for header row
    claim_number, policy_number,
    claimant_first_name, claimant_last_name,
    claim_type, loss_date, loss_description,
    estimated_loss, loss_location,
    'PENDING'
FROM tmp_import;

-- Update total rows in batch log
UPDATE ETL_BATCH_LOG SET TOTAL_ROWS = (
    SELECT COUNT(*) FROM STG_CLAIM_IMPORT WHERE BATCH_ID = ${batch_id}
) WHERE BATCH_ID = ${batch_id};

-- Return the count
SELECT COUNT(*) FROM STG_CLAIM_IMPORT WHERE BATCH_ID = ${batch_id};

DROP TABLE tmp_import;
EOSQL
)

    log_info "Loaded ${row_count} rows into staging (batch ${batch_id})"
    echo "${batch_id}"
}

# ---------------------------------------------------------------------------
# Run validation stored procedure
# ---------------------------------------------------------------------------
run_validation() {
    local batch_id="$1"
    log_info "Running data quality validation for batch ${batch_id}..."

    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would run SP_VALIDATE_STAGED_CLAIMS(${batch_id})"
        return 0
    fi

    PGPASSFILE="${PGPASSFILE}" psql -h "${DB_HOST}" -p "${DB_PORT}" \
        -U "${DB_USER}" -d "${DB_NAME}" --no-align --tuples-only -c \
        "SELECT SP_VALIDATE_STAGED_CLAIMS(${batch_id});"

    # Check reject rate
    local stats
    stats=$(PGPASSFILE="${PGPASSFILE}" psql -h "${DB_HOST}" -p "${DB_PORT}" \
        -U "${DB_USER}" -d "${DB_NAME}" --no-align --tuples-only -c \
        "SELECT TOTAL_ROWS, VALID_ROWS, REJECTED_ROWS, DUPLICATE_ROWS
         FROM ETL_BATCH_LOG WHERE BATCH_ID = ${batch_id};")

    local total valid rejected duplicates
    total=$(echo "${stats}" | cut -d'|' -f1)
    valid=$(echo "${stats}" | cut -d'|' -f2)
    rejected=$(echo "${stats}" | cut -d'|' -f3)
    duplicates=$(echo "${stats}" | cut -d'|' -f4)

    log_info "Validation results: total=${total}, valid=${valid}, rejected=${rejected}, duplicates=${duplicates}"

    # Abort if reject rate exceeds threshold
    if [[ "${total}" -gt 0 ]]; then
        local reject_rate=$(( rejected * 100 / total ))
        if [[ "${reject_rate}" -gt "${MAX_REJECT_RATE}" ]]; then
            log_error "Reject rate ${reject_rate}% exceeds threshold ${MAX_REJECT_RATE}% — aborting merge"
            PGPASSFILE="${PGPASSFILE}" psql -h "${DB_HOST}" -p "${DB_PORT}" \
                -U "${DB_USER}" -d "${DB_NAME}" -c \
                "UPDATE ETL_BATCH_LOG SET STATUS = 'FAILED',
                 ERROR_MESSAGE = 'Reject rate ${reject_rate}% exceeds threshold ${MAX_REJECT_RATE}%',
                 COMPLETED_AT = CURRENT_TIMESTAMP
                 WHERE BATCH_ID = ${batch_id};"
            return 1
        fi
    fi

    return 0
}

# ---------------------------------------------------------------------------
# Run merge stored procedure
# ---------------------------------------------------------------------------
run_merge() {
    local batch_id="$1"
    log_info "Merging valid records from batch ${batch_id} into production..."

    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would run SP_MERGE_STAGED_CLAIMS(${batch_id})"
        return 0
    fi

    local merged
    merged=$(PGPASSFILE="${PGPASSFILE}" psql -h "${DB_HOST}" -p "${DB_PORT}" \
        -U "${DB_USER}" -d "${DB_NAME}" --no-align --tuples-only -c \
        "SELECT SP_MERGE_STAGED_CLAIMS(${batch_id});")

    log_info "Merged ${merged} records into production CLAIM table"
}

# ---------------------------------------------------------------------------
# Generate reconciliation report
# ---------------------------------------------------------------------------
generate_reconciliation_report() {
    local batch_id="$1"
    local report_file="${ARCHIVE_DIR}/reconciliation_${TIMESTAMP}_batch${batch_id}.txt"

    log_info "Generating reconciliation report..."

    PGPASSFILE="${PGPASSFILE}" psql -h "${DB_HOST}" -p "${DB_PORT}" \
        -U "${DB_USER}" -d "${DB_NAME}" -c \
        "SELECT * FROM SP_ETL_RECONCILIATION(${batch_id});" > "${report_file}"

    # Also dump rejected records for review
    local reject_file="${REJECT_DIR}/rejected_claims_${TIMESTAMP}_batch${batch_id}.csv"
    PGPASSFILE="${PGPASSFILE}" psql -h "${DB_HOST}" -p "${DB_PORT}" \
        -U "${DB_USER}" -d "${DB_NAME}" --csv -c \
        "SELECT STG_ID, LINE_NUMBER, CLAIM_NUMBER, POLICY_NUMBER,
                VALIDATION_STATUS, VALIDATION_ERRORS
         FROM STG_CLAIM_IMPORT
         WHERE BATCH_ID = ${batch_id}
           AND VALIDATION_STATUS IN ('REJECTED', 'DUPLICATE')
         ORDER BY LINE_NUMBER;" > "${reject_file}"

    local reject_count
    reject_count=$(( $(wc -l < "${reject_file}") - 1 ))
    if [[ "${reject_count}" -gt 0 ]]; then
        log_warn "${reject_count} rejected records written to ${reject_file}"
    else
        rm -f "${reject_file}"  # Clean up empty reject file
    fi

    log_info "Reconciliation report: ${report_file}"
}

# ---------------------------------------------------------------------------
# Archive processed file
# ---------------------------------------------------------------------------
archive_file() {
    local csv_file="$1"
    local filename
    filename=$(basename "${csv_file}")
    local archive_name="${ARCHIVE_DIR}/${TIMESTAMP}_${filename}"

    mv "${csv_file}" "${archive_name}"
    log_info "Archived: ${filename} → $(basename "${archive_name}")"

    # Also archive control and checksum files if present
    for ext in ctl sha256; do
        local companion="${csv_file%.csv}.${ext}"
        if [[ -f "${companion}" ]]; then
            mv "${companion}" "${ARCHIVE_DIR}/${TIMESTAMP}_${filename%.csv}.${ext}"
        fi
    done
}

# ---------------------------------------------------------------------------
# Email notification
# ---------------------------------------------------------------------------
send_notification() {
    local status="$1"
    local details="$2"
    local subject="${MAIL_SUBJECT_PREFIX} Claims Import ${status} - $(date +%Y-%m-%d)"

    local body
    body="Grey Legacy Claims Import Report\n"
    body+="==================================\n"
    body+="Status:     ${status}\n"
    body+="Date:       $(date '+%Y-%m-%d %H:%M:%S')\n"
    body+="Server:     $(hostname)\n"
    body+="Log:        ${LOG_FILE}\n"
    body+="---\n"
    body+="${details}\n"

    echo -e "${body}" | mail -s "${subject}" -r "${MAIL_FROM}" "${MAIL_TO}" 2>/dev/null || \
        log_warn "Failed to send email notification"
}

# ---------------------------------------------------------------------------
# Process a single CSV file
# ---------------------------------------------------------------------------
process_file() {
    local csv_file="$1"
    local filename
    filename=$(basename "${csv_file}")
    log_info "--- Processing: ${filename} ---"

    # Move to processing directory to prevent re-pickup
    local processing_file="${PROCESSING_DIR}/${filename}"
    cp "${csv_file}" "${processing_file}"

    # Step 1: Validate control file
    if ! validate_control_file "${csv_file}"; then
        log_error "Control file validation failed for ${filename}"
        mv "${csv_file}" "${REJECT_DIR}/${TIMESTAMP}_${filename}"
        rm -f "${processing_file}"
        return 1
    fi

    # Step 2: Load into staging
    local batch_id
    batch_id=$(load_csv_to_staging "${processing_file}")
    if [[ -z "${batch_id}" || "${batch_id}" == "0" ]]; then
        log_error "Failed to load ${filename} into staging"
        rm -f "${processing_file}"
        return 1
    fi

    # Step 3: Run validation
    if ! run_validation "${batch_id}"; then
        log_error "Validation failed/aborted for batch ${batch_id}"
        archive_file "${csv_file}"
        rm -f "${processing_file}"
        return 1
    fi

    # Step 4: Merge valid records
    run_merge "${batch_id}"

    # Step 5: Reconciliation report
    generate_reconciliation_report "${batch_id}"

    # Step 6: Archive the source file
    archive_file "${csv_file}"
    rm -f "${processing_file}"

    log_info "--- Completed: ${filename} (batch ${batch_id}) ---"
    return 0
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
main() {
    log_info "=========================================="
    log_info "Claims ETL Import starting"
    log_info "=========================================="

    acquire_lock
    rotate_logs

    local files_processed=0
    local files_failed=0
    local details=""

    if [[ -n "${SPECIFIC_FILE}" ]]; then
        # Process a specific file
        if [[ -f "${SPECIFIC_FILE}" ]]; then
            if process_file "${SPECIFIC_FILE}"; then
                files_processed=$((files_processed + 1))
                details+="SUCCESS: $(basename "${SPECIFIC_FILE}")\n"
            else
                files_failed=$((files_failed + 1))
                details+="FAILED:  $(basename "${SPECIFIC_FILE}")\n"
            fi
        else
            log_error "File not found: ${SPECIFIC_FILE}"
            exit 1
        fi
    else
        # Poll inbound directory for CSV files
        local csv_files
        csv_files=$(find "${INBOUND_DIR}" -maxdepth 1 -name "${FILE_PATTERN}" -type f | sort)

        if [[ -z "${csv_files}" ]]; then
            log_info "No files found in ${INBOUND_DIR}"
            exit 0
        fi

        log_info "Found $(echo "${csv_files}" | wc -l) file(s) to process"

        while IFS= read -r csv_file; do
            if process_file "${csv_file}"; then
                files_processed=$((files_processed + 1))
                details+="SUCCESS: $(basename "${csv_file}")\n"
            else
                files_failed=$((files_failed + 1))
                details+="FAILED:  $(basename "${csv_file}")\n"
            fi
        done <<< "${csv_files}"
    fi

    # Summary
    local overall_status="SUCCESS"
    if [[ "${files_failed}" -gt 0 ]]; then
        overall_status="PARTIAL_FAILURE"
    fi
    if [[ "${files_processed}" -eq 0 && "${files_failed}" -gt 0 ]]; then
        overall_status="FAILURE"
    fi

    details+="\nSummary: ${files_processed} succeeded, ${files_failed} failed"
    send_notification "${overall_status}" "${details}"

    log_info "=========================================="
    log_info "ETL import completed: ${files_processed} succeeded, ${files_failed} failed"
    log_info "=========================================="

    [[ "${files_failed}" -eq 0 ]] && exit 0 || exit 1
}

main "$@"
