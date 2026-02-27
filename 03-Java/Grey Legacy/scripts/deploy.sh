#!/bin/bash
###############################################################################
# deploy.sh - Grey Legacy Claims System Deployment Script
# 
# Deploys the Grey Legacy WAR artifact to Apache Tomcat.
# Supports rollback, health checking, and CI/CD integration.
#
# Usage: ./deploy.sh [--war <path>] [--env <environment>] [--skip-backup]
# Exit Codes:
#   0 - Deployment successful
#   1 - General error
#   2 - Pre-deployment check failed
#   3 - Tomcat shutdown failed
#   4 - Deployment failed
#   5 - Health check failed (rollback attempted)
#   6 - Rollback failed
#
# Maintainer: Grey Legacy Infrastructure Team
# Last Modified: 2025-11-14
###############################################################################
set -euo pipefail
IFS=$'\n\t'

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
readonly TOMCAT_HOME="${TOMCAT_HOME:-/opt/tomcat}"
readonly APP_NAME="greylegacy"
readonly WAR_FILE="${WAR_FILE:-$(dirname "$0")/../web/target/${APP_NAME}.war}"
readonly BACKUP_DIR="/var/backups/greylegacy/deployments"
readonly LOG_FILE="/var/log/greylegacy/deploy-$(date +%Y%m%d-%H%M%S).log"
readonly DEPLOY_DIR="${TOMCAT_HOME}/webapps"
readonly HEALTH_URL="http://localhost:8080/${APP_NAME}/health"
readonly HEALTH_TIMEOUT=120          # seconds to wait for healthy status
readonly HEALTH_INTERVAL=5           # seconds between health checks
readonly SHUTDOWN_TIMEOUT=30         # seconds to wait for graceful shutdown
readonly MIN_DISK_MB=500             # minimum free disk space in MB
readonly REQUIRED_JAVA_MAJOR=8       # minimum Java major version
readonly TIMESTAMP="$(date +%Y%m%d-%H%M%S)"

# ---------------------------------------------------------------------------
# Color codes
# ---------------------------------------------------------------------------
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly CYAN='\033[0;36m'
readonly NC='\033[0m' # No Color

# ---------------------------------------------------------------------------
# Logging
# ---------------------------------------------------------------------------
mkdir -p "$(dirname "${LOG_FILE}")"
exec > >(tee -a "${LOG_FILE}") 2>&1

log_info()  { echo -e "${BLUE}[INFO]${NC}  $(date '+%Y-%m-%d %H:%M:%S') - $*"; }
log_ok()    { echo -e "${GREEN}[OK]${NC}    $(date '+%Y-%m-%d %H:%M:%S') - $*"; }
log_warn()  { echo -e "${YELLOW}[WARN]${NC}  $(date '+%Y-%m-%d %H:%M:%S') - $*"; }
log_error() { echo -e "${RED}[ERROR]${NC} $(date '+%Y-%m-%d %H:%M:%S') - $*"; }
log_step()  { echo -e "${CYAN}[STEP]${NC}  $(date '+%Y-%m-%d %H:%M:%S') - $*"; }

# ---------------------------------------------------------------------------
# Parse arguments
# ---------------------------------------------------------------------------
SKIP_BACKUP=false
ENVIRONMENT="production"
CUSTOM_WAR=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --war)         CUSTOM_WAR="$2"; shift 2 ;;
        --env)         ENVIRONMENT="$2"; shift 2 ;;
        --skip-backup) SKIP_BACKUP=true; shift ;;
        --help)
            echo "Usage: $0 [--war <path>] [--env <environment>] [--skip-backup]"
            exit 0
            ;;
        *) log_error "Unknown argument: $1"; exit 1 ;;
    esac
done

DEPLOY_WAR="${CUSTOM_WAR:-${WAR_FILE}}"

log_info "=============================================="
log_info "Grey Legacy Deployment - ${ENVIRONMENT}"
log_info "Timestamp: ${TIMESTAMP}"
log_info "WAR: ${DEPLOY_WAR}"
log_info "Target: ${DEPLOY_DIR}"
log_info "=============================================="

# ---------------------------------------------------------------------------
# Pre-deployment checks
# ---------------------------------------------------------------------------
pre_deploy_checks() {
    log_step "Running pre-deployment checks..."

    # Check WAR file exists
    if [[ ! -f "${DEPLOY_WAR}" ]]; then
        log_error "WAR file not found: ${DEPLOY_WAR}"
        exit 2
    fi
    log_ok "WAR file found ($(du -h "${DEPLOY_WAR}" | cut -f1))"

    # Check Java version
    if ! command -v java &>/dev/null; then
        log_error "Java is not installed or not in PATH"
        exit 2
    fi
    local java_version
    java_version=$(java -version 2>&1 | head -1 | awk -F'"' '{print $2}' | awk -F'.' '{if ($1 == "1") print $2; else print $1}')
    if [[ "${java_version}" -lt "${REQUIRED_JAVA_MAJOR}" ]]; then
        log_error "Java ${REQUIRED_JAVA_MAJOR}+ required, found Java ${java_version}"
        exit 2
    fi
    log_ok "Java version check passed (Java ${java_version})"

    # Check Tomcat installation
    if [[ ! -d "${TOMCAT_HOME}" ]]; then
        log_error "Tomcat home not found: ${TOMCAT_HOME}"
        exit 2
    fi
    if [[ ! -x "${TOMCAT_HOME}/bin/catalina.sh" ]]; then
        log_error "catalina.sh not found or not executable"
        exit 2
    fi
    log_ok "Tomcat installation verified"

    # Check disk space
    local avail_mb
    avail_mb=$(df -BM "${DEPLOY_DIR}" | tail -1 | awk '{print $4}' | tr -d 'M')
    if [[ "${avail_mb}" -lt "${MIN_DISK_MB}" ]]; then
        log_error "Insufficient disk space: ${avail_mb}MB available, ${MIN_DISK_MB}MB required"
        exit 2
    fi
    log_ok "Disk space check passed (${avail_mb}MB available)"

    # Verify WAR integrity
    if ! jar -tf "${DEPLOY_WAR}" &>/dev/null; then
        log_error "WAR file appears corrupt: ${DEPLOY_WAR}"
        exit 2
    fi
    log_ok "WAR integrity verified"
}

# ---------------------------------------------------------------------------
# Backup current deployment
# ---------------------------------------------------------------------------
backup_current() {
    if [[ "${SKIP_BACKUP}" == "true" ]]; then
        log_warn "Skipping backup (--skip-backup)"
        return 0
    fi

    log_step "Backing up current deployment..."
    mkdir -p "${BACKUP_DIR}"

    if [[ -f "${DEPLOY_DIR}/${APP_NAME}.war" ]]; then
        local backup_file="${BACKUP_DIR}/${APP_NAME}-${TIMESTAMP}.war"
        cp "${DEPLOY_DIR}/${APP_NAME}.war" "${backup_file}"
        log_ok "Backup created: ${backup_file}"
    else
        log_warn "No existing WAR to back up"
    fi

    # Keep only last 10 backups
    local count
    count=$(find "${BACKUP_DIR}" -name "${APP_NAME}-*.war" -type f | wc -l)
    if [[ "${count}" -gt 10 ]]; then
        log_info "Pruning old backups (keeping last 10)..."
        find "${BACKUP_DIR}" -name "${APP_NAME}-*.war" -type f | sort | head -n -10 | xargs rm -f
    fi
}

# ---------------------------------------------------------------------------
# Stop Tomcat
# ---------------------------------------------------------------------------
stop_tomcat() {
    log_step "Stopping Tomcat..."

    local pid
    pid=$(pgrep -f "catalina" || true)

    if [[ -z "${pid}" ]]; then
        log_warn "Tomcat does not appear to be running"
        return 0
    fi

    "${TOMCAT_HOME}/bin/catalina.sh" stop &>/dev/null || true

    local elapsed=0
    while [[ "${elapsed}" -lt "${SHUTDOWN_TIMEOUT}" ]]; do
        if ! pgrep -f "catalina" &>/dev/null; then
            log_ok "Tomcat stopped gracefully (${elapsed}s)"
            return 0
        fi
        sleep 2
        elapsed=$((elapsed + 2))
    done

    log_warn "Graceful shutdown timed out after ${SHUTDOWN_TIMEOUT}s, force-killing..."
    pid=$(pgrep -f "catalina" || true)
    if [[ -n "${pid}" ]]; then
        kill -9 "${pid}" 2>/dev/null || true
        sleep 2
    fi

    if pgrep -f "catalina" &>/dev/null; then
        log_error "Failed to stop Tomcat"
        exit 3
    fi
    log_ok "Tomcat force-stopped"
}

# ---------------------------------------------------------------------------
# Deploy WAR
# ---------------------------------------------------------------------------
deploy_war() {
    log_step "Deploying ${APP_NAME}.war..."

    # Remove old exploded directory and WAR
    rm -rf "${DEPLOY_DIR:?}/${APP_NAME}" "${DEPLOY_DIR:?}/${APP_NAME}.war"

    # Copy new WAR
    cp "${DEPLOY_WAR}" "${DEPLOY_DIR}/${APP_NAME}.war"
    chmod 644 "${DEPLOY_DIR}/${APP_NAME}.war"

    log_ok "WAR deployed to ${DEPLOY_DIR}/${APP_NAME}.war"
}

# ---------------------------------------------------------------------------
# Start Tomcat and health check
# ---------------------------------------------------------------------------
start_and_verify() {
    log_step "Starting Tomcat..."
    "${TOMCAT_HOME}/bin/catalina.sh" start

    log_step "Waiting for application health (timeout: ${HEALTH_TIMEOUT}s)..."
    local elapsed=0
    while [[ "${elapsed}" -lt "${HEALTH_TIMEOUT}" ]]; do
        local http_code
        http_code=$(curl -s -o /dev/null -w "%{http_code}" "${HEALTH_URL}" 2>/dev/null || echo "000")

        if [[ "${http_code}" == "200" ]]; then
            log_ok "Application is healthy (HTTP ${http_code}) after ${elapsed}s"
            return 0
        fi

        echo -ne "  Health check: HTTP ${http_code} (${elapsed}/${HEALTH_TIMEOUT}s)\r"
        sleep "${HEALTH_INTERVAL}"
        elapsed=$((elapsed + HEALTH_INTERVAL))
    done

    echo ""
    log_error "Health check failed after ${HEALTH_TIMEOUT}s"
    return 1
}

# ---------------------------------------------------------------------------
# Rollback
# ---------------------------------------------------------------------------
rollback() {
    log_warn "Initiating rollback..."

    local latest_backup
    latest_backup=$(find "${BACKUP_DIR}" -name "${APP_NAME}-*.war" -type f | sort | tail -1)

    if [[ -z "${latest_backup}" ]]; then
        log_error "No backup found for rollback!"
        exit 6
    fi

    log_info "Rolling back to: ${latest_backup}"
    stop_tomcat

    rm -rf "${DEPLOY_DIR:?}/${APP_NAME}" "${DEPLOY_DIR:?}/${APP_NAME}.war"
    cp "${latest_backup}" "${DEPLOY_DIR}/${APP_NAME}.war"

    "${TOMCAT_HOME}/bin/catalina.sh" start
    sleep 10

    local http_code
    http_code=$(curl -s -o /dev/null -w "%{http_code}" "${HEALTH_URL}" 2>/dev/null || echo "000")
    if [[ "${http_code}" == "200" ]]; then
        log_ok "Rollback successful"
    else
        log_error "Rollback may have failed (HTTP ${http_code})"
        exit 6
    fi
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
main() {
    local start_time
    start_time=$(date +%s)

    pre_deploy_checks
    backup_current
    stop_tomcat
    deploy_war

    if start_and_verify; then
        local end_time duration
        end_time=$(date +%s)
        duration=$((end_time - start_time))
        log_info "=============================================="
        log_ok "Deployment completed successfully in ${duration}s"
        log_info "=============================================="
        exit 0
    else
        log_error "Deployment failed - attempting rollback"
        rollback
        exit 5
    fi
}

main "$@"
