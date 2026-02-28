#!/bin/bash
###############################################################################
# uninstall.sh - Grey Legacy Claims System Uninstaller (Linux)
#
# Removes Grey Legacy application, stops and removes the systemd service,
# cleans up firewall rules, and optionally removes data and logs.
#
# Usage: Run as root or with sudo
#   sudo ./uninstall.sh [OPTIONS]
#
# Options:
#   --silent       Non-interactive uninstallation
#   --keep-data    Preserve application data directory
#   --keep-logs    Preserve log files
#   --keep-user    Do not remove the service user account
#
# Exit Codes:
#   0 - Uninstallation successful
#   1 - General error
#
# Maintainer: Grey Legacy Infrastructure Team
###############################################################################
set -uo pipefail

# ---------------------------------------------------------------------------
#  Default Configuration
# ---------------------------------------------------------------------------
APP_NAME="Grey Legacy Claims System"
APP_SHORT="greylegacy"
SILENT=false
KEEP_DATA=false
KEEP_LOGS=false
KEEP_USER=false
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# ---------------------------------------------------------------------------
#  Color Codes
# ---------------------------------------------------------------------------
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

log_info()  { echo -e "${BLUE}[INFO]${NC}  $*"; }
log_ok()    { echo -e "${GREEN}[OK]${NC}    $*"; }
log_warn()  { echo -e "${YELLOW}[WARN]${NC}  $*"; }
log_error() { echo -e "${RED}[ERROR]${NC} $*"; }

# ---------------------------------------------------------------------------
#  Load Install Metadata
# ---------------------------------------------------------------------------
META_FILE="${SCRIPT_DIR}/install.meta"
if [[ -f "${META_FILE}" ]]; then
    # shellcheck source=/dev/null
    source "${META_FILE}"
else
    # Fallback defaults
    INSTALL_DIR="/opt/greylegacy"
    TOMCAT_DIR="/opt/greylegacy/tomcat"
    CONF_DIR="/opt/greylegacy/conf"
    DATA_DIR="/opt/greylegacy/data"
    LOG_DIR="/var/log/greylegacy"
    SERVICE_NAME="greylegacy"
    APP_USER="greylegacy"
    APP_GROUP="greylegacy"
    APP_PORT=8080
fi

# ---------------------------------------------------------------------------
#  Parse Arguments
# ---------------------------------------------------------------------------
while [[ $# -gt 0 ]]; do
    case "$1" in
        --silent)    SILENT=true;    shift ;;
        --keep-data) KEEP_DATA=true; shift ;;
        --keep-logs) KEEP_LOGS=true; shift ;;
        --keep-user) KEEP_USER=true; shift ;;
        *)           shift ;;
    esac
done

# ---------------------------------------------------------------------------
#  Check Root Privileges
# ---------------------------------------------------------------------------
if [[ $EUID -ne 0 ]]; then
    log_error "This uninstaller must be run as root (use sudo)."
    exit 1
fi

# ---------------------------------------------------------------------------
#  Banner
# ---------------------------------------------------------------------------
echo ""
echo "  ==========================================================="
echo "   ${APP_NAME} - Linux Uninstaller"
echo "  ==========================================================="
echo ""
echo "   Install Directory : ${INSTALL_DIR}"
echo "   Service Name      : ${SERVICE_NAME}"
echo "   Keep Data         : ${KEEP_DATA}"
echo "   Keep Logs         : ${KEEP_LOGS}"
echo "   Keep User         : ${KEEP_USER}"
echo ""

if [[ "${SILENT}" == "false" ]]; then
    read -rp "Are you sure you want to uninstall? [y/N]: " CONFIRM
    if [[ "${CONFIRM,,}" != "y" ]]; then
        echo "Uninstallation cancelled by user."
        exit 0
    fi
fi

# ---------------------------------------------------------------------------
#  Step 1: Stop and Remove Systemd Service
# ---------------------------------------------------------------------------
log_info "[1/5] Stopping service..."

if systemctl is-active --quiet "${SERVICE_NAME}" 2>/dev/null; then
    log_info "Stopping service '${SERVICE_NAME}'..."
    systemctl stop "${SERVICE_NAME}" 2>/dev/null || true
    # Wait for process to terminate
    sleep 3
fi

if systemctl is-enabled --quiet "${SERVICE_NAME}" 2>/dev/null; then
    systemctl disable "${SERVICE_NAME}" 2>/dev/null || true
fi

if [[ -f "/etc/systemd/system/${SERVICE_NAME}.service" ]]; then
    rm -f "/etc/systemd/system/${SERVICE_NAME}.service"
    systemctl daemon-reload
    log_ok "Systemd service removed."
else
    log_info "No systemd service file found. Skipping."
fi

# ---------------------------------------------------------------------------
#  Step 2: Remove Firewall Rules
# ---------------------------------------------------------------------------
log_info "[2/5] Removing firewall rules..."

if command -v firewall-cmd &>/dev/null; then
    firewall-cmd --permanent --remove-port="${APP_PORT}/tcp" 2>/dev/null && \
    firewall-cmd --reload 2>/dev/null && \
    log_ok "firewalld: Port ${APP_PORT} rule removed." || \
    log_info "No firewalld rule to remove."
elif command -v ufw &>/dev/null; then
    ufw delete allow "${APP_PORT}/tcp" 2>/dev/null && \
    log_ok "ufw: Port ${APP_PORT} rule removed." || \
    log_info "No ufw rule to remove."
elif command -v iptables &>/dev/null; then
    iptables -D INPUT -p tcp --dport "${APP_PORT}" -j ACCEPT 2>/dev/null && \
    log_ok "iptables: Port ${APP_PORT} rule removed." || \
    log_info "No iptables rule to remove."
fi

# ---------------------------------------------------------------------------
#  Step 3: Remove Application Files
# ---------------------------------------------------------------------------
log_info "[3/5] Removing application files..."

if [[ -d "${INSTALL_DIR}" ]]; then
    # Remove Tomcat
    if [[ -d "${TOMCAT_DIR}" ]]; then
        rm -rf "${TOMCAT_DIR}"
        log_info "Tomcat removed."
    fi

    # Remove configuration
    if [[ -d "${CONF_DIR}" ]]; then
        rm -rf "${CONF_DIR}"
        log_info "Configuration removed."
    fi

    # Remove backups
    if [[ -d "${INSTALL_DIR}/backups" ]]; then
        rm -rf "${INSTALL_DIR}/backups"
        log_info "Backups removed."
    fi

    # Conditionally remove data
    if [[ "${KEEP_DATA}" == "false" ]]; then
        if [[ -d "${DATA_DIR}" ]]; then
            rm -rf "${DATA_DIR}"
            log_info "Data directory removed."
        fi
    else
        log_info "Keeping data directory: ${DATA_DIR}"
    fi

    # Remove metadata and uninstaller
    rm -f "${INSTALL_DIR}/install.meta" 2>/dev/null
    rm -f "${INSTALL_DIR}/uninstall.sh" 2>/dev/null

    # Try to remove install directory
    rmdir "${INSTALL_DIR}" 2>/dev/null && \
        log_ok "Install directory removed." || \
        log_info "Install directory not fully removed (preserved subdirectories)."
else
    log_info "Install directory not found: ${INSTALL_DIR}"
fi

# Conditionally remove logs
if [[ "${KEEP_LOGS}" == "false" ]]; then
    if [[ -d "${LOG_DIR}" ]]; then
        rm -rf "${LOG_DIR}"
        log_info "Logs removed."
    fi
else
    log_info "Keeping logs directory: ${LOG_DIR}"
fi

# ---------------------------------------------------------------------------
#  Step 4: Remove Service User
# ---------------------------------------------------------------------------
log_info "[4/5] Cleaning up service user..."

if [[ "${KEEP_USER}" == "false" ]]; then
    if id "${APP_USER}" &>/dev/null; then
        userdel "${APP_USER}" 2>/dev/null && \
            log_ok "User '${APP_USER}' removed." || \
            log_warn "Could not remove user '${APP_USER}'."
    fi
    if getent group "${APP_GROUP}" &>/dev/null; then
        groupdel "${APP_GROUP}" 2>/dev/null || true
    fi
else
    log_info "Keeping service user '${APP_USER}'."
fi

# ---------------------------------------------------------------------------
#  Step 5: Cleanup
# ---------------------------------------------------------------------------
log_info "[5/5] Final cleanup..."

# Remove any leftover PID files
rm -f /var/run/${APP_SHORT}.pid 2>/dev/null
rm -f /tmp/${APP_SHORT}-*.tmp 2>/dev/null

# Remove desktop shortcut
rm -f /usr/share/applications/greylegacy.desktop 2>/dev/null

log_ok "Cleanup complete."

# ---------------------------------------------------------------------------
#  Uninstallation Complete
# ---------------------------------------------------------------------------
echo ""
echo "  ==========================================================="
echo "   Uninstallation Complete!"
echo "  ==========================================================="
echo ""
[[ "${KEEP_DATA}" == "true" ]] && echo "   Data preserved at: ${DATA_DIR}"
[[ "${KEEP_LOGS}" == "true" ]] && echo "   Logs preserved at: ${LOG_DIR}"
[[ "${KEEP_USER}" == "true" ]] && echo "   User preserved: ${APP_USER}"
echo ""
echo "   ${APP_NAME} has been removed from this system."
echo "  ==========================================================="
echo ""

exit 0
