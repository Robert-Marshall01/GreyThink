#!/bin/bash
###############################################################################
# uninstall.sh - Grey Legacy Claims System Uninstaller (macOS)
#
# Removes Grey Legacy application, unloads and removes the LaunchDaemon,
# and cleans up application files.
#
# Usage: Run with sudo
#   sudo ./uninstall.sh [OPTIONS]
#
# Options:
#   --silent       Non-interactive uninstallation
#   --keep-data    Preserve application data directory
#   --keep-logs    Preserve log files
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
    INSTALL_DIR="/usr/local/greylegacy"
    TOMCAT_DIR="/usr/local/greylegacy/tomcat"
    CONF_DIR="/usr/local/greylegacy/conf"
    DATA_DIR="/usr/local/greylegacy/data"
    LOG_DIR="/usr/local/greylegacy/logs"
    SERVICE_LABEL="com.greylegacy.tomcat"
    APP_PORT=8080
fi

LAUNCH_DAEMON_DIR="/Library/LaunchDaemons"
PLIST_FILE="${LAUNCH_DAEMON_DIR}/${SERVICE_LABEL}.plist"

# ---------------------------------------------------------------------------
#  Parse Arguments
# ---------------------------------------------------------------------------
while [[ $# -gt 0 ]]; do
    case "$1" in
        --silent)    SILENT=true;    shift ;;
        --keep-data) KEEP_DATA=true; shift ;;
        --keep-logs) KEEP_LOGS=true; shift ;;
        *)           shift ;;
    esac
done

# ---------------------------------------------------------------------------
#  Check Root Privileges
# ---------------------------------------------------------------------------
if [[ $EUID -ne 0 ]]; then
    log_error "This uninstaller must be run with sudo."
    exit 1
fi

# ---------------------------------------------------------------------------
#  Check macOS
# ---------------------------------------------------------------------------
if [[ "$(uname)" != "Darwin" ]]; then
    log_error "This uninstaller is for macOS only."
    exit 1
fi

# ---------------------------------------------------------------------------
#  Banner
# ---------------------------------------------------------------------------
echo ""
echo "  ==========================================================="
echo "   ${APP_NAME} - macOS Uninstaller"
echo "  ==========================================================="
echo ""
echo "   Install Directory : ${INSTALL_DIR}"
echo "   Service Label     : ${SERVICE_LABEL}"
echo "   Keep Data         : ${KEEP_DATA}"
echo "   Keep Logs         : ${KEEP_LOGS}"
echo ""

if [[ "${SILENT}" == "false" ]]; then
    read -rp "Are you sure you want to uninstall? [y/N]: " CONFIRM
    if [[ "${CONFIRM}" != "y" && "${CONFIRM}" != "Y" ]]; then
        echo "Uninstallation cancelled by user."
        exit 0
    fi
fi

# ---------------------------------------------------------------------------
#  Step 1: Unload and Remove LaunchDaemon
# ---------------------------------------------------------------------------
log_info "[1/3] Stopping service..."

if [[ -f "${PLIST_FILE}" ]]; then
    # Check if loaded
    if launchctl list "${SERVICE_LABEL}" &>/dev/null; then
        log_info "Unloading LaunchDaemon '${SERVICE_LABEL}'..."
        launchctl unload "${PLIST_FILE}" 2>/dev/null || true
        sleep 3
    fi

    rm -f "${PLIST_FILE}"
    log_ok "LaunchDaemon removed."
else
    log_info "No LaunchDaemon plist found. Skipping."

    # Try to kill any running Tomcat processes for this install
    if [[ -f "${TOMCAT_DIR}/bin/shutdown.sh" ]]; then
        "${TOMCAT_DIR}/bin/shutdown.sh" 2>/dev/null || true
        sleep 3
    fi
fi

# ---------------------------------------------------------------------------
#  Step 2: Remove Application Files
# ---------------------------------------------------------------------------
log_info "[2/3] Removing application files..."

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

    # Conditionally remove logs
    if [[ "${KEEP_LOGS}" == "false" ]]; then
        if [[ -d "${LOG_DIR}" ]]; then
            rm -rf "${LOG_DIR}"
            log_info "Logs removed."
        fi
    else
        log_info "Keeping logs directory: ${LOG_DIR}"
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

# ---------------------------------------------------------------------------
#  Step 3: Cleanup
# ---------------------------------------------------------------------------
log_info "[3/3] Final cleanup..."

# Remove any temp files
rm -f /tmp/${APP_SHORT}-*.tmp 2>/dev/null

# Remove Application bundle
if [[ -d "/Applications/Grey Legacy.app" ]]; then
    rm -rf "/Applications/Grey Legacy.app"
    log_info "Application bundle removed."
fi

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
echo ""
echo "   ${APP_NAME} has been removed from this system."
echo "  ==========================================================="
echo ""

exit 0
