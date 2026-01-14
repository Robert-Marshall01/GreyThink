#!/bin/bash
# ═══════════════════════════════════════════════════════════════════════════════
# Grey Optimizer - macOS Uninstaller
# ═══════════════════════════════════════════════════════════════════════════════
#
# Removes Grey Optimizer launchd service from macOS.
#
# Usage:
#   sudo ./uninstall_macos.sh                  # Uninstall (keep data)
#   sudo ./uninstall_macos.sh --purge          # Remove everything
#
# ═══════════════════════════════════════════════════════════════════════════════

set -euo pipefail

VERSION="2.0.0"

# Paths
INSTALL_DIR="/opt/grey-optimizer"
CONFIG_DIR="/etc/grey-optimizer"
DATA_DIR="/var/lib/grey-optimizer"
LOG_DIR="/var/log/grey-optimizer"
SERVICE_LABEL="com.grey.optimizer"
PLIST_FILE="/Library/LaunchDaemons/${SERVICE_LABEL}.plist"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

log_info()    { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[OK]${NC} $1"; }
log_warn()    { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error()   { echo -e "${RED}[ERROR]${NC} $1" >&2; }

# ─────────────────────────────────────────────────────────────────────────────
# Argument Parsing
# ─────────────────────────────────────────────────────────────────────────────

PURGE=false

while [[ $# -gt 0 ]]; do
    case "$1" in
        --purge)
            PURGE=true
            shift
            ;;
        --help|-h)
            echo "Usage: sudo $0 [--purge]"
            echo ""
            echo "Options:"
            echo "  --purge    Remove all data including logs and audit database"
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            exit 1
            ;;
    esac
done

# ─────────────────────────────────────────────────────────────────────────────
# Preflight
# ─────────────────────────────────────────────────────────────────────────────

if [[ $EUID -ne 0 ]]; then
    log_error "This script must be run as root (use sudo)"
    exit 1
fi

if [[ "$(uname)" != "Darwin" ]]; then
    log_error "This uninstaller is for macOS only"
    exit 1
fi

# ─────────────────────────────────────────────────────────────────────────────
# Uninstall
# ─────────────────────────────────────────────────────────────────────────────

echo ""
echo -e "${BOLD}Grey Optimizer macOS Uninstaller v${VERSION}${NC}"
echo ""

# Unload service
if launchctl list | grep -q "$SERVICE_LABEL" 2>/dev/null; then
    log_info "Unloading service..."
    launchctl bootout system "$PLIST_FILE" 2>/dev/null || \
    launchctl unload "$PLIST_FILE" 2>/dev/null || true
    log_success "Service unloaded"
else
    log_info "Service is not loaded"
fi

# Remove plist
if [[ -f "$PLIST_FILE" ]]; then
    log_info "Removing plist..."
    rm -f "$PLIST_FILE"
    log_success "Plist removed"
else
    log_info "Plist not found"
fi

# Remove installation directory
if [[ -d "$INSTALL_DIR" ]]; then
    log_info "Removing installation directory..."
    rm -rf "$INSTALL_DIR"
    log_success "Removed $INSTALL_DIR"
else
    log_info "Installation directory not found"
fi

# Remove config
if [[ -d "$CONFIG_DIR" ]]; then
    rm -rf "$CONFIG_DIR"
    log_success "Removed $CONFIG_DIR"
fi

# Handle data and logs
if [[ "$PURGE" == "true" ]]; then
    log_warn "Purging all data..."
    
    [[ -d "$DATA_DIR" ]] && rm -rf "$DATA_DIR" && log_success "Removed $DATA_DIR"
    [[ -d "$LOG_DIR" ]] && rm -rf "$LOG_DIR" && log_success "Removed $LOG_DIR"
else
    echo ""
    log_warn "Data preserved for audit purposes:"
    [[ -d "$DATA_DIR" ]] && echo "  - $DATA_DIR"
    [[ -d "$LOG_DIR" ]] && echo "  - $LOG_DIR"
    echo ""
    echo "To remove all data, run: sudo $0 --purge"
fi

echo ""
echo -e "${GREEN}════════════════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}  Grey Optimizer uninstalled successfully${NC}"
echo -e "${GREEN}════════════════════════════════════════════════════════════════${NC}"
echo ""
