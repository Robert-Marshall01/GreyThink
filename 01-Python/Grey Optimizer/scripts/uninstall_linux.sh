#!/bin/bash
# ═══════════════════════════════════════════════════════════════════════════════
# Grey Optimizer - Linux Uninstaller
# ═══════════════════════════════════════════════════════════════════════════════
#
# Removes Grey Optimizer systemd service from Linux systems.
#
# Usage:
#   sudo ./uninstall_linux.sh                  # Uninstall (keep data)
#   sudo ./uninstall_linux.sh --purge          # Remove everything including data
#
# ═══════════════════════════════════════════════════════════════════════════════

set -euo pipefail

VERSION="2.0.0"

# Paths
INSTALL_DIR="/opt/grey-optimizer"
CONFIG_DIR="/etc/grey-optimizer"
DATA_DIR="/var/lib/grey-optimizer"
LOG_FILE="/var/log/grey-optimizer.log"
SERVICE_NAME="grey-optimizer"
SERVICE_FILE="/etc/systemd/system/${SERVICE_NAME}.service"

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
            echo "  --help     Show this help"
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

# ─────────────────────────────────────────────────────────────────────────────
# Uninstall
# ─────────────────────────────────────────────────────────────────────────────

echo ""
echo -e "${BOLD}Grey Optimizer Linux Uninstaller v${VERSION}${NC}"
echo ""

# Stop service
if systemctl is-active --quiet "$SERVICE_NAME" 2>/dev/null; then
    log_info "Stopping service..."
    systemctl stop "$SERVICE_NAME"
    log_success "Service stopped"
else
    log_info "Service is not running"
fi

# Disable service
if systemctl is-enabled --quiet "$SERVICE_NAME" 2>/dev/null; then
    log_info "Disabling service..."
    systemctl disable "$SERVICE_NAME"
    log_success "Service disabled"
else
    log_info "Service is not enabled"
fi

# Remove service file
if [[ -f "$SERVICE_FILE" ]]; then
    log_info "Removing service file..."
    rm -f "$SERVICE_FILE"
    systemctl daemon-reload
    log_success "Service file removed"
else
    log_info "Service file not found"
fi

# Remove installation directory
if [[ -d "$INSTALL_DIR" ]]; then
    log_info "Removing installation directory..."
    rm -rf "$INSTALL_DIR"
    log_success "Removed $INSTALL_DIR"
else
    log_info "Installation directory not found"
fi

# Remove config directory
if [[ -d "$CONFIG_DIR" ]]; then
    log_info "Removing configuration..."
    rm -rf "$CONFIG_DIR"
    log_success "Removed $CONFIG_DIR"
else
    log_info "Configuration directory not found"
fi

# Handle data and logs
if [[ "$PURGE" == "true" ]]; then
    log_warn "Purging all data (--purge specified)..."
    
    if [[ -d "$DATA_DIR" ]]; then
        rm -rf "$DATA_DIR"
        log_success "Removed $DATA_DIR"
    fi
    
    if [[ -f "$LOG_FILE" ]]; then
        rm -f "$LOG_FILE"
        log_success "Removed $LOG_FILE"
    fi
else
    echo ""
    log_warn "Data preserved for audit purposes:"
    [[ -d "$DATA_DIR" ]] && echo "  - $DATA_DIR"
    [[ -f "$LOG_FILE" ]] && echo "  - $LOG_FILE"
    echo ""
    echo "To remove all data, run: sudo $0 --purge"
fi

echo ""
echo -e "${GREEN}════════════════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}  Grey Optimizer uninstalled successfully${NC}"
echo -e "${GREEN}════════════════════════════════════════════════════════════════${NC}"
echo ""
