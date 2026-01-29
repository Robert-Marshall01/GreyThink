#!/bin/bash
#
# Grey Multi-Tenant Platform - Linux Clean Install
#
# Performs a complete uninstall followed by a fresh install.
# This ensures no leftover files or configurations interfere with the new installation.
#
# Usage:
#   sudo ./clean_install_linux.sh
#   sudo ./clean_install_linux.sh --keep-data
#   ./clean_install_linux.sh --user-install
#
# Options:
#   --keep-data        Preserve configuration and logs
#   --user-install     User-level installation (no sudo required)
#   --skip-service     Skip systemd service registration
#   --force            Skip confirmation prompt
#

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m'

# Configuration
APP_ID="grey-multitenant"
SERVICE_NAME="grey-multitenant"
BINARY_NAME="grey-core-api"

# Default options
USER_INSTALL=false
KEEP_DATA=false
SKIP_SERVICE=false
FORCE=false
PREFIX="/usr/local"

# Parse arguments
for arg in "$@"; do
    case $arg in
        --keep-data)
            KEEP_DATA=true
            shift
            ;;
        --user-install)
            USER_INSTALL=true
            shift
            ;;
        --skip-service)
            SKIP_SERVICE=true
            shift
            ;;
        --force)
            FORCE=true
            shift
            ;;
        --prefix=*)
            PREFIX="${arg#*=}"
            shift
            ;;
        *)
            ;;
    esac
done

# Set paths
if [ "$USER_INSTALL" = true ]; then
    BIN_DIR="$HOME/.local/bin"
    CONFIG_DIR="$HOME/.config/grey-multitenant"
    LOG_DIR="$HOME/.local/share/grey-multitenant/logs"
    DATA_DIR="$HOME/.local/share/grey-multitenant"
    SYSTEMD_DIR="$HOME/.config/systemd/user"
    DESKTOP_DIR="$HOME/.local/share/applications"
else
    BIN_DIR="$PREFIX/bin"
    CONFIG_DIR="/etc/grey-multitenant"
    LOG_DIR="/var/log/grey-multitenant"
    DATA_DIR="/var/lib/grey-multitenant"
    SYSTEMD_DIR="/etc/systemd/system"
    DESKTOP_DIR="/usr/share/applications"
fi

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo -e "${CYAN}============================================${NC}"
echo -e "${CYAN}  Grey Multi-Tenant Platform${NC}"
echo -e "${CYAN}  Clean Install - Linux${NC}"
echo -e "${CYAN}============================================${NC}"
echo ""

# Check for root if not user install
if [ "$USER_INSTALL" = false ] && [ "$EUID" -ne 0 ]; then
    echo -e "${RED}ERROR: This script must be run with sudo.${NC}"
    echo "       Or use --user-install for user-level installation."
    exit 1
fi

# Confirm
if [ "$FORCE" = false ]; then
    echo -e "${YELLOW}This will UNINSTALL and REINSTALL Grey Multi-Tenant Platform.${NC}"
    if [ "$KEEP_DATA" = false ]; then
        echo -e "${RED}WARNING: All configuration and log data will be deleted!${NC}"
    else
        echo -e "${GREEN}Configuration and logs will be preserved.${NC}"
    fi
    echo ""
    read -p "Are you sure you want to continue? (yes/no): " confirmation
    if [ "$confirmation" != "yes" ]; then
        echo -e "${YELLOW}Clean install cancelled.${NC}"
        exit 0
    fi
fi

# Phase 1: Uninstall
echo ""
echo -e "${MAGENTA}========== PHASE 1: UNINSTALL ==========${NC}"
echo ""

# Stop and disable service
if [ "$USER_INSTALL" = true ]; then
    if systemctl --user is-active --quiet "$SERVICE_NAME" 2>/dev/null; then
        echo -e "${YELLOW}Stopping user service...${NC}"
        systemctl --user stop "$SERVICE_NAME" 2>/dev/null || true
    fi
    systemctl --user disable "$SERVICE_NAME" 2>/dev/null || true
    rm -f "$SYSTEMD_DIR/$SERVICE_NAME.service"
    systemctl --user daemon-reload 2>/dev/null || true
else
    if systemctl is-active --quiet "$SERVICE_NAME" 2>/dev/null; then
        echo -e "${YELLOW}Stopping system service...${NC}"
        systemctl stop "$SERVICE_NAME" 2>/dev/null || true
    fi
    systemctl disable "$SERVICE_NAME" 2>/dev/null || true
    rm -f "$SYSTEMD_DIR/$SERVICE_NAME.service"
    systemctl daemon-reload 2>/dev/null || true
fi

# Kill any running processes
pkill -f "$BINARY_NAME" 2>/dev/null || true
sleep 1

# Remove binaries
rm -f "$BIN_DIR/$BINARY_NAME"
rm -f "$BIN_DIR/grey-launcher"
rm -f "$BIN_DIR/grey-uninstall"

# Remove desktop file
rm -f "$DESKTOP_DIR/$APP_ID.desktop"

# Remove data if not preserving
if [ "$KEEP_DATA" = false ]; then
    echo -e "${YELLOW}Removing configuration and data...${NC}"
    rm -rf "$CONFIG_DIR"
    rm -rf "$LOG_DIR"
    rm -rf "$DATA_DIR"
    echo -e "${GREEN}  Data removed.${NC}"
fi

# Remove service user (system install only)
if [ "$USER_INSTALL" = false ]; then
    if id -u grey &>/dev/null; then
        userdel grey 2>/dev/null || true
    fi
fi

echo ""
echo -e "${GREEN}Uninstall complete.${NC}"
echo ""

# Phase 2: Fresh Install
echo -e "${MAGENTA}========== PHASE 2: INSTALL ==========${NC}"
echo ""

# Find install script
INSTALL_SCRIPT="$SCRIPT_DIR/install.sh"

if [ ! -f "$INSTALL_SCRIPT" ]; then
    echo -e "${RED}ERROR: Install script not found: $INSTALL_SCRIPT${NC}"
    exit 1
fi

# Build install arguments
INSTALL_ARGS=""
if [ "$USER_INSTALL" = true ]; then
    INSTALL_ARGS="$INSTALL_ARGS --user-install"
fi
if [ "$SKIP_SERVICE" = true ]; then
    INSTALL_ARGS="$INSTALL_ARGS --skip-service"
fi
INSTALL_ARGS="$INSTALL_ARGS --prefix=$PREFIX"

# Run install
bash "$INSTALL_SCRIPT" $INSTALL_ARGS

# Verification
echo ""
echo -e "${MAGENTA}========== VERIFICATION ===========${NC}"
echo ""

# Check binary
if [ -x "$BIN_DIR/$BINARY_NAME" ]; then
    echo -e "${GREEN}Binary installed: OK${NC}"
else
    echo -e "${RED}Binary not found!${NC}"
fi

# Check config
if [ -f "$CONFIG_DIR/.env" ]; then
    echo -e "${GREEN}Configuration: OK${NC}"
else
    echo -e "${RED}Configuration not found!${NC}"
fi

# Check service (if not skipped)
if [ "$SKIP_SERVICE" = false ]; then
    if [ "$USER_INSTALL" = true ]; then
        if systemctl --user is-enabled "$SERVICE_NAME" &>/dev/null; then
            echo -e "${GREEN}Service registered: OK${NC}"
            systemctl --user start "$SERVICE_NAME" 2>/dev/null
            sleep 2
            if systemctl --user is-active --quiet "$SERVICE_NAME" 2>/dev/null; then
                echo -e "${GREEN}Service running: OK${NC}"
            else
                echo -e "${YELLOW}Service not running (may need PostgreSQL)${NC}"
            fi
        fi
    else
        if systemctl is-enabled "$SERVICE_NAME" &>/dev/null; then
            echo -e "${GREEN}Service registered: OK${NC}"
            systemctl start "$SERVICE_NAME" 2>/dev/null
            sleep 2
            if systemctl is-active --quiet "$SERVICE_NAME" 2>/dev/null; then
                echo -e "${GREEN}Service running: OK${NC}"
            else
                echo -e "${YELLOW}Service not running (may need PostgreSQL)${NC}"
            fi
        fi
    fi
fi

echo ""
echo -e "${GREEN}============================================${NC}"
echo -e "${GREEN}  Clean Install Complete!${NC}"
echo -e "${GREEN}============================================${NC}"
echo ""

