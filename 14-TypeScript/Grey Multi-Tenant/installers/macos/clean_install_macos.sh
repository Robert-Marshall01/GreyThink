#!/bin/bash
#
# Grey Multi-Tenant Platform - macOS Clean Install
#
# Performs a complete uninstall followed by a fresh install.
# This ensures no leftover files or configurations interfere with the new installation.
#
# Usage:
#   sudo ./clean_install_macos.sh
#   sudo ./clean_install_macos.sh --keep-data
#   ./clean_install_macos.sh --user-install
#
# Options:
#   --keep-data        Preserve configuration and logs
#   --user-install     User-level installation (no sudo required)
#   --skip-service     Skip launchd service registration
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
APP_BUNDLE_NAME="GreyMultiTenant.app"
SERVICE_NAME="com.grey.multitenant"
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
    INSTALL_DIR="$HOME/Applications/$APP_BUNDLE_NAME"
    BIN_DIR="$HOME/.local/bin"
    CONFIG_DIR="$HOME/.config/grey-multitenant"
    LOG_DIR="$HOME/Library/Logs/GreyMultiTenant"
    LAUNCHD_DIR="$HOME/Library/LaunchAgents"
else
    INSTALL_DIR="/Applications/$APP_BUNDLE_NAME"
    BIN_DIR="$PREFIX/bin"
    CONFIG_DIR="/etc/grey-multitenant"
    LOG_DIR="/var/log/grey-multitenant"
    LAUNCHD_DIR="/Library/LaunchDaemons"
fi

PLIST_NAME="$SERVICE_NAME.plist"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo -e "${CYAN}============================================${NC}"
echo -e "${CYAN}  Grey Multi-Tenant Platform${NC}"
echo -e "${CYAN}  Clean Install - macOS${NC}"
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

# Unload and remove launchd service
if [ -f "$LAUNCHD_DIR/$PLIST_NAME" ]; then
    echo -e "${YELLOW}Stopping launchd service...${NC}"
    launchctl unload "$LAUNCHD_DIR/$PLIST_NAME" 2>/dev/null || true
    rm -f "$LAUNCHD_DIR/$PLIST_NAME"
    echo -e "${GREEN}  Service removed.${NC}"
fi

# Kill any running processes
pkill -f "$BINARY_NAME" 2>/dev/null || true
sleep 1

# Remove app bundle
if [ -d "$INSTALL_DIR" ]; then
    echo -e "${YELLOW}Removing app bundle...${NC}"
    rm -rf "$INSTALL_DIR"
    echo -e "${GREEN}  App bundle removed.${NC}"
fi

# Remove binaries
rm -f "$BIN_DIR/$BINARY_NAME"
rm -f "$BIN_DIR/grey-launcher"
rm -f "$BIN_DIR/grey-uninstall"

# Remove data if not preserving
if [ "$KEEP_DATA" = false ]; then
    echo -e "${YELLOW}Removing configuration and data...${NC}"
    rm -rf "$CONFIG_DIR"
    rm -rf "$LOG_DIR"
    echo -e "${GREEN}  Data removed.${NC}"
fi

# Remove service user (system install only)
if [ "$USER_INSTALL" = false ]; then
    if dscl . -read /Users/_grey &>/dev/null; then
        dscl . -delete /Users/_grey 2>/dev/null || true
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

# Check app bundle
if [ -d "$INSTALL_DIR" ]; then
    echo -e "${GREEN}App bundle installed: OK${NC}"
else
    echo -e "${RED}App bundle not found!${NC}"
fi

# Check binary
if [ -x "$BIN_DIR/$BINARY_NAME" ]; then
    echo -e "${GREEN}Binary symlink: OK${NC}"
else
    echo -e "${YELLOW}Binary symlink not found${NC}"
fi

# Check config
if [ -f "$CONFIG_DIR/.env" ]; then
    echo -e "${GREEN}Configuration: OK${NC}"
else
    echo -e "${RED}Configuration not found!${NC}"
fi

# Check launchd service (if not skipped)
if [ "$SKIP_SERVICE" = false ]; then
    if [ -f "$LAUNCHD_DIR/$PLIST_NAME" ]; then
        echo -e "${GREEN}Launchd service registered: OK${NC}"
        # Try to start the service
        if [ "$USER_INSTALL" = true ]; then
            launchctl start "$SERVICE_NAME" 2>/dev/null
        else
            launchctl start "$SERVICE_NAME" 2>/dev/null
        fi
        sleep 2
        if launchctl list | grep -q "$SERVICE_NAME"; then
            echo -e "${GREEN}Service loaded: OK${NC}"
        else
            echo -e "${YELLOW}Service not running (may need PostgreSQL)${NC}"
        fi
    fi
fi

echo ""
echo -e "${GREEN}============================================${NC}"
echo -e "${GREEN}  Clean Install Complete!${NC}"
echo -e "${GREEN}============================================${NC}"
echo ""

