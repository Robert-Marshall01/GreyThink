#!/bin/bash
#
# Grey Multi-Tenant Platform - macOS Unified Uninstaller
#
# Removes both backend and GUI installations.
#
# Usage:
#   sudo ./uninstall.sh          # Uninstall everything
#   sudo ./uninstall.sh --keep-data  # Keep config and logs
#   ./uninstall.sh --gui-only    # Only remove GUI
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
APP_NAME="Grey Multi-Tenant"
APP_BUNDLE_NAME="GreyMultiTenant.app"
GUI_BUNDLE_NAME="Grey Multi-Tenant GUI.app"
SERVICE_NAME="com.grey.multitenant"
BINARY_NAME="grey-core-api"

# Parse arguments
KEEP_DATA=false
BACKEND_ONLY=false
GUI_ONLY=false
USER_INSTALL=false
PREFIX="/usr/local"

for arg in "$@"; do
    case $arg in
        --keep-data) KEEP_DATA=true ;;
        --backend-only) BACKEND_ONLY=true ;;
        --gui-only) GUI_ONLY=true ;;
        --user-install) USER_INSTALL=true ;;
        --prefix=*) PREFIX="${arg#*=}" ;;
    esac
done

# Set paths
if [ "$USER_INSTALL" = true ]; then
    BACKEND_INSTALL_DIR="$HOME/Applications/$APP_BUNDLE_NAME"
    GUI_INSTALL_DIR="$HOME/Applications/$GUI_BUNDLE_NAME"
    BIN_DIR="$HOME/.local/bin"
    CONFIG_DIR="$HOME/.config/grey-multitenant"
    LOG_DIR="$HOME/Library/Logs/GreyMultiTenant"
    GUI_DATA_DIR="$HOME/Library/Application Support/Grey Multi-Tenant GUI"
    LAUNCHD_DIR="$HOME/Library/LaunchAgents"
else
    BACKEND_INSTALL_DIR="/Applications/$APP_BUNDLE_NAME"
    GUI_INSTALL_DIR="/Applications/$GUI_BUNDLE_NAME"
    BIN_DIR="$PREFIX/bin"
    CONFIG_DIR="/etc/grey-multitenant"
    LOG_DIR="/var/log/grey-multitenant"
    GUI_DATA_DIR="$HOME/Library/Application Support/Grey Multi-Tenant GUI"
    LAUNCHD_DIR="/Library/LaunchDaemons"
fi

# Determine what to uninstall
UNINSTALL_BACKEND=true
UNINSTALL_GUI=true
[ "$GUI_ONLY" = true ] && UNINSTALL_BACKEND=false
[ "$BACKEND_ONLY" = true ] && UNINSTALL_GUI=false

echo -e "${CYAN}============================================${NC}"
echo -e "${CYAN}  Grey Multi-Tenant Platform Uninstaller${NC}"
echo -e "${CYAN}  macOS Unified Edition${NC}"
echo -e "${CYAN}============================================${NC}"
echo ""

# Check privileges for backend
if [ "$UNINSTALL_BACKEND" = true ] && [ "$USER_INSTALL" = false ] && [ "$EUID" -ne 0 ]; then
    echo -e "${RED}ERROR: Backend uninstall requires sudo.${NC}"
    echo "       Use --user-install or --gui-only"
    exit 1
fi

# Confirmation
echo -e "${YELLOW}This will remove:${NC}"
[ "$UNINSTALL_BACKEND" = true ] && echo "  - Backend service and app bundle"
[ "$UNINSTALL_GUI" = true ] && echo "  - GUI application"
[ "$KEEP_DATA" = false ] && echo -e "  ${RED}- All configuration and logs${NC}"
echo ""
read -p "Continue? (yes/no): " confirm
[ "$confirm" != "yes" ] && echo "Cancelled." && exit 0
echo ""

# ============================================
# BACKEND UNINSTALL
# ============================================
if [ "$UNINSTALL_BACKEND" = true ]; then
    echo -e "${MAGENTA}--- Removing Backend ---${NC}"
    
    echo -e "${YELLOW}[1] Stopping launchd service...${NC}"
    if [ -f "$LAUNCHD_DIR/$SERVICE_NAME.plist" ]; then
        launchctl unload "$LAUNCHD_DIR/$SERVICE_NAME.plist" 2>/dev/null || true
        rm -f "$LAUNCHD_DIR/$SERVICE_NAME.plist"
        echo -e "${GREEN}  Service removed.${NC}"
    else
        echo "  Service not found."
    fi

    echo -e "${YELLOW}[2] Removing app bundle...${NC}"
    if [ -d "$BACKEND_INSTALL_DIR" ]; then
        rm -rf "$BACKEND_INSTALL_DIR"
        echo -e "${GREEN}  App bundle removed.${NC}"
    else
        echo "  Not found."
    fi

    echo -e "${YELLOW}[3] Removing binaries...${NC}"
    rm -f "$BIN_DIR/$BINARY_NAME" 2>/dev/null || true
    rm -f "$BIN_DIR/grey-launcher" 2>/dev/null || true
    echo -e "${GREEN}  Binaries removed.${NC}"

    if [ "$KEEP_DATA" = false ]; then
        echo -e "${YELLOW}[4] Removing data...${NC}"
        rm -rf "$CONFIG_DIR" 2>/dev/null || true
        rm -rf "$LOG_DIR" 2>/dev/null || true
        # Remove service user if system install
        if [ "$USER_INSTALL" = false ]; then
            if dscl . -read /Users/_grey &>/dev/null 2>&1; then
                echo "  Removing service user _grey..."
                dscl . -delete /Users/_grey 2>/dev/null || true
            fi
        fi
        echo -e "${GREEN}  Data removed.${NC}"
    else
        echo -e "${YELLOW}[4] Data preserved.${NC}"
    fi
fi

# ============================================
# GUI UNINSTALL
# ============================================
if [ "$UNINSTALL_GUI" = true ]; then
    echo ""
    echo -e "${MAGENTA}--- Removing GUI ---${NC}"
    
    echo -e "${YELLOW}[1] Removing GUI app...${NC}"
    if [ -d "$GUI_INSTALL_DIR" ]; then
        rm -rf "$GUI_INSTALL_DIR"
        echo -e "${GREEN}  GUI removed.${NC}"
    else
        echo "  Not found."
    fi

    echo -e "${YELLOW}[2] Removing GUI launcher...${NC}"
    rm -f "$BIN_DIR/grey-gui" 2>/dev/null || true
    echo -e "${GREEN}  Launcher removed.${NC}"

    if [ "$KEEP_DATA" = false ]; then
        echo -e "${YELLOW}[3] Removing GUI data...${NC}"
        rm -rf "$GUI_DATA_DIR" 2>/dev/null || true
        echo -e "${GREEN}  Data removed.${NC}"
    fi
fi

# Cleanup
rm -f "$BIN_DIR/grey-uninstall" 2>/dev/null || true

# Summary
echo ""
echo -e "${GREEN}============================================${NC}"
echo -e "${GREEN}  Uninstallation Complete!${NC}"
echo -e "${GREEN}============================================${NC}"
echo ""
[ "$UNINSTALL_BACKEND" = true ] && echo "Removed: Backend service and files"
[ "$UNINSTALL_GUI" = true ] && echo "Removed: GUI application"
if [ "$KEEP_DATA" = true ]; then
    echo ""
    echo -e "${YELLOW}Data preserved at:${NC}"
    [ "$UNINSTALL_BACKEND" = true ] && echo "  $CONFIG_DIR"
fi
echo ""
