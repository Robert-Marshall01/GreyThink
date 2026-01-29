#!/bin/bash
#
# Grey Multi-Tenant Platform - Linux Unified Uninstaller
#
# Removes both backend and GUI installations.
#
# Usage:
#   sudo ./uninstall.sh           # Uninstall everything
#   sudo ./uninstall.sh --keep-data   # Keep config and logs
#   ./uninstall.sh --gui-only --user-install  # GUI only
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
GUI_APP_ID="grey-multitenant-gui"
SERVICE_NAME="grey-multitenant"
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
    BIN_DIR="$HOME/.local/bin"
    CONFIG_DIR="$HOME/.config/grey-multitenant"
    LOG_DIR="$HOME/.local/share/grey-multitenant/logs"
    DATA_DIR="$HOME/.local/share/grey-multitenant"
    SYSTEMD_DIR="$HOME/.config/systemd/user"
    DESKTOP_DIR="$HOME/.local/share/applications"
    ICON_DIR="$HOME/.local/share/icons/hicolor/256x256/apps"
    GUI_INSTALL_DIR="$HOME/.local/opt/grey-multitenant-gui"
    GUI_DATA_DIR="$HOME/.local/share/grey-multitenant-gui"
else
    BIN_DIR="$PREFIX/bin"
    CONFIG_DIR="/etc/grey-multitenant"
    LOG_DIR="/var/log/grey-multitenant"
    DATA_DIR="/var/lib/grey-multitenant"
    SYSTEMD_DIR="/etc/systemd/system"
    DESKTOP_DIR="/usr/share/applications"
    ICON_DIR="/usr/share/icons/hicolor/256x256/apps"
    GUI_INSTALL_DIR="/opt/grey-multitenant-gui"
    GUI_DATA_DIR="/var/lib/grey-multitenant-gui"
fi

# Determine what to uninstall
UNINSTALL_BACKEND=true
UNINSTALL_GUI=true
[ "$GUI_ONLY" = true ] && UNINSTALL_BACKEND=false
[ "$BACKEND_ONLY" = true ] && UNINSTALL_GUI=false

echo -e "${CYAN}============================================${NC}"
echo -e "${CYAN}  Grey Multi-Tenant Platform Uninstaller${NC}"
echo -e "${CYAN}  Linux Unified Edition${NC}"
echo -e "${CYAN}============================================${NC}"
echo ""

# Check privileges
if [ "$UNINSTALL_BACKEND" = true ] && [ "$USER_INSTALL" = false ] && [ "$EUID" -ne 0 ]; then
    echo -e "${RED}ERROR: Backend uninstall requires sudo.${NC}"
    echo "       Use --user-install or --gui-only"
    exit 1
fi

# Confirmation
echo -e "${YELLOW}This will remove:${NC}"
[ "$UNINSTALL_BACKEND" = true ] && echo "  - Backend service and files"
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
    
    echo -e "${YELLOW}[1] Stopping systemd service...${NC}"
    if [ -f "$SYSTEMD_DIR/$SERVICE_NAME.service" ]; then
        if [ "$USER_INSTALL" = true ]; then
            systemctl --user stop "$SERVICE_NAME.service" 2>/dev/null || true
            systemctl --user disable "$SERVICE_NAME.service" 2>/dev/null || true
            rm -f "$SYSTEMD_DIR/$SERVICE_NAME.service"
            systemctl --user daemon-reload
        else
            systemctl stop "$SERVICE_NAME.service" 2>/dev/null || true
            systemctl disable "$SERVICE_NAME.service" 2>/dev/null || true
            rm -f "$SYSTEMD_DIR/$SERVICE_NAME.service"
            systemctl daemon-reload
        fi
        echo -e "${GREEN}  Service removed.${NC}"
    else
        echo "  Service not found."
    fi

    echo -e "${YELLOW}[2] Removing binary...${NC}"
    rm -f "$BIN_DIR/$BINARY_NAME" 2>/dev/null || true
    rm -f "$BIN_DIR/grey-launcher" 2>/dev/null || true
    echo -e "${GREEN}  Binary removed.${NC}"

    echo -e "${YELLOW}[3] Removing desktop entry...${NC}"
    rm -f "$DESKTOP_DIR/$APP_ID.desktop" 2>/dev/null || true
    rm -f "$DESKTOP_DIR/${APP_ID}-gui.desktop" 2>/dev/null || true
    rm -f "$ICON_DIR/$APP_ID.png" 2>/dev/null || true
    command -v update-desktop-database &>/dev/null && update-desktop-database "$DESKTOP_DIR" 2>/dev/null || true
    command -v gtk-update-icon-cache &>/dev/null && gtk-update-icon-cache -f "$(dirname "$ICON_DIR")" 2>/dev/null || true
    echo -e "${GREEN}  Desktop entry removed.${NC}"

    if [ "$KEEP_DATA" = false ]; then
        echo -e "${YELLOW}[4] Removing data...${NC}"
        rm -rf "$CONFIG_DIR" 2>/dev/null || true
        rm -rf "$LOG_DIR" 2>/dev/null || true
        rm -rf "$DATA_DIR" 2>/dev/null || true
        # Remove service user
        [ "$USER_INSTALL" = false ] && id -u grey &>/dev/null && userdel grey 2>/dev/null || true
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
    
    echo -e "${YELLOW}[1] Removing GUI files...${NC}"
    rm -rf "$GUI_INSTALL_DIR" 2>/dev/null || true
    echo -e "${GREEN}  GUI removed.${NC}"

    echo -e "${YELLOW}[2] Removing GUI launcher...${NC}"
    rm -f "$BIN_DIR/grey-gui" 2>/dev/null || true
    echo -e "${GREEN}  Launcher removed.${NC}"

    echo -e "${YELLOW}[3] Removing GUI desktop entry...${NC}"
    rm -f "$DESKTOP_DIR/$GUI_APP_ID.desktop" 2>/dev/null || true
    command -v update-desktop-database &>/dev/null && update-desktop-database "$DESKTOP_DIR" 2>/dev/null || true
    echo -e "${GREEN}  Desktop entry removed.${NC}"

    if [ "$KEEP_DATA" = false ]; then
        echo -e "${YELLOW}[4] Removing GUI data...${NC}"
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
