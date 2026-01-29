#!/bin/bash
#
# Grey Multi-Tenant Platform - Linux Unified Installer
#
# Installs both the backend service (systemd) and GUI desktop application.
#
# Usage:
#   sudo ./install.sh            # Install both backend and GUI
#   sudo ./install.sh --skip-gui # Install backend only
#   ./install.sh --skip-backend --user-install  # GUI only (no sudo)
#
# Options:
#   --skip-backend     Skip backend installation
#   --skip-gui         Skip GUI installation
#   --skip-service     Install backend without systemd service
#   --user-install     Install for current user only
#   --prefix=/path     Installation prefix (default: /usr/local)
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
APP_ID="grey-multitenant"
GUI_APP_ID="grey-multitenant-gui"
SERVICE_NAME="grey-multitenant"
BINARY_NAME="grey-core-api"

# Parse arguments
SKIP_BACKEND=false
SKIP_GUI=false
SKIP_SERVICE=false
USER_INSTALL=false
PREFIX="/usr/local"

for arg in "$@"; do
    case $arg in
        --skip-backend) SKIP_BACKEND=true ;;
        --skip-gui) SKIP_GUI=true ;;
        --skip-service) SKIP_SERVICE=true ;;
        --user-install) USER_INSTALL=true ;;
        --prefix=*) PREFIX="${arg#*=}" ;;
    esac
done

# Set paths based on install type
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

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SOURCE_DIR="$(dirname "$(dirname "$SCRIPT_DIR")")"
GUI_DIR="$SOURCE_DIR/gui"

echo -e "${CYAN}============================================${NC}"
echo -e "${CYAN}  Grey Multi-Tenant Platform Installer${NC}"
echo -e "${CYAN}  Linux Unified Edition${NC}"
echo -e "${CYAN}============================================${NC}"
echo ""

# Check privileges
if [ "$SKIP_BACKEND" = false ] && [ "$USER_INSTALL" = false ] && [ "$EUID" -ne 0 ]; then
    echo -e "${RED}ERROR: Backend installation requires sudo.${NC}"
    echo "       Use --user-install or --skip-backend"
    exit 1
fi

# ============================================
# BACKEND INSTALLATION
# ============================================
if [ "$SKIP_BACKEND" = false ]; then
    echo -e "${MAGENTA}--- Backend Installation ---${NC}"
    echo ""

    # Check for Go
    if ! command -v go &> /dev/null; then
        echo -e "${RED}ERROR: Go is not installed. Please install Go 1.22+ first.${NC}"
        exit 1
    fi

    # Detect architecture
    ARCH=$(uname -m)
    case $ARCH in
        x86_64) GOARCH="amd64" ;;
        aarch64|arm64) GOARCH="arm64" ;;
        armv7l) GOARCH="arm" ;;
        *) echo -e "${RED}ERROR: Unsupported arch: $ARCH${NC}"; exit 1 ;;
    esac

    echo -e "${YELLOW}[1] Building backend...${NC}"
    GO_BUILD_DIR="$SOURCE_DIR/services/grey-core-api"
    if [ ! -d "$GO_BUILD_DIR" ]; then
        echo -e "${RED}ERROR: Source not found: $GO_BUILD_DIR${NC}"
        exit 1
    fi
    cd "$GO_BUILD_DIR"
    CGO_ENABLED=0 GOOS=linux GOARCH=$GOARCH go build -o "bin/$BINARY_NAME" ./cmd/api
    echo -e "${GREEN}  Build complete.${NC}"

    echo -e "${YELLOW}[2] Creating directories...${NC}"
    mkdir -p "$BIN_DIR" "$CONFIG_DIR" "$LOG_DIR" "$DATA_DIR" "$SYSTEMD_DIR" "$DESKTOP_DIR" "$ICON_DIR"
    echo -e "${GREEN}  Directories created.${NC}"

    echo -e "${YELLOW}[3] Installing binary...${NC}"
    cp "$GO_BUILD_DIR/bin/$BINARY_NAME" "$BIN_DIR/$BINARY_NAME"
    chmod +x "$BIN_DIR/$BINARY_NAME"
    echo -e "${GREEN}  Binary installed.${NC}"

    echo -e "${YELLOW}[4] Creating configuration...${NC}"
    CONFIG_FILE="$CONFIG_DIR/.env"
    if [ ! -f "$CONFIG_FILE" ]; then
        cat > "$CONFIG_FILE" << EOF
DATABASE_URL=postgres://grey:grey_password@localhost:5432/grey_multitenant?sslmode=disable
SERVER_HOST=0.0.0.0
SERVER_PORT=8080
GRPC_PORT=50051
JWT_SECRET=CHANGE_THIS_TO_A_SECURE_SECRET
CORS_ALLOWED_ORIGINS=http://localhost:3000
LOG_LEVEL=info
ENVIRONMENT=production
EOF
        [ "$USER_INSTALL" = false ] && chmod 600 "$CONFIG_FILE"
        echo -e "${GREEN}  Configuration created.${NC}"
    else
        echo -e "${GREEN}  Configuration exists.${NC}"
    fi

    if [ "$SKIP_SERVICE" = false ]; then
        echo -e "${YELLOW}[5] Creating systemd service...${NC}"
        
        if [ "$USER_INSTALL" = true ]; then
            cat > "$SYSTEMD_DIR/$SERVICE_NAME.service" << EOF
[Unit]
Description=Grey Multi-Tenant Platform
After=network-online.target

[Service]
Type=simple
ExecStart=$BIN_DIR/$BINARY_NAME
WorkingDirectory=$CONFIG_DIR
EnvironmentFile=-$CONFIG_FILE
Restart=on-failure

[Install]
WantedBy=default.target
EOF
            systemctl --user daemon-reload
            systemctl --user enable "$SERVICE_NAME.service"
        else
            # Create service user
            id -u grey &>/dev/null || useradd --system --no-create-home --shell /usr/sbin/nologin grey
            chown -R grey:grey "$CONFIG_DIR" "$LOG_DIR" "$DATA_DIR"
            
            cat > "$SYSTEMD_DIR/$SERVICE_NAME.service" << EOF
[Unit]
Description=Grey Multi-Tenant Platform
After=network-online.target postgresql.service

[Service]
Type=simple
User=grey
Group=grey
ExecStart=$BIN_DIR/$BINARY_NAME
WorkingDirectory=$CONFIG_DIR
EnvironmentFile=-$CONFIG_FILE
Restart=on-failure
StandardOutput=append:$LOG_DIR/grey-multitenant.log
StandardError=append:$LOG_DIR/grey-multitenant.error.log

[Install]
WantedBy=multi-user.target
EOF
            systemctl daemon-reload
            systemctl enable "$SERVICE_NAME.service"
        fi
        echo -e "${GREEN}  Service created.${NC}"
    fi

    # Install launcher and desktop entry
    echo -e "${YELLOW}[6] Creating desktop entry...${NC}"
    LAUNCHER_SOURCE="$SCRIPT_DIR/grey-launcher.sh"
    if [ -f "$LAUNCHER_SOURCE" ]; then
        cp "$LAUNCHER_SOURCE" "$BIN_DIR/grey-launcher"
        chmod +x "$BIN_DIR/grey-launcher"
    fi
    
    # Install icon
    ICON_SOURCE="$SCRIPT_DIR/../icons/grey-icon.png"
    if [ -f "$ICON_SOURCE" ]; then
        mkdir -p "$ICON_DIR"
        cp "$ICON_SOURCE" "$ICON_DIR/$APP_ID.png"
    fi
    
    cat > "$DESKTOP_DIR/$APP_ID.desktop" << EOF
[Desktop Entry]
Version=1.0
Type=Application
Name=$APP_NAME Backend
Comment=Grey Multi-Tenant backend launcher
Exec=$BIN_DIR/grey-launcher
Icon=$ICON_DIR/$APP_ID.png
Terminal=true
Categories=Development;Network;
EOF
    command -v update-desktop-database &>/dev/null && update-desktop-database "$DESKTOP_DIR" 2>/dev/null || true
    echo -e "${GREEN}  Desktop entry created.${NC}"
fi

# ============================================
# GUI INSTALLATION
# ============================================
if [ "$SKIP_GUI" = false ]; then
    echo ""
    echo -e "${MAGENTA}--- GUI Installation ---${NC}"
    echo ""

    echo -e "${YELLOW}[1] Checking for GUI package...${NC}"
    GUI_PACKAGE=""

    # Look for AppImage
    for file in "$GUI_DIR/release/"*.AppImage; do
        [ -f "$file" ] && GUI_PACKAGE="$file" && break
    done

    # Build if needed
    if [ -z "$GUI_PACKAGE" ]; then
        if command -v pnpm &> /dev/null; then
            echo "  Building GUI..."
            cd "$GUI_DIR"
            pnpm install --ignore-workspace 2>&1 | tail -n 3
            pnpm build 2>&1 | tail -n 3
            pnpm package:linux 2>&1 | tail -n 3
            for file in "$GUI_DIR/release/"*.AppImage; do
                [ -f "$file" ] && GUI_PACKAGE="$file" && break
            done
        else
            echo -e "${YELLOW}  pnpm not found. Skipping GUI.${NC}"
            SKIP_GUI=true
        fi
    fi

    if [ "$SKIP_GUI" = false ] && [ -n "$GUI_PACKAGE" ]; then
        echo -e "${GREEN}  Found: $(basename "$GUI_PACKAGE")${NC}"

        echo -e "${YELLOW}[2] Installing GUI...${NC}"
        mkdir -p "$GUI_INSTALL_DIR" "$GUI_INSTALL_DIR/icons" "$GUI_DATA_DIR"
        cp "$GUI_PACKAGE" "$GUI_INSTALL_DIR/grey-multitenant-gui.AppImage"
        chmod +x "$GUI_INSTALL_DIR/grey-multitenant-gui.AppImage"
        echo -e "${GREEN}  GUI installed.${NC}"

        echo -e "${YELLOW}[3] Creating GUI launcher...${NC}"
        cat > "$BIN_DIR/grey-gui" << EOF
#!/bin/bash
exec "$GUI_INSTALL_DIR/grey-multitenant-gui.AppImage" "\$@"
EOF
        chmod +x "$BIN_DIR/grey-gui"
        echo -e "${GREEN}  Launcher: grey-gui${NC}"

        echo -e "${YELLOW}[4] Creating GUI desktop entry...${NC}"
        # Copy icon if available
        ICON_SOURCE="$GUI_DIR/resources/icon.png"
        [ -f "$ICON_SOURCE" ] && cp "$ICON_SOURCE" "$GUI_INSTALL_DIR/icons/grey-gui.png"
        
        cat > "$DESKTOP_DIR/$GUI_APP_ID.desktop" << EOF
[Desktop Entry]
Version=1.0
Type=Application
Name=Grey Multi-Tenant GUI
Comment=Desktop application for Grey Multi-Tenant Platform
Exec=$GUI_INSTALL_DIR/grey-multitenant-gui.AppImage %U
Icon=$GUI_INSTALL_DIR/icons/grey-gui.png
Terminal=false
Categories=Utility;Development;
EOF
        chmod +x "$DESKTOP_DIR/$GUI_APP_ID.desktop"
        command -v update-desktop-database &>/dev/null && update-desktop-database "$DESKTOP_DIR" 2>/dev/null || true
        command -v gtk-update-icon-cache &>/dev/null && gtk-update-icon-cache -f "$(dirname "$ICON_DIR")" 2>/dev/null || true
        echo -e "${GREEN}  Desktop entry created.${NC}"
    fi
fi

# Install uninstaller
echo ""
echo -e "${YELLOW}Installing uninstaller...${NC}"
UNINSTALL_SCRIPT="$SCRIPT_DIR/uninstall.sh"
if [ -f "$UNINSTALL_SCRIPT" ]; then
    cp "$UNINSTALL_SCRIPT" "$BIN_DIR/grey-uninstall"
    chmod +x "$BIN_DIR/grey-uninstall"
fi
echo -e "${GREEN}  Uninstaller: grey-uninstall${NC}"

# Summary
echo ""
echo -e "${GREEN}============================================${NC}"
echo -e "${GREEN}  Installation Complete!${NC}"
echo -e "${GREEN}============================================${NC}"
echo ""
if [ "$SKIP_BACKEND" = false ]; then
    echo -e "${CYAN}Backend:${NC}"
    echo "  Binary: $BIN_DIR/$BINARY_NAME"
    echo "  Config: $CONFIG_FILE"
    if [ "$USER_INSTALL" = true ]; then
        echo "  Service: systemctl --user start $SERVICE_NAME"
    else
        echo "  Service: sudo systemctl start $SERVICE_NAME"
    fi
    echo ""
fi
if [ "$SKIP_GUI" = false ]; then
    echo -e "${CYAN}GUI:${NC}"
    echo "  App: $GUI_INSTALL_DIR"
    echo "  Launch: grey-gui or application menu"
    echo ""
fi
echo -e "${YELLOW}Commands:${NC}"
[ "$SKIP_BACKEND" = false ] && echo "  grey-launcher  # Start backend interactively"
[ "$SKIP_GUI" = false ] && echo "  grey-gui       # Launch GUI"
echo "  grey-uninstall # Remove installation"
echo ""
