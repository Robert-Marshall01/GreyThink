#!/bin/bash
#
# Grey Multi-Tenant Platform - macOS Unified Installer
#
# Installs both the backend service (launchd) and GUI desktop application.
#
# Usage:
#   sudo ./install.sh            # Install both backend and GUI
#   sudo ./install.sh --skip-gui # Install backend only
#   ./install.sh --skip-backend  # Install GUI only (no sudo needed)
#
# Options:
#   --skip-backend     Skip backend installation
#   --skip-gui         Skip GUI installation  
#   --skip-service     Install backend but skip launchd service
#   --user-install     Install for current user only
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
    BACKEND_INSTALL_DIR="$HOME/Applications/$APP_BUNDLE_NAME"
    GUI_INSTALL_DIR="$HOME/Applications"
    BIN_DIR="$HOME/.local/bin"
    CONFIG_DIR="$HOME/.config/grey-multitenant"
    LOG_DIR="$HOME/Library/Logs/GreyMultiTenant"
    GUI_DATA_DIR="$HOME/Library/Application Support/Grey Multi-Tenant GUI"
    LAUNCHD_DIR="$HOME/Library/LaunchAgents"
else
    BACKEND_INSTALL_DIR="/Applications/$APP_BUNDLE_NAME"
    GUI_INSTALL_DIR="/Applications"
    BIN_DIR="$PREFIX/bin"
    CONFIG_DIR="/etc/grey-multitenant"
    LOG_DIR="/var/log/grey-multitenant"
    GUI_DATA_DIR="$HOME/Library/Application Support/Grey Multi-Tenant GUI"
    LAUNCHD_DIR="/Library/LaunchDaemons"
fi

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SOURCE_DIR="$(dirname "$(dirname "$SCRIPT_DIR")")"
GUI_DIR="$SOURCE_DIR/gui"

echo -e "${CYAN}============================================${NC}"
echo -e "${CYAN}  Grey Multi-Tenant Platform Installer${NC}"
echo -e "${CYAN}  macOS Unified Edition${NC}"
echo -e "${CYAN}============================================${NC}"
echo ""

# Check privileges
if [ "$SKIP_BACKEND" = false ] && [ "$USER_INSTALL" = false ] && [ "$EUID" -ne 0 ]; then
    echo -e "${RED}ERROR: Backend installation requires sudo.${NC}"
    echo "       Use --user-install or --skip-backend for user-level installation."
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

    echo -e "${YELLOW}[1] Building backend...${NC}"
    GO_BUILD_DIR="$SOURCE_DIR/services/grey-core-api"
    if [ ! -d "$GO_BUILD_DIR" ]; then
        echo -e "${RED}ERROR: Source not found: $GO_BUILD_DIR${NC}"
        exit 1
    fi
    cd "$GO_BUILD_DIR"
    CGO_ENABLED=0 GOOS=darwin GOARCH=$(uname -m | sed 's/x86_64/amd64/') go build -o "bin/$BINARY_NAME" ./cmd/api
    echo -e "${GREEN}  Build complete.${NC}"

    echo -e "${YELLOW}[2] Creating directories...${NC}"
    mkdir -p "$BACKEND_INSTALL_DIR/Contents/MacOS"
    mkdir -p "$BACKEND_INSTALL_DIR/Contents/Resources"
    mkdir -p "$BIN_DIR"
    mkdir -p "$CONFIG_DIR"
    mkdir -p "$LOG_DIR"
    echo -e "${GREEN}  Directories created.${NC}"

    echo -e "${YELLOW}[3] Installing backend files...${NC}"
    cp "$GO_BUILD_DIR/bin/$BINARY_NAME" "$BACKEND_INSTALL_DIR/Contents/MacOS/$BINARY_NAME"
    chmod +x "$BACKEND_INSTALL_DIR/Contents/MacOS/$BINARY_NAME"
    ln -sf "$BACKEND_INSTALL_DIR/Contents/MacOS/$BINARY_NAME" "$BIN_DIR/$BINARY_NAME"
    
    # Create Info.plist
    cat > "$BACKEND_INSTALL_DIR/Contents/Info.plist" << EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleExecutable</key><string>$BINARY_NAME</string>
    <key>CFBundleIdentifier</key><string>com.grey.multitenant</string>
    <key>CFBundleName</key><string>$APP_NAME</string>
    <key>CFBundleVersion</key><string>1.0.0</string>
    <key>LSUIElement</key><true/>
</dict>
</plist>
EOF
    echo -e "${GREEN}  Backend installed.${NC}"

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
        echo -e "${YELLOW}[5] Registering launchd service...${NC}"
        [ -f "$LAUNCHD_DIR/$SERVICE_NAME.plist" ] && launchctl unload "$LAUNCHD_DIR/$SERVICE_NAME.plist" 2>/dev/null || true
        
        if [ "$USER_INSTALL" = true ]; then
            # User-level agent (runs as current user)
            cat > "$LAUNCHD_DIR/$SERVICE_NAME.plist" << EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key><string>$SERVICE_NAME</string>
    <key>ProgramArguments</key><array><string>$BACKEND_INSTALL_DIR/Contents/MacOS/$BINARY_NAME</string></array>
    <key>WorkingDirectory</key><string>$CONFIG_DIR</string>
    <key>EnvironmentVariables</key>
    <dict>
        <key>GREY_CONFIG_PATH</key>
        <string>$CONFIG_DIR/.env</string>
    </dict>
    <key>RunAtLoad</key><true/>
    <key>KeepAlive</key><true/>
    <key>StandardOutPath</key><string>$LOG_DIR/grey-multitenant.log</string>
    <key>StandardErrorPath</key><string>$LOG_DIR/grey-multitenant.error.log</string>
</dict>
</plist>
EOF
            launchctl load "$LAUNCHD_DIR/$SERVICE_NAME.plist"
        else
            # System-level daemon - create service user first
            if ! dscl . -read /Users/_grey &>/dev/null 2>&1; then
                echo "  Creating service user _grey..."
                # Find an available UID in the service range (200-400)
                NEW_UID=250
                while dscl . -list /Users UniqueID | awk '{print $2}' | grep -q "^${NEW_UID}$"; do
                    NEW_UID=$((NEW_UID + 1))
                done
                dscl . -create /Users/_grey
                dscl . -create /Users/_grey UserShell /usr/bin/false
                dscl . -create /Users/_grey RealName "Grey Multi-Tenant Service"
                dscl . -create /Users/_grey UniqueID $NEW_UID
                dscl . -create /Users/_grey PrimaryGroupID 20
                dscl . -create /Users/_grey NFSHomeDirectory /var/empty
            fi
            
            # Set ownership for service user
            chown -R _grey:staff "$CONFIG_DIR" 2>/dev/null || true
            chown -R _grey:staff "$LOG_DIR" 2>/dev/null || true
            
            cat > "$LAUNCHD_DIR/$SERVICE_NAME.plist" << EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key><string>$SERVICE_NAME</string>
    <key>ProgramArguments</key><array><string>$BACKEND_INSTALL_DIR/Contents/MacOS/$BINARY_NAME</string></array>
    <key>WorkingDirectory</key><string>$CONFIG_DIR</string>
    <key>EnvironmentVariables</key>
    <dict>
        <key>GREY_CONFIG_PATH</key>
        <string>$CONFIG_DIR/.env</string>
    </dict>
    <key>UserName</key><string>_grey</string>
    <key>GroupName</key><string>staff</string>
    <key>RunAtLoad</key><true/>
    <key>KeepAlive</key>
    <dict>
        <key>SuccessfulExit</key><false/>
        <key>Crashed</key><true/>
    </dict>
    <key>ThrottleInterval</key><integer>10</integer>
    <key>StandardOutPath</key><string>$LOG_DIR/grey-multitenant.log</string>
    <key>StandardErrorPath</key><string>$LOG_DIR/grey-multitenant.error.log</string>
</dict>
</plist>
EOF
            launchctl load "$LAUNCHD_DIR/$SERVICE_NAME.plist"
        fi
        echo -e "${GREEN}  Service registered.${NC}"
    fi

    # Copy launcher
    LAUNCHER_SOURCE="$SCRIPT_DIR/grey-launcher.sh"
    if [ -f "$LAUNCHER_SOURCE" ]; then
        cp "$LAUNCHER_SOURCE" "$BIN_DIR/grey-launcher"
        chmod +x "$BIN_DIR/grey-launcher"
    fi
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
    GUI_TYPE=""

    # Check for DMG
    for file in "$GUI_DIR/release/"*.dmg; do
        [ -f "$file" ] && GUI_PACKAGE="$file" && GUI_TYPE="dmg" && break
    done

    # Check for app bundle
    if [ -z "$GUI_PACKAGE" ]; then
        for loc in "$GUI_DIR/release/mac/$GUI_BUNDLE_NAME" "$GUI_DIR/release/mac-arm64/$GUI_BUNDLE_NAME"; do
            [ -d "$loc" ] && GUI_PACKAGE="$loc" && GUI_TYPE="app" && break
        done
    fi

    # Build if needed
    if [ -z "$GUI_PACKAGE" ]; then
        echo "  Building GUI..."
        if command -v pnpm &> /dev/null; then
            cd "$GUI_DIR"
            pnpm install --ignore-workspace 2>&1 | tail -n 3
            pnpm build 2>&1 | tail -n 3
            pnpm package:mac 2>&1 | tail -n 3
            for file in "$GUI_DIR/release/"*.dmg; do
                [ -f "$file" ] && GUI_PACKAGE="$file" && GUI_TYPE="dmg" && break
            done
            if [ -z "$GUI_PACKAGE" ]; then
                for loc in "$GUI_DIR/release/mac/$GUI_BUNDLE_NAME" "$GUI_DIR/release/mac-arm64/$GUI_BUNDLE_NAME"; do
                    [ -d "$loc" ] && GUI_PACKAGE="$loc" && GUI_TYPE="app" && break
                done
            fi
        else
            echo -e "${YELLOW}  pnpm not found. Skipping GUI.${NC}"
            SKIP_GUI=true
        fi
    fi

    if [ "$SKIP_GUI" = false ] && [ -n "$GUI_PACKAGE" ]; then
        echo -e "${GREEN}  Found: $(basename "$GUI_PACKAGE")${NC}"

        echo -e "${YELLOW}[2] Installing GUI...${NC}"
        mkdir -p "$GUI_DATA_DIR"
        
        if [ "$GUI_TYPE" = "dmg" ]; then
            MOUNT_POINT="/Volumes/Grey Multi-Tenant GUI"
            hdiutil attach "$GUI_PACKAGE" -mountpoint "$MOUNT_POINT" -quiet
            cp -R "$MOUNT_POINT/$GUI_BUNDLE_NAME" "$GUI_INSTALL_DIR/"
            hdiutil detach "$MOUNT_POINT" -quiet
        else
            cp -R "$GUI_PACKAGE" "$GUI_INSTALL_DIR/"
        fi
        xattr -dr com.apple.quarantine "$GUI_INSTALL_DIR/$GUI_BUNDLE_NAME" 2>/dev/null || true
        echo -e "${GREEN}  GUI installed to $GUI_INSTALL_DIR/$GUI_BUNDLE_NAME${NC}"

        # Install GUI launcher
        mkdir -p "$BIN_DIR"
        cat > "$BIN_DIR/grey-gui" << EOF
#!/bin/bash
open "$GUI_INSTALL_DIR/$GUI_BUNDLE_NAME"
EOF
        chmod +x "$BIN_DIR/grey-gui"
        echo -e "${GREEN}  Launcher: grey-gui${NC}"
    fi
fi

# Copy uninstaller
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
    echo "  App: $BACKEND_INSTALL_DIR"
    echo "  Config: $CONFIG_FILE"
    echo "  Binary: $BIN_DIR/$BINARY_NAME"
    echo ""
fi
if [ "$SKIP_GUI" = false ]; then
    echo -e "${CYAN}GUI:${NC}"
    echo "  App: $GUI_INSTALL_DIR/$GUI_BUNDLE_NAME"
    echo "  Launch: grey-gui or Spotlight"
    echo ""
fi
echo -e "${YELLOW}Commands:${NC}"
[ "$SKIP_BACKEND" = false ] && echo "  launchctl start $SERVICE_NAME"
[ "$SKIP_GUI" = false ] && echo "  grey-gui"
echo "  grey-uninstall"
echo ""
