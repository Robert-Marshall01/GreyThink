#!/usr/bin/env bash
# ─────────────────────────────────────────────────────────────
# Grey DB — macOS Installer
# Usage: sudo bash install-macos.sh
# ─────────────────────────────────────────────────────────────

set -euo pipefail
IFS=$'\n\t'

# ── Configuration ───────────────────────────────────────────
APP_NAME="greydb"
APP_DISPLAY="Grey DB"
APP_VERSION="1.0.0"
INSTALL_DIR="/usr/local/opt/greydb"
DATA_DIR="/usr/local/var/greydb"
LOG_DIR="/usr/local/var/log/greydb"
CONFIG_DIR="/usr/local/etc/greydb"
BIN_LINK="/usr/local/bin/greydb"
PLIST_LABEL="com.greythink.greydb-server"
PLIST_DIR="$HOME/Library/LaunchAgents"
PLIST_FILE="$PLIST_DIR/$PLIST_LABEL.plist"

# ── Colors ──────────────────────────────────────────────────
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
WHITE='\033[1;37m'
NC='\033[0m'

# ── Helpers ─────────────────────────────────────────────────
log_info()  { echo -e "  ${CYAN}[INFO]${NC}  $1"; }
log_ok()    { echo -e "  ${GREEN}[OK]${NC}    $1"; }
log_warn()  { echo -e "  ${YELLOW}[WARN]${NC}  $1"; }
log_error() { echo -e "  ${RED}[ERROR]${NC} $1"; }

write_log() {
    local level="$1"
    local msg="$2"
    if [[ -d "$LOG_DIR" ]]; then
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] [$level] $msg" >> "$LOG_DIR/install.log"
    fi
}

check_command() {
    command -v "$1" &> /dev/null
}

# Compare version strings: returns 0 if $1 >= $2
version_gte() {
    local IFS=.
    local i ver1=($1) ver2=($2)
    for ((i=0; i<${#ver2[@]}; i++)); do
        local v1="${ver1[i]:-0}"
        local v2="${ver2[i]:-0}"
        v1="${v1%%[!0-9]*}"
        v2="${v2%%[!0-9]*}"
        v1="${v1:-0}"
        v2="${v2:-0}"
        if (( v1 > v2 )); then return 0; fi
        if (( v1 < v2 )); then return 1; fi
    done
    return 0
}

# ── Banner ──────────────────────────────────────────────────
echo ""
echo -e "  ${MAGENTA}╔══════════════════════════════════════════════╗${NC}"
echo -e "  ${MAGENTA}║          Grey DB — macOS Installer           ║${NC}"
echo -e "  ${MAGENTA}║              Version ${APP_VERSION}                  ║${NC}"
echo -e "  ${MAGENTA}╚══════════════════════════════════════════════╝${NC}"
echo ""

# ── Check root ──────────────────────────────────────────────
if [[ $EUID -ne 0 ]]; then
    log_error "This installer must be run as root."
    echo -e "  Usage: ${WHITE}sudo bash $0${NC}"
    exit 1
fi

# Capture the real user (not root) for LaunchAgent ownership
REAL_USER="${SUDO_USER:-$USER}"
REAL_HOME="$(eval echo "~$REAL_USER")"
PLIST_DIR="$REAL_HOME/Library/LaunchAgents"
PLIST_FILE="$PLIST_DIR/$PLIST_LABEL.plist"

# ── Create directories ─────────────────────────────────────
echo -e "  ${WHITE}[1/8] Creating directories...${NC}"
mkdir -p "$INSTALL_DIR"
mkdir -p "$DATA_DIR"
mkdir -p "$LOG_DIR"
mkdir -p "$CONFIG_DIR"
mkdir -p "$PLIST_DIR"
log_ok "Directories created"
write_log "INFO" "Directories created"

# ── Check prerequisites ────────────────────────────────────
echo -e "  ${WHITE}[2/8] Checking prerequisites...${NC}"

# Node.js
if ! check_command node; then
    log_error "Node.js is not installed."
    log_info "Install via Homebrew: brew install node@20"
    exit 1
fi
NODE_VER="$(node --version | sed 's/^v//')"
if ! version_gte "$NODE_VER" "20.0.0"; then
    log_error "Node.js $NODE_VER is below minimum 20.0.0"
    exit 1
fi
log_ok "Node.js $NODE_VER"

# npm
if ! check_command npm; then
    log_error "npm is not installed."
    exit 1
fi
NPM_VER="$(npm --version)"
if ! version_gte "$NPM_VER" "9.0.0"; then
    log_error "npm $NPM_VER is below minimum 9.0.0"
    exit 1
fi
log_ok "npm $NPM_VER"

# Docker
if ! check_command docker; then
    log_error "Docker is not installed."
    log_info "Install Docker Desktop from https://docker.com"
    exit 1
fi
log_ok "Docker detected"

# Git (optional)
if ! check_command git; then
    log_warn "Git is not installed — optional but recommended"
else
    log_ok "Git detected"
fi

# ── Locate source ──────────────────────────────────────────
echo -e "  ${WHITE}[3/8] Locating source files...${NC}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SOURCE_DIR="$(dirname "$SCRIPT_DIR")"

if [[ ! -f "$SOURCE_DIR/package.json" ]]; then
    log_error "Cannot find Grey DB source at $SOURCE_DIR"
    log_error "Ensure this script is in the 'installers' folder inside the Grey DB project."
    exit 1
fi
log_ok "Source found at $SOURCE_DIR"

# ── Copy files ──────────────────────────────────────────────
echo -e "  ${WHITE}[4/8] Copying files to $INSTALL_DIR...${NC}"

rsync -a --delete \
    --exclude='node_modules' \
    --exclude='.git' \
    --exclude='*.log' \
    "$SOURCE_DIR/" "$INSTALL_DIR/"

# Clean stale build artifacts so tsc does a full rebuild
find "$INSTALL_DIR" -name '*.tsbuildinfo' -delete 2>/dev/null || true
rm -rf "$INSTALL_DIR"/packages/*/dist "$INSTALL_DIR"/examples/*/dist

log_ok "Files copied"
write_log "INFO" "Files copied to $INSTALL_DIR"

# ── Install npm dependencies ───────────────────────────────
echo -e "  ${WHITE}[5/8] Installing npm dependencies (this may take a few minutes)...${NC}"

cd "$INSTALL_DIR"
# Remove stale node_modules from any prior install attempt
rm -rf node_modules
export NODE_ENV=development
if ! npm install --include=dev --no-audit --no-fund 2>&1 | tail -3; then
    log_error "npm install failed"
    exit 1
fi
log_ok "npm dependencies installed"

# ── Build ───────────────────────────────────────────────────
echo -e "  ${WHITE}[6/8] Building Grey DB...${NC}"

cd "$INSTALL_DIR"
# Build in dependency order: core must compile before packages that depend on it
for ws in packages/core packages/server packages/cli packages/ui examples/crm; do
    if [[ -f "$ws/package.json" ]] && grep -q '"build"' "$ws/package.json"; then
        log_info "Building $ws..."
        if ! npm run build --workspace="$ws" 2>&1 | tail -5; then
            log_error "Build failed for $ws"
            exit 1
        fi
    fi
done
log_ok "Build completed"
write_log "INFO" "Build completed"

# ── Configure ──────────────────────────────────────────────
echo -e "  ${WHITE}[7/8] Configuring environment...${NC}"

ENV_FILE="$CONFIG_DIR/.env"
if [[ ! -f "$ENV_FILE" ]]; then
    cat > "$ENV_FILE" << 'ENVEOF'
# Grey DB Configuration
GREY_DB_HOST=localhost
GREY_DB_PORT=5432
GREY_DB_DATABASE=greydb
GREY_DB_USER=greydb
GREY_DB_PASSWORD=greydb_secret
PORT=4000
VITE_API_URL=http://localhost:4000
NODE_ENV=production
ENVEOF
    chmod 600 "$ENV_FILE"
    log_ok "Environment config created at $ENV_FILE"
else
    log_ok "Environment config already exists — skipping"
fi

# Symlink .env into install dir
ln -sf "$ENV_FILE" "$INSTALL_DIR/.env"

# ── Register CLI and launchd service ───────────────────────
echo -e "  ${WHITE}[8/8] Registering CLI and services...${NC}"

# CLI launcher
cat > "$BIN_LINK" << CLIEOF
#!/usr/bin/env bash
exec node "$INSTALL_DIR/packages/cli/dist/index.js" "\$@"
CLIEOF
chmod 755 "$BIN_LINK"
log_ok "CLI registered at $BIN_LINK"

# Detect the Node.js binary path for the plist
NODE_PATH="$(which node)"

# launchd plist — Grey DB Server (runs as the real user)
cat > "$PLIST_FILE" << PLISTEOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>$PLIST_LABEL</string>

    <key>ProgramArguments</key>
    <array>
        <string>$NODE_PATH</string>
        <string>$INSTALL_DIR/packages/server/dist/index.js</string>
    </array>

    <key>WorkingDirectory</key>
    <string>$INSTALL_DIR</string>

    <key>EnvironmentVariables</key>
    <dict>
        <key>NODE_ENV</key>
        <string>production</string>
        <key>GREY_DB_HOST</key>
        <string>localhost</string>
        <key>GREY_DB_PORT</key>
        <string>5432</string>
        <key>GREY_DB_DATABASE</key>
        <string>greydb</string>
        <key>GREY_DB_USER</key>
        <string>greydb</string>
        <key>GREY_DB_PASSWORD</key>
        <string>greydb_secret</string>
        <key>PORT</key>
        <string>4000</string>
    </dict>

    <key>RunAtLoad</key>
    <false/>

    <key>KeepAlive</key>
    <dict>
        <key>SuccessfulExit</key>
        <false/>
    </dict>

    <key>StandardOutPath</key>
    <string>$LOG_DIR/server.log</string>

    <key>StandardErrorPath</key>
    <string>$LOG_DIR/server-error.log</string>
</dict>
</plist>
PLISTEOF

chown "$REAL_USER" "$PLIST_FILE"
log_ok "launchd service created at $PLIST_FILE"

# Set ownership on install dirs (real user for macOS)
chown -R "$REAL_USER" "$INSTALL_DIR"
chown -R "$REAL_USER" "$DATA_DIR"
chown -R "$REAL_USER" "$LOG_DIR"
chown -R "$REAL_USER" "$CONFIG_DIR"

# Write install metadata
cat > "$CONFIG_DIR/install-info" << METAEOF
APP_NAME=$APP_NAME
APP_VERSION=$APP_VERSION
INSTALL_DIR=$INSTALL_DIR
DATA_DIR=$DATA_DIR
LOG_DIR=$LOG_DIR
CONFIG_DIR=$CONFIG_DIR
BIN_LINK=$BIN_LINK
PLIST_FILE=$PLIST_FILE
PLIST_LABEL=$PLIST_LABEL
INSTALL_DATE=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
METAEOF

write_log "INFO" "Installation completed"

# ── Done ────────────────────────────────────────────────────
echo ""
echo -e "  ${GREEN}╔══════════════════════════════════════════════╗${NC}"
echo -e "  ${GREEN}║       Grey DB installed successfully!        ║${NC}"
echo -e "  ${GREEN}╚══════════════════════════════════════════════╝${NC}"
echo ""
echo -e "  Install directory:  ${WHITE}$INSTALL_DIR${NC}"
echo -e "  Data directory:     ${WHITE}$DATA_DIR${NC}"
echo -e "  Config file:        ${WHITE}$ENV_FILE${NC}"
echo -e "  Logs:               ${WHITE}$LOG_DIR${NC}"
echo ""
echo -e "  ${YELLOW}Next steps:${NC}"
echo -e "    1. Start PostgreSQL:       ${WHITE}cd \"$INSTALL_DIR\" && docker compose up -d postgres${NC}"
echo -e "    2. Start the API server:   ${WHITE}launchctl load $PLIST_FILE${NC}"
echo -e "    3. Stop the API server:    ${WHITE}launchctl unload $PLIST_FILE${NC}"
echo -e "    4. Use the CLI:            ${WHITE}greydb --help${NC}"
echo ""
echo -e "  ${YELLOW}To uninstall:${NC}"
echo -e "    ${WHITE}sudo bash $INSTALL_DIR/installers/uninstall-macos.sh${NC}"
echo ""
