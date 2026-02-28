#!/usr/bin/env bash
# ─────────────────────────────────────────────────────────────
# Grey DB — Linux Installer
# Usage: sudo bash install-linux.sh
# ─────────────────────────────────────────────────────────────

set -euo pipefail
IFS=$'\n\t'

# ── Configuration ───────────────────────────────────────────
APP_NAME="greydb"
APP_DISPLAY="Grey DB"
APP_VERSION="1.0.0"
INSTALL_DIR="/opt/greydb"
DATA_DIR="/var/lib/greydb"
LOG_DIR="/var/log/greydb"
CONFIG_DIR="/etc/greydb"
BIN_LINK="/usr/local/bin/greydb"
SYSTEMD_DIR="/etc/systemd/system"

# ── Colors ──────────────────────────────────────────────────
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
WHITE='\033[1;37m'
NC='\033[0m' # No Color

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
        # Strip non-numeric characters
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
echo -e "  ${MAGENTA}║          Grey DB — Linux Installer           ║${NC}"
echo -e "  ${MAGENTA}║              Version ${APP_VERSION}                  ║${NC}"
echo -e "  ${MAGENTA}╚══════════════════════════════════════════════╝${NC}"
echo ""

# ── Check root ──────────────────────────────────────────────
if [[ $EUID -ne 0 ]]; then
    log_error "This installer must be run as root."
    echo -e "  Usage: ${WHITE}sudo bash $0${NC}"
    exit 1
fi

# ── Create directories ─────────────────────────────────────
echo -e "  ${WHITE}[1/8] Creating directories...${NC}"
mkdir -p "$INSTALL_DIR"
mkdir -p "$DATA_DIR"
mkdir -p "$LOG_DIR"
mkdir -p "$CONFIG_DIR"
log_ok "Directories created"
write_log "INFO" "Directories created"

# ── Check prerequisites ────────────────────────────────────
echo -e "  ${WHITE}[2/8] Checking prerequisites...${NC}"

# Node.js
if ! check_command node; then
    log_error "Node.js is not installed."
    log_info "Install it via: curl -fsSL https://deb.nodesource.com/setup_20.x | sudo -E bash - && sudo apt-get install -y nodejs"
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
    log_info "Install it via: https://docs.docker.com/engine/install/"
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

# Symlink .env into install dir for runtime
ln -sf "$ENV_FILE" "$INSTALL_DIR/.env"

# ── Register CLI and systemd services ──────────────────────
echo -e "  ${WHITE}[8/8] Registering CLI and services...${NC}"

# CLI symlink
cat > "$BIN_LINK" << CLIEOF
#!/usr/bin/env bash
exec node "$INSTALL_DIR/packages/cli/dist/index.js" "\$@"
CLIEOF
chmod 755 "$BIN_LINK"
log_ok "CLI registered at $BIN_LINK"

# Create a dedicated system user (no login shell, no home)
if ! id -u greydb &>/dev/null; then
    useradd --system --no-create-home --shell /usr/sbin/nologin greydb
    log_ok "System user 'greydb' created"
fi

# Set ownership
chown -R greydb:greydb "$INSTALL_DIR"
chown -R greydb:greydb "$DATA_DIR"
chown -R greydb:greydb "$LOG_DIR"
chown greydb:greydb "$ENV_FILE"

# systemd service — Grey DB Server
cat > "$SYSTEMD_DIR/greydb-server.service" << SVCEOF
[Unit]
Description=Grey DB API Server
After=network.target docker.service
Wants=docker.service

[Service]
Type=simple
User=greydb
Group=greydb
WorkingDirectory=$INSTALL_DIR
EnvironmentFile=$CONFIG_DIR/.env
ExecStart=/usr/bin/node $INSTALL_DIR/packages/server/dist/index.js
Restart=on-failure
RestartSec=5
StandardOutput=append:$LOG_DIR/server.log
StandardError=append:$LOG_DIR/server-error.log

# Security hardening
NoNewPrivileges=true
ProtectSystem=strict
ProtectHome=true
ReadWritePaths=$DATA_DIR $LOG_DIR
PrivateTmp=true

[Install]
WantedBy=multi-user.target
SVCEOF

systemctl daemon-reload
log_ok "systemd service 'greydb-server' created"

# Write install metadata for uninstaller
cat > "$CONFIG_DIR/install-info" << METAEOF
APP_NAME=$APP_NAME
APP_VERSION=$APP_VERSION
INSTALL_DIR=$INSTALL_DIR
DATA_DIR=$DATA_DIR
LOG_DIR=$LOG_DIR
CONFIG_DIR=$CONFIG_DIR
BIN_LINK=$BIN_LINK
INSTALL_DATE=$(date -Iseconds)
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
echo -e "    2. Start the API server:   ${WHITE}sudo systemctl start greydb-server${NC}"
echo -e "    3. Enable on boot:         ${WHITE}sudo systemctl enable greydb-server${NC}"
echo -e "    4. Use the CLI:            ${WHITE}greydb --help${NC}"
echo ""
echo -e "  ${YELLOW}To uninstall:${NC}"
echo -e "    ${WHITE}sudo bash $INSTALL_DIR/installers/uninstall-linux.sh${NC}"
echo ""
