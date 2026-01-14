#!/bin/bash
# ═══════════════════════════════════════════════════════════════════════════════
# Grey Optimizer - macOS Installer
# ═══════════════════════════════════════════════════════════════════════════════
#
# This script installs Grey Optimizer on macOS systems with launchd.
#
# Usage:
#   ./install_macos.sh                           # Simulation mode (default)
#   ./install_macos.sh --mode=live --confirm-live # Live mode (requires sudo)
#
# Flags:
#   --mode=simulation|live   Installation mode (default: simulation)
#   --confirm-live           Required confirmation for live mode
#   --preserve-logs          Keep existing logs during reinstall
#   --user-install           Install for current user only (no sudo needed)
#   --prefix=PATH            Installation prefix (default: /usr/local/grey-optimizer)
#
# Safety:
#   - Simulation mode: No sudo required, no system changes, logs actions
#   - Live mode: Creates backups before changes, health check required
#   - All actions logged to SQLite audit DB with HMAC signatures
#
# ═══════════════════════════════════════════════════════════════════════════════

set -euo pipefail

# ─────────────────────────────────────────────────────────────────────────────
# Script Directory and Version
# ─────────────────────────────────────────────────────────────────────────────

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
VERSION="1.0.0"
INSTALL_TIMESTAMP="$(date -u +%Y%m%d-%H%M%S)"

# ─────────────────────────────────────────────────────────────────────────────
# Default Configuration
# ─────────────────────────────────────────────────────────────────────────────

MODE="simulation"
CONFIRM_LIVE=false
PRESERVE_LOGS=false
USER_INSTALL=false
PREFIX="/usr/local/grey-optimizer"

# Installation paths
INSTALL_DIR=""
CONFIG_DIR=""
LOG_DIR=""
DATA_DIR=""
BACKUP_DIR=""
LAUNCHD_DIR=""

# Service configuration
SERVICE_LABEL="com.grey.optimizer"
PLIST_FILE=""

# Health check settings
HEALTH_ENDPOINT="http://127.0.0.1:8090/health"
HEALTH_TIMEOUT=30
HEALTH_RETRY_INTERVAL=2

# ─────────────────────────────────────────────────────────────────────────────
# Colors and Logging
# ─────────────────────────────────────────────────────────────────────────────

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

INSTALL_LOG=""

log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[OK]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1" >&2; }
log_sim() { echo -e "${CYAN}[SIMULATION]${NC} Would: $1"; }

# ─────────────────────────────────────────────────────────────────────────────
# Audit Functions
# ─────────────────────────────────────────────────────────────────────────────

AUDIT_SECRET="${GREY_AUDIT_SECRET:-$(hostname)-grey-optimizer-audit}"

init_audit_db() {
    local db_path="$1"
    
    if [[ "$MODE" == "simulation" ]]; then
        log_sim "Initialize audit database at $db_path"
        return 0
    fi
    
    mkdir -p "$(dirname "$db_path")"
    
    sqlite3 "$db_path" <<'SQL'
CREATE TABLE IF NOT EXISTS audit_log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp TEXT NOT NULL DEFAULT (datetime('now')),
    action TEXT NOT NULL,
    category TEXT NOT NULL,
    details TEXT,
    mode TEXT NOT NULL,
    user TEXT,
    hostname TEXT,
    success INTEGER,
    signature TEXT
);
CREATE TABLE IF NOT EXISTS checkpoints (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp TEXT NOT NULL DEFAULT (datetime('now')),
    checkpoint_type TEXT NOT NULL,
    state_hash TEXT NOT NULL,
    signature TEXT NOT NULL
);
SQL
}

audit_log() {
    local action="$1" category="$2" details="${3:-}" success="${4:-1}"
    local db_path="${DATA_DIR}/audit.db"
    
    if [[ "$MODE" == "simulation" ]]; then
        log_sim "Audit: $action ($category)"
        return 0
    fi
    
    local sign_data="${action}|${category}|${details}|${success}|$(date -u +%s)"
    local signature
    signature=$(echo -n "$sign_data" | openssl dgst -sha256 -hmac "$AUDIT_SECRET" 2>/dev/null | awk '{print $NF}')
    
    sqlite3 "$db_path" "INSERT INTO audit_log (action, category, details, mode, user, hostname, success, signature) VALUES ('$action', '$category', '$(echo "$details" | sed "s/'/''/g")', '$MODE', '$(whoami)', '$(hostname)', $success, '$signature');"
}

create_checkpoint() {
    local checkpoint_type="$1" state_data="$2"
    
    if [[ "$MODE" == "simulation" ]]; then
        log_sim "Create checkpoint: $checkpoint_type"
        return 0
    fi
    
    local db_path="${DATA_DIR}/audit.db"
    local state_hash signature
    state_hash=$(echo -n "$state_data" | shasum -a 256 | awk '{print $1}')
    signature=$(echo -n "${checkpoint_type}|${state_hash}|$(date -u +%s)" | openssl dgst -sha256 -hmac "$AUDIT_SECRET" 2>/dev/null | awk '{print $NF}')
    
    sqlite3 "$db_path" "INSERT INTO checkpoints (checkpoint_type, state_hash, signature) VALUES ('$checkpoint_type', '$state_hash', '$signature');"
}

# ─────────────────────────────────────────────────────────────────────────────
# Backup Functions
# ─────────────────────────────────────────────────────────────────────────────

create_backup() {
    local source="$1" backup_name="${2:-$(basename "$1")}"
    local backup_path="${BACKUP_DIR}/${INSTALL_TIMESTAMP}/${backup_name}"
    
    [[ ! -e "$source" ]] && return 0
    
    if [[ "$MODE" == "simulation" ]]; then
        log_sim "Backup $source -> $backup_path"
        return 0
    fi
    
    mkdir -p "$(dirname "$backup_path")"
    cp -a "$source" "$backup_path"
    audit_log "backup_created" "filesystem" "source=$source backup=$backup_path"
    log_info "Backup created: $backup_path"
}

# ─────────────────────────────────────────────────────────────────────────────
# System Detection
# ─────────────────────────────────────────────────────────────────────────────

check_macos() {
    if [[ "$(uname)" != "Darwin" ]]; then
        log_error "This installer is for macOS only."
        log_error "For Linux, use: ./install.sh"
        exit 1
    fi
    
    local macos_version
    macos_version=$(sw_vers -productVersion)
    log_info "macOS version: $macos_version"
    
    # Check minimum version (12.0 Monterey)
    local major_version
    major_version=$(echo "$macos_version" | cut -d. -f1)
    
    if [[ "$major_version" -lt 12 ]]; then
        log_warn "macOS 12 (Monterey) or later recommended"
    fi
}

check_dependencies() {
    local missing=()
    
    for cmd in python3 pip3 sqlite3 curl; do
        command -v "$cmd" &>/dev/null || missing+=("$cmd")
    done
    
    if [[ ${#missing[@]} -gt 0 ]]; then
        log_error "Missing required commands: ${missing[*]}"
        log_error "Install with: brew install ${missing[*]}"
        return 1
    fi
    
    # Check Python version
    local py_version
    py_version=$(python3 -c 'import sys; print(f"{sys.version_info.major}.{sys.version_info.minor}")')
    log_info "Python version: $py_version"
    
    if python3 -c "import sys; exit(0 if sys.version_info >= (3, 11) else 1)" 2>/dev/null; then
        log_success "Python 3.11+ detected"
    else
        log_warn "Python 3.11+ recommended (found $py_version)"
    fi
    
    return 0
}

# ─────────────────────────────────────────────────────────────────────────────
# Path Setup
# ─────────────────────────────────────────────────────────────────────────────

setup_paths() {
    INSTALL_DIR="$PREFIX"
    
    if [[ "$USER_INSTALL" == "true" ]]; then
        CONFIG_DIR="$HOME/.config/grey-optimizer"
        LOG_DIR="$HOME/Library/Logs/grey-optimizer"
        DATA_DIR="$HOME/Library/Application Support/grey-optimizer"
        BACKUP_DIR="$HOME/Library/Application Support/grey-optimizer/backups"
        LAUNCHD_DIR="$HOME/Library/LaunchAgents"
    else
        CONFIG_DIR="/etc/grey-optimizer"
        LOG_DIR="/var/log/grey-optimizer"
        DATA_DIR="/var/lib/grey-optimizer"
        BACKUP_DIR="/var/backup/grey-optimizer"
        LAUNCHD_DIR="/Library/LaunchDaemons"
    fi
    
    PLIST_FILE="${LAUNCHD_DIR}/${SERVICE_LABEL}.plist"
}

# ─────────────────────────────────────────────────────────────────────────────
# Installation Functions
# ─────────────────────────────────────────────────────────────────────────────

setup_directories() {
    local dirs=("$INSTALL_DIR" "$CONFIG_DIR" "$LOG_DIR" "$DATA_DIR" "$BACKUP_DIR")
    
    for dir in "${dirs[@]}"; do
        if [[ "$MODE" == "simulation" ]]; then
            log_sim "Create directory: $dir"
        else
            mkdir -p "$dir"
            log_info "Created: $dir"
        fi
    done
}

install_python_package() {
    log_info "Installing Python package..."
    
    if [[ "$MODE" == "simulation" ]]; then
        log_sim "Create venv at $INSTALL_DIR/.venv"
        log_sim "pip install aiosqlite aiohttp"
        log_sim "Create wrapper script at $INSTALL_DIR/run-daemon.sh"
        return 0
    fi
    
    python3 -m venv "$INSTALL_DIR/.venv"
    "$INSTALL_DIR/.venv/bin/pip" install --upgrade pip wheel
    "$INSTALL_DIR/.venv/bin/pip" install aiosqlite aiohttp
    
    # Copy daemon files
    cp -r "$PROJECT_ROOT/daemon" "$INSTALL_DIR/"
    
    # Create wrapper script (handles paths with spaces)
    cat > "$INSTALL_DIR/run-daemon.sh" <<EOF
#!/bin/bash
cd "$INSTALL_DIR"
exec "$INSTALL_DIR/.venv/bin/python" -u "$INSTALL_DIR/daemon/grey_daemon.py" "\$@"
EOF
    chmod +x "$INSTALL_DIR/run-daemon.sh"
    
    audit_log "python_installed" "installation" "venv=$INSTALL_DIR/.venv"
    log_success "Python package installed"
}

install_config() {
    local config_file="$CONFIG_DIR/config.yaml"
    
    create_backup "$config_file" "config.yaml"
    
    if [[ "$MODE" == "simulation" ]]; then
        log_sim "Install config to $config_file"
        return 0
    fi
    
    cat > "$config_file" <<'YAML'
# Grey Optimizer Configuration (macOS)
mode: simulation

targets:
  cpu: 90
  memory: 90
  disk: 90

api:
  host: 127.0.0.1
  port: 5000

profiles:
  conservative:
    cpu_nice: 10
    
  balanced:
    cpu_nice: 15
    
  aggressive:
    cpu_nice: 19

logging:
  level: INFO
  file: ~/Library/Logs/grey-optimizer/daemon.log
YAML
    
    audit_log "config_installed" "installation" "path=$config_file"
    log_success "Configuration installed"
}

install_launchd_plist() {
    log_info "Installing launchd plist..."
    
    create_backup "$PLIST_FILE" "${SERVICE_LABEL}.plist"
    
    if [[ "$MODE" == "simulation" ]]; then
        log_sim "Install plist to $PLIST_FILE"
        return 0
    fi
    
    local run_at_load="true"
    local keep_alive="true"
    
    cat > "$PLIST_FILE" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>${SERVICE_LABEL}</string>
    
    <key>ProgramArguments</key>
    <array>
        <string>${INSTALL_DIR}/run-daemon.sh</string>
    </array>
    
    <key>RunAtLoad</key>
    <${run_at_load}/>
    
    <key>KeepAlive</key>
    <dict>
        <key>SuccessfulExit</key>
        <false/>
    </dict>
    
    <key>WorkingDirectory</key>
    <string>${INSTALL_DIR}</string>
    
    <key>StandardOutPath</key>
    <string>${LOG_DIR}/daemon.log</string>
    
    <key>StandardErrorPath</key>
    <string>${LOG_DIR}/daemon.error.log</string>
    
    <key>EnvironmentVariables</key>
    <dict>
        <key>GREY_OPTIMIZER_CONFIG</key>
        <string>${CONFIG_DIR}/config.yaml</string>
    </dict>
    
    <key>ProcessType</key>
    <string>Background</string>
    
    <key>LowPriorityIO</key>
    <true/>
    
    <key>Nice</key>
    <integer>10</integer>
    
    <key>ThrottleInterval</key>
    <integer>10</integer>
</dict>
</plist>
EOF
    
    # Set permissions
    if [[ "$USER_INSTALL" == "false" ]]; then
        chown root:wheel "$PLIST_FILE"
        chmod 644 "$PLIST_FILE"
    fi
    
    audit_log "launchd_installed" "installation" "plist=$PLIST_FILE"
    log_success "launchd plist installed"
}

load_service() {
    log_info "Loading service..."
    
    if [[ "$MODE" == "simulation" ]]; then
        log_sim "launchctl bootstrap system $PLIST_FILE"
        return 0
    fi
    
    # Unload if already loaded (use bootout for newer macOS)
    if [[ "$USER_INSTALL" == "true" ]]; then
        launchctl bootout "gui/$(id -u)/${SERVICE_LABEL}" 2>/dev/null || true
        launchctl bootstrap "gui/$(id -u)" "$PLIST_FILE"
    else
        launchctl bootout "system/${SERVICE_LABEL}" 2>/dev/null || true
        launchctl bootstrap system "$PLIST_FILE"
    fi
    
    audit_log "service_loaded" "service" "label=$SERVICE_LABEL"
    log_success "Service loaded"
}

wait_for_health() {
    log_info "Waiting for service health..."
    
    if [[ "$MODE" == "simulation" ]]; then
        log_sim "Wait for $HEALTH_ENDPOINT"
        return 0
    fi
    
    local elapsed=0
    
    while [[ $elapsed -lt $HEALTH_TIMEOUT ]]; do
        if curl -sf "$HEALTH_ENDPOINT" >/dev/null 2>&1; then
            log_success "Health check passed"
            audit_log "health_check" "verification" "status=passed"
            return 0
        fi
        sleep "$HEALTH_RETRY_INTERVAL"
        elapsed=$((elapsed + HEALTH_RETRY_INTERVAL))
        echo -n "."
    done
    
    echo ""
    log_error "Health check failed after ${HEALTH_TIMEOUT}s"
    return 1
}

# ─────────────────────────────────────────────────────────────────────────────
# Rollback
# ─────────────────────────────────────────────────────────────────────────────

rollback_installation() {
    log_error "Installation failed, initiating rollback..."
    
    if [[ "$MODE" == "simulation" ]]; then
        log_sim "Rollback backups from $BACKUP_DIR/$INSTALL_TIMESTAMP"
        return 0
    fi
    
    launchctl unload "$PLIST_FILE" 2>/dev/null || true
    
    local backup_base="$BACKUP_DIR/$INSTALL_TIMESTAMP"
    if [[ -d "$backup_base" ]]; then
        for backup in "$backup_base"/*; do
            local name=$(basename "$backup")
            case "$name" in
                "${SERVICE_LABEL}.plist") cp -a "$backup" "$PLIST_FILE" ;;
                "config.yaml") cp -a "$backup" "$CONFIG_DIR/config.yaml" ;;
            esac
        done
    fi
    
    audit_log "rollback_completed" "installation" "backup_dir=$backup_base"
    log_warn "Rollback completed"
}

# ─────────────────────────────────────────────────────────────────────────────
# Consent Flow
# ─────────────────────────────────────────────────────────────────────────────

show_consent_checklist() {
    echo ""
    echo -e "${BOLD}═══════════════════════════════════════════════════════════════════${NC}"
    echo -e "${BOLD}       Grey Optimizer - macOS Live Mode Installation${NC}"
    echo -e "${BOLD}═══════════════════════════════════════════════════════════════════${NC}"
    echo ""
    echo "You are about to install Grey Optimizer in LIVE mode."
    echo ""
    echo -e "${YELLOW}Actions to be performed:${NC}"
    echo "  [1] Install files to: $INSTALL_DIR"
    echo "  [2] Create configuration in: $CONFIG_DIR"
    echo "  [3] Install launchd plist: $PLIST_FILE"
    echo "  [4] Load and start the service"
    echo ""
    echo -e "${GREEN}Safety measures:${NC}"
    echo "  ✓ All changes backed up to: $BACKUP_DIR"
    echo "  ✓ Rollback via: greyctl rollback"
    echo "  ✓ All actions logged to audit database"
    echo ""
    
    if [[ "$CONFIRM_LIVE" != "true" ]]; then
        echo -e "${RED}ERROR: Live mode requires --confirm-live flag${NC}"
        return 1
    fi
    return 0
}

# ─────────────────────────────────────────────────────────────────────────────
# Help
# ─────────────────────────────────────────────────────────────────────────────

show_help() {
    cat <<EOF
Grey Optimizer macOS Installer v${VERSION}

Usage: $0 [OPTIONS]

Options:
  --mode=MODE           simulation (default) or live
  --confirm-live        Required for live mode
  --preserve-logs       Keep existing logs
  --user-install        Install for current user only (~/Library/LaunchAgents)
  --prefix=PATH         Installation prefix (default: /usr/local/grey-optimizer)
  --help, -h            Show help

Examples:
  $0                                        # Simulation mode
  sudo $0 --mode=live --confirm-live        # System-wide live install
  $0 --mode=live --confirm-live --user-install  # User install

EOF
}

# ─────────────────────────────────────────────────────────────────────────────
# Argument Parsing
# ─────────────────────────────────────────────────────────────────────────────

parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --mode=*) MODE="${1#*=}"; shift ;;
            --confirm-live) CONFIRM_LIVE=true; shift ;;
            --preserve-logs) PRESERVE_LOGS=true; shift ;;
            --user-install) USER_INSTALL=true; shift ;;
            --prefix=*) PREFIX="${1#*=}"; shift ;;
            --help|-h) show_help; exit 0 ;;
            *) log_error "Unknown: $1"; exit 1 ;;
        esac
    done
    
    if [[ "$MODE" != "simulation" && "$MODE" != "live" ]]; then
        log_error "Invalid mode: $MODE"
        exit 1
    fi
    
    setup_paths
}

# ─────────────────────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────────────────────

main() {
    parse_args "$@"
    
    echo ""
    echo -e "${BOLD}Grey Optimizer macOS Installer v${VERSION}${NC}"
    echo -e "Mode: ${CYAN}${MODE}${NC}"
    echo ""
    
    # Checks
    check_macos
    check_dependencies
    
    if [[ "$MODE" == "live" && "$USER_INSTALL" == "false" && $EUID -ne 0 ]]; then
        log_error "System-wide live mode requires sudo"
        exit 1
    fi
    
    if [[ "$MODE" == "live" ]]; then
        show_consent_checklist || exit 1
    else
        log_info "Running in SIMULATION mode"
    fi
    
    # Initialize audit
    init_audit_db "$DATA_DIR/audit.db"
    create_checkpoint "pre_install" "$(date -u +%s)|$VERSION|$MODE"
    
    # Install
    trap 'rollback_installation' ERR
    
    audit_log "install_started" "installation" "version=$VERSION mode=$MODE"
    
    setup_directories
    install_python_package
    install_config
    install_launchd_plist
    load_service
    wait_for_health
    
    create_checkpoint "post_install" "$(date -u +%s)|$VERSION|success"
    audit_log "install_completed" "installation" "success=true"
    
    trap - ERR
    
    # Success
    echo ""
    echo -e "${GREEN}Grey Optimizer installed successfully!${NC}"
    echo ""
    echo "Commands:"
    echo "  greyctl status   - Check status"
    echo "  greyctl health   - Health check"
    echo "  greyctl rollback - Rollback changes"
    echo ""
    echo "Service management:"
    echo "  launchctl list | grep grey"
    echo "  launchctl unload $PLIST_FILE"
    echo ""
    
    if [[ "$MODE" == "simulation" ]]; then
        echo -e "${YELLOW}NOTE: Ran in simulation mode.${NC}"
        echo "For real install: sudo $0 --mode=live --confirm-live"
    fi
}

main "$@"
