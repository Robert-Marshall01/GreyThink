#!/bin/bash
# ═══════════════════════════════════════════════════════════════════════════════
# Grey Optimizer - Linux Installer
# ═══════════════════════════════════════════════════════════════════════════════
#
# Installs Grey Optimizer as a systemd service on Linux systems.
#
# Usage:
#   sudo ./install_linux.sh                    # Install in simulation mode
#   sudo ./install_linux.sh --live             # Install in live mode
#   sudo ./install_linux.sh --uninstall        # Uninstall
#
# Requirements:
#   - Python 3.8+ with venv support
#   - systemd
#   - Root privileges
#
# ═══════════════════════════════════════════════════════════════════════════════

set -euo pipefail

# ─────────────────────────────────────────────────────────────────────────────
# Configuration
# ─────────────────────────────────────────────────────────────────────────────

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
VERSION="2.0.0"

# Installation paths
INSTALL_DIR="/opt/grey-optimizer"
CONFIG_DIR="/etc/grey-optimizer"
DATA_DIR="/var/lib/grey-optimizer"
LOG_DIR="/var/log"
LOG_FILE="$LOG_DIR/grey-optimizer.log"

# Service configuration
SERVICE_NAME="grey-optimizer"
SERVICE_FILE="/etc/systemd/system/${SERVICE_NAME}.service"
HEALTH_URL="http://127.0.0.1:8090/health"
HEALTH_TIMEOUT=30

# Default mode
MODE="simulation"
ENFORCEMENT_INTERVAL=60

# ─────────────────────────────────────────────────────────────────────────────
# Colors
# ─────────────────────────────────────────────────────────────────────────────

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

log_info()    { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[OK]${NC} $1"; }
log_warn()    { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error()   { echo -e "${RED}[ERROR]${NC} $1" >&2; }

# ─────────────────────────────────────────────────────────────────────────────
# Argument Parsing
# ─────────────────────────────────────────────────────────────────────────────

show_help() {
    cat <<EOF
Grey Optimizer Linux Installer v${VERSION}

Usage: sudo $0 [OPTIONS]

Options:
  --live                Install in live mode (applies real changes)
  --simulation          Install in simulation mode (default)
  --interval SECONDS    Enforcement interval (default: 60)
  --uninstall           Uninstall the service
  --help, -h            Show this help

Examples:
  sudo $0                        # Install in simulation mode
  sudo $0 --live                 # Install in live mode
  sudo $0 --interval 30          # Enforce every 30 seconds
  sudo $0 --uninstall            # Remove installation

EOF
    exit 0
}

UNINSTALL=false

while [[ $# -gt 0 ]]; do
    case "$1" in
        --live)
            MODE="live"
            shift
            ;;
        --simulation)
            MODE="simulation"
            shift
            ;;
        --interval)
            ENFORCEMENT_INTERVAL="$2"
            shift 2
            ;;
        --uninstall)
            UNINSTALL=true
            shift
            ;;
        --help|-h)
            show_help
            ;;
        *)
            log_error "Unknown option: $1"
            exit 1
            ;;
    esac
done

# ─────────────────────────────────────────────────────────────────────────────
# Preflight Checks
# ─────────────────────────────────────────────────────────────────────────────

check_root() {
    if [[ $EUID -ne 0 ]]; then
        log_error "This script must be run as root (use sudo)"
        exit 1
    fi
}

check_systemd() {
    if ! command -v systemctl &> /dev/null; then
        log_error "systemd is required but not found"
        exit 1
    fi
}

check_python() {
    if ! command -v python3 &> /dev/null; then
        log_error "Python 3 is required but not found"
        exit 1
    fi
    
    local version
    version=$(python3 -c "import sys; print(f'{sys.version_info.major}.{sys.version_info.minor}')")
    log_info "Found Python $version"
}

# ─────────────────────────────────────────────────────────────────────────────
# Uninstall
# ─────────────────────────────────────────────────────────────────────────────

do_uninstall() {
    log_info "Uninstalling Grey Optimizer..."
    
    # Stop service
    if systemctl is-active --quiet "$SERVICE_NAME" 2>/dev/null; then
        log_info "Stopping service..."
        systemctl stop "$SERVICE_NAME"
        log_success "Service stopped"
    fi
    
    # Disable service
    if systemctl is-enabled --quiet "$SERVICE_NAME" 2>/dev/null; then
        log_info "Disabling service..."
        systemctl disable "$SERVICE_NAME"
        log_success "Service disabled"
    fi
    
    # Remove service file
    if [[ -f "$SERVICE_FILE" ]]; then
        rm -f "$SERVICE_FILE"
        systemctl daemon-reload
        log_success "Service file removed"
    fi
    
    # Remove installation directory
    if [[ -d "$INSTALL_DIR" ]]; then
        rm -rf "$INSTALL_DIR"
        log_success "Removed $INSTALL_DIR"
    fi
    
    # Keep data and logs for audit purposes
    log_warn "Data preserved at: $DATA_DIR"
    log_warn "Logs preserved at: $LOG_FILE"
    
    echo ""
    log_success "Grey Optimizer uninstalled successfully"
    exit 0
}

# ─────────────────────────────────────────────────────────────────────────────
# Installation
# ─────────────────────────────────────────────────────────────────────────────

create_directories() {
    log_info "Creating directories..."
    
    mkdir -p "$INSTALL_DIR"
    mkdir -p "$CONFIG_DIR"
    mkdir -p "$DATA_DIR"
    mkdir -p "$DATA_DIR/artifacts"
    
    log_success "Directories created"
}

setup_venv() {
    log_info "Setting up Python virtual environment..."
    
    local venv_dir="$INSTALL_DIR/.venv"
    
    if [[ ! -d "$venv_dir" ]]; then
        python3 -m venv "$venv_dir"
    fi
    
    # Install dependencies
    "$venv_dir/bin/pip" install --quiet --upgrade pip
    "$venv_dir/bin/pip" install --quiet aiosqlite aiohttp
    
    log_success "Virtual environment ready"
}

copy_files() {
    log_info "Copying daemon files..."
    
    # Copy daemon module
    mkdir -p "$INSTALL_DIR/daemon"
    cp -r "$PROJECT_DIR/daemon/"*.py "$INSTALL_DIR/daemon/" 2>/dev/null || true
    
    # Create __init__.py if missing
    touch "$INSTALL_DIR/daemon/__init__.py"
    
    # Copy web dashboard if exists
    if [[ -d "$PROJECT_DIR/web" ]]; then
        cp -r "$PROJECT_DIR/web" "$INSTALL_DIR/"
    fi
    
    log_success "Files copied"
}

create_wrapper_script() {
    log_info "Creating daemon wrapper script..."
    
    local wrapper="$INSTALL_DIR/run-daemon.sh"
    local daemon_args="--simulation"
    
    if [[ "$MODE" == "live" ]]; then
        daemon_args="--confirm-live"
    fi
    
    cat > "$wrapper" << EOF
#!/bin/bash
# Grey Optimizer Daemon Wrapper
# Generated: $(date -Iseconds)

cd "$INSTALL_DIR"
exec "$INSTALL_DIR/.venv/bin/python" -m daemon.grey_daemon $daemon_args --enforcement-interval $ENFORCEMENT_INTERVAL
EOF
    
    chmod +x "$wrapper"
    log_success "Wrapper script created"
}

create_systemd_service() {
    log_info "Creating systemd service..."
    
    cat > "$SERVICE_FILE" << EOF
[Unit]
Description=Grey Optimizer Daemon
Documentation=https://github.com/grey-optimizer/grey-optimizer
After=network.target

[Service]
Type=simple
ExecStart=$INSTALL_DIR/run-daemon.sh
Restart=on-failure
RestartSec=10
WorkingDirectory=$INSTALL_DIR
StandardOutput=append:$LOG_FILE
StandardError=append:$LOG_FILE
User=root
Environment=PYTHONUNBUFFERED=1

# Watchdog
WatchdogSec=120
NotifyAccess=main

[Install]
WantedBy=multi-user.target
EOF

    log_success "Service file created"
}

enable_and_start() {
    log_info "Enabling and starting service..."
    
    systemctl daemon-reload
    systemctl enable "$SERVICE_NAME"
    systemctl start "$SERVICE_NAME"
    
    log_success "Service started"
}

wait_for_health() {
    log_info "Waiting for daemon health check..."
    
    local attempts=0
    local max_attempts=$((HEALTH_TIMEOUT / 2))
    
    while [[ $attempts -lt $max_attempts ]]; do
        if curl -sf "$HEALTH_URL" > /dev/null 2>&1; then
            log_success "Health check passed"
            return 0
        fi
        
        attempts=$((attempts + 1))
        sleep 2
    done
    
    log_warn "Health check timed out (service may still be starting)"
    return 1
}

verify_installation() {
    log_info "Verifying installation..."
    
    local errors=0
    
    # Check service status
    if systemctl is-active --quiet "$SERVICE_NAME"; then
        log_success "Service is running"
    else
        log_error "Service is not running"
        errors=$((errors + 1))
    fi
    
    # Check if enabled
    if systemctl is-enabled --quiet "$SERVICE_NAME"; then
        log_success "Service is enabled for boot"
    else
        log_error "Service is not enabled"
        errors=$((errors + 1))
    fi
    
    # Check log file
    if [[ -f "$LOG_FILE" ]]; then
        log_success "Log file exists: $LOG_FILE"
    else
        log_warn "Log file not yet created"
    fi
    
    # Try health endpoint
    if curl -sf "$HEALTH_URL" > /dev/null 2>&1; then
        log_success "HTTP health endpoint responding"
    else
        log_warn "HTTP health endpoint not responding yet"
    fi
    
    return $errors
}

print_summary() {
    echo ""
    echo -e "${GREEN}════════════════════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}  Grey Optimizer installed successfully!${NC}"
    echo -e "${GREEN}════════════════════════════════════════════════════════════════${NC}"
    echo ""
    echo "Mode:           $MODE"
    echo "Interval:       ${ENFORCEMENT_INTERVAL}s"
    echo "Installation:   $INSTALL_DIR"
    echo "Configuration:  $CONFIG_DIR"
    echo "Data:           $DATA_DIR"
    echo "Log file:       $LOG_FILE"
    echo "Health URL:     $HEALTH_URL"
    echo ""
    echo "Commands:"
    echo "  Status:       sudo systemctl status $SERVICE_NAME"
    echo "  Logs:         sudo journalctl -u $SERVICE_NAME -f"
    echo "  Log file:     sudo tail -f $LOG_FILE"
    echo "  Stop:         sudo systemctl stop $SERVICE_NAME"
    echo "  Start:        sudo systemctl start $SERVICE_NAME"
    echo "  Uninstall:    sudo $0 --uninstall"
    echo ""
}

# ─────────────────────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────────────────────

main() {
    echo ""
    echo -e "${BOLD}Grey Optimizer Linux Installer v${VERSION}${NC}"
    echo ""
    
    check_root
    
    if [[ "$UNINSTALL" == "true" ]]; then
        do_uninstall
    fi
    
    check_systemd
    check_python
    
    # Stop existing service if running
    if systemctl is-active --quiet "$SERVICE_NAME" 2>/dev/null; then
        log_info "Stopping existing service..."
        systemctl stop "$SERVICE_NAME"
    fi
    
    create_directories
    setup_venv
    copy_files
    create_wrapper_script
    create_systemd_service
    enable_and_start
    
    # Wait for service to be healthy
    sleep 3
    wait_for_health || true
    
    verify_installation || true
    print_summary
}

main "$@"
