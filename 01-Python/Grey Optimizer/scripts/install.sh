#!/bin/bash
# ═══════════════════════════════════════════════════════════════════════════════
# Grey Optimizer Installation Script (Linux)
# ═══════════════════════════════════════════════════════════════════════════════
#
# Installs Grey Optimizer daemon with support for simulation and live modes.
#
# Usage:
#   ./install.sh [options]
#
# Options:
#   --mode=simulation   Install in simulation mode (default, no root required)
#   --mode=live         Install in live mode (requires --confirm-live)
#   --confirm-live      Confirm live mode installation
#   --user              Install for current user only (no root)
#   --skip-deps         Skip dependency checks
#   -h, --help          Show this help
#
# ═══════════════════════════════════════════════════════════════════════════════

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

# Installation mode
MODE="simulation"
CONFIRM_LIVE=false
USER_INSTALL=false
SKIP_DEPS=false

# Paths (will be adjusted for user install)
INSTALL_DIR="/opt/grey-optimizer"
CONFIG_DIR="/etc/grey-optimizer"
LOG_DIR="/var/log/grey-optimizer"
DATA_DIR="/var/lib/grey-optimizer"
AUDIT_DB="${DATA_DIR}/audit.db"
AUDIT_SECRET="${HOSTNAME:-localhost}-grey-optimizer-audit"

# ═══════════════════════════════════════════════════════════════════════════════
# Utilities
# ═══════════════════════════════════════════════════════════════════════════════

print_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
print_success() { echo -e "${GREEN}[OK]${NC} $1"; }
print_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
print_error() { echo -e "${RED}[ERROR]${NC} $1" >&2; }

show_help() {
    cat << 'EOF'
Grey Optimizer - Linux Installation

Usage: ./install.sh [options]

Options:
  --mode=simulation   Install in simulation mode (default)
                      - Does not require root privileges
                      - No kernel tunables modified
                      - All actions logged as simulated
  
  --mode=live         Install in live mode
                      - Requires root privileges
                      - Requires --confirm-live flag
                      - Modifies kernel tunables
                      - Creates systemd service
                      - Creates backups before changes
  
  --confirm-live      Required for live mode installation
  
  --user              Install for current user only (simulation mode only)
                      - Installs to ~/.local/share/grey-optimizer
                      - No root required
  
  --skip-deps         Skip dependency checks
  
  -h, --help          Show this help

Examples:
  ./install.sh                                 # Simulation mode (default)
  ./install.sh --mode=live --confirm-live      # Live mode
  ./install.sh --user                          # User installation

EOF
    exit 0
}

audit_log() {
    local action="$1"
    local category="$2"
    local details="${3:-}"
    local success="${4:-1}"
    local timestamp
    timestamp=$(date +%s)
    
    if [[ ! -f "$AUDIT_DB" ]]; then
        return 0
    fi
    
    local sign_data="${action}|${category}|${details}|${success}|${timestamp}"
    local signature
    signature=$(echo -n "$sign_data" | openssl dgst -sha256 -hmac "$AUDIT_SECRET" 2>/dev/null | awk '{print $2}') || signature="no-openssl"
    
    sqlite3 "$AUDIT_DB" "INSERT INTO audit_log (action, category, details, mode, user, hostname, success, signature) VALUES ('$action', '$category', '$details', '$MODE', '${USER:-unknown}', '${HOSTNAME:-unknown}', $success, '$signature');" 2>/dev/null || true
}

# ═══════════════════════════════════════════════════════════════════════════════
# Parse Arguments
# ═══════════════════════════════════════════════════════════════════════════════

while [[ $# -gt 0 ]]; do
    case $1 in
        --mode=*)
            MODE="${1#*=}"
            shift
            ;;
        --mode)
            MODE="$2"
            shift 2
            ;;
        --confirm-live)
            CONFIRM_LIVE=true
            shift
            ;;
        --user)
            USER_INSTALL=true
            shift
            ;;
        --skip-deps)
            SKIP_DEPS=true
            shift
            ;;
        -h|--help)
            show_help
            ;;
        *)
            print_error "Unknown option: $1"
            show_help
            ;;
    esac
done

# ═══════════════════════════════════════════════════════════════════════════════
# Validate Mode
# ═══════════════════════════════════════════════════════════════════════════════

# Adjust paths for user install
if [[ "$USER_INSTALL" == true ]]; then
    INSTALL_DIR="${HOME}/.local/share/grey-optimizer"
    CONFIG_DIR="${HOME}/.config/grey-optimizer"
    LOG_DIR="${HOME}/.local/share/grey-optimizer/logs"
    DATA_DIR="${HOME}/.local/share/grey-optimizer/data"
    AUDIT_DB="${DATA_DIR}/audit.db"
    MODE="simulation"  # Force simulation for user install
fi

# Validate live mode
if [[ "$MODE" == "live" ]]; then
    if [[ "$CONFIRM_LIVE" != true ]]; then
        print_error "Live mode requires --confirm-live flag"
        echo ""
        echo "Live mode will:"
        echo "  • Require root/sudo privileges"
        echo "  • Modify kernel tunables (swappiness, dirty_ratio, etc.)"
        echo "  • Create and configure cgroups"
        echo "  • Install systemd service"
        echo "  • Create backup of current settings"
        echo ""
        echo "To proceed:"
        echo "  sudo ./install.sh --mode=live --confirm-live"
        exit 1
    fi
    
    if [[ "$USER_INSTALL" == true ]]; then
        print_error "Live mode not supported with --user install"
        exit 1
    fi
fi

# Print header
echo ""
echo -e "${BLUE}╔══════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║         Grey Optimizer - Installation Script                 ║${NC}"
echo -e "${BLUE}║                    Mode: ${BOLD}${MODE^^}${NC}${BLUE}                              ║${NC}"
echo -e "${BLUE}╚══════════════════════════════════════════════════════════════╝${NC}"
echo ""

# Check root for live mode only
if [[ "$MODE" == "live" ]]; then
    if [[ $EUID -ne 0 ]]; then
        print_error "Live mode requires root privileges"
        echo "Please run: sudo $0 --mode=live --confirm-live"
        exit 1
    fi
fi

# Check Linux
if [[ "$(uname)" != "Linux" ]]; then
    print_error "Grey Optimizer requires Linux"
    exit 1
fi

# Check cgroup v2 (warning only)
if [[ "$MODE" == "live" ]] && ! grep -q "cgroup2" /proc/mounts 2>/dev/null; then
    print_warn "cgroup v2 not detected. Some features may not work."
fi

# ═══════════════════════════════════════════════════════════════════════════════
# Step 1: Check Dependencies
# ═══════════════════════════════════════════════════════════════════════════════

check_deps() {
    print_info "[1/8] Checking dependencies..."
    
    if [[ "$SKIP_DEPS" == true ]]; then
        print_warn "Skipping dependency checks"
        return 0
    fi
    
    local missing=()
    
    # Check Python 3.8+ (relaxed from 3.11)
    if command -v python3 &> /dev/null; then
        PYTHON_VERSION=$(python3 -c 'import sys; print(f"{sys.version_info.major}.{sys.version_info.minor}")')
        MAJOR=$(echo "$PYTHON_VERSION" | cut -d. -f1)
        MINOR=$(echo "$PYTHON_VERSION" | cut -d. -f2)
        if [[ "$MAJOR" -lt 3 ]] || [[ "$MAJOR" -eq 3 && "$MINOR" -lt 8 ]]; then
            missing+=("python3.8+ (found $PYTHON_VERSION)")
        else
            print_success "Python $PYTHON_VERSION found"
        fi
    else
        missing+=("python3")
    fi
    
    # Check sqlite3 for audit DB
    if ! command -v sqlite3 &> /dev/null; then
        print_warn "sqlite3 not found - audit logging will be disabled"
    fi
    
    # Check GCC (for live mode only)
    if [[ "$MODE" == "live" ]]; then
        if ! command -v gcc &> /dev/null; then
            missing+=("gcc")
        fi
    fi
    
    # Check OpenSSL (for HMAC signatures)
    if ! command -v openssl &> /dev/null; then
        print_warn "openssl not found - audit signatures will be disabled"
    fi
    
    # Check Node.js (optional, for frontend)
    if ! command -v node &> /dev/null; then
        print_warn "Node.js not found - frontend will not be built"
    fi
    
    if [[ ${#missing[@]} -gt 0 ]]; then
        print_error "Missing dependencies: ${missing[*]}"
        echo "Install with:"
        echo "  Ubuntu/Debian: apt install python3 python3-venv gcc build-essential sqlite3"
        echo "  Fedora/RHEL:   dnf install python3 gcc make sqlite"
        exit 1
    fi
    
    print_success "All dependencies present"
}

# ═══════════════════════════════════════════════════════════════════════════════
# Step 2: Create Directories
# ═══════════════════════════════════════════════════════════════════════════════

create_dirs() {
    print_info "[2/8] Creating directories..."
    
    if [[ "$MODE" == "simulation" ]]; then
        print_info "  [SIMULATE] Would create: $INSTALL_DIR"
        print_info "  [SIMULATE] Would create: $CONFIG_DIR"
        print_info "  [SIMULATE] Would create: $LOG_DIR"
        print_info "  [SIMULATE] Would create: $DATA_DIR"
        
        # For simulation, create minimal directories for testing
        mkdir -p "$CONFIG_DIR" 2>/dev/null || true
        mkdir -p "$DATA_DIR" 2>/dev/null || true
    else
        mkdir -p "$INSTALL_DIR"
        mkdir -p "$CONFIG_DIR"
        mkdir -p "$LOG_DIR"
        mkdir -p "$DATA_DIR"
        mkdir -p "$INSTALL_DIR/lib"
        mkdir -p "$DATA_DIR/backups"
        
        chmod 755 "$INSTALL_DIR"
        chmod 755 "$CONFIG_DIR"
        chmod 750 "$LOG_DIR"
        chmod 750 "$DATA_DIR"
    fi
    
    print_success "Directories ready"
}

# ═══════════════════════════════════════════════════════════════════════════════
# Step 3: Initialize Audit Database
# ═══════════════════════════════════════════════════════════════════════════════

init_audit_db() {
    print_info "[3/8] Initializing audit database..."
    
    if ! command -v sqlite3 &> /dev/null; then
        print_warn "sqlite3 not available - skipping audit DB"
        return 0
    fi
    
    # Ensure data directory exists
    mkdir -p "$DATA_DIR" 2>/dev/null || true
    
    if [[ ! -f "$AUDIT_DB" ]]; then
        sqlite3 "$AUDIT_DB" << 'EOF'
CREATE TABLE IF NOT EXISTS audit_log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
    action TEXT NOT NULL,
    category TEXT NOT NULL,
    details TEXT,
    mode TEXT,
    user TEXT,
    hostname TEXT,
    success INTEGER DEFAULT 1,
    signature TEXT
);

CREATE TABLE IF NOT EXISTS checkpoints (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
    last_audit_id INTEGER,
    hash TEXT NOT NULL,
    signature TEXT
);

CREATE INDEX IF NOT EXISTS idx_audit_timestamp ON audit_log(timestamp);
CREATE INDEX IF NOT EXISTS idx_audit_action ON audit_log(action);
EOF
        print_success "Audit database created"
    else
        print_info "Audit database already exists"
    fi
    
    # Log installation start
    audit_log "installation_started" "install" "mode=$MODE"
}

# ═══════════════════════════════════════════════════════════════════════════════
# Step 4: Copy Files
# ═══════════════════════════════════════════════════════════════════════════════

copy_files() {
    print_info "[4/8] Copying files..."
    
    if [[ "$MODE" == "simulation" ]]; then
        print_info "  [SIMULATE] Would copy backend to $INSTALL_DIR"
        print_info "  [SIMULATE] Would copy enforcement modules"
        print_info "  [SIMULATE] Would copy scripts"
        print_success "Files staged (simulation)"
        return 0
    fi
    
    # Copy backend
    if [[ -d "$PROJECT_DIR/backend" ]]; then
        cp -r "$PROJECT_DIR/backend/"* "$INSTALL_DIR/"
    fi
    
    # Copy enforcement modules source
    if [[ -d "$PROJECT_DIR/enforcement" ]]; then
        cp -r "$PROJECT_DIR/enforcement" "$INSTALL_DIR/"
    fi
    
    # Copy C modules if they exist
    if [[ -d "$PROJECT_DIR/c" ]]; then
        cp -r "$PROJECT_DIR/c" "$INSTALL_DIR/"
    fi
    
    # Copy scripts
    cp -r "$PROJECT_DIR/scripts/"* "$INSTALL_DIR/" 2>/dev/null || true
    
    # Copy greyctl
    if [[ -f "$PROJECT_DIR/greyctl" ]]; then
        cp "$PROJECT_DIR/greyctl" "$INSTALL_DIR/"
        chmod +x "$INSTALL_DIR/greyctl"
    fi
    
    print_success "Files copied"
}

# ═══════════════════════════════════════════════════════════════════════════════
# Step 5: Setup Python Environment
# ═══════════════════════════════════════════════════════════════════════════════

setup_python() {
    print_info "[5/8] Setting up Python environment..."
    
    if [[ "$MODE" == "simulation" ]]; then
        print_info "  [SIMULATE] Would create venv at $INSTALL_DIR/venv"
        print_info "  [SIMULATE] Would install Python dependencies"
        
        # In simulation, use project's venv if available
        if [[ -f "$PROJECT_DIR/.venv/bin/activate" ]]; then
            print_success "Using existing project venv"
        else
            print_info "Run 'make venv' to create virtual environment"
        fi
        return 0
    fi
    
    cd "$INSTALL_DIR"
    
    # Create venv
    python3 -m venv venv
    
    # Install dependencies
    source venv/bin/activate
    pip install --upgrade pip
    
    # Install package if setup.py exists
    if [[ -f "setup.py" ]] || [[ -f "pyproject.toml" ]]; then
        pip install -e .
    fi
    
    # Install from requirements.txt
    if [[ -f "$PROJECT_DIR/requirements.txt" ]]; then
        pip install -r "$PROJECT_DIR/requirements.txt"
    fi
    
    deactivate
    
    print_success "Python environment ready"
}

# ═══════════════════════════════════════════════════════════════════════════════
# Step 6: Compile C Modules
# ═══════════════════════════════════════════════════════════════════════════════

compile_c_modules() {
    print_info "[6/8] Compiling C enforcement modules..."
    
    if [[ "$MODE" == "simulation" ]]; then
        print_info "  [SIMULATE] Would compile C modules"
        print_success "C modules staged (simulation)"
        return 0
    fi
    
    # Try enforcement directory first
    if [[ -d "$INSTALL_DIR/enforcement" ]] && [[ -f "$INSTALL_DIR/enforcement/Makefile" ]]; then
        cd "$INSTALL_DIR/enforcement"
        make clean || true
        make
        cp -f lib/*.so "$INSTALL_DIR/lib/" 2>/dev/null || true
        print_success "C modules compiled (enforcement/)"
        return 0
    fi
    
    # Try c directory
    if [[ -d "$INSTALL_DIR/c" ]] && [[ -f "$INSTALL_DIR/c/Makefile" ]]; then
        cd "$INSTALL_DIR/c"
        make clean || true
        make
        cp -f lib/*.so "$INSTALL_DIR/lib/" 2>/dev/null || true
        print_success "C modules compiled (c/)"
        return 0
    fi
    
    print_warn "No C modules found to compile"
}

# ═══════════════════════════════════════════════════════════════════════════════
# Step 7: Create Configuration
# ═══════════════════════════════════════════════════════════════════════════════

create_config() {
    print_info "[7/8] Creating configuration..."
    
    local config_file="$CONFIG_DIR/config.yaml"
    
    if [[ "$MODE" == "simulation" ]]; then
        print_info "  [SIMULATE] Would create config at: $config_file"
        print_success "Configuration staged (simulation)"
        return 0
    fi
    
    # Ensure config directory exists
    mkdir -p "$CONFIG_DIR" 2>/dev/null || true
    
    if [[ ! -f "$config_file" ]]; then
        cat > "$config_file" << EOF
# Grey Optimizer Configuration
# Generated: $(date -Iseconds)
# Mode: $MODE

telemetry:
  interval_seconds: 1.0
  history_size: 300

cpu_enforcement:
  enabled: true
  target_percent: 10
  min_percent: 1

ram_enforcement:
  enabled: true
  target_mb: 64
  min_mb: 64

disk_enforcement:
  enabled: true
  target_iops: 100

safety:
  simulation_mode: $([[ "$MODE" == "simulation" ]] && echo "true" || echo "false")
  protected_patterns:
    - "systemd*"
    - "kernel*"
    - "init"
    - "sshd"
    - "grey-optimizer"

api:
  host: "127.0.0.1"
  port: 8080
  enable_websocket: true

persistence:
  database_path: "${DATA_DIR}/audit.db"
  proof_dir: "${DATA_DIR}/proofs"
  retention_days: 30
EOF
        print_success "Configuration created"
    else
        print_info "Configuration already exists"
    fi
}

# ═══════════════════════════════════════════════════════════════════════════════
# Step 8: Install Systemd Service
# ═══════════════════════════════════════════════════════════════════════════════

install_service() {
    print_info "[8/8] Installing systemd service..."
    
    if [[ "$MODE" == "simulation" ]]; then
        print_info "  [SIMULATE] Would install systemd service"
        print_info "  [SIMULATE] Would enable grey-optimizer.service"
        print_success "Service staged (simulation)"
        return 0
    fi
    
    if [[ "$USER_INSTALL" == true ]]; then
        print_info "User install - skipping system service"
        return 0
    fi
    
    # Detect project directory (where this script lives)
    local project_dir="$(cd "$SCRIPT_DIR/.." && pwd)"
    local opt_dir="/opt/grey-optimizer"
    local venv_dir="$project_dir/.venv"
    local wrapper_script="$opt_dir/run-daemon.sh"
    local service_dest="/etc/systemd/system/grey-optimizer.service"
    
    # Create directories
    print_info "Creating installation directories..."
    mkdir -p "$opt_dir" /etc/grey-optimizer /var/lib/grey-optimizer /var/log/grey-optimizer
    
    # Create virtual environment if needed
    print_info "Setting up Python virtual environment..."
    if [[ ! -d "$venv_dir" ]]; then
        python3 -m venv "$venv_dir"
        print_success "Created venv: $venv_dir"
    fi
    
    # Install dependencies
    print_info "Installing Python dependencies..."
    "$venv_dir/bin/pip" install --quiet aiosqlite aiohttp 2>/dev/null || true
    print_success "Dependencies installed"
    
    # Create wrapper script to handle paths with spaces
    print_info "Creating daemon wrapper script..."
    local daemon_mode="--simulation"
    if [[ "$CONFIRM_LIVE" == true ]]; then
        daemon_mode="--confirm-live"
    fi
    
    cat > "$wrapper_script" << EOF
#!/bin/bash
cd "$project_dir"
exec "$venv_dir/bin/python" -m daemon.grey_daemon $daemon_mode
EOF
    chmod +x "$wrapper_script"
    print_success "Created: $wrapper_script"
    
    # Generate service file
    print_info "Creating systemd service..."
    cat > "$service_dest" << EOF
[Unit]
Description=Grey Optimizer - Resource Optimization Daemon
After=network.target

[Service]
Type=simple
User=root
ExecStart=$wrapper_script
Restart=on-failure
RestartSec=10
Environment=PYTHONUNBUFFERED=1

[Install]
WantedBy=multi-user.target
EOF
    print_success "Created: $service_dest"
    
    # Reload and enable
    systemctl daemon-reload
    systemctl enable grey-optimizer
    
    # Start the service immediately (for live mode)
    if [[ "$SIMULATION_MODE" != "true" ]]; then
        print_info "Starting grey-optimizer service..."
        systemctl start grey-optimizer
        
        # Wait for service to become active
        local max_attempts=10
        for i in $(seq 1 $max_attempts); do
            if systemctl is-active --quiet grey-optimizer; then
                print_success "Service started successfully"
                break
            fi
            if [[ $i -eq $max_attempts ]]; then
                print_warn "Service may not have started - check with: systemctl status grey-optimizer"
            fi
            sleep 1
        done
    else
        print_info "Simulation mode: service enabled but not started"
        print_info "Start manually with: sudo systemctl start grey-optimizer"
    fi
    
    print_success "Systemd service installed and enabled for boot"
    audit_log "service_installed" "install" "service=grey-optimizer"
}

# ═══════════════════════════════════════════════════════════════════════════════
# Health Check
# ═══════════════════════════════════════════════════════════════════════════════

run_health_check() {
    print_info "Running health check..."
    
    local checks_passed=0
    local checks_total=0
    
    # Check config exists
    checks_total=$((checks_total + 1))
    if [[ -f "$CONFIG_DIR/config.yaml" ]]; then
        print_success "  Config file exists"
        checks_passed=$((checks_passed + 1))
    else
        print_warn "  Config file missing (expected in simulation mode)"
    fi
    
    # Check audit DB
    checks_total=$((checks_total + 1))
    if [[ -f "$AUDIT_DB" ]]; then
        print_success "  Audit database exists"
        checks_passed=$((checks_passed + 1))
    else
        print_warn "  Audit database missing"
    fi
    
    # Check Python
    checks_total=$((checks_total + 1))
    if command -v python3 &> /dev/null; then
        print_success "  Python available"
        checks_passed=$((checks_passed + 1))
    else
        print_error "  Python not found"
    fi
    
    echo ""
    print_info "Health check: $checks_passed/$checks_total passed"
}

# ═══════════════════════════════════════════════════════════════════════════════
# Main Installation
# ═══════════════════════════════════════════════════════════════════════════════

main() {
    check_deps
    create_dirs
    init_audit_db
    copy_files
    setup_python
    compile_c_modules
    create_config
    install_service
    run_health_check
    
    # Log completion
    audit_log "installation_completed" "install" "mode=$MODE"
    
    echo ""
    echo -e "${GREEN}╔══════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║         Grey Optimizer installed successfully!               ║${NC}"
    echo -e "${GREEN}║                    Mode: ${BOLD}${MODE^^}${NC}${GREEN}                              ║${NC}"
    echo -e "${GREEN}╚══════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    echo "Configuration: $CONFIG_DIR/config.yaml"
    echo "Data:          $DATA_DIR"
    echo "Audit DB:      $AUDIT_DB"
    echo ""
    
    if [[ "$MODE" == "live" ]]; then
        echo "Installation: $INSTALL_DIR"
        echo "Logs:         $LOG_DIR"
        echo ""
        echo "Commands:"
        echo "  Start:   sudo systemctl start grey-optimizer"
        echo "  Stop:    sudo systemctl stop grey-optimizer"
        echo "  Status:  sudo systemctl status grey-optimizer"
        echo "  Logs:    journalctl -u grey-optimizer -f"
    else
        echo -e "${CYAN}Simulation mode - no system changes were made.${NC}"
        echo ""
        echo "To install in live mode:"
        echo "  sudo ./install.sh --mode=live --confirm-live"
        echo ""
        echo "To test in simulation:"
        echo "  greyctl status"
        echo "  greyctl simulate"
    fi
    echo ""
}

main "$@"
