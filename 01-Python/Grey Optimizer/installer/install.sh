#!/bin/bash
# Grey Optimizer - Cross-Platform Installer (Linux/macOS)
# This script detects the OS and installs the appropriate service

set -euo pipefail

# ═══════════════════════════════════════════════════════════════════════════════
# COLORS AND FORMATTING
# ═══════════════════════════════════════════════════════════════════════════════
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

# ═══════════════════════════════════════════════════════════════════════════════
# CONFIGURATION
# ═══════════════════════════════════════════════════════════════════════════════
VERSION="1.0.0"
INSTALL_DIR="/opt/grey-optimizer"
CONFIG_DIR="/etc/grey-optimizer"
LOG_DIR="/var/log/grey-optimizer"
DATA_DIR="/var/lib/grey-optimizer"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

# Mode flags
SIMULATION_MODE=true
CONFIRM_LIVE=false
SKIP_CONSENT=false
UNINSTALL=false

# ═══════════════════════════════════════════════════════════════════════════════
# USAGE
# ═══════════════════════════════════════════════════════════════════════════════
usage() {
    cat << EOF
${BOLD}Grey Optimizer Installer v${VERSION}${NC}

${BOLD}Usage:${NC}
    $0 [OPTIONS]

${BOLD}Options:${NC}
    --simulation        Install in simulation mode (default, no root required for testing)
    --confirm-live      Enable live enforcement mode (REQUIRES ROOT, modifies system)
    --skip-consent      Skip interactive consent prompts (for automated installs)
    --uninstall         Remove Grey Optimizer and restore system settings
    -h, --help          Show this help message

${BOLD}Examples:${NC}
    # Install in simulation mode (safe, no system changes)
    $0 --simulation

    # Install in live mode with explicit consent
    sudo $0 --confirm-live

    # Automated live install (CI/CD)
    sudo $0 --confirm-live --skip-consent

    # Uninstall and restore
    sudo $0 --uninstall

${BOLD}Safety Notes:${NC}
    - Simulation mode runs without root and makes no system changes
    - Live mode requires root and explicit consent
    - All enforcement actions are logged and reversible
    - Use 'greyctl rollback' to undo any changes

EOF
    exit 0
}

# ═══════════════════════════════════════════════════════════════════════════════
# ARGUMENT PARSING
# ═══════════════════════════════════════════════════════════════════════════════
while [[ $# -gt 0 ]]; do
    case $1 in
        --simulation)
            SIMULATION_MODE=true
            shift
            ;;
        --confirm-live)
            SIMULATION_MODE=false
            CONFIRM_LIVE=true
            shift
            ;;
        --skip-consent)
            SKIP_CONSENT=true
            shift
            ;;
        --uninstall)
            UNINSTALL=true
            shift
            ;;
        -h|--help)
            usage
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            usage
            ;;
    esac
done

# ═══════════════════════════════════════════════════════════════════════════════
# OS DETECTION
# ═══════════════════════════════════════════════════════════════════════════════
detect_os() {
    OS_TYPE=""
    OS_NAME=""
    OS_VERSION=""
    INIT_SYSTEM=""
    
    case "$(uname -s)" in
        Linux*)
            OS_TYPE="linux"
            if [[ -f /etc/os-release ]]; then
                . /etc/os-release
                OS_NAME="${ID:-linux}"
                OS_VERSION="${VERSION_ID:-unknown}"
            elif [[ -f /etc/redhat-release ]]; then
                OS_NAME="rhel"
            elif [[ -f /etc/debian_version ]]; then
                OS_NAME="debian"
            else
                OS_NAME="linux"
            fi
            
            # Detect init system
            if command -v systemctl &> /dev/null && systemctl --version &> /dev/null; then
                INIT_SYSTEM="systemd"
            elif [[ -f /etc/init.d ]]; then
                INIT_SYSTEM="sysvinit"
            else
                INIT_SYSTEM="unknown"
            fi
            ;;
        Darwin*)
            OS_TYPE="macos"
            OS_NAME="macos"
            OS_VERSION="$(sw_vers -productVersion 2>/dev/null || echo 'unknown')"
            INIT_SYSTEM="launchd"
            ;;
        CYGWIN*|MINGW*|MSYS*)
            OS_TYPE="windows"
            OS_NAME="windows"
            INIT_SYSTEM="windows-service"
            echo -e "${YELLOW}Warning: Running on Windows via MSYS/Cygwin.${NC}"
            echo -e "${YELLOW}For best results, use install.ps1 from PowerShell.${NC}"
            ;;
        *)
            echo -e "${RED}Unsupported operating system: $(uname -s)${NC}"
            exit 1
            ;;
    esac
    
    echo -e "${CYAN}Detected OS: ${OS_NAME} ${OS_VERSION} (${OS_TYPE})${NC}"
    echo -e "${CYAN}Init system: ${INIT_SYSTEM}${NC}"
}

# ═══════════════════════════════════════════════════════════════════════════════
# PRIVILEGE CHECK
# ═══════════════════════════════════════════════════════════════════════════════
check_privileges() {
    if [[ "$SIMULATION_MODE" == "true" ]]; then
        echo -e "${GREEN}✓ Running in simulation mode - no root required${NC}"
        return 0
    fi
    
    if [[ $EUID -ne 0 ]]; then
        echo -e "${RED}Error: Live mode requires root privileges${NC}"
        echo -e "${YELLOW}Run with: sudo $0 --confirm-live${NC}"
        exit 1
    fi
    
    echo -e "${GREEN}✓ Running with root privileges${NC}"
}

# ═══════════════════════════════════════════════════════════════════════════════
# CONSENT FLOW
# ═══════════════════════════════════════════════════════════════════════════════
show_consent_checklist() {
    if [[ "$SKIP_CONSENT" == "true" ]]; then
        echo -e "${YELLOW}Consent prompts skipped (--skip-consent)${NC}"
        return 0
    fi
    
    if [[ "$SIMULATION_MODE" == "true" ]]; then
        echo -e "${GREEN}Simulation mode - no consent required${NC}"
        return 0
    fi
    
    echo ""
    echo -e "${BOLD}╔══════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BOLD}║              LIVE MODE CONSENT CHECKLIST                         ║${NC}"
    echo -e "${BOLD}╚══════════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    echo -e "${YELLOW}Grey Optimizer Live Mode will perform the following actions:${NC}"
    echo ""
    echo "  [1] Create cgroups and set CPU/memory/IO limits"
    echo "  [2] Modify process scheduling priorities"
    echo "  [3] Enable Kernel Same-page Merging (KSM) if available"
    echo "  [4] Drop filesystem caches when safe"
    echo "  [5] Create hardlinks for duplicate files (with backups)"
    echo "  [6] Install a system service that runs on boot"
    echo ""
    echo -e "${CYAN}Safety guarantees:${NC}"
    echo "  ✓ All actions are logged to an audit trail"
    echo "  ✓ All actions can be rolled back with 'greyctl rollback'"
    echo "  ✓ File operations create backups before modification"
    echo "  ✓ A watchdog monitors system health and auto-rolls back on issues"
    echo ""
    
    read -p "Do you understand and consent to these actions? (yes/no): " consent
    
    if [[ "$consent" != "yes" ]]; then
        echo -e "${RED}Consent not given. Aborting installation.${NC}"
        echo -e "${YELLOW}To install in simulation mode, run without --confirm-live${NC}"
        exit 1
    fi
    
    echo -e "${GREEN}✓ Consent confirmed${NC}"
}

# ═══════════════════════════════════════════════════════════════════════════════
# DEPENDENCY CHECKS
# ═══════════════════════════════════════════════════════════════════════════════
check_dependencies() {
    echo -e "${BLUE}Checking dependencies...${NC}"
    
    local missing=()
    
    # Python 3.11+
    if command -v python3 &> /dev/null; then
        PYTHON_VERSION=$(python3 -c 'import sys; print(f"{sys.version_info.major}.{sys.version_info.minor}")')
        MAJOR=$(echo "$PYTHON_VERSION" | cut -d. -f1)
        MINOR=$(echo "$PYTHON_VERSION" | cut -d. -f2)
        if [[ "$MAJOR" -lt 3 ]] || [[ "$MAJOR" -eq 3 && "$MINOR" -lt 11 ]]; then
            missing+=("python3.11+")
        else
            echo -e "${GREEN}  ✓ Python $PYTHON_VERSION${NC}"
        fi
    else
        missing+=("python3")
    fi
    
    # GCC for C modules
    if command -v gcc &> /dev/null; then
        echo -e "${GREEN}  ✓ GCC $(gcc --version | head -n1 | awk '{print $NF}')${NC}"
    elif command -v clang &> /dev/null; then
        echo -e "${GREEN}  ✓ Clang $(clang --version | head -n1 | awk '{print $4}')${NC}"
    else
        missing+=("gcc or clang")
    fi
    
    # SQLite
    if command -v sqlite3 &> /dev/null; then
        echo -e "${GREEN}  ✓ SQLite3${NC}"
    else
        echo -e "${YELLOW}  ⚠ SQLite3 CLI not found (optional)${NC}"
    fi
    
    # cgroup v2 check (Linux only)
    if [[ "$OS_TYPE" == "linux" ]]; then
        if grep -q "cgroup2" /proc/mounts 2>/dev/null; then
            echo -e "${GREEN}  ✓ cgroup v2 available${NC}"
        else
            echo -e "${YELLOW}  ⚠ cgroup v2 not mounted (some features limited)${NC}"
        fi
    fi
    
    if [[ ${#missing[@]} -gt 0 ]]; then
        echo -e "${RED}Missing dependencies: ${missing[*]}${NC}"
        echo ""
        echo "Install with:"
        case "$OS_NAME" in
            ubuntu|debian)
                echo "  sudo apt install python3.11 python3.11-venv build-essential"
                ;;
            fedora|rhel|centos)
                echo "  sudo dnf install python3.11 gcc make"
                ;;
            arch)
                echo "  sudo pacman -S python gcc make"
                ;;
            macos)
                echo "  brew install python@3.11"
                ;;
        esac
        exit 1
    fi
    
    echo -e "${GREEN}✓ All dependencies satisfied${NC}"
}

# ═══════════════════════════════════════════════════════════════════════════════
# DIRECTORY SETUP
# ═══════════════════════════════════════════════════════════════════════════════
create_directories() {
    echo -e "${BLUE}Creating directories...${NC}"
    
    if [[ "$SIMULATION_MODE" == "true" ]]; then
        # For simulation, use local directories
        INSTALL_DIR="$PROJECT_DIR"
        CONFIG_DIR="$PROJECT_DIR/config"
        LOG_DIR="$PROJECT_DIR/logs"
        DATA_DIR="$PROJECT_DIR/data"
    fi
    
    mkdir -p "$CONFIG_DIR"
    mkdir -p "$LOG_DIR"
    mkdir -p "$DATA_DIR/proofs"
    mkdir -p "$DATA_DIR/backups"
    
    if [[ "$SIMULATION_MODE" == "false" ]]; then
        mkdir -p "$INSTALL_DIR"
        mkdir -p "$INSTALL_DIR/lib"
        mkdir -p "$INSTALL_DIR/bin"
    fi
    
    echo -e "${GREEN}✓ Directories created${NC}"
}

# ═══════════════════════════════════════════════════════════════════════════════
# COPY FILES
# ═══════════════════════════════════════════════════════════════════════════════
copy_files() {
    if [[ "$SIMULATION_MODE" == "true" ]]; then
        echo -e "${CYAN}Simulation mode: using project directory in place${NC}"
        return 0
    fi
    
    echo -e "${BLUE}Copying files to ${INSTALL_DIR}...${NC}"
    
    # Copy backend
    cp -r "$PROJECT_DIR/backend/"* "$INSTALL_DIR/"
    
    # Copy enforcement modules
    cp -r "$PROJECT_DIR/enforcement" "$INSTALL_DIR/"
    
    # Copy scripts
    cp -r "$PROJECT_DIR/scripts/"* "$INSTALL_DIR/"
    
    # Copy CLI
    if [[ -f "$PROJECT_DIR/cli/greyctl" ]]; then
        cp "$PROJECT_DIR/cli/greyctl" "$INSTALL_DIR/bin/"
        chmod +x "$INSTALL_DIR/bin/greyctl"
        ln -sf "$INSTALL_DIR/bin/greyctl" /usr/local/bin/greyctl
    fi
    
    echo -e "${GREEN}✓ Files copied${NC}"
}

# ═══════════════════════════════════════════════════════════════════════════════
# PYTHON ENVIRONMENT
# ═══════════════════════════════════════════════════════════════════════════════
setup_python_env() {
    echo -e "${BLUE}Setting up Python environment...${NC}"
    
    local venv_dir="$INSTALL_DIR/venv"
    if [[ "$SIMULATION_MODE" == "true" ]]; then
        venv_dir="$PROJECT_DIR/backend/venv"
    fi
    
    if [[ ! -d "$venv_dir" ]]; then
        python3 -m venv "$venv_dir"
    fi
    
    source "$venv_dir/bin/activate"
    pip install --upgrade pip -q
    
    if [[ "$SIMULATION_MODE" == "true" ]]; then
        pip install -e "$PROJECT_DIR/backend[dev]" -q
    else
        pip install -e "$INSTALL_DIR[dev]" -q
    fi
    
    deactivate
    
    echo -e "${GREEN}✓ Python environment ready${NC}"
}

# ═══════════════════════════════════════════════════════════════════════════════
# COMPILE C MODULES
# ═══════════════════════════════════════════════════════════════════════════════
compile_c_modules() {
    echo -e "${BLUE}Compiling C enforcement modules...${NC}"
    
    local enforcement_dir="$PROJECT_DIR/enforcement"
    if [[ "$SIMULATION_MODE" == "false" ]]; then
        enforcement_dir="$INSTALL_DIR/enforcement"
    fi
    
    if [[ -f "$enforcement_dir/Makefile" ]]; then
        cd "$enforcement_dir"
        make clean 2>/dev/null || true
        make
        cd - > /dev/null
        echo -e "${GREEN}✓ C modules compiled${NC}"
    else
        echo -e "${YELLOW}⚠ No Makefile found, skipping C module compilation${NC}"
    fi
}

# ═══════════════════════════════════════════════════════════════════════════════
# CREATE CONFIGURATION
# ═══════════════════════════════════════════════════════════════════════════════
create_config() {
    echo -e "${BLUE}Creating configuration...${NC}"
    
    local config_file="$CONFIG_DIR/config.yaml"
    
    if [[ -f "$config_file" ]]; then
        echo -e "${YELLOW}Configuration already exists, preserving${NC}"
        return 0
    fi
    
    cat > "$config_file" << EOF
# Grey Optimizer Configuration
# Generated by installer on $(date -Iseconds)

telemetry:
  interval_seconds: 1.0
  history_size: 300
  warmup_seconds: 10

cpu_enforcement:
  enabled: true
  target_percent: 10
  min_percent: 1

ram_enforcement:
  enabled: true
  target_mb: 64
  min_mb: 64
  enable_ksm: true

disk_enforcement:
  enabled: true
  target_iops: 100
  enable_dedupe: true
  enable_sparse: true

safety:
  simulation_mode: ${SIMULATION_MODE}
  protected_patterns:
    - "systemd*"
    - "kernel*"
    - "init"
    - "sshd"
    - "grey-optimizer"
    - "launchd"
    - "WindowsService"
  rollback_on_failure: true
  max_enforcement_percent: 90
  watchdog_interval_seconds: 30
  backup_before_modify: true

api:
  host: "127.0.0.1"
  port: 8080
  enable_websocket: true

persistence:
  database_path: "${DATA_DIR}/audit.db"
  proof_dir: "${DATA_DIR}/proofs"
  backup_dir: "${DATA_DIR}/backups"
  retention_days: 30

logging:
  level: INFO
  file_path: "${LOG_DIR}/daemon.log"
EOF
    
    echo -e "${GREEN}✓ Configuration created${NC}"
}

# ═══════════════════════════════════════════════════════════════════════════════
# INSTALL SYSTEMD SERVICE (Linux)
# ═══════════════════════════════════════════════════════════════════════════════
install_systemd_service() {
    if [[ "$SIMULATION_MODE" == "true" ]]; then
        echo -e "${CYAN}Simulation mode: skipping systemd service installation${NC}"
        return 0
    fi
    
    echo -e "${BLUE}Installing systemd service...${NC}"
    
    cat > /etc/systemd/system/grey-optimizer.service << EOF
[Unit]
Description=Grey Optimizer - Resource Optimization Daemon
Documentation=https://github.com/grey-optimizer/grey-optimizer
After=network.target
Wants=network.target

[Service]
Type=notify
NotifyAccess=main

User=root
Group=root

WorkingDirectory=${INSTALL_DIR}

Environment=GREY_OPTIMIZER_CONFIG=${CONFIG_DIR}/config.yaml
Environment=GREY_OPTIMIZER_LOG_LEVEL=INFO
Environment=PYTHONUNBUFFERED=1

ExecStartPre=/bin/bash -c 'echo "Grey Optimizer starting at \$(date)"'
ExecStart=${INSTALL_DIR}/venv/bin/python -m grey_optimizer.daemon --immediate
ExecReload=/bin/kill -HUP \$MAINPID
ExecStop=/bin/kill -TERM \$MAINPID

Restart=on-failure
RestartSec=10s

ProtectSystem=strict
ReadWritePaths=${DATA_DIR} ${LOG_DIR} ${CONFIG_DIR} /sys/kernel/mm/ksm /sys/fs/cgroup
ProtectHome=read-only
PrivateTmp=true
NoNewPrivileges=false

CapabilityBoundingSet=CAP_SYS_NICE CAP_SYS_RESOURCE CAP_DAC_OVERRIDE CAP_SYS_ADMIN
AmbientCapabilities=CAP_SYS_NICE CAP_SYS_RESOURCE CAP_DAC_OVERRIDE CAP_SYS_ADMIN

LimitNOFILE=65535
LimitNPROC=4096

StandardOutput=journal
StandardError=journal
SyslogIdentifier=grey-optimizer

WatchdogSec=60

[Install]
WantedBy=multi-user.target
EOF

    # Optional timer for periodic enforcement refresh
    cat > /etc/systemd/system/grey-optimizer.timer << EOF
[Unit]
Description=Grey Optimizer Periodic Enforcement

[Timer]
OnBootSec=1min
OnUnitActiveSec=5min

[Install]
WantedBy=timers.target
EOF

    systemctl daemon-reload
    systemctl enable grey-optimizer.service
    
    echo -e "${GREEN}✓ systemd service installed${NC}"
}

# ═══════════════════════════════════════════════════════════════════════════════
# INSTALL LAUNCHD SERVICE (macOS)
# ═══════════════════════════════════════════════════════════════════════════════
install_launchd_service() {
    if [[ "$SIMULATION_MODE" == "true" ]]; then
        echo -e "${CYAN}Simulation mode: skipping launchd service installation${NC}"
        return 0
    fi
    
    echo -e "${BLUE}Installing launchd service...${NC}"
    
    local plist_path="/Library/LaunchDaemons/com.grey.optimizer.plist"
    
    cat > "$plist_path" << EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>com.grey.optimizer</string>
    
    <key>ProgramArguments</key>
    <array>
        <string>${INSTALL_DIR}/venv/bin/python</string>
        <string>-m</string>
        <string>grey_optimizer.daemon</string>
        <string>--immediate</string>
    </array>
    
    <key>WorkingDirectory</key>
    <string>${INSTALL_DIR}</string>
    
    <key>EnvironmentVariables</key>
    <dict>
        <key>GREY_OPTIMIZER_CONFIG</key>
        <string>${CONFIG_DIR}/config.yaml</string>
        <key>PYTHONUNBUFFERED</key>
        <string>1</string>
    </dict>
    
    <key>RunAtLoad</key>
    <true/>
    
    <key>KeepAlive</key>
    <dict>
        <key>SuccessfulExit</key>
        <false/>
    </dict>
    
    <key>StandardOutPath</key>
    <string>${LOG_DIR}/daemon.log</string>
    
    <key>StandardErrorPath</key>
    <string>${LOG_DIR}/daemon.error.log</string>
    
    <key>ProcessType</key>
    <string>Background</string>
</dict>
</plist>
EOF

    chmod 644 "$plist_path"
    chown root:wheel "$plist_path"
    
    echo -e "${GREEN}✓ launchd service installed${NC}"
}

# ═══════════════════════════════════════════════════════════════════════════════
# START SERVICE
# ═══════════════════════════════════════════════════════════════════════════════
start_service() {
    # Kill any existing daemon first
    pkill -f "grey_optimizer.daemon" 2>/dev/null || true
    fuser -k 8080/tcp 2>/dev/null || true
    fuser -k 8090/tcp 2>/dev/null || true
    sleep 2
    
    if [[ "$SIMULATION_MODE" == "true" ]]; then
        echo -e "${CYAN}Simulation mode: starting daemon in foreground...${NC}"
        echo -e "${YELLOW}Press Ctrl+C to stop${NC}"
        echo ""
        
        local venv_dir="$PROJECT_DIR/backend/venv"
        source "$venv_dir/bin/activate"
        export GREY_OPTIMIZER_CONFIG="$CONFIG_DIR/config.yaml"
        python -m grey_optimizer.daemon --immediate &
        DAEMON_PID=$!
        
        # Wait for health check (give it more time)
        echo -e "${BLUE}Waiting for daemon health check...${NC}"
        sleep 5
        
        if kill -0 $DAEMON_PID 2>/dev/null; then
            echo -e "${GREEN}✓ Daemon started successfully (PID: $DAEMON_PID)${NC}"
            echo ""
            echo -e "${CYAN}To stop: kill $DAEMON_PID${NC}"
            echo -e "${CYAN}To view logs: tail -f $LOG_DIR/daemon.log${NC}"
        else
            echo -e "${RED}✗ Daemon failed to start${NC}"
            exit 1
        fi
        
        return 0
    fi
    
    echo -e "${BLUE}Starting service...${NC}"
    
    case "$INIT_SYSTEM" in
        systemd)
            systemctl start grey-optimizer.service
            
            # Health check
            echo -e "${BLUE}Performing health check...${NC}"
            for i in {1..10}; do
                if systemctl is-active --quiet grey-optimizer.service; then
                    echo -e "${GREEN}✓ Service started and healthy${NC}"
                    return 0
                fi
                sleep 1
            done
            
            echo -e "${RED}✗ Service failed health check${NC}"
            journalctl -u grey-optimizer -n 20 --no-pager
            exit 1
            ;;
        launchd)
            launchctl bootstrap system /Library/LaunchDaemons/com.grey.optimizer.plist || \
            launchctl load /Library/LaunchDaemons/com.grey.optimizer.plist
            
            sleep 3
            if launchctl list | grep -q com.grey.optimizer; then
                echo -e "${GREEN}✓ Service started${NC}"
            else
                echo -e "${RED}✗ Service failed to start${NC}"
                exit 1
            fi
            ;;
    esac
}

# ═══════════════════════════════════════════════════════════════════════════════
# UNINSTALL
# ═══════════════════════════════════════════════════════════════════════════════
uninstall() {
    echo -e "${BLUE}Uninstalling Grey Optimizer...${NC}"
    
    # Stop any running daemon processes (simulation mode or otherwise)
    echo -e "${BLUE}Stopping daemon processes...${NC}"
    pkill -f "grey_optimizer.daemon" 2>/dev/null || true
    pkill -f "grey_optimizer" 2>/dev/null || true
    fuser -k 8080/tcp 2>/dev/null || true
    fuser -k 8090/tcp 2>/dev/null || true
    sleep 1
    
    # Stop service (for non-simulation installs)
    case "$INIT_SYSTEM" in
        systemd)
            systemctl stop grey-optimizer.service 2>/dev/null || true
            systemctl disable grey-optimizer.service 2>/dev/null || true
            rm -f /etc/systemd/system/grey-optimizer.service
            rm -f /etc/systemd/system/grey-optimizer.timer
            systemctl daemon-reload 2>/dev/null || true
            ;;
        launchd)
            launchctl bootout system /Library/LaunchDaemons/com.grey.optimizer.plist 2>/dev/null || \
            launchctl unload /Library/LaunchDaemons/com.grey.optimizer.plist 2>/dev/null || true
            rm -f /Library/LaunchDaemons/com.grey.optimizer.plist
            ;;
    esac
    
    # Rollback any enforcements
    if [[ -f "$INSTALL_DIR/venv/bin/python" ]]; then
        echo -e "${BLUE}Rolling back enforcement actions...${NC}"
        source "$INSTALL_DIR/venv/bin/activate"
        python -m grey_optimizer.cli rollback --force 2>/dev/null || true
        deactivate
    fi
    
    # Remove files (but preserve data/logs if requested)
    echo -e "${BLUE}Removing installation files...${NC}"
    rm -rf "$INSTALL_DIR"
    rm -f /usr/local/bin/greyctl
    
    read -p "Remove configuration and data? (yes/no): " remove_data
    if [[ "$remove_data" == "yes" ]]; then
        rm -rf "$CONFIG_DIR"
        rm -rf "$DATA_DIR"
        rm -rf "$LOG_DIR"
    else
        echo -e "${YELLOW}Preserving config, data, and logs${NC}"
    fi
    
    echo -e "${GREEN}✓ Grey Optimizer uninstalled${NC}"
    exit 0
}

# ═══════════════════════════════════════════════════════════════════════════════
# MAIN
# ═══════════════════════════════════════════════════════════════════════════════
main() {
    echo ""
    echo -e "${BOLD}╔══════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BOLD}║         Grey Optimizer - Installer v${VERSION}                       ║${NC}"
    echo -e "${BOLD}╚══════════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    
    detect_os
    
    if [[ "$UNINSTALL" == "true" ]]; then
        check_privileges
        uninstall
    fi
    
    check_privileges
    show_consent_checklist
    check_dependencies
    create_directories
    copy_files
    setup_python_env
    compile_c_modules
    create_config
    
    case "$INIT_SYSTEM" in
        systemd)
            install_systemd_service
            ;;
        launchd)
            install_launchd_service
            ;;
    esac
    
    start_service
    
    echo ""
    echo -e "${GREEN}╔══════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║         Installation Complete!                                   ║${NC}"
    echo -e "${GREEN}╚══════════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    
    if [[ "$SIMULATION_MODE" == "true" ]]; then
        echo -e "${YELLOW}Running in SIMULATION mode - no system changes applied${NC}"
        echo ""
        echo "To enable live enforcement:"
        echo "  sudo $0 --confirm-live"
    else
        echo -e "${GREEN}Running in LIVE mode - enforcement active${NC}"
    fi
    
    echo ""
    echo "Commands:"
    echo "  greyctl status    - View current status"
    echo "  greyctl rollback  - Rollback all changes"
    echo "  greyctl stop      - Stop the daemon"
    echo ""
}

main "$@"
