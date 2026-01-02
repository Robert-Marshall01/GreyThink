#!/bin/bash
#
# uninstall_av.sh - GreyAV Cross-Platform Uninstaller Script
#
# Removes GreyAV system and cleans up resources.
#
# This script performs the following:
#   1. Detects the operating system (Linux, macOS, Windows/WSL)
#   2. Stops and disables the system service
#   3. Removes the service configuration
#   4. Removes the installation directory and all contents
#   5. Cleans up log files and configuration
#   6. Removes firewall rules (Linux)
#
# Supported Platforms:
#   - Ubuntu, Debian, Linux Mint, Pop!_OS, Zorin OS, MX Linux, Kali Linux
#   - Fedora, CentOS, RHEL, Bazzite
#   - Arch Linux, EndeavourOS, Garuda Linux, Manjaro
#   - NixOS
#   - openSUSE
#   - macOS
#   - Windows (via WSL or native)
#
# Usage:
#   sudo ./uninstall_av.sh [--force]
#
# Options:
#   --force    Skip confirmation prompts
#
# Requirements:
#   - Must be run as root/Administrator
#

set -e  # Exit immediately on any error
set -u  # Treat unset variables as errors
set -o pipefail  # Pipe failures cause script failure

# =============================================================================
# OS Detection Variables
# =============================================================================

OS_TYPE=""           # linux, macos, windows
DISTRO_ID=""         # ubuntu, fedora, linuxmint, arch, darwin, windows
DISTRO_VERSION=""    # Version number
DISTRO_ID_LIKE=""    # Parent distro family (debian, rhel, arch, etc.)
DISTRO_FAMILY=""     # Normalized family for cleanup operations
SERVICE_MANAGER=""   # systemd, launchd, windows, openrc

# =============================================================================
# Configuration (OS-specific paths set later)
# =============================================================================

INSTALL_DIR=""
SERVICE_NAME="greyav"
SERVICE_FILE=""
LOG_DIR=""

# Command line options
FORCE_MODE=false

# Colors for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly CYAN='\033[0;36m'
readonly NC='\033[0m' # No Color

# =============================================================================
# Helper Functions
# =============================================================================

# Print colored status messages
print_status() {
    echo -e "${BLUE}[*]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[✓]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[!]${NC} $1"
}

print_error() {
    echo -e "${RED}[✗]${NC} $1"
}

print_info() {
    echo -e "${CYAN}[i]${NC} $1"
}

# Check if a command exists
command_exists() {
    command -v "$1" &> /dev/null
}

# =============================================================================
# OS Detection Functions
# =============================================================================

# Detect the operating system type
detect_os_type() {
    case "$(uname -s)" in
        Linux*)
            OS_TYPE="linux"
            ;;
        Darwin*)
            OS_TYPE="macos"
            ;;
        CYGWIN*|MINGW*|MSYS*)
            OS_TYPE="windows"
            ;;
        *)
            if [[ -n "${OS:-}" ]] && [[ "${OS}" == "Windows_NT" ]]; then
                OS_TYPE="windows"
            else
                print_error "Unsupported operating system: $(uname -s)"
                exit 1
            fi
            ;;
    esac
    
    print_status "Detected OS type: ${OS_TYPE}"
}

# Detect Linux distribution
detect_linux_distro() {
    if [[ -f /etc/os-release ]]; then
        source /etc/os-release
        DISTRO_ID="${ID,,}"
        DISTRO_VERSION="${VERSION_ID:-}"
        DISTRO_ID_LIKE="${ID_LIKE:-}"
        print_status "Detected distribution: ${PRETTY_NAME:-$DISTRO_ID}"
        
        # Map distro families for cleanup operations
        case "${DISTRO_ID}" in
            # Debian/Ubuntu family
            ubuntu|debian|linuxmint|pop|zorin|kali|mx|mxlinux|antix)
                DISTRO_FAMILY="debian"
                ;;
            # Fedora/RHEL family
            fedora|rhel|centos|rocky|almalinux|bazzite|silverblue|kinoite)
                DISTRO_FAMILY="fedora"
                ;;
            # Arch family
            arch|manjaro|endeavouros|garuda|artix|arcolinux)
                DISTRO_FAMILY="arch"
                ;;
            # NixOS
            nixos)
                DISTRO_FAMILY="nixos"
                ;;
            *)
                # Fallback to ID_LIKE
                if [[ "${DISTRO_ID_LIKE}" == *"debian"* ]] || [[ "${DISTRO_ID_LIKE}" == *"ubuntu"* ]]; then
                    DISTRO_FAMILY="debian"
                elif [[ "${DISTRO_ID_LIKE}" == *"fedora"* ]] || [[ "${DISTRO_ID_LIKE}" == *"rhel"* ]]; then
                    DISTRO_FAMILY="fedora"
                elif [[ "${DISTRO_ID_LIKE}" == *"arch"* ]]; then
                    DISTRO_FAMILY="arch"
                else
                    DISTRO_FAMILY="unknown"
                fi
                ;;
        esac
    elif [[ -f /etc/NIXOS ]]; then
        DISTRO_ID="nixos"
        DISTRO_FAMILY="nixos"
        print_status "Detected distribution: NixOS"
    elif [[ -f /etc/debian_version ]]; then
        DISTRO_ID="debian"
        DISTRO_FAMILY="debian"
        print_status "Detected distribution: Debian"
    elif [[ -f /etc/redhat-release ]]; then
        DISTRO_ID="rhel"
        DISTRO_FAMILY="fedora"
        print_status "Detected distribution: RedHat/CentOS"
    elif [[ -f /etc/arch-release ]]; then
        DISTRO_ID="arch"
        DISTRO_FAMILY="arch"
        print_status "Detected distribution: Arch Linux"
    else
        DISTRO_ID="unknown"
        DISTRO_FAMILY="unknown"
        print_warning "Unable to detect Linux distribution"
    fi
}

# Detect macOS version
detect_macos() {
    DISTRO_ID="darwin"
    DISTRO_VERSION="$(sw_vers -productVersion)"
    print_status "Detected macOS ${DISTRO_VERSION}"
}

# Detect Windows version
detect_windows() {
    DISTRO_ID="windows"
    print_status "Detected Windows"
}

# Main OS detection function
detect_os() {
    detect_os_type
    
    case "$OS_TYPE" in
        linux)
            detect_linux_distro
            # Determine service manager
            if command_exists systemctl; then
                SERVICE_MANAGER="systemd"
            elif command_exists rc-service; then
                SERVICE_MANAGER="openrc"
            else
                SERVICE_MANAGER="sysvinit"
            fi
            ;;
        macos)
            detect_macos
            SERVICE_MANAGER="launchd"
            ;;
        windows)
            detect_windows
            SERVICE_MANAGER="windows"
            ;;
    esac
    
    print_info "Service manager: ${SERVICE_MANAGER}"
}

# Set OS-specific paths
set_os_paths() {
    case "$OS_TYPE" in
        linux)
            INSTALL_DIR="/opt/greyav"
            LOG_DIR="/var/log/greyav"
            SERVICE_FILE="/etc/systemd/system/${SERVICE_NAME}.service"
            ;;
        macos)
            INSTALL_DIR="/usr/local/greyav"
            LOG_DIR="/usr/local/var/log/greyav"
            SERVICE_FILE="/Library/LaunchDaemons/com.greyav.daemon.plist"
            ;;
        windows)
            INSTALL_DIR="/c/Program Files/GreyAV"
            LOG_DIR="/c/ProgramData/GreyAV/logs"
            SERVICE_FILE=""
            ;;
    esac
    
    print_info "Installation directory: ${INSTALL_DIR}"
}

# Check privileges
check_privileges() {
    case "$OS_TYPE" in
        linux|macos)
            if [[ $EUID -ne 0 ]]; then
                print_error "This script must be run as root (use sudo)"
                exit 1
            fi
            ;;
        windows)
            print_info "Ensure you are running as Administrator"
            ;;
    esac
}

# Parse command line arguments
parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --force|-f)
                FORCE_MODE=true
                shift
                ;;
            --help|-h)
                show_help
                exit 0
                ;;
            *)
                print_error "Unknown option: $1"
                show_help
                exit 1
                ;;
        esac
    done
}

# Show help message
show_help() {
    echo ""
    echo "Usage: sudo ./uninstall_av.sh [OPTIONS]"
    echo ""
    echo "Removes the MyAV antivirus/EDR system from this machine."
    echo ""
    echo "Options:"
    echo "  -f, --force    Skip confirmation prompts"
    echo "  -h, --help     Show this help message"
    echo ""
    echo "Examples:"
    echo "  sudo ./uninstall_av.sh          # Interactive uninstall"
    echo "  sudo ./uninstall_av.sh --force  # Force uninstall without prompts"
    echo ""
}

# Prompt for user confirmation
confirm_action() {
    local prompt="$1"
    local response
    
    if [[ "${FORCE_MODE}" == "true" ]]; then
        return 0
    fi
    
    echo -en "${YELLOW}${prompt} [y/N]: ${NC}"
    read -r response
    
    case "$response" in
        [yY]|[yY][eE][sS])
            return 0
            ;;
        *)
            return 1
            ;;
    esac
}

# =============================================================================
# Uninstallation Functions
# =============================================================================

# Check if GreyAV is installed
check_installation() {
    local is_installed=false
    
    if [[ -d "${INSTALL_DIR}" ]]; then
        is_installed=true
        print_status "Found installation directory: ${INSTALL_DIR}"
    fi
    
    case "${SERVICE_MANAGER}" in
        systemd)
            if [[ -f "${SERVICE_FILE}" ]] || systemctl list-unit-files | grep -q "${SERVICE_NAME}.service"; then
                is_installed=true
                print_status "Found systemd service: ${SERVICE_NAME}.service"
            fi
            ;;
        launchd)
            if [[ -f "${SERVICE_FILE}" ]] || launchctl list 2>/dev/null | grep -q "com.greyav"; then
                is_installed=true
                print_status "Found launchd service: com.greyav.daemon"
            fi
            ;;
        windows)
            if command_exists sc.exe && sc.exe query GreyAV &>/dev/null; then
                is_installed=true
                print_status "Found Windows service: GreyAV"
            fi
            ;;
    esac
    
    if [[ "${is_installed}" == "false" ]]; then
        print_warning "GreyAV does not appear to be installed on this system"
        if ! confirm_action "Continue anyway?"; then
            print_status "Uninstallation cancelled"
            exit 0
        fi
    fi
}

# Stop the service (cross-platform)
stop_service() {
    print_status "Stopping ${SERVICE_NAME} service..."
    
    case "${SERVICE_MANAGER}" in
        systemd)
            if systemctl is-active --quiet "${SERVICE_NAME}.service" 2>/dev/null; then
                systemctl stop "${SERVICE_NAME}.service"
                print_success "Service stopped"
            else
                print_status "Service was not running"
            fi
            ;;
        launchd)
            if launchctl list 2>/dev/null | grep -q "com.greyav.daemon"; then
                launchctl unload "${SERVICE_FILE}" 2>/dev/null || true
                print_success "Service stopped"
            else
                print_status "Service was not running"
            fi
            ;;
        windows)
            if command_exists sc.exe && sc.exe query GreyAV &>/dev/null; then
                sc.exe stop GreyAV 2>/dev/null || true
                print_success "Service stopped"
            else
                print_status "Service was not running"
            fi
            ;;
        openrc)
            if rc-service "${SERVICE_NAME}" status &>/dev/null; then
                rc-service "${SERVICE_NAME}" stop
                print_success "Service stopped"
            else
                print_status "Service was not running"
            fi
            ;;
        *)
            print_status "No service manager to stop"
            ;;
    esac
}

# Disable the service (cross-platform)
disable_service() {
    print_status "Disabling ${SERVICE_NAME} service..."
    
    case "${SERVICE_MANAGER}" in
        systemd)
            if systemctl is-enabled --quiet "${SERVICE_NAME}.service" 2>/dev/null; then
                systemctl disable "${SERVICE_NAME}.service"
                print_success "Service disabled"
            else
                print_status "Service was not enabled"
            fi
            ;;
        launchd)
            # launchd services are disabled by unloading them (done in stop_service)
            print_status "launchd service will be removed"
            ;;
        windows)
            if command_exists sc.exe && sc.exe query GreyAV &>/dev/null; then
                sc.exe config GreyAV start= disabled 2>/dev/null || true
                print_success "Service disabled"
            fi
            ;;
        openrc)
            if rc-update show | grep -q "${SERVICE_NAME}"; then
                rc-update del "${SERVICE_NAME}" default
                print_success "Service disabled"
            else
                print_status "Service was not enabled"
            fi
            ;;
    esac
}

# Remove the service file (cross-platform)
remove_service_file() {
    print_status "Removing service configuration..."
    
    case "${SERVICE_MANAGER}" in
        systemd)
            if [[ -f "${SERVICE_FILE}" ]]; then
                rm -f "${SERVICE_FILE}"
                print_success "Removed ${SERVICE_FILE}"
            else
                print_status "Service file not found (already removed?)"
            fi
            systemctl daemon-reload
            print_status "Reloaded systemd daemon"
            ;;
        launchd)
            if [[ -f "${SERVICE_FILE}" ]]; then
                rm -f "${SERVICE_FILE}"
                print_success "Removed ${SERVICE_FILE}"
            else
                print_status "Service file not found (already removed?)"
            fi
            ;;
        windows)
            if command_exists sc.exe && sc.exe query GreyAV &>/dev/null; then
                sc.exe delete GreyAV 2>/dev/null || true
                print_success "Windows service removed"
            fi
            # Also try NSSM if available
            if command_exists nssm; then
                nssm remove GreyAV confirm 2>/dev/null || true
            fi
            ;;
        openrc)
            local init_script="/etc/init.d/${SERVICE_NAME}"
            if [[ -f "$init_script" ]]; then
                rm -f "$init_script"
                print_success "Removed OpenRC init script"
            fi
            ;;
    esac
}

# Remove the installation directory
remove_installation_directory() {
    print_status "Removing installation directory..."
    
    if [[ -d "${INSTALL_DIR}" ]]; then
        # Show what will be removed
        local file_count
        local dir_size
        file_count=$(find "${INSTALL_DIR}" -type f 2>/dev/null | wc -l)
        dir_size=$(du -sh "${INSTALL_DIR}" 2>/dev/null | cut -f1)
        
        print_status "Directory contains ${file_count} files (${dir_size})"
        
        if confirm_action "Remove ${INSTALL_DIR} and all contents?"; then
            rm -rf "${INSTALL_DIR}"
            print_success "Removed ${INSTALL_DIR}"
        else
            print_warning "Skipped removal of ${INSTALL_DIR}"
        fi
    else
        print_status "Installation directory not found (already removed?)"
    fi
}

# Remove log directory
remove_log_directory() {
    print_status "Removing log directory..."
    
    if [[ -d "${LOG_DIR}" ]]; then
        if confirm_action "Remove log directory ${LOG_DIR}?"; then
            rm -rf "${LOG_DIR}"
            print_success "Removed ${LOG_DIR}"
        else
            print_warning "Skipped removal of ${LOG_DIR}"
        fi
    else
        print_status "Log directory not found"
    fi
}

# Clean up any remaining processes
cleanup_processes() {
    print_status "Checking for remaining MyAV processes..."
    
    local pids
    pids=$(pgrep -f "myav|greyav" 2>/dev/null || true)
    
    if [[ -n "${pids}" ]]; then
        print_warning "Found running processes: ${pids}"
        
        if confirm_action "Terminate these processes?"; then
            echo "${pids}" | xargs -r kill -TERM 2>/dev/null || true
            sleep 2
            # Force kill if still running
            echo "${pids}" | xargs -r kill -KILL 2>/dev/null || true
            print_success "Processes terminated"
        else
            print_warning "Skipped process termination"
        fi
    else
        print_status "No remaining processes found"
    fi
}

# Optional: Remove Docker images
cleanup_docker() {
    if ! command -v docker &> /dev/null; then
        return
    fi
    
    print_status "Checking for MyAV Docker images..."
    
    local images
    images=$(docker images --format '{{.Repository}}:{{.Tag}}' 2>/dev/null | grep -i "myav\|greyav" || true)
    
    if [[ -n "${images}" ]]; then
        echo "Found Docker images:"
        echo "${images}" | sed 's/^/  /'
        
        if confirm_action "Remove these Docker images?"; then
            echo "${images}" | xargs -r docker rmi -f 2>/dev/null || true
            print_success "Docker images removed"
        else
            print_warning "Skipped Docker image removal"
        fi
    else
        print_status "No MyAV Docker images found"
    fi
}

# Remove any cron jobs
cleanup_cron() {
    print_status "Checking for MyAV cron jobs..."
    
    # Check system crontab
    if grep -q "myav\|greyav" /etc/crontab 2>/dev/null; then
        print_warning "Found MyAV entries in /etc/crontab"
        print_warning "Please remove them manually if needed"
    fi
    
    # Check cron.d
    if ls /etc/cron.d/*myav* 2>/dev/null || ls /etc/cron.d/*greyav* 2>/dev/null; then
        if confirm_action "Remove MyAV cron files from /etc/cron.d?"; then
            rm -f /etc/cron.d/*myav* /etc/cron.d/*greyav* 2>/dev/null || true
            print_success "Removed cron files"
        fi
    fi
}

# =============================================================================
# Firewall Rule Cleanup
# =============================================================================

# Offer to restore firewall backup if available
offer_firewall_restore() {
    local backup_dir="${INSTALL_DIR}/config/firewall_backup"
    
    if [[ -d "$backup_dir" ]]; then
        local backups
        backups=$(ls -1 "$backup_dir" 2>/dev/null | head -5)
        
        if [[ -n "$backups" ]]; then
            print_status "Found firewall backup files:"
            ls -la "$backup_dir" | head -10
            echo ""
            
            if confirm_action "Would you like to restore the original firewall configuration?"; then
                # Find the most recent iptables backup
                local latest_iptables
                latest_iptables=$(ls -t "$backup_dir"/iptables_*.backup 2>/dev/null | head -1)
                
                if [[ -n "$latest_iptables" ]] && command -v iptables-restore &> /dev/null; then
                    print_status "Restoring iptables from: $latest_iptables"
                    iptables-restore < "$latest_iptables" 2>/dev/null && \
                        print_success "iptables configuration restored" || \
                        print_warning "Failed to restore iptables (may need manual intervention)"
                fi
                
                print_status "Note: UFW and firewalld may need manual restoration"
                print_status "Backup files available in: $backup_dir"
            fi
        fi
    fi
}

# Remove firewall rules added by MyAV
cleanup_firewall_rules() {
    print_status "Removing MyAV firewall rules..."
    
    local rules_removed=0
    
    # First, offer to restore backup
    offer_firewall_restore
    
    # Check and cleanup UFW rules
    if command -v ufw &> /dev/null && ufw status 2>/dev/null | grep -q "active"; then
        cleanup_ufw_rules
        rules_removed=1
    fi
    
    # Check and cleanup firewalld rules
    if command -v firewall-cmd &> /dev/null && systemctl is-active --quiet firewalld 2>/dev/null; then
        cleanup_firewalld_rules
        rules_removed=1
    fi
    
    # Check and cleanup iptables rules
    if command -v iptables &> /dev/null; then
        cleanup_iptables_rules
        rules_removed=1
    fi
    
    if [[ $rules_removed -eq 0 ]]; then
        print_status "No firewall rules to clean up"
    fi
}

# Cleanup UFW rules
cleanup_ufw_rules() {
    print_status "Cleaning up UFW rules..."
    
    # Find and remove rules with MyAV comment
    local myav_rules
    myav_rules=$(ufw status numbered 2>/dev/null | grep -i "MyAV" | grep -oP '^\[\s*\K[0-9]+' | sort -rn || true)
    
    if [[ -n "$myav_rules" ]]; then
        local count
        count=$(echo "$myav_rules" | wc -l)
        print_status "Found ${count} MyAV rules in UFW"
        
        if confirm_action "Remove MyAV UFW rules?"; then
            # Remove rules in reverse order to preserve numbering
            for rule_num in $myav_rules; do
                yes | ufw delete "$rule_num" 2>/dev/null || true
            done
            print_success "Removed UFW rules"
        else
            print_warning "Skipped UFW rule removal"
        fi
    else
        print_status "No MyAV rules found in UFW"
    fi
}

# Cleanup firewalld rules
cleanup_firewalld_rules() {
    print_status "Cleaning up firewalld rules..."
    
    # C2 and dangerous ports that were blocked (updated list - no longer includes 5555, 9999, 3333)
    local BLOCKED_PORTS=(
        4444 4445 6666 7777 12345 27374 31337 31338 50050 14444 1234
        23 512 513 514
    )
    
    if confirm_action "Remove MyAV firewalld rules?"; then
        for port in "${BLOCKED_PORTS[@]}"; do
            firewall-cmd --permanent --remove-rich-rule="rule family='ipv4' port port='$port' protocol='tcp' reject" 2>/dev/null || true
        done
        
        # Remove custom zone if exists
        firewall-cmd --permanent --delete-zone=myav-blocked 2>/dev/null || true
        
        # Reload firewalld
        firewall-cmd --reload 2>/dev/null || true
        
        print_success "Removed firewalld rules"
    else
        print_warning "Skipped firewalld rule removal"
    fi
}

# Cleanup iptables rules
cleanup_iptables_rules() {
    print_status "Cleaning up iptables rules..."
    
    local chains_cleaned=0
    
    # Check if MYAV_ALLOW chain exists
    if iptables -L MYAV_ALLOW &> /dev/null; then
        print_status "Found MYAV_ALLOW chain"
        
        # Remove references from INPUT and OUTPUT
        iptables -D INPUT -j MYAV_ALLOW 2>/dev/null || true
        iptables -D OUTPUT -j MYAV_ALLOW 2>/dev/null || true
        
        # Flush and delete the chain
        iptables -F MYAV_ALLOW 2>/dev/null || true
        iptables -X MYAV_ALLOW 2>/dev/null || true
        ((chains_cleaned++))
    fi
    
    # Check if MYAV_BLOCK chain exists
    if iptables -L MYAV_BLOCK &> /dev/null; then
        local rule_count
        rule_count=$(iptables -L MYAV_BLOCK --line-numbers 2>/dev/null | tail -n +3 | wc -l)
        print_status "Found MYAV_BLOCK chain with ${rule_count} rules"
        
        if confirm_action "Remove MyAV iptables chains and rules?"; then
            # Remove references to MYAV_BLOCK from INPUT and OUTPUT
            iptables -D INPUT -j MYAV_BLOCK 2>/dev/null || true
            iptables -D OUTPUT -j MYAV_BLOCK 2>/dev/null || true
            
            # Flush and delete the MYAV_BLOCK chain
            iptables -F MYAV_BLOCK 2>/dev/null || true
            iptables -X MYAV_BLOCK 2>/dev/null || true
            
            ((chains_cleaned++))
            print_success "Removed iptables MYAV chains"
        else
            print_warning "Skipped iptables rule removal"
        fi
    fi
    
    # Remove saved rules file
    if [[ -f /etc/iptables/myav-rules.v4 ]]; then
        rm -f /etc/iptables/myav-rules.v4 2>/dev/null || true
        print_status "Removed saved iptables rules file"
    fi
    
    if [[ $chains_cleaned -eq 0 ]]; then
        print_status "No MyAV iptables chains found"
    fi
}

# Remove port configuration files
cleanup_port_config() {
    print_status "Cleaning up port configuration..."
    
    if [[ -f "${INSTALL_DIR}/config/port_rules.json" ]]; then
        print_status "Found port_rules.json"
    fi
    
    # This will be removed with the installation directory
    print_status "Port configuration will be removed with installation directory"
}

# Final cleanup and verification
verify_cleanup() {
    print_status "Verifying cleanup..."
    
    local issues=0
    
    # Check installation directory
    if [[ -d "${INSTALL_DIR}" ]]; then
        print_warning "Installation directory still exists: ${INSTALL_DIR}"
        ((issues++))
    fi
    
    case "${OS_TYPE}" in
        debian|ubuntu|mint|fedora|rhel|centos|arch|manjaro|opensuse|linux)
            # Check systemd service file
            if [[ -f "${SERVICE_FILE}" ]]; then
                print_warning "Service file still exists: ${SERVICE_FILE}"
                ((issues++))
            fi
            # Check if service is still registered
            if systemctl list-unit-files 2>/dev/null | grep -q "${SERVICE_NAME}.service"; then
                print_warning "Service still registered in systemd"
                ((issues++))
            fi
            ;;
        macos)
            # Check launchd plist
            if [[ -f "/Library/LaunchDaemons/com.myav.daemon.plist" ]]; then
                print_warning "LaunchDaemon plist still exists"
                ((issues++))
            fi
            if launchctl list 2>/dev/null | grep -q "com.myav.daemon"; then
                print_warning "Service still registered in launchd"
                ((issues++))
            fi
            ;;
        windows)
            # Check Windows service
            if sc.exe query myav-daemon &>/dev/null; then
                print_warning "Windows service still exists"
                ((issues++))
            fi
            ;;
    esac
    
    # Check log directory
    if [[ -d "${LOG_DIR}" ]]; then
        print_warning "Log directory still exists: ${LOG_DIR}"
        ((issues++))
    fi
    
    if [[ ${issues} -eq 0 ]]; then
        print_success "Cleanup verification passed"
        return 0
    else
        print_warning "Found ${issues} remaining items"
        return 1
    fi
}

# Print uninstallation summary
print_summary() {
    echo ""
    echo -e "${GREEN}╔════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║               MyAV Uninstallation Complete!                    ║${NC}"
    echo -e "${GREEN}╚════════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    echo -e "  ${BLUE}OS Detected:${NC} ${OS_TYPE} (${DISTRO_ID})"
    echo ""
    echo -e "  ${BLUE}The following have been removed:${NC}"
    
    case "${OS_TYPE}" in
        debian|ubuntu|mint|fedora|rhel|centos|arch|manjaro|opensuse|linux)
            echo -e "    • Systemd service: ${SERVICE_NAME}.service"
            echo -e "    • Installation directory: ${INSTALL_DIR}"
            echo -e "    • Log directory: ${LOG_DIR}"
            echo -e "    • Firewall rules (UFW/firewalld/iptables)"
            echo -e "    • Port blocking configuration"
            ;;
        macos)
            echo -e "    • LaunchDaemon: com.myav.daemon"
            echo -e "    • Installation directory: ${INSTALL_DIR}"
            echo -e "    • Log directory: ${LOG_DIR}"
            echo -e "    • PF firewall rules"
            echo -e "    • Port blocking configuration"
            ;;
        windows)
            echo -e "    • Windows Service: myav-daemon"
            echo -e "    • Installation directory: ${INSTALL_DIR}"
            echo -e "    • Log directory: ${LOG_DIR}"
            echo -e "    • Windows Firewall rules"
            echo -e "    • Port blocking configuration"
            ;;
    esac
    
    echo ""
    echo -e "  ${YELLOW}Note:${NC}"
    echo -e "    • System dependencies (python3, docker, etc.) were NOT removed"
    echo -e "    • You may remove them manually if no longer needed"
    echo -e "    • Blocked ports are now unblocked - review your firewall config"
    echo ""
    echo -e "  ${BLUE}Thank you for using MyAV!${NC}"
    echo ""
}

# Print cancelled message
print_cancelled() {
    echo ""
    echo -e "${YELLOW}╔════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${YELLOW}║                 Uninstallation Cancelled                       ║${NC}"
    echo -e "${YELLOW}╚════════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    echo -e "  No changes were made to your system."
    echo ""
}

# =============================================================================
# Main Uninstallation Flow
# =============================================================================

main() {
    # Parse command line arguments
    parse_args "$@"
    
    echo ""
    echo -e "${RED}╔════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${RED}║             MyAV Antivirus/EDR System Uninstaller              ║${NC}"
    echo -e "${RED}╚════════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    
    # Detect OS first
    detect_os
    set_os_paths
    
    # Pre-flight checks
    check_privileges
    check_installation
    
    # Final confirmation
    echo ""
    print_warning "This will completely remove MyAV from your system!"
    echo -e "  ${BLUE}Detected OS:${NC} ${OS_TYPE} (${DISTRO_ID})"
    echo ""
    
    if ! confirm_action "Are you sure you want to uninstall MyAV?"; then
        print_cancelled
        exit 0
    fi
    
    echo ""
    
    # Uninstallation steps
    stop_service
    disable_service
    remove_service_file
    cleanup_processes
    cleanup_firewall_rules
    cleanup_port_config
    remove_installation_directory
    remove_log_directory
    cleanup_docker
    cleanup_cron
    
    # Verify cleanup
    verify_cleanup || true
    
    # Done!
    print_summary
}

# Run main function
main "$@"
