#!/bin/bash
# ═══════════════════════════════════════════════════════════════════════════════
# Grey Optimizer - Cross-Platform Uninstaller
# ═══════════════════════════════════════════════════════════════════════════════
#
# This script safely removes Grey Optimizer from Linux and macOS systems.
# It stops services, removes files, and restores any modified kernel settings.
#
# Usage:
#   sudo ./uninstall.sh              # Interactive uninstall
#   sudo ./uninstall.sh --force      # Non-interactive uninstall
#   sudo ./uninstall.sh --keep-data  # Keep configuration and logs
#
# Safety:
#   - Rollback is performed before removal
#   - Audit logs can be preserved with --keep-data
#   - Kernel settings are restored to system defaults
#
# ═══════════════════════════════════════════════════════════════════════════════

set -euo pipefail

# ─────────────────────────────────────────────────────────────────────────────
# Configuration
# ─────────────────────────────────────────────────────────────────────────────

INSTALL_DIR="/opt/grey-optimizer"
CONFIG_DIR="/etc/grey-optimizer"
LOG_DIR="/var/log/grey-optimizer"
BACKUP_DIR="/var/backup/grey-optimizer"

SERVICE_NAME="grey-optimizer"
TIMER_NAME="grey-optimizer-refresh"

# CLI paths to remove
CLI_PATHS=(
    "/usr/local/bin/greyctl"
    "/usr/bin/greyctl"
)

# Colors for output
if [[ -t 1 ]]; then
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[1;33m'
    BLUE='\033[0;34m'
    NC='\033[0m'
else
    RED='' GREEN='' YELLOW='' BLUE='' NC=''
fi

# ─────────────────────────────────────────────────────────────────────────────
# Logging
# ─────────────────────────────────────────────────────────────────────────────

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[OK]${NC} $1"
}

# ─────────────────────────────────────────────────────────────────────────────
# Platform Detection
# ─────────────────────────────────────────────────────────────────────────────

detect_os() {
    case "$(uname -s)" in
        Linux*)  echo "linux" ;;
        Darwin*) echo "macos" ;;
        *)       echo "unknown" ;;
    esac
}

OS=$(detect_os)

# ─────────────────────────────────────────────────────────────────────────────
# Argument Parsing
# ─────────────────────────────────────────────────────────────────────────────

FORCE=false
KEEP_DATA=false

while [[ $# -gt 0 ]]; do
    case "$1" in
        --force|-f)
            FORCE=true
            shift
            ;;
        --keep-data|-k)
            KEEP_DATA=true
            shift
            ;;
        --help|-h)
            cat << 'EOF'
Grey Optimizer Uninstaller

Usage: sudo ./uninstall.sh [OPTIONS]

Options:
  --force, -f       Non-interactive uninstall (skip confirmation)
  --keep-data, -k   Keep configuration and log files
  --help, -h        Show this help message

Examples:
  sudo ./uninstall.sh              # Interactive uninstall
  sudo ./uninstall.sh --force      # Automated uninstall
  sudo ./uninstall.sh --keep-data  # Keep logs for audit

EOF
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            exit 1
            ;;
    esac
done

# ─────────────────────────────────────────────────────────────────────────────
# Root Check
# ─────────────────────────────────────────────────────────────────────────────

if [[ $EUID -ne 0 ]]; then
    log_error "This script must be run as root (use sudo)"
    exit 1
fi

# ─────────────────────────────────────────────────────────────────────────────
# Confirmation
# ─────────────────────────────────────────────────────────────────────────────

if [[ "$FORCE" != "true" ]]; then
    echo ""
    echo "═══════════════════════════════════════════════════════════════════"
    echo "           Grey Optimizer Uninstaller"
    echo "═══════════════════════════════════════════════════════════════════"
    echo ""
    echo "This will remove Grey Optimizer from your system:"
    echo "  • Stop and disable the service"
    echo "  • Remove program files from ${INSTALL_DIR}"
    echo "  • Remove CLI tool (greyctl)"
    echo "  • Restore kernel settings to defaults"
    
    if [[ "$KEEP_DATA" == "true" ]]; then
        echo ""
        echo "Configuration and logs will be PRESERVED:"
        echo "  • ${CONFIG_DIR}"
        echo "  • ${LOG_DIR}"
    else
        echo ""
        echo "Configuration and logs will be REMOVED:"
        echo "  • ${CONFIG_DIR}"
        echo "  • ${LOG_DIR}"
    fi
    
    echo ""
    read -p "Do you want to continue? [y/N] " -n 1 -r
    echo ""
    
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        log_info "Uninstall cancelled"
        exit 0
    fi
fi

# ─────────────────────────────────────────────────────────────────────────────
# Rollback Enforcement
# ─────────────────────────────────────────────────────────────────────────────

rollback_enforcement() {
    log_info "Rolling back any active enforcement..."
    
    # Try to use greyctl first
    for cli in "${CLI_PATHS[@]}"; do
        if [[ -x "$cli" ]]; then
            if "$cli" rollback 2>/dev/null; then
                log_success "Rollback via greyctl completed"
                return 0
            fi
        fi
    done
    
    # Manual rollback if greyctl unavailable
    if [[ "$OS" == "linux" ]]; then
        # Remove any cgroups we created
        if [[ -d "/sys/fs/cgroup/grey-optimizer" ]]; then
            # Move any processes back to root cgroup
            if [[ -f "/sys/fs/cgroup/grey-optimizer/cgroup.procs" ]]; then
                while read -r pid; do
                    echo "$pid" > /sys/fs/cgroup/cgroup.procs 2>/dev/null || true
                done < /sys/fs/cgroup/grey-optimizer/cgroup.procs
            fi
            
            # Remove the cgroup
            rmdir /sys/fs/cgroup/grey-optimizer 2>/dev/null || true
            log_success "Removed Grey Optimizer cgroup"
        fi
        
        # Reset scheduler settings (can't fully reverse, but reset to defaults)
        # This is a no-op; process priorities return to normal on exit
        
        log_success "Linux enforcement rolled back"
    
    elif [[ "$OS" == "macos" ]]; then
        # macOS: priorities return to normal when process exits
        log_success "macOS enforcement rolled back (automatic)"
    fi
}

# ─────────────────────────────────────────────────────────────────────────────
# Stop Service
# ─────────────────────────────────────────────────────────────────────────────

stop_service() {
    log_info "Stopping Grey Optimizer service..."
    
    if [[ "$OS" == "linux" ]]; then
        # Stop timer first
        if systemctl is-active --quiet "${TIMER_NAME}.timer" 2>/dev/null; then
            systemctl stop "${TIMER_NAME}.timer" || true
            systemctl disable "${TIMER_NAME}.timer" || true
            log_success "Stopped refresh timer"
        fi
        
        # Stop main service
        if systemctl is-active --quiet "${SERVICE_NAME}.service" 2>/dev/null; then
            systemctl stop "${SERVICE_NAME}.service" || true
            systemctl disable "${SERVICE_NAME}.service" || true
            log_success "Stopped main service"
        fi
        
        # Remove unit files
        rm -f "/etc/systemd/system/${SERVICE_NAME}.service"
        rm -f "/etc/systemd/system/${TIMER_NAME}.timer"
        rm -f "/etc/systemd/system/${TIMER_NAME}.service"
        
        systemctl daemon-reload
        log_success "Removed systemd units"
    
    elif [[ "$OS" == "macos" ]]; then
        PLIST_PATH="/Library/LaunchDaemons/com.grey.optimizer.plist"
        
        if [[ -f "$PLIST_PATH" ]]; then
            launchctl unload "$PLIST_PATH" 2>/dev/null || true
            rm -f "$PLIST_PATH"
            log_success "Removed launchd daemon"
        fi
    fi
}

# ─────────────────────────────────────────────────────────────────────────────
# Remove Files
# ─────────────────────────────────────────────────────────────────────────────

remove_files() {
    log_info "Removing program files..."
    
    # Remove installation directory
    if [[ -d "$INSTALL_DIR" ]]; then
        rm -rf "$INSTALL_DIR"
        log_success "Removed ${INSTALL_DIR}"
    else
        log_info "Installation directory not found (already removed?)"
    fi
    
    # Remove CLI
    for cli in "${CLI_PATHS[@]}"; do
        if [[ -f "$cli" ]] || [[ -L "$cli" ]]; then
            rm -f "$cli"
            log_success "Removed ${cli}"
        fi
    done
    
    # Remove config and logs if not keeping data
    if [[ "$KEEP_DATA" != "true" ]]; then
        if [[ -d "$CONFIG_DIR" ]]; then
            rm -rf "$CONFIG_DIR"
            log_success "Removed configuration: ${CONFIG_DIR}"
        fi
        
        if [[ -d "$LOG_DIR" ]]; then
            rm -rf "$LOG_DIR"
            log_success "Removed logs: ${LOG_DIR}"
        fi
    else
        log_info "Keeping configuration and logs (--keep-data)"
    fi
    
    # Remove backup directory if empty
    if [[ -d "$BACKUP_DIR" ]] && [[ -z "$(ls -A "$BACKUP_DIR")" ]]; then
        rmdir "$BACKUP_DIR"
        log_info "Removed empty backup directory"
    fi
}

# ─────────────────────────────────────────────────────────────────────────────
# Restore Kernel Settings
# ─────────────────────────────────────────────────────────────────────────────

restore_kernel_settings() {
    log_info "Restoring kernel settings to defaults..."
    
    if [[ "$OS" == "linux" ]]; then
        # Restore swappiness to default (60)
        if [[ -f /proc/sys/vm/swappiness ]]; then
            echo 60 > /proc/sys/vm/swappiness
            log_success "Restored vm.swappiness to 60"
        fi
        
        # Restore dirty ratio to default (20)
        if [[ -f /proc/sys/vm/dirty_ratio ]]; then
            echo 20 > /proc/sys/vm/dirty_ratio
            log_success "Restored vm.dirty_ratio to 20"
        fi
        
        # Restore dirty_background_ratio to default (10)
        if [[ -f /proc/sys/vm/dirty_background_ratio ]]; then
            echo 10 > /proc/sys/vm/dirty_background_ratio
            log_success "Restored vm.dirty_background_ratio to 10"
        fi
        
        # Remove any sysctl.d overrides we created
        if [[ -f /etc/sysctl.d/99-grey-optimizer.conf ]]; then
            rm -f /etc/sysctl.d/99-grey-optimizer.conf
            sysctl --system >/dev/null 2>&1 || true
            log_success "Removed sysctl overrides"
        fi
        
        # Disable KSM if we enabled it
        if [[ -f /sys/kernel/mm/ksm/run ]]; then
            echo 0 > /sys/kernel/mm/ksm/run
            log_success "Disabled KSM"
        fi
    
    elif [[ "$OS" == "macos" ]]; then
        log_info "macOS kernel settings are ephemeral (reset on reboot)"
    fi
}

# ─────────────────────────────────────────────────────────────────────────────
# Remove User/Group
# ─────────────────────────────────────────────────────────────────────────────

remove_user() {
    log_info "Checking for Grey Optimizer user..."
    
    if id "grey-optimizer" &>/dev/null; then
        if [[ "$OS" == "linux" ]]; then
            userdel grey-optimizer 2>/dev/null || true
            log_success "Removed user: grey-optimizer"
        elif [[ "$OS" == "macos" ]]; then
            dscl . -delete /Users/grey-optimizer 2>/dev/null || true
            log_success "Removed user: grey-optimizer"
        fi
    fi
    
    if getent group grey-optimizer &>/dev/null 2>&1 || dscl . -read /Groups/grey-optimizer &>/dev/null 2>&1; then
        if [[ "$OS" == "linux" ]]; then
            groupdel grey-optimizer 2>/dev/null || true
            log_success "Removed group: grey-optimizer"
        elif [[ "$OS" == "macos" ]]; then
            dscl . -delete /Groups/grey-optimizer 2>/dev/null || true
            log_success "Removed group: grey-optimizer"
        fi
    fi
}

# ─────────────────────────────────────────────────────────────────────────────
# Main Uninstall Flow
# ─────────────────────────────────────────────────────────────────────────────

main() {
    echo ""
    log_info "Starting Grey Optimizer uninstallation..."
    echo ""
    
    # Step 1: Rollback any active enforcement
    rollback_enforcement
    
    # Step 2: Stop and remove service
    stop_service
    
    # Step 3: Remove files
    remove_files
    
    # Step 4: Restore kernel settings
    restore_kernel_settings
    
    # Step 5: Remove user/group (optional, may want to keep for audit)
    # remove_user  # Uncomment to remove user
    
    echo ""
    echo "═══════════════════════════════════════════════════════════════════"
    log_success "Grey Optimizer has been uninstalled successfully!"
    echo "═══════════════════════════════════════════════════════════════════"
    
    if [[ "$KEEP_DATA" == "true" ]]; then
        echo ""
        echo "Configuration and logs have been preserved:"
        echo "  • ${CONFIG_DIR}"
        echo "  • ${LOG_DIR}"
        echo ""
        echo "To fully remove all data, run:"
        echo "  sudo rm -rf ${CONFIG_DIR} ${LOG_DIR}"
    fi
    
    echo ""
}

# Run main
main "$@"
