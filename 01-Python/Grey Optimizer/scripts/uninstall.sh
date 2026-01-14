#!/bin/bash
# ═══════════════════════════════════════════════════════════════════════════════
# Grey Optimizer - Linux Uninstaller
# ═══════════════════════════════════════════════════════════════════════════════
#
# Safely removes Grey Optimizer from Linux systems.
#
# Usage:
#   ./uninstall.sh                      # Dry run (shows what would be removed)
#   ./uninstall.sh --confirm-uninstall  # Actually remove
#   ./uninstall.sh --preserve-logs      # Keep logs and audit DB
#
# Safety:
#   - Dry run by default (requires --confirm-uninstall for actual removal)
#   - Stops service before removal
#   - Restores backups if available
#   - Idempotent (safe to run multiple times)
#   - All actions logged to audit DB
#
# ═══════════════════════════════════════════════════════════════════════════════

set -euo pipefail

# ─────────────────────────────────────────────────────────────────────────────
# Configuration
# ─────────────────────────────────────────────────────────────────────────────

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VERSION="1.0.0"

# Installation paths
INSTALL_DIR="/opt/grey-optimizer"
CONFIG_DIR="/etc/grey-optimizer"
LOG_DIR="/var/log/grey-optimizer"
DATA_DIR="/var/lib/grey-optimizer"
BACKUP_DIR="/var/backup/grey-optimizer"
SYSTEMD_DIR="/etc/systemd/system"

# Service configuration
SERVICE_NAME="grey-optimizer"
SERVICE_USER="grey-optimizer"

# CLI paths
CLI_PATHS=(
    "/usr/local/bin/greyctl"
    "/usr/bin/greyctl"
)

# ─────────────────────────────────────────────────────────────────────────────
# Flags
# ─────────────────────────────────────────────────────────────────────────────

CONFIRM_UNINSTALL=false
PRESERVE_LOGS=false
DRY_RUN=true

# ─────────────────────────────────────────────────────────────────────────────
# Colors
# ─────────────────────────────────────────────────────────────────────────────

if [[ -t 1 ]]; then
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[1;33m'
    BLUE='\033[0;34m'
    CYAN='\033[0;36m'
    BOLD='\033[1m'
    NC='\033[0m'
else
    RED='' GREEN='' YELLOW='' BLUE='' CYAN='' BOLD='' NC=''
fi

log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[OK]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1" >&2; }
log_dry() { echo -e "${CYAN}[DRY RUN]${NC} Would: $1"; }

# ─────────────────────────────────────────────────────────────────────────────
# Audit Functions
# ─────────────────────────────────────────────────────────────────────────────

AUDIT_SECRET="${GREY_AUDIT_SECRET:-$(hostname)-grey-optimizer-audit}"

audit_log() {
    local action="$1" category="$2" details="${3:-}" success="${4:-1}"
    local db_path="${DATA_DIR}/audit.db"
    
    [[ ! -f "$db_path" ]] && return 0
    [[ "$DRY_RUN" == "true" ]] && return 0
    
    local timestamp
    timestamp=$(date -u +%s)
    local sign_data="${action}|${category}|${details}|${success}|${timestamp}"
    local signature
    signature=$(echo -n "$sign_data" | openssl dgst -sha256 -hmac "$AUDIT_SECRET" 2>/dev/null | awk '{print $NF}')
    
    sqlite3 "$db_path" "INSERT INTO audit_log (action, category, details, mode, user, hostname, success, signature) VALUES ('$action', '$category', '$(echo "$details" | sed "s/'/''/g")', 'uninstall', '$(whoami)', '$(hostname)', $success, '$signature');" 2>/dev/null || true
}

# ─────────────────────────────────────────────────────────────────────────────
# Uninstall Functions
# ─────────────────────────────────────────────────────────────────────────────

stop_service() {
    log_info "Stopping service..."
    
    # Check if service exists
    if ! systemctl list-unit-files "${SERVICE_NAME}.service" &>/dev/null; then
        log_info "Service not found (already removed?)"
        return 0
    fi
    
    if [[ "$DRY_RUN" == "true" ]]; then
        log_dry "systemctl stop $SERVICE_NAME"
        log_dry "systemctl disable $SERVICE_NAME"
        return 0
    fi
    
    # Stop timer if exists
    if systemctl is-active --quiet "${SERVICE_NAME}.timer" 2>/dev/null; then
        systemctl stop "${SERVICE_NAME}.timer" || true
        systemctl disable "${SERVICE_NAME}.timer" || true
        log_success "Stopped timer"
    fi
    
    # Stop main service
    if systemctl is-active --quiet "${SERVICE_NAME}.service" 2>/dev/null; then
        systemctl stop "${SERVICE_NAME}.service" || true
        log_success "Stopped service"
    fi
    
    # Disable service
    if systemctl is-enabled --quiet "${SERVICE_NAME}.service" 2>/dev/null; then
        systemctl disable "${SERVICE_NAME}.service" || true
        log_success "Disabled service"
    fi
    
    audit_log "service_stopped" "uninstall" "name=$SERVICE_NAME"
}

remove_service_files() {
    log_info "Removing service files..."
    
    local files=(
        "$SYSTEMD_DIR/${SERVICE_NAME}.service"
        "$SYSTEMD_DIR/${SERVICE_NAME}.timer"
        "$SYSTEMD_DIR/${SERVICE_NAME}-refresh.service"
    )
    
    for file in "${files[@]}"; do
        if [[ -f "$file" ]]; then
            if [[ "$DRY_RUN" == "true" ]]; then
                log_dry "Remove $file"
            else
                rm -f "$file"
                log_success "Removed: $file"
            fi
        fi
    done
    
    if [[ "$DRY_RUN" == "false" ]]; then
        systemctl daemon-reload
        audit_log "service_files_removed" "uninstall" "systemd_dir=$SYSTEMD_DIR"
    fi
}

remove_installation() {
    log_info "Removing installation directory..."
    
    if [[ -d "$INSTALL_DIR" ]]; then
        if [[ "$DRY_RUN" == "true" ]]; then
            log_dry "Remove $INSTALL_DIR"
        else
            rm -rf "$INSTALL_DIR"
            log_success "Removed: $INSTALL_DIR"
            audit_log "install_dir_removed" "uninstall" "path=$INSTALL_DIR"
        fi
    else
        log_info "Installation directory not found"
    fi
}

remove_cli() {
    log_info "Removing CLI tool..."
    
    for cli in "${CLI_PATHS[@]}"; do
        if [[ -f "$cli" ]] || [[ -L "$cli" ]]; then
            if [[ "$DRY_RUN" == "true" ]]; then
                log_dry "Remove $cli"
            else
                rm -f "$cli"
                log_success "Removed: $cli"
            fi
        fi
    done
    
    if [[ "$DRY_RUN" == "false" ]]; then
        audit_log "cli_removed" "uninstall" "paths=${CLI_PATHS[*]}"
    fi
}

remove_config() {
    log_info "Removing configuration..."
    
    if [[ -d "$CONFIG_DIR" ]]; then
        if [[ "$DRY_RUN" == "true" ]]; then
            log_dry "Remove $CONFIG_DIR"
        else
            rm -rf "$CONFIG_DIR"
            log_success "Removed: $CONFIG_DIR"
            audit_log "config_removed" "uninstall" "path=$CONFIG_DIR"
        fi
    else
        log_info "Configuration directory not found"
    fi
}

remove_logs_and_data() {
    if [[ "$PRESERVE_LOGS" == "true" ]]; then
        log_info "Preserving logs and audit data (--preserve-logs)"
        return 0
    fi
    
    log_info "Removing logs and data..."
    
    # Remove logs
    if [[ -d "$LOG_DIR" ]]; then
        if [[ "$DRY_RUN" == "true" ]]; then
            log_dry "Remove $LOG_DIR"
        else
            rm -rf "$LOG_DIR"
            log_success "Removed: $LOG_DIR"
        fi
    fi
    
    # Remove data (including audit DB)
    if [[ -d "$DATA_DIR" ]]; then
        if [[ "$DRY_RUN" == "true" ]]; then
            log_dry "Remove $DATA_DIR"
        else
            rm -rf "$DATA_DIR"
            log_success "Removed: $DATA_DIR"
        fi
    fi
}

restore_backups() {
    log_info "Checking for system state to restore..."
    
    # Look for the most recent backup
    if [[ ! -d "$BACKUP_DIR" ]]; then
        log_info "No backups found"
        return 0
    fi
    
    local latest_backup
    latest_backup=$(ls -t "$BACKUP_DIR" 2>/dev/null | head -1)
    
    if [[ -z "$latest_backup" ]]; then
        log_info "No backups to restore"
        return 0
    fi
    
    local backup_path="$BACKUP_DIR/$latest_backup"
    log_info "Found backup: $backup_path"
    
    if [[ "$DRY_RUN" == "true" ]]; then
        log_dry "Restore any kernel/cgroup settings from backup"
        return 0
    fi
    
    # Restore any kernel settings if saved
    if [[ -f "$backup_path/sysctl.conf" ]]; then
        log_info "Restoring sysctl settings..."
        sysctl -p "$backup_path/sysctl.conf" 2>/dev/null || true
    fi
    
    audit_log "backups_restored" "uninstall" "backup_path=$backup_path"
}

remove_user() {
    log_info "Checking service user..."
    
    if ! id "$SERVICE_USER" &>/dev/null; then
        log_info "Service user not found"
        return 0
    fi
    
    if [[ "$DRY_RUN" == "true" ]]; then
        log_dry "Remove user: $SERVICE_USER"
        return 0
    fi
    
    # Don't remove user by default (may have files, audit trail)
    log_warn "Service user '$SERVICE_USER' not removed (manual removal: userdel $SERVICE_USER)"
}

cleanup_cgroups() {
    log_info "Cleaning up cgroups..."
    
    local cgroup_path="/sys/fs/cgroup/grey-optimizer"
    
    if [[ -d "$cgroup_path" ]]; then
        if [[ "$DRY_RUN" == "true" ]]; then
            log_dry "Remove cgroup: $cgroup_path"
            return 0
        fi
        
        # Move processes back to root cgroup
        if [[ -f "$cgroup_path/cgroup.procs" ]]; then
            while read -r pid; do
                echo "$pid" > /sys/fs/cgroup/cgroup.procs 2>/dev/null || true
            done < "$cgroup_path/cgroup.procs"
        fi
        
        rmdir "$cgroup_path" 2>/dev/null || true
        log_success "Removed cgroup"
        audit_log "cgroup_removed" "uninstall" "path=$cgroup_path"
    fi
}

restore_kernel_settings() {
    log_info "Restoring kernel settings to defaults..."
    
    if [[ "$DRY_RUN" == "true" ]]; then
        log_dry "Restore vm.swappiness to 60"
        log_dry "Restore vm.dirty_ratio to 20"
        log_dry "Disable KSM"
        return 0
    fi
    
    # Restore swappiness
    if [[ -f /proc/sys/vm/swappiness ]]; then
        echo 60 > /proc/sys/vm/swappiness
    fi
    
    # Restore dirty ratios
    if [[ -f /proc/sys/vm/dirty_ratio ]]; then
        echo 20 > /proc/sys/vm/dirty_ratio
    fi
    
    if [[ -f /proc/sys/vm/dirty_background_ratio ]]; then
        echo 10 > /proc/sys/vm/dirty_background_ratio
    fi
    
    # Disable KSM
    if [[ -f /sys/kernel/mm/ksm/run ]]; then
        echo 0 > /sys/kernel/mm/ksm/run
    fi
    
    # Remove sysctl overrides
    if [[ -f /etc/sysctl.d/99-grey-optimizer.conf ]]; then
        rm -f /etc/sysctl.d/99-grey-optimizer.conf
        sysctl --system >/dev/null 2>&1 || true
    fi
    
    log_success "Kernel settings restored"
    audit_log "kernel_settings_restored" "uninstall" ""
}

# ─────────────────────────────────────────────────────────────────────────────
# Help
# ─────────────────────────────────────────────────────────────────────────────

show_help() {
    cat <<EOF
Grey Optimizer Uninstaller v${VERSION}

Usage: $0 [OPTIONS]

Options:
  --confirm-uninstall   Actually remove (without this, performs dry run)
  --preserve-logs       Keep log files and audit database
  --help, -h            Show this help

Examples:
  # Dry run - show what would be removed
  $0
  
  # Actually uninstall
  sudo $0 --confirm-uninstall
  
  # Uninstall but keep logs for audit
  sudo $0 --confirm-uninstall --preserve-logs

Safety:
  - Dry run by default (shows what would happen)
  - Requires --confirm-uninstall for actual removal
  - Idempotent - safe to run multiple times
  - Restores kernel settings to defaults

EOF
}

# ─────────────────────────────────────────────────────────────────────────────
# Argument Parsing
# ─────────────────────────────────────────────────────────────────────────────

parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --confirm-uninstall)
                CONFIRM_UNINSTALL=true
                DRY_RUN=false
                shift
                ;;
            --preserve-logs)
                PRESERVE_LOGS=true
                shift
                ;;
            --help|-h)
                show_help
                exit 0
                ;;
            *)
                log_error "Unknown option: $1"
                show_help
                exit 1
                ;;
        esac
    done
}

# ─────────────────────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────────────────────

main() {
    parse_args "$@"
    
    echo ""
    echo -e "${BOLD}Grey Optimizer Uninstaller v${VERSION}${NC}"
    echo ""
    
    if [[ "$DRY_RUN" == "true" ]]; then
        echo -e "${CYAN}═══════════════════════════════════════════════════════════════════${NC}"
        echo -e "${CYAN}                         DRY RUN MODE${NC}"
        echo -e "${CYAN}   No changes will be made. Use --confirm-uninstall to remove.${NC}"
        echo -e "${CYAN}═══════════════════════════════════════════════════════════════════${NC}"
        echo ""
    else
        # Check for root
        if [[ $EUID -ne 0 ]]; then
            log_error "Uninstall requires root privileges. Use sudo."
            exit 1
        fi
        
        echo -e "${RED}═══════════════════════════════════════════════════════════════════${NC}"
        echo -e "${RED}                       LIVE UNINSTALL${NC}"
        echo -e "${RED}   Grey Optimizer will be removed from this system.${NC}"
        echo -e "${RED}═══════════════════════════════════════════════════════════════════${NC}"
        echo ""
    fi
    
    # Log start of uninstall
    audit_log "uninstall_started" "uninstall" "dry_run=$DRY_RUN preserve_logs=$PRESERVE_LOGS"
    
    # Uninstall steps
    stop_service
    remove_service_files
    cleanup_cgroups
    restore_kernel_settings
    restore_backups
    remove_cli
    remove_installation
    remove_config
    remove_logs_and_data
    remove_user
    
    # Log completion
    audit_log "uninstall_completed" "uninstall" "success=true"
    
    echo ""
    if [[ "$DRY_RUN" == "true" ]]; then
        echo -e "${CYAN}═══════════════════════════════════════════════════════════════════${NC}"
        echo -e "${CYAN}   Dry run complete. To actually uninstall, run:${NC}"
        echo -e "${CYAN}   sudo $0 --confirm-uninstall${NC}"
        echo -e "${CYAN}═══════════════════════════════════════════════════════════════════${NC}"
    else
        echo -e "${GREEN}═══════════════════════════════════════════════════════════════════${NC}"
        echo -e "${GREEN}   Grey Optimizer has been uninstalled.${NC}"
        echo -e "${GREEN}═══════════════════════════════════════════════════════════════════${NC}"
        
        if [[ "$PRESERVE_LOGS" == "true" ]]; then
            echo ""
            echo "Preserved data:"
            echo "  Logs: $LOG_DIR"
            echo "  Data: $DATA_DIR"
        fi
    fi
    echo ""
}

main "$@"
