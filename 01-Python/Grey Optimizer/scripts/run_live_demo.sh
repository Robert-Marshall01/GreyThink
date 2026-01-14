#!/usr/bin/env bash
# ═══════════════════════════════════════════════════════════════════════════════
# Grey Optimizer - Live Demo Script
# ═══════════════════════════════════════════════════════════════════════════════
#
# Interactive demonstration of Grey Optimizer memory enforcement features.
# Walks through KSM, zram, madvise, and drop_caches in both simulation
# and live modes with user confirmation at each step.
#
# Usage:
#   ./scripts/run_live_demo.sh [options]
#
# Options:
#   --auto          Skip interactive prompts (use defaults)
#   --simulation    Run all steps in simulation mode (default)
#   --live          Enable live mode with confirmations
#   -h, --help      Show help
#
# Requirements:
#   - Linux system (for KSM, zram, madvise, drop_caches)
#   - Root/sudo access for live mode
#   - greyctl CLI available
#
# ═══════════════════════════════════════════════════════════════════════════════

set -euo pipefail

# ═══════════════════════════════════════════════════════════════════════════════
# Configuration
# ═══════════════════════════════════════════════════════════════════════════════

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
GREYCTL="${PROJECT_ROOT}/greyctl"

# Demo options
AUTO_MODE=false
LIVE_MODE=false
SIMULATION_MODE=true

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
BOLD='\033[1m'
DIM='\033[2m'
NC='\033[0m'

# ═══════════════════════════════════════════════════════════════════════════════
# Utilities
# ═══════════════════════════════════════════════════════════════════════════════

print_banner() {
    echo ""
    echo -e "${CYAN}╔═══════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${CYAN}║${NC}  ${BOLD}${MAGENTA}Grey Optimizer - Live Demo${NC}                                     ${CYAN}║${NC}"
    echo -e "${CYAN}║${NC}  ${DIM}Interactive Memory Enforcement Demonstration${NC}                     ${CYAN}║${NC}"
    echo -e "${CYAN}╚═══════════════════════════════════════════════════════════════════╝${NC}"
    echo ""
}

print_section() {
    local title="$1"
    echo ""
    echo -e "${BOLD}${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BOLD}${BLUE}  $title${NC}"
    echo -e "${BOLD}${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo ""
}

log_info() { echo -e "${BLUE}ℹ${NC}  $1"; }
log_success() { echo -e "${GREEN}✓${NC}  $1"; }
log_warn() { echo -e "${YELLOW}⚠${NC}  $1"; }
log_error() { echo -e "${RED}✗${NC}  $1"; }
log_step() { echo -e "${MAGENTA}▸${NC}  ${BOLD}$1${NC}"; }

prompt_continue() {
    local message="${1:-Press Enter to continue...}"
    if [[ "$AUTO_MODE" == true ]]; then
        echo -e "${DIM}[$message - auto-continuing]${NC}"
        sleep 1
        return 0
    fi
    echo ""
    read -r -p "$(echo -e "${CYAN}$message${NC}")"
    return 0
}

prompt_yes_no() {
    local question="$1"
    local default="${2:-n}"
    
    if [[ "$AUTO_MODE" == true ]]; then
        echo -e "${DIM}[Auto: using default '$default']${NC}"
        [[ "$default" == "y" ]] && return 0 || return 1
    fi
    
    local prompt
    if [[ "$default" == "y" ]]; then
        prompt="[Y/n]"
    else
        prompt="[y/N]"
    fi
    
    echo ""
    read -r -p "$(echo -e "${YELLOW}$question $prompt: ${NC}")" answer
    
    case "${answer:-$default}" in
        [Yy]* ) return 0 ;;
        * ) return 1 ;;
    esac
}

run_command() {
    local description="$1"
    shift
    local cmd=("$@")
    
    echo ""
    echo -e "${DIM}Command: ${cmd[*]}${NC}"
    echo ""
    
    if "${cmd[@]}"; then
        log_success "$description"
        return 0
    else
        log_error "$description failed"
        return 1
    fi
}

show_help() {
    cat << EOF
Grey Optimizer - Live Demo

An interactive demonstration of memory enforcement features.

Usage: $(basename "$0") [options]

Options:
  --auto          Skip interactive prompts (use defaults)
  --simulation    Run all steps in simulation mode (default)
  --live          Enable live mode with confirmations
  -h, --help      Show this help

The demo will walk through:
  1. System Memory Overview
  2. KSM (Kernel Same-page Merging)
  3. zram Swap Configuration
  4. madvise Memory Reclamation
  5. Kernel Cache Management
  6. Full Memory Optimization

EOF
}

check_prerequisites() {
    print_section "Checking Prerequisites"
    
    local has_errors=false
    
    # Check OS
    if [[ "$(uname -s)" != "Linux" ]]; then
        log_error "This demo requires Linux"
        has_errors=true
    else
        log_success "Running on Linux: $(uname -r)"
    fi
    
    # Check greyctl
    if [[ -x "$GREYCTL" ]]; then
        log_success "greyctl found: $GREYCTL"
    else
        log_error "greyctl not found at: $GREYCTL"
        has_errors=true
    fi
    
    # Check Python
    if command -v python3 &> /dev/null; then
        log_success "Python3: $(python3 --version)"
    else
        log_error "Python3 not found"
        has_errors=true
    fi
    
    # Check root for live mode
    if [[ "$LIVE_MODE" == true ]]; then
        if [[ $EUID -eq 0 ]]; then
            log_success "Running as root (required for live mode)"
        else
            log_warn "Not running as root - live operations will require sudo"
        fi
    fi
    
    # Check KSM availability
    if [[ -d "/sys/kernel/mm/ksm" ]]; then
        log_success "KSM available"
    else
        log_warn "KSM not available on this system"
    fi
    
    # Check zram
    if modinfo zram &>/dev/null 2>&1 || [[ -d "/sys/block/zram0" ]]; then
        log_success "zram module available"
    else
        log_warn "zram may not be available"
    fi
    
    if [[ "$has_errors" == true ]]; then
        log_error "Prerequisites check failed"
        exit 1
    fi
    
    log_success "All prerequisites met"
}

# ═══════════════════════════════════════════════════════════════════════════════
# Demo Steps
# ═══════════════════════════════════════════════════════════════════════════════

demo_memory_overview() {
    print_section "Step 1: System Memory Overview"
    
    log_step "Checking current system memory state..."
    
    echo ""
    echo -e "${BOLD}Memory Info:${NC}"
    free -h
    
    echo ""
    echo -e "${BOLD}Memory Pressure (PSI):${NC}"
    if [[ -f "/proc/pressure/memory" ]]; then
        cat /proc/pressure/memory
    else
        echo "  (PSI not available)"
    fi
    
    echo ""
    echo -e "${BOLD}Top Memory Consumers:${NC}"
    ps aux --sort=-%mem | head -6 | awk '{printf "  %-8s %-20s %s MB\n", $2, substr($11,1,20), $6/1024}'
    
    prompt_continue "Press Enter to run greyctl memory-diagnose..."
    
    run_command "Memory diagnostics" python3 "$GREYCTL" memory-diagnose 2>&1 || true
    
    prompt_continue
}

demo_ksm() {
    print_section "Step 2: KSM (Kernel Same-page Merging)"
    
    log_info "KSM merges identical memory pages to save RAM."
    log_info "Useful for VMs, containers, and apps with redundant data."
    
    echo ""
    log_step "Checking current KSM status..."
    run_command "KSM status" python3 "$GREYCTL" ksm status 2>&1 || true
    
    if prompt_yes_no "Enable KSM (simulation mode)?"; then
        log_step "Enabling KSM in simulation mode..."
        run_command "KSM enable (simulation)" python3 "$GREYCTL" ksm enable 2>&1 || true
    fi
    
    if [[ "$LIVE_MODE" == true ]]; then
        if prompt_yes_no "Enable KSM for REAL (live mode)?"; then
            log_warn "This will modify /sys/kernel/mm/ksm/*"
            run_command "KSM enable (LIVE)" python3 "$GREYCTL" ksm enable --live 2>&1 || true
        fi
    fi
    
    prompt_continue
}

demo_zram() {
    print_section "Step 3: zram Swap Configuration"
    
    log_info "zram creates a compressed swap device in RAM."
    log_info "Improves performance by avoiding disk I/O for swap."
    
    echo ""
    log_step "Checking current zram status..."
    run_command "zram status" python3 "$GREYCTL" zram status 2>&1 || true
    
    echo ""
    echo -e "${BOLD}Current swap devices:${NC}"
    cat /proc/swaps 2>/dev/null || echo "  (no swap configured)"
    
    if prompt_yes_no "Enable zram swap (simulation mode)?"; then
        log_step "Enabling zram in simulation mode..."
        run_command "zram enable (simulation)" python3 "$GREYCTL" zram enable --size 512 2>&1 || true
    fi
    
    if [[ "$LIVE_MODE" == true ]]; then
        if prompt_yes_no "Enable zram swap for REAL (live mode)?"; then
            log_warn "This will create /dev/zram0 and enable it as swap"
            run_command "zram enable (LIVE)" python3 "$GREYCTL" zram enable --size 512 --live 2>&1 || true
        fi
    fi
    
    prompt_continue
}

demo_madvise() {
    print_section "Step 4: madvise Memory Reclamation"
    
    log_info "madvise can reclaim memory from running processes."
    log_info "Uses MADV_DONTNEED to release unused memory pages."
    
    # Find a good target process
    echo ""
    echo -e "${BOLD}Potential target processes (high memory):${NC}"
    ps aux --sort=-%mem | head -6 | tail -5 | awk '{printf "  PID %-8s  %s (%.0f MB)\n", $2, $11, $6/1024}'
    
    local target_pid=$$
    echo ""
    log_info "Using current shell (PID $target_pid) as demo target"
    
    if prompt_yes_no "Simulate madvise reclaim on PID $target_pid?"; then
        log_step "Running madvise reclaim (simulation)..."
        run_command "madvise reclaim (simulation)" python3 "$GREYCTL" madvise reclaim --pid=$target_pid 2>&1 || true
    fi
    
    prompt_continue
}

demo_dropcaches() {
    print_section "Step 5: Kernel Cache Management"
    
    log_info "drop_caches releases pagecache, dentries, and inodes."
    log_info "This operation is safe but may cause temporary I/O increase."
    
    echo ""
    log_step "Analyzing current cache state..."
    run_command "Cache analysis (simulation)" python3 "$GREYCTL" dropcaches simulate 2>&1 || true
    
    if [[ "$LIVE_MODE" == true ]]; then
        if prompt_yes_no "Drop kernel caches for REAL (live mode)?"; then
            log_warn "This will write to /proc/sys/vm/drop_caches"
            log_warn "Requires --confirm-live flag for safety"
            
            if prompt_yes_no "Are you SURE you want to drop caches?"; then
                run_command "Drop caches (LIVE)" python3 "$GREYCTL" dropcaches apply --confirm-live 2>&1 || true
            else
                log_info "Skipped live cache drop"
            fi
        fi
    else
        log_info "Live cache dropping skipped (use --live flag to enable)"
    fi
    
    prompt_continue
}

demo_full_enforcement() {
    print_section "Step 6: Full Memory Optimization"
    
    log_info "memory-apply combines all optimizations:"
    log_info "  • Enable KSM"
    log_info "  • Enable zram"
    log_info "  • Drop caches (with confirmation)"
    
    echo ""
    if prompt_yes_no "Run full memory optimization (simulation mode)?"; then
        log_step "Running full optimization (simulation)..."
        run_command "Full optimization (simulation)" python3 "$GREYCTL" memory-apply 2>&1 || true
    fi
    
    if [[ "$LIVE_MODE" == true ]]; then
        echo ""
        log_warn "LIVE MODE: This will make real system changes!"
        log_warn "Changes include:"
        log_warn "  • Modifying /sys/kernel/mm/ksm/*"
        log_warn "  • Creating zram swap device"
        log_warn "  • Dropping kernel caches"
        
        if prompt_yes_no "Apply full optimization for REAL?"; then
            if prompt_yes_no "Final confirmation - proceed with LIVE changes?"; then
                run_command "Full optimization (LIVE)" python3 "$GREYCTL" memory-apply --confirm-live 2>&1 || true
            fi
        fi
    fi
    
    prompt_continue
}

demo_artifacts() {
    print_section "Step 7: Proof Artifacts"
    
    log_info "Grey Optimizer generates proof artifacts for all actions."
    log_info "Artifacts include before/after snapshots with HMAC signatures."
    
    local artifacts_dir="/var/lib/grey-optimizer/artifacts"
    
    if [[ -d "$artifacts_dir" ]]; then
        echo ""
        echo -e "${BOLD}Artifact Directory:${NC} $artifacts_dir"
        echo ""
        local count
        count=$(find "$artifacts_dir" -name "*.json" 2>/dev/null | wc -l) || count=0
        log_info "Found $count artifact files"
        
        if [[ $count -gt 0 ]]; then
            echo ""
            echo -e "${BOLD}Recent artifacts:${NC}"
            find "$artifacts_dir" -name "*.json" -mmin -60 2>/dev/null | head -5 | while read -r f; do
                echo "  $(basename "$f")"
            done
        fi
    else
        log_info "Artifacts directory not found (daemon may not have run)"
    fi
    
    # Check validate tool
    if [[ -f "${PROJECT_ROOT}/tools/validate_artifact.py" ]]; then
        echo ""
        log_info "Artifact validator available: tools/validate_artifact.py"
    fi
    
    prompt_continue
}

demo_summary() {
    print_section "Demo Complete"
    
    echo ""
    echo -e "${GREEN}${BOLD}Summary:${NC}"
    echo ""
    echo "  Grey Optimizer provides safe, reversible memory enforcement with:"
    echo ""
    echo "  ${CYAN}•${NC} KSM (Kernel Same-page Merging)"
    echo "  ${CYAN}•${NC} zram compressed swap"
    echo "  ${CYAN}•${NC} madvise-based memory reclamation"
    echo "  ${CYAN}•${NC} Safe kernel cache management"
    echo ""
    echo "  ${BOLD}Key Features:${NC}"
    echo "  ${GREEN}✓${NC} Simulation mode by default"
    echo "  ${GREEN}✓${NC} Explicit --confirm-live required for real changes"
    echo "  ${GREEN}✓${NC} Before/after snapshots for all actions"
    echo "  ${GREEN}✓${NC} HMAC-signed proof artifacts"
    echo "  ${GREEN}✓${NC} Rollback support"
    echo ""
    echo "  ${BOLD}Quick Commands:${NC}"
    echo "  ${DIM}greyctl ksm status${NC}              - Check KSM status"
    echo "  ${DIM}greyctl zram status${NC}             - Check zram status"
    echo "  ${DIM}greyctl memory-diagnose${NC}         - Memory diagnostics"
    echo "  ${DIM}greyctl memory-apply${NC}            - Simulate full optimization"
    echo "  ${DIM}greyctl memory-apply --confirm-live${NC} - Apply for real"
    echo ""
    
    log_success "Thank you for trying Grey Optimizer!"
    echo ""
}

# ═══════════════════════════════════════════════════════════════════════════════
# Parse Arguments
# ═══════════════════════════════════════════════════════════════════════════════

while [[ $# -gt 0 ]]; do
    case $1 in
        --auto)
            AUTO_MODE=true
            shift
            ;;
        --simulation)
            SIMULATION_MODE=true
            LIVE_MODE=false
            shift
            ;;
        --live)
            LIVE_MODE=true
            SIMULATION_MODE=false
            shift
            ;;
        -h|--help)
            show_help
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            show_help
            exit 1
            ;;
    esac
done

# ═══════════════════════════════════════════════════════════════════════════════
# Main
# ═══════════════════════════════════════════════════════════════════════════════

main() {
    print_banner
    
    if [[ "$LIVE_MODE" == true ]]; then
        log_warn "LIVE MODE ENABLED - Real system changes will be available"
    else
        log_info "Running in SIMULATION mode (safe, no changes)"
    fi
    
    if [[ "$AUTO_MODE" == true ]]; then
        log_info "Auto mode enabled - using default choices"
    fi
    
    prompt_continue "Press Enter to begin the demo..."
    
    check_prerequisites
    demo_memory_overview
    demo_ksm
    demo_zram
    demo_madvise
    demo_dropcaches
    demo_full_enforcement
    demo_artifacts
    demo_summary
}

main "$@"
