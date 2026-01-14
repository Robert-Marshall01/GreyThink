#!/bin/bash
# ═══════════════════════════════════════════════════════════════════════════════
# Grey Optimizer - Live Mode Runner
# ═══════════════════════════════════════════════════════════════════════════════
#
# This script runs Grey Optimizer in LIVE MODE.
# ACTUAL CHANGES WILL BE MADE TO THE SYSTEM.
#
# REQUIREMENTS:
#   - Must be run as root (sudo)
#   - Must provide --confirm-live flag
#
# Usage:
#   sudo ./run_live.sh --profile balanced --confirm-live
#   sudo ./run_live.sh --profile aggressive --confirm-live --pid 1234
#
# ═══════════════════════════════════════════════════════════════════════════════

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

# Default settings
PROFILE="balanced"
TARGET_PIDS=""
CONFIRM_LIVE=false
VENV_DIR="${PROJECT_DIR}/.venv"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --profile)
            PROFILE="$2"
            shift 2
            ;;
        --pid)
            TARGET_PIDS="${TARGET_PIDS} --pid $2"
            shift 2
            ;;
        --confirm-live)
            CONFIRM_LIVE=true
            shift
            ;;
        --help|-h)
            echo "Grey Optimizer - Live Mode"
            echo ""
            echo "Usage: sudo $0 [OPTIONS] --confirm-live"
            echo ""
            echo "Options:"
            echo "  --profile PROFILE   Reclamation profile: conservative, balanced, aggressive"
            echo "  --pid PID           Target specific PID (can be repeated)"
            echo "  --confirm-live      REQUIRED: Confirm you understand this makes real changes"
            echo "  --help              Show this help"
            echo ""
            echo -e "${RED}WARNING: This script makes REAL changes to the system!${NC}"
            echo -e "${RED}Always run simulation first: ./run_simulation.sh${NC}"
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            exit 1
            ;;
    esac
done

# ═══════════════════════════════════════════════════════════════════════════════
# Safety Checks
# ═══════════════════════════════════════════════════════════════════════════════

# Check for --confirm-live flag
if [[ "$CONFIRM_LIVE" != "true" ]]; then
    echo -e "${RED}═══════════════════════════════════════════════════════════════════${NC}"
    echo -e "${RED}  ERROR: --confirm-live flag is REQUIRED${NC}"
    echo -e "${RED}═══════════════════════════════════════════════════════════════════${NC}"
    echo ""
    echo -e "${YELLOW}This script makes REAL changes to the system:${NC}"
    echo "  - Sets cgroup memory limits"
    echo "  - Calls madvise(MADV_DONTNEED) on process memory"
    echo "  - Enables/disables KSM (Kernel Same-page Merging)"
    echo "  - Configures zram (compressed swap)"
    echo "  - May drop page caches"
    echo ""
    echo -e "${BLUE}First, run a simulation to see estimated impact:${NC}"
    echo "  ./scripts/run_simulation.sh --profile $PROFILE"
    echo ""
    echo -e "${BLUE}Then, to apply for real:${NC}"
    echo "  sudo ./scripts/run_live.sh --profile $PROFILE --confirm-live"
    echo ""
    exit 1
fi

# Check for root privileges
if [[ $EUID -ne 0 ]]; then
    echo -e "${RED}═══════════════════════════════════════════════════════════════════${NC}"
    echo -e "${RED}  ERROR: This script must be run as root${NC}"
    echo -e "${RED}═══════════════════════════════════════════════════════════════════${NC}"
    echo ""
    echo "Run with sudo:"
    echo "  sudo ./scripts/run_live.sh --profile $PROFILE --confirm-live"
    echo ""
    exit 1
fi

# ═══════════════════════════════════════════════════════════════════════════════
# Display Warning Banner
# ═══════════════════════════════════════════════════════════════════════════════

echo ""
echo -e "${MAGENTA}╔══════════════════════════════════════════════════════════════════╗${NC}"
echo -e "${MAGENTA}║                                                                  ║${NC}"
echo -e "${MAGENTA}║        ${RED}⚠  LIVE MODE - REAL CHANGES WILL BE APPLIED  ⚠${MAGENTA}         ║${NC}"
echo -e "${MAGENTA}║                                                                  ║${NC}"
echo -e "${MAGENTA}╚══════════════════════════════════════════════════════════════════╝${NC}"
echo ""
echo -e "${BLUE}Profile:${NC}      ${PROFILE}"
echo -e "${BLUE}Target PIDs:${NC}  ${TARGET_PIDS:-all applicable}"
echo ""

# Final confirmation prompt
echo -e "${YELLOW}You are about to apply REAL memory reclamation.${NC}"
echo -e "${YELLOW}A backup will be created for rollback.${NC}"
echo ""
read -p "Are you sure you want to continue? [y/N] " -n 1 -r
echo ""

if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo -e "${BLUE}Aborted.${NC}"
    exit 0
fi

echo ""

# ═══════════════════════════════════════════════════════════════════════════════
# Setup
# ═══════════════════════════════════════════════════════════════════════════════

# Create data directories
mkdir -p "$PROJECT_DIR/data/proofs"
mkdir -p "$PROJECT_DIR/logs"
mkdir -p /var/lib/grey-optimizer/proofs

# Setup virtual environment
setup_venv() {
    if [[ ! -d "$VENV_DIR" ]]; then
        echo -e "${YELLOW}Creating Python virtual environment...${NC}"
        python3 -m venv "$VENV_DIR"
        
        echo -e "${YELLOW}Installing dependencies...${NC}"
        source "$VENV_DIR/bin/activate"
        pip install --quiet --upgrade pip
        pip install --quiet aiohttp aiosqlite pyyaml psutil requests
    else
        source "$VENV_DIR/bin/activate"
    fi
}

# Build C helpers
build_c_helpers() {
    local MADVISE_HELPER="${PROJECT_DIR}/c/madvise_helper"
    local CGROUP_HELPER="${PROJECT_DIR}/c/cgroup_helper"

    if [[ ! -f "$MADVISE_HELPER" ]] || [[ ! -f "$CGROUP_HELPER" ]]; then
        if [[ -d "${PROJECT_DIR}/c" ]] && [[ -f "${PROJECT_DIR}/c/Makefile" ]]; then
            echo -e "${YELLOW}Building C helpers...${NC}"
            cd "${PROJECT_DIR}/c"
            make clean 2>/dev/null || true
            make all
            cd "$PROJECT_DIR"
        fi
    fi
}

# ═══════════════════════════════════════════════════════════════════════════════
# Execute
# ═══════════════════════════════════════════════════════════════════════════════

setup_venv
build_c_helpers

# Ensure greyctl is executable
GREYCTL="${PROJECT_DIR}/greyctl"
if [[ ! -x "$GREYCTL" ]]; then
    chmod +x "$GREYCTL"
fi

echo ""
echo -e "${MAGENTA}Applying memory reclamation...${NC}"
echo ""

# Run live reclamation
"$GREYCTL" apply --profile "$PROFILE" --confirm-live $TARGET_PIDS

EXIT_CODE=$?

echo ""
if [[ $EXIT_CODE -eq 0 ]]; then
    echo -e "${GREEN}═══════════════════════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}  Memory reclamation completed successfully!${NC}"
    echo -e "${GREEN}═══════════════════════════════════════════════════════════════════${NC}"
    echo ""
    echo "View the proof artifact:"
    echo "  greyctl proof --list"
    echo ""
    echo "If you need to rollback:"
    echo "  sudo greyctl rollback"
else
    echo -e "${RED}═══════════════════════════════════════════════════════════════════${NC}"
    echo -e "${RED}  Memory reclamation encountered errors (exit code: $EXIT_CODE)${NC}"
    echo -e "${RED}═══════════════════════════════════════════════════════════════════${NC}"
    echo ""
    echo "Check the audit log:"
    echo "  greyctl audit --limit 10"
    echo ""
    echo "Consider rollback:"
    echo "  sudo greyctl rollback"
fi

exit $EXIT_CODE
