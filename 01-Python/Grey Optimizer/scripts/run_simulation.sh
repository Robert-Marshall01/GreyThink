#!/bin/bash
# ═══════════════════════════════════════════════════════════════════════════════
# Grey Optimizer - Simulation Mode Runner
# ═══════════════════════════════════════════════════════════════════════════════
#
# This script runs Grey Optimizer in SAFE SIMULATION MODE.
# No actual changes are made to the system.
#
# Usage:
#   ./run_simulation.sh                    # Run with default balanced profile
#   ./run_simulation.sh --profile aggressive
#   ./run_simulation.sh --pid 1234         # Target specific PID
#   ./run_simulation.sh --with-workloads   # Run with synthetic workloads
#
# ═══════════════════════════════════════════════════════════════════════════════

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

# Default settings
PROFILE="balanced"
DURATION=60
TARGET_PIDS=""
WITH_WORKLOADS=false
VENV_DIR="${PROJECT_DIR}/.venv"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --profile)
            PROFILE="$2"
            shift 2
            ;;
        --duration)
            DURATION="$2"
            shift 2
            ;;
        --pid)
            TARGET_PIDS="${TARGET_PIDS} --pid $2"
            shift 2
            ;;
        --with-workloads)
            WITH_WORKLOADS=true
            shift
            ;;
        --help|-h)
            echo "Grey Optimizer - Simulation Mode"
            echo ""
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --profile PROFILE   Reclamation profile: conservative, balanced, aggressive"
            echo "  --duration SECS     Simulation duration in seconds (default: 60)"
            echo "  --pid PID           Target specific PID (can be repeated)"
            echo "  --with-workloads    Start synthetic workloads for testing"
            echo "  --help              Show this help"
            echo ""
            echo "This script runs in SIMULATION MODE - no actual changes are made."
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            exit 1
            ;;
    esac
done

echo -e "${CYAN}═══════════════════════════════════════════════════════════════════${NC}"
echo -e "${CYAN}  Grey Optimizer - Simulation Mode${NC}"
echo -e "${CYAN}═══════════════════════════════════════════════════════════════════${NC}"
echo ""
echo -e "${BLUE}Profile:${NC}  ${PROFILE}"
echo -e "${BLUE}Duration:${NC} ${DURATION}s"
echo ""

# Create data directories
mkdir -p "$PROJECT_DIR/data/proofs"
mkdir -p "$PROJECT_DIR/logs"

# Check if venv exists, create if not
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

# Build C helpers if not present
build_c_helpers() {
    local MADVISE_HELPER="${PROJECT_DIR}/c/madvise_helper"
    local CGROUP_HELPER="${PROJECT_DIR}/c/cgroup_helper"

    if [[ ! -f "$MADVISE_HELPER" ]] || [[ ! -f "$CGROUP_HELPER" ]]; then
        if [[ -d "${PROJECT_DIR}/c" ]] && [[ -f "${PROJECT_DIR}/c/Makefile" ]]; then
            echo -e "${YELLOW}Building C helpers...${NC}"
            cd "${PROJECT_DIR}/c"
            make clean 2>/dev/null || true
            make all 2>/dev/null || echo -e "${YELLOW}C helper build skipped (missing dependencies)${NC}"
            cd "$PROJECT_DIR"
        fi
    fi
}

# Start synthetic workloads in background
start_workloads() {
    echo -e "${BLUE}Starting synthetic workloads...${NC}"
    
    # Memory-heavy process (Python)
    python3 -c "
import time
import sys
data = []
print('Memory hog started, PID:', flush=True)
try:
    while True:
        data.append(b'x' * 1000000)  # 1MB chunks
        if len(data) > 100:
            data = data[-50:]  # Keep last 50MB
        time.sleep(0.1)
except KeyboardInterrupt:
    pass
" &
    MEM_PID=$!
    
    # CPU-heavy process
    (while true; do 
        : $((i=i+1))
        if [[ $i -gt 1000000 ]]; then i=0; fi
    done) &
    CPU_PID=$!
    
    echo -e "${GREEN}✓ Workloads started (PIDs: $CPU_PID, $MEM_PID)${NC}"
    TARGET_PIDS="${TARGET_PIDS} --pid $MEM_PID"
    
    # Store PIDs for cleanup
    echo "$CPU_PID $MEM_PID" > /tmp/grey_optimizer_workload_pids
}

# Stop workloads
stop_workloads() {
    echo -e "${BLUE}Stopping synthetic workloads...${NC}"
    
    if [[ -f /tmp/grey_optimizer_workload_pids ]]; then
        read -r PIDS < /tmp/grey_optimizer_workload_pids
        for PID in $PIDS; do
            kill "$PID" 2>/dev/null || true
        done
        rm -f /tmp/grey_optimizer_workload_pids
    fi
    
    echo -e "${GREEN}✓ Workloads stopped${NC}"
}

# Cleanup on exit
cleanup() {
    echo ""
    if [[ "$WITH_WORKLOADS" == "true" ]]; then
        stop_workloads
    fi
    echo -e "${GREEN}Done!${NC}"
}
trap cleanup EXIT

# Main
main() {
    setup_venv
    build_c_helpers
    
    # Ensure greyctl is executable
    GREYCTL="${PROJECT_DIR}/greyctl"
    if [[ ! -x "$GREYCTL" ]]; then
        chmod +x "$GREYCTL"
    fi
    
    if [[ "$WITH_WORKLOADS" == "true" ]]; then
        start_workloads
        sleep 2  # Let workloads initialize
    fi
    
    echo ""
    echo -e "${GREEN}Running simulation...${NC}"
    echo ""
    
    # Run simulation via greyctl
    "$GREYCTL" simulate --profile "$PROFILE" --duration "$DURATION" $TARGET_PIDS
    
    echo ""
    echo -e "${CYAN}═══════════════════════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}Simulation complete.${NC}"
    echo ""
    echo -e "To apply these changes for real:"
    echo -e "  ${YELLOW}sudo ./scripts/run_live.sh --profile ${PROFILE} --confirm-live${NC}"
    echo ""
}

main "$@"
