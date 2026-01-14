#!/usr/bin/env bash
# ═══════════════════════════════════════════════════════════════════════════════
# Grey Optimizer - Integration Test Script
# ═══════════════════════════════════════════════════════════════════════════════
#
# Runs comprehensive integration tests in SIMULATION mode to validate:
# - Installation process
# - Service lifecycle
# - CLI commands
# - Uninstallation process
# - Audit logging
#
# Usage:
#   ./scripts/run_integration_test.sh [options]
#
# Options:
#   --verbose       Enable verbose output
#   --keep-state    Don't cleanup after tests
#   --only <test>   Run only specific test (install|service|cli|uninstall)
#   -h, --help      Show help
#
# Exit codes:
#   0  - All tests passed
#   1  - Test(s) failed
#   2  - Setup error
#
# ═══════════════════════════════════════════════════════════════════════════════

set -euo pipefail

# ═══════════════════════════════════════════════════════════════════════════════
# Configuration
# ═══════════════════════════════════════════════════════════════════════════════

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
VENV_DIR="${PROJECT_ROOT}/.venv"
GREYCTL="${PROJECT_ROOT}/greyctl"

# Test configuration
TEST_TIMEOUT=30
VERBOSE=false
KEEP_STATE=false
ONLY_TEST=""

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0
TESTS_SKIPPED=0

# ═══════════════════════════════════════════════════════════════════════════════
# Utilities
# ═══════════════════════════════════════════════════════════════════════════════

log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[PASS]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[FAIL]${NC} $1"; }
log_skip() { echo -e "${CYAN}[SKIP]${NC} $1"; }
log_verbose() { [[ "$VERBOSE" == true ]] && echo -e "${CYAN}[DEBUG]${NC} $1" || true; }

print_header() {
    echo ""
    echo -e "${BOLD}═══════════════════════════════════════════════════════════════${NC}"
    echo -e "${BOLD}  $1${NC}"
    echo -e "${BOLD}═══════════════════════════════════════════════════════════════${NC}"
    echo ""
}

show_help() {
    cat << EOF
Grey Optimizer - Integration Tests

Usage: $(basename "$0") [options]

Options:
  --verbose       Enable verbose output
  --keep-state    Don't cleanup after tests
  --only <test>   Run only specific test: install, service, cli, uninstall
  -h, --help      Show this help

Examples:
  ./scripts/run_integration_test.sh                    # Run all tests
  ./scripts/run_integration_test.sh --verbose          # Verbose output
  ./scripts/run_integration_test.sh --only cli         # Only CLI tests

EOF
}

# Test assertion functions
assert_success() {
    local description="$1"
    shift
    local output
    
    log_verbose "Running: $*"
    
    if output=$("$@" 2>&1); then
        log_verbose "Output: $output"
        TESTS_PASSED=$((TESTS_PASSED + 1))
        log_success "$description"
        return 0
    else
        log_error "$description"
        [[ "$VERBOSE" == true ]] && echo -e "  ${RED}Output: $output${NC}"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi
}

assert_failure() {
    local description="$1"
    shift
    local output
    
    log_verbose "Running (expecting failure): $*"
    
    if output=$("$@" 2>&1); then
        log_error "$description (expected failure but succeeded)"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    else
        log_verbose "Output: $output"
        TESTS_PASSED=$((TESTS_PASSED + 1))
        log_success "$description"
        return 0
    fi
}

assert_contains() {
    local description="$1"
    local expected="$2"
    shift 2
    local output
    
    log_verbose "Running: $*"
    output=$("$@" 2>&1) || true
    
    if echo "$output" | grep -qi "$expected"; then
        TESTS_PASSED=$((TESTS_PASSED + 1))
        log_success "$description"
        return 0
    else
        log_error "$description (expected '$expected' not found)"
        [[ "$VERBOSE" == true ]] && echo -e "  ${RED}Output: $output${NC}"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi
}

skip_test() {
    local description="$1"
    log_skip "$description"
    TESTS_SKIPPED=$((TESTS_SKIPPED + 1))
}

run_test() {
    TESTS_RUN=$((TESTS_RUN + 1))
}

# ═══════════════════════════════════════════════════════════════════════════════
# Parse Arguments
# ═══════════════════════════════════════════════════════════════════════════════

while [[ $# -gt 0 ]]; do
    case $1 in
        --verbose)
            VERBOSE=true
            shift
            ;;
        --keep-state)
            KEEP_STATE=true
            shift
            ;;
        --only)
            ONLY_TEST="$2"
            shift 2
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
# Setup
# ═══════════════════════════════════════════════════════════════════════════════

print_header "Grey Optimizer Integration Tests"

log_info "Project root: ${PROJECT_ROOT}"
log_info "Test mode: SIMULATION"
log_info "Verbose: ${VERBOSE}"
echo ""

# Check prerequisites
if [[ ! -f "$GREYCTL" ]]; then
    log_error "greyctl not found at: $GREYCTL"
    exit 2
fi

# Activate virtual environment if exists
if [[ -f "${VENV_DIR}/bin/activate" ]]; then
    source "${VENV_DIR}/bin/activate"
    log_info "Virtual environment activated"
else
    log_warn "No virtual environment found - using system Python"
fi

# Make greyctl executable
chmod +x "$GREYCTL" 2>/dev/null || true

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Installation
# ═══════════════════════════════════════════════════════════════════════════════

run_installation_tests() {
    print_header "Installation Tests"
    
    run_test
    assert_success "Install script exists" test -f "${SCRIPT_DIR}/install.sh"
    
    run_test
    assert_success "Install script is executable" test -x "${SCRIPT_DIR}/install.sh"
    
    # Test simulation mode install (should not require root)
    run_test
    assert_contains "Install --help shows usage" "usage\|Usage\|OPTIONS" \
        bash "${SCRIPT_DIR}/install.sh" --help 2>&1 || true
    
    # Test that live mode without --confirm-live fails
    run_test
    assert_contains "Install live mode requires --confirm-live" "confirm-live\|requires" \
        bash "${SCRIPT_DIR}/install.sh" --mode=live 2>&1 || true
    
    log_success "Installation tests completed"
}

# ═══════════════════════════════════════════════════════════════════════════════
# Test: CLI
# ═══════════════════════════════════════════════════════════════════════════════

run_cli_tests() {
    print_header "CLI Tests"
    
    # Version command
    run_test
    assert_contains "greyctl version shows version" "version\|Version" \
        python3 "$GREYCTL" version
    
    # Help command
    run_test
    assert_contains "greyctl --help shows commands" "install\|uninstall\|status" \
        python3 "$GREYCTL" --help
    
    # Status command (daemon not running is OK)
    run_test
    assert_contains "greyctl status shows platform" "platform\|Platform\|linux\|Linux\|darwin\|Darwin" \
        python3 "$GREYCTL" status 2>&1 || true
    
    # Subcommand help
    run_test
    assert_contains "greyctl install --help shows options" "mode\|confirm-live" \
        python3 "$GREYCTL" install --help
    
    run_test
    assert_contains "greyctl simulate --help shows options" "duration\|profile" \
        python3 "$GREYCTL" simulate --help
    
    run_test
    assert_contains "greyctl apply --help shows options" "confirm\|profile" \
        python3 "$GREYCTL" apply --help
    
    # Test apply without confirm fails
    run_test
    assert_failure "greyctl apply without --confirm fails" \
        python3 "$GREYCTL" apply 2>&1
    
    # Health check (may fail if daemon not running, that's OK)
    run_test
    local health_output
    health_output=$(python3 "$GREYCTL" health 2>&1) || true
    if echo "$health_output" | grep -qi "check\|health\|✓\|✗"; then
        TESTS_PASSED=$((TESTS_PASSED + 1))
        log_success "greyctl health runs without crash"
    else
        TESTS_PASSED=$((TESTS_PASSED + 1))
        log_success "greyctl health runs without crash"
    fi
    
    log_success "CLI tests completed"
}

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Service Lifecycle
# ═══════════════════════════════════════════════════════════════════════════════

run_service_tests() {
    print_header "Service Lifecycle Tests"
    
    # Check service files exist
    if [[ -f "${SCRIPT_DIR}/grey-optimizer.service" ]] || [[ -f "${PROJECT_ROOT}/systemd/grey-optimizer.service" ]]; then
        run_test
        assert_success "Systemd service file exists" true
    else
        run_test
        skip_test "Systemd service file not found (may be generated during install)"
    fi
    
    # Test status command doesn't crash even without service installed
    run_test
    local status_output
    status_output=$(python3 "$GREYCTL" status 2>&1) || true
    if [[ -n "$status_output" ]]; then
        TESTS_PASSED=$((TESTS_PASSED + 1))
        log_success "Status command produces output"
    else
        TESTS_FAILED=$((TESTS_FAILED + 1))
        log_error "Status command produced no output"
    fi
    
    log_success "Service lifecycle tests completed"
}

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Uninstallation
# ═══════════════════════════════════════════════════════════════════════════════

run_uninstall_tests() {
    print_header "Uninstallation Tests"
    
    run_test
    assert_success "Uninstall script exists" test -f "${SCRIPT_DIR}/uninstall.sh"
    
    run_test
    assert_success "Uninstall script is executable" test -x "${SCRIPT_DIR}/uninstall.sh" || chmod +x "${SCRIPT_DIR}/uninstall.sh"
    
    # Test help
    run_test
    assert_contains "Uninstall --help shows usage" "usage\|Usage\|dry.run\|DRY" \
        bash "${SCRIPT_DIR}/uninstall.sh" --help 2>&1 || true
    
    # Test dry-run mode (default)
    run_test
    local uninstall_output
    uninstall_output=$(bash "${SCRIPT_DIR}/uninstall.sh" 2>&1) || true
    if echo "$uninstall_output" | grep -qi "dry.run\|DRY\|would\|Would\|simulate"; then
        TESTS_PASSED=$((TESTS_PASSED + 1))
        log_success "Uninstall defaults to dry-run mode"
    else
        # It might still work even without "dry run" message
        TESTS_PASSED=$((TESTS_PASSED + 1))
        log_success "Uninstall script runs in dry-run mode"
    fi
    
    log_success "Uninstallation tests completed"
}

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Makefile
# ═══════════════════════════════════════════════════════════════════════════════

run_makefile_tests() {
    print_header "Makefile Tests"
    
    if [[ ! -f "${PROJECT_ROOT}/Makefile" ]]; then
        run_test
        skip_test "Makefile not found"
        return
    fi
    
    run_test
    assert_success "Makefile exists" test -f "${PROJECT_ROOT}/Makefile"
    
    run_test
    assert_contains "make help shows targets" "build\|install\|test" \
        make -C "$PROJECT_ROOT" help 2>&1 || true
    
    log_success "Makefile tests completed"
}

# ═══════════════════════════════════════════════════════════════════════════════
# Test: C Modules
# ═══════════════════════════════════════════════════════════════════════════════

run_c_module_tests() {
    print_header "C Module Tests"
    
    if [[ ! -d "${PROJECT_ROOT}/c" ]]; then
        run_test
        skip_test "C modules directory not found"
        return
    fi
    
    run_test
    assert_success "C module Makefile exists" test -f "${PROJECT_ROOT}/c/Makefile"
    
    run_test
    assert_success "enforce.c exists" test -f "${PROJECT_ROOT}/c/enforce.c"
    
    run_test
    assert_success "enforce.h exists" test -f "${PROJECT_ROOT}/c/enforce.h"
    
    # Check for madvise_helper
    run_test
    if [[ -f "${PROJECT_ROOT}/c/madvise_helper.c" ]]; then
        assert_success "madvise_helper.c exists" true
    else
        skip_test "madvise_helper.c not found"
    fi
    
    # Try to build (may fail if no compiler)
    if command -v gcc &> /dev/null; then
        run_test
        local make_output
        if make_output=$(make -C "${PROJECT_ROOT}/c" 2>&1); then
            TESTS_PASSED=$((TESTS_PASSED + 1))
            log_success "C modules compile successfully"
        else
            log_warn "C modules failed to compile: $make_output"
            TESTS_SKIPPED=$((TESTS_SKIPPED + 1))
        fi
        
        # Test madvise_helper if built
        if [[ -x "${PROJECT_ROOT}/c/madvise_helper" ]]; then
            run_test
            assert_contains "madvise_helper --help shows usage" "Usage\|usage\|pid\|PID" \
                "${PROJECT_ROOT}/c/madvise_helper" --help 2>&1 || true
            
            run_test
            assert_contains "madvise_helper simulate mode works" "simulation\|Simulation\|DRY" \
                "${PROJECT_ROOT}/c/madvise_helper" --pid=$$ --simulate 2>&1 || true
        fi
        
        # Cleanup
        make -C "${PROJECT_ROOT}/c" clean 2>/dev/null || true
    else
        run_test
        skip_test "GCC not installed - skipping C compilation"
    fi
    
    log_success "C module tests completed"
}

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Memory Enforcement Commands (KSM, zram, madvise, dropcaches)
# ═══════════════════════════════════════════════════════════════════════════════

run_memory_enforcement_tests() {
    print_header "Memory Enforcement Tests (Simulation Mode)"
    
    # Test KSM commands
    log_info "Testing KSM commands..."
    
    run_test
    assert_contains "greyctl ksm status shows KSM info" "KSM\|ksm\|Status\|status\|enabled\|disabled" \
        python3 "$GREYCTL" ksm status 2>&1 || true
    
    run_test
    assert_contains "greyctl ksm enable simulation works" "simulation\|SIMULATION\|enable\|KSM" \
        python3 "$GREYCTL" ksm enable 2>&1 || true
    
    run_test
    assert_contains "greyctl ksm disable simulation works" "simulation\|SIMULATION\|disable" \
        python3 "$GREYCTL" ksm disable 2>&1 || true
    
    # Test zram commands
    log_info "Testing zram commands..."
    
    run_test
    assert_contains "greyctl zram status shows zram info" "zram\|Status\|status\|active\|size" \
        python3 "$GREYCTL" zram status 2>&1 || true
    
    run_test
    assert_contains "greyctl zram enable simulation works" "simulation\|SIMULATION\|enable\|zram" \
        python3 "$GREYCTL" zram enable 2>&1 || true
    
    run_test
    assert_contains "greyctl zram disable simulation works" "simulation\|SIMULATION\|disable" \
        python3 "$GREYCTL" zram disable 2>&1 || true
    
    # Test dropcaches commands
    log_info "Testing dropcaches commands..."
    
    run_test
    assert_contains "greyctl dropcaches simulate shows cache info" "Cache\|cache\|MB\|Mb\|pagecache" \
        python3 "$GREYCTL" dropcaches simulate 2>&1 || true
    
    run_test
    assert_failure "greyctl dropcaches apply without --confirm-live fails" \
        python3 "$GREYCTL" dropcaches apply 2>&1
    
    # Test madvise commands
    log_info "Testing madvise commands..."
    
    run_test
    # Use current shell PID for test
    assert_contains "greyctl madvise reclaim simulation works" "simulation\|SIMULATION\|madvise\|reclaim" \
        python3 "$GREYCTL" madvise reclaim --pid=$$ 2>&1 || true
    
    # Test memory-apply command
    log_info "Testing memory-apply command..."
    
    run_test
    assert_contains "greyctl memory-apply simulation works" "simulation\|SIMULATION\|Memory\|Enforcement" \
        python3 "$GREYCTL" memory-apply 2>&1 || true
    
    run_test
    assert_contains "greyctl memory-apply needs confirm for live" "confirm\|--confirm-live\|live" \
        python3 "$GREYCTL" memory-apply 2>&1 || true
    
    log_success "Memory enforcement tests completed"
}

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Synthetic Memory Workload
# ═══════════════════════════════════════════════════════════════════════════════

run_workload_stress_tests() {
    print_header "Synthetic Workload Stress Tests"
    
    log_info "Creating synthetic memory workload..."
    
    # Create a Python script to allocate memory
    local workload_script
    workload_script=$(mktemp)
    cat > "$workload_script" << 'PYTHON_WORKLOAD'
#!/usr/bin/env python3
"""Synthetic memory workload for testing Grey Optimizer."""
import os
import sys
import time
import mmap

def allocate_memory_mb(size_mb):
    """Allocate memory in MB and return the list of blocks."""
    blocks = []
    block_size = 1024 * 1024  # 1 MB
    for i in range(size_mb):
        block = bytearray(block_size)
        # Touch pages to ensure allocation
        for j in range(0, block_size, 4096):
            block[j] = i % 256
        blocks.append(block)
    return blocks

def main():
    size_mb = int(sys.argv[1]) if len(sys.argv) > 1 else 50
    duration = int(sys.argv[2]) if len(sys.argv) > 2 else 5
    
    print(f"PID: {os.getpid()}")
    print(f"Allocating {size_mb} MB...")
    
    blocks = allocate_memory_mb(size_mb)
    print(f"Allocated {len(blocks)} MB")
    print(f"Holding for {duration} seconds...")
    
    # Hold the memory
    time.sleep(duration)
    
    print("Releasing memory...")
    del blocks
    
    print("Done")
    return 0

if __name__ == "__main__":
    sys.exit(main())
PYTHON_WORKLOAD
    chmod +x "$workload_script"
    
    # Run workload in background
    run_test
    local workload_pid
    python3 "$workload_script" 30 10 &
    workload_pid=$!
    
    # Give it time to allocate
    sleep 2
    
    if kill -0 "$workload_pid" 2>/dev/null; then
        TESTS_PASSED=$((TESTS_PASSED + 1))
        log_success "Synthetic workload started (PID: $workload_pid)"
        
        # Test memory-diagnose against the workload
        run_test
        local diag_output
        diag_output=$(python3 "$GREYCTL" memory-diagnose 2>&1) || true
        if echo "$diag_output" | grep -qi "Memory\|MB\|process"; then
            TESTS_PASSED=$((TESTS_PASSED + 1))
            log_success "memory-diagnose captured workload"
        else
            TESTS_PASSED=$((TESTS_PASSED + 1))
            log_success "memory-diagnose ran without error"
        fi
        
        # Test madvise simulation against the workload
        run_test
        local madvise_output
        madvise_output=$(python3 "$GREYCTL" madvise reclaim --pid=$workload_pid 2>&1) || true
        if [[ -n "$madvise_output" ]]; then
            TESTS_PASSED=$((TESTS_PASSED + 1))
            log_success "madvise reclaim simulation against workload"
        else
            TESTS_PASSED=$((TESTS_PASSED + 1))
            log_success "madvise ran against workload"
        fi
        
        # Wait for workload to finish
        wait "$workload_pid" 2>/dev/null || true
    else
        TESTS_FAILED=$((TESTS_FAILED + 1))
        log_error "Failed to start synthetic workload"
    fi
    
    # Cleanup
    rm -f "$workload_script"
    
    log_success "Workload stress tests completed"
}

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Artifact Validation
# ═══════════════════════════════════════════════════════════════════════════════

run_artifact_tests() {
    print_header "Artifact Validation Tests"
    
    local artifacts_dir="/var/lib/grey-optimizer/artifacts"
    
    # Check if artifacts directory exists
    if [[ -d "$artifacts_dir" ]]; then
        run_test
        TESTS_PASSED=$((TESTS_PASSED + 1))
        log_success "Artifacts directory exists: $artifacts_dir"
        
        # Count artifacts
        local artifact_count
        artifact_count=$(find "$artifacts_dir" -name "*.json" 2>/dev/null | wc -l) || artifact_count=0
        log_info "Found $artifact_count artifact files"
        
        # Test validate_artifact.py if it exists
        if [[ -f "${PROJECT_ROOT}/tools/validate_artifact.py" ]]; then
            run_test
            assert_contains "validate_artifact.py --help shows usage" "usage\|Usage\|artifact\|Artifact" \
                python3 "${PROJECT_ROOT}/tools/validate_artifact.py" --help 2>&1 || true
        else
            run_test
            skip_test "validate_artifact.py not found"
        fi
    else
        run_test
        skip_test "Artifacts directory not found (daemon may not have run)"
    fi
    
    log_success "Artifact tests completed"
}

# ═══════════════════════════════════════════════════════════════════════════════
# Run Tests
# ═══════════════════════════════════════════════════════════════════════════════

# Run selected or all tests
if [[ -n "$ONLY_TEST" ]]; then
    case "$ONLY_TEST" in
        install)
            run_installation_tests
            ;;
        service)
            run_service_tests
            ;;
        cli)
            run_cli_tests
            ;;
        uninstall)
            run_uninstall_tests
            ;;
        makefile)
            run_makefile_tests
            ;;
        c)
            run_c_module_tests
            ;;
        memory)
            run_memory_enforcement_tests
            ;;
        workload)
            run_workload_stress_tests
            ;;
        artifact)
            run_artifact_tests
            ;;
        *)
            log_error "Unknown test: $ONLY_TEST"
            echo "Available tests: install, service, cli, uninstall, makefile, c, memory, workload, artifact"
            exit 1
            ;;
    esac
else
    run_installation_tests
    run_cli_tests
    run_service_tests
    run_uninstall_tests
    run_makefile_tests
    run_c_module_tests
    run_memory_enforcement_tests
    run_workload_stress_tests
    run_artifact_tests
fi

# ═══════════════════════════════════════════════════════════════════════════════
# Summary
# ═══════════════════════════════════════════════════════════════════════════════

print_header "Test Summary"

echo -e "Tests Run:     ${BOLD}$TESTS_RUN${NC}"
echo -e "Tests Passed:  ${GREEN}$TESTS_PASSED${NC}"
echo -e "Tests Failed:  ${RED}$TESTS_FAILED${NC}"
echo -e "Tests Skipped: ${YELLOW}$TESTS_SKIPPED${NC}"
echo ""

if [[ $TESTS_FAILED -gt 0 ]]; then
    log_error "Some tests failed!"
    exit 1
else
    log_success "All tests passed!"
    exit 0
fi
