#!/bin/bash
# ═══════════════════════════════════════════════════════════════════════════════
# Grey Optimizer - Startup Test Script
# ═══════════════════════════════════════════════════════════════════════════════
#
# This script tests the daemon startup process on the current OS.
#
# Usage:
#   ./run_startup_test.sh                    # Run all tests
#   ./run_startup_test.sh --quick            # Quick health check only
#   ./run_startup_test.sh --install          # Test full install flow
#
# Exit codes:
#   0 - All tests passed
#   1 - Test failures
#   2 - Prerequisites missing
#
# ═══════════════════════════════════════════════════════════════════════════════

set -euo pipefail

# ─────────────────────────────────────────────────────────────────────────────
# Configuration
# ─────────────────────────────────────────────────────────────────────────────

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
VERSION="2.0.0"

HEALTH_ENDPOINT="http://127.0.0.1:8090/health"
HEALTH_TIMEOUT=30

# Test modes
QUICK_TEST=false
INSTALL_TEST=false

# Counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

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

# ─────────────────────────────────────────────────────────────────────────────
# Output Functions
# ─────────────────────────────────────────────────────────────────────────────

log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_pass() { echo -e "${GREEN}[PASS]${NC} $1"; }
log_fail() { echo -e "${RED}[FAIL]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_test() { echo -e "${CYAN}[TEST]${NC} $1"; }

test_pass() {
    log_pass "$1"
    ((TESTS_PASSED++)) || true
    ((TESTS_RUN++)) || true
}

test_fail() {
    log_fail "$1"
    ((TESTS_FAILED++)) || true
    ((TESTS_RUN++)) || true
}

# ─────────────────────────────────────────────────────────────────────────────
# OS Detection
# ─────────────────────────────────────────────────────────────────────────────

detect_os() {
    case "$(uname -s)" in
        Linux*)  echo "linux" ;;
        Darwin*) echo "macos" ;;
        CYGWIN*|MINGW*|MSYS*) echo "windows" ;;
        *)       echo "unknown" ;;
    esac
}

OS_TYPE=$(detect_os)

# ─────────────────────────────────────────────────────────────────────────────
# Test Functions
# ─────────────────────────────────────────────────────────────────────────────

test_python_version() {
    log_test "Checking Python version..."
    
    if ! command -v python3 &>/dev/null; then
        test_fail "Python 3 not found"
        return 1
    fi
    
    local version
    version=$(python3 -c 'import sys; print(f"{sys.version_info.major}.{sys.version_info.minor}")')
    
    if python3 -c "import sys; exit(0 if sys.version_info >= (3, 8) else 1)" 2>/dev/null; then
        test_pass "Python $version (>= 3.8 required)"
        return 0
    else
        test_fail "Python $version found, but 3.8+ required"
        return 1
    fi
}

test_dependencies() {
    log_test "Checking Python dependencies..."
    
    local missing=()
    local python_cmd="python3"
    
    # Use venv python if available
    if [[ -f "$PROJECT_ROOT/.venv/bin/python" ]]; then
        python_cmd="$PROJECT_ROOT/.venv/bin/python"
        log_info "  Using venv: $python_cmd"
    fi
    
    for module in aiosqlite aiohttp asyncio; do
        if ! "$python_cmd" -c "import $module" 2>/dev/null; then
            missing+=("$module")
        fi
    done
    
    if [[ ${#missing[@]} -eq 0 ]]; then
        test_pass "All Python dependencies available"
        return 0
    else
        test_fail "Missing Python modules: ${missing[*]}"
        return 1
    fi
}

test_daemon_import() {
    log_test "Testing daemon module import..."
    
    local daemon_path="$PROJECT_ROOT/daemon/grey_daemon.py"
    
    if [[ ! -f "$daemon_path" ]]; then
        test_fail "Daemon file not found: $daemon_path"
        return 1
    fi
    
    if python3 -c "import sys; sys.path.insert(0, '$PROJECT_ROOT/daemon'); import grey_daemon" 2>/dev/null; then
        test_pass "Daemon module imports successfully"
        return 0
    else
        test_fail "Daemon module import failed"
        return 1
    fi
}

test_daemon_syntax() {
    log_test "Testing daemon syntax..."
    
    local daemon_path="$PROJECT_ROOT/daemon/grey_daemon.py"
    
    # Use compile() instead of py_compile to avoid pycache permission issues
    if python3 -c "compile(open('$daemon_path').read(), '$daemon_path', 'exec')" 2>/dev/null; then
        test_pass "Daemon syntax is valid"
        return 0
    else
        test_fail "Daemon has syntax errors"
        return 1
    fi
}

test_health_endpoint() {
    log_test "Testing health endpoint ($HEALTH_ENDPOINT)..."
    
    if ! command -v curl &>/dev/null; then
        log_warn "curl not available, skipping health check"
        return 0
    fi
    
    local response
    if response=$(curl -sf "$HEALTH_ENDPOINT" 2>/dev/null); then
        local status
        status=$(echo "$response" | python3 -c "import sys,json; print(json.load(sys.stdin).get('status',''))" 2>/dev/null || true)
        
        if [[ "$status" == "healthy" ]]; then
            test_pass "Health endpoint returns healthy status"
            
            # Extract and display additional info
            local version mode
            version=$(echo "$response" | python3 -c "import sys,json; print(json.load(sys.stdin).get('version','?'))" 2>/dev/null || true)
            mode=$(echo "$response" | python3 -c "import sys,json; print(json.load(sys.stdin).get('mode','?'))" 2>/dev/null || true)
            log_info "  Version: $version, Mode: $mode"
            return 0
        else
            test_fail "Health endpoint returned status: $status"
            return 1
        fi
    else
        test_fail "Health endpoint not responding"
        return 1
    fi
}

test_service_status_linux() {
    log_test "Checking systemd service status..."
    
    if ! command -v systemctl &>/dev/null; then
        log_warn "systemctl not available"
        return 0
    fi
    
    if systemctl is-active --quiet grey-optimizer 2>/dev/null; then
        test_pass "grey-optimizer service is running"
        return 0
    elif systemctl is-enabled --quiet grey-optimizer 2>/dev/null; then
        test_fail "Service is enabled but not running"
        return 1
    else
        log_warn "grey-optimizer service not installed"
        return 0
    fi
}

test_service_status_macos() {
    log_test "Checking launchd service status..."
    
    local label="com.grey.optimizer"
    
    if launchctl list 2>/dev/null | grep -q "$label"; then
        test_pass "launchd service is loaded"
        return 0
    else
        log_warn "launchd service not loaded"
        return 0
    fi
}

test_service_status() {
    case "$OS_TYPE" in
        linux)  test_service_status_linux ;;
        macos)  test_service_status_macos ;;
        windows) log_warn "Windows service check not supported in bash" ;;
        *)      log_warn "Unknown OS, skipping service check" ;;
    esac
}

test_log_files() {
    log_test "Checking log files..."
    
    local log_path
    case "$OS_TYPE" in
        linux)  log_path="/var/log/grey-optimizer.log" ;;
        macos)  log_path="/var/log/grey-optimizer/daemon.log" ;;
        *)      log_path="" ;;
    esac
    
    if [[ -z "$log_path" ]]; then
        log_warn "Log path unknown for OS: $OS_TYPE"
        return 0
    fi
    
    if [[ -f "$log_path" ]]; then
        test_pass "Log file exists: $log_path"
        
        # Check for recent activity
        local lines
        lines=$(wc -l < "$log_path")
        log_info "  Log has $lines lines"
        
        # Show last few lines
        log_info "  Recent log entries:"
        tail -3 "$log_path" 2>/dev/null | while read -r line; do
            log_info "    $line"
        done
        
        return 0
    else
        log_warn "Log file not found: $log_path"
        return 0
    fi
}

test_daemon_direct_start() {
    log_test "Testing daemon direct start..."
    
    local daemon_path="$PROJECT_ROOT/daemon/grey_daemon.py"
    local pid_file="/tmp/grey-test-daemon.pid"
    
    # Start daemon in background
    log_info "  Starting daemon in test mode..."
    python3 "$daemon_path" --mode=simulation &
    local daemon_pid=$!
    echo $daemon_pid > "$pid_file"
    
    # Wait for startup
    sleep 3
    
    # Check if still running
    if kill -0 "$daemon_pid" 2>/dev/null; then
        test_pass "Daemon started and running (PID: $daemon_pid)"
        
        # Test health endpoint
        sleep 2
        if curl -sf "$HEALTH_ENDPOINT" &>/dev/null; then
            test_pass "Health endpoint responding"
        else
            log_warn "Health endpoint not responding (may need more time)"
        fi
        
        # Cleanup
        log_info "  Stopping test daemon..."
        kill "$daemon_pid" 2>/dev/null || true
        rm -f "$pid_file"
        return 0
    else
        test_fail "Daemon exited unexpectedly"
        rm -f "$pid_file"
        return 1
    fi
}

test_install_script_exists() {
    log_test "Checking install scripts..."
    
    local found=0
    
    case "$OS_TYPE" in
        linux)
            if [[ -f "$SCRIPT_DIR/install_linux.sh" ]]; then
                test_pass "Linux installer found: install_linux.sh"
                ((found++)) || true
            fi
            ;;
        macos)
            if [[ -f "$SCRIPT_DIR/install_macos.sh" ]]; then
                test_pass "macOS installer found: install_macos.sh"
                ((found++)) || true
            fi
            ;;
        windows)
            if [[ -f "$SCRIPT_DIR/install_windows.ps1" ]]; then
                test_pass "Windows installer found: install_windows.ps1"
                ((found++)) || true
            fi
            ;;
    esac
    
    if [[ $found -eq 0 ]]; then
        test_fail "No installer found for OS: $OS_TYPE"
        return 1
    fi
    
    return 0
}

test_installer_simulation() {
    log_test "Testing installer in simulation mode..."
    
    local installer
    case "$OS_TYPE" in
        linux)  installer="$SCRIPT_DIR/install_linux.sh" ;;
        macos)  installer="$SCRIPT_DIR/install_macos.sh" ;;
        *)      
            log_warn "Installer simulation not available for: $OS_TYPE"
            return 0
            ;;
    esac
    
    if [[ ! -x "$installer" ]]; then
        chmod +x "$installer" 2>/dev/null || true
    fi
    
    # Run in simulation mode
    if "$installer" --simulation 2>&1 | head -20 | grep -qi "simulation\|would"; then
        test_pass "Installer runs in simulation mode"
        return 0
    else
        test_fail "Installer simulation mode failed"
        return 1
    fi
}

# ─────────────────────────────────────────────────────────────────────────────
# Test Suites
# ─────────────────────────────────────────────────────────────────────────────

run_quick_tests() {
    echo ""
    echo -e "${BOLD}Running Quick Tests${NC}"
    echo "═══════════════════════════════════════════════════════════════════"
    
    test_health_endpoint
    test_service_status
}

run_standard_tests() {
    echo ""
    echo -e "${BOLD}Running Standard Tests${NC}"
    echo "═══════════════════════════════════════════════════════════════════"
    
    test_python_version
    test_dependencies
    test_daemon_syntax
    test_daemon_import
    test_install_script_exists
    test_health_endpoint
    test_service_status
    test_log_files
}

run_install_tests() {
    echo ""
    echo -e "${BOLD}Running Installation Tests${NC}"
    echo "═══════════════════════════════════════════════════════════════════"
    
    test_python_version
    test_dependencies
    test_daemon_syntax
    test_daemon_import
    test_install_script_exists
    test_installer_simulation
    test_daemon_direct_start
    test_health_endpoint
}

# ─────────────────────────────────────────────────────────────────────────────
# Results
# ─────────────────────────────────────────────────────────────────────────────

show_results() {
    echo ""
    echo "═══════════════════════════════════════════════════════════════════"
    echo -e "${BOLD}Test Results${NC}"
    echo "═══════════════════════════════════════════════════════════════════"
    echo ""
    
    echo -e "  Tests Run:    ${BOLD}$TESTS_RUN${NC}"
    echo -e "  ${GREEN}Passed:       $TESTS_PASSED${NC}"
    
    if [[ $TESTS_FAILED -gt 0 ]]; then
        echo -e "  ${RED}Failed:       $TESTS_FAILED${NC}"
    else
        echo -e "  Failed:       $TESTS_FAILED"
    fi
    
    echo ""
    
    if [[ $TESTS_FAILED -eq 0 ]]; then
        echo -e "${GREEN}${BOLD}All tests passed!${NC}"
        return 0
    else
        echo -e "${RED}${BOLD}Some tests failed.${NC}"
        return 1
    fi
}

# ─────────────────────────────────────────────────────────────────────────────
# Help
# ─────────────────────────────────────────────────────────────────────────────

show_help() {
    cat <<EOF
Grey Optimizer Startup Test v${VERSION}

Usage: $0 [OPTIONS]

Options:
  --quick       Quick health check only
  --install     Full installation test (includes daemon start)
  --help, -h    Show this help

Examples:
  $0                    # Run standard tests
  $0 --quick            # Just check health endpoint
  $0 --install          # Full installation verification

Tests Include:
  - Python version check (3.8+ required)
  - Python dependencies (aiosqlite, aiohttp)
  - Daemon syntax validation
  - Daemon module import
  - Install script presence
  - Health endpoint check
  - Service status (OS-specific)
  - Log file check

EOF
}

# ─────────────────────────────────────────────────────────────────────────────
# Argument Parsing
# ─────────────────────────────────────────────────────────────────────────────

parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --quick)     QUICK_TEST=true; shift ;;
            --install)   INSTALL_TEST=true; shift ;;
            --help|-h)   show_help; exit 0 ;;
            *)           log_warn "Unknown option: $1"; shift ;;
        esac
    done
}

# ─────────────────────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────────────────────

main() {
    parse_args "$@"
    
    echo ""
    echo -e "${BOLD}═══════════════════════════════════════════════════════════════════${NC}"
    echo -e "${BOLD}  Grey Optimizer - Startup Test v${VERSION}${NC}"
    echo -e "${BOLD}═══════════════════════════════════════════════════════════════════${NC}"
    echo ""
    echo -e "  OS:           ${CYAN}$OS_TYPE${NC}"
    echo -e "  Project Root: ${CYAN}$PROJECT_ROOT${NC}"
    echo -e "  Test Mode:    ${CYAN}$(if $QUICK_TEST; then echo 'Quick'; elif $INSTALL_TEST; then echo 'Install'; else echo 'Standard'; fi)${NC}"
    
    if $QUICK_TEST; then
        run_quick_tests
    elif $INSTALL_TEST; then
        run_install_tests
    else
        run_standard_tests
    fi
    
    show_results
    exit_code=$?
    
    echo ""
    exit $exit_code
}

main "$@"
