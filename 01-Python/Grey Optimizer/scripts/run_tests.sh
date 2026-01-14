#!/bin/bash
# Grey Optimizer - Run Tests Script
# Runs the full test suite

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

echo -e "${BLUE}╔══════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║         Grey Optimizer - Test Suite                          ║${NC}"
echo -e "${BLUE}╚══════════════════════════════════════════════════════════════╝${NC}"
echo ""

# Check venv
if [[ ! -d "$PROJECT_DIR/backend/venv" ]]; then
    echo -e "${YELLOW}Virtual environment not found. Running setup...${NC}"
    "$SCRIPT_DIR/setup.sh"
fi

# Run backend tests
run_backend_tests() {
    echo -e "${BLUE}[1/3] Running backend unit tests...${NC}"
    
    cd "$PROJECT_DIR/backend"
    source venv/bin/activate
    
    pytest tests/ -v --tb=short --cov=grey_optimizer --cov-report=term-missing || {
        echo -e "${RED}Backend tests failed${NC}"
        return 1
    }
    
    deactivate
    echo -e "${GREEN}✓ Backend tests passed${NC}"
}

# Run verification tests
run_verification_tests() {
    echo -e "${BLUE}[2/3] Running verification tests...${NC}"
    
    cd "$PROJECT_DIR/tests"
    
    if [[ -f "test_verification.py" ]]; then
        source "$PROJECT_DIR/backend/venv/bin/activate"
        pytest test_verification.py -v --tb=short || {
            echo -e "${RED}Verification tests failed${NC}"
            return 1
        }
        deactivate
    else
        echo -e "${YELLOW}Verification tests not found, skipping${NC}"
    fi
    
    echo -e "${GREEN}✓ Verification tests passed${NC}"
}

# Run integration tests
run_integration_tests() {
    echo -e "${BLUE}[3/3] Running integration tests...${NC}"
    
    cd "$PROJECT_DIR/tests"
    
    if [[ -f "test_integration.py" ]]; then
        source "$PROJECT_DIR/backend/venv/bin/activate"
        pytest test_integration.py -v --tb=short || {
            echo -e "${RED}Integration tests failed${NC}"
            return 1
        }
        deactivate
    else
        echo -e "${YELLOW}Integration tests not found, skipping${NC}"
    fi
    
    echo -e "${GREEN}✓ Integration tests passed${NC}"
}

# Main
main() {
    local failed=0
    
    run_backend_tests || ((failed++))
    run_verification_tests || ((failed++))
    run_integration_tests || ((failed++))
    
    echo ""
    if [[ $failed -eq 0 ]]; then
        echo -e "${GREEN}╔══════════════════════════════════════════════════════════════╗${NC}"
        echo -e "${GREEN}║         All tests passed!                                    ║${NC}"
        echo -e "${GREEN}╚══════════════════════════════════════════════════════════════╝${NC}"
    else
        echo -e "${RED}╔══════════════════════════════════════════════════════════════╗${NC}"
        echo -e "${RED}║         $failed test suite(s) failed                              ║${NC}"
        echo -e "${RED}╚══════════════════════════════════════════════════════════════╝${NC}"
        exit 1
    fi
}

main "$@"
