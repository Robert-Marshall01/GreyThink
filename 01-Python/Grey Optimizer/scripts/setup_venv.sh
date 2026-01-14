#!/usr/bin/env bash
# ═══════════════════════════════════════════════════════════════════════════════
# Grey Optimizer - Python Virtual Environment Setup
# ═══════════════════════════════════════════════════════════════════════════════
#
# Creates and configures a Python virtual environment for Grey Optimizer.
#
# Usage:
#   ./scripts/setup_venv.sh [options]
#
# Options:
#   --dev           Install development dependencies
#   --force         Force recreation of venv (delete existing)
#   --python PATH   Specify Python interpreter path
#   --no-upgrade    Skip pip upgrade
#   -h, --help      Show this help
#
# ═══════════════════════════════════════════════════════════════════════════════

set -euo pipefail

# ═══════════════════════════════════════════════════════════════════════════════
# Configuration
# ═══════════════════════════════════════════════════════════════════════════════

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
VENV_DIR="${PROJECT_ROOT}/.venv"
REQUIREMENTS_FILE="${PROJECT_ROOT}/requirements.txt"
REQUIREMENTS_DEV_FILE="${PROJECT_ROOT}/requirements-dev.txt"

# Defaults
PYTHON_CMD="python3"
INSTALL_DEV=false
FORCE_RECREATE=false
UPGRADE_PIP=true

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# ═══════════════════════════════════════════════════════════════════════════════
# Utilities
# ═══════════════════════════════════════════════════════════════════════════════

print_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
print_success() { echo -e "${GREEN}[OK]${NC} $1"; }
print_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
print_error() { echo -e "${RED}[ERROR]${NC} $1" >&2; }

print_header() {
    echo ""
    echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}  $1${NC}"
    echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
    echo ""
}

show_help() {
    cat << EOF
Grey Optimizer - Python Virtual Environment Setup

Usage: $(basename "$0") [options]

Options:
  --dev           Install development dependencies (pytest, black, etc.)
  --force         Force recreation of venv (deletes existing)
  --python PATH   Specify Python interpreter path (default: python3)
  --no-upgrade    Skip pip upgrade
  -h, --help      Show this help

Examples:
  ./scripts/setup_venv.sh                  # Basic setup
  ./scripts/setup_venv.sh --dev            # With dev dependencies
  ./scripts/setup_venv.sh --force --dev    # Fresh install with dev deps
  ./scripts/setup_venv.sh --python python3.11

EOF
}

# ═══════════════════════════════════════════════════════════════════════════════
# Parse Arguments
# ═══════════════════════════════════════════════════════════════════════════════

while [[ $# -gt 0 ]]; do
    case $1 in
        --dev)
            INSTALL_DEV=true
            shift
            ;;
        --force)
            FORCE_RECREATE=true
            shift
            ;;
        --python)
            PYTHON_CMD="$2"
            shift 2
            ;;
        --no-upgrade)
            UPGRADE_PIP=false
            shift
            ;;
        -h|--help)
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

# ═══════════════════════════════════════════════════════════════════════════════
# Validation
# ═══════════════════════════════════════════════════════════════════════════════

print_header "Grey Optimizer - Virtual Environment Setup"

# Check Python availability
if ! command -v "$PYTHON_CMD" &> /dev/null; then
    print_error "Python not found: $PYTHON_CMD"
    print_info "Please install Python 3.8+ or specify path with --python"
    exit 1
fi

# Check Python version
PYTHON_VERSION=$("$PYTHON_CMD" -c 'import sys; print(f"{sys.version_info.major}.{sys.version_info.minor}")')
PYTHON_MAJOR=$("$PYTHON_CMD" -c 'import sys; print(sys.version_info.major)')
PYTHON_MINOR=$("$PYTHON_CMD" -c 'import sys; print(sys.version_info.minor)')

if [[ "$PYTHON_MAJOR" -lt 3 ]] || [[ "$PYTHON_MAJOR" -eq 3 && "$PYTHON_MINOR" -lt 8 ]]; then
    print_error "Python 3.8+ required. Found: Python $PYTHON_VERSION"
    exit 1
fi

print_success "Python $PYTHON_VERSION found at: $(which "$PYTHON_CMD")"

# Check venv module availability
if ! "$PYTHON_CMD" -c "import venv" &> /dev/null; then
    print_error "Python venv module not available"
    print_info "On Debian/Ubuntu: sudo apt install python3-venv"
    print_info "On Fedora: sudo dnf install python3-venv"
    exit 1
fi

# ═══════════════════════════════════════════════════════════════════════════════
# Create Virtual Environment
# ═══════════════════════════════════════════════════════════════════════════════

cd "$PROJECT_ROOT"

# Handle existing venv
if [[ -d "$VENV_DIR" ]]; then
    if [[ "$FORCE_RECREATE" == true ]]; then
        print_warn "Removing existing virtual environment..."
        rm -rf "$VENV_DIR"
    else
        print_info "Virtual environment exists. Use --force to recreate."
        
        # Check if it's valid
        if [[ -f "${VENV_DIR}/bin/python" ]]; then
            print_success "Existing venv is valid"
        else
            print_warn "Existing venv appears corrupted. Recreating..."
            rm -rf "$VENV_DIR"
        fi
    fi
fi

# Create venv if needed
if [[ ! -d "$VENV_DIR" ]]; then
    print_info "Creating virtual environment at ${VENV_DIR}..."
    "$PYTHON_CMD" -m venv "$VENV_DIR"
    print_success "Virtual environment created"
fi

# ═══════════════════════════════════════════════════════════════════════════════
# Activate and Install Dependencies
# ═══════════════════════════════════════════════════════════════════════════════

# Activate venv
source "${VENV_DIR}/bin/activate"
print_success "Virtual environment activated"

# Upgrade pip
if [[ "$UPGRADE_PIP" == true ]]; then
    print_info "Upgrading pip..."
    pip install --upgrade pip --quiet
    print_success "pip upgraded to $(pip --version | cut -d' ' -f2)"
fi

# Install core dependencies
if [[ -f "$REQUIREMENTS_FILE" ]]; then
    print_info "Installing dependencies from requirements.txt..."
    pip install -r "$REQUIREMENTS_FILE" --quiet
    print_success "Core dependencies installed"
else
    print_warn "No requirements.txt found"
    
    # Install minimal dependencies
    print_info "Installing minimal dependencies..."
    pip install \
        flask \
        flask-cors \
        requests \
        psutil \
        pyyaml \
        schedule \
        cryptography \
        --quiet
    print_success "Minimal dependencies installed"
    
    # Generate requirements.txt
    print_info "Generating requirements.txt..."
    pip freeze > "$REQUIREMENTS_FILE"
    print_success "requirements.txt generated"
fi

# Install development dependencies
if [[ "$INSTALL_DEV" == true ]]; then
    if [[ -f "$REQUIREMENTS_DEV_FILE" ]]; then
        print_info "Installing development dependencies..."
        pip install -r "$REQUIREMENTS_DEV_FILE" --quiet
    else
        print_info "Installing common development tools..."
        pip install \
            pytest \
            pytest-cov \
            pytest-asyncio \
            black \
            isort \
            flake8 \
            mypy \
            pdoc \
            watchdog \
            --quiet
        
        # Generate requirements-dev.txt
        print_info "Generating requirements-dev.txt..."
        cat > "$REQUIREMENTS_DEV_FILE" << 'EOF'
# Development dependencies for Grey Optimizer
pytest>=7.0.0
pytest-cov>=4.0.0
pytest-asyncio>=0.21.0
black>=23.0.0
isort>=5.12.0
flake8>=6.0.0
mypy>=1.0.0
pdoc>=14.0.0
watchdog>=3.0.0
EOF
    fi
    print_success "Development dependencies installed"
fi

# ═══════════════════════════════════════════════════════════════════════════════
# Install Package in Editable Mode
# ═══════════════════════════════════════════════════════════════════════════════

# If there's a setup.py or pyproject.toml, install the package
if [[ -f "${PROJECT_ROOT}/setup.py" ]] || [[ -f "${PROJECT_ROOT}/pyproject.toml" ]]; then
    print_info "Installing package in editable mode..."
    pip install -e "$PROJECT_ROOT" --quiet 2>/dev/null || true
    print_success "Package installed"
fi

# If there's a backend directory with its own setup
if [[ -d "${PROJECT_ROOT}/backend" ]] && [[ -f "${PROJECT_ROOT}/backend/setup.py" ]]; then
    print_info "Installing backend package..."
    pip install -e "${PROJECT_ROOT}/backend" --quiet 2>/dev/null || true
    print_success "Backend package installed"
fi

# ═══════════════════════════════════════════════════════════════════════════════
# Verification
# ═══════════════════════════════════════════════════════════════════════════════

print_header "Verification"

# Show installed packages summary
INSTALLED_COUNT=$(pip list 2>/dev/null | wc -l)
print_success "Installed ${INSTALLED_COUNT} packages"

# Test key imports
print_info "Testing key imports..."

IMPORT_ERRORS=0

for module in flask requests psutil yaml; do
    if python -c "import $module" 2>/dev/null; then
        echo -e "  ${GREEN}✓${NC} $module"
    else
        echo -e "  ${RED}✗${NC} $module"
        IMPORT_ERRORS=$((IMPORT_ERRORS + 1))
    fi
done

if [[ "$IMPORT_ERRORS" -gt 0 ]]; then
    print_warn "$IMPORT_ERRORS modules failed to import"
else
    print_success "All key modules available"
fi

# ═══════════════════════════════════════════════════════════════════════════════
# Create Activation Script
# ═══════════════════════════════════════════════════════════════════════════════

# Create a convenient activation script
ACTIVATE_SCRIPT="${PROJECT_ROOT}/activate.sh"
cat > "$ACTIVATE_SCRIPT" << 'EOF'
#!/usr/bin/env bash
# Convenience script to activate the Grey Optimizer virtual environment
# Usage: source ./activate.sh

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VENV_DIR="${SCRIPT_DIR}/.venv"

if [[ -d "$VENV_DIR" ]]; then
    source "${VENV_DIR}/bin/activate"
    echo "Grey Optimizer venv activated. Python: $(which python)"
    echo "To deactivate, run: deactivate"
else
    echo "Virtual environment not found. Run: ./scripts/setup_venv.sh"
fi
EOF
chmod +x "$ACTIVATE_SCRIPT"

# ═══════════════════════════════════════════════════════════════════════════════
# Summary
# ═══════════════════════════════════════════════════════════════════════════════

print_header "Setup Complete"

echo -e "Virtual environment: ${GREEN}${VENV_DIR}${NC}"
echo -e "Python:              ${GREEN}$(which python)${NC}"
echo -e "pip:                 ${GREEN}$(pip --version | cut -d' ' -f2)${NC}"
echo ""
echo -e "${BLUE}To activate the virtual environment:${NC}"
echo ""
echo -e "  source ${VENV_DIR}/bin/activate"
echo ""
echo -e "  ${BLUE}OR${NC}"
echo ""
echo -e "  source ./activate.sh"
echo ""
echo -e "${BLUE}Then run:${NC}"
echo ""
echo -e "  make build          # Build all components"
echo -e "  make install        # Install (simulation mode)"
echo -e "  make test           # Run tests"
echo ""

# Deactivate at end of script
# Note: The user needs to source activate.sh themselves for interactive use
