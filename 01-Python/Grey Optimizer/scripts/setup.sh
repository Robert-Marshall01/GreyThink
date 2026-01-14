#!/bin/bash
# Grey Optimizer - Developer Setup Script
# Prepares the development environment for local testing

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
echo -e "${BLUE}║         Grey Optimizer - Developer Setup                     ║${NC}"
echo -e "${BLUE}╚══════════════════════════════════════════════════════════════╝${NC}"
echo ""

# Check Python
if ! command -v python3 &> /dev/null; then
    echo -e "${RED}Error: Python 3 not found${NC}"
    exit 1
fi

# Setup backend
setup_backend() {
    echo -e "${BLUE}[1/4] Setting up backend...${NC}"
    
    cd "$PROJECT_DIR/backend"
    
    if [[ ! -d "venv" ]]; then
        python3 -m venv venv
    fi
    
    source venv/bin/activate
    pip install --upgrade pip
    pip install -e ".[dev]"
    deactivate
    
    echo -e "${GREEN}✓ Backend ready${NC}"
}

# Compile C modules
compile_modules() {
    echo -e "${BLUE}[2/4] Compiling C modules...${NC}"
    
    cd "$PROJECT_DIR/enforcement"
    
    if [[ -f "Makefile" ]]; then
        make clean || true
        make
        echo -e "${GREEN}✓ C modules compiled${NC}"
    else
        echo -e "${YELLOW}⚠ No Makefile found, skipping C modules${NC}"
    fi
}

# Setup frontend
setup_frontend() {
    echo -e "${BLUE}[3/4] Setting up frontend...${NC}"
    
    cd "$PROJECT_DIR/frontend"
    
    if command -v npm &> /dev/null; then
        npm install
        echo -e "${GREEN}✓ Frontend ready${NC}"
    else
        echo -e "${YELLOW}⚠ npm not found, skipping frontend${NC}"
    fi
}

# Create local config
create_dev_config() {
    echo -e "${BLUE}[4/4] Creating development config...${NC}"
    
    local config_file="$PROJECT_DIR/config.dev.yaml"
    
    if [[ ! -f "$config_file" ]]; then
        cat > "$config_file" << 'EOF'
# Grey Optimizer - Development Configuration

telemetry:
  interval_seconds: 1.0
  history_size: 300

cpu_enforcement:
  enabled: true
  target_percent: 10
  min_percent: 1

ram_enforcement:
  enabled: true
  target_mb: 64
  min_mb: 64

disk_enforcement:
  enabled: true
  target_iops: 100

safety:
  simulation_mode: true  # Always use simulation for development
  protected_patterns:
    - "systemd*"
    - "kernel*"
    - "code"
    - "python"
    - "node"

api:
  host: "127.0.0.1"
  port: 8080
  enable_websocket: true

persistence:
  database_path: "./data/audit.db"
  proof_dir: "./data/proofs"
  retention_days: 7
EOF
        mkdir -p "$PROJECT_DIR/data"
        echo -e "${GREEN}✓ Development config created${NC}"
    else
        echo -e "${YELLOW}Config already exists${NC}"
    fi
}

# Main
main() {
    setup_backend
    compile_modules
    setup_frontend
    create_dev_config
    
    echo ""
    echo -e "${GREEN}╔══════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║         Development environment ready!                       ║${NC}"
    echo -e "${GREEN}╚══════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    echo "Quick start commands:"
    echo ""
    echo "  Backend (daemon):"
    echo "    cd backend && source venv/bin/activate"
    echo "    GREY_OPTIMIZER_CONFIG=../config.dev.yaml python -m grey_optimizer.daemon"
    echo ""
    echo "  Frontend (dev server):"
    echo "    cd frontend && npm run dev"
    echo ""
    echo "  Run tests:"
    echo "    cd backend && source venv/bin/activate && pytest"
    echo ""
}

main "$@"
