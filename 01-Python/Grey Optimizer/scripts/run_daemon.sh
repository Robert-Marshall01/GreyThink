#!/bin/bash
# Grey Optimizer - Run Daemon Script
# Runs the optimizer daemon for development

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

# Create data directory
mkdir -p "$PROJECT_DIR/data/proofs"

# Check venv
if [[ ! -d "$PROJECT_DIR/backend/venv" ]]; then
    echo "Virtual environment not found. Running setup..."
    "$SCRIPT_DIR/setup.sh"
fi

cd "$PROJECT_DIR/backend"
source venv/bin/activate

export GREY_OPTIMIZER_CONFIG="${GREY_OPTIMIZER_CONFIG:-$PROJECT_DIR/config.dev.yaml}"
export GREY_OPTIMIZER_LOG_LEVEL="${GREY_OPTIMIZER_LOG_LEVEL:-INFO}"

echo "Starting Grey Optimizer daemon..."
echo "Config: $GREY_OPTIMIZER_CONFIG"
echo ""

python -m grey_optimizer.daemon "$@"
