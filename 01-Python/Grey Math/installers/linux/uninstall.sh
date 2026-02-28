#!/usr/bin/env bash
# ============================================================================
# Grey Math Uninstaller for Linux
# Standalone uninstaller — can also be run from the installed copy
# ============================================================================

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo ""
echo "  ============================================"
echo "    Grey Math - Linux Uninstaller"
echo "  ============================================"
echo ""

INSTALL_DIR="${HOME}/.local/share/greymath"
BIN_DIR="${HOME}/.local/bin"
DESKTOP_DIR="${HOME}/.local/share/applications"

# --- Check if installed ---
if [ ! -d "$INSTALL_DIR" ]; then
    echo -e "${BLUE}[INFO]${NC}  Grey Math does not appear to be installed at:"
    echo "         ${INSTALL_DIR}"
    exit 0
fi

# --- Confirm ---
read -rp "Are you sure you want to uninstall Grey Math? (y/N): " CONFIRM
if [[ ! "$CONFIRM" =~ ^[Yy]$ ]]; then
    echo -e "${BLUE}[INFO]${NC}  Uninstallation cancelled."
    exit 0
fi

echo ""

# --- Remove launcher scripts ---
echo -e "${BLUE}[1/3]${NC} Removing launcher scripts ..."
rm -f "${BIN_DIR}/greymath"
rm -f "${BIN_DIR}/greymath-python"
echo -e "${GREEN}[OK]${NC}    Launchers removed."

# --- Remove desktop entry ---
echo -e "${BLUE}[2/3]${NC} Removing desktop entry ..."
rm -f "${DESKTOP_DIR}/greymath.desktop"
echo -e "${GREEN}[OK]${NC}    Desktop entry removed."

# --- Remove installation directory ---
echo -e "${BLUE}[3/3]${NC} Removing installation directory ..."
rm -rf "$INSTALL_DIR"
echo -e "${GREEN}[OK]${NC}    Installation directory removed."

echo ""
echo "  ============================================"
echo "    Grey Math has been uninstalled."
echo "  ============================================"
echo ""
