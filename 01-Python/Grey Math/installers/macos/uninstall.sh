#!/usr/bin/env bash
# ============================================================================
# Grey Math Uninstaller for macOS
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
echo "    Grey Math - macOS Uninstaller"
echo "  ============================================"
echo ""

INSTALL_DIR="${HOME}/Library/Application Support/GreyMath"
BIN_DIR="/usr/local/bin"
APP_BUNDLE="${HOME}/Applications/Grey Math.app"

# --- Check if installed ---
if [ ! -d "$INSTALL_DIR" ] && [ ! -d "$APP_BUNDLE" ]; then
    echo -e "${BLUE}[INFO]${NC}  Grey Math does not appear to be installed."
    exit 0
fi

# --- Confirm ---
read -rp "Are you sure you want to uninstall Grey Math? (y/N): " CONFIRM
if [[ ! "$CONFIRM" =~ ^[Yy]$ ]]; then
    echo -e "${BLUE}[INFO]${NC}  Uninstallation cancelled."
    exit 0
fi

echo ""

# --- Remove launcher symlinks ---
echo -e "${BLUE}[1/4]${NC} Removing command-line launchers ..."
if [ -w "$BIN_DIR" ]; then
    rm -f "${BIN_DIR}/greymath"
    rm -f "${BIN_DIR}/greymath-python"
else
    echo "  Removing symlinks requires admin access."
    sudo rm -f "${BIN_DIR}/greymath" 2>/dev/null || true
    sudo rm -f "${BIN_DIR}/greymath-python" 2>/dev/null || true
fi
echo -e "${GREEN}[OK]${NC}    Launchers removed."

# --- Remove app bundle ---
echo -e "${BLUE}[2/4]${NC} Removing application bundle ..."
if [ -d "$APP_BUNDLE" ]; then
    rm -rf "$APP_BUNDLE"
    echo -e "${GREEN}[OK]${NC}    Application bundle removed."
else
    echo -e "${BLUE}[INFO]${NC}  No application bundle found — skipping."
fi

# --- Remove installation directory ---
echo -e "${BLUE}[3/4]${NC} Removing installation directory ..."
if [ -d "$INSTALL_DIR" ]; then
    rm -rf "$INSTALL_DIR"
    echo -e "${GREEN}[OK]${NC}    Installation directory removed."
else
    echo -e "${BLUE}[INFO]${NC}  No installation directory found — skipping."
fi

# --- Clear Launch Services cache ---
echo -e "${BLUE}[4/4]${NC} Clearing Launch Services cache ..."
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user 2>/dev/null || true
echo -e "${GREEN}[OK]${NC}    Cache cleared."

echo ""
echo "  ============================================"
echo "    Grey Math has been uninstalled."
echo "  ============================================"
echo ""
