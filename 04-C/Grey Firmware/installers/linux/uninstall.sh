#!/usr/bin/env bash
# ============================================================================
# Grey Firmware - Linux Uninstaller
# ============================================================================
# Removes all files installed by install.sh
#
# Usage:
#   sudo ./uninstall.sh [--prefix /opt/grey-firmware]
#   or
#   sudo grey_firmware_uninstall
# ============================================================================

set -euo pipefail

APP_NAME="grey-firmware"
DEFAULT_PREFIX="/opt/grey-firmware"

# ---------- Parse arguments ----------
PREFIX="$DEFAULT_PREFIX"
while [[ $# -gt 0 ]]; do
    case "$1" in
        --prefix)
            PREFIX="$2"
            shift 2
            ;;
        -h|--help)
            echo "Usage: sudo $0 [--prefix /install/path]"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# ---------- Pre-flight checks ----------
if [[ $EUID -ne 0 ]]; then
    echo "Error: This uninstaller must be run as root (sudo)."
    exit 1
fi

if [[ ! -d "$PREFIX" ]]; then
    echo "Error: Installation not found at $PREFIX"
    exit 1
fi

echo "╔════════════════════════════════════════════════╗"
echo "║   Grey Firmware - Linux Uninstaller            ║"
echo "╚════════════════════════════════════════════════╝"
echo ""
echo "  Removing installation at: $PREFIX"
echo ""

# ---------- Remove symlinks ----------
echo "[1/4] Removing symlinks from /usr/local/bin..."
rm -f /usr/local/bin/grey_firmware
rm -f /usr/local/bin/grey_firmware_gui

# ---------- Remove desktop entry ----------
echo "[2/4] Removing desktop entry..."
rm -f /usr/share/applications/grey-firmware.desktop
if command -v update-desktop-database &>/dev/null; then
    update-desktop-database /usr/share/applications 2>/dev/null || true
fi

# ---------- Remove installed files ----------
echo "[3/4] Removing installed files..."
rm -f "$PREFIX/bin/grey_firmware"
rm -f "$PREFIX/bin/grey_firmware_gui"
rm -f "$PREFIX/bin/grey_firmware_uninstall"
rm -rf "$PREFIX/include"
rm -rf "$PREFIX/share"

# ---------- Remove prefix directory if empty ----------
echo "[4/4] Cleaning up..."
rmdir "$PREFIX/bin" 2>/dev/null || true
rmdir "$PREFIX" 2>/dev/null || true

echo ""
echo "Grey Firmware has been uninstalled."
echo ""
