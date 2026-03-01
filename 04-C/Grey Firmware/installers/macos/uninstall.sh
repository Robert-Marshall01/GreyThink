#!/usr/bin/env bash
# ============================================================================
# Grey Firmware - macOS Uninstaller
# ============================================================================
# Removes the Grey Firmware .app bundle and CLI symlinks.
#
# Usage:
#   sudo ./uninstall.sh
#   or
#   sudo grey_firmware_uninstall
# ============================================================================

set -euo pipefail

APP_BUNDLE="/Applications/Grey Firmware.app"
USR_LOCAL_BIN="/usr/local/bin"

# ---------- Pre-flight checks ----------
if [[ $EUID -ne 0 ]]; then
    echo "Error: This uninstaller must be run as root (sudo)."
    exit 1
fi

echo "╔═══════════════════════════════════════════════════╗"
echo "║   Grey Firmware - macOS Uninstaller                ║"
echo "╚═══════════════════════════════════════════════════╝"
echo ""

# ---------- Remove CLI symlinks ----------
echo "[1/2] Removing CLI symlinks..."
rm -f "$USR_LOCAL_BIN/grey_firmware"
rm -f "$USR_LOCAL_BIN/grey_firmware_gui"
rm -f "$USR_LOCAL_BIN/grey_firmware_uninstall"

# ---------- Remove .app bundle ----------
echo "[2/2] Removing application bundle..."
if [[ -d "$APP_BUNDLE" ]]; then
    rm -rf "$APP_BUNDLE"
    echo "  Removed: $APP_BUNDLE"
else
    echo "  Application bundle not found (already removed?)."
fi

echo ""
echo "Grey Firmware has been uninstalled."
echo ""
