#!/usr/bin/env bash
# ============================================================================
# Grey Firmware - macOS Installer
# ============================================================================
# Creates a macOS .app bundle in /Applications and optional CLI symlinks.
#
# Usage:
#   sudo ./install.sh
#
# Requirements:
#   - Pre-built binaries in ../../build/ (run cmake --build build first)
#   - GTK3 runtime: brew install gtk+3
# ============================================================================

set -euo pipefail

APP_NAME="Grey Firmware"
APP_BUNDLE="/Applications/Grey Firmware.app"
APP_VERSION="1.0.0"
BUNDLE_ID="org.greyfirmware.gui"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
BUILD_DIR="$PROJECT_ROOT/build"
USR_LOCAL_BIN="/usr/local/bin"

# ---------- Pre-flight checks ----------
if [[ $EUID -ne 0 ]]; then
    echo "Error: This installer must be run as root (sudo)."
    exit 1
fi

if [[ ! -f "$BUILD_DIR/grey_firmware" ]]; then
    echo "Error: Build artifacts not found in $BUILD_DIR"
    echo "       Run 'cmake --build build' first."
    exit 1
fi

echo "╔═══════════════════════════════════════════════╗"
echo "║   Grey Firmware $APP_VERSION - macOS Installer      ║"
echo "╚═══════════════════════════════════════════════╝"
echo ""

# ---------- Remove old installation if present ----------
if [[ -d "$APP_BUNDLE" ]]; then
    echo "  Removing previous installation..."
    rm -rf "$APP_BUNDLE"
fi

# ---------- Create .app bundle structure ----------
echo "[1/5] Creating application bundle..."
CONTENTS="$APP_BUNDLE/Contents"
MACOS_DIR="$CONTENTS/MacOS"
RESOURCES="$CONTENTS/Resources"

mkdir -p "$MACOS_DIR"
mkdir -p "$RESOURCES"
mkdir -p "$RESOURCES/include"
mkdir -p "$RESOURCES/docs"

# ---------- Info.plist ----------
echo "[2/5] Writing Info.plist..."
cat > "$CONTENTS/Info.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleName</key>
    <string>Grey Firmware</string>
    <key>CFBundleDisplayName</key>
    <string>Grey Firmware</string>
    <key>CFBundleIdentifier</key>
    <string>${BUNDLE_ID}</string>
    <key>CFBundleVersion</key>
    <string>${APP_VERSION}</string>
    <key>CFBundleShortVersionString</key>
    <string>${APP_VERSION}</string>
    <key>CFBundleExecutable</key>
    <string>grey_firmware_gui</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
    <key>CFBundleSignature</key>
    <string>GREY</string>
    <key>LSMinimumSystemVersion</key>
    <string>10.13</string>
    <key>NSHighResolutionCapable</key>
    <true/>
    <key>NSHumanReadableCopyright</key>
    <string>Grey Firmware Team</string>
</dict>
</plist>
PLIST

# ---------- Install binaries ----------
echo "[3/5] Installing binaries..."
install -m 755 "$BUILD_DIR/grey_firmware"     "$MACOS_DIR/grey_firmware"
if [[ -f "$BUILD_DIR/grey_firmware_gui" ]]; then
    install -m 755 "$BUILD_DIR/grey_firmware_gui" "$MACOS_DIR/grey_firmware_gui"
fi

# ---------- Install resources ----------
echo "[4/5] Installing resources..."
cp "$PROJECT_ROOT/LICENSE"   "$RESOURCES/"
cp "$PROJECT_ROOT/README.md" "$RESOURCES/"
cp -r "$PROJECT_ROOT/include/"* "$RESOURCES/include/" 2>/dev/null || true
if [[ -d "$PROJECT_ROOT/docs" ]]; then
    cp -r "$PROJECT_ROOT/docs/"* "$RESOURCES/docs/" 2>/dev/null || true
fi

# ---------- CLI symlinks ----------
echo "[5/5] Creating CLI symlinks in $USR_LOCAL_BIN..."
mkdir -p "$USR_LOCAL_BIN"
ln -sf "$MACOS_DIR/grey_firmware"     "$USR_LOCAL_BIN/grey_firmware"
if [[ -f "$MACOS_DIR/grey_firmware_gui" ]]; then
    ln -sf "$MACOS_DIR/grey_firmware_gui" "$USR_LOCAL_BIN/grey_firmware_gui"
fi

# Install uninstaller
install -m 755 "$SCRIPT_DIR/uninstall.sh" "$MACOS_DIR/grey_firmware_uninstall"
ln -sf "$MACOS_DIR/grey_firmware_uninstall" "$USR_LOCAL_BIN/grey_firmware_uninstall"

echo ""
echo "Installation complete!"
echo ""
echo "  App:       /Applications/Grey Firmware.app"
echo "  CLI:       grey_firmware"
echo "  GUI:       grey_firmware_gui  (or open the app from Launchpad)"
echo "  Uninstall: sudo grey_firmware_uninstall"
echo ""
