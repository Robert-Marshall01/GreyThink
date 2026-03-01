#!/usr/bin/env bash
# ============================================================================
# Grey Firmware - Linux Installer
# ============================================================================
# Installs the Grey Firmware CLI and GUI to /opt/grey-firmware with symlinks
# in /usr/local/bin, a .desktop entry, and an uninstall script.
#
# Usage:
#   sudo ./install.sh [--prefix /opt/grey-firmware]
#
# Requirements:
#   - Pre-built binaries in ../../build/ (run cmake --build build first)
#   - GTK3 runtime: sudo apt install libgtk-3-0   (Debian/Ubuntu)
#                   sudo dnf install gtk3          (Fedora/RHEL)
# ============================================================================

set -euo pipefail

APP_NAME="grey-firmware"
APP_VERSION="1.0.0"
DEFAULT_PREFIX="/opt/grey-firmware"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
BUILD_DIR="$PROJECT_ROOT/build"

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
    echo "Error: This installer must be run as root (sudo)."
    exit 1
fi

if [[ ! -f "$BUILD_DIR/grey_firmware" ]]; then
    echo "Error: Build artifacts not found in $BUILD_DIR"
    echo "       Run 'cmake --build build' first."
    exit 1
fi

echo "╔════════════════════════════════════════════╗"
echo "║   Grey Firmware $APP_VERSION - Linux Installer   ║"
echo "╚════════════════════════════════════════════╝"
echo ""
echo "  Install prefix: $PREFIX"
echo ""

# ---------- Install directories ----------
install -d "$PREFIX/bin"
install -d "$PREFIX/include"
install -d "$PREFIX/share/doc/$APP_NAME"
install -d "$PREFIX/share/applications"

# ---------- Install binaries ----------
echo "[1/5] Installing binaries..."
install -m 755 "$BUILD_DIR/grey_firmware"     "$PREFIX/bin/grey_firmware"

if [[ -f "$BUILD_DIR/grey_firmware_gui" ]]; then
    install -m 755 "$BUILD_DIR/grey_firmware_gui" "$PREFIX/bin/grey_firmware_gui"
fi

# ---------- Install headers ----------
echo "[2/5] Installing headers..."
cp -r "$PROJECT_ROOT/include/"* "$PREFIX/include/"

# ---------- Install documentation ----------
echo "[3/5] Installing documentation..."
install -m 644 "$PROJECT_ROOT/LICENSE"   "$PREFIX/share/doc/$APP_NAME/"
install -m 644 "$PROJECT_ROOT/README.md" "$PREFIX/share/doc/$APP_NAME/"

if [[ -d "$PROJECT_ROOT/docs" ]]; then
    cp -r "$PROJECT_ROOT/docs/"* "$PREFIX/share/doc/$APP_NAME/"
fi

# ---------- Symlinks into PATH ----------
echo "[4/5] Creating symlinks in /usr/local/bin..."
ln -sf "$PREFIX/bin/grey_firmware"     /usr/local/bin/grey_firmware
if [[ -f "$PREFIX/bin/grey_firmware_gui" ]]; then
    ln -sf "$PREFIX/bin/grey_firmware_gui" /usr/local/bin/grey_firmware_gui
fi

# ---------- Desktop entry ----------
echo "[5/5] Installing desktop entry..."
cat > /usr/share/applications/grey-firmware.desktop <<EOF
[Desktop Entry]
Type=Application
Name=Grey Firmware
GenericName=Firmware Dashboard
Comment=Modular Firmware Framework Demonstration
Exec=$PREFIX/bin/grey_firmware_gui
Icon=utilities-system-monitor
Terminal=false
Categories=Development;Engineering;Electronics;
Keywords=firmware;embedded;IoT;CAN;MQTT;
StartupNotify=true
EOF
chmod 644 /usr/share/applications/grey-firmware.desktop

# Update desktop database if available
if command -v update-desktop-database &>/dev/null; then
    update-desktop-database /usr/share/applications 2>/dev/null || true
fi

# ---------- Install uninstaller ----------
install -m 755 "$SCRIPT_DIR/uninstall.sh" "$PREFIX/bin/grey_firmware_uninstall"

echo ""
echo "Installation complete!"
echo ""
echo "  CLI:   grey_firmware"
echo "  GUI:   grey_firmware_gui"
echo "  Uninstall: sudo $PREFIX/bin/grey_firmware_uninstall"
echo ""
