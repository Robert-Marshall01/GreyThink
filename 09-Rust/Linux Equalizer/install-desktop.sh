#!/bin/bash
# Install Linux Equalizer desktop shortcut and binary
# This makes the app appear in your Ubuntu application menu

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
BINARY_SRC="$SCRIPT_DIR/target/release/linux-equalizer"
BINARY_DEST="$HOME/.local/bin/linux-equalizer"
DESKTOP_SRC="$SCRIPT_DIR/linux-equalizer.desktop"
DESKTOP_DEST="$HOME/.local/share/applications/linux-equalizer.desktop"
AUTOSTART_DEST="$HOME/.config/autostart/linux-equalizer.desktop"

echo "🎛️  Linux Equalizer Desktop Installer"
echo "======================================"
echo ""

# Check if binary exists
if [ ! -f "$BINARY_SRC" ]; then
    echo "❌ Binary not found. Building first..."
    cd "$SCRIPT_DIR"
    cargo build --release
fi

# Create directories
mkdir -p "$HOME/.local/bin"
mkdir -p "$HOME/.local/share/applications"

# Copy binary
echo "📦 Installing binary to ~/.local/bin/"
cp "$BINARY_SRC" "$BINARY_DEST"
chmod +x "$BINARY_DEST"

# Check if ~/.local/bin is in PATH
if [[ ":$PATH:" != *":$HOME/.local/bin:"* ]]; then
    echo ""
    echo "⚠️  Note: ~/.local/bin is not in your PATH"
    echo "   Add this to your ~/.bashrc:"
    echo '   export PATH="$HOME/.local/bin:$PATH"'
    echo ""
fi

# Install desktop file
echo "🖥️  Installing desktop entry..."
cp "$DESKTOP_SRC" "$DESKTOP_DEST"

# Update desktop database
if command -v update-desktop-database &> /dev/null; then
    update-desktop-database "$HOME/.local/share/applications" 2>/dev/null || true
fi

echo ""
echo "✅ Installation complete!"
echo ""
echo "You can now:"
echo "  • Search for 'Linux Equalizer' in your application menu"
echo "  • Or run 'linux-equalizer' from terminal"
echo ""

# Ask about autostart
read -p "🚀 Enable autostart on login? [y/N] " -n 1 -r
echo ""
if [[ $REPLY =~ ^[Yy]$ ]]; then
    mkdir -p "$HOME/.config/autostart"
    cat > "$AUTOSTART_DEST" << EOF
[Desktop Entry]
Type=Application
Name=Linux Equalizer
Comment=System-wide audio equalizer (background)
Exec=linux-equalizer --headless
Icon=multimedia-volume-control
Terminal=false
Categories=Audio;AudioVideo;
X-GNOME-Autostart-enabled=true
StartupNotify=false
Hidden=false
EOF
    echo "✅ Autostart enabled! The equalizer will run in background on login."
    echo "   (Use the GUI app to adjust settings)"
else
    # Remove autostart if it exists
    rm -f "$AUTOSTART_DEST"
    echo "ℹ️  Autostart not enabled. You can run this script again to enable it."
fi

echo ""
echo "🎵 Enjoy your music with Linux Equalizer!"
