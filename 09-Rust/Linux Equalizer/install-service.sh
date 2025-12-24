#!/bin/bash
# Install Linux Equalizer as a systemd user service

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BINARY="$SCRIPT_DIR/target/release/linux-equalizer"
SERVICE_FILE="$SCRIPT_DIR/linux-equalizer.service"
DESKTOP_FILE="$SCRIPT_DIR/linux-equalizer.desktop"
USER_ID=$(id -u)

echo "=== Linux Equalizer Service Installer ==="
echo

# Check if binary exists
if [ ! -f "$BINARY" ]; then
    echo "Error: Binary not found at $BINARY"
    echo "Please run 'cargo build --release' first."
    exit 1
fi

echo "Binary found: $BINARY"
echo "User ID: $USER_ID"
echo

# Create systemd user directory if it doesn't exist
mkdir -p ~/.config/systemd/user

# Create service file with proper path substitution
cat > ~/.config/systemd/user/linux-equalizer.service << EOF
[Unit]
Description=Linux Equalizer - System-Wide Audio Equalizer
After=pipewire.service pipewire-pulse.service pulseaudio.service sound.target
Wants=pipewire-pulse.service

[Service]
Type=simple
ExecStart="$BINARY" --headless
Restart=on-failure
RestartSec=5
Environment="PIPEWIRE_RUNTIME_DIR=/run/user/$USER_ID"
Environment="PULSE_RUNTIME_PATH=/run/user/$USER_ID/pulse"
Environment="XDG_RUNTIME_DIR=/run/user/$USER_ID"

[Install]
WantedBy=default.target
EOF

echo "✓ Service file installed to ~/.config/systemd/user/linux-equalizer.service"

# Reload systemd
systemctl --user daemon-reload

echo "✓ Systemd daemon reloaded"

# Ask user which startup method they prefer
echo
echo "Choose startup method:"
echo "1) Systemd service (recommended for servers/background use)"
echo "2) Desktop autostart (recommended for desktop environments)"
echo "3) Both"
echo "4) None (manual installation)"
read -p "Enter choice [1-4]: " choice

case $choice in
    1)
        systemctl --user enable linux-equalizer.service
        systemctl --user start linux-equalizer.service
        echo "✓ Systemd service enabled and started"
        echo "  - View status: systemctl --user status linux-equalizer"
        echo "  - View logs: journalctl --user -u linux-equalizer -f"
        echo "  - Stop service: systemctl --user stop linux-equalizer"
        echo "  - Disable autostart: systemctl --user disable linux-equalizer"
        ;;
    2)
        mkdir -p ~/.config/autostart
        sed "s|/home/robert-marshall01/Desktop/Linux Equalizer|$SCRIPT_DIR|g" "$DESKTOP_FILE" > ~/.config/autostart/linux-equalizer.desktop
        chmod +x ~/.config/autostart/linux-equalizer.desktop
        echo "✓ Desktop autostart entry created"
        echo "  - Location: ~/.config/autostart/linux-equalizer.desktop"
        echo "  - Remove file to disable autostart"
        ;;
    3)
        systemctl --user enable linux-equalizer.service
        systemctl --user start linux-equalizer.service
        mkdir -p ~/.config/autostart
        sed "s|/home/robert-marshall01/Desktop/Linux Equalizer|$SCRIPT_DIR|g" "$DESKTOP_FILE" > ~/.config/autostart/linux-equalizer.desktop
        chmod +x ~/.config/autostart/linux-equalizer.desktop
        echo "✓ Both systemd service and desktop autostart configured"
        echo "  Note: You may want to choose only one to avoid running twice"
        ;;
    4)
        echo "Service installed but not enabled."
        echo "To enable manually:"
        echo "  systemctl --user enable linux-equalizer.service"
        echo "  systemctl --user start linux-equalizer.service"
        ;;
    *)
        echo "Invalid choice. Service installed but not enabled."
        ;;
esac

echo
echo "=== Installation Complete ==="
echo
echo "The equalizer will now start automatically on login."
echo "To open the GUI for adjustments, run: $BINARY"
echo
