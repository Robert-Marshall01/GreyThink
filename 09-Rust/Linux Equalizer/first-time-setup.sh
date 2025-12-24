#!/bin/bash
# First-time setup script for Linux Equalizer

echo "=== Linux Equalizer Setup ==="
echo ""
echo "This script will:"
echo "1. Install the systemd service for background operation"
echo "2. Start the equalizer service"
echo "3. Guide you to set it as your default audio device"
echo ""
read -p "Continue? (y/n) " -n 1 -r
echo ""

if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Setup cancelled."
    exit 0
fi

# Build the project
echo ""
echo "Building Linux Equalizer..."
cargo build --release

if [ $? -ne 0 ]; then
    echo "Build failed. Please fix any errors and try again."
    exit 1
fi

# Install the service
echo ""
echo "Installing systemd service..."
bash install-service.sh

# Start the service
echo ""
echo "Starting the equalizer service..."
systemctl --user start linux-equalizer.service

sleep 2

# Check if it's running
if systemctl --user is-active --quiet linux-equalizer.service; then
    echo ""
    echo "✓ Service is running!"
    echo ""
    echo "=== IMPORTANT: Final Step ==="
    echo ""
    echo "The virtual audio device 'Linux Equalizer' has been created."
    echo "To use it, you need to select it as your output device:"
    echo ""
    echo "1. Open your system Sound Settings"
    echo "2. Under 'Output', select 'Linux Equalizer'"
    echo "3. Play some audio to test"
    echo ""
    echo "Your audio will now be processed through the equalizer!"
    echo ""
    echo "=== Usage ==="
    echo "• To adjust EQ: run ./target/release/linux-equalizer"
    echo "• To check status: systemctl --user status linux-equalizer"
    echo "• To stop: systemctl --user stop linux-equalizer"
    echo ""
else
    echo ""
    echo "⚠ Service failed to start. Checking logs..."
    journalctl --user -u linux-equalizer -n 20 --no-pager
fi
