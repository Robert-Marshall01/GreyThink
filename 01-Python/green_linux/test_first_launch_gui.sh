#!/bin/bash
# Test script for first-launch timezone dialog
# This script helps simulate and verify the first-launch experience

set -e

CONFIG_FILE="$HOME/.config/powerapp/config.json"

echo "==================================="
echo "PowerApp First Launch Test"
echo "==================================="
echo ""

# Check if config exists
if [ -f "$CONFIG_FILE" ]; then
    echo "Config file exists at: $CONFIG_FILE"
    echo "Creating backup and removing for first-launch test..."
    mv "$CONFIG_FILE" "${CONFIG_FILE}.backup.$(date +%s)"
    echo "Backup created. Original config removed."
else
    echo "No config file found (good for first-launch test)"
fi

echo ""
echo "Launching PowerApp..."
echo "EXPECTED BEHAVIOR:"
echo "  1. A setup window should appear first"
echo "  2. You should see a 'Initial Setup: Select Your Timezone' dialog"
echo "  3. Select your timezone and click 'Continue'"
echo "  4. The main PowerApp window should then appear"
echo ""
echo "If you see the main window directly without the timezone setup,"
echo "that's a BUG - please report it!"
echo ""

# Launch the app
python3 -m powerapp.gtk.main

echo ""
echo "App closed."
