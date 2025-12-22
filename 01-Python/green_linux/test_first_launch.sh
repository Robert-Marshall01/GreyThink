#!/bin/bash
# Script to simulate first launch by temporarily moving config file

CONFIG_FILE=~/.config/powerapp/config.json
BACKUP_FILE=~/.config/powerapp/config.json.backup

if [ -f "$CONFIG_FILE" ]; then
    echo "Backing up existing config to $BACKUP_FILE"
    mv "$CONFIG_FILE" "$BACKUP_FILE"
fi

echo "Config file removed - launching app to test first-time setup..."
echo "You should see the timezone setup dialog appear first."
echo ""
echo "After you select your timezone, the main window will appear."
echo ""

# Launch the app
cd /home/robertmarshall/Desktop/green_linux
python -m powerapp.gtk.main

# Restore backup if it exists
if [ -f "$BACKUP_FILE" ] && [ ! -f "$CONFIG_FILE" ]; then
    echo ""
    echo "App closed without setting timezone. Restoring backup..."
    mv "$BACKUP_FILE" "$CONFIG_FILE"
fi
