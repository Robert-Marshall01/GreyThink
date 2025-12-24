#!/bin/bash
# Uninstall Linux Equalizer service

echo "=== Linux Equalizer Service Uninstaller ==="
echo

# Stop and disable systemd service
if systemctl --user is-active --quiet linux-equalizer.service; then
    systemctl --user stop linux-equalizer.service
    echo "✓ Service stopped"
fi

if systemctl --user is-enabled --quiet linux-equalizer.service 2>/dev/null; then
    systemctl --user disable linux-equalizer.service
    echo "✓ Service disabled"
fi

# Remove service file
if [ -f ~/.config/systemd/user/linux-equalizer.service ]; then
    rm ~/.config/systemd/user/linux-equalizer.service
    echo "✓ Service file removed"
fi

# Remove desktop autostart entry
if [ -f ~/.config/autostart/linux-equalizer.desktop ]; then
    rm ~/.config/autostart/linux-equalizer.desktop
    echo "✓ Desktop autostart entry removed"
fi

# Reload systemd
systemctl --user daemon-reload

echo
echo "=== Uninstallation Complete ==="
echo "The equalizer will no longer start automatically."
echo
