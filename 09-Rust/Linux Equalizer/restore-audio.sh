#!/bin/bash

# Restore normal audio routing (undo setup-audio.sh)

echo "Restoring normal audio routing..."

# Get the original default sink (try to detect hardware)
HARDWARE_SINK=$(pactl list short sinks | grep -v "equalizer_sink" | head -n1 | cut -f2)

if [ -z "$HARDWARE_SINK" ]; then
    echo "Could not detect hardware sink, using alsa_output as fallback"
    HARDWARE_SINK=$(pactl list short sinks | grep "alsa_output" | head -n1 | cut -f2)
fi

# Set hardware as default
if [ -n "$HARDWARE_SINK" ]; then
    echo "Setting $HARDWARE_SINK as default..."
    pactl set-default-sink "$HARDWARE_SINK"
fi

# Unload loopback modules
echo "Removing loopback modules..."
pactl list short modules | grep "module-loopback" | cut -f1 | while read -r mod; do
    pactl unload-module "$mod"
done

# Remove equalizer sink
echo "Removing equalizer sink..."
pactl list short modules | grep "equalizer_sink" | cut -f1 | while read -r mod; do
    pactl unload-module "$mod"
done

echo "✓ Audio routing restored to normal!"
