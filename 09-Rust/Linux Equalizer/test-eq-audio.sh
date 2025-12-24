#!/bin/bash
set -e

echo "=== EQUALIZER AUDIO ROUTING TEST ==="
echo

# Find hardware sink
HW_SINK=$(pactl list short sinks | grep -v linux_eq | awk '{print $2}' | head -1)
echo "Hardware sink: $HW_SINK"

# Create virtual device
echo "Creating virtual device..."
pactl load-module module-null-sink sink_name=linux_eq_output sink_properties=device.description='Linux_Equalizer' rate=48000 channels=2 > /dev/null

# Restore default
pactl set-default-sink "$HW_SINK"
echo "Default kept at: $HW_SINK"

# Start audio pipeline
echo "Starting audio pipeline..."
parec --rate=48000 --channels=2 --format=float32le --device=linux_eq_output.monitor --latency-msec=20 | \
pacat --rate=48000 --channels=2 --format=float32le --device="$HW_SINK" --latency-msec=20 &
PIPELINE_PID=$!

sleep 1

echo
echo "✓ Pipeline running (PID: $PIPELINE_PID)"
echo "✓ Now play test sound through linux_eq_output..."
echo

# Play test sound THROUGH the equalizer device
paplay -d linux_eq_output /usr/share/sounds/alsa/Front_Center.wav 2>&1

echo
echo "Did you hear the sound? (y/n)"
read -r answer

# Cleanup
kill $PIPELINE_PID 2>/dev/null
pactl unload-module module-null-sink 2>/dev/null

if [[ "$answer" == "y" ]]; then
    echo "✓ SUCCESS: Audio routing works!"
    exit 0
else
    echo "✗ FAILURE: No audio heard"
    exit 1
fi
