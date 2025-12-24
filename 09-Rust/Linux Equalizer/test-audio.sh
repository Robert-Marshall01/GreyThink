#!/bin/bash

# Linux Equalizer Test Script
# This script plays a test tone and applies the equalizer settings in real-time

echo "Linux Equalizer - Audio Test"
echo "=============================="
echo ""
echo "This will play a test tone with your current equalizer settings applied."
echo "Press Ctrl+C to stop."
echo ""

# Check if the equalizer app is running
if ! pgrep -f "linux-equalizer" > /dev/null; then
    echo "Starting equalizer GUI..."
    cargo run --release &
    EQ_PID=$!
    sleep 2
fi

# Install dependencies if needed
if ! command -v pactl &> /dev/null; then
    echo "PulseAudio tools not found. Please install pulseaudio-utils"
    exit 1
fi

if ! command -v speaker-test &> /dev/null; then
    echo "speaker-test not found. Please install alsa-utils"
    exit 1
fi

echo "Playing test tone through equalizer..."
echo "(Adjust the sliders in the GUI to hear the effects)"
echo ""

# Play a test tone
speaker-test -t sine -f 1000 -l 1

echo ""
echo "Test complete!"
