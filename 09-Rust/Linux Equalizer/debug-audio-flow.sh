#!/bin/bash

echo "=== AUDIO FLOW DIAGNOSTIC ==="
echo ""

# Cleanup first
echo "1. Cleaning up..."
pkill -9 parec pacat 2>/dev/null
pactl unload-module module-null-sink 2>/dev/null
sleep 0.5

# Test 1: Direct hardware playback
echo ""
echo "2. TEST 1: Direct hardware playback"
pactl set-default-sink alsa_output.pci-0000_00_1f.3.analog-stereo
echo "   Playing sound directly to hardware..."
paplay /usr/share/sounds/freedesktop/stereo/complete.oga 2>/dev/null
echo "   Did you hear Test 1? (Direct)"

# Wait
sleep 2

# Test 2: Through virtual sink with parec/pacat
echo ""
echo "3. TEST 2: Through virtual sink with parec|pacat"

# Create virtual sink
echo "   Creating virtual sink..."
MODULE_ID=$(pactl load-module module-null-sink sink_name=debug_eq rate=48000 channels=2)
echo "   Module ID: $MODULE_ID"

# Wait for it to be ready
sleep 0.5

# Set volume
pactl set-sink-volume debug_eq 150%
echo "   Volume set to 150%"

# Set as default
pactl set-default-sink debug_eq
echo "   Set as default sink"
pactl info | grep "Default Sink"

# Start the capture/playback chain
echo ""
echo "   Starting parec|pacat chain..."
parec --rate=48000 --channels=2 --format=float32le --device=debug_eq.monitor --latency-msec=30 2>&1 | \
pacat --rate=48000 --channels=2 --format=float32le --device=alsa_output.pci-0000_00_1f.3.analog-stereo --latency-msec=30 2>&1 &
CHAIN_PID=$!
echo "   Chain PID: $CHAIN_PID"

sleep 1

# Verify chain is running
echo ""
echo "   Checking processes:"
ps aux | grep -E "parec|pacat" | grep -v grep

# Play test sound
echo ""
echo "   Playing sound through virtual sink (TEST 2)..."
paplay /usr/share/sounds/freedesktop/stereo/complete.oga 2>/dev/null &
PLAY_PID=$!

# Show what's happening
sleep 0.5
echo ""
echo "   Current sink-inputs:"
pactl list sink-inputs 2>/dev/null | grep -E "Sink:|application.name" 

# Wait for sound to finish
wait $PLAY_PID 2>/dev/null
sleep 1

echo ""
echo "   Did you hear Test 2? (Through virtual chain)"

# Cleanup
echo ""
echo "4. Cleaning up..."
kill $CHAIN_PID 2>/dev/null
pactl unload-module $MODULE_ID 2>/dev/null
pactl set-default-sink alsa_output.pci-0000_00_1f.3.analog-stereo

echo ""
echo "=== DIAGNOSTIC COMPLETE ==="
echo "If you heard Test 1 but NOT Test 2, there's a routing issue."
echo "If you heard both, the basic chain works and the issue is in the app."
