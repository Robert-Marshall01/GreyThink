#!/bin/bash

echo "==============================================="
echo "  Linux Equalizer - Complete Audio Test"
echo "==============================================="
echo ""

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "Step 1: Checking current audio state..."
echo "----------------------------------------"
echo "Current sinks:"
pactl list short sinks
echo ""
echo "Default sink:"
pactl info | grep "Default Sink"
echo ""
echo "Active audio streams:"
pactl list short sink-inputs
echo ""

echo "Step 2: Checking for running equalizer..."
echo "----------------------------------------"
if pgrep -f linux-equalizer > /dev/null; then
    echo -e "${GREEN}✓ Equalizer is running${NC}"
    echo "Processes:"
    ps aux | grep -E "linux-equalizer|parec|pacat" | grep -v grep
else
    echo -e "${RED}✗ Equalizer is NOT running${NC}"
fi
echo ""

echo "Step 3: Testing audio playback..."
echo "----------------------------------------"
echo "Playing test tone for 2 seconds..."
speaker-test -t sine -f 440 -c 2 -l 1 2>&1 | head -5 &
SPEAKER_PID=$!
sleep 2
kill $SPEAKER_PID 2>/dev/null
echo ""
echo "Did you hear the test tone? (y/n)"
read -r RESPONSE

if [[ "$RESPONSE" =~ ^[Yy]$ ]]; then
    echo -e "${GREEN}✓ Audio is working!${NC}"
else
    echo -e "${RED}✗ No audio detected${NC}"
    echo ""
    echo "Troubleshooting information:"
    echo "1. Check if virtual device is receiving audio:"
    pactl list short sink-inputs
    echo ""
    echo "2. Check if parec/pacat are running:"
    pgrep -a parec
    pgrep -a pacat
    echo ""
    echo "3. Check virtual sink state:"
    pactl list sinks | grep -A 10 "Name: linux_eq_output"
fi

echo ""
echo "Step 4: Testing with real application..."
echo "----------------------------------------"
echo "You can now:"
echo "  1. Open Spotify or YouTube"
echo "  2. Play some music"
echo "  3. Check if audio plays through the equalizer"
echo "  4. Adjust EQ sliders in the GUI to hear changes"
echo ""
echo "To monitor audio routing in real-time:"
echo "  watch -n 1 'pactl list short sink-inputs'"
echo ""
echo "To check for errors:"
echo "  journalctl -f | grep -E 'parec|pacat|equalizer'"
echo ""
