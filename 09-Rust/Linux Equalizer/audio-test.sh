#!/bin/bash
# Simple GUI to test audio routing

echo "╔═══════════════════════════════════════════════════════════╗"
echo "║         Linux Equalizer - Audio Test Tool                ║"
echo "╚═══════════════════════════════════════════════════════════╝"
echo ""

# Check if equalizer is running
if ! pgrep -f "linux-equalizer" > /dev/null; then
    echo "⚠️  Equalizer not running!"
    echo "   Start it with: ./target/release/linux-equalizer --headless &"
    exit 1
fi

echo "✓ Equalizer is running"
echo ""

# Show available sinks
echo "Available audio devices:"
echo "------------------------"
pactl list short sinks | while IFS=$'\t' read -r id name driver format status; do
    DEFAULT=""
    if [ "$(pactl get-default-sink)" = "$name" ]; then
        DEFAULT=" [DEFAULT]"
    fi
    STATUS_ICON="⚪"
    if [ "$status" = "RUNNING" ]; then
        STATUS_ICON="🟢"
    elif [ "$status" = "IDLE" ]; then
        STATUS_ICON="🟡"
    fi
    echo "  $STATUS_ICON $name$DEFAULT"
done
echo ""

# Test 1: Direct hardware playback
echo "Test 1: Playing sound directly to hardware (no EQ)..."
paplay -d alsa_output.pci-0000_00_1f.3.hdmi-stereo /usr/share/sounds/alsa/Front_Center.wav 2>/dev/null
echo "  ✓ Did you hear a beep from your speakers/headphones?"
echo ""

# Test 2: Through equalizer
echo "Test 2: Playing sound through Linux Equalizer (with EQ)..."
paplay -d linux_eq_output /usr/share/sounds/alsa/Front_Center.wav 2>/dev/null
echo "  ✓ Did you hear a beep? (This one went through the EQ)"
echo ""

# Test 3: Check if audio is being processed
echo "Test 3: Checking audio pipeline..."
if ps aux | grep -E "parec.*linux_eq_output.monitor" | grep -v grep > /dev/null; then
    echo "  ✓ parec is capturing from monitor"
else
    echo "  ❌ parec is NOT running!"
fi

if ps aux | grep -E "pacat.*alsa_output" | grep -v grep > /dev/null; then
    echo "  ✓ pacat is outputting to hardware"
else
    echo "  ❌ pacat is NOT running!"
fi
echo ""

# Test 4: Volume check
echo "Test 4: Volume levels..."
LINUX_EQ_VOL=$(pactl list sinks | grep -A 15 "Name: linux_eq_output" | grep "Volume:" | head -1 | awk '{print $5}')
HARDWARE_VOL=$(pactl list sinks | grep -A 15 "Name: alsa_output" | grep "Volume:" | head -1 | awk '{print $5}')
echo "  Linux Equalizer volume: $LINUX_EQ_VOL"
echo "  Hardware volume: $HARDWARE_VOL"
echo ""

# Instructions
echo "═══════════════════════════════════════════════════════════"
echo "To use the equalizer with your apps:"
echo ""
echo "  1. Open Sound Settings (Settings → Sound)"
echo "  2. Go to 'Applications' or 'Output' tab"
echo "  3. For Spotify/Firefox/etc., select 'Linux Equalizer'"
echo ""
echo "OR use this command to move currently playing audio:"
echo "  pactl list short sink-inputs  # Find the stream ID"
echo "  pactl move-sink-input <ID> linux_eq_output"
echo "═══════════════════════════════════════════════════════════"
