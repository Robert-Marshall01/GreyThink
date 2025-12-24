#!/bin/bash

# Setup script for Linux Equalizer system-wide audio processing
# This configures PulseAudio to route all audio through the equalizer

echo "=================================="
echo "Linux Equalizer - Audio Setup"
echo "=================================="
echo ""

# Check if PulseAudio is running
if ! pgrep -x pulseaudio > /dev/null && ! pgrep -x pipewire-pulse > /dev/null; then
    echo "❌ PulseAudio/PipeWire not running!"
    echo "Please start your audio server first."
    exit 1
fi

echo "✓ Audio server detected"
echo ""

# Get default sink
DEFAULT_SINK=$(pactl get-default-sink)
echo "Current default sink: $DEFAULT_SINK"
echo ""

echo "Setting up audio routing for system-wide equalization..."
echo ""

# Create a null sink for the equalizer
echo "1. Creating virtual equalizer sink..."
pactl load-module module-null-sink sink_name=equalizer_sink sink_properties=device.description="Equalizer" || {
    echo "   Sink might already exist, continuing..."
}

# Load loopback from equalizer to actual output
echo "2. Creating loopback to hardware output..."
LOOPBACK_MODULE=$(pactl load-module module-loopback source=equalizer_sink.monitor sink="$DEFAULT_SINK" latency_msec=1)

# Set equalizer as default
echo "3. Setting equalizer as default output..."
pactl set-default-sink equalizer_sink

echo ""
echo "✓ Audio routing configured!"
echo ""
echo "Now run the equalizer app:"
echo "  cargo run --release"
echo ""
echo "All audio will now flow through the equalizer."
echo ""
echo "To restore normal audio:"
echo "  pactl set-default-sink $DEFAULT_SINK"
echo "  pactl unload-module $LOOPBACK_MODULE"
echo ""
echo "Or simply run: ./restore-audio.sh"
