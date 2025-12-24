#!/bin/bash
# Test audio routing through Linux Equalizer

echo "=== Testing Linux Equalizer Audio Routing ==="
echo ""

# 1. Check if equalizer is running
if ! pgrep -f "linux-equalizer" > /dev/null; then
    echo "❌ Equalizer not running! Starting it..."
    cd "/home/robert-marshall01/Desktop/Linux Equalizer"
    ./target/release/linux-equalizer --headless &
    sleep 3
fi

# 2. Check sinks
echo "Available audio sinks:"
pactl list short sinks | grep -E "(linux_eq|hdmi|analog)" | while read line; do
    echo "  - $line"
done
echo ""

# 3. Check default sink
DEFAULT=$(pactl get-default-sink)
echo "Default sink: $DEFAULT"
echo ""

# 4. Play a test tone through Linux Equalizer
echo "Playing test tone through 'Linux Equalizer' device..."
echo "(You should hear a 440Hz sine wave for 3 seconds)"
echo ""

# Play tone directly to linux_eq_output
paplay -d linux_eq_output /usr/share/sounds/alsa/Front_Center.wav 2>/dev/null &
PAPLAY_PID=$!

sleep 1

# Check if audio is flowing
echo ""
echo "Checking audio flow:"
pactl list short sink-inputs | while read line; do
    echo "  Active stream: $line"
done

wait $PAPLAY_PID

echo ""
echo "=== Test Complete ==="
echo ""
echo "If you heard audio, the equalizer is working!"
echo "If not, check:"
echo "  1. Is parec/pacat running? (ps aux | grep -E 'parec|pacat')"
echo "  2. Is monitor source available? (pactl list sources | grep linux_eq)"
echo "  3. Try: pactl set-sink-volume linux_eq_output 100%"
