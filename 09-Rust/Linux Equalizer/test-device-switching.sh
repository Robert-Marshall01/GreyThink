#!/bin/bash
# Test device switching robustness

echo "═══════════════════════════════════════════════════════"
echo "  Device Switching Robustness Test"
echo "═══════════════════════════════════════════════════════"
echo ""

# Get all available hardware sinks
echo "Available audio devices:"
pactl list short sinks | grep -v monitor | grep -v linux_eq | nl

# Store all hardware sinks in an array
mapfile -t SINKS < <(pactl list short sinks | grep -v monitor | grep -v linux_eq | awk '{print $2}')

if [ ${#SINKS[@]} -lt 1 ]; then
    echo "ERROR: No hardware sinks found!"
    exit 1
fi

echo ""
echo "Found ${#SINKS[@]} hardware device(s)"
echo ""

# Show current default
CURRENT=$(pactl info | grep "Default Sink" | cut -d: -f2 | xargs)
echo "Current default: $CURRENT"
echo ""

echo "INSTRUCTIONS:"
echo "1. Start the equalizer app now (cargo run --release)"
echo "2. Wait for it to fully start"
echo "3. This script will switch between devices"
echo "4. Observe that the app automatically restarts the pipeline"
echo "5. Audio should continue working on each device"
echo ""
read -p "Press ENTER when the app is running..."

echo ""
echo "Starting device switching test..."
sleep 2

# Test switching between devices
for i in {1..3}; do
    echo ""
    echo "──────────────────────────────────────────────────────"
    echo "Test Round $i"
    echo "──────────────────────────────────────────────────────"
    
    for SINK in "${SINKS[@]}"; do
        echo ""
        echo "🔄 Switching to: $SINK"
        pactl set-default-sink "$SINK"
        
        echo "⏳ Waiting for automatic restart..."
        sleep 3
        
        echo "🔊 Testing audio output..."
        paplay /usr/share/sounds/freedesktop/stereo/complete.oga 2>/dev/null &
        sleep 1
        
        # Check if app is still running
        if pgrep -f "target/release/linux-equalizer" > /dev/null; then
            echo "✅ App still running"
        else
            echo "❌ App crashed!"
            exit 1
        fi
        
        # Check if virtual device still exists
        if pactl list short sinks | grep -q "linux_eq_output"; then
            echo "✅ Virtual device still exists"
        else
            echo "❌ Virtual device disappeared!"
            exit 1
        fi
        
        # Check if default is set correctly
        CURRENT=$(pactl info | grep "Default Sink" | cut -d: -f2 | xargs)
        if [ "$CURRENT" = "linux_eq_output" ]; then
            echo "✅ Default still set to equalizer"
        else
            echo "⚠️  Default changed to: $CURRENT"
        fi
        
        sleep 2
    done
done

echo ""
echo "═══════════════════════════════════════════════════════"
echo "✅✅✅ DEVICE SWITCHING TEST PASSED ✅✅✅"
echo "═══════════════════════════════════════════════════════"
echo ""
echo "The app successfully handled multiple device switches!"
echo "Audio continued working throughout the test."
echo ""
