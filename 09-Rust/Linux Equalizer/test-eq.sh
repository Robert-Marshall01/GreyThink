#!/bin/bash
# Test script to verify EQ is working

echo "=== Testing Linux Equalizer ==="
echo ""
echo "1. Run the equalizer:"
echo "   cargo run --release"
echo ""
echo "2. Move ANY slider (e.g., boost 32Hz to +6dB)"
echo ""
echo "3. Check that the preset file shows the changes:"
echo "   cat ~/.config/easyeffects/output/LinuxEqualizer.json | grep '\"gain\"' | head -3"
echo ""
echo "4. Verify EasyEffects loaded the preset:"
echo "   gsettings get com.github.wwmm.easyeffects last-loaded-output-preset"
echo ""
echo "5. Play audio and verify it sounds different"
echo ""
echo "=== Quick Test ==="
read -p "Press Enter to start test..."

# Start the equalizer in background
timeout 10 cargo run --release &
EQ_PID=$!

sleep 5

echo ""
echo "Checking preset file..."
if [ -f ~/.config/easyeffects/output/LinuxEqualizer.json ]; then
    echo "✓ Preset file exists"
    cat ~/.config/easyeffects/output/LinuxEqualizer.json | grep '"gain"' | head -5
else
    echo "✗ Preset file not created"
fi

echo ""
echo "Checking EasyEffects status..."
ACTIVE=$(gsettings get com.github.wwmm.easyeffects last-loaded-output-preset)
echo "Active preset: $ACTIVE"

if [[ "$ACTIVE" == *"LinuxEqualizer"* ]]; then
    echo "✓ LinuxEqualizer is ACTIVE - system audio is being processed!"
else
    echo "✗ Preset not active - trying to activate..."
    easyeffects -l LinuxEqualizer
    sleep 1
    ACTIVE=$(gsettings get com.github.wwmm.easyeffects last-loaded-output-preset)
    echo "Now active: $ACTIVE"
fi

kill $EQ_PID 2>/dev/null
