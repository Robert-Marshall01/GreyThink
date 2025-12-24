# Critical Fixes Applied - Deep Code Inspection

## Date: 2025

## ROOT CAUSES IDENTIFIED

### 1. **Default Sink Auto-Assignment (PRIMARY BUG)**
- **Problem**: PipeWire automatically sets newly created sinks as the system default
- **Effect**: ALL system audio was being routed through the equalizer, even when users selected other devices
- **Symptom**: Robotic/echoey sound on ALL devices, not just "Linux Equalizer"
- **Fix**: Immediately restore hardware sink as default after creating virtual device
- **Location**: `/src/audio.rs` lines ~68-74

```rust
// CRITICAL FIX: PipeWire auto-sets new sinks as default!
// Immediately restore the hardware sink as default
let restore_result = Command::new("pactl")
    .args(&["set-default-sink", &hardware_sink])
    .output();
```

### 2. **Biquad Filter Coefficient Sign Error**
- **Problem**: Feedback coefficients (b1, b2) were not negated
- **Effect**: Filter instability, potential phase issues, and audio artifacts
- **Symptom**: Robotic/metallic sound quality
- **Fix**: Added negation to feedback coefficients as per RBJ Audio EQ Cookbook
- **Location**: `/src/equalizer.rs` lines ~87-88

```rust
b1: -a1 / a0, // Feedback coefficients (negated)
b2: -a2 / a0, // Feedback coefficients (negated)
```

## BEFORE vs AFTER

### Before:
```
App (Spotify) → [System] → linux_eq_output (DEFAULT) → EQ → Hardware
App (Firefox) → [System] → linux_eq_output (DEFAULT) → EQ → Hardware
Other Device  → [System] → linux_eq_output (DEFAULT) → EQ → Hardware  ❌ WRONG!
```

### After:
```
App (Spotify) → linux_eq_output (MANUAL) → EQ → Hardware  ✓
App (Firefox) → Hardware (DEFAULT)                         ✓
Other Device  → Hardware (DEFAULT)                         ✓
```

## TESTING INSTRUCTIONS

1. **Stop all instances**:
   ```bash
   systemctl --user stop linux-equalizer.service
   killall -9 parec pacat linux-equalizer 2>/dev/null || true
   ```

2. **Remove old virtual device**:
   ```bash
   pactl list short modules | grep linux_eq_output | cut -f1 | xargs -I {} pactl unload-module {}
   ```

3. **Start the equalizer**:
   ```bash
   ./target/release/linux-equalizer --headless &
   ```

4. **Verify default sink is NOT changed**:
   ```bash
   pactl info | grep "Default Sink"
   # Should show your hardware device, NOT linux_eq_output
   ```

5. **List available sinks**:
   ```bash
   pactl list short sinks
   # You should see both hardware and 'linux_eq_output'
   ```

6. **Manually select "Linux Equalizer" in apps**:
   - Open Sound Settings → Output
   - For apps you want to EQ: Select "Linux Equalizer"
   - Other apps will continue using hardware device

## EXPECTED BEHAVIOR

✓ **Equalizer Device**: Available as "Linux Equalizer" in sound settings
✓ **Default Device**: Remains your hardware sink (NOT changed)
✓ **Device Isolation**: Only apps explicitly set to "Linux Equalizer" are affected
✓ **Other Devices**: Unaffected, no robotic/echo sound
✓ **Audio Quality**: Clean, low-latency, no artifacts

## TECHNICAL DETAILS

### Biquad Filter Math
The peaking EQ uses the RBJ Audio EQ Cookbook formulas:
- Numerator (feedforward): b0, b1, b2
- Denominator (feedback): a0, a1, a2
- Direct Form I implementation with negated feedback coefficients

### Device Routing
- Virtual null sink: `linux_eq_output`
- Monitor source: `linux_eq_output.monitor` (auto-created)
- Audio flow: Monitor → EQ processing → Hardware sink
- Capture: `parec` with ultra-low-latency flags
- Playback: `pacat` with ultra-low-latency flags

### Buffer Configuration
- Sample rate: 48000 Hz
- Format: float32le (32-bit floating point)
- Channels: 2 (stereo)
- Buffer size: 3840 bytes (960 stereo samples, ~10ms latency)

## FILES MODIFIED

1. `/src/audio.rs`: Added immediate default sink restoration
2. `/src/equalizer.rs`: Fixed biquad filter coefficients (negated feedback)

## REMAINING TASKS

- [ ] Test with multiple applications simultaneously
- [ ] Verify no audio artifacts with high gain settings
- [ ] Test startup behavior (systemd service)
- [ ] Document manual app-to-device assignment workflow
- [ ] Consider adding auto-detection of apps to route through EQ
