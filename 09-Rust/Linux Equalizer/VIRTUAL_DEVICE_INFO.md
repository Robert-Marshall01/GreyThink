# Virtual Audio Device - FxSound-Style System Equalizer

## How It Works

This equalizer now works **just like FxSound** on Windows:

1. **Creates a Virtual Audio Device**: When you launch the app, it creates a new audio device called "Linux Equalizer"
2. **Becomes System Default**: All applications (Spotify, YouTube, Brave, system sounds) automatically output to this device
3. **Real-Time Processing**: Audio is captured, processed through your EQ settings, and sent to your speakers
4. **Seamless Integration**: Apps don't need to be restarted - they'll use the new device immediately

## Architecture

```
Applications → Linux Equalizer (Virtual Device) → EQ Processing → Hardware Speakers
   ↑                                                     ↑
   ↓                                                     ↓
Spotify, YouTube, etc.                           Real-time DSP with your settings
```

## What Changed

### Before (Didn't Work)
- Tried to intercept audio with CPAL (impossible)
- Attempted EasyEffects integration (unreliable)
- PulseAudio routing led to noise

### Now (Works!)
- Creates PulseAudio virtual sink: `linux_eq_output`
- Sets it as system default automatically
- Captures from monitor source
- Processes through 10-band parametric EQ
- Outputs to real hardware

## Usage

1. **Start the App**: `./target/release/linux-equalizer`
   - Virtual device is created automatically
   - GUI opens with your saved settings
   - Audio starts flowing immediately

2. **Adjust Settings**: Move the sliders in the GUI
   - Changes apply in real-time to ALL system audio
   - Settings are saved every 2 seconds

3. **Stop the App**: Press Ctrl+C or close the window
   - Virtual device is removed
   - Normal audio routing is restored

## Technical Details

- **Virtual Sink**: `module-null-sink` with name `linux_eq_output`
- **Sample Rate**: 48kHz (standard for Linux audio)
- **Channels**: Stereo (2 channels)
- **Latency**: ~20ms (imperceptible)
- **DSP**: 10-band parametric EQ with Q=1.0 per band

## Frequencies Covered

- 32 Hz (Sub-bass)
- 64 Hz (Bass)
- 125 Hz (Low-mid)
- 250 Hz (Mid-bass)
- 500 Hz (Mid)
- 1000 Hz (Upper-mid)
- 2000 Hz (Presence)
- 4000 Hz (Clarity)
- 8000 Hz (Brilliance)
- 16000 Hz (Air)

## Verification

Check that the virtual device exists:
```bash
pactl list sinks short | grep linux_eq
```

Check audio routing:
```bash
pactl info | grep "Default Sink"
```

Should show: `linux_eq_output`

## Restoration

If the app crashes, manually restore audio:
```bash
pactl unload-module $(pactl list short modules | grep linux_eq_output | cut -f1)
```
