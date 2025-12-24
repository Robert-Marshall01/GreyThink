# Linux Equalizer - Quick Start Guide

## What's New: Robust Device Switching! 🎉

The equalizer now **automatically handles device changes** without crashing. Switch between headphones, speakers, USB audio, or any device - the app will automatically detect the change and restart the audio pipeline seamlessly.

## Starting the Equalizer

```bash
cargo run --release
```

Or use the GUI that appears in your applications menu after installation.

## Features

✅ **System-wide audio processing** - affects ALL applications  
✅ **10-band parametric equalizer** - precise frequency control  
✅ **Real-time adjustments** - no lag when moving sliders  
✅ **Automatic device switching** - works with any audio device  
✅ **Preset save/load** - remember your favorite settings  
✅ **Low latency** - 20ms for responsive audio  
✅ **Background mode** - run as a systemd service  

## Device Switching

### What Happens Automatically
When you switch audio devices (e.g., from speakers to headphones):

1. **Detection** (2 seconds): The app detects the device change
2. **Restart** (500ms): The audio pipeline automatically restarts
3. **Resume**: Audio continues on the new device with EQ active

### What You'll See
```
🔔 Device change detected!
   Old device: alsa_output.pci-0000_00_1f.3.analog-stereo
   New device: alsa_output.usb-Device-Name.analog-stereo
   🔄 Requesting automatic restart...

🔄 Restarting audio pipeline for device change...

=== Starting Audio Pipeline ===
📥 Input:  linux_eq_output.monitor (system audio capture)
🎚️  Process: 10-band parametric equalizer
📤 Output: alsa_output.usb-Device-Name.analog-stereo (your speakers/headphones)

✅✅✅ SYSTEM-WIDE EQUALIZER IS NOW ACTIVE ✅✅✅
```

### Error Recovery
The app also automatically recovers from:
- Device unplugged
- Audio driver errors
- Connection failures
- Process crashes

## Testing Device Switching

Run the automated test:

```bash
./test-device-switching.sh
```

This will cycle through all your audio devices and verify the app handles switches correctly.

## Manual Testing

1. Start the app
2. Play some music
3. Switch devices in your system settings (right-click speaker icon → sound settings)
4. Observe the automatic restart message in the terminal
5. Verify audio continues on the new device

## Current Audio Status

Check which device is active:

```bash
pactl info | grep "Default Sink"
```

List all available devices:

```bash
pactl list short sinks | grep -v monitor
```

## Troubleshooting

### App Not Starting?
```bash
# Clean up any existing virtual devices
killall -9 parec pacat
pactl unload-module module-null-sink
pactl list short modules | grep linux_eq

# Then restart
cargo run --release
```

### Audio Issues After Device Switch?
The app should handle this automatically now. If issues persist:
1. Check the terminal output for error messages
2. Verify the device exists: `pactl list short sinks`
3. Try manually switching back and forth between devices

### No Audio at All?
```bash
# Verify virtual device exists
pactl list short sinks | grep linux_eq

# Check if processes are running
ps aux | grep -E "parec|pacat"

# Verify default sink
pactl info | grep "Default Sink"
# Should show: linux_eq_output
```

## Advanced Usage

### Headless Mode (No GUI)
```bash
cargo run --release -- --headless
```

### Install as System Service
```bash
./install-service.sh
systemctl --user start linux-equalizer
systemctl --user enable linux-equalizer  # Auto-start on boot
```

### Stop the Service
```bash
systemctl --user stop linux-equalizer
```

## How It Works

```
Your Apps → Linux_Equalizer (virtual) → EQ Processing → Your Speakers/Headphones
                     ↑
              (System Default)
```

1. **Virtual Sink**: Creates a virtual audio device called "Linux_Equalizer"
2. **Default Routing**: Sets it as the system default (all apps auto-route here)
3. **Processing**: Captures audio, applies EQ, outputs to hardware
4. **Device Monitoring**: Watches for device changes every 2 seconds
5. **Auto-Recovery**: Restarts pipeline when devices change or errors occur

## Performance

- **Latency**: ~20ms (barely noticeable)
- **CPU Usage**: ~2-3% on average systems
- **Memory**: ~50MB
- **Detection Time**: 2 seconds for device changes
- **Restart Time**: ~500ms (brief silence during switch)

## Known Limitations

1. **Detection Delay**: 2-second polling interval for device changes
2. **Brief Gap**: ~500ms silence during automatic restart
3. **Bluetooth**: May have slightly higher latency with Bluetooth devices

## Documentation

- [DEVICE-SWITCHING-FIX.md](DEVICE-SWITCHING-FIX.md) - Technical details on robustness implementation
- [TESTING-GUIDE.md](TESTING-GUIDE.md) - Comprehensive testing procedures
- [README.md](README.md) - Full documentation and architecture

## Support

If you encounter issues:
1. Check terminal output for error messages
2. Run `./test-audio-complete.sh` to diagnose routing
3. Review [DEVICE-SWITCHING-FIX.md](DEVICE-SWITCHING-FIX.md) for technical details
4. Check system audio with `pactl list short sinks`

**Enjoy your system-wide equalizer with robust device switching!** 🎵🎧
