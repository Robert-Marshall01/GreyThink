# Linux Equalizer Usage Guide

## Background Service (Headless Mode)

The equalizer **IS** running in the background as a systemd service.

### Check Status
```bash
systemctl --user status linux-equalizer
```

### The Service:
- ✅ Runs automatically on login (if enabled)
- ✅ Processes all system audio in the background
- ✅ No GUI window appears
- ✅ Uses saved EQ settings from last GUI session

### Control the Service
```bash
# Stop the background service
systemctl --user stop linux-equalizer

# Start the background service
systemctl --user start linux-equalizer

# Restart (after changes)
systemctl --user restart linux-equalizer

# Disable autostart
systemctl --user disable linux-equalizer

# Enable autostart
systemctl --user enable linux-equalizer
```

## GUI Mode

To adjust EQ settings while the background service is running:

```bash
cd "/home/robert-marshall01/Desktop/Linux Equalizer"
./target/release/linux-equalizer
```

**IMPORTANT**: When you run the GUI while the service is running:
- The GUI will show you the current settings
- Moving sliders updates the background service in real-time
- The GUI and service share the same settings file
- When you close the GUI, settings are saved and the service continues

## Verify It's Working

1. Check the service is running:
   ```bash
   systemctl --user status linux-equalizer
   ```

2. Check audio routing:
   ```bash
   pactl list sinks short
   ```
   You should see `linux_eq_output` in the list.

3. Play audio - it should go through the "Linux Equalizer" device.

## Audio Quality Settings

Current settings (optimized to prevent echo):
- Latency: 20ms (imperceptible delay)
- Buffer: 10ms chunks
- Sample rate: 48kHz
- Format: 32-bit float

## Troubleshooting

### Echo Issue
If you hear echo, check if multiple instances are running:
```bash
ps aux | grep linux-equalizer
```
There should only be ONE process. If you see multiple, stop them all:
```bash
systemctl --user stop linux-equalizer
pkill -f linux-equalizer
```
Then start only the service:
```bash
systemctl --user start linux-equalizer
```

### Not Working After Reboot
Check if the service is enabled:
```bash
systemctl --user is-enabled linux-equalizer
```
If it says "disabled", enable it:
```bash
systemctl --user enable linux-equalizer
```
