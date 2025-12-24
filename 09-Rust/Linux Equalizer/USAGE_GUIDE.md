# How to Use Linux Equalizer (After Critical Fixes)

## Quick Start

### 1. Start the Equalizer
```bash
# Option A: Background mode (no GUI)
./target/release/linux-equalizer --headless &

# Option B: With GUI
./target/release/linux-equalizer

# Option C: As a systemd service (autostart)
systemctl --user enable --now linux-equalizer.service
```

### 2. Verify Setup
```bash
# Check that default sink is your hardware (NOT linux_eq_output)
pactl info | grep "Default Sink"
# Should show: Default Sink: alsa_output.pci-0000_00_1f.3.hdmi-stereo

# List all available sinks
pactl list short sinks
# You should see both hardware and linux_eq_output
```

### 3. Route Apps Through Equalizer

**IMPORTANT**: The equalizer device is NOT set as default. You must manually select it for each app.

#### Method A: System Sound Settings (Per-App Routing)
1. Open Settings → Sound
2. Go to "Applications" tab
3. For each app you want to EQ (Spotify, Firefox, etc.):
   - Find the app in the list
   - Change "Output Device" to **"Linux Equalizer"**

#### Method B: PulseAudio Volume Control (pavucontrol)
```bash
# Install if not already installed
sudo apt install pavucontrol

# Run
pavucontrol
```
1. Go to "Playback" tab
2. Play audio in your app (Spotify, YouTube, etc.)
3. The app will appear in the list
4. Click the device dropdown for that app
5. Select **"Linux Equalizer"**

#### Method C: Command Line (pactl)
```bash
# List running audio streams
pactl list short sink-inputs

# Move a specific stream to the equalizer
# Replace <STREAM_ID> with the number from the list above
pactl move-sink-input <STREAM_ID> linux_eq_output
```

### 4. Adjust EQ Settings

#### GUI Mode
- Open the GUI (if not already running)
- Adjust the 10 frequency bands with sliders
- Settings save automatically to `~/.config/linux-equalizer/presets/current.json`
- Changes apply in real-time

#### Headless Mode
- Settings are loaded from `~/.config/linux-equalizer/presets/current.json`
- To change settings while running headless:
  1. Stop the service: `systemctl --user stop linux-equalizer.service`
  2. Run with GUI: `./target/release/linux-equalizer`
  3. Adjust settings
  4. Restart service: `systemctl --user start linux-equalizer.service`

## Understanding the Audio Flow

```
┌─────────────┐
│  Spotify    │──┐
└─────────────┘  │
                 │    ┌──────────────────┐    ┌─────────────┐    ┌──────────────┐
┌─────────────┐  ├───→│ Linux_Equalizer  │───→│  10-Band EQ │───→│   Hardware   │
│  Firefox    │──┘    │  (Virtual Sink)  │    │  Processing │    │   Speakers   │
└─────────────┘       └──────────────────┘    └─────────────┘    └──────────────┘

┌─────────────┐       ┌──────────────────┐
│  Other Apps │──────→│     Hardware     │ (Direct, no EQ)
└─────────────┘       │     Speakers     │
                      └──────────────────┘
```

- **Apps routed to "Linux Equalizer"**: Audio is processed through the EQ
- **Apps using default device**: Audio goes directly to hardware (NO EQ)

## Troubleshooting

### Problem: No audio from EQ'd apps
**Solution**:
```bash
# Check if the equalizer is running
ps aux | grep linux-equalizer

# Check if audio pipeline is active
ps aux | grep -E "parec|pacat"

# Restart the equalizer
systemctl --user restart linux-equalizer.service
```

### Problem: Robotic/echoey sound
**This should be FIXED now!** If you still experience this:
1. Verify default sink is NOT linux_eq_output:
   ```bash
   pactl info | grep "Default Sink"
   ```
2. If it shows `linux_eq_output`, manually restore:
   ```bash
   pactl set-default-sink alsa_output.pci-0000_00_1f.3.hdmi-stereo
   ```

### Problem: Can't find "Linux Equalizer" in sound settings
**Solution**:
```bash
# Check if virtual device exists
pactl list short sinks | grep linux_eq

# If not found, restart the equalizer
systemctl --user restart linux-equalizer.service
```

### Problem: Changes not applying
**Solution**:
```bash
# Headless mode reloads settings every 5 seconds
# Wait a few seconds after saving

# Or restart to force reload
systemctl --user restart linux-equalizer.service
```

## Advanced Usage

### Restore Normal Audio (Remove Equalizer)
```bash
# Stop the service
systemctl --user stop linux-equalizer.service

# Remove virtual device
pactl list short modules | grep linux_eq_output | cut -f1 | xargs -I {} pactl unload-module {}

# Restore default sink (if needed)
pactl set-default-sink alsa_output.pci-0000_00_1f.3.hdmi-stereo
```

### Check Audio Latency
```bash
# Monitor buffer stats
pactl stat
```

### Debug Mode
```bash
# Run with verbose output
./target/release/linux-equalizer --headless 2>&1 | tee eq-debug.log
```

## Performance Tips

- **Latency**: Current buffer is 10ms (960 samples @ 48kHz) - ultra-low latency
- **CPU Usage**: Biquad filters are very efficient (~1-2% CPU on modern systems)
- **Sample Rate**: Fixed at 48000 Hz for best quality
- **Format**: 32-bit floating point for maximum precision

## Keyboard Shortcuts (GUI Mode)

- **Ctrl+R**: Reset all bands to 0 dB
- **Ctrl+S**: Save current preset
- **Ctrl+Q**: Quit (audio processing continues in background if service is enabled)

## Configuration Files

- **Current Settings**: `~/.config/linux-equalizer/presets/current.json`
- **Systemd Service**: `~/.config/systemd/user/linux-equalizer.service`
- **Autostart Entry**: `~/.config/autostart/linux-equalizer.desktop`

## Uninstall

```bash
# Stop and disable service
systemctl --user stop linux-equalizer.service
systemctl --user disable linux-equalizer.service

# Remove autostart
rm -f ~/.config/autostart/linux-equalizer.desktop
rm -f ~/.config/systemd/user/linux-equalizer.service

# Restore audio
./restore-audio.sh

# Remove files (optional)
rm -rf ~/.config/linux-equalizer
```
