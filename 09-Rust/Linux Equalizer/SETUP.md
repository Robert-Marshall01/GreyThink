# Setting Up System-Wide Equalization

The Linux Equalizer GUI allows you to configure equalization settings. To apply these settings system-wide to all audio on Linux, you need to integrate it with PulseAudio or PipeWire.

## Quick Setup with PulseAudio

### Option 1: Using PulseAudio with LADSPA (Recommended)

1. **Install PulseAudio LADSPA module** (usually pre-installed):
   ```bash
   sudo apt install pulseaudio-module-ladspa swh-plugins
   ```

2. **Install EasyEffects** (modern GUI with EQ):
   ```bash
   # Ubuntu/Debian
   sudo apt install easyeffects
   
   # Fedora
   sudo dnf install easyeffects
   
   # Arch
   sudo pacman -S easyeffects
   ```

3. EasyEffects provides a full 30-band equalizer with PipeWire/PulseAudio integration

### Option 2: Using PipeWire (Modern Linux Audio)

PipeWire is the modern replacement for PulseAudio and has built-in filtering support.

1. **Check if you're using PipeWire**:
   ```bash
   pactl info | grep "Server Name"
   ```
   
   If it shows "PulseAudio (on PipeWire)", you're using PipeWire.

2. **Use EasyEffects** (works with PipeWire):
   ```bash
   flatpak install flathub com.github.wwmm.easyeffects
   flatpak run com.github.wwmm.easyeffects
   ```

### Option 3: Manual PulseAudio Setup with this Equalizer

While this equalizer provides a GUI for configuring EQ settings, applying them system-wide requires additional setup:

1. **Create a null sink for routing**:
   ```bash
   pactl load-module module-null-sink sink_name=eq_sink sink_properties=device.description="Equalizer"
   ```

2. **Set default output**:
   ```bash
   pactl set-default-sink eq_sink
   ```

3. **Load loopback module** (routes audio from eq_sink to your speakers):
   ```bash
   pactl load-module module-loopback source=eq_sink.monitor sink=<your_actual_sink>
   ```

   Find your actual sink with:
   ```bash
   pactl list short sinks
   ```

## Recommended Solution

For most users, we recommend using **EasyEffects** which provides:
- ✅ System-wide audio processing
- ✅ Professional 30-band equalizer
- ✅ Real-time visualization
- ✅ Presets and profiles
- ✅ Additional effects (compressor, limiter, etc.)

This equalizer app is great for learning about DSP and audio processing in Rust, or for building custom audio tools.

## Using This Equalizer's Settings

The equalizer saves settings to `~/.config/linux-equalizer/current.json`. You can use these settings programmatically or reference them when configuring other equalizers.

## Alternative: ALSA Plugin

For lower-level system integration, you could create an ALSA plugin, but this requires more complex setup and is not recommended for most users.

## Future Development

To make this equalizer work system-wide without external tools, it would need to:
1. Implement a PipeWire filter node
2. Or create a PulseAudio sink module
3. Or implement LADSPA plugin interface

Contributions welcome!
