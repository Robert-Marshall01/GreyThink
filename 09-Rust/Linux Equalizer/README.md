# Linux Equalizer

A **system-wide audio equalizer** for Linux written in Rust, featuring:
- ✨ Real-time 10-band parametric EQ (via LADSPA)
- 🎯 Zero-latency processing (native PulseAudio/PipeWire integration)
- 🖥️ Modern GTK4 GUI
- 🔊 **System-wide audio** - ALL applications go through the EQ
- 🚀 Background/headless mode
- ⚡ Startup service integration
- 💾 Preset save/load
- 🧹 Automatic cleanup on exit (no orphaned devices)

## v0.3.0 - Major Architecture Overhaul ✅

**What's New:**
- ✅ Uses **LADSPA** (`mbeq`) for rock-solid audio processing
- ✅ **System-wide EQ** - automatically set as default output
- ✅ No more parec/pacat pipeline or custom DSP glitches
- ✅ Proper cleanup on exit - hardware default restored automatically
- ✅ Orphaned device detection and cleanup on startup
- ✅ Reliable device switching without crashes

## ⚠️ Important: Keep the App Running

> **The equalizer only works while the application window is open.**  
> When you close the window, audio routing is restored to normal.

For background operation, use headless mode:
```bash
linux-equalizer --headless
```

## How It Works

```
ALL Audio Sources → "Linux Equalizer" (LADSPA) → 15-Band EQ → Speakers
```

1. Creates a **LADSPA EQ sink** using PulseAudio's `module-ladspa-sink`
2. Sets it as the **system default output** automatically
3. All audio plays through the equalizer
4. When you close the app, it **restores** your original hardware device

## Features

- **10-Band Equalizer**: Adjust frequencies from 32 Hz to 16 kHz
- **Graphical Interface**: Modern GTK4 interface with vertical sliders
- **Preset Management**: Save and load custom equalizer presets
- **Auto-save**: Current settings are automatically saved
- **Biquad Filters**: High-quality peaking EQ filters for each band
- **Extensible**: DSP engine can be integrated with audio pipelines

## Frequency Bands

- 32 Hz, 64 Hz, 125 Hz, 250 Hz, 500 Hz
- 1 kHz, 2 kHz, 4 kHz, 8 kHz, 16 kHz

Each band can be adjusted from -12 dB to +12 dB.

## Installation

### Step 1: Install Dependencies

#### Ubuntu/Debian (22.04+)
```bash
sudo apt update
sudo apt install -y \
    libgtk-4-dev \
    pkg-config \
    build-essential \
    swh-plugins \
    ladspa-sdk
```

#### Fedora
```bash
sudo dnf install -y gtk4-devel pkg-config gcc ladspa-swh-plugins ladspa-sdk
```

#### Arch Linux
```bash
sudo pacman -S gtk4 pkg-config base-devel swh-plugins ladspa
```

### Step 2: Install Rust

If you don't have Rust installed:
```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source ~/.cargo/env
```

### Step 3: Build the Application

```bash
git clone <repository-url>
cd Linux\ Equalizer
cargo build --release
```

### Step 4: Install Desktop Shortcut (Optional)

This adds the app to your Ubuntu application menu:
```bash
./install-desktop.sh
```

After installation, search for **"Linux Equalizer"** in your application menu.

## Dependencies Summary

| Package | Purpose |
|---------|----------|
| `libgtk-4-dev` | GTK4 GUI framework |
| `pkg-config` | Build configuration |
| `build-essential` | Compiler toolchain |
| `swh-plugins` | LADSPA EQ plugins (contains `mbeq`) |
| `ladspa-sdk` | LADSPA plugin tools |

## Quick Start

### Option 1: Run from Terminal
```bash
./target/release/linux-equalizer
```

### Option 2: Run from Application Menu
1. Run `./install-desktop.sh` first
2. Search for **"Linux Equalizer"** in your app menu
3. Click to launch

### Option 3: Headless/Background Mode
```bash
./target/release/linux-equalizer --headless
```
Runs without GUI. Use the GUI app to adjust settings.

## What Happens When You Launch

1. ✅ A virtual audio device "Linux Equalizer" is created
2. ✅ It becomes your system's default output automatically
3. ✅ ALL audio now goes through the 10-band EQ
4. ✅ Adjust sliders in real-time to shape your sound
5. ✅ When you close the app, normal audio is restored

## Running

```bash
cargo run --release
```

Or run the compiled binary:
```bash
./target/release/linux-equalizer
```

## Usage

1. **Launch the GUI**: Run `cargo run --release` or `./target/release/linux-equalizer`
2. **Adjust Bands**: Move the vertical sliders to adjust the gain for each frequency band
3. **Enable/Disable**: Use the switch in the top-right corner to enable or disable the equalizer
4. **Save Preset**: Click "Save Preset" to save your current settings
5. **Load Preset**: Click "Load Preset" to restore saved settings
6. **Reset**: Click "Reset" to set all bands back to 0 dB

## Running as a Background Service

To have the equalizer start automatically on login:

```bash
./install-desktop.sh
# Answer 'y' when asked about autostart
```

Or manually install the systemd service:
```bash
./install-service.sh
```

## Configuration

Presets are saved in `~/.config/linux-equalizer/` as JSON files. The current settings are auto-saved to `current.json`.

## Architecture

- **main.rs**: Application entry point and GTK initialization
- **equalizer.rs**: Core DSP logic with biquad filters
- **gui.rs**: GTK4 user interface implementation
- **audio.rs**: Audio processing functions for buffer manipulation
- **config.rs**: Configuration and preset management

## Technical Details

### Audio Processing

The equalizer uses second-order IIR biquad filters configured as peaking EQ filters. Each frequency band is processed independently, allowing for precise frequency response shaping.

### Filter Implementation

- **Filter Type**: Peaking EQ (Parametric)
- **Order**: 2nd order (12 dB/octave)
- **Quality Factor (Q)**: 1.0 (moderate bandwidth)
- **Gain Range**: -12 dB to +12 dB

## Troubleshooting

### GTK Theme Issues

If the interface doesn't display correctly, try setting:
```bash
export GTK_THEME=Adwaita
```

## License

MIT License - feel free to use and modify as needed.

## Contributing

Contributions are welcome! Please feel free to submit pull requests or open issues.

## Future Enhancements

- [ ] PipeWire filter node integration for system-wide EQ
- [ ] Visualization of frequency spectrum
- [ ] More preset options (Rock, Jazz, Classical, etc.)
- [ ] System tray integration
- [ ] Parametric EQ mode with adjustable Q factor
- [ ] Import/export presets
- [ ] Real-time frequency analyzer
