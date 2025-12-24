# Autostart Setup for Linux Equalizer

## Quick Install

Run the installation script:
```bash
./install-service.sh
```

## What Gets Installed

### Option 1: Systemd Service (Recommended)
- Service file: `~/.config/systemd/user/linux-equalizer.service`
- Starts automatically on login
- Runs in background (headless mode)
- View status: `systemctl --user status linux-equalizer`
- View logs: `journalctl --user -u linux-equalizer -f`

### Option 2: Desktop Autostart
- Desktop entry: `~/.config/autostart/linux-equalizer.desktop`
- Starts when desktop environment loads
- Better for GUI-focused use

## Manual Installation

### Systemd Service:
```bash
# Copy service file
mkdir -p ~/.config/systemd/user
cp linux-equalizer.service ~/.config/systemd/user/

# Edit paths in service file if needed
nano ~/.config/systemd/user/linux-equalizer.service

# Enable and start
systemctl --user daemon-reload
systemctl --user enable linux-equalizer.service
systemctl --user start linux-equalizer.service
```

### Desktop Autostart:
```bash
# Copy desktop file
mkdir -p ~/.config/autostart
cp linux-equalizer.desktop ~/.config/autostart/

# Edit paths if needed
nano ~/.config/autostart/linux-equalizer.desktop
```

## Uninstall

Run:
```bash
./uninstall-service.sh
```

Or manually:
```bash
systemctl --user stop linux-equalizer.service
systemctl --user disable linux-equalizer.service
rm ~/.config/systemd/user/linux-equalizer.service
rm ~/.config/autostart/linux-equalizer.desktop
systemctl --user daemon-reload
```

## Usage After Installation

- **To adjust EQ:** Run `./target/release/linux-equalizer` (opens GUI)
- **Background service:** Runs automatically with last saved settings
- **Settings persist:** Changes in GUI are auto-saved every 2 seconds
