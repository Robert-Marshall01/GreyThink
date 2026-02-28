# Grey Physics Installers

Platform-specific installers and uninstallers for Grey Physics.

## Quick Start

### Cross-Platform (Recommended)

```bash
# Install on current platform
python installers/build_installers.py install

# Install with optional extras
python installers/build_installers.py install --extras viz,ide

# System-wide install (requires admin/root)
python installers/build_installers.py install --all-users

# Uninstall
python installers/build_installers.py uninstall

# Package installers for distribution
python installers/build_installers.py package
```

### Windows

```batch
REM Double-click or run from Command Prompt:
installers\windows\install.bat

REM Or use PowerShell directly:
powershell -ExecutionPolicy Bypass -File installers\windows\install.ps1

REM All-users install (run as Administrator):
powershell -ExecutionPolicy Bypass -File installers\windows\install.ps1 -AllUsers

REM With optional extras:
powershell -ExecutionPolicy Bypass -File installers\windows\install.ps1 -Extras "viz,ide"

REM To uninstall:
installers\windows\uninstall.bat
```

### Linux

```bash
# User install
bash installers/linux/install.sh

# System-wide install
sudo bash installers/linux/install.sh --all-users

# With optional extras
bash installers/linux/install.sh --extras viz,ide

# Custom directory
bash installers/linux/install.sh --install-dir /my/custom/path

# To uninstall
~/.local/share/grey-physics/uninstall.sh
```

### macOS

```bash
# User install
bash installers/macos/install.sh

# System-wide install
sudo bash installers/macos/install.sh --all-users

# With optional extras
bash installers/macos/install.sh --extras viz,ide

# To uninstall
~/Library/GreyPhysics/uninstall.sh
```

## What the Installers Do

### Install

1. **Verify Python** — checks that Python >= 3.10 is available
2. **Create virtual environment** — isolated from system Python
3. **Install grey-physics** — via pip into the virtual environment
4. **Create launcher scripts** — `grey-physics-python` command
5. **Platform integration**:
   - **Windows**: Start Menu shortcut, PATH entry
   - **Linux**: Desktop entry (.desktop file), PATH symlink
   - **macOS**: Application bundle (.app), PATH symlink
6. **Write manifest** — `install-manifest.json` for the uninstaller
7. **Copy uninstaller** — into the install directory for convenience

### Uninstall

1. **Read manifest** — locates all installed components
2. **Remove PATH entries / symlinks**
3. **Remove shortcuts** — Start Menu (Windows), .desktop (Linux), .app (macOS)
4. **Remove install directory** — virtual environment and all files

## Default Install Locations

| Platform | User Install | All-Users Install |
|----------|-------------|-------------------|
| Windows  | `%LOCALAPPDATA%\GreyPhysics` | `%ProgramFiles%\GreyPhysics` |
| Linux    | `~/.local/share/grey-physics` | `/opt/grey-physics` |
| macOS    | `~/Library/GreyPhysics` | `/opt/grey-physics` |

## Requirements

- **Python 3.10+** must be installed and accessible on PATH
- **Linux**: `python3-venv` package may be needed (`sudo apt install python3-venv`)
- **Windows**: PowerShell 5.1+ (included with Windows 10/11)
- **macOS**: Xcode Command Line Tools or Homebrew Python recommended

## Packaging for Distribution

```bash
python installers/build_installers.py package --output-dir dist/installers
```

This creates:
- `grey-physics-installer-windows.zip`
- `grey-physics-installer-linux.tar.gz`
- `grey-physics-installer-macos.tar.gz`
- `grey-physics-installer-all.zip` (all platforms combined)
