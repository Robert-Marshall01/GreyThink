# Grey Math — Installers

Platform-specific installers and uninstallers for Grey Math.

## Requirements

- **Python 3.11+** must be installed on the target system
- **Windows**: No additional requirements
- **Linux**: `python3-venv` package (`sudo apt install python3.11-venv` on Debian/Ubuntu)
- **macOS**: Python via Homebrew (`brew install python@3.11`) or python.org

## Quick Install

### Windows
```cmd
installers\windows\install.bat
```

### Linux
```bash
chmod +x installers/linux/install.sh
./installers/linux/install.sh
```

### macOS
```bash
chmod +x installers/macos/install.sh
./installers/macos/install.sh
```

## Quick Uninstall

### Windows
```cmd
installers\windows\uninstall.bat
```
Or run the uninstaller from: `%LOCALAPPDATA%\GreyMath\uninstall.bat`

### Linux
```bash
chmod +x installers/linux/uninstall.sh
./installers/linux/uninstall.sh
```
Or run: `~/.local/share/greymath/uninstall.sh`

### macOS
```bash
chmod +x installers/macos/uninstall.sh
./installers/macos/uninstall.sh
```
Or run: `~/Library/Application Support/GreyMath/uninstall.sh`

## Building Distributable Packages

To create distributable archives for all platforms:

```bash
chmod +x installers/build.sh
./installers/build.sh all
```

Build a single platform:
```bash
./installers/build.sh windows   # Creates dist/installers/greymath-0.1.0-windows.zip
./installers/build.sh linux     # Creates dist/installers/greymath-0.1.0-linux.tar.gz + .deb
./installers/build.sh macos     # Creates dist/installers/greymath-0.1.0-macos.tar.gz
```

## Installation Locations

| Platform | Install Directory | Launchers |
|----------|-------------------|-----------|
| Windows  | `%LOCALAPPDATA%\GreyMath` | `%LOCALAPPDATA%\GreyMath\bin\` (added to PATH) |
| Linux    | `~/.local/share/greymath` | `~/.local/bin/greymath` |
| macOS    | `~/Library/Application Support/GreyMath` | `/usr/local/bin/greymath` + `~/Applications/Grey Math.app` |

## What Each Installer Does

1. Verifies Python 3.11+ is available
2. Creates an isolated virtual environment
3. Installs Grey Math and all dependencies into the venv
4. Creates launcher scripts (`greymath`, `greymath-python`)
5. Adds platform-appropriate entries (PATH / desktop entry / .app bundle)
6. Creates a local uninstaller for easy removal

## What Each Uninstaller Does

1. Removes launcher scripts and symlinks
2. Removes desktop entries / .app bundle / Start Menu shortcuts
3. Removes the installation directory (including the virtual environment)
4. Cleans up PATH entries (Windows) or Launch Services cache (macOS)
