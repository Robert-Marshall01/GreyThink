# Grey Multi-Tenant Platform - Installation Guide

This directory contains cross-platform installers, uninstallers, and launcher scripts for the Grey Multi-Tenant Platform.

## Quick Start

### Windows

```powershell
# Install (Run PowerShell as Administrator)
.\installers\windows\install.ps1

# Uninstall
.\installers\windows\uninstall.ps1

# Clean reinstall (removes everything and installs fresh)
.\installers\windows\clean_install_windows.ps1
```

### Linux

```bash
# Install (requires sudo)
sudo ./installers/linux/install.sh

# User-level install (no sudo required)
./installers/linux/install.sh --user-install

# Uninstall
sudo grey-uninstall
# or
sudo ./installers/linux/uninstall.sh

# Clean reinstall
sudo ./installers/linux/clean_install_linux.sh
```

### macOS

```bash
# Install (requires sudo)
sudo ./installers/macos/install.sh

# User-level install (no sudo required)
./installers/macos/install.sh --user-install

# Uninstall
sudo grey-uninstall
# or
sudo ./installers/macos/uninstall.sh

# Clean reinstall
sudo ./installers/macos/clean_install_macos.sh
```

## Prerequisites

Before installation, ensure you have:

1. **Go 1.22+** - Required to build the application
2. **PostgreSQL 14+** - Database backend (can run via Docker)

### Starting PostgreSQL with Docker

```bash
docker compose up -d postgres
```

## Installation Options

All installers support various options:

| Option | Description |
|--------|-------------|
| `--prefix=/path` | Installation prefix (default: `/usr/local` on Unix, `C:\Program Files` on Windows) |
| `--skip-service` | Skip service registration (for manual control) |
| `--user-install` | Install for current user only (no admin/sudo required) |
| `--keep-data` | Preserve configuration and logs during uninstall |
| `--force` | Skip confirmation prompts (for clean-install scripts) |

### Windows Example

```powershell
.\install_windows.ps1 -InstallDir "D:\Apps\Grey" -SkipService
```

### Linux/macOS Example

```bash
sudo ./install_linux.sh --prefix=/opt --skip-service
./install_linux.sh --user-install  # No sudo needed
```

## File Locations

### Windows (System Install)

| Type | Location |
|------|----------|
| Binary | `C:\Program Files\GreyMultiTenant\bin\grey-core-api.exe` |
| Configuration | `C:\Program Files\GreyMultiTenant\config\.env` |
| Logs | `C:\ProgramData\GreyMultiTenant\logs\` |
| Launcher | `C:\Program Files\GreyMultiTenant\grey-launcher.ps1` |

### Linux (System Install)

| Type | Location |
|------|----------|
| Binary | `/usr/local/bin/grey-core-api` |
| Configuration | `/etc/grey-multitenant/.env` |
| Logs | `/var/log/grey-multitenant/` |
| Launcher | `/usr/local/bin/grey-launcher` |
| Service | `/etc/systemd/system/grey-multitenant.service` |

### Linux (User Install)

| Type | Location |
|------|----------|
| Binary | `~/.local/bin/grey-core-api` |
| Configuration | `~/.config/grey-multitenant/.env` |
| Logs | `~/.local/share/grey-multitenant/logs/` |
| Launcher | `~/.local/bin/grey-launcher` |
| Service | `~/.config/systemd/user/grey-multitenant.service` |

### macOS (System Install)

| Type | Location |
|------|----------|
| App Bundle | `/Applications/GreyMultiTenant.app` |
| Binary | `/usr/local/bin/grey-core-api` (symlink) |
| Configuration | `/etc/grey-multitenant/.env` |
| Logs | `/var/log/grey-multitenant/` |
| Launcher | `/usr/local/bin/grey-launcher` |
| Service | `/Library/LaunchDaemons/com.grey.multitenant.plist` |

### macOS (User Install)

| Type | Location |
|------|----------|
| App Bundle | `~/Applications/GreyMultiTenant.app` |
| Binary | `~/.local/bin/grey-core-api` (symlink) |
| Configuration | `~/.config/grey-multitenant/.env` |
| Logs | `~/Library/Logs/GreyMultiTenant/` |
| Launcher | `~/.local/bin/grey-launcher` |
| Service | `~/Library/LaunchAgents/com.grey.multitenant.plist` |

## Service Management

### Windows

```powershell
# Start/Stop/Restart
Start-Service GreyMultiTenant
Stop-Service GreyMultiTenant
Restart-Service GreyMultiTenant

# Check status
Get-Service GreyMultiTenant

# View logs
Get-Content "C:\ProgramData\GreyMultiTenant\logs\grey-multitenant.log" -Tail 50
```

### Linux (System)

```bash
# Start/Stop/Restart
sudo systemctl start grey-multitenant
sudo systemctl stop grey-multitenant
sudo systemctl restart grey-multitenant

# Check status
sudo systemctl status grey-multitenant

# View logs
sudo journalctl -u grey-multitenant -f
```

### Linux (User)

```bash
systemctl --user start grey-multitenant
systemctl --user stop grey-multitenant
systemctl --user status grey-multitenant
journalctl --user -u grey-multitenant -f
```

### macOS

```bash
# System install
sudo launchctl start com.grey.multitenant
sudo launchctl stop com.grey.multitenant

# User install
launchctl start com.grey.multitenant
launchctl stop com.grey.multitenant

# View logs
tail -f /var/log/grey-multitenant/grey-multitenant.log
```

## Launcher Scripts

The launcher scripts provide a convenient way to run the application manually with prerequisite checking:

- **Windows**: `grey-launcher.ps1` - Checks PostgreSQL, sets environment, runs with visible console
- **Linux**: `grey-launcher` - Checks PostgreSQL, sets environment, runs with logging
- **macOS**: `grey-launcher` - Checks PostgreSQL, sets environment, runs with logging

### Features

- ✅ Auto-detects installation paths (system vs user install)
- ✅ Automatically sets `GREY_CONFIG_PATH` environment variable
- ✅ Checks PostgreSQL availability with 3 retries
- ✅ Logs startup information to timestamped log files
- ✅ Works interactively or non-interactively

## Configuration

After installation, edit the `.env` configuration file:

```bash
# Linux/macOS (system)
sudo nano /etc/grey-multitenant/.env

# Linux/macOS (user)
nano ~/.config/grey-multitenant/.env

# Windows (in elevated PowerShell)
notepad "C:\Program Files\GreyMultiTenant\config\.env"
```

### Required Configuration

```env
DATABASE_URL=postgres://username:password@localhost:5432/grey_multitenant?sslmode=disable
JWT_SECRET=your-secure-secret-here
```

### All Configuration Options

```env
# Database
DATABASE_URL=postgres://grey:grey_password@localhost:5432/grey_multitenant?sslmode=disable

# Server
SERVER_HOST=0.0.0.0
SERVER_PORT=8080
GRPC_PORT=50051

# Authentication
JWT_SECRET=CHANGE_THIS_TO_A_SECURE_SECRET
JWT_ACCESS_TOKEN_EXPIRY=15m
JWT_REFRESH_TOKEN_EXPIRY=168h

# Security
CORS_ALLOWED_ORIGINS=http://localhost:3000

# Logging
LOG_LEVEL=info
ENVIRONMENT=production
```

## Troubleshooting

### Application Won't Start

1. **Check PostgreSQL**:
   ```bash
   # Docker
   docker ps | grep postgres
   
   # Linux systemd
   sudo systemctl status postgresql
   
   # macOS Homebrew
   brew services list | grep postgresql
   ```

2. **Check configuration**:
   ```bash
   cat /etc/grey-multitenant/.env  # Verify DATABASE_URL is correct
   ```

3. **Check logs**:
   - Windows: `C:\ProgramData\GreyMultiTenant\logs\`
   - Linux: `/var/log/grey-multitenant/` or `journalctl -u grey-multitenant`
   - macOS: `/var/log/grey-multitenant/`

### Service Won't Start

1. **Check service status**:
   ```bash
   # Linux
   sudo systemctl status grey-multitenant
   
   # macOS
   sudo launchctl list | grep grey
   ```

2. **Run manually to see errors**:
   ```bash
   grey-launcher
   ```

### Clean Reinstall

If something is broken, perform a clean reinstall:

```bash
# Linux
sudo ./clean_install_linux.sh

# macOS  
sudo ./clean_install_macos.sh

# Windows (as Administrator)
.\clean_install_windows.ps1
```

## GUI Application

The Grey Multi-Tenant GUI provides a graphical interface to manage the platform.

### GUI Installation

The GUI is automatically installed alongside the backend when using the unified installers. 
If you want to install the GUI separately:

```bash
# Linux/macOS (GUI only)
./install.sh --skip-backend

# Windows
.\install.ps1 -SkipBackend
```

### GUI Locations

| OS | Location |
|----|----------|
| Windows | `C:\Program Files\GreyMultiTenantGUI\` |
| Linux | `/opt/grey-multitenant-gui/` (AppImage) |
| macOS | `/Applications/Grey Multi-Tenant GUI.app` |

### Launching the GUI

| OS | Method |
|----|--------|
| Windows | Start Menu > Grey Multi-Tenant > Grey Multi-Tenant GUI |
| Linux | `grey-gui` command or application menu |
| macOS | `grey-gui` command or Spotlight search |

### GUI Data Locations

| OS | Configuration/Data |
|----|-------------------|
| Windows | `%APPDATA%\GreyMultiTenantGUI\` |
| Linux | `~/.config/grey-multitenant-gui/` or `~/.local/share/grey-multitenant-gui/` |
| macOS | `~/Library/Application Support/Grey Multi-Tenant GUI/` |

## Log File Locations

### Startup Logs (from launcher)

| OS | Location |
|----|----------|
| Windows | `C:\ProgramData\GreyMultiTenant\logs\startup-*.log` |
| Linux (System) | `/var/log/grey-multitenant/startup-*.log` |
| Linux (User) | `~/.local/share/grey-multitenant/logs/startup-*.log` |
| macOS (System) | `/var/log/grey-multitenant/startup-*.log` |
| macOS (User) | `~/Library/Logs/GreyMultiTenant/startup-*.log` |

### Service Logs

| OS | Location |
|----|----------|
| Windows | Event Viewer > Applications and Services Logs |
| Linux | `journalctl -u grey-multitenant -f` |
| macOS | `tail -f /var/log/grey-multitenant/grey-multitenant.log` |

## Verifying Services

### Windows

```powershell
# Check if service is running
Get-Service GreyMultiTenant

# Check if API responds
Invoke-RestMethod -Uri "http://localhost:8080/health"
```

### Linux

```bash
# Check service
sudo systemctl status grey-multitenant

# Check API
curl http://localhost:8080/health
```

### macOS

```bash
# Check launchd
sudo launchctl list | grep grey

# Check API
curl http://localhost:8080/health
```

## Architecture

```
installers/
├── windows/
│   ├── install.ps1               # Main installer
│   ├── uninstall.ps1             # Uninstaller
│   ├── clean_install_windows.ps1 # Clean reinstall
│   ├── grey-launcher.ps1         # Self-healing launcher
│   └── grey-service.xml          # WinSW service config
├── linux/
│   ├── install.sh                # Main installer
│   ├── uninstall.sh              # Uninstaller
│   ├── clean_install_linux.sh    # Clean reinstall
│   ├── grey-launcher.sh          # Self-healing launcher
│   ├── grey-multitenant.service  # systemd unit file
│   └── grey-multitenant.desktop  # XDG desktop entry
├── macos/
│   ├── install.sh                # Main installer
│   ├── uninstall.sh              # Uninstaller
│   ├── clean_install_macos.sh    # Clean reinstall
│   ├── grey-launcher.sh          # Self-healing launcher
│   └── com.grey.multitenant.plist # launchd plist
└── icons/
    ├── grey-icon.ico             # Windows icon
    ├── grey-icon.png             # Linux icon
    └── grey-icon.icns            # macOS icon
```

## Security Notes

- The service runs as a dedicated user (`grey` on Linux, `_grey` on macOS) for system installs
- Configuration files have restricted permissions (600) for system installs
- systemd services include security hardening (NoNewPrivileges, ProtectSystem, etc.)
- JWT_SECRET should be changed from the default before production use

