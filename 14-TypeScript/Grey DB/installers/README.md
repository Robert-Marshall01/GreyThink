# Grey DB — Installers

Platform-specific installers and uninstallers for Grey DB.

## Prerequisites (all platforms)

| Tool            | Minimum Version | Install From                                |
|-----------------|-----------------|---------------------------------------------|
| **Node.js**     | 20+             | https://nodejs.org                          |
| **npm**         | 9+              | Bundled with Node.js                        |
| **Docker**      | Any recent      | https://docker.com                          |
| **Git**         | Any (optional)  | https://git-scm.com                         |

---

## Quick Start

### Linux

```bash
sudo bash installers/install.sh
```

### macOS

```bash
sudo bash installers/install.sh
```

### Windows (PowerShell — Run as Administrator)

```powershell
powershell -ExecutionPolicy Bypass -File installers\install.ps1
```

---

## What the Installers Do

| Step | Linux | macOS | Windows |
|------|-------|-------|---------|
| Check prerequisites | Node.js, npm, Docker | Node.js, npm, Docker | Node.js, npm, Docker |
| Copy files | `/opt/greydb` | `/usr/local/opt/greydb` | `C:\Program Files\GreyDB` |
| Install npm deps | `npm install` | `npm install` | `npm install` |
| Build project | `npm run build` | `npm run build` | `npm run build` |
| Create config | `/etc/greydb/.env` | `/usr/local/etc/greydb/.env` | `%ProgramData%\GreyDB\config\.env` |
| Register CLI | `/usr/local/bin/greydb` | `/usr/local/bin/greydb` | `greydb.cmd` + PATH |
| System service | systemd unit | launchd plist | Registry uninstall entry |
| Log directory | `/var/log/greydb` | `/usr/local/var/log/greydb` | `%ProgramData%\GreyDB\logs` |

---

## Uninstalling

### Linux

```bash
sudo bash /opt/greydb/installers/uninstall-linux.sh
```

### macOS

```bash
sudo bash /usr/local/opt/greydb/installers/uninstall-macos.sh
```

### Windows (PowerShell — Run as Administrator)

```powershell
powershell -ExecutionPolicy Bypass -File "C:\Program Files\GreyDB\installers\uninstall.ps1"
```

Or uninstall via **Settings → Apps → Grey DB**.

---

## What the Uninstallers Do

1. **Stop services** — systemd/launchd/Docker containers
2. **Remove CLI** — symlink or PATH entry
3. **Remove install directory** — all application files
4. **Prompt for data removal** — database volumes, config, logs (optional)
5. **Clean up system entries** — systemd units, launchd plists, registry keys, system users

---

## File Listing

| File | Purpose |
|------|---------|
| `install.sh` | Cross-platform launcher (detects OS, delegates) |
| `uninstall.sh` | Cross-platform uninstall launcher |
| `install-linux.sh` | Linux installer (systemd service) |
| `uninstall-linux.sh` | Linux uninstaller |
| `install-macos.sh` | macOS installer (launchd service) |
| `uninstall-macos.sh` | macOS uninstaller |
| `install.ps1` | Windows installer (PowerShell) |
| `uninstall.ps1` | Windows uninstaller (PowerShell) |
