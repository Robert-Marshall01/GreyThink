# Grey Legacy Installers

Platform-specific installers and uninstallers for the Grey Legacy Claims System.

## Prerequisites

- **Java 8+** (JDK) must be installed on the target machine
- **Apache Tomcat 8.5** — either bundled with the installer or pre-installed
- The application WAR file (`greylegacy.war`), built via `mvn clean package -pl web -am`

## Directory Structure

```
installers/
├── build-installers.sh     # Packages all platform installers for distribution
├── windows/
│   ├── install.cmd          # Windows installer (batch)
│   └── uninstall.cmd        # Windows uninstaller (batch)
├── linux/
│   ├── install.sh           # Linux installer (bash, systemd)
│   └── uninstall.sh         # Linux uninstaller (bash)
└── macos/
    ├── install.sh           # macOS installer (bash, LaunchDaemon)
    └── uninstall.sh         # macOS uninstaller (bash)
```

## Building Installer Packages

From the project root:

```bash
chmod +x installers/build-installers.sh
./installers/build-installers.sh
```

This builds the WAR and creates distributable archives in `dist/`:
- `greylegacy-1.0.0-windows.zip`
- `greylegacy-1.0.0-linux.tar.gz`
- `greylegacy-1.0.0-macos.tar.gz`

Use `--skip-build` to skip the Maven build if you already have a WAR file.

---

## Windows

### Install

Run as **Administrator**:

```cmd
install.cmd
```

Options:
| Flag | Description |
|------|-------------|
| `--silent` | Non-interactive installation |
| `--install-dir <path>` | Custom install directory (default: `C:\GreyLegacy`) |
| `--port <port>` | HTTP port (default: `8080`) |

What the installer does:
1. Verifies Java installation and `JAVA_HOME`
2. Creates directory structure under install directory
3. Extracts/locates Apache Tomcat
4. Deploys the application WAR
5. Copies configuration files and generates `setenv.bat`
6. Registers a Windows service (`GreyLegacyTomcat`) with auto-start and failure recovery
7. Adds a firewall rule for the HTTP port
8. Creates Add/Remove Programs entry for the uninstaller

### Uninstall

Run as **Administrator**:

```cmd
uninstall.cmd
```

Options:
| Flag | Description |
|------|-------------|
| `--silent` | Non-interactive uninstallation |
| `--keep-data` | Preserve the data directory |
| `--keep-logs` | Preserve log files |

---

## Linux

### Install

```bash
sudo ./install.sh
```

Options:
| Flag | Description |
|------|-------------|
| `--silent` | Non-interactive installation |
| `--install-dir <path>` | Custom install directory (default: `/opt/greylegacy`) |
| `--port <port>` | HTTP port (default: `8080`) |
| `--user <user>` | Service user account (default: `greylegacy`) |
| `--skip-firewall` | Skip firewall configuration |

What the installer does:
1. Verifies Java installation
2. Creates a dedicated service user (`greylegacy`)
3. Creates directory structure under install directory
4. Extracts/locates Apache Tomcat
5. Deploys the application WAR
6. Copies configuration files and generates `setenv.sh`
7. Sets file ownership and permissions
8. Creates a systemd service with security hardening and auto-restart
9. Configures firewall (firewalld, ufw, or iptables)

### Uninstall

```bash
sudo ./uninstall.sh
```

Options:
| Flag | Description |
|------|-------------|
| `--silent` | Non-interactive uninstallation |
| `--keep-data` | Preserve the data directory |
| `--keep-logs` | Preserve log files |
| `--keep-user` | Keep the service user account |

---

## macOS

### Install

```bash
sudo ./install.sh
```

Options:
| Flag | Description |
|------|-------------|
| `--silent` | Non-interactive installation |
| `--install-dir <path>` | Custom install directory (default: `/usr/local/greylegacy`) |
| `--port <port>` | HTTP port (default: `8080`) |

What the installer does:
1. Verifies Java installation (uses `/usr/libexec/java_home`)
2. Creates directory structure under install directory
3. Extracts/locates Apache Tomcat
4. Deploys the application WAR
5. Copies configuration files and generates `setenv.sh`
6. Creates a macOS LaunchDaemon (`com.greylegacy.tomcat`) for auto-start
7. Sets file permissions

### Uninstall

```bash
sudo ./uninstall.sh
```

Options:
| Flag | Description |
|------|-------------|
| `--silent` | Non-interactive uninstallation |
| `--keep-data` | Preserve the data directory |
| `--keep-logs` | Preserve log files |

---

## Bundling Tomcat

To create a self-contained installer that includes Tomcat:

- **Windows**: Place `apache-tomcat-8.5.100-windows-x64.zip` in `installers/windows/`
- **Linux**: Place `apache-tomcat-8.5.100.tar.gz` in `installers/linux/`
- **macOS**: Place `apache-tomcat-8.5.100.tar.gz` in `installers/macos/`

If Tomcat is not bundled, the installer checks for an existing installation and provides download instructions if none is found.
