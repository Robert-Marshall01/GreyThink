#!/usr/bin/env bash
# ============================================================================
# Grey Math Installer for macOS
# Version: 0.1.0
# Requires: Python 3.11+
# ============================================================================

set -euo pipefail

APP_NAME="Grey Math"
APP_ID="greymath"
VERSION="0.1.0"
MIN_PYTHON_MAJOR=3
MIN_PYTHON_MINOR=11

# --- Colors ---
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

info()  { echo -e "${BLUE}[INFO]${NC}  $*"; }
ok()    { echo -e "${GREEN}[OK]${NC}    $*"; }
warn()  { echo -e "${YELLOW}[WARN]${NC}  $*"; }
error() { echo -e "${RED}[ERROR]${NC} $*"; }

echo ""
echo "  ============================================"
echo "    ${APP_NAME} - macOS Installer v${VERSION}"
echo "  ============================================"
echo ""

# --- Configuration ---
INSTALL_DIR="${HOME}/Library/Application Support/GreyMath"
VENV_DIR="${INSTALL_DIR}/venv"
BIN_DIR="/usr/local/bin"
APP_DIR="${HOME}/Applications"
UNINSTALLER="${INSTALL_DIR}/uninstall.sh"

# --- Detect Python ---
info "[1/7] Checking for Python ${MIN_PYTHON_MAJOR}.${MIN_PYTHON_MINOR}+ ..."

PYTHON_CMD=""
for cmd in python3 python; do
    if command -v "$cmd" &>/dev/null; then
        PY_VER=$("$cmd" --version 2>&1 | awk '{print $2}')
        PY_MAJOR=$(echo "$PY_VER" | cut -d. -f1)
        PY_MINOR=$(echo "$PY_VER" | cut -d. -f2)
        if [ "$PY_MAJOR" -ge "$MIN_PYTHON_MAJOR" ] && [ "$PY_MINOR" -ge "$MIN_PYTHON_MINOR" ] 2>/dev/null; then
            PYTHON_CMD="$cmd"
            break
        elif [ "$PY_MAJOR" -gt "$MIN_PYTHON_MAJOR" ] 2>/dev/null; then
            PYTHON_CMD="$cmd"
            break
        fi
    fi
done

if [ -z "$PYTHON_CMD" ]; then
    error "Python ${MIN_PYTHON_MAJOR}.${MIN_PYTHON_MINOR}+ is required but was not found."
    echo "  Install it using one of these methods:"
    echo "    Homebrew: brew install python@3.11"
    echo "    Official: https://www.python.org/downloads/macos/"
    exit 1
fi

ok "Found: Python ${PY_VER} (${PYTHON_CMD})"

# --- Check for venv module ---
info "[2/7] Checking for Python venv module ..."
if ! "$PYTHON_CMD" -m venv --help &>/dev/null; then
    error "Python venv module is not available."
    echo "  If using Homebrew: brew reinstall python@3.11"
    exit 1
fi
ok "venv module available."

# --- Create installation directory ---
info "[3/7] Creating installation directory ..."
if [ -d "$INSTALL_DIR" ]; then
    warn "Installation directory already exists: ${INSTALL_DIR}"
    read -rp "         Overwrite existing installation? (y/N): " OVERWRITE
    if [[ ! "$OVERWRITE" =~ ^[Yy]$ ]]; then
        info "Installation cancelled."
        exit 0
    fi
    rm -rf "$INSTALL_DIR"
fi
mkdir -p "$INSTALL_DIR"
ok "${INSTALL_DIR}"

# --- Create virtual environment ---
info "[4/7] Creating Python virtual environment ..."
"$PYTHON_CMD" -m venv "$VENV_DIR"
ok "Virtual environment created."

# --- Install Grey Math ---
info "[5/7] Installing Grey Math ..."
# shellcheck source=/dev/null
source "${VENV_DIR}/bin/activate"

pip install --upgrade pip >/dev/null 2>&1

# Resolve the package directory (relative to this script)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PACKAGE_DIR="$(cd "${SCRIPT_DIR}/../.." && pwd)"

pip install "${PACKAGE_DIR}" 2>&1
ok "Grey Math installed successfully."

deactivate

# --- Create launcher scripts ---
info "[6/7] Creating launcher scripts ..."

# Main launcher
cat > "${INSTALL_DIR}/greymath" << LAUNCHER
#!/usr/bin/env bash
source "${VENV_DIR}/bin/activate"
python -m greymath "\$@"
LAUNCHER
chmod +x "${INSTALL_DIR}/greymath"

# Python-in-env launcher
cat > "${INSTALL_DIR}/greymath-python" << LAUNCHER
#!/usr/bin/env bash
source "${VENV_DIR}/bin/activate"
python "\$@"
LAUNCHER
chmod +x "${INSTALL_DIR}/greymath-python"

# Symlink to /usr/local/bin (may require sudo)
if [ -w "$BIN_DIR" ]; then
    ln -sf "${INSTALL_DIR}/greymath" "${BIN_DIR}/greymath"
    ln -sf "${INSTALL_DIR}/greymath-python" "${BIN_DIR}/greymath-python"
    ok "Launchers symlinked to ${BIN_DIR}"
else
    warn "${BIN_DIR} is not writable."
    echo "  Run these commands to create symlinks (requires admin password):"
    echo "    sudo ln -sf '${INSTALL_DIR}/greymath' '${BIN_DIR}/greymath'"
    echo "    sudo ln -sf '${INSTALL_DIR}/greymath-python' '${BIN_DIR}/greymath-python'"
    echo ""
    read -rp "  Run with sudo now? (y/N): " DO_SUDO
    if [[ "$DO_SUDO" =~ ^[Yy]$ ]]; then
        sudo ln -sf "${INSTALL_DIR}/greymath" "${BIN_DIR}/greymath"
        sudo ln -sf "${INSTALL_DIR}/greymath-python" "${BIN_DIR}/greymath-python"
        ok "Launchers symlinked to ${BIN_DIR}"
    else
        warn "Skipping symlinks. You can run Grey Math directly:"
        echo "    '${INSTALL_DIR}/greymath'"
    fi
fi

# --- Create macOS .app bundle (minimal) ---
info "[7/7] Creating application bundle ..."
mkdir -p "$APP_DIR"
APP_BUNDLE="${APP_DIR}/Grey Math.app"
CONTENTS_DIR="${APP_BUNDLE}/Contents"
MACOS_DIR="${CONTENTS_DIR}/MacOS"

rm -rf "$APP_BUNDLE"
mkdir -p "$MACOS_DIR"

# Info.plist
cat > "${CONTENTS_DIR}/Info.plist" << PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleName</key>
    <string>Grey Math</string>
    <key>CFBundleDisplayName</key>
    <string>Grey Math</string>
    <key>CFBundleIdentifier</key>
    <string>com.greythink.greymath</string>
    <key>CFBundleVersion</key>
    <string>${VERSION}</string>
    <key>CFBundleShortVersionString</key>
    <string>${VERSION}</string>
    <key>CFBundleExecutable</key>
    <string>greymath</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
    <key>LSMinimumSystemVersion</key>
    <string>12.0</string>
</dict>
</plist>
PLIST

# App executable
cat > "${MACOS_DIR}/greymath" << APPSCRIPT
#!/usr/bin/env bash
# Open Terminal and run Grey Math
osascript -e 'tell application "Terminal"
    activate
    do script "source \"${VENV_DIR}/bin/activate\" && python -m greymath"
end tell'
APPSCRIPT
chmod +x "${MACOS_DIR}/greymath"

ok "Application bundle created at ${APP_BUNDLE}"

# --- Create uninstaller ---
cat > "$UNINSTALLER" << 'UNINSTALL_SCRIPT'
#!/usr/bin/env bash
set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

echo ""
echo "  ============================================"
echo "    Grey Math - macOS Uninstaller"
echo "  ============================================"
echo ""

INSTALL_DIR="${HOME}/Library/Application Support/GreyMath"
BIN_DIR="/usr/local/bin"
APP_BUNDLE="${HOME}/Applications/Grey Math.app"

if [ ! -d "$INSTALL_DIR" ]; then
    echo -e "${BLUE}[INFO]${NC}  Grey Math does not appear to be installed."
    exit 0
fi

read -rp "Are you sure you want to uninstall Grey Math? (y/N): " CONFIRM
if [[ ! "$CONFIRM" =~ ^[Yy]$ ]]; then
    echo -e "${BLUE}[INFO]${NC}  Uninstallation cancelled."
    exit 0
fi

echo ""

echo -e "${BLUE}[1/4]${NC} Removing command-line launchers ..."
if [ -w "$BIN_DIR" ]; then
    rm -f "${BIN_DIR}/greymath"
    rm -f "${BIN_DIR}/greymath-python"
else
    sudo rm -f "${BIN_DIR}/greymath" 2>/dev/null || true
    sudo rm -f "${BIN_DIR}/greymath-python" 2>/dev/null || true
fi
echo -e "${GREEN}[OK]${NC}    Launchers removed."

echo -e "${BLUE}[2/4]${NC} Removing application bundle ..."
rm -rf "$APP_BUNDLE"
echo -e "${GREEN}[OK]${NC}    Application bundle removed."

echo -e "${BLUE}[3/4]${NC} Removing installation directory ..."
rm -rf "$INSTALL_DIR"
echo -e "${GREEN}[OK]${NC}    Installation directory removed."

echo -e "${BLUE}[4/4]${NC} Clearing Launch Services cache ..."
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user 2>/dev/null || true
echo -e "${GREEN}[OK]${NC}    Cache cleared."

echo ""
echo "  ============================================"
echo "    Grey Math has been uninstalled."
echo "  ============================================"
echo ""
UNINSTALL_SCRIPT
chmod +x "$UNINSTALLER"
ok "Uninstaller created at ${UNINSTALLER}"

echo ""
echo "  ============================================"
echo "    Installation Complete!"
echo "  ============================================"
echo ""
echo "  Install location : ${INSTALL_DIR}"
echo "  App bundle       : ${APP_BUNDLE}"
echo "  Launcher         : ${BIN_DIR}/greymath"
echo "  Python env       : ${BIN_DIR}/greymath-python"
echo "  Uninstaller      : ${UNINSTALLER}"
echo ""
echo "  Usage:"
echo "    greymath              - Launch Grey Math from terminal"
echo "    greymath-python       - Python with Grey Math packages"
echo "    Open 'Grey Math' from ~/Applications or Launchpad"
echo ""
