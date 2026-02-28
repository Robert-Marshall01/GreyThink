#!/usr/bin/env bash
# ============================================================================
# Grey Math Installer for Linux
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
NC='\033[0m' # No Color

info()  { echo -e "${BLUE}[INFO]${NC}  $*"; }
ok()    { echo -e "${GREEN}[OK]${NC}    $*"; }
warn()  { echo -e "${YELLOW}[WARN]${NC}  $*"; }
error() { echo -e "${RED}[ERROR]${NC} $*"; }

echo ""
echo "  ============================================"
echo "    ${APP_NAME} - Linux Installer v${VERSION}"
echo "  ============================================"
echo ""

# --- Configuration ---
INSTALL_DIR="${HOME}/.local/share/greymath"
VENV_DIR="${INSTALL_DIR}/venv"
BIN_DIR="${HOME}/.local/bin"
DESKTOP_DIR="${HOME}/.local/share/applications"
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
    echo "  Install it using your package manager:"
    echo "    Ubuntu/Debian: sudo apt install python3.11 python3.11-venv"
    echo "    Fedora:        sudo dnf install python3.11"
    echo "    Arch:          sudo pacman -S python"
    exit 1
fi

ok "Found: Python ${PY_VER} (${PYTHON_CMD})"

# --- Check for venv module ---
info "[2/7] Checking for Python venv module ..."
if ! "$PYTHON_CMD" -m venv --help &>/dev/null; then
    error "Python venv module is not available."
    echo "  Install it:"
    echo "    Ubuntu/Debian: sudo apt install python3.11-venv"
    echo "    Fedora:        sudo dnf install python3.11-devel"
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
mkdir -p "$BIN_DIR"

# Main launcher
cat > "${BIN_DIR}/greymath" << LAUNCHER
#!/usr/bin/env bash
source "${VENV_DIR}/bin/activate"
python -m greymath "\$@"
LAUNCHER
chmod +x "${BIN_DIR}/greymath"

# Python-in-env launcher
cat > "${BIN_DIR}/greymath-python" << LAUNCHER
#!/usr/bin/env bash
source "${VENV_DIR}/bin/activate"
python "\$@"
LAUNCHER
chmod +x "${BIN_DIR}/greymath-python"

ok "Launchers created in ${BIN_DIR}"

# --- Create .desktop entry ---
mkdir -p "$DESKTOP_DIR"
cat > "${DESKTOP_DIR}/greymath.desktop" << DESKTOP
[Desktop Entry]
Name=Grey Math
Comment=A Research-Grade Mathematical IDE
Exec=${BIN_DIR}/greymath
Terminal=true
Type=Application
Categories=Science;Math;Education;Development;
StartupNotify=false
DESKTOP
chmod +x "${DESKTOP_DIR}/greymath.desktop"
ok "Desktop entry created."

# --- Create uninstaller ---
info "[7/7] Creating uninstaller ..."
cat > "$UNINSTALLER" << 'UNINSTALL_SCRIPT'
#!/usr/bin/env bash
set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

echo ""
echo "  ============================================"
echo "    Grey Math - Linux Uninstaller"
echo "  ============================================"
echo ""

INSTALL_DIR="${HOME}/.local/share/greymath"
BIN_DIR="${HOME}/.local/bin"
DESKTOP_DIR="${HOME}/.local/share/applications"

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
echo -e "${BLUE}[1/3]${NC} Removing launcher scripts ..."
rm -f "${BIN_DIR}/greymath"
rm -f "${BIN_DIR}/greymath-python"
echo -e "${GREEN}[OK]${NC}    Launchers removed."

echo -e "${BLUE}[2/3]${NC} Removing desktop entry ..."
rm -f "${DESKTOP_DIR}/greymath.desktop"
echo -e "${GREEN}[OK]${NC}    Desktop entry removed."

echo -e "${BLUE}[3/3]${NC} Removing installation directory ..."
rm -rf "$INSTALL_DIR"
echo -e "${GREEN}[OK]${NC}    Installation directory removed."

echo ""
echo "  ============================================"
echo "    Grey Math has been uninstalled."
echo "  ============================================"
echo ""
UNINSTALL_SCRIPT
chmod +x "$UNINSTALLER"
ok "Uninstaller created at ${UNINSTALLER}"

# --- Check PATH ---
if [[ ":$PATH:" != *":${BIN_DIR}:"* ]]; then
    warn "${BIN_DIR} is not in your PATH."
    echo "  Add it to your shell profile:"
    echo "    echo 'export PATH=\"\$HOME/.local/bin:\$PATH\"' >> ~/.bashrc"
    echo "  Then restart your terminal or run: source ~/.bashrc"
fi

echo ""
echo "  ============================================"
echo "    Installation Complete!"
echo "  ============================================"
echo ""
echo "  Install location : ${INSTALL_DIR}"
echo "  Launcher         : ${BIN_DIR}/greymath"
echo "  Python env       : ${BIN_DIR}/greymath-python"
echo "  Desktop entry    : ${DESKTOP_DIR}/greymath.desktop"
echo "  Uninstaller      : ${UNINSTALLER}"
echo ""
echo "  Usage:"
echo "    greymath              - Launch Grey Math"
echo "    greymath-python       - Python with Grey Math packages"
echo ""
