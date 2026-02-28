#!/usr/bin/env bash
# ──────────────────────────────────────────────────────────────────────
# Grey Physics Installer for macOS
# ──────────────────────────────────────────────────────────────────────
# Usage:
#   ./install.sh                           # user install (~/Library/GreyPhysics)
#   sudo ./install.sh --all-users          # system-wide install (/opt/grey-physics)
#   ./install.sh --install-dir /my/path    # custom directory
#   ./install.sh --extras viz,ide          # install optional extras
# ──────────────────────────────────────────────────────────────────────
set -euo pipefail

# ── Constants ──────────────────────────────────────────────────────────
APP_NAME="Grey Physics"
APP_ID="grey-physics"
PACKAGE_NAME="grey-physics"
MIN_PYTHON_MAJOR=3
MIN_PYTHON_MINOR=10
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# ── Defaults ───────────────────────────────────────────────────────────
INSTALL_DIR=""
ALL_USERS=false
EXTRAS=""

# ── Color helpers ──────────────────────────────────────────────────────
RED='\033[0;31m'
GREEN='\033[0;32m'
CYAN='\033[0;36m'
YELLOW='\033[1;33m'
NC='\033[0m'

status()  { echo -e "${CYAN}[*]${NC} $1"; }
success() { echo -e "${GREEN}[+]${NC} $1"; }
failure() { echo -e "${RED}[-]${NC} $1"; }

# ── Argument parsing ──────────────────────────────────────────────────
while [[ $# -gt 0 ]]; do
    case "$1" in
        --all-users)    ALL_USERS=true; shift ;;
        --install-dir)  INSTALL_DIR="$2"; shift 2 ;;
        --extras)       EXTRAS="$2"; shift 2 ;;
        -h|--help)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --all-users          Install system-wide (requires root)"
            echo "  --install-dir DIR    Custom installation directory"
            echo "  --extras EXTRAS      Comma-separated extras: gpu, viz, ide, all, dev"
            echo "  -h, --help           Show this help message"
            exit 0
            ;;
        *)
            failure "Unknown option: $1"
            exit 1
            ;;
    esac
done

# ── Pre-flight checks ─────────────────────────────────────────────────
echo ""
echo -e "${YELLOW}==========================================${NC}"
echo -e "${YELLOW}  ${APP_NAME} Installer (macOS)${NC}"
echo -e "${YELLOW}==========================================${NC}"
echo ""

# Verify we're on macOS
if [[ "$(uname -s)" != "Darwin" ]]; then
    failure "This installer is for macOS only."
    failure "Please use the appropriate installer for your platform."
    exit 1
fi

# Check root for all-users install
if $ALL_USERS && [[ $EUID -ne 0 ]]; then
    failure "All-users installation requires root privileges."
    failure "Please re-run with: sudo $0 --all-users"
    exit 1
fi

# Resolve install directory
if [[ -z "$INSTALL_DIR" ]]; then
    if $ALL_USERS; then
        INSTALL_DIR="/opt/${APP_ID}"
    else
        INSTALL_DIR="${HOME}/Library/GreyPhysics"
    fi
fi

status "Install directory: ${INSTALL_DIR}"

# ── Find Python ────────────────────────────────────────────────────────
find_python() {
    # On macOS, check Homebrew, framework, and system pythons
    local candidates=(
        "python3"
        "python"
        "/opt/homebrew/bin/python3"
        "/usr/local/bin/python3"
        "/Library/Frameworks/Python.framework/Versions/Current/bin/python3"
    )
    for cmd in "${candidates[@]}"; do
        if command -v "$cmd" &>/dev/null || [[ -x "$cmd" ]]; then
            local version_str
            version_str=$("$cmd" --version 2>&1) || continue
            if [[ "$version_str" =~ Python\ ([0-9]+)\.([0-9]+) ]]; then
                local major="${BASH_REMATCH[1]}"
                local minor="${BASH_REMATCH[2]}"
                if [[ $major -gt $MIN_PYTHON_MAJOR ]] || \
                   [[ $major -eq $MIN_PYTHON_MAJOR && $minor -ge $MIN_PYTHON_MINOR ]]; then
                    if [[ "$cmd" == /* ]]; then
                        echo "$cmd"
                    else
                        command -v "$cmd"
                    fi
                    return 0
                fi
            fi
        fi
    done
    return 1
}

status "Locating Python >= ${MIN_PYTHON_MAJOR}.${MIN_PYTHON_MINOR} ..."
PYTHON_EXE=$(find_python) || {
    failure "Python >= ${MIN_PYTHON_MAJOR}.${MIN_PYTHON_MINOR} not found."
    failure "Please install Python 3.10+:"
    failure "  Homebrew: brew install python@3.12"
    failure "  Official: https://www.python.org/downloads/macos/"
    exit 1
}

PY_VERSION=$("$PYTHON_EXE" --version 2>&1)
success "Found: ${PY_VERSION} at ${PYTHON_EXE}"

# ── Installation ───────────────────────────────────────────────────────

# 1. Create install directory
status "Creating install directory ..."
mkdir -p "$INSTALL_DIR"

# 2. Create virtual environment
VENV_DIR="${INSTALL_DIR}/venv"
if [[ -d "$VENV_DIR" ]]; then
    status "Removing existing virtual environment ..."
    rm -rf "$VENV_DIR"
fi
status "Creating virtual environment ..."
"$PYTHON_EXE" -m venv "$VENV_DIR"
success "Virtual environment created."

VENV_PYTHON="${VENV_DIR}/bin/python"
VENV_PIP="${VENV_DIR}/bin/pip"

# 3. Upgrade pip
status "Upgrading pip ..."
"$VENV_PYTHON" -m pip install --upgrade pip --quiet

# 4. Install the package
PIP_TARGET="$PROJECT_ROOT"
if [[ -n "$EXTRAS" ]]; then
    PIP_TARGET="${PROJECT_ROOT}[${EXTRAS}]"
fi

status "Installing ${PACKAGE_NAME} ..."
"$VENV_PIP" install "$PIP_TARGET" --quiet
if ! "$VENV_PYTHON" -c "import grey_physics" 2>/dev/null; then
    failure "Package installed but import verification failed."
    exit 1
fi
success "${PACKAGE_NAME} installed successfully."

# 5. Create launcher script
LAUNCHER="${INSTALL_DIR}/grey-physics"
cat > "$LAUNCHER" << 'LAUNCHER_EOF'
#!/usr/bin/env bash
# Grey Physics GUI launcher
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
exec "${SCRIPT_DIR}/venv/bin/python" -m grey_physics "$@"
LAUNCHER_EOF
chmod +x "$LAUNCHER"
success "Launcher script created."

# 6. Create symlink in bin directory
if $ALL_USERS; then
    BIN_DIR="/usr/local/bin"
else
    BIN_DIR="${HOME}/.local/bin"
    mkdir -p "$BIN_DIR"
fi

SYMLINK_PATH="${BIN_DIR}/grey-physics"
if [[ -L "$SYMLINK_PATH" ]] || [[ -f "$SYMLINK_PATH" ]]; then
    rm -f "$SYMLINK_PATH"
fi
ln -s "$LAUNCHER" "$SYMLINK_PATH"
success "Symlink created: ${SYMLINK_PATH} -> ${LAUNCHER}"

# 7. Create macOS Application wrapper (optional .app bundle)
if ! $ALL_USERS; then
    APP_DIR="${HOME}/Applications"
else
    APP_DIR="/Applications"
fi
APP_BUNDLE="${APP_DIR}/Grey Physics.app"

status "Creating application bundle ..."
mkdir -p "${APP_BUNDLE}/Contents/MacOS"
mkdir -p "${APP_BUNDLE}/Contents/Resources"

# Info.plist
cat > "${APP_BUNDLE}/Contents/Info.plist" << PLIST_EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleName</key>
    <string>Grey Physics</string>
    <key>CFBundleDisplayName</key>
    <string>Grey Physics</string>
    <key>CFBundleIdentifier</key>
    <string>com.greythink.grey-physics</string>
    <key>CFBundleVersion</key>
    <string>0.1.0</string>
    <key>CFBundleShortVersionString</key>
    <string>0.1.0</string>
    <key>CFBundleExecutable</key>
    <string>grey-physics</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
    <key>LSMinimumSystemVersion</key>
    <string>10.15</string>
    <key>NSHighResolutionCapable</key>
    <true/>
</dict>
</plist>
PLIST_EOF

# Executable wrapper for the .app bundle
cat > "${APP_BUNDLE}/Contents/MacOS/grey-physics" << APP_EOF
#!/usr/bin/env bash
# Grey Physics macOS Application Launcher
exec "${INSTALL_DIR}/venv/bin/python" -m grey_physics "\$@"
APP_EOF
chmod +x "${APP_BUNDLE}/Contents/MacOS/grey-physics"
success "Application bundle created: ${APP_BUNDLE}"

# 8. Write installation manifest
MANIFEST="${INSTALL_DIR}/install-manifest.json"
cat > "$MANIFEST" << MANIFEST_EOF
{
    "app_name": "${APP_NAME}",
    "package_name": "${PACKAGE_NAME}",
    "install_dir": "${INSTALL_DIR}",
    "venv_dir": "${VENV_DIR}",
    "all_users": ${ALL_USERS},
    "bin_dir": "${BIN_DIR}",
    "symlink": "${SYMLINK_PATH}",
    "app_bundle": "${APP_BUNDLE}",
    "installed_at": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
    "python_exe": "${PYTHON_EXE}",
    "platform": "macos"
}
MANIFEST_EOF
success "Installation manifest written."

# 9. Copy uninstaller to install directory
UNINSTALLER_SRC="${SCRIPT_DIR}/uninstall.sh"
if [[ -f "$UNINSTALLER_SRC" ]]; then
    cp "$UNINSTALLER_SRC" "${INSTALL_DIR}/uninstall.sh"
    chmod +x "${INSTALL_DIR}/uninstall.sh"
    success "Uninstaller copied to install directory."
fi

# ── Summary ────────────────────────────────────────────────────────────
echo ""
echo -e "${GREEN}==========================================${NC}"
echo -e "${GREEN}  ${APP_NAME} installed successfully!${NC}"
echo -e "${GREEN}==========================================${NC}"
echo ""
echo "  Install directory : ${INSTALL_DIR}"
echo "  Python            : ${VENV_PYTHON}"
echo "  Launcher          : ${SYMLINK_PATH}"
echo "  Application       : ${APP_BUNDLE}"
echo ""
echo "  Usage:"
echo "    grey-physics --version"
echo ""
echo "  To uninstall, run:"
if $ALL_USERS; then
    echo "    sudo ${INSTALL_DIR}/uninstall.sh"
else
    echo "    ${INSTALL_DIR}/uninstall.sh"
fi
echo ""
if ! $ALL_USERS; then
    if [[ ":$PATH:" != *":${BIN_DIR}:"* ]]; then
        echo -e "  ${YELLOW}NOTE:${NC} ${BIN_DIR} is not on your PATH."
        echo "  Add it by running:"
        echo "    echo 'export PATH=\"\${HOME}/.local/bin:\${PATH}\"' >> ~/.zshrc && source ~/.zshrc"
        echo ""
    fi
fi
