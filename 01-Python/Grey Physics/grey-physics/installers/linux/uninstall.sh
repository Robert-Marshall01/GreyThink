#!/usr/bin/env bash
# ──────────────────────────────────────────────────────────────────────
# Grey Physics Uninstaller for Linux
# ──────────────────────────────────────────────────────────────────────
# Usage:
#   ./uninstall.sh                         # auto-detect from manifest
#   ./uninstall.sh --install-dir /my/path  # specify install directory
#   sudo ./uninstall.sh                    # for all-users installations
# ──────────────────────────────────────────────────────────────────────
set -euo pipefail

# ── Constants ──────────────────────────────────────────────────────────
APP_NAME="Grey Physics"
APP_ID="grey-physics"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# ── Defaults ───────────────────────────────────────────────────────────
INSTALL_DIR=""

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
        --install-dir)  INSTALL_DIR="$2"; shift 2 ;;
        -h|--help)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --install-dir DIR    Installation directory to remove"
            echo "  -h, --help           Show this help message"
            exit 0
            ;;
        *)
            failure "Unknown option: $1"
            exit 1
            ;;
    esac
done

# ── Pre-flight ─────────────────────────────────────────────────────────
echo ""
echo -e "${YELLOW}==========================================${NC}"
echo -e "${YELLOW}  ${APP_NAME} Uninstaller (Linux)${NC}"
echo -e "${YELLOW}==========================================${NC}"
echo ""

# ── Locate manifest ───────────────────────────────────────────────────
MANIFEST=""
ALL_USERS=false
SYMLINK=""
DESKTOP_FILE=""
BIN_DIR=""

find_manifest() {
    local search_paths=()

    if [[ -n "$INSTALL_DIR" ]]; then
        search_paths+=("${INSTALL_DIR}/install-manifest.json")
    fi

    # Check script's own directory (uninstaller may live in install dir)
    search_paths+=("${SCRIPT_DIR}/install-manifest.json")

    # Default locations
    search_paths+=("${HOME}/.local/share/${APP_ID}/install-manifest.json")
    search_paths+=("/opt/${APP_ID}/install-manifest.json")

    for mp in "${search_paths[@]}"; do
        if [[ -f "$mp" ]]; then
            echo "$mp"
            return 0
        fi
    done
    return 1
}

MANIFEST_PATH=$(find_manifest) || true

if [[ -n "$MANIFEST_PATH" ]]; then
    status "Found manifest: ${MANIFEST_PATH}"

    # Parse JSON manifest (using Python for reliability, since jq may not be installed)
    if command -v python3 &>/dev/null; then
        eval "$(python3 -c "
import json, sys
with open('$MANIFEST_PATH') as f:
    m = json.load(f)
print(f'INSTALL_DIR={chr(34)}{m.get(\"install_dir\", \"\")}{chr(34)}')
print(f'ALL_USERS={str(m.get(\"all_users\", False)).lower()}')
print(f'SYMLINK={chr(34)}{m.get(\"symlink\", \"\")}{chr(34)}')
print(f'DESKTOP_FILE={chr(34)}{m.get(\"desktop_file\", \"\")}{chr(34)}')
print(f'BIN_DIR={chr(34)}{m.get(\"bin_dir\", \"\")}{chr(34)}')
" 2>/dev/null)" || true
    fi
fi

# Fallback: try to detect install dir
if [[ -z "$INSTALL_DIR" ]]; then
    if [[ -d "${HOME}/.local/share/${APP_ID}" ]]; then
        INSTALL_DIR="${HOME}/.local/share/${APP_ID}"
    elif [[ -d "/opt/${APP_ID}" ]]; then
        INSTALL_DIR="/opt/${APP_ID}"
        ALL_USERS=true
    else
        failure "Could not locate Grey Physics installation."
        failure "Please specify --install-dir with the installation path."
        exit 1
    fi
fi

status "Uninstalling from: ${INSTALL_DIR}"

# Check permissions for all-users uninstall
if $ALL_USERS && [[ $EUID -ne 0 ]]; then
    failure "This was an all-users installation. Root privileges are required."
    failure "Please re-run with: sudo $0"
    exit 1
fi

# ── Confirmation ───────────────────────────────────────────────────────
echo ""
read -rp "This will remove ${APP_NAME} from '${INSTALL_DIR}'. Continue? (y/N) " response
case "$response" in
    [yY]|[yY][eE][sS]) ;;
    *)
        status "Uninstallation cancelled."
        exit 0
        ;;
esac

# ── Remove symlink ────────────────────────────────────────────────────
if [[ -z "$SYMLINK" ]]; then
    # Try default locations
    if $ALL_USERS; then
        SYMLINK="/usr/local/bin/grey-physics"
    else
        SYMLINK="${HOME}/.local/bin/grey-physics"
    fi
fi

if [[ -L "$SYMLINK" ]] || [[ -f "$SYMLINK" ]]; then
    status "Removing symlink: ${SYMLINK}"
    rm -f "$SYMLINK"
    success "Symlink removed."
else
    status "No symlink found at ${SYMLINK}."
fi

# ── Remove desktop entry ──────────────────────────────────────────────
if [[ -z "$DESKTOP_FILE" ]]; then
    if $ALL_USERS; then
        DESKTOP_FILE="/usr/share/applications/${APP_ID}.desktop"
    else
        DESKTOP_FILE="${HOME}/.local/share/applications/${APP_ID}.desktop"
    fi
fi

if [[ -f "$DESKTOP_FILE" ]]; then
    status "Removing desktop entry: ${DESKTOP_FILE}"
    rm -f "$DESKTOP_FILE"
    # Update desktop database if available
    DESKTOP_DIR="$(dirname "$DESKTOP_FILE")"
    if command -v update-desktop-database &>/dev/null; then
        update-desktop-database "$DESKTOP_DIR" 2>/dev/null || true
    fi
    success "Desktop entry removed."
else
    status "No desktop entry found."
fi

# ── Remove installation directory ──────────────────────────────────────
if [[ -d "$INSTALL_DIR" ]]; then
    status "Removing installation directory ..."
    # Move out of install dir if we're in it
    if [[ "$PWD" == "$INSTALL_DIR"* ]]; then
        cd /tmp
    fi
    rm -rf "$INSTALL_DIR"
    success "Installation directory removed."
else
    status "Installation directory already removed."
fi

# ── Summary ────────────────────────────────────────────────────────────
echo ""
echo -e "${GREEN}==========================================${NC}"
echo -e "${GREEN}  ${APP_NAME} uninstalled successfully.${NC}"
echo -e "${GREEN}==========================================${NC}"
echo ""
