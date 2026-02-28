#!/usr/bin/env bash
# ──────────────────────────────────────────────────────────────────────
# Grey Physics Uninstaller for macOS
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
echo -e "${YELLOW}  ${APP_NAME} Uninstaller (macOS)${NC}"
echo -e "${YELLOW}==========================================${NC}"
echo ""

# ── Locate manifest ───────────────────────────────────────────────────
ALL_USERS=false
SYMLINK=""
APP_BUNDLE=""
BIN_DIR=""

find_manifest() {
    local search_paths=()

    if [[ -n "$INSTALL_DIR" ]]; then
        search_paths+=("${INSTALL_DIR}/install-manifest.json")
    fi

    # Check script's own directory
    search_paths+=("${SCRIPT_DIR}/install-manifest.json")

    # Default macOS locations
    search_paths+=("${HOME}/Library/GreyPhysics/install-manifest.json")
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

    # Parse JSON manifest using Python
    if command -v python3 &>/dev/null; then
        eval "$(python3 -c "
import json, sys
with open('$MANIFEST_PATH') as f:
    m = json.load(f)
print(f'INSTALL_DIR={chr(34)}{m.get(\"install_dir\", \"\")}{chr(34)}')
print(f'ALL_USERS={str(m.get(\"all_users\", False)).lower()}')
print(f'SYMLINK={chr(34)}{m.get(\"symlink\", \"\")}{chr(34)}')
print(f'APP_BUNDLE={chr(34)}{m.get(\"app_bundle\", \"\")}{chr(34)}')
print(f'BIN_DIR={chr(34)}{m.get(\"bin_dir\", \"\")}{chr(34)}')
" 2>/dev/null)" || true
    fi
fi

# Fallback: try to detect install dir
if [[ -z "$INSTALL_DIR" ]]; then
    if [[ -d "${HOME}/Library/GreyPhysics" ]]; then
        INSTALL_DIR="${HOME}/Library/GreyPhysics"
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

# ── Remove .app bundle ────────────────────────────────────────────────
if [[ -z "$APP_BUNDLE" ]]; then
    if $ALL_USERS; then
        APP_BUNDLE="/Applications/Grey Physics.app"
    else
        APP_BUNDLE="${HOME}/Applications/Grey Physics.app"
    fi
fi

if [[ -d "$APP_BUNDLE" ]]; then
    status "Removing application bundle: ${APP_BUNDLE}"
    rm -rf "$APP_BUNDLE"
    success "Application bundle removed."
else
    status "No application bundle found."
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
