#!/bin/bash
###############################################################################
# uninstall-gui.sh - Grey Legacy GUI Uninstaller Launcher (Linux/macOS)
#
# Launches the graphical uninstaller.
# Run with sudo for full functionality.
###############################################################################
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
JAR_FILE="${SCRIPT_DIR}/gui/greylegacy-installer.jar"

if ! command -v java &>/dev/null; then
    echo "[ERROR] Java not found."
    exit 1
fi

if [[ ! -f "${JAR_FILE}" ]]; then
    echo "[ERROR] GUI jar not found. Run install-gui.sh first to build it."
    exit 1
fi

echo "Launching Grey Legacy Uninstaller..."
java -jar "${JAR_FILE}" --uninstall "$@"
