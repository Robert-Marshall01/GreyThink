#!/bin/bash
###############################################################################
# install-gui.sh - Grey Legacy GUI Installer Launcher (Linux/macOS)
#
# Compiles the GUI if the JAR doesn't exist, then launches it.
# For Linux: Run with sudo for full functionality.
# For macOS: Run with sudo for full functionality.
###############################################################################
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
GUI_DIR="${SCRIPT_DIR}/gui"
JAR_FILE="${GUI_DIR}/greylegacy-installer.jar"

# Check Java
if ! command -v java &>/dev/null; then
    echo "[ERROR] Java not found. Install JDK 8+ first."
    exit 1
fi

# Build JAR if not present
if [[ ! -f "${JAR_FILE}" ]]; then
    echo "Building GUI installer..."
    if ! command -v javac &>/dev/null; then
        echo "[ERROR] javac not found. Install a JDK to build the GUI."
        exit 1
    fi
    chmod +x "${GUI_DIR}/build-gui.sh"
    bash "${GUI_DIR}/build-gui.sh"
fi

echo "Launching Grey Legacy Installer..."
java -jar "${JAR_FILE}" "$@"
