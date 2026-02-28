#!/usr/bin/env bash
# ─────────────────────────────────────────────────────────────
# Grey DB — Cross-Platform Install Launcher
# Detects the OS and runs the appropriate installer.
# Usage: sudo bash install.sh
# ─────────────────────────────────────────────────────────────

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

case "$(uname -s)" in
    Linux*)
        echo "Detected Linux — launching Linux installer..."
        exec bash "$SCRIPT_DIR/install-linux.sh"
        ;;
    Darwin*)
        echo "Detected macOS — launching macOS installer..."
        exec bash "$SCRIPT_DIR/install-macos.sh"
        ;;
    CYGWIN*|MINGW*|MSYS*)
        echo "Detected Windows (Git Bash / MSYS) — please use PowerShell instead:"
        echo ""
        echo "  Right-click install.ps1 → 'Run with PowerShell'"
        echo "  Or: powershell -ExecutionPolicy Bypass -File \"$SCRIPT_DIR\\install.ps1\""
        echo ""
        exit 1
        ;;
    *)
        echo "Unsupported operating system: $(uname -s)"
        exit 1
        ;;
esac
