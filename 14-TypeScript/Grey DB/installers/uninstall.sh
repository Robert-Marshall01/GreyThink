#!/usr/bin/env bash
# ─────────────────────────────────────────────────────────────
# Grey DB — Cross-Platform Uninstall Launcher
# Detects the OS and runs the appropriate uninstaller.
# Usage: sudo bash uninstall.sh
# ─────────────────────────────────────────────────────────────

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

case "$(uname -s)" in
    Linux*)
        echo "Detected Linux — launching Linux uninstaller..."
        exec bash "$SCRIPT_DIR/uninstall-linux.sh"
        ;;
    Darwin*)
        echo "Detected macOS — launching macOS uninstaller..."
        exec bash "$SCRIPT_DIR/uninstall-macos.sh"
        ;;
    CYGWIN*|MINGW*|MSYS*)
        echo "Detected Windows (Git Bash / MSYS) — please use PowerShell instead:"
        echo ""
        echo "  Right-click uninstall.ps1 → 'Run with PowerShell'"
        echo "  Or: powershell -ExecutionPolicy Bypass -File \"$SCRIPT_DIR\\uninstall.ps1\""
        echo ""
        exit 1
        ;;
    *)
        echo "Unsupported operating system: $(uname -s)"
        exit 1
        ;;
esac
