#!/usr/bin/env bash
# ============================================================================
# Grey Math — Build Distributable Installer Packages
#
# Creates platform-specific distributable archives:
#   - Windows: greymath-0.1.0-windows.zip
#   - Linux:   greymath-0.1.0-linux.tar.gz + .deb package
#   - macOS:   greymath-0.1.0-macos.tar.gz
#
# Usage: bash installers/build.sh [windows|linux|macos|all]
# ============================================================================

set -euo pipefail

VERSION="0.1.0"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
DIST_DIR="${PROJECT_DIR}/dist/installers"

RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

info()  { echo -e "${BLUE}[BUILD]${NC} $*"; }
ok()    { echo -e "${GREEN}[OK]${NC}    $*"; }

mkdir -p "$DIST_DIR"

build_windows() {
    info "Building Windows installer package ..."
    local OUT="${DIST_DIR}/greymath-${VERSION}-windows"
    rm -rf "$OUT" "${OUT}.zip"
    mkdir -p "$OUT"

    # Copy installer scripts
    cp "${SCRIPT_DIR}/windows/install.bat" "$OUT/"
    cp "${SCRIPT_DIR}/windows/uninstall.bat" "$OUT/"

    # Build wheel for bundling
    cd "$PROJECT_DIR"
    pip wheel . --no-deps -w "${OUT}/dist" 2>/dev/null || python -m build --wheel --outdir "${OUT}/dist" 2>/dev/null || {
        info "No build tool available — archive will install from source."
    }

    # Copy source as fallback
    mkdir -p "${OUT}/source"
    cp -r "${PROJECT_DIR}/greymath" "${OUT}/source/"
    cp "${PROJECT_DIR}/pyproject.toml" "${OUT}/source/"
    cp "${PROJECT_DIR}/README.md" "${OUT}/source/" 2>/dev/null || true
    cp "${PROJECT_DIR}/LICENSE" "${OUT}/source/" 2>/dev/null || true

    # Create README for the archive
    cat > "${OUT}/README.txt" << README
Grey Math ${VERSION} — Windows Installer
==========================================

Requirements: Python 3.11+

To Install:
  1. Double-click install.bat
  2. Follow the on-screen instructions

To Uninstall:
  1. Double-click uninstall.bat
  -OR-
  2. Run the uninstaller from: %LOCALAPPDATA%\GreyMath\uninstall.bat
README

    # Create zip
    cd "$DIST_DIR"
    if command -v zip &>/dev/null; then
        zip -r "${OUT}.zip" "$(basename "$OUT")" >/dev/null
    else
        tar -czf "${OUT}.tar.gz" "$(basename "$OUT")"
    fi
    rm -rf "$OUT"
    ok "Windows package: ${OUT}.zip (or .tar.gz)"
}

build_linux() {
    info "Building Linux installer package ..."
    local OUT="${DIST_DIR}/greymath-${VERSION}-linux"
    rm -rf "$OUT" "${OUT}.tar.gz"
    mkdir -p "$OUT"

    # Copy installer scripts
    cp "${SCRIPT_DIR}/linux/install.sh" "$OUT/"
    cp "${SCRIPT_DIR}/linux/uninstall.sh" "$OUT/"
    chmod +x "${OUT}/install.sh" "${OUT}/uninstall.sh"

    # Copy source
    mkdir -p "${OUT}/source"
    cp -r "${PROJECT_DIR}/greymath" "${OUT}/source/"
    cp "${PROJECT_DIR}/pyproject.toml" "${OUT}/source/"
    cp "${PROJECT_DIR}/README.md" "${OUT}/source/" 2>/dev/null || true
    cp "${PROJECT_DIR}/LICENSE" "${OUT}/source/" 2>/dev/null || true

    cat > "${OUT}/README.txt" << README
Grey Math ${VERSION} — Linux Installer
========================================

Requirements: Python 3.11+, python3-venv

To Install:
  chmod +x install.sh
  ./install.sh

To Uninstall:
  chmod +x uninstall.sh
  ./uninstall.sh
  -OR-
  ~/.local/share/greymath/uninstall.sh
README

    cd "$DIST_DIR"
    tar -czf "${OUT}.tar.gz" "$(basename "$OUT")"
    rm -rf "$OUT"
    ok "Linux package: ${OUT}.tar.gz"

    # Build .deb package if dpkg-deb is available
    if command -v dpkg-deb &>/dev/null; then
        info "Building .deb package ..."
        local DEB_DIR="${DIST_DIR}/deb-build"
        rm -rf "$DEB_DIR"
        mkdir -p "${DEB_DIR}/DEBIAN"
        mkdir -p "${DEB_DIR}/opt/greymath"
        mkdir -p "${DEB_DIR}/usr/local/bin"

        cat > "${DEB_DIR}/DEBIAN/control" << CONTROL
Package: greymath
Version: ${VERSION}
Section: science
Priority: optional
Architecture: all
Depends: python3 (>= 3.11), python3-venv
Maintainer: Grey Math Project
Description: A Research-Grade Mathematical IDE
 Grey Math is a modular, extensible, production-grade mathematical
 operating system and IDE designed for constructing, analyzing, and
 extending intelligence systems using advanced mathematics.
CONTROL

        # Post-install script
        cat > "${DEB_DIR}/DEBIAN/postinst" << 'POSTINST'
#!/bin/bash
set -e
cd /opt/greymath
if command -v python3 &>/dev/null; then
    python3 -m venv /opt/greymath/venv
    /opt/greymath/venv/bin/pip install --upgrade pip >/dev/null 2>&1
    /opt/greymath/venv/bin/pip install /opt/greymath/source/ 2>&1
fi
POSTINST
        chmod 755 "${DEB_DIR}/DEBIAN/postinst"

        # Pre-remove script
        cat > "${DEB_DIR}/DEBIAN/prerm" << 'PRERM'
#!/bin/bash
set -e
rm -rf /opt/greymath/venv
PRERM
        chmod 755 "${DEB_DIR}/DEBIAN/prerm"

        # Copy source
        cp -r "${PROJECT_DIR}/greymath" "${DEB_DIR}/opt/greymath/source/"
        cp "${PROJECT_DIR}/pyproject.toml" "${DEB_DIR}/opt/greymath/source/"
        cp "${PROJECT_DIR}/README.md" "${DEB_DIR}/opt/greymath/source/" 2>/dev/null || true
        cp "${PROJECT_DIR}/LICENSE" "${DEB_DIR}/opt/greymath/source/" 2>/dev/null || true
        mkdir -p "${DEB_DIR}/opt/greymath/source/greymath"
        cp -r "${PROJECT_DIR}/greymath/"* "${DEB_DIR}/opt/greymath/source/greymath/"

        # Launcher symlink
        cat > "${DEB_DIR}/usr/local/bin/greymath" << 'LAUNCHER'
#!/usr/bin/env bash
source /opt/greymath/venv/bin/activate
python -m greymath "$@"
LAUNCHER
        chmod 755 "${DEB_DIR}/usr/local/bin/greymath"

        dpkg-deb --build "$DEB_DIR" "${DIST_DIR}/greymath_${VERSION}_all.deb" >/dev/null 2>&1
        rm -rf "$DEB_DIR"
        ok ".deb package: ${DIST_DIR}/greymath_${VERSION}_all.deb"
    fi
}

build_macos() {
    info "Building macOS installer package ..."
    local OUT="${DIST_DIR}/greymath-${VERSION}-macos"
    rm -rf "$OUT" "${OUT}.tar.gz"
    mkdir -p "$OUT"

    # Copy installer scripts
    cp "${SCRIPT_DIR}/macos/install.sh" "$OUT/"
    cp "${SCRIPT_DIR}/macos/uninstall.sh" "$OUT/"
    chmod +x "${OUT}/install.sh" "${OUT}/uninstall.sh"

    # Copy source
    mkdir -p "${OUT}/source"
    cp -r "${PROJECT_DIR}/greymath" "${OUT}/source/"
    cp "${PROJECT_DIR}/pyproject.toml" "${OUT}/source/"
    cp "${PROJECT_DIR}/README.md" "${OUT}/source/" 2>/dev/null || true
    cp "${PROJECT_DIR}/LICENSE" "${OUT}/source/" 2>/dev/null || true

    cat > "${OUT}/README.txt" << README
Grey Math ${VERSION} — macOS Installer
========================================

Requirements: Python 3.11+ (install via Homebrew: brew install python@3.11)

To Install:
  chmod +x install.sh
  ./install.sh

To Uninstall:
  chmod +x uninstall.sh
  ./uninstall.sh
  -OR-
  ~/Library/Application\ Support/GreyMath/uninstall.sh
README

    cd "$DIST_DIR"
    tar -czf "${OUT}.tar.gz" "$(basename "$OUT")"
    rm -rf "$OUT"
    ok "macOS package: ${OUT}.tar.gz"
}

# --- Main ---
TARGET="${1:-all}"

echo ""
echo "  ============================================"
echo "    Grey Math — Build Installers"
echo "  ============================================"
echo ""

case "$TARGET" in
    windows) build_windows ;;
    linux)   build_linux ;;
    macos)   build_macos ;;
    all)
        build_windows
        build_linux
        build_macos
        ;;
    *)
        echo "Usage: $0 [windows|linux|macos|all]"
        exit 1
        ;;
esac

echo ""
echo "  Build output: ${DIST_DIR}/"
ls -lh "${DIST_DIR}/" 2>/dev/null
echo ""
