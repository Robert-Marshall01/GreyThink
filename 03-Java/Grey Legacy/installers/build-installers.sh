#!/bin/bash
###############################################################################
# build-installers.sh - Build installer packages for all platforms
#
# Builds the Grey Legacy WAR artifact and packages it with the appropriate
# installer scripts for distribution on each target platform.
#
# Usage: ./build-installers.sh [--skip-build] [--output-dir <path>]
#
# Prerequisites: Maven, Java 8+, tar, zip
#
# Maintainer: Grey Legacy Infrastructure Team
###############################################################################
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
OUTPUT_DIR="${PROJECT_ROOT}/dist"
SKIP_BUILD=false
APP_VERSION="1.0.0"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        --skip-build)   SKIP_BUILD=true;    shift ;;
        --output-dir)   OUTPUT_DIR="$2";    shift 2 ;;
        *)              shift ;;
    esac
done

echo "==========================================================="
echo " Grey Legacy - Installer Package Builder"
echo "==========================================================="
echo ""

# ---------------------------------------------------------------------------
#  Step 1: Build WAR
# ---------------------------------------------------------------------------
if [[ "${SKIP_BUILD}" == "false" ]]; then
    echo "[1/4] Building application WAR..."
    cd "${PROJECT_ROOT}"
    mvn clean package -pl web -am -DskipTests -q
    echo "[OK]   WAR built successfully."
else
    echo "[1/4] Skipping Maven build (--skip-build)."
fi

WAR_FILE="${PROJECT_ROOT}/web/target/greylegacy.war"
if [[ ! -f "${WAR_FILE}" ]]; then
    echo "[WARN] WAR file not found at ${WAR_FILE}."
    echo "       Installers will be created without the WAR."
fi

# ---------------------------------------------------------------------------
#  Step 2: Prepare output directory
# ---------------------------------------------------------------------------
echo "[2/4] Preparing output directory..."
mkdir -p "${OUTPUT_DIR}"

# ---------------------------------------------------------------------------
#  Step 3: Package Windows installer
# ---------------------------------------------------------------------------
echo "[3/4] Packaging installers..."

# --- Windows ---
echo "  - Windows installer (.zip)..."
WIN_DIR=$(mktemp -d)
mkdir -p "${WIN_DIR}/greylegacy-${APP_VERSION}-windows"
cp "${SCRIPT_DIR}/windows/install.cmd" "${WIN_DIR}/greylegacy-${APP_VERSION}-windows/"
cp "${SCRIPT_DIR}/windows/uninstall.cmd" "${WIN_DIR}/greylegacy-${APP_VERSION}-windows/"
[[ -f "${WAR_FILE}" ]] && cp "${WAR_FILE}" "${WIN_DIR}/greylegacy-${APP_VERSION}-windows/greylegacy.war"

# Include config files
if [[ -d "${PROJECT_ROOT}/config" ]]; then
    cp -r "${PROJECT_ROOT}/config" "${WIN_DIR}/greylegacy-${APP_VERSION}-windows/config"
fi

cd "${WIN_DIR}"
zip -r "${OUTPUT_DIR}/greylegacy-${APP_VERSION}-windows.zip" "greylegacy-${APP_VERSION}-windows" -q
rm -rf "${WIN_DIR}"
echo "  [OK] ${OUTPUT_DIR}/greylegacy-${APP_VERSION}-windows.zip"

# --- Linux ---
echo "  - Linux installer (.tar.gz)..."
LINUX_DIR=$(mktemp -d)
mkdir -p "${LINUX_DIR}/greylegacy-${APP_VERSION}-linux"
cp "${SCRIPT_DIR}/linux/install.sh" "${LINUX_DIR}/greylegacy-${APP_VERSION}-linux/"
cp "${SCRIPT_DIR}/linux/uninstall.sh" "${LINUX_DIR}/greylegacy-${APP_VERSION}-linux/"
chmod +x "${LINUX_DIR}/greylegacy-${APP_VERSION}-linux/"*.sh
[[ -f "${WAR_FILE}" ]] && cp "${WAR_FILE}" "${LINUX_DIR}/greylegacy-${APP_VERSION}-linux/greylegacy.war"

if [[ -d "${PROJECT_ROOT}/config" ]]; then
    cp -r "${PROJECT_ROOT}/config" "${LINUX_DIR}/greylegacy-${APP_VERSION}-linux/config"
fi

cd "${LINUX_DIR}"
tar czf "${OUTPUT_DIR}/greylegacy-${APP_VERSION}-linux.tar.gz" "greylegacy-${APP_VERSION}-linux"
rm -rf "${LINUX_DIR}"
echo "  [OK] ${OUTPUT_DIR}/greylegacy-${APP_VERSION}-linux.tar.gz"

# --- macOS ---
echo "  - macOS installer (.tar.gz)..."
MAC_DIR=$(mktemp -d)
mkdir -p "${MAC_DIR}/greylegacy-${APP_VERSION}-macos"
cp "${SCRIPT_DIR}/macos/install.sh" "${MAC_DIR}/greylegacy-${APP_VERSION}-macos/"
cp "${SCRIPT_DIR}/macos/uninstall.sh" "${MAC_DIR}/greylegacy-${APP_VERSION}-macos/"
chmod +x "${MAC_DIR}/greylegacy-${APP_VERSION}-macos/"*.sh
[[ -f "${WAR_FILE}" ]] && cp "${WAR_FILE}" "${MAC_DIR}/greylegacy-${APP_VERSION}-macos/greylegacy.war"

if [[ -d "${PROJECT_ROOT}/config" ]]; then
    cp -r "${PROJECT_ROOT}/config" "${MAC_DIR}/greylegacy-${APP_VERSION}-macos/config"
fi

cd "${MAC_DIR}"
tar czf "${OUTPUT_DIR}/greylegacy-${APP_VERSION}-macos.tar.gz" "greylegacy-${APP_VERSION}-macos"
rm -rf "${MAC_DIR}"
echo "  [OK] ${OUTPUT_DIR}/greylegacy-${APP_VERSION}-macos.tar.gz"

# ---------------------------------------------------------------------------
#  Step 4: Summary
# ---------------------------------------------------------------------------
echo ""
echo "[4/4] Build complete!"
echo ""
echo "  Installer packages:"
ls -lh "${OUTPUT_DIR}"/greylegacy-${APP_VERSION}-* 2>/dev/null
echo ""
echo "  Distribution:"
echo "    Windows: greylegacy-${APP_VERSION}-windows.zip"
echo "    Linux:   greylegacy-${APP_VERSION}-linux.tar.gz"
echo "    macOS:   greylegacy-${APP_VERSION}-macos.tar.gz"
echo ""
