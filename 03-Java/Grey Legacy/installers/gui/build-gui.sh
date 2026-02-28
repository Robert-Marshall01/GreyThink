#!/bin/bash
###############################################################################
# build-gui.sh - Compile and package the Grey Legacy GUI JARs
#
# Compiles the Java Swing GUI and packages runnable JARs:
#   - greylegacy-installer.jar  (installer/uninstaller wizard)
#   - greylegacy-app.jar        (desktop dashboard application)
#
# Usage: ./build-gui.sh
# Prerequisites: JDK 8+
###############################################################################
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SRC_DIR="${SCRIPT_DIR}/src"
BUILD_DIR="${SCRIPT_DIR}/build"
JAR_FILE="${SCRIPT_DIR}/greylegacy-installer.jar"
APP_JAR="${SCRIPT_DIR}/greylegacy-app.jar"

echo "==========================================================="
echo " Grey Legacy - GUI Builder"
echo "==========================================================="

# Check for javac
if ! command -v javac &>/dev/null; then
    echo "[ERROR] javac not found. Install a JDK (8+) and ensure it is in PATH."
    exit 1
fi

echo "[1/3] Compiling Java sources..."
rm -rf "${BUILD_DIR}"
mkdir -p "${BUILD_DIR}"

# Collect source files, quoting paths to handle spaces
JAVA_FILES=()
while IFS= read -r -d '' f; do
    JAVA_FILES+=("$f")
done < <(find "${SRC_DIR}" -name "*.java" -print0)

javac -d "${BUILD_DIR}" -source 8 -target 8 "${JAVA_FILES[@]}" 2>&1
echo "[OK]   Compilation successful."

echo "[2/4] Creating JAR manifest (installer)..."
cat > "${BUILD_DIR}/MANIFEST.MF" <<EOF
Manifest-Version: 1.0
Main-Class: com.greylegacy.installer.InstallerGUI
EOF

echo "[3/4] Packaging installer JAR..."
cd "${BUILD_DIR}"
jar cfm "${JAR_FILE}" MANIFEST.MF com/
cd "${SCRIPT_DIR}"

echo "[4/4] Packaging desktop app JAR..."
cat > "${BUILD_DIR}/MANIFEST-APP.MF" <<EOF
Manifest-Version: 1.0
Main-Class: com.greylegacy.app.GreyLegacyApp
EOF

cd "${BUILD_DIR}"
jar cfm "${APP_JAR}" MANIFEST-APP.MF com/
cd "${SCRIPT_DIR}"
rm -rf "${BUILD_DIR}"

echo ""
echo "[OK]   Built: ${JAR_FILE}"
echo "[OK]   Built: ${APP_JAR}"
echo ""
echo "  To run the installer GUI:"
echo "    java -jar ${JAR_FILE}"
echo ""
echo "  To run the uninstaller GUI:"
echo "    java -jar ${JAR_FILE} --uninstall"
echo ""
echo "  To run the desktop app:"
echo "    java -jar ${APP_JAR}"
echo ""
