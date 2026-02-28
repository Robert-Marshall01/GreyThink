#!/bin/bash
###############################################################################
# install.sh - Grey Legacy Claims System Installer (macOS)
#
# Installs Grey Legacy with Apache Tomcat 8.5 as a macOS LaunchDaemon.
# Handles prerequisite checks, directory setup, WAR deployment,
# LaunchDaemon registration, and environment configuration.
#
# Usage: Run with sudo
#   sudo ./install.sh [OPTIONS]
#
# Options:
#   --silent              Non-interactive installation
#   --install-dir <path>  Installation directory (default: /usr/local/greylegacy)
#   --port <port>         Application HTTP port (default: 8080)
#
# Exit Codes:
#   0 - Installation successful
#   1 - General error
#   2 - Prerequisite check failed
#   3 - File operation failed
#   4 - Service registration failed
#
# Maintainer: Grey Legacy Infrastructure Team
###############################################################################
set -euo pipefail
IFS=$'\n\t'

# ---------------------------------------------------------------------------
#  Default Configuration
# ---------------------------------------------------------------------------
APP_NAME="Grey Legacy Claims System"
APP_SHORT="greylegacy"
APP_VERSION="1.0.0"
SERVICE_LABEL="com.greylegacy.tomcat"
INSTALL_DIR="/usr/local/greylegacy"
TOMCAT_VERSION="8.5.100"
REQUIRED_JAVA_MAJOR=8
APP_PORT=8080
SHUTDOWN_PORT=8005
SILENT=false
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# macOS-specific paths
LAUNCH_DAEMON_DIR="/Library/LaunchDaemons"
PLIST_FILE="${LAUNCH_DAEMON_DIR}/${SERVICE_LABEL}.plist"

# ---------------------------------------------------------------------------
#  Color Codes
# ---------------------------------------------------------------------------
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

log_info()  { echo -e "${BLUE}[INFO]${NC}  $*"; }
log_ok()    { echo -e "${GREEN}[OK]${NC}    $*"; }
log_warn()  { echo -e "${YELLOW}[WARN]${NC}  $*"; }
log_error() { echo -e "${RED}[ERROR]${NC} $*"; }

# ---------------------------------------------------------------------------
#  Parse Arguments
# ---------------------------------------------------------------------------
while [[ $# -gt 0 ]]; do
    case "$1" in
        --silent)       SILENT=true;        shift ;;
        --install-dir)  INSTALL_DIR="$2";   shift 2 ;;
        --port)         APP_PORT="$2";      shift 2 ;;
        *)              shift ;;
    esac
done

# ---------------------------------------------------------------------------
#  Check Root Privileges
# ---------------------------------------------------------------------------
if [[ $EUID -ne 0 ]]; then
    log_error "This installer must be run with sudo."
    exit 1
fi

# ---------------------------------------------------------------------------
#  Check macOS
# ---------------------------------------------------------------------------
if [[ "$(uname)" != "Darwin" ]]; then
    log_error "This installer is for macOS only."
    log_error "Use the Linux installer for Linux systems."
    exit 1
fi

# ---------------------------------------------------------------------------
#  Banner
# ---------------------------------------------------------------------------
echo ""
echo "  ==========================================================="
echo "   ${APP_NAME} - macOS Installer v${APP_VERSION}"
echo "  ==========================================================="
echo ""
echo "   Install Directory : ${INSTALL_DIR}"
echo "   Application Port  : ${APP_PORT}"
echo "   Service Label     : ${SERVICE_LABEL}"
echo ""

if [[ "${SILENT}" == "false" ]]; then
    read -rp "Proceed with installation? [Y/n]: " CONFIRM
    if [[ "${CONFIRM}" == "n" || "${CONFIRM}" == "N" ]]; then
        echo "Installation cancelled by user."
        exit 0
    fi
fi

# ---------------------------------------------------------------------------
#  Step 1: Verify Java Installation
# ---------------------------------------------------------------------------
log_info "[1/7] Checking Java installation..."

if ! command -v java &>/dev/null; then
    log_error "Java is not installed."
    log_error "Install via Homebrew: brew install --cask temurin@8"
    log_error "Or download from: https://adoptium.net/"
    exit 2
fi

JAVA_VER=$(java -version 2>&1 | head -1 | sed 's/.*"\(.*\)".*/\1/')
log_ok "Java found: ${JAVA_VER}"

if [[ -z "${JAVA_HOME:-}" ]]; then
    if /usr/libexec/java_home &>/dev/null; then
        JAVA_HOME=$(/usr/libexec/java_home)
    else
        JAVA_HOME=$(dirname "$(dirname "$(readlink -f "$(command -v java)")")")
    fi
    log_warn "JAVA_HOME not set. Detected: ${JAVA_HOME}"
fi

# ---------------------------------------------------------------------------
#  Step 2: Create Directory Structure
# ---------------------------------------------------------------------------
log_info "[2/7] Creating directory structure..."

TOMCAT_DIR="${INSTALL_DIR}/tomcat"
CONF_DIR="${INSTALL_DIR}/conf"
DATA_DIR="${INSTALL_DIR}/data"
LOG_DIR="${INSTALL_DIR}/logs"
BACKUP_DIR="${INSTALL_DIR}/backups"

for dir in "${INSTALL_DIR}" "${TOMCAT_DIR}" "${CONF_DIR}" "${DATA_DIR}" "${LOG_DIR}" "${BACKUP_DIR}"; do
    mkdir -p "${dir}"
done

log_ok "Directory structure created."

# ---------------------------------------------------------------------------
#  Step 3: Install Apache Tomcat
# ---------------------------------------------------------------------------
log_info "[3/7] Setting up Apache Tomcat ${TOMCAT_VERSION}..."

TOMCAT_ARCHIVE="${SCRIPT_DIR}/apache-tomcat-${TOMCAT_VERSION}.tar.gz"

if [[ -f "${TOMCAT_ARCHIVE}" ]]; then
    log_info "Found bundled Tomcat archive."
    mkdir -p /tmp/greylegacy-tomcat-extract
    tar xzf "${TOMCAT_ARCHIVE}" -C /tmp/greylegacy-tomcat-extract
    cp -a /tmp/greylegacy-tomcat-extract/apache-tomcat-${TOMCAT_VERSION}/* "${TOMCAT_DIR}/"
    rm -rf /tmp/greylegacy-tomcat-extract
    log_ok "Tomcat extracted to ${TOMCAT_DIR}."
elif [[ -f "${TOMCAT_DIR}/bin/catalina.sh" ]]; then
    log_ok "Existing Tomcat found at ${TOMCAT_DIR}."
else
    log_info "Tomcat not found locally. Downloading Apache Tomcat ${TOMCAT_VERSION}..."
    TOMCAT_URL="https://archive.apache.org/dist/tomcat/tomcat-8/v${TOMCAT_VERSION}/bin/apache-tomcat-${TOMCAT_VERSION}.tar.gz"
    TOMCAT_DL="/tmp/apache-tomcat-${TOMCAT_VERSION}.tar.gz"

    if command -v curl &>/dev/null; then
        curl -fSL --progress-bar -o "${TOMCAT_DL}" "${TOMCAT_URL}"
    elif command -v wget &>/dev/null; then
        wget -q --show-progress -O "${TOMCAT_DL}" "${TOMCAT_URL}"
    else
        log_error "Neither curl nor wget found. Cannot download Tomcat."
        log_error "Install curl or place apache-tomcat-${TOMCAT_VERSION}.tar.gz in ${SCRIPT_DIR}/"
        exit 2
    fi

    if [[ ! -f "${TOMCAT_DL}" ]]; then
        log_error "Download failed. Check your internet connection."
        exit 2
    fi

    mkdir -p /tmp/greylegacy-tomcat-extract
    tar xzf "${TOMCAT_DL}" -C /tmp/greylegacy-tomcat-extract
    cp -a /tmp/greylegacy-tomcat-extract/apache-tomcat-${TOMCAT_VERSION}/* "${TOMCAT_DIR}/"
    rm -rf /tmp/greylegacy-tomcat-extract "${TOMCAT_DL}"
    log_ok "Tomcat ${TOMCAT_VERSION} downloaded and installed."
fi

# Ensure scripts are executable
chmod +x "${TOMCAT_DIR}"/bin/*.sh 2>/dev/null || true

# ---------------------------------------------------------------------------
#  Step 4: Deploy Application WAR
# ---------------------------------------------------------------------------
log_info "[4/7] Deploying application..."

WAR_SOURCE="${SCRIPT_DIR}/../../web/target/${APP_SHORT}.war"
ALT_WAR="${SCRIPT_DIR}/${APP_SHORT}.war"
DEPLOY_TARGET="${TOMCAT_DIR}/webapps/${APP_SHORT}.war"

if [[ -f "${WAR_SOURCE}" ]]; then
    cp "${WAR_SOURCE}" "${DEPLOY_TARGET}"
    log_ok "Deployed WAR from build output."
elif [[ -f "${ALT_WAR}" ]]; then
    cp "${ALT_WAR}" "${DEPLOY_TARGET}"
    log_ok "Deployed WAR from installer directory."
else
    log_warn "WAR file not found. Build the project first:"
    log_warn "  mvn clean package -pl web -am"
    log_warn "Continuing installation without WAR deployment..."
fi

# ---------------------------------------------------------------------------
#  Step 5: Configure Application
# ---------------------------------------------------------------------------
log_info "[5/7] Configuring application..."

# Copy configuration files
CONFIG_SOURCE="${SCRIPT_DIR}/../../config"
if [[ -d "${CONFIG_SOURCE}/tomcat" ]]; then
    [[ -f "${CONFIG_SOURCE}/tomcat/server.xml" ]] && \
        cp "${CONFIG_SOURCE}/tomcat/server.xml" "${TOMCAT_DIR}/conf/server.xml"
    [[ -f "${CONFIG_SOURCE}/tomcat/tomcat-users.xml" ]] && \
        cp "${CONFIG_SOURCE}/tomcat/tomcat-users.xml" "${TOMCAT_DIR}/conf/tomcat-users.xml"
fi
[[ -f "${CONFIG_SOURCE}/security/jaas.conf" ]] && \
    cp "${CONFIG_SOURCE}/security/jaas.conf" "${CONF_DIR}/jaas.conf"

# Create environment configuration
cat > "${CONF_DIR}/environment.properties" <<EOF
# Grey Legacy Environment Configuration
# Generated by installer on $(date)
GL_HOME=${INSTALL_DIR}
GL_CONF=${CONF_DIR}
GL_LOG=${LOG_DIR}
GL_DATA=${DATA_DIR}
APP_PORT=${APP_PORT}
SHUTDOWN_PORT=${SHUTDOWN_PORT}
EOF

# Create Tomcat setenv.sh (use heredoc with literal quoting to prevent expansion)
cat > "${TOMCAT_DIR}/bin/setenv.sh" <<'SETENV'
#!/bin/bash
export CATALINA_HOME="TOMCAT_DIR_PLACEHOLDER"
export CATALINA_BASE="TOMCAT_DIR_PLACEHOLDER"
export JAVA_HOME="JAVA_HOME_PLACEHOLDER"

JAVA_OPTS="-server -Xms1024m -Xmx2048m -XX:MaxMetaspaceSize=512m"
JAVA_OPTS="${JAVA_OPTS} -XX:+UseG1GC -XX:MaxGCPauseMillis=200"
JAVA_OPTS="${JAVA_OPTS} -XX:+HeapDumpOnOutOfMemoryError"
JAVA_OPTS="${JAVA_OPTS} -XX:HeapDumpPath=LOG_DIR_PLACEHOLDER/heapdump.hprof"
JAVA_OPTS="${JAVA_OPTS} -Dgl.home=INSTALL_DIR_PLACEHOLDER"
JAVA_OPTS="${JAVA_OPTS} -Dgl.conf=CONF_DIR_PLACEHOLDER"
JAVA_OPTS="${JAVA_OPTS} -Dgl.log=LOG_DIR_PLACEHOLDER"
JAVA_OPTS="${JAVA_OPTS} -Djava.security.auth.login.config=CONF_DIR_PLACEHOLDER/jaas.conf"
export JAVA_OPTS
SETENV

# Replace placeholders (macOS sed requires '' for in-place)
sed -i '' "s|TOMCAT_DIR_PLACEHOLDER|${TOMCAT_DIR}|g" "${TOMCAT_DIR}/bin/setenv.sh"
sed -i '' "s|JAVA_HOME_PLACEHOLDER|${JAVA_HOME}|g" "${TOMCAT_DIR}/bin/setenv.sh"
sed -i '' "s|LOG_DIR_PLACEHOLDER|${LOG_DIR}|g" "${TOMCAT_DIR}/bin/setenv.sh"
sed -i '' "s|INSTALL_DIR_PLACEHOLDER|${INSTALL_DIR}|g" "${TOMCAT_DIR}/bin/setenv.sh"
sed -i '' "s|CONF_DIR_PLACEHOLDER|${CONF_DIR}|g" "${TOMCAT_DIR}/bin/setenv.sh"
chmod +x "${TOMCAT_DIR}/bin/setenv.sh"

log_ok "Application configured."

# ---------------------------------------------------------------------------
#  Step 6: Create LaunchDaemon
# ---------------------------------------------------------------------------
log_info "[6/7] Creating LaunchDaemon..."

cat > "${PLIST_FILE}" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>${SERVICE_LABEL}</string>

    <key>ProgramArguments</key>
    <array>
        <string>${TOMCAT_DIR}/bin/catalina.sh</string>
        <string>run</string>
    </array>

    <key>EnvironmentVariables</key>
    <dict>
        <key>JAVA_HOME</key>
        <string>${JAVA_HOME}</string>
        <key>CATALINA_HOME</key>
        <string>${TOMCAT_DIR}</string>
        <key>CATALINA_BASE</key>
        <string>${TOMCAT_DIR}</string>
    </dict>

    <key>RunAtLoad</key>
    <true/>

    <key>KeepAlive</key>
    <true/>

    <key>StandardOutPath</key>
    <string>${LOG_DIR}/catalina-stdout.log</string>

    <key>StandardErrorPath</key>
    <string>${LOG_DIR}/catalina-stderr.log</string>

    <key>WorkingDirectory</key>
    <string>${INSTALL_DIR}</string>
</dict>
</plist>
EOF

chown root:wheel "${PLIST_FILE}"
chmod 644 "${PLIST_FILE}"

log_ok "LaunchDaemon '${SERVICE_LABEL}' created."

# ---------------------------------------------------------------------------
#  Step 7: Set Permissions
# ---------------------------------------------------------------------------
log_info "[7/7] Setting file permissions..."

# macOS doesn't use a dedicated service user by default; run as root via LaunchDaemon
chown -R root:wheel "${INSTALL_DIR}"
chmod -R 755 "${INSTALL_DIR}"
chmod 750 "${CONF_DIR}"

log_ok "Permissions set."

# ---------------------------------------------------------------------------
#  Save Installation Metadata
# ---------------------------------------------------------------------------
cat > "${INSTALL_DIR}/install.meta" <<EOF
INSTALL_DIR=${INSTALL_DIR}
TOMCAT_DIR=${TOMCAT_DIR}
CONF_DIR=${CONF_DIR}
DATA_DIR=${DATA_DIR}
LOG_DIR=${LOG_DIR}
SERVICE_LABEL=${SERVICE_LABEL}
APP_PORT=${APP_PORT}
APP_VERSION=${APP_VERSION}
INSTALL_DATE=$(date -Iseconds 2>/dev/null || date +%Y-%m-%dT%H:%M:%S)
EOF

# Copy uninstaller
if [[ -f "${SCRIPT_DIR}/uninstall.sh" ]]; then
    cp "${SCRIPT_DIR}/uninstall.sh" "${INSTALL_DIR}/uninstall.sh"
    chmod +x "${INSTALL_DIR}/uninstall.sh"
fi

# ---------------------------------------------------------------------------
#  Desktop Application
# ---------------------------------------------------------------------------
APP_JAR_SRC="${SCRIPT_DIR}/../gui/greylegacy-app.jar"
if [[ -f "${APP_JAR_SRC}" ]]; then
    cp "${APP_JAR_SRC}" "${INSTALL_DIR}/greylegacy-app.jar"
    chmod 644 "${INSTALL_DIR}/greylegacy-app.jar"

    # Create launcher script
    cat > "${INSTALL_DIR}/greylegacy-app.sh" <<'LAUNCHER'
#!/bin/bash
JAR_DIR="$(cd "$(dirname "$0")" && pwd)"
exec java -jar "${JAR_DIR}/greylegacy-app.jar" "$@"
LAUNCHER
    chmod +x "${INSTALL_DIR}/greylegacy-app.sh"

    # Create macOS Application wrapper
    APP_BUNDLE="/Applications/Grey Legacy.app"
    mkdir -p "${APP_BUNDLE}/Contents/MacOS"
    cat > "${APP_BUNDLE}/Contents/MacOS/GreyLegacy" <<APPSCRIPT
#!/bin/bash
exec java -jar "${INSTALL_DIR}/greylegacy-app.jar"
APPSCRIPT
    chmod +x "${APP_BUNDLE}/Contents/MacOS/GreyLegacy"
    cat > "${APP_BUNDLE}/Contents/Info.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleName</key>
    <string>Grey Legacy</string>
    <key>CFBundleExecutable</key>
    <string>GreyLegacy</string>
    <key>CFBundleIdentifier</key>
    <string>com.greylegacy.dashboard</string>
    <key>CFBundleVersion</key>
    <string>${APP_VERSION}</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
</dict>
</plist>
PLIST
    log_ok "Desktop application installed (Applications folder)."
else
    log_warn "Desktop app JAR not found — skipping desktop shortcut."
fi

# ---------------------------------------------------------------------------
#  Installation Complete
# ---------------------------------------------------------------------------
echo ""
echo "  ==========================================================="
echo "   Installation Complete!"
echo "  ==========================================================="
echo ""
echo "   Install Directory : ${INSTALL_DIR}"
echo "   Tomcat Directory  : ${TOMCAT_DIR}"
echo "   Application URL   : http://localhost:${APP_PORT}/${APP_SHORT}/"
echo "   Service Label     : ${SERVICE_LABEL}"
echo ""
echo "   To start the application:"
echo "     sudo launchctl load ${PLIST_FILE}"
echo ""
echo "   To stop the application:"
echo "     sudo launchctl unload ${PLIST_FILE}"
echo ""
echo "   To check status:"
echo "     sudo launchctl list | grep ${SERVICE_LABEL}"
echo ""
echo "   To view logs:"
echo "     tail -f ${LOG_DIR}/catalina-stdout.log"
echo ""
echo "   To uninstall:"
echo "     sudo ${INSTALL_DIR}/uninstall.sh"
echo ""
echo "  ==========================================================="
echo ""

exit 0
