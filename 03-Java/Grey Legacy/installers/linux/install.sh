#!/bin/bash
###############################################################################
# install.sh - Grey Legacy Claims System Installer (Linux)
#
# Installs Grey Legacy as a systemd service with Apache Tomcat 8.5.
# Handles prerequisite checks, directory setup, WAR deployment,
# systemd service registration, firewall rules, and user creation.
#
# Usage: Run as root or with sudo
#   sudo ./install.sh [OPTIONS]
#
# Options:
#   --silent              Non-interactive installation
#   --install-dir <path>  Installation directory (default: /opt/greylegacy)
#   --port <port>         Application HTTP port (default: 8080)
#   --user <user>         Service user (default: greylegacy)
#   --skip-firewall       Skip firewall configuration
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
SERVICE_NAME="greylegacy"
INSTALL_DIR="/opt/greylegacy"
TOMCAT_VERSION="8.5.100"
REQUIRED_JAVA_MAJOR=8
APP_PORT=8080
SHUTDOWN_PORT=8005
APP_USER="greylegacy"
APP_GROUP="greylegacy"
SILENT=false
SKIP_FIREWALL=false
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

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
        --silent)         SILENT=true;          shift ;;
        --install-dir)    INSTALL_DIR="$2";     shift 2 ;;
        --port)           APP_PORT="$2";        shift 2 ;;
        --user)           APP_USER="$2";        shift 2 ;;
        --skip-firewall)  SKIP_FIREWALL=true;   shift ;;
        *)                shift ;;
    esac
done

# ---------------------------------------------------------------------------
#  Check Root Privileges
# ---------------------------------------------------------------------------
if [[ $EUID -ne 0 ]]; then
    log_error "This installer must be run as root (use sudo)."
    exit 1
fi

# ---------------------------------------------------------------------------
#  Banner
# ---------------------------------------------------------------------------
echo ""
echo "  ==========================================================="
echo "   ${APP_NAME} - Linux Installer v${APP_VERSION}"
echo "  ==========================================================="
echo ""
echo "   Install Directory : ${INSTALL_DIR}"
echo "   Application Port  : ${APP_PORT}"
echo "   Service User      : ${APP_USER}"
echo "   Service Name      : ${SERVICE_NAME}"
echo ""

if [[ "${SILENT}" == "false" ]]; then
    read -rp "Proceed with installation? [Y/n]: " CONFIRM
    if [[ "${CONFIRM,,}" == "n" ]]; then
        echo "Installation cancelled by user."
        exit 0
    fi
fi

# ---------------------------------------------------------------------------
#  Step 1: Verify Java Installation
# ---------------------------------------------------------------------------
log_info "[1/9] Checking Java installation..."

if ! command -v java &>/dev/null; then
    log_error "Java is not installed."
    log_error "Install OpenJDK 8+:"
    log_error "  Debian/Ubuntu: sudo apt-get install openjdk-8-jdk"
    log_error "  RHEL/CentOS:   sudo yum install java-1.8.0-openjdk-devel"
    exit 2
fi

JAVA_VER=$(java -version 2>&1 | head -1 | sed 's/.*"\(.*\)".*/\1/')
log_ok "Java found: ${JAVA_VER}"

if [[ -z "${JAVA_HOME:-}" ]]; then
    JAVA_HOME=$(dirname "$(dirname "$(readlink -f "$(command -v java)")")")
    log_warn "JAVA_HOME not set. Detected: ${JAVA_HOME}"
fi

# ---------------------------------------------------------------------------
#  Step 2: Create Service User
# ---------------------------------------------------------------------------
log_info "[2/9] Creating service user..."

if id "${APP_USER}" &>/dev/null; then
    log_ok "User '${APP_USER}' already exists."
else
    groupadd -r "${APP_GROUP}" 2>/dev/null || true
    useradd -r -g "${APP_GROUP}" -d "${INSTALL_DIR}" -s /usr/sbin/nologin \
        -c "${APP_NAME} service account" "${APP_USER}"
    log_ok "Created service user '${APP_USER}'."
fi

# ---------------------------------------------------------------------------
#  Step 3: Create Directory Structure
# ---------------------------------------------------------------------------
log_info "[3/9] Creating directory structure..."

TOMCAT_DIR="${INSTALL_DIR}/tomcat"
CONF_DIR="${INSTALL_DIR}/conf"
DATA_DIR="${INSTALL_DIR}/data"
LOG_DIR="/var/log/greylegacy"
BACKUP_DIR="${INSTALL_DIR}/backups"

for dir in "${INSTALL_DIR}" "${TOMCAT_DIR}" "${CONF_DIR}" "${DATA_DIR}" "${LOG_DIR}" "${BACKUP_DIR}"; do
    mkdir -p "${dir}"
done

log_ok "Directory structure created."

# ---------------------------------------------------------------------------
#  Step 4: Install Apache Tomcat
# ---------------------------------------------------------------------------
log_info "[4/9] Setting up Apache Tomcat ${TOMCAT_VERSION}..."

TOMCAT_ARCHIVE="${SCRIPT_DIR}/apache-tomcat-${TOMCAT_VERSION}.tar.gz"

if [[ -f "${TOMCAT_ARCHIVE}" ]]; then
    log_info "Found bundled Tomcat archive."
    tar xzf "${TOMCAT_ARCHIVE}" -C "${INSTALL_DIR}" --strip-components=1 \
        --transform="s|^[^/]*|tomcat|" 2>/dev/null || \
    (
        mkdir -p /tmp/greylegacy-tomcat-extract
        tar xzf "${TOMCAT_ARCHIVE}" -C /tmp/greylegacy-tomcat-extract
        cp -a /tmp/greylegacy-tomcat-extract/apache-tomcat-${TOMCAT_VERSION}/* "${TOMCAT_DIR}/"
        rm -rf /tmp/greylegacy-tomcat-extract
    )
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
        log_error "Install curl (apt install curl) or place apache-tomcat-${TOMCAT_VERSION}.tar.gz in ${SCRIPT_DIR}/"
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

# Ensure Tomcat scripts are executable
chmod +x "${TOMCAT_DIR}"/bin/*.sh 2>/dev/null || true

# ---------------------------------------------------------------------------
#  Step 5: Deploy Application WAR
# ---------------------------------------------------------------------------
log_info "[5/9] Deploying application..."

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
#  Step 6: Configure Application
# ---------------------------------------------------------------------------
log_info "[6/9] Configuring application..."

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

# Create Tomcat setenv.sh
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

# Replace placeholders in setenv.sh
sed -i "s|TOMCAT_DIR_PLACEHOLDER|${TOMCAT_DIR}|g" "${TOMCAT_DIR}/bin/setenv.sh"
sed -i "s|JAVA_HOME_PLACEHOLDER|${JAVA_HOME}|g" "${TOMCAT_DIR}/bin/setenv.sh"
sed -i "s|LOG_DIR_PLACEHOLDER|${LOG_DIR}|g" "${TOMCAT_DIR}/bin/setenv.sh"
sed -i "s|INSTALL_DIR_PLACEHOLDER|${INSTALL_DIR}|g" "${TOMCAT_DIR}/bin/setenv.sh"
sed -i "s|CONF_DIR_PLACEHOLDER|${CONF_DIR}|g" "${TOMCAT_DIR}/bin/setenv.sh"
chmod +x "${TOMCAT_DIR}/bin/setenv.sh"

log_ok "Application configured."

# ---------------------------------------------------------------------------
#  Step 7: Set Permissions
# ---------------------------------------------------------------------------
log_info "[7/9] Setting file permissions..."

chown -R "${APP_USER}:${APP_GROUP}" "${INSTALL_DIR}"
chown -R "${APP_USER}:${APP_GROUP}" "${LOG_DIR}"
chmod 750 "${INSTALL_DIR}"
chmod 750 "${CONF_DIR}"
chmod 755 "${LOG_DIR}"

log_ok "Permissions set."

# ---------------------------------------------------------------------------
#  Step 8: Create Systemd Service
# ---------------------------------------------------------------------------
log_info "[8/9] Creating systemd service..."

cat > /etc/systemd/system/${SERVICE_NAME}.service <<EOF
[Unit]
Description=${APP_NAME}
Documentation=https://github.com/greylegacy/grey-legacy
After=network.target postgresql.service
Wants=postgresql.service

[Service]
Type=forking
User=${APP_USER}
Group=${APP_GROUP}
Environment="JAVA_HOME=${JAVA_HOME}"
Environment="CATALINA_HOME=${TOMCAT_DIR}"
Environment="CATALINA_BASE=${TOMCAT_DIR}"
Environment="CATALINA_PID=${TOMCAT_DIR}/temp/tomcat.pid"

ExecStart=${TOMCAT_DIR}/bin/startup.sh
ExecStop=${TOMCAT_DIR}/bin/shutdown.sh 30 -force
PIDFile=${TOMCAT_DIR}/temp/tomcat.pid

# Restart policy
Restart=on-failure
RestartSec=10
StartLimitIntervalSec=60
StartLimitBurst=3

# Security hardening
NoNewPrivileges=true
ProtectSystem=strict
ProtectHome=true
ReadWritePaths=${INSTALL_DIR} ${LOG_DIR} /tmp
PrivateTmp=true

# Resource limits
LimitNOFILE=65535
LimitNPROC=4096

[Install]
WantedBy=multi-user.target
EOF

systemctl daemon-reload
systemctl enable "${SERVICE_NAME}" 2>/dev/null
log_ok "Systemd service '${SERVICE_NAME}' created and enabled."

# ---------------------------------------------------------------------------
#  Step 9: Configure Firewall
# ---------------------------------------------------------------------------
log_info "[9/9] Configuring firewall..."

if [[ "${SKIP_FIREWALL}" == "true" ]]; then
    log_info "Firewall configuration skipped (--skip-firewall)."
else
    if command -v firewall-cmd &>/dev/null; then
        # firewalld (RHEL/CentOS/Fedora)
        firewall-cmd --permanent --add-port="${APP_PORT}/tcp" 2>/dev/null && \
        firewall-cmd --reload 2>/dev/null && \
        log_ok "firewalld: Port ${APP_PORT} opened." || \
        log_warn "firewalld configuration failed. Configure manually."
    elif command -v ufw &>/dev/null; then
        # ufw (Ubuntu/Debian)
        ufw allow "${APP_PORT}/tcp" comment "Grey Legacy HTTP" 2>/dev/null && \
        log_ok "ufw: Port ${APP_PORT} opened." || \
        log_warn "ufw configuration failed. Configure manually."
    elif command -v iptables &>/dev/null; then
        # iptables fallback
        iptables -I INPUT -p tcp --dport "${APP_PORT}" -j ACCEPT 2>/dev/null && \
        log_ok "iptables: Port ${APP_PORT} opened." || \
        log_warn "iptables configuration failed. Configure manually."
    else
        log_warn "No firewall tool found. Configure port ${APP_PORT} manually."
    fi
fi

# ---------------------------------------------------------------------------
#  Save Installation Metadata
# ---------------------------------------------------------------------------
cat > "${INSTALL_DIR}/install.meta" <<EOF
INSTALL_DIR=${INSTALL_DIR}
TOMCAT_DIR=${TOMCAT_DIR}
CONF_DIR=${CONF_DIR}
DATA_DIR=${DATA_DIR}
LOG_DIR=${LOG_DIR}
SERVICE_NAME=${SERVICE_NAME}
APP_USER=${APP_USER}
APP_GROUP=${APP_GROUP}
APP_PORT=${APP_PORT}
APP_VERSION=${APP_VERSION}
INSTALL_DATE=$(date -Iseconds)
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

    # Create desktop shortcut
    cat > /usr/share/applications/greylegacy.desktop <<DESKTOP
[Desktop Entry]
Name=Grey Legacy Claims System
Comment=Dashboard for Grey Legacy Claims System
Exec=java -jar ${INSTALL_DIR}/greylegacy-app.jar
Icon=utilities-system-monitor
Terminal=false
Type=Application
Categories=Office;Java;
DESKTOP
    chmod 644 /usr/share/applications/greylegacy.desktop
    log_ok "Desktop application installed."
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
echo "   Service Name      : ${SERVICE_NAME}"
echo "   Service User      : ${APP_USER}"
echo ""
echo "   To start the application:"
echo "     sudo systemctl start ${SERVICE_NAME}"
echo ""
echo "   To check status:"
echo "     sudo systemctl status ${SERVICE_NAME}"
echo ""
echo "   To view logs:"
echo "     journalctl -u ${SERVICE_NAME} -f"
echo "     tail -f ${LOG_DIR}/catalina.out"
echo ""
echo "   To uninstall:"
echo "     sudo ${INSTALL_DIR}/uninstall.sh"
echo ""
echo "  ==========================================================="
echo ""

exit 0
