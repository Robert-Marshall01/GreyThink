#!/usr/bin/env bash
# ─────────────────────────────────────────────────────────────
# Grey DB — Linux Uninstaller
# Usage: sudo bash uninstall-linux.sh
# ─────────────────────────────────────────────────────────────

set -euo pipefail
IFS=$'\n\t'

# ── Configuration ───────────────────────────────────────────
INSTALL_DIR="/opt/greydb"
DATA_DIR="/var/lib/greydb"
LOG_DIR="/var/log/greydb"
CONFIG_DIR="/etc/greydb"
BIN_LINK="/usr/local/bin/greydb"
SERVICE_NAME="greydb-server"
SYSTEMD_DIR="/etc/systemd/system"

# ── Colors ──────────────────────────────────────────────────
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
WHITE='\033[1;37m'
NC='\033[0m'

log_ok()    { echo -e "  ${GREEN}[OK]${NC}    $1"; }
log_warn()  { echo -e "  ${YELLOW}[WARN]${NC}  $1"; }
log_info()  { echo -e "  ${CYAN}[INFO]${NC}  $1"; }

# ── Banner ──────────────────────────────────────────────────
echo ""
echo -e "  ${MAGENTA}╔══════════════════════════════════════════════╗${NC}"
echo -e "  ${MAGENTA}║         Grey DB — Linux Uninstaller          ║${NC}"
echo -e "  ${MAGENTA}╚══════════════════════════════════════════════╝${NC}"
echo ""

# ── Check root ──────────────────────────────────────────────
if [[ $EUID -ne 0 ]]; then
    echo -e "  ${RED}[ERROR]${NC} This uninstaller must be run as root."
    echo -e "  Usage: ${WHITE}sudo bash $0${NC}"
    exit 1
fi

# ── Load install info if available ──────────────────────────
if [[ -f "$CONFIG_DIR/install-info" ]]; then
    # Only source known safe keys
    while IFS='=' read -r key value; do
        case "$key" in
            INSTALL_DIR|DATA_DIR|LOG_DIR|CONFIG_DIR|BIN_LINK)
                declare "$key=$value"
                ;;
        esac
    done < "$CONFIG_DIR/install-info"
fi

# ── Confirm ─────────────────────────────────────────────────
echo -e "  ${YELLOW}This will remove Grey DB from your system.${NC}"
echo ""
echo -e "  Install directory:  ${WHITE}$INSTALL_DIR${NC}"
echo -e "  Data directory:     ${WHITE}$DATA_DIR${NC}"
echo -e "  Config directory:   ${WHITE}$CONFIG_DIR${NC}"
echo -e "  Log directory:      ${WHITE}$LOG_DIR${NC}"
echo ""
read -rp "  Are you sure you want to uninstall Grey DB? (y/N) " response
if [[ ! "$response" =~ ^[Yy]([Ee][Ss])?$ ]]; then
    echo -e "  ${YELLOW}Uninstall cancelled.${NC}"
    exit 0
fi
echo ""

# ── Stop and disable systemd service ──────────────────────
echo -e "  ${WHITE}[1/6] Stopping services...${NC}"

if systemctl is-active --quiet "$SERVICE_NAME" 2>/dev/null; then
    systemctl stop "$SERVICE_NAME"
    log_ok "Service '$SERVICE_NAME' stopped"
else
    log_warn "Service '$SERVICE_NAME' not running — skipping"
fi

if systemctl is-enabled --quiet "$SERVICE_NAME" 2>/dev/null; then
    systemctl disable "$SERVICE_NAME"
    log_ok "Service '$SERVICE_NAME' disabled"
fi

if [[ -f "$SYSTEMD_DIR/$SERVICE_NAME.service" ]]; then
    rm -f "$SYSTEMD_DIR/$SERVICE_NAME.service"
    systemctl daemon-reload
    log_ok "Service file removed"
fi

# ── Stop Docker containers ─────────────────────────────────
echo -e "  ${WHITE}[2/6] Stopping Docker containers...${NC}"

if command -v docker &>/dev/null; then
    for container in greydb-postgres greydb-server greydb-ui; do
        if docker ps -q --filter "name=$container" 2>/dev/null | grep -q .; then
            docker stop "$container" 2>/dev/null || true
            docker rm "$container" 2>/dev/null || true
            log_ok "Stopped and removed container: $container"
        fi
    done

    read -rp "  Remove Docker volumes (database data will be lost)? (y/N) " vol_response
    if [[ "$vol_response" =~ ^[Yy]([Ee][Ss])?$ ]]; then
        docker volume rm grey-db_pgdata 2>/dev/null || true
        docker volume rm greydb_pgdata 2>/dev/null || true
        log_ok "Docker volumes removed"
    fi
else
    log_warn "Docker not found — skipping container cleanup"
fi

# ── Remove CLI symlink ─────────────────────────────────────
echo -e "  ${WHITE}[3/6] Removing CLI...${NC}"

if [[ -f "$BIN_LINK" || -L "$BIN_LINK" ]]; then
    rm -f "$BIN_LINK"
    log_ok "CLI removed from $BIN_LINK"
else
    log_warn "CLI not found at $BIN_LINK — skipping"
fi

# ── Remove install directory ───────────────────────────────
echo -e "  ${WHITE}[4/6] Removing installation files...${NC}"

if [[ -d "$INSTALL_DIR" ]]; then
    rm -rf "$INSTALL_DIR"
    log_ok "Removed $INSTALL_DIR"
else
    log_warn "Install directory not found — skipping"
fi

# ── Remove data and logs ──────────────────────────────────
echo -e "  ${WHITE}[5/6] Cleaning up data and logs...${NC}"

if [[ -d "$DATA_DIR" ]]; then
    read -rp "  Remove data directory ($DATA_DIR)? (y/N) " data_response
    if [[ "$data_response" =~ ^[Yy]([Ee][Ss])?$ ]]; then
        rm -rf "$DATA_DIR"
        log_ok "Removed $DATA_DIR"
    else
        log_warn "Data directory preserved"
    fi
fi

if [[ -d "$LOG_DIR" ]]; then
    rm -rf "$LOG_DIR"
    log_ok "Removed $LOG_DIR"
fi

# ── Remove config and system user ──────────────────────────
echo -e "  ${WHITE}[6/6] Removing config and system user...${NC}"

if [[ -d "$CONFIG_DIR" ]]; then
    rm -rf "$CONFIG_DIR"
    log_ok "Removed $CONFIG_DIR"
fi

if id -u greydb &>/dev/null; then
    userdel greydb 2>/dev/null || true
    log_ok "System user 'greydb' removed"
fi

# ── Done ────────────────────────────────────────────────────
echo ""
echo -e "  ${GREEN}╔══════════════════════════════════════════════╗${NC}"
echo -e "  ${GREEN}║     Grey DB uninstalled successfully!        ║${NC}"
echo -e "  ${GREEN}╚══════════════════════════════════════════════╝${NC}"
echo ""
