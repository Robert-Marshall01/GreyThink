#!/usr/bin/env bash
# ─────────────────────────────────────────────────────────────
# Grey DB — macOS Uninstaller
# Usage: sudo bash uninstall-macos.sh
# ─────────────────────────────────────────────────────────────

set -euo pipefail
IFS=$'\n\t'

# ── Configuration ───────────────────────────────────────────
INSTALL_DIR="/usr/local/opt/greydb"
DATA_DIR="/usr/local/var/greydb"
LOG_DIR="/usr/local/var/log/greydb"
CONFIG_DIR="/usr/local/etc/greydb"
BIN_LINK="/usr/local/bin/greydb"
PLIST_LABEL="com.greythink.greydb-server"

# Determine real user
REAL_USER="${SUDO_USER:-$USER}"
REAL_HOME="$(eval echo "~$REAL_USER")"
PLIST_FILE="$REAL_HOME/Library/LaunchAgents/$PLIST_LABEL.plist"

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
echo -e "  ${MAGENTA}║         Grey DB — macOS Uninstaller          ║${NC}"
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
    while IFS='=' read -r key value; do
        case "$key" in
            INSTALL_DIR|DATA_DIR|LOG_DIR|CONFIG_DIR|BIN_LINK|PLIST_FILE|PLIST_LABEL)
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

# ── Unload launchd service ─────────────────────────────────
echo -e "  ${WHITE}[1/6] Stopping services...${NC}"

if [[ -f "$PLIST_FILE" ]]; then
    # Unload as the real user, not root
    sudo -u "$REAL_USER" launchctl unload "$PLIST_FILE" 2>/dev/null || true
    rm -f "$PLIST_FILE"
    log_ok "launchd service unloaded and plist removed"
else
    log_warn "launchd plist not found — skipping"
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

# ── Remove CLI ─────────────────────────────────────────────
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

# ── Remove config ──────────────────────────────────────────
echo -e "  ${WHITE}[6/6] Removing config...${NC}"

if [[ -d "$CONFIG_DIR" ]]; then
    rm -rf "$CONFIG_DIR"
    log_ok "Removed $CONFIG_DIR"
fi

# ── Done ────────────────────────────────────────────────────
echo ""
echo -e "  ${GREEN}╔══════════════════════════════════════════════╗${NC}"
echo -e "  ${GREEN}║     Grey DB uninstalled successfully!        ║${NC}"
echo -e "  ${GREEN}╚══════════════════════════════════════════════╝${NC}"
echo ""
