#!/bin/bash
#
# Grey Multi-Tenant Platform Launcher
# Self-healing launcher that checks prerequisites and launches the API with visible logging.
#

set -e

# Determine script location
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Auto-detect installation paths (user-install vs system-install)
if [ -x "$HOME/Applications/GreyMultiTenant.app/Contents/MacOS/grey-core-api" ]; then
    APP_DIR="$HOME/Applications/GreyMultiTenant.app/Contents/MacOS"
    CONFIG_DIR="$HOME/.config/grey-multitenant"
    LOG_DIR="$HOME/Library/Logs/GreyMultiTenant"
elif [ -x "/Applications/GreyMultiTenant.app/Contents/MacOS/grey-core-api" ]; then
    APP_DIR="/Applications/GreyMultiTenant.app/Contents/MacOS"
    CONFIG_DIR="/etc/grey-multitenant"
    LOG_DIR="/var/log/grey-multitenant"
elif [ -x "$SCRIPT_DIR/grey-core-api" ]; then
    APP_DIR="$SCRIPT_DIR"
    CONFIG_DIR="$SCRIPT_DIR"
    LOG_DIR="$SCRIPT_DIR/logs"
else
    APP_DIR="/Applications/GreyMultiTenant.app/Contents/MacOS"
    CONFIG_DIR="/etc/grey-multitenant"
    LOG_DIR="/var/log/grey-multitenant"
fi

# Find config file
CONFIG_FILE=""
for path in "$CONFIG_DIR/.env" "$HOME/.config/grey-multitenant/.env" "/etc/grey-multitenant/.env" "$SCRIPT_DIR/.env"; do
    if [ -f "$path" ]; then
        CONFIG_FILE="$path"
        break
    fi
done

# Export GREY_CONFIG_PATH if config found
if [ -n "$CONFIG_FILE" ]; then
    export GREY_CONFIG_PATH="$CONFIG_FILE"
fi

LOG_FILE="$LOG_DIR/startup-$(date +%Y-%m-%d-%H%M%S).log"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

log() {
    local level="$1"
    shift
    local message="$*"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo -e "[$timestamp] [$level] $message" | tee -a "$LOG_FILE" 2>/dev/null || echo -e "[$timestamp] [$level] $message"
}

# Ensure log directory
mkdir -p "$LOG_DIR" 2>/dev/null || true

echo -e "${CYAN}============================================${NC}"
echo -e "${CYAN}  Grey Multi-Tenant Platform${NC}"
echo -e "${CYAN}============================================${NC}"
echo ""

log "INFO" "Binary: $APP_DIR/grey-core-api"
if [ -n "$CONFIG_FILE" ]; then
    log "INFO" "Configuration: $CONFIG_FILE"
else
    log "WARN" "No .env config file found, using environment variables"
fi

log "INFO" "Checking prerequisites..."

# Check PostgreSQL with retry
PG_READY=false
MAX_RETRIES=3
RETRY_DELAY=2

for attempt in $(seq 1 $MAX_RETRIES); do
    log "INFO" "Checking PostgreSQL on localhost:5432... (attempt $attempt/$MAX_RETRIES)"

# Check Docker
if command -v docker &>/dev/null; then
    CONTAINER=$(docker ps --filter "name=grey-postgres" --format "{{.Names}}" 2>/dev/null || true)
    if [ "$CONTAINER" = "grey-postgres" ]; then
        log "INFO" "PostgreSQL running in Docker container"
        PG_READY=true
    fi
fi

# Check Homebrew PostgreSQL
if [ "$PG_READY" = false ] && command -v brew &>/dev/null; then
    if brew services list 2>/dev/null | grep -q "postgresql.*started"; then
        log "INFO" "PostgreSQL running via Homebrew"
        PG_READY=true
    fi
fi

# Test connection
if command -v pg_isready &>/dev/null && pg_isready -h localhost -p 5432 -q 2>/dev/null; then
    log "INFO" "PostgreSQL is accepting connections on port 5432"
    PG_READY=true
elif nc -z localhost 5432 2>/dev/null; then
    log "INFO" "PostgreSQL port 5432 is open"
    PG_READY=true
fi

    # Break if ready
    if [ "$PG_READY" = true ]; then
        break
    fi
    
    # Wait before retry (except on last attempt)
    if [ $attempt -lt $MAX_RETRIES ]; then
        log "INFO" "Waiting $RETRY_DELAY seconds before retry..."
        sleep $RETRY_DELAY
    fi
done

if [ "$PG_READY" = false ]; then
    echo ""
    echo -e "${YELLOW}PostgreSQL not running. Attempting to start via Docker...${NC}"
    
    # Try to start PostgreSQL via Docker
    if command -v docker &>/dev/null; then
        # Check if grey-postgres container exists
        CONTAINER_EXISTS=$(docker ps -a --filter "name=grey-postgres" --format "{{.Names}}" 2>/dev/null || true)
        if [ "$CONTAINER_EXISTS" = "grey-postgres" ]; then
            log "INFO" "Starting existing grey-postgres container..."
            docker start grey-postgres 2>/dev/null
            sleep 3
            CONTAINER=$(docker ps --filter "name=grey-postgres" --format "{{.Names}}" 2>/dev/null || true)
            if [ "$CONTAINER" = "grey-postgres" ]; then
                log "INFO" "PostgreSQL container started successfully"
                PG_READY=true
            fi
        else
            # Try to find docker-compose.yml and start postgres service
            for compose_dir in "$HOME/Desktop/Grey Multi-Tenant" "/Applications/GreyMultiTenant" "$SCRIPT_DIR/../.."; do
                if [ -f "$compose_dir/docker-compose.yml" ]; then
                    log "INFO" "Found docker-compose.yml at $compose_dir"
                    pushd "$compose_dir" > /dev/null 2>&1
                    docker compose up -d postgres 2>/dev/null
                    popd > /dev/null 2>&1
                    sleep 5
                    CONTAINER=$(docker ps --filter "name=grey-postgres" --format "{{.Names}}" 2>/dev/null || true)
                    if [ "$CONTAINER" = "grey-postgres" ]; then
                        log "INFO" "PostgreSQL container started via docker compose"
                        PG_READY=true
                        break
                    fi
                fi
            done
        fi
    fi
fi

if [ "$PG_READY" = false ]; then
    echo ""
    echo -e "${RED}ERROR: PostgreSQL is not running!${NC}"
    echo ""
    echo -e "${YELLOW}To fix, choose one of these options:${NC}"
    echo "  1. Install Docker Desktop and run: docker compose up -d postgres"
    echo "  2. Install PostgreSQL via Homebrew: brew install postgresql@16 && brew services start postgresql@16"
    echo ""
    log "FATAL" "Startup aborted: PostgreSQL not available"
    # Only prompt in interactive terminals
    if [ -t 0 ]; then
        read -p "Press Enter to exit..."
    fi
    exit 1
fi

# Launch the application
log "INFO" "Starting Grey Core API..."
echo ""
echo -e "${YELLOW}Server starting... Press Ctrl+C to stop.${NC}"
echo ""

"$APP_DIR/grey-core-api" 2>&1 | tee -a "$LOG_FILE"
EXIT_CODE=${PIPESTATUS[0]}

if [ $EXIT_CODE -ne 0 ]; then
    echo ""
    echo -e "${RED}Application exited with error code: $EXIT_CODE${NC}"
    log "ERROR" "Application exited with code $EXIT_CODE"
    echo -e "${YELLOW}Log file: $LOG_FILE${NC}"
    echo ""
    # Only prompt in interactive terminals
    if [ -t 0 ]; then
        read -p "Press Enter to exit..."
    fi
fi

exit $EXIT_CODE
