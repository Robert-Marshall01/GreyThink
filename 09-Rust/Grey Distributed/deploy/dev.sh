#!/bin/bash
# =============================================================================
# Grey Distributed - Single Node Development Mode
# =============================================================================
#
# This script starts a single-node Grey cluster for local development.
# All components run in-process with relaxed consistency and timeout settings.
#
# Usage:
#   ./dev.sh                  # Start with defaults
#   ./dev.sh --debug          # Enable debug logging
#   ./dev.sh --port 8080      # Use specific port
#   ./dev.sh --data /tmp/grey # Use specific data directory
#
# Environment Variables:
#   GREY_LOG_LEVEL      - Log level (debug, info, warn, error)
#   GREY_DATA_DIR       - Data directory path
#   GREY_HTTP_PORT      - HTTP API port
#   GREY_GRPC_PORT      - gRPC API port
#
set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Default configuration
DEFAULT_HTTP_PORT=8080
DEFAULT_GRPC_PORT=9090
DEFAULT_DATA_DIR="./data/dev"
DEFAULT_LOG_LEVEL="info"

# Parse arguments
HTTP_PORT="${GREY_HTTP_PORT:-$DEFAULT_HTTP_PORT}"
GRPC_PORT="${GREY_GRPC_PORT:-$DEFAULT_GRPC_PORT}"
DATA_DIR="${GREY_DATA_DIR:-$DEFAULT_DATA_DIR}"
LOG_LEVEL="${GREY_LOG_LEVEL:-$DEFAULT_LOG_LEVEL}"
DEBUG_MODE=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --debug)
            DEBUG_MODE=true
            LOG_LEVEL="debug"
            shift
            ;;
        --port)
            HTTP_PORT="$2"
            shift 2
            ;;
        --grpc-port)
            GRPC_PORT="$2"
            shift 2
            ;;
        --data)
            DATA_DIR="$2"
            shift 2
            ;;
        --help)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --debug        Enable debug logging"
            echo "  --port N       HTTP API port (default: $DEFAULT_HTTP_PORT)"
            echo "  --grpc-port N  gRPC API port (default: $DEFAULT_GRPC_PORT)"
            echo "  --data PATH    Data directory (default: $DEFAULT_DATA_DIR)"
            echo "  --help         Show this help"
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            exit 1
            ;;
    esac
done

echo -e "${BLUE}"
echo "  ____                   ____  _     _        _ _           _           _ "
echo " / ___|_ __ ___ _   _   |  _ \(_)___| |_ _ __(_) |__  _   _| |_ ___  __| |"
echo "| |  _| '__/ _ \ | | |  | | | | / __| __| '__| | '_ \| | | | __/ _ \/ _\` |"
echo "| |_| | | |  __/ |_| |  | |_| | \__ \ |_| |  | | |_) | |_| | ||  __/ (_| |"
echo " \____|_|  \___|\__, |  |____/|_|___/\__|_|  |_|_.__/ \__,_|\__\___|\__,_|"
echo "                |___/                                                     "
echo -e "${NC}"
echo -e "${GREEN}Development Mode${NC}"
echo ""

# Create data directory
echo -e "${YELLOW}Creating data directory: $DATA_DIR${NC}"
mkdir -p "$DATA_DIR"/{raft,storage,logs}

# Generate development configuration
CONFIG_FILE="$DATA_DIR/config.yaml"
echo -e "${YELLOW}Generating configuration: $CONFIG_FILE${NC}"

cat > "$CONFIG_FILE" << EOF
# Grey Distributed - Development Configuration
# Generated: $(date -u +"%Y-%m-%dT%H:%M:%SZ")

cluster:
  mode: single
  node_id: dev-node-1
  cluster_id: grey-dev-cluster
  
  # Single node - no real consensus needed
  raft:
    election_timeout: 1s
    heartbeat_interval: 100ms
    election_enabled: false  # No elections in single node
    snapshot_threshold: 1000
    
network:
  http:
    bind: "0.0.0.0:${HTTP_PORT}"
    read_timeout: 30s
    write_timeout: 30s
    
  grpc:
    bind: "0.0.0.0:${GRPC_PORT}"
    max_message_size: 16777216  # 16MB
    
  # Protocol settings
  grey_protocol:
    enabled: true
    bind: "0.0.0.0:9191"
    compression: none  # Disable for dev simplicity
    
storage:
  data_dir: "${DATA_DIR}/storage"
  
  # Relaxed consistency for dev
  replication_factor: 1
  read_quorum: 1
  write_quorum: 1
  
  # In-memory cache
  cache:
    enabled: true
    max_size: 268435456  # 256MB
    
scheduler:
  # Single worker for predictable debugging
  workers: 1
  queue_size: 1000
  
  # All tasks in single priority lane
  priority_lanes:
    - name: default
      weight: 100
      
governance:
  # No quotas in dev mode
  quotas_enabled: false
  throttling_enabled: false
  
  # Generous limits
  default_rate_limit: 10000
  
observability:
  log_level: "${LOG_LEVEL}"
  log_format: text  # Human readable
  log_output: stdout
  
  # Tracing disabled in dev
  tracing:
    enabled: ${DEBUG_MODE}
    sampler: always  # Sample everything when enabled
    
  # Metrics for debugging
  metrics:
    enabled: true
    endpoint: "/metrics"
    
security:
  # Relaxed for development
  tls_enabled: false
  auth_enabled: false
  
  # Development keys
  node_key_path: "${DATA_DIR}/node.key"
  
fault_tolerance:
  # Aggressive for fast dev feedback
  failure_detection:
    phi_threshold: 4.0  # Lower = faster detection
    min_samples: 3
    
  # No self-healing in single node
  self_healing:
    enabled: false
EOF

echo -e "${GREEN}Configuration generated.${NC}"
echo ""
echo -e "${BLUE}Configuration Summary:${NC}"
echo "  HTTP API:     http://localhost:${HTTP_PORT}"
echo "  gRPC API:     localhost:${GRPC_PORT}"
echo "  Data Dir:     ${DATA_DIR}"
echo "  Log Level:    ${LOG_LEVEL}"
echo ""

# Check if binary exists, if not suggest building
BINARY="./bin/greyd"
if [[ ! -f "$BINARY" ]]; then
    echo -e "${YELLOW}Binary not found. Building...${NC}"
    if [[ -f "Makefile" ]]; then
        make build
    else
        echo -e "${YELLOW}Building with go build...${NC}"
        go build -o "$BINARY" ./cmd/greyd
    fi
fi

# Start the server
echo -e "${GREEN}Starting Grey Distributed in development mode...${NC}"
echo -e "${YELLOW}Press Ctrl+C to stop.${NC}"
echo ""

# Set environment variables
export GREY_CONFIG="$CONFIG_FILE"
export GREY_DEV_MODE=true

# Run with proper signal handling
if [[ "$DEBUG_MODE" == "true" ]]; then
    # Enable Go race detector in debug mode
    exec go run -race ./cmd/greyd --config "$CONFIG_FILE"
else
    exec "$BINARY" --config "$CONFIG_FILE"
fi
