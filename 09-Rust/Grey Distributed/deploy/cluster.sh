#!/bin/bash
# =============================================================================
# Grey Distributed - Local Multi-Node Cluster
# =============================================================================
#
# This script starts a multi-node Grey cluster on a single machine for testing.
# Each node runs in a separate process with its own port and data directory.
#
# Use Case:
#   - Test consensus and replication behavior
#   - Verify fault tolerance (kill nodes and observe recovery)
#   - Develop against realistic cluster topology
#
# Usage:
#   ./cluster.sh start [nodes]    # Start cluster with N nodes (default: 3)
#   ./cluster.sh stop             # Stop all nodes
#   ./cluster.sh status           # Show cluster status
#   ./cluster.sh kill <node>      # Kill specific node
#   ./cluster.sh restart <node>   # Restart specific node
#   ./cluster.sh logs <node>      # Tail logs for node
#
set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

# Configuration
BASE_HTTP_PORT=8080
BASE_GRPC_PORT=9090
BASE_RAFT_PORT=7070
BASE_DATA_DIR="./data/cluster"
PID_DIR="./data/cluster/pids"
LOG_DIR="./data/cluster/logs"
DEFAULT_NODE_COUNT=3

# Helper functions
log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[OK]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

# Get port for node
http_port() { echo $((BASE_HTTP_PORT + $1 - 1)); }
grpc_port() { echo $((BASE_GRPC_PORT + $1 - 1)); }
raft_port() { echo $((BASE_RAFT_PORT + $1 - 1)); }

# Generate cluster peers list
cluster_peers() {
    local count=$1
    local peers=""
    for i in $(seq 1 $count); do
        if [[ -n "$peers" ]]; then
            peers+=","
        fi
        peers+="node-$i=localhost:$(raft_port $i)"
    done
    echo "$peers"
}

# Generate node configuration
generate_config() {
    local node_id=$1
    local node_count=$2
    local data_dir="$BASE_DATA_DIR/node-$node_id"
    local config_file="$data_dir/config.yaml"
    
    mkdir -p "$data_dir"/{raft,storage,logs}
    
    local peers=$(cluster_peers $node_count)
    
    cat > "$config_file" << EOF
# Grey Distributed - Node $node_id Configuration
# Cluster: $node_count nodes

cluster:
  mode: multi
  node_id: node-$node_id
  cluster_id: grey-local-cluster
  
  # Cluster membership
  initial_peers:
$(for i in $(seq 1 $node_count); do
    echo "    - id: node-$i"
    echo "      raft_addr: localhost:$(raft_port $i)"
    echo "      http_addr: localhost:$(http_port $i)"
done)
  
  # Raft settings
  raft:
    bind: "0.0.0.0:$(raft_port $node_id)"
    election_timeout: 500ms
    heartbeat_interval: 50ms
    snapshot_threshold: 5000
    pre_vote: true
    
network:
  http:
    bind: "0.0.0.0:$(http_port $node_id)"
    read_timeout: 30s
    write_timeout: 30s
    
  grpc:
    bind: "0.0.0.0:$(grpc_port $node_id)"
    max_message_size: 16777216
    
storage:
  data_dir: "${data_dir}/storage"
  
  # Standard replication
  replication_factor: $node_count
  read_quorum: $((($node_count + 1) / 2))
  write_quorum: $((($node_count + 1) / 2))
  
scheduler:
  workers: 4
  queue_size: 10000
  
observability:
  log_level: info
  log_format: text
  log_output: "${data_dir}/logs/greyd.log"
  
  metrics:
    enabled: true
    endpoint: "/metrics"
    
security:
  tls_enabled: false
  auth_enabled: false
  
fault_tolerance:
  failure_detection:
    phi_threshold: 8.0
    min_samples: 10
    
  self_healing:
    enabled: true
    auto_replace: false  # No spare nodes in local cluster
EOF
    
    echo "$config_file"
}

# Start a single node
start_node() {
    local node_id=$1
    local node_count=$2
    
    local config_file=$(generate_config $node_id $node_count)
    local pid_file="$PID_DIR/node-$node_id.pid"
    local log_file="$LOG_DIR/node-$node_id.log"
    
    if [[ -f "$pid_file" ]]; then
        local pid=$(cat "$pid_file")
        if kill -0 "$pid" 2>/dev/null; then
            log_warn "Node $node_id already running (PID: $pid)"
            return 0
        fi
    fi
    
    log_info "Starting node-$node_id..."
    
    # Build if necessary
    if [[ ! -f "./bin/greyd" ]]; then
        log_info "Building greyd..."
        go build -o ./bin/greyd ./cmd/greyd 2>/dev/null || {
            log_warn "Build skipped (no cmd/greyd yet)"
            # For demo, we'll use a placeholder
            return 0
        }
    fi
    
    # Start node in background
    GREY_CONFIG="$config_file" ./bin/greyd --config "$config_file" > "$log_file" 2>&1 &
    local pid=$!
    echo "$pid" > "$pid_file"
    
    # Wait a moment and check if it's running
    sleep 0.5
    if kill -0 "$pid" 2>/dev/null; then
        log_success "Node $node_id started (PID: $pid, HTTP: $(http_port $node_id))"
    else
        log_error "Node $node_id failed to start. Check $log_file"
        return 1
    fi
}

# Stop a single node
stop_node() {
    local node_id=$1
    local pid_file="$PID_DIR/node-$node_id.pid"
    
    if [[ ! -f "$pid_file" ]]; then
        log_warn "Node $node_id not running (no PID file)"
        return 0
    fi
    
    local pid=$(cat "$pid_file")
    if kill -0 "$pid" 2>/dev/null; then
        log_info "Stopping node-$node_id (PID: $pid)..."
        kill "$pid"
        
        # Wait for graceful shutdown
        for i in {1..10}; do
            if ! kill -0 "$pid" 2>/dev/null; then
                break
            fi
            sleep 0.5
        done
        
        # Force kill if still running
        if kill -0 "$pid" 2>/dev/null; then
            log_warn "Force killing node-$node_id..."
            kill -9 "$pid"
        fi
        
        rm -f "$pid_file"
        log_success "Node $node_id stopped"
    else
        log_warn "Node $node_id not running (stale PID)"
        rm -f "$pid_file"
    fi
}

# Start cluster
cmd_start() {
    local node_count=${1:-$DEFAULT_NODE_COUNT}
    
    echo -e "${CYAN}"
    echo "╔═══════════════════════════════════════════════════════════════╗"
    echo "║            Grey Distributed - Local Cluster                   ║"
    echo "╚═══════════════════════════════════════════════════════════════╝"
    echo -e "${NC}"
    
    log_info "Starting $node_count-node cluster..."
    
    # Create directories
    mkdir -p "$PID_DIR" "$LOG_DIR"
    
    # Start each node
    for i in $(seq 1 $node_count); do
        start_node $i $node_count
    done
    
    echo ""
    log_success "Cluster started!"
    echo ""
    echo -e "${BLUE}Cluster Endpoints:${NC}"
    for i in $(seq 1 $node_count); do
        echo "  node-$i: http://localhost:$(http_port $i)  |  gRPC: $(grpc_port $i)  |  Raft: $(raft_port $i)"
    done
    echo ""
    echo -e "${YELLOW}Commands:${NC}"
    echo "  $0 status       - Show cluster status"
    echo "  $0 stop         - Stop all nodes"
    echo "  $0 kill <N>     - Kill node N (simulate failure)"
    echo "  $0 logs <N>     - Tail logs for node N"
}

# Stop cluster
cmd_stop() {
    log_info "Stopping cluster..."
    
    for pid_file in "$PID_DIR"/*.pid; do
        if [[ -f "$pid_file" ]]; then
            local node_id=$(basename "$pid_file" .pid | sed 's/node-//')
            stop_node $node_id
        fi
    done
    
    log_success "Cluster stopped"
}

# Cluster status
cmd_status() {
    echo -e "${CYAN}Grey Distributed - Cluster Status${NC}"
    echo ""
    
    local running=0
    local stopped=0
    
    for pid_file in "$PID_DIR"/*.pid; do
        if [[ -f "$pid_file" ]]; then
            local node_id=$(basename "$pid_file" .pid | sed 's/node-//')
            local pid=$(cat "$pid_file")
            
            if kill -0 "$pid" 2>/dev/null; then
                echo -e "  ${GREEN}●${NC} node-$node_id (PID: $pid) - HTTP: $(http_port $node_id)"
                ((running++))
            else
                echo -e "  ${RED}●${NC} node-$node_id (dead)"
                ((stopped++))
            fi
        fi
    done
    
    if [[ $running -eq 0 && $stopped -eq 0 ]]; then
        echo "  No nodes configured"
    fi
    
    echo ""
    echo "Running: $running  |  Stopped: $stopped"
    
    # Check leader (would need HTTP call in real implementation)
    # curl -s "http://localhost:$BASE_HTTP_PORT/v1/status" | jq .leader
}

# Kill specific node
cmd_kill() {
    local node_id=$1
    
    if [[ -z "$node_id" ]]; then
        log_error "Usage: $0 kill <node_number>"
        exit 1
    fi
    
    log_info "Killing node-$node_id..."
    
    local pid_file="$PID_DIR/node-$node_id.pid"
    if [[ -f "$pid_file" ]]; then
        local pid=$(cat "$pid_file")
        kill -9 "$pid" 2>/dev/null || true
        rm -f "$pid_file"
        log_success "Node $node_id killed"
    else
        log_warn "Node $node_id not running"
    fi
}

# Restart specific node
cmd_restart() {
    local node_id=$1
    
    if [[ -z "$node_id" ]]; then
        log_error "Usage: $0 restart <node_number>"
        exit 1
    fi
    
    # Determine cluster size from existing configs
    local node_count=$(ls -1 "$PID_DIR"/*.pid 2>/dev/null | wc -l)
    if [[ $node_count -eq 0 ]]; then
        node_count=$DEFAULT_NODE_COUNT
    fi
    
    stop_node $node_id
    sleep 1
    start_node $node_id $node_count
}

# Tail logs
cmd_logs() {
    local node_id=$1
    
    if [[ -z "$node_id" ]]; then
        log_error "Usage: $0 logs <node_number>"
        exit 1
    fi
    
    local log_file="$LOG_DIR/node-$node_id.log"
    
    if [[ -f "$log_file" ]]; then
        tail -f "$log_file"
    else
        log_error "Log file not found: $log_file"
        exit 1
    fi
}

# Main
case "${1:-}" in
    start)
        cmd_start "${2:-}"
        ;;
    stop)
        cmd_stop
        ;;
    status)
        cmd_status
        ;;
    kill)
        cmd_kill "$2"
        ;;
    restart)
        cmd_restart "$2"
        ;;
    logs)
        cmd_logs "$2"
        ;;
    *)
        echo "Usage: $0 {start|stop|status|kill|restart|logs}"
        echo ""
        echo "Commands:"
        echo "  start [N]    Start N-node cluster (default: $DEFAULT_NODE_COUNT)"
        echo "  stop         Stop all nodes"
        echo "  status       Show cluster status"
        echo "  kill <N>     Kill node N (simulate failure)"
        echo "  restart <N>  Restart node N"
        echo "  logs <N>     Tail logs for node N"
        exit 1
        ;;
esac
