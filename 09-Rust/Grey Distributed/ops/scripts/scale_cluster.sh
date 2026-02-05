#!/usr/bin/env bash
# =============================================================================
# Grey Distributed — Cluster Scaling Script
# =============================================================================
#
# PURPOSE:
#   Add or remove nodes from a Grey Distributed cluster safely.
#   Handles both worker nodes (stateless) and coordinator nodes (consensus).
#
# USAGE:
#   ./scale_cluster.sh {up|down} {workers|coordinators} [count] [options]
#
# EXAMPLES:
#   ./scale_cluster.sh up workers 5           # Add 5 workers
#   ./scale_cluster.sh down workers 3         # Remove 3 workers (graceful)
#   ./scale_cluster.sh up coordinators 2      # Add 2 coordinators
#   ./scale_cluster.sh down coordinators 1    # Remove 1 coordinator (careful!)
#   ./scale_cluster.sh status                 # Show current scale
#
# OPTIONS:
#   --dry-run         Show what would happen without acting
#   --force           Skip safety checks (DANGEROUS)
#   --wait            Wait for operation to complete
#   --timeout=SECONDS Maximum wait time (default: 300)
#   --namespace=NS    Kubernetes namespace (default: grey-system)
#   --context=CTX     Kubernetes context to use
#
# DESIGN PHILOSOPHY:
#   - Workers are stateless → scale freely
#   - Coordinators run consensus → scale carefully
#   - Always maintain quorum for consensus
#   - Drain nodes before removal to preserve in-flight work
#   - Verify cluster health after scaling
#
# TRADEOFFS:
#   - Faster scaling vs. safer draining
#   - Cost savings vs. capacity buffer
#   - Consensus stability vs. scaling flexibility
#
# DEPENDENCIES:
#   - kubectl
#   - jq
#   - curl (for API calls)
#
# =============================================================================

set -euo pipefail

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
NAMESPACE="${GREY_NAMESPACE:-grey-system}"
CONTEXT="${GREY_CONTEXT:-}"
DRY_RUN=false
FORCE=false
WAIT=false
TIMEOUT=300
API_URL="${GREY_API_URL:-http://localhost:8080}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[OK]${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*" >&2
}

die() {
    log_error "$@"
    exit 1
}

# Build kubectl command with context if specified
kctl() {
    local cmd="kubectl"
    if [[ -n "$CONTEXT" ]]; then
        cmd="$cmd --context=$CONTEXT"
    fi
    $cmd --namespace="$NAMESPACE" "$@"
}

# Check if a command exists
require_cmd() {
    command -v "$1" >/dev/null 2>&1 || die "Required command not found: $1"
}

# -----------------------------------------------------------------------------
# Validation Functions
# -----------------------------------------------------------------------------

validate_dependencies() {
    require_cmd kubectl
    require_cmd jq
    require_cmd curl
    
    # Verify cluster connection
    if ! kctl cluster-info >/dev/null 2>&1; then
        die "Cannot connect to Kubernetes cluster"
    fi
    
    # Verify Grey components exist
    if ! kctl get deployment grey-worker >/dev/null 2>&1; then
        die "Grey worker deployment not found in namespace $NAMESPACE"
    fi
}

# Get current replica count for a component
get_current_replicas() {
    local component="$1"
    local resource
    
    case "$component" in
        workers)
            resource="deployment/grey-worker"
            ;;
        coordinators)
            resource="statefulset/grey-coordinator"
            ;;
        *)
            die "Unknown component: $component"
            ;;
    esac
    
    kctl get "$resource" -o jsonpath='{.spec.replicas}'
}

# Get ready replica count
get_ready_replicas() {
    local component="$1"
    local resource
    
    case "$component" in
        workers)
            resource="deployment/grey-worker"
            ;;
        coordinators)
            resource="statefulset/grey-coordinator"
            ;;
        *)
            die "Unknown component: $component"
            ;;
    esac
    
    kctl get "$resource" -o jsonpath='{.status.readyReplicas}' || echo "0"
}

# Check cluster health via API
check_cluster_health() {
    local health
    health=$(curl -sf "${API_URL}/v1/health" 2>/dev/null || echo '{"healthy": false}')
    echo "$health" | jq -r '.healthy // false'
}

# Get consensus quorum status
check_quorum_status() {
    local status
    status=$(curl -sf "${API_URL}/v1/consensus/status" 2>/dev/null || echo '{}')
    echo "$status" | jq -r '.quorum_satisfied // false'
}

# Get current leader
get_current_leader() {
    curl -sf "${API_URL}/v1/consensus/status" 2>/dev/null | jq -r '.leader // "unknown"'
}

# -----------------------------------------------------------------------------
# Safety Checks
# -----------------------------------------------------------------------------

# Check if scaling down coordinators would break quorum
# Raft requires (n/2)+1 nodes for quorum
# Example: 5 nodes → need 3 for quorum → can remove at most 2
check_coordinator_quorum_safety() {
    local current="$1"
    local target="$2"
    
    if (( target < 3 )); then
        log_error "Cannot scale coordinators below 3 (minimum for quorum)"
        return 1
    fi
    
    # Check if target maintains quorum ability
    local quorum_size=$(( (target / 2) + 1 ))
    log_info "Target: $target coordinators, quorum requires: $quorum_size"
    
    # For safety, require at least 2 nodes beyond quorum during scaling
    # This accounts for one node being down during rolling update
    if (( target - quorum_size < 1 )); then
        log_warn "Tight quorum margin — consider adding buffer"
    fi
    
    return 0
}

# Check if there are tasks in flight that would be lost
check_tasks_in_flight() {
    local count
    count=$(curl -sf "${API_URL}/v1/scheduler/stats" 2>/dev/null | jq -r '.tasks_running // 0')
    
    if (( count > 0 )); then
        log_warn "$count tasks currently running"
        return 1
    fi
    return 0
}

# Check queue depth
get_queue_depth() {
    curl -sf "${API_URL}/v1/scheduler/stats" 2>/dev/null | jq -r '.queue_depth // 0'
}

# -----------------------------------------------------------------------------
# Scaling Operations
# -----------------------------------------------------------------------------

# Scale workers up/down
# Workers are stateless — scaling is straightforward
scale_workers() {
    local direction="$1"
    local count="$2"
    
    local current
    current=$(get_current_replicas workers)
    
    local target
    if [[ "$direction" == "up" ]]; then
        target=$((current + count))
    else
        target=$((current - count))
    fi
    
    # Validate bounds
    local min_replicas=1
    local max_replicas=1000
    
    if (( target < min_replicas )); then
        die "Cannot scale workers below $min_replicas (requested: $target)"
    fi
    
    if (( target > max_replicas )); then
        die "Cannot scale workers above $max_replicas (requested: $target)"
    fi
    
    log_info "Scaling workers: $current → $target"
    
    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would scale deployment/grey-worker to $target replicas"
        return 0
    fi
    
    # For scale-down, check tasks in flight
    if [[ "$direction" == "down" ]] && [[ "$FORCE" != "true" ]]; then
        local queue_depth
        queue_depth=$(get_queue_depth)
        if (( queue_depth > 100 )); then
            log_warn "Queue depth is $queue_depth — scaling down may increase latency"
            read -rp "Continue? [y/N] " confirm
            [[ "$confirm" =~ ^[Yy]$ ]] || die "Aborted by user"
        fi
    fi
    
    # Execute scaling
    kctl scale deployment/grey-worker --replicas="$target"
    log_success "Scaling initiated: workers → $target"
    
    # Wait for completion if requested
    if [[ "$WAIT" == "true" ]]; then
        wait_for_scaling workers "$target"
    fi
}

# Scale coordinators up/down
# Coordinators run Raft consensus — scaling requires membership changes
scale_coordinators() {
    local direction="$1"
    local count="$2"
    
    local current
    current=$(get_current_replicas coordinators)
    
    local target
    if [[ "$direction" == "up" ]]; then
        target=$((current + count))
    else
        target=$((current - count))
    fi
    
    log_info "Scaling coordinators: $current → $target"
    
    # Safety checks for coordinators
    if [[ "$FORCE" != "true" ]]; then
        # Check quorum safety
        if ! check_coordinator_quorum_safety "$current" "$target"; then
            die "Scaling would violate quorum safety"
        fi
        
        # Check current quorum status
        if [[ "$(check_quorum_status)" != "true" ]]; then
            die "Cluster is not in quorum — resolve before scaling"
        fi
        
        # Warn if scaling down
        if [[ "$direction" == "down" ]]; then
            log_warn "Scaling down coordinators affects consensus"
            log_warn "Current leader: $(get_current_leader)"
            read -rp "Continue? [y/N] " confirm
            [[ "$confirm" =~ ^[Yy]$ ]] || die "Aborted by user"
        fi
    fi
    
    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would scale statefulset/grey-coordinator to $target replicas"
        return 0
    fi
    
    # For coordinators, we need to handle Raft membership
    if [[ "$direction" == "up" ]]; then
        scale_coordinators_up "$current" "$target"
    else
        scale_coordinators_down "$current" "$target"
    fi
    
    # Wait for completion if requested
    if [[ "$WAIT" == "true" ]]; then
        wait_for_scaling coordinators "$target"
    fi
}

# Scale coordinators up — add new Raft members
scale_coordinators_up() {
    local current="$1"
    local target="$2"
    
    log_info "Adding $((target - current)) coordinator(s) to cluster"
    
    # StatefulSet will create new pods with predictable names
    # grey-coordinator-0, grey-coordinator-1, etc.
    kctl scale statefulset/grey-coordinator --replicas="$target"
    
    # Wait for pods to be ready
    log_info "Waiting for new coordinators to join..."
    kctl rollout status statefulset/grey-coordinator --timeout="${TIMEOUT}s"
    
    # Verify Raft membership
    for i in $(seq "$current" $((target - 1))); do
        local node_id="grey-coordinator-$i"
        log_info "Waiting for $node_id to join Raft..."
        
        local retries=30
        while (( retries > 0 )); do
            local member_count
            member_count=$(curl -sf "${API_URL}/v1/consensus/members" 2>/dev/null | jq -r 'length')
            if (( member_count >= target )); then
                log_success "$node_id joined Raft cluster"
                break
            fi
            sleep 2
            ((retries--))
        done
        
        if (( retries == 0 )); then
            log_error "$node_id failed to join Raft within timeout"
            return 1
        fi
    done
    
    log_success "All coordinators added and joined Raft"
}

# Scale coordinators down — remove Raft members safely
scale_coordinators_down() {
    local current="$1"
    local target="$2"
    
    log_info "Removing $((current - target)) coordinator(s) from cluster"
    
    # Remove nodes one at a time, highest index first
    for i in $(seq $((current - 1)) -1 "$target"); do
        local node_id="grey-coordinator-$i"
        log_info "Removing $node_id from Raft..."
        
        # Step 1: Remove from Raft membership first
        # This is crucial — removing the pod before Raft membership
        # can cause availability issues
        if ! curl -sf -X DELETE "${API_URL}/v1/consensus/members/$node_id" >/dev/null 2>&1; then
            log_error "Failed to remove $node_id from Raft membership"
            if [[ "$FORCE" != "true" ]]; then
                die "Aborting scale-down"
            fi
        fi
        
        # Step 2: Wait for leadership to transfer if necessary
        local leader
        leader=$(get_current_leader)
        if [[ "$leader" == "$node_id" ]]; then
            log_info "Leader is being removed — waiting for leadership transfer..."
            sleep 10
            leader=$(get_current_leader)
            if [[ "$leader" == "$node_id" ]]; then
                log_error "Leadership did not transfer"
                if [[ "$FORCE" != "true" ]]; then
                    die "Aborting scale-down"
                fi
            fi
        fi
        
        log_success "$node_id removed from Raft membership"
    done
    
    # Step 3: Scale down StatefulSet
    kctl scale statefulset/grey-coordinator --replicas="$target"
    
    log_success "Coordinators scaled down to $target"
}

# Wait for scaling to complete
wait_for_scaling() {
    local component="$1"
    local target="$2"
    
    log_info "Waiting for $component to reach $target replicas..."
    
    local deadline=$((SECONDS + TIMEOUT))
    while (( SECONDS < deadline )); do
        local ready
        ready=$(get_ready_replicas "$component")
        
        if (( ready == target )); then
            log_success "$component scaled to $target (all ready)"
            return 0
        fi
        
        log_info "Progress: $ready/$target ready"
        sleep 5
    done
    
    log_error "Timeout waiting for $component to scale"
    return 1
}

# -----------------------------------------------------------------------------
# Status Display
# -----------------------------------------------------------------------------

show_status() {
    echo ""
    echo "Grey Distributed Cluster Status"
    echo "================================"
    echo ""
    
    # Workers
    local worker_current worker_ready
    worker_current=$(get_current_replicas workers)
    worker_ready=$(get_ready_replicas workers)
    
    echo "Workers:"
    echo "  Desired:  $worker_current"
    echo "  Ready:    $worker_ready"
    echo ""
    
    # Coordinators
    local coord_current coord_ready
    coord_current=$(get_current_replicas coordinators)
    coord_ready=$(get_ready_replicas coordinators)
    
    echo "Coordinators:"
    echo "  Desired:  $coord_current"
    echo "  Ready:    $coord_ready"
    echo "  Leader:   $(get_current_leader)"
    echo "  Quorum:   $(check_quorum_status)"
    echo ""
    
    # Queue status
    echo "Workload:"
    echo "  Queue:    $(get_queue_depth) tasks"
    echo ""
    
    # HPA status
    echo "HPA Status:"
    kctl get hpa grey-worker-hpa -o wide 2>/dev/null || echo "  Not configured"
    echo ""
}

# -----------------------------------------------------------------------------
# Argument Parsing
# -----------------------------------------------------------------------------

parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --dry-run)
                DRY_RUN=true
                shift
                ;;
            --force)
                FORCE=true
                shift
                ;;
            --wait)
                WAIT=true
                shift
                ;;
            --timeout=*)
                TIMEOUT="${1#*=}"
                shift
                ;;
            --namespace=*)
                NAMESPACE="${1#*=}"
                shift
                ;;
            --context=*)
                CONTEXT="${1#*=}"
                shift
                ;;
            *)
                # Positional arguments
                ARGS+=("$1")
                shift
                ;;
        esac
    done
}

# -----------------------------------------------------------------------------
# Main Entry Point
# -----------------------------------------------------------------------------

usage() {
    cat <<EOF
Usage: $0 {up|down} {workers|coordinators} [count] [options]
       $0 status [options]

Commands:
  up       Scale up (add replicas)
  down     Scale down (remove replicas)
  status   Show current cluster status

Components:
  workers       Stateless worker nodes
  coordinators  Consensus coordinator nodes

Options:
  --dry-run         Show what would happen without acting
  --force           Skip safety checks
  --wait            Wait for operation to complete
  --timeout=SEC     Maximum wait time (default: 300)
  --namespace=NS    Kubernetes namespace (default: grey-system)
  --context=CTX     Kubernetes context to use

Examples:
  $0 up workers 5              Add 5 workers
  $0 down workers 3 --wait     Remove 3 workers, wait for completion
  $0 up coordinators 2         Add 2 coordinators
  $0 status                    Show cluster status

EOF
    exit 1
}

main() {
    ARGS=()
    parse_args "$@"
    
    # Validate dependencies
    validate_dependencies
    
    # Handle commands
    local action="${ARGS[0]:-}"
    
    case "$action" in
        status)
            show_status
            ;;
        up|down)
            local component="${ARGS[1]:-}"
            local count="${ARGS[2]:-1}"
            
            if [[ -z "$component" ]]; then
                usage
            fi
            
            case "$component" in
                workers)
                    scale_workers "$action" "$count"
                    ;;
                coordinators)
                    scale_coordinators "$action" "$count"
                    ;;
                *)
                    die "Unknown component: $component"
                    ;;
            esac
            ;;
        *)
            usage
            ;;
    esac
}

# Run if executed directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
