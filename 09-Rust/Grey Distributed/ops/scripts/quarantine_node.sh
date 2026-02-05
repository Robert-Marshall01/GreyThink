#!/usr/bin/env bash
# =============================================================================
# Grey Distributed — Node Quarantine Script
# =============================================================================
#
# PURPOSE:
#   Isolate a misbehaving or suspect node from the Grey cluster.
#   Quarantine removes the node from active participation while keeping
#   it running for debugging and forensics.
#
# USAGE:
#   ./quarantine_node.sh {quarantine|release|status|list} [node-id] [options]
#
# EXAMPLES:
#   ./quarantine_node.sh quarantine grey-worker-7       # Quarantine a worker
#   ./quarantine_node.sh quarantine grey-coordinator-2  # Quarantine coordinator
#   ./quarantine_node.sh release grey-worker-7          # Release from quarantine
#   ./quarantine_node.sh status grey-worker-7           # Check node status
#   ./quarantine_node.sh list                           # List all quarantined nodes
#
# OPTIONS:
#   --reason=TEXT     Reason for quarantine (required for quarantine action)
#   --duration=TIME   Auto-release after duration (e.g., 30m, 2h)
#   --dry-run         Show what would happen without acting
#   --force           Skip safety checks
#   --preserve-data   Keep node data for forensics (default: true)
#   --namespace=NS    Kubernetes namespace (default: grey-system)
#
# QUARANTINE MECHANICS:
#   1. Remove node from load balancer / service endpoints
#   2. Drain pending work from node
#   3. Block new work assignment
#   4. Remove from consensus membership (if coordinator)
#   5. Apply network policy to isolate traffic
#   6. Keep pod running for debugging
#
# TRADEOFFS:
#   - Full isolation vs. debugging access
#   - Immediate action vs. graceful drain
#   - Coordinator quarantine affects quorum
#   - Network isolation may hide cascading failures
#
# WHEN TO QUARANTINE:
#   - Node is producing incorrect results
#   - Node is experiencing hardware issues
#   - Security incident suspected
#   - Performance anomalies detected
#   - Chaos engineering experiments
#
# DEPENDENCIES:
#   - kubectl
#   - jq
#   - curl
#
# =============================================================================

set -euo pipefail

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
NAMESPACE="${GREY_NAMESPACE:-grey-system}"
DRY_RUN=false
FORCE=false
PRESERVE_DATA=true
REASON=""
DURATION=""
API_URL="${GREY_API_URL:-http://localhost:8080}"
QUARANTINE_LABEL="grey.io/quarantined"
QUARANTINE_ANNOTATION="grey.io/quarantine-reason"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
NC='\033[0m'

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

log_quarantine() {
    echo -e "${MAGENTA}[QUARANTINE]${NC} $*"
}

die() {
    log_error "$@"
    exit 1
}

kctl() {
    kubectl --namespace="$NAMESPACE" "$@"
}

require_cmd() {
    command -v "$1" >/dev/null 2>&1 || die "Required command not found: $1"
}

# Get timestamp in ISO format
timestamp() {
    date -u +"%Y-%m-%dT%H:%M:%SZ"
}

# -----------------------------------------------------------------------------
# Node Discovery
# -----------------------------------------------------------------------------

# Get pod name from node ID
# Node ID can be pod name or a shorter form
resolve_pod_name() {
    local node_id="$1"
    
    # Check if it's already a valid pod
    if kctl get pod "$node_id" >/dev/null 2>&1; then
        echo "$node_id"
        return 0
    fi
    
    # Try to find by label
    local pod
    pod=$(kctl get pods -l "app.kubernetes.io/instance=$node_id" -o jsonpath='{.items[0].metadata.name}' 2>/dev/null)
    
    if [[ -n "$pod" ]]; then
        echo "$pod"
        return 0
    fi
    
    return 1
}

# Determine node type (worker or coordinator)
get_node_type() {
    local pod="$1"
    
    local labels
    labels=$(kctl get pod "$pod" -o jsonpath='{.metadata.labels}')
    
    if echo "$labels" | grep -q "coordinator"; then
        echo "coordinator"
    else
        echo "worker"
    fi
}

# Check if node is currently quarantined
is_quarantined() {
    local pod="$1"
    
    local label
    label=$(kctl get pod "$pod" -o jsonpath="{.metadata.labels.${QUARANTINE_LABEL//./-}}" 2>/dev/null)
    
    [[ "$label" == "true" ]]
}

# Get quarantine reason
get_quarantine_reason() {
    local pod="$1"
    kctl get pod "$pod" -o jsonpath="{.metadata.annotations.${QUARANTINE_ANNOTATION//./-}}" 2>/dev/null
}

# Get quarantine timestamp
get_quarantine_time() {
    local pod="$1"
    kctl get pod "$pod" -o jsonpath='{.metadata.annotations.grey\.io/quarantine-time}' 2>/dev/null
}

# -----------------------------------------------------------------------------
# Quarantine Operations
# -----------------------------------------------------------------------------

# Quarantine a node
quarantine_node() {
    local node_id="$1"
    
    # Resolve pod name
    local pod
    if ! pod=$(resolve_pod_name "$node_id"); then
        die "Could not find node: $node_id"
    fi
    
    log_quarantine "Quarantining node: $pod"
    
    # Check if already quarantined
    if is_quarantined "$pod"; then
        log_warn "Node $pod is already quarantined"
        log_info "Reason: $(get_quarantine_reason "$pod")"
        log_info "Since:  $(get_quarantine_time "$pod")"
        return 0
    fi
    
    # Validate reason is provided
    if [[ -z "$REASON" ]] && [[ "$FORCE" != "true" ]]; then
        die "Quarantine reason required (use --reason='...')"
    fi
    
    local node_type
    node_type=$(get_node_type "$pod")
    log_info "Node type: $node_type"
    
    # Special handling for coordinators
    if [[ "$node_type" == "coordinator" ]]; then
        quarantine_coordinator "$pod"
    else
        quarantine_worker "$pod"
    fi
    
    # Record quarantine in API
    record_quarantine_event "$pod" "quarantine"
    
    log_success "Node $pod quarantined"
    log_info "To release: $0 release $pod"
}

# Quarantine a worker node
quarantine_worker() {
    local pod="$1"
    
    log_info "Step 1/5: Draining pending work..."
    if ! drain_node "$pod"; then
        log_warn "Drain failed — continuing with force"
    fi
    
    log_info "Step 2/5: Removing from service endpoints..."
    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would remove pod from service"
    else
        # Add label to exclude from services
        kctl label pod "$pod" "grey.io/endpoint-exclude=true" --overwrite
    fi
    
    log_info "Step 3/5: Blocking new work assignment..."
    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would block work assignment via API"
    else
        curl -sf -X POST "${API_URL}/v1/scheduler/nodes/$pod/block" \
            -H "Content-Type: application/json" \
            -d '{"reason": "'"$REASON"'"}' >/dev/null 2>&1 || true
    fi
    
    log_info "Step 4/5: Applying network isolation..."
    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would apply quarantine network policy"
    else
        apply_quarantine_network_policy "$pod"
    fi
    
    log_info "Step 5/5: Marking pod as quarantined..."
    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would add quarantine labels"
    else
        kctl label pod "$pod" "${QUARANTINE_LABEL}=true" --overwrite
        kctl annotate pod "$pod" "${QUARANTINE_ANNOTATION}=${REASON:-unspecified}" --overwrite
        kctl annotate pod "$pod" "grey.io/quarantine-time=$(timestamp)" --overwrite
        
        if [[ -n "$DURATION" ]]; then
            kctl annotate pod "$pod" "grey.io/quarantine-duration=$DURATION" --overwrite
        fi
    fi
}

# Quarantine a coordinator node
# Extra caution required — affects consensus
quarantine_coordinator() {
    local pod="$1"
    
    # Check quorum impact
    log_info "Checking quorum impact..."
    local current_members
    current_members=$(curl -sf "${API_URL}/v1/consensus/members" 2>/dev/null | jq -r 'length' || echo "0")
    
    local remaining=$((current_members - 1))
    local quorum_needed=$(( (current_members / 2) + 1 ))
    
    if (( remaining < quorum_needed )) && [[ "$FORCE" != "true" ]]; then
        die "Quarantining $pod would break quorum ($remaining < $quorum_needed needed)"
    fi
    
    # Check if this is the leader
    local leader
    leader=$(curl -sf "${API_URL}/v1/consensus/status" 2>/dev/null | jq -r '.leader' || echo "")
    
    if [[ "$leader" == "$pod" ]]; then
        log_warn "This is the current leader — will trigger election"
        if [[ "$FORCE" != "true" ]]; then
            read -rp "Continue with leader quarantine? [y/N] " confirm
            [[ "$confirm" =~ ^[Yy]$ ]] || die "Aborted by user"
        fi
    fi
    
    log_info "Step 1/6: Draining pending work..."
    drain_node "$pod" || log_warn "Drain incomplete"
    
    log_info "Step 2/6: Transferring leadership if needed..."
    if [[ "$DRY_RUN" != "true" ]] && [[ "$leader" == "$pod" ]]; then
        curl -sf -X POST "${API_URL}/v1/consensus/transfer-leadership" >/dev/null 2>&1 || true
        sleep 5
    fi
    
    log_info "Step 3/6: Removing from Raft membership..."
    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would remove from Raft"
    else
        curl -sf -X DELETE "${API_URL}/v1/consensus/members/$pod" >/dev/null 2>&1 || true
    fi
    
    log_info "Step 4/6: Removing from service endpoints..."
    if [[ "$DRY_RUN" != "true" ]]; then
        kctl label pod "$pod" "grey.io/endpoint-exclude=true" --overwrite
    fi
    
    log_info "Step 5/6: Applying network isolation..."
    if [[ "$DRY_RUN" != "true" ]]; then
        apply_quarantine_network_policy "$pod"
    fi
    
    log_info "Step 6/6: Marking pod as quarantined..."
    if [[ "$DRY_RUN" != "true" ]]; then
        kctl label pod "$pod" "${QUARANTINE_LABEL}=true" --overwrite
        kctl annotate pod "$pod" "${QUARANTINE_ANNOTATION}=${REASON:-unspecified}" --overwrite
        kctl annotate pod "$pod" "grey.io/quarantine-time=$(timestamp)" --overwrite
        kctl annotate pod "$pod" "grey.io/quarantine-node-type=coordinator" --overwrite
    fi
}

# Drain work from a node
drain_node() {
    local pod="$1"
    local timeout=60
    
    # Tell scheduler to drain
    if ! curl -sf -X POST "${API_URL}/v1/scheduler/nodes/$pod/drain" \
        -H "Content-Type: application/json" \
        -d '{"timeout_seconds": '"$timeout"'}' >/dev/null 2>&1; then
        log_warn "Drain API call failed"
        return 1
    fi
    
    # Wait for drain to complete
    local deadline=$((SECONDS + timeout))
    while (( SECONDS < deadline )); do
        local running
        running=$(curl -sf "${API_URL}/v1/scheduler/nodes/$pod/stats" 2>/dev/null | jq -r '.tasks_running // 0')
        
        if (( running == 0 )); then
            log_success "Node drained"
            return 0
        fi
        
        log_info "Waiting for $running tasks to complete..."
        sleep 5
    done
    
    log_warn "Drain timed out — $running tasks still running"
    return 1
}

# Apply network isolation
apply_quarantine_network_policy() {
    local pod="$1"
    
    # Get pod IP for targeted isolation
    local pod_ip
    pod_ip=$(kctl get pod "$pod" -o jsonpath='{.status.podIP}')
    
    local policy_name="grey-quarantine-${pod}"
    
    cat <<EOF | kctl apply -f -
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: ${policy_name}
  namespace: ${NAMESPACE}
  labels:
    grey.io/quarantine-policy: "true"
    grey.io/quarantine-target: "${pod}"
spec:
  # Apply to the quarantined pod only
  podSelector:
    matchLabels:
      statefulset.kubernetes.io/pod-name: "${pod}"
  policyTypes:
    - Ingress
    - Egress
  ingress:
    # Allow debug access from operators
    - from:
        - namespaceSelector:
            matchLabels:
              name: grey-operators
      ports:
        - port: 22
        - port: 8080
    # Allow Prometheus to continue scraping
    - from:
        - namespaceSelector:
            matchLabels:
              name: monitoring
      ports:
        - port: 9090
  egress:
    # Allow DNS
    - to:
        - namespaceSelector: {}
      ports:
        - port: 53
          protocol: UDP
    # Allow reaching storage (for forensics)
    - to:
        - podSelector:
            matchLabels:
              app: grey-storage
      ports:
        - port: 6379
EOF

    log_success "Network policy applied: $policy_name"
}

# Record quarantine event for audit
record_quarantine_event() {
    local pod="$1"
    local action="$2"
    
    local event='{
        "timestamp": "'"$(timestamp)"'",
        "action": "'"$action"'",
        "node": "'"$pod"'",
        "reason": "'"${REASON:-unspecified}"'",
        "operator": "'"${USER:-unknown}"'",
        "duration": "'"${DURATION:-indefinite}"'"
    }'
    
    curl -sf -X POST "${API_URL}/v1/audit/events" \
        -H "Content-Type: application/json" \
        -d "$event" >/dev/null 2>&1 || true
}

# -----------------------------------------------------------------------------
# Release Operations
# -----------------------------------------------------------------------------

# Release a node from quarantine
release_node() {
    local node_id="$1"
    
    local pod
    if ! pod=$(resolve_pod_name "$node_id"); then
        die "Could not find node: $node_id"
    fi
    
    if ! is_quarantined "$pod"; then
        die "Node $pod is not quarantined"
    fi
    
    log_quarantine "Releasing node from quarantine: $pod"
    
    local node_type
    node_type=$(kctl get pod "$pod" -o jsonpath='{.metadata.annotations.grey\.io/quarantine-node-type}' 2>/dev/null || echo "worker")
    
    if [[ "$node_type" == "coordinator" ]]; then
        release_coordinator "$pod"
    else
        release_worker "$pod"
    fi
    
    record_quarantine_event "$pod" "release"
    log_success "Node $pod released from quarantine"
}

# Release a worker from quarantine
release_worker() {
    local pod="$1"
    
    log_info "Step 1/4: Removing network isolation..."
    if [[ "$DRY_RUN" != "true" ]]; then
        kctl delete networkpolicy "grey-quarantine-${pod}" --ignore-not-found
    fi
    
    log_info "Step 2/4: Unblocking work assignment..."
    if [[ "$DRY_RUN" != "true" ]]; then
        curl -sf -X DELETE "${API_URL}/v1/scheduler/nodes/$pod/block" >/dev/null 2>&1 || true
    fi
    
    log_info "Step 3/4: Adding back to service endpoints..."
    if [[ "$DRY_RUN" != "true" ]]; then
        kctl label pod "$pod" "grey.io/endpoint-exclude-" --overwrite 2>/dev/null || true
    fi
    
    log_info "Step 4/4: Removing quarantine labels..."
    if [[ "$DRY_RUN" != "true" ]]; then
        kctl label pod "$pod" "${QUARANTINE_LABEL}-" --overwrite 2>/dev/null || true
        kctl annotate pod "$pod" "${QUARANTINE_ANNOTATION}-" --overwrite 2>/dev/null || true
        kctl annotate pod "$pod" "grey.io/quarantine-time-" --overwrite 2>/dev/null || true
        kctl annotate pod "$pod" "grey.io/quarantine-duration-" --overwrite 2>/dev/null || true
    fi
}

# Release a coordinator from quarantine
release_coordinator() {
    local pod="$1"
    
    log_info "Step 1/5: Removing network isolation..."
    if [[ "$DRY_RUN" != "true" ]]; then
        kctl delete networkpolicy "grey-quarantine-${pod}" --ignore-not-found
    fi
    
    log_info "Step 2/5: Adding back to Raft membership..."
    if [[ "$DRY_RUN" != "true" ]]; then
        # Get pod IP for Raft address
        local pod_ip
        pod_ip=$(kctl get pod "$pod" -o jsonpath='{.status.podIP}')
        
        curl -sf -X POST "${API_URL}/v1/consensus/members" \
            -H "Content-Type: application/json" \
            -d '{"node_id": "'"$pod"'", "address": "'"$pod_ip"':4100"}' >/dev/null 2>&1 || true
    fi
    
    log_info "Step 3/5: Waiting for Raft sync..."
    if [[ "$DRY_RUN" != "true" ]]; then
        local retries=30
        while (( retries > 0 )); do
            local state
            state=$(curl -sf "${API_URL}/v1/consensus/members/$pod" 2>/dev/null | jq -r '.state')
            
            if [[ "$state" == "follower" ]] || [[ "$state" == "leader" ]]; then
                log_success "Node rejoined Raft as $state"
                break
            fi
            
            sleep 2
            ((retries--))
        done
    fi
    
    log_info "Step 4/5: Adding back to service endpoints..."
    if [[ "$DRY_RUN" != "true" ]]; then
        kctl label pod "$pod" "grey.io/endpoint-exclude-" --overwrite 2>/dev/null || true
    fi
    
    log_info "Step 5/5: Removing quarantine labels..."
    if [[ "$DRY_RUN" != "true" ]]; then
        kctl label pod "$pod" "${QUARANTINE_LABEL}-" --overwrite 2>/dev/null || true
        kctl annotate pod "$pod" "${QUARANTINE_ANNOTATION}-" --overwrite 2>/dev/null || true
        kctl annotate pod "$pod" "grey.io/quarantine-time-" --overwrite 2>/dev/null || true
        kctl annotate pod "$pod" "grey.io/quarantine-node-type-" --overwrite 2>/dev/null || true
    fi
}

# -----------------------------------------------------------------------------
# Status and Listing
# -----------------------------------------------------------------------------

# Show status of a specific node
show_node_status() {
    local node_id="$1"
    
    local pod
    if ! pod=$(resolve_pod_name "$node_id"); then
        die "Could not find node: $node_id"
    fi
    
    echo ""
    echo "Node Status: $pod"
    echo "========================"
    
    local quarantined="No"
    if is_quarantined "$pod"; then
        quarantined="Yes"
    fi
    
    echo "Quarantined:  $quarantined"
    
    if [[ "$quarantined" == "Yes" ]]; then
        echo "Reason:       $(get_quarantine_reason "$pod")"
        echo "Since:        $(get_quarantine_time "$pod")"
        echo "Duration:     $(kctl get pod "$pod" -o jsonpath='{.metadata.annotations.grey\.io/quarantine-duration}' 2>/dev/null || echo 'indefinite')"
    fi
    
    echo ""
    echo "Pod Details:"
    kctl get pod "$pod" -o wide
    echo ""
    
    # Network policies
    echo "Quarantine Policies:"
    kctl get networkpolicy -l "grey.io/quarantine-target=$pod" 2>/dev/null || echo "  None"
    echo ""
}

# List all quarantined nodes
list_quarantined() {
    echo ""
    echo "Quarantined Nodes"
    echo "================="
    echo ""
    
    local pods
    pods=$(kctl get pods -l "${QUARANTINE_LABEL}=true" -o jsonpath='{.items[*].metadata.name}' 2>/dev/null)
    
    if [[ -z "$pods" ]]; then
        echo "No nodes currently quarantined"
        return 0
    fi
    
    printf "%-30s %-20s %-25s %s\n" "NODE" "TYPE" "SINCE" "REASON"
    printf "%-30s %-20s %-25s %s\n" "----" "----" "-----" "------"
    
    for pod in $pods; do
        local node_type
        node_type=$(kctl get pod "$pod" -o jsonpath='{.metadata.annotations.grey\.io/quarantine-node-type}' 2>/dev/null || echo "worker")
        
        local since
        since=$(get_quarantine_time "$pod")
        
        local reason
        reason=$(get_quarantine_reason "$pod")
        
        printf "%-30s %-20s %-25s %s\n" "$pod" "$node_type" "$since" "$reason"
    done
    
    echo ""
}

# -----------------------------------------------------------------------------
# Argument Parsing
# -----------------------------------------------------------------------------

parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --reason=*)
                REASON="${1#*=}"
                shift
                ;;
            --duration=*)
                DURATION="${1#*=}"
                shift
                ;;
            --dry-run)
                DRY_RUN=true
                shift
                ;;
            --force)
                FORCE=true
                shift
                ;;
            --preserve-data)
                PRESERVE_DATA=true
                shift
                ;;
            --namespace=*)
                NAMESPACE="${1#*=}"
                shift
                ;;
            *)
                ARGS+=("$1")
                shift
                ;;
        esac
    done
}

# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------

usage() {
    cat <<EOF
Usage: $0 {quarantine|release|status|list} [node-id] [options]

Commands:
  quarantine NODE   Isolate a node from the cluster
  release NODE      Release a node from quarantine
  status NODE       Show quarantine status of a node
  list              List all quarantined nodes

Options:
  --reason=TEXT     Reason for quarantine (required)
  --duration=TIME   Auto-release after duration (e.g., 30m, 2h)
  --dry-run         Show what would happen without acting
  --force           Skip safety checks
  --namespace=NS    Kubernetes namespace (default: grey-system)

Examples:
  $0 quarantine grey-worker-7 --reason="Memory leak detected"
  $0 quarantine grey-coordinator-2 --reason="Security audit" --duration=1h
  $0 release grey-worker-7
  $0 list

EOF
    exit 1
}

main() {
    ARGS=()
    parse_args "$@"
    
    require_cmd kubectl
    require_cmd jq
    require_cmd curl
    
    local action="${ARGS[0]:-}"
    local node_id="${ARGS[1]:-}"
    
    case "$action" in
        quarantine)
            [[ -n "$node_id" ]] || usage
            quarantine_node "$node_id"
            ;;
        release)
            [[ -n "$node_id" ]] || usage
            release_node "$node_id"
            ;;
        status)
            [[ -n "$node_id" ]] || usage
            show_node_status "$node_id"
            ;;
        list)
            list_quarantined
            ;;
        *)
            usage
            ;;
    esac
}

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
