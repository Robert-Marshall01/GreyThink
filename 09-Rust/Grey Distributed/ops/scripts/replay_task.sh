#!/usr/bin/env bash
# =============================================================================
# Grey Distributed — Task Replay Script
# =============================================================================
#
# PURPOSE:
#   Replay failed or completed tasks from the Grey audit log.
#   Supports single task replay, batch replay, and interactive inspection.
#
# USAGE:
#   ./replay_task.sh {replay|inspect|search|batch} [task-id] [options]
#
# EXAMPLES:
#   ./replay_task.sh replay task-abc123           # Replay a specific task
#   ./replay_task.sh inspect task-abc123          # Inspect task details
#   ./replay_task.sh search --status=failed       # Find failed tasks
#   ./replay_task.sh batch --query="status=failed AND tenant=acme"
#   ./replay_task.sh replay task-abc123 --at=2024-01-15T10:30:00Z
#
# OPTIONS:
#   --at=TIMESTAMP    Replay with state as of timestamp
#   --with-deps       Also replay dependent tasks
#   --priority=N      Override priority (1-10)
#   --tenant=ID       Filter by tenant
#   --status=STATUS   Filter by status (failed, timeout, cancelled)
#   --since=DURATION  Filter tasks from last duration (e.g., 1h, 24h)
#   --limit=N         Maximum tasks to replay (default: 100)
#   --dry-run         Show what would be replayed
#   --wait            Wait for replay to complete
#   --namespace=NS    Kubernetes namespace
#
# REPLAY MECHANICS:
#   1. Fetch original task definition from audit log
#   2. Optionally reconstruct state at point-in-time
#   3. Re-submit task to scheduler with replay metadata
#   4. Track replay progress and outcome
#   5. Record replay audit trail
#
# TRADEOFFS:
#   - Exact replay vs. current state replay
#   - Single task vs. cascade replay
#   - Idempotency requirements
#   - Resource consumption for replays
#
# WHEN TO REPLAY:
#   - Task failed due to transient error
#   - Need to reproduce a bug
#   - Testing changes with production workload
#   - Recovery from partial outage
#
# DEPENDENCIES:
#   - curl
#   - jq
#
# =============================================================================

set -euo pipefail

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
NAMESPACE="${GREY_NAMESPACE:-grey-system}"
API_URL="${GREY_API_URL:-http://localhost:8080}"
DRY_RUN=false
WAIT=false
WITH_DEPS=false
LIMIT=100
PRIORITY=""
TENANT=""
STATUS=""
SINCE=""
AT_TIMESTAMP=""

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
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

log_replay() {
    echo -e "${CYAN}[REPLAY]${NC} $*"
}

die() {
    log_error "$@"
    exit 1
}

require_cmd() {
    command -v "$1" >/dev/null 2>&1 || die "Required command not found: $1"
}

timestamp() {
    date -u +"%Y-%m-%dT%H:%M:%SZ"
}

# API helper
api_call() {
    local method="$1"
    local endpoint="$2"
    shift 2
    
    curl -sf -X "$method" "${API_URL}${endpoint}" \
        -H "Content-Type: application/json" \
        "$@"
}

# -----------------------------------------------------------------------------
# Task Information
# -----------------------------------------------------------------------------

# Fetch task details from audit log
fetch_task() {
    local task_id="$1"
    local at="${2:-}"
    
    local endpoint="/v1/audit/tasks/$task_id"
    if [[ -n "$at" ]]; then
        endpoint="${endpoint}?at=$at"
    fi
    
    api_call GET "$endpoint" 2>/dev/null
}

# Check if task exists
task_exists() {
    local task_id="$1"
    fetch_task "$task_id" >/dev/null 2>&1
}

# Get task status
get_task_status() {
    local task_id="$1"
    fetch_task "$task_id" | jq -r '.status // "unknown"'
}

# Get task dependencies
get_task_dependencies() {
    local task_id="$1"
    fetch_task "$task_id" | jq -r '.dependencies[]? // empty'
}

# Get task execution history
get_task_history() {
    local task_id="$1"
    api_call GET "/v1/audit/tasks/$task_id/history" 2>/dev/null
}

# -----------------------------------------------------------------------------
# Inspect Operations
# -----------------------------------------------------------------------------

inspect_task() {
    local task_id="$1"
    
    if ! task_exists "$task_id"; then
        die "Task not found: $task_id"
    fi
    
    local task
    task=$(fetch_task "$task_id")
    
    echo ""
    echo "Task Details: $task_id"
    echo "======================================"
    echo ""
    
    # Basic info
    echo "Status:      $(echo "$task" | jq -r '.status')"
    echo "Tenant:      $(echo "$task" | jq -r '.tenant // "default"')"
    echo "Priority:    $(echo "$task" | jq -r '.priority // 5')"
    echo "Created:     $(echo "$task" | jq -r '.created_at')"
    echo "Updated:     $(echo "$task" | jq -r '.updated_at')"
    echo ""
    
    # Task definition
    echo "Definition:"
    echo "-----------"
    echo "$task" | jq '.definition'
    echo ""
    
    # Dependencies
    local deps
    deps=$(echo "$task" | jq -r '.dependencies[]? // empty')
    if [[ -n "$deps" ]]; then
        echo "Dependencies:"
        echo "-------------"
        while IFS= read -r dep; do
            local dep_status
            dep_status=$(get_task_status "$dep")
            echo "  - $dep ($dep_status)"
        done <<< "$deps"
        echo ""
    fi
    
    # Execution history
    echo "Execution History:"
    echo "------------------"
    local history
    if history=$(get_task_history "$task_id" 2>/dev/null); then
        echo "$history" | jq -r '.executions[] | "  [\(.started_at)] \(.status) on \(.node) (\(.duration_ms)ms)"'
    else
        echo "  No execution history available"
    fi
    echo ""
    
    # Error details if failed
    local status
    status=$(echo "$task" | jq -r '.status')
    if [[ "$status" == "failed" ]]; then
        echo "Error Details:"
        echo "--------------"
        echo "$task" | jq -r '.error // "No error details"'
        echo ""
        
        echo "Stack Trace:"
        echo "------------"
        echo "$task" | jq -r '.stack_trace // "No stack trace"'
        echo ""
    fi
    
    # Replay info if replayed before
    local replayed
    replayed=$(echo "$task" | jq -r '.replay_of // empty')
    if [[ -n "$replayed" ]]; then
        echo "Replay Info:"
        echo "------------"
        echo "  Replay of: $replayed"
        echo "  Replay at: $(echo "$task" | jq -r '.replayed_at')"
        echo ""
    fi
}

# -----------------------------------------------------------------------------
# Search Operations
# -----------------------------------------------------------------------------

search_tasks() {
    log_info "Searching for tasks..."
    
    # Build query parameters
    local params=""
    
    if [[ -n "$STATUS" ]]; then
        params="${params}&status=$STATUS"
    fi
    
    if [[ -n "$TENANT" ]]; then
        params="${params}&tenant=$TENANT"
    fi
    
    if [[ -n "$SINCE" ]]; then
        params="${params}&since=$SINCE"
    fi
    
    params="${params}&limit=$LIMIT"
    params="${params#&}"  # Remove leading &
    
    local result
    result=$(api_call GET "/v1/audit/tasks?$params" 2>/dev/null)
    
    local count
    count=$(echo "$result" | jq -r '.tasks | length')
    
    echo ""
    echo "Search Results ($count tasks)"
    echo "=============================="
    echo ""
    
    if (( count == 0 )); then
        echo "No tasks found matching criteria"
        return 0
    fi
    
    printf "%-40s %-12s %-15s %-25s\n" "TASK ID" "STATUS" "TENANT" "UPDATED"
    printf "%-40s %-12s %-15s %-25s\n" "-------" "------" "------" "-------"
    
    echo "$result" | jq -r '.tasks[] | "\(.id)\t\(.status)\t\(.tenant // "default")\t\(.updated_at)"' | \
        while IFS=$'\t' read -r id status tenant updated; do
            printf "%-40s %-12s %-15s %-25s\n" "$id" "$status" "$tenant" "$updated"
        done
    
    echo ""
    
    # Return task IDs for batch processing
    echo "$result" | jq -r '.tasks[].id'
}

# -----------------------------------------------------------------------------
# Replay Operations
# -----------------------------------------------------------------------------

# Replay a single task
replay_task() {
    local task_id="$1"
    
    if ! task_exists "$task_id"; then
        die "Task not found: $task_id"
    fi
    
    log_replay "Preparing to replay task: $task_id"
    
    # Fetch original task
    local task
    if [[ -n "$AT_TIMESTAMP" ]]; then
        log_info "Reconstructing state at: $AT_TIMESTAMP"
        task=$(fetch_task "$task_id" "$AT_TIMESTAMP")
    else
        task=$(fetch_task "$task_id")
    fi
    
    # Check current status
    local current_status
    current_status=$(echo "$task" | jq -r '.status')
    
    if [[ "$current_status" == "running" ]]; then
        die "Task is currently running — cannot replay"
    fi
    
    if [[ "$current_status" == "completed" ]]; then
        log_warn "Task completed successfully — replay anyway?"
        if [[ "$DRY_RUN" != "true" ]]; then
            read -rp "Continue? [y/N] " confirm
            [[ "$confirm" =~ ^[Yy]$ ]] || die "Aborted"
        fi
    fi
    
    # Check and replay dependencies if requested
    if [[ "$WITH_DEPS" == "true" ]]; then
        log_info "Checking dependencies..."
        local deps
        deps=$(get_task_dependencies "$task_id")
        
        while IFS= read -r dep; do
            [[ -z "$dep" ]] && continue
            
            local dep_status
            dep_status=$(get_task_status "$dep")
            
            if [[ "$dep_status" != "completed" ]]; then
                log_info "Replaying dependency: $dep ($dep_status)"
                replay_task "$dep"
            fi
        done <<< "$deps"
    fi
    
    # Build replay request
    local replay_request
    replay_request=$(echo "$task" | jq '{
        original_task_id: .id,
        definition: .definition,
        tenant: .tenant,
        dependencies: .dependencies,
        replay_metadata: {
            replay_at: "'"$(timestamp)"'",
            original_status: .status,
            original_error: .error
        }
    }')
    
    # Override priority if specified
    if [[ -n "$PRIORITY" ]]; then
        replay_request=$(echo "$replay_request" | jq ".priority = $PRIORITY")
    else
        replay_request=$(echo "$replay_request" | jq '.priority = '"$(echo "$task" | jq '.priority // 5')")
    fi
    
    # Show what would be replayed
    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would submit replay:"
        echo "$replay_request" | jq .
        return 0
    fi
    
    # Submit replay
    log_info "Submitting replay..."
    local result
    result=$(api_call POST "/v1/scheduler/replay" -d "$replay_request")
    
    local new_task_id
    new_task_id=$(echo "$result" | jq -r '.task_id')
    
    log_success "Replay submitted: $new_task_id"
    
    # Wait for completion if requested
    if [[ "$WAIT" == "true" ]]; then
        wait_for_task "$new_task_id"
    fi
    
    echo "$new_task_id"
}

# Batch replay multiple tasks
batch_replay() {
    local query="$1"
    
    log_replay "Batch replay with query: $query"
    
    # Search for matching tasks
    local task_ids
    task_ids=$(search_tasks 2>/dev/null | tail -n +1)
    
    local count
    count=$(echo "$task_ids" | wc -l)
    
    if (( count == 0 )); then
        log_info "No tasks to replay"
        return 0
    fi
    
    log_info "Found $count tasks to replay"
    
    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would replay:"
        echo "$task_ids" | head -20
        if (( count > 20 )); then
            echo "  ... and $((count - 20)) more"
        fi
        return 0
    fi
    
    # Confirm batch replay
    read -rp "Replay all $count tasks? [y/N] " confirm
    [[ "$confirm" =~ ^[Yy]$ ]] || die "Aborted"
    
    # Submit batch
    local success=0
    local failed=0
    
    while IFS= read -r task_id; do
        [[ -z "$task_id" ]] && continue
        
        log_info "Replaying: $task_id"
        if replay_task "$task_id" >/dev/null 2>&1; then
            ((success++))
        else
            log_warn "Failed to replay: $task_id"
            ((failed++))
        fi
    done <<< "$task_ids"
    
    echo ""
    log_info "Batch replay complete:"
    log_info "  Success: $success"
    log_info "  Failed:  $failed"
}

# Wait for task completion
wait_for_task() {
    local task_id="$1"
    local timeout="${2:-300}"
    
    log_info "Waiting for task $task_id..."
    
    local deadline=$((SECONDS + timeout))
    while (( SECONDS < deadline )); do
        local status
        status=$(api_call GET "/v1/scheduler/tasks/$task_id/status" 2>/dev/null | jq -r '.status')
        
        case "$status" in
            completed)
                log_success "Task completed successfully"
                return 0
                ;;
            failed)
                log_error "Task failed"
                return 1
                ;;
            cancelled)
                log_warn "Task was cancelled"
                return 1
                ;;
            running|pending)
                log_info "Status: $status"
                sleep 5
                ;;
            *)
                log_warn "Unknown status: $status"
                sleep 5
                ;;
        esac
    done
    
    log_error "Timeout waiting for task"
    return 1
}

# -----------------------------------------------------------------------------
# Replay Analysis
# -----------------------------------------------------------------------------

# Analyze a failed task for replay viability
analyze_task() {
    local task_id="$1"
    
    log_info "Analyzing task for replay viability: $task_id"
    
    if ! task_exists "$task_id"; then
        die "Task not found: $task_id"
    fi
    
    local task
    task=$(fetch_task "$task_id")
    
    echo ""
    echo "Replay Viability Analysis"
    echo "========================="
    echo ""
    
    local viable=true
    local warnings=()
    
    # Check idempotency
    local idempotent
    idempotent=$(echo "$task" | jq -r '.definition.idempotent // false')
    if [[ "$idempotent" != "true" ]]; then
        warnings+=("Task is not marked idempotent — replay may have side effects")
    fi
    
    # Check for external dependencies
    local has_external
    has_external=$(echo "$task" | jq -r '.definition.external_calls // [] | length > 0')
    if [[ "$has_external" == "true" ]]; then
        warnings+=("Task makes external calls — ensure external systems can handle replay")
    fi
    
    # Check state dependencies
    local state_deps
    state_deps=$(echo "$task" | jq -r '.definition.state_dependencies // [] | length')
    if (( state_deps > 0 )); then
        log_info "Task has $state_deps state dependencies"
        
        # Check if state is still available
        local state_available
        state_available=$(api_call GET "/v1/state/check?task_id=$task_id" 2>/dev/null | jq -r '.available // false')
        
        if [[ "$state_available" != "true" ]]; then
            warnings+=("Required state may not be available — consider point-in-time replay")
        fi
    fi
    
    # Check error type
    local error_type
    error_type=$(echo "$task" | jq -r '.error.type // "unknown"')
    
    case "$error_type" in
        transient|timeout|resource_exhausted)
            log_info "Error type '$error_type' is typically recoverable"
            ;;
        validation|permission|not_found)
            warnings+=("Error type '$error_type' may not be resolved by replay")
            viable=false
            ;;
        *)
            warnings+=("Unknown error type — investigate before replay")
            ;;
    esac
    
    # Check retry count
    local retries
    retries=$(echo "$task" | jq -r '.retry_count // 0')
    if (( retries >= 3 )); then
        warnings+=("Task already retried $retries times")
    fi
    
    # Display analysis
    if [[ "$viable" == "true" ]]; then
        log_success "Replay appears viable"
    else
        log_warn "Replay may not be effective"
    fi
    
    if (( ${#warnings[@]} > 0 )); then
        echo ""
        echo "Warnings:"
        for w in "${warnings[@]}"; do
            echo "  ⚠ $w"
        done
    fi
    
    echo ""
    echo "Recommendations:"
    if [[ -n "$AT_TIMESTAMP" ]] || (( state_deps > 0 )); then
        echo "  • Consider using --at=TIMESTAMP for point-in-time replay"
    fi
    if [[ "$idempotent" != "true" ]]; then
        echo "  • Verify replay won't duplicate side effects"
    fi
    if (( retries >= 3 )); then
        echo "  • Investigate root cause before replaying again"
    fi
    echo ""
}

# -----------------------------------------------------------------------------
# Argument Parsing
# -----------------------------------------------------------------------------

parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --at=*)
                AT_TIMESTAMP="${1#*=}"
                shift
                ;;
            --with-deps)
                WITH_DEPS=true
                shift
                ;;
            --priority=*)
                PRIORITY="${1#*=}"
                shift
                ;;
            --tenant=*)
                TENANT="${1#*=}"
                shift
                ;;
            --status=*)
                STATUS="${1#*=}"
                shift
                ;;
            --since=*)
                SINCE="${1#*=}"
                shift
                ;;
            --limit=*)
                LIMIT="${1#*=}"
                shift
                ;;
            --dry-run)
                DRY_RUN=true
                shift
                ;;
            --wait)
                WAIT=true
                shift
                ;;
            --namespace=*)
                NAMESPACE="${1#*=}"
                shift
                ;;
            --query=*)
                QUERY="${1#*=}"
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
Usage: $0 {replay|inspect|search|batch|analyze} [task-id] [options]

Commands:
  replay TASK-ID    Replay a specific task
  inspect TASK-ID   Show detailed task information
  search            Search for tasks by criteria
  batch             Batch replay matching tasks
  analyze TASK-ID   Analyze task for replay viability

Options:
  --at=TIMESTAMP    Replay with state as of timestamp
  --with-deps       Also replay dependent tasks
  --priority=N      Override priority (1-10)
  --tenant=ID       Filter by tenant
  --status=STATUS   Filter by status (failed, timeout, cancelled)
  --since=DURATION  Filter tasks from last duration (e.g., 1h, 24h)
  --limit=N         Maximum tasks (default: 100)
  --dry-run         Show what would be replayed
  --wait            Wait for replay to complete
  --query=EXPR      Query expression for batch replay

Examples:
  $0 replay task-abc123
  $0 replay task-abc123 --at=2024-01-15T10:30:00Z --wait
  $0 inspect task-abc123
  $0 search --status=failed --since=1h
  $0 batch --status=failed --tenant=acme
  $0 analyze task-abc123

EOF
    exit 1
}

main() {
    ARGS=()
    parse_args "$@"
    
    require_cmd curl
    require_cmd jq
    
    local action="${ARGS[0]:-}"
    local task_id="${ARGS[1]:-}"
    
    case "$action" in
        replay)
            [[ -n "$task_id" ]] || usage
            replay_task "$task_id"
            ;;
        inspect)
            [[ -n "$task_id" ]] || usage
            inspect_task "$task_id"
            ;;
        search)
            search_tasks
            ;;
        batch)
            batch_replay "${QUERY:-}"
            ;;
        analyze)
            [[ -n "$task_id" ]] || usage
            analyze_task "$task_id"
            ;;
        *)
            usage
            ;;
    esac
}

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
