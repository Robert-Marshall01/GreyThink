#!/bin/bash
# =============================================================================
# VERIFICATION SCRIPT - Local Kubernetes + PostgreSQL Setup
# =============================================================================
# This script verifies that the local Kubernetes environment is properly
# configured and all components are functioning correctly.
#
# Usage: ./verify-setup.sh
#
# Checks performed:
#   1. Kubernetes cluster connectivity
#   2. Pod status verification
#   3. Service exposure and port validation
#   4. Ingress routing test
#   5. PostgreSQL database connectivity and query execution
#
# Exit codes:
#   0 - All checks passed
#   1 - One or more checks failed

# =============================================================================
# STRICT MODE AND CONFIGURATION
# =============================================================================

# Enable strict error handling
set -uo pipefail  # Exit on undefined vars, pipe failures (but not -e, we handle errors manually)

# Namespace where resources are deployed
readonly NAMESPACE="local-dev"

# Expected service ports
readonly APP_SERVICE_PORT=80
readonly APP_NODEPORT=30080
readonly POSTGRES_PORT=5432

# Ingress hostnames to test
readonly APP_HOSTNAME="sample-app.local"
readonly LOCALHOST="localhost"

# Database credentials (from Kubernetes secret - base64 decoded)
readonly DB_USER="appuser"
readonly DB_NAME="appdb"

# Timeout settings (in seconds)
readonly CURL_TIMEOUT=5
readonly KUBECTL_TIMEOUT=10

# =============================================================================
# COLOR CODES FOR OUTPUT
# =============================================================================

readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly BOLD='\033[1m'
readonly NC='\033[0m'  # No Color

# =============================================================================
# COUNTERS FOR FINAL SUMMARY
# =============================================================================

TESTS_PASSED=0
TESTS_FAILED=0

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Print a section header
print_header() {
    echo ""
    echo -e "${BOLD}${BLUE}========================================${NC}"
    echo -e "${BOLD}${BLUE}  $1${NC}"
    echo -e "${BOLD}${BLUE}========================================${NC}"
    echo ""
}

# Print a pass message and increment counter
pass() {
    echo -e "${GREEN}✅ PASS:${NC} $1"
    ((TESTS_PASSED++))
}

# Print a fail message and increment counter
fail() {
    echo -e "${RED}❌ FAIL:${NC} $1"
    ((TESTS_FAILED++))
}

# Print an info message
info() {
    echo -e "${BLUE}ℹ️  INFO:${NC} $1"
}

# Print a warning message
warn() {
    echo -e "${YELLOW}⚠️  WARN:${NC} $1"
}

# Print command being executed (for transparency)
print_cmd() {
    echo -e "${YELLOW}   → Running:${NC} $1"
}

# =============================================================================
# CHECK 1: KUBERNETES CLUSTER CONNECTIVITY
# =============================================================================

check_cluster_connectivity() {
    print_header "Check 1: Kubernetes Cluster Connectivity"
    
    # Verify kubectl can communicate with the cluster
    # 'kubectl cluster-info' returns cluster endpoint details
    print_cmd "kubectl cluster-info"
    
    if kubectl cluster-info --request-timeout="${KUBECTL_TIMEOUT}s" &> /dev/null; then
        pass "kubectl can connect to the Kubernetes cluster"
        
        # Get cluster name for additional info
        local cluster_name
        cluster_name=$(kubectl config current-context 2>/dev/null)
        info "Current context: ${cluster_name}"
    else
        fail "Cannot connect to Kubernetes cluster"
        info "Ensure kind/minikube is running: 'kind get clusters' or 'minikube status'"
        return 1
    fi
    
    # Verify the target namespace exists
    # 'kubectl get namespace' checks if namespace is created
    print_cmd "kubectl get namespace ${NAMESPACE}"
    
    if kubectl get namespace "${NAMESPACE}" --request-timeout="${KUBECTL_TIMEOUT}s" &> /dev/null; then
        pass "Namespace '${NAMESPACE}' exists"
    else
        fail "Namespace '${NAMESPACE}' does not exist"
        info "Create it with: kubectl apply -f kubernetes-local/namespace.yaml"
        return 1
    fi
}

# =============================================================================
# CHECK 2: POD STATUS VERIFICATION
# =============================================================================

check_pods_running() {
    print_header "Check 2: Pod Status Verification"
    
    # Get all pods in the namespace
    # -o wide shows additional details like node and IP
    print_cmd "kubectl get pods -n ${NAMESPACE}"
    echo ""
    kubectl get pods -n "${NAMESPACE}" -o wide 2>/dev/null || true
    echo ""
    
    # Check sample-app pods are Running
    # jsonpath extracts the phase field from pod status
    print_cmd "Checking sample-app pod status..."
    
    local app_pods_ready
    # Count pods with 'Running' status for app=sample-app
    app_pods_ready=$(kubectl get pods -n "${NAMESPACE}" \
        -l app=sample-app \
        --field-selector=status.phase=Running \
        --no-headers 2>/dev/null | wc -l)
    
    if [[ ${app_pods_ready} -ge 1 ]]; then
        pass "sample-app pods are running (${app_pods_ready} replica(s))"
    else
        fail "sample-app pods are not running"
        # Show pod events for debugging
        info "Checking pod events for debugging..."
        kubectl describe pods -n "${NAMESPACE}" -l app=sample-app 2>/dev/null | grep -A 5 "Events:" || true
        return 1
    fi
    
    # Check postgres pod is Running
    print_cmd "Checking postgres pod status..."
    
    local postgres_pods_ready
    postgres_pods_ready=$(kubectl get pods -n "${NAMESPACE}" \
        -l app=postgres \
        --field-selector=status.phase=Running \
        --no-headers 2>/dev/null | wc -l)
    
    if [[ ${postgres_pods_ready} -ge 1 ]]; then
        pass "postgres pod is running"
    else
        fail "postgres pod is not running"
        info "Checking pod events for debugging..."
        kubectl describe pods -n "${NAMESPACE}" -l app=postgres 2>/dev/null | grep -A 5 "Events:" || true
        return 1
    fi
    
    # Check all pods are in Ready condition
    # This ensures containers passed their readiness probes
    print_cmd "Verifying all pods have Ready condition..."
    
    local not_ready_pods
    not_ready_pods=$(kubectl get pods -n "${NAMESPACE}" \
        --no-headers 2>/dev/null | grep -v "1/1\|2/2" | wc -l)
    
    if [[ ${not_ready_pods} -eq 0 ]]; then
        pass "All pods have all containers ready"
    else
        warn "Some pods have containers not ready (${not_ready_pods} pod(s))"
        kubectl get pods -n "${NAMESPACE}" --no-headers 2>/dev/null | grep -v "1/1\|2/2" || true
    fi
}

# =============================================================================
# CHECK 3: SERVICE EXPOSURE AND PORT VALIDATION
# =============================================================================

check_services() {
    print_header "Check 3: Service Exposure and Port Validation"
    
    # List all services in the namespace
    print_cmd "kubectl get services -n ${NAMESPACE}"
    echo ""
    kubectl get services -n "${NAMESPACE}" -o wide 2>/dev/null || true
    echo ""
    
    # Verify sample-app-service exists and has correct port
    # ClusterIP service should expose port 80
    print_cmd "Checking sample-app-service configuration..."
    
    local app_service_port
    # Extract the port number from the service spec using jsonpath
    app_service_port=$(kubectl get service sample-app-service \
        -n "${NAMESPACE}" \
        -o jsonpath='{.spec.ports[0].port}' 2>/dev/null)
    
    if [[ "${app_service_port}" == "${APP_SERVICE_PORT}" ]]; then
        pass "sample-app-service is exposed on port ${APP_SERVICE_PORT}"
    else
        fail "sample-app-service port mismatch (expected: ${APP_SERVICE_PORT}, got: ${app_service_port:-not found})"
    fi
    
    # Verify NodePort service exists and has correct nodePort
    print_cmd "Checking sample-app-nodeport configuration..."
    
    local nodeport_port
    nodeport_port=$(kubectl get service sample-app-nodeport \
        -n "${NAMESPACE}" \
        -o jsonpath='{.spec.ports[0].nodePort}' 2>/dev/null)
    
    if [[ "${nodeport_port}" == "${APP_NODEPORT}" ]]; then
        pass "sample-app-nodeport exposes NodePort ${APP_NODEPORT}"
    else
        fail "sample-app-nodeport mismatch (expected: ${APP_NODEPORT}, got: ${nodeport_port:-not found})"
    fi
    
    # Verify postgres-service exists and has correct port
    print_cmd "Checking postgres-service configuration..."
    
    local postgres_service_port
    postgres_service_port=$(kubectl get service postgres-service \
        -n "${NAMESPACE}" \
        -o jsonpath='{.spec.ports[0].port}' 2>/dev/null)
    
    if [[ "${postgres_service_port}" == "${POSTGRES_PORT}" ]]; then
        pass "postgres-service is exposed on port ${POSTGRES_PORT}"
    else
        fail "postgres-service port mismatch (expected: ${POSTGRES_PORT}, got: ${postgres_service_port:-not found})"
    fi
    
    # Verify services have endpoints (pods are actually backing the service)
    print_cmd "Checking service endpoints..."
    
    local app_endpoints
    app_endpoints=$(kubectl get endpoints sample-app-service \
        -n "${NAMESPACE}" \
        -o jsonpath='{.subsets[*].addresses[*].ip}' 2>/dev/null)
    
    if [[ -n "${app_endpoints}" ]]; then
        pass "sample-app-service has active endpoints: ${app_endpoints}"
    else
        fail "sample-app-service has no endpoints (no pods backing the service)"
    fi
    
    local postgres_endpoints
    postgres_endpoints=$(kubectl get endpoints postgres-service \
        -n "${NAMESPACE}" \
        -o jsonpath='{.subsets[*].addresses[*].ip}' 2>/dev/null)
    
    if [[ -n "${postgres_endpoints}" ]]; then
        pass "postgres-service has active endpoints: ${postgres_endpoints}"
    else
        fail "postgres-service has no endpoints (no pods backing the service)"
    fi
}

# =============================================================================
# CHECK 4: INGRESS ROUTING TEST
# =============================================================================

check_ingress() {
    print_header "Check 4: Ingress Routing Test"
    
    # Display ingress configuration
    print_cmd "kubectl get ingress -n ${NAMESPACE}"
    echo ""
    kubectl get ingress -n "${NAMESPACE}" -o wide 2>/dev/null || true
    echo ""
    
    # Check if ingress controller is running in ingress-nginx namespace
    print_cmd "Checking ingress controller status..."
    
    local ingress_ready
    ingress_ready=$(kubectl get pods -n ingress-nginx \
        -l app.kubernetes.io/component=controller \
        --field-selector=status.phase=Running \
        --no-headers 2>/dev/null | wc -l)
    
    if [[ ${ingress_ready} -ge 1 ]]; then
        pass "NGINX Ingress Controller is running"
    else
        warn "NGINX Ingress Controller may not be installed"
        info "Install with: kubectl apply -f https://raw.githubusercontent.com/kubernetes/ingress-nginx/main/deploy/static/provider/kind/deploy.yaml"
    fi
    
    # Test ingress via localhost (works with kind port mapping)
    # curl -s: silent mode, -o /dev/null: discard output, -w: custom output format
    print_cmd "Testing HTTP request to http://${LOCALHOST}/ via ingress..."
    
    local http_code
    http_code=$(curl -s -o /dev/null -w "%{http_code}" \
        --connect-timeout "${CURL_TIMEOUT}" \
        --max-time "${CURL_TIMEOUT}" \
        "http://${LOCALHOST}/" 2>/dev/null || echo "000")
    
    if [[ "${http_code}" == "200" ]]; then
        pass "Ingress routing works via localhost (HTTP ${http_code})"
    elif [[ "${http_code}" == "000" ]]; then
        fail "Cannot connect to ingress (connection refused or timeout)"
        info "Ensure kind cluster has port mappings configured"
    else
        warn "Ingress returned HTTP ${http_code} (expected 200)"
    fi
    
    # Test via NodePort as fallback
    print_cmd "Testing HTTP request via NodePort http://${LOCALHOST}:${APP_NODEPORT}/..."
    
    local nodeport_http_code
    nodeport_http_code=$(curl -s -o /dev/null -w "%{http_code}" \
        --connect-timeout "${CURL_TIMEOUT}" \
        --max-time "${CURL_TIMEOUT}" \
        "http://${LOCALHOST}:${APP_NODEPORT}/" 2>/dev/null || echo "000")
    
    if [[ "${nodeport_http_code}" == "200" ]]; then
        pass "NodePort routing works (HTTP ${nodeport_http_code})"
    elif [[ "${nodeport_http_code}" == "000" ]]; then
        warn "Cannot connect via NodePort (may need kind port mapping for 30080)"
    else
        warn "NodePort returned HTTP ${nodeport_http_code}"
    fi
    
    # Test if sample-app.local resolves (requires /etc/hosts entry)
    print_cmd "Testing if ${APP_HOSTNAME} is configured in /etc/hosts..."
    
    if grep -q "${APP_HOSTNAME}" /etc/hosts 2>/dev/null; then
        pass "${APP_HOSTNAME} is configured in /etc/hosts"
        
        # Test the actual hostname
        local hostname_http_code
        hostname_http_code=$(curl -s -o /dev/null -w "%{http_code}" \
            --connect-timeout "${CURL_TIMEOUT}" \
            --max-time "${CURL_TIMEOUT}" \
            "http://${APP_HOSTNAME}/" 2>/dev/null || echo "000")
        
        if [[ "${hostname_http_code}" == "200" ]]; then
            pass "Ingress routing works via ${APP_HOSTNAME} (HTTP ${hostname_http_code})"
        else
            warn "Request to ${APP_HOSTNAME} returned HTTP ${hostname_http_code}"
        fi
    else
        warn "${APP_HOSTNAME} not found in /etc/hosts"
        info "Add with: echo '127.0.0.1 ${APP_HOSTNAME}' | sudo tee -a /etc/hosts"
    fi
}

# =============================================================================
# CHECK 5: POSTGRESQL DATABASE CONNECTIVITY
# =============================================================================

check_postgres() {
    print_header "Check 5: PostgreSQL Database Connectivity"
    
    # Get the postgres pod name for exec commands
    # We use -o jsonpath to extract just the pod name
    print_cmd "Getting postgres pod name..."
    
    local postgres_pod
    postgres_pod=$(kubectl get pods -n "${NAMESPACE}" \
        -l app=postgres \
        -o jsonpath='{.items[0].metadata.name}' 2>/dev/null)
    
    if [[ -z "${postgres_pod}" ]]; then
        fail "Cannot find postgres pod"
        return 1
    fi
    
    info "Found postgres pod: ${postgres_pod}"
    
    # Test PostgreSQL connectivity using pg_isready
    # pg_isready is a utility to check PostgreSQL server connection
    print_cmd "kubectl exec ${postgres_pod} -- pg_isready -U ${DB_USER} -d ${DB_NAME}"
    
    if kubectl exec -n "${NAMESPACE}" "${postgres_pod}" -- \
        pg_isready -U "${DB_USER}" -d "${DB_NAME}" &> /dev/null; then
        pass "PostgreSQL is accepting connections"
    else
        fail "PostgreSQL is not accepting connections"
        return 1
    fi
    
    # Run a simple query to verify database functionality
    # 'SELECT 1' is a minimal query to test database responsiveness
    print_cmd "kubectl exec ${postgres_pod} -- psql -U ${DB_USER} -d ${DB_NAME} -c 'SELECT 1;'"
    echo ""
    
    local query_result
    query_result=$(kubectl exec -n "${NAMESPACE}" "${postgres_pod}" -- \
        psql -U "${DB_USER}" -d "${DB_NAME}" -t -c "SELECT 1;" 2>/dev/null)
    
    # Trim whitespace and check if result is '1'
    query_result=$(echo "${query_result}" | tr -d '[:space:]')
    
    if [[ "${query_result}" == "1" ]]; then
        pass "PostgreSQL query 'SELECT 1' returned expected result"
    else
        fail "PostgreSQL query failed or returned unexpected result: '${query_result}'"
        return 1
    fi
    
    # Test database version query for additional validation
    print_cmd "Querying PostgreSQL version..."
    
    local pg_version
    pg_version=$(kubectl exec -n "${NAMESPACE}" "${postgres_pod}" -- \
        psql -U "${DB_USER}" -d "${DB_NAME}" -t -c "SELECT version();" 2>/dev/null | head -1)
    
    if [[ -n "${pg_version}" ]]; then
        pass "PostgreSQL version query successful"
        info "Version: $(echo "${pg_version}" | xargs)"  # xargs trims whitespace
    else
        warn "Could not retrieve PostgreSQL version"
    fi
    
    # Test creating and dropping a temporary table
    # This validates write permissions and DDL execution
    print_cmd "Testing database write capability (create/drop temp table)..."
    
    local ddl_result
    ddl_result=$(kubectl exec -n "${NAMESPACE}" "${postgres_pod}" -- \
        psql -U "${DB_USER}" -d "${DB_NAME}" -c "
            CREATE TEMP TABLE verify_test (id INT);
            INSERT INTO verify_test VALUES (1);
            SELECT id FROM verify_test;
            DROP TABLE verify_test;
        " 2>&1)
    
    if echo "${ddl_result}" | grep -q "1 row" || echo "${ddl_result}" | grep -q "INSERT 0 1"; then
        pass "Database write operations work correctly"
    else
        warn "Database write test had unexpected output"
    fi
}

# =============================================================================
# SUMMARY
# =============================================================================

print_summary() {
    print_header "Verification Summary"
    
    local total_tests=$((TESTS_PASSED + TESTS_FAILED))
    
    echo -e "  ${GREEN}Passed:${NC} ${TESTS_PASSED}"
    echo -e "  ${RED}Failed:${NC} ${TESTS_FAILED}"
    echo -e "  ${BOLD}Total:${NC}  ${total_tests}"
    echo ""
    
    if [[ ${TESTS_FAILED} -eq 0 ]]; then
        echo -e "${GREEN}${BOLD}🎉 All checks passed! Your local environment is ready.${NC}"
        echo ""
        echo "Access your application at:"
        echo "  • http://localhost (via Ingress)"
        echo "  • http://localhost:${APP_NODEPORT} (via NodePort)"
        echo "  • http://${APP_HOSTNAME} (if /etc/hosts configured)"
        echo ""
        return 0
    else
        echo -e "${RED}${BOLD}⚠️  Some checks failed. Review the errors above.${NC}"
        echo ""
        echo "Common fixes:"
        echo "  • Run: ./scripts/setup-local.sh"
        echo "  • Check pods: kubectl get pods -n ${NAMESPACE}"
        echo "  • View logs: kubectl logs -n ${NAMESPACE} -l app=sample-app"
        echo ""
        return 1
    fi
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

main() {
    echo ""
    echo -e "${BOLD}╔══════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BOLD}║  Local Kubernetes + PostgreSQL Verification Script           ║${NC}"
    echo -e "${BOLD}║  Namespace: ${NAMESPACE}                                          ║${NC}"
    echo -e "${BOLD}╚══════════════════════════════════════════════════════════════╝${NC}"
    
    # Run all checks
    # Each check function handles its own errors and continues
    check_cluster_connectivity
    check_pods_running
    check_services
    check_ingress
    check_postgres
    
    # Print final summary and exit with appropriate code
    print_summary
}

# Execute main function
main "$@"
