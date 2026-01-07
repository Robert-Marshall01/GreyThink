#!/bin/bash
# =============================================================================
# LOCAL DEVELOPMENT SETUP SCRIPT
# =============================================================================
# This script sets up the complete local development environment
# including Docker, Kubernetes (kind), and all required resources
#
# Usage: ./setup-local.sh [--destroy]
#
# Prerequisites:
#   - Docker installed and running
#   - kind installed (https://kind.sigs.k8s.io/docs/user/quick-start/#installation)
#   - kubectl installed
#   - terraform installed (optional, for database.tf)

# SECURITY: Strict bash settings
set -euo pipefail  # Exit on error, undefined vars, pipe failures
IFS=$'\n\t'        # Safer word splitting

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
# SECURITY: Use readonly for constants
readonly CLUSTER_NAME="local-dev"
readonly NAMESPACE="local-dev"
# shellcheck disable=SC2155
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly SCRIPT_DIR
# shellcheck disable=SC2155
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
readonly PROJECT_DIR

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

check_prerequisites() {
    log_info "Checking prerequisites..."
    
    # Check Docker
    if ! command -v docker &> /dev/null; then
        log_error "Docker is not installed. Please install Docker first."
        exit 1
    fi
    
    if ! docker info &> /dev/null; then
        log_error "Docker daemon is not running. Please start Docker."
        exit 1
    fi
    
    # Check kind
    if ! command -v kind &> /dev/null; then
        log_error "kind is not installed. Install from: https://kind.sigs.k8s.io/docs/user/quick-start/#installation"
        exit 1
    fi
    
    # Check kubectl
    if ! command -v kubectl &> /dev/null; then
        log_error "kubectl is not installed. Please install kubectl."
        exit 1
    fi
    
    log_success "All prerequisites met!"
}

# =============================================================================
# CLUSTER MANAGEMENT
# =============================================================================

create_cluster() {
    log_info "Creating kind cluster: ${CLUSTER_NAME}..."
    
    # Check if cluster already exists
    if kind get clusters 2>/dev/null | grep -q "^${CLUSTER_NAME}$"; then
        log_warn "Cluster ${CLUSTER_NAME} already exists. Skipping creation."
        return
    fi
    
    # Create cluster with config
    kind create cluster --config "${PROJECT_DIR}/kubernetes-local/kind-config.yaml" --name "${CLUSTER_NAME}"
    
    log_success "Cluster ${CLUSTER_NAME} created successfully!"
}

install_ingress() {
    log_info "Installing NGINX Ingress Controller..."
    
    # Install ingress-nginx for kind
    kubectl apply -f https://raw.githubusercontent.com/kubernetes/ingress-nginx/main/deploy/static/provider/kind/deploy.yaml
    
    log_info "Waiting for ingress controller to be ready..."
    kubectl wait --namespace ingress-nginx \
        --for=condition=ready pod \
        --selector=app.kubernetes.io/component=controller \
        --timeout=120s
    
    log_success "Ingress controller installed and ready!"
}

# =============================================================================
# APPLICATION DEPLOYMENT
# =============================================================================

build_docker_image() {
    log_info "Building Docker image..."
    
    cd "${PROJECT_DIR}/docker"
    # SECURITY: Use specific version tag instead of 'latest'
    docker build -t sample-app:1.0.0 .
    
    log_success "Docker image built successfully!"
}

load_image_to_kind() {
    log_info "Loading Docker image into kind cluster..."
    
    # SECURITY: Use specific version tag
    kind load docker-image sample-app:1.0.0 --name "${CLUSTER_NAME}"
    
    log_success "Image loaded into kind!"
}

deploy_kubernetes_resources() {
    log_info "Deploying Kubernetes resources..."
    
    cd "${PROJECT_DIR}/kubernetes-local"
    
    # Apply namespace first
    kubectl apply -f namespace.yaml
    
    # Wait for namespace to be ready
    kubectl wait --for=jsonpath='{.status.phase}'=Active namespace/${NAMESPACE} --timeout=30s
    
    # Apply deployments (includes ConfigMap, Secret, PVC)
    kubectl apply -f deployments.yaml
    
    # Apply services
    kubectl apply -f services.yaml
    
    # Apply ingress
    kubectl apply -f ingress.yaml
    
    log_success "Kubernetes resources deployed!"
}

wait_for_pods() {
    log_info "Waiting for pods to be ready..."
    
    # Wait for sample-app pods
    kubectl wait --namespace ${NAMESPACE} \
        --for=condition=ready pod \
        --selector=app=sample-app \
        --timeout=120s
    
    # Wait for postgres pod
    kubectl wait --namespace ${NAMESPACE} \
        --for=condition=ready pod \
        --selector=app=postgres \
        --timeout=120s
    
    log_success "All pods are ready!"
}

# =============================================================================
# STATUS AND INFO
# =============================================================================

show_status() {
    echo ""
    echo "=========================================="
    echo "  Local Development Environment Status"
    echo "=========================================="
    echo ""
    
    log_info "Cluster: ${CLUSTER_NAME}"
    echo ""
    
    log_info "Pods:"
    kubectl get pods -n ${NAMESPACE}
    echo ""
    
    log_info "Services:"
    kubectl get services -n ${NAMESPACE}
    echo ""
    
    log_info "Ingress:"
    kubectl get ingress -n ${NAMESPACE}
    echo ""
    
    echo "=========================================="
    echo "  Access URLs"
    echo "=========================================="
    echo ""
    echo "  Application (via Ingress):  http://localhost"
    echo "  Application (via NodePort): http://localhost:30080"
    echo "  API endpoint:               http://api.sample-app.local"
    echo ""
    echo "  Note: Add these to /etc/hosts:"
    echo "    127.0.0.1 sample-app.local"
    echo "    127.0.0.1 api.sample-app.local"
    echo ""
    echo "=========================================="
    echo "  Useful Commands"
    echo "=========================================="
    echo ""
    echo "  View logs:     kubectl logs -n ${NAMESPACE} -l app=sample-app -f"
    echo "  Shell into:    kubectl exec -n ${NAMESPACE} -it deploy/sample-app -- /bin/sh"
    echo "  Port forward:  kubectl port-forward -n ${NAMESPACE} svc/sample-app-service 8080:80"
    echo "  DB shell:      kubectl exec -n ${NAMESPACE} -it deploy/postgres -- psql -U appuser -d appdb"
    echo ""
}

# =============================================================================
# CLEANUP
# =============================================================================

destroy_cluster() {
    log_warn "Destroying cluster ${CLUSTER_NAME}..."
    
    if kind get clusters 2>/dev/null | grep -q "^${CLUSTER_NAME}$"; then
        kind delete cluster --name "${CLUSTER_NAME}"
        log_success "Cluster ${CLUSTER_NAME} destroyed!"
    else
        log_warn "Cluster ${CLUSTER_NAME} does not exist."
    fi
}

# =============================================================================
# MAIN
# =============================================================================

main() {
    echo ""
    echo "=========================================="
    echo "  Local Cloud-Native Development Setup"
    echo "=========================================="
    echo ""
    
    # Handle --destroy flag
    # Use ${1:-} to provide empty default when no argument is passed (prevents unbound variable error)
    if [[ "${1:-}" == "--destroy" ]]; then
        destroy_cluster
        exit 0
    fi
    
    # Run setup steps
    check_prerequisites
    create_cluster
    install_ingress
    build_docker_image
    load_image_to_kind
    deploy_kubernetes_resources
    wait_for_pods
    show_status
    
    log_success "Setup complete! Your local environment is ready."
}

# Run main function
main "$@"
