#!/bin/bash
#--------------------------------------------------------------
# Kubernetes Deployment Script for Digital Organism
# compliance-as-code | digital organism metaphor
#
# Usage:
#   ./deploy.sh              # Deploy all resources
#   ./deploy.sh build        # Build Docker image only
#   ./deploy.sh apply        # Run Terraform job
#   ./deploy.sh scan         # Run compliance scan
#   ./deploy.sh destroy      # Remove all resources
#   ./deploy.sh logs         # View pod logs
#--------------------------------------------------------------

set -e

NAMESPACE="compliance-system"
IMAGE_NAME="compliance-terraform"
IMAGE_TAG="latest"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

# Build Docker image
build_image() {
    log_info "🐳 Building Docker image: ${IMAGE_NAME}:${IMAGE_TAG}"
    docker build -t "${IMAGE_NAME}:${IMAGE_TAG}" .
    log_success "Docker image built successfully"
}

# Deploy to Kubernetes
deploy() {
    log_info "🚀 Deploying Digital Organism to Kubernetes"
    
    # Check for required secrets
    if ! kubectl get secret postgres-credentials -n ${NAMESPACE} &>/dev/null; then
        log_warn "Secrets not found. Creating from environment variables..."
        
        if [ -z "$TF_VAR_postgres_password" ] || [ -z "$TF_VAR_compliance_user_password" ]; then
            log_error "Please set TF_VAR_postgres_password and TF_VAR_compliance_user_password"
            exit 1
        fi
        
        kubectl create namespace ${NAMESPACE} --dry-run=client -o yaml | kubectl apply -f -
        
        kubectl create secret generic postgres-credentials \
            --from-literal=username="${TF_VAR_postgres_username:-admin}" \
            --from-literal=password="${TF_VAR_postgres_password}" \
            --from-literal=compliance-username="compliance_user" \
            --from-literal=compliance-password="${TF_VAR_compliance_user_password}" \
            -n ${NAMESPACE} \
            --dry-run=client -o yaml | kubectl apply -f -
        
        log_success "Secrets created"
    fi
    
    # Apply Kustomize resources
    log_info "Applying Kubernetes resources..."
    kubectl apply -k k8s/
    
    log_success "Deployment complete!"
    log_info "Waiting for PostgreSQL to be ready..."
    kubectl wait --for=condition=ready pod -l app=postgres -n ${NAMESPACE} --timeout=120s
    
    log_success "🧬 Digital Organism deployed!"
}

# Run Terraform apply job
run_terraform() {
    log_info "🔧 Running Terraform apply job"
    
    # Delete existing job if present
    kubectl delete job terraform-apply -n ${NAMESPACE} --ignore-not-found
    
    # Apply the job
    kubectl apply -f k8s/terraform-job.yaml
    
    # Wait and show logs
    log_info "Waiting for job to start..."
    sleep 5
    kubectl logs -f job/terraform-apply -n ${NAMESPACE} || true
}

# Run compliance scan
run_scan() {
    log_info "🛡️ Running compliance scan"
    
    # Trigger the cronjob manually
    kubectl create job --from=cronjob/compliance-scan manual-scan-$(date +%s) -n ${NAMESPACE}
    
    log_info "Scan job created. View logs with: $0 logs"
}

# View logs
view_logs() {
    log_info "📋 Viewing pod logs"
    
    echo ""
    echo "=== PostgreSQL Logs ==="
    kubectl logs -l app=postgres -n ${NAMESPACE} --tail=20 || true
    
    echo ""
    echo "=== Recent Terraform Jobs ==="
    kubectl logs -l app=terraform -n ${NAMESPACE} --tail=50 || true
}

# Get status
status() {
    log_info "📊 Digital Organism Status"
    
    echo ""
    echo "=== Pods ==="
    kubectl get pods -n ${NAMESPACE}
    
    echo ""
    echo "=== Services ==="
    kubectl get svc -n ${NAMESPACE}
    
    echo ""
    echo "=== Jobs ==="
    kubectl get jobs -n ${NAMESPACE}
    
    echo ""
    echo "=== CronJobs ==="
    kubectl get cronjobs -n ${NAMESPACE}
}

# Destroy all resources
destroy() {
    log_warn "🗑️ Destroying Digital Organism"
    read -p "Are you sure? This will delete all data! (y/N) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        kubectl delete -k k8s/ --ignore-not-found
        kubectl delete namespace ${NAMESPACE} --ignore-not-found
        log_success "All resources destroyed"
    else
        log_info "Cancelled"
    fi
}

# Port forward for local access
port_forward() {
    log_info "🔌 Port forwarding PostgreSQL to localhost:5432"
    kubectl port-forward svc/postgres-service 5432:5432 -n ${NAMESPACE}
}

# Main
case "${1:-deploy}" in
    build)
        build_image
        ;;
    deploy)
        build_image
        deploy
        ;;
    apply|terraform)
        run_terraform
        ;;
    scan)
        run_scan
        ;;
    logs)
        view_logs
        ;;
    status)
        status
        ;;
    destroy|delete)
        destroy
        ;;
    forward|port-forward)
        port_forward
        ;;
    *)
        echo "Usage: $0 {build|deploy|apply|scan|logs|status|destroy|forward}"
        exit 1
        ;;
esac
