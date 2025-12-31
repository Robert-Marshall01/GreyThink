#!/bin/bash
# Kubernetes deployment script for biz_intelli

set -e

NAMESPACE="biz-intelli"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "=== Deploying biz_intelli to Kubernetes ==="

# Create namespace
echo "Creating namespace..."
kubectl apply -f "$SCRIPT_DIR/namespace.yaml"

# Create secrets (IMPORTANT: Update secret.yaml with real passwords first!)
echo "Creating secrets..."
kubectl apply -f "$SCRIPT_DIR/secret.yaml"

# Create configmaps
echo "Creating configmaps..."
kubectl apply -f "$SCRIPT_DIR/configmap.yaml"

# Create persistent volume claim
echo "Creating PVC..."
kubectl apply -f "$SCRIPT_DIR/pvc.yaml"

# Deploy MySQL
echo "Deploying MySQL..."
kubectl apply -f "$SCRIPT_DIR/deployment.yaml"

# Create services
echo "Creating services..."
kubectl apply -f "$SCRIPT_DIR/service.yaml"

echo ""
echo "=== Deployment Complete ==="
echo ""
echo "To check status:"
echo "  kubectl get pods -n $NAMESPACE"
echo "  kubectl get svc -n $NAMESPACE"
echo ""
echo "To connect to MySQL from within the cluster:"
echo "  kubectl run -it --rm mysql-client --image=mysql:8.0 --restart=Never -n $NAMESPACE -- mysql -h mysql -u bi_user -p"
echo ""
echo "To port-forward for local access:"
echo "  kubectl port-forward svc/mysql 3306:3306 -n $NAMESPACE"
