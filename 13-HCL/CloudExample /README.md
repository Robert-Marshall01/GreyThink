# Cloud Example - Infrastructure as Code

This project provides infrastructure as code for deploying a containerized application. It supports both **local development** (Docker/kind/minikube) and **Azure cloud** deployment.

> ⚠️ **Disclaimer:** This project has undergone limited testing and may contain bugs. Use at your own risk and thoroughly test before using in production environments.

## 📋 Overview

### Local Development
- **Docker** - Containerization with Docker Compose
- **Kubernetes (kind/minikube)** - Local Kubernetes cluster
- **PostgreSQL** - Local database via Docker

### Azure Cloud Deployment
- **Azure Kubernetes Service (AKS)** - Managed Kubernetes cluster with auto-scaling
- **Azure Database for PostgreSQL Flexible Server** - Managed PostgreSQL with high availability
- **Azure Key Vault** - Secure secrets management
- **Azure Log Analytics** - Centralized logging and monitoring

## 🗂️ Project Structure

```
├── docker/                     # Docker configuration (local dev)
│   ├── Dockerfile              # Application container definition
│   ├── docker-compose.yaml     # Docker Compose for quick local dev
│   └── public/                 # Static files for sample app
│       └── index.html
│
├── kubernetes-local/           # Kubernetes manifests (local dev)
│   ├── namespace.yaml          # Kubernetes namespace definition
│   ├── deployments.yaml        # App and PostgreSQL deployments
│   ├── services.yaml           # Service definitions
│   ├── ingress.yaml            # Ingress configuration
│   └── kind-config.yaml        # Kind cluster configuration
│
├── kubernetes/                 # Kubernetes manifests (Azure)
│   ├── namespace.yaml          # Namespace definition
│   ├── deployment.yaml         # Application deployment
│   ├── service.yaml            # Service exposure
│   ├── configmap-secret.yaml   # Configuration and secrets
│   ├── ingress.yaml            # Ingress controller rules
│   ├── autoscaling.yaml        # Horizontal Pod Autoscaler
│   ├── network-policy.yaml     # Network security policies
│   ├── rbac.yaml               # Role-based access control
│   └── resource-quotas.yaml    # Resource limits
│
├── terraform/                  # Terraform for Azure
│   ├── main.tf                 # Main Azure resources
│   ├── variables.tf            # Input variables
│   └── outputs.tf              # Output values
│
├── terraform-local/            # Terraform for local dev
│   └── database.tf             # Local PostgreSQL via Docker
│
└── scripts/
    └── setup-local.sh          # Automated local setup script
```

---

# 🏠 Local Development

Run everything locally without any cloud provider dependencies.

## 🚀 Quick Start (Local)

### Option 1: Automated Setup (Recommended)

```bash
# Make the script executable
chmod +x scripts/setup-local.sh

# Run the setup script
./scripts/setup-local.sh
```

### Option 2: Docker Compose Only

For quick development without Kubernetes:

```bash
cd docker
docker-compose up -d

# Access at http://localhost:8080
# PostgreSQL at localhost:5433 (mapped from container's 5432)
```

### Option 3: Manual Kubernetes Setup

#### Prerequisites

- Docker installed and running
- kind installed: `brew install kind` (macOS) or [installation guide](https://kind.sigs.k8s.io/)
- kubectl installed: `brew install kubectl` (macOS)

#### Steps

1. **Create the kind cluster:**
   ```bash
   kind create cluster --config kubernetes-local/kind-config.yaml --name local-dev
   ```

2. **Install NGINX Ingress Controller:**
   ```bash
   kubectl apply -f https://raw.githubusercontent.com/kubernetes/ingress-nginx/main/deploy/static/provider/kind/deploy.yaml
   
   # Wait for it to be ready
   kubectl wait --namespace ingress-nginx \
     --for=condition=ready pod \
     --selector=app.kubernetes.io/component=controller \
     --timeout=90s
   ```

3. **Build and load the Docker image:**
   ```bash
   cd docker
   docker build -t sample-app:latest .
   kind load docker-image sample-app:latest --name local-dev
   ```

4. **Deploy Kubernetes resources:**
   ```bash
   cd ../kubernetes-local
   kubectl apply -f namespace.yaml
   kubectl apply -f deployments.yaml
   kubectl apply -f services.yaml
   kubectl apply -f ingress.yaml
   ```

5. **Update /etc/hosts:**
   ```bash
   echo "127.0.0.1 sample-app.local api.sample-app.local" | sudo tee -a /etc/hosts
   ```

### Option 4: Terraform Local Database

Use Terraform to provision a local PostgreSQL container:

```bash
cd terraform-local
terraform init
terraform apply

# Get connection details
terraform output -json
```

## 🌐 Local Access Points

| Service | URL | Description |
|---------|-----|-------------|
| Application (Ingress) | `http://localhost` | Via NGINX Ingress |
| Application (NodePort) | `http://localhost:30080` | Direct NodePort access |
| Application (hostname) | `http://sample-app.local` | Requires /etc/hosts entry |
| Application (Compose) | `http://localhost:8080` | Docker Compose only |

## 🗄️ Local Port Reference

| Resource | Port | Protocol | Description |
|----------|------|----------|-------------|
| App Container | 80 | TCP | NGINX internal port |
| App Service | 80 | TCP | ClusterIP service port |
| App NodePort | 30080 | TCP | External access port |
| Ingress HTTP | 80 | TCP | Ingress controller port |
| Ingress HTTPS | 443 | TCP | Ingress controller TLS port |
| PostgreSQL | 5432 | TCP | Database port |

## 🧹 Local Cleanup

```bash
# Using script
./scripts/setup-local.sh --destroy

# Or manually
kind delete cluster --name local-dev

# Clean up Docker Compose
cd docker && docker-compose down -v

# Clean up Terraform
cd terraform-local && terraform destroy
```

---

# ☁️ Azure Cloud Deployment

## 🚀 Prerequisites

Before you begin, ensure you have the following installed:

1. **Azure CLI** (v2.50+)
   ```bash
   # Install on Linux
   curl -sL https://aka.ms/InstallAzureCLIDeb | sudo bash
   
   # Verify installation
   az --version
   ```

2. **Terraform** (v1.0+)
   ```bash
   # Install on Linux (Ubuntu/Debian)
   wget -O- https://apt.releases.hashicorp.com/gpg | sudo gpg --dearmor -o /usr/share/keyrings/hashicorp-archive-keyring.gpg
   echo "deb [signed-by=/usr/share/keyrings/hashicorp-archive-keyring.gpg] https://apt.releases.hashicorp.com $(lsb_release -cs) main" | sudo tee /etc/apt/sources.list.d/hashicorp.list
   sudo apt update && sudo apt install terraform
   
   # Verify installation
   terraform --version
   ```

3. **kubectl** (v1.28+)
   ```bash
   # Install on Linux
   curl -LO "https://dl.k8s.io/release/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl"
   sudo install -o root -g root -m 0755 kubectl /usr/local/bin/kubectl
   
   # Verify installation
   kubectl version --client
   ```

4. **Azure Subscription** with required permissions

## 🔧 Quick Start

### Step 1: Authenticate with Azure

```bash
# Login to Azure
az login

# Set your subscription (if you have multiple)
az account set --subscription "<your-subscription-id>"

# Verify the active subscription
az account show
```

### Step 2: Deploy Infrastructure with Terraform

```bash
# Navigate to the terraform directory
cd terraform

# Initialize Terraform (downloads providers)
terraform init

# Review the execution plan
terraform plan

# Apply the configuration (creates Azure resources)
terraform apply

# When prompted, type 'yes' to confirm
```

**Expected output:** Terraform will display the created resources and output values including the AKS cluster name and PostgreSQL server details.

### Step 3: Configure kubectl

```bash
# Get AKS credentials (updates ~/.kube/config)
az aks get-credentials \
  --resource-group rg-cloud-example \
  --name aks-cloud-example

# Verify connection to the cluster
kubectl cluster-info
kubectl get nodes
```

### Step 4: Deploy Kubernetes Manifests

```bash
# Navigate to the kubernetes directory
cd ../kubernetes

# Apply manifests in order
kubectl apply -f namespace.yaml
kubectl apply -f rbac.yaml
kubectl apply -f configmap-secret.yaml
kubectl apply -f network-policy.yaml
kubectl apply -f resource-quotas.yaml
kubectl apply -f deployment.yaml
kubectl apply -f service.yaml
kubectl apply -f autoscaling.yaml
kubectl apply -f ingress.yaml

# Or apply all at once
kubectl apply -f .
```

## ✅ Testing the Deployment

### 1. Verify Terraform Resources

```bash
cd terraform

# Check Terraform state
terraform show

# List specific resources
terraform state list
```

### 2. Verify Kubernetes Resources

```bash
# Check all resources in the sample-app namespace
kubectl get all -n sample-app

# Expected output:
# - 3 pods running (sample-app deployment)
# - 1 service (sample-app-service)
# - 1 deployment (sample-app)
# - 1 replicaset
```

### 3. Test Pod Health

```bash
# Check pod status
kubectl get pods -n sample-app -o wide

# View pod logs
kubectl logs -n sample-app -l app=sample-app --tail=50

# Check pod details
kubectl describe pod -n sample-app -l app=sample-app
```

### 4. Test Service Connectivity

```bash
# Port-forward to access the application locally
kubectl port-forward -n sample-app svc/sample-app-service 8080:80

# In another terminal, test the endpoint
curl http://localhost:8080

# Or open in browser: http://localhost:8080
```

### 5. Verify Horizontal Pod Autoscaler

```bash
# Check HPA status
kubectl get hpa -n sample-app

# Watch HPA metrics
kubectl get hpa -n sample-app -w
```

### 6. Run Integration Tests

```bash
# Check if pods are ready
kubectl wait --for=condition=ready pod -l app=sample-app -n sample-app --timeout=120s

# Verify deployment rollout status
kubectl rollout status deployment/sample-app -n sample-app

# Test network connectivity between pods
kubectl run test-pod --image=busybox --rm -it --restart=Never -n sample-app -- wget -qO- http://sample-app-service:80
```

## 🛠️ Common Operations

### Scaling

```bash
# Manually scale the deployment
kubectl scale deployment sample-app -n sample-app --replicas=5

# Check scaling status
kubectl get pods -n sample-app
```

### Updating the Application

```bash
# Update container image
kubectl set image deployment/sample-app sample-app=nginx:1.26-alpine -n sample-app

# Watch rollout status
kubectl rollout status deployment/sample-app -n sample-app

# Rollback if needed
kubectl rollout undo deployment/sample-app -n sample-app
```

### Viewing Logs

```bash
# Stream logs from all pods
kubectl logs -f -n sample-app -l app=sample-app

# View logs from a specific pod
kubectl logs -n sample-app <pod-name>
```

## 🧹 Cleanup

### Remove Kubernetes Resources

```bash
# Delete all resources in the namespace
kubectl delete -f kubernetes/

# Or delete the entire namespace
kubectl delete namespace sample-app
```

### Destroy Terraform Infrastructure

```bash
cd terraform

# Preview what will be destroyed
terraform plan -destroy

# Destroy all resources
terraform destroy

# When prompted, type 'yes' to confirm
```

> ⚠️ **Warning:** This will permanently delete all Azure resources including the database and any stored data.

## 📝 Configuration

### Terraform Variables

Customize deployment by creating a `terraform.tfvars` file:

```hcl
# terraform/terraform.tfvars
resource_group_name = "rg-my-app-prod"
location            = "westeurope"

# AKS Configuration
aks_cluster_name    = "aks-my-app-prod"
aks_node_count      = 3
aks_vm_size         = "Standard_DS3_v2"

# PostgreSQL Configuration
postgres_server_name = "psql-my-app-prod"
postgres_sku_name    = "GP_Standard_D4s_v3"

# Tags
tags = {
  Environment = "Production"
  Project     = "MyApp"
  Owner       = "platform-team"
}
```

### Environment-Specific Deployments

```bash
# Development
terraform apply -var-file="environments/dev.tfvars"

# Production
terraform apply -var-file="environments/prod.tfvars"
```

## 🔒 Security Notes

1. **Secrets Management:** In production, use Azure Key Vault with the CSI driver instead of Kubernetes Secrets
2. **Network Policies:** Network policies are enabled by default to restrict pod-to-pod communication
3. **RBAC:** Azure RBAC is enabled for AKS authentication
4. **SSL/TLS:** PostgreSQL connections require SSL

## 📊 Monitoring

Access logs and metrics in Azure Portal:

1. Navigate to **Log Analytics Workspace** → `log-rg-cloud-example`
2. Run queries against AKS and PostgreSQL logs
3. Set up **Azure Monitor** alerts for critical metrics

## ❓ Troubleshooting

### Pods Not Starting

```bash
# Check pod events
kubectl describe pod -n sample-app -l app=sample-app

# Check resource quotas
kubectl describe resourcequota -n sample-app
```

### Terraform Errors

```bash
# Refresh state
terraform refresh

# Unlock state if locked
terraform force-unlock <lock-id>
```

### AKS Connection Issues

```bash
# Re-authenticate
az login
az aks get-credentials --resource-group rg-cloud-example --name aks-cloud-example --overwrite-existing
```

## 📚 Additional Resources

- [Azure AKS Documentation](https://docs.microsoft.com/azure/aks/)
- [Terraform Azure Provider](https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs)
- [Kubernetes Documentation](https://kubernetes.io/docs/)

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
