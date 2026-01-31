# Grey AI Internal - Deployment Guide

This document covers deploying Grey AI Internal using Docker Compose and Kubernetes.

## Table of Contents

- [Docker Compose (Local/Dev)](#docker-compose-localdev)
- [Kubernetes Deployment](#kubernetes-deployment)
- [GPU Support](#gpu-support)
- [Troubleshooting](#troubleshooting)

---

## Docker Compose (Local/Dev)

### Prerequisites

- Docker Engine 20.10+
- Docker Compose v2.0+
- 16GB RAM minimum (8GB for SD model)
- 50GB disk space (for model caches)

### Quick Start

```bash
# Clone and navigate to project
cd "Grey AI Internal"

# Copy environment example (customize as needed)
cp .env.example .env

# Start all services
docker-compose up -d

# Check status
docker-compose ps

# View logs
docker-compose logs -f
```

### Services

| Service | Port | Description |
|---------|------|-------------|
| Frontend | 3000 | React app with nginx |
| Backend | 8000 | FastAPI server |
| PostgreSQL | 5433 | Database |
| Ollama | 11434 | LLM API |
| Stable Diffusion | 7860 | Image generation |

### First-Time Setup

After starting, pull the LLM model:

```bash
# Pull the default model (llama3)
docker exec grey-ai-ollama ollama pull llama3

# Or use a smaller model for testing
docker exec grey-ai-ollama ollama pull tinyllama
```

The Stable Diffusion model (~4GB) downloads automatically on first request.

---

## Kubernetes Deployment

### Prerequisites

- Kubernetes cluster (1.25+)
- kubectl configured
- Container registry access
- Ingress controller (nginx-ingress recommended)
- cert-manager (optional, for TLS)

### Build and Push Images

```bash
# Set your registry
export REGISTRY=your-registry.com

# Build images
docker build -t $REGISTRY/grey-ai-backend:latest ./backend
docker build -t $REGISTRY/grey-ai-frontend:latest ./frontend
docker build -t $REGISTRY/grey-ai-sd:latest ./stable-diffusion

# Push to registry
docker push $REGISTRY/grey-ai-backend:latest
docker push $REGISTRY/grey-ai-frontend:latest
docker push $REGISTRY/grey-ai-sd:latest
```

### Deploy with Kustomize

```bash
# Development environment (minimal resources)
kubectl apply -k k8s/overlays/development

# Production environment (GPU enabled, scaled)
kubectl apply -k k8s/overlays/production

# Or deploy base configuration
kubectl apply -k k8s/
```

### Verify Deployment

```bash
# Check namespace
kubectl get all -n grey-ai

# Watch pods come up
kubectl get pods -n grey-ai -w

# Check services
kubectl get svc -n grey-ai

# Check ingress
kubectl get ingress -n grey-ai
```

### Configure Ingress

1. Update the domain in [k8s/frontend.yaml](k8s/frontend.yaml):
   ```yaml
   spec:
     rules:
       - host: grey-ai.yourdomain.com
   ```

2. For TLS, uncomment the `tls` section and configure cert-manager.

### Pull LLM Model

```bash
# Get the Ollama pod name
OLLAMA_POD=$(kubectl get pods -n grey-ai -l app=ollama -o jsonpath='{.items[0].metadata.name}')

# Pull model
kubectl exec -n grey-ai $OLLAMA_POD -- ollama pull llama3
```

### Update Secrets

**Important:** Change the database password in production:

```bash
# Generate a new password
NEW_PASSWORD=$(openssl rand -base64 32)
echo "New password: $NEW_PASSWORD"

# Create base64 encoded secret
echo -n "$NEW_PASSWORD" | base64

# Update k8s/secret.yaml with the new base64 value
```

---

## GPU Support

### Docker Compose

Uncomment GPU sections in `docker-compose.yml`:

```yaml
stable-diffusion:
  deploy:
    resources:
      reservations:
        devices:
          - driver: nvidia
            count: 1
            capabilities: [gpu]
```

Set `USE_CPU=false` in `.env`:

```bash
USE_CPU=false
```

### Kubernetes

Use the production overlay which enables GPU:

```bash
kubectl apply -k k8s/overlays/production
```

**Requirements:**
- NVIDIA GPU on nodes
- NVIDIA device plugin installed
- Nodes labeled with `nvidia.com/gpu=true`

Install NVIDIA device plugin:

```bash
kubectl apply -f https://raw.githubusercontent.com/NVIDIA/k8s-device-plugin/v0.14.1/nvidia-device-plugin.yml
```

---

## Configuration

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `POSTGRES_PASSWORD` | `greyai_secret_change_me` | Database password |
| `OLLAMA_MODEL` | `llama3` | LLM model to use |
| `SD_MODEL_ID` | `runwayml/stable-diffusion-v1-5` | SD model |
| `SD_TIMEOUT` | `600` | Image generation timeout (seconds) |
| `USE_CPU` | `true` | Run SD on CPU (slower) |
| `DEBUG` | `false` | Enable debug mode |

### Kubernetes ConfigMap

Edit [k8s/configmap.yaml](k8s/configmap.yaml) for non-sensitive settings.

### Kubernetes Secrets

Edit [k8s/secret.yaml](k8s/secret.yaml) for sensitive settings.

---

## Troubleshooting

### Docker Compose

```bash
# View all logs
docker-compose logs -f

# View specific service
docker-compose logs -f backend

# Restart a service
docker-compose restart backend

# Rebuild and restart
docker-compose up -d --build backend

# Full reset
docker-compose down -v
docker-compose up -d --build
```

### Kubernetes

```bash
# Describe pod for events
kubectl describe pod <pod-name> -n grey-ai

# View logs
kubectl logs <pod-name> -n grey-ai

# Follow logs
kubectl logs -f <pod-name> -n grey-ai

# Exec into pod
kubectl exec -it <pod-name> -n grey-ai -- /bin/sh

# Check events
kubectl get events -n grey-ai --sort-by='.lastTimestamp'
```

### Common Issues

**502 Bad Gateway**
- Check if backend is healthy: `kubectl get pods -n grey-ai`
- Check backend logs: `kubectl logs -l app=backend -n grey-ai`

**Image Generation Timeout**
- CPU inference is slow (~6 min for 10 steps)
- Increase `SD_TIMEOUT` in ConfigMap
- Enable GPU for 100x speedup

**Model Download Slow**
- First startup downloads ~4GB for SD
- Ensure PVC is attached correctly
- Check network connectivity

**Database Connection Failed**
- Wait for postgres pod to be ready
- Check secret values match configmap

---

## Architecture

```
                                    ┌─────────────────┐
                                    │    Ingress      │
                                    │  (nginx-ingress)│
                                    └────────┬────────┘
                                             │
                    ┌────────────────────────┼────────────────────────┐
                    │                        │                        │
                    ▼                        ▼                        ▼
           ┌───────────────┐        ┌───────────────┐        ┌───────────────┐
           │   Frontend    │        │    Backend    │        │   API Docs    │
           │   (React)     │───────▶│   (FastAPI)   │◀───────│   (/docs)     │
           └───────────────┘        └───────┬───────┘        └───────────────┘
                                            │
                    ┌───────────────────────┼───────────────────────┐
                    │                       │                       │
                    ▼                       ▼                       ▼
           ┌───────────────┐       ┌───────────────┐       ┌───────────────┐
           │   PostgreSQL  │       │    Ollama     │       │ Stable Diffusion│
           │  (Database)   │       │    (LLM)      │       │   (Images)     │
           └───────────────┘       └───────────────┘       └───────────────┘
```

---

## Monitoring (Optional)

For production, consider adding:

- **Prometheus + Grafana** for metrics
- **Loki** for log aggregation
- **Jaeger** for distributed tracing

Example Prometheus ServiceMonitor:

```yaml
apiVersion: monitoring.coreos.com/v1
kind: ServiceMonitor
metadata:
  name: grey-ai-backend
  namespace: grey-ai
spec:
  selector:
    matchLabels:
      app: backend
  endpoints:
    - port: http
      path: /metrics
```
