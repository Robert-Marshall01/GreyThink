# Containerization Guide for input_asm

This document describes how to build and run the `input_asm` application using Docker and Kubernetes.

## Project Overview

- **Language**: x86 Assembly (NASM, 32-bit)
- **Platform**: Linux (uses `int 0x80` syscalls)
- **Type**: Interactive CLI application
- **Ports**: None (stdin/stdout only)

## Docker

### Build the Image

```bash
# Build with default tag
docker build -t input-asm:latest .

# Build with specific version
docker build -t input-asm:0.2.0 .
```

### Run the Container

Since this is an interactive CLI application, you must run with `-it` flags:

```bash
# Run interactively
docker run -it --rm input-asm:latest

# Run with specific version
docker run -it --rm input-asm:0.2.0
```

### Using Docker Compose

```bash
# Build and run
docker-compose up --build

# Run in detached mode (attach later)
docker-compose up -d
docker attach input-asm

# Clean up
docker-compose down
```

## Kubernetes

### Prerequisites

- Kubernetes cluster (minikube, kind, k3s, or cloud provider)
- `kubectl` configured to access your cluster
- Container image pushed to a registry accessible by the cluster

### Push Image to Registry

```bash
# Tag for your registry
docker tag input-asm:0.2.0 your-registry.com/input-asm:0.2.0

# Push to registry
docker push your-registry.com/input-asm:0.2.0
```

### Deploy to Kubernetes

```bash
# Apply all manifests using kustomize
kubectl apply -k k8s/

# Or apply individually
kubectl apply -f k8s/configmap.yaml
kubectl apply -f k8s/deployment.yaml
kubectl apply -f k8s/service.yaml
```

### Interact with the Pod

Since this is an interactive CLI application:

```bash
# Get pod name
kubectl get pods -l app=input-asm

# Attach to the pod for interactive input
kubectl attach -it deployment/input-asm

# Or exec into the pod
kubectl exec -it deployment/input-asm -- /app/input_asm
```

### Update Image Tag

Using kustomize, you can update the image tag:

```bash
# Edit k8s/kustomization.yaml and change newTag
# Or use kubectl to set image
kubectl set image deployment/input-asm input-asm=your-registry.com/input-asm:0.3.0
```

### Clean Up

```bash
# Remove all resources
kubectl delete -k k8s/

# Or remove individually
kubectl delete -f k8s/
```

## File Structure

```
.
├── Dockerfile              # Multi-stage Docker build
├── .dockerignore           # Files excluded from build context
├── docker-compose.yml      # Local development compose file
└── k8s/
    ├── kustomization.yaml  # Kustomize configuration
    ├── configmap.yaml      # Environment configuration
    ├── deployment.yaml     # Pod deployment specification
    └── service.yaml        # Headless service (for discovery)
```

## Security Features

- **Non-root user**: Runs as `appuser` (UID 1000)
- **Read-only filesystem**: Container filesystem is read-only
- **No privilege escalation**: Prevented at container level
- **Minimal base image**: Uses `debian:bookworm-slim`
- **Multi-stage build**: Build tools not included in runtime image
- **Dropped capabilities**: All Linux capabilities dropped

## Resource Limits

| Resource | Request | Limit |
|----------|---------|-------|
| CPU | 10m | 50m |
| Memory | 16Mi | 32Mi |
| Storage | 10Mi | 50Mi |

## Troubleshooting

### Image Build Fails

Ensure you have 32-bit support:
```bash
# Check if build succeeds
docker build --progress=plain -t input-asm:latest .
```

### Container Exits Immediately

This is expected behavior. The application waits for input. Run with `-it`:
```bash
docker run -it --rm input-asm:latest
```

### Kubernetes Pod CrashLoopBackOff

Interactive CLI pods may restart if stdin is not attached. This is normal for batch-style jobs. Consider using a Kubernetes Job instead of Deployment for one-time execution.
