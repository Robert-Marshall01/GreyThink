# Age Verification - Containerization Guide

This document provides instructions for building and deploying the Age Verification CLI application using Docker and Kubernetes.

## Project Analysis

| Attribute | Value |
|-----------|-------|
| Language | JavaScript (Node.js ES6+) |
| Runtime | Node.js 18+ |
| Dependencies | prompt-sync |
| Type | Interactive CLI Application |
| Ports | None (CLI app) |

## Files Generated

```
age_verification/
├── Dockerfile              # Multi-stage Docker build
├── docker-compose.yml      # Local development/testing
├── .dockerignore           # Docker build exclusions
├── package.json            # Node.js dependencies
└── k8s/
    ├── kustomization.yaml  # Kustomize configuration
    ├── namespace.yaml      # Kubernetes namespace
    ├── configmap.yaml      # Configuration data
    ├── deployment.yaml     # Application deployment
    └── service.yaml        # Headless service (CLI app)
```

## Docker Usage

### Build the Image

```bash
docker build -t age-verification:1.0.0 .
```

### Run Interactively

Since this is a CLI application requiring user input, run with `-it` flags:

```bash
docker run -it --rm age-verification:1.0.0
```

### Using Docker Compose

```bash
# Build and run
docker-compose run --rm age-verification

# Build only
docker-compose build
```

## Kubernetes Deployment

### Prerequisites

- Kubernetes cluster (minikube, kind, or cloud provider)
- kubectl configured
- Container image pushed to a registry

### Deploy with Kustomize

```bash
# Apply all resources
kubectl apply -k k8s/

# Check deployment status
kubectl get pods -n age-verification

# View logs
kubectl logs -n age-verification -l app=age-verification
```

### Interactive Access

To interact with the CLI application in Kubernetes:

```bash
# Attach to running pod
kubectl attach -it -n age-verification deployment/age-verification

# Or exec into the pod
kubectl exec -it -n age-verification deployment/age-verification -- node age_verification.js
```

### Cleanup

```bash
kubectl delete -k k8s/
```

## Security Features

- **Non-root user**: Container runs as UID 1001
- **Read-only filesystem**: Root filesystem is read-only
- **No privilege escalation**: Prevents privilege escalation attacks
- **Dropped capabilities**: All Linux capabilities dropped
- **Resource limits**: CPU and memory limits defined
- **No service account mounting**: Token not auto-mounted

## Notes

- This is an interactive CLI application, not a web service
- No HTTP endpoints are exposed
- Liveness/readiness probes are not applicable
- The Kubernetes Service is headless since no ports are exposed
