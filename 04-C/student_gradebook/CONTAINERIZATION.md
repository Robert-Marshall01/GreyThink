# Student Gradebook - Containerization

## Project Structure

```
student_gradebook/
├── student_gradebook.c      # Main application source
├── Dockerfile               # Multi-stage Docker build
├── .dockerignore            # Files excluded from Docker context
├── docker-compose.yml       # Docker Compose configuration
└── k8s/                     # Kubernetes manifests
    ├── kustomization.yaml   # Kustomize configuration
    ├── namespace.yaml       # Dedicated namespace
    ├── configmap.yaml       # Application configuration
    ├── deployment.yaml      # Kubernetes Deployment
    ├── service.yaml         # Kubernetes Service (headless)
    └── job.yaml             # Alternative: Run as one-time Job
```

## Docker Usage

### Build the Image

```bash
# Build production image (minimal scratch-based)
docker build -t student-gradebook:1.0.0 .

# Build debug image (with shell access)
docker build --target debug -t student-gradebook:1.0.0-debug .
```

### Run the Container

Since this is an interactive CLI application, use `-it` flags:

```bash
# Run interactively with the debug image
docker run -it --rm student-gradebook:1.0.0-debug

# Using docker-compose
docker-compose run --rm gradebook
```

## Kubernetes Usage

### Deploy with Kustomize

```bash
# Apply all manifests
kubectl apply -k k8s/

# Or apply individually
kubectl apply -f k8s/namespace.yaml
kubectl apply -f k8s/configmap.yaml
kubectl apply -f k8s/deployment.yaml
kubectl apply -f k8s/service.yaml
```

### Interactive Access

```bash
# Access the running pod interactively
kubectl -n gradebook exec -it deployment/student-gradebook -- /app/student_gradebook

# Or run a one-time interactive pod
kubectl run -n gradebook -it --rm gradebook-cli \
  --image=student-gradebook:1.0.0 \
  --restart=Never \
  -- /app/student_gradebook
```

### Run as a Job

```bash
# Run as a one-time job (non-interactive demo)
kubectl apply -f k8s/job.yaml
kubectl -n gradebook logs job/student-gradebook-job
```

## Image Details

| Stage | Base Image | Size | Use Case |
|-------|------------|------|----------|
| `production` | `scratch` | ~1MB | Minimal production image (static binary) |
| `debug` | `alpine:3.19` | ~7MB | Development/debugging with shell access |

## Security Features

- ✅ Multi-stage build for minimal attack surface
- ✅ Non-root user execution (UID 1000)
- ✅ Read-only root filesystem
- ✅ Dropped all Linux capabilities
- ✅ No privilege escalation allowed
- ✅ Resource limits enforced
- ✅ Service account token not mounted

## Notes

- This is a CLI application that requires interactive terminal input
- The scratch-based production image has no shell (use debug stage for troubleshooting)
- For Kubernetes, the deployment keeps the pod running with `sleep infinity` for interactive exec access
