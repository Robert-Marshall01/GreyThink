# MoneyJava - Containerization Guide

This document provides instructions for running the MoneyJava application using Docker and Kubernetes.

## Project Overview

MoneyJava is a Java Swing desktop application for personal finance management. Since it's a GUI application, running it in containers requires X11 display forwarding.

## Prerequisites

- Docker 20.10+ and Docker Compose 2.0+
- For Kubernetes: kubectl and a running cluster
- For GUI display: X11 server (Linux native, XQuartz on macOS, VcXsrv on Windows)

---

## Docker

### Build the Image

```bash
docker build -t moneyjava:v1.0 .
```

### Run with Docker (Linux)

```bash
# Allow X11 access for Docker
xhost +local:docker

# Run the container
docker run -it --rm \
  -e DISPLAY=$DISPLAY \
  -v /tmp/.X11-unix:/tmp/.X11-unix:rw \
  -v moneyjava-data:/app/data \
  --name moneyjava \
  moneyjava:v1.0

# Revoke X11 access when done
xhost -local:docker
```

### Run with Docker Compose

```bash
# Allow X11 access
xhost +local:docker

# Start the application
docker-compose up

# Stop the application
docker-compose down
```

---

## Kubernetes

### Deploy to Kubernetes

```bash
# Apply all manifests
kubectl apply -f k8s/

# Or apply individually
kubectl apply -f k8s/configmap.yaml
kubectl apply -f k8s/pvc.yaml
kubectl apply -f k8s/deployment.yaml
kubectl apply -f k8s/service.yaml
```

### Check Deployment Status

```bash
kubectl get pods -l app=moneyjava
kubectl logs -f deployment/moneyjava
```

### Delete Deployment

```bash
kubectl delete -f k8s/
```

---

## Notes

### GUI Applications in Containers

Running Java Swing GUI applications in containers requires X11 forwarding:

- **Linux**: Native X11, use `DISPLAY` environment variable and mount `/tmp/.X11-unix`
- **macOS**: Install XQuartz, run `xhost + 127.0.0.1`
- **Windows**: Install VcXsrv or similar X server

### Data Persistence

User data (`users.dat`) is stored in the `/app/data` directory which is:
- Mounted as a Docker volume (`moneyjava-data`)
- Backed by a PersistentVolumeClaim in Kubernetes

### Kubernetes GUI Considerations

Running GUI apps in Kubernetes typically requires:
- VNC server in the container, or
- X11 forwarding to nodes, or
- Virtual framebuffer (Xvfb) for headless operation

For production use, consider converting the application to a web-based interface.

---

## File Structure

```
MoneyJava/
├── Dockerfile              # Multi-stage Docker build
├── docker-compose.yml      # Docker Compose configuration
├── .dockerignore           # Docker build exclusions
├── k8s/
│   ├── deployment.yaml     # Kubernetes Deployment
│   ├── service.yaml        # Kubernetes Service
│   ├── configmap.yaml      # Configuration settings
│   └── pvc.yaml            # Persistent Volume Claim
└── CONTAINERIZATION.md     # This file
```
