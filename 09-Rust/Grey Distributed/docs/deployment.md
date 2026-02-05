# Grey Distributed — Deployment Guide

This document covers deployment strategies from single-node development to global-scale production.

---

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Single-Node Development](#single-node-development)
3. [Local Multi-Node Cluster](#local-multi-node-cluster)
4. [Production Deployment](#production-deployment)
5. [Cloud-Scale Deployment](#cloud-scale-deployment)
6. [Hybrid Edge + Cloud Federation](#hybrid-edge--cloud-federation)
7. [Operations](#operations)

---

## Prerequisites

### Hardware Requirements

| Environment | CPU | Memory | Storage | Network |
|------------|-----|--------|---------|---------|
| Dev (single) | 2+ cores | 4 GB | 20 GB SSD | localhost |
| Dev (multi) | 4+ cores | 8 GB | 50 GB SSD | localhost |
| Production | 8+ cores | 32 GB | 500 GB NVMe | 10 Gbps |
| Edge | 4+ cores | 16 GB | 100 GB SSD | 1 Gbps |

### Software Requirements

```bash
# Required
rustc >= 1.75.0
cargo >= 1.75.0
docker >= 24.0
docker-compose >= 2.20

# Optional
kubectl >= 1.28
helm >= 3.13
terraform >= 1.6
```

### Build the Binary

```bash
# Debug build
cargo build

# Release build with optimizations
cargo build --release

# Cross-compile for Linux (from macOS)
cross build --release --target x86_64-unknown-linux-gnu
```

---

## Single-Node Development

Fastest way to start developing against Grey Distributed.

### Quick Start

```bash
# Start single-node cluster
./deploy/dev.sh start

# Or manually:
cargo run -- \
  --mode single \
  --data-dir ./data \
  --http-addr 127.0.0.1:8080 \
  --grpc-addr 127.0.0.1:9090
```

### Configuration (dev.yaml)

```yaml
# Grey Distributed Development Configuration
mode: single

node:
  id: dev-node-1
  name: "Development Node"
  
networking:
  http_addr: "127.0.0.1:8080"
  grpc_addr: "127.0.0.1:9090"
  
storage:
  data_dir: "./data"
  wal_dir: "./data/wal"
  
consensus:
  # Single-node doesn't need consensus, but we run it for API compatibility
  enabled: true
  election_timeout_ms: 1000
  
scheduler:
  enabled: true
  default_priority: 50
  
observability:
  tracing:
    enabled: true
    sample_rate: 1.0  # 100% sampling in dev
    exporter: "stdout"
  metrics:
    enabled: true
    port: 9091
  logging:
    level: "debug"
    format: "pretty"
    
security:
  tls:
    enabled: false  # Disable TLS for local dev
  auth:
    enabled: false  # Disable auth for local dev
```

### Environment Variables

```bash
export GREY_LOG_LEVEL=debug
export GREY_DATA_DIR=./data
export GREY_HTTP_ADDR=127.0.0.1:8080
export GREY_GRPC_ADDR=127.0.0.1:9090
```

### Verify Installation

```bash
# Health check
curl http://localhost:8080/health

# Cluster status
curl http://localhost:8080/v1/cluster/status

# Metrics
curl http://localhost:9091/metrics
```

---

## Local Multi-Node Cluster

For testing distributed behavior locally.

### Using Docker Compose

```yaml
# docker-compose.yml
version: '3.8'

services:
  grey-node-1:
    image: grey-distributed:latest
    container_name: grey-node-1
    hostname: grey-node-1
    ports:
      - "8081:8080"
      - "9091:9090"
    environment:
      - GREY_NODE_ID=1
      - GREY_CLUSTER_PEERS=grey-node-2:9090,grey-node-3:9090
      - GREY_RAFT_BOOTSTRAP=true
    volumes:
      - grey-data-1:/data
    networks:
      - grey-network

  grey-node-2:
    image: grey-distributed:latest
    container_name: grey-node-2
    hostname: grey-node-2
    ports:
      - "8082:8080"
      - "9092:9090"
    environment:
      - GREY_NODE_ID=2
      - GREY_CLUSTER_PEERS=grey-node-1:9090,grey-node-3:9090
    volumes:
      - grey-data-2:/data
    networks:
      - grey-network

  grey-node-3:
    image: grey-distributed:latest
    container_name: grey-node-3
    hostname: grey-node-3
    ports:
      - "8083:8080"
      - "9093:9090"
    environment:
      - GREY_NODE_ID=3
      - GREY_CLUSTER_PEERS=grey-node-1:9090,grey-node-2:9090
    volumes:
      - grey-data-3:/data
    networks:
      - grey-network

volumes:
  grey-data-1:
  grey-data-2:
  grey-data-3:

networks:
  grey-network:
    driver: bridge
```

### Start Cluster

```bash
# Build and start
docker-compose up --build -d

# View logs
docker-compose logs -f

# Scale to 5 nodes
docker-compose up --scale grey-node=5 -d

# Stop cluster
docker-compose down -v
```

### Manual Multi-Node

```bash
# Terminal 1 - Bootstrap node
./target/release/greyd \
  --node-id 1 \
  --http-addr 127.0.0.1:8081 \
  --grpc-addr 127.0.0.1:9001 \
  --data-dir ./data/node1 \
  --bootstrap

# Terminal 2
./target/release/greyd \
  --node-id 2 \
  --http-addr 127.0.0.1:8082 \
  --grpc-addr 127.0.0.1:9002 \
  --data-dir ./data/node2 \
  --join 127.0.0.1:9001

# Terminal 3
./target/release/greyd \
  --node-id 3 \
  --http-addr 127.0.0.1:8083 \
  --grpc-addr 127.0.0.1:9003 \
  --data-dir ./data/node3 \
  --join 127.0.0.1:9001
```

### Cluster Verification

```bash
# Check cluster membership
curl http://localhost:8081/v1/cluster/members

# Check leader
curl http://localhost:8081/v1/cluster/leader

# Raft status
curl http://localhost:8081/v1/raft/status

# Test failover (kill leader, verify new election)
docker stop grey-node-1
sleep 5
curl http://localhost:8082/v1/cluster/leader
```

---

## Production Deployment

### Kubernetes Deployment

```yaml
# grey-deployment.yaml
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: grey-cluster
  namespace: grey-system
spec:
  serviceName: grey-headless
  replicas: 5
  podManagementPolicy: Parallel
  selector:
    matchLabels:
      app: grey-distributed
  template:
    metadata:
      labels:
        app: grey-distributed
    spec:
      terminationGracePeriodSeconds: 60
      affinity:
        podAntiAffinity:
          requiredDuringSchedulingIgnoredDuringExecution:
            - labelSelector:
                matchLabels:
                  app: grey-distributed
              topologyKey: kubernetes.io/hostname
      containers:
        - name: grey
          image: grey-distributed:v1.0.0
          ports:
            - name: http
              containerPort: 8080
            - name: grpc
              containerPort: 9090
            - name: raft
              containerPort: 9091
            - name: metrics
              containerPort: 9092
          env:
            - name: POD_NAME
              valueFrom:
                fieldRef:
                  fieldPath: metadata.name
            - name: GREY_NODE_ID
              value: "$(POD_NAME)"
            - name: GREY_CLUSTER_DNS
              value: "grey-headless.grey-system.svc.cluster.local"
          resources:
            requests:
              cpu: "4"
              memory: "16Gi"
            limits:
              cpu: "8"
              memory: "32Gi"
          volumeMounts:
            - name: data
              mountPath: /data
            - name: config
              mountPath: /etc/grey
            - name: tls
              mountPath: /etc/grey/tls
              readOnly: true
          livenessProbe:
            httpGet:
              path: /health/live
              port: 8080
            initialDelaySeconds: 30
            periodSeconds: 10
          readinessProbe:
            httpGet:
              path: /health/ready
              port: 8080
            initialDelaySeconds: 10
            periodSeconds: 5
      volumes:
        - name: config
          configMap:
            name: grey-config
        - name: tls
          secret:
            secretName: grey-tls
  volumeClaimTemplates:
    - metadata:
        name: data
      spec:
        accessModes: ["ReadWriteOnce"]
        storageClassName: fast-ssd
        resources:
          requests:
            storage: 500Gi
---
apiVersion: v1
kind: Service
metadata:
  name: grey-headless
  namespace: grey-system
spec:
  clusterIP: None
  selector:
    app: grey-distributed
  ports:
    - name: http
      port: 8080
    - name: grpc
      port: 9090
    - name: raft
      port: 9091
---
apiVersion: v1
kind: Service
metadata:
  name: grey-lb
  namespace: grey-system
spec:
  type: LoadBalancer
  selector:
    app: grey-distributed
  ports:
    - name: http
      port: 80
      targetPort: 8080
    - name: grpc
      port: 443
      targetPort: 9090
```

### Helm Chart Values

```yaml
# values-production.yaml
replicaCount: 5

image:
  repository: grey-distributed
  tag: v1.0.0
  pullPolicy: IfNotPresent

resources:
  requests:
    cpu: "4"
    memory: "16Gi"
  limits:
    cpu: "8"
    memory: "32Gi"

persistence:
  enabled: true
  storageClass: fast-ssd
  size: 500Gi

config:
  consensus:
    election_timeout_ms: 3000
    heartbeat_interval_ms: 500
    snapshot_threshold: 10000
  
  scheduler:
    max_concurrent_tasks: 1000
    priority_lanes: 4
  
  governance:
    cpu_limit_millicores: 8000
    memory_limit_bytes: 34359738368  # 32 GB
    
  storage:
    replication_factor: 3
    read_quorum: 2
    write_quorum: 2
    compaction_interval_secs: 3600
    
  security:
    tls:
      enabled: true
      cert_secret: grey-tls
    auth:
      enabled: true
      method: mtls
    
  observability:
    tracing:
      enabled: true
      sample_rate: 0.01  # 1% in production
      exporter: otlp
      endpoint: "otel-collector:4317"
    metrics:
      enabled: true
      interval_secs: 15

affinity:
  podAntiAffinity:
    requiredDuringSchedulingIgnoredDuringExecution:
      - labelSelector:
          matchLabels:
            app: grey-distributed
        topologyKey: topology.kubernetes.io/zone

tolerations:
  - key: "dedicated"
    operator: "Equal"
    value: "grey"
    effect: "NoSchedule"

nodeSelector:
  workload-type: stateful
```

### Deploy with Helm

```bash
# Install
helm install grey ./charts/grey-distributed \
  -f values-production.yaml \
  -n grey-system \
  --create-namespace

# Upgrade
helm upgrade grey ./charts/grey-distributed \
  -f values-production.yaml \
  -n grey-system

# Rollback
helm rollback grey 1 -n grey-system
```

---

## Cloud-Scale Deployment

### Multi-Region Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                         Global Load Balancer                         │
│                    (Latency-based routing)                           │
└─────────────────┬───────────────────────────────────┬───────────────┘
                  │                                   │
        ┌─────────▼─────────┐               ┌────────▼────────┐
        │   US-East Region  │               │  EU-West Region │
        │                   │               │                 │
        │  ┌─────────────┐  │               │ ┌─────────────┐ │
        │  │  Zone A     │  │               │ │  Zone A     │ │
        │  │  3 nodes    │  │◄─────────────►│ │  3 nodes    │ │
        │  └─────────────┘  │   Async       │ └─────────────┘ │
        │                   │   Repl.       │                 │
        │  ┌─────────────┐  │               │ ┌─────────────┐ │
        │  │  Zone B     │  │               │ │  Zone B     │ │
        │  │  3 nodes    │  │               │ │  3 nodes    │ │
        │  └─────────────┘  │               │ └─────────────┘ │
        │                   │               │                 │
        └───────────────────┘               └─────────────────┘
```

### Terraform Configuration

```hcl
# main.tf
terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
}

# Variables
variable "regions" {
  default = ["us-east-1", "eu-west-1", "ap-southeast-1"]
}

variable "nodes_per_zone" {
  default = 3
}

# Create cluster in each region
module "grey_cluster" {
  for_each = toset(var.regions)
  source   = "./modules/grey-cluster"
  
  region          = each.value
  nodes_per_zone  = var.nodes_per_zone
  instance_type   = "r6g.2xlarge"
  storage_size_gb = 500
  
  vpc_cidr        = "10.${index(var.regions, each.value)}.0.0/16"
  
  federation_peers = [
    for r in var.regions : 
    "grey-cluster.${r}.internal:9090"
    if r != each.value
  ]
}

# Global Load Balancer
resource "aws_globalaccelerator_accelerator" "grey" {
  name            = "grey-global"
  ip_address_type = "IPV4"
  enabled         = true
}

resource "aws_globalaccelerator_listener" "grey" {
  accelerator_arn = aws_globalaccelerator_accelerator.grey.id
  protocol        = "TCP"
  
  port_range {
    from_port = 443
    to_port   = 443
  }
}

resource "aws_globalaccelerator_endpoint_group" "grey" {
  for_each = module.grey_cluster
  
  listener_arn                  = aws_globalaccelerator_listener.grey.id
  endpoint_group_region         = each.key
  traffic_dial_percentage       = 100
  health_check_interval_seconds = 10
  health_check_path             = "/health"
  health_check_protocol         = "HTTPS"
  
  endpoint_configuration {
    endpoint_id = each.value.nlb_arn
    weight      = 100
  }
}
```

### Cross-Region Replication

```yaml
# federation.yaml
federation:
  enabled: true
  mode: async  # async | sync | leader-follow
  
  regions:
    us-east-1:
      endpoints:
        - "grey-1.us-east-1.internal:9090"
        - "grey-2.us-east-1.internal:9090"
        - "grey-3.us-east-1.internal:9090"
      priority: 1  # Primary
      
    eu-west-1:
      endpoints:
        - "grey-1.eu-west-1.internal:9090"
        - "grey-2.eu-west-1.internal:9090"
        - "grey-3.eu-west-1.internal:9090"
      priority: 2
      
    ap-southeast-1:
      endpoints:
        - "grey-1.ap-southeast-1.internal:9090"
        - "grey-2.ap-southeast-1.internal:9090"
        - "grey-3.ap-southeast-1.internal:9090"
      priority: 3
  
  replication:
    # Async replication settings
    max_lag_seconds: 30
    batch_size: 1000
    compression: lz4
    
  conflict_resolution:
    strategy: last-write-wins  # or: custom, vector-clock
    timestamp_source: hybrid-logical-clock
    
  failover:
    automatic: true
    detection_timeout_seconds: 30
    min_healthy_replicas: 2
```

---

## Hybrid Edge + Cloud Federation

### Architecture

```
┌────────────────────────────────────────────────────────────────────┐
│                          Cloud Core                                 │
│  ┌────────────────┐  ┌────────────────┐  ┌────────────────┐        │
│  │  Region US     │  │  Region EU     │  │  Region APAC   │        │
│  │  (Primary)     │  │  (Replica)     │  │  (Replica)     │        │
│  └───────┬────────┘  └───────┬────────┘  └───────┬────────┘        │
└──────────┼───────────────────┼───────────────────┼─────────────────┘
           │                   │                   │
           │  Federation Protocol (gRPC + TLS)     │
           │                   │                   │
    ┌──────┴──────┐     ┌──────┴──────┐     ┌─────┴───────┐
    │             │     │             │     │             │
┌───▼───┐    ┌───▼───┐ ┌───▼───┐    ┌───▼───┐ ┌───▼───┐  ┌───▼───┐
│Edge 1 │    │Edge 2 │ │Edge 3 │    │Edge 4 │ │Edge 5 │  │Edge 6 │
│Factory│    │Retail │ │Branch │    │Campus │ │Mobile │  │IoT Hub│
└───────┘    └───────┘ └───────┘    └───────┘ └───────┘  └───────┘
```

### Edge Node Configuration

```yaml
# edge-config.yaml
mode: edge

node:
  id: edge-factory-001
  name: "Factory Floor Edge"
  location:
    latitude: 40.7128
    longitude: -74.0060
    
federation:
  enabled: true
  role: leaf  # leaf | intermediate | core
  
  upstream:
    endpoints:
      - "grey-core.us-east-1.cloud.example.com:443"
      - "grey-core.eu-west-1.cloud.example.com:443"
    tls:
      enabled: true
      ca_cert: /etc/grey/tls/ca.crt
      client_cert: /etc/grey/tls/edge.crt
      client_key: /etc/grey/tls/edge.key
      
  sync:
    # What to sync from cloud
    pull:
      - configs
      - policies
      - reference-data
    # What to push to cloud
    push:
      - metrics
      - events
      - aggregated-data
    
  offline:
    enabled: true
    max_offline_duration_hours: 168  # 1 week
    local_persistence: true
    queue_size_mb: 1024
    
  bandwidth:
    limit_mbps: 10
    priority_traffic:
      - control-plane
      - critical-events
      
storage:
  # Local storage for edge
  data_dir: /data/grey
  max_size_gb: 100
  
  # Tiered storage
  hot_tier:
    type: nvme
    size_gb: 20
  warm_tier:
    type: ssd
    size_gb: 80
    
scheduler:
  # Edge-specific scheduling
  local_affinity: prefer  # Run tasks locally when possible
  cloud_offload:
    enabled: true
    when: high-load  # or: always, never, scheduled
```

### Federation Protocol

```yaml
# Edge-to-cloud data flow
protocol:
  version: 2
  
  channels:
    # Control plane (high priority, low bandwidth)
    control:
      type: bidirectional
      priority: critical
      encryption: aes-256-gcm
      messages:
        - heartbeat
        - config-update
        - membership-change
        - failover-signal
        
    # Data plane (bulk data sync)
    data:
      type: push-pull
      priority: normal
      compression: zstd
      batching:
        max_size_bytes: 1048576
        max_delay_ms: 5000
      messages:
        - data-sync
        - snapshot-transfer
        
    # Event stream (real-time events)
    events:
      type: push
      priority: high
      encryption: aes-256-gcm
      messages:
        - alert
        - metric-sample
        - audit-event
```

---

## Operations

### Health Checks

```bash
# Overall health
curl http://localhost:8080/health

# Detailed health
curl http://localhost:8080/health/detailed

# Response:
{
  "status": "healthy",
  "checks": {
    "consensus": {"status": "healthy", "role": "leader", "term": 42},
    "storage": {"status": "healthy", "used_bytes": 123456789},
    "scheduler": {"status": "healthy", "active_tasks": 150},
    "network": {"status": "healthy", "connected_peers": 4}
  }
}
```

### Monitoring

```yaml
# Prometheus scrape config
scrape_configs:
  - job_name: grey-distributed
    kubernetes_sd_configs:
      - role: pod
        namespaces:
          names: [grey-system]
    relabel_configs:
      - source_labels: [__meta_kubernetes_pod_label_app]
        regex: grey-distributed
        action: keep
```

### Key Metrics

| Metric | Description | Alert Threshold |
|--------|-------------|-----------------|
| `grey_consensus_leader_changes_total` | Leader elections | > 5/hour |
| `grey_consensus_commit_latency_seconds` | Commit latency | p99 > 100ms |
| `grey_scheduler_queue_depth` | Pending tasks | > 10000 |
| `grey_storage_disk_usage_ratio` | Disk utilization | > 0.85 |
| `grey_network_rpc_errors_total` | RPC failures | > 10/min |
| `grey_governance_quota_exceeded_total` | Quota violations | > 0 |

### Backup & Recovery

```bash
# Create snapshot
curl -X POST http://localhost:8080/v1/admin/snapshot

# List snapshots
curl http://localhost:8080/v1/admin/snapshots

# Restore from snapshot
./greyd restore --snapshot /backups/snapshot-2024-01-15.tar.gz

# Continuous backups (add to cron)
0 */6 * * * /opt/grey/backup.sh --dest s3://grey-backups/
```

### Scaling

```bash
# Add node to cluster
./greyd join --cluster grey-cluster.svc:9090

# Remove node (graceful)
curl -X POST http://localhost:8080/v1/admin/leave

# Rebalance after membership change
curl -X POST http://localhost:8080/v1/admin/rebalance
```

### Upgrades

```bash
# Rolling upgrade (Kubernetes)
kubectl set image statefulset/grey-cluster \
  grey=grey-distributed:v1.1.0 \
  -n grey-system

# Watch progress
kubectl rollout status statefulset/grey-cluster -n grey-system

# Rollback if needed
kubectl rollout undo statefulset/grey-cluster -n grey-system
```

### Troubleshooting

```bash
# Debug logs
./greyd --log-level=trace

# Dump Raft state
curl http://localhost:8080/v1/debug/raft

# Network diagnostics
curl http://localhost:8080/v1/debug/network

# Goroutine dump
curl http://localhost:8080/debug/pprof/goroutine?debug=2

# Memory profile
curl http://localhost:8080/debug/pprof/heap > heap.pprof
go tool pprof heap.pprof
```

---

## Security Checklist

- [ ] TLS enabled for all communications
- [ ] mTLS for inter-node traffic
- [ ] API authentication enabled
- [ ] Network policies restrict pod traffic
- [ ] Secrets stored in vault/secrets manager
- [ ] Audit logging enabled
- [ ] Key rotation configured
- [ ] RBAC policies defined
- [ ] Security scanning in CI/CD
- [ ] Vulnerability patching schedule

---

## Disaster Recovery

### RPO/RTO Targets

| Scenario | RPO | RTO |
|----------|-----|-----|
| Single node failure | 0 (sync replication) | < 30s |
| Zone failure | 0 (zone-redundant) | < 60s |
| Region failure | < 30s (async repl) | < 5min |
| Data corruption | Point-in-time | < 30min |

### Runbooks

1. **Single Node Failure**: Automatic failover, no action needed
2. **Leader Failure**: New election within 3 seconds
3. **Zone Outage**: Traffic routes to healthy zones
4. **Region Outage**: Promote replica region, update DNS
5. **Data Corruption**: Restore from snapshot + WAL replay
