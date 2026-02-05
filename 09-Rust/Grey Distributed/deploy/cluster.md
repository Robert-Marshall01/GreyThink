# Cluster Deployment Guide

Production cluster deployment for Grey Distributed.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                     Load Balancer (L4)                          │
└─────────────────────────┬───────────────────────────────────────┘
                          │
        ┌─────────────────┼─────────────────┐
        ▼                 ▼                 ▼
   ┌─────────┐       ┌─────────┐       ┌─────────┐
   │  Node 1 │◄─────►│  Node 2 │◄─────►│  Node 3 │
   │ (Leader)│  Raft │         │       │         │
   └────┬────┘       └────┬────┘       └────┬────┘
        │                 │                 │
        ▼                 ▼                 ▼
   ┌─────────┐       ┌─────────┐       ┌─────────┐
   │ Storage │       │ Storage │       │ Storage │
   │  (SSD)  │       │  (SSD)  │       │  (SSD)  │
   └─────────┘       └─────────┘       └─────────┘
```

## Hardware Requirements

### Minimum (3-node cluster)

| Resource | Minimum | Recommended |
|----------|---------|-------------|
| CPU      | 4 cores | 8+ cores    |
| RAM      | 8 GB    | 16+ GB      |
| Storage  | 100 GB SSD | 500+ GB NVMe |
| Network  | 1 Gbps  | 10 Gbps     |

### Network Latency

- Intra-cluster: < 1ms (same data center)
- Cross-AZ: < 2ms (same region)
- Cross-region: Not recommended for single cluster

## Cluster Sizing

### Small (Development/Testing)
- 3 nodes
- 10K req/sec
- 100M keys

### Medium (Production)
- 5 nodes
- 100K req/sec
- 1B keys

### Large (High-scale)
- 7+ nodes
- 500K+ req/sec
- 10B+ keys

### Quorum Considerations

| Nodes | Write Quorum | Read Quorum | Fault Tolerance |
|-------|--------------|-------------|-----------------|
| 3     | 2            | 2           | 1 node          |
| 5     | 3            | 3           | 2 nodes         |
| 7     | 4            | 4           | 3 nodes         |

## Deployment Steps

### 1. Prepare Nodes

```bash
# Install dependencies
apt-get update
apt-get install -y build-essential

# Create grey user
useradd -r -s /bin/false grey
mkdir -p /var/lib/grey /etc/grey
chown grey:grey /var/lib/grey
```

### 2. Configure systemd

```ini
# /etc/systemd/system/greyd.service
[Unit]
Description=Grey Distributed Node
After=network.target

[Service]
Type=simple
User=grey
Group=grey
ExecStart=/usr/local/bin/greyd --config /etc/grey/grey.yaml
Restart=always
RestartSec=5
LimitNOFILE=65536

[Install]
WantedBy=multi-user.target
```

### 3. Configure Nodes

Node 1 (`/etc/grey/grey.yaml`):
```yaml
node:
  id: node-1
  data_dir: /var/lib/grey

network:
  api_addr: 0.0.0.0:8080
  raft_addr: 0.0.0.0:9080
  gossip_addr: 0.0.0.0:7946

cluster:
  bootstrap_peers:
    - node-1.grey.internal:9080
    - node-2.grey.internal:9080
    - node-3.grey.internal:9080
  bootstrap_expect: 3

consensus:
  election_timeout: 1000ms
  heartbeat_interval: 150ms

storage:
  engine: rocksdb
  shards: 32
  replication_factor: 3
  write_quorum: 2
  read_quorum: 2
```

### 4. Bootstrap Cluster

```bash
# Start node 1 first (bootstrap leader)
systemctl start greyd

# Wait for bootstrap
sleep 5

# Start remaining nodes
ssh node-2 "systemctl start greyd"
ssh node-3 "systemctl start greyd"

# Verify cluster
greyctl cluster status
```

### 5. Verify Cluster Health

```bash
# Check cluster members
greyctl cluster members

# Check leader
greyctl cluster leader

# Check replication status
greyctl cluster replication
```

## Load Balancer Configuration

### HAProxy

```haproxy
frontend grey_api
    bind *:8080
    mode tcp
    default_backend grey_nodes

backend grey_nodes
    mode tcp
    balance roundrobin
    option tcp-check
    server node1 node-1.grey.internal:8080 check
    server node2 node-2.grey.internal:8080 check
    server node3 node-3.grey.internal:8080 check
```

### NGINX

```nginx
upstream grey_cluster {
    least_conn;
    server node-1.grey.internal:8080 weight=1;
    server node-2.grey.internal:8080 weight=1;
    server node-3.grey.internal:8080 weight=1;
}

server {
    listen 8080;
    location / {
        proxy_pass http://grey_cluster;
        proxy_connect_timeout 5s;
    }
}
```

## Security

### TLS Configuration

```yaml
security:
  tls:
    enabled: true
    cert_file: /etc/grey/certs/server.crt
    key_file: /etc/grey/certs/server.key
    ca_file: /etc/grey/certs/ca.crt
    client_auth: require

  mtls:
    enabled: true
    verify_client: true
```

### Generate Certificates

```bash
# Generate CA
openssl genrsa -out ca.key 4096
openssl req -new -x509 -key ca.key -out ca.crt -days 365

# Generate node certificates
for node in node-1 node-2 node-3; do
  openssl genrsa -out ${node}.key 2048
  openssl req -new -key ${node}.key -out ${node}.csr \
    -subj "/CN=${node}.grey.internal"
  openssl x509 -req -in ${node}.csr -CA ca.crt -CAkey ca.key \
    -CAcreateserial -out ${node}.crt -days 365
done
```

## Monitoring

### Prometheus Metrics

```yaml
# prometheus.yml
scrape_configs:
  - job_name: grey
    static_configs:
      - targets:
        - node-1.grey.internal:10080
        - node-2.grey.internal:10080
        - node-3.grey.internal:10080
```

### Key Metrics

| Metric | Description | Alert Threshold |
|--------|-------------|-----------------|
| `grey_raft_leader` | Current leader | Changes > 3/min |
| `grey_raft_term` | Raft term | Increases > 5/min |
| `grey_storage_used_bytes` | Disk usage | > 80% |
| `grey_request_latency_p99` | Request latency | > 100ms |
| `grey_replication_lag` | Replication lag | > 1000 entries |

### Alerting

```yaml
# Alert rules
groups:
  - name: grey
    rules:
      - alert: GreyLeaderFlapping
        expr: changes(grey_raft_leader[5m]) > 3
        for: 2m
        labels:
          severity: warning
        annotations:
          summary: "Leader changing frequently"

      - alert: GreyHighLatency
        expr: grey_request_latency_p99 > 0.1
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "High request latency"
```

## Operations

### Adding Nodes

```bash
# Add node to cluster
greyctl cluster add-member node-4.grey.internal:9080

# Verify
greyctl cluster members
```

### Removing Nodes

```bash
# Remove node (graceful)
greyctl cluster remove-member node-4

# Force remove (if node is down)
greyctl cluster remove-member node-4 --force
```

### Rolling Upgrades

```bash
# Upgrade one node at a time
for node in node-1 node-2 node-3; do
  ssh $node "systemctl stop greyd"
  ssh $node "apt install -y grey-distributed"
  ssh $node "systemctl start greyd"
  
  # Wait for node to rejoin
  sleep 30
  greyctl cluster status
done
```

### Backup and Restore

```bash
# Create snapshot
greyctl snapshot create --output /backup/grey-$(date +%Y%m%d).snap

# Restore (requires cluster shutdown)
greyctl snapshot restore --input /backup/grey-20240101.snap
```

## Troubleshooting

### Leader Election Failures

1. Check network connectivity between nodes
2. Verify time synchronization (NTP)
3. Check disk I/O latency
4. Review Raft logs

### Split-Brain Prevention

Grey uses Raft quorum to prevent split-brain:
- Writes require majority (N/2 + 1)
- Partitioned minority becomes read-only
- Automatic healing on reconnection

### Data Recovery

```bash
# Check data integrity
greyctl fsck --data-dir /var/lib/grey

# Repair if needed
greyctl repair --data-dir /var/lib/grey
```

## Next Steps

- [Cloud Deployment](cloud.md) - AWS/GCP/Azure
- [Hybrid Deployment](hybrid.md) - Multi-cloud setup
- [Monitoring Guide](../docs/observability.md)
