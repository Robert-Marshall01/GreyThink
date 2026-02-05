# Hybrid & Multi-Cloud Deployment

Deploy Grey Distributed across multiple clouds and edge locations.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           Grey Federation                                    │
└─────────────────────────────────────────────────────────────────────────────┘
         │                         │                         │
         ▼                         ▼                         ▼
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│   AWS Region    │     │   GCP Region    │     │  Edge Location  │
│  ┌───────────┐  │     │  ┌───────────┐  │     │  ┌───────────┐  │
│  │  Grey     │  │     │  │  Grey     │  │     │  │  Grey     │  │
│  │ Cluster A │◄─┼─────┼──│ Cluster B │◄─┼─────┼──│ Cluster C │  │
│  │ (Primary) │  │     │  │ (Replica) │  │     │  │ (Replica) │  │
│  └───────────┘  │     │  └───────────┘  │     │  └───────────┘  │
└─────────────────┘     └─────────────────┘     └─────────────────┘
```

## Federation Setup

### Topology Types

| Topology | Description | Use Case |
|----------|-------------|----------|
| Active-Passive | One primary, N replicas | DR, geo-redundancy |
| Active-Active | Multi-primary with conflict resolution | Low-latency global |
| Hub-Spoke | Central hub + edge clusters | IoT, CDN |

### Federation Configuration

```yaml
# federation.yaml
federation:
  name: global-grey
  
  # Federation control plane
  control_plane:
    endpoint: federation.grey.internal:8443
    tls:
      cert_file: /etc/grey/federation-cert.pem
      key_file: /etc/grey/federation-key.pem
      ca_file: /etc/grey/federation-ca.pem
  
  # Member clusters
  clusters:
    - name: aws-east
      endpoint: grey-east.example.com:8080
      region: us-east-1
      role: primary
      weight: 100  # Traffic weight
      
    - name: gcp-west
      endpoint: grey-west.example.com:8080
      region: us-west1
      role: replica
      weight: 50
      
    - name: edge-chicago
      endpoint: grey-chi.edge.internal:8080
      region: edge-chi
      role: replica
      weight: 25
      capabilities:
        - read-only
        - cache
  
  # Replication settings
  replication:
    mode: async           # sync | async | semi-sync
    batch_size: 1000
    interval: 100ms
    conflict_resolution: last-write-wins  # lww | primary-wins | custom
  
  # Failover configuration
  failover:
    auto_failover: true
    detection_time: 30s
    promotion_timeout: 60s
    min_insync_replicas: 1
```

## Cross-Cloud Networking

### VPN Mesh

```
AWS VPC (10.0.0.0/16) ◄──Site-to-Site VPN──► GCP VPC (10.1.0.0/16)
        │                                              │
        └──────────────Site-to-Site VPN────────────────┘
                              │
                    Edge Network (10.2.0.0/16)
```

### WireGuard Configuration

```ini
# /etc/wireguard/grey-mesh.conf (AWS node)
[Interface]
PrivateKey = <aws-private-key>
Address = 10.100.0.1/24
ListenPort = 51820

# GCP peer
[Peer]
PublicKey = <gcp-public-key>
AllowedIPs = 10.100.0.2/32, 10.1.0.0/16
Endpoint = gcp-gateway.example.com:51820
PersistentKeepalive = 25

# Edge peer
[Peer]
PublicKey = <edge-public-key>
AllowedIPs = 10.100.0.3/32, 10.2.0.0/16
Endpoint = edge-gateway.example.com:51820
PersistentKeepalive = 25
```

### Cloud-Specific VPN Setup

```hcl
# AWS - VPN Gateway
resource "aws_vpn_gateway" "grey" {
  vpc_id = aws_vpc.grey.id
}

resource "aws_customer_gateway" "gcp" {
  bgp_asn    = 65000
  ip_address = var.gcp_vpn_ip
  type       = "ipsec.1"
}

resource "aws_vpn_connection" "aws_to_gcp" {
  vpn_gateway_id      = aws_vpn_gateway.grey.id
  customer_gateway_id = aws_customer_gateway.gcp.id
  type                = "ipsec.1"
  static_routes_only  = false
}
```

## Edge Deployment

### Lightweight Edge Configuration

```yaml
# edge-grey.yaml
node:
  mode: edge
  parent_cluster: grey-primary.example.com:8080
  
  # Limited resources
  resources:
    max_memory: 4Gi
    max_storage: 50Gi
    max_shards: 4
  
  # Local cache
  cache:
    enabled: true
    size: 2Gi
    ttl: 300s
    prefetch:
      - pattern: "hot/*"
        interval: 60s
  
  # Offline support
  offline:
    enabled: true
    max_queue_size: 10000
    sync_on_reconnect: true
    conflict_resolution: queue-order
```

### Edge Hardware Requirements

| Component | Minimum | Recommended |
|-----------|---------|-------------|
| CPU | 2 cores | 4 cores |
| Memory | 4 GB | 8 GB |
| Storage | 50 GB SSD | 100 GB NVMe |
| Network | 10 Mbps | 100 Mbps |

### Container Deployment (Edge)

```yaml
# docker-compose.edge.yaml
version: '3.8'
services:
  grey-edge:
    image: grey-distributed:edge
    restart: always
    environment:
      - GREY_MODE=edge
      - GREY_PARENT=grey-primary.example.com:8080
      - GREY_CACHE_SIZE=2G
    volumes:
      - grey-data:/var/lib/grey
      - ./grey-edge.yaml:/etc/grey/grey.yaml
    ports:
      - "8080:8080"
    deploy:
      resources:
        limits:
          cpus: '2'
          memory: 4G

volumes:
  grey-data:
```

## Data Consistency

### Consistency Modes

| Mode | Latency | Durability | Use Case |
|------|---------|------------|----------|
| Strong | High | Guaranteed | Financial, inventory |
| Eventual | Low | Async | Analytics, cache |
| Bounded | Medium | Time-bounded | User sessions |

### Per-Operation Consistency

```rust
// Client-side consistency selection
let result = client
    .get("user:123")
    .consistency(Consistency::Strong)  // Read from primary
    .await?;

let result = client
    .get("analytics:daily")
    .consistency(Consistency::Eventual)  // Read from local replica
    .await?;
```

### Conflict Resolution

```yaml
# Custom conflict resolution
replication:
  conflict_resolution:
    default: last-write-wins
    
    rules:
      - pattern: "inventory/*"
        strategy: primary-wins
        
      - pattern: "counter/*"
        strategy: crdt-merge
        crdt_type: g-counter
        
      - pattern: "document/*"
        strategy: custom
        handler: /opt/grey/conflict-resolver.wasm
```

## Monitoring Across Regions

### Unified Metrics

```yaml
# prometheus-federation.yaml
global:
  external_labels:
    cluster: ${CLUSTER_NAME}
    region: ${REGION}

scrape_configs:
  - job_name: grey
    static_configs:
      - targets: ['localhost:9090']

remote_write:
  - url: https://metrics.grey.internal/api/v1/write
    remote_timeout: 30s
    queue_config:
      max_samples_per_send: 5000
```

### Cross-Region Dashboard

```
┌─────────────────────────────────────────────────────────────────┐
│                    Grey Federation Status                        │
├───────────────┬────────────┬──────────────┬────────────────────│
│ Cluster       │ Status     │ Lag          │ Throughput         │
├───────────────┼────────────┼──────────────┼────────────────────│
│ aws-east      │ ● Primary  │ -            │ 45,230 ops/s       │
│ gcp-west      │ ● Replica  │ 82ms         │ 12,450 ops/s       │
│ edge-chicago  │ ● Replica  │ 340ms        │ 2,100 ops/s        │
│ edge-london   │ ○ Offline  │ N/A          │ 0 ops/s            │
└───────────────┴────────────┴──────────────┴────────────────────┘
```

### Alerting Rules

```yaml
# alerts.yaml
groups:
  - name: federation
    rules:
      - alert: ReplicaLagHigh
        expr: grey_replication_lag_seconds > 5
        for: 2m
        labels:
          severity: warning
        annotations:
          summary: "Replica {{ $labels.cluster }} lag is {{ $value }}s"
      
      - alert: ClusterOffline
        expr: up{job="grey"} == 0
        for: 1m
        labels:
          severity: critical
        annotations:
          summary: "Cluster {{ $labels.cluster }} is offline"
      
      - alert: CrossRegionLatencyHigh
        expr: grey_cross_region_latency_p99 > 200
        for: 5m
        labels:
          severity: warning
```

## Disaster Recovery

### Failover Procedure

1. **Automatic Detection**: Federation monitor detects primary failure
2. **Quorum Check**: Verify replica quorum (N/2 + 1)
3. **Selection**: Choose replica with lowest replication lag
4. **Promotion**: Promote replica to primary
5. **DNS Update**: Update DNS or load balancer to new primary
6. **Catchup**: Other replicas sync from new primary

### Manual Failover

```bash
# Force failover to specific cluster
grey-ctl federation failover \
  --federation global-grey \
  --target gcp-west \
  --force

# Check failover status
grey-ctl federation status --federation global-grey
```

### Recovery After Failure

```bash
# Rejoin recovered cluster as replica
grey-ctl federation join \
  --federation global-grey \
  --cluster aws-east \
  --role replica \
  --sync-from gcp-west
```

## Security Considerations

### mTLS Between Clusters

```yaml
security:
  mtls:
    enabled: true
    cert_rotation_interval: 24h
    
  cluster_auth:
    method: certificate
    verify_peer: true
    allowed_clusters:
      - CN=aws-east.grey.internal
      - CN=gcp-west.grey.internal
      - CN=edge-*.grey.internal
```

### Network Segmentation

- Use private connectivity (VPN, dedicated interconnect)
- Encrypt all cross-region traffic
- Implement network policies for pod-to-pod communication
- Use separate service accounts per cluster

## Next Steps

- [Cloud Deployment](cloud.md) - Single-cloud setup
- [Cluster Operations](cluster.md) - Day-2 operations
- [Architecture Guide](../docs/ARCHITECTURE.md)
