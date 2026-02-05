# Case Study: Hybrid Edge-Cloud Deployment

**Workload**: Industrial IoT with edge processing and cloud aggregation  
**Scale**: 1,000 edge sites, 50K devices, 10M events/min  
**Grey Features**: Edge federation, intermittent connectivity, hierarchical consensus

---

## System Architecture

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│                          Hybrid Edge-Cloud Topology                              │
├─────────────────────────────────────────────────────────────────────────────────┤
│                                                                                   │
│  ┌──────────────────────────────────────────────────────────────────────────┐   │
│  │                           Cloud Region (us-east-1)                        │   │
│  │   ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐    │   │
│  │   │   Grey      │  │   Grey      │  │   Grey      │  │   Grey      │    │   │
│  │   │ Consensus   │◀▶│ Consensus   │◀▶│ Consensus   │◀▶│ Consensus   │    │   │
│  │   │   Node 1    │  │   Node 2    │  │   Node 3    │  │   Node 4    │    │   │
│  │   └─────────────┘  └─────────────┘  └─────────────┘  └─────────────┘    │   │
│  │                              │                                            │   │
│  │                              ▼                                            │   │
│  │   ┌──────────────────────────────────────────────────────────────────┐  │   │
│  │   │  Cloud Services: Analytics, ML Training, Long-term Storage       │  │   │
│  │   └──────────────────────────────────────────────────────────────────┘  │   │
│  └──────────────────────────────────────────────────────────────────────────┘   │
│                                    │                                             │
│         ┌──────────────────────────┼──────────────────────────┐                 │
│         │                          │                          │                 │
│         ▼                          ▼                          ▼                 │
│  ┌─────────────┐           ┌─────────────┐           ┌─────────────┐           │
│  │Regional Hub │           │Regional Hub │           │Regional Hub │           │
│  │  (us-west)  │           │  (eu-west)  │           │ (ap-south)  │           │
│  │  100 sites  │           │  200 sites  │           │  150 sites  │           │
│  └──────┬──────┘           └──────┬──────┘           └──────┬──────┘           │
│         │                          │                          │                 │
│    ┌────┼────┐                ┌────┼────┐                ┌────┼────┐            │
│    │    │    │                │    │    │                │    │    │            │
│    ▼    ▼    ▼                ▼    ▼    ▼                ▼    ▼    ▼            │
│  ┌───┐┌───┐┌───┐            ┌───┐┌───┐┌───┐            ┌───┐┌───┐┌───┐        │
│  │E1 ││E2 ││E3 │   ...      │E1 ││E2 ││E3 │   ...      │E1 ││E2 ││E3 │  ...   │
│  └───┘└───┘└───┘            └───┘└───┘└───┘            └───┘└───┘└───┘        │
│  Edge Sites                 Edge Sites                 Edge Sites             │
│  Grey Mini (ARM)            Grey Mini (ARM)            Grey Mini (ARM)        │
│                                                                                 │
└─────────────────────────────────────────────────────────────────────────────────┘
```

---

## Workload Description

### Device Profile

| Device Type | Count | Data Rate | Latency Requirement | Processing |
|-------------|-------|-----------|---------------------|------------|
| Temperature sensors | 20K | 1 msg/min | <5s | Simple threshold |
| Vibration monitors | 15K | 10 msg/sec | <100ms | FFT analysis |
| Cameras (manufacturing) | 5K | 30 fps | <50ms | Defect detection |
| Energy meters | 8K | 1 msg/15s | <60s | Aggregation |
| PLCs | 2K | 100 msg/sec | <10ms | Process control |

### Data Flow

```
Data Flow Hierarchy:

Device → Edge → Regional Hub → Cloud

┌─────────────────────────────────────────────────────────────────┐
│ Device Layer                                                     │
│   Raw data: 10M events/min                                       │
│   Data rate: 2.5 GB/min                                          │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ Filter, aggregate
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ Edge Layer (1,000 sites)                                         │
│   Filtered: 2M events/min (80% reduction)                        │
│   Aggregated: 500 MB/min                                         │
│   Local decisions: 50K/min                                       │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ Batch uploads, anomalies only
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ Regional Hub (3 hubs)                                            │
│   Uploads: 500K events/min                                       │
│   Bandwidth: 100 MB/min per hub                                  │
│   Cross-site correlation                                         │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ Analytics, training data
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ Cloud Layer                                                      │
│   Long-term storage: 200K events/min                             │
│   ML training data: Batch (hourly)                               │
│   Dashboard queries: 10K/min                                     │
└─────────────────────────────────────────────────────────────────┘
```

---

## Grey Configuration

### Edge Site Deployment

```yaml
# grey-edge-site.yaml
edge:
  site_id: factory-us-west-015
  hardware:
    type: raspberry-pi-4   # or jetson-nano for ML
    cpu: 4 cores
    memory: 8GB
    storage: 128GB SSD
    
  grey:
    mode: edge
    parent_hub: regional-hub-us-west
    
  consensus:
    # Lightweight consensus for edge
    type: single-node  # No local quorum needed
    state_snapshots: true
    snapshot_interval: 5m
    
  connectivity:
    primary: 4g-lte
    backup: satellite
    offline_buffer: 24h  # Store-and-forward
    sync_interval: 30s   # When connected
    
  local_processing:
    enabled: true
    rules:
      - type: threshold
        condition: "temperature > 85°C"
        action: local_alert
      - type: anomaly
        model: edge-anomaly-v2
        action: flag_and_upload
      - type: aggregate
        interval: 1m
        fields: [avg, min, max, count]
```

### Regional Hub Configuration

```yaml
# grey-regional-hub.yaml
hub:
  name: regional-hub-us-west
  location: us-west-2
  
  grey:
    mode: hub
    parent_cloud: grey-cloud-primary
    managed_sites: 100
    
  consensus:
    type: raft
    nodes: 3
    election_timeout: 500ms
    
  aggregation:
    window: 5m
    retention: 7d
    
  connectivity:
    primary: dedicated-fiber
    bandwidth: 10Gbps
    upstream_batch_interval: 60s
    
  site_management:
    heartbeat_interval: 10s
    offline_threshold: 5m
    firmware_distribution: true
```

### Cloud Coordinator

```yaml
# grey-cloud-coordinator.yaml
cloud:
  name: grey-cloud-primary
  
  grey:
    mode: coordinator
    consensus:
      type: raft
      nodes: 5
      cross_region: true
      
  federation:
    hubs:
      - regional-hub-us-west
      - regional-hub-eu-west  
      - regional-hub-ap-south
    sync_strategy: eventual
    conflict_resolution: timestamp
    
  storage:
    hot: dynamodb
    warm: s3-standard
    cold: s3-glacier
    tiering:
      hot_retention: 24h
      warm_retention: 30d
      cold_retention: 7y
      
  analytics:
    streaming: kinesis-firehose
    batch: emr
    ml: sagemaker
```

---

## Intermittent Connectivity Handling

### Offline Operation

```
Edge Site Offline Behavior:

┌─────────────────────────────────────────────────────────────────┐
│ Connection State Machine                                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                   │
│   ┌──────────┐     failure      ┌──────────┐                     │
│   │          │ ─────────────────▶│          │                     │
│   │  ONLINE  │                   │ OFFLINE  │                     │
│   │          │◀───────────────── │          │                     │
│   └──────────┘     recovered     └──────────┘                     │
│        │                              │                           │
│        │ degraded                     │ prolonged                 │
│        ▼                              ▼                           │
│   ┌──────────┐                   ┌──────────┐                     │
│   │          │                   │          │                     │
│   │ DEGRADED │                   │  ISLAND  │                     │
│   │ (partial)│                   │  (solo)  │                     │
│   └──────────┘                   └──────────┘                     │
│                                                                   │
└─────────────────────────────────────────────────────────────────┘

Offline capabilities:
✓ Local sensor ingestion
✓ Threshold alerting
✓ Data buffering (24h)
✓ Local ML inference
✗ Cloud queries
✗ Firmware updates
✗ Cross-site correlation
```

### Store-and-Forward

```rust
// Grey edge store-and-forward implementation
struct OfflineBuffer {
    max_size: usize,        // 24 hours @ avg data rate
    priority_queue: bool,   // Critical events first
    compression: bool,      // LZ4 for bandwidth savings
}

async fn handle_offline_event(event: Event) {
    // Buffer locally
    let critical = event.is_anomaly() || event.is_alert();
    offline_buffer.enqueue(event, critical).await;
    
    // Local processing continues
    if let Some(action) = local_rules.evaluate(&event) {
        execute_local_action(action).await;
    }
}

async fn on_reconnect() {
    // Sync state with hub
    let delta = grey_client.calculate_delta().await;
    
    // Upload buffered events (critical first)
    while let Some(batch) = offline_buffer.dequeue_batch(1000).await {
        grey_client.upload_batch(batch).await?;
        
        // Adaptive rate based on available bandwidth
        tokio::time::sleep(adaptive_delay()).await;
    }
    
    // Receive any missed commands
    grey_client.sync_commands().await;
}
```

### Conflict Resolution

| Conflict Type | Detection | Resolution | Example |
|---------------|-----------|------------|---------|
| State divergence | Vector clocks | Last-writer-wins | Config updated while offline |
| Duplicate events | Event ID hash | Deduplicate | Same event sent twice |
| Command ordering | Sequence numbers | Reorder on sync | Firmware update vs config change |
| Time skew | NTP + HLC | Adjust timestamps | Clocks drifted while offline |

---

## Performance Results

### Edge Processing Latency

| Operation | P50 | P99 | Max |
|-----------|-----|-----|-----|
| Sensor ingestion | 0.5ms | 2ms | 10ms |
| Threshold evaluation | 1ms | 5ms | 20ms |
| Local ML inference | 15ms | 45ms | 100ms |
| Local alert | 5ms | 20ms | 50ms |
| Hub upload (batch) | 200ms | 800ms | 2s |

### Data Reduction

```
Data Reduction Pipeline:

Raw Data:        ████████████████████████████████████████ 10M events/min
                                                           (2.5 GB/min)
                              │
                              │ Edge filtering (threshold, dedup)
                              ▼
After Edge:      ████████  2M events/min
                          (500 MB/min) — 80% reduction
                              │
                              │ Hub aggregation (windows, rollups)
                              ▼
After Hub:       ████  500K events/min
                      (100 MB/min) — 95% reduction from raw
                              │
                              │ Cloud tiering (anomalies, samples)
                              ▼
Cloud Storage:   ██  200K events/min
                    (50 MB/min) — 98% reduction from raw
```

### Offline Resilience

| Metric | Target | Achieved |
|--------|--------|----------|
| Max offline duration | 24h | 36h (with compression) |
| Buffer fill rate | 1.5 GB/day | 1.2 GB/day |
| Sync time after reconnect | <30 min | 18 min avg |
| Event loss during offline | 0% | 0% |
| Duplicate rate after sync | <0.01% | 0.003% |

---

## Hierarchical Consensus

### Consensus Layers

```
Consensus Hierarchy:

┌─────────────────────────────────────────────────────────────────┐
│ Layer 1: Cloud Coordinator (Global)                              │
│   Scope: Cross-hub coordination, global config                  │
│   Nodes: 5 (quorum: 3)                                          │
│   Latency: 50-200ms                                             │
│   Consistency: Strong                                            │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ Async replication
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ Layer 2: Regional Hub (Regional)                                 │
│   Scope: Site management, regional aggregation                  │
│   Nodes: 3 per hub (quorum: 2)                                  │
│   Latency: 10-50ms                                              │
│   Consistency: Strong within region                             │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ Eventual sync
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ Layer 3: Edge Site (Local)                                       │
│   Scope: Local state, device management                         │
│   Nodes: 1 (single-node)                                        │
│   Latency: <5ms                                                 │
│   Consistency: Local only                                        │
└─────────────────────────────────────────────────────────────────┘
```

### State Propagation

| Direction | Trigger | Latency | Consistency |
|-----------|---------|---------|-------------|
| Edge → Hub | Event batch, 30s interval | 50-500ms | Eventual |
| Hub → Cloud | Aggregation, 60s interval | 100-800ms | Eventual |
| Cloud → Hub | Config change, command | 200ms-5s | Strong (wait for ack) |
| Hub → Edge | Config push, firmware | 1-30s | Best-effort (retry) |

---

## Resource Allocation

### Per-Tier Configuration

| Tier | Hardware | Count | Monthly Cost |
|------|----------|-------|--------------|
| Edge (basic) | RPi 4 (8GB) | 800 | $8,000 |
| Edge (ML) | Jetson Nano | 200 | $12,000 |
| Hub | c6i.2xlarge (3x per hub) | 9 | $5,400 |
| Cloud coordinator | c6i.xlarge (5x) | 5 | $1,500 |
| Cloud storage | S3 + DynamoDB | - | $15,000 |
| Connectivity | 4G/Fiber/Satellite | 1000 sites | $25,000 |
| **Total** | | | **$66,900** |

### Bandwidth Planning

| Link | Bandwidth | Utilization | Cost |
|------|-----------|-------------|------|
| Edge → Hub (4G) | 10 Mbps avg | 15% | Included |
| Edge → Hub (Fiber) | 100 Mbps avg | 5% | Included |
| Hub → Cloud | 1 Gbps | 30% | $3,000/mo |
| Inter-hub | 100 Mbps | 10% | $1,500/mo |

---

## Tradeoff Analysis

### Edge vs. Cloud Processing

| Location | Latency | Cost | Complexity | Use Case |
|----------|---------|------|------------|----------|
| Device | <1ms | Lowest | Lowest | Simple threshold |
| Edge | 1-50ms | Low | Medium | ML inference, aggregation |
| Hub | 50-500ms | Medium | Medium | Cross-site correlation |
| Cloud | 200ms-2s | Highest | Highest | Training, analytics |

**Recommendation**: Process 80% at edge, 15% at hub, 5% at cloud.

### Connectivity Modes

| Mode | Bandwidth | Latency | Reliability | Cost |
|------|-----------|---------|-------------|------|
| Fiber | 1 Gbps | 5ms | 99.9% | High |
| 4G LTE | 50 Mbps | 30ms | 95% | Medium |
| 5G | 500 Mbps | 10ms | 97% | High |
| Satellite | 50 Mbps | 500ms | 90% | Medium |
| LoRaWAN | 50 Kbps | 100ms | 85% | Low |

**Recommendation**: Primary 4G/5G with satellite backup for remote sites.

### Buffer Size vs. Offline Duration

| Buffer Size | Max Offline | Storage Cost | Sync Time |
|-------------|-------------|--------------|-----------|
| 8 GB | 6 hours | $20 | 5 min |
| 32 GB | 24 hours | $50 | 18 min |
| 128 GB | 4 days | $150 | 72 min |
| 512 GB | 16 days | $400 | 5+ hours |

**Recommendation**: 32-128 GB based on site criticality.

---

## Monitoring Dashboards

### Edge Fleet Monitoring

```promql
# Online site percentage
sum(grey_edge_site_online) / count(grey_edge_site_status) * 100

# Offline sites by region
count(grey_edge_site_online == 0) by (region)

# Buffer utilization
grey_edge_buffer_bytes / grey_edge_buffer_capacity * 100

# Local processing rate
rate(grey_edge_events_processed_total[5m]) by (site_id)

# Sync lag (time since last successful sync)
time() - grey_edge_last_sync_timestamp
```

### Hub Aggregation

```promql
# Events received from edges
rate(grey_hub_events_received_total[5m]) by (hub)

# Aggregation backlog
grey_hub_aggregation_queue_depth

# Site connectivity by hub
sum(grey_edge_site_online) by (hub) / 
sum(grey_edge_sites_managed) by (hub) * 100

# Data reduction ratio
1 - (grey_hub_bytes_uploaded / grey_hub_bytes_received)
```

### Alert Configuration

```yaml
alerts:
  - name: EdgeSiteOffline
    condition: grey_edge_site_online == 0
    for: 10m
    severity: warning
    
  - name: EdgeSiteOfflineCritical
    condition: grey_edge_site_online == 0
    for: 1h
    severity: critical
    runbook: /runbooks/edge-offline.md
    
  - name: EdgeBufferNearFull
    condition: grey_edge_buffer_bytes / grey_edge_buffer_capacity > 0.8
    severity: warning
    
  - name: HubSyncBacklog
    condition: grey_hub_aggregation_queue_depth > 1000000
    severity: warning
    
  - name: FleetConnectivityDegraded
    condition: sum(grey_edge_site_online) / count(grey_edge_site_status) < 0.9
    severity: critical
```

---

## Deployment Checklist

### Edge Site Deployment

- [ ] Hardware provisioned (RPi/Jetson)
- [ ] Grey edge agent installed
- [ ] Site ID and credentials configured
- [ ] Connectivity verified (primary + backup)
- [ ] Local processing rules deployed
- [ ] Monitoring agent installed
- [ ] Offline buffer sized appropriately
- [ ] Initial sync completed

### Regional Hub Deployment

- [ ] 3-node Grey cluster deployed
- [ ] Consensus quorum verified
- [ ] Upstream connectivity to cloud confirmed
- [ ] Edge site registrations completed
- [ ] Aggregation pipelines configured
- [ ] Cross-hub replication tested

### Fleet Rollout

- [ ] Phase 1: Pilot (10 sites) - 2 weeks
- [ ] Phase 2: Regional (100 sites) - 4 weeks
- [ ] Phase 3: Full deployment (1000 sites) - 12 weeks
- [ ] Monitoring dashboards operational
- [ ] Alerting thresholds calibrated
- [ ] Runbooks documented
- [ ] On-call rotation established

---

## Lessons Learned

1. **Offline-first design essential**: Initial design assumed connectivity. After field deployment, 15% of sites had daily disconnections. Redesigned for offline-first.

2. **Buffer sizing critical**: 8 GB initial buffers filled during 12-hour outages. Upgraded to 32 GB standard, 128 GB for remote sites.

3. **Edge ML model versioning**: Deploying incompatible models caused inference failures. Implemented model versioning with fallback.

4. **Time synchronization challenges**: NTP drift during offline periods caused ordering issues. HLC + post-sync correction resolved.

5. **Firmware update rollouts**: Simultaneous updates to 100 sites overwhelmed hub. Implemented staged rollout with 10-site waves.

6. **Satellite bandwidth costs**: Initial estimates 50% below actual. Added aggressive compression and priority-based uploads.

7. **Local alert fatigue**: Threshold-based alerts generated 10x expected volume. Added hysteresis and rate limiting.
