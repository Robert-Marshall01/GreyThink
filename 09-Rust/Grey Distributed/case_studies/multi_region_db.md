# Case Study: Multi-Region Database Orchestration

**Workload**: Geo-distributed PostgreSQL + Cassandra with cross-region consistency  
**Scale**: 500K reads/sec, 50K writes/sec across 5 regions  
**Grey Features**: Cross-region consensus, conflict resolution, replication management

---

## System Architecture

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│                        Multi-Region Database Cluster                             │
├─────────────────────────────────────────────────────────────────────────────────┤
│                                                                                  │
│   us-east-1          eu-west-1          ap-south-1         ap-northeast-1       │
│  ┌─────────┐        ┌─────────┐        ┌─────────┐        ┌─────────┐          │
│  │   PG    │◀──────▶│   PG    │◀──────▶│   PG    │◀──────▶│   PG    │          │
│  │ Primary │        │ Primary │        │ Replica │        │ Replica │          │
│  └────┬────┘        └────┬────┘        └────┬────┘        └────┬────┘          │
│       │                  │                  │                  │                │
│  ┌────┴────┐        ┌────┴────┐        ┌────┴────┐        ┌────┴────┐          │
│  │Cassandra│◀──────▶│Cassandra│◀──────▶│Cassandra│◀──────▶│Cassandra│          │
│  │  (RF=3) │        │  (RF=3) │        │  (RF=3) │        │  (RF=3) │          │
│  └────┬────┘        └────┬────┘        └────┬────┘        └────┬────┘          │
│       │                  │                  │                  │                │
│       └──────────────────┼──────────────────┼──────────────────┘                │
│                          │                  │                                    │
│                    ┌─────┴──────────────────┴─────┐                             │
│                    │   Grey Distributed Consensus  │                             │
│                    │   • Global ordering            │                             │
│                    │   • Conflict resolution        │                             │
│                    │   • Failover orchestration     │                             │
│                    └──────────────────────────────┘                             │
│                                                                                  │
└─────────────────────────────────────────────────────────────────────────────────┘
```

---

## Workload Description

### Data Model

| Store | Data Type | Consistency | Access Pattern |
|-------|-----------|-------------|----------------|
| **PostgreSQL** | User accounts, orders, transactions | Strong | OLTP, complex queries |
| **Cassandra** | Events, sessions, time-series | Eventual | High-volume writes |

### Traffic Distribution

```
Region Traffic Distribution (requests/sec):

us-east-1:    ████████████████████████████████  180K (36%)
eu-west-1:    ████████████████████████  120K (24%)
ap-south-1:   ████████████████████  100K (20%)
ap-northeast-1: ████████████████  80K (16%)
sa-east-1:    ████  20K (4%)

Total: 500K reads/sec, 50K writes/sec
```

### Query Classification

| Query Type | Volume | Latency Target | Consistency |
|------------|--------|----------------|-------------|
| User lookup | 200K/s | <10ms | Eventual (cached) |
| Balance check | 100K/s | <20ms | Strong |
| Transaction submit | 30K/s | <100ms | Strong |
| Event ingest | 150K/s | <50ms | Eventual |
| Analytics | 20K/s | <500ms | Stale OK |

---

## Grey Configuration

### Cluster Setup

```yaml
# grey-multiregion-db.yaml
cluster:
  name: global-db-cluster
  regions:
    - name: us-east-1
      role: primary
      databases:
        - type: postgresql
          nodes: 3
          storage: 10TB
        - type: cassandra
          nodes: 6
          storage: 50TB
          
    - name: eu-west-1
      role: primary
      databases:
        - type: postgresql
          nodes: 3
          storage: 10TB
        - type: cassandra
          nodes: 6
          storage: 50TB
          
    - name: ap-south-1
      role: replica
      databases:
        - type: postgresql
          nodes: 2
          replication: async
        - type: cassandra
          nodes: 4
          storage: 30TB
          
    - name: ap-northeast-1
      role: replica
      databases:
        - type: postgresql
          nodes: 2
          replication: async
        - type: cassandra
          nodes: 4
          storage: 30TB

consensus:
  raft:
    # Cross-region election timeout accounts for latency
    election_timeout: 500ms
    heartbeat_interval: 150ms
    
  conflict_resolution:
    strategy: last-writer-wins
    timestamp_source: hybrid-logical-clock
    
replication:
  postgresql:
    mode: streaming
    sync_standby_count: 1
    max_lag_bytes: 16MB
    
  cassandra:
    consistency_level: LOCAL_QUORUM
    replication_factor: 3
    cross_dc_strategy: NetworkTopologyStrategy
```

### Routing Rules

```yaml
# Traffic routing configuration
routing:
  rules:
    - query_type: read
      consistency: eventual
      strategy: nearest-region
      fallback: random
      
    - query_type: read
      consistency: strong
      strategy: route-to-primary
      fallback: wait-for-sync
      
    - query_type: write
      strategy: primary-region
      replication: sync-to-one-async-to-rest
      
  affinity:
    - pattern: "user:*"
      region: user.home_region
    - pattern: "order:*"
      region: order.created_region
```

---

## Performance Results

### Latency by Region

| Region | Read P50 | Read P99 | Write P50 | Write P99 |
|--------|----------|----------|-----------|-----------|
| us-east-1 | 4ms | 15ms | 18ms | 45ms |
| eu-west-1 | 5ms | 18ms | 22ms | 52ms |
| ap-south-1 | 8ms | 28ms | 85ms | 180ms |
| ap-northeast-1 | 12ms | 35ms | 90ms | 195ms |
| **Cross-region** | 45ms | 120ms | 150ms | 350ms |

### Cross-Region Replication Lag

```
Replication Lag Distribution:

us-east → eu-west:    ████████  80ms avg
us-east → ap-south:   ████████████████  160ms avg
us-east → ap-ne:      ██████████████████  180ms avg

Sync replication (us-east ↔ eu-west):
  P50: 75ms
  P99: 145ms
  
Async replication (to replicas):
  P50: 150ms
  P99: 450ms
  Max observed: 2.4s (during network congestion)
```

### Throughput Scaling

| Configuration | Read Throughput | Write Throughput | Notes |
|---------------|-----------------|------------------|-------|
| Single region | 120K/s | 25K/s | Baseline |
| 2 primary regions | 280K/s | 45K/s | +133% reads |
| 2 primary + 2 replica | 500K/s | 50K/s | +316% reads |
| Grey coordination | 520K/s | 52K/s | +4% (smarter routing) |

---

## Conflict Resolution

### Conflict Types and Resolution

| Conflict Type | Detection | Resolution Strategy | Example |
|---------------|-----------|---------------------|---------|
| **Write-write** | Vector clock divergence | Last-writer-wins (HLC) | Two users update same profile |
| **Schema conflict** | DDL collision | Abort later transaction | Concurrent column add |
| **Constraint violation** | FK/unique check | Application callback | Duplicate order ID |
| **Partition divergence** | Merkle tree diff | Automatic merge | Network partition heal |

### Conflict Metrics

```
Conflict Statistics (24-hour window):

Total writes:           4.3M
Conflicts detected:     127
Conflict rate:          0.003%

Resolution breakdown:
  └─ Last-writer-wins:  89 (70%)
  └─ App callback:      31 (24%)
  └─ Manual review:     7 (6%)

Average resolution time:
  └─ Automatic:         0.8ms
  └─ Callback:          45ms
  └─ Manual:            4.2 hours
```

### Hybrid Logical Clock

```
HLC Timestamp Format:
┌────────────────────────────────────────────────────────┐
│ Physical Time (48 bits) │ Logical Counter (16 bits)   │
│ Unix millis             │ Per-node counter            │
└────────────────────────────────────────────────────────┘

Properties:
- Monotonically increasing per node
- Cross-node ordering via physical time
- Causality tracking via logical counter
- Max clock skew tolerance: 500ms
```

---

## Failover Behavior

### PostgreSQL Primary Failover

```
Timeline: Primary (us-east-1) failure

T+0s      Primary becomes unresponsive
T+0.5s    Grey detects missed heartbeats
T+2s      Failover initiated (after confirmation)
T+2.1s    Sync replica (eu-west-1) promoted to primary
T+2.3s    Connection strings updated via Grey DNS
T+2.5s    Write traffic redirected to new primary
T+3s      Async replicas repoint to new primary
T+15s     Former primary quarantined for investigation

Total write unavailability: 2.5 seconds
Reads: Available throughout (from replicas)
Data loss: 0 (sync replica was up-to-date)
```

### Cross-Region Network Partition

| Scenario | us-east + eu-west | ap-south + ap-ne | Resolution |
|----------|-------------------|------------------|------------|
| Split-brain prevention | Primary quorum | Read-only mode | Automatic |
| Write routing | Continue accepting | Queue locally | On heal: replay |
| Read routing | Local serving | Local serving | Stale data flag |
| Partition duration | - | - | Alert at 30s |

### Cassandra Repair

```yaml
# Grey-managed repair schedule
repair:
  schedule:
    - region: us-east-1
      window: "02:00-04:00 UTC"
      frequency: daily
      
    - region: eu-west-1
      window: "02:00-04:00 UTC"
      frequency: daily
      
    - region: ap-south-1
      window: "18:00-20:00 UTC"
      frequency: daily

  cross_dc_repair:
    frequency: weekly
    day: sunday
    window: "04:00-08:00 UTC"
    
  incremental: true
  parallelism: 2
```

---

## Resource Allocation

### Per-Region Capacity

| Region | PostgreSQL | Cassandra | Grey Nodes | Network |
|--------|------------|-----------|------------|---------|
| us-east-1 | 3x r6g.4xlarge | 6x i3.2xlarge | 3x c6i.xlarge | 10 Gbps |
| eu-west-1 | 3x r6g.4xlarge | 6x i3.2xlarge | 3x c6i.xlarge | 10 Gbps |
| ap-south-1 | 2x r6g.2xlarge | 4x i3.2xlarge | 2x c6i.xlarge | 5 Gbps |
| ap-northeast-1 | 2x r6g.2xlarge | 4x i3.2xlarge | 2x c6i.xlarge | 5 Gbps |

### Cost Analysis

| Component | Monthly Cost | % of Total |
|-----------|-------------|------------|
| PostgreSQL instances | $18,400 | 28% |
| Cassandra instances | $32,200 | 49% |
| Grey consensus nodes | $2,400 | 4% |
| Cross-region transfer | $8,500 | 13% |
| Storage | $4,200 | 6% |
| **Total** | **$65,700** | 100% |

**vs. Single-region baseline**: +85% cost, but provides:
- 4x read capacity
- <100ms read latency globally
- Regional failure tolerance
- Regulatory compliance (data residency)

---

## Tradeoff Analysis

### Consistency vs. Latency

| Consistency Level | Write Latency | Read Latency | Durability |
|-------------------|---------------|--------------|------------|
| Async all | 18ms | 4ms | Best-effort |
| Sync one DC | 85ms | 4ms | Regional |
| Sync two DC | 180ms | 4ms | Cross-region |
| Sync quorum (3) | 250ms | 45ms | Global |

**Recommendation**: Sync one DC for most writes; sync two DC for financial transactions.

### Replication Factor vs. Storage Cost

| RF | Storage Multiplier | Read Availability | Write Availability |
|----|-------------------|-------------------|-------------------|
| 2 | 2x | 99.5% | 99% |
| 3 | 3x | 99.95% | 99.5% |
| 4 | 4x | 99.99% | 99.9% |
| 5 | 5x | 99.999% | 99.95% |

**Recommendation**: RF=3 for production; RF=5 for critical metadata.

### Read Routing Strategy

| Strategy | Latency | Consistency | Complexity |
|----------|---------|-------------|------------|
| Nearest | Lowest | Eventual | Low |
| Primary-only | Highest | Strong | Low |
| Read-your-writes | Medium | Session | Medium |
| Causal | Medium | Causal | High |

---

## Monitoring Dashboards

### Key Metrics

```promql
# Cross-region replication lag
grey_replication_lag_seconds{source="us-east-1", target="eu-west-1"}

# Write throughput by region
rate(grey_db_writes_total[5m]) by (region)

# Conflict rate
rate(grey_conflicts_total[1h]) / rate(grey_writes_total[1h])

# Failover events
grey_failover_total{db="postgresql"}

# Query routing distribution
grey_query_routing_total{strategy="nearest"} by (region)

# Connection pool saturation
grey_connection_pool_active / grey_connection_pool_max
```

### Alert Rules

```yaml
alerts:
  - name: ReplicationLagHigh
    condition: grey_replication_lag_seconds > 5
    severity: warning
    
  - name: ReplicationLagCritical
    condition: grey_replication_lag_seconds > 30
    severity: critical
    action: page-oncall
    
  - name: ConflictRateElevated
    condition: rate(grey_conflicts_total[1h]) > 10
    severity: warning
    
  - name: FailoverOccurred
    condition: increase(grey_failover_total[5m]) > 0
    severity: critical
    action: page-oncall
    
  - name: CrossRegionLatencyHigh
    condition: grey_cross_region_latency_p99 > 500ms
    severity: warning
```

---

## Lessons Learned

1. **HLC essential**: Physical clocks alone caused 0.1% of writes to have incorrect ordering. HLC reduced this to <0.001%.

2. **Sync replication cost**: Sync replication between us-east-1 and eu-west-1 added 65ms to P99 write latency but was required for RPO=0.

3. **Read-your-writes complexity**: Implementing session-scoped read-your-writes required client-side version tracking; Grey's SDK handled this transparently.

4. **Cassandra repair coordination**: Uncoordinated repairs caused 40% throughput drops. Grey's scheduled, throttled repairs reduced impact to 5%.

5. **Cross-region cost**: Network transfer costs were 3x higher than expected. Implemented compression (40% reduction) and delta replication (25% reduction).

6. **Failover testing**: Monthly failover drills reduced actual failover time from 45s to 2.5s through runbook improvements.
