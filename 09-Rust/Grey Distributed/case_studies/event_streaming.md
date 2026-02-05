# Case Study: Event Streaming Integration

**Workload**: High-throughput event ingestion with exactly-once processing  
**Scale**: 2M events/sec, 100TB+ daily data volume  
**Grey Features**: Kafka integration, exactly-once semantics, ordered processing

---

## System Architecture

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│                         Event Streaming Platform                                  │
├─────────────────────────────────────────────────────────────────────────────────┤
│                                                                                   │
│  ┌─────────────┐    ┌─────────────────────────────────────────────────────────┐ │
│  │   Event     │    │                     Kafka Cluster                        │ │
│  │   Sources   │───▶│  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐    │ │
│  │  (10K/sec   │    │  │ Topic A │  │ Topic B │  │ Topic C │  │ Topic D │    │ │
│  │   each)     │    │  │ 32 parts│  │ 64 parts│  │128 parts│  │ 16 parts│    │ │
│  └─────────────┘    │  └─────────┘  └─────────┘  └─────────┘  └─────────┘    │ │
│                      └─────────────────────────────────────────────────────────┘ │
│                                           │                                       │
│                                           ▼                                       │
│  ┌─────────────────────────────────────────────────────────────────────────────┐│
│  │                        Grey Distributed Scheduler                            ││
│  │   • Consumer group coordination        • Offset management                   ││
│  │   • Exactly-once transaction support  • Backpressure management             ││
│  │   • Partition rebalancing             • Dead letter queue handling           ││
│  └─────────────────────────────────────────────────────────────────────────────┘│
│                                           │                                       │
│                                           ▼                                       │
│  ┌─────────────────────────────────────────────────────────────────────────────┐│
│  │                         Processing Workers                                   ││
│  │   ┌────────┐  ┌────────┐  ┌────────┐  ┌────────┐  ┌────────┐              ││
│  │   │Worker 1│  │Worker 2│  │Worker 3│  │Worker 4│  │Worker N│              ││
│  │   │  8 CPU │  │  8 CPU │  │  8 CPU │  │  8 CPU │  │  8 CPU │              ││
│  │   └────────┘  └────────┘  └────────┘  └────────┘  └────────┘              ││
│  └─────────────────────────────────────────────────────────────────────────────┘│
│                                           │                                       │
│                                           ▼                                       │
│  ┌───────────────┐  ┌───────────────┐  ┌───────────────┐  ┌───────────────┐   │
│  │  Data Lake    │  │  Real-time    │  │    Search     │  │   Alerting    │   │
│  │    (S3)       │  │    (Redis)    │  │ (Elasticsearch)│  │  (PagerDuty)  │   │
│  └───────────────┘  └───────────────┘  └───────────────┘  └───────────────┘   │
│                                                                                   │
└─────────────────────────────────────────────────────────────────────────────────┘
```

---

## Workload Description

### Event Types

| Event Type | Volume | Avg Size | Processing Time | Ordering |
|------------|--------|----------|-----------------|----------|
| User clicks | 800K/s | 500B | 2ms | Per-user |
| Page views | 500K/s | 1KB | 3ms | Per-session |
| Transactions | 200K/s | 2KB | 15ms | Per-account |
| System logs | 400K/s | 800B | 1ms | Per-host |
| IoT sensors | 100K/s | 200B | 5ms | Per-device |

### Topic Configuration

```yaml
topics:
  user-events:
    partitions: 64
    replication_factor: 3
    retention: 7d
    compaction: false
    key: user_id
    
  transactions:
    partitions: 128
    replication_factor: 3
    retention: 30d
    compaction: false
    key: account_id
    exactly_once: true
    
  system-logs:
    partitions: 32
    replication_factor: 2
    retention: 3d
    compaction: false
    key: host_id
    
  iot-telemetry:
    partitions: 256
    replication_factor: 2
    retention: 1d
    compaction: true
    key: device_id
```

---

## Grey Configuration

### Kafka Connector Setup

```yaml
# grey-kafka-connector.yaml
connector:
  name: grey-kafka-bridge
  kafka:
    bootstrap_servers:
      - kafka-1.prod:9092
      - kafka-2.prod:9092
      - kafka-3.prod:9092
    security:
      protocol: SASL_SSL
      mechanism: SCRAM-SHA-512
      
  consumer:
    group_id: grey-processing-group
    auto_offset_reset: earliest
    enable_auto_commit: false  # Grey manages offsets
    max_poll_records: 500
    fetch_max_bytes: 52428800  # 50MB
    session_timeout_ms: 45000
    
  producer:
    acks: all
    enable_idempotence: true
    transactional_id_prefix: grey-tx-
    batch_size: 16384
    linger_ms: 5
    compression_type: lz4
    
  grey:
    exactly_once: true
    offset_storage: grey-consensus
    checkpoint_interval: 1000  # Every 1000 messages
    max_inflight_batches: 4
```

### Processing Pipeline

```yaml
# grey-stream-pipeline.yaml
pipeline:
  name: event-enrichment
  
  stages:
    - id: deserialize
      type: transform
      config:
        format: avro
        schema_registry: http://schema-registry:8081
        
    - id: validate
      type: filter
      config:
        condition: "event.timestamp > now() - 24h"
        dead_letter_topic: validation-failures
        
    - id: enrich
      type: lookup
      config:
        source: redis-cluster
        key_field: user_id
        enrichment_fields: [segment, tier, country]
        cache_ttl: 300s
        
    - id: aggregate
      type: window
      config:
        type: tumbling
        duration: 1m
        key: [user_id, event_type]
        aggregations:
          - count()
          - sum(amount)
          - avg(latency)
        late_arrival_tolerance: 5m
        
    - id: sink
      type: multi-sink
      config:
        sinks:
          - type: s3
            path: s3://datalake/events/
            format: parquet
            partition_by: [date, hour]
          - type: elasticsearch
            index: events-{date}
            bulk_size: 5000
          - type: redis
            expire: 3600
            key_pattern: "realtime:{user_id}"
            
  error_handling:
    retry_count: 3
    retry_backoff: exponential
    dead_letter_topic: processing-failures
```

---

## Exactly-Once Processing

### Transaction Flow

```
Exactly-Once Processing Flow:

┌─────────────────────────────────────────────────────────────────────┐
│   1. Consume batch from Kafka                                        │
│      └─ Grey tracks: topic, partition, offset range                 │
├─────────────────────────────────────────────────────────────────────┤
│   2. Begin Grey transaction                                          │
│      └─ Transaction ID: grey-tx-{worker_id}-{sequence}              │
├─────────────────────────────────────────────────────────────────────┤
│   3. Process events                                                  │
│      └─ Transformations, enrichments, aggregations                  │
├─────────────────────────────────────────────────────────────────────┤
│   4. Write to sinks (transactional)                                  │
│      └─ S3: write manifest, then data                               │
│      └─ ES: bulk with txn markers                                   │
│      └─ Redis: MULTI/EXEC block                                     │
├─────────────────────────────────────────────────────────────────────┤
│   5. Commit Grey transaction                                         │
│      └─ Atomically: commit offsets + sink markers                   │
├─────────────────────────────────────────────────────────────────────┤
│   6. If failure at any step:                                         │
│      └─ Rollback transaction                                        │
│      └─ Events reprocessed from last committed offset               │
└─────────────────────────────────────────────────────────────────────┘
```

### Idempotency Keys

```rust
// Grey's idempotency key generation
struct IdempotencyKey {
    topic: String,
    partition: i32,
    offset: i64,
    processor_id: String,
}

impl IdempotencyKey {
    fn to_string(&self) -> String {
        format!("{}-{}-{}-{}", 
            self.topic, 
            self.partition, 
            self.offset,
            self.processor_id
        )
    }
}

// Sink writes include idempotency key
async fn write_to_sink(event: Event, key: IdempotencyKey) -> Result<()> {
    let existing = grey_client
        .check_idempotency(&key.to_string())
        .await?;
        
    if existing.is_some() {
        // Already processed, skip
        return Ok(());
    }
    
    // Process and mark complete
    sink.write(event).await?;
    grey_client.mark_complete(&key.to_string()).await?;
    
    Ok(())
}
```

---

## Performance Results

### Throughput Benchmarks

| Configuration | Events/sec | Latency P50 | Latency P99 | CPU Util |
|---------------|------------|-------------|-------------|----------|
| Kafka direct | 1.8M | 8ms | 45ms | 65% |
| Grey (at-least-once) | 2.2M | 12ms | 55ms | 72% |
| Grey (exactly-once) | 2.0M | 18ms | 75ms | 78% |
| Grey + enrichment | 1.5M | 35ms | 120ms | 85% |

### Partition Scaling

```
Throughput vs Partition Count:

Partitions:  Events/sec:        Consumers:
    16       ████                400K          4
    32       ████████            800K          8
    64       ████████████████    1.5M          16
   128       ████████████████████████████ 2.2M 32
   256       ██████████████████████████████ 2.4M 48
   512       ██████████████████████████████ 2.5M 64

Diminishing returns after 256 partitions due to:
- Consumer coordination overhead
- Rebalancing latency
- Network saturation
```

### Batch Size Impact

| Batch Size | Throughput | Latency | Memory/Worker |
|------------|------------|---------|---------------|
| 100 | 500K/s | 5ms | 256MB |
| 500 | 1.2M/s | 15ms | 512MB |
| 1,000 | 1.8M/s | 25ms | 1GB |
| 5,000 | 2.1M/s | 50ms | 2GB |
| 10,000 | 2.2M/s | 95ms | 4GB |

**Recommendation**: Batch size 1,000-5,000 for balance of throughput and latency.

---

## Backpressure Management

### Flow Control

```yaml
backpressure:
  strategy: adaptive
  
  thresholds:
    warning:
      queue_depth: 10000
      action: slow_fetch
      
    critical:
      queue_depth: 50000
      action: pause_fetch
      
    recovery:
      queue_depth: 5000
      action: resume_normal
      
  rate_limiting:
    enabled: true
    algorithm: token_bucket
    tokens_per_second: 100000
    burst_size: 10000
    
  circuit_breaker:
    enabled: true
    failure_threshold: 10
    timeout: 30s
    half_open_requests: 5
```

### Backpressure Metrics

```
Backpressure Events (24h window):

Warning triggers:  ████████████████████  247
Critical triggers: ████  12
Pause duration:    Total 18 minutes (0.02% of day)

Consumer lag distribution:
  0-1s:   ██████████████████████████████████████████ 92%
  1-5s:   ████  6%
  5-30s:  █ 1.5%
  >30s:   ░ 0.5%
```

---

## Failure Recovery

### Consumer Failure

```
Timeline: Consumer worker crash

T+0s      Worker-5 process terminates unexpectedly
T+0s      Partitions 15-18 have no active consumer
T+3s      Grey session timeout expires
T+3.1s    Rebalance triggered for consumer group
T+3.5s    Partitions reassigned to Worker-3, Worker-7
T+4s      New consumers begin fetching from committed offsets
T+4.5s    Processing resumes, events replayed from T-18s (last commit)

Impact:
  - 18 seconds of events reprocessed (idempotent, no duplicates)
  - P99 latency spike to 4.5s for affected partitions
  - Zero data loss
```

### Kafka Broker Failure

| Scenario | Detection | Recovery | Data Impact |
|----------|-----------|----------|-------------|
| Single broker | 10s | 30s | None (RF=3) |
| Two brokers | 10s | 60s | None (RF=3) |
| Controller | 15s | 45s | None |
| All ZooKeeper | 5s | - | Kafka unavailable |

### Grey Transaction Failure

```
Transaction failure handling:

1. Transaction timeout (30s)
   └─ Grey aborts, releases locks
   └─ Consumer rejoins at last committed offset
   └─ Sink writes rolled back (if supported)

2. Coordinator failure
   └─ New coordinator elected (Grey consensus)
   └─ In-flight transactions recovered from log
   └─ Commit or abort based on transaction state

3. Sink write failure
   └─ Transaction aborted
   └─ Retry with exponential backoff
   └─ Dead letter after max retries
```

---

## Resource Allocation

### Worker Sizing

| Workload | CPU | Memory | Network | Workers |
|----------|-----|--------|---------|---------|
| Low (100K/s) | 4 cores | 4GB | 1Gbps | 4 |
| Medium (500K/s) | 8 cores | 8GB | 5Gbps | 16 |
| High (1M/s) | 8 cores | 16GB | 10Gbps | 32 |
| Very High (2M/s) | 16 cores | 32GB | 25Gbps | 64 |

### Kafka Cluster

```yaml
kafka_cluster:
  brokers: 12
  instance_type: i3.4xlarge  # 16 vCPU, 122GB RAM, 2x1.9TB NVMe
  
  per_broker:
    partitions: ~250
    throughput: 200K msg/s
    network: 2Gbps in + 2Gbps out
    
  storage:
    type: nvme
    total: 45TB
    usable: 32TB (after RF=3)
    retention: 7 days average
```

### Cost Analysis

| Component | Monthly Cost | % of Total |
|-----------|-------------|------------|
| Kafka brokers (12) | $28,800 | 42% |
| Grey consensus (3) | $1,800 | 3% |
| Worker instances (64) | $25,600 | 37% |
| Network transfer | $8,200 | 12% |
| Storage (S3) | $4,100 | 6% |
| **Total** | **$68,500** | 100% |

---

## Tradeoff Analysis

### At-Least-Once vs. Exactly-Once

| Semantic | Throughput | Latency | Complexity | Use Case |
|----------|------------|---------|------------|----------|
| At-most-once | 2.5M/s | 8ms | Low | Metrics, logs |
| At-least-once | 2.2M/s | 12ms | Medium | Events (idempotent sinks) |
| Exactly-once | 2.0M/s | 18ms | High | Transactions, billing |

**Overhead**: Exactly-once adds ~9% latency and reduces throughput by 9% due to:
- Transaction coordination
- Two-phase commit
- Offset storage in consensus log

### Ordering Guarantees

| Scope | Throughput Impact | Use Case |
|-------|-------------------|----------|
| No ordering | Baseline | Independent events |
| Per-key | -5% | User sessions |
| Per-partition | -10% | Strict sequence |
| Global | -40% | Total ordering (rare) |

### Checkpoint Frequency

| Interval | Reprocessing on Failure | Latency Impact | Disk I/O |
|----------|-------------------------|----------------|----------|
| 100 msgs | 100 events | +2ms | High |
| 1,000 msgs | 1,000 events | +0.5ms | Medium |
| 10,000 msgs | 10,000 events | +0.1ms | Low |
| 60 seconds | ~120K events | Minimal | Very Low |

**Recommendation**: 1,000 messages for balance of safety and performance.

---

## Monitoring Dashboards

### Key Metrics

```promql
# Consumer lag by partition
kafka_consumer_lag{group="grey-processing-group"} by (topic, partition)

# Processing throughput
rate(grey_events_processed_total[1m])

# Exactly-once transaction rate
rate(grey_transactions_committed_total[1m])

# Failed transactions
rate(grey_transactions_aborted_total[1m])

# End-to-end latency (produce to process)
histogram_quantile(0.99, 
  rate(grey_event_latency_seconds_bucket[5m])
)

# Backpressure state
grey_backpressure_state{worker=~".*"}

# Dead letter queue depth
grey_dlq_depth{topic=~".*-failures"}
```

### Alert Configuration

```yaml
alerts:
  - name: ConsumerLagHigh
    condition: kafka_consumer_lag > 100000
    for: 5m
    severity: warning
    
  - name: ConsumerLagCritical
    condition: kafka_consumer_lag > 1000000
    for: 2m
    severity: critical
    runbook: /runbooks/kafka-lag.md
    
  - name: TransactionAbortRateHigh
    condition: rate(grey_transactions_aborted_total[5m]) / rate(grey_transactions_total[5m]) > 0.01
    severity: warning
    
  - name: DeadLetterQueueGrowing
    condition: delta(grey_dlq_depth[1h]) > 1000
    severity: warning
    
  - name: ProcessingLatencyHigh
    condition: histogram_quantile(0.99, grey_event_latency_seconds_bucket) > 0.5
    severity: warning
```

---

## Lessons Learned

1. **Partition count matters**: Started with 32 partitions; scaling to 128 improved throughput 2.5x. Rebalancing with 512+ partitions took >60s.

2. **Exactly-once overhead**: Transaction overhead was 15% higher than expected. Batching transactions (1000 events/tx) reduced overhead to 9%.

3. **Consumer group coordination**: Grey's consensus-based offset storage eliminated Kafka's `__consumer_offsets` bottleneck at scale.

4. **Backpressure essential**: Without backpressure, downstream failures caused consumer lag to spike to 10M+ events.

5. **Schema evolution**: Avro schema compatibility checks prevented 3 production incidents from malformed events.

6. **Dead letter queues**: 0.01% of events failed validation; DLQ processing recovered 85% after source fixes.

7. **Rebalancing impact**: Cooperative sticky rebalancing reduced partition movement by 80% compared to eager rebalancing.
