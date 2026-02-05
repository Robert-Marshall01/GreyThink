# Grey Distributed — Performance Economics

This document analyzes the performance and cost tradeoffs in Grey Distributed, providing formulas, models, and decision frameworks for capacity planning.

---

## 1. Executive Summary

Grey's performance economics are governed by fundamental distributed systems tradeoffs:

| Dimension | Tradeoff | Grey Default |
|-----------|----------|--------------|
| **Consistency vs. Latency** | Stronger consistency = more round trips | Linearizable (configurable) |
| **Durability vs. Throughput** | More replicas = more writes | N=3, W=2 |
| **Availability vs. Consistency** | CAP theorem | CP (strong consistency) |
| **Cost vs. Performance** | More resources = lower latency | Balance at P99 target |

**Key Insight**: Grey optimizes for **predictable latency** over maximum throughput, accepting slightly lower peak performance for bounded tail latency.

---

## 2. Latency vs. Throughput Analysis

### 2.1 Latency Components

```
Total Latency = Network RTT + Quorum Wait + Processing + Queue Wait
             = L_net + L_quorum + L_proc + L_queue

Where:
  L_net     = Network round-trip time (varies by distance)
  L_quorum  = Time to reach quorum (depends on N, W, R)
  L_proc    = CPU processing time
  L_queue   = Queue wait time (Little's Law: λ × W)
```

**Latency Breakdown (typical production)**:

| Component | P50 | P90 | P99 | P99.9 |
|-----------|-----|-----|-----|-------|
| Network RTT | 0.5ms | 1ms | 5ms | 50ms |
| Quorum Wait | 1ms | 3ms | 10ms | 100ms |
| Processing | 0.1ms | 0.2ms | 1ms | 5ms |
| Queue Wait | 0ms | 1ms | 10ms | 100ms |
| **Total** | **1.6ms** | **5.2ms** | **26ms** | **255ms** |

### 2.2 Throughput Model

```
Throughput = min(CPU_capacity, Network_capacity, Disk_capacity, Consensus_capacity)

Consensus-limited throughput:
  T_consensus = 1 / (L_commit)
  L_commit = 2 × RTT + L_disk_sync  (Raft requires 2 RTTs for commit)

CPU-limited throughput:
  T_cpu = Cores × (1 / L_proc)

Network-limited throughput:
  T_net = Bandwidth / (RequestSize × ReplicationFactor)
```

**Throughput Scaling**:

| Cluster Size | Consensus TPS | Storage TPS | Notes |
|-------------|---------------|-------------|-------|
| 1 node | 100K+ | 50K+ | No replication overhead |
| 3 nodes | 30K | 25K | Quorum writes |
| 5 nodes | 25K | 22K | More coordination |
| 7 nodes | 20K | 18K | Diminishing returns |

### 2.3 Latency-Throughput Curve

```
Latency
   │
   │                            ╭────── Saturation
   │                          ╭─╯
   │                        ╭─╯
   │                      ╭─╯
   │                    ╭─╯
   │         ╭─────────╯
   │    ╭────╯
   └────┴──────────────────────────── Throughput
        │         │         │
     Low Load  Optimal   Overload
                 (Target this)
```

**Formula (M/M/1 approximation)**:

$$L = \frac{1}{\mu - \lambda}$$

Where:
- $L$ = average latency
- $\mu$ = service rate (max throughput)  
- $\lambda$ = arrival rate (current load)

**Knee Point**: Operate at 70% capacity to avoid latency explosion.

---

## 3. Tail Latency Mitigation

### 3.1 Problem: Tail Latency Amplification

In distributed systems, request latency is dominated by the slowest component:

$$P(all\ fast) = P(one\ fast)^{N_{parallel}}$$

**Example**: 
- P99 latency per node: 10ms
- 10 parallel requests: P99 becomes ~1 - (0.99)^10 = 9.6% chance of slow request

### 3.2 Strategy: Hedged Requests

Send redundant requests after a delay:

```go
// Hedge after P95 latency has elapsed
func HedgedRequest(ctx context.Context, key string) (Value, error) {
    responses := make(chan result, 2)
    
    // Primary request
    go func() {
        val, err := storage.Get(ctx, key)
        responses <- result{val, err}
    }()
    
    // Hedge after 5ms (P95)
    time.AfterFunc(5*time.Millisecond, func() {
        val, err := storage.Get(ctx, key)
        responses <- result{val, err}
    })
    
    // Return first response
    r := <-responses
    return r.val, r.err
}
```

**Cost/Benefit**:

| Metric | Without Hedge | With Hedge | Impact |
|--------|---------------|------------|--------|
| P99 latency | 50ms | 10ms | -80% |
| P99.9 latency | 200ms | 20ms | -90% |
| Extra requests | 0% | 5% | +5% load |

**When to Hedge**:
- Hedge delay: P95 latency
- Hedge threshold: Only if primary not complete
- Cancel policy: Cancel hedge on primary response

### 3.3 Strategy: Adaptive Retries

```go
type AdaptiveRetry struct {
    baseDelay    time.Duration
    maxDelay     time.Duration
    maxRetries   int
    
    // Exponential backoff with jitter
    // Delay = min(maxDelay, baseDelay × 2^attempt × (0.5 + random(0.5)))
}

func (r *AdaptiveRetry) Execute(ctx context.Context, op func() error) error {
    var lastErr error
    delay := r.baseDelay
    
    for attempt := 0; attempt <= r.maxRetries; attempt++ {
        if err := op(); err == nil {
            return nil
        } else if isRetryable(err) {
            lastErr = err
            jitter := 0.5 + rand.Float64()*0.5
            sleep := time.Duration(float64(delay) * jitter)
            time.Sleep(sleep)
            delay = min(delay*2, r.maxDelay)
        } else {
            return err
        }
    }
    return lastErr
}
```

**Retry Budget**: Limit total retries to prevent cascade failures.

$$RetryBudget = \frac{BaseRequests \times MaxRetryRate}{1 + RetryAmplification}$$

---

## 4. Replication Cost Model

### 4.1 Quorum Configuration

| Config | R | W | Properties | Write Cost | Read Cost |
|--------|---|---|------------|------------|-----------|
| N=3, R=2, W=2 | 2 | 2 | Strong consistency | 2x writes | 2x reads |
| N=3, R=1, W=3 | 1 | 3 | Fast reads, slow writes | 3x writes | 1x reads |
| N=3, R=3, W=1 | 3 | 1 | Fast writes, slow reads | 1x write | 3x reads |
| N=5, R=3, W=3 | 3 | 3 | Tolerate 2 failures | 3x writes | 3x reads |

### 4.2 Write Amplification

$$WriteAmplification = ReplicationFactor \times CompressionOverhead \times WALWrites$$

**Example** (N=3, 1:1 compression, WAL enabled):
$$WriteAmp = 3 \times 1.0 \times 2 = 6\times$$

Each 1KB write costs 6KB of storage I/O.

### 4.3 Storage Cost Formula

$$MonthlyCost_{storage} = DataSize \times ReplicationFactor \times \frac{PricePerGB}{Compression}$$

**Example**:
- 1TB data
- N=3 replication
- $0.10/GB/month
- 2:1 compression

$$Cost = 1000GB \times 3 \times \frac{\$0.10}{2} = \$150/month$$

### 4.4 Consistency Level Cost

| Level | Latency | Availability | Use Case |
|-------|---------|--------------|----------|
| **Strong** (R+W>N) | Higher (quorum) | Lower (need quorum) | Financial, inventory |
| **Session** | Medium | Medium | User sessions |
| **Eventual** | Lowest | Highest | Analytics, cache |

---

## 5. Sharding Economics

### 5.1 Shard Sizing

$$OptimalShardSize = \sqrt{\frac{DataSize \times ShardingOverhead}{RebalanceCost}}$$

**Rules of Thumb**:
- Too small: Metadata overhead dominates
- Too large: Rebalancing is slow and risky
- Sweet spot: 1-10GB per shard

### 5.2 Hotspot Economics

**Cost of Hotspot**:

$$HotspotCost = (HotSpotLoad - AvgLoad) \times LatencyPenalty \times Duration$$

**Detection Threshold**:

$$Hotspot = ShardLoad > Mean + 2\sigma$$

**Redistribution Cost**:

| Action | Latency Impact | Duration | When to Use |
|--------|---------------|----------|-------------|
| Range split | 100ms spike | Seconds | Large shards |
| Key migration | 10ms/key | Minutes | Small moves |
| Full rebalance | 50% degradation | Hours | Major redistribution |

### 5.3 Cross-Shard Transaction Cost

$$XactCost = BaseLatency \times NumShards \times 2 (for\ 2PC)$$

**Example**: 3-shard transaction, 5ms base latency:
$$Cost = 5ms \times 3 \times 2 = 30ms$$

**Mitigation**: Colocate related data on same shard.

---

## 6. Resource Economics

### 6.1 CPU/Memory Ceiling Analysis

```
Utilization Efficiency
   │
   │  ┌─────────────────────┐
   │  │    Safe Zone        │ ← 40-70% utilization
   │  │    (Optimal)        │
   │  └─────────────────────┘
   │                          ┌─────┐
   │                          │Risky│ ← 70-85%
   │                          └─────┘
   │                                  ┌───┐
   │                                  │BAD│ ← 85%+
   │                                  └───┘
   └──────────────────────────────────────── Utilization %
        0%    40%     70%    85%   100%
```

**Utilization Targets**:

| Resource | Target | Alert | Critical |
|----------|--------|-------|----------|
| CPU | 60% | 75% | 90% |
| Memory | 70% | 80% | 90% |
| Disk | 60% | 75% | 85% |
| Network | 50% | 70% | 85% |

### 6.2 Cost-Performance Ratio

$$CostPerformanceRatio = \frac{MonthlyInfrastructureCost}{ThroughputPerSecond \times AvailabilityPercent}$$

**Example** (optimizing for cost):
- 3-node cluster: $1,500/month, 25K TPS, 99.9% availability
- Ratio: $1,500 / (25,000 × 0.999) = $0.060 per 1000 TPS

### 6.3 Predictive vs. Reactive Scaling

| Approach | Pros | Cons | Best For |
|----------|------|------|----------|
| **Reactive** | Simple, no waste | Delay during scale | Unpredictable loads |
| **Predictive** | No latency impact | May over-provision | Predictable patterns |
| **Hybrid** | Best of both | Complex | Production systems |

**Predictive Scaling Formula**:

$$TargetCapacity_{t+\Delta} = CurrentLoad_t \times (1 + GrowthRate) \times SafetyMargin$$

**Where**:
- $\Delta$ = scaling lead time (time to add capacity)
- $GrowthRate$ = historical trend
- $SafetyMargin$ = 1.2-1.5 (20-50% buffer)

---

## 7. Multi-Region Economics

### 7.1 Bandwidth Costs

| Provider | Intra-region | Cross-region | Cross-cloud |
|----------|--------------|--------------|-------------|
| AWS | Free | $0.02/GB | $0.09/GB |
| GCP | Free | $0.01/GB | $0.08/GB |
| Azure | Free | $0.02/GB | $0.087/GB |

**Monthly Cross-Region Cost**:

$$Cost = DataSize \times ReplicationFactor \times ChurnRate \times PricePerGB$$

**Example** (100GB data, N=3, 10% daily churn, AWS):
$$DailyCost = 100GB \times 3 \times 0.10 \times \$0.02 = \$0.60/day = \$18/month$$

### 7.2 Consistency vs. Availability Tradeoffs

```
                    Consistency
                         │
                         │  ┌────────────┐
         Strong ─────────┼──┤ Synchronous│
         Consistency     │  │ Replication│
                         │  └────────────┘
                         │         │
                         │         │ Latency: 50-200ms
                         │         │ Availability: 99.9%
                         │         ▼
                         │  ┌────────────┐
         Session ────────┼──┤ Session    │
         Consistency     │  │ Stickiness │
                         │  └────────────┘
                         │         │
                         │         │ Latency: 10-50ms
                         │         │ Availability: 99.95%
                         │         ▼
                         │  ┌────────────┐
         Eventual ───────┼──┤ Async      │
         Consistency     │  │ Replication│
                         │  └────────────┘
                         │
                         └───────────────────── Availability
                                   
                              99.9%    99.99%   99.999%
```

### 7.3 Multi-Region Topology Costs

| Topology | Latency | Cost | Consistency | Use Case |
|----------|---------|------|-------------|----------|
| **Active-Passive** | Local reads, remote writes | $$ | Strong | DR only |
| **Active-Active (sync)** | Cross-region RTT | $$$ | Strong | Global strong consistency |
| **Active-Active (async)** | Local | $$ | Eventual | High availability, some conflicts |
| **Edge + Cloud** | Local | $ | Tiered | IoT, CDN |

### 7.4 Geo-Replication Overhead

**Synchronous Geo-Replication**:

$$Latency_{write} = max(RTT_{region1}, RTT_{region2}, ..., RTT_{regionN})$$

**Example** (US-West, US-East, EU-West):
- US-West → US-East: 70ms RTT
- US-West → EU-West: 150ms RTT
- Quorum: 2 of 3

$$Latency = max(70ms, 150ms) = 150ms$$

**Asynchronous Geo-Replication**:

$$Latency_{write} = LocalQuorumLatency + AsyncQueueTime$$
$$DataLoss_{max} = AsyncLag \times WriteRate$$

---

## 8. Cost Optimization Strategies

### 8.1 Right-Sizing Checklist

| Check | Action | Savings |
|-------|--------|---------|
| CPU < 40% consistently | Downsize instance | 20-40% |
| Memory < 50% consistently | Reduce RAM | 10-30% |
| Disk IOPS < 50% | Use cheaper storage | 30-50% |
| Cross-region traffic high | Add local caches | 50-80% |

### 8.2 Reserved vs. On-Demand

| Commitment | Discount | Break-even | Risk |
|------------|----------|------------|------|
| On-demand | 0% | N/A | None |
| 1-year reserved | 30-40% | 5-6 months | Medium |
| 3-year reserved | 50-60% | 12-15 months | High |

**Decision Framework**:

$$UseReserved = BaselineLoad > \frac{ReservedCost}{OnDemandCost} \times 12 months$$

### 8.3 Spot/Preemptible for Batch

**Safe for**:
- Background batch jobs
- Data migration
- Non-critical analytics

**Not safe for**:
- Consensus voters
- Critical data replicas
- SLA-bound workloads

---

## 9. Performance Formulas Reference

### 9.1 Latency

| Metric | Formula |
|--------|---------|
| Average latency | $L_{avg} = \frac{1}{\mu - \lambda}$ |
| P99 latency (exponential) | $L_{99} \approx L_{avg} \times 4.6$ |
| Quorum latency | $L_{quorum} = \text{median}(L_{replica1}, L_{replica2}, ..., L_{replicaN})$ |
| Hedged latency | $L_{hedged} = min(L_{primary}, L_{hedge})$ |

### 9.2 Throughput

| Metric | Formula |
|--------|---------|
| Max throughput | $T_{max} = \frac{Capacity}{ServiceTime}$ |
| Sustainable throughput | $T_{sustainable} = 0.7 \times T_{max}$ |
| Write throughput | $T_{write} = \frac{T_{node}}{ReplicationFactor}$ |

### 9.3 Cost

| Metric | Formula |
|--------|---------|
| Storage cost | $C_{storage} = Size \times N \times Price \div Compression$ |
| Bandwidth cost | $C_{bandwidth} = Size \times Churn \times N \times Price$ |
| Compute cost | $C_{compute} = Instances \times InstancePrice \times Hours$ |

---

## 10. Decision Tables

### 10.1 Replication Factor Selection

| Requirement | Recommended N | R | W |
|-------------|---------------|---|---|
| Dev/test | 1 | 1 | 1 |
| Single-region production | 3 | 2 | 2 |
| Multi-region (2 regions) | 5 | 3 | 3 |
| Multi-region (3 regions) | 7 | 4 | 4 |

### 10.2 Consistency Level Selection

| Use Case | Consistency | Justification |
|----------|-------------|---------------|
| Financial transactions | Strong | Data integrity critical |
| User preferences | Session | User sees own writes |
| Analytics counters | Eventual | Approximate is fine |
| Product catalog | Cached eventual | Read-heavy, stale OK |

### 10.3 Scaling Trigger Thresholds

| Metric | Scale Up | Scale Down |
|--------|----------|------------|
| CPU | > 75% for 5 min | < 40% for 30 min |
| Memory | > 80% for 5 min | < 50% for 30 min |
| Queue depth | > 1000 for 1 min | < 100 for 30 min |
| Latency P99 | > target × 1.5 | < target × 0.5 |

---

## 11. Monitoring Dashboards

### 11.1 Key Performance Indicators

| KPI | Target | Alert | Critical |
|-----|--------|-------|----------|
| Write latency P99 | < 50ms | > 100ms | > 500ms |
| Read latency P99 | < 20ms | > 50ms | > 200ms |
| Throughput | > 10K TPS | < 5K TPS | < 1K TPS |
| Error rate | < 0.1% | > 0.5% | > 1% |
| Replication lag | < 100ms | > 1s | > 10s |

### 11.2 Cost Allocation

```
Monthly Cost Breakdown
┌────────────────────────────────────────────────┐
│ Compute:     ████████████████████  45%         │
│ Storage:     ██████████████       30%         │
│ Network:     ██████               15%         │
│ Other:       ████                 10%         │
└────────────────────────────────────────────────┘
```

---

## 12. Summary: Performance Economics Principles

1. **Optimize for P99, not average**: Tail latency dominates user experience
2. **Hedge expensive operations**: Redundancy is cheaper than tail latency
3. **Right-size quorums**: R + W > N is minimum; adjust for read/write ratio
4. **Monitor cost per request**: Track $/1M requests, not just total spend
5. **Plan for 2x growth**: Scaling takes time; stay ahead of demand
6. **Colocate related data**: Cross-shard/cross-region operations are expensive
7. **Use tiered consistency**: Not everything needs strong consistency
8. **Measure, don't guess**: Profile before optimizing
