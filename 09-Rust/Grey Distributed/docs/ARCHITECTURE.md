# Grey Distributed Architecture

## Design Philosophy

Grey Distributed follows these core principles:

1. **Correctness over Performance**: We never sacrifice consistency guarantees for speed.
2. **Explicit Failure Handling**: Every failure mode is documented and handled.
3. **Deterministic Execution**: Given the same inputs, the system produces identical outputs.
4. **Observable by Default**: Every state transition is traceable and auditable.

---

## 1. Consensus & Coordination

### Why Raft over Paxos?

We implement Raft-based consensus for the following reasons:

1. **Understandability**: Raft's decomposition into leader election, log replication, and safety is easier to reason about and audit.
2. **Strong Leadership**: Single-leader model simplifies client interactions and log ordering.
3. **Membership Changes**: Joint consensus for safe cluster reconfiguration.

### Key Design Decisions

| Decision | Rationale |
|----------|-----------|
| Heartbeat interval: 150ms | Balances failure detection latency vs. network overhead |
| Election timeout: 300-500ms (randomized) | Prevents split-vote scenarios while maintaining responsiveness |
| Snapshot threshold: 10,000 entries | Bounds log size; chosen based on typical state machine size |
| Pre-vote extension | Prevents disruptive elections from partitioned nodes |

### Split-Brain Prevention

```
┌─────────────────────────────────────────────────────────────┐
│                    Split-Brain Mitigation                    │
├─────────────────────────────────────────────────────────────┤
│  1. Fencing Tokens: Monotonic epoch attached to all writes  │
│  2. Lease-based Leadership: Leader must renew within TTL    │
│  3. Quorum Reads: Optional strong reads through majority    │
│  4. Witness Nodes: Odd-numbered clusters for tie-breaking   │
└─────────────────────────────────────────────────────────────┘
```

---

## 2. Cluster State Model

### Event Sourcing

All cluster state changes are persisted as an append-only event log:

```
Event N: TaskSubmitted{id: "t1", tenant: "acme", priority: HIGH}
Event N+1: ResourceAllocated{task: "t1", cpu: 4, ram: 8GB}
Event N+2: TaskStarted{id: "t1", node: "node-3"}
```

**Why Event Sourcing?**
- Complete audit trail for compliance (Grey Multi-Tenant)
- Enables replay debugging (Grey AI Internal training)
- Natural fit for Raft log replication
- Supports temporal queries ("what was the state at time T?")

### Snapshotting Strategy

```
┌─────────────────────────────────────────────────────────────┐
│              Snapshot Decision Matrix                        │
├─────────────────────────────────────────────────────────────┤
│  Trigger: log_size > threshold OR time_since_last > 1 hour  │
│  Format: MessagePack (faster than JSON, smaller than Proto) │
│  Compression: LZ4 (prioritize speed over ratio)             │
│  Retention: Last 3 snapshots + all after last checkpoint    │
└─────────────────────────────────────────────────────────────┘
```

---

## 3. Scheduler Design

### Deterministic Scheduling

The scheduler MUST produce identical scheduling decisions given:
- Same task queue state
- Same cluster resource state  
- Same timestamp (discretized to scheduling epoch)

This enables:
- Replay debugging of scheduling decisions
- State machine replication across nodes
- Predictable behavior for capacity planning

### Priority Lanes

```
┌─────────────────────────────────────────────────────────────┐
│  Lane 0 (CRITICAL): System tasks, leader election, health  │
│  Lane 1 (HIGH):     User-facing latency-sensitive tasks    │
│  Lane 2 (NORMAL):   Standard batch processing              │
│  Lane 3 (LOW):      Background maintenance, compaction     │
│  Lane 4 (PREEMPT):  Opportunistic, can be killed anytime   │
└─────────────────────────────────────────────────────────────┘
```

### Fairness Model

We use Weighted Fair Queuing (WFQ) within each priority lane:
- Each tenant receives a weight proportional to their quota
- Deficit Round Robin prevents starvation
- Burst allowance: 2x quota for short periods

---

## 4. Resource Governance

### Hierarchical Quotas

```
Cluster (100% CPU, 100% RAM)
├── Tenant: Acme Corp (30% CPU, 25% RAM)
│   ├── Namespace: production (80% of tenant)
│   └── Namespace: staging (20% of tenant)
├── Tenant: Beta Inc (25% CPU, 30% RAM)
└── Reserved: System (10% CPU, 10% RAM)
```

### Adaptive Throttling

Rather than hard rejections, we implement graduated throttling:

1. **Green Zone** (< 70% quota): No throttling
2. **Yellow Zone** (70-90%): Delay scheduling by (usage - 70%) * 100ms
3. **Red Zone** (> 90%): Queue new requests, reject if queue > 1000
4. **Emergency** (> 100%): Preempt lowest-priority tasks

### Hotspot Detection

We use exponential moving averages to detect resource hotspots:
- Node CPU > 85% sustained for 5 minutes → trigger rebalance
- Memory pressure detected via `/proc/meminfo` → proactive eviction
- Network saturation → route traffic to other nodes

---

## 5. Storage Layer

### Sharding Strategy

We use consistent hashing with virtual nodes:
- 256 virtual nodes per physical node
- Minimizes redistribution on node addition/removal
- Automatic detection of hot shards → split/merge

### Replication

```
┌─────────────────────────────────────────────────────────────┐
│                 Replication Factor: 3                        │
│  Write Quorum (W): 2  │  Read Quorum (R): 2                 │
│  Consistency: W + R > N ensures strong consistency          │
├─────────────────────────────────────────────────────────────┤
│  Replica Placement:                                          │
│  - First replica: Hash-determined primary                   │
│  - Second replica: Next node in ring (different rack)       │
│  - Third replica: Different availability zone               │
└─────────────────────────────────────────────────────────────┘
```

### Anti-Entropy Repair

Background process running every 6 hours:
1. Build Merkle tree of each shard
2. Compare trees with replicas
3. Identify divergent ranges
4. Stream missing data bidirectionally

---

## 6. Networking (Grey Protocol v2)

### Protocol Design

```
┌────────────────────────────────────────────────────────────┐
│  Grey Protocol v2 Frame Format                             │
├────────────────────────────────────────────────────────────┤
│  [4] Magic: "GREY"                                         │
│  [2] Version: 0x0002                                       │
│  [2] Flags: Compressed|Encrypted|Priority|Idempotent       │
│  [8] Message ID: Unique, monotonic per connection          │
│  [8] Timestamp: Unix nanos                                 │
│  [4] Payload Length                                        │
│  [N] Payload (MessagePack encoded)                         │
│  [4] CRC32 Checksum                                        │
└────────────────────────────────────────────────────────────┘
```

### Backpressure Mechanism

We implement credit-based flow control:
- Receiver advertises available buffer space (credits)
- Sender cannot exceed outstanding credits
- Credits replenished as messages are processed
- Prevents unbounded memory growth during slowdowns

### Circuit Breaking

Three-state circuit breaker per destination:
1. **Closed**: Normal operation, tracking error rate
2. **Open**: Failing fast, returning errors immediately (duration: 30s)
3. **Half-Open**: Allow probe requests, transition based on result

---

## 7. Fault Tolerance

### Φ-Accrual Failure Detector

Rather than binary alive/dead, we compute a suspicion level (Φ):
- Φ < 1: Normal operation
- 1 ≤ Φ < 8: Suspect (increase monitoring)
- Φ ≥ 8: Presumed dead (initiate failover)

Advantages:
- Adapts to varying network conditions
- Provides confidence level for cascading decisions
- Reduces false positives during GC pauses

### Self-Healing Pipeline

```
Detection → Quarantine → Diagnosis → Recovery/Removal
    │            │           │              │
    ▼            ▼           ▼              ▼
 φ-accrual   Fence all   Run health     If healthy:
 threshold   traffic     checks          reintegrate
             from node                  If not: remove
```

---

## 8. Observability

### Distributed Tracing

Every request carries a trace context:
- Trace ID: 128-bit globally unique
- Span ID: 64-bit unique within trace
- Parent Span ID: Links causal relationships
- Baggage: Key-value pairs propagated through system

### Causal Logging

Logs include vector clocks for ordering:
```json
{
  "timestamp": "2026-02-03T10:15:30.000Z",
  "vector_clock": {"node-1": 42, "node-2": 37, "node-3": 41},
  "event": "TaskCompleted",
  "task_id": "t-12345"
}
```

### Proof Bundles

Every task completion generates a proof bundle:
- Input hash (deterministic content hash)
- Execution log (all state transitions)
- Output hash
- Signed by executing node
- Stored for compliance (configurable retention)

---

## 9. Security Model

### Node Identity

```
┌─────────────────────────────────────────────────────────────┐
│                Node Identity Chain                           │
├─────────────────────────────────────────────────────────────┤
│  1. Bootstrap: Node generates Ed25519 keypair               │
│  2. Registration: Node CSR sent to cluster CA               │
│  3. Attestation: TPM-based hardware attestation (optional)  │
│  4. Certificate: X.509 cert with node ID, roles, expiry     │
│  5. Rotation: Automatic renewal before 50% of validity      │
└─────────────────────────────────────────────────────────────┘
```

### Signed State Transitions

All Raft log entries include:
- Proposer node signature
- Epoch number (prevents replay)
- Hash chain linking to previous entry

### Tenant Isolation

```
┌─────────────────────────────────────────────────────────────┐
│               Isolation Hierarchy                            │
├─────────────────────────────────────────────────────────────┤
│  Level 1: Namespace isolation (logical)                     │
│  Level 2: Resource quotas (prevents noisy neighbor)         │
│  Level 3: Network policies (L3/L4 filtering)                │
│  Level 4: Workload isolation (containers/VMs)               │
│  Level 5: GreyAV anomaly detection (behavioral)             │
└─────────────────────────────────────────────────────────────┘
```

---

## 10. Deployment Models

| Mode | Nodes | Use Case | Consensus |
|------|-------|----------|-----------|
| Dev | 1 | Local development | Disabled (single-node) |
| Local | 3-5 | Integration testing | Full Raft |
| Cloud | 5-99 | Production | Raft + witness |
| Hybrid | N | Edge + central | Hierarchical consensus |

### Hybrid Federation

```
                    ┌─────────────────┐
                    │  Central Cloud  │
                    │  (Source of     │
                    │   Truth)        │
                    └────────┬────────┘
           ┌─────────────────┼─────────────────┐
           │                 │                 │
    ┌──────▼─────┐    ┌──────▼─────┐    ┌──────▼─────┐
    │ Edge Site A│    │ Edge Site B│    │ Edge Site C│
    │ (3 nodes)  │    │ (3 nodes)  │    │ (3 nodes)  │
    └────────────┘    └────────────┘    └────────────┘
```

Each edge site runs local consensus; central cloud provides:
- Global configuration distribution
- Cross-site coordination
- Aggregated observability
- Disaster recovery

---

## Appendix: Key Tradeoffs

| Tradeoff | Our Choice | Rationale |
|----------|------------|-----------|
| CAP | CP (Consistency + Partition Tolerance) | Financial/compliance workloads require consistency |
| Latency vs. Durability | Durable by default, async option available | Data loss is unacceptable for most use cases |
| Complexity vs. Features | Feature-rich with modular opt-in | Customers can disable unused features |
| Push vs. Pull | Hybrid (push for real-time, pull for batch) | Matches workload characteristics |
