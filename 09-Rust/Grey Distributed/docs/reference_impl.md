# Grey Distributed — Reference Implementation Guide

This document explains the minimal reference implementations in `src/*/minimal.rs`, their design tradeoffs, and how they relate to the production code.

## Overview

The minimal implementations serve three purposes:

1. **Education** — Demonstrate core concepts without production complexity
2. **Verification** — Provide a ground truth for testing the full implementation
3. **Reproducibility** — Enable others to verify our benchmarks and claims

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        Grey Distributed                         │
├─────────────────────────────────────────────────────────────────┤
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐          │
│  │  Consensus   │  │  Scheduler   │  │   Network    │          │
│  │ (Raft-based) │  │  (Priority)  │  │  (Protocol)  │          │
│  └──────────────┘  └──────────────┘  └──────────────┘          │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐          │
│  │   Storage    │  │    Fault     │  │Observability │          │
│  │  (Sharded)   │  │  (Detector)  │  │  (Tracing)   │          │
│  └──────────────┘  └──────────────┘  └──────────────┘          │
└─────────────────────────────────────────────────────────────────┘
```

## Module Reference

### 1. Consensus (`src/consensus/minimal.rs`)

**Purpose:** Implements a minimal Raft consensus protocol for leader election and log replication.

**Key Types:**
```rust
enum NodeState { Follower, Candidate, Leader }
struct RaftNode<T> { state, term, log, voted_for, ... }
struct LogEntry<T> { term, index, command }
```

**Core Operations:**
- `start_election()` — Transition to candidate, request votes
- `handle_vote_request()` — Grant vote if term is higher
- `append_entries()` — Leader appends to log, replicates to followers
- `commit_entries()` — Apply committed entries to state machine

**Tradeoffs:**
| Aspect | Minimal | Production |
|--------|---------|------------|
| Log Storage | In-memory `Vec` | Persistent WAL |
| Network | Direct function calls | Async RPC |
| Concurrency | Single-threaded | Multi-threaded with locks |
| Snapshotting | Not implemented | Log compaction + snapshots |

**Related TLA+ Spec:** [repro/specs/consensus.tla](../repro/specs/consensus.tla)

### 2. Scheduler (`src/scheduler/minimal.rs`)

**Purpose:** Implements priority-based task scheduling with work stealing.

**Key Types:**
```rust
struct Task { id, priority, tenant_id, resources, dependencies }
struct Scheduler { queues: PriorityQueues, workers: Vec<Worker> }
enum TaskState { Pending, Running, Completed, Failed }
```

**Core Operations:**
- `submit(task)` — Add task to priority queue
- `schedule()` — Assign highest-priority ready task to a worker
- `complete(task_id)` — Mark task complete, unblock dependents
- `steal_work(from, to)` — Work stealing for load balancing

**Tradeoffs:**
| Aspect | Minimal | Production |
|--------|---------|------------|
| Priority Queue | `BinaryHeap` | Lock-free skip list |
| Dependencies | Simple DAG | Distributed dependency tracking |
| Fairness | Priority only | Multi-tenant fair scheduling |
| Preemption | Not implemented | Cooperative preemption |

**Related TLA+ Spec:** [repro/specs/governance.tla](../repro/specs/governance.tla) (resource quotas)

### 3. Network (`src/network/minimal.rs`)

**Purpose:** Implements the network protocol for node communication.

**Key Types:**
```rust
enum Message { Heartbeat, VoteRequest, VoteResponse, AppendEntries, ... }
struct Protocol { node_id, peers, message_handler }
struct Connection { peer_id, state, last_seen }
```

**Core Operations:**
- `send(peer, message)` — Send message to peer
- `broadcast(message)` — Send to all peers
- `handle_message(from, message)` — Process incoming message
- `establish_connection(peer)` — Create connection to peer

**Tradeoffs:**
| Aspect | Minimal | Production |
|--------|---------|------------|
| Transport | In-memory channels | TCP/QUIC with TLS |
| Serialization | Direct struct passing | Protobuf/MessagePack |
| Reliability | Assume reliable | Retries, timeouts, backoff |
| Discovery | Static peer list | Dynamic service discovery |

### 4. Storage (`src/storage/minimal.rs`)

**Purpose:** Implements sharded key-value storage with quorum operations.

**Key Types:**
```rust
struct ShardedStorage<K, V> { shards: Vec<Shard<K, V>> }
struct Shard<K, V> { data: HashMap<K, VersionedValue<V>> }
struct VersionedValue<V> { value: V, version: u64, timestamp: u64 }
enum QuorumLevel { One, Majority, All }
```

**Core Operations:**
- `get(key, quorum)` — Read with quorum consistency
- `put(key, value, quorum)` — Write with quorum durability
- `delete(key, quorum)` — Delete with tombstone
- `compare_and_swap(key, expected, new)` — Atomic CAS

**Tradeoffs:**
| Aspect | Minimal | Production |
|--------|---------|------------|
| Persistence | In-memory | RocksDB/PostgreSQL |
| Sharding | Hash-based | Consistent hashing with rebalancing |
| Replication | Implicit via quorum | Explicit replication factor |
| Compaction | Not implemented | Background compaction |

### 5. Fault Detection (`src/fault/minimal.rs`)

**Purpose:** Implements failure detection using the Phi Accrual algorithm.

**Key Types:**
```rust
struct FailureDetector { nodes: HashMap<NodeId, HeartbeatHistory> }
struct HeartbeatHistory { intervals: VecDeque<Duration>, last_heartbeat: Instant }
struct CircuitBreaker { state: CircuitState, failure_count: u32 }
enum NodeState { Healthy, Suspect, Dead, Quarantined }
```

**Core Operations:**
- `record_heartbeat(node)` — Update heartbeat history
- `phi(node)` — Calculate suspicion level (0.0 to ∞)
- `is_alive(node)` — Check if node is considered alive
- `quarantine(node)` — Mark node for quarantine

**Tradeoffs:**
| Aspect | Minimal | Production |
|--------|---------|------------|
| Phi Calculation | Simple mean/stddev | Exponential distribution fit |
| History | Fixed-size deque | Adaptive window |
| Network Model | Assume uniform latency | Network topology awareness |
| Recovery | Manual | Automatic with backoff |

### 6. Observability (`src/observability/minimal.rs`)

**Purpose:** Implements distributed tracing, causal logging, and metrics.

**Key Types:**
```rust
struct TraceContext { trace_id: u128, span_id: u64, parent_id: Option<u64> }
struct Span { context, name, start_time, duration, attributes }
struct LamportClock { counter: AtomicU64 }
struct CausalLog { entries: Vec<CausalEntry>, clock: LamportClock }
```

**Core Operations:**
- `start_span(name)` — Begin a new span
- `end_span()` — Complete span with duration
- `log_causal(message)` — Log with Lamport timestamp
- `increment(counter)` / `set(gauge)` / `observe(histogram)` — Metrics

**Tradeoffs:**
| Aspect | Minimal | Production |
|--------|---------|------------|
| Trace Context | W3C Trace Context | Full OpenTelemetry |
| Export | In-memory buffer | OTLP export to Jaeger/Tempo |
| Sampling | None (all traces) | Head-based/tail-based sampling |
| Metrics | Simple counters | Prometheus exposition format |

## Design Principles

### 1. Simplicity Over Performance

The minimal implementations prioritize clarity:

```rust
// Minimal: Simple loop
fn schedule(&mut self) -> Option<Task> {
    self.queue.pop()  // O(log n) but clear
}

// Production: Lock-free concurrent access
fn schedule(&self) -> Option<Task> {
    self.skip_list.pop_front()  // O(1) amortized, thread-safe
}
```

### 2. Single-Threaded Safety

Minimal implementations avoid threading complexity:

```rust
// Minimal: &mut self guarantees exclusive access
impl RaftNode {
    fn append_entries(&mut self, entries: Vec<LogEntry>) { ... }
}

// Production: Interior mutability with synchronization
impl RaftNode {
    fn append_entries(&self, entries: Vec<LogEntry>) {
        let mut log = self.log.write().unwrap();
        ...
    }
}
```

### 3. Explicit Over Implicit

Error handling is explicit, not hidden:

```rust
// Minimal: Return Result with context
fn put(&mut self, key: K, value: V) -> Result<(), StorageError> {
    if self.is_full() {
        return Err(StorageError::CapacityExceeded);
    }
    self.data.insert(key, value);
    Ok(())
}
```

### 4. Composability

Modules have clear interfaces:

```rust
// Each module exposes a minimal trait
pub trait Consensus {
    fn propose(&mut self, command: Command) -> Result<LogIndex, Error>;
    fn commit_index(&self) -> LogIndex;
}

pub trait Storage {
    fn get(&self, key: &Key) -> Result<Option<Value>, Error>;
    fn put(&mut self, key: Key, value: Value) -> Result<(), Error>;
}
```

## Correctness Claims

The minimal implementations satisfy these properties:

### Consensus
- **Election Safety:** At most one leader per term
- **Log Matching:** Logs with same index and term have same content
- **Leader Completeness:** Committed entries exist in future leaders' logs
- **State Machine Safety:** Same index → same command applied

### Scheduler
- **Priority Ordering:** Higher priority tasks run before lower priority
- **Dependency Respect:** Tasks with unmet dependencies don't run
- **Progress:** Ready tasks eventually execute

### Storage
- **Quorum Consistency:** Majority writes are durable
- **Version Ordering:** Newer versions have higher version numbers
- **CAS Atomicity:** Compare-and-swap is atomic

### Fault Detection
- **Eventually Accurate:** Dead nodes eventually detected
- **No False Positives:** Healthy nodes with timely heartbeats not suspected
- **Bounded Suspicion:** Phi values have consistent interpretation

### Observability
- **Causal Ordering:** Happened-before relationships preserved
- **Trace Completeness:** All spans in a trace are collected
- **Clock Monotonicity:** Lamport clocks never decrease

## Testing the Reference Implementation

### Unit Tests

```bash
# Run minimal implementation tests
cargo test --lib minimal

# Run with output
cargo test --lib minimal -- --nocapture
```

### Property-Based Testing

```rust
#[cfg(test)]
mod tests {
    use proptest::prelude::*;
    
    proptest! {
        #[test]
        fn storage_get_after_put(key: u64, value: Vec<u8>) {
            let mut storage = ShardedStorage::new(4);
            storage.put(key, value.clone(), QuorumLevel::Majority)?;
            let result = storage.get(&key, QuorumLevel::One)?;
            assert_eq!(result, Some(value));
        }
    }
}
```

### Comparison Testing

```rust
#[test]
fn minimal_matches_production() {
    let mut minimal = minimal::RaftNode::new(1);
    let mut production = raft::RaftNode::new(1);
    
    // Apply same operations
    for op in operations {
        minimal.apply(op.clone());
        production.apply(op);
    }
    
    // Verify same state
    assert_eq!(minimal.commit_index(), production.commit_index());
}
```

## Extending the Reference

To add new functionality:

1. **Define Types** — Add to `minimal.rs` with clear documentation
2. **Implement Core Logic** — Keep under 500 lines per module
3. **Add Tests** — Property-based tests preferred
4. **Update TLA+ Spec** — Verify formal properties
5. **Document Tradeoffs** — Explain minimal vs production differences

## Related Documentation

- [Architecture Overview](ARCHITECTURE.md) — Full system design
- [Reproducibility Guide](reproducibility.md) — Running benchmarks
- [Formal Verification](FORMAL_VERIFICATION.md) — TLA+ specifications
- [API Reference](api_reference.md) — Full API documentation

## Version History

| Version | Changes |
|---------|---------|
| 1.0.0 | Initial 6 minimal modules |
| 1.1.0 | Added observability module |
| 1.2.0 | Enhanced fault detection |
