# Minimal 3-Node Cluster Case Study

This case study walks through setting up, running, and validating a 3-node Grey Distributed cluster using the minimal reference implementation.

## Prerequisites

- Rust 1.70+
- Docker 24.0+ (for containerized deployment)
- kubectl 1.28+ (for Kubernetes deployment)
- 4 GB RAM, 2 CPU cores

## Cluster Topology

```
┌─────────────────────────────────────────────────────────────────┐
│                    3-Node Grey Cluster                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                   │
│   ┌───────────┐       ┌───────────┐       ┌───────────┐         │
│   │  Node 0   │◀─────▶│  Node 1   │◀─────▶│  Node 2   │         │
│   │  (Leader) │       │ (Follower)│       │ (Follower)│         │
│   │           │       │           │       │           │         │
│   │ Port 5000 │       │ Port 5001 │       │ Port 5002 │         │
│   └───────────┘       └───────────┘       └───────────┘         │
│         │                   │                   │               │
│         └───────────────────┼───────────────────┘               │
│                             │                                    │
│                             ▼                                    │
│                    ┌─────────────────┐                          │
│                    │   Raft Consensus │                          │
│                    │   Quorum: 2/3    │                          │
│                    └─────────────────┘                          │
│                                                                   │
└─────────────────────────────────────────────────────────────────┘
```

## Step 1: Build the Reference Implementation

```bash
# Clone and build
cd /path/to/grey-distributed

# Build minimal implementation
cargo build --release --features minimal

# Verify build
./target/release/greyd --version
```

## Step 2: Start the Cluster

### Option A: Local Processes

```bash
# Terminal 1: Node 0 (initial leader)
GREY_NODE_ID=0 \
GREY_CLUSTER_PEERS=localhost:5001,localhost:5002 \
GREY_LISTEN_ADDR=localhost:5000 \
./target/release/greyd

# Terminal 2: Node 1
GREY_NODE_ID=1 \
GREY_CLUSTER_PEERS=localhost:5000,localhost:5002 \
GREY_LISTEN_ADDR=localhost:5001 \
./target/release/greyd

# Terminal 3: Node 2
GREY_NODE_ID=2 \
GREY_CLUSTER_PEERS=localhost:5000,localhost:5001 \
GREY_LISTEN_ADDR=localhost:5002 \
./target/release/greyd
```

### Option B: Docker Compose

```bash
cd repro
docker-compose up -d

# View logs
docker-compose logs -f
```

### Option C: Kubernetes

```bash
cd repro/k8s
kubectl apply -f minimal_cluster.yaml

# Wait for pods
kubectl wait --for=condition=ready pod -l app=grey-node --timeout=120s

# View logs
kubectl logs -l app=grey-node -f
```

## Step 3: Verify Cluster Formation

### Check Cluster Status

```bash
# Using greyctl
greyctl cluster status

# Expected output:
# Cluster: grey-minimal
# State: Healthy
# Leader: node-0 (term: 1)
# Nodes:
#   - node-0: Leader, term=1, log_index=0
#   - node-1: Follower, term=1, log_index=0
#   - node-2: Follower, term=1, log_index=0
```

### Verify Consensus

```bash
# Submit a test command
greyctl consensus write "test-key" "test-value"

# Read back (should succeed on any node)
greyctl consensus read "test-key"
# Output: test-value

# Check log replication
greyctl cluster log-status
# Expected:
#   node-0: commit_index=1, last_log_index=1
#   node-1: commit_index=1, last_log_index=1
#   node-2: commit_index=1, last_log_index=1
```

## Step 4: Test Fault Tolerance

### Leader Failure

```bash
# Identify current leader
greyctl cluster leader
# Output: node-0

# Kill the leader
docker-compose stop grey-node-0
# OR
kubectl delete pod grey-node-0

# Wait for new election (should take <500ms)
sleep 2

# Verify new leader elected
greyctl cluster leader
# Output: node-1 or node-2

# Verify cluster still operational
greyctl consensus write "key2" "value2"
greyctl consensus read "key2"
# Output: value2
```

### Network Partition

```bash
# Simulate partition: isolate node-2
iptables -A INPUT -s grey-node-2 -j DROP
iptables -A OUTPUT -d grey-node-2 -j DROP

# Verify cluster still has quorum (2/3)
greyctl cluster status
# Output: 2 nodes healthy, 1 unreachable

# Writes should still succeed
greyctl consensus write "key3" "value3"

# Heal partition
iptables -D INPUT -s grey-node-2 -j DROP
iptables -D OUTPUT -d grey-node-2 -j DROP

# Verify node-2 catches up
sleep 5
greyctl cluster log-status
# All nodes should have same log index
```

### Minority Partition

```bash
# Isolate 2 nodes (minority can't elect leader)
docker-compose stop grey-node-1 grey-node-2

# Remaining node should NOT be able to accept writes
greyctl consensus write "key4" "value4"
# Expected: Error - quorum not available

# Restore nodes
docker-compose start grey-node-1 grey-node-2

# Writes should resume
sleep 3
greyctl consensus write "key4" "value4"
# Expected: Success
```

## Step 5: Benchmark the Cluster

### Run Reproducibility Benchmarks

```bash
# Set deterministic seed
export GREY_SEED=42

# Run benchmarks
cd repro/benchmarks
cargo bench --bench minimal_bench

# Save results
cargo bench --bench minimal_bench -- --save-baseline v1.0
```

### Expected Results

| Benchmark | 3-Node Cluster | 5-Node Cluster | Notes |
|-----------|----------------|----------------|-------|
| Leader election | 5-10ms | 8-15ms | +50% with more nodes |
| Log replication (100 entries) | 50-100ms | 80-150ms | Quorum limited |
| Throughput | 2,000-5,000 ops/s | 1,500-3,500 ops/s | Network bound |
| Read latency (local) | <1ms | <1ms | No consensus needed |
| Read latency (linearizable) | 5-15ms | 10-20ms | Requires quorum |

### Reproduce Specific Workload

```bash
# Run specific benchmark
cargo bench --bench minimal_bench -- consensus/throughput

# Compare against baseline
cargo bench --bench minimal_bench -- --baseline v1.0

# Expected: Results within ±25% of baseline
```

## Step 6: Collect Metrics

### Prometheus Metrics

```bash
# Scrape metrics from each node
curl -s http://localhost:5000/metrics
curl -s http://localhost:5001/metrics
curl -s http://localhost:5002/metrics

# Key metrics to observe:
#   grey_consensus_term
#   grey_consensus_log_index
#   grey_consensus_commit_index
#   grey_cluster_leader (1 for leader, 0 for follower)
#   grey_rpc_latency_seconds
```

### Distributed Traces

```bash
# Enable tracing
export GREY_TRACE_ENDPOINT=http://jaeger:14268/api/traces

# Submit traced request
greyctl --trace consensus write "traced-key" "traced-value"

# View trace in Jaeger UI
open http://localhost:16686
```

## Step 7: Validate Correctness

### Linearizability Check

```bash
# Run linearizability test
cargo test --test linearizability -- --nocapture

# Expected output:
# Running linearizability checker...
# Submitted 1000 operations
# Verified 1000 responses
# Result: PASS (all operations linearizable)
```

### Safety Invariants

```bash
# Verify safety properties
cargo test --test safety_invariants -- --nocapture

# Checks:
# ✓ Only one leader per term
# ✓ Log entries committed are never lost
# ✓ All nodes agree on committed entries
# ✓ Elections respect quorum
```

## Step 8: Cleanup

```bash
# Docker Compose
docker-compose down -v

# Kubernetes
kubectl delete -f minimal_cluster.yaml

# Local processes
pkill -f greyd
```

## Troubleshooting

### Election Not Completing

```
Symptom: No leader elected after 30 seconds
Cause: Network connectivity or clock skew
Fix:
  1. Verify all nodes can reach each other
  2. Check NTP synchronization
  3. Increase election timeout: GREY_ELECTION_TIMEOUT=1000ms
```

### Split-Brain Detected

```
Symptom: Multiple nodes claim leadership
Cause: Network partition healed incorrectly
Fix:
  1. Stop all nodes
  2. Clear state: rm -rf /var/grey/raft/*
  3. Restart with clean state
  4. Re-bootstrap cluster
```

### Log Divergence

```
Symptom: Nodes have different log entries at same index
Cause: Bug or byzantine failure
Fix:
  1. Identify divergent node
  2. Quarantine node: greyctl cluster quarantine node-X
  3. Investigate logs
  4. Restore from healthy node snapshot
```

## Appendix: Configuration Reference

| Environment Variable | Default | Description |
|---------------------|---------|-------------|
| GREY_NODE_ID | 0 | Unique node identifier |
| GREY_LISTEN_ADDR | 0.0.0.0:5000 | Listen address |
| GREY_CLUSTER_PEERS | (empty) | Comma-separated peer addresses |
| GREY_ELECTION_TIMEOUT | 150ms | Election timeout |
| GREY_HEARTBEAT_INTERVAL | 50ms | Leader heartbeat interval |
| GREY_LOG_LEVEL | info | Logging level |
| GREY_DATA_DIR | /var/grey | Data directory |
| GREY_SEED | (random) | Deterministic seed for testing |
