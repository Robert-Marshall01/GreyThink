# Grey Distributed — Reproducibility Guide

This guide explains how to reproduce all benchmarks, verify correctness claims, and validate the Grey Distributed system's behavior.

## Table of Contents

1. [Quick Start](#quick-start)
2. [Environment Setup](#environment-setup)
3. [Running Benchmarks](#running-benchmarks)
4. [Reproducing Case Studies](#reproducing-case-studies)
5. [Formal Verification](#formal-verification)
6. [Validating Results](#validating-results)
7. [Troubleshooting](#troubleshooting)

## Quick Start

```bash
# Clone and build
git clone https://github.com/grey-distributed/grey
cd grey

# Run all benchmarks with deterministic seed
GREY_SEED=42 cargo bench --all

# Verify with TLA+ model checker
cd repro/specs && tlc consensus.tla

# Start local cluster
docker-compose -f repro/docker-compose.yml up -d
```

## Environment Setup

### Requirements

| Component | Minimum Version | Purpose |
|-----------|----------------|---------|
| Rust | 1.75+ | Core implementation |
| Docker | 24.0+ | Container runtime |
| Docker Compose | 2.20+ | Multi-container orchestration |
| TLA+ Tools | 1.8+ | Formal verification |
| Kubernetes | 1.28+ | Production deployment (optional) |
| Go | 1.21+ | CLI and utilities |

### Installing Dependencies

```bash
# Rust toolchain
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustup component add clippy rustfmt

# TLA+ Toolbox
brew install tla-plus-toolbox  # macOS
# Or download from: https://github.com/tlaplus/tlaplus/releases

# Docker
curl -fsSL https://get.docker.com | sh

# Kubernetes tools (optional)
brew install kubectl helm k9s
```

### Building from Source

```bash
# Debug build (fast compilation, slower runtime)
cargo build

# Release build (optimized, use for benchmarks)
cargo build --release

# Run tests
cargo test --all
```

## Running Benchmarks

### Deterministic Benchmarking

All benchmarks use the `GREY_SEED` environment variable for reproducibility:

```bash
# Set seed for deterministic results
export GREY_SEED=42

# Run all benchmarks
cargo bench --all

# Run specific benchmark
cargo bench --bench consensus_bench
cargo bench --bench scheduler_bench
cargo bench --bench network_bench
cargo bench --bench storage_bench
```

### Benchmark Output

Results are saved to `target/criterion/`:

```
target/criterion/
├── consensus/
│   ├── leader_election/
│   │   ├── base/
│   │   └── report/
│   └── log_replication/
├── scheduler/
│   ├── task_submit/
│   └── priority_scheduling/
└── report/
    └── index.html    # Open in browser
```

### Expected Results

| Benchmark | Metric | Expected | Tolerance |
|-----------|--------|----------|-----------|
| Leader Election (3 nodes) | Latency | ~150μs | ±20% |
| Log Replication (100 entries) | Throughput | ~50k/s | ±15% |
| Task Submit | Latency | ~10μs | ±25% |
| Priority Scheduling | Latency | ~25μs | ±20% |

### Minimal Benchmark Harness

For quick verification, use the minimal benchmark:

```bash
# Build minimal benchmark
rustc repro/benchmarks/minimal_bench.rs -o minimal_bench

# Or with Criterion
cd repro/benchmarks
cargo bench
```

## Reproducing Case Studies

### Local Cluster (Docker)

```bash
# Start 3-node cluster with observability
docker-compose -f repro/docker-compose.yml up -d

# Verify cluster health
curl http://localhost:8080/health
curl http://localhost:8081/health
curl http://localhost:8082/health

# View cluster metrics
open http://localhost:9090  # Prometheus
open http://localhost:3200  # Grafana

# Run sample workload
docker-compose exec grey-node-1 greyctl submit --task-file /examples/sample.json

# View traces
open http://localhost:16686  # Jaeger
```

### Kubernetes Cluster

```bash
# Apply manifests
kubectl apply -f repro/k8s/minimal_cluster.yaml

# Wait for pods
kubectl -n grey-system wait --for=condition=Ready pod -l app=grey --timeout=120s

# Check cluster status
kubectl -n grey-system get pods
kubectl -n grey-system logs grey-0

# Port-forward for local access
kubectl -n grey-system port-forward svc/grey-headless 8080:8080
```

### Case Study Walkthrough

Follow the detailed guide in [repro/case_study/minimal_cluster.md](../repro/case_study/minimal_cluster.md):

1. **Build** — Compile from source
2. **Start** — Launch 3-node cluster
3. **Verify** — Health checks and leader election
4. **Test** — Fault tolerance scenarios
5. **Benchmark** — Performance measurements
6. **Validate** — Compare against expected results

## Formal Verification

### TLA+ Model Checking

Grey's safety properties are formally verified using TLA+:

```bash
cd repro/specs

# Verify consensus safety
tlc consensus.tla -config consensus.cfg

# Verify governance (resource quotas)
tlc governance.tla -config governance.cfg

# Verify isolation (tenant separation)
tlc isolation.tla -config isolation.cfg
```

### Configuration Files

Create config files for model checking:

**consensus.cfg:**
```
SPECIFICATION Spec
CONSTANTS
    Nodes = {n1, n2, n3}
    MaxLogLength = 5
    MaxTerm = 3
INVARIANTS
    Safety
    TypeOK
```

**governance.cfg:**
```
SPECIFICATION Spec
CONSTANTS
    Tenants = {t1, t2, t3}
    MaxCPU = 1000
    MaxMemory = 10000
    MaxTasks = 10
INVARIANTS
    Safety
    QuotaEnforcement
```

**isolation.cfg:**
```
SPECIFICATION Spec
CONSTANTS
    Tenants = {t1, t2, t3}
    Namespaces = {ns1, ns2, ns3}
    MaxObjects = 5
INVARIANTS
    Isolation
    MemoryDisjoint
```

### Interpreting Results

Successful verification output:
```
Model checking completed. No error has been found.
  Diameter of state graph: 15
  States found: 12847
  Distinct states: 3421
```

If violations are found, TLC provides a trace:
```
Error: Invariant Safety is violated.
Error: The following behavior constitutes a counterexample:
State 1: <Initial predicate>
State 2: StartElection
...
```

## Validating Results

### Comparing Benchmark Results

```bash
# Run benchmark twice with same seed
GREY_SEED=42 cargo bench --bench consensus_bench -- --save-baseline run1
GREY_SEED=42 cargo bench --bench consensus_bench -- --save-baseline run2

# Compare baselines
cargo bench --bench consensus_bench -- --baseline run1 --compare run2
```

### Statistical Validation

Criterion reports include:
- **Mean** — Average execution time
- **Median** — 50th percentile
- **Std Dev** — Standard deviation
- **MAD** — Median Absolute Deviation
- **Outliers** — Count and classification

Expected variance:
- Same seed, same hardware: <5% difference
- Same seed, different hardware: <25% difference
- Different seeds: Results may vary significantly

### Correctness Validation

```bash
# Run integration tests
cargo test --test failure_recovery
cargo test --test multi_tenant
cargo test --test network_stress

# Run property-based tests
cargo test --test proptest -- --nocapture
```

## Troubleshooting

### Common Issues

**Benchmark variance too high:**
```bash
# Ensure system is idle
# Use taskset for CPU pinning
taskset -c 0-3 cargo bench

# Increase sample size
cargo bench -- --sample-size 500
```

**Docker cluster won't start:**
```bash
# Check logs
docker-compose logs grey-node-1

# Reset volumes
docker-compose down -v
docker-compose up -d
```

**TLA+ model checking is slow:**
```
# Reduce state space
CONSTANTS
    Nodes = {n1, n2}  # Fewer nodes
    MaxLogLength = 3  # Smaller logs
```

**Kubernetes pods not ready:**
```bash
# Check events
kubectl -n grey-system get events --sort-by=.lastTimestamp

# Check resource quotas
kubectl describe ns grey-system

# Check persistent volumes
kubectl -n grey-system get pvc
```

### Getting Help

- **Issues:** https://github.com/grey-distributed/grey/issues
- **Discussions:** https://github.com/grey-distributed/grey/discussions
- **Documentation:** https://docs.grey-distributed.io

## Artifact Summary

| Artifact | Location | Purpose |
|----------|----------|---------|
| Minimal Implementations | `src/*/minimal.rs` | Reference code |
| Benchmark Harness | `repro/benchmarks/` | Performance testing |
| Case Study | `repro/case_study/` | Step-by-step guide |
| Docker Compose | `repro/docker-compose.yml` | Local deployment |
| Kubernetes Manifest | `repro/k8s/` | Production deployment |
| TLA+ Specifications | `repro/specs/` | Formal verification |
| Criterion Benchmarks | `benchmarks/*.rs` | Detailed benchmarks |

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2024-01 | Initial reproducibility bundle |
| 1.1.0 | 2024-02 | Added TLA+ specifications |
| 1.2.0 | 2024-03 | Kubernetes manifests |
