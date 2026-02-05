# Grey Distributed — Integration Test Workflows

This document describes the end-to-end workflows validated by integration tests.

## Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                          Grey Distributed Test Matrix                        │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────┐    ┌─────────────┐  │
│  │  Bootstrap  │───►│  Pipeline   │───►│  Recovery   │───►│  Production │  │
│  │   Tests     │    │   Tests     │    │   Tests     │    │   Ready     │  │
│  └─────────────┘    └─────────────┘    └─────────────┘    └─────────────┘  │
│         │                 │                  │                  ▲          │
│         └─────────────────┴──────────────────┴──────────────────┘          │
│                                                                              │
│  Cross-cutting:                                                              │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────┐                      │
│  │Multi-Tenant │    │  Network    │    │  Security   │                      │
│  │  Isolation  │    │   Stress    │    │   & Auth    │                      │
│  └─────────────┘    └─────────────┘    └─────────────┘                      │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

## 1. Cluster Bootstrapping Workflow

**Test File**: `tests/bootstrap.rs`

### Purpose

Validates that a Grey cluster can initialize from scratch and reach a stable operating state.

### Workflow Steps

```
┌──────────────────────────────────────────────────────────────────────┐
│ 1. CREATE NODES                                                       │
│    ┌─────┐     ┌─────┐     ┌─────┐                                  │
│    │Node1│     │Node2│     │Node3│                                  │
│    └──┬──┘     └──┬──┘     └──┬──┘                                  │
│       │           │           │                                      │
├───────┼───────────┼───────────┼──────────────────────────────────────┤
│ 2. BOOTSTRAP LEADER                                                  │
│       │                                                              │
│       ▼                                                              │
│    Node1.bootstrap()  ────► Role=Leader, Term=1                      │
│                                                                      │
├──────────────────────────────────────────────────────────────────────┤
│ 3. INITIALIZE GOVERNANCE                                             │
│                                                                      │
│    QuotaManager.register_tenant(system)                              │
│    QuotaManager.register_tenant(default)                             │
│                                                                      │
├──────────────────────────────────────────────────────────────────────┤
│ 4. REGISTER WORKERS                                                  │
│                                                                      │
│    Scheduler.register_worker(node2, resources)                       │
│    Scheduler.register_worker(node3, resources)                       │
│                                                                      │
├──────────────────────────────────────────────────────────────────────┤
│ 5. VERIFY CLUSTER                                                    │
│                                                                      │
│    assert!(leader.role == Leader)                                    │
│    assert!(quotas.active)                                            │
│    assert!(workers.registered)                                       │
│                                                                      │
└──────────────────────────────────────────────────────────────────────┘
```

### Key Invariants

| Invariant | Description |
|-----------|-------------|
| Single Leader | Exactly one node has `role == Leader` |
| Term Monotonic | Term number never decreases |
| Quorum Exists | Majority of voters reachable |
| Quota Active | All tenants have enforced quotas |

### Test Cases

- `test_single_node_bootstrap` - Single node becomes leader
- `test_multi_node_cluster_formation` - Multiple nodes form cluster
- `test_leader_election_mechanics` - Vote handling works correctly
- `test_reject_stale_term_votes` - Old terms are rejected
- `test_quota_initialization` - Quotas are properly set
- `test_worker_registration` - Workers can register

---

## 2. Task Execution Pipeline Workflow

**Test File**: `tests/task_pipeline.rs`

### Purpose

Validates the complete lifecycle of a distributed task from submission to completion.

### Workflow Steps

```
┌──────────────────────────────────────────────────────────────────────┐
│                         TASK LIFECYCLE                                │
└──────────────────────────────────────────────────────────────────────┘

    ┌─────────┐      ┌─────────┐      ┌─────────┐      ┌─────────┐
    │ Submit  │─────►│Schedule │─────►│ Execute │─────►│Complete │
    └────┬────┘      └────┬────┘      └────┬────┘      └────┬────┘
         │                │                │                │
         ▼                ▼                ▼                ▼
    ┌─────────┐      ┌─────────┐      ┌─────────┐      ┌─────────┐
    │ Pending │      │Assigned │      │ Running │      │Completed│
    └─────────┘      └─────────┘      └─────────┘      └─────────┘

    ┌──────────────────────────────────────────────────────────────┐
    │ PROOF ARTIFACT                                               │
    │  - task_id: TaskId                                           │
    │  - worker_id: NodeId                                         │
    │  - started_at: Instant                                       │
    │  - completed_at: Instant                                     │
    │  - cpu_time_us: u64                                          │
    │  - result_hash: [u8; 32]                                     │
    └──────────────────────────────────────────────────────────────┘
```

### State Machine

```
               ┌──────────────────────────────────────┐
               │                                      │
               ▼         fail (retries < max)         │
    ┌─────────────────┐ ─────────────────────────────►│
    │     Pending     │                               │
    └────────┬────────┘                               │
             │ schedule                               │
             ▼                                        │
    ┌─────────────────┐                               │
    │    Assigned     │                               │
    └────────┬────────┘                               │
             │ start                                  │
             ▼                                        │
    ┌─────────────────┐      fail (retries >= max)    │
    │    Running      │ ─────────────────────────┐    │
    └────────┬────────┘                          │    │
             │ complete                          │    │
             ▼                                   ▼    │
    ┌─────────────────┐                 ┌─────────────────┐
    │   Completed     │                 │     Failed      │
    └─────────────────┘                 └─────────────────┘
```

### Test Cases

- `test_basic_task_submission` - Task submission generates ID
- `test_deterministic_scheduling` - Same inputs produce same schedule
- `test_resource_governance_enforcement` - Resources are checked
- `test_complete_task_execution_flow` - Full lifecycle works
- `test_priority_based_scheduling` - Priorities are respected
- `test_work_stealing` - Idle workers steal from busy ones
- `test_task_failure_and_retry` - Failures trigger retries
- `test_batch_task_submission` - Batch submission works
- `test_proof_artifact_integrity` - Proofs are accurate

---

## 3. Failure & Recovery Workflow

**Test File**: `tests/failure_recovery.rs`

### Purpose

Validates system resilience when nodes fail and the recovery process.

### Failure Detection

```
┌───────────────────────────────────────────────────────────────────────┐
│                      FAILURE DETECTION                                 │
└───────────────────────────────────────────────────────────────────────┘

    Heartbeat-Based                    Phi Accrual
    ┌────────────────┐                 ┌────────────────┐
    │                │                 │                │
    │  if elapsed >  │                 │  phi = -log(   │
    │    timeout     │                 │    P(alive))   │
    │  then SUSPECT  │                 │                │
    │                │                 │  if phi > 8    │
    │                │                 │  then SUSPECT  │
    └────────────────┘                 └────────────────┘

    Trade-off: Simple vs Adaptive
```

### Recovery Workflow

```
┌──────────────────────────────────────────────────────────────────────┐
│ 1. DETECT FAILURE                                                    │
│                                                                      │
│    HeartbeatDetector.detect() ───► failed_nodes: [NodeId]            │
│                                                                      │
├──────────────────────────────────────────────────────────────────────┤
│ 2. QUARANTINE                                                        │
│                                                                      │
│    for node in failed_nodes:                                         │
│        QuarantineManager.quarantine(node, reason, duration)          │
│                                                                      │
├──────────────────────────────────────────────────────────────────────┤
│ 3. RECONFIGURE MEMBERSHIP                                            │
│                                                                      │
│    if node.role == Leader:                                           │
│        trigger_election()                                            │
│    else:                                                             │
│        remove_from_voters(node)                                      │
│                                                                      │
├──────────────────────────────────────────────────────────────────────┤
│ 4. REPLAY INCOMPLETE TASKS                                           │
│                                                                      │
│    events = EventLog.load_from_checkpoint()                          │
│    incomplete = find_incomplete_tasks(events)                        │
│    for task in incomplete:                                           │
│        Scheduler.resubmit(task)                                      │
│                                                                      │
├──────────────────────────────────────────────────────────────────────┤
│ 5. RESTORE NODE (when it comes back)                                 │
│                                                                      │
│    node.recover()                                                    │
│    verify_health(node)                                               │
│    lift_quarantine(node)                                             │
│    rejoin_cluster(node)                                              │
│                                                                      │
└──────────────────────────────────────────────────────────────────────┘
```

### Circuit Breaker States

```
                 failures >= threshold
    ┌───────────┐ ──────────────────────► ┌───────────┐
    │  CLOSED   │                         │   OPEN    │
    │ (normal)  │ ◄────────────────────── │ (failing) │
    └───────────┘   successes >= threshold└─────┬─────┘
         ▲                                      │
         │           timeout expires            │
         │                    ┌─────────────────┘
         │                    ▼
         │              ┌───────────┐
         └──────────────│ HALF-OPEN │
           success      │  (probing)│
                        └───────────┘
```

### Test Cases

- `test_heartbeat_failure_detection` - Heartbeat timeout detection
- `test_phi_accrual_detection` - Probabilistic detection
- `test_quarantine_after_failure` - Failed nodes isolated
- `test_circuit_breaker_activation` - Breaker opens/closes correctly
- `test_membership_reconfiguration` - Cluster reconfigures
- `test_task_replay_after_recovery` - Incomplete tasks found
- `test_leader_failover` - New leader elected
- `test_recovery_after_partition` - Partition healing works
- `test_cascading_failure_prevention` - Failures don't cascade

---

## 4. Multi-Tenant Isolation Workflow

**Test File**: `tests/multi_tenant.rs`

### Purpose

Validates that tenants are isolated from each other in a shared cluster.

### Isolation Model

```
┌───────────────────────────────────────────────────────────────────────┐
│                         MULTI-TENANT ISOLATION                         │
└───────────────────────────────────────────────────────────────────────┘

     Tenant 1                Shared Infra               Tenant 2
    ┌─────────┐             ┌───────────┐             ┌─────────┐
    │  Tasks  │             │ Scheduler │             │  Tasks  │
    │  Quota  │◄───────────►│   Nodes   │◄───────────►│  Quota  │
    │ Metrics │             │  Storage  │             │ Metrics │
    └─────────┘             └───────────┘             └─────────┘
         │                       │                         │
         │    ┌──────────────────┴─────────────────────┐   │
         │    │           ISOLATION BOUNDARY           │   │
         │    └──────────────────┬─────────────────────┘   │
         │                       │                         │
         └───────────────────────┴─────────────────────────┘
                               ✗
                      NO CROSS-TENANT ACCESS
```

### Quota Enforcement

```
┌─────────────────────────────────────────────────────────────────────────┐
│ TenantQuota                                                             │
├─────────────────────────────────────────────────────────────────────────┤
│ - tenant_id: TenantId                                                   │
│ - max_memory_bytes: u64                                                 │
│ - max_cpu_units: u32                                                    │
│ - max_storage_bytes: u64                                                │
│ - max_tasks_per_second: u64                                             │
│ - max_concurrent_tasks: u64                                             │
│ - priority_weight: f64                                                  │
└─────────────────────────────────────────────────────────────────────────┘

    Request ──► Check Quota ──┬──► Allowed ──► Process
                              │
                              └──► Denied ──► Reject + Log
```

### Fair Share Scheduling

```
    Tenant A (weight=1)      Tenant B (weight=2)
         │                        │
         ▼                        ▼
    ┌─────────┐              ┌─────────┐
    │ ~33% CPU│              │ ~67% CPU│
    └─────────┘              └─────────┘

    Fair share = (tenant_weight / sum_weights) × total_resources
```

### Test Cases

- `test_quota_enforcement` - Quotas limit resources
- `test_fair_scheduling` - Equal tenants get equal share
- `test_weighted_fair_scheduling` - Weights affect allocation
- `test_resource_isolation` - No cross-tenant access
- `test_noisy_neighbor_prevention` - One tenant can't affect others
- `test_concurrent_tenant_submission` - Concurrency is safe
- `test_observability_separation` - Metrics are per-tenant

---

## 5. Network Stress Workflow

**Test File**: `tests/network_stress.rs`

### Purpose

Validates system behavior under adverse network conditions.

### Stress Scenarios

```
┌───────────────────────────────────────────────────────────────────────┐
│                         NETWORK STRESS SCENARIOS                       │
└───────────────────────────────────────────────────────────────────────┘

    1. PACKET LOSS                  2. LATENCY JITTER
    ┌───────────────┐               ┌───────────────┐
    │ ● ● ● ✗ ● ✗ ● │               │ 10ms          │
    │   10% loss    │               │ 60ms ←─ jitter│
    └───────────────┘               │ 15ms          │
                                    └───────────────┘

    3. PARTITION                    4. CONGESTION
    ┌───────┐ ✗ ┌───────┐          ┌───────────────┐
    │ A ──► B   │ C ◄── │          │ ▓▓▓▓▓▓▓▓▓▓    │
    │           │       │          │ queue buildup │
    └───────┘   └───────┘          └───────────────┘
```

### Backpressure Mechanism

```
    Sender                                    Receiver
    ┌──────┐                                  ┌──────┐
    │ Send │──────► Queue ─────────────────►  │ Proc │
    │ Rate │       ┌────────────────────┐     │ Rate │
    └──┬───┘       │▓▓▓▓▓▓▓▓░░░░░░░░░░░│     └──────┘
       │           └────────────────────┘
       │                    │
       │◄───────────────────┘
       │     BACKPRESSURE SIGNAL
       │     (slow down / reject)
       ▼
    Reduce send rate
```

### Test Cases

- `test_normal_network_operation` - Baseline healthy network
- `test_packet_loss_handling` - System handles loss
- `test_latency_jitter` - Handles variable latency
- `test_network_partition` - Handles partitions
- `test_backpressure_propagation` - Backpressure works
- `test_circuit_breaker_activation` - Breakers protect system
- `test_recovery_after_congestion` - Recovers when clear
- `test_queue_depth_monitoring` - Queue signals work
- `test_throughput_under_stress` - Degrades gracefully

---

## 6. Security & Attestation Workflow

**Test File**: `tests/security.rs`

### Purpose

Validates security mechanisms for node authentication and attestation.

### Authentication Flow

```
┌──────────────────────────────────────────────────────────────────────┐
│                         NODE JOIN AUTHENTICATION                      │
└──────────────────────────────────────────────────────────────────────┘

    Node                                              Cluster
    ┌────┐                                            ┌────────┐
    │    │ ─────── 1. Present Certificate ──────────► │        │
    │    │                                            │        │
    │    │ ◄────── 2. Challenge ────────────────────  │ Verify │
    │    │                                            │ Chain  │
    │    │ ─────── 3. Attestation Evidence ─────────► │        │
    │    │                                            │        │
    │    │ ◄────── 4. Accept/Reject ────────────────  │        │
    └────┘                                            └────────┘
```

### Attestation Types

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│    HARDWARE     │     │    SOFTWARE     │     │      NONE       │
│   (SGX/TDX)     │     │   (Fallback)    │     │   (Rejected)    │
├─────────────────┤     ├─────────────────┤     ├─────────────────┤
│ + Tamper-proof  │     │ + Works without │     │ - No trust      │
│ + Root of trust │     │   special HW    │     │ - Blocked       │
│ - HW required   │     │ - Spoofable     │     │                 │
└─────────────────┘     └─────────────────┘     └─────────────────┘
        ▲                       ▲                       ▲
        │                       │                       │
        └───────────────────────┴───────────────────────┘
                         Trust Level: High ───────► Low
```

### Certificate Validation

```
    Certificate
    ┌────────────────────────────────────────┐
    │ subject: NodeId                        │
    │ issuer: CA_NodeId                      │  1. Check expiration
    │ not_before: timestamp                  │  2. Check signature
    │ not_after: timestamp                   │  3. Check issuer is trusted
    │ signature: [u8; 64]                    │
    └────────────────────────────────────────┘
              │
              ▼
    ┌────────────────────────────────────────┐
    │  is_expired? ─────► REJECT             │
    │  is_valid_sig? ──► continue / REJECT   │
    │  is_trusted_ca? ─► ACCEPT / REJECT     │
    └────────────────────────────────────────┘
```

### Test Cases

- `test_identity_generation` - Key derivation works
- `test_certificate_validation` - Cert checks are correct
- `test_hardware_attestation` - TEE attestation verified
- `test_software_attestation` - Fallback attestation works
- `test_node_join_authentication` - Full auth flow
- `test_unauthorized_node_rejection` - Bad nodes rejected
- `test_audit_logging` - Events are logged
- `test_node_blocking` - Compromised nodes blocked
- `test_trust_verification` - Trust status accurate

---

## Running Tests

```bash
# Run all integration tests
cargo test --tests

# Run specific test file
cargo test --test bootstrap
cargo test --test task_pipeline
cargo test --test failure_recovery
cargo test --test multi_tenant
cargo test --test network_stress
cargo test --test security

# Run with output
cargo test --tests -- --nocapture

# Run specific test
cargo test test_complete_bootstrap_workflow
```

## Test Coverage Goals

| Workflow | Coverage Target | Critical Path |
|----------|-----------------|---------------|
| Bootstrap | 90% | Raft initialization |
| Pipeline | 85% | Task state machine |
| Recovery | 90% | Failure detection |
| Multi-Tenant | 85% | Quota enforcement |
| Network | 80% | Backpressure, circuit breaker |
| Security | 95% | Authentication, attestation |

## Design Decisions in Tests

### 1. Simulated vs Real Components

Tests use simulated components for:
- **Determinism**: Same inputs produce same outputs
- **Speed**: No network delays or I/O
- **Isolation**: Tests don't affect each other

Trade-off: May miss real-world timing issues.

### 2. Synchronous Testing

Integration tests run synchronously for:
- **Reproducibility**: Results are consistent
- **Debugging**: Easier to trace failures

Trade-off: Doesn't test async race conditions.

### 3. In-Memory State

All state is in-memory for:
- **Performance**: Fast test execution
- **Cleanup**: No leftover files

Trade-off: Doesn't test persistence layer.

## Future Enhancements

1. **Chaos Engineering**: Add fault injection in production-like environment
2. **Property-Based Testing**: Use QuickCheck for invariant verification
3. **Fuzzing**: Fuzz protocol handlers for security
4. **Performance Benchmarks**: Add latency/throughput benchmarks
5. **End-to-End Tests**: Test with real network between processes
