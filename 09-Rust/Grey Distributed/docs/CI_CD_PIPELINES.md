# Grey Distributed — CI/CD Pipeline Design

This document describes the CI/CD pipeline architecture, design decisions, and operational procedures for Grey Distributed.

## Pipeline Overview

```
┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│   Build &   │───▶│    Test     │───▶│   Verify    │
│    Lint     │    │   Suite     │    │  (TLA+)     │
└─────────────┘    └─────────────┘    └─────────────┘
       │                  │                  │
       ▼                  ▼                  ▼
┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│  Security   │    │ Benchmark   │    │   Deploy    │
│    Scan     │    │    Suite    │    │  Pipeline   │
└─────────────┘    └─────────────┘    └─────────────┘
```

## Pipeline Files

| File | Purpose | Trigger |
|------|---------|---------|
| `build.yml` | Compile Rust/Go, lint, static analysis | Push, PR |
| `test.yml` | Unit tests, integration tests, coverage | Push, PR, Nightly |
| `verify.yml` | TLA+ model checking, invariant tests | Push, PR, Nightly |
| `benchmark.yml` | Performance benchmarks, stress tests | PR (perf paths), Nightly |
| `security.yml` | Vulnerability scans, attestation tests | Push, PR, Daily |
| `deploy.yml` | Container builds, multi-env deployment | Push (main), Tags, Manual |

---

## 1. Build & Lint Pipeline

### Purpose
First gate ensuring code compiles and meets style/safety requirements.

### Stages

| Stage | Description | Failure Action |
|-------|-------------|----------------|
| Rust Build | Compile debug + release, check formatting | Block PR |
| Go Build | Compile, cross-platform check, lint | Block PR |
| Dependency Audit | Check for CVEs in dependencies | Advisory |

### Design Decisions

**Why separate Rust and Go builds?**
- Different toolchains require separate caching strategies
- Independent failures help isolate issues
- Parallel execution reduces total time

**Why strict Clippy lints?**
- `-D clippy::unwrap_used`: Prevents panics in production
- `-D clippy::expect_used`: Forces explicit error handling
- These catch ~15% of production bugs at compile time

**Tradeoffs**
- Strict lints may reject valid code patterns
- Dual-language builds take 5-7 minutes
- Cache invalidation on lockfile changes rebuilds everything

---

## 2. Test Suite Pipeline

### Purpose
Comprehensive testing across unit, integration, and property-based tests.

### Stages

| Stage | Tests | Coverage Threshold |
|-------|-------|-------------------|
| Rust Unit Tests | All `#[test]` functions | 70% |
| Go Unit Tests | All `_test.go` files | 80% |
| Rust Integration | 6 integration test files | N/A |
| Go Integration | Live cluster tests | N/A |
| Property Tests | Proptest, fuzz (nightly) | N/A |

### Integration Test Suites

| Suite | Purpose | Timeout |
|-------|---------|---------|
| `bootstrap` | Cluster formation, leader election | 2 min |
| `task_pipeline` | End-to-end task execution | 3 min |
| `failure_recovery` | Node failure, recovery | 4 min |
| `multi_tenant` | Isolation, quotas, fairness | 3 min |
| `network_stress` | Packet loss, partitions | 5 min |
| `security` | Attestation, certificates | 3 min |

### Design Decisions

**Why run integration tests in a matrix?**
- Parallel execution across 6 test files
- Failure isolation per suite
- Easier to identify which subsystem failed

**Why 70% Rust / 80% Go coverage?**
- Rust's type system catches bugs Go doesn't
- Go code handles more I/O and requires more test coverage
- Thresholds based on empirical defect rates

**Tradeoffs**
- Full test suite takes 10-15 minutes
- Race detection (`-race`) adds 2-10x overhead
- Integration tests require cluster setup time

---

## 3. Formal Verification Pipeline

### Purpose
Mathematically prove safety properties hold for all possible executions.

### TLA+ Specifications

| Spec | Properties Verified |
|------|---------------------|
| `GreyConsensus.tla` | Agreement, LogMatching, Termination |
| `GreyReplication.tla` | Consistency, Durability |
| `GreyScheduler.tla` | Fairness, NoStarvation |

### Invariants Checked

```
Agreement:        ∀ committed entries identical across nodes
LogMatching:      ∀ terms, if entries match at index, all prior match
NoQuotaViolation: ∀ tenants, resource usage ≤ allocated quota
```

### Design Decisions

**Why TLA+ over other formal methods?**
- Industry proven (Amazon, Microsoft)
- Models concurrent/distributed systems well
- Can verify unbounded state spaces with symmetry reduction

**Extended verification (nightly)**
- 5 nodes, 5 terms, 5 log entries
- Takes 1-2 hours but finds edge cases missed by smaller models
- 8GB heap required for state space exploration

**Tradeoffs**
- TLA+ has steep learning curve
- Runtime invariant checks add ~5% overhead
- Model checking is exponential in state space size

---

## 4. Performance Benchmark Pipeline

### Purpose
Detect performance regressions and establish baseline metrics.

### Benchmarks

| Benchmark | Metric | Threshold |
|-----------|--------|-----------|
| Consensus latency | p99 | < 100ms |
| Scheduler throughput | ops/sec | > 10,000 |
| Network stress | p99.9 | < 500ms |
| Memory growth | MB/hour | < 50 |

### Design Decisions

**Why run benchmarks on PRs (perf paths only)?**
- Full benchmarks on every PR waste resources
- Path filters target consensus, scheduler, storage, network
- Still runs nightly for complete coverage

**Why compare against main branch?**
- Establishes relative performance change
- Catches regressions before merge
- Uses `benchstat` / `critcmp` for statistical comparison

**Tradeoffs**
- CI runners have variable performance (±10%)
- Microbenchmarks don't capture real-world behavior
- Long stress tests occupy CI resources

### Performance Economics Report

Generated nightly with:
- CPU/RAM utilization trends
- Scaling efficiency metrics
- Recommendations for bottleneck investigation

---

## 5. Security Pipeline

### Purpose
Identify vulnerabilities, verify attestation, and test isolation.

### Security Scans

| Scan | Tool | Severity |
|------|------|----------|
| Rust dependencies | cargo-audit | Critical = Block |
| Go dependencies | govulncheck | High = Warn |
| Container | Trivy | Critical/High |
| SAST | gosec, clippy | High = Warn |
| Secrets | gitleaks | Any = Block |

### Security Tests

| Test | Purpose |
|------|---------|
| Attestation tests | Verify node identity chain |
| Tenant isolation | Cross-tenant access blocked |
| State authorization | No unauthorized transitions |
| Injection tests | SQL/Command injection protection |

### Design Decisions

**Why block on secrets but not all vulnerabilities?**
- Secrets in repo = immediate breach risk
- CVEs may have no exploit path in our code
- Blocking on all CVEs would halt development

**Why daily (not hourly) security scans?**
- CVE databases update daily
- Reduces unnecessary compute
- Critical issues trigger manual workflow

**Tradeoffs**
- Deep penetration tests are slow (30+ min)
- False positives require triage time
- Attestation tests simulate TEE (no real hardware in CI)

---

## 6. Deployment Pipeline

### Environments

| Environment | Strategy | Approval |
|-------------|----------|----------|
| Dev | Direct deploy | None |
| Staging | Blue-Green | None |
| Production | Canary (10% → 50% → 100%) | Manual |

### Deployment Flow

```
Build Images
     │
     ▼
┌────────────────────────────────────────────┐
│                Dev Deploy                   │
│  • Start local cluster                      │
│  • Run smoke tests                          │
└────────────────────────────────────────────┘
     │
     ▼
┌────────────────────────────────────────────┐
│              Staging Deploy                 │
│  • Blue-Green deployment                    │
│  • Pre-switch tests on new version          │
│  • Traffic switch                           │
│  • Post-switch validation                   │
└────────────────────────────────────────────┘
     │
     ▼ (manual approval)
┌────────────────────────────────────────────┐
│            Production Deploy                │
│  • Canary at 10% (10 min monitor)           │
│  • Scale to 50% (5 min monitor)             │
│  • Full rollout to stable                   │
│  • Edge node deployment                     │
└────────────────────────────────────────────┘
```

### Rollback Procedure

1. **Staging**: Traffic switch back to old color (< 30s)
2. **Production**: `kubectl rollout undo` (< 2 min)
3. **Edge**: DaemonSet rollback via annotation

### Design Decisions

**Why Blue-Green for staging?**
- Instant rollback capability
- No mixed-version traffic
- Simpler than canary for pre-prod

**Why Canary for production?**
- Limits blast radius of bad deploys
- Metrics-based automatic rollback
- Gradual confidence building

**Tradeoffs**
- Blue-Green requires 2x staging resources
- Canary requires traffic splitting (Istio/Linkerd)
- Edge deployments have higher latency tolerance

---

## Operational Procedures

### Manual Workflow Triggers

```bash
# Run extended verification
gh workflow run verify.yml -f extended=true

# Deploy to specific environment
gh workflow run deploy.yml -f environment=staging

# Production rollback
gh workflow run deploy.yml -f environment=production -f rollback=true

# Deep security scan
gh workflow run security.yml -f deep_scan=true
```

### Monitoring Pipelines

- **Slack notifications**: #grey-alerts (failures), #grey-deploys (deployments)
- **GitHub Actions dashboard**: https://github.com/{org}/grey/actions
- **Artifacts retention**: 7 days (tests), 30 days (security/benchmarks), 90 days (performance reports)

### Pipeline Maintenance

| Task | Frequency | Owner |
|------|-----------|-------|
| Update TLA+ tools version | Quarterly | Platform |
| Review coverage thresholds | Monthly | Tech Lead |
| Calibrate performance thresholds | Monthly | Performance |
| Audit security scan false positives | Weekly | Security |
| Prune old artifacts | Automatic | GitHub |

---

## Cost Optimization

### CI Minutes Estimation (Monthly)

| Pipeline | Runs/Month | Minutes/Run | Total |
|----------|------------|-------------|-------|
| Build | 200 | 7 | 1,400 |
| Test | 200 | 15 | 3,000 |
| Verify | 50 | 30 | 1,500 |
| Benchmark | 30 | 45 | 1,350 |
| Security | 30 | 25 | 750 |
| Deploy | 20 | 20 | 400 |
| **Total** | | | **8,400** |

### Optimization Strategies

1. **Caching**: Cargo/Go module caches reduce rebuild time by 60%
2. **Path filters**: Only run benchmarks on performance-critical changes
3. **Concurrency limits**: Prevent duplicate runs on same branch
4. **Conditional jobs**: Skip extended tests on non-main branches

---

## Troubleshooting

### Common Issues

| Issue | Cause | Resolution |
|-------|-------|------------|
| Flaky tests | Race condition | Enable `-race`, add sync points |
| TLA+ timeout | State space explosion | Reduce model bounds |
| Cache miss | Lockfile changed | Expected, wait for rebuild |
| Deployment stuck | Pod not ready | Check liveness probe, resource limits |
| Security scan false positive | CVE in unused code path | Add to ignore list with justification |

### Debugging Failed Pipelines

1. Check the job logs in GitHub Actions
2. Download artifacts for detailed outputs
3. Re-run with debug logging: `ACTIONS_STEP_DEBUG=true`
4. For integration tests, check cluster logs artifact

---

## Future Improvements

- [ ] Add Chaos Mesh integration for production chaos testing
- [ ] Implement GitOps with ArgoCD for declarative deployments
- [ ] Add OpenTelemetry tracing to pipeline stages
- [ ] Implement deployment windows (no Friday deploys)
- [ ] Add cost tracking per pipeline

---

*Last updated: 2026-02-03*
