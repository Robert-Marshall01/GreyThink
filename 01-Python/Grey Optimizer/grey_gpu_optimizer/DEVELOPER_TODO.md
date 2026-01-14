# Grey GPU Optimizer - Developer TODO Checklist

> **Version**: 1.0.0  
> **Last Updated**: 2026-01-11  
> **Project**: grey_gpu_optimizer - Production-ready GPU optimization toolkit

This document provides a comprehensive development checklist organized by component.
Each task includes priority, estimated time, relevant files/commands, and acceptance criteria.

---

## Table of Contents

1. [GPU Spec Detection](#1-gpu-spec-detection)
2. [Optimization Planner](#2-optimization-planner)
3. [Enforcement System](#3-enforcement-system)
4. [Daemon](#4-daemon)
5. [CLI Interface](#5-cli-interface)
6. [Logging & Artifacts](#6-logging--artifacts)
7. [Tests](#7-tests)
8. [Deployment & CI/CD](#8-deployment--cicd)

---

## 1. GPU Spec Detection

### Tasks

| # | Task | Priority | Est. Time | File/Command | Status |
|---|------|----------|-----------|--------------|--------|
| 1.1 | Implement nvidia-smi parser | 🔴 Critical | 2h | [optimizer.py](grey_gpu_optimizer/optimizer.py) `_parse_nvidia_smi()` | ✅ Done |
| 1.2 | Implement rocm-smi parser | 🔴 Critical | 2h | [optimizer.py](grey_gpu_optimizer/optimizer.py) `_parse_rocm_smi()` | ✅ Done |
| 1.3 | Implement Intel GPU detection | 🟡 High | 3h | [optimizer.py](grey_gpu_optimizer/optimizer.py) `_detect_intel_gpu()` | ✅ Done |
| 1.4 | Add lspci/lshw fallback | 🟡 High | 1h | [optimizer.py](grey_gpu_optimizer/optimizer.py) `_parse_lspci()` | ✅ Done |
| 1.5 | Add PyTorch/PyCUDA probing | 🟢 Medium | 2h | [optimizer.py](grey_gpu_optimizer/optimizer.py) `_probe_torch_cuda()` | ✅ Done |
| 1.6 | Multi-GPU detection support | 🟢 Medium | 3h | [optimizer.py](grey_gpu_optimizer/optimizer.py) `detect_gpus()` | ⬜ TODO |
| 1.7 | Vulkan info parsing | 🟢 Medium | 1h | [optimizer.py](grey_gpu_optimizer/optimizer.py) `_parse_vulkaninfo()` | ✅ Done |
| 1.8 | Thermal limit extraction | 🟡 High | 1h | Add to nvidia-smi query | ✅ Done |

### Acceptance Criteria

- [ ] `detect_gpus()` returns `list[GPUSpec]` with all GPU fields populated
- [ ] VRAM values match `nvidia-smi` / `rocm-smi` output within 1%
- [ ] Detection works offline (no vendor tools) with lspci fallback
- [ ] Spec is saved to `~/.grey_optimizer/gpu_spec.json`

### 🧪 SMOKE TEST: Detection

```bash
# Run detection and verify output
cd /home/robert-marshall01/Desktop/Grey\ Optimizer
python -c "
from grey_gpu_optimizer import detect_gpus
specs = detect_gpus()
print(f'Detected {len(specs)} GPU(s)')
for s in specs:
    print(f'  {s.vendor}: {s.model} - {s.vram_total_mb}MB VRAM')
"
```

**Expected Output** (example):
```
Detected 1 GPU(s)
  nvidia: NVIDIA GeForce RTX 4090 - 24576MB VRAM
```

---

## 2. Optimization Planner

### Tasks

| # | Task | Priority | Est. Time | File/Command | Status |
|---|------|----------|-----------|--------------|--------|
| 2.1 | Implement VRAM cap calculation | 🔴 Critical | 2h | [optimizer.py](grey_gpu_optimizer/optimizer.py) `plan_optimizations()` | ✅ Done |
| 2.2 | Implement batch size heuristics | 🔴 Critical | 2h | Based on VRAM/compute capability | ✅ Done |
| 2.3 | Implement thermal thresholds | 🟡 High | 1h | Vendor-specific defaults | ✅ Done |
| 2.4 | Add CUDA/ROCm-specific tuning | 🟡 High | 2h | Scale batches for CUDA version | ✅ Done |
| 2.5 | Implement confidence scoring | 🟡 High | 1h | Based on detection quality | ✅ Done |
| 2.6 | Add deduplication estimation | 🟢 Medium | 1h | [dedup.py](grey_gpu_optimizer/utils/dedup.py) | ✅ Done |
| 2.7 | Add chunking strategy | 🟢 Medium | 2h | [chunking.py](grey_gpu_optimizer/utils/chunking.py) | ✅ Done |
| 2.8 | Config override support | 🟢 Medium | 1h | Load from YAML files | ✅ Done |

### Acceptance Criteria

- [ ] `plan_optimizations(spec, mode="safe")` returns valid `OptimizationPlan`
- [ ] Plans adapt to spec: smaller VRAM → lower caps, Intel → reduced batches
- [ ] Confidence score > 0.7 for complete specs, < 0.5 for minimal specs
- [ ] Rationale dict explains each decision

### 🧪 SMOKE TEST: Planning

```bash
# Generate safe and aggressive plans
python -c "
from grey_gpu_optimizer import detect_gpus, plan_optimizations
specs = detect_gpus()
safe = plan_optimizations(specs[0], mode='safe')
agg = plan_optimizations(specs[0], mode='aggressive')
print(f'Safe:   VRAM cap={safe.per_process_vram_cap_mb}MB, batch={safe.recommended_batch_size}')
print(f'Aggr:   VRAM cap={agg.per_process_vram_cap_mb}MB, batch={agg.recommended_batch_size}')
print(f'Safe conf: {safe.confidence:.2f}, Aggr conf: {agg.confidence:.2f}')
"
```

**Expected Output**:
```
Safe:   VRAM cap=18432MB, batch=64
Aggr:   VRAM cap=22118MB, batch=128
Safe conf: 0.82, Aggr conf: 0.80
```

---

## 3. Enforcement System

### Tasks

| # | Task | Priority | Est. Time | File/Command | Status |
|---|------|----------|-----------|--------------|--------|
| 3.1 | Implement process discovery | 🔴 Critical | 2h | [enforcement.py](grey_gpu_optimizer/utils/enforcement.py) `get_gpu_processes()` | ✅ Done |
| 3.2 | Implement SIGSTOP/SIGCONT throttle | 🔴 Critical | 2h | `throttle_process()` | ✅ Done |
| 3.3 | Implement CPU affinity control | 🟡 High | 1h | `set_cpu_affinity()` | ✅ Done |
| 3.4 | Implement nice value adjustment | 🟡 High | 1h | `set_process_priority()` | ✅ Done |
| 3.5 | Implement nvidia-smi power control | 🟡 High | 2h | `set_nvidia_power_limit()` | ✅ Done |
| 3.6 | Implement cgroup v2 integration | 🟡 High | 3h | `create_gpu_cgroup()` | ✅ Done |
| 3.7 | Implement thermal throttle check | 🔴 Critical | 1h | `thermal_throttle_check()` | ✅ Done |
| 3.8 | Implement VRAM limit enforcement | 🔴 Critical | 2h | `enforce_vram_limits()` | ✅ Done |
| 3.9 | Add AMD (rocm-smi) enforcement | 🟢 Medium | 2h | AMD-specific commands | ⬜ TODO |
| 3.10 | Add GPU reset capability | 🟢 Medium | 1h | `reset_nvidia_gpu()` | ✅ Done |

### Acceptance Criteria

- [ ] `enforce_vram_limits()` throttles processes exceeding cap
- [ ] Enforcement produces measurable VRAM reduction (check before/after)
- [ ] All enforcement respects `dry_run=True` default
- [ ] Destructive actions require `consent=True` AND `confirm=True`
- [ ] Thermal throttle suspends processes when GPU > cooldown_threshold_c

### 🧪 SMOKE TEST: Enforcement (Dry Run)

```bash
# Test enforcement in dry-run mode
python -c "
from grey_gpu_optimizer.utils.enforcement import (
    get_gpu_processes, enforce_vram_limits, get_vram_usage
)

# Get current state
vram = get_vram_usage()
procs = get_gpu_processes()
print(f'VRAM: {vram[\"used_mb\"]}MB / {vram[\"total_mb\"]}MB')
print(f'GPU processes: {len(procs)}')
for p in procs[:3]:
    print(f'  PID {p.pid}: {p.name} - {p.gpu_memory_mb}MB')

# Dry-run enforcement
result = enforce_vram_limits(per_process_cap_mb=2048, dry_run=True)
print(f'Dry-run result: {len(result.actions)} actions simulated')
"
```

**Expected Output**:
```
VRAM: 1234MB / 24576MB
GPU processes: 2
  PID 12345: python - 512MB
  PID 12346: cuda_worker - 256MB
Dry-run result: 0 actions simulated
```

---

## 4. Daemon

### Tasks

| # | Task | Priority | Est. Time | File/Command | Status |
|---|------|----------|-----------|--------------|--------|
| 4.1 | Implement monitoring loop | 🔴 Critical | 3h | [daemon.py](grey_gpu_optimizer/daemon.py) `_monitoring_loop()` | ✅ Done |
| 4.2 | Load spec/plan at startup | 🔴 Critical | 1h | Auto-detect if missing | ✅ Done |
| 4.3 | Implement thermal breach detection | 🟡 High | 1h | Check against plan thresholds | ✅ Done |
| 4.4 | Implement VRAM breach detection | 🟡 High | 1h | Check against plan cap | ✅ Done |
| 4.5 | Signal handling (SIGTERM/SIGINT) | 🟡 High | 1h | Graceful shutdown | ✅ Done |
| 4.6 | JSON log emission | 🟡 High | 1h | Structured events | ✅ Done |
| 4.7 | Status file generation | 🟢 Medium | 1h | `write_status_file()` | ✅ Done |
| 4.8 | Systemd service integration | 🟢 Medium | 2h | Service templates | ✅ Done |
| 4.9 | Configurable interval | 🟢 Medium | 0.5h | CLI flag `--interval` | ✅ Done |
| 4.10 | Background thread mode | 🟢 Medium | 1h | `start()` / `stop()` | ✅ Done |

### Acceptance Criteria

- [ ] Daemon runs indefinitely with `grey-gpu-opt start-daemon`
- [ ] Daemon loads or triggers detection on startup
- [ ] Daemon applies plan in dry-run mode by default
- [ ] Daemon logs structured JSON to `~/.grey_optimizer/logs/`
- [ ] Ctrl+C or SIGTERM triggers graceful shutdown

### 🧪 SMOKE TEST: Daemon Start

```bash
# Start daemon in foreground (Ctrl+C to stop)
timeout 5 python -m grey_gpu_optimizer.daemon --interval 2 --dry-run || true
echo "Daemon ran for 5 seconds"

# Check for log output
ls -la ~/.grey_optimizer/logs/
```

**Expected Output**:
```
Starting daemon in foreground (interval=2s)
[INFO] Detected: nvidia NVIDIA GeForce RTX 4090
[INFO] Generated safe plan with confidence 0.82
[INFO] Sample: vram_used=1234MB, temp=45.0C
... (repeat every 2s)
Daemon ran for 5 seconds
```

---

## 5. CLI Interface

### Tasks

| # | Task | Priority | Est. Time | File/Command | Status |
|---|------|----------|-----------|--------------|--------|
| 5.1 | Implement `detect` subcommand | 🔴 Critical | 1h | [cli.py](grey_gpu_optimizer/cli.py) | ✅ Done |
| 5.2 | Implement `plan` subcommand | 🔴 Critical | 1h | `--mode safe\|aggressive` | ✅ Done |
| 5.3 | Implement `apply` subcommand | 🔴 Critical | 2h | `--dry-run`, `--force`, `--confirm` | ✅ Done |
| 5.4 | Implement `status` subcommand | 🟡 High | 1h | Show daemon/GPU status | ✅ Done |
| 5.5 | Implement `start-daemon` | 🟡 High | 1h | `--interval`, `--force` | ✅ Done |
| 5.6 | Add JSON/YAML/table output formats | 🟡 High | 1h | `--output json\|yaml\|table` | ✅ Done |
| 5.7 | Add verbose mode | 🟢 Medium | 0.5h | `-v / --verbose` | ✅ Done |
| 5.8 | Add version flag | 🟢 Medium | 0.5h | `--version` | ✅ Done |
| 5.9 | Entry point registration | 🟢 Medium | 0.5h | `pyproject.toml` | ⬜ TODO |

### Acceptance Criteria

- [ ] `grey-gpu-opt detect --json` outputs valid JSON
- [ ] `grey-gpu-opt plan --mode aggressive` generates aggressive plan
- [ ] `grey-gpu-opt apply --dry-run` simulates without changes
- [ ] `grey-gpu-opt apply --force --confirm` applies with consent
- [ ] `grey-gpu-opt status` shows current GPU and daemon state

### 🧪 SMOKE TEST: CLI Commands

```bash
# Test all CLI subcommands
cd /home/robert-marshall01/Desktop/Grey\ Optimizer

# Detection
python -m grey_gpu_optimizer.cli detect --output json | python -m json.tool | head -20

# Planning
python -m grey_gpu_optimizer.cli plan --mode safe

# Apply (dry-run)
python -m grey_gpu_optimizer.cli apply --dry-run

# Status
python -m grey_gpu_optimizer.cli status
```

**Expected Output**:
```
{
  "vendor": "nvidia",
  "model": "NVIDIA GeForce RTX 4090",
  ...
}

============================================================
  Optimization Plan (safe)
============================================================
  mode             : safe
  per_process_vram_cap_mb : 18432
  ...
Confidence: 0.82 (HIGH - recommendations are well-supported by detected specs)

============================================================
  Apply Result (DRY-RUN)
============================================================
  success          : True
  dry_run          : True
  ...
```

---

## 6. Logging & Artifacts

### Tasks

| # | Task | Priority | Est. Time | File/Command | Status |
|---|------|----------|-----------|--------------|--------|
| 6.1 | Implement JSON log formatter | 🟡 High | 1h | [logging_config.py](grey_gpu_optimizer/logging_config.py) | ✅ Done |
| 6.2 | Implement rotating log files | 🟡 High | 1h | 10MB max, 5 backups | ✅ Done |
| 6.3 | Generate VRAM reclamation log | 🟡 High | 1h | `vram_reclamation.log` | ✅ Done |
| 6.4 | Generate thermal signature CSV | 🟡 High | 1h | `thermal_signature.csv` | ✅ Done |
| 6.5 | Generate fairness heatmap CSV | 🟢 Medium | 1h | `fairness_heatmap.csv` | ✅ Done |
| 6.6 | Generate artifact report JSON | 🟡 High | 1h | `artifact_report.json` | ✅ Done |
| 6.7 | Implement log event helper | 🟢 Medium | 0.5h | `log_event()` | ✅ Done |
| 6.8 | Add log file rotation cleanup | 🟢 Medium | 1h | Delete logs > 30 days | ⬜ TODO |

### Acceptance Criteria

- [ ] JSON logs contain: `timestamp`, `component`, `event`, `details`
- [ ] Artifacts show before/after metrics (VRAM, thermals)
- [ ] Artifacts written to `~/.grey_optimizer/artifacts/`
- [ ] Logs are valid JSON parseable by `jq`

### 🧪 SMOKE TEST: Artifact Generation

```bash
# Generate artifacts and verify
python -c "
from grey_gpu_optimizer import detect_gpus, plan_optimizations, apply_plan
from pathlib import Path

specs = detect_gpus()
plan = plan_optimizations(specs[0], mode='safe')
result = apply_plan(plan, dry_run=True)

print('Generated artifacts:')
for name, path in result.artifacts.items():
    p = Path(path)
    exists = '✅' if p.exists() else '❌'
    size = p.stat().st_size if p.exists() else 0
    print(f'  {exists} {name}: {path} ({size} bytes)')
"
```

**Expected Output**:
```
Generated artifacts:
  ✅ vram_reclamation_log: /home/user/.grey_optimizer/artifacts/vram_reclamation_20260111_123456.log (256 bytes)
  ✅ thermal_signature_csv: /home/user/.grey_optimizer/artifacts/thermal_signature_20260111_123456.csv (128 bytes)
  ✅ fairness_heatmap_csv: /home/user/.grey_optimizer/artifacts/fairness_heatmap_20260111_123456.csv (256 bytes)
  ✅ artifact_report_json: /home/user/.grey_optimizer/artifacts/artifact_report_20260111_123456.json (1024 bytes)
```

---

## 7. Tests

### Tasks

| # | Task | Priority | Est. Time | File/Command | Status |
|---|------|----------|-----------|--------------|--------|
| 7.1 | Unit tests: GPUSpec parsing | 🔴 Critical | 2h | Mock nvidia-smi, rocm-smi output | ✅ Done |
| 7.2 | Unit tests: Planner decisions | 🔴 Critical | 2h | Different specs → different plans | ✅ Done |
| 7.3 | Unit tests: Apply dry-run | 🟡 High | 1h | Verify no side effects | ✅ Done |
| 7.4 | Integration tests: Full workflow | 🟡 High | 2h | Detect → Plan → Apply | ✅ Done |
| 7.5 | Edge case tests | 🟡 High | 2h | Zero VRAM, unknown vendor | ✅ Done |
| 7.6 | Enforcement tests with mocks | 🟡 High | 2h | Mock psutil, subprocess | ⬜ TODO |
| 7.7 | Daemon tests | 🟢 Medium | 2h | Start/stop, signal handling | ⬜ TODO |
| 7.8 | CLI tests | 🟢 Medium | 1h | Argument parsing | ⬜ TODO |
| 7.9 | Smoke tests | 🔴 Critical | 1h | Quick sanity checks | ✅ Done |
| 7.10 | Coverage report | 🟢 Medium | 1h | `pytest --cov` | ⬜ TODO |

### Acceptance Criteria

- [ ] All unit tests pass with `pytest tests/ -v`
- [ ] Tests mock external CLIs (nvidia-smi, rocm-smi)
- [ ] Test coverage > 70%
- [ ] Planner tests assert decisions change with specs

### 🧪 SMOKE TEST: Run All Tests

```bash
# Run full test suite
cd /home/robert-marshall01/Desktop/Grey\ Optimizer
pytest tests/test_grey_gpu_optimizer_comprehensive.py -v --tb=short

# Run only smoke tests
pytest tests/ -v -k "smoke" --tb=short

# Run with coverage
pytest tests/ --cov=grey_gpu_optimizer --cov-report=term-missing
```

**Expected Output**:
```
tests/test_grey_gpu_optimizer_comprehensive.py::TestSmokeTests::test_smoke_gpuspec_creation PASSED
tests/test_grey_gpu_optimizer_comprehensive.py::TestSmokeTests::test_smoke_optimization_plan_creation PASSED
tests/test_grey_gpu_optimizer_comprehensive.py::TestSmokeTests::test_smoke_apply_result_creation PASSED
tests/test_grey_gpu_optimizer_comprehensive.py::TestSmokeTests::test_smoke_detect_gpus PASSED
tests/test_grey_gpu_optimizer_comprehensive.py::TestSmokeTests::test_smoke_plan_optimizations PASSED
tests/test_grey_gpu_optimizer_comprehensive.py::TestSmokeTests::test_smoke_apply_plan_dry_run PASSED
...
========================= X passed in Y.YYs =========================
```

---

## 8. Deployment & CI/CD

### Tasks

| # | Task | Priority | Est. Time | File/Command | Status |
|---|------|----------|-----------|--------------|--------|
| 8.1 | Create pyproject.toml | 🟡 High | 1h | Package metadata, entry points | ⬜ TODO |
| 8.2 | Create setup.py (legacy) | 🟢 Medium | 0.5h | Fallback for pip | ⬜ TODO |
| 8.3 | Create requirements.txt | 🟡 High | 0.5h | Minimal dependencies | ⬜ TODO |
| 8.4 | Create Makefile | 🟢 Medium | 1h | `make install`, `make test` | ⬜ TODO |
| 8.5 | Create Docker image | 🟢 Medium | 2h | NVIDIA runtime support | ⬜ TODO |
| 8.6 | Create GitHub Actions workflow | 🟢 Medium | 2h | CI/CD pipeline | ⬜ TODO |
| 8.7 | Create systemd service files | 🟡 High | 1h | Safe and aggressive modes | ✅ Done |
| 8.8 | Create install script | 🟢 Medium | 1h | `install.sh` | ⬜ TODO |
| 8.9 | Create uninstall script | 🟢 Medium | 0.5h | `uninstall.sh` | ⬜ TODO |
| 8.10 | Documentation | 🟢 Medium | 2h | README, API docs | ⬜ TODO |

### Acceptance Criteria

- [ ] `pip install -e .` works from project root
- [ ] `grey-gpu-opt` command available after install
- [ ] Systemd service starts with `systemctl start grey_optimizer`
- [ ] Docker image runs with NVIDIA runtime

### 🧪 SMOKE TEST: Package Installation

```bash
# Test package installation
cd /home/robert-marshall01/Desktop/Grey\ Optimizer
pip install -e ".[dev]" 2>/dev/null || pip install -e .

# Verify entry point (if configured)
grey-gpu-opt --version 2>/dev/null || python -m grey_gpu_optimizer.cli --version
```

**Expected Output**:
```
grey-gpu-opt 1.0.0
```

---

## Summary

### Progress Overview

| Component | Complete | In Progress | TODO |
|-----------|----------|-------------|------|
| Spec Detection | 7/8 | 0 | 1 |
| Planner | 8/8 | 0 | 0 |
| Enforcement | 9/10 | 0 | 1 |
| Daemon | 10/10 | 0 | 0 |
| CLI | 8/9 | 0 | 1 |
| Logging | 7/8 | 0 | 1 |
| Tests | 6/10 | 0 | 4 |
| Deployment | 2/10 | 0 | 8 |

### Priority Legend

- 🔴 **Critical** - Core functionality, must complete first
- 🟡 **High** - Important for production readiness
- 🟢 **Medium** - Nice to have, can defer

### Quick Commands

```bash
# Run detection
python -m grey_gpu_optimizer.cli detect

# Generate plan
python -m grey_gpu_optimizer.cli plan --mode safe

# Apply plan (dry-run)
python -m grey_gpu_optimizer.cli apply --dry-run

# Start daemon
python -m grey_gpu_optimizer.cli start-daemon --interval 30

# Run tests
pytest tests/test_grey_gpu_optimizer_comprehensive.py -v

# Run smoke tests only
pytest tests/ -v -k "smoke"
```

---

## Notes

### Safety Reminders

1. **Always start with dry-run**: All enforcement defaults to `dry_run=True`
2. **Destructive actions need double confirmation**: `consent=True` AND `confirm=True`
3. **Test on non-production GPUs first**: Especially for aggressive mode
4. **Monitor thermals**: GPU temperature can spike during enforcement

### Dependencies

- **Required**: `subprocess`, `json`, `logging`, `pathlib`
- **Optional**: `psutil` (for process management), `pyyaml` (for YAML support)
- **Development**: `pytest`, `pytest-cov`

### File Structure

```
grey_gpu_optimizer/
├── __init__.py           # Package exports
├── optimizer.py          # Core detection, planning, apply
├── daemon.py             # Monitoring daemon
├── cli.py                # CLI interface
├── logging_config.py     # Structured logging
├── example_plan.yaml     # Example optimization plans
├── utils/
│   ├── __init__.py
│   ├── chunking.py       # Batch size optimization
│   ├── dedup.py          # VRAM deduplication
│   └── enforcement.py    # Real enforcement actions
└── service_templates/
    ├── grey_optimizer.service           # Safe mode systemd
    ├── grey_optimizer_aggressive.service # Aggressive mode
    └── README.md
```
