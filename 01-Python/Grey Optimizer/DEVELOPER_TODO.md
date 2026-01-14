# DEVELOPER_TODO.md - Grey GPU Optimizer

> **Last Updated**: 2026-01-11  
> **Status**: Active Development - Debugging Phase  
> **Priority Legend**: 🔴 Critical | 🟠 High | 🟡 Medium | 🟢 Low
> 
> **Current Focus**: Move from **micro improvements** (+50–100 FPS) to **macro improvements** (+500–2000 FPS)

---

## Overview

This checklist tracks all development tasks for the Grey GPU Optimizer project.
Each item includes priority, estimated time, relevant file/command, and acceptance criteria.

---

## 🚨 DEBUG - Root Cause Diagnosis for Micro-Only Gains

> **Problem**: Optimizer produces only +50–100 FPS on 5900 FPS baseline (< 2% improvement)
> **Goal**: Achieve macro improvements (+500–2000 FPS, 10–30% improvement)

### Detection Completeness (Root Cause #1)

- [ ] 🔴 **Verify all 10 required spec fields are populated**
  - **Command**: `python debug_gpu_optimizer.py verify-spec --spec ~/.grey_optimizer/gpu_spec.json`
  - **Acceptance**: `confidence_score >= 0.8`, `missing_fields == []`
  - **Fix if failing**: Update `detect_gpus()` to query all fields from NVML/ROCm

- [ ] 🔴 **Ensure `num_sm` is detected (not defaulted to 0)**
  - **File**: [grey_gpu_optimizer/optimizer.py](grey_gpu_optimizer/optimizer.py)
  - **Reason**: `num_sm=0` causes batch size to fall back to minimum
  - **Command**: `nvidia-smi --query-gpu=gpu_num_sm --format=csv`

- [ ] 🔴 **Ensure `thermal_limits` dict is populated**
  - **File**: [grey_gpu_optimizer/optimizer.py](grey_gpu_optimizer/optimizer.py)
  - **Reason**: Empty `thermal_limits` uses hardcoded 70°C threshold (too conservative)
  - **Fix**: Query `nvidia-smi --query-gpu=gpu_temp_throttle,gpu_temp_max`

### Planner Heuristics (Root Cause #2)

- [ ] 🔴 **Raise SAFE_VRAM_UTILIZATION from 0.70 to 0.80**
  - **File**: [grey_gpu_optimizer/optimizer.py](grey_gpu_optimizer/optimizer.py)
  - **Reason**: 70% leaves 30% VRAM unused, limiting batch sizes
  - **Acceptance**: Safe mode uses 80% of VRAM

- [ ] 🔴 **Remove or reduce Intel/AMD batch reduction factor**
  - **File**: [grey_gpu_optimizer/optimizer.py](grey_gpu_optimizer/optimizer.py)
  - **Reason**: 0.5x penalty is too aggressive for modern AMD/Intel GPUs
  - **Acceptance**: Batch size penalty <= 0.8x (if any)

- [ ] 🟠 **Add SM-aware batch scaling**
  - **File**: [grey_gpu_optimizer/optimizer.py](grey_gpu_optimizer/optimizer.py)
  - **Reason**: Without `num_sm`, batch size doesn't scale with GPU cores
  - **Acceptance**: `recommended_batch_size` proportional to `num_sm`

- [ ] 🟠 **Use spec `thermal_limits.throttle_c` instead of hardcoded 80°C**
  - **File**: [grey_gpu_optimizer/optimizer.py](grey_gpu_optimizer/optimizer.py)
  - **Acceptance**: `cooldown_threshold_c` = `thermal_limits.throttle_c - 3`

### Enforcement Activity (Root Cause #3)

- [ ] 🔴 **Confirm daemon runs with `dry_run=False`**
  - **Command**: `python debug_gpu_optimizer.py check-enforcement --log ~/.grey_optimizer/daemon.log`
  - **Acceptance**: `dry_run_only == False`, `actions_count > 0`

- [ ] 🔴 **Add consent/confirm flags to production config**
  - **File**: `~/.grey_optimizer/daemon_config.yaml`
  - **Acceptance**: `consent: true`, `confirm: true` for live enforcement

- [ ] 🟠 **Reduce daemon polling interval to 5s**
  - **File**: [grey_gpu_optimizer/daemon.py](grey_gpu_optimizer/daemon.py)
  - **Reason**: 10s interval misses transient thermal spikes
  - **Acceptance**: `poll_interval_seconds: 5`

### Diagnostic Commands Reference

```bash
# 1. Verify spec integrity
python debug_gpu_optimizer.py verify-spec --spec ~/.grey_optimizer/gpu_spec.json

# 2. Trace plan decisions (look for [ISSUE] flags)
python debug_gpu_optimizer.py trace-plan \
    --plan ~/.grey_optimizer/optimization_plan.yaml \
    --spec ~/.grey_optimizer/gpu_spec.json

# 3. Check enforcement activity
python debug_gpu_optimizer.py check-enforcement --log ~/.grey_optimizer/daemon.log

# 4. Generate full diagnostic report
python debug_gpu_optimizer.py report \
    --spec ~/.grey_optimizer/gpu_spec.json \
    --plan ~/.grey_optimizer/optimization_plan.yaml \
    --log ~/.grey_optimizer/daemon.log \
    --out diagnostic_report.json
```

### CI Quality Gates

| Check | Threshold | Action |
|-------|-----------|--------|
| `plan_confidence` | >= 0.5 | PR blocked if below |
| `detection_success` | `true` | PR blocked if false |
| `overall_health_score` | >= 50 | PR blocked if below |

### Expected Outcomes After Fixes

| Metric | Before | After |
|--------|--------|-------|
| Spec confidence | < 0.5 | >= 0.8 |
| VRAM utilization | 20-40% | 70-90% |
| Batch size | 2-4 | 16-32 |
| FPS improvement | +50-100 | +500-2000 |

---

## 🔍 DETECT - GPU Hardware Detection

### Core Detection

- [ ] 🔴 **Implement NVIDIA GPU memory bandwidth detection**
  - **Est. Time**: 2h
  - **File**: [grey_gpu_optimizer/optimizer.py](grey_gpu_optimizer/optimizer.py#L200-L250)
  - **Command**: `nvidia-smi --query-gpu=memory.bandwidth_total --format=csv`
  - **Acceptance**: GPUSpec.memory_bus_width populated from nvidia-smi

- [ ] 🔴 **Add AMD rocm-smi full memory info parsing**
  - **Est. Time**: 3h
  - **File**: [grey_gpu_optimizer/optimizer.py](grey_gpu_optimizer/optimizer.py#L250-L300)
  - **Command**: `rocm-smi --showmeminfo vram --showtemp`
  - **Acceptance**: GPUSpec populated with vram_total_mb, vram_free_mb, temp for AMD

- [ ] 🟠 **Implement intel_gpu_top metrics parsing**
  - **Est. Time**: 4h
  - **File**: [grey_gpu_optimizer/optimizer.py](grey_gpu_optimizer/optimizer.py#L300-L350)
  - **Command**: `intel_gpu_top -J` (JSON output)
  - **Acceptance**: Intel iGPU utilization and frequency captured

- [ ] 🟡 **Add multi-GPU detection and indexing**
  - **Est. Time**: 3h
  - **File**: [grey_gpu_optimizer/optimizer.py](grey_gpu_optimizer/optimizer.py#L400-L450)
  - **Command**: `nvidia-smi -L`
  - **Acceptance**: detect_gpus() returns list with all GPUs, not just first

- [ ] 🟢 **Add Vulkan device enumeration fallback**
  - **Est. Time**: 2h
  - **File**: [grey_gpu_optimizer/optimizer.py](grey_gpu_optimizer/optimizer.py#L350-L380)
  - **Command**: `vulkaninfo --summary`
  - **Acceptance**: Vulkan version and device name parsed as fallback

### 🧪 SMOKE TEST - Detection
```bash
# Run detection and verify output
grey-gpu-opt detect --output json | jq '.vendor, .model, .vram_total_mb'
# Expected: non-empty strings and vram > 0
```

---

## 📋 PLAN - Optimization Planning

### Planning Logic

- [ ] 🔴 **Add model-size-aware batch sizing**
  - **Est. Time**: 3h
  - **File**: [grey_gpu_optimizer/optimizer.py](grey_gpu_optimizer/optimizer.py#L550-L620)
  - **Acceptance**: plan_optimizations accepts optional model_size_mb parameter

- [ ] 🔴 **Implement workload-type differentiation**
  - **Est. Time**: 4h
  - **File**: [grey_gpu_optimizer/optimizer.py](grey_gpu_optimizer/optimizer.py#L500-L550)
  - **Acceptance**: Different plans for inference vs training workloads

- [ ] 🟠 **Add power-budget-aware planning**
  - **Est. Time**: 3h
  - **File**: [grey_gpu_optimizer/optimizer.py](grey_gpu_optimizer/optimizer.py#L620-L670)
  - **Command**: `nvidia-smi --query-gpu=power.limit --format=csv`
  - **Acceptance**: Plan adjusts batch size based on TDP headroom

- [ ] 🟡 **Implement ML framework detection for hints**
  - **Est. Time**: 2h
  - **File**: [grey_gpu_optimizer/optimizer.py](grey_gpu_optimizer/optimizer.py#L670-L700)
  - **Acceptance**: Detect if PyTorch/TensorFlow/JAX is installed, adjust defaults

- [ ] 🟢 **Add historical performance-based tuning**
  - **Est. Time**: 5h
  - **File**: [grey_gpu_optimizer/optimizer.py](grey_gpu_optimizer/optimizer.py#L700-L750)
  - **Acceptance**: Plans incorporate learnings from previous runs

### 🧪 SMOKE TEST - Planning
```bash
# Generate both plans and compare
grey-gpu-opt plan --mode safe --output json > /tmp/safe.json
grey-gpu-opt plan --mode aggressive --output json > /tmp/aggressive.json
diff <(jq '.per_process_vram_cap_mb' /tmp/safe.json) \
     <(jq '.per_process_vram_cap_mb' /tmp/aggressive.json)
# Expected: aggressive cap > safe cap
```

---

## ⚡ ENFORCE - Plan Application

### Enforcement Actions

- [ ] 🔴 **Implement nvidia-smi GPU application clocks setting**
  - **Est. Time**: 3h
  - **File**: [grey_gpu_optimizer/optimizer.py](grey_gpu_optimizer/optimizer.py#L800-L850)
  - **Command**: `nvidia-smi -ac <mem_clock>,<gpu_clock>`
  - **Acceptance**: Can set/reset application clocks with consent flags

- [ ] 🔴 **Add cgroup v2 memory limit enforcement**
  - **Est. Time**: 4h
  - **File**: [grey_gpu_optimizer/optimizer.py](grey_gpu_optimizer/optimizer.py#L850-L900)
  - **Acceptance**: VRAM cap enforced via cgroup memory.max for GPU processes

- [ ] 🟠 **Implement process-level CUDA_VISIBLE_DEVICES control**
  - **Est. Time**: 2h
  - **File**: [grey_gpu_optimizer/optimizer.py](grey_gpu_optimizer/optimizer.py#L900-L930)
  - **Acceptance**: Can restrict processes to specific GPUs

- [ ] 🟠 **Add nvidia-smi persistence mode management**
  - **Est. Time**: 1h
  - **Command**: `nvidia-smi -pm 1`
  - **Acceptance**: Persistence mode enabled for faster driver loads

- [ ] 🟡 **Implement graceful process throttling**
  - **Est. Time**: 4h
  - **File**: [grey_gpu_optimizer/daemon.py](grey_gpu_optimizer/daemon.py#L200-L250)
  - **Acceptance**: Throttle via SIGSTOP/SIGCONT with grace period

- [ ] 🟢 **Add rollback mechanism for failed enforcement**
  - **Est. Time**: 3h
  - **File**: [grey_gpu_optimizer/optimizer.py](grey_gpu_optimizer/optimizer.py#L930-L980)
  - **Acceptance**: Failed apply_plan restores previous state

### 🧪 SMOKE TEST - Enforcement
```bash
# Dry-run should succeed without changes
grey-gpu-opt apply --dry-run
echo $?  # Expected: 0

# Verify artifacts created
ls ~/.grey_optimizer/artifacts/*.log
# Expected: vram_reclamation_*.log exists
```

---

## 🖥️ CLI - Command Line Interface

### CLI Enhancements

- [ ] 🔴 **Add --watch flag for continuous status**
  - **Est. Time**: 2h
  - **File**: [grey_gpu_optimizer/cli.py](grey_gpu_optimizer/cli.py#L180-L220)
  - **Acceptance**: `grey-gpu-opt status --watch` updates every N seconds

- [ ] 🟠 **Implement --format flag for output formats**
  - **Est. Time**: 1h
  - **File**: [grey_gpu_optimizer/cli.py](grey_gpu_optimizer/cli.py#L50-L80)
  - **Acceptance**: Support table, json, yaml, csv output formats

- [ ] 🟠 **Add shell completion scripts**
  - **Est. Time**: 2h
  - **File**: New: `completions/grey-gpu-opt.bash`, `.zsh`, `.fish`
  - **Acceptance**: Tab completion works for subcommands and flags

- [ ] 🟡 **Add --quiet and --debug verbosity flags**
  - **Est. Time**: 1h
  - **File**: [grey_gpu_optimizer/cli.py](grey_gpu_optimizer/cli.py#L280-L320)
  - **Acceptance**: --quiet suppresses output, --debug shows full traces

- [ ] 🟢 **Add interactive mode for plan review**
  - **Est. Time**: 3h
  - **File**: [grey_gpu_optimizer/cli.py](grey_gpu_optimizer/cli.py#L150-L180)
  - **Acceptance**: `grey-gpu-opt apply --interactive` prompts before each action

### 🧪 SMOKE TEST - CLI
```bash
# Verify all subcommands work
grey-gpu-opt --help
grey-gpu-opt detect --help
grey-gpu-opt plan --help
grey-gpu-opt apply --help
grey-gpu-opt status --help
grey-gpu-opt start-daemon --help
# Expected: all show help without errors
```

---

## ⚙️ CONFIG - Configuration System

### Configuration

- [ ] 🔴 **Implement YAML config file loading hierarchy**
  - **Est. Time**: 2h
  - **File**: [grey_gpu_optimizer/optimizer.py](grey_gpu_optimizer/optimizer.py#L950-L1000)
  - **Acceptance**: /etc/grey_optimizer/ → ~/.config/grey_optimizer/ → env vars

- [ ] 🟠 **Add environment variable overrides**
  - **Est. Time**: 1h
  - **File**: [grey_gpu_optimizer/optimizer.py](grey_gpu_optimizer/optimizer.py#L1000-L1030)
  - **Acceptance**: GREY_VRAM_CAP_MB, GREY_MODE, etc. override config

- [ ] 🟡 **Implement config validation with schema**
  - **Est. Time**: 3h
  - **File**: New: `grey_gpu_optimizer/config_schema.py`
  - **Acceptance**: Invalid config values produce helpful error messages

- [ ] 🟢 **Add config migration for version upgrades**
  - **Est. Time**: 2h
  - **File**: New: `grey_gpu_optimizer/config_migration.py`
  - **Acceptance**: Old config files auto-upgraded to new schema

### 🧪 SMOKE TEST - Config
```bash
# Test config override
echo "mode: aggressive" > ~/.grey_optimizer/gpu_config.yaml
grey-gpu-opt plan --output json | jq '.mode'
# Expected: "aggressive" (if implemented)
rm ~/.grey_optimizer/gpu_config.yaml
```

---

## 🧪 TESTS - Test Coverage

### Unit Tests

- [ ] 🔴 **Add 80% coverage for optimizer.py**
  - **Est. Time**: 4h
  - **File**: [tests/test_optimizer.py](tests/test_optimizer.py)
  - **Command**: `pytest tests/test_optimizer.py --cov=grey_gpu_optimizer.optimizer --cov-report=term-missing`
  - **Acceptance**: Coverage >= 80%

- [ ] 🔴 **Add daemon lifecycle tests**
  - **Est. Time**: 3h
  - **File**: [tests/test_daemon.py](tests/test_daemon.py) (create)
  - **Acceptance**: Tests for start, stop, signal handling, error recovery

- [ ] 🟠 **Add CLI integration tests**
  - **Est. Time**: 2h
  - **File**: [tests/test_cli.py](tests/test_cli.py) (create)
  - **Acceptance**: All CLI subcommands tested with mock data

- [ ] 🟡 **Add property-based tests for planning**
  - **Est. Time**: 3h
  - **File**: [tests/test_optimizer.py](tests/test_optimizer.py)
  - **Acceptance**: Hypothesis tests for edge cases in plan generation

- [ ] 🟢 **Add performance regression tests**
  - **Est. Time**: 2h
  - **File**: [tests/test_performance.py](tests/test_performance.py) (create)
  - **Acceptance**: Detection < 2s, planning < 100ms benchmarks

### 🧪 SMOKE TEST - Tests
```bash
# Run full test suite
pytest tests/ -v --tb=short
# Expected: All tests pass

# Check coverage
pytest tests/ --cov=grey_gpu_optimizer --cov-fail-under=60
# Expected: Coverage >= 60%
```

---

## 📚 DOCS - Documentation

### Documentation

- [ ] 🔴 **Write README.md with quick start**
  - **Est. Time**: 2h
  - **File**: `README.md`
  - **Acceptance**: Installation, basic usage, examples documented

- [ ] 🟠 **Add API documentation with docstrings**
  - **Est. Time**: 3h
  - **File**: All `grey_gpu_optimizer/*.py`
  - **Acceptance**: All public functions have Google-style docstrings

- [ ] 🟠 **Create ARCHITECTURE.md**
  - **Est. Time**: 2h
  - **File**: `docs/ARCHITECTURE.md`
  - **Acceptance**: Module relationships and data flow documented

- [ ] 🟡 **Add troubleshooting guide**
  - **Est. Time**: 1h
  - **File**: `docs/TROUBLESHOOTING.md`
  - **Acceptance**: Common errors and solutions documented

- [ ] 🟢 **Generate Sphinx/MkDocs site**
  - **Est. Time**: 4h
  - **File**: `docs/` directory
  - **Acceptance**: HTML docs generated from docstrings

### 🧪 SMOKE TEST - Docs
```bash
# Verify README exists and is readable
head -20 README.md
# Expected: Title and description visible

# Verify all modules have docstrings
python -c "import grey_gpu_optimizer; help(grey_gpu_optimizer.detect_gpus)"
# Expected: Help text displayed
```

---

## 🚀 DEPLOY - Packaging & Distribution

### Packaging

- [ ] 🔴 **Create pyproject.toml with dependencies**
  - **Est. Time**: 1h
  - **File**: `pyproject.toml`
  - **Acceptance**: `pip install .` works, CLI entry point registered

- [ ] 🔴 **Add setup.py for legacy compatibility**
  - **Est. Time**: 30m
  - **File**: `setup.py`
  - **Acceptance**: `python setup.py install` works

- [ ] 🟠 **Create systemd service installation script**
  - **Est. Time**: 1h
  - **File**: `scripts/install-service.sh`
  - **Acceptance**: Script installs and enables grey-optimizer.service

- [ ] 🟡 **Add Docker container support**
  - **Est. Time**: 2h
  - **File**: `Dockerfile`
  - **Acceptance**: `docker build . && docker run grey-gpu-optimizer detect`

- [ ] 🟢 **Publish to PyPI**
  - **Est. Time**: 1h
  - **Command**: `python -m build && twine upload dist/*`
  - **Acceptance**: `pip install grey-gpu-optimizer` works

### 🧪 SMOKE TEST - Deploy
```bash
# Test local installation
pip install -e .
which grey-gpu-opt
grey-gpu-opt --version
# Expected: 1.0.0
```

---

## 🔧 UTILS - Utility Modules

### Chunking Utilities

- [ ] 🟠 **Add gradient accumulation calculator**
  - **Est. Time**: 2h
  - **File**: [grey_gpu_optimizer/utils/chunking.py](grey_gpu_optimizer/utils/chunking.py)
  - **Acceptance**: Compute accumulation steps for target effective batch

- [ ] 🟡 **Implement memory-efficient attention chunking**
  - **Est. Time**: 4h
  - **File**: [grey_gpu_optimizer/utils/chunking.py](grey_gpu_optimizer/utils/chunking.py)
  - **Acceptance**: Chunk strategy for flash attention alternatives

### Deduplication Utilities

- [ ] 🟠 **Add real VRAM buffer tracking (CUDA)**
  - **Est. Time**: 6h
  - **File**: [grey_gpu_optimizer/utils/dedup.py](grey_gpu_optimizer/utils/dedup.py)
  - **Acceptance**: Track actual CUDA allocations via cudaMalloc hooks

- [ ] 🟡 **Implement content-aware dedup scoring**
  - **Est. Time**: 3h
  - **File**: [grey_gpu_optimizer/utils/dedup.py](grey_gpu_optimizer/utils/dedup.py)
  - **Acceptance**: Prioritize dedup candidates by access frequency

### 🧪 SMOKE TEST - Utils
```bash
# Test chunking calculation
python -c "
from grey_gpu_optimizer.utils.chunking import compute_optimal_batch_size
from grey_gpu_optimizer import GPUSpec
spec = GPUSpec(vendor='nvidia', vram_total_mb=8192)
result = compute_optimal_batch_size(spec)
print(f'Batch size: {result.batch_size}')
"
# Expected: Batch size > 0
```

---

## 📊 Progress Summary

| Category | Total | Done | Remaining |
|----------|-------|------|-----------|
| **Debug**| **11**| **0**| **11**    |
| Detect   | 5     | 0    | 5         |
| Plan     | 5     | 0    | 5         |
| Enforce  | 6     | 0    | 6         |
| CLI      | 5     | 0    | 5         |
| Config   | 4     | 0    | 4         |
| Tests    | 5     | 0    | 5         |
| Docs     | 5     | 0    | 5         |
| Deploy   | 5     | 0    | 5         |
| Utils    | 4     | 0    | 4         |
| **Total**| **55**| **0**| **55**    |

---

## Quick Start Checklist

For MVP functionality, complete these items first (in order):

**Phase 1: Debug & Fix (Current Priority)**
1. [ ] 🔴 Debug: Verify all 10 spec fields populated
2. [ ] 🔴 Debug: Ensure `num_sm` detected (not 0)
3. [ ] 🔴 Debug: Raise SAFE_VRAM_UTILIZATION to 0.80
4. [ ] 🔴 Debug: Confirm daemon runs with dry_run=False

**Phase 2: Core Features**
5. [ ] 🔴 Detect: NVIDIA memory bandwidth detection
6. [ ] 🔴 Plan: Model-size-aware batch sizing
7. [ ] 🔴 Enforce: cgroup v2 memory limits
8. [ ] 🔴 Tests: 80% coverage for optimizer.py
9. [ ] 🔴 Docs: README.md with quick start
10. [ ] 🔴 Deploy: pyproject.toml with dependencies

---

## Contributing

When completing a task:
1. Create a branch: `feature/<category>-<short-name>`
2. Implement with tests
3. Update this checklist: change `- [ ]` to `- [x]`
4. Update Progress Summary counts
5. Submit PR with checklist item in description
