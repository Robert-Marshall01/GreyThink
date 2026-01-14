# GPU Optimizer Developer TODO Checklist

> **Module:** `daemon/gpu_optimizer.py`  
> **Tests:** `tests/test_gpu_optimizer.py`  
> **Last Updated:** 2026-01-12  

---

## Detect

- [x] **Implement `detect_gpus()` function**  
  - Priority: P0  
  - Estimated Time: 1h  
  - Command/File: `daemon/gpu_optimizer.py::detect_gpus()`  
  - Acceptance Criteria: Parse vendor CLI tools (nvidia-smi, rocm-smi), fallback to lspci/lshw, return normalized JSON spec  

- [x] **Implement nvidia-smi GPU query parsing**  
  - Priority: P0  
  - Estimated Time: 1h  
  - Command/File: `daemon/gpu_optimizer.py::_parse_nvidia_smi()`  
  - Acceptance Criteria: Parse GPU name, VRAM total/free, driver version, CUDA version, SM count from nvidia-smi output  

- [x] **Implement rocm-smi AMD detection**  
  - Priority: P0  
  - Estimated Time: 1h  
  - Command/File: `daemon/gpu_optimizer.py::_parse_rocm_smi()`  
  - Acceptance Criteria: Parse Radeon model, VRAM, ROCm version; populate GPUSpec with AMD-specific fields  

- [ ] **Add intel_gpu_top support for Intel Arc/UHD**  
  - Priority: P1  
  - Estimated Time: 2h  
  - Command/File: `daemon/gpu_optimizer.py::_parse_intel_gpu_top()`  
  - Acceptance Criteria: Detect Intel GPUs, extract Xe/Arc model names, populate GPUSpec.vendor="intel"  

- [x] **Implement vulkaninfo parsing**  
  - Priority: P1  
  - Estimated Time: 1h  
  - Command/File: `daemon/gpu_optimizer.py::_parse_vulkaninfo()`  
  - Acceptance Criteria: Extract Vulkan version, device name, vendor ID from vulkaninfo --summary  

- [x] **Implement glxinfo parsing**  
  - Priority: P2  
  - Estimated Time: 30m  
  - Command/File: `daemon/gpu_optimizer.py::_parse_glxinfo()`  
  - Acceptance Criteria: Extract OpenGL version string, vendor, renderer from glxinfo output  

- [x] **Add lspci fallback detection**  
  - Priority: P1  
  - Estimated Time: 1h  
  - Command/File: `daemon/gpu_optimizer.py::_parse_lspci()`  
  - Acceptance Criteria: Detect all VGA controllers, extract vendor (nvidia/amd/intel) and model substring  

- [x] **Add lshw fallback detection**  
  - Priority: P2  
  - Estimated Time: 1h  
  - Command/File: `daemon/gpu_optimizer.py::_parse_lshw()`  
  - Acceptance Criteria: Parse product, vendor, driver, bus info from lshw -C display output  

- [ ] **Implement torch.cuda probe**  
  - Priority: P1  
  - Estimated Time: 45m  
  - Command/File: `daemon/gpu_optimizer.py::_probe_torch_cuda()`  
  - Acceptance Criteria: If torch available, extract device_count, VRAM per device, CUDA arch list  

- [ ] **Implement pyopencl probe**  
  - Priority: P2  
  - Estimated Time: 45m  
  - Command/File: `daemon/gpu_optimizer.py::_probe_pyopencl()`  
  - Acceptance Criteria: Enumerate OpenCL platforms/devices, extract global_mem_size, vendor  

- [ ] **Implement pycuda probe**  
  - Priority: P2  
  - Estimated Time: 45m  
  - Command/File: `daemon/gpu_optimizer.py::_probe_pycuda()`  
  - Acceptance Criteria: Initialize CUDA context, extract device name, compute capability, total memory  

- [x] **Save detected GPU spec to ~/.grey_optimizer/**  
  - Priority: P0  
  - Estimated Time: 30m  
  - Command/File: `daemon/gpu_optimizer.py::_save_spec()`  
  - Acceptance Criteria: Write JSON to gpu_spec.json with all normalized fields  

- [x] **SMOKE TEST: Detect category**  
  - Priority: P0  
  - Estimated Time: 10m  
  - Command/File: `python daemon/gpu_optimizer.py detect --json`  
  - Acceptance Criteria: No exceptions raised, returns valid JSON array, GPUSpec has notes if detection incomplete  

---

## Plan

- [x] **Implement `plan_optimizations(spec, mode)`**  
  - Priority: P0  
  - Estimated Time: 2h  
  - Command/File: `daemon/gpu_optimizer.py::plan_optimizations()`  
  - Acceptance Criteria: Use VRAM size, vendor, compute capability to set per-process caps, batch sizes, thermal thresholds  

- [x] **Implement safe mode VRAM calculation (75%)**  
  - Priority: P0  
  - Estimated Time: 30m  
  - Command/File: `daemon/gpu_optimizer.py::plan_optimizations()` mode="safe" branch  
  - Acceptance Criteria: per_process_vram_cap_mb == int(vram_total * 0.75), preemptive_throttle=False  

- [x] **Implement aggressive mode VRAM calculation (90%)**  
  - Priority: P0  
  - Estimated Time: 30m  
  - Command/File: `daemon/gpu_optimizer.py::plan_optimizations()` mode="aggressive" branch  
  - Acceptance Criteria: per_process_vram_cap_mb == int(vram_total * 0.90), preemptive_throttle=True  

- [x] **Add VRAM deduplication & compression prototype**  
  - Priority: P1  
  - Estimated Time: 1h  
  - Command/File: `daemon/gpu_optimizer.py::plan_optimizations()` dedup section  
  - Acceptance Criteria: Plan includes estimated_reclaimed_mb, dedup_buffer_count, compression_ratio  

- [x] **Add confidence scoring for detection quality**  
  - Priority: P1  
  - Estimated Time: 1h  
  - Command/File: `daemon/gpu_optimizer.py::plan_optimizations()` confidence logic  
  - Acceptance Criteria: Score 0.0-1.0 based on detection_sources count, field completeness; >=3 sources → >=0.7  

- [x] **Add rationale strings explaining each decision**  
  - Priority: P1  
  - Estimated Time: 30m  
  - Command/File: `daemon/gpu_optimizer.py::OptimizationPlan.rationale`  
  - Acceptance Criteria: Each plan parameter has a human-readable explanation string  

- [x] **Implement batch size heuristic based on VRAM**  
  - Priority: P1  
  - Estimated Time: 45m  
  - Command/File: `daemon/gpu_optimizer.py::plan_optimizations()` batch_size logic  
  - Acceptance Criteria: <=4GB→8, <=8GB→16, <=16GB→32, <=24GB→64, >24GB→128; doubled for aggressive  

- [x] **Implement tensor chunk sizing**  
  - Priority: P1  
  - Estimated Time: 30m  
  - Command/File: `daemon/gpu_optimizer.py::plan_optimizations()` tensor_chunk logic  
  - Acceptance Criteria: safe=vram/8, aggressive=vram/4, minimum 512MB  

- [x] **Add vendor-specific thermal thresholds**  
  - Priority: P1  
  - Estimated Time: 30m  
  - Command/File: `daemon/gpu_optimizer.py::plan_optimizations()` thermal logic  
  - Acceptance Criteria: NVIDIA: 83C/70C, AMD: 85C/72C, Intel: 80C/65C; aggressive +5C cooldown  

- [x] **Implement fair scheduling policy selection**  
  - Priority: P2  
  - Estimated Time: 30m  
  - Command/File: `daemon/gpu_optimizer.py::plan_optimizations()` scheduling logic  
  - Acceptance Criteria: safe→round_robin, aggressive→priority_based; single GPU special handling  

- [x] **Add multi-tenant fairness scheduling planner**  
  - Priority: P1  
  - Estimated Time: 45m  
  - Command/File: `daemon/gpu_optimizer.py::OptimizationPlan` fairness fields  
  - Acceptance Criteria: Includes max_concurrent_kernels, fairness_weight_per_process, utilization_heatmap_enabled  

- [x] **Add config override merging**  
  - Priority: P1  
  - Estimated Time: 45m  
  - Command/File: `daemon/gpu_optimizer.py::plan_optimizations()` override handling  
  - Acceptance Criteria: Load ~/.grey_optimizer/config.yaml, merge overrides, add notes for each override  

- [x] **SMOKE TEST: Plan category**  
  - Priority: P0  
  - Estimated Time: 10m  
  - Command/File: `python daemon/gpu_optimizer.py plan --mode safe --output /tmp/plan.yaml`  
  - Acceptance Criteria: Exits 0, creates valid YAML file, plan.mode=="safe", confidence_overall exists  

---

## Enforce

- [x] **Implement dry-run action collection**  
  - Priority: P0  
  - Estimated Time: 30m  
  - Command/File: `daemon/gpu_optimizer.py::apply_plan()` dry_run=True path  
  - Acceptance Criteria: No actual changes made, actions_applied contains [DRY-RUN] prefix for each action  

- [x] **Set GREY_* environment variables on apply**  
  - Priority: P0  
  - Estimated Time: 30m  
  - Command/File: `daemon/gpu_optimizer.py::apply_plan()` env var setting  
  - Acceptance Criteria: Sets GREY_VRAM_CAP_MB, GREY_COOLDOWN_C, GREY_RESUME_C, GREY_BATCH_SIZE in os.environ  

- [x] **Persist applied plan to ~/.grey_optimizer/last_applied_plan.yaml**  
  - Priority: P1  
  - Estimated Time: 30m  
  - Command/File: `daemon/gpu_optimizer.py::_save_plan()`  
  - Acceptance Criteria: Creates YAML file with full plan, timestamp; readable by load_last_plan()  

- [x] **Add metrics collection to ApplyResult**  
  - Priority: P1  
  - Estimated Time: 30m  
  - Command/File: `daemon/gpu_optimizer.py::apply_plan()` metrics dict  
  - Acceptance Criteria: Include timestamp, plan_mode, actions_count, dry_run flag  

- [x] **Add artifact outputs (VRAM, thermal, fairness)**  
  - Priority: P1  
  - Estimated Time: 1h  
  - Command/File: `daemon/gpu_optimizer.py::write_artifact_report()`  
  - Acceptance Criteria: Generates vram_reclamation.json, thermal_signature.csv, fairness_heatmap.csv  

- [ ] **Implement nvidia-smi persistence mode toggle**  
  - Priority: P2  
  - Estimated Time: 45m  
  - Command/File: `daemon/gpu_optimizer.py::apply_plan()` nvidia persistence  
  - Acceptance Criteria: Run `nvidia-smi -pm 1` when destructive=True, verify with exit code, add to actions  

- [ ] **Add power limit setting for aggressive mode**  
  - Priority: P2  
  - Estimated Time: 1h  
  - Command/File: `daemon/gpu_optimizer.py::apply_plan()` power limit logic  
  - Acceptance Criteria: If aggressive and destructive=True, set power limit via nvidia-smi; requires explicit consent  

- [x] **SMOKE TEST: Enforce category**  
  - Priority: P0  
  - Estimated Time: 10m  
  - Command/File: `python daemon/gpu_optimizer.py apply --dry-run`  
  - Acceptance Criteria: Exits 0, prints "[DRY-RUN]" actions, no system changes made  

---

## CLI

- [x] **Implement detect subcommand**  
  - Priority: P0  
  - Estimated Time: 45m  
  - Command/File: `daemon/gpu_optimizer.py::cmd_detect()`  
  - Acceptance Criteria: `python gpu_optimizer.py detect` outputs GPUSpec, --json outputs valid JSON array  

- [x] **Implement plan subcommand with --mode and --output**  
  - Priority: P0  
  - Estimated Time: 45m  
  - Command/File: `daemon/gpu_optimizer.py::cmd_plan()`  
  - Acceptance Criteria: --mode safe|aggressive, --output writes YAML/JSON  

- [x] **Implement apply subcommand with --dry-run default**  
  - Priority: P0  
  - Estimated Time: 45m  
  - Command/File: `daemon/gpu_optimizer.py::cmd_apply()`  
  - Acceptance Criteria: Default dry_run=True, --no-dry-run for actual apply, --destructive requires confirm  

- [x] **Implement status subcommand**  
  - Priority: P1  
  - Estimated Time: 30m  
  - Command/File: `daemon/gpu_optimizer.py::cmd_status()`  
  - Acceptance Criteria: Shows GPU detection status, active plan, config/artifact directories  

- [x] **Implement start-daemon subcommand**  
  - Priority: P1  
  - Estimated Time: 1h  
  - Command/File: `daemon/gpu_optimizer.py::cmd_start_daemon()`  
  - Acceptance Criteria: Starts GPUOptimizerDaemon with --interval, --dry-run, --consent flags  

- [x] **Add --verbose flag for all subcommands**  
  - Priority: P1  
  - Estimated Time: 30m  
  - Command/File: `daemon/gpu_optimizer.py` argparse setup  
  - Acceptance Criteria: -v/--verbose enables DEBUG logging, shows detection sources, timing info  

- [ ] **Add --config override file option**  
  - Priority: P1  
  - Estimated Time: 30m  
  - Command/File: `daemon/gpu_optimizer.py` plan subcommand  
  - Acceptance Criteria: --config path/to/overrides.yaml loads and merges config_overrides dict  

- [x] **SMOKE TEST: CLI category**  
  - Priority: P0  
  - Estimated Time: 15m  
  - Command/File: `python daemon/gpu_optimizer.py --help`  
  - Acceptance Criteria: All subcommands show help, no import errors, detect/plan/apply/status/start-daemon work  

---

## Daemon

- [x] **Scaffold daemon loop in GPUOptimizerDaemon class**  
  - Priority: P0  
  - Estimated Time: 2h  
  - Command/File: `daemon/gpu_optimizer.py::GPUOptimizerDaemon`  
  - Acceptance Criteria: Load GPU spec at startup, monitor VRAM/thermal/utilization, apply plan in dry-run  

- [x] **Implement structured JSON logging in daemon**  
  - Priority: P1  
  - Estimated Time: 30m  
  - Command/File: `daemon/gpu_optimizer.py::GPUOptimizerDaemon::_log_structured()`  
  - Acceptance Criteria: Log entries have timestamp, event, details fields  

- [x] **Write periodic artifact reports**  
  - Priority: P1  
  - Estimated Time: 30m  
  - Command/File: `daemon/gpu_optimizer.py::GPUOptimizerDaemon::_run_loop()`  
  - Acceptance Criteria: Writes artifact report every 10 iterations  

- [x] **SMOKE TEST: Daemon category**  
  - Priority: P0  
  - Estimated Time: 10m  
  - Command/File: `python daemon/gpu_optimizer.py start-daemon --interval 5 --dry-run`  
  - Acceptance Criteria: Daemon starts, logs ticks, can be stopped with Ctrl+C  

---

## Config

- [x] **Create ~/.grey_optimizer/ directory structure**  
  - Priority: P0  
  - Estimated Time: 15m  
  - Command/File: `daemon/gpu_optimizer.py::CONFIG_DIR`  
  - Acceptance Criteria: Creates directory if not exists  

- [x] **Implement config.yaml loading and validation**  
  - Priority: P1  
  - Estimated Time: 45m  
  - Command/File: `daemon/gpu_optimizer.py::load_config_overrides()`  
  - Acceptance Criteria: Load YAML, merge with plan defaults  

- [x] **Implement last_gpu_spec.json persistence**  
  - Priority: P1  
  - Estimated Time: 30m  
  - Command/File: `daemon/gpu_optimizer.py::_save_spec()` and `load_last_spec()`  
  - Acceptance Criteria: Save after detection, load returns list[GPUSpec], handles missing file gracefully  

- [x] **Implement last_plan.yaml persistence**  
  - Priority: P1  
  - Estimated Time: 30m  
  - Command/File: `daemon/gpu_optimizer.py::_save_plan()` and `load_last_plan()`  
  - Acceptance Criteria: Save after planning, load returns OptimizationPlan, handles missing file gracefully  

- [x] **SMOKE TEST: Config category**  
  - Priority: P0  
  - Estimated Time: 10m  
  - Command/File: `python daemon/gpu_optimizer.py status`  
  - Acceptance Criteria: CONFIG_DIR shown, artifacts dir exists  

---

## Tests

- [x] **Write parser unit tests with mocked CLI output**  
  - Priority: P0  
  - Estimated Time: 2h  
  - Command/File: `tests/test_gpu_optimizer.py::TestParsers`  
  - Acceptance Criteria: Mock nvidia-smi, rocm-smi, lspci outputs; verify field extraction  

- [x] **Write detection integration tests (no-vendor-tools fallback)**  
  - Priority: P0  
  - Estimated Time: 1.5h  
  - Command/File: `tests/test_gpu_optimizer.py::TestIntegration::test_full_workflow_no_vendor_tools`  
  - Acceptance Criteria: Mock all tools as missing, verify graceful fallback, GPUSpec has notes  

- [x] **Write planning unit tests for safe/aggressive modes**  
  - Priority: P0  
  - Estimated Time: 1h  
  - Command/File: `tests/test_gpu_optimizer.py::TestPlanning`  
  - Acceptance Criteria: Verify VRAM caps, batch sizes, thermal thresholds for both modes  

- [x] **Write apply unit tests with dry-run verification**  
  - Priority: P0  
  - Estimated Time: 1h  
  - Command/File: `tests/test_gpu_optimizer.py::TestApply`  
  - Acceptance Criteria: Verify dry_run=True by default, no side effects, actions list populated  

- [ ] **Write CLI argument parsing tests**  
  - Priority: P1  
  - Estimated Time: 1h  
  - Command/File: `tests/test_gpu_optimizer.py::TestCLI`  
  - Acceptance Criteria: Test detect --json, plan --mode, apply --dry-run argument handling  

- [ ] **Write edge case tests (empty specs, zero VRAM, malformed output)**  
  - Priority: P1  
  - Estimated Time: 1h  
  - Command/File: `tests/test_gpu_optimizer.py::TestEdgeCases`  
  - Acceptance Criteria: No exceptions on edge inputs, reasonable defaults applied  

- [ ] **SMOKE TEST: Tests category**  
  - Priority: P0  
  - Estimated Time: 5m  
  - Command/File: `pytest tests/test_gpu_optimizer.py -v --tb=short`  
  - Acceptance Criteria: All tests pass, no import errors, coverage >= 80%  

---

## Docs

- [ ] **Add module docstring with usage examples**  
  - Priority: P1  
  - Estimated Time: 30m  
  - Command/File: `daemon/gpu_optimizer.py` module docstring  
  - Acceptance Criteria: Docstring includes CLI examples, API examples, config file format  

- [ ] **Document GPUSpec normalized fields**  
  - Priority: P1  
  - Estimated Time: 30m  
  - Command/File: `daemon/gpu_optimizer.py::GPUSpec` docstring  
  - Acceptance Criteria: All 17 fields documented with types, examples, and detection sources  

- [ ] **Document OptimizationPlan fields and heuristics**  
  - Priority: P1  
  - Estimated Time: 30m  
  - Command/File: `daemon/gpu_optimizer.py::OptimizationPlan` docstring  
  - Acceptance Criteria: All 8 optimization params documented with safe/aggressive values  

- [ ] **Add README section for GPU optimization**  
  - Priority: P2  
  - Estimated Time: 45m  
  - Command/File: `README.md` GPU Optimization section  
  - Acceptance Criteria: Quick start, CLI reference, config file example, troubleshooting  

- [ ] **SMOKE TEST: Docs category**  
  - Priority: P1  
  - Estimated Time: 5m  
  - Command/File: `python -c "import daemon.gpu_optimizer; help(daemon.gpu_optimizer)"`  
  - Acceptance Criteria: Module help displays correctly, no missing docstrings for public functions  

---

## Release Checklist

- [ ] **All unit tests pass**  
  - Priority: P0  
  - Command: `pytest tests/test_gpu_optimizer.py -v`  
  - Acceptance Criteria: Exit code 0, all tests green  

- [ ] **All smoke tests pass**  
  - Priority: P0  
  - Command: `python daemon/gpu_optimizer.py detect --json | python -m json.tool`  
  - Acceptance Criteria: Valid JSON output, no exceptions  

- [ ] **Type hints validated with mypy**  
  - Priority: P1  
  - Command: `mypy daemon/gpu_optimizer.py --strict`  
  - Acceptance Criteria: No errors, type coverage 100%  

- [ ] **Linting passes**  
  - Priority: P1  
  - Command: `ruff check daemon/gpu_optimizer.py`  
  - Acceptance Criteria: No errors or warnings  

- [ ] **Dry-run mode tested on real hardware**  
  - Priority: P0  
  - Command: `python daemon/gpu_optimizer.py apply --dry-run`  
  - Acceptance Criteria: Shows planned actions, no actual system changes  

- [ ] **Safe mode tested on real hardware**  
  - Priority: P0  
  - Command: `python daemon/gpu_optimizer.py detect && python daemon/gpu_optimizer.py plan --mode safe`  
  - Acceptance Criteria: Detection returns valid spec, plan has reasonable values  

- [ ] **Documentation reviewed**  
  - Priority: P1  
  - Command: `grep -c '"""' daemon/gpu_optimizer.py`  
  - Acceptance Criteria: All public functions have docstrings  

- [ ] **Version bump completed**  
  - Priority: P0  
  - Command: Update `__version__` in `daemon/gpu_optimizer.py`  
  - Acceptance Criteria: Version follows semver, changelog updated  

---

## Notes

- **Safety First:** Always default to `dry_run=True`. Never force-kill GPU processes without explicit `destructive=True` consent.
- **Confidence Scores:** Plan confidence < 0.5 should trigger warnings in CLI output.
- **Vendor Priority:** Detection order is nvidia-smi → rocm-smi → intel_gpu_top → lspci → lshw → glxinfo → vulkaninfo → Python libs.
- **Config Directory:** All state persists to `~/.grey_optimizer/` with 0700 permissions.
