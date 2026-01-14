"""
grey_gpu_optimizer - GPU Immune-System Governance Framework

A production-ready GPU optimization framework that implements:
- GPU hardware detection with multi-vendor support
- Safe and aggressive optimization planning
- Non-destructive enforcement with dry-run mode
- Daemon-based continuous monitoring
- Artifact generation for audit trails

Safety: All operations default to read-only or dry-run mode.
Destructive actions require explicit consent flags.

Usage:
    from grey_gpu_optimizer import detect_gpus, plan_optimizations, apply_plan
    
    # Detect GPU hardware
    specs = detect_gpus()
    
    # Generate optimization plan
    plan = plan_optimizations(specs[0], mode="safe")
    
    # Apply with dry-run (default)
    result = apply_plan(plan, dry_run=True)

CLI:
    grey-gpu-opt detect
    grey-gpu-opt plan --mode safe
    grey-gpu-opt apply --dry-run
    grey-gpu-opt status
    grey-gpu-opt start-daemon --interval 30
"""

from __future__ import annotations

__version__ = "1.0.0"
__author__ = "Grey Optimizer Team"

# Core optimizer functions
from grey_gpu_optimizer.optimizer import (
    GPUSpec,
    OptimizationPlan,
    ApplyResult,
    detect_gpus,
    plan_optimizations,
    apply_plan,
    save_spec,
    load_spec,
    save_plan,
    load_plan,
    load_config_overrides,
    CONFIG_DIR,
    SPEC_FILE,
    PLAN_FILE,
    ARTIFACTS_DIR,
)

# Daemon
from grey_gpu_optimizer.daemon import (
    GPUOptimizerDaemon,
    DaemonStatus,
    MonitorSample,
    start_daemon,
    stop_daemon,
    daemon_status,
    sample_gpu_metrics,
)

# Logging
from grey_gpu_optimizer.logging_config import (
    setup_logging,
    get_logger,
    get_json_logger,
    log_event,
)

# Utilities - lazy import to avoid potential circular imports
def __getattr__(name: str):
    """Lazy import utilities."""
    if name in (
        "compute_optimal_batch_size",
        "adaptive_batch_resizer",
        "chunk_tensor_for_checkpointing",
        "BatchSizeRecommendation",
        "ChunkingStrategy",
    ):
        from grey_gpu_optimizer.utils import chunking
        return getattr(chunking, name)
    
    if name in (
        "VRAMDeduplicator",
        "estimate_reclaimed_mb",
        "find_duplicate_buffers",
        "BufferInfo",
        "DeduplicationReport",
    ):
        from grey_gpu_optimizer.utils import dedup
        return getattr(dedup, name)
    
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")


__all__ = [
    # Version
    "__version__",
    # Core optimizer
    "GPUSpec",
    "OptimizationPlan",
    "ApplyResult",
    "detect_gpus",
    "plan_optimizations",
    "apply_plan",
    "save_spec",
    "load_spec",
    "save_plan",
    "load_plan",
    "load_config_overrides",
    # Config paths
    "CONFIG_DIR",
    "SPEC_FILE",
    "PLAN_FILE",
    "ARTIFACTS_DIR",
    # Daemon
    "GPUOptimizerDaemon",
    "DaemonStatus",
    "MonitorSample",
    "start_daemon",
    "stop_daemon",
    "daemon_status",
    "sample_gpu_metrics",
    # Chunking utilities
    "compute_optimal_batch_size",
    "adaptive_batch_resizer",
    "chunk_tensor_for_checkpointing",
    "BatchSizeRecommendation",
    "ChunkingStrategy",
    # Dedup utilities
    "VRAMDeduplicator",
    "estimate_reclaimed_mb",
    "find_duplicate_buffers",
    "BufferInfo",
    "DeduplicationReport",
    # Logging
    "setup_logging",
    "get_logger",
    "get_json_logger",
    "log_event",
]
