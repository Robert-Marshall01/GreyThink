#!/usr/bin/env python3
"""
grey_gpu_optimizer/utils/__init__.py - Utility Package

This package provides utility modules for the Grey GPU Optimizer:

- chunking: Tensor chunking and batch size optimization
- dedup: VRAM deduplication utilities
- enforcement: Real GPU enforcement actions (throttling, cgroups, nvidia-smi)
"""

from __future__ import annotations

from grey_gpu_optimizer.utils.chunking import (
    compute_optimal_batch_size,
    adaptive_batch_resizer,
    chunk_tensor_for_checkpointing,
    BatchSizeRecommendation,
    ChunkingStrategy,
)
from grey_gpu_optimizer.utils.dedup import (
    VRAMDeduplicator,
    estimate_reclaimed_mb,
    find_duplicate_buffers,
    BufferInfo,
    DeduplicationReport,
)
from grey_gpu_optimizer.utils.enforcement import (
    get_gpu_processes,
    get_vram_usage,
    get_gpu_temperature,
    throttle_process,
    set_process_priority,
    set_cpu_affinity,
    set_nvidia_power_limit,
    set_nvidia_persistence_mode,
    create_gpu_cgroup,
    move_process_to_cgroup,
    thermal_throttle_check,
    enforce_vram_limits,
    apply_full_enforcement,
    generate_enforcement_artifacts,
    EnforcementAction,
    EnforcementResult,
    ProcessInfo,
)

__all__ = [
    # Chunking
    "compute_optimal_batch_size",
    "adaptive_batch_resizer",
    "chunk_tensor_for_checkpointing",
    "BatchSizeRecommendation",
    "ChunkingStrategy",
    # Dedup
    "VRAMDeduplicator",
    "estimate_reclaimed_mb",
    "find_duplicate_buffers",
    "BufferInfo",
    "DeduplicationReport",
    # Enforcement
    "get_gpu_processes",
    "get_vram_usage",
    "get_gpu_temperature",
    "throttle_process",
    "set_process_priority",
    "set_cpu_affinity",
    "set_nvidia_power_limit",
    "set_nvidia_persistence_mode",
    "create_gpu_cgroup",
    "move_process_to_cgroup",
    "thermal_throttle_check",
    "enforce_vram_limits",
    "apply_full_enforcement",
    "generate_enforcement_artifacts",
    "EnforcementAction",
    "EnforcementResult",
    "ProcessInfo",
]
