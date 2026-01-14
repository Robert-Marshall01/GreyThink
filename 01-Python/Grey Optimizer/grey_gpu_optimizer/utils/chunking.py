#!/usr/bin/env python3
"""
grey_gpu_optimizer/utils/chunking.py - Tensor Chunking Utilities

This module provides utilities for optimal tensor chunking and batch
sizing to maximize GPU utilization while preventing OOM errors.

Features:
- Compute optimal batch size based on GPU specs
- Adaptive batch resizing based on VRAM pressure
- Gradient checkpointing chunk size calculation

The heuristics are vendor-aware and adjust for VRAM size, memory
bandwidth, and compute capability.

Usage:
    from grey_gpu_optimizer.utils.chunking import compute_optimal_batch_size
    
    spec = detect_gpus()[0]
    batch_size = compute_optimal_batch_size(spec, model_size_mb=500)
"""

from __future__ import annotations

import logging
import math
from dataclasses import dataclass
from typing import Any, Generator

logger = logging.getLogger("grey_gpu_optimizer.utils.chunking")


# =============================================================================
# Data Classes
# =============================================================================

@dataclass
class BatchSizeRecommendation:
    """
    Recommendation for optimal batch size.
    
    Attributes:
        batch_size: Recommended batch size
        max_batch_size: Maximum safe batch size
        min_batch_size: Minimum viable batch size
        vram_per_sample_mb: Estimated VRAM per sample
        confidence: Confidence score (0.0-1.0)
        rationale: Explanation of the recommendation
    """
    batch_size: int
    max_batch_size: int
    min_batch_size: int
    vram_per_sample_mb: float
    confidence: float
    rationale: str


@dataclass
class ChunkingStrategy:
    """
    Strategy for tensor chunking.
    
    Attributes:
        chunk_size_mb: Size of each chunk in MB
        num_chunks: Number of chunks
        overlap_mb: Overlap between chunks (for gradient checkpointing)
        activation_checkpointing: Whether to enable activation checkpointing
        offload_to_cpu: Whether to offload intermediate activations to CPU
    """
    chunk_size_mb: int
    num_chunks: int
    overlap_mb: int
    activation_checkpointing: bool
    offload_to_cpu: bool


# =============================================================================
# Batch Size Computation
# =============================================================================

def compute_optimal_batch_size(
    gpu_spec: Any,
    model_size_mb: int = 0,
    sample_size_mb: float = 1.0,
    target_utilization: float = 0.75,
    prefer_power_of_2: bool = True
) -> BatchSizeRecommendation:
    """
    Compute the optimal batch size for a given GPU and model.
    
    This function uses heuristics based on:
    - Available VRAM
    - Model size
    - Sample size
    - GPU vendor characteristics
    - Compute capability
    
    Args:
        gpu_spec: GPUSpec object with GPU information
        model_size_mb: Model size in MB (weights + optimizer states)
        sample_size_mb: Estimated VRAM per sample (activations, gradients)
        target_utilization: Target VRAM utilization (0.0-1.0)
        prefer_power_of_2: Round to nearest power of 2
    
    Returns:
        BatchSizeRecommendation with optimal batch size
    
    Example:
        >>> spec = detect_gpus()[0]
        >>> rec = compute_optimal_batch_size(spec, model_size_mb=1000)
        >>> print(f"Use batch size: {rec.batch_size}")
    """
    # Get VRAM from spec
    vram_total = getattr(gpu_spec, "vram_total_mb", 0)
    vram_free = getattr(gpu_spec, "vram_free_mb", 0)
    vendor = getattr(gpu_spec, "vendor", "unknown")
    compute_cap = getattr(gpu_spec, "compute_capability", "")
    
    # Fallback for unknown VRAM
    if vram_total == 0:
        vram_total = 4096  # Conservative default
        confidence_base = 0.3
        rationale = "VRAM unknown, using conservative defaults"
    else:
        confidence_base = 0.7
        rationale = f"Based on {vram_total}MB total VRAM"
    
    # Use free VRAM if available, else estimate
    if vram_free > 0:
        available_vram = vram_free * target_utilization
    else:
        # Estimate: total - model - overhead
        overhead = 512  # System overhead
        available_vram = (vram_total - model_size_mb - overhead) * target_utilization
    
    if available_vram <= 0:
        # Model too large for GPU
        return BatchSizeRecommendation(
            batch_size=1,
            max_batch_size=1,
            min_batch_size=1,
            vram_per_sample_mb=sample_size_mb,
            confidence=0.2,
            rationale="VRAM insufficient, use gradient accumulation or model parallelism"
        )
    
    # Compute raw batch size
    raw_batch = int(available_vram / sample_size_mb)
    
    # Vendor-specific adjustments
    if vendor == "intel":
        # Intel GPUs have different memory characteristics
        raw_batch = int(raw_batch * 0.7)
        rationale += "; reduced for Intel GPU memory patterns"
    elif vendor == "amd":
        # AMD GPUs may need slightly smaller batches
        raw_batch = int(raw_batch * 0.9)
        rationale += "; adjusted for AMD memory management"
    
    # Compute capability adjustment for NVIDIA
    if compute_cap:
        try:
            major = int(compute_cap.split(".")[0])
            if major >= 8:
                # Ampere+ benefits from larger batches
                raw_batch = int(raw_batch * 1.1)
                rationale += f"; scaled up for compute capability {compute_cap}"
        except (ValueError, IndexError):
            pass
    
    # Ensure minimum batch size
    raw_batch = max(1, raw_batch)
    
    # Round to power of 2 if preferred
    if prefer_power_of_2 and raw_batch >= 2:
        # Find nearest power of 2
        log2 = math.log2(raw_batch)
        batch_size = 2 ** int(log2)  # Round down
        if batch_size < raw_batch and 2 ** int(log2 + 1) <= raw_batch * 1.5:
            batch_size = 2 ** int(log2 + 1)  # Round up if close
    else:
        batch_size = raw_batch
    
    # Calculate bounds
    max_batch = int(batch_size * 1.5)
    min_batch = max(1, batch_size // 4)
    
    # Confidence based on detection quality
    confidence = confidence_base
    if vram_free > 0:
        confidence += 0.1
    if compute_cap:
        confidence += 0.1
    confidence = min(0.95, confidence)
    
    return BatchSizeRecommendation(
        batch_size=batch_size,
        max_batch_size=max_batch,
        min_batch_size=min_batch,
        vram_per_sample_mb=sample_size_mb,
        confidence=confidence,
        rationale=rationale
    )


def adaptive_batch_resizer(
    current_batch: int,
    vram_used_pct: float,
    target_pct: float = 75.0,
    min_batch: int = 1,
    max_batch: int = 256,
    step_factor: float = 0.25
) -> int:
    """
    Adaptively resize batch based on current VRAM utilization.
    
    This function provides dynamic batch resizing during training
    to maintain optimal VRAM utilization.
    
    Args:
        current_batch: Current batch size
        vram_used_pct: Current VRAM utilization percentage
        target_pct: Target utilization percentage
        min_batch: Minimum allowed batch size
        max_batch: Maximum allowed batch size
        step_factor: Factor for batch size adjustment
    
    Returns:
        New recommended batch size
    
    Example:
        >>> # VRAM at 90%, reduce batch
        >>> new_batch = adaptive_batch_resizer(32, vram_used_pct=90.0)
        >>> # new_batch might be 24
    """
    diff = target_pct - vram_used_pct
    
    if abs(diff) < 5.0:
        # Within tolerance, no change
        return current_batch
    
    # Calculate adjustment
    adjustment = int(current_batch * step_factor * (diff / 100.0))
    
    # Apply bounds
    new_batch = current_batch + adjustment
    new_batch = max(min_batch, min(max_batch, new_batch))
    
    # Round to nice number
    if new_batch >= 8:
        new_batch = (new_batch // 4) * 4
    
    logger.debug(
        f"Adaptive resize: {current_batch} -> {new_batch} "
        f"(VRAM: {vram_used_pct:.1f}% target: {target_pct:.1f}%)"
    )
    
    return max(min_batch, new_batch)


# =============================================================================
# Tensor Chunking for Gradient Checkpointing
# =============================================================================

def chunk_tensor_for_checkpointing(
    tensor_size_mb: float,
    vram_available_mb: int,
    min_chunk_mb: int = 32,
    max_chunk_mb: int = 512
) -> ChunkingStrategy:
    """
    Determine optimal chunking strategy for gradient checkpointing.
    
    Gradient checkpointing trades compute for memory by recomputing
    activations during backward pass. This function determines the
    optimal chunk size to balance memory savings with recomputation cost.
    
    Args:
        tensor_size_mb: Total tensor size in MB
        vram_available_mb: Available VRAM in MB
        min_chunk_mb: Minimum chunk size
        max_chunk_mb: Maximum chunk size
    
    Returns:
        ChunkingStrategy with recommended chunking parameters
    
    Example:
        >>> strategy = chunk_tensor_for_checkpointing(2048, vram_available_mb=4096)
        >>> print(f"Use {strategy.num_chunks} chunks of {strategy.chunk_size_mb}MB")
    """
    # If tensor fits in memory, no chunking needed
    if tensor_size_mb <= vram_available_mb * 0.5:
        return ChunkingStrategy(
            chunk_size_mb=int(tensor_size_mb),
            num_chunks=1,
            overlap_mb=0,
            activation_checkpointing=False,
            offload_to_cpu=False
        )
    
    # Calculate chunks to fit within available VRAM
    # Keep some headroom for gradients
    target_chunk = int(vram_available_mb * 0.3)
    chunk_size = max(min_chunk_mb, min(max_chunk_mb, target_chunk))
    
    # Calculate number of chunks
    num_chunks = int(math.ceil(tensor_size_mb / chunk_size))
    
    # Determine if CPU offload is needed
    offload = tensor_size_mb > vram_available_mb * 2
    
    # Calculate overlap for gradient continuity
    overlap = min(chunk_size // 4, 64)
    
    return ChunkingStrategy(
        chunk_size_mb=chunk_size,
        num_chunks=num_chunks,
        overlap_mb=overlap,
        activation_checkpointing=True,
        offload_to_cpu=offload
    )


def generate_chunk_indices(
    total_elements: int,
    chunk_size: int,
    overlap: int = 0
) -> Generator[tuple[int, int], None, None]:
    """
    Generate start/end indices for chunked processing.
    
    Args:
        total_elements: Total number of elements
        chunk_size: Size of each chunk
        overlap: Overlap between chunks
    
    Yields:
        Tuples of (start_idx, end_idx)
    
    Example:
        >>> for start, end in generate_chunk_indices(1000, 256, overlap=32):
        ...     process_chunk(data[start:end])
    """
    if chunk_size <= 0:
        raise ValueError("chunk_size must be positive")
    
    if overlap < 0:
        raise ValueError("overlap must be non-negative")
    
    step = chunk_size - overlap
    if step <= 0:
        step = chunk_size  # Fallback if overlap >= chunk_size
    
    start = 0
    while start < total_elements:
        end = min(start + chunk_size, total_elements)
        yield (start, end)
        start += step
        if end >= total_elements:
            break
