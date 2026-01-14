#!/usr/bin/env python3
"""
grey_gpu_optimizer/utils/dedup.py - VRAM Deduplication Utilities

This module provides prototype utilities for VRAM deduplication,
which identifies and consolidates duplicate memory buffers to
reclaim GPU memory.

Features:
- Estimate reclaimable memory from deduplication
- Identify duplicate buffers by content hash
- Track deduplication candidates
- Report compression ratios

Note: This is a prototype implementation. Actual VRAM deduplication
requires vendor-specific APIs or custom memory allocators.

Usage:
    from grey_gpu_optimizer.utils.dedup import VRAMDeduplicator
    
    dedup = VRAMDeduplicator()
    dedup.register_buffer("weights_layer1", data, size_mb=100)
    report = dedup.analyze()
"""

from __future__ import annotations

import hashlib
import logging
from dataclasses import dataclass, field
from datetime import datetime, timezone
from typing import Any

logger = logging.getLogger("grey_gpu_optimizer.utils.dedup")


# =============================================================================
# Data Classes
# =============================================================================

@dataclass
class BufferInfo:
    """
    Information about a tracked VRAM buffer.
    
    Attributes:
        name: Buffer identifier/name
        size_mb: Size in megabytes
        content_hash: SHA256 hash of buffer contents
        created_at: Timestamp when registered
        reference_count: Number of references
        is_duplicate: Whether this is a duplicate of another buffer
        original_name: Name of the original buffer if duplicate
    """
    name: str
    size_mb: float
    content_hash: str
    created_at: str = ""
    reference_count: int = 1
    is_duplicate: bool = False
    original_name: str = ""
    
    def __post_init__(self) -> None:
        if not self.created_at:
            self.created_at = datetime.now(timezone.utc).isoformat()


@dataclass
class DeduplicationReport:
    """
    Report on deduplication analysis.
    
    Attributes:
        total_buffers: Total number of tracked buffers
        unique_buffers: Number of unique buffers
        duplicate_buffers: Number of duplicate buffers
        total_size_mb: Total size of all buffers
        unique_size_mb: Size of unique buffers only
        reclaimable_mb: Memory that could be reclaimed
        compression_ratio: Ratio of total to unique size
        duplicate_groups: Groups of duplicate buffers
    """
    total_buffers: int = 0
    unique_buffers: int = 0
    duplicate_buffers: int = 0
    total_size_mb: float = 0.0
    unique_size_mb: float = 0.0
    reclaimable_mb: float = 0.0
    compression_ratio: float = 1.0
    duplicate_groups: dict[str, list[str]] = field(default_factory=dict)


# =============================================================================
# VRAM Deduplicator Class
# =============================================================================

class VRAMDeduplicator:
    """
    VRAM deduplication tracker and analyzer.
    
    This class tracks VRAM buffers and identifies duplicates based on
    content hashing. It can estimate memory savings from deduplication.
    
    Note: This is a tracking/analysis tool. Actual deduplication requires
    integration with the memory allocator or framework-specific APIs.
    
    Example:
        >>> dedup = VRAMDeduplicator()
        >>> 
        >>> # Register buffers (in real use, integrate with framework)
        >>> dedup.register_buffer("layer1_weights", weights1, 100)
        >>> dedup.register_buffer("layer2_weights", weights2, 100)  # duplicate!
        >>> 
        >>> # Analyze for duplicates
        >>> report = dedup.analyze()
        >>> print(f"Reclaimable: {report.reclaimable_mb}MB")
    """
    
    def __init__(self) -> None:
        """Initialize the deduplicator."""
        self._buffers: dict[str, BufferInfo] = {}
        self._hash_to_name: dict[str, str] = {}
    
    def register_buffer(
        self,
        name: str,
        data: Any,
        size_mb: float = 0.0
    ) -> BufferInfo:
        """
        Register a VRAM buffer for deduplication tracking.
        
        Args:
            name: Unique name for this buffer
            data: Buffer data (or hash if pre-computed)
            size_mb: Size in megabytes (estimated if not provided)
        
        Returns:
            BufferInfo for the registered buffer
        """
        # Compute hash
        if isinstance(data, str) and len(data) == 64:
            # Already a hash
            content_hash = data
        elif hasattr(data, "tobytes"):
            # NumPy/PyTorch tensor
            content_hash = hashlib.sha256(data.tobytes()).hexdigest()
        elif isinstance(data, bytes):
            content_hash = hashlib.sha256(data).hexdigest()
        else:
            # Use string representation as fallback
            content_hash = hashlib.sha256(str(data).encode()).hexdigest()
        
        # Estimate size if not provided
        if size_mb == 0:
            if hasattr(data, "nbytes"):
                size_mb = data.nbytes / (1024 * 1024)
            elif hasattr(data, "numel") and hasattr(data, "element_size"):
                size_mb = (data.numel() * data.element_size()) / (1024 * 1024)
            elif isinstance(data, bytes):
                size_mb = len(data) / (1024 * 1024)
        
        # Check for duplicate
        is_duplicate = content_hash in self._hash_to_name
        original_name = self._hash_to_name.get(content_hash, "")
        
        # Create buffer info
        info = BufferInfo(
            name=name,
            size_mb=size_mb,
            content_hash=content_hash,
            is_duplicate=is_duplicate,
            original_name=original_name
        )
        
        # Track buffer
        self._buffers[name] = info
        
        if not is_duplicate:
            self._hash_to_name[content_hash] = name
        else:
            # Increment reference count on original
            if original_name in self._buffers:
                self._buffers[original_name].reference_count += 1
            logger.debug(f"Duplicate buffer detected: {name} matches {original_name}")
        
        return info
    
    def unregister_buffer(self, name: str) -> bool:
        """
        Remove a buffer from tracking.
        
        Args:
            name: Buffer name to remove
        
        Returns:
            True if buffer was found and removed
        """
        if name not in self._buffers:
            return False
        
        info = self._buffers[name]
        
        # If this is the original, remove from hash map
        if not info.is_duplicate:
            if info.content_hash in self._hash_to_name:
                del self._hash_to_name[info.content_hash]
        
        del self._buffers[name]
        return True
    
    def analyze(self) -> DeduplicationReport:
        """
        Analyze tracked buffers for deduplication opportunities.
        
        Returns:
            DeduplicationReport with analysis results
        """
        if not self._buffers:
            return DeduplicationReport()
        
        # Group buffers by hash
        hash_groups: dict[str, list[str]] = {}
        for name, info in self._buffers.items():
            if info.content_hash not in hash_groups:
                hash_groups[info.content_hash] = []
            hash_groups[info.content_hash].append(name)
        
        # Calculate metrics
        total_buffers = len(self._buffers)
        unique_buffers = len(hash_groups)
        duplicate_buffers = total_buffers - unique_buffers
        
        total_size = sum(info.size_mb for info in self._buffers.values())
        
        # Unique size: size of one buffer per hash group
        unique_size = sum(
            self._buffers[names[0]].size_mb
            for names in hash_groups.values()
            if names
        )
        
        reclaimable = total_size - unique_size
        
        compression_ratio = total_size / unique_size if unique_size > 0 else 1.0
        
        # Build duplicate groups (only groups with > 1 buffer)
        duplicate_groups = {
            h: names for h, names in hash_groups.items()
            if len(names) > 1
        }
        
        return DeduplicationReport(
            total_buffers=total_buffers,
            unique_buffers=unique_buffers,
            duplicate_buffers=duplicate_buffers,
            total_size_mb=total_size,
            unique_size_mb=unique_size,
            reclaimable_mb=reclaimable,
            compression_ratio=compression_ratio,
            duplicate_groups=duplicate_groups
        )
    
    def get_duplicates(self) -> list[tuple[str, str, float]]:
        """
        Get list of duplicate buffers with their originals.
        
        Returns:
            List of (duplicate_name, original_name, size_mb) tuples
        """
        duplicates = []
        for name, info in self._buffers.items():
            if info.is_duplicate:
                duplicates.append((name, info.original_name, info.size_mb))
        return duplicates
    
    def clear(self) -> None:
        """Clear all tracked buffers."""
        self._buffers.clear()
        self._hash_to_name.clear()


# =============================================================================
# Utility Functions
# =============================================================================

def estimate_reclaimed_mb(
    vram_total_mb: int,
    expected_duplication_ratio: float = 0.15
) -> int:
    """
    Estimate reclaimable VRAM from deduplication.
    
    This function provides a rough estimate based on typical
    duplication patterns in ML workloads.
    
    Args:
        vram_total_mb: Total VRAM capacity in MB
        expected_duplication_ratio: Expected ratio of duplicate memory (0.0-1.0)
    
    Returns:
        Estimated reclaimable memory in MB
    
    Heuristics:
        - Small models (< 4GB): 5-10% duplication
        - Medium models (4-16GB): 10-20% duplication
        - Large models (> 16GB): 15-25% duplication
    """
    # Adjust ratio based on VRAM size
    if vram_total_mb < 4096:
        ratio = min(0.10, expected_duplication_ratio)
    elif vram_total_mb < 16384:
        ratio = expected_duplication_ratio
    else:
        ratio = min(0.25, expected_duplication_ratio * 1.5)
    
    return int(vram_total_mb * ratio)


def find_duplicate_buffers(
    buffers: list[tuple[str, Any, float]]
) -> dict[str, list[str]]:
    """
    Find duplicate buffers in a list.
    
    Args:
        buffers: List of (name, data, size_mb) tuples
    
    Returns:
        Dictionary mapping hash to list of buffer names with that hash
    
    Example:
        >>> buffers = [
        ...     ("a", data1, 100),
        ...     ("b", data2, 100),  # same as data1
        ...     ("c", data3, 200),
        ... ]
        >>> duplicates = find_duplicate_buffers(buffers)
        >>> # {"hash1": ["a", "b"]}
    """
    dedup = VRAMDeduplicator()
    
    for name, data, size_mb in buffers:
        dedup.register_buffer(name, data, size_mb)
    
    report = dedup.analyze()
    return report.duplicate_groups


def compute_buffer_hash(data: Any) -> str:
    """
    Compute SHA256 hash of buffer data.
    
    Args:
        data: Buffer data (bytes, tensor, or string)
    
    Returns:
        Hexadecimal hash string
    """
    if hasattr(data, "tobytes"):
        return hashlib.sha256(data.tobytes()).hexdigest()
    elif isinstance(data, bytes):
        return hashlib.sha256(data).hexdigest()
    else:
        return hashlib.sha256(str(data).encode()).hexdigest()
