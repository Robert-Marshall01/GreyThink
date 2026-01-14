#!/usr/bin/env python3
"""
Grey Optimizer - Synthetic Workload Generator

This module generates controlled, synthetic workloads for CPU, memory, and disk
to enable accurate before/after measurements of the optimizer's effectiveness.

The workloads are designed to be:
- Deterministic: Same seed produces same behavior
- Controllable: Duration and intensity are configurable  
- Safe: Resource usage is capped and cleanly released
- Measurable: Produces consistent baseline metrics

Usage:
    from workload_generator import WorkloadGenerator
    
    async with WorkloadGenerator() as gen:
        await gen.cpu_workload(duration=10, intensity=0.8)
        await gen.memory_workload(mb=512, duration=10)
        await gen.disk_workload(mb=100, duration=10)
"""

import asyncio
import hashlib
import mmap
import os
import platform
import random
import struct
import tempfile
import time
from dataclasses import dataclass, field
from pathlib import Path
from typing import Callable, Optional

import psutil


@dataclass
class WorkloadMetrics:
    """Metrics captured during a workload run."""
    
    workload_type: str
    start_time: float
    end_time: float
    duration_requested: float
    duration_actual: float
    
    # CPU metrics
    cpu_percent_avg: float = 0.0
    cpu_percent_max: float = 0.0
    
    # Memory metrics
    memory_rss_start_mb: float = 0.0
    memory_rss_peak_mb: float = 0.0
    memory_rss_end_mb: float = 0.0
    
    # Disk metrics
    bytes_written: int = 0
    bytes_read: int = 0
    iops_write: float = 0.0
    iops_read: float = 0.0
    
    # Additional info
    platform: str = field(default_factory=platform.system)
    error: Optional[str] = None


class WorkloadGenerator:
    """
    Generates synthetic workloads for verification testing.
    
    All workloads are async and can be run concurrently or sequentially.
    Resources are automatically cleaned up when the context exits.
    """
    
    def __init__(
        self,
        seed: int = 42,
        temp_dir: Optional[Path] = None,
        max_memory_mb: int = 2048,
        max_disk_mb: int = 1024
    ):
        """
        Initialize the workload generator.
        
        Args:
            seed: Random seed for deterministic behavior
            temp_dir: Directory for temporary files (uses system temp if None)
            max_memory_mb: Maximum memory allocation (safety cap)
            max_disk_mb: Maximum disk allocation (safety cap)
        """
        self.seed = seed
        self.random = random.Random(seed)
        self.temp_dir = temp_dir or Path(tempfile.gettempdir())
        self.max_memory_mb = max_memory_mb
        self.max_disk_mb = max_disk_mb
        
        # Track allocations for cleanup
        self._memory_allocations: list[mmap.mmap] = []
        self._temp_files: list[Path] = []
        self._running = False
        
        # Process handle for metrics
        self._process = psutil.Process()
    
    async def __aenter__(self) -> 'WorkloadGenerator':
        """Enter async context."""
        self._running = True
        return self
    
    async def __aexit__(self, *args) -> None:
        """Exit async context and cleanup."""
        self._running = False
        await self.cleanup()
    
    async def cleanup(self) -> None:
        """Release all allocated resources."""
        # Release memory mappings
        for mm in self._memory_allocations:
            try:
                mm.close()
            except Exception:
                pass
        self._memory_allocations.clear()
        
        # Remove temporary files
        for path in self._temp_files:
            try:
                path.unlink(missing_ok=True)
            except Exception:
                pass
        self._temp_files.clear()
    
    def _capture_baseline(self) -> dict:
        """Capture baseline system metrics."""
        return {
            'cpu_percent': psutil.cpu_percent(interval=0.1),
            'memory_rss_mb': self._process.memory_info().rss / (1024 * 1024),
            'memory_percent': psutil.virtual_memory().percent,
            'disk_io': psutil.disk_io_counters(),
            'timestamp': time.time()
        }
    
    async def cpu_workload(
        self,
        duration: float = 10.0,
        intensity: float = 0.5,
        callback: Optional[Callable[[float], None]] = None
    ) -> WorkloadMetrics:
        """
        Generate CPU workload by performing hash calculations.
        
        This workload is CPU-bound and does not allocate significant memory.
        The intensity parameter controls what fraction of time is spent computing.
        
        Args:
            duration: How long to run the workload (seconds)
            intensity: CPU intensity from 0.0 to 1.0 (fraction of time computing)
            callback: Optional callback for progress (0.0 to 1.0)
        
        Returns:
            WorkloadMetrics with CPU statistics
        """
        intensity = max(0.1, min(1.0, intensity))
        
        metrics = WorkloadMetrics(
            workload_type='cpu',
            start_time=time.time(),
            end_time=0,
            duration_requested=duration,
            duration_actual=0,
            memory_rss_start_mb=self._process.memory_info().rss / (1024 * 1024)
        )
        
        cpu_samples = []
        peak_memory = metrics.memory_rss_start_mb
        
        # Data to hash (deterministic based on seed)
        data = self.random.randbytes(4096)
        
        start = time.time()
        iteration = 0
        
        try:
            while (elapsed := time.time() - start) < duration:
                if not self._running:
                    break
                
                # Compute phase: hash repeatedly
                compute_time = 0.01 * intensity  # 10ms * intensity
                compute_start = time.time()
                
                while time.time() - compute_start < compute_time:
                    # SHA-256 is CPU-intensive
                    for _ in range(100):
                        hashlib.sha256(data).digest()
                        data = hashlib.sha256(data).digest() + data[32:]
                
                # Sleep phase: allow other work
                sleep_time = 0.01 * (1 - intensity)
                if sleep_time > 0:
                    await asyncio.sleep(sleep_time)
                
                # Sample metrics
                if iteration % 10 == 0:
                    cpu = psutil.cpu_percent(interval=0)
                    cpu_samples.append(cpu)
                    
                    mem = self._process.memory_info().rss / (1024 * 1024)
                    peak_memory = max(peak_memory, mem)
                    
                    if callback:
                        callback(elapsed / duration)
                
                iteration += 1
        
        except Exception as e:
            metrics.error = str(e)
        
        metrics.end_time = time.time()
        metrics.duration_actual = metrics.end_time - metrics.start_time
        metrics.cpu_percent_avg = sum(cpu_samples) / len(cpu_samples) if cpu_samples else 0
        metrics.cpu_percent_max = max(cpu_samples) if cpu_samples else 0
        metrics.memory_rss_peak_mb = peak_memory
        metrics.memory_rss_end_mb = self._process.memory_info().rss / (1024 * 1024)
        
        return metrics
    
    async def memory_workload(
        self,
        mb: int = 256,
        duration: float = 10.0,
        pattern: str = 'random',
        callback: Optional[Callable[[float], None]] = None
    ) -> WorkloadMetrics:
        """
        Generate memory workload by allocating and accessing memory.
        
        This workload allocates memory and performs access patterns to ensure
        the memory is actually used (not just allocated).
        
        Args:
            mb: Amount of memory to allocate (capped at max_memory_mb)
            duration: How long to maintain allocation (seconds)
            pattern: Access pattern - 'random', 'sequential', or 'sparse'
            callback: Optional callback for progress
        
        Returns:
            WorkloadMetrics with memory statistics
        """
        mb = min(mb, self.max_memory_mb)
        
        metrics = WorkloadMetrics(
            workload_type='memory',
            start_time=time.time(),
            end_time=0,
            duration_requested=duration,
            duration_actual=0,
            memory_rss_start_mb=self._process.memory_info().rss / (1024 * 1024)
        )
        
        peak_memory = metrics.memory_rss_start_mb
        
        try:
            # Create anonymous mmap for memory allocation
            size = mb * 1024 * 1024
            mm = mmap.mmap(-1, size, access=mmap.ACCESS_WRITE)
            self._memory_allocations.append(mm)
            
            # Initialize memory to ensure it's allocated
            chunk_size = 4096  # Write in page-sized chunks
            data = self.random.randbytes(chunk_size)
            
            for offset in range(0, size, chunk_size):
                mm.seek(offset)
                mm.write(data)
                
                if offset % (50 * 1024 * 1024) == 0:  # Every 50MB
                    await asyncio.sleep(0)
            
            # Now maintain and access for duration
            start = time.time()
            
            while (elapsed := time.time() - start) < duration:
                if not self._running:
                    break
                
                # Access pattern
                if pattern == 'random':
                    for _ in range(1000):
                        offset = self.random.randint(0, size - chunk_size)
                        mm.seek(offset)
                        mm.read(chunk_size)
                
                elif pattern == 'sequential':
                    for offset in range(0, size, chunk_size * 100):
                        mm.seek(offset)
                        mm.read(chunk_size)
                        if not self._running:
                            break
                
                elif pattern == 'sparse':
                    for _ in range(100):
                        offset = self.random.randint(0, size - chunk_size)
                        mm.seek(offset)
                        mm.read(chunk_size)
                
                # Sample metrics
                mem = self._process.memory_info().rss / (1024 * 1024)
                peak_memory = max(peak_memory, mem)
                
                if callback:
                    callback(elapsed / duration)
                
                await asyncio.sleep(0.1)
        
        except Exception as e:
            metrics.error = str(e)
        
        metrics.end_time = time.time()
        metrics.duration_actual = metrics.end_time - metrics.start_time
        metrics.memory_rss_peak_mb = peak_memory
        metrics.memory_rss_end_mb = self._process.memory_info().rss / (1024 * 1024)
        
        return metrics
    
    async def disk_workload(
        self,
        mb: int = 100,
        duration: float = 10.0,
        block_size: int = 4096,
        callback: Optional[Callable[[float], None]] = None
    ) -> WorkloadMetrics:
        """
        Generate disk I/O workload.
        
        This workload writes and reads files to generate disk activity.
        
        Args:
            mb: Total amount of data to write (capped at max_disk_mb)
            duration: Target duration for the workload
            block_size: Block size for I/O operations
            callback: Optional callback for progress
        
        Returns:
            WorkloadMetrics with disk I/O statistics
        """
        mb = min(mb, self.max_disk_mb)
        
        metrics = WorkloadMetrics(
            workload_type='disk',
            start_time=time.time(),
            end_time=0,
            duration_requested=duration,
            duration_actual=0,
            memory_rss_start_mb=self._process.memory_info().rss / (1024 * 1024)
        )
        
        io_before = psutil.disk_io_counters()
        
        # Create temp file
        temp_path = self.temp_dir / f'grey_workload_{self.random.randint(0, 999999)}.tmp'
        self._temp_files.append(temp_path)
        
        total_bytes = mb * 1024 * 1024
        bytes_written = 0
        bytes_read = 0
        write_ops = 0
        read_ops = 0
        
        try:
            # Write phase
            data = self.random.randbytes(block_size)
            
            with open(temp_path, 'wb', buffering=0) as f:
                while bytes_written < total_bytes:
                    if not self._running:
                        break
                    
                    f.write(data)
                    bytes_written += block_size
                    write_ops += 1
                    
                    if write_ops % 100 == 0:
                        await asyncio.sleep(0)
                        if callback:
                            progress = bytes_written / total_bytes * 0.5
                            callback(progress)
            
            # Sync to disk
            os.sync() if hasattr(os, 'sync') else None
            
            # Read phase
            with open(temp_path, 'rb', buffering=0) as f:
                while bytes_read < total_bytes:
                    if not self._running:
                        break
                    
                    chunk = f.read(block_size)
                    if not chunk:
                        break
                    
                    bytes_read += len(chunk)
                    read_ops += 1
                    
                    if read_ops % 100 == 0:
                        await asyncio.sleep(0)
                        if callback:
                            progress = 0.5 + (bytes_read / total_bytes * 0.5)
                            callback(progress)
        
        except Exception as e:
            metrics.error = str(e)
        
        metrics.end_time = time.time()
        metrics.duration_actual = metrics.end_time - metrics.start_time
        metrics.bytes_written = bytes_written
        metrics.bytes_read = bytes_read
        
        if metrics.duration_actual > 0:
            metrics.iops_write = write_ops / metrics.duration_actual
            metrics.iops_read = read_ops / metrics.duration_actual
        
        metrics.memory_rss_end_mb = self._process.memory_info().rss / (1024 * 1024)
        
        return metrics
    
    async def combined_workload(
        self,
        cpu_intensity: float = 0.5,
        memory_mb: int = 256,
        disk_mb: int = 50,
        duration: float = 30.0,
        callback: Optional[Callable[[str, float], None]] = None
    ) -> dict[str, WorkloadMetrics]:
        """
        Run combined workloads to simulate realistic system load.
        
        Args:
            cpu_intensity: CPU workload intensity (0.0 to 1.0)
            memory_mb: Memory to allocate
            disk_mb: Disk I/O amount
            duration: Total duration for the workload
            callback: Callback receiving (workload_type, progress)
        
        Returns:
            Dictionary mapping workload type to its metrics
        """
        results = {}
        
        # Run sequentially to get clean measurements
        for name, coro in [
            ('cpu', self.cpu_workload(
                duration=duration/3, 
                intensity=cpu_intensity,
                callback=lambda p: callback('cpu', p) if callback else None
            )),
            ('memory', self.memory_workload(
                mb=memory_mb,
                duration=duration/3,
                callback=lambda p: callback('memory', p) if callback else None
            )),
            ('disk', self.disk_workload(
                mb=disk_mb,
                duration=duration/3,
                callback=lambda p: callback('disk', p) if callback else None
            ))
        ]:
            results[name] = await coro
        
        return results


async def main():
    """Demo the workload generator."""
    print("Grey Optimizer - Workload Generator Demo")
    print("=" * 50)
    
    async with WorkloadGenerator(seed=42) as gen:
        print("\n1. CPU Workload (5 seconds, 70% intensity)...")
        cpu_metrics = await gen.cpu_workload(duration=5, intensity=0.7)
        print(f"   Avg CPU: {cpu_metrics.cpu_percent_avg:.1f}%")
        print(f"   Peak CPU: {cpu_metrics.cpu_percent_max:.1f}%")
        
        print("\n2. Memory Workload (128MB for 5 seconds)...")
        mem_metrics = await gen.memory_workload(mb=128, duration=5)
        print(f"   Start RSS: {mem_metrics.memory_rss_start_mb:.1f} MB")
        print(f"   Peak RSS: {mem_metrics.memory_rss_peak_mb:.1f} MB")
        print(f"   End RSS: {mem_metrics.memory_rss_end_mb:.1f} MB")
        
        print("\n3. Disk Workload (50MB)...")
        disk_metrics = await gen.disk_workload(mb=50)
        print(f"   Written: {disk_metrics.bytes_written / 1024 / 1024:.1f} MB")
        print(f"   Read: {disk_metrics.bytes_read / 1024 / 1024:.1f} MB")
        print(f"   Write IOPS: {disk_metrics.iops_write:.0f}")
        print(f"   Read IOPS: {disk_metrics.iops_read:.0f}")
    
    print("\n" + "=" * 50)
    print("Workload generator demo complete.")


if __name__ == '__main__':
    asyncio.run(main())
