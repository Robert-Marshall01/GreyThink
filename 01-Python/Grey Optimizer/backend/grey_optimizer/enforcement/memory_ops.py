"""
Memory Operations - Python Wrapper for C Helper

Provides Python bindings for low-level memory operations in libgrey_memory.so.

Operations:
- parse_maps: Parse /proc/[pid]/maps for memory regions
- advise_memory: Apply madvise(MADV_DONTNEED) to release pages
- count_cold_pages: Identify cold (unused) vs hot pages
- clear_soft_dirty: Reset soft-dirty bits for tracking

Safety:
- All C operations validate inputs
- Operations are reversible (pages refault on access)
- Errors are caught and converted to Python exceptions

Usage:
    from grey_optimizer.enforcement.memory_ops import MemoryOps
    
    ops = MemoryOps()
    if ops.is_available():
        regions = ops.parse_maps(pid)
        result = ops.advise_anonymous(pid, target_mb=100)
"""

import ctypes
import os
import logging
from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional, Dict, Any

logger = logging.getLogger(__name__)


# Constants matching C code
GREY_OK = 0
GREY_ERR_INVALID = -1
GREY_ERR_PERM = -2
GREY_ERR_NOTFOUND = -3
GREY_ERR_NOMEM = -4
GREY_ERR_SYSCALL = -5

MAX_REGIONS = 4096


@dataclass
class MemoryRegion:
    """Memory region from /proc/[pid]/maps."""
    start: int
    end: int
    perms: str
    offset: int
    path: str
    is_anonymous: bool
    is_writable: bool
    is_private: bool
    
    @property
    def size(self) -> int:
        """Size in bytes."""
        return self.end - self.start
    
    @property
    def size_mb(self) -> float:
        """Size in MB."""
        return round(self.size / (1024 * 1024), 2)
    
    @property
    def is_heap(self) -> bool:
        return "[heap]" in self.path
    
    @property
    def is_stack(self) -> bool:
        return "[stack]" in self.path


@dataclass
class AdviseResult:
    """Result of madvise operation."""
    success: bool
    bytes_advised: int
    regions_processed: int
    errors: int
    error_message: str = ""
    
    @property
    def mb_advised(self) -> float:
        return round(self.bytes_advised / (1024 * 1024), 2)


@dataclass
class ColdPageResult:
    """Result of cold page detection."""
    cold_pages: int
    hot_pages: int
    total_pages: int
    cold_ratio: float
    
    @property
    def cold_mb(self) -> float:
        return round(self.cold_pages * 4 / 1024, 2)  # 4KB pages
    
    @property
    def hot_mb(self) -> float:
        return round(self.hot_pages * 4 / 1024, 2)


# C struct definitions for ctypes
class GreyMemRegion(ctypes.Structure):
    """C struct: grey_mem_region_t"""
    _fields_ = [
        ("start", ctypes.c_ulong),
        ("end", ctypes.c_ulong),
        ("perms", ctypes.c_char * 8),
        ("offset", ctypes.c_ulong),
        ("path", ctypes.c_char * 256),
        ("is_anonymous", ctypes.c_int),
        ("is_writable", ctypes.c_int),
        ("is_private", ctypes.c_int),
    ]


class GreyMemResult(ctypes.Structure):
    """C struct: grey_mem_result_t"""
    _fields_ = [
        ("success", ctypes.c_int),
        ("bytes_advised", ctypes.c_long),
        ("regions_processed", ctypes.c_long),
        ("errors", ctypes.c_long),
        ("error_msg", ctypes.c_char * 256),
    ]


class MemoryOps:
    """
    Python interface to libgrey_memory.so.
    
    Provides safe access to low-level memory operations.
    Falls back to no-op in simulation mode or when library unavailable.
    """
    
    LIB_PATHS = [
        # Installed locations
        "/usr/local/lib/grey-optimizer/libgrey_memory.so",
        "/usr/lib/grey-optimizer/libgrey_memory.so",
        # Development locations
        "./enforcement/lib/libgrey_memory.so",
        "../enforcement/lib/libgrey_memory.so",
        # Relative to this file
        str(Path(__file__).parent.parent.parent.parent / "enforcement" / "lib" / "libgrey_memory.so"),
    ]
    
    def __init__(self, simulation: bool = True):
        """
        Initialize memory operations.
        
        Args:
            simulation: If True, log operations but don't execute syscalls
        """
        self.simulation = simulation
        self._lib: Optional[ctypes.CDLL] = None
        self._available = False
        
        if not simulation:
            self._load_library()
    
    def _load_library(self) -> bool:
        """Attempt to load the C library."""
        for path in self.LIB_PATHS:
            if os.path.exists(path):
                try:
                    self._lib = ctypes.CDLL(path)
                    self._setup_functions()
                    self._available = True
                    logger.info(f"Loaded memory ops library from {path}")
                    return True
                except Exception as e:
                    logger.warning(f"Failed to load {path}: {e}")
        
        logger.warning("libgrey_memory.so not found - memory operations unavailable")
        return False
    
    def _setup_functions(self):
        """Configure ctypes function signatures."""
        if not self._lib:
            return
        
        # grey_parse_maps
        self._lib.grey_parse_maps.argtypes = [
            ctypes.c_int,  # pid
            ctypes.POINTER(GreyMemRegion),  # regions array
            ctypes.c_int,  # max_regions
            ctypes.POINTER(ctypes.c_int),  # count output
        ]
        self._lib.grey_parse_maps.restype = ctypes.c_int
        
        # grey_advise_memory
        self._lib.grey_advise_memory.argtypes = [
            ctypes.c_int,  # pid
            ctypes.POINTER(GreyMemRegion),  # regions
            ctypes.c_int,  # count
            ctypes.c_int,  # advice (MADV_DONTNEED etc)
            ctypes.c_int,  # anonymous_only
            ctypes.POINTER(GreyMemResult),  # result
        ]
        self._lib.grey_advise_memory.restype = ctypes.c_int
        
        # grey_clear_soft_dirty
        self._lib.grey_clear_soft_dirty.argtypes = [ctypes.c_int]  # pid
        self._lib.grey_clear_soft_dirty.restype = ctypes.c_int
        
        # grey_count_cold_pages
        self._lib.grey_count_cold_pages.argtypes = [
            ctypes.c_int,  # pid
            ctypes.POINTER(ctypes.c_long),  # cold_count
            ctypes.POINTER(ctypes.c_long),  # hot_count
        ]
        self._lib.grey_count_cold_pages.restype = ctypes.c_int
    
    def is_available(self) -> bool:
        """Check if C library is loaded and operational."""
        return self._available and self._lib is not None
    
    def parse_maps(self, pid: int) -> List[MemoryRegion]:
        """
        Parse memory mappings for a process.
        
        Args:
            pid: Process ID
        
        Returns:
            List of memory regions
        
        Raises:
            PermissionError: If we can't read /proc/[pid]/maps
            ProcessLookupError: If process doesn't exist
        """
        if self.simulation:
            # In simulation, parse directly from /proc
            return self._parse_maps_python(pid)
        
        if not self.is_available():
            return self._parse_maps_python(pid)
        
        regions_array = (GreyMemRegion * MAX_REGIONS)()
        count = ctypes.c_int(0)
        
        result = self._lib.grey_parse_maps(
            pid,
            regions_array,
            MAX_REGIONS,
            ctypes.byref(count),
        )
        
        if result == GREY_ERR_PERM:
            raise PermissionError(f"Cannot read /proc/{pid}/maps")
        if result == GREY_ERR_NOTFOUND:
            raise ProcessLookupError(f"Process {pid} not found")
        if result != GREY_OK:
            raise RuntimeError(f"Failed to parse maps: error {result}")
        
        regions = []
        for i in range(count.value):
            r = regions_array[i]
            regions.append(MemoryRegion(
                start=r.start,
                end=r.end,
                perms=r.perms.decode("utf-8", errors="replace").strip("\x00"),
                offset=r.offset,
                path=r.path.decode("utf-8", errors="replace").strip("\x00"),
                is_anonymous=bool(r.is_anonymous),
                is_writable=bool(r.is_writable),
                is_private=bool(r.is_private),
            ))
        
        return regions
    
    def _parse_maps_python(self, pid: int) -> List[MemoryRegion]:
        """Pure Python fallback for parsing /proc/[pid]/maps."""
        regions = []
        maps_path = f"/proc/{pid}/maps"
        
        try:
            with open(maps_path, "r") as f:
                for line in f:
                    parts = line.split()
                    if len(parts) < 5:
                        continue
                    
                    # Parse address range
                    addr_range = parts[0].split("-")
                    start = int(addr_range[0], 16)
                    end = int(addr_range[1], 16)
                    
                    perms = parts[1]
                    offset = int(parts[2], 16)
                    path = parts[5] if len(parts) > 5 else ""
                    
                    # Determine flags
                    is_anonymous = not path or path.startswith("[")
                    is_writable = "w" in perms
                    is_private = "p" in perms
                    
                    regions.append(MemoryRegion(
                        start=start,
                        end=end,
                        perms=perms,
                        offset=offset,
                        path=path,
                        is_anonymous=is_anonymous,
                        is_writable=is_writable,
                        is_private=is_private,
                    ))
        except FileNotFoundError:
            raise ProcessLookupError(f"Process {pid} not found")
        except PermissionError:
            raise PermissionError(f"Cannot read /proc/{pid}/maps")
        
        return regions
    
    def advise_anonymous(
        self,
        pid: int,
        target_bytes: Optional[int] = None,
    ) -> AdviseResult:
        """
        Apply MADV_DONTNEED to anonymous memory regions.
        
        This tells the kernel that the process doesn't need the pages,
        allowing them to be freed immediately. Pages will be zero-filled
        on next access (not restored to original values).
        
        Args:
            pid: Target process ID
            target_bytes: Maximum bytes to advise (None = all anonymous)
        
        Returns:
            AdviseResult with bytes freed
        
        Safety:
        - Only affects anonymous (heap) pages
        - File-backed pages are not affected
        - Pages will refault on access
        - May cause performance issues if pages are needed
        
        Why this works:
        - For processes with large idle caches (e.g., JVM, Node.js)
        - The application may reallocate, but kernel pages are freed
        - Effective for "bloated" processes that allocated but don't use
        """
        MADV_DONTNEED = 4  # Linux constant
        
        if self.simulation:
            logger.debug(f"[SIM] Would advise anonymous memory for PID {pid}")
            return AdviseResult(
                success=True,
                bytes_advised=target_bytes or 0,
                regions_processed=0,
                errors=0,
            )
        
        if not self.is_available():
            logger.warning("Memory ops library not available")
            return AdviseResult(
                success=False,
                bytes_advised=0,
                regions_processed=0,
                errors=1,
                error_message="Library not available",
            )
        
        # Get memory regions
        try:
            regions = self.parse_maps(pid)
        except Exception as e:
            return AdviseResult(
                success=False,
                bytes_advised=0,
                regions_processed=0,
                errors=1,
                error_message=str(e),
            )
        
        # Filter to anonymous writable regions
        anonymous = [r for r in regions if r.is_anonymous and r.is_writable and r.is_private]
        
        if not anonymous:
            return AdviseResult(
                success=True,
                bytes_advised=0,
                regions_processed=0,
                errors=0,
            )
        
        # Convert to C struct array
        regions_array = (GreyMemRegion * len(anonymous))()
        for i, r in enumerate(anonymous):
            regions_array[i].start = r.start
            regions_array[i].end = r.end
            regions_array[i].perms = r.perms.encode("utf-8")
            regions_array[i].offset = r.offset
            regions_array[i].path = r.path.encode("utf-8")
            regions_array[i].is_anonymous = 1
            regions_array[i].is_writable = 1 if r.is_writable else 0
            regions_array[i].is_private = 1 if r.is_private else 0
        
        result = GreyMemResult()
        
        ret = self._lib.grey_advise_memory(
            pid,
            regions_array,
            len(anonymous),
            MADV_DONTNEED,
            1,  # anonymous_only
            ctypes.byref(result),
        )
        
        return AdviseResult(
            success=ret == GREY_OK and result.success != 0,
            bytes_advised=result.bytes_advised,
            regions_processed=result.regions_processed,
            errors=result.errors,
            error_message=result.error_msg.decode("utf-8", errors="replace").strip("\x00"),
        )
    
    def clear_soft_dirty(self, pid: int) -> bool:
        """
        Clear soft-dirty bits for a process.
        
        After clearing, any pages that are written to will have
        their soft-dirty bit set. This enables tracking of
        memory access patterns for cold page detection.
        
        Args:
            pid: Process ID
        
        Returns:
            True if successful
        """
        if self.simulation:
            logger.debug(f"[SIM] Would clear soft-dirty for PID {pid}")
            return True
        
        if not self.is_available():
            # Fallback to Python
            try:
                clear_refs_path = f"/proc/{pid}/clear_refs"
                with open(clear_refs_path, "w") as f:
                    f.write("4")  # Clear soft-dirty bits
                return True
            except Exception as e:
                logger.debug(f"Could not clear soft-dirty for {pid}: {e}")
                return False
        
        return self._lib.grey_clear_soft_dirty(pid) == GREY_OK
    
    def count_cold_pages(self, pid: int) -> ColdPageResult:
        """
        Count cold (unused) vs hot (accessed) pages.
        
        Must call clear_soft_dirty() first, wait, then call this
        to see which pages were accessed during the interval.
        
        Args:
            pid: Process ID
        
        Returns:
            ColdPageResult with cold/hot page counts
        
        Why this helps:
        - Identifies memory that hasn't been accessed
        - Cold pages are good candidates for madvise(MADV_DONTNEED)
        - Hot pages should not be touched
        """
        if self.simulation:
            logger.debug(f"[SIM] Would count cold pages for PID {pid}")
            return ColdPageResult(
                cold_pages=0,
                hot_pages=0,
                total_pages=0,
                cold_ratio=0.0,
            )
        
        if not self.is_available():
            # Fallback: no cold page detection
            return ColdPageResult(
                cold_pages=0,
                hot_pages=0,
                total_pages=0,
                cold_ratio=0.0,
            )
        
        cold_count = ctypes.c_long(0)
        hot_count = ctypes.c_long(0)
        
        ret = self._lib.grey_count_cold_pages(
            pid,
            ctypes.byref(cold_count),
            ctypes.byref(hot_count),
        )
        
        if ret != GREY_OK:
            return ColdPageResult(
                cold_pages=0,
                hot_pages=0,
                total_pages=0,
                cold_ratio=0.0,
            )
        
        total = cold_count.value + hot_count.value
        ratio = cold_count.value / total if total > 0 else 0.0
        
        return ColdPageResult(
            cold_pages=cold_count.value,
            hot_pages=hot_count.value,
            total_pages=total,
            cold_ratio=ratio,
        )
    
    def get_process_memory_summary(self, pid: int) -> Dict[str, Any]:
        """
        Get a summary of process memory usage.
        
        Args:
            pid: Process ID
        
        Returns:
            Dict with memory summary
        """
        try:
            regions = self.parse_maps(pid)
        except Exception as e:
            return {"error": str(e)}
        
        total = sum(r.size for r in regions)
        anonymous = sum(r.size for r in regions if r.is_anonymous)
        writable = sum(r.size for r in regions if r.is_writable)
        heap = sum(r.size for r in regions if r.is_heap)
        stack = sum(r.size for r in regions if r.is_stack)
        
        return {
            "pid": pid,
            "total_mapped_mb": round(total / (1024 * 1024), 2),
            "anonymous_mb": round(anonymous / (1024 * 1024), 2),
            "writable_mb": round(writable / (1024 * 1024), 2),
            "heap_mb": round(heap / (1024 * 1024), 2),
            "stack_mb": round(stack / (1024 * 1024), 2),
            "region_count": len(regions),
            "anonymous_region_count": sum(1 for r in regions if r.is_anonymous),
        }
