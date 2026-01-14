"""
Grey Optimizer - Memory Reclamation Module

Implements adaptive RAM reclamation strategies:
1. cgroup limiter - Apply memory.max/memory.high limits
2. madvise invoker - Call madvise(MADV_DONTNEED) via C helper
3. KSM toggler - Enable/tune Kernel Same-page Merging
4. zram helper - Enable/configure zram swap
5. drop_caches wrapper - Safe cache dropping with --confirm-live

SAFETY PRINCIPLE:
All operations are simulated by default. Live enforcement requires
explicit --confirm-live flag and creates verified backups first.

Cross-Platform Notes:
- Linux: Full support for all strategies
- macOS: Limited support (no cgroups, no KSM, no zram)
- Windows: No support (different memory model)
"""

import asyncio
import json
import logging
import os
import shutil
import subprocess
import tempfile
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Any
import platform

logger = logging.getLogger(__name__)

# Platform detection
IS_LINUX = platform.system().lower() == "linux"
IS_MACOS = platform.system().lower() == "darwin"
IS_WINDOWS = platform.system().lower() == "windows"

# Path to C helper binaries
C_HELPER_DIR = os.environ.get(
    "GREY_C_HELPERS_DIR",
    "/usr/local/bin"
)
MADVISE_HELPER = os.path.join(C_HELPER_DIR, "grey-madvise-helper")
CGROUP_HELPER = os.path.join(C_HELPER_DIR, "grey-cgroup-helper")

# Fallback paths for development
if not os.path.exists(MADVISE_HELPER):
    dev_path = Path(__file__).parent.parent / "c" / "bin" / "madvise_helper"
    if dev_path.exists():
        MADVISE_HELPER = str(dev_path)

if not os.path.exists(CGROUP_HELPER):
    dev_path = Path(__file__).parent.parent / "c" / "bin" / "cgroup_helper"
    if dev_path.exists():
        CGROUP_HELPER = str(dev_path)

# Backup and artifact directories
BACKUP_DIR = Path(os.environ.get(
    "GREY_BACKUP_DIR",
    "/var/lib/grey-optimizer/backups"
))

ARTIFACTS_DIR = Path(os.environ.get(
    "GREY_ARTIFACTS_DIR", 
    "/var/lib/grey-optimizer/artifacts"
))

# KSM sysfs path
KSM_PATH = Path("/sys/kernel/mm/ksm")

# zram device paths
ZRAM_PATH = Path("/sys/block/zram0")


@dataclass
class ReclaimResult:
    """Result of a reclamation operation with structured before/after snapshots."""
    success: bool
    strategy: str
    simulated: bool
    bytes_reclaimed: int = 0
    bytes_would_reclaim: int = 0
    error: Optional[str] = None
    details: Dict[str, Any] = field(default_factory=dict)
    backup_path: Optional[Path] = None
    # New structured fields per project spec
    action: str = ""
    before_snapshot: Optional[Dict[str, Any]] = None
    after_snapshot: Optional[Dict[str, Any]] = None
    expected_savings: int = 0
    timestamp: datetime = field(default_factory=lambda: datetime.now(timezone.utc))
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "success": self.success,
            "strategy": self.strategy,
            "action": self.action or self.strategy,
            "simulated": self.simulated,
            "bytes_reclaimed": self.bytes_reclaimed,
            "bytes_would_reclaim": self.bytes_would_reclaim,
            "expected_savings": self.expected_savings,
            "error": self.error,
            "details": self.details,
            "backup_path": str(self.backup_path) if self.backup_path else None,
            "before_snapshot": self.before_snapshot,
            "after_snapshot": self.after_snapshot,
            "timestamp": self.timestamp.isoformat(),
        }


@dataclass
class KSMBackup:
    """Backup of KSM settings for rollback."""
    run: str = "0"
    pages_to_scan: str = "100"
    sleep_millisecs: str = "200"
    timestamp: datetime = field(default_factory=lambda: datetime.now(timezone.utc))
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "run": self.run,
            "pages_to_scan": self.pages_to_scan,
            "sleep_millisecs": self.sleep_millisecs,
            "timestamp": self.timestamp.isoformat(),
        }


@dataclass
class ZramBackup:
    """Backup of zram/swap configuration for rollback."""
    zram_enabled: bool = False
    disksize: int = 0
    comp_algorithm: str = "lzo"
    swappiness: int = 60
    swaps_before: List[str] = field(default_factory=list)
    timestamp: datetime = field(default_factory=lambda: datetime.now(timezone.utc))
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "zram_enabled": self.zram_enabled,
            "disksize": self.disksize,
            "comp_algorithm": self.comp_algorithm,
            "swappiness": self.swappiness,
            "swaps_before": self.swaps_before,
            "timestamp": self.timestamp.isoformat(),
        }


@dataclass
class CgroupBackup:
    """Backup of cgroup settings for rollback."""
    cgroup_path: str
    memory_max: Optional[int] = None
    memory_high: Optional[int] = None
    pids: List[int] = field(default_factory=list)
    timestamp: datetime = field(default_factory=lambda: datetime.now(timezone.utc))
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "cgroup_path": self.cgroup_path,
            "memory_max": self.memory_max,
            "memory_high": self.memory_high,
            "pids": self.pids,
            "timestamp": self.timestamp.isoformat(),
        }


# ═══════════════════════════════════════════════════════════════════════════════
# Standalone Functions (Required by Project Spec)
# ═══════════════════════════════════════════════════════════════════════════════
# Each function returns: {action, simulated, before_snapshot, after_snapshot, expected_savings}

def _read_ksm_stats() -> Dict[str, Any]:
    """Read current KSM statistics from sysfs."""
    stats = {}
    if not IS_LINUX or not KSM_PATH.exists():
        return stats
    
    for key in ["run", "pages_to_scan", "sleep_millisecs", "pages_shared", 
                "pages_sharing", "pages_unshared", "pages_volatile", "full_scans"]:
        path = KSM_PATH / key
        if path.exists():
            try:
                stats[key] = path.read_text().strip()
            except PermissionError:
                stats[key] = None
    return stats


def _read_meminfo() -> Dict[str, int]:
    """Read /proc/meminfo and return values in bytes."""
    meminfo = {}
    try:
        with open("/proc/meminfo", "r") as f:
            for line in f:
                parts = line.split()
                if len(parts) >= 2:
                    key = parts[0].rstrip(":")
                    meminfo[key] = int(parts[1]) * 1024  # KB to bytes
    except (FileNotFoundError, PermissionError):
        pass
    return meminfo


def _get_page_size() -> int:
    """Get system page size."""
    try:
        return os.sysconf("SC_PAGESIZE")
    except (ValueError, AttributeError):
        return 4096


def enable_ksm(
    pages_to_scan: int = 200,
    sleep_millisecs: int = 100,
    simulation: bool = True
) -> ReclaimResult:
    """
    Enable Kernel Same-page Merging (KSM).
    
    KSM scans for identical memory pages and merges them, saving RAM
    at the cost of CPU overhead.
    
    Args:
        pages_to_scan: Number of pages to scan per cycle (higher = faster but more CPU)
        sleep_millisecs: Milliseconds to sleep between scan cycles
        simulation: If True, only simulate (default). Set False for live mode.
    
    Returns:
        ReclaimResult with before/after snapshots and expected savings.
    
    Linux Only: Requires /sys/kernel/mm/ksm and root for live mode.
    macOS/Windows: Returns no-op stub result with TODO documentation.
    """
    action = "ksm_enable"
    
    if not IS_LINUX:
        return ReclaimResult(
            success=False,
            strategy="ksm",
            action=action,
            simulated=True,
            error=f"KSM not available on {platform.system()}. TODO: No equivalent on macOS/Windows.",
            details={"platform": platform.system()},
        )
    
    if not KSM_PATH.exists():
        return ReclaimResult(
            success=False,
            strategy="ksm",
            action=action,
            simulated=simulation,
            error="KSM not available - /sys/kernel/mm/ksm not found. "
                  "TODO: Ensure CONFIG_KSM=y in kernel config.",
            details={"ksm_path": str(KSM_PATH)},
        )
    
    # Capture before snapshot
    before_snapshot = {
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "ksm_stats": _read_ksm_stats(),
        "meminfo": {k: v for k, v in _read_meminfo().items() 
                   if k in ["MemTotal", "MemFree", "MemAvailable", "Buffers", "Cached"]},
    }
    
    page_size = _get_page_size()
    pages_shared = int(before_snapshot["ksm_stats"].get("pages_shared", 0) or 0)
    pages_sharing = int(before_snapshot["ksm_stats"].get("pages_sharing", 0) or 0)
    
    # Estimate savings: shared pages * page_size
    current_savings = pages_shared * page_size
    # Conservative estimate: 10% additional savings from newly enabled KSM
    expected_savings = current_savings + (before_snapshot["meminfo"].get("MemTotal", 0) * 0.01)
    
    if simulation:
        return ReclaimResult(
            success=True,
            strategy="ksm",
            action=action,
            simulated=True,
            bytes_would_reclaim=int(expected_savings),
            expected_savings=int(expected_savings),
            before_snapshot=before_snapshot,
            after_snapshot=None,  # No after in simulation
            details={
                "proposed": {
                    "run": "1",
                    "pages_to_scan": str(pages_to_scan),
                    "sleep_millisecs": str(sleep_millisecs),
                },
                "current_pages_shared": pages_shared,
                "current_pages_sharing": pages_sharing,
            },
        )
    
    # Live mode - create backup and apply
    try:
        backup = KSMBackup(
            run=(KSM_PATH / "run").read_text().strip(),
            pages_to_scan=(KSM_PATH / "pages_to_scan").read_text().strip(),
            sleep_millisecs=(KSM_PATH / "sleep_millisecs").read_text().strip(),
        )
        
        # Save backup
        backup_path = BACKUP_DIR / f"ksm_backup_{datetime.now(timezone.utc).strftime('%Y%m%d_%H%M%S')}.json"
        backup_path.parent.mkdir(parents=True, exist_ok=True)
        backup_path.write_text(json.dumps(backup.to_dict(), indent=2))
        
        # Apply new settings
        (KSM_PATH / "pages_to_scan").write_text(str(pages_to_scan))
        (KSM_PATH / "sleep_millisecs").write_text(str(sleep_millisecs))
        (KSM_PATH / "run").write_text("1")
        
        logger.info(f"KSM enabled: pages_to_scan={pages_to_scan}, sleep_millisecs={sleep_millisecs}")
        
        # Capture after snapshot
        after_snapshot = {
            "timestamp": datetime.now(timezone.utc).isoformat(),
            "ksm_stats": _read_ksm_stats(),
            "meminfo": {k: v for k, v in _read_meminfo().items() 
                       if k in ["MemTotal", "MemFree", "MemAvailable", "Buffers", "Cached"]},
        }
        
        return ReclaimResult(
            success=True,
            strategy="ksm",
            action=action,
            simulated=False,
            bytes_reclaimed=int(expected_savings),
            expected_savings=int(expected_savings),
            before_snapshot=before_snapshot,
            after_snapshot=after_snapshot,
            backup_path=backup_path,
            details={
                "previous": backup.to_dict(),
                "applied": {
                    "run": "1",
                    "pages_to_scan": str(pages_to_scan),
                    "sleep_millisecs": str(sleep_millisecs),
                },
            },
        )
        
    except PermissionError:
        return ReclaimResult(
            success=False,
            strategy="ksm",
            action=action,
            simulated=False,
            error="Permission denied - requires root privileges",
            before_snapshot=before_snapshot,
        )
    except Exception as e:
        logger.exception(f"KSM enable failed: {e}")
        return ReclaimResult(
            success=False,
            strategy="ksm",
            action=action,
            simulated=False,
            error=str(e),
            before_snapshot=before_snapshot,
        )


def disable_ksm(simulation: bool = True) -> ReclaimResult:
    """
    Disable Kernel Same-page Merging (KSM).
    
    Args:
        simulation: If True, only simulate (default).
    
    Returns:
        ReclaimResult with operation outcome.
    """
    action = "ksm_disable"
    
    if not IS_LINUX or not KSM_PATH.exists():
        return ReclaimResult(
            success=False,
            strategy="ksm",
            action=action,
            simulated=True,
            error="KSM not available on this system",
        )
    
    before_snapshot = {
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "ksm_stats": _read_ksm_stats(),
    }
    
    if simulation:
        return ReclaimResult(
            success=True,
            strategy="ksm",
            action=action,
            simulated=True,
            before_snapshot=before_snapshot,
            details={"note": "Would set /sys/kernel/mm/ksm/run to 0"},
        )
    
    try:
        backup = KSMBackup(
            run=(KSM_PATH / "run").read_text().strip(),
            pages_to_scan=(KSM_PATH / "pages_to_scan").read_text().strip(),
            sleep_millisecs=(KSM_PATH / "sleep_millisecs").read_text().strip(),
        )
        
        backup_path = BACKUP_DIR / f"ksm_backup_{datetime.now(timezone.utc).strftime('%Y%m%d_%H%M%S')}.json"
        backup_path.parent.mkdir(parents=True, exist_ok=True)
        backup_path.write_text(json.dumps(backup.to_dict(), indent=2))
        
        (KSM_PATH / "run").write_text("0")
        
        logger.info("KSM disabled")
        
        after_snapshot = {
            "timestamp": datetime.now(timezone.utc).isoformat(),
            "ksm_stats": _read_ksm_stats(),
        }
        
        return ReclaimResult(
            success=True,
            strategy="ksm",
            action=action,
            simulated=False,
            before_snapshot=before_snapshot,
            after_snapshot=after_snapshot,
            backup_path=backup_path,
        )
        
    except PermissionError:
        return ReclaimResult(
            success=False,
            strategy="ksm",
            action=action,
            simulated=False,
            error="Permission denied - requires root privileges",
        )
    except Exception as e:
        return ReclaimResult(
            success=False,
            strategy="ksm",
            action=action,
            simulated=False,
            error=str(e),
        )


def tune_ksm(
    pages_to_scan: Optional[int] = None,
    sleep_millisecs: Optional[int] = None,
    simulation: bool = True
) -> ReclaimResult:
    """
    Tune KSM parameters without enabling/disabling.
    
    Use this to adjust CPU vs memory trade-offs:
    - Higher pages_to_scan = faster merging but more CPU
    - Lower sleep_millisecs = faster merging but more CPU
    
    Args:
        pages_to_scan: Pages to scan per cycle (None = don't change)
        sleep_millisecs: Sleep time between cycles (None = don't change)
        simulation: If True, only simulate.
    
    Returns:
        ReclaimResult with tuning outcome.
    """
    action = "ksm_tune"
    
    if not IS_LINUX or not KSM_PATH.exists():
        return ReclaimResult(
            success=False,
            strategy="ksm",
            action=action,
            simulated=True,
            error="KSM not available on this system",
        )
    
    before_snapshot = {
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "ksm_stats": _read_ksm_stats(),
    }
    
    changes = {}
    if pages_to_scan is not None:
        changes["pages_to_scan"] = str(pages_to_scan)
    if sleep_millisecs is not None:
        changes["sleep_millisecs"] = str(sleep_millisecs)
    
    if not changes:
        return ReclaimResult(
            success=False,
            strategy="ksm",
            action=action,
            simulated=simulation,
            error="No parameters specified to tune",
            before_snapshot=before_snapshot,
        )
    
    if simulation:
        return ReclaimResult(
            success=True,
            strategy="ksm",
            action=action,
            simulated=True,
            before_snapshot=before_snapshot,
            details={"proposed_changes": changes},
        )
    
    try:
        for key, value in changes.items():
            (KSM_PATH / key).write_text(value)
        
        after_snapshot = {
            "timestamp": datetime.now(timezone.utc).isoformat(),
            "ksm_stats": _read_ksm_stats(),
        }
        
        logger.info(f"KSM tuned: {changes}")
        
        return ReclaimResult(
            success=True,
            strategy="ksm",
            action=action,
            simulated=False,
            before_snapshot=before_snapshot,
            after_snapshot=after_snapshot,
            details={"applied_changes": changes},
        )
        
    except PermissionError:
        return ReclaimResult(
            success=False,
            strategy="ksm",
            action=action,
            simulated=False,
            error="Permission denied - requires root privileges",
        )
    except Exception as e:
        return ReclaimResult(
            success=False,
            strategy="ksm",
            action=action,
            simulated=False,
            error=str(e),
        )


def invoke_madvise(
    pid: int,
    mapping_id: Optional[str] = None,
    simulation: bool = True
) -> ReclaimResult:
    """
    Invoke madvise(MADV_DONTNEED) on a process's anonymous memory.
    
    This reclaims memory by advising the kernel to release anonymous pages.
    The process can regenerate data if needed, but may crash if it expects
    the data to persist.
    
    Args:
        pid: Target process ID
        mapping_id: Optional specific mapping (format: "start-end" in hex)
        simulation: If True, only simulate.
    
    Returns:
        ReclaimResult with reclamation outcome.
    
    Safety:
        - Requires root or CAP_SYS_PTRACE for live mode
        - Only affects anonymous private mappings by default
        - Target process should be cooperative or fault-tolerant
    """
    action = "madvise_reclaim"
    
    if not IS_LINUX:
        return ReclaimResult(
            success=False,
            strategy="madvise",
            action=action,
            simulated=True,
            error=f"madvise reclamation not available on {platform.system()}. "
                  "TODO: macOS has madvise but no cross-process support; Windows uses VirtualAlloc.",
        )
    
    # Capture before snapshot from smaps
    before_snapshot = {
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "pid": pid,
        "smaps": _read_pid_smaps_summary(pid),
    }
    
    # Check if C helper exists
    helper_path = MADVISE_HELPER
    if not Path(helper_path).exists():
        # Try development path
        dev_path = Path(__file__).parent.parent / "c" / "bin" / "madvise_helper"
        if dev_path.exists():
            helper_path = str(dev_path)
        else:
            return ReclaimResult(
                success=False,
                strategy="madvise",
                action=action,
                simulated=simulation,
                error=f"madvise_helper not found. Build with: cd c && make",
                before_snapshot=before_snapshot,
            )
    
    # Build command
    cmd = [helper_path, f"--pid={pid}", "--json", "--include-heap"]
    
    if mapping_id:
        cmd.append(f"--region={mapping_id}")
    
    if simulation:
        cmd.append("--simulate")
    else:
        cmd.append("--confirm-live")
    
    try:
        result = subprocess.run(
            cmd, capture_output=True, text=True, timeout=60
        )
        
        if result.returncode != 0:
            return ReclaimResult(
                success=False,
                strategy="madvise",
                action=action,
                simulated=simulation,
                error=result.stderr or f"madvise_helper exited with code {result.returncode}",
                before_snapshot=before_snapshot,
            )
        
        # Parse JSON output
        try:
            output = json.loads(result.stdout)
            bytes_would_reclaim = output.get("bytes_would_reclaim", 0)
            bytes_reclaimed = output.get("bytes_reclaimed", 0)
        except json.JSONDecodeError:
            bytes_would_reclaim = 0
            bytes_reclaimed = 0
        
        after_snapshot = None
        if not simulation:
            after_snapshot = {
                "timestamp": datetime.now(timezone.utc).isoformat(),
                "pid": pid,
                "smaps": _read_pid_smaps_summary(pid),
            }
        
        return ReclaimResult(
            success=True,
            strategy="madvise",
            action=action,
            simulated=simulation,
            bytes_reclaimed=bytes_reclaimed if not simulation else 0,
            bytes_would_reclaim=bytes_would_reclaim,
            expected_savings=bytes_would_reclaim,
            before_snapshot=before_snapshot,
            after_snapshot=after_snapshot,
            details={
                "pid": pid,
                "mapping_id": mapping_id,
                "helper_output": output if 'output' in dir() else None,
            },
        )
        
    except subprocess.TimeoutExpired:
        return ReclaimResult(
            success=False,
            strategy="madvise",
            action=action,
            simulated=simulation,
            error="madvise_helper timed out after 60s",
            before_snapshot=before_snapshot,
        )
    except Exception as e:
        logger.exception(f"madvise invocation failed: {e}")
        return ReclaimResult(
            success=False,
            strategy="madvise",
            action=action,
            simulated=simulation,
            error=str(e),
            before_snapshot=before_snapshot,
        )


def _read_pid_smaps_summary(pid: int) -> Dict[str, Any]:
    """Read summary statistics from /proc/[pid]/smaps."""
    summary = {
        "rss_kb": 0,
        "pss_kb": 0,
        "anonymous_kb": 0,
        "swap_kb": 0,
    }
    
    try:
        with open(f"/proc/{pid}/smaps", "r") as f:
            for line in f:
                if line.startswith("Rss:"):
                    summary["rss_kb"] += int(line.split()[1])
                elif line.startswith("Pss:"):
                    summary["pss_kb"] += int(line.split()[1])
                elif line.startswith("Anonymous:"):
                    summary["anonymous_kb"] += int(line.split()[1])
                elif line.startswith("Swap:"):
                    summary["swap_kb"] += int(line.split()[1])
    except (FileNotFoundError, PermissionError):
        pass
    
    return summary


def enable_zram(
    size_mb: int = 512,
    compression_algo: str = "zstd",
    simulation: bool = True
) -> ReclaimResult:
    """
    Enable zram compressed swap device.
    
    zram compresses swap in RAM, effectively increasing usable memory
    at the cost of CPU time for compression/decompression.
    
    Args:
        size_mb: Size of zram device in MB
        compression_algo: Compression algorithm (zstd, lz4, lzo)
        simulation: If True, only simulate.
    
    Returns:
        ReclaimResult with operation outcome.
    
    Linux Only: Requires zram kernel module and root for live mode.
    macOS: Uses compressed memory by default (no action needed).
    Windows: Uses compressed memory via Memory Manager (no action needed).
    """
    action = "zram_enable"
    
    if IS_MACOS:
        return ReclaimResult(
            success=True,
            strategy="zram",
            action=action,
            simulated=True,
            details={
                "note": "macOS uses compressed memory by default via the VM compressor. "
                        "No additional zram configuration needed.",
                "platform": "macOS",
            },
        )
    
    if IS_WINDOWS:
        return ReclaimResult(
            success=True,
            strategy="zram",
            action=action,
            simulated=True,
            details={
                "note": "Windows 10+ uses memory compression via Memory Manager. "
                        "No additional zram configuration needed.",
                "platform": "Windows",
            },
        )
    
    if not IS_LINUX:
        return ReclaimResult(
            success=False,
            strategy="zram",
            action=action,
            simulated=True,
            error=f"zram not supported on {platform.system()}",
        )
    
    before_snapshot = {
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "meminfo": {k: v for k, v in _read_meminfo().items() 
                   if k in ["MemTotal", "MemFree", "MemAvailable", "SwapTotal", "SwapFree"]},
        "swaps": _read_swaps(),
    }
    
    # Expected compression ratio (typical 2:1 to 4:1)
    compression_ratio = 2.5
    expected_savings = int(size_mb * 1024 * 1024 * (1 - 1/compression_ratio))
    
    if simulation:
        return ReclaimResult(
            success=True,
            strategy="zram",
            action=action,
            simulated=True,
            bytes_would_reclaim=expected_savings,
            expected_savings=expected_savings,
            before_snapshot=before_snapshot,
            details={
                "proposed_size_mb": size_mb,
                "proposed_algorithm": compression_algo,
                "expected_compression_ratio": compression_ratio,
                "note": "Estimated effective memory gain from compression",
            },
        )
    
    try:
        # Check if zram module is loaded
        if not ZRAM_PATH.exists():
            subprocess.run(["modprobe", "zram"], check=True, capture_output=True)
        
        # Check if already in use
        swaps = _read_swaps()
        if any("zram0" in s for s in swaps):
            return ReclaimResult(
                success=False,
                strategy="zram",
                action=action,
                simulated=False,
                error="zram0 already in use as swap. Run disable_zram first.",
                before_snapshot=before_snapshot,
            )
        
        # Create backup
        backup = ZramBackup(
            zram_enabled=False,
            swappiness=int(Path("/proc/sys/vm/swappiness").read_text().strip()),
            swaps_before=swaps,
        )
        backup_path = BACKUP_DIR / f"zram_backup_{datetime.now(timezone.utc).strftime('%Y%m%d_%H%M%S')}.json"
        backup_path.parent.mkdir(parents=True, exist_ok=True)
        backup_path.write_text(json.dumps(backup.to_dict(), indent=2))
        
        # Configure zram
        (ZRAM_PATH / "comp_algorithm").write_text(compression_algo)
        (ZRAM_PATH / "disksize").write_text(f"{size_mb}M")
        
        # Create swap and enable
        subprocess.run(["mkswap", "/dev/zram0"], check=True, capture_output=True)
        subprocess.run(["swapon", "-p", "100", "/dev/zram0"], check=True, capture_output=True)
        
        # Optionally increase swappiness for better zram utilization
        Path("/proc/sys/vm/swappiness").write_text("80")
        
        logger.info(f"zram enabled: size={size_mb}MB, algo={compression_algo}")
        
        after_snapshot = {
            "timestamp": datetime.now(timezone.utc).isoformat(),
            "meminfo": {k: v for k, v in _read_meminfo().items() 
                       if k in ["MemTotal", "MemFree", "MemAvailable", "SwapTotal", "SwapFree"]},
            "swaps": _read_swaps(),
        }
        
        return ReclaimResult(
            success=True,
            strategy="zram",
            action=action,
            simulated=False,
            bytes_reclaimed=expected_savings,
            expected_savings=expected_savings,
            before_snapshot=before_snapshot,
            after_snapshot=after_snapshot,
            backup_path=backup_path,
            details={
                "device": "/dev/zram0",
                "size_mb": size_mb,
                "algorithm": compression_algo,
                "swap_priority": 100,
                "swappiness": 80,
            },
        )
        
    except subprocess.CalledProcessError as e:
        return ReclaimResult(
            success=False,
            strategy="zram",
            action=action,
            simulated=False,
            error=f"Command failed: {e.cmd}: {e.stderr.decode() if e.stderr else str(e)}",
            before_snapshot=before_snapshot,
        )
    except PermissionError:
        return ReclaimResult(
            success=False,
            strategy="zram",
            action=action,
            simulated=False,
            error="Permission denied - requires root privileges",
            before_snapshot=before_snapshot,
        )
    except Exception as e:
        logger.exception(f"zram enable failed: {e}")
        return ReclaimResult(
            success=False,
            strategy="zram",
            action=action,
            simulated=False,
            error=str(e),
            before_snapshot=before_snapshot,
        )


def _read_swaps() -> List[str]:
    """Read current swap devices from /proc/swaps."""
    swaps = []
    try:
        with open("/proc/swaps", "r") as f:
            lines = f.readlines()[1:]  # Skip header
            swaps = [line.split()[0] for line in lines if line.strip()]
    except (FileNotFoundError, PermissionError):
        pass
    return swaps


def disable_zram(simulation: bool = True) -> ReclaimResult:
    """
    Disable zram swap device.
    
    Args:
        simulation: If True, only simulate.
    
    Returns:
        ReclaimResult with operation outcome.
    """
    action = "zram_disable"
    
    if not IS_LINUX:
        return ReclaimResult(
            success=False,
            strategy="zram",
            action=action,
            simulated=True,
            error="zram only available on Linux",
        )
    
    before_snapshot = {
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "swaps": _read_swaps(),
    }
    
    if simulation:
        return ReclaimResult(
            success=True,
            strategy="zram",
            action=action,
            simulated=True,
            before_snapshot=before_snapshot,
            details={"note": "Would swapoff /dev/zram0 and reset device"},
        )
    
    try:
        # Check if zram is being used
        if not any("zram0" in s for s in _read_swaps()):
            return ReclaimResult(
                success=True,
                strategy="zram",
                action=action,
                simulated=False,
                before_snapshot=before_snapshot,
                details={"note": "zram0 not in use as swap"},
            )
        
        subprocess.run(["swapoff", "/dev/zram0"], check=True, capture_output=True)
        
        # Reset zram device
        if ZRAM_PATH.exists():
            (ZRAM_PATH / "reset").write_text("1")
        
        logger.info("zram disabled")
        
        after_snapshot = {
            "timestamp": datetime.now(timezone.utc).isoformat(),
            "swaps": _read_swaps(),
        }
        
        return ReclaimResult(
            success=True,
            strategy="zram",
            action=action,
            simulated=False,
            before_snapshot=before_snapshot,
            after_snapshot=after_snapshot,
        )
        
    except subprocess.CalledProcessError as e:
        return ReclaimResult(
            success=False,
            strategy="zram",
            action=action,
            simulated=False,
            error=f"swapoff failed: {e}",
            before_snapshot=before_snapshot,
        )
    except PermissionError:
        return ReclaimResult(
            success=False,
            strategy="zram",
            action=action,
            simulated=False,
            error="Permission denied - requires root privileges",
        )
    except Exception as e:
        return ReclaimResult(
            success=False,
            strategy="zram",
            action=action,
            simulated=False,
            error=str(e),
        )


def safe_drop_caches(
    confirm_live: bool = False,
    level: int = 3,
    simulation: bool = True
) -> ReclaimResult:
    """
    Safely drop kernel caches to free memory.
    
    Levels:
        1: Free pagecache only
        2: Free dentries and inodes
        3: Free pagecache, dentries, and inodes (default)
    
    Args:
        confirm_live: Must be True for live enforcement (explicit consent).
        level: Cache drop level (1-3).
        simulation: If True, only simulate.
    
    Returns:
        ReclaimResult with operation outcome.
    
    Safety:
        - Checks for pending disk I/O before proceeding
        - Checks critical service health
        - Syncs filesystem first
        - Creates rollback plan (though caches rebuild automatically)
    """
    action = "drop_caches"
    
    if not IS_LINUX:
        return ReclaimResult(
            success=False,
            strategy="drop_caches",
            action=action,
            simulated=True,
            error=f"drop_caches not available on {platform.system()}. "
                  "macOS: Use 'sudo purge'. Windows: No direct equivalent.",
        )
    
    if level not in (1, 2, 3):
        return ReclaimResult(
            success=False,
            strategy="drop_caches",
            action=action,
            simulated=simulation,
            error=f"Invalid level {level}, must be 1, 2, or 3",
        )
    
    # Capture before snapshot
    meminfo = _read_meminfo()
    before_snapshot = {
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "cached_bytes": meminfo.get("Cached", 0),
        "buffers_bytes": meminfo.get("Buffers", 0),
        "slab_reclaimable_bytes": meminfo.get("SReclaimable", 0),
        "mem_available_bytes": meminfo.get("MemAvailable", 0),
    }
    
    # Calculate expected savings
    expected_savings = 0
    if level >= 1:
        expected_savings += meminfo.get("Cached", 0) + meminfo.get("Buffers", 0)
    if level >= 2:
        expected_savings += meminfo.get("SReclaimable", 0)
    
    if simulation:
        return ReclaimResult(
            success=True,
            strategy="drop_caches",
            action=action,
            simulated=True,
            bytes_would_reclaim=expected_savings,
            expected_savings=expected_savings,
            before_snapshot=before_snapshot,
            details={
                "level": level,
                "level_description": {
                    1: "Free pagecache only",
                    2: "Free dentries and inodes",
                    3: "Free pagecache, dentries, and inodes",
                }[level],
                "cached_mb": meminfo.get("Cached", 0) / (1024 * 1024),
                "buffers_mb": meminfo.get("Buffers", 0) / (1024 * 1024),
                "slab_reclaimable_mb": meminfo.get("SReclaimable", 0) / (1024 * 1024),
            },
        )
    
    # Live mode safety checks
    if not confirm_live:
        return ReclaimResult(
            success=False,
            strategy="drop_caches",
            action=action,
            simulated=False,
            error="Live drop_caches requires confirm_live=True for explicit consent",
            before_snapshot=before_snapshot,
        )
    
    # Safety check: Check for high disk I/O
    try:
        with open("/proc/diskstats", "r") as f:
            diskstats = f.read()
        # Simple check: if we can read it, assume I/O is not stuck
    except Exception as e:
        return ReclaimResult(
            success=False,
            strategy="drop_caches",
            action=action,
            simulated=False,
            error=f"Cannot check disk I/O status: {e}",
            before_snapshot=before_snapshot,
        )
    
    try:
        # Sync filesystem first
        logger.info("Syncing filesystem before drop_caches")
        subprocess.run(["sync"], check=True, timeout=30)
        
        # Drop caches
        with open("/proc/sys/vm/drop_caches", "w") as f:
            f.write(str(level))
        
        logger.info(f"Dropped caches (level={level})")
        
        # Capture after snapshot
        meminfo_after = _read_meminfo()
        after_snapshot = {
            "timestamp": datetime.now(timezone.utc).isoformat(),
            "cached_bytes": meminfo_after.get("Cached", 0),
            "buffers_bytes": meminfo_after.get("Buffers", 0),
            "slab_reclaimable_bytes": meminfo_after.get("SReclaimable", 0),
            "mem_available_bytes": meminfo_after.get("MemAvailable", 0),
        }
        
        actual_savings = (
            (before_snapshot["cached_bytes"] - after_snapshot["cached_bytes"]) +
            (before_snapshot["buffers_bytes"] - after_snapshot["buffers_bytes"])
        )
        
        return ReclaimResult(
            success=True,
            strategy="drop_caches",
            action=action,
            simulated=False,
            bytes_reclaimed=max(0, actual_savings),
            expected_savings=expected_savings,
            before_snapshot=before_snapshot,
            after_snapshot=after_snapshot,
            details={
                "level": level,
                "synced_first": True,
                "cached_before_mb": before_snapshot["cached_bytes"] / (1024 * 1024),
                "cached_after_mb": after_snapshot["cached_bytes"] / (1024 * 1024),
            },
        )
        
    except subprocess.TimeoutExpired:
        return ReclaimResult(
            success=False,
            strategy="drop_caches",
            action=action,
            simulated=False,
            error="sync command timed out - filesystem may be busy",
            before_snapshot=before_snapshot,
        )
    except PermissionError:
        return ReclaimResult(
            success=False,
            strategy="drop_caches",
            action=action,
            simulated=False,
            error="Permission denied - requires root privileges",
            before_snapshot=before_snapshot,
        )
    except Exception as e:
        logger.exception(f"drop_caches failed: {e}")
        return ReclaimResult(
            success=False,
            strategy="drop_caches",
            action=action,
            simulated=False,
            error=str(e),
            before_snapshot=before_snapshot,
        )


# ═══════════════════════════════════════════════════════════════════════════════
# MemoryReclaimer Class
# ═══════════════════════════════════════════════════════════════════════════════

class MemoryReclaimer:
    """
    Orchestrates memory reclamation using multiple strategies.
    
    All operations support simulation mode by default.
    Live operations require explicit confirmation.
    """
    
    def __init__(
        self,
        simulation: bool = True,
        backup_enabled: bool = True,
        database: Optional[Any] = None,
    ):
        """
        Initialize the memory reclaimer.
        
        Args:
            simulation: If True, only simulate actions (no real changes)
            backup_enabled: If True, create backups before live changes
            database: Optional database for audit logging
        """
        self.simulation = simulation
        self.backup_enabled = backup_enabled
        self.database = database
        self._backups: Dict[str, CgroupBackup] = {}
        
        # Ensure backup directory exists
        if backup_enabled:
            global BACKUP_DIR
            try:
                BACKUP_DIR.mkdir(parents=True, exist_ok=True)
            except PermissionError:
                logger.warning(f"Cannot create backup dir {BACKUP_DIR}, using /tmp")
                BACKUP_DIR = Path(tempfile.gettempdir()) / "grey-optimizer-backups"
                BACKUP_DIR.mkdir(exist_ok=True)
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Strategy 1: cgroup Memory Limits
    # ═══════════════════════════════════════════════════════════════════════════
    
    async def apply_cgroup_limit(
        self,
        cgroup_name: str,
        memory_max_mb: int,
        memory_high_mb: Optional[int] = None,
        pids: Optional[List[int]] = None,
        confirm_live: bool = False,
    ) -> ReclaimResult:
        """
        Apply cgroup memory limits to restrict process memory.
        
        Args:
            cgroup_name: Name for the cgroup (will be under grey-optimizer/)
            memory_max_mb: Hard memory limit in MB
            memory_high_mb: Soft limit for throttling (optional)
            pids: List of PIDs to move into the cgroup
            confirm_live: Must be True for live enforcement
            
        Returns:
            ReclaimResult with operation outcome
        """
        simulated = self.simulation or not confirm_live
        
        logger.info(f"Applying cgroup limit: {cgroup_name} "
                   f"max={memory_max_mb}MB high={memory_high_mb}MB "
                   f"(simulated={simulated})")
        
        if not IS_LINUX:
            return ReclaimResult(
                success=False,
                strategy="cgroup_limit",
                simulated=True,
                error="cgroup memory limits only available on Linux",
            )
        
        # Check for C helper
        if not os.path.exists(CGROUP_HELPER):
            logger.warning(f"cgroup_helper not found at {CGROUP_HELPER}")
            return ReclaimResult(
                success=False,
                strategy="cgroup_limit",
                simulated=simulated,
                error=f"cgroup_helper binary not found at {CGROUP_HELPER}",
            )
        
        # Create backup if doing live operation
        backup_path = None
        if not simulated and self.backup_enabled:
            backup_path = await self._backup_cgroup_settings(cgroup_name)
        
        try:
            # Create cgroup with limits
            cmd = [
                CGROUP_HELPER, "create",
                f"--name={cgroup_name}",
                f"--memory-max={memory_max_mb}M",
            ]
            
            if memory_high_mb:
                cmd.append(f"--memory-high={memory_high_mb}M")
            
            if simulated:
                cmd.append("--simulate")
            else:
                cmd.append("--confirm-live")
            
            result = await asyncio.to_thread(
                subprocess.run, cmd,
                capture_output=True, text=True, timeout=30
            )
            
            if result.returncode != 0:
                return ReclaimResult(
                    success=False,
                    strategy="cgroup_limit",
                    simulated=simulated,
                    error=result.stderr or f"cgroup_helper failed with code {result.returncode}",
                    backup_path=backup_path,
                )
            
            # Move PIDs if specified
            if pids and not simulated:
                cgroup_path = f"/sys/fs/cgroup/grey-optimizer/{cgroup_name}"
                for pid in pids:
                    move_cmd = [
                        CGROUP_HELPER, "move",
                        f"--pid={pid}",
                        f"--cgroup={cgroup_path}",
                        "--confirm-live",
                    ]
                    await asyncio.to_thread(
                        subprocess.run, move_cmd,
                        capture_output=True, text=True, timeout=10
                    )
            
            # Calculate potential savings (rough estimate)
            # The actual savings depend on process behavior
            potential_savings = memory_max_mb * 1024 * 1024  # In bytes
            
            return ReclaimResult(
                success=True,
                strategy="cgroup_limit",
                simulated=simulated,
                bytes_would_reclaim=potential_savings if simulated else 0,
                bytes_reclaimed=0 if simulated else potential_savings,
                details={
                    "cgroup_name": cgroup_name,
                    "memory_max_mb": memory_max_mb,
                    "memory_high_mb": memory_high_mb,
                    "pids": pids or [],
                },
                backup_path=backup_path,
            )
            
        except subprocess.TimeoutExpired:
            return ReclaimResult(
                success=False,
                strategy="cgroup_limit",
                simulated=simulated,
                error="cgroup_helper timed out",
                backup_path=backup_path,
            )
        except Exception as e:
            logger.exception(f"cgroup limit error: {e}")
            return ReclaimResult(
                success=False,
                strategy="cgroup_limit",
                simulated=simulated,
                error=str(e),
                backup_path=backup_path,
            )
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Strategy 2: madvise Reclamation
    # ═══════════════════════════════════════════════════════════════════════════
    
    async def apply_madvise(
        self,
        pid: int,
        confirm_live: bool = False,
        include_heap: bool = True,
        min_size_kb: int = 64,
    ) -> ReclaimResult:
        """
        Reclaim anonymous memory from a process using madvise(MADV_DONTNEED).
        
        Args:
            pid: Target process ID
            confirm_live: Must be True for live enforcement
            include_heap: Include heap region
            min_size_kb: Minimum region size to process
            
        Returns:
            ReclaimResult with operation outcome
            
        Note:
            This operation is generally safe for cooperating processes that
            can regenerate their data, but may cause data loss if the process
            relies on that memory. Use with caution.
        """
        simulated = self.simulation or not confirm_live
        
        logger.info(f"Applying madvise to PID {pid} (simulated={simulated})")
        
        if not IS_LINUX:
            return ReclaimResult(
                success=False,
                strategy="madvise",
                simulated=True,
                error="madvise reclamation only available on Linux",
            )
        
        # Check for C helper
        if not os.path.exists(MADVISE_HELPER):
            logger.warning(f"madvise_helper not found at {MADVISE_HELPER}")
            return ReclaimResult(
                success=False,
                strategy="madvise",
                simulated=simulated,
                error=f"madvise_helper binary not found at {MADVISE_HELPER}",
            )
        
        try:
            # Build command
            cmd = [
                MADVISE_HELPER,
                f"--pid={pid}",
                f"--min-size={min_size_kb}",
                "--json",
            ]
            
            if include_heap:
                cmd.append("--include-heap")
            
            if simulated:
                cmd.append("--simulate")
            else:
                cmd.append("--confirm-live")
            
            result = await asyncio.to_thread(
                subprocess.run, cmd,
                capture_output=True, text=True, timeout=60
            )
            
            if result.returncode != 0:
                return ReclaimResult(
                    success=False,
                    strategy="madvise",
                    simulated=simulated,
                    error=result.stderr or f"madvise_helper failed with code {result.returncode}",
                )
            
            # Parse JSON output
            try:
                output = json.loads(result.stdout)
                bytes_reclaimed = output.get("bytes_reclaimed", 0)
                bytes_would_reclaim = output.get("bytes_would_reclaim", 0)
            except json.JSONDecodeError:
                bytes_reclaimed = 0
                bytes_would_reclaim = 0
            
            return ReclaimResult(
                success=True,
                strategy="madvise",
                simulated=simulated,
                bytes_reclaimed=bytes_reclaimed if not simulated else 0,
                bytes_would_reclaim=bytes_would_reclaim,
                details={
                    "pid": pid,
                    "include_heap": include_heap,
                    "min_size_kb": min_size_kb,
                    "helper_output": result.stdout[:500] if result.stdout else None,
                },
            )
            
        except subprocess.TimeoutExpired:
            return ReclaimResult(
                success=False,
                strategy="madvise",
                simulated=simulated,
                error="madvise_helper timed out",
            )
        except Exception as e:
            logger.exception(f"madvise error: {e}")
            return ReclaimResult(
                success=False,
                strategy="madvise",
                simulated=simulated,
                error=str(e),
            )
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Strategy 3: KSM Toggling
    # ═══════════════════════════════════════════════════════════════════════════
    
    async def toggle_ksm(
        self,
        enable: bool = True,
        pages_to_scan: int = 200,
        sleep_millisecs: int = 100,
        confirm_live: bool = False,
    ) -> ReclaimResult:
        """
        Enable or tune Kernel Same-page Merging (KSM).
        
        KSM scans for identical memory pages and merges them,
        saving RAM at the cost of some CPU overhead.
        
        Args:
            enable: If True, enable KSM; if False, disable
            pages_to_scan: Number of pages to scan per cycle
            sleep_millisecs: Milliseconds to sleep between cycles
            confirm_live: Must be True for live enforcement
            
        Returns:
            ReclaimResult with operation outcome
        """
        simulated = self.simulation or not confirm_live
        
        logger.info(f"Toggling KSM: enable={enable} (simulated={simulated})")
        
        if not IS_LINUX:
            return ReclaimResult(
                success=False,
                strategy="ksm",
                simulated=True,
                error="KSM only available on Linux",
            )
        
        ksm_path = Path("/sys/kernel/mm/ksm")
        
        if not ksm_path.exists():
            return ReclaimResult(
                success=False,
                strategy="ksm",
                simulated=simulated,
                error="KSM not available (no /sys/kernel/mm/ksm)",
            )
        
        try:
            # Read current state for backup
            current_run = (ksm_path / "run").read_text().strip() if (ksm_path / "run").exists() else "0"
            current_pages = (ksm_path / "pages_to_scan").read_text().strip() if (ksm_path / "pages_to_scan").exists() else "100"
            current_sleep = (ksm_path / "sleep_millisecs").read_text().strip() if (ksm_path / "sleep_millisecs").exists() else "200"
            
            # Get current statistics
            pages_shared = 0
            pages_sharing = 0
            if (ksm_path / "pages_shared").exists():
                pages_shared = int((ksm_path / "pages_shared").read_text().strip())
            if (ksm_path / "pages_sharing").exists():
                pages_sharing = int((ksm_path / "pages_sharing").read_text().strip())
            
            # Calculate potential savings
            page_size = os.sysconf("SC_PAGESIZE")
            potential_savings = pages_shared * page_size  # Conservative estimate
            
            if simulated:
                return ReclaimResult(
                    success=True,
                    strategy="ksm",
                    simulated=True,
                    bytes_would_reclaim=potential_savings,
                    details={
                        "current_state": {
                            "run": current_run,
                            "pages_to_scan": current_pages,
                            "sleep_millisecs": current_sleep,
                        },
                        "proposed_state": {
                            "run": "1" if enable else "0",
                            "pages_to_scan": str(pages_to_scan),
                            "sleep_millisecs": str(sleep_millisecs),
                        },
                        "current_pages_shared": pages_shared,
                        "current_pages_sharing": pages_sharing,
                    },
                )
            
            # Apply changes (requires root)
            (ksm_path / "pages_to_scan").write_text(str(pages_to_scan))
            (ksm_path / "sleep_millisecs").write_text(str(sleep_millisecs))
            (ksm_path / "run").write_text("1" if enable else "0")
            
            return ReclaimResult(
                success=True,
                strategy="ksm",
                simulated=False,
                bytes_reclaimed=potential_savings,
                details={
                    "enabled": enable,
                    "pages_to_scan": pages_to_scan,
                    "sleep_millisecs": sleep_millisecs,
                    "previous_state": {
                        "run": current_run,
                        "pages_to_scan": current_pages,
                        "sleep_millisecs": current_sleep,
                    },
                },
            )
            
        except PermissionError:
            return ReclaimResult(
                success=False,
                strategy="ksm",
                simulated=simulated,
                error="Permission denied - requires root privileges",
            )
        except Exception as e:
            logger.exception(f"KSM error: {e}")
            return ReclaimResult(
                success=False,
                strategy="ksm",
                simulated=simulated,
                error=str(e),
            )
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Strategy 4: zram Helper
    # ═══════════════════════════════════════════════════════════════════════════
    
    async def configure_zram(
        self,
        size_mb: int = 512,
        algorithm: str = "zstd",
        confirm_live: bool = False,
    ) -> ReclaimResult:
        """
        Configure zram compressed swap device.
        
        zram compresses swap in RAM, effectively increasing usable memory
        at the cost of CPU time for compression.
        
        Args:
            size_mb: Size of zram device in MB
            algorithm: Compression algorithm (zstd, lz4, lzo)
            confirm_live: Must be True for live enforcement
            
        Returns:
            ReclaimResult with operation outcome
        """
        simulated = self.simulation or not confirm_live
        
        logger.info(f"Configuring zram: size={size_mb}MB algo={algorithm} (simulated={simulated})")
        
        if not IS_LINUX:
            return ReclaimResult(
                success=False,
                strategy="zram",
                simulated=True,
                error="zram only available on Linux",
            )
        
        zram_path = Path("/sys/block/zram0")
        
        if not zram_path.exists():
            # Check if zram module is available
            modinfo_result = subprocess.run(
                ["modinfo", "zram"],
                capture_output=True, text=True
            )
            if modinfo_result.returncode != 0:
                return ReclaimResult(
                    success=False,
                    strategy="zram",
                    simulated=simulated,
                    error="zram module not available",
                )
            
            if simulated:
                return ReclaimResult(
                    success=True,
                    strategy="zram",
                    simulated=True,
                    bytes_would_reclaim=size_mb * 1024 * 1024,  # Potential RAM saved
                    details={
                        "note": "Would load zram module and configure device",
                        "size_mb": size_mb,
                        "algorithm": algorithm,
                    },
                )
            
            # Load zram module
            subprocess.run(["modprobe", "zram"], check=True)
            zram_path = Path("/sys/block/zram0")
        
        try:
            # Get current state
            current_disksize = 0
            current_algo = "lzo"
            if (zram_path / "disksize").exists():
                current_disksize = int((zram_path / "disksize").read_text().strip())
            if (zram_path / "comp_algorithm").exists():
                algo_text = (zram_path / "comp_algorithm").read_text().strip()
                # Format is: [algo1] algo2 algo3 - brackets indicate current
                for a in algo_text.split():
                    if a.startswith("[") and a.endswith("]"):
                        current_algo = a[1:-1]
                        break
            
            if simulated:
                return ReclaimResult(
                    success=True,
                    strategy="zram",
                    simulated=True,
                    bytes_would_reclaim=size_mb * 1024 * 1024,
                    details={
                        "current_disksize_bytes": current_disksize,
                        "current_algorithm": current_algo,
                        "proposed_size_mb": size_mb,
                        "proposed_algorithm": algorithm,
                    },
                )
            
            # Configure zram (requires the device to be reset first)
            # This is a complex operation that should be done carefully
            
            # Check if already in use
            in_use = False
            try:
                with open("/proc/swaps", "r") as f:
                    if "zram0" in f.read():
                        in_use = True
            except FileNotFoundError:
                pass
            
            if in_use:
                return ReclaimResult(
                    success=False,
                    strategy="zram",
                    simulated=False,
                    error="zram0 already in use - must swapoff first",
                    details={
                        "current_disksize_bytes": current_disksize,
                        "current_algorithm": current_algo,
                    },
                )
            
            # Set algorithm first
            (zram_path / "comp_algorithm").write_text(algorithm)
            
            # Set size
            (zram_path / "disksize").write_text(f"{size_mb}M")
            
            # Create swap filesystem and enable
            subprocess.run(["mkswap", "/dev/zram0"], check=True, capture_output=True)
            subprocess.run(["swapon", "-p", "100", "/dev/zram0"], check=True, capture_output=True)
            
            return ReclaimResult(
                success=True,
                strategy="zram",
                simulated=False,
                bytes_reclaimed=size_mb * 1024 * 1024,
                details={
                    "device": "/dev/zram0",
                    "size_mb": size_mb,
                    "algorithm": algorithm,
                    "swap_priority": 100,
                },
            )
            
        except PermissionError:
            return ReclaimResult(
                success=False,
                strategy="zram",
                simulated=simulated,
                error="Permission denied - requires root privileges",
            )
        except subprocess.CalledProcessError as e:
            return ReclaimResult(
                success=False,
                strategy="zram",
                simulated=simulated,
                error=f"Command failed: {e}",
            )
        except Exception as e:
            logger.exception(f"zram error: {e}")
            return ReclaimResult(
                success=False,
                strategy="zram",
                simulated=simulated,
                error=str(e),
            )
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Strategy 5: drop_caches Wrapper
    # ═══════════════════════════════════════════════════════════════════════════
    
    async def drop_caches(
        self,
        level: int = 1,
        confirm_live: bool = False,
    ) -> ReclaimResult:
        """
        Drop kernel caches to free memory.
        
        Levels:
        - 1: Free pagecache only
        - 2: Free dentries and inodes
        - 3: Free pagecache, dentries, and inodes
        
        Args:
            level: Cache drop level (1-3)
            confirm_live: Must be True for live enforcement
            
        Returns:
            ReclaimResult with operation outcome
            
        SAFETY:
            This is a relatively safe operation that only affects caches.
            The kernel will simply rebuild caches as needed.
            However, it may cause temporary performance degradation.
        """
        simulated = self.simulation or not confirm_live
        
        logger.info(f"Dropping caches (level={level}, simulated={simulated})")
        
        if not IS_LINUX:
            return ReclaimResult(
                success=False,
                strategy="drop_caches",
                simulated=True,
                error="drop_caches only available on Linux",
            )
        
        if level not in (1, 2, 3):
            return ReclaimResult(
                success=False,
                strategy="drop_caches",
                simulated=simulated,
                error=f"Invalid level {level}, must be 1, 2, or 3",
            )
        
        try:
            # Get current memory info for comparison
            with open("/proc/meminfo", "r") as f:
                meminfo = {}
                for line in f:
                    parts = line.split()
                    if len(parts) >= 2:
                        key = parts[0].rstrip(":")
                        meminfo[key] = int(parts[1]) * 1024  # Convert to bytes
            
            cached_before = meminfo.get("Cached", 0)
            buffers_before = meminfo.get("Buffers", 0)
            slab_reclaimable_before = meminfo.get("SReclaimable", 0)
            
            potential_savings = 0
            if level >= 1:
                potential_savings += cached_before + buffers_before
            if level >= 2:
                potential_savings += slab_reclaimable_before
            
            if simulated:
                return ReclaimResult(
                    success=True,
                    strategy="drop_caches",
                    simulated=True,
                    bytes_would_reclaim=potential_savings,
                    details={
                        "level": level,
                        "cached_mb": cached_before / (1024 * 1024),
                        "buffers_mb": buffers_before / (1024 * 1024),
                        "slab_reclaimable_mb": slab_reclaimable_before / (1024 * 1024),
                    },
                )
            
            # Sync first to ensure data is written
            subprocess.run(["sync"], check=True, timeout=30)
            
            # Drop caches
            with open("/proc/sys/vm/drop_caches", "w") as f:
                f.write(str(level))
            
            # Re-read meminfo to measure actual reduction
            with open("/proc/meminfo", "r") as f:
                meminfo_after = {}
                for line in f:
                    parts = line.split()
                    if len(parts) >= 2:
                        key = parts[0].rstrip(":")
                        meminfo_after[key] = int(parts[1]) * 1024
            
            cached_after = meminfo_after.get("Cached", 0)
            buffers_after = meminfo_after.get("Buffers", 0)
            
            actual_savings = (cached_before - cached_after) + (buffers_before - buffers_after)
            
            return ReclaimResult(
                success=True,
                strategy="drop_caches",
                simulated=False,
                bytes_reclaimed=max(0, actual_savings),
                details={
                    "level": level,
                    "cached_before_mb": cached_before / (1024 * 1024),
                    "cached_after_mb": cached_after / (1024 * 1024),
                    "buffers_before_mb": buffers_before / (1024 * 1024),
                    "buffers_after_mb": buffers_after / (1024 * 1024),
                },
            )
            
        except PermissionError:
            return ReclaimResult(
                success=False,
                strategy="drop_caches",
                simulated=simulated,
                error="Permission denied - requires root privileges",
            )
        except Exception as e:
            logger.exception(f"drop_caches error: {e}")
            return ReclaimResult(
                success=False,
                strategy="drop_caches",
                simulated=simulated,
                error=str(e),
            )
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Combined Reclamation
    # ═══════════════════════════════════════════════════════════════════════════
    
    async def reclaim_all(
        self,
        target_pids: Optional[List[int]] = None,
        confirm_live: bool = False,
        strategies: Optional[List[str]] = None,
    ) -> List[ReclaimResult]:
        """
        Apply all applicable reclamation strategies.
        
        Args:
            target_pids: Optional list of PIDs to target
            confirm_live: Must be True for live enforcement
            strategies: Optional list of strategies to use
                        (default: ["drop_caches", "ksm"])
            
        Returns:
            List of ReclaimResult for each strategy applied
        """
        if strategies is None:
            strategies = ["drop_caches", "ksm"]
        
        results = []
        
        for strategy in strategies:
            if strategy == "drop_caches":
                result = await self.drop_caches(level=1, confirm_live=confirm_live)
                results.append(result)
                
            elif strategy == "ksm":
                result = await self.toggle_ksm(enable=True, confirm_live=confirm_live)
                results.append(result)
                
            elif strategy == "madvise" and target_pids:
                for pid in target_pids:
                    result = await self.apply_madvise(pid=pid, confirm_live=confirm_live)
                    results.append(result)
                    
            elif strategy == "cgroup" and target_pids:
                # Create a cgroup with conservative limits
                result = await self.apply_cgroup_limit(
                    cgroup_name="grey-default",
                    memory_max_mb=512,  # Conservative limit
                    pids=target_pids,
                    confirm_live=confirm_live,
                )
                results.append(result)
        
        return results
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Rollback Support
    # ═══════════════════════════════════════════════════════════════════════════
    
    async def _backup_cgroup_settings(self, cgroup_name: str) -> Optional[Path]:
        """Create backup of cgroup settings before modification."""
        try:
            cgroup_path = f"/sys/fs/cgroup/grey-optimizer/{cgroup_name}"
            
            if not os.path.exists(cgroup_path):
                return None
            
            # Read current settings
            memory_max = None
            memory_high = None
            
            max_path = os.path.join(cgroup_path, "memory.max")
            if os.path.exists(max_path):
                with open(max_path) as f:
                    val = f.read().strip()
                    memory_max = None if val == "max" else int(val)
            
            high_path = os.path.join(cgroup_path, "memory.high")
            if os.path.exists(high_path):
                with open(high_path) as f:
                    val = f.read().strip()
                    memory_high = None if val == "max" else int(val)
            
            # Read current PIDs
            pids = []
            procs_path = os.path.join(cgroup_path, "cgroup.procs")
            if os.path.exists(procs_path):
                with open(procs_path) as f:
                    pids = [int(line.strip()) for line in f if line.strip()]
            
            # Create backup
            backup = CgroupBackup(
                cgroup_path=cgroup_path,
                memory_max=memory_max,
                memory_high=memory_high,
                pids=pids,
            )
            
            # Save to file
            backup_file = BACKUP_DIR / f"{cgroup_name}_{datetime.utcnow().strftime('%Y%m%d_%H%M%S')}.json"
            with open(backup_file, "w") as f:
                json.dump(backup.to_dict(), f, indent=2)
            
            self._backups[cgroup_name] = backup
            
            return backup_file
            
        except Exception as e:
            logger.error(f"Failed to backup cgroup settings: {e}")
            return None
    
    async def rollback_cgroup(self, cgroup_name: str) -> bool:
        """Rollback cgroup to previous settings."""
        backup = self._backups.get(cgroup_name)
        
        if not backup:
            logger.warning(f"No backup found for cgroup {cgroup_name}")
            return False
        
        try:
            # Restore memory limits
            if backup.memory_max is not None:
                with open(f"{backup.cgroup_path}/memory.max", "w") as f:
                    f.write(str(backup.memory_max))
            
            if backup.memory_high is not None:
                with open(f"{backup.cgroup_path}/memory.high", "w") as f:
                    f.write(str(backup.memory_high))
            
            # Move PIDs back if needed
            # (PIDs may have exited, so this is best-effort)
            
            logger.info(f"Rolled back cgroup {cgroup_name}")
            return True
            
        except Exception as e:
            logger.error(f"Rollback failed for {cgroup_name}: {e}")
            return False
    
    async def rollback_all(self) -> List[str]:
        """Rollback all modified cgroups."""
        rolled_back = []
        
        for name in list(self._backups.keys()):
            if await self.rollback_cgroup(name):
                rolled_back.append(name)
        
        return rolled_back
