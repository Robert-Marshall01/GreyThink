#!/usr/bin/env python3
"""
Process Lineage Tracking Subsystem for GreyAV
==============================================

A modular process lineage tracking system that monitors parent/child
process relationships and provides ancestry chains for behavioral analysis.

Architecture:
    ProcessMonitor → LineageTracker → ProcessInfo
                  ↓
    EventBus (with lineage-enriched events)

Features:
    - Track parent/child process relationships
    - Build full ancestry chains for any PID
    - Thread-safe design for concurrent access
    - Extensible metadata support
    - Integration with behavioral event system

Usage:
    python3 process_lineage.py [--verbose] [--duration SECONDS]

Author: GreyAV Team
License: MIT
"""

import abc
import argparse
import logging
import os
import queue
import random
import threading
import time
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum, auto
from typing import Any, Callable, Dict, List, Optional, Set, Tuple

# Try to import psutil for real process monitoring
try:
    import psutil
    PSUTIL_AVAILABLE = True
except ImportError:
    PSUTIL_AVAILABLE = False


# =============================================================================
# Configuration
# =============================================================================

class LineageConfig:
    """Configuration for the lineage tracking subsystem."""
    
    # Process table settings
    MAX_PROCESSES: int = 10000          # Maximum tracked processes
    PRUNE_INTERVAL: float = 60.0        # Seconds between prune cycles
    PROCESS_TTL: float = 3600.0         # Keep terminated processes for 1 hour
    
    # Monitoring settings
    POLL_INTERVAL: float = 1.0          # Seconds between process table polls
    ENABLE_MOCK_MODE: bool = not PSUTIL_AVAILABLE
    
    # Event settings
    INCLUDE_FULL_LINEAGE: bool = True   # Include full ancestry in events
    MAX_LINEAGE_DEPTH: int = 50         # Prevent infinite loops


# =============================================================================
# Data Structures
# =============================================================================

class ProcessState(Enum):
    """Process lifecycle states."""
    RUNNING = auto()
    TERMINATED = auto()
    UNKNOWN = auto()


@dataclass
class ProcessInfo:
    """
    Represents information about a process.
    
    This data structure captures essential process attributes and supports
    extensible metadata for future enhancements like hash verification,
    signature validation, and privilege tracking.
    
    Attributes:
        pid: Process ID
        ppid: Parent Process ID
        executable: Path to the executable
        cmdline: Full command line
        user: Username running the process
        start_time: When the process started
        state: Current process state
        metadata: Extensible dictionary for additional attributes
    """
    pid: int
    ppid: int
    executable: str
    cmdline: str
    user: str
    start_time: float
    state: ProcessState = ProcessState.RUNNING
    end_time: Optional[float] = None
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    def __post_init__(self):
        """Initialize default metadata fields."""
        # Reserved fields for future expansion
        self.metadata.setdefault("hash", None)           # File hash
        self.metadata.setdefault("signature", None)      # Code signature
        self.metadata.setdefault("privileges", [])       # Process privileges
        self.metadata.setdefault("environment", {})      # Environment variables
        self.metadata.setdefault("working_dir", None)    # Working directory
        self.metadata.setdefault("open_files", [])       # Open file handles
        self.metadata.setdefault("network_connections", [])  # Network connections
        self.metadata.setdefault("memory_maps", [])      # Memory mappings
        self.metadata.setdefault("threads", [])          # Thread information
        self.metadata.setdefault("tags", set())          # Custom tags
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization."""
        return {
            "pid": self.pid,
            "ppid": self.ppid,
            "executable": self.executable,
            "cmdline": self.cmdline,
            "user": self.user,
            "start_time": self.start_time,
            "start_time_iso": datetime.fromtimestamp(self.start_time).isoformat(),
            "state": self.state.name,
            "end_time": self.end_time,
            "metadata": {k: v for k, v in self.metadata.items() if v}
        }
    
    def __str__(self) -> str:
        exe_name = os.path.basename(self.executable) if self.executable else "unknown"
        return f"[{self.pid}] {exe_name} (parent: {self.ppid}, user: {self.user})"
    
    def __repr__(self) -> str:
        return f"ProcessInfo(pid={self.pid}, ppid={self.ppid}, exe={self.executable!r})"


@dataclass
class LineageChain:
    """
    Represents a complete process ancestry chain.
    
    Attributes:
        target_pid: The PID whose lineage this represents
        chain: List of ProcessInfo from root (init) to target
        depth: Number of ancestors
        complete: Whether chain reaches the root process
    """
    target_pid: int
    chain: List[ProcessInfo]
    depth: int
    complete: bool
    
    def get_root(self) -> Optional[ProcessInfo]:
        """Get the root ancestor."""
        return self.chain[0] if self.chain else None
    
    def get_parent(self) -> Optional[ProcessInfo]:
        """Get the immediate parent."""
        return self.chain[-2] if len(self.chain) >= 2 else None
    
    def get_target(self) -> Optional[ProcessInfo]:
        """Get the target process."""
        return self.chain[-1] if self.chain else None
    
    def contains_executable(self, exe_name: str) -> bool:
        """Check if any ancestor matches the executable name."""
        exe_name_lower = exe_name.lower()
        return any(
            exe_name_lower in (p.executable or "").lower()
            for p in self.chain
        )
    
    def to_list(self) -> List[Dict[str, Any]]:
        """Convert chain to list of dictionaries."""
        return [p.to_dict() for p in self.chain]
    
    def __str__(self) -> str:
        if not self.chain:
            return f"LineageChain(pid={self.target_pid}, empty)"
        
        path = " → ".join(
            os.path.basename(p.executable) if p.executable else f"pid:{p.pid}"
            for p in self.chain
        )
        return f"LineageChain: {path}"


# =============================================================================
# Lineage Tracker
# =============================================================================

class LineageTracker:
    """
    Thread-safe process lineage tracker.
    
    Maintains a table of process information and provides methods to
    query parent/child relationships and build complete ancestry chains.
    
    Thread Safety:
        All public methods are thread-safe using a read-write lock pattern.
        Multiple readers can access the table concurrently, but writes
        require exclusive access.
    
    Usage:
        tracker = LineageTracker()
        tracker.register_process(1234, 1, "/usr/bin/python", "python script.py", "root")
        lineage = tracker.get_lineage(1234)
        print(lineage)
    """
    
    def __init__(self, max_processes: int = LineageConfig.MAX_PROCESSES):
        """
        Initialize the lineage tracker.
        
        Args:
            max_processes: Maximum number of processes to track
        """
        self._processes: Dict[int, ProcessInfo] = {}
        self._children: Dict[int, Set[int]] = defaultdict(set)
        self._max_processes = max_processes
        self._lock = threading.RLock()
        self._logger = logging.getLogger("LineageTracker")
        
        # Statistics
        self._stats = {
            "registered": 0,
            "terminated": 0,
            "pruned": 0,
            "lookups": 0
        }
    
    def register_process(
        self,
        pid: int,
        ppid: int,
        executable: str,
        cmdline: str,
        user: str,
        start_time: Optional[float] = None,
        metadata: Optional[Dict[str, Any]] = None
    ) -> ProcessInfo:
        """
        Register a new process in the lineage table.
        
        Args:
            pid: Process ID
            ppid: Parent Process ID
            executable: Path to the executable
            cmdline: Full command line
            user: Username running the process
            start_time: Process start time (default: now)
            metadata: Additional metadata dictionary
            
        Returns:
            The created ProcessInfo object
        """
        with self._lock:
            # Check if already registered
            if pid in self._processes:
                existing = self._processes[pid]
                # Update if same process instance
                if existing.state == ProcessState.RUNNING:
                    self._logger.debug(f"Process {pid} already registered")
                    return existing
            
            # Create new process info
            process = ProcessInfo(
                pid=pid,
                ppid=ppid,
                executable=executable,
                cmdline=cmdline,
                user=user,
                start_time=start_time or time.time(),
                state=ProcessState.RUNNING,
                metadata=metadata or {}
            )
            
            # Store in table
            self._processes[pid] = process
            self._children[ppid].add(pid)
            self._stats["registered"] += 1
            
            self._logger.debug(f"Registered process: {process}")
            
            # Enforce max processes limit
            if len(self._processes) > self._max_processes:
                self._prune_oldest()
            
            return process
    
    def mark_terminated(self, pid: int) -> Optional[ProcessInfo]:
        """
        Mark a process as terminated.
        
        Args:
            pid: Process ID to mark as terminated
            
        Returns:
            The ProcessInfo if found, None otherwise
        """
        with self._lock:
            if pid not in self._processes:
                return None
            
            process = self._processes[pid]
            process.state = ProcessState.TERMINATED
            process.end_time = time.time()
            self._stats["terminated"] += 1
            
            self._logger.debug(f"Marked terminated: {pid}")
            return process
    
    def get_process(self, pid: int) -> Optional[ProcessInfo]:
        """
        Get process information by PID.
        
        Args:
            pid: Process ID to look up
            
        Returns:
            ProcessInfo if found, None otherwise
        """
        with self._lock:
            self._stats["lookups"] += 1
            return self._processes.get(pid)
    
    def get_parent(self, pid: int) -> Optional[ProcessInfo]:
        """
        Get the parent process of a given PID.
        
        Args:
            pid: Process ID to get parent for
            
        Returns:
            Parent ProcessInfo if found, None otherwise
        """
        with self._lock:
            process = self._processes.get(pid)
            if not process:
                return None
            
            return self._processes.get(process.ppid)
    
    def get_children(self, pid: int) -> List[ProcessInfo]:
        """
        Get all child processes of a given PID.
        
        Args:
            pid: Process ID to get children for
            
        Returns:
            List of child ProcessInfo objects
        """
        with self._lock:
            child_pids = self._children.get(pid, set())
            return [
                self._processes[child_pid]
                for child_pid in child_pids
                if child_pid in self._processes
            ]
    
    def get_lineage(
        self,
        pid: int,
        max_depth: int = LineageConfig.MAX_LINEAGE_DEPTH
    ) -> LineageChain:
        """
        Get the complete ancestry chain for a process.
        
        Builds a list of ProcessInfo from the root ancestor (typically init/systemd)
        down to the target process.
        
        Args:
            pid: Process ID to get lineage for
            max_depth: Maximum ancestry depth to prevent infinite loops
            
        Returns:
            LineageChain containing the ancestry
        """
        with self._lock:
            chain: List[ProcessInfo] = []
            visited: Set[int] = set()
            current_pid = pid
            complete = False
            
            # Build chain from target to root
            while current_pid and len(chain) < max_depth:
                if current_pid in visited:
                    self._logger.warning(f"Circular reference detected at PID {current_pid}")
                    break
                
                visited.add(current_pid)
                
                process = self._processes.get(current_pid)
                if not process:
                    break
                
                chain.append(process)
                
                # Check if we reached root (ppid == 0 or ppid == pid for init)
                if process.ppid == 0 or process.ppid == process.pid:
                    complete = True
                    break
                
                current_pid = process.ppid
            
            # Reverse to get root → target order
            chain.reverse()
            
            return LineageChain(
                target_pid=pid,
                chain=chain,
                depth=len(chain),
                complete=complete
            )
    
    def get_descendants(
        self,
        pid: int,
        max_depth: int = LineageConfig.MAX_LINEAGE_DEPTH
    ) -> List[ProcessInfo]:
        """
        Get all descendants of a process (children, grandchildren, etc.).
        
        Args:
            pid: Process ID to get descendants for
            max_depth: Maximum depth to traverse
            
        Returns:
            List of all descendant ProcessInfo objects
        """
        with self._lock:
            descendants: List[ProcessInfo] = []
            to_visit: List[Tuple[int, int]] = [(pid, 0)]
            visited: Set[int] = set()
            
            while to_visit:
                current_pid, depth = to_visit.pop(0)
                
                if current_pid in visited or depth > max_depth:
                    continue
                
                visited.add(current_pid)
                
                for child_pid in self._children.get(current_pid, set()):
                    if child_pid in self._processes:
                        descendants.append(self._processes[child_pid])
                        to_visit.append((child_pid, depth + 1))
            
            return descendants
    
    def prune_terminated_processes(
        self,
        max_age: float = LineageConfig.PROCESS_TTL
    ) -> int:
        """
        Remove old terminated processes to prevent unbounded growth.
        
        Args:
            max_age: Maximum age in seconds for terminated processes
            
        Returns:
            Number of processes pruned
        """
        with self._lock:
            current_time = time.time()
            to_remove: List[int] = []
            
            for pid, process in self._processes.items():
                if process.state == ProcessState.TERMINATED:
                    if process.end_time and (current_time - process.end_time) > max_age:
                        to_remove.append(pid)
            
            for pid in to_remove:
                self._remove_process(pid)
            
            self._stats["pruned"] += len(to_remove)
            
            if to_remove:
                self._logger.info(f"Pruned {len(to_remove)} terminated processes")
            
            return len(to_remove)
    
    def _prune_oldest(self) -> None:
        """Remove oldest terminated processes when at capacity."""
        terminated = [
            (p.end_time or p.start_time, pid)
            for pid, p in self._processes.items()
            if p.state == ProcessState.TERMINATED
        ]
        
        if terminated:
            terminated.sort()
            # Remove oldest 10%
            to_remove = terminated[:max(1, len(terminated) // 10)]
            for _, pid in to_remove:
                self._remove_process(pid)
    
    def _remove_process(self, pid: int) -> None:
        """Remove a process from the tracker."""
        if pid in self._processes:
            process = self._processes[pid]
            del self._processes[pid]
            
            # Remove from parent's children set
            if process.ppid in self._children:
                self._children[process.ppid].discard(pid)
    
    def get_stats(self) -> Dict[str, Any]:
        """Get tracker statistics."""
        with self._lock:
            running = sum(
                1 for p in self._processes.values()
                if p.state == ProcessState.RUNNING
            )
            terminated = sum(
                1 for p in self._processes.values()
                if p.state == ProcessState.TERMINATED
            )
            
            return {
                **self._stats,
                "total_tracked": len(self._processes),
                "running": running,
                "terminated": terminated,
                "parent_entries": len(self._children)
            }
    
    def clear(self) -> None:
        """Clear all tracked processes."""
        with self._lock:
            self._processes.clear()
            self._children.clear()
            self._logger.info("Cleared all tracked processes")


# =============================================================================
# Process Monitor
# =============================================================================

class ProcessMonitor(abc.ABC):
    """
    Abstract base class for process monitoring.
    
    Subclasses implement OS-specific or mock monitoring logic to detect
    new and terminated processes, reporting them to a LineageTracker.
    """
    
    def __init__(self, lineage_tracker: LineageTracker):
        """
        Initialize the process monitor.
        
        Args:
            lineage_tracker: The lineage tracker to report to
        """
        self.tracker = lineage_tracker
        self._running = False
        self._thread: Optional[threading.Thread] = None
        self._stop_event = threading.Event()
        self._logger = logging.getLogger(self.__class__.__name__)
    
    def start(self) -> None:
        """Start monitoring processes."""
        if self._running:
            self._logger.warning("Monitor already running")
            return
        
        self._stop_event.clear()
        self._running = True
        self._thread = threading.Thread(
            target=self._monitor_loop,
            name=f"ProcessMonitor-{id(self)}",
            daemon=True
        )
        self._thread.start()
        self._logger.info("Started process monitoring")
    
    def stop(self) -> None:
        """Stop monitoring processes."""
        self._stop_event.set()
        self._running = False
        
        if self._thread:
            self._thread.join(timeout=5.0)
        
        self._logger.info("Stopped process monitoring")
    
    @abc.abstractmethod
    def _monitor_loop(self) -> None:
        """Main monitoring loop - implement in subclasses."""
        pass
    
    @abc.abstractmethod
    def snapshot_current_processes(self) -> None:
        """Capture snapshot of all current processes."""
        pass


class PsutilProcessMonitor(ProcessMonitor):
    """
    Process monitor using psutil for real OS process monitoring.
    
    Uses psutil to poll the process table and detect new/terminated processes.
    """
    
    def __init__(
        self,
        lineage_tracker: LineageTracker,
        poll_interval: float = LineageConfig.POLL_INTERVAL
    ):
        """
        Initialize the psutil-based process monitor.
        
        Args:
            lineage_tracker: The lineage tracker to report to
            poll_interval: Seconds between process table polls
        """
        super().__init__(lineage_tracker)
        self.poll_interval = poll_interval
        self._known_pids: Set[int] = set()
    
    def snapshot_current_processes(self) -> None:
        """Capture all currently running processes."""
        if not PSUTIL_AVAILABLE:
            self._logger.warning("psutil not available")
            return
        
        for proc in psutil.process_iter(['pid', 'ppid', 'exe', 'cmdline', 'username', 'create_time']):
            try:
                info = proc.info
                self.tracker.register_process(
                    pid=info['pid'],
                    ppid=info['ppid'] or 0,
                    executable=info['exe'] or "",
                    cmdline=" ".join(info['cmdline'] or []),
                    user=info['username'] or "unknown",
                    start_time=info['create_time']
                )
                self._known_pids.add(info['pid'])
            except (psutil.NoSuchProcess, psutil.AccessDenied, psutil.ZombieProcess):
                continue
    
    def _monitor_loop(self) -> None:
        """Poll process table for changes."""
        if not PSUTIL_AVAILABLE:
            self._logger.error("psutil not available - cannot monitor")
            return
        
        # Initial snapshot
        self.snapshot_current_processes()
        
        while not self._stop_event.is_set():
            try:
                self._poll_processes()
            except Exception as e:
                self._logger.error(f"Error polling processes: {e}")
            
            self._stop_event.wait(self.poll_interval)
    
    def _poll_processes(self) -> None:
        """Poll for new and terminated processes."""
        current_pids: Set[int] = set()
        
        for proc in psutil.process_iter(['pid', 'ppid', 'exe', 'cmdline', 'username', 'create_time']):
            try:
                info = proc.info
                pid = info['pid']
                current_pids.add(pid)
                
                # New process detected
                if pid not in self._known_pids:
                    self.tracker.register_process(
                        pid=pid,
                        ppid=info['ppid'] or 0,
                        executable=info['exe'] or "",
                        cmdline=" ".join(info['cmdline'] or []),
                        user=info['username'] or "unknown",
                        start_time=info['create_time']
                    )
                    self._logger.debug(f"New process: {pid}")
                    
            except (psutil.NoSuchProcess, psutil.AccessDenied, psutil.ZombieProcess):
                continue
        
        # Detect terminated processes
        terminated = self._known_pids - current_pids
        for pid in terminated:
            self.tracker.mark_terminated(pid)
            self._logger.debug(f"Terminated process: {pid}")
        
        self._known_pids = current_pids


class MockProcessMonitor(ProcessMonitor):
    """
    Mock process monitor for testing and demonstration.
    
    Simulates process creation and termination without real OS access.
    """
    
    def __init__(
        self,
        lineage_tracker: LineageTracker,
        poll_interval: float = LineageConfig.POLL_INTERVAL
    ):
        """
        Initialize the mock process monitor.
        
        Args:
            lineage_tracker: The lineage tracker to report to
            poll_interval: Seconds between simulated events
        """
        super().__init__(lineage_tracker)
        self.poll_interval = poll_interval
        self._next_pid = 1000
        self._active_pids: Set[int] = set()
        
        # Simulated process tree templates
        self._process_templates = [
            {"exe": "/bin/bash", "cmdline": "/bin/bash", "user": "root"},
            {"exe": "/usr/bin/python3", "cmdline": "python3 script.py", "user": "user"},
            {"exe": "/usr/bin/curl", "cmdline": "curl https://example.com", "user": "user"},
            {"exe": "/usr/bin/wget", "cmdline": "wget https://example.com/file", "user": "user"},
            {"exe": "/usr/bin/ssh", "cmdline": "ssh user@host", "user": "user"},
            {"exe": "/usr/bin/nc", "cmdline": "nc -l 4444", "user": "user"},
            {"exe": "/tmp/malware", "cmdline": "/tmp/malware --callback", "user": "www-data"},
            {"exe": "/usr/bin/base64", "cmdline": "base64 -d", "user": "user"},
            {"exe": "/bin/sh", "cmdline": "sh -c 'whoami'", "user": "root"},
            {"exe": "/usr/bin/perl", "cmdline": "perl -e 'exec(\"/bin/sh\")'", "user": "user"},
        ]
    
    def snapshot_current_processes(self) -> None:
        """Create initial mock process tree."""
        # Create init process
        self.tracker.register_process(
            pid=1, ppid=0,
            executable="/sbin/init",
            cmdline="/sbin/init",
            user="root",
            start_time=time.time() - 86400  # Started 1 day ago
        )
        self._active_pids.add(1)
        
        # Create systemd/kthreadd
        self.tracker.register_process(
            pid=2, ppid=1,
            executable="/lib/systemd/systemd",
            cmdline="/lib/systemd/systemd --system",
            user="root",
            start_time=time.time() - 86400
        )
        self._active_pids.add(2)
        
        # Create a few initial services
        for i, template in enumerate(self._process_templates[:3], start=100):
            self.tracker.register_process(
                pid=i, ppid=2,
                executable=template["exe"],
                cmdline=template["cmdline"],
                user=template["user"],
                start_time=time.time() - 3600
            )
            self._active_pids.add(i)
        
        self._next_pid = 200
    
    def _monitor_loop(self) -> None:
        """Generate simulated process events."""
        self.snapshot_current_processes()
        
        while not self._stop_event.is_set():
            try:
                self._simulate_activity()
            except Exception as e:
                self._logger.error(f"Error in mock monitor: {e}")
            
            self._stop_event.wait(self.poll_interval)
    
    def _simulate_activity(self) -> None:
        """Simulate random process activity."""
        # Randomly spawn new processes
        if random.random() < 0.3 and self._active_pids:
            parent_pid = random.choice(list(self._active_pids))
            template = random.choice(self._process_templates)
            
            new_pid = self._next_pid
            self._next_pid += 1
            
            self.tracker.register_process(
                pid=new_pid,
                ppid=parent_pid,
                executable=template["exe"],
                cmdline=template["cmdline"],
                user=template["user"]
            )
            self._active_pids.add(new_pid)
            self._logger.debug(f"Mock: spawned {new_pid} from {parent_pid}")
        
        # Randomly terminate processes (but not init or systemd)
        if random.random() < 0.1:
            candidates = [p for p in self._active_pids if p > 100]
            if candidates:
                terminated_pid = random.choice(candidates)
                self.tracker.mark_terminated(terminated_pid)
                self._active_pids.discard(terminated_pid)
                self._logger.debug(f"Mock: terminated {terminated_pid}")


# =============================================================================
# Lineage-Enhanced Event System
# =============================================================================

class LineageEventType(Enum):
    """Event types for lineage-aware events."""
    PROCESS_CREATED = auto()
    PROCESS_TERMINATED = auto()
    SUSPICIOUS_LINEAGE = auto()
    LINEAGE_QUERY = auto()


@dataclass
class LineageEvent:
    """
    An event enriched with process lineage information.
    
    Attributes:
        event_type: Type of event
        pid: Process ID associated with this event
        timestamp: When the event occurred
        data: Event-specific data
        lineage: Full ancestry chain for the process
        sensor_name: Name of sensor that generated this event
    """
    event_type: LineageEventType
    pid: int
    timestamp: float
    data: Dict[str, Any]
    lineage: Optional[LineageChain] = None
    sensor_name: str = "LineageMonitor"
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary with full lineage information."""
        result = {
            "event_type": self.event_type.name,
            "pid": self.pid,
            "timestamp": self.timestamp,
            "timestamp_iso": datetime.fromtimestamp(self.timestamp).isoformat(),
            "data": self.data,
            "sensor_name": self.sensor_name
        }
        
        if self.lineage:
            result["lineage"] = {
                "chain": self.lineage.to_list(),
                "depth": self.lineage.depth,
                "complete": self.lineage.complete
            }
        
        return result


class LineageEventBus:
    """
    Event bus with integrated lineage tracking.
    
    Automatically enriches events with process lineage information
    before forwarding to handlers.
    """
    
    def __init__(self, lineage_tracker: LineageTracker):
        """
        Initialize the lineage-aware event bus.
        
        Args:
            lineage_tracker: Lineage tracker for ancestry lookups
        """
        self.tracker = lineage_tracker
        self._handlers: List[Callable[[LineageEvent], None]] = []
        self._event_queue: queue.Queue = queue.Queue(maxsize=10000)
        self._processor_thread: Optional[threading.Thread] = None
        self._stop_event = threading.Event()
        self._logger = logging.getLogger("LineageEventBus")
        self._event_count = 0
    
    def add_handler(self, handler: Callable[[LineageEvent], None]) -> None:
        """Add an event handler."""
        self._handlers.append(handler)
    
    def emit(
        self,
        event_type: LineageEventType,
        pid: int,
        data: Dict[str, Any],
        sensor_name: str = "Unknown",
        include_lineage: bool = True
    ) -> None:
        """
        Emit an event with optional lineage enrichment.
        
        Args:
            event_type: Type of event
            pid: Associated process ID
            data: Event-specific data
            sensor_name: Name of the emitting sensor
            include_lineage: Whether to include full lineage
        """
        lineage = None
        if include_lineage and LineageConfig.INCLUDE_FULL_LINEAGE:
            lineage = self.tracker.get_lineage(pid)
        
        event = LineageEvent(
            event_type=event_type,
            pid=pid,
            timestamp=time.time(),
            data=data,
            lineage=lineage,
            sensor_name=sensor_name
        )
        
        try:
            self._event_queue.put_nowait(event)
            self._event_count += 1
        except queue.Full:
            self._logger.warning("Event queue full, dropping event")
    
    def start(self) -> None:
        """Start event processing."""
        self._stop_event.clear()
        self._processor_thread = threading.Thread(
            target=self._process_events,
            name="LineageEventBus-Processor",
            daemon=True
        )
        self._processor_thread.start()
        self._logger.info("Started event processing")
    
    def stop(self) -> None:
        """Stop event processing."""
        self._stop_event.set()
        if self._processor_thread:
            self._processor_thread.join(timeout=5.0)
        self._logger.info("Stopped event processing")
    
    def _process_events(self) -> None:
        """Process events from the queue."""
        while not self._stop_event.is_set():
            try:
                event = self._event_queue.get(timeout=0.5)
                self._dispatch_event(event)
            except queue.Empty:
                continue
            except Exception as e:
                self._logger.error(f"Error processing event: {e}")
    
    def _dispatch_event(self, event: LineageEvent) -> None:
        """Dispatch event to all handlers."""
        for handler in self._handlers:
            try:
                handler(event)
            except Exception as e:
                self._logger.error(f"Handler error: {e}")
    
    def get_stats(self) -> Dict[str, Any]:
        """Get event bus statistics."""
        return {
            "event_count": self._event_count,
            "queue_size": self._event_queue.qsize(),
            "handler_count": len(self._handlers)
        }


# =============================================================================
# Lineage-Aware Sensor Base
# =============================================================================

class LineageSensor(abc.ABC):
    """
    Base class for sensors that emit lineage-enriched events.
    
    Sensors can attach PID information to events, which will be
    automatically enriched with full ancestry chains.
    """
    
    def __init__(self, name: str, event_bus: LineageEventBus):
        """
        Initialize the lineage-aware sensor.
        
        Args:
            name: Sensor name
            event_bus: The lineage event bus to emit to
        """
        self.name = name
        self._event_bus = event_bus
        self._running = False
        self._thread: Optional[threading.Thread] = None
        self._stop_event = threading.Event()
        self._logger = logging.getLogger(f"Sensor.{name}")
    
    def emit(
        self,
        event_type: LineageEventType,
        pid: int,
        data: Dict[str, Any]
    ) -> None:
        """
        Emit an event with lineage information.
        
        Args:
            event_type: Type of event
            pid: Process ID for lineage lookup
            data: Event-specific data
        """
        self._event_bus.emit(
            event_type=event_type,
            pid=pid,
            data=data,
            sensor_name=self.name
        )
    
    def start(self) -> None:
        """Start the sensor."""
        self._stop_event.clear()
        self._running = True
        self._thread = threading.Thread(
            target=self._run,
            name=f"Sensor-{self.name}",
            daemon=True
        )
        self._thread.start()
    
    def stop(self) -> None:
        """Stop the sensor."""
        self._stop_event.set()
        self._running = False
        if self._thread:
            self._thread.join(timeout=5.0)
    
    def _run(self) -> None:
        """Wrapper for monitor loop."""
        try:
            self._monitor()
        except Exception as e:
            self._logger.error(f"Sensor error: {e}")
    
    @abc.abstractmethod
    def _monitor(self) -> None:
        """Implement monitoring logic."""
        pass


class SuspiciousLineageSensor(LineageSensor):
    """
    Sensor that detects suspicious process lineages.
    
    Analyzes process ancestry chains for patterns that indicate
    malicious activity, such as:
    - Web server spawning shell
    - Office application spawning command interpreter
    - Unusual parent/child relationships
    """
    
    SUSPICIOUS_PATTERNS = [
        # (parent_pattern, child_pattern, description)
        ("apache|nginx|httpd", "sh|bash|cmd|powershell", "Web server spawning shell"),
        ("sshd", "python|perl|ruby", "SSH spawning scripting interpreter"),
        ("java|javaw", "sh|bash|cmd", "Java spawning shell"),
        ("office|word|excel", "cmd|powershell|wscript", "Office spawning interpreter"),
        ("browser|chrome|firefox", "sh|bash|cmd", "Browser spawning shell"),
    ]
    
    def __init__(
        self,
        event_bus: LineageEventBus,
        lineage_tracker: LineageTracker
    ):
        """Initialize the suspicious lineage sensor."""
        super().__init__("SuspiciousLineageSensor", event_bus)
        self.tracker = lineage_tracker
        self._checked_pids: Set[int] = set()
    
    def _monitor(self) -> None:
        """Check for suspicious lineage patterns."""
        while not self._stop_event.is_set():
            try:
                self._check_lineages()
            except Exception as e:
                self._logger.error(f"Error checking lineages: {e}")
            
            self._stop_event.wait(2.0)
    
    def _check_lineages(self) -> None:
        """Check all processes for suspicious lineages."""
        stats = self.tracker.get_stats()
        
        # Get all tracked processes
        for pid in list(self.tracker._processes.keys()):
            if pid in self._checked_pids:
                continue
            
            process = self.tracker.get_process(pid)
            if not process or process.state != ProcessState.RUNNING:
                continue
            
            # Check lineage for suspicious patterns
            lineage = self.tracker.get_lineage(pid)
            suspicious = self._analyze_lineage(lineage)
            
            if suspicious:
                self.emit(
                    event_type=LineageEventType.SUSPICIOUS_LINEAGE,
                    pid=pid,
                    data={
                        "reason": suspicious,
                        "executable": process.executable,
                        "lineage_depth": lineage.depth
                    }
                )
            
            self._checked_pids.add(pid)
    
    def _analyze_lineage(self, lineage: LineageChain) -> Optional[str]:
        """
        Analyze a lineage chain for suspicious patterns.
        
        Args:
            lineage: The lineage chain to analyze
            
        Returns:
            Description of suspicious pattern if found, None otherwise
        """
        if len(lineage.chain) < 2:
            return None
        
        # Check each parent/child pair in the chain
        for i in range(len(lineage.chain) - 1):
            parent = lineage.chain[i]
            child = lineage.chain[i + 1]
            
            parent_exe = (parent.executable or "").lower()
            child_exe = (child.executable or "").lower()
            
            for parent_pattern, child_pattern, description in self.SUSPICIOUS_PATTERNS:
                import re
                if (re.search(parent_pattern, parent_exe) and
                    re.search(child_pattern, child_exe)):
                    return description
        
        return None


# =============================================================================
# Demo and Example Usage
# =============================================================================

def print_lineage_tree(tracker: LineageTracker, pid: int, indent: int = 0) -> None:
    """Print a process and its descendants as a tree."""
    process = tracker.get_process(pid)
    if not process:
        return
    
    prefix = "  " * indent + ("└─ " if indent > 0 else "")
    exe_name = os.path.basename(process.executable) if process.executable else "unknown"
    state = "✓" if process.state == ProcessState.RUNNING else "✗"
    print(f"{prefix}[{process.pid}] {exe_name} ({process.user}) {state}")
    
    for child in tracker.get_children(pid):
        print_lineage_tree(tracker, child.pid, indent + 1)


def demo_event_handler(event: LineageEvent) -> None:
    """Example event handler that prints events with lineage."""
    print(f"\n{'='*60}")
    print(f"EVENT: {event.event_type.name}")
    print(f"PID: {event.pid}")
    print(f"Sensor: {event.sensor_name}")
    print(f"Data: {event.data}")
    
    if event.lineage and event.lineage.chain:
        print(f"\nLineage ({event.lineage.depth} levels):")
        for i, proc in enumerate(event.lineage.chain):
            arrow = "└─→" if i == len(event.lineage.chain) - 1 else "├──"
            exe = os.path.basename(proc.executable) if proc.executable else "?"
            print(f"  {arrow} [{proc.pid}] {exe}")
    
    print("="*60)


def main():
    """Main entry point for demonstration."""
    parser = argparse.ArgumentParser(
        description="Process Lineage Tracking Subsystem for GreyAV",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s                    Run with mock monitoring
  %(prog)s --verbose          Enable verbose logging
  %(prog)s --duration 30      Run for 30 seconds
  %(prog)s --real             Use real process monitoring (requires psutil)
        """
    )
    
    parser.add_argument("-v", "--verbose", action="store_true",
                        help="Enable verbose logging")
    parser.add_argument("-d", "--duration", type=float, default=None,
                        help="Run duration in seconds")
    parser.add_argument("--real", action="store_true",
                        help="Use real process monitoring (requires psutil)")
    
    args = parser.parse_args()
    
    # Setup logging
    level = logging.DEBUG if args.verbose else logging.INFO
    logging.basicConfig(
        level=level,
        format='%(asctime)s [%(levelname)s] %(name)s: %(message)s',
        datefmt='%H:%M:%S'
    )
    logger = logging.getLogger("Demo")
    
    print("""
    ╔══════════════════════════════════════════════════════════╗
    ║     PROCESS LINEAGE TRACKING - GreyAV v3.0               ║
    ║                                                          ║
    ║  Features:                                               ║
    ║    • Track parent/child process relationships            ║
    ║    • Build complete ancestry chains                      ║
    ║    • Detect suspicious lineage patterns                  ║
    ║    • Enrich events with lineage metadata                 ║
    ║                                                          ║
    ║  Press Ctrl+C to stop                                    ║
    ╚══════════════════════════════════════════════════════════╝
    """)
    
    # Initialize components
    logger.info("Initializing lineage tracker...")
    tracker = LineageTracker()
    
    # Choose monitor type
    if args.real and PSUTIL_AVAILABLE:
        logger.info("Using real process monitoring (psutil)")
        monitor = PsutilProcessMonitor(tracker)
    else:
        if args.real and not PSUTIL_AVAILABLE:
            logger.warning("psutil not available, falling back to mock mode")
        logger.info("Using mock process monitoring")
        monitor = MockProcessMonitor(tracker)
    
    # Initialize event bus
    event_bus = LineageEventBus(tracker)
    event_bus.add_handler(demo_event_handler)
    
    # Initialize suspicious lineage sensor
    suspicious_sensor = SuspiciousLineageSensor(event_bus, tracker)
    
    # Start monitoring
    logger.info("Starting process monitoring...")
    monitor.start()
    event_bus.start()
    suspicious_sensor.start()
    
    try:
        start_time = time.time()
        iteration = 0
        
        while True:
            if args.duration and (time.time() - start_time) >= args.duration:
                break
            
            time.sleep(5)
            iteration += 1
            
            # Print stats periodically
            stats = tracker.get_stats()
            print(f"\n[Stats] Tracked: {stats['total_tracked']}, "
                  f"Running: {stats['running']}, "
                  f"Terminated: {stats['terminated']}")
            
            # Print process tree every 3 iterations
            if iteration % 3 == 0:
                print("\n[Process Tree]")
                print_lineage_tree(tracker, 1)
            
            # Demo: Query lineage for a random process
            if stats['running'] > 0:
                sample_pid = random.choice([
                    pid for pid, p in tracker._processes.items()
                    if p.state == ProcessState.RUNNING
                ])
                lineage = tracker.get_lineage(sample_pid)
                print(f"\n[Lineage Query] {lineage}")
            
            # Prune old processes
            pruned = tracker.prune_terminated_processes()
            if pruned:
                print(f"[Cleanup] Pruned {pruned} terminated processes")
                
    except KeyboardInterrupt:
        print("\nShutting down...")
    
    finally:
        suspicious_sensor.stop()
        monitor.stop()
        event_bus.stop()
    
    # Print final summary
    stats = tracker.get_stats()
    print(f"""
{'='*60}
PROCESS LINEAGE TRACKING - SUMMARY
{'='*60}
Total Registered: {stats['registered']}
Total Terminated: {stats['terminated']}
Total Pruned: {stats['pruned']}
Lineage Lookups: {stats['lookups']}
Currently Tracked: {stats['total_tracked']}
Event Bus: {event_bus.get_stats()}
{'='*60}
    """)


if __name__ == "__main__":
    main()
