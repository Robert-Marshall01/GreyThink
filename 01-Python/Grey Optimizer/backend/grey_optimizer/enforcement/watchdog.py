"""
Memory Safety Watchdog

Monitors system stability during and after memory enforcement actions.
Automatically triggers rollback when instability is detected.

Monitors:
1. OOM events in managed cgroups (memory.events)
2. Critical service health (systemd, sshd, dbus)
3. Available memory thresholds
4. Process death/crash detection
5. Memory pressure stalls (memory.pressure)

Safety Features:
- Continuous background monitoring during enforcement
- Automatic rollback on OOM detection
- Critical service watchlist (never kill these)
- Configurable thresholds and timeouts
- Audit logging of all stability events

Usage:
    from grey_optimizer.enforcement.watchdog import MemoryWatchdog
    
    watchdog = MemoryWatchdog(enforcement_manager)
    
    async with watchdog.monitor():
        # Enforcement actions here
        await manager.enforce_memory_limit(...)
    
    # Watchdog checks stability after exiting context
"""

import asyncio
import logging
import os
import signal
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Dict, Any, List, Optional, Set, Callable, Awaitable
from enum import Enum

logger = logging.getLogger(__name__)


class StabilityStatus(str, Enum):
    """System stability status."""
    STABLE = "stable"
    WARNING = "warning"
    CRITICAL = "critical"
    ROLLBACK_REQUIRED = "rollback_required"


@dataclass
class StabilityEvent:
    """A stability-related event."""
    timestamp: str
    event_type: str
    severity: str  # "info", "warning", "error", "critical"
    source: str
    message: str
    details: Dict[str, Any] = field(default_factory=dict)
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "timestamp": self.timestamp,
            "event_type": self.event_type,
            "severity": self.severity,
            "source": self.source,
            "message": self.message,
            "details": self.details,
        }


@dataclass
class StabilityCheck:
    """Result of a stability check."""
    status: StabilityStatus
    events: List[StabilityEvent] = field(default_factory=list)
    oom_count: int = 0
    dead_services: List[str] = field(default_factory=list)
    available_mb: int = 0
    pressure_avg10: float = 0.0  # Memory pressure over 10s
    
    @property
    def is_stable(self) -> bool:
        return self.status == StabilityStatus.STABLE
    
    @property
    def needs_rollback(self) -> bool:
        return self.status == StabilityStatus.ROLLBACK_REQUIRED


class CgroupEventMonitor:
    """
    Monitors cgroup memory events for OOM kills.
    
    Watches /sys/fs/cgroup/*/memory.events for:
    - oom_kill: Process killed due to OOM
    - high: Memory usage crossed high threshold
    - max: Memory usage hit hard limit
    """
    
    CGROUP_ROOT = "/sys/fs/cgroup"
    
    def __init__(self):
        self._baseline_events: Dict[str, Dict[str, int]] = {}
        self._monitored_cgroups: Set[str] = set()
    
    def add_cgroup(self, cgroup_path: str):
        """Add a cgroup to monitor."""
        self._monitored_cgroups.add(cgroup_path)
        self._baseline_events[cgroup_path] = self._read_events(cgroup_path)
    
    def _read_events(self, cgroup_path: str) -> Dict[str, int]:
        """Read current event counts from memory.events."""
        events = {}
        events_path = f"{cgroup_path}/memory.events"
        
        try:
            if not os.path.exists(events_path):
                return events
            
            with open(events_path, "r") as f:
                for line in f:
                    parts = line.strip().split()
                    if len(parts) == 2:
                        try:
                            events[parts[0]] = int(parts[1])
                        except ValueError:
                            pass
        except Exception as e:
            logger.debug(f"Could not read {events_path}: {e}")
        
        return events
    
    def check_new_events(self) -> List[StabilityEvent]:
        """Check for new OOM or limit events since baseline."""
        new_events = []
        now = datetime.utcnow().isoformat()
        
        for cgroup_path in self._monitored_cgroups:
            current = self._read_events(cgroup_path)
            baseline = self._baseline_events.get(cgroup_path, {})
            
            # Check OOM kills
            oom_kills = current.get("oom_kill", 0) - baseline.get("oom_kill", 0)
            if oom_kills > 0:
                new_events.append(StabilityEvent(
                    timestamp=now,
                    event_type="oom_kill",
                    severity="critical",
                    source=cgroup_path,
                    message=f"{oom_kills} OOM kill(s) detected in {cgroup_path}",
                    details={"oom_kills": oom_kills},
                ))
            
            # Check high threshold crossings
            high_events = current.get("high", 0) - baseline.get("high", 0)
            if high_events > 0:
                new_events.append(StabilityEvent(
                    timestamp=now,
                    event_type="memory_high",
                    severity="warning",
                    source=cgroup_path,
                    message=f"Memory high threshold crossed {high_events} time(s)",
                    details={"high_events": high_events},
                ))
            
            # Check max limit hits
            max_events = current.get("max", 0) - baseline.get("max", 0)
            if max_events > 0:
                new_events.append(StabilityEvent(
                    timestamp=now,
                    event_type="memory_max",
                    severity="warning",
                    source=cgroup_path,
                    message=f"Memory max limit hit {max_events} time(s)",
                    details={"max_events": max_events},
                ))
        
        return new_events
    
    def get_total_oom_kills(self) -> int:
        """Get total new OOM kills across all monitored cgroups."""
        total = 0
        for cgroup_path in self._monitored_cgroups:
            current = self._read_events(cgroup_path)
            baseline = self._baseline_events.get(cgroup_path, {})
            total += current.get("oom_kill", 0) - baseline.get("oom_kill", 0)
        return total


class ServiceHealthMonitor:
    """
    Monitors critical system services.
    
    Checks that essential services are running:
    - systemd (init)
    - sshd (remote access)
    - dbus (system bus)
    - Custom protected services
    """
    
    # Services that should always be running
    DEFAULT_CRITICAL_SERVICES = [
        "systemd",
        "sshd",
        "dbus-daemon",
        "dbus",
    ]
    
    def __init__(self, additional_services: Optional[List[str]] = None):
        self.critical_services = list(self.DEFAULT_CRITICAL_SERVICES)
        if additional_services:
            self.critical_services.extend(additional_services)
        
        self._baseline_pids: Dict[str, int] = {}
        self._take_baseline()
    
    def _take_baseline(self):
        """Record PIDs of critical services."""
        for service in self.critical_services:
            pid = self._find_service_pid(service)
            if pid:
                self._baseline_pids[service] = pid
    
    def _find_service_pid(self, service_name: str) -> Optional[int]:
        """Find PID of a service by name."""
        try:
            for pid_dir in os.listdir("/proc"):
                if not pid_dir.isdigit():
                    continue
                
                try:
                    comm_path = f"/proc/{pid_dir}/comm"
                    with open(comm_path, "r") as f:
                        comm = f.read().strip()
                        if comm == service_name:
                            return int(pid_dir)
                except (FileNotFoundError, PermissionError):
                    continue
        except Exception:
            pass
        
        return None
    
    def check_services(self) -> List[str]:
        """
        Check which critical services are not running.
        
        Returns:
            List of dead service names
        """
        dead_services = []
        
        for service in self.critical_services:
            pid = self._find_service_pid(service)
            
            if pid is None:
                # Service not found by name - might have different name
                baseline_pid = self._baseline_pids.get(service)
                if baseline_pid:
                    # Check if original PID still exists
                    if not os.path.exists(f"/proc/{baseline_pid}"):
                        dead_services.append(service)
            else:
                # Update baseline if service restarted
                self._baseline_pids[service] = pid
        
        return dead_services


class MemoryPressureMonitor:
    """
    Monitors system memory pressure via PSI.
    
    Uses Pressure Stall Information (PSI) from /proc/pressure/memory
    to detect when the system is under memory pressure.
    """
    
    PSI_PATH = "/proc/pressure/memory"
    
    # Thresholds (percentage of time spent waiting for memory)
    WARNING_AVG10 = 10.0   # 10% of last 10s waiting
    CRITICAL_AVG10 = 50.0  # 50% of last 10s waiting
    
    def __init__(self):
        self._available = os.path.exists(self.PSI_PATH)
    
    def is_available(self) -> bool:
        return self._available
    
    def read_pressure(self) -> Dict[str, float]:
        """
        Read memory pressure statistics.
        
        Returns dict with:
        - some_avg10, some_avg60, some_avg300: % time some tasks stalled
        - full_avg10, full_avg60, full_avg300: % time all tasks stalled
        """
        pressure = {}
        
        if not self._available:
            return pressure
        
        try:
            with open(self.PSI_PATH, "r") as f:
                for line in f:
                    parts = line.strip().split()
                    if len(parts) < 4:
                        continue
                    
                    prefix = parts[0]  # "some" or "full"
                    
                    for part in parts[1:]:
                        if "=" not in part:
                            continue
                        key, value = part.split("=", 1)
                        try:
                            pressure[f"{prefix}_{key}"] = float(value)
                        except ValueError:
                            pass
        except Exception as e:
            logger.debug(f"Could not read memory pressure: {e}")
        
        return pressure
    
    def get_status(self) -> StabilityStatus:
        """Get current pressure-based status."""
        pressure = self.read_pressure()
        avg10 = pressure.get("some_avg10", 0.0)
        
        if avg10 >= self.CRITICAL_AVG10:
            return StabilityStatus.CRITICAL
        if avg10 >= self.WARNING_AVG10:
            return StabilityStatus.WARNING
        return StabilityStatus.STABLE


class MemoryWatchdog:
    """
    Coordinates stability monitoring during memory enforcement.
    
    Combines:
    - cgroup OOM event monitoring
    - Critical service health checks
    - Memory pressure monitoring
    - Available memory threshold checks
    
    Automatically triggers rollback when instability detected.
    """
    
    # Minimum available memory before triggering rollback (MB)
    MIN_AVAILABLE_MB = 128
    
    # How often to check stability (seconds)
    CHECK_INTERVAL = 1.0
    
    def __init__(
        self,
        enforcement_manager = None,
        database = None,
        rollback_callback: Optional[Callable[[], Awaitable[None]]] = None,
        protected_services: Optional[List[str]] = None,
    ):
        """
        Initialize the watchdog.
        
        Args:
            enforcement_manager: Manager to call for rollback
            database: Database for audit logging
            rollback_callback: Optional custom rollback function
            protected_services: Additional services to monitor
        """
        self.enforcement_manager = enforcement_manager
        self.database = database
        self.rollback_callback = rollback_callback
        
        # Initialize monitors
        self.cgroup_monitor = CgroupEventMonitor()
        self.service_monitor = ServiceHealthMonitor(protected_services)
        self.pressure_monitor = MemoryPressureMonitor()
        
        # State
        self._running = False
        self._monitor_task: Optional[asyncio.Task] = None
        self._events: List[StabilityEvent] = []
        self._check_count = 0
        self._rollback_triggered = False
    
    def add_cgroup(self, cgroup_path: str):
        """Add a cgroup to monitor for OOM events."""
        self.cgroup_monitor.add_cgroup(cgroup_path)
    
    async def check_stability(self) -> StabilityCheck:
        """
        Perform a complete stability check.
        
        Returns:
            StabilityCheck with current status and any events
        """
        now = datetime.utcnow().isoformat()
        events = []
        status = StabilityStatus.STABLE
        
        # Check cgroup events
        cgroup_events = self.cgroup_monitor.check_new_events()
        events.extend(cgroup_events)
        
        oom_count = self.cgroup_monitor.get_total_oom_kills()
        if oom_count > 0:
            status = StabilityStatus.ROLLBACK_REQUIRED
        
        # Check critical services
        dead_services = self.service_monitor.check_services()
        if dead_services:
            events.append(StabilityEvent(
                timestamp=now,
                event_type="service_dead",
                severity="critical",
                source="service_monitor",
                message=f"Critical services not running: {dead_services}",
                details={"dead_services": dead_services},
            ))
            status = StabilityStatus.ROLLBACK_REQUIRED
        
        # Check available memory
        available_mb = self._get_available_mb()
        if available_mb < self.MIN_AVAILABLE_MB:
            events.append(StabilityEvent(
                timestamp=now,
                event_type="low_memory",
                severity="critical",
                source="meminfo",
                message=f"Available memory critically low: {available_mb}MB",
                details={"available_mb": available_mb},
            ))
            if status != StabilityStatus.ROLLBACK_REQUIRED:
                status = StabilityStatus.CRITICAL
        
        # Check memory pressure
        pressure_status = self.pressure_monitor.get_status()
        pressure = self.pressure_monitor.read_pressure()
        avg10 = pressure.get("some_avg10", 0.0)
        
        if pressure_status == StabilityStatus.CRITICAL:
            events.append(StabilityEvent(
                timestamp=now,
                event_type="high_pressure",
                severity="warning",
                source="psi",
                message=f"High memory pressure: {avg10:.1f}% avg10",
                details={"pressure": pressure},
            ))
            if status == StabilityStatus.STABLE:
                status = StabilityStatus.WARNING
        
        # Store events
        self._events.extend(events)
        
        return StabilityCheck(
            status=status,
            events=events,
            oom_count=oom_count,
            dead_services=dead_services,
            available_mb=available_mb,
            pressure_avg10=avg10,
        )
    
    def _get_available_mb(self) -> int:
        """Get available memory in MB."""
        try:
            with open("/proc/meminfo", "r") as f:
                for line in f:
                    if line.startswith("MemAvailable:"):
                        kb = int(line.split()[1])
                        return kb // 1024
        except Exception:
            pass
        return 0
    
    async def _monitor_loop(self):
        """Background monitoring loop."""
        logger.info("Memory watchdog started")
        
        while self._running:
            try:
                check = await self.check_stability()
                self._check_count += 1
                
                if check.needs_rollback and not self._rollback_triggered:
                    logger.warning(f"Stability check failed: {check.status.value}")
                    await self._trigger_rollback(check)
                elif check.status == StabilityStatus.CRITICAL:
                    logger.warning(f"System critical: {[e.message for e in check.events]}")
                
                await asyncio.sleep(self.CHECK_INTERVAL)
                
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Watchdog error: {e}")
                await asyncio.sleep(self.CHECK_INTERVAL)
        
        logger.info("Memory watchdog stopped")
    
    async def _trigger_rollback(self, check: StabilityCheck):
        """Trigger rollback due to instability."""
        logger.warning("Triggering rollback due to instability")
        self._rollback_triggered = True
        
        # Log to database
        if self.database:
            try:
                await self.database.log_event(
                    event_type="stability_rollback",
                    data={
                        "status": check.status.value,
                        "oom_count": check.oom_count,
                        "dead_services": check.dead_services,
                        "available_mb": check.available_mb,
                        "events": [e.to_dict() for e in check.events],
                    },
                )
            except Exception as e:
                logger.error(f"Failed to log rollback: {e}")
        
        # Execute rollback
        try:
            if self.rollback_callback:
                await self.rollback_callback()
            elif self.enforcement_manager:
                await self.enforcement_manager.rollback_all()
            else:
                logger.warning("No rollback mechanism configured")
        except Exception as e:
            logger.error(f"Rollback failed: {e}")
    
    async def start(self):
        """Start background monitoring."""
        if self._running:
            return
        
        self._running = True
        self._rollback_triggered = False
        self._events.clear()
        self._check_count = 0
        
        self._monitor_task = asyncio.create_task(self._monitor_loop())
    
    async def stop(self):
        """Stop background monitoring."""
        self._running = False
        
        if self._monitor_task:
            self._monitor_task.cancel()
            try:
                await self._monitor_task
            except asyncio.CancelledError:
                pass
            self._monitor_task = None
    
    async def __aenter__(self):
        """Context manager entry - start monitoring."""
        await self.start()
        return self
    
    async def __aexit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit - stop monitoring and final check."""
        await self.stop()
        
        # Final stability check
        check = await self.check_stability()
        if check.needs_rollback and not self._rollback_triggered:
            await self._trigger_rollback(check)
        
        return False  # Don't suppress exceptions
    
    def get_events(self) -> List[Dict[str, Any]]:
        """Get all recorded stability events."""
        return [e.to_dict() for e in self._events]
    
    def get_summary(self) -> Dict[str, Any]:
        """Get monitoring summary."""
        return {
            "check_count": self._check_count,
            "event_count": len(self._events),
            "rollback_triggered": self._rollback_triggered,
            "events": self.get_events()[-10:],  # Last 10 events
        }
