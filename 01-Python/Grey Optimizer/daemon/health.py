"""
Grey Optimizer - Health Module

Health checks and watchdog functionality:
- Monitor for OOM events in kernel logs
- Check critical service health
- Monitor memory pressure
- Trigger automatic rollback on failure

Safety is the top priority - if anything looks wrong,
we roll back enforcement actions immediately.
"""

import asyncio
import logging
import os
import re
import subprocess
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Set

logger = logging.getLogger(__name__)


@dataclass
class HealthStatus:
    """Current health status of the system."""
    healthy: bool = True
    timestamp: datetime = field(default_factory=datetime.utcnow)
    checks: Dict[str, bool] = field(default_factory=dict)
    issues: List[str] = field(default_factory=list)
    oom_detected: bool = False
    memory_pressure_high: bool = False
    critical_services_ok: bool = True
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "healthy": self.healthy,
            "timestamp": self.timestamp.isoformat(),
            "checks": self.checks,
            "issues": self.issues,
            "oom_detected": self.oom_detected,
            "memory_pressure_high": self.memory_pressure_high,
            "critical_services_ok": self.critical_services_ok,
        }


@dataclass
class RollbackEvent:
    """Record of an automatic rollback."""
    timestamp: datetime
    trigger: str
    success: bool
    details: Dict[str, Any] = field(default_factory=dict)
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "timestamp": self.timestamp.isoformat(),
            "trigger": self.trigger,
            "success": self.success,
            "details": self.details,
        }


class OOMMonitor:
    """
    Monitors for OOM (Out of Memory) killer events.
    
    Watches kernel logs for OOM-related messages to detect
    when the system is under severe memory pressure.
    """
    
    # OOM patterns to look for in dmesg
    OOM_PATTERNS = [
        r"Out of memory:",
        r"oom-killer:",
        r"Killed process \d+",
        r"Memory cgroup out of memory",
        r"invoked oom-killer:",
    ]
    
    def __init__(self):
        self._last_check = datetime.utcnow()
        self._oom_count = 0
        self._compiled_patterns = [re.compile(p) for p in self.OOM_PATTERNS]
    
    async def check_oom(self) -> Dict[str, Any]:
        """
        Check for recent OOM events.
        
        Returns:
            Dict with OOM detection results
        """
        result = {
            "oom_detected": False,
            "oom_count": 0,
            "recent_kills": [],
            "last_check": self._last_check.isoformat(),
        }
        
        try:
            # Read recent kernel messages
            dmesg_output = await asyncio.to_thread(self._read_dmesg)
            
            if not dmesg_output:
                return result
            
            # Parse for OOM events
            oom_events = []
            killed_pids = []
            
            for line in dmesg_output.split("\n"):
                for pattern in self._compiled_patterns:
                    if pattern.search(line):
                        oom_events.append(line.strip())
                        
                        # Extract killed PID if present
                        pid_match = re.search(r"Killed process (\d+)", line)
                        if pid_match:
                            killed_pids.append(int(pid_match.group(1)))
                        break
            
            result["oom_detected"] = len(oom_events) > 0
            result["oom_count"] = len(oom_events)
            result["recent_kills"] = killed_pids[:10]  # Limit to 10
            
            if oom_events:
                logger.warning(f"OOM events detected: {len(oom_events)}")
            
            self._last_check = datetime.utcnow()
            self._oom_count = len(oom_events)
            
        except Exception as e:
            logger.error(f"OOM check failed: {e}")
            result["error"] = str(e)
        
        return result
    
    def _read_dmesg(self) -> str:
        """Read recent kernel messages."""
        try:
            # Try dmesg command first
            result = subprocess.run(
                ["dmesg", "--time-format=iso", "-T"],
                capture_output=True,
                text=True,
                timeout=5,
            )
            if result.returncode == 0:
                return result.stdout
            
            # Fallback to reading /dev/kmsg or /var/log/kern.log
            for path in ["/var/log/kern.log", "/var/log/messages"]:
                if os.path.exists(path):
                    with open(path, "r") as f:
                        # Read last 1000 lines
                        lines = f.readlines()[-1000:]
                        return "".join(lines)
            
            return ""
            
        except subprocess.TimeoutExpired:
            return ""
        except PermissionError:
            return ""


class MemoryPressureMonitor:
    """
    Monitors memory pressure using PSI (Pressure Stall Information).
    
    PSI provides information about resource contention - when tasks
    are stalled waiting for memory.
    """
    
    PSI_PATH = Path("/proc/pressure/memory")
    
    # Pressure thresholds (percentage)
    THRESHOLD_WARN = 10.0      # 10% stall time = warning
    THRESHOLD_CRITICAL = 30.0  # 30% stall time = critical
    
    def __init__(self):
        self._available = self.PSI_PATH.exists()
        self._last_pressure = {}
    
    def is_available(self) -> bool:
        """Check if PSI is available on this system."""
        return self._available
    
    async def get_pressure(self) -> Dict[str, Any]:
        """
        Get current memory pressure statistics.
        
        Returns:
            Dict with pressure metrics
        """
        if not self._available:
            return {
                "available": False,
                "error": "PSI not available (requires Linux 4.20+)",
            }
        
        try:
            content = await asyncio.to_thread(self._read_psi)
            
            result = {
                "available": True,
                "some": {},
                "full": {},
                "critical": False,
                "warning": False,
            }
            
            for line in content.split("\n"):
                if line.startswith("some"):
                    result["some"] = self._parse_psi_line(line)
                elif line.startswith("full"):
                    result["full"] = self._parse_psi_line(line)
            
            # Check thresholds
            full_avg10 = result["full"].get("avg10", 0)
            if full_avg10 >= self.THRESHOLD_CRITICAL:
                result["critical"] = True
                result["warning"] = True
            elif full_avg10 >= self.THRESHOLD_WARN:
                result["warning"] = True
            
            self._last_pressure = result
            return result
            
        except Exception as e:
            logger.error(f"Pressure check failed: {e}")
            return {
                "available": True,
                "error": str(e),
            }
    
    def _read_psi(self) -> str:
        """Read PSI file synchronously."""
        return self.PSI_PATH.read_text()
    
    def _parse_psi_line(self, line: str) -> Dict[str, float]:
        """Parse a PSI line like 'some avg10=0.00 avg60=0.00 avg300=0.00 total=0'"""
        result = {}
        
        for part in line.split():
            if "=" in part:
                key, value = part.split("=", 1)
                try:
                    result[key] = float(value)
                except ValueError:
                    pass
        
        return result


class ServiceMonitor:
    """
    Monitors critical system services.
    
    If essential services die after we apply enforcement,
    we should roll back immediately.
    """
    
    # Services that should always be running on a healthy system
    CRITICAL_SERVICES = {
        "systemd",      # Init system
        "dbus-daemon",  # Message bus
    }
    
    # Services to check if they were running at startup
    OPTIONAL_SERVICES = {
        "sshd",
        "NetworkManager",
        "chronyd",
        "ntpd",
    }
    
    def __init__(self):
        self._initial_services: Set[str] = set()
        self._captured_initial = False
    
    async def capture_initial_state(self) -> None:
        """Capture which services are running at startup."""
        try:
            running = await asyncio.to_thread(self._get_running_services)
            self._initial_services = running
            self._captured_initial = True
            logger.info(f"Captured {len(running)} running services for monitoring")
        except Exception as e:
            logger.error(f"Failed to capture initial services: {e}")
    
    async def check_services(self) -> Dict[str, Any]:
        """
        Check if critical and initially-running services are still up.
        
        Returns:
            Dict with service health information
        """
        result = {
            "healthy": True,
            "critical_ok": True,
            "dead_services": [],
            "services_checked": [],
        }
        
        try:
            running = await asyncio.to_thread(self._get_running_services)
            
            # Check critical services
            for service in self.CRITICAL_SERVICES:
                result["services_checked"].append(service)
                if not self._service_running(service, running):
                    result["critical_ok"] = False
                    result["healthy"] = False
                    result["dead_services"].append(service)
                    logger.warning(f"Critical service not running: {service}")
            
            # Check initially-running optional services
            if self._captured_initial:
                for service in self.OPTIONAL_SERVICES & self._initial_services:
                    if not self._service_running(service, running):
                        result["dead_services"].append(service)
                        logger.warning(f"Initially-running service stopped: {service}")
            
        except Exception as e:
            logger.error(f"Service check failed: {e}")
            result["error"] = str(e)
        
        return result
    
    def _get_running_services(self) -> Set[str]:
        """Get set of running process names."""
        running = set()
        
        try:
            for pid in os.listdir("/proc"):
                if not pid.isdigit():
                    continue
                try:
                    comm_path = f"/proc/{pid}/comm"
                    with open(comm_path, "r") as f:
                        name = f.read().strip()
                        running.add(name)
                except (FileNotFoundError, PermissionError):
                    continue
        except Exception:
            pass
        
        return running
    
    def _service_running(self, name: str, running: Set[str]) -> bool:
        """Check if a service is running (fuzzy match)."""
        for proc in running:
            if name in proc or proc in name:
                return True
        return False


class HealthWatchdog:
    """
    Main health watchdog that orchestrates all health checks
    and triggers rollback when needed.
    
    The watchdog runs in the background and monitors:
    - OOM killer activity
    - Memory pressure
    - Critical service health
    
    If any check fails, it triggers an automatic rollback.
    """
    
    def __init__(
        self,
        rollback_callback: Optional[Callable[[], Any]] = None,
        check_interval: float = 5.0,
    ):
        """
        Initialize the watchdog.
        
        Args:
            rollback_callback: Async function to call when rollback is needed
            check_interval: Seconds between health checks
        """
        self.rollback_callback = rollback_callback
        self.check_interval = check_interval
        
        self.oom_monitor = OOMMonitor()
        self.pressure_monitor = MemoryPressureMonitor()
        self.service_monitor = ServiceMonitor()
        
        self._running = False
        self._task: Optional[asyncio.Task] = None
        self._rollback_events: List[RollbackEvent] = []
        self._last_status: Optional[HealthStatus] = None
        self._rollback_triggered = False
    
    async def start(self) -> None:
        """Start the watchdog background task."""
        if self._running:
            return
        
        self._running = True
        
        # Capture initial service state
        await self.service_monitor.capture_initial_state()
        
        # Start background monitoring
        self._task = asyncio.create_task(self._monitor_loop())
        logger.info("Health watchdog started")
    
    async def stop(self) -> None:
        """Stop the watchdog."""
        self._running = False
        
        if self._task:
            self._task.cancel()
            try:
                await self._task
            except asyncio.CancelledError:
                pass
        
        logger.info("Health watchdog stopped")
    
    async def check_health(self) -> HealthStatus:
        """
        Perform a complete health check.
        
        Returns:
            HealthStatus with all check results
        """
        status = HealthStatus()
        
        # OOM check
        oom_result = await self.oom_monitor.check_oom()
        status.oom_detected = oom_result.get("oom_detected", False)
        status.checks["oom"] = not status.oom_detected
        if status.oom_detected:
            status.healthy = False
            status.issues.append(f"OOM killer active: {oom_result.get('oom_count', 0)} events")
        
        # Memory pressure check
        pressure_result = await self.pressure_monitor.get_pressure()
        status.memory_pressure_high = pressure_result.get("critical", False)
        status.checks["memory_pressure"] = not status.memory_pressure_high
        if status.memory_pressure_high:
            status.healthy = False
            full_avg = pressure_result.get("full", {}).get("avg10", 0)
            status.issues.append(f"Memory pressure critical: {full_avg:.1f}% stall time")
        
        # Service check
        service_result = await self.service_monitor.check_services()
        status.critical_services_ok = service_result.get("critical_ok", True)
        status.checks["services"] = status.critical_services_ok
        if not status.critical_services_ok:
            status.healthy = False
            dead = service_result.get("dead_services", [])
            status.issues.append(f"Critical services down: {', '.join(dead)}")
        
        self._last_status = status
        return status
    
    async def _monitor_loop(self) -> None:
        """Background monitoring loop."""
        while self._running:
            try:
                status = await self.check_health()
                
                if not status.healthy and not self._rollback_triggered:
                    logger.warning(f"Health check failed: {status.issues}")
                    await self._trigger_rollback(status)
                
            except Exception as e:
                logger.error(f"Health check error: {e}")
            
            await asyncio.sleep(self.check_interval)
    
    async def _trigger_rollback(self, status: HealthStatus) -> None:
        """Trigger automatic rollback due to health failure."""
        trigger = "; ".join(status.issues) or "Unknown health issue"
        logger.warning(f"Triggering automatic rollback: {trigger}")
        
        self._rollback_triggered = True
        event = RollbackEvent(
            timestamp=datetime.utcnow(),
            trigger=trigger,
            success=False,
        )
        
        try:
            if self.rollback_callback:
                await self.rollback_callback()
                event.success = True
                logger.info("Automatic rollback completed successfully")
            else:
                logger.warning("No rollback callback configured")
                event.details["error"] = "No rollback callback"
                
        except Exception as e:
            logger.exception(f"Rollback failed: {e}")
            event.details["error"] = str(e)
        
        self._rollback_events.append(event)
    
    def reset_rollback_flag(self) -> None:
        """Reset the rollback flag to allow future rollbacks."""
        self._rollback_triggered = False
    
    def get_status(self) -> Optional[HealthStatus]:
        """Get the last health status."""
        return self._last_status
    
    def get_rollback_events(self) -> List[Dict[str, Any]]:
        """Get all rollback events."""
        return [e.to_dict() for e in self._rollback_events]
    
    def get_summary(self) -> Dict[str, Any]:
        """Get a summary of watchdog state."""
        return {
            "running": self._running,
            "last_status": self._last_status.to_dict() if self._last_status else None,
            "rollback_triggered": self._rollback_triggered,
            "rollback_count": len(self._rollback_events),
            "oom_monitor_available": True,
            "pressure_monitor_available": self.pressure_monitor.is_available(),
        }


class QuickHealthCheck:
    """
    Quick health check for CLI usage.
    
    Performs a one-shot health check without starting background monitoring.
    """
    
    @staticmethod
    async def run() -> HealthStatus:
        """Run a quick health check."""
        oom = OOMMonitor()
        pressure = MemoryPressureMonitor()
        services = ServiceMonitor()
        
        status = HealthStatus()
        
        # OOM check
        oom_result = await oom.check_oom()
        status.oom_detected = oom_result.get("oom_detected", False)
        status.checks["oom"] = not status.oom_detected
        if status.oom_detected:
            status.healthy = False
            status.issues.append("OOM killer recently active")
        
        # Pressure check
        if pressure.is_available():
            pressure_result = await pressure.get_pressure()
            status.memory_pressure_high = pressure_result.get("critical", False)
            status.checks["memory_pressure"] = not status.memory_pressure_high
            if status.memory_pressure_high:
                status.healthy = False
                status.issues.append("High memory pressure")
        else:
            status.checks["memory_pressure"] = True  # N/A
        
        # Services check
        service_result = await services.check_services()
        status.critical_services_ok = service_result.get("critical_ok", True)
        status.checks["services"] = status.critical_services_ok
        if not status.critical_services_ok:
            status.healthy = False
            status.issues.append("Critical services not running")
        
        # Memory availability check
        try:
            with open("/proc/meminfo", "r") as f:
                for line in f:
                    if line.startswith("MemAvailable:"):
                        available_kb = int(line.split()[1])
                        available_mb = available_kb / 1024
                        status.checks["memory_available"] = available_mb > 100
                        if available_mb < 100:
                            status.healthy = False
                            status.issues.append(f"Very low memory: {available_mb:.0f} MB available")
                        break
        except Exception:
            pass
        
        return status
