"""
Grey Optimizer Main Daemon

The central orchestrator that:
1. Collects telemetry from /proc and cgroups (Linux), equivalent APIs (macOS/Windows)
2. Applies policy decisions
3. Executes enforcement actions
4. Serves the REST/WebSocket API
5. Maintains audit logs and proof artifacts

Safety Principle: All enforcement actions are logged, reversible
where possible, and gated behind consent for destructive operations.

Cross-Platform Support:
- Linux: Full cgroup v2, KSM, sched_setscheduler
- macOS: renice, purge (limited), sparse files
- Windows: Job Objects, SetPriorityClass, NTFS hardlinks
"""

import asyncio
import logging
import signal
import sys
import os
import platform
from pathlib import Path
from datetime import datetime
from typing import Optional

from .config import Config
from .telemetry.collector import TelemetryCollector
from .policy.engine import PolicyEngine
from .enforcement.manager import EnforcementManager
from .api.server import APIServer
from .persistence.database import Database
from .persistence.proof_generator import ProofGenerator

# Platform detection
PLATFORM = platform.system().lower()
IS_LINUX = PLATFORM == "linux"
IS_MACOS = PLATFORM == "darwin"
IS_WINDOWS = PLATFORM == "windows"

# Configure logging - will be reconfigured after args parsed
# Start with minimal console output (WARNING and above only)
log_level = os.environ.get("GREY_OPTIMIZER_LOG_LEVEL", "INFO").upper()
logging.basicConfig(
    level=getattr(logging, log_level, logging.INFO),
    format="%(asctime)s [%(levelname)s] %(name)s: %(message)s",
    handlers=[
        logging.NullHandler(),  # Suppress console output by default
    ]
)
logger = logging.getLogger(__name__)

# systemd watchdog support (Linux only)
try:
    import sdnotify
    SDNOTIFY_AVAILABLE = True
except ImportError:
    SDNOTIFY_AVAILABLE = False


class GreyOptimizerDaemon:
    """
    Main daemon class for Grey Optimizer.
    
    Coordinates all subsystems and manages the lifecycle of
    telemetry collection, policy enforcement, and API serving.
    """
    
    def __init__(self, config: Config):
        """
        Initialize the daemon with configuration.
        
        Args:
            config: Validated configuration object
        """
        self.config = config
        self.running = False
        self._shutdown_event = asyncio.Event()
        
        # Initialize subsystems
        self.database = Database(config.persistence.db_path)
        self.telemetry = TelemetryCollector(config.telemetry)
        self.policy = PolicyEngine(config)
        self.enforcement = EnforcementManager(
            config=config,
            database=self.database,
            simulation=config.simulation_mode
        )
        self.proof_generator = ProofGenerator(
            proofs_dir=config.persistence.proofs_dir,
            hmac_key=config.persistence.hmac_key
        )
        self.api_server = APIServer(
            config=config.api,
            telemetry=self.telemetry,
            policy=self.policy,
            enforcement=self.enforcement,
            database=self.database
        )
        
        # Baseline metrics storage
        self.baseline_metrics: Optional[dict] = None
        self.enforcement_start_time: Optional[datetime] = None
        
        logger.info(f"Grey Optimizer initialized (simulation={config.simulation_mode})")
    
    async def start(self) -> None:
        """
        Start the daemon and all subsystems.
        
        This method blocks until shutdown is signaled.
        """
        logger.info("Starting Grey Optimizer daemon...")
        self.running = True
        
        # Setup signal handlers
        loop = asyncio.get_event_loop()
        for sig in (signal.SIGTERM, signal.SIGINT):
            loop.add_signal_handler(sig, self._signal_handler)
        
        try:
            # Initialize database
            await self.database.initialize()
            logger.info("Database initialized")
            
            # Log startup
            await self.database.log_action(
                action="daemon_start",
                subsystem="core",
                parameters={"simulation_mode": self.config.simulation_mode},
                outcome="success"
            )
            
            # Start subsystems concurrently
            await asyncio.gather(
                self._run_telemetry_loop(),
                self._run_enforcement_loop(),
                self._run_watchdog_loop(),
                self.api_server.start(),
            )
            
        except asyncio.CancelledError:
            logger.info("Daemon cancelled")
        except Exception as e:
            logger.exception(f"Daemon error: {e}")
            raise
        finally:
            await self._shutdown()
    
    async def _run_telemetry_loop(self) -> None:
        """
        Continuously collect telemetry metrics.
        
        Metrics are collected at the configured interval and
        stored for policy decisions and API consumption.
        """
        logger.info("Starting telemetry collection loop")
        
        while self.running:
            try:
                # Collect current metrics
                metrics = await self.telemetry.collect()
                
                # During warmup, accumulate baseline
                if self.baseline_metrics is None:
                    warmup_remaining = self.config.telemetry.warmup_period
                    if warmup_remaining > 0:
                        logger.info(f"Warmup: collecting baseline ({warmup_remaining:.1f}s remaining)")
                        self.config.telemetry.warmup_period -= self.config.telemetry.sample_interval
                    else:
                        # Capture baseline
                        self.baseline_metrics = await self.telemetry.get_baseline()
                        self.enforcement_start_time = datetime.utcnow()
                        logger.info(f"Baseline captured: CPU={self.baseline_metrics['cpu']['usage_percent']:.1f}%, "
                                  f"RAM={self.baseline_metrics['ram']['used_mb']:.0f}MB")
                
                # Notify API clients
                await self.api_server.broadcast_metrics(metrics)
                
            except Exception as e:
                logger.error(f"Telemetry error: {e}")
            
            await asyncio.sleep(self.config.telemetry.sample_interval)
    
    async def _run_enforcement_loop(self) -> None:
        """
        Apply enforcement based on policy decisions.
        
        This loop only activates after the warmup period when
        baseline metrics have been captured.
        """
        logger.info("Starting enforcement loop")
        
        while self.running:
            try:
                # Wait for baseline before enforcing
                if self.baseline_metrics is None:
                    await asyncio.sleep(1)
                    continue
                
                # Get current metrics
                current = await self.telemetry.get_current()
                
                # Get policy decisions
                decisions = await self.policy.evaluate(
                    baseline=self.baseline_metrics,
                    current=current
                )
                
                # Apply enforcement actions
                for decision in decisions:
                    if decision.action_required:
                        result = await self.enforcement.apply(decision)
                        
                        # Log the action
                        await self.database.log_action(
                            action=decision.action_type,
                            subsystem=decision.subsystem,
                            parameters=decision.parameters,
                            outcome="success" if result.success else "failure",
                            details=result.details
                        )
                        
                        # Generate proof artifact if significant reduction
                        if result.success and result.reduction_percent >= 10:
                            await self.proof_generator.generate(
                                subsystem=decision.subsystem,
                                baseline=self.baseline_metrics,
                                current=current,
                                reduction=result.reduction_percent
                            )
                
            except Exception as e:
                logger.error(f"Enforcement error: {e}")
            
            await asyncio.sleep(self.config.telemetry.sample_interval)
    
    async def _run_watchdog_loop(self) -> None:
        """
        Monitor system stability and trigger rollback if needed.
        
        Safety: This loop ensures we don't destabilize the system
        by checking for OOM conditions and critical service failures.
        """
        if not self.config.safety.watchdog_enabled:
            logger.info("Watchdog disabled")
            return
        
        logger.info("Starting watchdog loop")
        
        while self.running:
            try:
                # Check for OOM conditions
                oom_detected = await self._check_oom_conditions()
                if oom_detected:
                    logger.warning("OOM condition detected, triggering rollback")
                    await self._emergency_rollback("oom_detected")
                
                # Check critical services
                critical_down = await self._check_critical_services()
                if critical_down:
                    logger.warning(f"Critical service down: {critical_down}")
                    await self._emergency_rollback(f"critical_service_down:{critical_down}")
                
            except Exception as e:
                logger.error(f"Watchdog error: {e}")
            
            await asyncio.sleep(self.config.safety.watchdog_interval)
    
    async def _check_oom_conditions(self) -> bool:
        """
        Check if OOM killer is active or imminent.
        
        Returns True if we should rollback to prevent system instability.
        """
        try:
            # Check for recent OOM kills in dmesg (simplified check)
            # In production, use netlink socket for real-time OOM notifications
            meminfo = await self.telemetry.get_meminfo()
            
            # If available memory is critically low, consider it OOM risk
            available_mb = meminfo.get("available_mb", float("inf"))
            if available_mb < 100:  # Less than 100MB available
                return True
            
            return False
        except Exception:
            return False
    
    async def _check_critical_services(self) -> Optional[str]:
        """
        Check if critical system services are running.
        
        Returns service name if a critical service is down, None otherwise.
        Only checks for services that were running at startup.
        """
        # Only check for systemd/init - sshd is optional on desktops
        critical_services = ["systemd"]
        
        for service in critical_services:
            try:
                # Check if process exists
                found = False
                for pid in os.listdir("/proc"):
                    if not pid.isdigit():
                        continue
                    try:
                        with open(f"/proc/{pid}/comm", "r") as f:
                            comm = f.read().strip()
                            if service in comm:
                                found = True
                                break
                    except (FileNotFoundError, PermissionError):
                        continue
                
                if not found:
                    return service
                    
            except Exception:
                pass
        
        return None
    
    async def _emergency_rollback(self, reason: str) -> None:
        """
        Perform emergency rollback of all enforcement actions.
        
        Safety: This is the nuclear option - removes all cgroup
        restrictions and restores default scheduler settings.
        """
        logger.warning(f"Emergency rollback triggered: {reason}")
        
        try:
            await self.enforcement.rollback_all()
            
            await self.database.log_action(
                action="emergency_rollback",
                subsystem="core",
                parameters={"reason": reason},
                outcome="success"
            )
            
            # Notify API clients
            await self.api_server.broadcast_alert({
                "type": "emergency_rollback",
                "reason": reason,
                "timestamp": datetime.utcnow().isoformat()
            })
            
        except Exception as e:
            logger.exception(f"Rollback failed: {e}")
    
    def _signal_handler(self) -> None:
        """Handle shutdown signals gracefully."""
        logger.info("Shutdown signal received")
        self.running = False
        self._shutdown_event.set()
    
    async def _shutdown(self) -> None:
        """Clean shutdown of all subsystems."""
        logger.info("Shutting down Grey Optimizer...")
        
        try:
            # Stop API server
            await self.api_server.stop()
            
            # Rollback enforcement if configured
            if not self.config.simulation_mode:
                await self.enforcement.rollback_all()
            
            # Log shutdown
            await self.database.log_action(
                action="daemon_stop",
                subsystem="core",
                parameters={},
                outcome="success"
            )
            
            # Close database
            await self.database.close()
            
        except Exception as e:
            logger.error(f"Shutdown error: {e}")
        
        logger.info("Grey Optimizer shutdown complete")


async def main() -> None:
    """Main entry point for the daemon."""
    import argparse
    
    parser = argparse.ArgumentParser(
        description="Grey Optimizer - Cross-platform resource optimization daemon"
    )
    parser.add_argument(
        "--config", "-c",
        type=str,
        default=os.environ.get("GREY_OPTIMIZER_CONFIG", "config.yaml"),
        help="Path to configuration file"
    )
    parser.add_argument(
        "--simulation", "-s",
        action="store_true",
        default=os.environ.get("GREY_OPTIMIZER_SIMULATION", "").lower() == "true",
        help="Run in simulation mode (no real enforcement)"
    )
    parser.add_argument(
        "--immediate", "-i",
        action="store_true",
        help="Apply enforcement immediately after startup (for service mode)"
    )
    parser.add_argument(
        "--verbose", "-v",
        action="store_true",
        help="Enable verbose logging"
    )
    
    args = parser.parse_args()
    
    # Configure logging based on verbose flag
    root_logger = logging.getLogger()
    root_logger.handlers.clear()
    
    if args.verbose:
        # Verbose mode: log to console
        console_handler = logging.StreamHandler(sys.stdout)
        console_handler.setFormatter(logging.Formatter("%(asctime)s [%(levelname)s] %(name)s: %(message)s"))
        root_logger.addHandler(console_handler)
        root_logger.setLevel(logging.DEBUG)
    else:
        # Normal mode: log to file only, no console output
        log_dir = os.environ.get("GREY_OPTIMIZER_LOG_DIR", "/var/log/grey-optimizer")
        if not os.path.exists(log_dir):
            try:
                os.makedirs(log_dir, exist_ok=True)
            except PermissionError:
                log_dir = "/tmp"
        log_file = os.path.join(log_dir, "daemon.log")
        try:
            file_handler = logging.FileHandler(log_file)
            file_handler.setFormatter(logging.Formatter("%(asctime)s [%(levelname)s] %(name)s: %(message)s"))
            root_logger.addHandler(file_handler)
        except PermissionError:
            # Fall back to NullHandler if can't write to log file
            root_logger.addHandler(logging.NullHandler())
    
    # Platform info
    logger.info(f"Platform: {platform.system()} {platform.release()} ({platform.machine()})")
    if IS_LINUX:
        logger.info("Full enforcement capabilities available (cgroups, KSM, scheduler)")
    elif IS_MACOS:
        logger.info("Limited enforcement capabilities (renice, sparse files)")
    elif IS_WINDOWS:
        logger.info("Windows enforcement mode (Job Objects, priority class)")
    
    # Load configuration
    config = Config.load(args.config)
    
    # Apply command-line overrides
    if args.simulation:
        config.simulation_mode = True
    if args.verbose:
        config.verbose = True
    if args.immediate:
        config.immediate_enforcement = True
    
    # Create and run daemon
    daemon = GreyOptimizerDaemon(config)
    await daemon.start()


if __name__ == "__main__":
    asyncio.run(main())
