"""
Grey Optimizer - Unified Daemon

Asyncio-based daemon that orchestrates:
- Telemetry collection (snapshots from /proc, cgroups)
- Policy engine (decides when/how to reclaim memory)
- Enforcement coordinator (applies reclamation strategies)
- Health endpoint (HTTP on localhost)
- WebSocket for real-time dashboard updates

This daemon integrates all modules:
- memory_reclaim.py: RAM reclamation strategies
- telemetry.py: Memory snapshot collection
- audit.py: Append-only audit with HMAC checkpoints
- health.py: Watchdog for auto-rollback

Safety Principle: Simulation by default, --confirm-live for real actions.

Usage:
    python grey_daemon.py --simulation
    python grey_daemon.py --confirm-live  # Requires root
    python grey_daemon.py --mode=simulation --enforcement-interval 30
"""

import asyncio
import json
import logging
import os
import signal
import sys
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List, Optional
import platform

# Handle imports for both module and standalone execution
try:
    from .memory_reclaim import MemoryReclaimer, ReclaimResult
    from .telemetry import TelemetrySnapshotter, MemorySnapshot, SnapshotComparison, SnapshotStore
    from .audit import AuditDatabase
    from .health import HealthWatchdog, QuickHealthCheck
except ImportError:
    # Standalone execution - add parent to path
    sys.path.insert(0, str(Path(__file__).parent))
    from memory_reclaim import MemoryReclaimer, ReclaimResult
    from telemetry import TelemetrySnapshotter, MemorySnapshot, SnapshotComparison, SnapshotStore
    from audit import AuditDatabase
    from health import HealthWatchdog, QuickHealthCheck

# Configure logger
logger = logging.getLogger("grey-optimizer")

# Version
VERSION = "2.0.0"

# Default paths based on OS
if sys.platform == "win32":
    DEFAULT_DATA_DIR = Path(os.environ.get("PROGRAMDATA", "C:\\ProgramData")) / "GreyOptimizer"
    DEFAULT_LOG_FILE = DEFAULT_DATA_DIR / "grey-optimizer.log"
else:
    DEFAULT_DATA_DIR = Path("/var/lib/grey-optimizer")
    DEFAULT_LOG_FILE = Path("/var/log/grey-optimizer.log")

DEFAULT_DB_PATH = Path(os.environ.get(
    "GREY_DB_PATH",
    str(DEFAULT_DATA_DIR / "grey.db")
))
DEFAULT_AUDIT_PATH = Path(os.environ.get(
    "GREY_AUDIT_PATH",
    str(DEFAULT_DATA_DIR / "audit.db")
))
DEFAULT_ARTIFACTS_DIR = Path(os.environ.get(
    "GREY_ARTIFACTS_DIR",
    str(DEFAULT_DATA_DIR / "artifacts")
))
DEFAULT_HTTP_PORT = int(os.environ.get("GREY_HTTP_PORT", "8090"))


@dataclass
class DaemonConfig:
    """Configuration for the daemon."""
    simulation: bool = True
    confirm_live: bool = False
    warmup_seconds: float = 10.0
    sample_interval: float = 5.0
    enforcement_interval: float = 60.0  # Run enforcement every N seconds
    continuous: bool = True  # Run enforcement continuously vs once
    http_port: int = DEFAULT_HTTP_PORT
    http_host: str = "127.0.0.1"
    db_path: Path = DEFAULT_DB_PATH
    audit_path: Path = DEFAULT_AUDIT_PATH
    artifacts_dir: Path = DEFAULT_ARTIFACTS_DIR
    watchdog_enabled: bool = True
    watchdog_interval: float = 5.0
    target_pids: Optional[List[int]] = None
    reclaim_strategies: List[str] = None
    
    def __post_init__(self):
        if self.reclaim_strategies is None:
            self.reclaim_strategies = ["drop_caches", "ksm"]


class ProofArtifact:
    """
    Generates proof artifacts that demonstrate RAM reclamation.
    
    Artifacts are JSON files with:
    - Baseline snapshot
    - Post-enforcement snapshot
    - Calculated savings
    - HMAC signature for integrity
    """
    
    def __init__(self, artifacts_dir: Path, audit_db: AuditDatabase):
        self.artifacts_dir = artifacts_dir
        self.audit_db = audit_db
        self.artifacts_dir.mkdir(parents=True, exist_ok=True)
    
    async def generate(
        self,
        baseline: MemorySnapshot,
        post: MemorySnapshot,
        reclaim_results: List[ReclaimResult],
        simulated: bool = True,
    ) -> Path:
        """
        Generate a proof artifact.
        
        Args:
            baseline: Snapshot before enforcement
            post: Snapshot after enforcement
            reclaim_results: Results from each reclamation strategy
            simulated: Whether this was a simulation
            
        Returns:
            Path to the generated artifact
        """
        comparison = SnapshotComparison(baseline=baseline, post=post)
        
        artifact = {
            "version": "1.0",
            "generated_at": datetime.now(timezone.utc).isoformat(),
            "simulated": simulated,
            "platform": {
                "system": platform.system(),
                "release": platform.release(),
                "machine": platform.machine(),
            },
            "baseline": baseline.to_dict(),
            "post": post.to_dict(),
            "savings": {
                "system_reduction_mb": round(comparison.system_reduction_mb, 2),
                "system_reduction_percent": round(comparison.system_reduction_percent, 2),
                "cache_reduction_mb": round(comparison.cache_reduction_bytes / (1024 * 1024), 2),
                "process_rss_reduction_mb": round(comparison.process_rss_reduction / (1024 * 1024), 2),
                "process_pss_reduction_mb": round(comparison.process_pss_reduction / (1024 * 1024), 2),
            },
            "strategies": [r.to_dict() for r in reclaim_results],
            "per_process_changes": comparison.get_per_process_comparison()[:20],  # Top 20
        }
        
        # Generate HMAC if not simulated
        if not simulated:
            data = json.dumps(artifact, sort_keys=True)
            artifact["hmac"] = self.audit_db.hmac.sign(data)
        else:
            artifact["hmac"] = None
            artifact["note"] = "Simulated - no HMAC signature"
        
        # Save artifact
        timestamp = datetime.now(timezone.utc).strftime("%Y%m%d_%H%M%S")
        filename = f"proof_{timestamp}_{'sim' if simulated else 'live'}.json"
        artifact_path = self.artifacts_dir / filename
        
        with open(artifact_path, "w") as f:
            json.dump(artifact, f, indent=2)
        
        logger.info(f"Generated proof artifact: {artifact_path}")
        return artifact_path


class SimpleHTTPServer:
    """
    Simple HTTP server for health endpoints and status page.
    
    Provides:
    - GET /health - Quick health check
    - GET /api/status - Current daemon status
    - GET /api/metrics - Current telemetry metrics
    - GET /api/artifacts - List proof artifacts
    - GET /status.html - Dashboard page
    - WebSocket /ws - Real-time updates
    """
    
    def __init__(self, daemon: "GreyDaemon"):
        self.daemon = daemon
        self._server = None
        self._websockets: List[Any] = []
    
    async def start(self) -> None:
        """Start the HTTP server."""
        try:
            from aiohttp import web
            
            app = web.Application()
            app.router.add_get("/health", self._handle_health)
            app.router.add_get("/api/status", self._handle_status)
            app.router.add_get("/api/metrics", self._handle_metrics)
            app.router.add_get("/api/artifacts", self._handle_artifacts)
            app.router.add_get("/api/audit", self._handle_audit)
            app.router.add_get("/ws", self._handle_websocket)
            
            # Serve static files if available
            web_dir = Path(__file__).parent.parent / "web"
            if web_dir.exists():
                app.router.add_static("/", web_dir)
            
            runner = web.AppRunner(app)
            await runner.setup()
            
            self._server = web.TCPSite(
                runner,
                self.daemon.config.http_host,
                self.daemon.config.http_port,
            )
            await self._server.start()
            
            logger.info(f"HTTP server started on "
                       f"http://{self.daemon.config.http_host}:{self.daemon.config.http_port}")
            
        except ImportError:
            logger.warning("aiohttp not installed - HTTP server disabled")
    
    async def stop(self) -> None:
        """Stop the HTTP server."""
        if self._server:
            await self._server.stop()
    
    async def broadcast(self, data: Dict[str, Any]) -> None:
        """Broadcast data to all connected WebSocket clients."""
        if not self._websockets:
            return
        
        message = json.dumps(data)
        for ws in self._websockets[:]:
            try:
                await ws.send_str(message)
            except Exception:
                self._websockets.remove(ws)
    
    async def _handle_health(self, request):
        """Return 200 OK if daemon is healthy."""
        from aiohttp import web
        
        try:
            # Quick health check
            uptime = 0
            if self.daemon.start_time:
                uptime = (datetime.now(timezone.utc) - self.daemon.start_time).total_seconds()
            
            return web.json_response({
                "status": "healthy",
                "version": VERSION,
                "uptime_seconds": round(uptime, 1),
                "simulation_mode": self.daemon.config.simulation,
                "running": self.daemon.running,
            }, status=200)
        except Exception as e:
            return web.json_response({
                "status": "unhealthy",
                "error": str(e),
            }, status=503)
    
    async def _handle_status(self, request):
        from aiohttp import web
        
        uptime = 0
        if self.daemon.start_time:
            uptime = (datetime.now(timezone.utc) - self.daemon.start_time).total_seconds()
        
        return web.json_response({
            "status": "running",
            "version": VERSION,
            "simulation_mode": self.daemon.config.simulation,
            "baseline_captured": self.daemon.baseline is not None,
            "uptime_seconds": round(uptime, 1),
            "watchdog": self.daemon.watchdog.get_summary() if self.daemon.watchdog else None,
        })
    
    async def _handle_metrics(self, request):
        from aiohttp import web
        
        if self.daemon.current_snapshot:
            return web.json_response(self.daemon.current_snapshot.to_dict())
        return web.json_response({"error": "No metrics available"})
    
    async def _handle_artifacts(self, request):
        from aiohttp import web
        
        artifacts = []
        if self.daemon.config.artifacts_dir.exists():
            for f in sorted(self.daemon.config.artifacts_dir.glob("proof_*.json"), reverse=True):
                try:
                    with open(f) as fp:
                        data = json.load(fp)
                        artifacts.append({
                            "filename": f.name,
                            "generated_at": data.get("generated_at"),
                            "simulated": data.get("simulated"),
                            "savings_mb": data.get("savings", {}).get("system_reduction_mb"),
                        })
                except Exception:
                    pass
        
        return web.json_response({"artifacts": artifacts[:20]})
    
    async def _handle_audit(self, request):
        from aiohttp import web
        
        entries = await self.daemon.audit.get_recent_entries(limit=50)
        return web.json_response({"entries": entries})
    
    async def _handle_websocket(self, request):
        from aiohttp import web
        
        ws = web.WebSocketResponse()
        await ws.prepare(request)
        self._websockets.append(ws)
        
        try:
            async for msg in ws:
                pass  # We only send, don't receive
        finally:
            self._websockets.remove(ws)
        
        return ws


class GreyDaemon:
    """
    Main daemon class orchestrating all Grey Optimizer components.
    
    Lifecycle:
    1. Initialize subsystems
    2. Capture baseline during warmup period
    3. Apply enforcement strategies
    4. Capture post snapshot and generate proof artifact
    5. Monitor health and auto-rollback if needed
    """
    
    def __init__(self, config: DaemonConfig):
        self.config = config
        
        # Ensure directories exist
        config.db_path.parent.mkdir(parents=True, exist_ok=True)
        config.artifacts_dir.mkdir(parents=True, exist_ok=True)
        
        # Initialize subsystems
        self.reclaimer = MemoryReclaimer(
            simulation=config.simulation,
            backup_enabled=True,
        )
        
        self.snapshotter = TelemetrySnapshotter(
            target_pids=config.target_pids,
            track_all_processes=True,
            min_rss_mb=10.0,
        )
        
        self.snapshot_store = SnapshotStore(config.db_path)
        
        self.audit = AuditDatabase(config.audit_path)
        
        self.proof_generator = ProofArtifact(config.artifacts_dir, self.audit)
        
        self.watchdog: Optional[HealthWatchdog] = None
        if config.watchdog_enabled:
            self.watchdog = HealthWatchdog(
                rollback_callback=self._do_rollback,
                check_interval=config.watchdog_interval,
            )
        
        self.http_server = SimpleHTTPServer(self)
        
        # State
        self.running = False
        self.start_time: Optional[datetime] = None
        self.baseline: Optional[MemorySnapshot] = None
        self.current_snapshot: Optional[MemorySnapshot] = None
        self._shutdown_event = asyncio.Event()
    
    async def start(self) -> None:
        """Start the daemon and all subsystems."""
        logger.info("Starting Grey Daemon...")
        logger.info(f"Mode: {'SIMULATION' if self.config.simulation else 'LIVE'}")
        
        self.running = True
        self.start_time = datetime.now(timezone.utc)
        
        # Initialize databases
        await self.snapshot_store.initialize()
        await self.audit.initialize()
        
        # Log startup
        await self.audit.log(
            action="daemon_start",
            subsystem="core",
            parameters={
                "simulation": self.config.simulation,
                "warmup_seconds": self.config.warmup_seconds,
            },
            outcome="success",
        )
        
        # Setup signal handlers
        loop = asyncio.get_event_loop()
        for sig in (signal.SIGTERM, signal.SIGINT):
            loop.add_signal_handler(sig, self._handle_signal)
        
        try:
            # Start HTTP server
            await self.http_server.start()
            
            # Start watchdog
            if self.watchdog:
                await self.watchdog.start()
            
            # Run main loop
            await self._main_loop()
            
        except asyncio.CancelledError:
            logger.info("Daemon cancelled")
        except Exception as e:
            logger.exception(f"Daemon error: {e}")
        finally:
            await self._shutdown()
    
    async def _main_loop(self) -> None:
        """Main daemon loop."""
        warmup_remaining = self.config.warmup_seconds
        last_enforcement_time = 0.0
        enforcement_count = 0
        
        while self.running:
            try:
                current_time = asyncio.get_event_loop().time()
                
                # Take snapshot
                self.current_snapshot = await self.snapshotter.take_snapshot(
                    source="periodic" if self.baseline else "warmup"
                )
                
                # Broadcast to websocket clients
                await self.http_server.broadcast({
                    "type": "metrics",
                    "data": self.current_snapshot.to_dict(),
                })
                
                # Warmup phase
                if warmup_remaining > 0:
                    logger.info(f"Warmup: {warmup_remaining:.1f}s remaining")
                    warmup_remaining -= self.config.sample_interval
                    
                    if warmup_remaining <= 0:
                        # Capture baseline
                        self.baseline = await self.snapshotter.take_snapshot(source="baseline")
                        await self.snapshot_store.save_snapshot(self.baseline)
                        
                        logger.info(f"Baseline captured: "
                                   f"Used={self.baseline.used_mb:.0f}MB, "
                                   f"Available={self.baseline.available_mb:.0f}MB")
                        
                        await self.audit.log(
                            action="baseline_captured",
                            subsystem="telemetry",
                            parameters={"used_mb": self.baseline.used_mb},
                            outcome="success",
                        )
                        last_enforcement_time = current_time  # Start enforcement timer
                    
                    await asyncio.sleep(self.config.sample_interval)
                    continue
                
                # Enforcement phase - run periodically based on config
                time_since_last = current_time - last_enforcement_time
                should_enforce = (
                    (self.config.continuous and time_since_last >= self.config.enforcement_interval) or
                    (not self.config.continuous and enforcement_count == 0)
                )
                
                if should_enforce:
                    enforcement_count += 1
                    logger.info(f"Running enforcement cycle #{enforcement_count}")
                    await self._run_enforcement()
                    last_enforcement_time = current_time
                    
                    if self.config.continuous:
                        logger.info(f"Next enforcement in {self.config.enforcement_interval:.0f}s")
                
                await asyncio.sleep(self.config.sample_interval)
                
            except Exception as e:
                logger.error(f"Main loop error: {e}")
                await asyncio.sleep(self.config.sample_interval)
    
    async def _run_enforcement(self) -> None:
        """Run memory reclamation enforcement."""
        logger.info("Starting enforcement...")
        
        # Apply reclamation strategies
        results = await self.reclaimer.reclaim_all(
            target_pids=self.config.target_pids,
            confirm_live=self.config.confirm_live,
            strategies=self.config.reclaim_strategies,
        )
        
        # Log each result
        for result in results:
            await self.audit.log(
                action=f"reclaim_{result.strategy}",
                subsystem="memory_reclaim",
                parameters=result.details,
                outcome="simulated" if result.simulated else ("success" if result.success else "failure"),
                details={
                    "bytes_reclaimed": result.bytes_reclaimed,
                    "bytes_would_reclaim": result.bytes_would_reclaim,
                    "error": result.error,
                },
            )
        
        # Wait for memory to settle
        await asyncio.sleep(2.0)
        
        # Take post snapshot
        post = await self.snapshotter.take_snapshot(source="post")
        await self.snapshot_store.save_snapshot(post)
        
        # Generate proof artifact
        artifact_path = await self.proof_generator.generate(
            baseline=self.baseline,
            post=post,
            reclaim_results=results,
            simulated=self.config.simulation,
        )
        
        # Create signed checkpoint
        checkpoint = await self.audit.create_checkpoint(
            metadata={
                "baseline_used_mb": self.baseline.used_mb,
                "post_used_mb": post.used_mb,
                "artifact": str(artifact_path),
            }
        )
        
        # Broadcast results
        comparison = SnapshotComparison(baseline=self.baseline, post=post)
        await self.http_server.broadcast({
            "type": "enforcement_complete",
            "data": {
                "reduction_mb": comparison.system_reduction_mb,
                "reduction_percent": comparison.system_reduction_percent,
                "artifact": str(artifact_path),
                "checkpoint_id": checkpoint.id,
            },
        })
        
        logger.info(f"Enforcement complete: "
                   f"Reduction={comparison.system_reduction_mb:.1f}MB "
                   f"({comparison.system_reduction_percent:.1f}%)")
    
    async def _do_rollback(self) -> None:
        """Perform rollback of all enforcement actions."""
        logger.warning("Executing rollback...")
        
        rolled_back = await self.reclaimer.rollback_all()
        
        await self.audit.log(
            action="rollback",
            subsystem="core",
            parameters={"cgroups_rolled_back": rolled_back},
            outcome="success" if rolled_back else "partial",
        )
    
    def _handle_signal(self) -> None:
        """Handle shutdown signal."""
        logger.info("Shutdown signal received")
        self.running = False
        self._shutdown_event.set()
    
    async def _shutdown(self) -> None:
        """Clean shutdown of all subsystems."""
        logger.info("Shutting down Grey Daemon...")
        
        # Stop watchdog
        if self.watchdog:
            await self.watchdog.stop()
        
        # Stop HTTP server
        await self.http_server.stop()
        
        # Log shutdown
        await self.audit.log(
            action="daemon_stop",
            subsystem="core",
            parameters={},
            outcome="success",
        )
        
        logger.info("Grey Daemon shutdown complete")


async def main():
    """Main entry point for the daemon."""
    import argparse
    
    parser = argparse.ArgumentParser(
        description="Grey Optimizer Daemon - Adaptive RAM Reclamation"
    )
    parser.add_argument(
        "--simulation", "-s",
        action="store_true",
        default=True,
        help="Run in simulation mode (default)"
    )
    parser.add_argument(
        "--confirm-live",
        action="store_true",
        help="Enable live enforcement (requires root)"
    )
    parser.add_argument(
        "--warmup",
        type=float,
        default=10.0,
        help="Warmup period in seconds (default: 10)"
    )
    parser.add_argument(
        "--port",
        type=int,
        default=DEFAULT_HTTP_PORT,
        help=f"HTTP port (default: {DEFAULT_HTTP_PORT})"
    )
    parser.add_argument(
        "--no-watchdog",
        action="store_true",
        help="Disable health watchdog"
    )
    parser.add_argument(
        "--verbose", "-v",
        action="store_true",
        help="Verbose logging"
    )
    parser.add_argument(
        "--pid",
        type=int,
        action="append",
        dest="pids",
        help="Target PID (can be specified multiple times)"
    )
    parser.add_argument(
        "--enforcement-interval",
        type=float,
        default=60.0,
        help="Seconds between enforcement cycles (default: 60)"
    )
    parser.add_argument(
        "--once",
        action="store_true",
        help="Run enforcement only once, then just monitor"
    )
    
    args = parser.parse_args()
    
    # Configure logging
    level = logging.DEBUG if args.verbose else logging.INFO
    logging.basicConfig(
        level=level,
        format="%(asctime)s [%(levelname)s] %(name)s: %(message)s",
    )
    
    # Build config
    config = DaemonConfig(
        simulation=not args.confirm_live,
        confirm_live=args.confirm_live,
        warmup_seconds=args.warmup,
        http_port=args.port,
        watchdog_enabled=not args.no_watchdog,
        target_pids=args.pids,
        enforcement_interval=args.enforcement_interval,
        continuous=not args.once,
    )
    
    # Log configuration
    logger.info(f"Enforcement interval: {config.enforcement_interval}s")
    logger.info(f"Continuous mode: {config.continuous}")
    
    # Create and run daemon
    daemon = GreyDaemon(config)
    await daemon.start()


if __name__ == "__main__":
    asyncio.run(main())
