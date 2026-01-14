"""
API Server

Provides REST endpoints and WebSocket connections for the
Grey Optimizer dashboard and external integrations.

Uses aiohttp for async HTTP handling.
"""

import asyncio
import json
import logging
import os
from datetime import datetime
from typing import Dict, Any, List, Set, Optional
from aiohttp import web, WSMsgType

from ..config import APIConfig

logger = logging.getLogger(__name__)


class APIServer:
    """
    Async HTTP/WebSocket server for Grey Optimizer.
    
    Provides:
    - GET /api/status - Current daemon status
    - GET /api/metrics - Current metrics
    - GET /api/metrics/history - Historical metrics
    - GET /api/config - Current configuration
    - POST /api/config - Update configuration
    - POST /api/rollback - Trigger rollback
    - GET /api/proofs - List proof artifacts
    - GET /api/proofs/:id - Get specific proof
    - GET /api/audit - Audit log entries
    - WS /ws - Real-time metrics stream
    """
    
    def __init__(
        self,
        config: APIConfig,
        telemetry,
        policy,
        enforcement,
        database
    ):
        """
        Initialize the API server.
        
        Args:
            config: API configuration
            telemetry: TelemetryCollector instance
            policy: PolicyEngine instance
            enforcement: EnforcementManager instance
            database: Database instance
        """
        self.config = config
        self.telemetry = telemetry
        self.policy = policy
        self.enforcement = enforcement
        self.database = database
        
        # Web application
        self.app = web.Application()
        self.runner: Optional[web.AppRunner] = None
        self.site: Optional[web.TCPSite] = None
        
        # WebSocket connections
        self._ws_clients: Set[web.WebSocketResponse] = set()
        
        # Setup routes
        self._setup_routes()
        
        logger.info(f"API server initialized ({config.host}:{config.port})")
    
    def _setup_routes(self) -> None:
        """Configure API routes."""
        self.app.router.add_get("/api/status", self._handle_status)
        self.app.router.add_get("/api/metrics", self._handle_metrics)
        self.app.router.add_get("/api/metrics/history", self._handle_metrics_history)
        self.app.router.add_get("/api/config", self._handle_get_config)
        self.app.router.add_post("/api/config", self._handle_set_config)
        self.app.router.add_post("/api/rollback", self._handle_rollback)
        self.app.router.add_get("/api/proofs", self._handle_list_proofs)
        self.app.router.add_get("/api/proofs/{proof_id}", self._handle_get_proof)
        self.app.router.add_get("/api/audit", self._handle_audit_log)
        self.app.router.add_get("/api/decisions", self._handle_decisions)
        self.app.router.add_get("/ws", self._handle_websocket)
        
        # Static files for frontend (if built)
        frontend_paths = [
            "frontend/dist",
            "../frontend/dist",
            "../../frontend/dist",
        ]
        for frontend_path in frontend_paths:
            if os.path.isdir(frontend_path):
                self.app.router.add_static("/", frontend_path, show_index=True)
                logger.info(f"Serving frontend from: {frontend_path}")
                break
        else:
            logger.warning("Frontend not found - API-only mode (no static files)")
        
        # CORS middleware
        if self.config.cors_enabled:
            self.app.middlewares.append(self._cors_middleware)
    
    @web.middleware
    async def _cors_middleware(self, request: web.Request, handler) -> web.Response:
        """Add CORS headers for development."""
        if request.method == "OPTIONS":
            response = web.Response()
        else:
            response = await handler(request)
        
        origin = request.headers.get("Origin", "")
        if origin in self.config.cors_origins or "*" in self.config.cors_origins:
            response.headers["Access-Control-Allow-Origin"] = origin
            response.headers["Access-Control-Allow-Methods"] = "GET, POST, OPTIONS"
            response.headers["Access-Control-Allow-Headers"] = "Content-Type"
        
        return response
    
    async def start(self) -> None:
        """Start the API server."""
        self.runner = web.AppRunner(self.app)
        await self.runner.setup()
        
        self.site = web.TCPSite(
            self.runner,
            self.config.host,
            self.config.port
        )
        await self.site.start()
        
        logger.info(f"API server listening on http://{self.config.host}:{self.config.port}")
        
        # Keep running until cancelled
        while True:
            await asyncio.sleep(3600)
    
    async def stop(self) -> None:
        """Stop the API server."""
        # Close all WebSocket connections
        for ws in list(self._ws_clients):
            await ws.close()
        
        # Stop the site
        if self.site:
            await self.site.stop()
        
        # Cleanup runner
        if self.runner:
            await self.runner.cleanup()
        
        logger.info("API server stopped")
    
    async def broadcast_metrics(self, metrics) -> None:
        """
        Broadcast metrics to all WebSocket clients.
        
        Args:
            metrics: MetricsSnapshot to broadcast
        """
        if not self._ws_clients:
            return
        
        message = json.dumps({
            "type": "metrics",
            "data": {
                "timestamp": metrics.timestamp.isoformat(),
                "cpu": metrics.cpu,
                "ram": metrics.ram,
                "disk": metrics.disk
            }
        })
        
        # Send to all connected clients
        dead_clients = set()
        for ws in self._ws_clients:
            try:
                await ws.send_str(message)
            except Exception:
                dead_clients.add(ws)
        
        # Remove dead connections
        self._ws_clients -= dead_clients
    
    async def broadcast_alert(self, alert: Dict[str, Any]) -> None:
        """
        Broadcast an alert to all WebSocket clients.
        
        Args:
            alert: Alert data to broadcast
        """
        if not self._ws_clients:
            return
        
        message = json.dumps({
            "type": "alert",
            "data": alert
        })
        
        for ws in list(self._ws_clients):
            try:
                await ws.send_str(message)
            except Exception:
                self._ws_clients.discard(ws)
    
    # === Route Handlers ===
    
    async def _handle_status(self, request: web.Request) -> web.Response:
        """GET /api/status - Daemon status."""
        return web.json_response({
            "status": "running",
            "version": "1.0.0",
            "simulation_mode": self.enforcement.simulation,
            "uptime_seconds": 0,  # TODO: Track uptime
            "websocket_clients": len(self._ws_clients)
        })
    
    async def _handle_metrics(self, request: web.Request) -> web.Response:
        """GET /api/metrics - Current metrics."""
        try:
            metrics = await self.telemetry.get_current()
            return web.json_response(metrics)
        except Exception as e:
            return web.json_response({"error": str(e)}, status=500)
    
    async def _handle_metrics_history(self, request: web.Request) -> web.Response:
        """GET /api/metrics/history - Historical metrics."""
        try:
            seconds = int(request.query.get("seconds", "60"))
            history = self.telemetry.get_history(seconds)
            return web.json_response({"history": history})
        except Exception as e:
            return web.json_response({"error": str(e)}, status=500)
    
    async def _handle_get_config(self, request: web.Request) -> web.Response:
        """GET /api/config - Current configuration."""
        # Return non-sensitive config
        # Note: In a real implementation, access config through the telemetry/enforcement objects
        return web.json_response({
            "simulation_mode": self.enforcement.simulation,
            "cpu_enabled": True,
            "ram_enabled": True,
            "disk_enabled": True
        })
    
    async def _handle_set_config(self, request: web.Request) -> web.Response:
        """POST /api/config - Update configuration."""
        try:
            data = await request.json()
            # Validate and apply configuration changes
            # This is a simplified implementation
            return web.json_response({"status": "ok", "updated": list(data.keys())})
        except Exception as e:
            return web.json_response({"error": str(e)}, status=400)
    
    async def _handle_rollback(self, request: web.Request) -> web.Response:
        """POST /api/rollback - Trigger rollback."""
        try:
            await self.enforcement.rollback_all()
            return web.json_response({"status": "ok", "message": "Rollback completed"})
        except Exception as e:
            return web.json_response({"error": str(e)}, status=500)
    
    async def _handle_list_proofs(self, request: web.Request) -> web.Response:
        """GET /api/proofs - List proof artifacts."""
        try:
            proofs = await self.database.get_proofs(limit=100)
            return web.json_response({"proofs": proofs})
        except Exception as e:
            return web.json_response({"error": str(e)}, status=500)
    
    async def _handle_get_proof(self, request: web.Request) -> web.Response:
        """GET /api/proofs/:id - Get specific proof."""
        proof_id = request.match_info.get("proof_id")
        try:
            proof = await self.database.get_proof(proof_id)
            if proof:
                return web.json_response(proof)
            return web.json_response({"error": "Proof not found"}, status=404)
        except Exception as e:
            return web.json_response({"error": str(e)}, status=500)
    
    async def _handle_audit_log(self, request: web.Request) -> web.Response:
        """GET /api/audit - Audit log entries."""
        try:
            limit = int(request.query.get("limit", "100"))
            entries = await self.database.get_audit_log(limit)
            return web.json_response({"entries": entries})
        except Exception as e:
            return web.json_response({"error": str(e)}, status=500)
    
    async def _handle_decisions(self, request: web.Request) -> web.Response:
        """GET /api/decisions - Policy decision history."""
        try:
            limit = int(request.query.get("limit", "100"))
            decisions = self.policy.get_decision_history(limit)
            return web.json_response({"decisions": decisions})
        except Exception as e:
            return web.json_response({"error": str(e)}, status=500)
    
    async def _handle_websocket(self, request: web.Request) -> web.WebSocketResponse:
        """WS /ws - Real-time metrics stream."""
        ws = web.WebSocketResponse()
        await ws.prepare(request)
        
        self._ws_clients.add(ws)
        logger.info(f"WebSocket client connected ({len(self._ws_clients)} total)")
        
        try:
            # Send initial status
            await ws.send_str(json.dumps({
                "type": "connected",
                "data": {
                    "simulation_mode": self.enforcement.simulation,
                    "timestamp": datetime.utcnow().isoformat()
                }
            }))
            
            # Handle incoming messages
            async for msg in ws:
                if msg.type == WSMsgType.TEXT:
                    try:
                        data = json.loads(msg.data)
                        # Handle client commands if needed
                        if data.get("command") == "ping":
                            await ws.send_str(json.dumps({"type": "pong"}))
                    except json.JSONDecodeError:
                        pass
                elif msg.type == WSMsgType.ERROR:
                    logger.error(f"WebSocket error: {ws.exception()}")
                    break
                    
        finally:
            self._ws_clients.discard(ws)
            logger.info(f"WebSocket client disconnected ({len(self._ws_clients)} total)")
        
        return ws
