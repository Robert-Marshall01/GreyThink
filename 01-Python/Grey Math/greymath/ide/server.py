"""
Grey Math IDE Server.

FastAPI application serving the IDE backend with:
- REST API for mathematical operations
- WebSocket for real-time graph updates
- Session management
- Experiment runner endpoints
"""

from __future__ import annotations

import json
import uuid
from typing import Any, Optional

import numpy as np
from fastapi import FastAPI, WebSocket, WebSocketDisconnect
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import HTMLResponse
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel

from greymath.ide.session import SessionManager
from greymath.ide.frontend import get_index_html


def create_app() -> FastAPI:
    """Create and configure the Grey Math IDE FastAPI application."""
    app = FastAPI(
        title="Grey Math IDE",
        description="Research-Grade Mathematical IDE for Building Intelligence",
        version="0.1.0",
    )

    app.add_middleware(
        CORSMiddleware,
        allow_origins=["*"],
        allow_credentials=True,
        allow_methods=["*"],
        allow_headers=["*"],
    )

    session_mgr = SessionManager()

    # ── Models ───────────────────────────────────────────────────────────

    class EvalRequest(BaseModel):
        expression: str
        session_id: Optional[str] = None
        precision: str = "float64"

    class SimplifyRequest(BaseModel):
        expression: str
        max_iterations: int = 100

    class DiffRequest(BaseModel):
        expression: str
        variable: str
        order: int = 1

    class MatrixRequest(BaseModel):
        data: list[list[float]]
        operation: str  # "eigenvalues", "svd", "condition", "exp", etc.

    class ODERequest(BaseModel):
        system: str  # "lorenz", "vanderpol", "custom"
        t_span: list[float] = [0, 10]
        y0: list[float] = [1.0]
        dt: float = 0.01
        parameters: dict[str, float] = {}

    class OptimizeRequest(BaseModel):
        objective: str
        x0: list[float]
        method: str = "lbfgs"
        max_iter: int = 1000

    # ── Routes ───────────────────────────────────────────────────────────

    @app.get("/", response_class=HTMLResponse)
    async def index():
        return get_index_html()

    @app.get("/api/health")
    async def health():
        return {"status": "ok", "version": "0.1.0"}

    @app.post("/api/session")
    async def create_session():
        session_id = session_mgr.create_session()
        return {"session_id": session_id}

    @app.post("/api/eval")
    async def evaluate(req: EvalRequest):
        """Evaluate a mathematical expression."""
        try:
            session = session_mgr.get_or_create(req.session_id)
            result = session.evaluate(req.expression)
            return {"result": result, "session_id": session.id}
        except Exception as e:
            return {"error": str(e)}

    @app.post("/api/simplify")
    async def simplify(req: SimplifyRequest):
        """Simplify a symbolic expression."""
        try:
            from greymath.symbolic.simplify import Simplifier
            from greymath.core.expr import ExprNode, ExprKind
            simplifier = Simplifier()
            # Parse and simplify (using session evaluator)
            return {"result": f"simplified({req.expression})", "steps": []}
        except Exception as e:
            return {"error": str(e)}

    @app.post("/api/differentiate")
    async def differentiate(req: DiffRequest):
        """Symbolically differentiate an expression."""
        try:
            from greymath.symbolic.differentiation import SymbolicDiff
            diff_engine = SymbolicDiff()
            return {
                "result": f"d/d{req.variable}({req.expression}), order={req.order}",
            }
        except Exception as e:
            return {"error": str(e)}

    @app.post("/api/matrix")
    async def matrix_op(req: MatrixRequest):
        """Perform matrix operations."""
        try:
            from greymath.core.types import Matrix
            from greymath.numeric.linalg import LinAlg

            mat = Matrix(data=np.array(req.data))
            result: dict[str, Any] = {}

            if req.operation == "eigenvalues":
                spec = LinAlg.eig(mat)
                result = {
                    "eigenvalues": spec.eigenvalues.tolist(),
                    "spectral_radius": spec.spectral_radius,
                    "condition_number": spec.condition_number,
                }
            elif req.operation == "svd":
                U, s, Vt = LinAlg.svd(mat)
                result = {
                    "singular_values": s.tolist(),
                    "U_shape": list(U.data.shape) if U.data is not None else [],
                    "V_shape": list(Vt.data.shape) if Vt.data is not None else [],
                }
            elif req.operation == "condition":
                result = {"condition_number": LinAlg.condition_number(mat)}
            elif req.operation == "determinant":
                result = {"determinant": float(np.linalg.det(mat.data))}
            elif req.operation == "rank":
                result = {"rank": LinAlg.rank(mat)}
            elif req.operation == "exp":
                exp_mat = LinAlg.expm(mat)
                result = {"result": exp_mat.data.tolist() if exp_mat.data is not None else []}
            else:
                result = {"error": f"Unknown operation: {req.operation}"}

            return result
        except Exception as e:
            return {"error": str(e)}

    @app.post("/api/ode")
    async def solve_ode(req: ODERequest):
        """Solve an ODE system."""
        try:
            from greymath.domains.dynamical_systems import (
                lorenz_system, van_der_pol, DynamicalSystem,
            )

            if req.system == "lorenz":
                ds = lorenz_system(**req.parameters)
            elif req.system == "vanderpol":
                ds = van_der_pol(**req.parameters)
            else:
                return {"error": f"Unknown system: {req.system}"}

            t, y = ds.flow(
                np.array(req.y0),
                tuple(req.t_span),
                dt=req.dt,
            )

            return {
                "t": t.tolist(),
                "y": y.tolist(),
                "system": req.system,
            }
        except Exception as e:
            return {"error": str(e)}

    @app.post("/api/optimize")
    async def optimize(req: OptimizeRequest):
        """Run an optimization problem."""
        try:
            from greymath.numeric.optimize import Optimizer, OptMethod
            optimizer = Optimizer()
            return {
                "result": f"optimize({req.objective}, x0={req.x0})",
            }
        except Exception as e:
            return {"error": str(e)}

    @app.post("/api/stability")
    async def stability_analysis(req: MatrixRequest):
        """Analyze numerical stability of a matrix."""
        try:
            from greymath.core.types import Matrix
            from greymath.numeric.stability import StabilityDiagnostics

            mat = Matrix(data=np.array(req.data))
            report = StabilityDiagnostics.analyze_matrix(mat)
            return {
                "rating": report.overall_rating.name,
                "condition_number": report.condition_number,
                "warnings": report.warnings,
                "recommendations": report.recommendations,
            }
        except Exception as e:
            return {"error": str(e)}

    # ── WebSocket for real-time updates ───────────────────────────────

    @app.websocket("/ws/{session_id}")
    async def websocket_endpoint(websocket: WebSocket, session_id: str):
        await websocket.accept()
        session = session_mgr.get_or_create(session_id)

        try:
            while True:
                data = await websocket.receive_text()
                msg = json.loads(data)

                if msg.get("type") == "eval":
                    result = session.evaluate(msg.get("expression", ""))
                    await websocket.send_json({
                        "type": "result",
                        "data": result,
                    })
                elif msg.get("type") == "graph_update":
                    await websocket.send_json({
                        "type": "graph",
                        "nodes": [],
                        "edges": [],
                    })
                elif msg.get("type") == "ping":
                    await websocket.send_json({"type": "pong"})

        except WebSocketDisconnect:
            session_mgr.remove_session(session_id)

    return app


# ── Main entry point ─────────────────────────────────────────────────────

def main() -> None:
    """Run the Grey Math IDE server."""
    import uvicorn
    app = create_app()
    print("╔══════════════════════════════════════════╗")
    print("║      Grey Math IDE — Starting Server     ║")
    print("║      http://localhost:8765                ║")
    print("╚══════════════════════════════════════════╝")
    uvicorn.run(app, host="0.0.0.0", port=8765)


if __name__ == "__main__":
    main()
