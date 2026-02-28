"""
Grey Math — JSON-RPC Server for agent/LLM integration.

Implements JSON-RPC 2.0 over both HTTP and WebSocket transports.
Provides structured access to all Grey Math capabilities.
"""

from __future__ import annotations

import json
import traceback
from dataclasses import dataclass, field
from typing import Any, Callable, Optional

from greymath.ide.session import SessionManager


# ── JSON-RPC types ─────────────────────────────────────────────────────────────

@dataclass
class RPCRequest:
    method: str
    params: dict[str, Any] = field(default_factory=dict)
    id: Optional[str | int] = None
    jsonrpc: str = "2.0"

    @classmethod
    def from_dict(cls, d: dict) -> "RPCRequest":
        return cls(
            method=d["method"],
            params=d.get("params", {}),
            id=d.get("id"),
            jsonrpc=d.get("jsonrpc", "2.0"),
        )


@dataclass
class RPCResponse:
    id: Optional[str | int]
    result: Any = None
    error: Optional[dict[str, Any]] = None
    jsonrpc: str = "2.0"

    def to_dict(self) -> dict[str, Any]:
        d: dict[str, Any] = {"jsonrpc": self.jsonrpc, "id": self.id}
        if self.error is not None:
            d["error"] = self.error
        else:
            d["result"] = self.result
        return d


def rpc_error(id_: Any, code: int, message: str, data: Any = None) -> RPCResponse:
    err: dict[str, Any] = {"code": code, "message": message}
    if data is not None:
        err["data"] = data
    return RPCResponse(id=id_, error=err)


# Standard JSON-RPC error codes
PARSE_ERROR = -32700
INVALID_REQUEST = -32600
METHOD_NOT_FOUND = -32601
INVALID_PARAMS = -32602
INTERNAL_ERROR = -32603

# Custom error codes
EVAL_ERROR = -32000
SANDBOX_ERROR = -32001
SESSION_ERROR = -32002


# ── RPC Method Registry ───────────────────────────────────────────────────────

@dataclass
class RPCMethod:
    name: str
    handler: Callable
    description: str = ""
    params_schema: dict[str, Any] = field(default_factory=dict)


class RPCServer:
    """JSON-RPC 2.0 server with method registration and dispatch."""

    def __init__(self) -> None:
        self._methods: dict[str, RPCMethod] = {}
        self._session_mgr = SessionManager()
        self._register_builtins()

    def register(
        self,
        name: str,
        handler: Callable,
        description: str = "",
        params_schema: dict[str, Any] | None = None,
    ) -> None:
        self._methods[name] = RPCMethod(
            name=name,
            handler=handler,
            description=description,
            params_schema=params_schema or {},
        )

    def list_methods(self) -> list[dict[str, Any]]:
        return [
            {
                "name": m.name,
                "description": m.description,
                "params": m.params_schema,
            }
            for m in self._methods.values()
        ]

    async def handle(self, raw: str) -> str:
        """Process a raw JSON-RPC request string, return a JSON response string."""
        try:
            data = json.loads(raw)
        except json.JSONDecodeError as e:
            return json.dumps(
                rpc_error(None, PARSE_ERROR, f"Parse error: {e}").to_dict()
            )

        # Handle batch requests
        if isinstance(data, list):
            results = [await self._dispatch(d) for d in data]
            # Filter out notifications (no id)
            responses = [r.to_dict() for r in results if r.id is not None]
            return json.dumps(responses)

        resp = await self._dispatch(data)
        return json.dumps(resp.to_dict())

    async def _dispatch(self, data: dict) -> RPCResponse:
        try:
            req = RPCRequest.from_dict(data)
        except (KeyError, TypeError) as e:
            return rpc_error(
                data.get("id"), INVALID_REQUEST, f"Invalid request: {e}"
            )

        method = self._methods.get(req.method)
        if method is None:
            return rpc_error(
                req.id, METHOD_NOT_FOUND, f"Method not found: {req.method}"
            )

        try:
            result = method.handler(**req.params)
            # Support both sync and async handlers
            if hasattr(result, "__await__"):
                result = await result
            return RPCResponse(id=req.id, result=result)
        except TypeError as e:
            return rpc_error(req.id, INVALID_PARAMS, f"Invalid params: {e}")
        except Exception as e:
            return rpc_error(
                req.id,
                INTERNAL_ERROR,
                str(e),
                data=traceback.format_exc(),
            )

    # ── Built-in methods ───────────────────────────────────────────────────

    def _register_builtins(self) -> None:
        self.register("rpc.listMethods", self.list_methods, "List all registered RPC methods")
        self.register("session.create", self._session_create, "Create a new evaluation session")
        self.register("session.destroy", self._session_destroy, "Destroy an evaluation session")
        self.register("session.list", self._session_list, "List active sessions")
        self.register("eval", self._eval, "Evaluate a math expression",
                       {"expression": "string", "session_id": "string (optional)"})
        self.register("eval.batch", self._eval_batch, "Evaluate multiple expressions",
                       {"expressions": "list[string]", "session_id": "string (optional)"})
        self.register("simplify", self._simplify, "Simplify an expression")
        self.register("differentiate", self._differentiate, "Differentiate an expression")
        self.register("matrix.solve", self._matrix_solve, "Solve a linear system Ax = b")
        self.register("matrix.eig", self._matrix_eig, "Compute eigenvalues and eigenvectors")
        self.register("ode.solve", self._ode_solve, "Solve an ODE system")
        self.register("optimize", self._optimize, "Run optimization on an objective")
        self.register("stability.analyze", self._stability_analyze, "Analyze numerical stability")

    def _session_create(self) -> dict[str, str]:
        sid = self._session_mgr.create()
        return {"session_id": sid}

    def _session_destroy(self, session_id: str) -> dict[str, bool]:
        self._session_mgr.destroy(session_id)
        return {"ok": True}

    def _session_list(self) -> dict[str, list[str]]:
        return {"sessions": self._session_mgr.list_sessions()}

    def _eval(self, expression: str, session_id: str | None = None) -> dict:
        if session_id is None:
            session_id = self._session_mgr.create()
        session = self._session_mgr.get(session_id)
        if session is None:
            raise ValueError(f"Session not found: {session_id}")
        return session.evaluate(expression)

    def _eval_batch(
        self, expressions: list[str], session_id: str | None = None
    ) -> dict:
        if session_id is None:
            session_id = self._session_mgr.create()
        session = self._session_mgr.get(session_id)
        if session is None:
            raise ValueError(f"Session not found: {session_id}")
        results = [session.evaluate(expr) for expr in expressions]
        return {"results": results, "session_id": session_id}

    def _simplify(self, expression: str) -> dict:
        from greymath.core.expr import Expr, ExprKind
        from greymath.symbolic.simplify import Simplifier

        node = Expr.sym(expression)
        simplifier = Simplifier()
        simplified = simplifier.simplify(node)
        return {"input": expression, "output": simplified.to_string()}

    def _differentiate(self, expression: str, variable: str = "x") -> dict:
        from greymath.core.expr import Expr, ExprKind
        from greymath.symbolic.differentiation import SymbolicDiff

        node = Expr.sym(expression)
        differ = SymbolicDiff()
        result = differ.differentiate(node, variable)
        return {
            "input": expression,
            "variable": variable,
            "derivative": result.to_string(),
        }

    def _matrix_solve(self, A: list[list[float]], b: list[float]) -> dict:
        import numpy as np
        from greymath.numeric.linalg import LinAlg

        la = LinAlg()
        A_arr = np.array(A, dtype=np.float64)
        b_arr = np.array(b, dtype=np.float64)
        x = la.solve(A_arr, b_arr)
        return {"x": x.tolist()}

    def _matrix_eig(self, A: list[list[float]]) -> dict:
        import numpy as np
        from greymath.numeric.linalg import LinAlg

        la = LinAlg()
        A_arr = np.array(A, dtype=np.float64)
        vals, vecs = la.eig(A_arr)
        return {
            "eigenvalues": vals.tolist(),
            "eigenvectors": vecs.tolist(),
        }

    def _ode_solve(
        self,
        y0: list[float],
        t_span: list[float],
        dt: float = 0.01,
        method: str = "rk4",
    ) -> dict:
        return {"error": "ODE solve requires a function definition via session eval"}

    def _optimize(
        self,
        method: str = "gradient_descent",
        x0: list[float] | None = None,
    ) -> dict:
        return {"error": "Optimize requires an objective function via session eval"}

    def _stability_analyze(self, A: list[list[float]]) -> dict:
        import numpy as np
        from greymath.numeric.stability import StabilityDiagnostics

        diag = StabilityDiagnostics()
        A_arr = np.array(A, dtype=np.float64)
        report = diag.analyze_matrix(A_arr)
        return {
            "condition_number": report.condition_number,
            "well_conditioned": report.well_conditioned,
            "warnings": report.warnings,
            "recommendations": report.recommendations,
        }
