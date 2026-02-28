"""
Session management for the Grey Math IDE.

Each session maintains:
- A symbolic workspace (expression DAG)
- Variable bindings
- Computation history
- Evaluation context
"""

from __future__ import annotations

import uuid
from dataclasses import dataclass, field
from typing import Any, Optional

import numpy as np

from greymath.core.expr import ExprDAG, ExprNode, Expr
from greymath.core.types import Scalar, Vector, Matrix


@dataclass
class Session:
    """An IDE session with workspace state."""
    id: str = field(default_factory=lambda: str(uuid.uuid4())[:12])
    variables: dict[str, Any] = field(default_factory=dict)
    history: list[dict[str, Any]] = field(default_factory=list)
    expr_dag: ExprDAG = field(default_factory=ExprDAG)

    def evaluate(self, expression: str) -> Any:
        """
        Evaluate a mathematical expression in this session's context.

        Supports:
        - Arithmetic: 2 + 3, sin(pi/4)
        - Variable assignment: x = 5
        - Matrix operations: [[1,2],[3,4]]
        - Function calls: det([[1,2],[3,4]])
        """
        expression = expression.strip()

        # Variable assignment
        if "=" in expression and not expression.startswith("=="):
            parts = expression.split("=", 1)
            var_name = parts[0].strip()
            value_expr = parts[1].strip()
            try:
                value = self._eval_numeric(value_expr)
                self.variables[var_name] = value
                self._record(expression, value)
                return {"assigned": var_name, "value": self._serialize(value)}
            except Exception as e:
                return {"error": str(e)}

        # Evaluation
        try:
            result = self._eval_numeric(expression)
            self._record(expression, result)
            return {"result": self._serialize(result)}
        except Exception as e:
            return {"error": str(e)}

    def _eval_numeric(self, expr: str) -> Any:
        """Evaluate a numeric expression."""
        # Build a safe evaluation context
        safe_globals = {
            "__builtins__": {},
            "np": np,
            "sin": np.sin, "cos": np.cos, "tan": np.tan,
            "exp": np.exp, "log": np.log, "sqrt": np.sqrt,
            "pi": np.pi, "e": np.e,
            "abs": np.abs,
            "array": np.array,
            "matrix": lambda data: Matrix(data=np.array(data)),
            "det": lambda m: float(np.linalg.det(np.array(m) if not isinstance(m, np.ndarray) else m)),
            "inv": lambda m: np.linalg.inv(np.array(m) if not isinstance(m, np.ndarray) else m),
            "eig": lambda m: np.linalg.eigvals(np.array(m) if not isinstance(m, np.ndarray) else m),
            "svd": lambda m: np.linalg.svd(np.array(m) if not isinstance(m, np.ndarray) else m, compute_uv=False),
            "norm": lambda x: float(np.linalg.norm(np.array(x) if not isinstance(x, np.ndarray) else x)),
            "trace": lambda m: float(np.trace(np.array(m) if not isinstance(m, np.ndarray) else m)),
            "transpose": lambda m: np.array(m).T,
            "zeros": np.zeros,
            "ones": np.ones,
            "eye": np.eye,
            "linspace": np.linspace,
        }
        # Add session variables
        safe_globals.update(self.variables)

        return eval(expr, safe_globals)

    def _serialize(self, value: Any) -> Any:
        """Serialize a value for JSON response."""
        if isinstance(value, np.ndarray):
            return {"type": "array", "data": value.tolist(), "shape": list(value.shape)}
        if isinstance(value, (int, float)):
            return {"type": "scalar", "value": value}
        if isinstance(value, complex):
            return {"type": "complex", "real": value.real, "imag": value.imag}
        if isinstance(value, Matrix):
            return {
                "type": "matrix",
                "data": value.data.tolist() if value.data is not None else None,
                "shape": [value.rows, value.cols],
            }
        return {"type": "unknown", "repr": str(value)}

    def _record(self, expression: str, result: Any) -> None:
        """Record a computation in history."""
        self.history.append({
            "expression": expression,
            "result": str(result)[:200],
        })


class SessionManager:
    """Manages multiple IDE sessions."""

    def __init__(self) -> None:
        self._sessions: dict[str, Session] = {}

    def create_session(self) -> str:
        """Create a new session and return its ID."""
        session = Session()
        self._sessions[session.id] = session
        return session.id

    def get(self, session_id: str) -> Optional[Session]:
        return self._sessions.get(session_id)

    def get_or_create(self, session_id: Optional[str] = None) -> Session:
        if session_id and session_id in self._sessions:
            return self._sessions[session_id]
        session = Session(id=session_id or str(uuid.uuid4())[:12])
        self._sessions[session.id] = session
        return session

    def remove_session(self, session_id: str) -> None:
        self._sessions.pop(session_id, None)

    @property
    def active_count(self) -> int:
        return len(self._sessions)
