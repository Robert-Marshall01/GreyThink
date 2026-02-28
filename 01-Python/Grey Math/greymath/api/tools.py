"""
Grey Math — Tool Definitions for Function-Calling LLMs.

Provides structured tool/function schemas for LLMs (OpenAI, Anthropic, etc.)
that expose Grey Math capabilities as callable tools.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Callable, Optional


@dataclass
class ToolParameter:
    name: str
    type: str
    description: str
    required: bool = True
    default: Any = None
    enum: list[str] | None = None


@dataclass
class ToolDefinition:
    name: str
    description: str
    parameters: list[ToolParameter] = field(default_factory=list)
    handler: Optional[Callable] = None
    category: str = "math"

    def to_openai_schema(self) -> dict[str, Any]:
        """Export as OpenAI function-calling schema."""
        props = {}
        required = []
        for p in self.parameters:
            prop: dict[str, Any] = {"type": p.type, "description": p.description}
            if p.enum:
                prop["enum"] = p.enum
            props[p.name] = prop
            if p.required:
                required.append(p.name)

        return {
            "type": "function",
            "function": {
                "name": self.name,
                "description": self.description,
                "parameters": {
                    "type": "object",
                    "properties": props,
                    "required": required,
                },
            },
        }

    def to_anthropic_schema(self) -> dict[str, Any]:
        """Export as Anthropic tool-use schema."""
        props = {}
        required = []
        for p in self.parameters:
            prop: dict[str, Any] = {"type": p.type, "description": p.description}
            if p.enum:
                prop["enum"] = p.enum
            props[p.name] = prop
            if p.required:
                required.append(p.name)

        return {
            "name": self.name,
            "description": self.description,
            "input_schema": {
                "type": "object",
                "properties": props,
                "required": required,
            },
        }


class ToolRegistry:
    """Registry for Grey Math tools that can be exported for LLM function calling."""

    def __init__(self) -> None:
        self._tools: dict[str, ToolDefinition] = {}

    def register(self, tool: ToolDefinition) -> None:
        self._tools[tool.name] = tool

    def get(self, name: str) -> ToolDefinition | None:
        return self._tools.get(name)

    def list_tools(self, category: str | None = None) -> list[ToolDefinition]:
        tools = list(self._tools.values())
        if category:
            tools = [t for t in tools if t.category == category]
        return tools

    def to_openai_tools(self, category: str | None = None) -> list[dict]:
        return [t.to_openai_schema() for t in self.list_tools(category)]

    def to_anthropic_tools(self, category: str | None = None) -> list[dict]:
        return [t.to_anthropic_schema() for t in self.list_tools(category)]

    async def execute(self, name: str, arguments: dict[str, Any]) -> Any:
        tool = self._tools.get(name)
        if tool is None:
            raise ValueError(f"Unknown tool: {name}")
        if tool.handler is None:
            raise ValueError(f"Tool {name} has no handler")
        result = tool.handler(**arguments)
        if hasattr(result, "__await__"):
            result = await result
        return result


def get_math_tools() -> ToolRegistry:
    """Create a registry pre-populated with all standard Grey Math tools."""
    registry = ToolRegistry()

    # ── Evaluation ─────────────────────────────────────────────────────────
    registry.register(ToolDefinition(
        name="evaluate_expression",
        description="Evaluate a mathematical expression. Supports arithmetic, linear algebra, calculus notation.",
        parameters=[
            ToolParameter("expression", "string", "The mathematical expression to evaluate"),
            ToolParameter("precision", "string", "Precision mode: float64, float128, arbitrary",
                          required=False, default="float64", enum=["float64", "float128", "arbitrary"]),
        ],
        category="evaluation",
    ))

    # ── Linear Algebra ─────────────────────────────────────────────────────
    registry.register(ToolDefinition(
        name="matrix_eigendecomposition",
        description="Compute eigenvalues and eigenvectors of a matrix.",
        parameters=[
            ToolParameter("matrix", "array", "2D array representing the matrix"),
            ToolParameter("symmetric", "boolean", "Whether the matrix is symmetric (uses faster algorithm)",
                          required=False, default=False),
        ],
        category="linalg",
    ))

    registry.register(ToolDefinition(
        name="solve_linear_system",
        description="Solve the linear system Ax = b for x.",
        parameters=[
            ToolParameter("A", "array", "Coefficient matrix (2D array)"),
            ToolParameter("b", "array", "Right-hand side vector"),
        ],
        category="linalg",
    ))

    registry.register(ToolDefinition(
        name="matrix_decomposition",
        description="Compute a matrix decomposition (LU, QR, SVD, Cholesky, Schur).",
        parameters=[
            ToolParameter("matrix", "array", "The matrix to decompose"),
            ToolParameter("method", "string", "Decomposition method",
                          enum=["lu", "qr", "svd", "cholesky", "schur"]),
        ],
        category="linalg",
    ))

    # ── Calculus ───────────────────────────────────────────────────────────
    registry.register(ToolDefinition(
        name="differentiate",
        description="Compute the symbolic derivative of an expression.",
        parameters=[
            ToolParameter("expression", "string", "The expression to differentiate"),
            ToolParameter("variable", "string", "The variable to differentiate with respect to"),
            ToolParameter("order", "integer", "Order of differentiation", required=False, default=1),
        ],
        category="calculus",
    ))

    registry.register(ToolDefinition(
        name="series_expansion",
        description="Compute a Taylor/Laurent series expansion.",
        parameters=[
            ToolParameter("expression", "string", "The expression to expand"),
            ToolParameter("variable", "string", "The expansion variable"),
            ToolParameter("point", "number", "The point around which to expand", required=False, default=0),
            ToolParameter("order", "integer", "Number of terms", required=False, default=5),
            ToolParameter("kind", "string", "Type of expansion", required=False, enum=["taylor", "laurent"]),
        ],
        category="calculus",
    ))

    # ── ODEs ───────────────────────────────────────────────────────────────
    registry.register(ToolDefinition(
        name="solve_ode",
        description="Numerically solve a system of ordinary differential equations.",
        parameters=[
            ToolParameter("system_description", "string", "Description of the ODE system"),
            ToolParameter("initial_conditions", "array", "Initial state vector"),
            ToolParameter("t_span", "array", "Time interval [t0, tf]"),
            ToolParameter("method", "string", "Integration method",
                          required=False, enum=["euler", "rk4", "rk45", "bdf"]),
        ],
        category="ode",
    ))

    # ── Optimization ───────────────────────────────────────────────────────
    registry.register(ToolDefinition(
        name="optimize_function",
        description="Find the minimum of an objective function.",
        parameters=[
            ToolParameter("objective_description", "string", "Description of the objective function"),
            ToolParameter("x0", "array", "Initial guess"),
            ToolParameter("method", "string", "Optimization method",
                          required=False, enum=["gradient_descent", "newton", "lbfgs", "nelder_mead"]),
            ToolParameter("constraints", "string", "Description of constraints", required=False),
        ],
        category="optimization",
    ))

    # ── Stability Analysis ─────────────────────────────────────────────────
    registry.register(ToolDefinition(
        name="analyze_stability",
        description="Analyze the numerical stability of a matrix or computation.",
        parameters=[
            ToolParameter("matrix", "array", "The matrix to analyze"),
            ToolParameter("analysis_type", "string", "Type of analysis",
                          required=False, enum=["condition", "spectral", "full"]),
        ],
        category="stability",
    ))

    # ── Stochastic ─────────────────────────────────────────────────────────
    registry.register(ToolDefinition(
        name="sample_distribution",
        description="Sample from a probability distribution using MCMC.",
        parameters=[
            ToolParameter("distribution_description", "string", "Description of the target distribution"),
            ToolParameter("n_samples", "integer", "Number of samples to draw", required=False, default=1000),
            ToolParameter("method", "string", "Sampling method",
                          required=False, enum=["metropolis_hastings", "hmc"]),
        ],
        category="stochastic",
    ))

    # ── Simplification ─────────────────────────────────────────────────────
    registry.register(ToolDefinition(
        name="simplify_expression",
        description="Simplify a symbolic mathematical expression using algebraic rules.",
        parameters=[
            ToolParameter("expression", "string", "The expression to simplify"),
            ToolParameter("strategy", "string", "Simplification strategy",
                          required=False, enum=["normalize", "greedy", "exhaustive"]),
        ],
        category="symbolic",
    ))

    return registry
