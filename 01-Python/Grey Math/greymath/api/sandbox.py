"""
Grey Math — Sandboxed Expression Evaluator.

Provides a restricted execution environment for evaluating mathematical
expressions from untrusted sources (LLMs, agents, external APIs).
Blocks dangerous operations while allowing full math capability.
"""

from __future__ import annotations

import ast
import math
import operator
from dataclasses import dataclass, field
from typing import Any, Callable


# ── Whitelist of allowed operations ────────────────────────────────────────────

SAFE_MATH_FUNCS: dict[str, Callable] = {
    # Basic
    "abs": abs,
    "round": round,
    "min": min,
    "max": max,
    "sum": sum,
    "len": len,
    # Math module
    "sin": math.sin,
    "cos": math.cos,
    "tan": math.tan,
    "asin": math.asin,
    "acos": math.acos,
    "atan": math.atan,
    "atan2": math.atan2,
    "sinh": math.sinh,
    "cosh": math.cosh,
    "tanh": math.tanh,
    "exp": math.exp,
    "log": math.log,
    "log2": math.log2,
    "log10": math.log10,
    "sqrt": math.sqrt,
    "ceil": math.ceil,
    "floor": math.floor,
    "factorial": math.factorial,
    "gcd": math.gcd,
    "pow": pow,
}

SAFE_CONSTANTS: dict[str, Any] = {
    "pi": math.pi,
    "e": math.e,
    "tau": math.tau,
    "inf": math.inf,
    "nan": math.nan,
    "True": True,
    "False": False,
    "None": None,
}

SAFE_OPERATORS = {
    ast.Add: operator.add,
    ast.Sub: operator.sub,
    ast.Mult: operator.mul,
    ast.Div: operator.truediv,
    ast.FloorDiv: operator.floordiv,
    ast.Mod: operator.mod,
    ast.Pow: operator.pow,
    ast.USub: operator.neg,
    ast.UAdd: operator.pos,
    ast.BitAnd: operator.and_,
    ast.BitOr: operator.or_,
    ast.BitXor: operator.xor,
    ast.LShift: operator.lshift,
    ast.RShift: operator.rshift,
    ast.Invert: operator.invert,
}

SAFE_COMPARISONS = {
    ast.Eq: operator.eq,
    ast.NotEq: operator.ne,
    ast.Lt: operator.lt,
    ast.LtE: operator.le,
    ast.Gt: operator.gt,
    ast.GtE: operator.ge,
}

# AST node types that are forbidden
FORBIDDEN_NODES = {
    ast.Import,
    ast.ImportFrom,
    ast.Global,
    ast.Nonlocal,
    ast.Delete,
    ast.ClassDef,
    ast.AsyncFunctionDef,
    ast.AsyncFor,
    ast.AsyncWith,
    ast.Await,
    ast.Yield,
    ast.YieldFrom,
    ast.Raise,
    ast.Try,
    ast.With,
}

# Attributes / builtins forbidden even as strings
FORBIDDEN_ATTRS = {
    "__import__",
    "__builtins__",
    "__class__",
    "__subclasses__",
    "__bases__",
    "__mro__",
    "__globals__",
    "__code__",
    "__closure__",
    "eval",
    "exec",
    "compile",
    "open",
    "input",
    "breakpoint",
    "exit",
    "quit",
    "getattr",
    "setattr",
    "delattr",
    "vars",
    "dir",
    "type",
    "super",
    "globals",
    "locals",
    "__dict__",
}


@dataclass
class SandboxResult:
    success: bool
    value: Any = None
    error: str | None = None
    warnings: list[str] = field(default_factory=list)


class SecurityViolation(Exception):
    """Raised when sandbox detects a dangerous operation."""
    pass


class SandboxedEvaluator:
    """
    Safe expression evaluator that restricts operations to mathematical
    functions only. Prevents arbitrary code execution.
    """

    def __init__(
        self,
        max_depth: int = 20,
        max_result_size: int = 10_000_000,  # 10MB
        timeout_seconds: float = 30.0,
        extra_functions: dict[str, Callable] | None = None,
        extra_variables: dict[str, Any] | None = None,
    ) -> None:
        self.max_depth = max_depth
        self.max_result_size = max_result_size
        self.timeout_seconds = timeout_seconds

        self._functions: dict[str, Callable] = {**SAFE_MATH_FUNCS}
        if extra_functions:
            self._functions.update(extra_functions)

        self._variables: dict[str, Any] = {**SAFE_CONSTANTS}
        if extra_variables:
            self._variables.update(extra_variables)

    def evaluate(self, expression: str) -> SandboxResult:
        """Evaluate a mathematical expression safely."""
        expression = expression.strip()
        if not expression:
            return SandboxResult(success=False, error="Empty expression")

        # Length check
        if len(expression) > 10_000:
            return SandboxResult(success=False, error="Expression too long (max 10000 chars)")

        try:
            tree = ast.parse(expression, mode="eval")
        except SyntaxError as e:
            # Try as exec (assignments, multi-line)
            try:
                tree = ast.parse(expression, mode="exec")
                return self._exec_safe(tree)
            except SyntaxError:
                return SandboxResult(success=False, error=f"Syntax error: {e}")

        try:
            self._validate_ast(tree)
            result = self._eval_node(tree.body, depth=0)
            return SandboxResult(success=True, value=result)
        except SecurityViolation as e:
            return SandboxResult(success=False, error=f"Security violation: {e}")
        except Exception as e:
            return SandboxResult(success=False, error=f"Evaluation error: {e}")

    def set_variable(self, name: str, value: Any) -> None:
        if name in FORBIDDEN_ATTRS:
            raise SecurityViolation(f"Cannot set forbidden name: {name}")
        self._variables[name] = value

    def get_variable(self, name: str) -> Any:
        return self._variables.get(name)

    # ── AST validation ─────────────────────────────────────────────────────

    def _validate_ast(self, tree: ast.AST) -> None:
        """Walk the AST and reject dangerous constructs."""
        for node in ast.walk(tree):
            node_type = type(node)

            if node_type in FORBIDDEN_NODES:
                raise SecurityViolation(f"Forbidden construct: {node_type.__name__}")

            if isinstance(node, ast.Attribute):
                if node.attr in FORBIDDEN_ATTRS:
                    raise SecurityViolation(f"Forbidden attribute: {node.attr}")

            if isinstance(node, ast.Name):
                if node.id in FORBIDDEN_ATTRS:
                    raise SecurityViolation(f"Forbidden name: {node.id}")

            if isinstance(node, ast.Call):
                if isinstance(node.func, ast.Name):
                    if node.func.id in FORBIDDEN_ATTRS:
                        raise SecurityViolation(f"Forbidden call: {node.func.id}")

    # ── Safe evaluation ────────────────────────────────────────────────────

    def _eval_node(self, node: ast.AST, depth: int = 0) -> Any:
        if depth > self.max_depth:
            raise SecurityViolation("Maximum expression depth exceeded")

        if isinstance(node, ast.Constant):
            return node.value

        if isinstance(node, ast.Name):
            name = node.id
            if name in self._variables:
                return self._variables[name]
            if name in self._functions:
                return self._functions[name]
            raise NameError(f"Undefined: {name}")

        if isinstance(node, ast.UnaryOp):
            op = SAFE_OPERATORS.get(type(node.op))
            if op is None:
                raise SecurityViolation(f"Unsupported unary op: {type(node.op).__name__}")
            return op(self._eval_node(node.operand, depth + 1))

        if isinstance(node, ast.BinOp):
            op = SAFE_OPERATORS.get(type(node.op))
            if op is None:
                raise SecurityViolation(f"Unsupported binary op: {type(node.op).__name__}")
            left = self._eval_node(node.left, depth + 1)
            right = self._eval_node(node.right, depth + 1)
            # Protect against huge exponents
            if isinstance(node.op, ast.Pow):
                if isinstance(right, (int, float)) and abs(right) > 10000:
                    raise SecurityViolation(f"Exponent too large: {right}")
            return op(left, right)

        if isinstance(node, ast.BoolOp):
            if isinstance(node.op, ast.And):
                return all(self._eval_node(v, depth + 1) for v in node.values)
            if isinstance(node.op, ast.Or):
                return any(self._eval_node(v, depth + 1) for v in node.values)

        if isinstance(node, ast.Compare):
            left = self._eval_node(node.left, depth + 1)
            for op_node, comparator in zip(node.ops, node.comparators):
                op = SAFE_COMPARISONS.get(type(op_node))
                if op is None:
                    raise SecurityViolation(f"Unsupported comparison: {type(op_node).__name__}")
                right = self._eval_node(comparator, depth + 1)
                if not op(left, right):
                    return False
                left = right
            return True

        if isinstance(node, ast.Call):
            func = self._eval_node(node.func, depth + 1)
            if not callable(func):
                raise SecurityViolation(f"Not callable: {func}")
            args = [self._eval_node(a, depth + 1) for a in node.args]
            kwargs = {kw.arg: self._eval_node(kw.value, depth + 1) for kw in node.keywords}
            return func(*args, **kwargs)

        if isinstance(node, ast.IfExp):
            test = self._eval_node(node.test, depth + 1)
            return self._eval_node(node.body if test else node.orelse, depth + 1)

        if isinstance(node, ast.List):
            return [self._eval_node(e, depth + 1) for e in node.elts]

        if isinstance(node, ast.Tuple):
            return tuple(self._eval_node(e, depth + 1) for e in node.elts)

        if isinstance(node, ast.Dict):
            keys = [self._eval_node(k, depth + 1) for k in node.keys]
            vals = [self._eval_node(v, depth + 1) for v in node.values]
            return dict(zip(keys, vals))

        if isinstance(node, ast.Subscript):
            val = self._eval_node(node.value, depth + 1)
            sl = self._eval_node(node.slice, depth + 1)
            return val[sl]

        if isinstance(node, ast.Index):
            return self._eval_node(node.value, depth + 1)  # type: ignore

        if isinstance(node, ast.ListComp):
            return self._eval_listcomp(node, depth + 1)

        raise SecurityViolation(f"Unsupported expression type: {type(node).__name__}")

    def _eval_listcomp(self, node: ast.ListComp, depth: int) -> list:
        """Safely evaluate a list comprehension."""
        if len(node.generators) > 2:
            raise SecurityViolation("Max 2 generators in list comprehension")

        result = []
        gen = node.generators[0]
        iterable = self._eval_node(gen.iter, depth + 1)

        if not hasattr(iterable, "__iter__"):
            raise SecurityViolation("Not iterable in comprehension")
        if hasattr(iterable, "__len__") and len(iterable) > 100_000:
            raise SecurityViolation("Iterable too large in comprehension")

        target_name = gen.target.id if isinstance(gen.target, ast.Name) else None
        if target_name is None:
            raise SecurityViolation("Only simple variable targets in comprehensions")

        old_val = self._variables.get(target_name)
        try:
            for item in iterable:
                self._variables[target_name] = item
                # Check conditions
                if all(self._eval_node(cond, depth + 1) for cond in gen.ifs):
                    result.append(self._eval_node(node.elt, depth + 1))
                if len(result) > 100_000:
                    raise SecurityViolation("Comprehension result too large")
        finally:
            if old_val is not None:
                self._variables[target_name] = old_val
            elif target_name in self._variables:
                del self._variables[target_name]

        return result

    def _exec_safe(self, tree: ast.Module) -> SandboxResult:
        """Handle simple assignment statements safely."""
        self._validate_ast(tree)
        last_value = None

        for stmt in tree.body:
            if isinstance(stmt, ast.Assign):
                value = self._eval_node(stmt.value)
                for target in stmt.targets:
                    if isinstance(target, ast.Name):
                        self._variables[target.id] = value
                        last_value = value
                    else:
                        return SandboxResult(
                            success=False, error="Only simple name assignments allowed"
                        )
            elif isinstance(stmt, ast.Expr):
                last_value = self._eval_node(stmt.value)
            elif isinstance(stmt, ast.FunctionDef):
                # Allow simple function definitions for math
                return SandboxResult(
                    success=False,
                    error="Function definitions not allowed in sandbox mode",
                )
            else:
                return SandboxResult(
                    success=False,
                    error=f"Unsupported statement: {type(stmt).__name__}",
                )

        return SandboxResult(success=True, value=last_value)
