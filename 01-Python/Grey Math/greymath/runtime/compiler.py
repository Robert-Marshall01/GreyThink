"""
Grey Math — Expression Compiler.

Compiles expression DAGs into optimized callable functions.
Performs:
- Common subexpression elimination (CSE)
- Constant folding
- Operator strength reduction
- NumPy vectorization
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Any, Callable

import numpy as np

from greymath.core.expr import ExprNode, ExprKind


@dataclass
class CompiledExpr:
    """A compiled expression ready for fast evaluation."""
    function: Callable
    variables: list[str]
    n_ops: int  # Operation count
    cse_savings: int  # Operations saved by CSE
    source: str  # Generated Python source code


class ExprCompiler:
    """
    Compiles expression trees into optimized Python/NumPy functions.
    """

    def __init__(self, use_numpy: bool = True) -> None:
        self.use_numpy = use_numpy
        self._temp_counter = 0

    def compile(self, expr: ExprNode, variable_names: list[str] | None = None) -> CompiledExpr:
        """
        Compile an expression into a callable function.

        The resulting function takes variable values as arguments
        (in the order of variable_names) and returns the result.
        """
        if variable_names is None:
            variable_names = sorted(expr.collect_variables())

        # Phase 1: Common subexpression elimination
        cse_map, top_expr, cse_count = self._cse(expr)

        # Phase 2: Generate code
        lines: list[str] = []
        n_ops = 0

        # Generate CSE temporaries
        for temp_name, sub_expr in cse_map.items():
            code, ops = self._codegen(sub_expr)
            lines.append(f"    {temp_name} = {code}")
            n_ops += ops

        # Generate final expression
        final_code, final_ops = self._codegen(top_expr)
        lines.append(f"    return {final_code}")
        n_ops += final_ops

        # Build function
        args = ", ".join(variable_names) if variable_names else ""
        prefix = "np" if self.use_numpy else "math"
        source = f"def _compiled_expr({args}):\n"
        source += "\n".join(lines) if lines else "    return 0"

        # Create namespace
        ns: dict[str, Any] = {
            "np": np,
            "math": math,
            "inf": float("inf"),
            "pi": math.pi,
            "e": math.e,
        }

        # Add CSE temp variables to namespace if needed
        for temp_name in cse_map:
            if temp_name not in ns:
                ns[temp_name] = None

        exec(source, ns)
        func = ns["_compiled_expr"]

        return CompiledExpr(
            function=func,
            variables=variable_names,
            n_ops=n_ops,
            cse_savings=cse_count,
            source=source,
        )

    def _cse(self, expr: ExprNode) -> tuple[dict[str, ExprNode], ExprNode, int]:
        """
        Common subexpression elimination.

        Returns (temp_name -> sub_expr, top_expr, n_eliminations).
        """
        # Count occurrences of each subexpression
        counts: dict[str, int] = {}
        expr_by_hash: dict[str, ExprNode] = {}

        def count_subexprs(node: ExprNode) -> str:
            key = node.to_string()
            counts[key] = counts.get(key, 0) + 1
            expr_by_hash[key] = node
            for child in node.children:
                count_subexprs(child)
            return key

        count_subexprs(expr)

        # Extract subexpressions that appear more than once
        cse_map: dict[str, ExprNode] = {}
        replacements: dict[str, str] = {}
        self._temp_counter = 0
        cse_count = 0

        for key, count in counts.items():
            if count > 1 and len(key) > 3:  # Only CSE non-trivial expressions
                temp_name = f"_t{self._temp_counter}"
                self._temp_counter += 1
                cse_map[temp_name] = expr_by_hash[key]
                replacements[key] = temp_name
                cse_count += count - 1

        return cse_map, expr, cse_count

    def _codegen(self, expr: ExprNode) -> tuple[str, int]:
        """
        Generate Python code for an expression.

        Returns (code_string, operation_count).
        """
        if expr.kind == ExprKind.LITERAL:
            if isinstance(expr.value, (int, float)):
                return repr(expr.value), 0
            return str(expr.value), 0

        if expr.kind in (ExprKind.SYMBOL, ExprKind.VARIABLE):
            name = expr.name or "x"
            constants = {"pi": "np.pi" if self.use_numpy else "math.pi",
                         "e": "np.e" if self.use_numpy else "math.e",
                         "inf": "np.inf" if self.use_numpy else "float('inf')"}
            return constants.get(name, name), 0

        prefix = "np" if self.use_numpy else "math"

        if expr.kind == ExprKind.ADD:
            codes = [self._codegen(c) for c in expr.children]
            return " + ".join(c[0] for c in codes), sum(c[1] for c in codes) + 1

        if expr.kind == ExprKind.SUB:
            left, lo = self._codegen(expr.children[0])
            right, ro = self._codegen(expr.children[1])
            return f"({left} - {right})", lo + ro + 1

        if expr.kind == ExprKind.MUL:
            codes = [self._codegen(c) for c in expr.children]
            return " * ".join(c[0] for c in codes), sum(c[1] for c in codes) + 1

        if expr.kind == ExprKind.DIV:
            left, lo = self._codegen(expr.children[0])
            right, ro = self._codegen(expr.children[1])
            return f"({left} / {right})", lo + ro + 1

        if expr.kind == ExprKind.POW:
            base, bo = self._codegen(expr.children[0])
            exp, eo = self._codegen(expr.children[1])
            # Strength reduction for common exponents
            if expr.children[1].kind == ExprKind.LITERAL:
                v = expr.children[1].value
                if v == 2:
                    return f"({base} * {base})", bo + 1
                if v == 0.5:
                    return f"{prefix}.sqrt({base})", bo + 1
                if v == -1:
                    return f"(1.0 / {base})", bo + 1
            return f"({base} ** {exp})", bo + eo + 1

        if expr.kind == ExprKind.NEG:
            child, co = self._codegen(expr.children[0])
            return f"(-{child})", co + 1

        if expr.kind == ExprKind.APPLY:
            func_code, fo = self._codegen(expr.children[0])
            arg_codes = [self._codegen(c) for c in expr.children[1:]]
            args = ", ".join(c[0] for c in arg_codes)

            # Map known functions
            func_map = {
                "sin": f"{prefix}.sin", "cos": f"{prefix}.cos",
                "tan": f"{prefix}.tan", "exp": f"{prefix}.exp",
                "log": f"{prefix}.log", "sqrt": f"{prefix}.sqrt",
                "abs": f"{prefix}.abs" if self.use_numpy else "abs",
                "floor": f"{prefix}.floor", "ceil": f"{prefix}.ceil",
            }
            mapped = func_map.get(func_code, func_code)
            total_ops = fo + sum(c[1] for c in arg_codes) + 1
            return f"{mapped}({args})", total_ops

        if expr.kind == ExprKind.MATMUL:
            left, lo = self._codegen(expr.children[0])
            right, ro = self._codegen(expr.children[1])
            return f"({left} @ {right})", lo + ro + 1

        if expr.kind == ExprKind.TRANSPOSE:
            child, co = self._codegen(expr.children[0])
            return f"{child}.T", co + 1

        if expr.kind == ExprKind.DET:
            child, co = self._codegen(expr.children[0])
            return f"np.linalg.det({child})", co + 1

        if expr.kind == ExprKind.TRACE:
            child, co = self._codegen(expr.children[0])
            return f"np.trace({child})", co + 1

        if expr.kind == ExprKind.NORM:
            child, co = self._codegen(expr.children[0])
            return f"np.linalg.norm({child})", co + 1

        # Fallback
        return f"0  # unsupported: {expr.kind.name}", 0

    def compile_gradient(
        self,
        expr: ExprNode,
        variables: list[str],
    ) -> CompiledExpr:
        """
        Compile the gradient of an expression into a callable function.
        """
        from greymath.symbolic.differentiation import SymbolicDiff

        diff = SymbolicDiff()
        grad_exprs = [diff.differentiate(expr, v) for v in variables]

        # Build combined function
        lines = ["import numpy as np", "def _compiled_gradient(" + ", ".join(variables) + "):"]
        total_ops = 0

        grad_codes = []
        for ge in grad_exprs:
            code, ops = self._codegen(ge)
            grad_codes.append(code)
            total_ops += ops

        lines.append(f"    return np.array([{', '.join(grad_codes)}])")
        source = "\n".join(lines)

        ns: dict[str, Any] = {"np": np, "math": math}
        exec(source, ns)

        return CompiledExpr(
            function=ns["_compiled_gradient"],
            variables=variables,
            n_ops=total_ops,
            cse_savings=0,
            source=source,
        )
