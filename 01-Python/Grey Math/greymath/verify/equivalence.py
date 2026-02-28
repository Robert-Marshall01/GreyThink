"""
Grey Math — Symbolic Equivalence Checking.

Determines if two expressions are symbolically equivalent
using normalization, canonical forms, and random evaluation.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any

import numpy as np

from greymath.core.expr import ExprNode, ExprKind, ExprDAG


@dataclass
class EquivalenceResult:
    """Result of an equivalence check."""
    equivalent: bool
    method: str  # How equivalence was determined
    confidence: float  # 0.0 to 1.0
    details: str = ""


class SymbolicEquivalence:
    """
    Check symbolic equivalence of mathematical expressions.

    Uses multiple strategies:
    1. Structural equality (hash comparison)
    2. Canonical form normalization
    3. Random numerical evaluation
    4. Algebraic identity matching
    """

    def __init__(self, n_eval_points: int = 100, tolerance: float = 1e-10) -> None:
        self.n_eval_points = n_eval_points
        self.tolerance = tolerance

    def check(self, expr_a: ExprNode, expr_b: ExprNode) -> EquivalenceResult:
        """Check if two expressions are equivalent."""

        # Strategy 1: Direct structural hash equality
        if self._structural_equal(expr_a, expr_b):
            return EquivalenceResult(
                equivalent=True, method="structural", confidence=1.0,
                details="Expressions are structurally identical",
            )

        # Strategy 2: Canonical form comparison
        dag = ExprDAG()
        canon_a = dag.canonicalize(expr_a)
        canon_b = dag.canonicalize(expr_b)
        if self._structural_equal(canon_a, canon_b):
            return EquivalenceResult(
                equivalent=True, method="canonical", confidence=1.0,
                details="Expressions have the same canonical form",
            )

        # Strategy 3: Simplification + comparison
        from greymath.symbolic.simplify import Simplifier
        simplifier = Simplifier()
        simp_a = simplifier.simplify(expr_a)
        simp_b = simplifier.simplify(expr_b)
        if self._structural_equal(simp_a, simp_b):
            return EquivalenceResult(
                equivalent=True, method="simplification", confidence=0.99,
                details="Expressions simplify to the same form",
            )

        # Strategy 4: Difference simplification (a - b == 0?)
        diff = ExprNode(kind=ExprKind.SUB, children=[expr_a, expr_b])
        diff_simp = simplifier.simplify(diff)
        if (diff_simp.kind == ExprKind.LITERAL and
                isinstance(diff_simp.value, (int, float)) and
                abs(diff_simp.value) < self.tolerance):
            return EquivalenceResult(
                equivalent=True, method="difference_zero", confidence=0.99,
                details="Difference simplifies to zero",
            )

        # Strategy 5: Numerical evaluation
        variables = expr_a.collect_variables() | expr_b.collect_variables()
        if variables:
            num_result = self._numerical_check(expr_a, expr_b, variables)
            if num_result is not None:
                return num_result

        return EquivalenceResult(
            equivalent=False, method="all_strategies", confidence=0.95,
            details="No strategy could prove equivalence",
        )

    def _structural_equal(self, a: ExprNode, b: ExprNode) -> bool:
        """Check structural equality of two expression nodes."""
        if a.kind != b.kind:
            return False
        if a.value != b.value:
            return False
        if a.name != b.name:
            return False
        if len(a.children) != len(b.children):
            return False
        return all(
            self._structural_equal(ca, cb)
            for ca, cb in zip(a.children, b.children)
        )

    def _numerical_check(
        self,
        expr_a: ExprNode,
        expr_b: ExprNode,
        variables: set[str],
    ) -> EquivalenceResult | None:
        """Evaluate both expressions at random points."""
        import math

        var_list = sorted(variables)
        n_agree = 0
        n_disagree = 0
        n_invalid = 0

        for _ in range(self.n_eval_points):
            # Generate random assignment
            assignment = {v: np.random.uniform(-10, 10) for v in var_list}

            try:
                val_a = self._evaluate(expr_a, assignment)
                val_b = self._evaluate(expr_b, assignment)

                if val_a is None or val_b is None:
                    n_invalid += 1
                    continue

                if (math.isnan(val_a) and math.isnan(val_b)) or \
                   (math.isinf(val_a) and math.isinf(val_b) and
                        (val_a > 0) == (val_b > 0)):
                    n_agree += 1
                elif abs(val_a - val_b) < self.tolerance * max(1, abs(val_a), abs(val_b)):
                    n_agree += 1
                else:
                    n_disagree += 1
            except Exception:
                n_invalid += 1

        total_valid = n_agree + n_disagree
        if total_valid == 0:
            return None

        if n_disagree > 0:
            return EquivalenceResult(
                equivalent=False, method="numerical", confidence=0.99,
                details=f"Disagreed at {n_disagree}/{total_valid} evaluation points",
            )

        if n_agree >= 10:
            confidence = min(0.99, 1.0 - (1.0 / (2 ** n_agree)))
            return EquivalenceResult(
                equivalent=True, method="numerical", confidence=confidence,
                details=f"Agreed at all {n_agree} valid evaluation points",
            )

        return None

    def _evaluate(self, expr: ExprNode, assignment: dict[str, float]) -> float | None:
        """Evaluate an expression with variable assignments."""
        import math

        if expr.kind == ExprKind.LITERAL:
            return float(expr.value) if expr.value is not None else None

        if expr.kind == ExprKind.SYMBOL:
            name = expr.name or ""
            if name in assignment:
                return assignment[name]
            # Constants
            constants = {"pi": math.pi, "e": math.e, "tau": math.tau}
            return constants.get(name)

        if expr.kind == ExprKind.VARIABLE:
            name = expr.name or ""
            return assignment.get(name)

        children_vals = [self._evaluate(c, assignment) for c in expr.children]
        if any(v is None for v in children_vals):
            return None

        vals = [float(v) for v in children_vals]  # type: ignore

        ops = {
            ExprKind.ADD: lambda: vals[0] + vals[1],
            ExprKind.SUB: lambda: vals[0] - vals[1],
            ExprKind.MUL: lambda: vals[0] * vals[1],
            ExprKind.DIV: lambda: vals[0] / vals[1] if vals[1] != 0 else None,
            ExprKind.NEG: lambda: -vals[0],
            ExprKind.POW: lambda: vals[0] ** vals[1] if abs(vals[1]) < 100 else None,
        }

        op = ops.get(expr.kind)
        if op is None:
            return None

        try:
            result = op()
            if result is not None and math.isfinite(result):
                return result
            return result
        except (OverflowError, ZeroDivisionError, ValueError):
            return None
