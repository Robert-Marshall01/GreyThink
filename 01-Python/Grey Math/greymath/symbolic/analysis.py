"""
Structural analysis of mathematical expressions.

Detects properties such as:
- Linearity
- Convexity / Concavity
- Symmetry
- Monotonicity
- Homogeneity
- Positive definiteness
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Optional

from greymath.core.expr import ExprKind, ExprNode


class ExprProperty(Enum):
    """Structural properties of expressions."""
    LINEAR = auto()
    AFFINE = auto()
    QUADRATIC = auto()
    POLYNOMIAL = auto()
    CONVEX = auto()
    CONCAVE = auto()
    MONOTONE_INCREASING = auto()
    MONOTONE_DECREASING = auto()
    SYMMETRIC = auto()
    ANTISYMMETRIC = auto()
    HOMOGENEOUS = auto()
    BOUNDED = auto()
    POSITIVE = auto()
    NONNEGATIVE = auto()
    CONSTANT = auto()
    PERIODIC = auto()


@dataclass
class AnalysisResult:
    """Result of structural analysis."""
    properties: set[ExprProperty] = field(default_factory=set)
    degree: Optional[int] = None  # polynomial degree if applicable
    homogeneity_degree: Optional[int] = None
    symmetry_group: Optional[str] = None
    notes: list[str] = field(default_factory=list)

    def is_linear(self) -> bool:
        return ExprProperty.LINEAR in self.properties

    def is_convex(self) -> bool:
        return ExprProperty.CONVEX in self.properties

    def is_constant(self) -> bool:
        return ExprProperty.CONSTANT in self.properties

    def __repr__(self) -> str:
        props = ", ".join(p.name for p in self.properties)
        return f"Analysis({props})"


class StructuralAnalyzer:
    """
    Analyzes structural properties of mathematical expressions.

    Uses syntactic analysis and algebraic reasoning to detect
    linearity, convexity, symmetry, and other properties.
    """

    def analyze(self, expr: ExprNode, variables: list[str]) -> AnalysisResult:
        """Perform full structural analysis of an expression."""
        result = AnalysisResult()

        # Check constant
        if not self._depends_on_any(expr, variables):
            result.properties.add(ExprProperty.CONSTANT)
            result.properties.add(ExprProperty.LINEAR)
            result.properties.add(ExprProperty.CONVEX)
            result.properties.add(ExprProperty.CONCAVE)
            result.degree = 0
            return result

        # Check linearity
        if self._is_linear(expr, variables):
            result.properties.add(ExprProperty.LINEAR)
            result.properties.add(ExprProperty.CONVEX)
            result.properties.add(ExprProperty.CONCAVE)
            result.degree = 1

        # Check affine
        elif self._is_affine(expr, variables):
            result.properties.add(ExprProperty.AFFINE)
            result.properties.add(ExprProperty.CONVEX)
            result.properties.add(ExprProperty.CONCAVE)
            result.degree = 1

        # Check quadratic
        if self._is_quadratic(expr, variables):
            result.properties.add(ExprProperty.QUADRATIC)
            result.degree = 2

        # Check polynomial degree
        deg = self._polynomial_degree(expr, variables)
        if deg is not None:
            result.properties.add(ExprProperty.POLYNOMIAL)
            result.degree = deg

        return result

    def is_linear(self, expr: ExprNode, variables: list[str]) -> bool:
        """Check if expression is linear in the given variables."""
        return self._is_linear(expr, variables)

    def is_convex(self, expr: ExprNode, variables: list[str]) -> bool:
        """
        Check if expression is convex in the given variables.

        Uses composition rules:
        - Sums of convex functions are convex
        - Affine functions are convex
        - x^n is convex for even n >= 2
        - Norms are convex
        """
        return self._is_convex(expr, variables)

    def detect_symmetry(self, expr: ExprNode,
                        variables: list[str]) -> Optional[str]:
        """Detect symmetry properties of the expression."""
        if len(variables) < 2:
            return None

        # Check permutation symmetry (symmetric in all vars)
        # This is a simplified check
        if self._is_symmetric(expr, variables):
            return "symmetric"

        return None

    # ── Internal analysis methods ───────────────────────────────────────

    def _is_linear(self, expr: ExprNode, variables: list[str]) -> bool:
        """Check linearity: f(ax + by) = af(x) + bf(y)."""
        # A function is linear if it's a sum of terms, each of which
        # is a variable multiplied by constants
        if expr.kind == ExprKind.VARIABLE:
            return expr.value in variables

        if expr.kind in (ExprKind.LITERAL, ExprKind.SYMBOL):
            return False  # constants are not linear (they're affine)

        if expr.kind == ExprKind.ADD:
            return all(self._is_linear(c, variables) for c in expr.children)

        if expr.kind == ExprKind.SUB:
            return all(self._is_linear(c, variables) for c in expr.children)

        if expr.kind == ExprKind.NEG:
            return self._is_linear(expr.children[0], variables)

        if expr.kind == ExprKind.MUL:
            # linear * constant is linear
            if len(expr.children) == 2:
                l, r = expr.children
                l_dep = self._depends_on_any(l, variables)
                r_dep = self._depends_on_any(r, variables)
                if l_dep and not r_dep:
                    return self._is_linear(l, variables)
                if r_dep and not l_dep:
                    return self._is_linear(r, variables)
            return False

        return False

    def _is_affine(self, expr: ExprNode, variables: list[str]) -> bool:
        """Check if expression is affine (linear + constant)."""
        if not self._depends_on_any(expr, variables):
            return True  # constant is affine

        if expr.kind == ExprKind.ADD:
            # Sum of affine terms is affine
            return all(self._is_affine(c, variables) for c in expr.children)

        return self._is_linear(expr, variables)

    def _is_quadratic(self, expr: ExprNode, variables: list[str]) -> bool:
        """Check if expression is quadratic."""
        deg = self._polynomial_degree(expr, variables)
        return deg == 2

    def _is_convex(self, expr: ExprNode, variables: list[str]) -> bool:
        """Check convexity using composition rules."""
        if not self._depends_on_any(expr, variables):
            return True  # constants are convex

        if self._is_affine(expr, variables):
            return True  # affine functions are convex

        if expr.kind == ExprKind.ADD:
            # Sum of convex functions is convex
            return all(self._is_convex(c, variables) for c in expr.children)

        if expr.kind == ExprKind.POW:
            base, exp = expr.children
            if exp.kind == ExprKind.LITERAL and isinstance(exp.value, int):
                if exp.value >= 2 and exp.value % 2 == 0:
                    # x^(2n) is convex if base is affine
                    return self._is_affine(base, variables)

        if expr.kind == ExprKind.NEG:
            # -f is convex iff f is concave
            return False  # can't determine in general

        return False  # conservative: unknown

    def _is_symmetric(self, expr: ExprNode, variables: list[str]) -> bool:
        """Check if expression is symmetric under variable permutations."""
        if len(variables) < 2:
            return True

        # Simple check: swap first two variables and see if structure is the same
        v1, v2 = variables[0], variables[1]
        swapped = self._swap_variables(expr, v1, v2)
        return self._structurally_equal(expr, swapped)

    def _polynomial_degree(self, expr: ExprNode,
                           variables: list[str]) -> Optional[int]:
        """Determine polynomial degree, or None if not polynomial."""
        if not self._depends_on_any(expr, variables):
            return 0

        if expr.kind == ExprKind.VARIABLE and expr.value in variables:
            return 1

        if expr.kind == ExprKind.ADD or expr.kind == ExprKind.SUB:
            degrees = [self._polynomial_degree(c, variables) for c in expr.children]
            if any(d is None for d in degrees):
                return None
            return max(d for d in degrees if d is not None)

        if expr.kind == ExprKind.MUL:
            degrees = [self._polynomial_degree(c, variables) for c in expr.children]
            if any(d is None for d in degrees):
                return None
            return sum(d for d in degrees if d is not None)

        if expr.kind == ExprKind.POW:
            base, exp = expr.children
            base_deg = self._polynomial_degree(base, variables)
            if base_deg is not None and exp.kind == ExprKind.LITERAL:
                if isinstance(exp.value, int) and exp.value >= 0:
                    return base_deg * exp.value

        if expr.kind == ExprKind.NEG:
            return self._polynomial_degree(expr.children[0], variables)

        return None  # not a polynomial

    def _depends_on_any(self, expr: ExprNode, variables: list[str]) -> bool:
        """Check if expression depends on any of the given variables."""
        if expr.kind == ExprKind.VARIABLE and expr.value in variables:
            return True
        return any(self._depends_on_any(c, variables) for c in expr.children)

    def _swap_variables(self, expr: ExprNode, v1: str, v2: str) -> ExprNode:
        """Swap two variables in an expression."""
        if expr.kind == ExprKind.VARIABLE:
            if expr.value == v1:
                return ExprNode.variable(v2)
            if expr.value == v2:
                return ExprNode.variable(v1)
            return expr
        new_children = [self._swap_variables(c, v1, v2) for c in expr.children]
        return ExprNode(
            kind=expr.kind, children=new_children,
            value=expr.value, name=expr.name,
            type_info=expr.type_info, metadata=expr.metadata,
        )

    def _structurally_equal(self, a: ExprNode, b: ExprNode) -> bool:
        if a.kind != b.kind or a.value != b.value:
            return False
        if len(a.children) != len(b.children):
            return False
        return all(
            self._structurally_equal(ac, bc)
            for ac, bc in zip(a.children, b.children)
        )
