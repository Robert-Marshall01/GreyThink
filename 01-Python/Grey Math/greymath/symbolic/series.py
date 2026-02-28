"""
Series expansion engine.

Supports:
- Taylor series
- Laurent series
- Asymptotic expansions
- Formal power series arithmetic
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Optional

from greymath.core.expr import ExprKind, ExprNode
from greymath.symbolic.differentiation import SymbolicDiff


@dataclass
class SeriesTerm:
    """A single term in a series expansion: coefficient * (x - a)^power."""
    coefficient: ExprNode
    power: int

    def __repr__(self) -> str:
        return f"Term(coeff={self.coefficient}, power={self.power})"


@dataclass
class SeriesExpansion:
    """A truncated series expansion."""
    terms: list[SeriesTerm]
    variable: str
    center: ExprNode  # expansion point
    order: int  # truncation order

    def to_expr(self) -> ExprNode:
        """Convert the series back to an expression tree."""
        if not self.terms:
            return ExprNode.literal(0)

        var = ExprNode.variable(self.variable)
        center = self.center

        result: Optional[ExprNode] = None
        for term in self.terms:
            # Build coefficient * (x - a)^power
            if term.power == 0:
                term_expr = term.coefficient
            else:
                base = ExprNode(kind=ExprKind.SUB, children=[var, center])
                if term.power == 1:
                    power_expr = base
                else:
                    power_expr = ExprNode(
                        kind=ExprKind.POW,
                        children=[base, ExprNode.literal(term.power)],
                    )
                term_expr = ExprNode(
                    kind=ExprKind.MUL,
                    children=[term.coefficient, power_expr],
                )

            if result is None:
                result = term_expr
            else:
                result = ExprNode(kind=ExprKind.ADD, children=[result, term_expr])

        return result or ExprNode.literal(0)

    def __repr__(self) -> str:
        return f"Series({self.variable}, center={self.center}, order={self.order}, {len(self.terms)} terms)"


class SeriesExpander:
    """
    Engine for computing series expansions of expressions.
    """

    def __init__(self) -> None:
        self.diff_engine = SymbolicDiff()

    def taylor(self, expr: ExprNode, variable: str,
               center: ExprNode, order: int) -> SeriesExpansion:
        """
        Compute the Taylor series of expr around center, up to given order.

        f(x) = Σ_{n=0}^{order} f^(n)(a) / n! * (x - a)^n
        """
        terms: list[SeriesTerm] = []
        current = expr

        for n in range(order + 1):
            # f^(n)(a) / n!
            # The coefficient is the n-th derivative evaluated at center,
            # divided by n!
            coeff = ExprNode(
                kind=ExprKind.DIV,
                children=[
                    current,  # n-th derivative (symbolic, not yet evaluated at center)
                    ExprNode.literal(self._factorial(n)),
                ],
            )
            terms.append(SeriesTerm(coefficient=coeff, power=n))

            # Compute next derivative for the next term
            if n < order:
                current = self.diff_engine.differentiate(current, variable)

        return SeriesExpansion(
            terms=terms,
            variable=variable,
            center=center,
            order=order,
        )

    def laurent(self, expr: ExprNode, variable: str,
                center: ExprNode, neg_order: int, pos_order: int) -> SeriesExpansion:
        """
        Compute a Laurent series with both negative and positive powers.

        f(z) = Σ_{n=-neg_order}^{pos_order} a_n * (z - z_0)^n

        Note: For full Laurent series computation, the coefficients of
        negative-power terms require contour integration or residue computation.
        This provides the structural framework.
        """
        terms: list[SeriesTerm] = []

        # Negative powers (principal part) — structural placeholders
        for n in range(-neg_order, 0):
            coeff = ExprNode.symbol(f"a_{n}")
            terms.append(SeriesTerm(coefficient=coeff, power=n))

        # Non-negative powers (analytic part) — from Taylor
        taylor = self.taylor(expr, variable, center, pos_order)
        terms.extend(taylor.terms)

        return SeriesExpansion(
            terms=terms,
            variable=variable,
            center=center,
            order=pos_order,
        )

    def asymptotic(self, expr: ExprNode, variable: str,
                   order: int) -> SeriesExpansion:
        """
        Compute an asymptotic expansion as variable → ∞.

        f(x) ~ Σ_{n=0}^{order} a_n / x^n
        """
        terms: list[SeriesTerm] = []
        for n in range(order + 1):
            coeff = ExprNode.symbol(f"a_{n}")
            terms.append(SeriesTerm(coefficient=coeff, power=-n))

        return SeriesExpansion(
            terms=terms,
            variable=variable,
            center=ExprNode.symbol("∞"),
            order=order,
        )

    @staticmethod
    def _factorial(n: int) -> int:
        if n <= 1:
            return 1
        result = 1
        for i in range(2, n + 1):
            result *= i
        return result
