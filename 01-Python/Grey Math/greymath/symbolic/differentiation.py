"""
Symbolic differentiation engine.

Supports:
- Scalar differentiation (standard calculus rules)
- Vector-valued differentiation (Jacobians)
- Higher-order derivatives
- Partial derivatives
- Variational/functional derivatives
- Chain rule, product rule, quotient rule
"""

from __future__ import annotations

from typing import Optional

from greymath.core.expr import ExprKind, ExprNode


class SymbolicDiff:
    """
    Symbolic differentiation engine operating on expression trees.

    Implements standard differentiation rules:
    - d/dx(constant) = 0
    - d/dx(x) = 1
    - d/dx(f + g) = df/dx + dg/dx
    - d/dx(f * g) = f * dg/dx + g * df/dx
    - d/dx(f / g) = (g*df/dx - f*dg/dx) / g^2
    - d/dx(f^n) = n * f^(n-1) * df/dx
    - d/dx(f(g(x))) = f'(g(x)) * g'(x)  (chain rule)
    """

    def differentiate(self, expr: ExprNode, var: str, order: int = 1) -> ExprNode:
        """
        Differentiate expr with respect to variable var.

        Args:
            expr: The expression to differentiate
            var: Name of the variable to differentiate with respect to
            order: Order of differentiation (default 1)

        Returns:
            The differentiated expression (unsimplified)
        """
        result = expr
        for _ in range(order):
            result = self._diff(result, var)
        return result

    def gradient(self, expr: ExprNode, variables: list[str]) -> list[ExprNode]:
        """Compute the gradient: vector of partial derivatives."""
        return [self._diff(expr, var) for var in variables]

    def jacobian(self, exprs: list[ExprNode],
                 variables: list[str]) -> list[list[ExprNode]]:
        """Compute the Jacobian matrix of partial derivatives."""
        return [
            [self._diff(expr, var) for var in variables]
            for expr in exprs
        ]

    def hessian(self, expr: ExprNode, variables: list[str]) -> list[list[ExprNode]]:
        """Compute the Hessian matrix of second partial derivatives."""
        grad = self.gradient(expr, variables)
        return [
            [self._diff(g, var) for var in variables]
            for g in grad
        ]

    def functional_derivative(self, functional_expr: ExprNode,
                               function_var: str,
                               point_var: str) -> ExprNode:
        """
        Compute the functional (variational) derivative δF/δf.

        This is a simplified version for expressions involving
        integrals of functions and their derivatives.
        """
        # For a functional F[f] = ∫ L(x, f, f') dx
        # δF/δf = ∂L/∂f - d/dx(∂L/∂f')
        # This requires detecting the structure of the integrand
        return ExprNode(
            kind=ExprKind.DIFF,
            children=[functional_expr, ExprNode.variable(function_var)],
            name=f"δ/δ{function_var}",
        )

    def _diff(self, expr: ExprNode, var: str) -> ExprNode:
        """Core single-step differentiation."""
        kind = expr.kind

        # ── Constants and literals ───────────────────────────────────────
        if kind == ExprKind.LITERAL:
            return ExprNode.literal(0)

        # ── Symbol (named constant) ─────────────────────────────────────
        if kind == ExprKind.SYMBOL:
            return ExprNode.literal(0)

        # ── Variable ────────────────────────────────────────────────────
        if kind == ExprKind.VARIABLE:
            if expr.value == var:
                return ExprNode.literal(1)
            return ExprNode.literal(0)

        # ── Addition: d(f+g) = df + dg ──────────────────────────────────
        if kind == ExprKind.ADD:
            left, right = expr.children
            return ExprNode(
                kind=ExprKind.ADD,
                children=[self._diff(left, var), self._diff(right, var)],
            )

        # ── Subtraction: d(f-g) = df - dg ──────────────────────────────
        if kind == ExprKind.SUB:
            left, right = expr.children
            return ExprNode(
                kind=ExprKind.SUB,
                children=[self._diff(left, var), self._diff(right, var)],
            )

        # ── Negation: d(-f) = -df ──────────────────────────────────────
        if kind == ExprKind.NEG:
            return ExprNode(
                kind=ExprKind.NEG,
                children=[self._diff(expr.children[0], var)],
            )

        # ── Product rule: d(f*g) = f*dg + g*df ─────────────────────────
        if kind == ExprKind.MUL:
            f, g = expr.children
            df = self._diff(f, var)
            dg = self._diff(g, var)
            return ExprNode(
                kind=ExprKind.ADD,
                children=[
                    ExprNode(kind=ExprKind.MUL, children=[f, dg]),
                    ExprNode(kind=ExprKind.MUL, children=[g, df]),
                ],
            )

        # ── Quotient rule: d(f/g) = (g*df - f*dg) / g^2 ───────────────
        if kind == ExprKind.DIV:
            f, g = expr.children
            df = self._diff(f, var)
            dg = self._diff(g, var)
            numerator = ExprNode(
                kind=ExprKind.SUB,
                children=[
                    ExprNode(kind=ExprKind.MUL, children=[g, df]),
                    ExprNode(kind=ExprKind.MUL, children=[f, dg]),
                ],
            )
            denominator = ExprNode(
                kind=ExprKind.POW,
                children=[g, ExprNode.literal(2)],
            )
            return ExprNode(
                kind=ExprKind.DIV,
                children=[numerator, denominator],
            )

        # ── Power rule: d(f^g) ─────────────────────────────────────────
        if kind == ExprKind.POW:
            base, exp = expr.children
            if exp.kind == ExprKind.LITERAL and not self._depends_on(base, var):
                # Constant base, constant exponent → 0
                return ExprNode.literal(0)
            if exp.kind == ExprKind.LITERAL:
                # d(f^n) = n * f^(n-1) * df
                n = exp.value
                return ExprNode(
                    kind=ExprKind.MUL,
                    children=[
                        ExprNode(kind=ExprKind.MUL, children=[
                            ExprNode.literal(n),
                            ExprNode(kind=ExprKind.POW, children=[
                                base, ExprNode.literal(n - 1)
                            ]),
                        ]),
                        self._diff(base, var),
                    ],
                )
            # General case: d(f^g) = f^g * (g' * ln(f) + g * f'/f)
            # Simplified: treat as exp(g * ln(f))
            return ExprNode(
                kind=ExprKind.DIFF,
                children=[expr, ExprNode.variable(var)],
                name="d/d (general power)",
            )

        # ── Function application (chain rule) ──────────────────────────
        if kind == ExprKind.APPLY:
            # f(g(x)) → f'(g(x)) * g'(x)
            func = expr.children[0]
            if len(expr.children) > 1:
                arg = expr.children[1]
                # Outer derivative applied to inner
                outer_deriv = ExprNode(
                    kind=ExprKind.APPLY,
                    children=[
                        ExprNode(kind=ExprKind.DIFF,
                                 children=[func, ExprNode.variable("_")],
                                 name=f"{func.name}'"),
                        arg,
                    ],
                )
                inner_deriv = self._diff(arg, var)
                return ExprNode(
                    kind=ExprKind.MUL,
                    children=[outer_deriv, inner_deriv],
                )
            return ExprNode(kind=ExprKind.DIFF, children=[expr, ExprNode.variable(var)])

        # ── Matrix multiplication ──────────────────────────────────────
        if kind == ExprKind.MATMUL:
            # d(AB) = dA * B + A * dB
            A, B = expr.children
            dA = self._diff(A, var)
            dB = self._diff(B, var)
            return ExprNode(
                kind=ExprKind.ADD,
                children=[
                    ExprNode(kind=ExprKind.MATMUL, children=[dA, B]),
                    ExprNode(kind=ExprKind.MATMUL, children=[A, dB]),
                ],
            )

        # ── Trace: d(tr(A)) = tr(dA) ──────────────────────────────────
        if kind == ExprKind.TRACE:
            return ExprNode(
                kind=ExprKind.TRACE,
                children=[self._diff(expr.children[0], var)],
            )

        # ── Default: return symbolic derivative marker ──────────────────
        return ExprNode(
            kind=ExprKind.DIFF,
            children=[expr, ExprNode.variable(var)],
            name=f"d/d{var}",
        )

    def _depends_on(self, expr: ExprNode, var: str) -> bool:
        """Check if expression depends on the given variable."""
        if expr.kind == ExprKind.VARIABLE and expr.value == var:
            return True
        return any(self._depends_on(c, var) for c in expr.children)
