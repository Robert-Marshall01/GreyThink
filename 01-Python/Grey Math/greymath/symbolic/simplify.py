"""
Algebraic simplification engine.

Provides standard algebraic simplification rules:
- Additive/multiplicative identity and annihilation
- Commutativity and associativity folding
- Constant folding
- Distributive law
- Power rules
- Logarithmic and trigonometric identities
"""

from __future__ import annotations

from typing import Optional

from greymath.core.expr import ExprKind, ExprNode
from greymath.symbolic.pattern import Pattern, PatternMatcher
from greymath.symbolic.rewrite import RewriteEngine, RewriteRule, RuleRegistry


def _is_zero(node: ExprNode) -> bool:
    return node.kind == ExprKind.LITERAL and node.value == 0


def _is_one(node: ExprNode) -> bool:
    return node.kind == ExprKind.LITERAL and node.value == 1


def _is_literal(node: ExprNode) -> bool:
    return node.kind == ExprKind.LITERAL and isinstance(node.value, (int, float))


class Simplifier:
    """
    Algebraic simplification engine.

    Applies a curated set of rewrite rules to simplify expressions.
    Rules are organized by category and priority.
    """

    def __init__(self) -> None:
        self.registry = RuleRegistry()
        self.engine = RewriteEngine(self.registry)
        self._register_standard_rules()

    def simplify(self, expr: ExprNode, max_iterations: int = 100) -> ExprNode:
        """Simplify an expression by applying rewrite rules until convergence."""
        self.engine.max_iterations = max_iterations
        return self.engine.rewrite(expr)

    def _register_standard_rules(self) -> None:
        """Register all standard algebraic simplification rules."""
        # ── Additive identity: x + 0 = x ────────────────────────────────
        self.registry.register(RewriteRule(
            name="add_zero_right",
            pattern=Pattern.add(Pattern.wildcard("x"),
                               Pattern.where(_is_zero)),
            replacement=lambda b: b["x"],
            priority=100,
            category="arithmetic",
            description="x + 0 → x",
        ))

        self.registry.register(RewriteRule(
            name="add_zero_left",
            pattern=Pattern.add(Pattern.where(_is_zero),
                               Pattern.wildcard("x")),
            replacement=lambda b: b["x"],
            priority=100,
            category="arithmetic",
            description="0 + x → x",
        ))

        # ── Multiplicative identity: x * 1 = x ─────────────────────────
        self.registry.register(RewriteRule(
            name="mul_one_right",
            pattern=Pattern.mul(Pattern.wildcard("x"),
                               Pattern.where(_is_one)),
            replacement=lambda b: b["x"],
            priority=100,
            category="arithmetic",
            description="x * 1 → x",
        ))

        self.registry.register(RewriteRule(
            name="mul_one_left",
            pattern=Pattern.mul(Pattern.where(_is_one),
                               Pattern.wildcard("x")),
            replacement=lambda b: b["x"],
            priority=100,
            category="arithmetic",
            description="1 * x → x",
        ))

        # ── Multiplicative annihilation: x * 0 = 0 ─────────────────────
        self.registry.register(RewriteRule(
            name="mul_zero_right",
            pattern=Pattern.mul(Pattern.wildcard("x"),
                               Pattern.where(_is_zero)),
            replacement=lambda b: ExprNode.literal(0),
            priority=100,
            category="arithmetic",
            description="x * 0 → 0",
        ))

        self.registry.register(RewriteRule(
            name="mul_zero_left",
            pattern=Pattern.mul(Pattern.where(_is_zero),
                               Pattern.wildcard("x")),
            replacement=lambda b: ExprNode.literal(0),
            priority=100,
            category="arithmetic",
            description="0 * x → 0",
        ))

        # ── Double negation: -(-x) = x ─────────────────────────────────
        self.registry.register(RewriteRule(
            name="double_neg",
            pattern=Pattern.neg(Pattern.neg(Pattern.wildcard("x"))),
            replacement=lambda b: b["x"],
            priority=90,
            category="arithmetic",
            description="--x → x",
        ))

        # ── x - x = 0 ──────────────────────────────────────────────────
        self.registry.register(RewriteRule(
            name="sub_self",
            pattern=Pattern.exact(ExprKind.SUB, children=[
                Pattern.wildcard("x"), Pattern.wildcard("x")
            ]),
            replacement=lambda b: ExprNode.literal(0),
            priority=90,
            category="arithmetic",
            description="x - x → 0",
        ))

        # ── x / x = 1 (x ≠ 0) ──────────────────────────────────────────
        self.registry.register(RewriteRule(
            name="div_self",
            pattern=Pattern.exact(ExprKind.DIV, children=[
                Pattern.wildcard("x"), Pattern.wildcard("x")
            ]),
            replacement=lambda b: ExprNode.literal(1),
            condition=lambda b: not _is_zero(b["x"]),
            priority=90,
            category="arithmetic",
            description="x / x → 1 (x ≠ 0)",
        ))

        # ── Power rules ────────────────────────────────────────────────
        self.registry.register(RewriteRule(
            name="pow_zero",
            pattern=Pattern.pow_pat(Pattern.wildcard("x"),
                                    Pattern.where(_is_zero)),
            replacement=lambda b: ExprNode.literal(1),
            priority=90,
            category="power",
            description="x^0 → 1",
        ))

        self.registry.register(RewriteRule(
            name="pow_one",
            pattern=Pattern.pow_pat(Pattern.wildcard("x"),
                                    Pattern.where(_is_one)),
            replacement=lambda b: b["x"],
            priority=90,
            category="power",
            description="x^1 → x",
        ))

        # ── Constant folding ───────────────────────────────────────────
        self.registry.register(RewriteRule(
            name="constant_fold_add",
            pattern=Pattern.add(
                Pattern.where(_is_literal, "a"),
                Pattern.where(_is_literal, "b"),
            ),
            replacement=lambda b: ExprNode.literal(b["a"].value + b["b"].value),
            priority=80,
            category="constant_folding",
            description="constant + constant → result",
        ))

        self.registry.register(RewriteRule(
            name="constant_fold_mul",
            pattern=Pattern.mul(
                Pattern.where(_is_literal, "a"),
                Pattern.where(_is_literal, "b"),
            ),
            replacement=lambda b: ExprNode.literal(b["a"].value * b["b"].value),
            priority=80,
            category="constant_folding",
            description="constant * constant → result",
        ))

        self.registry.register(RewriteRule(
            name="constant_fold_sub",
            pattern=Pattern.exact(ExprKind.SUB, children=[
                Pattern.where(_is_literal, "a"),
                Pattern.where(_is_literal, "b"),
            ]),
            replacement=lambda b: ExprNode.literal(b["a"].value - b["b"].value),
            priority=80,
            category="constant_folding",
            description="constant - constant → result",
        ))

        # ── Transpose of transpose ─────────────────────────────────────
        self.registry.register(RewriteRule(
            name="transpose_transpose",
            pattern=Pattern.exact(ExprKind.TRANSPOSE, children=[
                Pattern.exact(ExprKind.TRANSPOSE, children=[
                    Pattern.wildcard("A")
                ])
            ]),
            replacement=lambda b: b["A"],
            priority=85,
            category="linear_algebra",
            description="(A^T)^T → A",
        ))

        # ── Inverse of inverse ─────────────────────────────────────────
        self.registry.register(RewriteRule(
            name="inverse_inverse",
            pattern=Pattern.exact(ExprKind.INVERSE, children=[
                Pattern.exact(ExprKind.INVERSE, children=[
                    Pattern.wildcard("A")
                ])
            ]),
            replacement=lambda b: b["A"],
            priority=85,
            category="linear_algebra",
            description="(A^{-1})^{-1} → A",
        ))
