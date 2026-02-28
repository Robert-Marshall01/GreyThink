"""
Rewrite rule engine for the Grey Math symbolic compiler.

Provides:
- Rule definition with pattern → replacement
- Priority-based rule registry
- Strategy engines (normalization, greedy, exhaustive)
- Convergence-checked rewriting
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Callable, Optional

from greymath.core.expr import ExprKind, ExprNode
from greymath.symbolic.pattern import Pattern, PatternMatcher, MatchResult


class RewriteStrategy(Enum):
    """Strategies for applying rewrite rules."""
    NORMALIZE = auto()     # apply rules until fixed point
    GREEDY = auto()        # apply first matching rule
    EXHAUSTIVE = auto()    # explore all rewrites
    BOTTOM_UP = auto()     # rewrite leaves first
    TOP_DOWN = auto()      # rewrite root first


@dataclass
class RewriteRule:
    """
    A rewrite rule: pattern → replacement.

    When the pattern matches a subexpression, the replacement function
    is called with the match bindings to produce the rewritten expression.
    """
    name: str
    pattern: Pattern
    replacement: Callable[[dict[str, ExprNode]], ExprNode]
    priority: int = 0  # higher = applied first
    condition: Optional[Callable[[dict[str, ExprNode]], bool]] = None
    description: str = ""
    category: str = "general"

    def apply(self, expr: ExprNode, matcher: PatternMatcher) -> Optional[ExprNode]:
        """Try to apply this rule to an expression."""
        result = matcher.match(self.pattern, expr)
        if not result:
            return None
        if self.condition and not self.condition(result.bindings):
            return None
        return self.replacement(result.bindings)

    def __repr__(self) -> str:
        return f"Rule({self.name}, priority={self.priority})"


class RuleRegistry:
    """
    Registry of rewrite rules organized by priority and category.
    """

    def __init__(self) -> None:
        self._rules: list[RewriteRule] = []
        self._categories: dict[str, list[RewriteRule]] = {}

    def register(self, rule: RewriteRule) -> None:
        """Register a rewrite rule."""
        self._rules.append(rule)
        self._rules.sort(key=lambda r: -r.priority)
        cat = rule.category
        if cat not in self._categories:
            self._categories[cat] = []
        self._categories[cat].append(rule)
        self._categories[cat].sort(key=lambda r: -r.priority)

    def get_rules(self, category: Optional[str] = None) -> list[RewriteRule]:
        """Get rules, optionally filtered by category."""
        if category is not None:
            return self._categories.get(category, [])
        return list(self._rules)

    def remove(self, name: str) -> None:
        """Remove a rule by name."""
        self._rules = [r for r in self._rules if r.name != name]
        for cat in self._categories:
            self._categories[cat] = [
                r for r in self._categories[cat] if r.name != name
            ]

    @property
    def count(self) -> int:
        return len(self._rules)

    def __repr__(self) -> str:
        return f"RuleRegistry({self.count} rules)"


class RewriteEngine:
    """
    Engine that applies rewrite rules to expression trees.

    Supports multiple strategies:
    - NORMALIZE: apply rules bottom-up until fixed point
    - GREEDY: apply first matching rule at each step
    - TOP_DOWN: rewrite from root down
    - BOTTOM_UP: rewrite from leaves up
    """

    def __init__(self, registry: Optional[RuleRegistry] = None,
                 max_iterations: int = 1000) -> None:
        self.registry = registry or RuleRegistry()
        self.matcher = PatternMatcher()
        self.max_iterations = max_iterations
        self._steps: list[tuple[str, ExprNode]] = []

    @property
    def steps(self) -> list[tuple[str, ExprNode]]:
        """Return the rewrite steps taken in the last rewrite."""
        return list(self._steps)

    def rewrite(self, expr: ExprNode,
                strategy: RewriteStrategy = RewriteStrategy.NORMALIZE,
                category: Optional[str] = None) -> ExprNode:
        """Rewrite an expression using the specified strategy."""
        self._steps = []
        rules = self.registry.get_rules(category)

        if strategy == RewriteStrategy.NORMALIZE:
            return self._normalize(expr, rules)
        elif strategy == RewriteStrategy.GREEDY:
            return self._greedy(expr, rules)
        elif strategy == RewriteStrategy.BOTTOM_UP:
            return self._bottom_up(expr, rules)
        elif strategy == RewriteStrategy.TOP_DOWN:
            return self._top_down(expr, rules)
        else:
            return self._normalize(expr, rules)

    def _normalize(self, expr: ExprNode, rules: list[RewriteRule]) -> ExprNode:
        """Apply rules bottom-up until convergence."""
        current = expr
        for iteration in range(self.max_iterations):
            rewritten = self._single_pass_bottom_up(current, rules)
            if self._structurally_equal(current, rewritten):
                break  # fixed point reached
            current = rewritten
        return current

    def _greedy(self, expr: ExprNode, rules: list[RewriteRule]) -> ExprNode:
        """Apply the first matching rule at each position."""
        for rule in rules:
            result = rule.apply(expr, self.matcher)
            if result is not None:
                self._steps.append((rule.name, result))
                return result
        # Try children
        new_children = [self._greedy(c, rules) for c in expr.children]
        return ExprNode(
            kind=expr.kind, children=new_children,
            value=expr.value, name=expr.name,
            type_info=expr.type_info, metadata=expr.metadata,
        )

    def _bottom_up(self, expr: ExprNode, rules: list[RewriteRule]) -> ExprNode:
        """Rewrite bottom-up (leaves first)."""
        new_children = [self._bottom_up(c, rules) for c in expr.children]
        current = ExprNode(
            kind=expr.kind, children=new_children,
            value=expr.value, name=expr.name,
            type_info=expr.type_info, metadata=expr.metadata,
        )
        return self._apply_rules_once(current, rules)

    def _top_down(self, expr: ExprNode, rules: list[RewriteRule]) -> ExprNode:
        """Rewrite top-down (root first)."""
        current = self._apply_rules_once(expr, rules)
        new_children = [self._top_down(c, rules) for c in current.children]
        return ExprNode(
            kind=current.kind, children=new_children,
            value=current.value, name=current.name,
            type_info=current.type_info, metadata=current.metadata,
        )

    def _single_pass_bottom_up(self, expr: ExprNode,
                                rules: list[RewriteRule]) -> ExprNode:
        """One bottom-up pass applying rules at each node."""
        new_children = [
            self._single_pass_bottom_up(c, rules)
            for c in expr.children
        ]
        current = ExprNode(
            kind=expr.kind, children=new_children,
            value=expr.value, name=expr.name,
            type_info=expr.type_info, metadata=expr.metadata,
        )
        return self._apply_rules_once(current, rules)

    def _apply_rules_once(self, expr: ExprNode,
                          rules: list[RewriteRule]) -> ExprNode:
        """Try each rule on expr, return first successful rewrite or original."""
        for rule in rules:
            result = rule.apply(expr, self.matcher)
            if result is not None:
                self._steps.append((rule.name, result))
                return result
        return expr

    def _structurally_equal(self, a: ExprNode, b: ExprNode) -> bool:
        if a.kind != b.kind or a.value != b.value:
            return False
        if len(a.children) != len(b.children):
            return False
        return all(
            self._structurally_equal(ac, bc)
            for ac, bc in zip(a.children, b.children)
        )
