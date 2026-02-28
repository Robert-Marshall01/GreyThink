"""
Pattern matching for the symbolic engine.

Implements structural pattern matching on expression trees/DAGs
with support for wildcards, typed patterns, and conditional matching.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Callable, Optional

from greymath.core.expr import ExprKind, ExprNode


class PatternKind(Enum):
    """Types of pattern elements."""
    EXACT = auto()       # match exact node
    WILDCARD = auto()    # match any single node (capture)
    WILDCARD_SEQ = auto()  # match zero or more nodes
    TYPED = auto()       # match node of specific kind
    PREDICATE = auto()   # match if predicate holds
    ALTERNATIVE = auto()  # match any of several patterns


@dataclass
class Pattern:
    """
    A pattern for matching expression tree structures.

    Supports:
    - Exact matching on ExprKind and value
    - Wildcard captures with named bindings
    - Type-constrained wildcards
    - Predicate-based matching
    - Alternation (OR patterns)
    """
    kind: PatternKind
    expr_kind: Optional[ExprKind] = None
    value: Any = None
    name: Optional[str] = None  # capture name for wildcards
    children: list["Pattern"] = field(default_factory=list)
    predicate: Optional[Callable[[ExprNode], bool]] = None
    alternatives: list["Pattern"] = field(default_factory=list)

    # ── Constructors ─────────────────────────────────────────────────────

    @staticmethod
    def exact(expr_kind: ExprKind, value: Any = None,
              children: Optional[list["Pattern"]] = None) -> "Pattern":
        """Match an exact node kind and optional value."""
        return Pattern(
            kind=PatternKind.EXACT,
            expr_kind=expr_kind,
            value=value,
            children=children or [],
        )

    @staticmethod
    def wildcard(name: str = "_") -> "Pattern":
        """Match any single expression node and capture it."""
        return Pattern(kind=PatternKind.WILDCARD, name=name)

    @staticmethod
    def typed(expr_kind: ExprKind, name: Optional[str] = None) -> "Pattern":
        """Match any node of a specific kind."""
        return Pattern(kind=PatternKind.TYPED, expr_kind=expr_kind, name=name)

    @staticmethod
    def where(predicate: Callable[[ExprNode], bool],
              name: Optional[str] = None) -> "Pattern":
        """Match any node satisfying a predicate."""
        return Pattern(kind=PatternKind.PREDICATE, predicate=predicate, name=name)

    @staticmethod
    def alt(*patterns: "Pattern") -> "Pattern":
        """Match any of several alternative patterns."""
        return Pattern(kind=PatternKind.ALTERNATIVE, alternatives=list(patterns))

    # ── Pattern operators ────────────────────────────────────────────────

    @staticmethod
    def add(left: "Pattern", right: "Pattern") -> "Pattern":
        return Pattern.exact(ExprKind.ADD, children=[left, right])

    @staticmethod
    def mul(left: "Pattern", right: "Pattern") -> "Pattern":
        return Pattern.exact(ExprKind.MUL, children=[left, right])

    @staticmethod
    def pow_pat(base: "Pattern", exp: "Pattern") -> "Pattern":
        return Pattern.exact(ExprKind.POW, children=[base, exp])

    @staticmethod
    def neg(operand: "Pattern") -> "Pattern":
        return Pattern.exact(ExprKind.NEG, children=[operand])

    @staticmethod
    def diff_pat(expr: "Pattern", var: "Pattern") -> "Pattern":
        return Pattern.exact(ExprKind.DIFF, children=[expr, var])

    def __repr__(self) -> str:
        if self.kind == PatternKind.WILDCARD:
            return f"?{self.name}"
        if self.kind == PatternKind.EXACT:
            return f"P({self.expr_kind.name if self.expr_kind else '?'})"
        if self.kind == PatternKind.TYPED:
            return f"P(:{self.expr_kind.name if self.expr_kind else '?'})"
        return f"P({self.kind.name})"


@dataclass
class MatchResult:
    """Result of a pattern match, containing captured bindings."""
    matched: bool = False
    bindings: dict[str, ExprNode] = field(default_factory=dict)

    @staticmethod
    def success(bindings: Optional[dict[str, ExprNode]] = None) -> "MatchResult":
        return MatchResult(matched=True, bindings=bindings or {})

    @staticmethod
    def failure() -> "MatchResult":
        return MatchResult(matched=False)

    def __bool__(self) -> bool:
        return self.matched


class PatternMatcher:
    """
    Matches patterns against expression trees.

    Supports recursive structural matching with backtracking,
    wildcard captures, and conditional predicates.
    """

    def match(self, pattern: Pattern, expr: ExprNode) -> MatchResult:
        """Attempt to match pattern against expr, returning bindings on success."""
        return self._match_impl(pattern, expr, {})

    def find_all(self, pattern: Pattern, expr: ExprNode) -> list[MatchResult]:
        """Find all subexpressions matching the pattern."""
        results: list[MatchResult] = []
        self._find_all_impl(pattern, expr, results)
        return results

    def _match_impl(self, pattern: Pattern, expr: ExprNode,
                    bindings: dict[str, ExprNode]) -> MatchResult:
        """Core matching implementation."""
        if pattern.kind == PatternKind.WILDCARD:
            # Wildcard: match anything, capture if named
            if pattern.name and pattern.name != "_":
                if pattern.name in bindings:
                    # Check consistency
                    existing = bindings[pattern.name]
                    if not self._structurally_equal(existing, expr):
                        return MatchResult.failure()
                else:
                    bindings[pattern.name] = expr
            return MatchResult.success(bindings)

        elif pattern.kind == PatternKind.EXACT:
            # Exact: match kind and optionally value
            if pattern.expr_kind is not None and expr.kind != pattern.expr_kind:
                return MatchResult.failure()
            if pattern.value is not None and expr.value != pattern.value:
                return MatchResult.failure()
            # Match children
            if pattern.children:
                if len(pattern.children) != len(expr.children):
                    return MatchResult.failure()
                for p_child, e_child in zip(pattern.children, expr.children):
                    result = self._match_impl(p_child, e_child, bindings)
                    if not result:
                        return MatchResult.failure()
            return MatchResult.success(bindings)

        elif pattern.kind == PatternKind.TYPED:
            # Typed: match any node of specific kind
            if pattern.expr_kind is not None and expr.kind != pattern.expr_kind:
                return MatchResult.failure()
            if pattern.name and pattern.name != "_":
                bindings[pattern.name] = expr
            return MatchResult.success(bindings)

        elif pattern.kind == PatternKind.PREDICATE:
            # Predicate: match if predicate holds
            if pattern.predicate is not None and not pattern.predicate(expr):
                return MatchResult.failure()
            if pattern.name and pattern.name != "_":
                bindings[pattern.name] = expr
            return MatchResult.success(bindings)

        elif pattern.kind == PatternKind.ALTERNATIVE:
            # Alternative: try each pattern
            for alt in pattern.alternatives:
                result = self._match_impl(alt, expr, dict(bindings))
                if result:
                    return result
            return MatchResult.failure()

        return MatchResult.failure()

    def _find_all_impl(self, pattern: Pattern, expr: ExprNode,
                       results: list[MatchResult]) -> None:
        """Recursively find all matching subexpressions."""
        result = self.match(pattern, expr)
        if result:
            results.append(result)
        for child in expr.children:
            self._find_all_impl(pattern, child, results)

    def _structurally_equal(self, a: ExprNode, b: ExprNode) -> bool:
        """Check structural equality of two expression nodes."""
        if a.kind != b.kind or a.value != b.value:
            return False
        if len(a.children) != len(b.children):
            return False
        return all(
            self._structurally_equal(ac, bc)
            for ac, bc in zip(a.children, b.children)
        )
