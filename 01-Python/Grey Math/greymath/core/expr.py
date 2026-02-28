"""
Expression graph representation for the Grey Math IR.

All mathematical expressions are represented as directed acyclic graphs (DAGs)
of ExprNode objects. This enables:
- Structural sharing (common subexpression elimination)
- Pattern matching for rewrite rules
- Efficient symbolic manipulation
- Graph-based compositionality
"""

from __future__ import annotations

import uuid
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Optional, Sequence

from greymath.core.metadata import Metadata


class ExprKind(Enum):
    """Classification of expression nodes."""
    # Atomic
    LITERAL = auto()
    SYMBOL = auto()
    VARIABLE = auto()

    # Arithmetic
    ADD = auto()
    SUB = auto()
    MUL = auto()
    DIV = auto()
    POW = auto()
    NEG = auto()

    # Functions
    APPLY = auto()       # function application
    COMPOSE = auto()     # function composition
    LAMBDA = auto()      # anonymous function

    # Calculus
    DIFF = auto()        # differentiation
    INTEGRAL = auto()    # integration
    LIMIT = auto()       # limit
    SUM = auto()         # summation
    PRODUCT = auto()     # product

    # Linear algebra
    MATMUL = auto()
    TRANSPOSE = auto()
    INVERSE = auto()
    TRACE = auto()
    DETERMINANT = auto()
    KRONECKER = auto()

    # Tensor
    TENSOR_PRODUCT = auto()
    CONTRACT = auto()

    # Logical
    EQUAL = auto()
    LESS = auto()
    GREATER = auto()
    AND = auto()
    OR = auto()
    NOT = auto()
    FORALL = auto()
    EXISTS = auto()

    # Set operations
    UNION = auto()
    INTERSECTION = auto()
    SET_DIFF = auto()
    ELEMENT_OF = auto()

    # Special
    PIECEWISE = auto()
    CONDITIONAL = auto()
    INDEX = auto()       # indexing/subscript


@dataclass
class ExprNode:
    """
    A node in the expression DAG.

    Each node has a kind, optional children, an optional value (for literals),
    and metadata. The DAG structure allows structural sharing.
    """
    kind: ExprKind
    children: list["ExprNode"] = field(default_factory=list)
    value: Any = None           # for LITERAL, SYMBOL, VARIABLE
    name: Optional[str] = None  # human-readable label
    type_info: Optional[str] = None  # type annotation
    metadata: Metadata = field(default_factory=Metadata)
    id: str = field(default_factory=lambda: str(uuid.uuid4())[:12])

    # ── Construction helpers ─────────────────────────────────────────────

    @staticmethod
    def literal(value: Any, name: Optional[str] = None) -> "ExprNode":
        return ExprNode(kind=ExprKind.LITERAL, value=value, name=name)

    @staticmethod
    def symbol(name: str, type_info: Optional[str] = None) -> "ExprNode":
        return ExprNode(kind=ExprKind.SYMBOL, value=name, name=name, type_info=type_info)

    @staticmethod
    def variable(name: str, type_info: Optional[str] = None) -> "ExprNode":
        return ExprNode(kind=ExprKind.VARIABLE, value=name, name=name, type_info=type_info)

    # ── Arithmetic operators ─────────────────────────────────────────────

    def __add__(self, other: "ExprNode") -> "ExprNode":
        return ExprNode(kind=ExprKind.ADD, children=[self, other], name="+")

    def __sub__(self, other: "ExprNode") -> "ExprNode":
        return ExprNode(kind=ExprKind.SUB, children=[self, other], name="-")

    def __mul__(self, other: "ExprNode") -> "ExprNode":
        return ExprNode(kind=ExprKind.MUL, children=[self, other], name="*")

    def __truediv__(self, other: "ExprNode") -> "ExprNode":
        return ExprNode(kind=ExprKind.DIV, children=[self, other], name="/")

    def __pow__(self, other: "ExprNode") -> "ExprNode":
        return ExprNode(kind=ExprKind.POW, children=[self, other], name="^")

    def __neg__(self) -> "ExprNode":
        return ExprNode(kind=ExprKind.NEG, children=[self], name="-")

    # ── Calculus constructors ────────────────────────────────────────────

    def diff(self, var: "ExprNode", order: int = 1) -> "ExprNode":
        """Differentiate this expression with respect to var."""
        result = self
        for _ in range(order):
            result = ExprNode(kind=ExprKind.DIFF, children=[result, var], name="d/d")
        return result

    def integrate(self, var: "ExprNode",
                  lower: Optional["ExprNode"] = None,
                  upper: Optional["ExprNode"] = None) -> "ExprNode":
        """Integrate with optional bounds."""
        children = [self, var]
        if lower is not None:
            children.append(lower)
        if upper is not None:
            children.append(upper)
        return ExprNode(kind=ExprKind.INTEGRAL, children=children, name="∫")

    # ── Tree operations ──────────────────────────────────────────────────

    def is_leaf(self) -> bool:
        return len(self.children) == 0

    def is_atomic(self) -> bool:
        return self.kind in (ExprKind.LITERAL, ExprKind.SYMBOL, ExprKind.VARIABLE)

    def depth(self) -> int:
        if self.is_leaf():
            return 0
        return 1 + max(c.depth() for c in self.children)

    def size(self) -> int:
        """Count total nodes in subtree."""
        return 1 + sum(c.size() for c in self.children)

    def collect_symbols(self) -> set[str]:
        """Collect all symbol names in the expression."""
        symbols: set[str] = set()
        if self.kind == ExprKind.SYMBOL and self.value is not None:
            symbols.add(str(self.value))
        for child in self.children:
            symbols.update(child.collect_symbols())
        return symbols

    def collect_variables(self) -> set[str]:
        """Collect all variable names in the expression."""
        variables: set[str] = set()
        if self.kind == ExprKind.VARIABLE and self.value is not None:
            variables.add(str(self.value))
        for child in self.children:
            variables.update(child.collect_variables())
        return variables

    def substitute(self, var_name: str, replacement: "ExprNode") -> "ExprNode":
        """Substitute all occurrences of a variable with a replacement expression."""
        if self.kind in (ExprKind.VARIABLE, ExprKind.SYMBOL) and self.value == var_name:
            return replacement
        if not self.children:
            return self
        new_children = [c.substitute(var_name, replacement) for c in self.children]
        return ExprNode(
            kind=self.kind,
            children=new_children,
            value=self.value,
            name=self.name,
            type_info=self.type_info,
            metadata=self.metadata,
        )

    def map(self, fn: Any) -> "ExprNode":
        """Apply fn to every node in post-order."""
        new_children = [c.map(fn) for c in self.children]
        new_node = ExprNode(
            kind=self.kind,
            children=new_children,
            value=self.value,
            name=self.name,
            type_info=self.type_info,
            metadata=self.metadata,
        )
        return fn(new_node)

    def to_string(self, indent: int = 0) -> str:
        """Pretty-print the expression tree."""
        prefix = "  " * indent
        parts = [f"{prefix}{self.kind.name}"]
        if self.value is not None:
            parts[0] += f"({self.value})"
        if self.name and self.kind not in (ExprKind.SYMBOL, ExprKind.VARIABLE, ExprKind.LITERAL):
            parts[0] += f" [{self.name}]"
        for child in self.children:
            parts.append(child.to_string(indent + 1))
        return "\n".join(parts)

    def __repr__(self) -> str:
        if self.is_atomic():
            return f"Expr({self.value})"
        kind = self.kind.name
        children_str = ", ".join(repr(c) for c in self.children)
        return f"Expr({kind}, [{children_str}])"


@dataclass
class ExprDAG:
    """
    A directed acyclic graph of expressions with structural sharing.

    Provides canonical hash-consing so that identical subexpressions
    share the same node, enabling efficient equivalence checking.
    """
    nodes: dict[str, ExprNode] = field(default_factory=dict)
    roots: list[str] = field(default_factory=list)
    _hash_table: dict[int, str] = field(default_factory=dict)

    def add(self, node: ExprNode) -> str:
        """Add a node to the DAG, reusing existing identical nodes."""
        h = self._structural_hash(node)
        if h in self._hash_table:
            return self._hash_table[h]
        self.nodes[node.id] = node
        self._hash_table[h] = node.id
        return node.id

    def add_root(self, node: ExprNode) -> str:
        """Add a root expression to the DAG."""
        nid = self.add(node)
        if nid not in self.roots:
            self.roots.append(nid)
        return nid

    def get(self, node_id: str) -> Optional[ExprNode]:
        return self.nodes.get(node_id)

    def _structural_hash(self, node: ExprNode) -> int:
        """Compute a structural hash for canonical deduplication."""
        child_hashes = tuple(self._structural_hash(c) for c in node.children)
        return hash((node.kind, node.value, child_hashes))

    def topological_order(self) -> list[ExprNode]:
        """Return nodes in topological order (leaves first)."""
        visited: set[str] = set()
        order: list[ExprNode] = []

        def dfs(node: ExprNode) -> None:
            if node.id in visited:
                return
            visited.add(node.id)
            for child in node.children:
                dfs(child)
            order.append(node)

        for root_id in self.roots:
            root = self.nodes.get(root_id)
            if root:
                dfs(root)
        return order

    def __repr__(self) -> str:
        return f"ExprDAG(nodes={len(self.nodes)}, roots={len(self.roots)})"


# ─── Convenience: Build expressions with Expr ───────────────────────────────

class Expr:
    """Convenience namespace for building expression trees."""

    @staticmethod
    def lit(value: Any) -> ExprNode:
        return ExprNode.literal(value)

    @staticmethod
    def sym(name: str) -> ExprNode:
        return ExprNode.symbol(name)

    @staticmethod
    def var(name: str) -> ExprNode:
        return ExprNode.variable(name)

    @staticmethod
    def add(a: ExprNode, b: ExprNode) -> ExprNode:
        return a + b

    @staticmethod
    def mul(a: ExprNode, b: ExprNode) -> ExprNode:
        return a * b

    @staticmethod
    def apply(func: ExprNode, *args: ExprNode) -> ExprNode:
        return ExprNode(kind=ExprKind.APPLY, children=[func, *args], name="apply")

    @staticmethod
    def diff(expr: ExprNode, var: ExprNode, order: int = 1) -> ExprNode:
        return expr.diff(var, order)

    @staticmethod
    def integral(expr: ExprNode, var: ExprNode,
                 lower: Optional[ExprNode] = None,
                 upper: Optional[ExprNode] = None) -> ExprNode:
        return expr.integrate(var, lower, upper)

    @staticmethod
    def matmul(a: ExprNode, b: ExprNode) -> ExprNode:
        return ExprNode(kind=ExprKind.MATMUL, children=[a, b], name="@")

    @staticmethod
    def transpose(a: ExprNode) -> ExprNode:
        return ExprNode(kind=ExprKind.TRANSPOSE, children=[a], name="T")

    @staticmethod
    def trace(a: ExprNode) -> ExprNode:
        return ExprNode(kind=ExprKind.TRACE, children=[a], name="tr")

    @staticmethod
    def det(a: ExprNode) -> ExprNode:
        return ExprNode(kind=ExprKind.DETERMINANT, children=[a], name="det")

    @staticmethod
    def inv(a: ExprNode) -> ExprNode:
        return ExprNode(kind=ExprKind.INVERSE, children=[a], name="inv")

    @staticmethod
    def sum_expr(body: ExprNode, index: ExprNode,
                 lower: ExprNode, upper: ExprNode) -> ExprNode:
        return ExprNode(kind=ExprKind.SUM, children=[body, index, lower, upper], name="Σ")
