"""
Grey Physics IR — Base Types and Type System

Provides the foundational type infrastructure for the Grey Physics IR.
All IR nodes inherit from IRNode which provides:
  - Unique identity (uuid)
  - Metadata (units, symmetries, stability, domain info)
  - Graph-based composition via children/dependencies
  - Canonical form computation for equivalence checking
  - Symbolic and numeric dual representations
"""

from __future__ import annotations

import uuid
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Dict, List, Optional, Tuple, Union, Sequence
import numpy as np


# ---------------------------------------------------------------------------
# Unit system
# ---------------------------------------------------------------------------

class BaseUnit(Enum):
    """SI base dimensions."""
    LENGTH = auto()
    TIME = auto()
    MASS = auto()
    CHARGE = auto()
    TEMPERATURE = auto()
    AMOUNT = auto()
    LUMINOSITY = auto()
    DIMENSIONLESS = auto()


@dataclass(frozen=True)
class Units:
    """Dimensional analysis unit descriptor.

    Stores exponents of base dimensions, e.g. velocity = LENGTH^1 · TIME^-1.
    """
    exponents: Tuple[float, ...] = (0, 0, 0, 0, 0, 0, 0, 0)  # one per BaseUnit

    @staticmethod
    def dimensionless() -> Units:
        return Units()

    @staticmethod
    def length() -> Units:
        e = [0.0] * 8
        e[BaseUnit.LENGTH.value - 1] = 1.0
        return Units(tuple(e))

    @staticmethod
    def time() -> Units:
        e = [0.0] * 8
        e[BaseUnit.TIME.value - 1] = 1.0
        return Units(tuple(e))

    @staticmethod
    def mass() -> Units:
        e = [0.0] * 8
        e[BaseUnit.MASS.value - 1] = 1.0
        return Units(tuple(e))

    @staticmethod
    def velocity() -> Units:
        e = [0.0] * 8
        e[BaseUnit.LENGTH.value - 1] = 1.0
        e[BaseUnit.TIME.value - 1] = -1.0
        return Units(tuple(e))

    @staticmethod
    def acceleration() -> Units:
        e = [0.0] * 8
        e[BaseUnit.LENGTH.value - 1] = 1.0
        e[BaseUnit.TIME.value - 1] = -2.0
        return Units(tuple(e))

    @staticmethod
    def energy() -> Units:
        e = [0.0] * 8
        e[BaseUnit.MASS.value - 1] = 1.0
        e[BaseUnit.LENGTH.value - 1] = 2.0
        e[BaseUnit.TIME.value - 1] = -2.0
        return Units(tuple(e))

    @staticmethod
    def force() -> Units:
        e = [0.0] * 8
        e[BaseUnit.MASS.value - 1] = 1.0
        e[BaseUnit.LENGTH.value - 1] = 1.0
        e[BaseUnit.TIME.value - 1] = -2.0
        return Units(tuple(e))

    def __mul__(self, other: Units) -> Units:
        return Units(tuple(a + b for a, b in zip(self.exponents, other.exponents)))

    def __truediv__(self, other: Units) -> Units:
        return Units(tuple(a - b for a, b in zip(self.exponents, other.exponents)))

    def __pow__(self, n: float) -> Units:
        return Units(tuple(a * n for a in self.exponents))

    def is_compatible(self, other: Units) -> bool:
        return self.exponents == other.exponents


# ---------------------------------------------------------------------------
# Stability / Domain metadata
# ---------------------------------------------------------------------------

class StabilityLevel(Enum):
    """Indicates numerical/analytical stability of an IR node."""
    STABLE = auto()
    CONDITIONALLY_STABLE = auto()
    UNSTABLE = auto()
    UNKNOWN = auto()


class DomainType(Enum):
    """Physical domain classification."""
    CLASSICAL_MECHANICS = auto()
    ELECTROMAGNETISM = auto()
    FLUID_DYNAMICS = auto()
    QUANTUM_MECHANICS = auto()
    RELATIVITY = auto()
    FIELD_THEORY = auto()
    CHAOS = auto()
    PLASMA = auto()
    THERMODYNAMICS = auto()
    EXPERIMENTAL = auto()


@dataclass
class Metadata:
    """Rich metadata attached to every IR node."""
    units: Units = field(default_factory=Units.dimensionless)
    stability: StabilityLevel = StabilityLevel.UNKNOWN
    domain: Optional[DomainType] = None
    symmetries: List[str] = field(default_factory=list)
    tags: Dict[str, Any] = field(default_factory=dict)
    description: str = ""

    def with_units(self, u: Units) -> Metadata:
        return Metadata(u, self.stability, self.domain, self.symmetries,
                        self.tags, self.description)


# ---------------------------------------------------------------------------
# Expression tree
# ---------------------------------------------------------------------------

class ExprKind(Enum):
    """Kind of symbolic expression node."""
    SYMBOL = auto()
    CONSTANT = auto()
    ADD = auto()
    MUL = auto()
    POW = auto()
    DIV = auto()
    NEG = auto()
    FUNC = auto()          # sin, cos, exp, log, …
    DERIVATIVE = auto()    # ∂/∂x
    INTEGRAL = auto()      # ∫ dx
    INDEXED = auto()       # T^{μν}
    DELTA = auto()         # Kronecker / Dirac delta
    COMMUTATOR = auto()    # [A, B]
    TENSOR_PRODUCT = auto()


@dataclass
class Expr:
    """A symbolic expression node forming an expression DAG.

    This is the universal symbolic representation used throughout Grey Physics.
    Expressions are immutable by convention (construct new trees, don't mutate).
    """
    kind: ExprKind
    name: str = ""
    value: Optional[complex] = None
    children: Tuple[Expr, ...] = ()
    indices: Tuple[str, ...] = ()   # for indexed expressions
    metadata: Metadata = field(default_factory=Metadata)

    # ------ constructors ------

    @staticmethod
    def symbol(name: str, **meta_kw: Any) -> Expr:
        return Expr(ExprKind.SYMBOL, name=name, metadata=Metadata(**meta_kw))

    @staticmethod
    def constant(value: complex) -> Expr:
        return Expr(ExprKind.CONSTANT, value=value)

    @staticmethod
    def zero() -> Expr:
        return Expr.constant(0)

    @staticmethod
    def one() -> Expr:
        return Expr.constant(1)

    @staticmethod
    def imaginary_unit() -> Expr:
        return Expr.constant(1j)

    # ------ arithmetic ------

    def __add__(self, other: Expr) -> Expr:
        if not isinstance(other, Expr):
            other = Expr.constant(complex(other))
        return Expr(ExprKind.ADD, children=(self, other))

    def __radd__(self, other: Any) -> Expr:
        return Expr.constant(complex(other)) + self

    def __mul__(self, other: Expr) -> Expr:
        if not isinstance(other, Expr):
            other = Expr.constant(complex(other))
        return Expr(ExprKind.MUL, children=(self, other))

    def __rmul__(self, other: Any) -> Expr:
        return Expr.constant(complex(other)) * self

    def __neg__(self) -> Expr:
        return Expr(ExprKind.NEG, children=(self,))

    def __sub__(self, other: Expr) -> Expr:
        return self + (-other)

    def __truediv__(self, other: Expr) -> Expr:
        if not isinstance(other, Expr):
            other = Expr.constant(complex(other))
        return Expr(ExprKind.DIV, children=(self, other))

    def __pow__(self, other: Expr) -> Expr:
        if not isinstance(other, Expr):
            other = Expr.constant(complex(other))
        return Expr(ExprKind.POW, children=(self, other))

    # ------ calculus ------

    def diff(self, var: Expr) -> Expr:
        """Symbolic derivative ∂self/∂var."""
        return Expr(ExprKind.DERIVATIVE, children=(self, var))

    def integrate(self, var: Expr, lower: Optional[Expr] = None,
                  upper: Optional[Expr] = None) -> Expr:
        """Symbolic integral ∫ self d(var) from lower to upper."""
        bounds = tuple(x for x in (lower, upper) if x is not None)
        return Expr(ExprKind.INTEGRAL, children=(self, var) + bounds)

    # ------ indexing ------

    def indexed(self, *indices: str) -> Expr:
        """Attach tensor indices: T^{mu nu} → T.indexed('mu','nu')."""
        return Expr(ExprKind.INDEXED, children=(self,), indices=indices)

    # ------ utility ------

    def substitute(self, old: Expr, new: Expr) -> Expr:
        """Return a copy with every occurrence of `old` replaced by `new`."""
        if self._structural_eq(old):
            return new
        if not self.children:
            return self
        new_children = tuple(c.substitute(old, new) for c in self.children)
        return Expr(self.kind, self.name, self.value, new_children,
                    self.indices, self.metadata)

    def _structural_eq(self, other: Expr) -> bool:
        if self.kind != other.kind:
            return False
        if self.name != other.name:
            return False
        if self.value != other.value:
            return False
        if len(self.children) != len(other.children):
            return False
        return all(a._structural_eq(b) for a, b in zip(self.children, other.children))

    def free_symbols(self) -> set[str]:
        """Collect all symbol names in this expression tree."""
        syms: set[str] = set()
        if self.kind == ExprKind.SYMBOL:
            syms.add(self.name)
        for c in self.children:
            syms.update(c.free_symbols())
        return syms

    def __repr__(self) -> str:
        if self.kind == ExprKind.SYMBOL:
            idx = "".join(f"^{i}" for i in self.indices)
            return f"{self.name}{idx}"
        if self.kind == ExprKind.CONSTANT:
            if self.value is not None and self.value.imag == 0:
                return str(self.value.real)
            return str(self.value)
        if self.kind == ExprKind.ADD:
            return f"({self.children[0]} + {self.children[1]})"
        if self.kind == ExprKind.MUL:
            return f"({self.children[0]} * {self.children[1]})"
        if self.kind == ExprKind.DIV:
            return f"({self.children[0]} / {self.children[1]})"
        if self.kind == ExprKind.POW:
            return f"({self.children[0]}^{self.children[1]})"
        if self.kind == ExprKind.NEG:
            return f"(-{self.children[0]})"
        if self.kind == ExprKind.DERIVATIVE:
            return f"∂({self.children[0]})/∂({self.children[1]})"
        if self.kind == ExprKind.INTEGRAL:
            return f"∫({self.children[0]})d({self.children[1]})"
        if self.kind == ExprKind.FUNC:
            args = ", ".join(str(c) for c in self.children)
            return f"{self.name}({args})"
        if self.kind == ExprKind.COMMUTATOR:
            return f"[{self.children[0]}, {self.children[1]}]"
        return f"Expr({self.kind.name}, {self.children})"


def func(name: str, *args: Expr) -> Expr:
    """Build a named function expression: sin(x), exp(y), etc."""
    return Expr(ExprKind.FUNC, name=name, children=args)


def commutator(a: Expr, b: Expr) -> Expr:
    """Symbolic commutator [A, B]."""
    return Expr(ExprKind.COMMUTATOR, children=(a, b))


# ---------------------------------------------------------------------------
# IR Node base
# ---------------------------------------------------------------------------

class IRNode(ABC):
    """Base class for all Grey Physics IR nodes.

    Every physical/mathematical object in the IR inherits from this.
    Provides identity, metadata, dependency tracking, and canonical form.
    """

    def __init__(self, name: str = "", metadata: Optional[Metadata] = None):
        self.id: str = str(uuid.uuid4())
        self.name: str = name
        self.metadata: Metadata = metadata or Metadata()
        self._children: List[IRNode] = []

    @property
    def children(self) -> List[IRNode]:
        return list(self._children)

    def add_child(self, child: IRNode) -> None:
        self._children.append(child)

    @abstractmethod
    def canonical_form(self) -> str:
        """Return a canonical string for equivalence comparison."""
        ...

    def is_equivalent(self, other: IRNode) -> bool:
        return self.canonical_form() == other.canonical_form()

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}('{self.name}')"


# ---------------------------------------------------------------------------
# Coordinate & index helpers
# ---------------------------------------------------------------------------

@dataclass(frozen=True)
class Coordinate:
    """A named coordinate variable."""
    name: str
    index: int = 0

    def to_expr(self) -> Expr:
        return Expr.symbol(self.name)


@dataclass(frozen=True)
class IndexSlot:
    """Describes a single tensor index (upper or lower)."""
    name: str
    upper: bool = True  # True = contravariant, False = covariant

    def flip(self) -> IndexSlot:
        return IndexSlot(self.name, not self.upper)

    def __repr__(self) -> str:
        arrow = "^" if self.upper else "_"
        return f"{arrow}{self.name}"


@dataclass(frozen=True)
class TensorRank:
    """(p, q) tensor rank: p contravariant, q covariant indices."""
    contravariant: int = 0
    covariant: int = 0

    @property
    def total(self) -> int:
        return self.contravariant + self.covariant

    def __repr__(self) -> str:
        return f"({self.contravariant},{self.covariant})"


# ---------------------------------------------------------------------------
# Numeric representation helper
# ---------------------------------------------------------------------------

class NumericTensor:
    """Wraps a numpy array with index metadata for numeric computations."""

    def __init__(self, data: np.ndarray, indices: Optional[List[IndexSlot]] = None):
        self.data = data
        self.indices = indices or []

    @property
    def shape(self) -> Tuple[int, ...]:
        return self.data.shape

    @property
    def rank(self) -> TensorRank:
        up = sum(1 for i in self.indices if i.upper)
        down = len(self.indices) - up
        return TensorRank(up, down)

    def contract(self, idx1: int, idx2: int) -> NumericTensor:
        """Contract indices idx1 and idx2 (trace‑like operation)."""
        result = np.trace(self.data, axis1=idx1, axis2=idx2)
        new_indices = [i for k, i in enumerate(self.indices)
                       if k not in (idx1, idx2)]
        return NumericTensor(result, new_indices)

    def __repr__(self) -> str:
        idx_str = "".join(str(i) for i in self.indices)
        return f"NumericTensor{idx_str}{list(self.data.shape)}"
