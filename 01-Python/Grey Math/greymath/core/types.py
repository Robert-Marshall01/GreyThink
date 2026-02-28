"""
Core type system for the Grey Math IR.

Provides strongly-typed mathematical objects with algebraic type-class behavior,
canonical forms for symbolic equivalence, and metadata for precision/stability.

Type Hierarchy:
    MathType (abstract base)
    ├── Scalar          — real, complex, integer, rational
    ├── Vector          — elements of vector spaces
    ├── Matrix          — linear maps between finite-dim spaces
    ├── Tensor          — multi-linear maps
    ├── MathFunction    — maps between mathematical spaces
    ├── Functional      — maps from function spaces to scalars
    ├── LinearOperator  — bounded/unbounded operators
    ├── Measure         — probability measures and distributions
    ├── ManifoldPoint   — points on manifolds
    ├── AlgebraicStructure — groups, rings, fields, modules
    ├── CategoryObject  — objects in a category
    ├── DiffOperator    — differential operators
    ├── OptProblem      — optimization problems
    ├── StochasticProcess — stochastic processes
    ├── SpectralObject  — eigenvalues, eigenvectors, resolvents
"""

from __future__ import annotations

import uuid
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Generic, Optional, Protocol, Sequence, TypeVar, Union

import numpy as np

from greymath.core.metadata import Metadata, Precision


# ─── Enumerations ────────────────────────────────────────────────────────────

class ScalarField(Enum):
    """Number fields for scalar types."""
    REAL = auto()
    COMPLEX = auto()
    INTEGER = auto()
    RATIONAL = auto()
    BOOLEAN = auto()


class NormType(Enum):
    """Types of norms."""
    L1 = auto()
    L2 = auto()
    LINF = auto()
    FROBENIUS = auto()
    SPECTRAL = auto()
    NUCLEAR = auto()
    CUSTOM = auto()


class OperatorKind(Enum):
    """Classification of operators."""
    LINEAR = auto()
    NONLINEAR = auto()
    BOUNDED = auto()
    UNBOUNDED = auto()
    COMPACT = auto()
    SELF_ADJOINT = auto()
    UNITARY = auto()
    NORMAL = auto()
    POSITIVE = auto()
    PROJECTION = auto()


class StructureKind(Enum):
    """Kinds of algebraic structures."""
    GROUP = auto()
    ABELIAN_GROUP = auto()
    RING = auto()
    COMMUTATIVE_RING = auto()
    FIELD = auto()
    MODULE = auto()
    VECTOR_SPACE = auto()
    ALGEBRA = auto()
    LIE_ALGEBRA = auto()
    LIE_GROUP = auto()


class OptSense(Enum):
    """Optimization direction."""
    MINIMIZE = auto()
    MAXIMIZE = auto()


class DiffOpKind(Enum):
    """Kinds of differential operators."""
    GRADIENT = auto()
    DIVERGENCE = auto()
    CURL = auto()
    LAPLACIAN = auto()
    HESSIAN = auto()
    JACOBIAN = auto()
    DIRECTIONAL = auto()
    COVARIANT = auto()
    LIE = auto()
    EXTERIOR = auto()
    VARIATIONAL = auto()


# ─── Protocols (Type-Classes) ────────────────────────────────────────────────

class HasNorm(Protocol):
    """Protocol for objects that have a norm."""
    def norm(self, norm_type: NormType = NormType.L2) -> "Scalar": ...


class HasInnerProduct(Protocol):
    """Protocol for objects in inner product spaces."""
    def inner(self, other: Any) -> "Scalar": ...


class Composable(Protocol):
    """Protocol for composable objects (operators, morphisms)."""
    def compose(self, other: Any) -> Any: ...


class HasAdjoint(Protocol):
    """Protocol for objects with adjoints."""
    def adjoint(self) -> Any: ...


class Differentiable(Protocol):
    """Protocol for differentiable objects."""
    def diff(self, var: Any, order: int = 1) -> Any: ...


# ─── Base Type ───────────────────────────────────────────────────────────────

T = TypeVar("T")


@dataclass
class MathType(ABC):
    """
    Abstract base for all mathematical IR types.

    Every mathematical object in Grey Math is a MathType with:
    - A unique id for graph-based composition
    - Optional metadata (precision, stability, domain assumptions)
    - Canonical form support for symbolic equivalence
    """
    id: str = field(default_factory=lambda: str(uuid.uuid4())[:12])
    metadata: Metadata = field(default_factory=Metadata)

    @abstractmethod
    def type_name(self) -> str:
        """Return the human-readable type name."""
        ...

    @abstractmethod
    def canonical(self) -> "MathType":
        """Return the canonical form for equivalence checking."""
        ...

    def with_metadata(self, **kwargs: Any) -> "MathType":
        """Return a copy with updated metadata."""
        import copy
        obj = copy.deepcopy(self)
        for k, v in kwargs.items():
            setattr(obj.metadata, k, v)
        return obj

    def __repr__(self) -> str:
        return f"{self.type_name()}(id={self.id})"


# ─── Scalar ──────────────────────────────────────────────────────────────────

@dataclass
class Scalar(MathType):
    """
    A scalar value in a specified number field.

    Supports real, complex, integer, rational, and boolean values.
    Tracks precision and can operate in arbitrary-precision mode.
    """
    value: Union[int, float, complex, None] = None
    symbol: Optional[str] = None  # symbolic name, e.g., "alpha"
    field_type: ScalarField = ScalarField.REAL
    is_symbolic: bool = False

    def type_name(self) -> str:
        return f"Scalar[{self.field_type.name}]"

    def canonical(self) -> "Scalar":
        if self.is_symbolic:
            return self
        if self.field_type == ScalarField.REAL and isinstance(self.value, float):
            # Canonical: remove trailing zeros
            return Scalar(value=self.value, field_type=self.field_type)
        return self

    @staticmethod
    def real(value: float, symbol: Optional[str] = None) -> "Scalar":
        return Scalar(value=value, symbol=symbol, field_type=ScalarField.REAL)

    @staticmethod
    def complex_val(re: float, im: float) -> "Scalar":
        return Scalar(value=complex(re, im), field_type=ScalarField.COMPLEX)

    @staticmethod
    def integer(value: int) -> "Scalar":
        return Scalar(value=value, field_type=ScalarField.INTEGER)

    @staticmethod
    def symbolic(name: str, field_type: ScalarField = ScalarField.REAL) -> "Scalar":
        return Scalar(symbol=name, field_type=field_type, is_symbolic=True)

    def __repr__(self) -> str:
        if self.is_symbolic:
            return f"Scalar({self.symbol})"
        return f"Scalar({self.value})"


# ─── Vector ──────────────────────────────────────────────────────────────────

@dataclass
class Vector(MathType):
    """
    An element of a vector space.

    Can be concrete (numpy array) or symbolic (list of expressions).
    Tracks dimension, basis, and inner product structure.
    """
    components: Optional[np.ndarray] = None
    symbolic_components: Optional[list[Any]] = None
    dim: Optional[int] = None
    basis_label: str = "standard"
    is_symbolic: bool = False

    def __post_init__(self) -> None:
        if self.components is not None and self.dim is None:
            self.dim = len(self.components)
        if self.symbolic_components is not None and self.dim is None:
            self.dim = len(self.symbolic_components)

    def type_name(self) -> str:
        return f"Vector[R^{self.dim}]"

    def canonical(self) -> "Vector":
        return self

    def norm(self, norm_type: NormType = NormType.L2) -> Scalar:
        if self.components is None:
            raise ValueError("Cannot compute norm of symbolic vector without evaluation")
        if norm_type == NormType.L1:
            return Scalar.real(float(np.sum(np.abs(self.components))))
        elif norm_type == NormType.L2:
            return Scalar.real(float(np.linalg.norm(self.components)))
        elif norm_type == NormType.LINF:
            return Scalar.real(float(np.max(np.abs(self.components))))
        raise ValueError(f"Unsupported norm type for vectors: {norm_type}")

    def inner(self, other: "Vector") -> Scalar:
        if self.components is None or other.components is None:
            raise ValueError("Cannot compute inner product of symbolic vectors")
        return Scalar.real(float(np.dot(self.components, other.components)))

    def __repr__(self) -> str:
        if self.is_symbolic:
            return f"Vector(symbolic, dim={self.dim})"
        return f"Vector(dim={self.dim})"


# ─── Matrix ──────────────────────────────────────────────────────────────────

@dataclass
class Matrix(MathType):
    """
    A matrix representing a linear map between finite-dimensional spaces.

    Supports dense and sparse representations, symbolic entries,
    and tracks structural properties (symmetry, positive definiteness, etc.).
    """
    data: Optional[np.ndarray] = None
    symbolic_entries: Optional[list[list[Any]]] = None
    rows: Optional[int] = None
    cols: Optional[int] = None
    is_symbolic: bool = False
    properties: set[str] = field(default_factory=set)  # "symmetric", "positive_definite", etc.

    def __post_init__(self) -> None:
        if self.data is not None:
            self.rows, self.cols = self.data.shape

    def type_name(self) -> str:
        return f"Matrix[{self.rows}x{self.cols}]"

    def canonical(self) -> "Matrix":
        return self

    def is_square(self) -> bool:
        return self.rows == self.cols

    def norm(self, norm_type: NormType = NormType.FROBENIUS) -> Scalar:
        if self.data is None:
            raise ValueError("Cannot compute norm of symbolic matrix")
        if norm_type == NormType.FROBENIUS:
            return Scalar.real(float(np.linalg.norm(self.data, "fro")))
        elif norm_type == NormType.SPECTRAL:
            return Scalar.real(float(np.linalg.norm(self.data, 2)))
        elif norm_type == NormType.NUCLEAR:
            return Scalar.real(float(np.sum(np.linalg.svd(self.data, compute_uv=False))))
        raise ValueError(f"Unsupported norm type for matrices: {norm_type}")

    def adjoint(self) -> "Matrix":
        if self.data is not None:
            return Matrix(data=self.data.conj().T, properties=set(self.properties))
        raise ValueError("Cannot compute adjoint of symbolic matrix without evaluation")

    def trace(self) -> Scalar:
        if self.data is not None and self.is_square():
            return Scalar.real(float(np.trace(self.data)))
        raise ValueError("Trace requires a square numeric matrix")

    def __repr__(self) -> str:
        return f"Matrix({self.rows}x{self.cols}, props={self.properties})"


# ─── Tensor ──────────────────────────────────────────────────────────────────

@dataclass
class Tensor(MathType):
    """
    A multi-linear map / multi-dimensional array.

    Represents tensors of arbitrary rank with covariant/contravariant indices.
    """
    data: Optional[np.ndarray] = None
    shape: Optional[tuple[int, ...]] = None
    rank: Optional[tuple[int, int]] = None  # (contravariant, covariant)
    index_labels: Optional[list[str]] = None
    is_symbolic: bool = False

    def __post_init__(self) -> None:
        if self.data is not None and self.shape is None:
            self.shape = self.data.shape

    def type_name(self) -> str:
        if self.rank:
            return f"Tensor[({self.rank[0]},{self.rank[1]}), shape={self.shape}]"
        return f"Tensor[shape={self.shape}]"

    def canonical(self) -> "Tensor":
        return self

    def contract(self, idx1: int, idx2: int) -> "Tensor":
        """Contract two indices (trace over those dimensions)."""
        if self.data is None:
            raise ValueError("Cannot contract symbolic tensor without evaluation")
        result = np.trace(self.data, axis1=idx1, axis2=idx2)
        return Tensor(data=result)

    def __repr__(self) -> str:
        return f"Tensor(shape={self.shape}, rank={self.rank})"


# ─── MathFunction ────────────────────────────────────────────────────────────

@dataclass
class MathFunction(MathType):
    """
    A mathematical function f: X → Y.

    Can be concrete (callable), symbolic (expression tree), or defined
    by composition of other functions.
    """
    name: str = "f"
    domain: Optional[str] = None  # symbolic domain description
    codomain: Optional[str] = None
    callable_impl: Optional[Any] = None  # actual Python callable
    expression: Optional[Any] = None  # symbolic expression (ExprNode)
    is_symbolic: bool = True

    def type_name(self) -> str:
        d = self.domain or "?"
        c = self.codomain or "?"
        return f"Function[{d} → {c}]"

    def canonical(self) -> "MathFunction":
        return self

    def __call__(self, *args: Any) -> Any:
        if self.callable_impl is not None:
            return self.callable_impl(*args)
        raise ValueError(f"Function '{self.name}' has no callable implementation")

    def compose(self, other: "MathFunction") -> "MathFunction":
        """Return f ∘ g."""
        def composed(*args: Any) -> Any:
            return self(other(*args))
        return MathFunction(
            name=f"({self.name} ∘ {other.name})",
            domain=other.domain,
            codomain=self.codomain,
            callable_impl=composed if (self.callable_impl and other.callable_impl) else None,
        )

    def __repr__(self) -> str:
        return f"Function({self.name}: {self.domain} → {self.codomain})"


# ─── Functional ──────────────────────────────────────────────────────────────

@dataclass
class Functional(MathType):
    """
    A functional: a map from a function space to scalars.

    F: C(X) → R, e.g., integration functionals, norms, entropy.
    """
    name: str = "F"
    function_space: Optional[str] = None
    callable_impl: Optional[Any] = None
    expression: Optional[Any] = None
    is_linear: bool = False

    def type_name(self) -> str:
        return f"Functional[{self.function_space or '?'} → R]"

    def canonical(self) -> "Functional":
        return self

    def __call__(self, func: MathFunction) -> Scalar:
        if self.callable_impl is not None:
            result = self.callable_impl(func)
            if isinstance(result, Scalar):
                return result
            return Scalar.real(float(result))
        raise ValueError(f"Functional '{self.name}' has no callable implementation")

    def __repr__(self) -> str:
        return f"Functional({self.name})"


# ─── LinearOperator ─────────────────────────────────────────────────────────

@dataclass
class LinearOperator(MathType):
    """
    A linear operator A: X → Y between (possibly infinite-dimensional) spaces.

    Supports bounded/unbounded, compact, self-adjoint, unitary operators.
    Can be represented as a matrix for finite-dim spaces.
    """
    name: str = "A"
    domain_space: Optional[str] = None
    codomain_space: Optional[str] = None
    matrix_rep: Optional[Matrix] = None
    kind: set[OperatorKind] = field(default_factory=lambda: {OperatorKind.LINEAR})
    callable_impl: Optional[Any] = None

    def type_name(self) -> str:
        kinds = ",".join(k.name for k in self.kind)
        return f"Operator[{kinds}]({self.domain_space} → {self.codomain_space})"

    def canonical(self) -> "LinearOperator":
        return self

    def apply(self, x: Union[Vector, MathFunction]) -> Union[Vector, MathFunction]:
        if self.callable_impl is not None:
            return self.callable_impl(x)
        if self.matrix_rep is not None and isinstance(x, Vector) and x.components is not None:
            result = self.matrix_rep.data @ x.components
            return Vector(components=result)
        raise ValueError(f"Cannot apply operator '{self.name}'")

    def compose(self, other: "LinearOperator") -> "LinearOperator":
        """Return A ∘ B."""
        def composed(x: Any) -> Any:
            return self.apply(other.apply(x))
        return LinearOperator(
            name=f"({self.name} ∘ {other.name})",
            domain_space=other.domain_space,
            codomain_space=self.codomain_space,
            callable_impl=composed,
        )

    def adjoint(self) -> "LinearOperator":
        if self.matrix_rep is not None:
            adj_mat = self.matrix_rep.adjoint()
            return LinearOperator(
                name=f"{self.name}*",
                domain_space=self.codomain_space,
                codomain_space=self.domain_space,
                matrix_rep=adj_mat,
            )
        raise ValueError("Adjoint requires matrix representation")

    def commutator(self, other: "LinearOperator") -> "LinearOperator":
        """Compute [A, B] = AB - BA."""
        ab = self.compose(other)
        ba = other.compose(self)
        return LinearOperator(
            name=f"[{self.name},{other.name}]",
            domain_space=self.domain_space,
            codomain_space=self.codomain_space,
            callable_impl=lambda x: Vector(
                components=ab.apply(x).components - ba.apply(x).components
            ) if hasattr(ab.apply(x), 'components') else None,
        )

    def __repr__(self) -> str:
        return f"Operator({self.name})"


# ─── Measure ─────────────────────────────────────────────────────────────────

@dataclass
class Measure(MathType):
    """
    A measure on a measurable space, including probability distributions.

    Supports σ-finite measures, probability measures, and signed measures.
    """
    name: str = "μ"
    space: Optional[str] = None
    is_probability: bool = False
    is_finite: bool = True
    density: Optional[MathFunction] = None  # density w.r.t. Lebesgue
    parameters: dict[str, Any] = field(default_factory=dict)
    distribution_type: Optional[str] = None  # "normal", "uniform", etc.

    def type_name(self) -> str:
        if self.is_probability:
            return f"ProbMeasure[{self.space or '?'}]"
        return f"Measure[{self.space or '?'}]"

    def canonical(self) -> "Measure":
        return self

    @staticmethod
    def normal(mean: float = 0.0, std: float = 1.0) -> "Measure":
        return Measure(
            name="N",
            space="R",
            is_probability=True,
            distribution_type="normal",
            parameters={"mean": mean, "std": std},
        )

    @staticmethod
    def uniform(a: float = 0.0, b: float = 1.0) -> "Measure":
        return Measure(
            name="U",
            space="R",
            is_probability=True,
            distribution_type="uniform",
            parameters={"a": a, "b": b},
        )

    def __repr__(self) -> str:
        if self.distribution_type:
            return f"Measure({self.distribution_type}, {self.parameters})"
        return f"Measure({self.name})"


# ─── ManifoldPoint ───────────────────────────────────────────────────────────

@dataclass
class ManifoldPoint(MathType):
    """
    A point on a differentiable manifold, with chart/coordinate info.
    """
    manifold_name: str = "M"
    coordinates: Optional[np.ndarray] = None
    chart_name: str = "default"
    dim: Optional[int] = None

    def __post_init__(self) -> None:
        if self.coordinates is not None and self.dim is None:
            self.dim = len(self.coordinates)

    def type_name(self) -> str:
        return f"ManifoldPoint[{self.manifold_name}, dim={self.dim}]"

    def canonical(self) -> "ManifoldPoint":
        return self

    def __repr__(self) -> str:
        return f"ManifoldPoint({self.manifold_name}, chart={self.chart_name})"


# ─── AlgebraicStructure ─────────────────────────────────────────────────────

@dataclass
class AlgebraicStructure(MathType):
    """
    An algebraic structure: groups, rings, fields, modules, algebras.

    Tracks the kind, underlying set description, and operations.
    """
    name: str = ""
    kind: StructureKind = StructureKind.GROUP
    elements_desc: Optional[str] = None
    operations: dict[str, Any] = field(default_factory=dict)
    properties: set[str] = field(default_factory=set)

    def type_name(self) -> str:
        return f"{self.kind.name}({self.name})"

    def canonical(self) -> "AlgebraicStructure":
        return self

    def __repr__(self) -> str:
        return f"AlgebraicStructure({self.kind.name}: {self.name})"


# ─── CategoryObject ─────────────────────────────────────────────────────────

@dataclass
class CategoryObject(MathType):
    """
    An object in a category, part of the category-theoretic IR.
    """
    name: str = ""
    category_name: str = ""

    def type_name(self) -> str:
        return f"CatObj[{self.category_name}]({self.name})"

    def canonical(self) -> "CategoryObject":
        return self

    def __repr__(self) -> str:
        return f"CatObj({self.name} in {self.category_name})"


@dataclass
class Morphism(MathType):
    """
    A morphism between objects in a category.
    """
    name: str = ""
    source: Optional[CategoryObject] = None
    target: Optional[CategoryObject] = None
    category_name: str = ""

    def type_name(self) -> str:
        s = self.source.name if self.source else "?"
        t = self.target.name if self.target else "?"
        return f"Morphism[{s} → {t}]"

    def canonical(self) -> "Morphism":
        return self

    def compose(self, other: "Morphism") -> "Morphism":
        """Compose morphisms: self ∘ other."""
        if self.source and other.target and self.source.name != other.target.name:
            raise ValueError("Morphism composition: source/target mismatch")
        return Morphism(
            name=f"({self.name} ∘ {other.name})",
            source=other.source,
            target=self.target,
            category_name=self.category_name,
        )

    def __repr__(self) -> str:
        s = self.source.name if self.source else "?"
        t = self.target.name if self.target else "?"
        return f"Morphism({self.name}: {s} → {t})"


@dataclass
class Functor(MathType):
    """
    A functor between categories.
    """
    name: str = ""
    source_category: str = ""
    target_category: str = ""
    object_map: Optional[Any] = None
    morphism_map: Optional[Any] = None

    def type_name(self) -> str:
        return f"Functor[{self.source_category} → {self.target_category}]"

    def canonical(self) -> "Functor":
        return self

    def __repr__(self) -> str:
        return f"Functor({self.name}: {self.source_category} → {self.target_category})"


@dataclass
class NaturalTransformation(MathType):
    """
    A natural transformation between functors.
    """
    name: str = ""
    source_functor: Optional[Functor] = None
    target_functor: Optional[Functor] = None

    def type_name(self) -> str:
        sf = self.source_functor.name if self.source_functor else "?"
        tf = self.target_functor.name if self.target_functor else "?"
        return f"NatTrans[{sf} ⇒ {tf}]"

    def canonical(self) -> "NaturalTransformation":
        return self

    def __repr__(self) -> str:
        sf = self.source_functor.name if self.source_functor else "?"
        tf = self.target_functor.name if self.target_functor else "?"
        return f"NatTrans({self.name}: {sf} ⇒ {tf})"


# ─── DiffOperator ───────────────────────────────────────────────────────────

@dataclass
class DiffOperator(MathType):
    """
    A differential operator (gradient, Hessian, Laplacian, etc.).
    """
    kind: DiffOpKind = DiffOpKind.GRADIENT
    variable: Optional[str] = None
    order: int = 1
    callable_impl: Optional[Any] = None

    def type_name(self) -> str:
        return f"DiffOp[{self.kind.name}, order={self.order}]"

    def canonical(self) -> "DiffOperator":
        return self

    def apply(self, target: MathType) -> MathType:
        if self.callable_impl is not None:
            return self.callable_impl(target)
        raise ValueError(f"DiffOperator {self.kind.name} has no callable implementation")

    def __repr__(self) -> str:
        return f"DiffOp({self.kind.name}, var={self.variable}, order={self.order})"


# ─── OptProblem ──────────────────────────────────────────────────────────────

@dataclass
class OptProblem(MathType):
    """
    An optimization problem: min/max f(x) subject to constraints.
    """
    name: str = "P"
    sense: OptSense = OptSense.MINIMIZE
    objective: Optional[MathFunction] = None
    variables: list[str] = field(default_factory=list)
    equality_constraints: list[MathFunction] = field(default_factory=list)
    inequality_constraints: list[MathFunction] = field(default_factory=list)
    domain_constraints: dict[str, tuple[Optional[float], Optional[float]]] = field(
        default_factory=dict
    )
    is_convex: Optional[bool] = None

    def type_name(self) -> str:
        sense = "min" if self.sense == OptSense.MINIMIZE else "max"
        return f"OptProblem[{sense}, vars={len(self.variables)}]"

    def canonical(self) -> "OptProblem":
        return self

    def __repr__(self) -> str:
        sense = "min" if self.sense == OptSense.MINIMIZE else "max"
        return f"OptProblem({sense} {self.name})"


# ─── StochasticProcess ───────────────────────────────────────────────────────

@dataclass
class StochasticProcess(MathType):
    """
    A stochastic process {X_t : t ∈ T}.
    """
    name: str = "X"
    index_set: str = "R+"  # time parameter space
    state_space: str = "R"
    process_type: Optional[str] = None  # "brownian", "poisson", "markov", etc.
    parameters: dict[str, Any] = field(default_factory=dict)

    def type_name(self) -> str:
        return f"StochasticProcess[{self.process_type or 'generic'}]"

    def canonical(self) -> "StochasticProcess":
        return self

    @staticmethod
    def brownian(dim: int = 1) -> "StochasticProcess":
        return StochasticProcess(
            name="W", process_type="brownian_motion",
            parameters={"dim": dim},
        )

    @staticmethod
    def poisson(rate: float = 1.0) -> "StochasticProcess":
        return StochasticProcess(
            name="N", process_type="poisson",
            parameters={"rate": rate},
        )

    def __repr__(self) -> str:
        return f"StochProc({self.name}, type={self.process_type})"


# ─── SpectralObject ──────────────────────────────────────────────────────────

@dataclass
class SpectralObject(MathType):
    """
    Spectral data: eigenvalues, eigenvectors, spectral decomposition, resolvents.
    """
    operator: Optional[LinearOperator] = None
    eigenvalues: Optional[np.ndarray] = None
    eigenvectors: Optional[np.ndarray] = None
    spectral_radius: Optional[float] = None
    condition_number: Optional[float] = None

    def type_name(self) -> str:
        n = len(self.eigenvalues) if self.eigenvalues is not None else "?"
        return f"Spectral[n_eig={n}]"

    def canonical(self) -> "SpectralObject":
        return self

    def resolvent_at(self, z: complex) -> Matrix:
        """Compute resolvent (A - zI)^{-1} at point z."""
        if self.operator is None or self.operator.matrix_rep is None:
            raise ValueError("Resolvent requires operator with matrix representation")
        A = self.operator.matrix_rep.data
        n = A.shape[0]
        R = np.linalg.inv(A - z * np.eye(n))
        return Matrix(data=R)

    def __repr__(self) -> str:
        n = len(self.eigenvalues) if self.eigenvalues is not None else 0
        return f"Spectral(n_eigenvalues={n}, radius={self.spectral_radius})"
