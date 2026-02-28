"""
Experimental IR Extensions.

Extends the Grey Math core IR with new types and operators for
bleeding-edge mathematical structures. Each type supports:
- Symbolic representation
- Numeric execution
- Stability metadata
- Domain constraints
- Compositionality

New IR types:
    OperatorSpace         — spaces of bounded/unbounded operators
    ExperimentalManifold  — manifolds with charts, atlas, fiber bundles
    Chart, Atlas          — coordinate charts and atlases
    Connection            — connections on fiber bundles
    FiberBundle           — fiber bundle structures
    ProbabilityMeasure    — probability measures on abstract spaces
    ExperimentalSigmaAlgebra — σ-algebras for measure theory
    SDEProcess            — stochastic differential equation processes
    Flow                  — flows on manifolds/state spaces
    VectorFieldIR         — tangent vector fields
    ExperimentalDynamicalSystem — full dynamical system IR type
    MonoidalStructure     — monoidal category structures
    PDEOperator           — PDE operators (heat, wave, HJ, etc.)
    GeometricFlow         — geometric flows (Ricci, mean curvature, etc.)
"""

from __future__ import annotations

import uuid
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Callable, Optional, Union

import numpy as np
from numpy.typing import NDArray

from greymath.core.types import (
    MathType, Scalar, Vector, Matrix, Tensor,
    LinearOperator, Measure, ManifoldPoint,
    CategoryObject, Morphism, Functor,
    OperatorKind, MathFunction, SpectralObject,
)
from greymath.core.metadata import (
    Metadata, Precision, PrecisionMode,
    StabilityInfo, StabilityRating,
)


# ─── Enumerations ────────────────────────────────────────────────────────────

class OperatorSpaceKind(Enum):
    """Classification of operator spaces."""
    BOUNDED = auto()
    UNBOUNDED = auto()
    COMPACT = auto()
    TRACE_CLASS = auto()
    HILBERT_SCHMIDT = auto()
    SCHATTEN_P = auto()


class PDEKind(Enum):
    """Kinds of PDE operators."""
    HEAT = auto()
    WAVE = auto()
    HAMILTON_JACOBI = auto()
    SCHRODINGER = auto()
    NAVIER_STOKES = auto()
    LAPLACE = auto()
    POISSON = auto()
    DIFFUSION = auto()
    ADVECTION = auto()
    REACTION_DIFFUSION = auto()
    VARIATIONAL = auto()
    CUSTOM = auto()


class GeometricFlowKind(Enum):
    """Kinds of geometric flows."""
    RICCI = auto()
    MEAN_CURVATURE = auto()
    YAMABE = auto()
    WILLMORE = auto()
    INVERSE_MEAN_CURVATURE = auto()
    CURVE_SHORTENING = auto()
    CALABI = auto()
    KAHLER_RICCI = auto()
    CUSTOM = auto()


class SDEKind(Enum):
    """Kinds of stochastic differential equations."""
    ITO = auto()
    STRATONOVICH = auto()
    BACKWARD = auto()


class MonoidalKind(Enum):
    """Kinds of monoidal structures."""
    STRICT = auto()
    SYMMETRIC = auto()
    BRAIDED = auto()
    CARTESIAN = auto()
    COCARTESIAN = auto()
    CLOSED = auto()
    COMPACT_CLOSED = auto()
    DAGGER = auto()


# ─── OperatorSpace ───────────────────────────────────────────────────────────

@dataclass
class OperatorSpace(MathType):
    """
    A space of operators B(H) or a subspace thereof.

    Represents bounded operators B(H), unbounded operators on a dense domain,
    compact operators K(H), trace-class operators, Hilbert-Schmidt operators,
    and Schatten p-class operators.
    """
    name: str = "B(H)"
    kind: OperatorSpaceKind = OperatorSpaceKind.BOUNDED
    hilbert_space_dim: Optional[int] = None
    schatten_p: Optional[float] = None  # for Schatten p-class
    operators: list[LinearOperator] = field(default_factory=list)
    is_algebra: bool = True  # True if closed under composition

    def type_name(self) -> str:
        return f"OperatorSpace[{self.kind.name}]"

    def canonical(self) -> "OperatorSpace":
        return self

    def add_operator(self, op: LinearOperator) -> None:
        """Register an operator in this space."""
        self.operators.append(op)

    def norm(self, op: LinearOperator) -> float:
        """Compute the operator norm appropriate to this space."""
        if op.matrix_rep is None or op.matrix_rep.data is None:
            raise ValueError("Requires matrix representation")
        A = op.matrix_rep.data
        if self.kind == OperatorSpaceKind.TRACE_CLASS:
            return float(np.sum(np.linalg.svd(A, compute_uv=False)))
        elif self.kind == OperatorSpaceKind.HILBERT_SCHMIDT:
            return float(np.linalg.norm(A, "fro"))
        elif self.kind == OperatorSpaceKind.SCHATTEN_P and self.schatten_p is not None:
            svs = np.linalg.svd(A, compute_uv=False)
            return float(np.sum(svs ** self.schatten_p) ** (1.0 / self.schatten_p))
        return float(np.linalg.norm(A, 2))

    def __repr__(self) -> str:
        return f"OperatorSpace({self.name}, {self.kind.name})"


# ─── Chart and Atlas ─────────────────────────────────────────────────────────

@dataclass
class Chart(MathType):
    """
    A coordinate chart (U, φ) on a manifold.

    Maps an open subset U ⊂ M to R^n via a homeomorphism φ.
    """
    name: str = "φ"
    domain_name: str = "U"
    dim: int = 0
    coord_map: Optional[Callable[[NDArray], NDArray]] = None
    inverse_map: Optional[Callable[[NDArray], NDArray]] = None
    jacobian: Optional[Callable[[NDArray], NDArray]] = None

    def type_name(self) -> str:
        return f"Chart[{self.name}: {self.domain_name} → R^{self.dim}]"

    def canonical(self) -> "Chart":
        return self

    def apply(self, point: NDArray) -> NDArray:
        """Map a point from manifold to coordinates."""
        if self.coord_map is None:
            raise ValueError("Chart has no coordinate map")
        return self.coord_map(point)

    def inverse(self, coords: NDArray) -> NDArray:
        """Map from coordinates back to manifold."""
        if self.inverse_map is None:
            raise ValueError("Chart has no inverse map")
        return self.inverse_map(coords)

    def transition_map(self, other: "Chart", point: NDArray) -> NDArray:
        """Compute transition map φ_other ∘ φ_self^{-1}."""
        manifold_point = self.inverse(point)
        return other.apply(manifold_point)

    def __repr__(self) -> str:
        return f"Chart({self.name}: {self.domain_name} → R^{self.dim})"


@dataclass
class Atlas(MathType):
    """An atlas: a collection of compatible charts covering a manifold."""
    name: str = "A"
    manifold_name: str = "M"
    charts: list[Chart] = field(default_factory=list)
    transition_maps: dict[tuple[str, str], Callable] = field(default_factory=dict)

    def type_name(self) -> str:
        return f"Atlas[{self.manifold_name}, {len(self.charts)} charts]"

    def canonical(self) -> "Atlas":
        return self

    def add_chart(self, chart: Chart) -> None:
        self.charts.append(chart)

    def __repr__(self) -> str:
        return f"Atlas({self.manifold_name}, {len(self.charts)} charts)"


# ─── Connection ──────────────────────────────────────────────────────────────

@dataclass
class Connection(MathType):
    """
    A connection on a vector/fiber bundle, enabling parallel transport
    and covariant differentiation.
    """
    name: str = "∇"
    manifold_name: str = "M"
    christoffel_fn: Optional[Callable[[NDArray], NDArray]] = None
    is_metric_compatible: bool = False
    is_torsion_free: bool = False  # Levi-Civita if both True

    def type_name(self) -> str:
        lc = " (Levi-Civita)" if (self.is_metric_compatible and self.is_torsion_free) else ""
        return f"Connection[{self.name} on {self.manifold_name}{lc}]"

    def canonical(self) -> "Connection":
        return self

    def christoffel(self, point: NDArray) -> NDArray:
        """Evaluate Christoffel symbols Γ^k_{ij} at a point."""
        if self.christoffel_fn is None:
            raise ValueError("Connection has no Christoffel symbol function")
        return self.christoffel_fn(point)

    def covariant_derivative(
        self, vector_field: Callable[[NDArray], NDArray],
        direction: NDArray, point: NDArray, eps: float = 1e-7
    ) -> NDArray:
        """
        Compute covariant derivative ∇_direction V at a point.

        ∇_X V = dV(X) + Γ(V, X) in coordinates.
        """
        n = len(point)
        V = vector_field(point)
        gamma = self.christoffel(point)

        # Directional derivative of V
        dV = np.zeros(n)
        for i in range(n):
            p_plus = point.copy()
            p_minus = point.copy()
            p_plus[i] += eps
            p_minus[i] -= eps
            dV_di = (vector_field(p_plus) - vector_field(p_minus)) / (2 * eps)
            dV += direction[i] * dV_di

        # Connection term: Γ^k_{ij} X^i V^j
        conn_term = np.zeros(n)
        for k in range(n):
            for i in range(n):
                for j in range(n):
                    conn_term[k] += gamma[k, i, j] * direction[i] * V[j]

        return dV + conn_term

    def __repr__(self) -> str:
        return f"Connection({self.name} on {self.manifold_name})"


# ─── FiberBundle ─────────────────────────────────────────────────────────────

@dataclass
class FiberBundle(MathType):
    """
    A fiber bundle (E, M, π, F):
    - E: total space
    - M: base manifold
    - π: projection E → M
    - F: typical fiber
    """
    name: str = "E"
    base_manifold: str = "M"
    fiber_type: str = "R^k"
    total_dim: Optional[int] = None
    base_dim: Optional[int] = None
    fiber_dim: Optional[int] = None
    projection: Optional[Callable[[NDArray], NDArray]] = None
    local_trivialization: Optional[Callable[[NDArray], tuple[NDArray, NDArray]]] = None
    connection: Optional[Connection] = None

    def type_name(self) -> str:
        return f"FiberBundle[{self.name}: {self.fiber_type} → E → {self.base_manifold}]"

    def canonical(self) -> "FiberBundle":
        return self

    def project(self, total_point: NDArray) -> NDArray:
        """Project from total space to base."""
        if self.projection is not None:
            return self.projection(total_point)
        if self.base_dim is not None:
            return total_point[:self.base_dim]
        raise ValueError("No projection defined")

    def trivialize(self, total_point: NDArray) -> tuple[NDArray, NDArray]:
        """Local trivialization: E → U × F."""
        if self.local_trivialization is not None:
            return self.local_trivialization(total_point)
        if self.base_dim is not None and self.fiber_dim is not None:
            return total_point[:self.base_dim], total_point[self.base_dim:]
        raise ValueError("No trivialization defined")

    def __repr__(self) -> str:
        return f"FiberBundle({self.fiber_type} → {self.name} → {self.base_manifold})"


# ─── ProbabilityMeasure ──────────────────────────────────────────────────────

@dataclass
class ProbabilityMeasure(MathType):
    """
    A probability measure on a measurable space (Ω, F).

    Extends the core Measure type with full measure-theoretic semantics:
    countable additivity, absolute continuity, Radon-Nikodym derivatives.
    """
    name: str = "P"
    space: str = "Ω"
    sigma_algebra: str = "F"
    density: Optional[Callable[[NDArray], float]] = None
    log_density: Optional[Callable[[NDArray], float]] = None
    sample_fn: Optional[Callable[[int], NDArray]] = None
    radon_nikodym: Optional[Callable[[NDArray], float]] = None  # dP/dQ
    parameters: dict[str, Any] = field(default_factory=dict)
    is_absolutely_continuous: bool = True
    support: Optional[str] = None

    def type_name(self) -> str:
        return f"ProbMeasure[{self.name} on ({self.space}, {self.sigma_algebra})]"

    def canonical(self) -> "ProbabilityMeasure":
        return self

    def evaluate_density(self, x: NDArray) -> float:
        if self.density is not None:
            return self.density(x)
        if self.log_density is not None:
            return float(np.exp(self.log_density(x)))
        raise ValueError("No density function defined")

    def sample(self, n: int) -> NDArray:
        if self.sample_fn is not None:
            return self.sample_fn(n)
        raise ValueError("No sampling function defined")

    def __repr__(self) -> str:
        return f"ProbMeasure({self.name} on {self.space})"


# ─── SDEProcess ──────────────────────────────────────────────────────────────

@dataclass
class SDEProcess(MathType):
    """
    A stochastic differential equation process.

    In Itô form: dX_t = μ(X_t, t) dt + σ(X_t, t) dW_t
    In Stratonovich form: dX_t = μ(X_t, t) dt + σ(X_t, t) ∘ dW_t
    """
    name: str = "X"
    dim: int = 1
    noise_dim: int = 1
    sde_kind: SDEKind = SDEKind.ITO
    drift: Optional[Callable[[NDArray, float], NDArray]] = None   # μ(x, t)
    diffusion: Optional[Callable[[NDArray, float], NDArray]] = None  # σ(x, t)
    initial_condition: Optional[NDArray] = None
    parameters: dict[str, Any] = field(default_factory=dict)

    def type_name(self) -> str:
        return f"SDE[{self.sde_kind.name}, dim={self.dim}]"

    def canonical(self) -> "SDEProcess":
        return self

    def evaluate_drift(self, x: NDArray, t: float) -> NDArray:
        if self.drift is None:
            return np.zeros(self.dim)
        return self.drift(x, t)

    def evaluate_diffusion(self, x: NDArray, t: float) -> NDArray:
        if self.diffusion is None:
            return np.eye(self.dim, self.noise_dim)
        return self.diffusion(x, t)

    @staticmethod
    def geometric_brownian_motion(
        mu: float = 0.05, sigma: float = 0.2, x0: float = 1.0
    ) -> "SDEProcess":
        """dS = μS dt + σS dW."""
        return SDEProcess(
            name="GBM",
            dim=1, noise_dim=1, sde_kind=SDEKind.ITO,
            drift=lambda x, t: np.array([mu * x[0]]),
            diffusion=lambda x, t: np.array([[sigma * x[0]]]),
            initial_condition=np.array([x0]),
            parameters={"mu": mu, "sigma": sigma},
        )

    @staticmethod
    def ornstein_uhlenbeck(
        theta: float = 1.0, mu: float = 0.0, sigma: float = 0.3, x0: float = 0.0
    ) -> "SDEProcess":
        """dX = θ(μ - X) dt + σ dW."""
        return SDEProcess(
            name="OU",
            dim=1, noise_dim=1, sde_kind=SDEKind.ITO,
            drift=lambda x, t: np.array([theta * (mu - x[0])]),
            diffusion=lambda x, t: np.array([[sigma]]),
            initial_condition=np.array([x0]),
            parameters={"theta": theta, "mu": mu, "sigma": sigma},
        )

    @staticmethod
    def langevin_dynamics(
        potential_grad: Callable[[NDArray], NDArray],
        beta: float = 1.0, dim: int = 1,
    ) -> "SDEProcess":
        """dX = -∇V(X) dt + √(2/β) dW (overdamped Langevin)."""
        sigma = np.sqrt(2.0 / beta)
        return SDEProcess(
            name="Langevin",
            dim=dim, noise_dim=dim, sde_kind=SDEKind.ITO,
            drift=lambda x, t: -potential_grad(x),
            diffusion=lambda x, t: sigma * np.eye(dim),
            parameters={"beta": beta},
        )

    def __repr__(self) -> str:
        return f"SDE({self.name}, {self.sde_kind.name}, dim={self.dim})"


# ─── Flow ────────────────────────────────────────────────────────────────────

@dataclass
class Flow(MathType):
    """
    A flow φ: M × R → M on a manifold or state space.

    Satisfies:
    - φ(x, 0) = x
    - φ(φ(x, s), t) = φ(x, s + t)
    """
    name: str = "φ"
    dim: int = 0
    flow_map: Optional[Callable[[NDArray, float], NDArray]] = None
    generator: Optional[Callable[[NDArray], NDArray]] = None  # the vector field
    is_volume_preserving: bool = False
    is_symplectic: bool = False

    def type_name(self) -> str:
        props = []
        if self.is_volume_preserving:
            props.append("volume-preserving")
        if self.is_symplectic:
            props.append("symplectic")
        prop_str = f" ({', '.join(props)})" if props else ""
        return f"Flow[{self.name}, dim={self.dim}{prop_str}]"

    def canonical(self) -> "Flow":
        return self

    def evaluate(self, x: NDArray, t: float) -> NDArray:
        """Evaluate the flow at (x, t)."""
        if self.flow_map is not None:
            return self.flow_map(x, t)
        raise ValueError("No flow map defined; use numeric integration")

    def orbit(self, x0: NDArray, times: NDArray) -> NDArray:
        """Compute the orbit {φ(x0, t) : t ∈ times}."""
        return np.array([self.evaluate(x0, t) for t in times])

    def __repr__(self) -> str:
        return f"Flow({self.name}, dim={self.dim})"


# ─── VectorFieldIR ──────────────────────────────────────────────────────────

@dataclass
class VectorFieldIR(MathType):
    """
    A tangent vector field X on a manifold M.

    X: M → TM assigns a tangent vector to each point.
    """
    name: str = "X"
    dim: int = 0
    manifold_name: str = "M"
    evaluate_fn: Optional[Callable[[NDArray], NDArray]] = None
    is_smooth: bool = True
    is_divergence_free: bool = False
    is_killing: bool = False

    def type_name(self) -> str:
        return f"VectorField[{self.name} on {self.manifold_name}]"

    def canonical(self) -> "VectorFieldIR":
        return self

    def evaluate(self, point: NDArray) -> NDArray:
        """Evaluate the vector field at a point."""
        if self.evaluate_fn is not None:
            return self.evaluate_fn(point)
        raise ValueError("No evaluation function for vector field")

    def lie_bracket(self, other: "VectorFieldIR", point: NDArray,
                    eps: float = 1e-7) -> NDArray:
        """Compute Lie bracket [X, Y](p) via finite differences."""
        n = len(point)
        X_p = self.evaluate(point)
        Y_p = other.evaluate(point)

        # [X, Y] = DY · X - DX · Y (in coordinates)
        DX = np.zeros((n, n))
        DY = np.zeros((n, n))
        for i in range(n):
            p_plus = point.copy()
            p_minus = point.copy()
            p_plus[i] += eps
            p_minus[i] -= eps
            DX[:, i] = (self.evaluate(p_plus) - self.evaluate(p_minus)) / (2 * eps)
            DY[:, i] = (other.evaluate(p_plus) - other.evaluate(p_minus)) / (2 * eps)

        return DY @ X_p - DX @ Y_p

    def divergence(self, point: NDArray, eps: float = 1e-7) -> float:
        """Compute divergence div(X) at a point."""
        n = len(point)
        div = 0.0
        for i in range(n):
            p_plus = point.copy()
            p_minus = point.copy()
            p_plus[i] += eps
            p_minus[i] -= eps
            dXi = (self.evaluate(p_plus)[i] - self.evaluate(p_minus)[i]) / (2 * eps)
            div += dXi
        return div

    def __repr__(self) -> str:
        return f"VectorField({self.name} on {self.manifold_name})"


# ─── MonoidalStructure ───────────────────────────────────────────────────────

@dataclass
class MonoidalStructure(MathType):
    """
    A monoidal structure on a category: (C, ⊗, I).

    Provides a tensor product bifunctor ⊗: C × C → C and a unit object I,
    with associator and unitor natural isomorphisms.
    """
    name: str = "⊗"
    category_name: str = "C"
    kind: MonoidalKind = MonoidalKind.SYMMETRIC
    unit_object: Optional[str] = None
    tensor_product: Optional[Callable[[Any, Any], Any]] = None
    associator: Optional[Callable] = None
    left_unitor: Optional[Callable] = None
    right_unitor: Optional[Callable] = None
    braiding: Optional[Callable] = None  # for symmetric/braided

    def type_name(self) -> str:
        return f"Monoidal[{self.kind.name}]({self.category_name})"

    def canonical(self) -> "MonoidalStructure":
        return self

    def tensor(self, a: Any, b: Any) -> Any:
        """Apply tensor product a ⊗ b."""
        if self.tensor_product is not None:
            return self.tensor_product(a, b)
        raise ValueError("No tensor product defined")

    def __repr__(self) -> str:
        return f"Monoidal({self.kind.name} on {self.category_name})"


# ─── PDEOperator ─────────────────────────────────────────────────────────────

@dataclass
class PDEOperator(MathType):
    """
    A partial differential equation operator.

    Represents PDEs of the form:
        ∂u/∂t = L[u]  (parabolic, e.g., heat equation)
        ∂²u/∂t² = L[u]  (hyperbolic, e.g., wave equation)
        L[u] = f  (elliptic, e.g., Poisson)
        H(x, ∇u) = 0  (Hamilton-Jacobi)

    L is the spatial operator, which may include diffusion, advection,
    reaction, and nonlinear terms.
    """
    name: str = "L"
    pde_kind: PDEKind = PDEKind.HEAT
    spatial_dim: int = 1
    order: int = 2
    is_linear: bool = True
    is_elliptic: bool = False
    is_parabolic: bool = True
    is_hyperbolic: bool = False
    coefficients: dict[str, Any] = field(default_factory=dict)
    operator_fn: Optional[Callable] = None  # L[u](x) → value
    boundary_conditions: dict[str, Any] = field(default_factory=dict)

    def type_name(self) -> str:
        return f"PDE[{self.pde_kind.name}, dim={self.spatial_dim}, order={self.order}]"

    def canonical(self) -> "PDEOperator":
        return self

    def apply(self, u: Callable[[NDArray], float], x: NDArray) -> float:
        """Apply the PDE operator to a function u at point x."""
        if self.operator_fn is not None:
            return self.operator_fn(u, x)
        raise ValueError("No operator function defined")

    @staticmethod
    def heat_equation(diffusivity: float = 1.0, dim: int = 1) -> "PDEOperator":
        """∂u/∂t = α ∇²u."""
        def op(u: Callable, x: NDArray, eps: float = 1e-5) -> float:
            laplacian = 0.0
            for i in range(dim):
                x_plus = x.copy()
                x_minus = x.copy()
                x_plus[i] += eps
                x_minus[i] -= eps
                laplacian += (u(x_plus) - 2 * u(x) + u(x_minus)) / (eps ** 2)
            return diffusivity * laplacian

        return PDEOperator(
            name="Heat", pde_kind=PDEKind.HEAT, spatial_dim=dim,
            is_linear=True, is_parabolic=True,
            coefficients={"diffusivity": diffusivity},
            operator_fn=op,
        )

    @staticmethod
    def wave_equation(speed: float = 1.0, dim: int = 1) -> "PDEOperator":
        """∂²u/∂t² = c² ∇²u."""
        def op(u: Callable, x: NDArray, eps: float = 1e-5) -> float:
            laplacian = 0.0
            for i in range(dim):
                x_plus = x.copy()
                x_minus = x.copy()
                x_plus[i] += eps
                x_minus[i] -= eps
                laplacian += (u(x_plus) - 2 * u(x) + u(x_minus)) / (eps ** 2)
            return speed ** 2 * laplacian

        return PDEOperator(
            name="Wave", pde_kind=PDEKind.WAVE, spatial_dim=dim,
            is_linear=True, is_hyperbolic=True, is_parabolic=False,
            coefficients={"speed": speed},
            operator_fn=op,
        )

    @staticmethod
    def hamilton_jacobi(hamiltonian: Callable[[NDArray, NDArray], float],
                        dim: int = 1) -> "PDEOperator":
        """∂u/∂t + H(x, ∇u) = 0."""
        def op(u: Callable, x: NDArray, eps: float = 1e-5) -> float:
            grad = np.zeros(dim)
            for i in range(dim):
                x_plus = x.copy()
                x_minus = x.copy()
                x_plus[i] += eps
                x_minus[i] -= eps
                grad[i] = (u(x_plus) - u(x_minus)) / (2 * eps)
            return -hamiltonian(x, grad)

        return PDEOperator(
            name="HJ", pde_kind=PDEKind.HAMILTON_JACOBI, spatial_dim=dim,
            is_linear=False, is_parabolic=True,
            operator_fn=op,
        )

    def __repr__(self) -> str:
        return f"PDEOp({self.name}, {self.pde_kind.name}, dim={self.spatial_dim})"


# ─── GeometricFlow ───────────────────────────────────────────────────────────

@dataclass
class GeometricFlow(MathType):
    """
    A geometric flow: evolution of geometric structures (metrics, submanifolds)
    driven by curvature or other geometric quantities.

    ∂g/∂t = F(g, Rm, Ric, R)

    Examples:
    - Ricci flow: ∂g/∂t = -2 Ric(g)
    - Mean curvature flow: dX/dt = H · n
    - Yamabe flow: ∂g/∂t = -(R - r)g  where r = avg scalar curvature
    """
    name: str = "Ricci"
    flow_kind: GeometricFlowKind = GeometricFlowKind.RICCI
    dim: int = 0
    evolution_fn: Optional[Callable[[NDArray, float], NDArray]] = None
    singularity_time: Optional[float] = None
    normalized: bool = False

    def type_name(self) -> str:
        return f"GeometricFlow[{self.flow_kind.name}, dim={self.dim}]"

    def canonical(self) -> "GeometricFlow":
        return self

    def evolve(self, metric: NDArray, dt: float) -> NDArray:
        """Evolve the metric by one time step."""
        if self.evolution_fn is not None:
            return self.evolution_fn(metric, dt)
        raise ValueError("No evolution function defined")

    def __repr__(self) -> str:
        return f"GeometricFlow({self.flow_kind.name}, dim={self.dim})"
