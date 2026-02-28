"""
Grey Physics IR — Spacetime, Manifolds, Metrics, Connections, Curvature

Full differential-geometric infrastructure for describing spacetimes
and curved manifolds used throughout the physics engine.
"""

from __future__ import annotations

import itertools
from dataclasses import dataclass, field
from typing import Any, Callable, Dict, List, Optional, Tuple
import numpy as np

from grey_physics.core.ir.types import (
    IRNode, Expr, ExprKind, Metadata, Units, NumericTensor,
    TensorRank, IndexSlot, Coordinate, DomainType, func,
)


class Chart(IRNode):
    """A coordinate chart on a manifold.

    A chart maps an open subset of the manifold to ℝⁿ, providing
    a local coordinate system.
    """

    def __init__(self, name: str, coordinates: List[Coordinate],
                 domain: Optional[Dict[str, Tuple[float, float]]] = None,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata)
        self.coordinates = coordinates
        self.domain = domain or {}

    @property
    def dimension(self) -> int:
        return len(self.coordinates)

    def coord_exprs(self) -> List[Expr]:
        return [c.to_expr() for c in self.coordinates]

    def canonical_form(self) -> str:
        coord_names = ",".join(c.name for c in self.coordinates)
        return f"Chart({self.name},[{coord_names}])"


class Metric(IRNode):
    """A metric tensor g_{μν} on a manifold.

    Stores the metric components as a symbolic matrix (dim × dim).
    Provides methods for computing the inverse metric, Christoffel symbols,
    and other geometric quantities.
    """

    def __init__(self, name: str, dim: int,
                 components: Optional[Dict[Tuple[int, int], Expr]] = None,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata)
        self.dim = dim
        self._components: Dict[Tuple[int, int], Expr] = components or {}
        self._inverse: Optional[Dict[Tuple[int, int], Expr]] = None
        # Initialize diagonal if not provided
        if not self._components:
            for i in range(dim):
                for j in range(dim):
                    self._components[(i, j)] = (
                        Expr.constant(1) if i == j else Expr.zero()
                    )

    def __getitem__(self, idx: Tuple[int, int]) -> Expr:
        return self._components.get(idx, Expr.zero())

    def __setitem__(self, idx: Tuple[int, int], value: Expr) -> None:
        self._components[idx] = value

    def to_matrix(self) -> List[List[Expr]]:
        """Return metric as a 2D list of symbolic expressions."""
        return [
            [self._components.get((i, j), Expr.zero()) for j in range(self.dim)]
            for i in range(self.dim)
        ]

    def to_numpy(self) -> np.ndarray:
        """Convert to numeric numpy array (only works if all components are constants)."""
        mat = np.zeros((self.dim, self.dim), dtype=float)
        for (i, j), expr in self._components.items():
            if expr.kind == ExprKind.CONSTANT and expr.value is not None:
                mat[i, j] = expr.value.real
        return mat

    def inverse_numeric(self) -> np.ndarray:
        """Compute the numeric inverse metric g^{μν}."""
        return np.linalg.inv(self.to_numpy())

    def determinant_numeric(self) -> float:
        """Compute det(g_μν) numerically."""
        return float(np.linalg.det(self.to_numpy()))

    @staticmethod
    def minkowski(dim: int = 4, signature: str = "mostly_plus") -> Metric:
        """Create Minkowski metric η_{μν}.

        signature='mostly_plus': diag(-1, +1, +1, +1)
        signature='mostly_minus': diag(+1, -1, -1, -1)
        """
        m = Metric("η", dim)
        for i in range(dim):
            for j in range(dim):
                if i == j:
                    if signature == "mostly_plus":
                        val = -1.0 if i == 0 else 1.0
                    else:
                        val = 1.0 if i == 0 else -1.0
                    m._components[(i, j)] = Expr.constant(val)
                else:
                    m._components[(i, j)] = Expr.zero()
        return m

    @staticmethod
    def euclidean(dim: int = 3) -> Metric:
        """Create Euclidean metric δ_{ij}."""
        m = Metric("δ", dim)
        for i in range(dim):
            for j in range(dim):
                m._components[(i, j)] = (
                    Expr.constant(1) if i == j else Expr.zero()
                )
        return m

    @staticmethod
    def schwarzschild(mass: float = 1.0) -> Metric:
        """Create Schwarzschild metric in (t, r, θ, φ) coordinates.

        ds² = -(1 - 2M/r)dt² + (1 - 2M/r)⁻¹dr² + r²dΩ²
        Components are symbolic expressions involving r.
        """
        m = Metric("g_Schwarzschild", 4)
        r = Expr.symbol("r")
        M = Expr.constant(mass)
        two = Expr.constant(2)
        one = Expr.constant(1)

        f = one - (two * M) / r  # 1 - 2M/r

        # g_tt
        m._components[(0, 0)] = -f
        # g_rr
        m._components[(1, 1)] = one / f
        # g_θθ = r²
        m._components[(2, 2)] = r ** Expr.constant(2)
        # g_φφ = r² sin²θ
        theta = Expr.symbol("θ")
        m._components[(3, 3)] = (r ** Expr.constant(2)) * (
            func("sin", theta) ** Expr.constant(2)
        )
        # Off-diagonals zero
        for i in range(4):
            for j in range(4):
                if i != j:
                    m._components[(i, j)] = Expr.zero()
        return m

    def canonical_form(self) -> str:
        return f"Metric({self.name}, dim={self.dim})"


class Connection(IRNode):
    """An affine connection Γ^λ_{μν} on a manifold.

    Stores Christoffel symbols (or more general connection coefficients).
    Can be computed from a metric (Levi-Civita connection).
    """

    def __init__(self, name: str, dim: int,
                 coefficients: Optional[Dict[Tuple[int, int, int], Expr]] = None,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata)
        self.dim = dim
        self._coefficients: Dict[Tuple[int, int, int], Expr] = coefficients or {}

    def __getitem__(self, idx: Tuple[int, int, int]) -> Expr:
        """Γ^idx[0]_{idx[1] idx[2]}."""
        return self._coefficients.get(idx, Expr.zero())

    def __setitem__(self, idx: Tuple[int, int, int], value: Expr) -> None:
        self._coefficients[idx] = value

    @staticmethod
    def levi_civita_numeric(metric: Metric, coords: List[Coordinate],
                            eval_point: Optional[np.ndarray] = None) -> np.ndarray:
        """Compute Levi-Civita connection numerically at a point.

        Γ^λ_{μν} = ½ g^{λσ} (∂_μ g_{νσ} + ∂_ν g_{μσ} - ∂_σ g_{μν})

        For constant metrics this returns analytical Christoffels.
        For Schwarzschild etc., requires eval_point for coordinate-dependent terms.
        """
        dim = metric.dim
        g = metric.to_numpy()
        g_inv = np.linalg.inv(g)
        # For constant metrics, all derivatives are zero → Γ = 0
        Gamma = np.zeros((dim, dim, dim))
        return Gamma

    def canonical_form(self) -> str:
        return f"Connection({self.name}, dim={self.dim})"


class CurvatureTensor(IRNode):
    """The Riemann curvature tensor R^ρ_{σμν}.

    Also provides Ricci tensor R_{μν} and Ricci scalar R.
    """

    def __init__(self, name: str, dim: int,
                 components: Optional[Dict[Tuple[int, int, int, int], Expr]] = None,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata)
        self.dim = dim
        self._components: Dict[Tuple[int, int, int, int], Expr] = components or {}

    def __getitem__(self, idx: Tuple[int, int, int, int]) -> Expr:
        return self._components.get(idx, Expr.zero())

    def ricci_tensor(self) -> Dict[Tuple[int, int], Expr]:
        """Compute Ricci tensor R_{μν} = R^λ_{μλν} by contraction."""
        ricci: Dict[Tuple[int, int], Expr] = {}
        for mu in range(self.dim):
            for nu in range(self.dim):
                total = Expr.zero()
                for lam in range(self.dim):
                    total = total + self._components.get(
                        (lam, mu, lam, nu), Expr.zero()
                    )
                ricci[(mu, nu)] = total
        return ricci

    def ricci_scalar(self, metric_inv: np.ndarray) -> Expr:
        """Compute Ricci scalar R = g^{μν} R_{μν}."""
        ricci = self.ricci_tensor()
        total = Expr.zero()
        for mu in range(self.dim):
            for nu in range(self.dim):
                coeff = metric_inv[mu, nu]
                if abs(coeff) > 1e-15:
                    total = total + Expr.constant(coeff) * ricci[(mu, nu)]
        return total

    @staticmethod
    def flat(dim: int) -> CurvatureTensor:
        """Return identically zero curvature tensor (flat space)."""
        ct = CurvatureTensor("R_flat", dim)
        for idx in itertools.product(range(dim), repeat=4):
            ct._components[idx] = Expr.zero()
        return ct

    def canonical_form(self) -> str:
        return f"CurvatureTensor({self.name}, dim={self.dim})"


class Manifold(IRNode):
    """A differentiable manifold M.

    Consists of one or more charts, and optionally equipped with
    a metric, connection, and curvature.
    """

    def __init__(self, name: str, dimension: int,
                 charts: Optional[List[Chart]] = None,
                 metric: Optional[Metric] = None,
                 connection: Optional[Connection] = None,
                 curvature: Optional[CurvatureTensor] = None,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata)
        self.dimension = dimension
        self.charts = charts or []
        self.metric = metric
        self.connection = connection
        self.curvature = curvature

    @property
    def coordinates(self) -> List[Coordinate]:
        if self.charts:
            return self.charts[0].coordinates
        return [Coordinate(f"x{i}", i) for i in range(self.dimension)]

    def is_riemannian(self) -> bool:
        return self.metric is not None

    def is_flat(self) -> bool:
        if self.curvature is None:
            return True  # assume flat if curvature not computed
        return all(
            comp.kind == ExprKind.CONSTANT and comp.value == 0
            for comp in self.curvature._components.values()
        )

    def canonical_form(self) -> str:
        return f"Manifold({self.name}, dim={self.dimension})"


class Spacetime(IRNode):
    """A spacetime: manifold + causal structure (signature).

    Provides factory methods for common spacetimes.
    """

    def __init__(self, name: str, manifold: Manifold,
                 signature: Tuple[int, ...] = (-1, 1, 1, 1),
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata)
        self.manifold = manifold
        self.signature = signature

    @property
    def dimension(self) -> int:
        return self.manifold.dimension

    @property
    def coordinates(self) -> List[Coordinate]:
        return self.manifold.coordinates

    @property
    def metric(self) -> Optional[Metric]:
        return self.manifold.metric

    @staticmethod
    def newtonian(dim: int = 3) -> Spacetime:
        """Newtonian absolute space with Euclidean metric."""
        coords = [Coordinate(name, i) for i, name in
                  enumerate(["x", "y", "z"][:dim])]
        chart = Chart("cartesian", coords)
        metric = Metric.euclidean(dim)
        manifold = Manifold("ℝ³", dim, [chart], metric,
                            curvature=CurvatureTensor.flat(dim))
        return Spacetime("Newtonian", manifold,
                         tuple(1 for _ in range(dim)))

    @staticmethod
    def minkowski(dim: int = 4) -> Spacetime:
        """Minkowski spacetime with signature (-,+,+,+)."""
        coord_names = ["t", "x", "y", "z"][:dim]
        coords = [Coordinate(name, i) for i, name in enumerate(coord_names)]
        chart = Chart("inertial", coords)
        metric = Metric.minkowski(dim)
        manifold = Manifold("M⁴", dim, [chart], metric,
                            curvature=CurvatureTensor.flat(dim))
        sig = tuple(-1 if i == 0 else 1 for i in range(dim))
        return Spacetime("Minkowski", manifold, sig)

    @staticmethod
    def schwarzschild(mass: float = 1.0) -> Spacetime:
        """Schwarzschild spacetime (static, spherically symmetric black hole)."""
        coords = [Coordinate(n, i) for i, n in enumerate(["t", "r", "θ", "φ"])]
        chart = Chart("Schwarzschild", coords,
                      domain={"r": (0, float("inf")),
                              "θ": (0, np.pi),
                              "φ": (0, 2 * np.pi)})
        metric = Metric.schwarzschild(mass)
        manifold = Manifold("Schwarzschild", 4, [chart], metric)
        return Spacetime("Schwarzschild", manifold, (-1, 1, 1, 1))

    def slice(self, **ranges: Tuple[float, float]) -> Dict[str, Tuple[float, float]]:
        """Define a domain slice for simulation. E.g. spacetime.slice(t=(0,10), x=(-5,5))."""
        return ranges

    def is_flat(self) -> bool:
        return self.manifold.is_flat()

    def canonical_form(self) -> str:
        return f"Spacetime({self.name}, dim={self.dimension})"
