"""
Differential Geometry Module.

Provides:
- Manifolds, charts, atlases
- Riemannian metrics and curvature
- Geodesics, exponential and logarithmic maps
- Riemannian optimization (gradient descent on manifolds)
- Tangent spaces and vector fields
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Callable, Optional

import numpy as np
from numpy.typing import NDArray


# ─── Manifold Interface ─────────────────────────────────────────────────────

class Manifold(ABC):
    """
    Abstract base for Riemannian manifolds.

    A manifold M is equipped with:
    - A dimension
    - A metric tensor g(x) at each point
    - Projection to the tangent space
    - Exponential and logarithmic maps
    - Geodesic computation
    - Curvature computation
    """

    @property
    @abstractmethod
    def dim(self) -> int:
        """Dimension of the manifold."""
        ...

    @property
    @abstractmethod
    def name(self) -> str:
        """Name of the manifold."""
        ...

    @abstractmethod
    def metric(self, x: NDArray) -> NDArray:
        """Riemannian metric tensor g(x) at point x."""
        ...

    @abstractmethod
    def inner_product(self, x: NDArray, u: NDArray, v: NDArray) -> float:
        """Inner product <u, v>_x in the tangent space at x."""
        ...

    @abstractmethod
    def project_tangent(self, x: NDArray, v: NDArray) -> NDArray:
        """Project vector v onto the tangent space at x."""
        ...

    @abstractmethod
    def exp_map(self, x: NDArray, v: NDArray) -> NDArray:
        """Exponential map: Exp_x(v) → M."""
        ...

    @abstractmethod
    def log_map(self, x: NDArray, y: NDArray) -> NDArray:
        """Logarithmic map: Log_x(y) → T_x M."""
        ...

    def distance(self, x: NDArray, y: NDArray) -> float:
        """Geodesic distance d(x, y)."""
        v = self.log_map(x, y)
        return float(np.sqrt(self.inner_product(x, v, v)))

    def norm(self, x: NDArray, v: NDArray) -> float:
        """Norm of tangent vector v at x."""
        return float(np.sqrt(self.inner_product(x, v, v)))

    def parallel_transport(self, x: NDArray, y: NDArray, v: NDArray) -> NDArray:
        """Parallel transport of v from T_x M to T_y M along geodesic."""
        # Default: Schild's ladder approximation
        mid = self.exp_map(x, 0.5 * self.log_map(x, y))
        x_prime = self.exp_map(x, v)
        mid_to_x_prime = self.log_map(mid, x_prime)
        return self.log_map(y, self.exp_map(mid, mid_to_x_prime))


# ─── Euclidean Space ────────────────────────────────────────────────────────

class EuclideanSpace(Manifold):
    """R^n with the standard metric."""

    def __init__(self, n: int) -> None:
        self._dim = n

    @property
    def dim(self) -> int:
        return self._dim

    @property
    def name(self) -> str:
        return f"R^{self._dim}"

    def metric(self, x: NDArray) -> NDArray:
        return np.eye(self._dim)

    def inner_product(self, x: NDArray, u: NDArray, v: NDArray) -> float:
        return float(np.dot(u, v))

    def project_tangent(self, x: NDArray, v: NDArray) -> NDArray:
        return v

    def exp_map(self, x: NDArray, v: NDArray) -> NDArray:
        return x + v

    def log_map(self, x: NDArray, y: NDArray) -> NDArray:
        return y - x


# ─── Sphere ─────────────────────────────────────────────────────────────────

class Sphere(Manifold):
    """
    The n-sphere S^n = {x ∈ R^{n+1} : ||x|| = 1}.
    """

    def __init__(self, n: int) -> None:
        self._dim = n

    @property
    def dim(self) -> int:
        return self._dim

    @property
    def name(self) -> str:
        return f"S^{self._dim}"

    def metric(self, x: NDArray) -> NDArray:
        """Induced metric from ambient R^{n+1}."""
        n = len(x)
        return np.eye(n) - np.outer(x, x)

    def inner_product(self, x: NDArray, u: NDArray, v: NDArray) -> float:
        return float(np.dot(u, v))

    def project_tangent(self, x: NDArray, v: NDArray) -> NDArray:
        """Project to tangent space: v - <v,x>x."""
        return v - np.dot(v, x) * x

    def exp_map(self, x: NDArray, v: NDArray) -> NDArray:
        """Exp_x(v) = cos(||v||)x + sin(||v||) v/||v||."""
        norm_v = np.linalg.norm(v)
        if norm_v < 1e-15:
            return x
        return np.cos(norm_v) * x + np.sin(norm_v) * v / norm_v

    def log_map(self, x: NDArray, y: NDArray) -> NDArray:
        """Log_x(y) = d(x,y) * (y - <y,x>x) / ||y - <y,x>x||."""
        proj = y - np.dot(y, x) * x
        norm_proj = np.linalg.norm(proj)
        if norm_proj < 1e-15:
            return np.zeros_like(x)
        theta = np.arccos(np.clip(np.dot(x, y), -1, 1))
        return theta * proj / norm_proj

    def random_point(self, rng: Optional[np.random.Generator] = None) -> NDArray:
        """Sample a uniform point on the sphere."""
        rng = rng or np.random.default_rng()
        x = rng.standard_normal(self._dim + 1)
        return x / np.linalg.norm(x)


# ─── Stiefel Manifold ──────────────────────────────────────────────────────

class StiefelManifold(Manifold):
    """
    Stiefel manifold St(n, p): set of n×p orthonormal matrices.
    Used in optimization on orthogonality constraints.
    """

    def __init__(self, n: int, p: int) -> None:
        self._n = n
        self._p = p

    @property
    def dim(self) -> int:
        return self._n * self._p - self._p * (self._p + 1) // 2

    @property
    def name(self) -> str:
        return f"St({self._n},{self._p})"

    def metric(self, X: NDArray) -> NDArray:
        """Canonical metric (identity on ambient space)."""
        return np.eye(self._n * self._p)

    def inner_product(self, X: NDArray, U: NDArray, V: NDArray) -> float:
        return float(np.trace(U.T @ V))

    def project_tangent(self, X: NDArray, Z: NDArray) -> NDArray:
        """Project Z onto T_X St(n,p)."""
        # Tangent space: {Z : X^T Z + Z^T X = 0}
        sym = X.T @ Z
        return Z - X @ (sym + sym.T) / 2

    def exp_map(self, X: NDArray, V: NDArray) -> NDArray:
        """Retraction via QR decomposition (approximation to exp map)."""
        Y = X + V
        Q, R = np.linalg.qr(Y)
        # Ensure consistent signs
        return Q @ np.diag(np.sign(np.diag(R)))

    def log_map(self, X: NDArray, Y: NDArray) -> NDArray:
        """Approximate log map."""
        return self.project_tangent(X, Y - X)


# ─── Riemannian Curvature ──────────────────────────────────────────────────

@dataclass
class CurvatureData:
    """Curvature information at a point on a manifold."""
    point: NDArray
    sectional_curvature: Optional[float] = None
    ricci_curvature: Optional[NDArray] = None
    scalar_curvature: Optional[float] = None
    christoffel_symbols: Optional[NDArray] = None

    def __repr__(self) -> str:
        return f"Curvature(K={self.sectional_curvature}, R_scalar={self.scalar_curvature})"


class RiemannianGeometry:
    """
    Compute Riemannian geometric quantities on manifolds.
    """

    @staticmethod
    def christoffel_symbols(
        metric_fn: Callable[[NDArray], NDArray],
        x: NDArray,
        eps: float = 1e-6,
    ) -> NDArray:
        """
        Compute Christoffel symbols Γ^k_{ij} via numerical differentiation
        of the metric tensor.
        """
        n = len(x)
        g = metric_fn(x)
        g_inv = np.linalg.inv(g)

        # Numerical derivatives of metric components
        dg = np.zeros((n, n, n))  # dg[i,j,k] = ∂g_{ij}/∂x^k
        for k in range(n):
            x_plus = x.copy()
            x_minus = x.copy()
            x_plus[k] += eps
            x_minus[k] -= eps
            dg[:, :, k] = (metric_fn(x_plus) - metric_fn(x_minus)) / (2 * eps)

        # Γ^k_{ij} = (1/2) g^{kl} (∂g_{li}/∂x^j + ∂g_{lj}/∂x^i - ∂g_{ij}/∂x^l)
        gamma = np.zeros((n, n, n))
        for k in range(n):
            for i in range(n):
                for j in range(n):
                    s = 0.0
                    for l in range(n):
                        s += g_inv[k, l] * (
                            dg[l, i, j] + dg[l, j, i] - dg[i, j, l]
                        )
                    gamma[k, i, j] = 0.5 * s

        return gamma

    @staticmethod
    def sectional_curvature(
        metric_fn: Callable[[NDArray], NDArray],
        x: NDArray,
        u: NDArray,
        v: NDArray,
        eps: float = 1e-5,
    ) -> float:
        """
        Estimate sectional curvature K(u,v) at point x.

        Uses the Bertrand-Diguet-Puiseux theorem approximation.
        """
        g = metric_fn(x)
        g_uv = float(u @ g @ v)
        g_uu = float(u @ g @ u)
        g_vv = float(v @ g @ v)
        area_sq = g_uu * g_vv - g_uv ** 2

        if abs(area_sq) < 1e-30:
            return 0.0

        # Finite-difference Riemann tensor approximation
        # Simplified: use parallel transport defect
        n = len(x)
        gamma = RiemannianGeometry.christoffel_symbols(metric_fn, x, eps)

        # R^l_{ijk} approximation
        R = np.zeros((n, n, n, n))
        for l in range(n):
            for i in range(n):
                for j in range(n):
                    for k in range(n):
                        for m in range(n):
                            R[l, i, j, k] += (
                                gamma[l, j, k] * gamma[m, i, m]
                                - gamma[l, i, k] * gamma[m, j, m]
                            )

        # K(u,v) = R(u,v,v,u) / (g(u,u)g(v,v) - g(u,v)^2)
        Ruvvu = 0.0
        for l in range(n):
            for i in range(n):
                for j in range(n):
                    for k in range(n):
                        Ruvvu += g[l, k] * R[k, i, j, l] * u[i] * v[j] * v[l] * u[k]

        return Ruvvu / area_sq if abs(area_sq) > 1e-30 else 0.0


# ─── Riemannian Optimization ───────────────────────────────────────────────

class RiemannianOptimizer:
    """
    Gradient descent on Riemannian manifolds.

    Minimizes f(x) for x ∈ M using:
    1. Compute Riemannian gradient (project Euclidean gradient to tangent space)
    2. Step via exponential map
    """

    def minimize(
        self,
        manifold: Manifold,
        f: Callable[[NDArray], float],
        grad_f: Callable[[NDArray], NDArray],
        x0: NDArray,
        learning_rate: float = 0.01,
        max_iter: int = 1000,
        tol: float = 1e-8,
    ) -> tuple[NDArray, list[float]]:
        """
        Riemannian gradient descent.

        Returns: (optimal_point, loss_history)
        """
        x = x0.copy()
        losses = [f(x)]

        for i in range(max_iter):
            # Euclidean gradient
            egrad = grad_f(x)

            # Riemannian gradient: project to tangent space
            rgrad = manifold.project_tangent(x, egrad)

            # Check convergence
            grad_norm = manifold.norm(x, rgrad)
            if grad_norm < tol:
                break

            # Step via exponential map (negative gradient direction)
            x = manifold.exp_map(x, -learning_rate * rgrad)
            losses.append(f(x))

        return x, losses
