"""
Advanced Differential Geometry — Experimental Module.

Extends the core differential geometry with:
- Tangent and cotangent bundles
- Fiber bundles with connections
- Riemannian curvature tensors (full Riemann, Ricci, scalar, Weyl)
- Geodesic flows and geodesic equations
- Lie groups and Lie algebras
- Exponential map for Lie groups
- Geometric PDEs and natural gradient
- Cartan's moving frames
- Levi-Civita connection computation
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Callable, Optional

import numpy as np
from numpy.typing import NDArray

from greymath.domains.differential_geometry import Manifold, RiemannianGeometry
from greymath.experimental.ir_extensions import (
    Chart, Atlas, Connection, FiberBundle,
    VectorFieldIR, GeometricFlow, GeometricFlowKind,
)


# ─── Tangent / Cotangent Bundles ─────────────────────────────────────────────

@dataclass
class TangentBundle:
    """
    The tangent bundle TM of a manifold M.

    TM = ∪_{p∈M} T_p M is itself a manifold of dimension 2n (if M has dim n).
    A point in TM is (p, v) where p ∈ M and v ∈ T_p M.
    """
    manifold: Manifold
    name: str = "TM"

    @property
    def total_dim(self) -> int:
        return 2 * self.manifold.dim

    def project(self, bundle_point: NDArray) -> NDArray:
        """Project (p, v) → p."""
        return bundle_point[: self.manifold.dim]

    def fiber(self, bundle_point: NDArray) -> NDArray:
        """Extract the tangent vector v from (p, v)."""
        return bundle_point[self.manifold.dim:]

    def zero_section(self, point: NDArray) -> NDArray:
        """The zero section: p → (p, 0)."""
        return np.concatenate([point, np.zeros(self.manifold.dim)])

    def __repr__(self) -> str:
        return f"TangentBundle({self.manifold.name}, dim={self.total_dim})"


@dataclass
class CotangentBundle:
    """
    The cotangent bundle T*M of a manifold M.

    T*M = ∪_{p∈M} T*_p M. Points are (p, ω) where ω is a covector.
    The cotangent bundle carries a natural symplectic structure.
    """
    manifold: Manifold
    name: str = "T*M"

    @property
    def total_dim(self) -> int:
        return 2 * self.manifold.dim

    def canonical_symplectic_form(self, z: NDArray) -> NDArray:
        """
        The canonical symplectic form ω = Σ dp_i ∧ dq_i.
        Returns the symplectic matrix J.
        """
        n = self.manifold.dim
        J = np.zeros((2 * n, 2 * n))
        J[:n, n:] = np.eye(n)
        J[n:, :n] = -np.eye(n)
        return J

    def hamiltonian_vector_field(
        self, H_grad: Callable[[NDArray], NDArray]
    ) -> Callable[[NDArray], NDArray]:
        """
        Given ∇H, compute the Hamiltonian vector field X_H = J ∇H.
        """
        n = self.manifold.dim

        def X_H(z: NDArray) -> NDArray:
            grad = H_grad(z)
            # X_H = (∂H/∂p, -∂H/∂q)
            return np.concatenate([grad[n:], -grad[:n]])

        return X_H

    def __repr__(self) -> str:
        return f"CotangentBundle({self.manifold.name})"


# ─── Curvature Tensors ──────────────────────────────────────────────────────

class CurvatureTensors:
    """
    Compute Riemannian curvature tensors from a metric.

    Provides:
    - Christoffel symbols Γ^k_{ij}
    - Riemann curvature tensor R^l_{ijk}
    - Ricci tensor R_{ij}
    - Scalar curvature R
    - Weyl tensor (in dim ≥ 3)
    - Sectional curvature K(u, v)
    """

    @staticmethod
    def christoffel(
        metric_fn: Callable[[NDArray], NDArray],
        point: NDArray, eps: float = 1e-6,
    ) -> NDArray:
        """Compute Christoffel symbols Γ^k_{ij} via numerical differentiation."""
        n = len(point)
        g = metric_fn(point)
        g_inv = np.linalg.inv(g)

        dg = np.zeros((n, n, n))
        for k in range(n):
            e_k = np.zeros(n)
            e_k[k] = eps
            dg[:, :, k] = (metric_fn(point + e_k) - metric_fn(point - e_k)) / (2 * eps)

        gamma = np.zeros((n, n, n))
        for k in range(n):
            for i in range(n):
                for j in range(n):
                    s = 0.0
                    for l in range(n):
                        s += g_inv[k, l] * (dg[l, i, j] + dg[l, j, i] - dg[i, j, l])
                    gamma[k, i, j] = 0.5 * s
        return gamma

    @staticmethod
    def riemann_tensor(
        metric_fn: Callable[[NDArray], NDArray],
        point: NDArray, eps: float = 1e-5,
    ) -> NDArray:
        """
        Compute Riemann curvature tensor R^l_{ijk}.

        R^l_{ijk} = ∂_j Γ^l_{ik} - ∂_i Γ^l_{jk} + Γ^l_{jm} Γ^m_{ik} - Γ^l_{im} Γ^m_{jk}
        """
        n = len(point)
        gamma = CurvatureTensors.christoffel(metric_fn, point, eps)

        # Derivative of Christoffel symbols
        dgamma = np.zeros((n, n, n, n))  # dgamma[l,i,j,k] = ∂Γ^l_{ij}/∂x^k
        for k in range(n):
            e_k = np.zeros(n)
            e_k[k] = eps
            gamma_plus = CurvatureTensors.christoffel(metric_fn, point + e_k, eps)
            gamma_minus = CurvatureTensors.christoffel(metric_fn, point - e_k, eps)
            dgamma[:, :, :, k] = (gamma_plus - gamma_minus) / (2 * eps)

        R = np.zeros((n, n, n, n))
        for l in range(n):
            for i in range(n):
                for j in range(n):
                    for k in range(n):
                        R[l, i, j, k] = dgamma[l, i, k, j] - dgamma[l, j, k, i]
                        for m in range(n):
                            R[l, i, j, k] += (
                                gamma[l, j, m] * gamma[m, i, k]
                                - gamma[l, i, m] * gamma[m, j, k]
                            )
        return R

    @staticmethod
    def ricci_tensor(
        metric_fn: Callable[[NDArray], NDArray],
        point: NDArray, eps: float = 1e-5,
    ) -> NDArray:
        """
        Ricci tensor R_{ij} = R^k_{ikj} (contraction of Riemann tensor).
        """
        n = len(point)
        R = CurvatureTensors.riemann_tensor(metric_fn, point, eps)
        Ric = np.zeros((n, n))
        for i in range(n):
            for j in range(n):
                for k in range(n):
                    Ric[i, j] += R[k, i, k, j]
        return Ric

    @staticmethod
    def scalar_curvature(
        metric_fn: Callable[[NDArray], NDArray],
        point: NDArray, eps: float = 1e-5,
    ) -> float:
        """Scalar curvature R = g^{ij} R_{ij}."""
        g = metric_fn(point)
        g_inv = np.linalg.inv(g)
        Ric = CurvatureTensors.ricci_tensor(metric_fn, point, eps)
        return float(np.trace(g_inv @ Ric))

    @staticmethod
    def sectional_curvature(
        metric_fn: Callable[[NDArray], NDArray],
        point: NDArray, u: NDArray, v: NDArray,
        eps: float = 1e-5,
    ) -> float:
        """
        Sectional curvature K(u, v) = R(u,v,v,u) / (|u|²|v|² - <u,v>²).
        """
        n = len(point)
        g = metric_fn(point)
        R = CurvatureTensors.riemann_tensor(metric_fn, point, eps)

        # Lower the first index: R_{lijk} = g_{lm} R^m_{ijk}
        R_lower = np.einsum("lm,mijk->lijk", g, R)

        # Compute R(u,v,v,u) = R_{lijk} u^l v^i v^j u^k
        Ruvvu = np.einsum("lijk,l,i,j,k->", R_lower, u, v, v, u)

        g_uu = float(u @ g @ u)
        g_vv = float(v @ g @ v)
        g_uv = float(u @ g @ v)
        denom = g_uu * g_vv - g_uv ** 2

        if abs(denom) < 1e-30:
            return 0.0
        return float(Ruvvu / denom)

    @staticmethod
    def weyl_tensor(
        metric_fn: Callable[[NDArray], NDArray],
        point: NDArray, eps: float = 1e-5,
    ) -> Optional[NDArray]:
        """
        Weyl conformal curvature tensor (defined for dim ≥ 3).

        W_{ijkl} = R_{ijkl} - (2/(n-2))(g_{i[k}R_{l]j} - g_{j[k}R_{l]i})
                   + (2/((n-1)(n-2))) R g_{i[k} g_{l]j}
        """
        n = len(point)
        if n < 3:
            return None

        g = metric_fn(point)
        R_tensor = CurvatureTensors.riemann_tensor(metric_fn, point, eps)
        R_lower = np.einsum("lm,mijk->lijk", g, R_tensor)
        Ric = CurvatureTensors.ricci_tensor(metric_fn, point, eps)
        R_scalar = float(np.trace(np.linalg.inv(g) @ Ric))

        W = np.zeros((n, n, n, n))
        for i in range(n):
            for j in range(n):
                for k in range(n):
                    for l in range(n):
                        W[i, j, k, l] = R_lower[i, j, k, l]
                        # Ricci part
                        W[i, j, k, l] -= (1.0 / (n - 2)) * (
                            g[i, k] * Ric[l, j] - g[i, l] * Ric[k, j]
                            - g[j, k] * Ric[l, i] + g[j, l] * Ric[k, i]
                        )
                        # Scalar curvature part
                        W[i, j, k, l] += (R_scalar / ((n - 1) * (n - 2))) * (
                            g[i, k] * g[l, j] - g[i, l] * g[k, j]
                        )
        return W


# ─── Geodesic Solver ─────────────────────────────────────────────────────────

class GeodesicSolver:
    """
    Solve the geodesic equation on Riemannian manifolds.

    The geodesic equation is:
        d²x^k/dt² + Γ^k_{ij} (dx^i/dt)(dx^j/dt) = 0

    This is a second-order ODE system, converted to first-order:
        dx^k/dt = v^k
        dv^k/dt = -Γ^k_{ij} v^i v^j
    """

    @staticmethod
    def solve(
        metric_fn: Callable[[NDArray], NDArray],
        x0: NDArray, v0: NDArray,
        t_span: tuple[float, float] = (0.0, 1.0),
        n_steps: int = 1000,
        eps: float = 1e-6,
    ) -> tuple[NDArray, NDArray, NDArray]:
        """
        Integrate the geodesic equation.

        Args:
            metric_fn: Function returning metric tensor at a point
            x0: Initial position
            v0: Initial velocity (tangent vector)
            t_span: Integration interval
            n_steps: Number of time steps

        Returns:
            (times, positions, velocities)
        """
        n = len(x0)
        dt = (t_span[1] - t_span[0]) / n_steps
        times = np.linspace(t_span[0], t_span[1], n_steps + 1)
        positions = np.zeros((n_steps + 1, n))
        velocities = np.zeros((n_steps + 1, n))

        positions[0] = x0.copy()
        velocities[0] = v0.copy()

        for step in range(n_steps):
            x = positions[step]
            v = velocities[step]

            gamma = CurvatureTensors.christoffel(metric_fn, x, eps)

            # Geodesic acceleration: a^k = -Γ^k_{ij} v^i v^j
            a = np.zeros(n)
            for k in range(n):
                for i in range(n):
                    for j in range(n):
                        a[k] -= gamma[k, i, j] * v[i] * v[j]

            # RK4 integration
            def rhs(state: NDArray) -> NDArray:
                xx, vv = state[:n], state[n:]
                g = CurvatureTensors.christoffel(metric_fn, xx, eps)
                acc = np.zeros(n)
                for k in range(n):
                    for i in range(n):
                        for j in range(n):
                            acc[k] -= g[k, i, j] * vv[i] * vv[j]
                return np.concatenate([vv, acc])

            state = np.concatenate([x, v])
            k1 = rhs(state)
            k2 = rhs(state + 0.5 * dt * k1)
            k3 = rhs(state + 0.5 * dt * k2)
            k4 = rhs(state + dt * k3)
            state_new = state + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)

            positions[step + 1] = state_new[:n]
            velocities[step + 1] = state_new[n:]

        return times, positions, velocities

    @staticmethod
    def geodesic_distance(
        metric_fn: Callable[[NDArray], NDArray],
        x: NDArray, y: NDArray, n_steps: int = 100,
    ) -> float:
        """
        Approximate geodesic distance by integrating the line element
        along a straight path (lower bound, exact for flat metrics).
        """
        n = len(x)
        total = 0.0
        for i in range(n_steps):
            t = i / n_steps
            t_next = (i + 1) / n_steps
            p = x + t * (y - x)
            dp = (y - x) / n_steps
            g = metric_fn(p)
            ds = float(np.sqrt(dp @ g @ dp))
            total += ds
        return total


# ─── Lie Groups and Lie Algebras ─────────────────────────────────────────────

class LieGroupKind(Enum):
    """Standard Lie groups."""
    GL = auto()    # General linear GL(n)
    SL = auto()    # Special linear SL(n)
    SO = auto()    # Special orthogonal SO(n)
    SU = auto()    # Special unitary SU(n)
    SE = auto()    # Special Euclidean SE(n)
    SP = auto()    # Symplectic Sp(2n)
    CUSTOM = auto()


@dataclass
class LieAlgebra:
    """
    A Lie algebra 𝔤: the tangent space at the identity of a Lie group,
    equipped with the Lie bracket [·, ·].
    """
    name: str = "g"
    dim: int = 0
    basis: Optional[list[NDArray]] = None
    structure_constants: Optional[NDArray] = None  # c^k_{ij}: [e_i, e_j] = c^k_{ij} e_k

    def bracket(self, X: NDArray, Y: NDArray) -> NDArray:
        """Compute the Lie bracket [X, Y] = XY - YX (for matrix Lie algebras)."""
        return X @ Y - Y @ X

    def adjoint(self, X: NDArray) -> NDArray:
        """Adjoint representation ad(X): Y → [X, Y]."""
        # For matrix algebras, ad(X) is a linear map
        if self.dim == 0:
            return X  # fallback
        n = X.shape[0]
        return X  # return the matrix; actual adjoint needs basis

    def killing_form(self, X: NDArray, Y: NDArray) -> float:
        """
        Killing form B(X, Y) = tr(ad(X) ∘ ad(Y)).

        For matrix Lie algebras: B(X, Y) = 2n tr(XY) (for su(n)),
        or can be computed via the structure constants.
        """
        if self.structure_constants is not None:
            d = self.dim
            # B_{ij} = c^k_{im} c^m_{jk}
            # Simplified: trace of adX ∘ adY matrix
            pass
        # For matrix algebras
        n = X.shape[0]
        return float(2 * n * np.trace(X @ Y))

    def is_semisimple(self) -> bool:
        """Check semisimplicity via non-degeneracy of Killing form."""
        if self.basis is None or len(self.basis) == 0:
            return False
        d = len(self.basis)
        B = np.zeros((d, d))
        for i in range(d):
            for j in range(d):
                B[i, j] = self.killing_form(self.basis[i], self.basis[j])
        return abs(np.linalg.det(B)) > 1e-10

    def __repr__(self) -> str:
        return f"LieAlgebra({self.name}, dim={self.dim})"


@dataclass
class LieGroup:
    """
    A Lie group G: a smooth manifold that is also a group.

    Operations: multiplication, inversion, exponential map, logarithmic map.
    """
    name: str = "G"
    kind: LieGroupKind = LieGroupKind.CUSTOM
    matrix_dim: int = 0  # dimension of matrix representation
    lie_algebra: Optional[LieAlgebra] = None

    def identity(self) -> NDArray:
        """Return the identity element."""
        return np.eye(self.matrix_dim)

    def multiply(self, g: NDArray, h: NDArray) -> NDArray:
        """Group multiplication."""
        return g @ h

    def inverse(self, g: NDArray) -> NDArray:
        """Group inverse."""
        return np.linalg.inv(g)

    def exp(self, X: NDArray) -> NDArray:
        """Exponential map exp: 𝔤 → G."""
        from scipy.linalg import expm
        return expm(X)

    def log(self, g: NDArray) -> NDArray:
        """Logarithmic map log: G → 𝔤."""
        from scipy.linalg import logm
        return logm(g).real

    def adjoint_representation(self, g: NDArray, X: NDArray) -> NDArray:
        """Adjoint action Ad(g)X = gXg^{-1}."""
        return g @ X @ self.inverse(g)

    def left_translate(self, g: NDArray, h: NDArray) -> NDArray:
        """Left translation L_g(h) = gh."""
        return self.multiply(g, h)

    def right_translate(self, g: NDArray, h: NDArray) -> NDArray:
        """Right translation R_g(h) = hg."""
        return self.multiply(h, g)

    @staticmethod
    def SO(n: int) -> "LieGroup":
        """Special orthogonal group SO(n)."""
        # Basis for so(n): skew-symmetric matrices
        basis = []
        for i in range(n):
            for j in range(i + 1, n):
                B = np.zeros((n, n))
                B[i, j] = 1.0
                B[j, i] = -1.0
                basis.append(B)
        algebra = LieAlgebra(
            name=f"so({n})", dim=n * (n - 1) // 2, basis=basis
        )
        return LieGroup(
            name=f"SO({n})", kind=LieGroupKind.SO,
            matrix_dim=n, lie_algebra=algebra,
        )

    @staticmethod
    def SU(n: int) -> "LieGroup":
        """Special unitary group SU(n)."""
        return LieGroup(
            name=f"SU({n})", kind=LieGroupKind.SU,
            matrix_dim=n,
            lie_algebra=LieAlgebra(name=f"su({n})", dim=n * n - 1),
        )

    @staticmethod
    def GL(n: int) -> "LieGroup":
        """General linear group GL(n)."""
        return LieGroup(
            name=f"GL({n})", kind=LieGroupKind.GL,
            matrix_dim=n,
            lie_algebra=LieAlgebra(name=f"gl({n})", dim=n * n),
        )

    def __repr__(self) -> str:
        return f"LieGroup({self.name}, dim={self.matrix_dim})"


# ─── Levi-Civita Connection ─────────────────────────────────────────────────

class LeviCivitaConnection:
    """
    The unique torsion-free, metric-compatible connection on a Riemannian manifold.
    """

    @staticmethod
    def from_metric(metric_fn: Callable[[NDArray], NDArray]) -> Connection:
        """Construct the Levi-Civita connection from a metric."""
        return Connection(
            name="∇_LC",
            is_metric_compatible=True,
            is_torsion_free=True,
            christoffel_fn=lambda p: CurvatureTensors.christoffel(metric_fn, p),
        )

    @staticmethod
    def parallel_transport(
        metric_fn: Callable[[NDArray], NDArray],
        curve: NDArray,  # (n_steps, dim) curve on manifold
        v0: NDArray,     # initial tangent vector
        eps: float = 1e-6,
    ) -> NDArray:
        """
        Parallel transport of v0 along a curve using the Levi-Civita connection.

        Solves: dv^k/dt + Γ^k_{ij} (dx^i/dt) v^j = 0
        """
        n_steps = len(curve) - 1
        n = len(v0)
        v = v0.copy()

        for step in range(n_steps):
            x = curve[step]
            dx = curve[step + 1] - curve[step]
            gamma = CurvatureTensors.christoffel(metric_fn, x, eps)

            # dv^k = -Γ^k_{ij} dx^i v^j
            dv = np.zeros(n)
            for k in range(n):
                for i in range(n):
                    for j in range(n):
                        dv[k] -= gamma[k, i, j] * dx[i] * v[j]
            v = v + dv

        return v


# ─── Natural Gradient ───────────────────────────────────────────────────────

class NaturalGradient:
    """
    Natural gradient computation for optimization on statistical manifolds.

    The natural gradient is F^{-1} ∇L where F is the Fisher information matrix.
    This gives the steepest descent direction in the Riemannian geometry of
    the parameter space.
    """

    @staticmethod
    def fisher_information_matrix(
        log_prob_fn: Callable[[NDArray, NDArray], float],
        theta: NDArray,
        samples: NDArray,
        eps: float = 1e-5,
    ) -> NDArray:
        """
        Estimate the Fisher information matrix F(θ) via Monte Carlo:
        F_{ij} = E[∂log p/∂θ_i · ∂log p/∂θ_j]
        """
        n_params = len(theta)
        n_samples = len(samples)

        grad_log_p = np.zeros((n_samples, n_params))
        for s in range(n_samples):
            for i in range(n_params):
                theta_plus = theta.copy()
                theta_minus = theta.copy()
                theta_plus[i] += eps
                theta_minus[i] -= eps
                grad_log_p[s, i] = (
                    log_prob_fn(theta_plus, samples[s])
                    - log_prob_fn(theta_minus, samples[s])
                ) / (2 * eps)

        F = (grad_log_p.T @ grad_log_p) / n_samples
        return F

    @staticmethod
    def natural_gradient_step(
        loss_grad: NDArray,
        fisher_matrix: NDArray,
        damping: float = 1e-4,
    ) -> NDArray:
        """
        Compute natural gradient: F^{-1} ∇L.

        Uses Tikhonov regularization for numerical stability:
        (F + λI)^{-1} ∇L
        """
        n = fisher_matrix.shape[0]
        F_reg = fisher_matrix + damping * np.eye(n)
        return np.linalg.solve(F_reg, loss_grad)

    @staticmethod
    def natural_gradient_descent(
        loss_fn: Callable[[NDArray], float],
        grad_fn: Callable[[NDArray], NDArray],
        fisher_fn: Callable[[NDArray], NDArray],
        theta0: NDArray,
        learning_rate: float = 0.01,
        damping: float = 1e-4,
        max_iter: int = 1000,
        tol: float = 1e-8,
    ) -> tuple[NDArray, list[float]]:
        """
        Natural gradient descent optimization.

        Returns: (optimal_params, loss_history)
        """
        theta = theta0.copy()
        losses = [loss_fn(theta)]

        for _ in range(max_iter):
            grad = grad_fn(theta)
            F = fisher_fn(theta)
            nat_grad = NaturalGradient.natural_gradient_step(grad, F, damping)

            if np.linalg.norm(nat_grad) < tol:
                break

            theta = theta - learning_rate * nat_grad
            losses.append(loss_fn(theta))

        return theta, losses


# ─── Geometric Flows ────────────────────────────────────────────────────────

class GeometricFlowSolver:
    """
    Numerical solvers for geometric flows.
    """

    @staticmethod
    def ricci_flow_step(
        metric_fn: Callable[[NDArray], NDArray],
        point: NDArray,
        dt: float,
        eps: float = 1e-5,
    ) -> NDArray:
        """
        One step of the Ricci flow: ∂g/∂t = -2 Ric(g).

        Returns the evolved metric at the point.
        """
        g = metric_fn(point)
        Ric = CurvatureTensors.ricci_tensor(metric_fn, point, eps)
        return g - 2 * dt * Ric

    @staticmethod
    def normalized_ricci_flow_step(
        metric_fn: Callable[[NDArray], NDArray],
        point: NDArray,
        dt: float,
        eps: float = 1e-5,
    ) -> NDArray:
        """
        Normalized Ricci flow: ∂g/∂t = -2 Ric + (2r/n) g
        where r is the average scalar curvature.
        """
        n = len(point)
        g = metric_fn(point)
        Ric = CurvatureTensors.ricci_tensor(metric_fn, point, eps)
        R_scalar = float(np.trace(np.linalg.inv(g) @ Ric))
        r_avg = R_scalar / n
        return g - 2 * dt * Ric + (2 * r_avg / n) * dt * g

    @staticmethod
    def mean_curvature_flow_step(
        surface_points: NDArray,
        normals: NDArray,
        mean_curvatures: NDArray,
        dt: float,
    ) -> NDArray:
        """
        One step of mean curvature flow: dX/dt = H · n.

        Args:
            surface_points: (N, dim) array of surface points
            normals: (N, dim) array of unit normals
            mean_curvatures: (N,) array of mean curvatures

        Returns: evolved surface points
        """
        return surface_points + dt * mean_curvatures[:, np.newaxis] * normals

    @staticmethod
    def evolve_metric(
        metric_fn: Callable[[NDArray], NDArray],
        point: NDArray,
        flow_kind: GeometricFlowKind,
        t_total: float,
        n_steps: int = 100,
    ) -> list[NDArray]:
        """
        Evolve a metric under a geometric flow for time t_total.

        Returns list of metrics at each time step.
        """
        dt = t_total / n_steps
        metrics = [metric_fn(point)]

        current_metric_data = metrics[0].copy()

        for _ in range(n_steps):
            # Create a callable from current metric data for next step
            def current_metric_fn(p: NDArray, m: NDArray = current_metric_data) -> NDArray:
                return m.copy()

            if flow_kind == GeometricFlowKind.RICCI:
                new_metric = GeometricFlowSolver.ricci_flow_step(
                    current_metric_fn, point, dt
                )
            elif flow_kind == GeometricFlowKind.YAMABE:
                new_metric = GeometricFlowSolver.normalized_ricci_flow_step(
                    current_metric_fn, point, dt
                )
            else:
                new_metric = GeometricFlowSolver.ricci_flow_step(
                    current_metric_fn, point, dt
                )

            current_metric_data = new_metric
            metrics.append(new_metric.copy())

        return metrics
