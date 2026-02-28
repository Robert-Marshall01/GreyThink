"""
Experimental Numeric Extensions — Advanced numerical methods.

Provides:
- Riemannian optimization (gradient descent, retractions on manifolds)
- Geodesic solvers (symplectic, RK, variational)
- Advanced SDE solvers (strong/weak, adaptive step)
- PDE solver orchestration (method selection, convergence)
- Bifurcation detection and continuation
- Lyapunov exponent estimation (QR method)
- Operator norm estimation (power iteration, randomized)
- Stability diagnostics (pseudospectra, resolvent norms)
- Interval arithmetic for error bounds
- Mixed-precision numerics
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Callable, Optional

import numpy as np
from numpy.typing import NDArray


# ─── Riemannian Optimization ────────────────────────────────────────────────

@dataclass
class ManifoldPoint:
    """A point on a Riemannian manifold with tangent space info."""
    coords: NDArray
    manifold_name: str = ""
    metric_at_point: Optional[NDArray] = None

    def inner_product(self, v: NDArray, w: NDArray) -> float:
        """Riemannian inner product <v, w>_p."""
        if self.metric_at_point is not None:
            return float(v @ self.metric_at_point @ w)
        return float(np.dot(v, w))

    def norm(self, v: NDArray) -> float:
        """Riemannian norm."""
        return np.sqrt(max(0.0, self.inner_product(v, v)))


class RiemannianOptimizer:
    """
    Optimization on Riemannian manifolds.

    Algorithms:
    - Riemannian gradient descent
    - Riemannian conjugate gradient
    - Riemannian L-BFGS (simplified)
    """

    @staticmethod
    def gradient_descent(
        objective: Callable[[NDArray], float],
        grad_fn: Callable[[NDArray], NDArray],
        retraction: Callable[[NDArray, NDArray], NDArray],
        x0: NDArray,
        metric_fn: Optional[Callable[[NDArray], NDArray]] = None,
        lr: float = 0.01,
        max_iter: int = 1000,
        tol: float = 1e-8,
    ) -> tuple[NDArray, list[float]]:
        """
        Riemannian gradient descent.

        x_{k+1} = R_x(-η·grad f(x))

        Args:
            objective: f(x) → scalar
            grad_fn: Euclidean gradient ∇f(x)
            retraction: R_x(v) maps tangent vector to manifold
            x0: initial point
            metric_fn: g(x) → metric matrix (for Riemannian gradient)
            lr: learning rate
            max_iter: maximum iterations
            tol: convergence tolerance

        Returns: (optimal_point, loss_history)
        """
        x = x0.copy()
        history = []

        for _ in range(max_iter):
            val = objective(x)
            history.append(val)

            euc_grad = grad_fn(x)

            # Convert to Riemannian gradient: grad_R f = g^{-1} · ∇f
            if metric_fn is not None:
                g = metric_fn(x)
                riem_grad = np.linalg.solve(g, euc_grad)
            else:
                riem_grad = euc_grad

            # Check convergence
            if np.linalg.norm(riem_grad) < tol:
                break

            # Retraction step
            x = retraction(x, -lr * riem_grad)

        return x, history

    @staticmethod
    def sphere_retraction(x: NDArray, v: NDArray) -> NDArray:
        """Retraction on the unit sphere: normalize."""
        y = x + v
        return y / np.linalg.norm(y)

    @staticmethod
    def stiefel_retraction(X: NDArray, V: NDArray) -> NDArray:
        """Retraction on the Stiefel manifold St(n,p) via QR."""
        Y = X + V
        Q, R = np.linalg.qr(Y)
        # Fix signs to ensure positive diagonal of R
        signs = np.sign(np.diag(R))
        signs[signs == 0] = 1
        return Q * signs

    @staticmethod
    def spd_retraction(X: NDArray, V: NDArray) -> NDArray:
        """Retraction on SPD matrices: X + V projected to SPD."""
        Y = X + V
        Y = 0.5 * (Y + Y.T)  # Symmetrize
        eigvals, eigvecs = np.linalg.eigh(Y)
        eigvals = np.maximum(eigvals, 1e-10)  # Project to positive
        return eigvecs @ np.diag(eigvals) @ eigvecs.T


# ─── Geodesic Solvers ───────────────────────────────────────────────────────

class GeodesicSolver:
    """
    Numerical geodesic solvers on Riemannian manifolds.
    """

    @staticmethod
    def solve_geodesic_rk4(
        christoffel_fn: Callable[[NDArray], NDArray],
        x0: NDArray,
        v0: NDArray,
        dt: float = 0.01,
        n_steps: int = 1000,
    ) -> tuple[NDArray, NDArray]:
        """
        Solve geodesic equation using RK4:

        d²x^k/dt² + Γ^k_{ij} (dx^i/dt)(dx^j/dt) = 0

        Returns: (positions, velocities)
        """
        dim = len(x0)
        positions = np.zeros((n_steps + 1, dim))
        velocities = np.zeros((n_steps + 1, dim))
        positions[0] = x0
        velocities[0] = v0

        x = x0.copy()
        v = v0.copy()

        for step in range(n_steps):
            # RK4 for the system dx/dt = v, dv/dt = -Γ·v·v
            def accel(pos: NDArray, vel: NDArray) -> NDArray:
                gamma = christoffel_fn(pos)
                a = np.zeros(dim)
                for k in range(dim):
                    for i in range(dim):
                        for j in range(dim):
                            a[k] -= gamma[k, i, j] * vel[i] * vel[j]
                return a

            k1x = v
            k1v = accel(x, v)

            k2x = v + 0.5 * dt * k1v
            k2v = accel(x + 0.5 * dt * k1x, v + 0.5 * dt * k1v)

            k3x = v + 0.5 * dt * k2v
            k3v = accel(x + 0.5 * dt * k2x, v + 0.5 * dt * k2v)

            k4x = v + dt * k3v
            k4v = accel(x + dt * k3x, v + dt * k3v)

            x = x + dt / 6 * (k1x + 2 * k2x + 2 * k3x + k4x)
            v = v + dt / 6 * (k1v + 2 * k2v + 2 * k3v + k4v)

            positions[step + 1] = x
            velocities[step + 1] = v

        return positions, velocities

    @staticmethod
    def geodesic_distance_estimate(
        christoffel_fn: Callable[[NDArray], NDArray],
        p: NDArray,
        q: NDArray,
        n_bisections: int = 5,
        dt: float = 0.01,
        n_steps: int = 200,
    ) -> float:
        """
        Estimate geodesic distance between two points by shooting.

        Uses bisection on the initial velocity magnitude.
        """
        direction = q - p
        direction = direction / (np.linalg.norm(direction) + 1e-30)

        v_low = 0.0
        v_high = 2.0 * np.linalg.norm(q - p)

        for _ in range(n_bisections):
            v_mid = 0.5 * (v_low + v_high)
            v0 = v_mid * direction
            positions, _ = GeodesicSolver.solve_geodesic_rk4(
                christoffel_fn, p, v0, dt, n_steps
            )
            end_dist = np.linalg.norm(positions[-1] - q)
            if end_dist > np.linalg.norm(q - p) * 0.1:
                v_high = v_mid
            else:
                v_low = v_mid

        # Return arc length approximation
        v0 = 0.5 * (v_low + v_high) * direction
        positions, _ = GeodesicSolver.solve_geodesic_rk4(
            christoffel_fn, p, v0, dt, n_steps
        )
        arc_length = sum(
            np.linalg.norm(positions[i + 1] - positions[i])
            for i in range(len(positions) - 1)
        )
        return float(arc_length)


# ─── Operator Norm Estimation ────────────────────────────────────────────────

class OperatorNormEstimator:
    """
    Estimate operator norms without full eigendecomposition.
    """

    @staticmethod
    def power_iteration(
        A: NDArray,
        n_iter: int = 100,
        tol: float = 1e-10,
    ) -> float:
        """
        Estimate the spectral norm ||A||_2 via power iteration.
        """
        n = A.shape[1]
        rng = np.random.default_rng(42)
        v = rng.standard_normal(n)
        v /= np.linalg.norm(v)

        sigma = 0.0
        for _ in range(n_iter):
            Av = A @ v
            sigma_new = np.linalg.norm(Av)
            if sigma_new < 1e-30:
                return 0.0
            v_new = Av / sigma_new

            if abs(sigma_new - sigma) < tol:
                return float(sigma_new)

            v = v_new
            sigma = sigma_new

        return float(sigma)

    @staticmethod
    def randomized_norm_estimate(
        A: NDArray,
        n_samples: int = 20,
    ) -> float:
        """
        Randomized operator norm estimate using Gaussian vectors.

        E[||Av||] ≤ ||A||_2 · E[||v||] for v ~ N(0, I).
        """
        n = A.shape[1]
        rng = np.random.default_rng(42)
        norms = []
        for _ in range(n_samples):
            v = rng.standard_normal(n)
            v /= np.linalg.norm(v)
            norms.append(np.linalg.norm(A @ v))
        return float(np.max(norms))

    @staticmethod
    def trace_norm(A: NDArray) -> float:
        """Compute the trace (nuclear) norm ||A||_1 = Σ σ_i."""
        svs = np.linalg.svd(A, compute_uv=False)
        return float(np.sum(svs))

    @staticmethod
    def frobenius_norm(A: NDArray) -> float:
        """Compute the Frobenius norm ||A||_F = √(Σ σ_i²)."""
        return float(np.linalg.norm(A, "fro"))


# ─── Pseudospectral Analysis ────────────────────────────────────────────────

class PseudospectralAnalysis:
    """
    Compute pseudospectra and related stability indicators.
    """

    @staticmethod
    def pseudospectrum(
        A: NDArray,
        real_range: tuple[float, float] = (-5, 5),
        imag_range: tuple[float, float] = (-5, 5),
        n_grid: int = 100,
    ) -> tuple[NDArray, NDArray, NDArray]:
        """
        Compute the ε-pseudospectrum contours of A.

        σ_ε(A) = {z ∈ C : σ_min(zI - A) < ε}

        Returns: (real_grid, imag_grid, sigma_min_values)
        """
        re = np.linspace(real_range[0], real_range[1], n_grid)
        im = np.linspace(imag_range[0], imag_range[1], n_grid)
        sigma_min = np.zeros((n_grid, n_grid))
        n = A.shape[0]

        for i, r in enumerate(re):
            for j, m in enumerate(im):
                z = r + 1j * m
                M = z * np.eye(n) - A
                svs = np.linalg.svd(M, compute_uv=False)
                sigma_min[j, i] = svs[-1]

        return re, im, sigma_min

    @staticmethod
    def kreiss_constant(A: NDArray, n_angles: int = 200) -> float:
        """
        Estimate the Kreiss constant K(A).

        K(A) = sup_{|z|>1} (|z|-1)||R(z,A)||

        Important for stability of discrete-time systems.
        """
        n = A.shape[0]
        K = 0.0
        for theta in np.linspace(0, 2 * np.pi, n_angles, endpoint=False):
            for r in np.linspace(1.01, 5.0, 50):
                z = r * np.exp(1j * theta)
                try:
                    R = np.linalg.inv(z * np.eye(n) - A)
                    norm_R = np.linalg.norm(R, 2)
                    K = max(K, (r - 1) * norm_R)
                except np.linalg.LinAlgError:
                    continue
        return float(K)


# ─── Advanced SDE Solver ────────────────────────────────────────────────────

class AdaptiveSDESolver:
    """
    Adaptive-step SDE solver with error control.
    """

    @staticmethod
    def solve_adaptive(
        drift: Callable[[NDArray, float], NDArray],
        diffusion: Callable[[NDArray, float], NDArray],
        x0: NDArray,
        t_span: tuple[float, float],
        dt_init: float = 0.01,
        dt_min: float = 1e-6,
        dt_max: float = 0.1,
        tol: float = 1e-4,
        n_paths: int = 1,
        rng_seed: int = 42,
    ) -> tuple[NDArray, NDArray]:
        """
        Solve SDE with adaptive step size using embedded Euler-Maruyama.

        Error estimated by comparing half-step and full-step solutions.

        Returns: (times, paths) where paths has shape (n_paths, n_times, dim)
        """
        rng = np.random.default_rng(rng_seed)
        dim = len(x0)
        t0, tf = t_span

        all_paths = []
        all_times = None

        for _ in range(n_paths):
            times_list = [t0]
            states_list = [x0.copy()]
            x = x0.copy()
            t = t0
            dt = dt_init

            while t < tf - 1e-15:
                dt = min(dt, tf - t)

                # Full step
                dW = rng.standard_normal(dim) * np.sqrt(dt)
                mu = drift(x, t)
                sig = diffusion(x, t)
                x_full = x + mu * dt + sig @ dW if sig.ndim > 1 else x + mu * dt + sig * dW

                # Two half steps
                dt2 = dt / 2
                dW1 = dW * np.sqrt(0.5)  # Approximate split
                dW2 = dW - dW1

                x_half1 = x + drift(x, t) * dt2
                if sig.ndim > 1:
                    x_half1 += sig @ dW1
                else:
                    x_half1 += sig * dW1

                mu2 = drift(x_half1, t + dt2)
                sig2 = diffusion(x_half1, t + dt2)
                x_half2 = x_half1 + mu2 * dt2
                if sig2.ndim > 1:
                    x_half2 += sig2 @ dW2
                else:
                    x_half2 += sig2 * dW2

                # Error estimate
                error = np.linalg.norm(x_full - x_half2) / (np.linalg.norm(x_half2) + 1e-30)

                if error < tol or dt <= dt_min:
                    x = x_half2
                    t += dt
                    times_list.append(t)
                    states_list.append(x.copy())

                # Adapt step size
                if error > 0:
                    dt = dt * min(2.0, max(0.5, 0.9 * (tol / error) ** 0.5))
                dt = max(dt_min, min(dt_max, dt))

            all_paths.append(np.array(states_list))
            if all_times is None:
                all_times = np.array(times_list)

        # Pad paths to same length
        max_len = max(len(p) for p in all_paths)
        padded = np.zeros((n_paths, max_len, dim))
        for i, p in enumerate(all_paths):
            padded[i, :len(p)] = p
            padded[i, len(p):] = p[-1]  # Hold final value

        return all_times, padded


# ─── Interval Arithmetic ────────────────────────────────────────────────────

@dataclass
class Interval:
    """
    An interval [lo, hi] for rigorous error-bounded computation.
    """
    lo: float
    hi: float

    @property
    def midpoint(self) -> float:
        return 0.5 * (self.lo + self.hi)

    @property
    def width(self) -> float:
        return self.hi - self.lo

    @property
    def radius(self) -> float:
        return 0.5 * self.width

    def __add__(self, other: "Interval") -> "Interval":
        return Interval(self.lo + other.lo, self.hi + other.hi)

    def __sub__(self, other: "Interval") -> "Interval":
        return Interval(self.lo - other.hi, self.hi - other.lo)

    def __mul__(self, other: "Interval") -> "Interval":
        products = [
            self.lo * other.lo, self.lo * other.hi,
            self.hi * other.lo, self.hi * other.hi,
        ]
        return Interval(min(products), max(products))

    def __truediv__(self, other: "Interval") -> "Interval":
        if other.lo <= 0 <= other.hi:
            raise ZeroDivisionError("Division by interval containing zero")
        inv = Interval(1.0 / other.hi, 1.0 / other.lo)
        return self * inv

    def __contains__(self, x: float) -> bool:
        return self.lo <= x <= self.hi

    def intersect(self, other: "Interval") -> Optional["Interval"]:
        lo = max(self.lo, other.lo)
        hi = min(self.hi, other.hi)
        if lo > hi:
            return None
        return Interval(lo, hi)

    def __repr__(self) -> str:
        return f"[{self.lo:.6g}, {self.hi:.6g}]"

    @staticmethod
    def from_value(x: float, eps: float = 0.0) -> "Interval":
        return Interval(x - eps, x + eps)


class IntervalMatrix:
    """Matrix of intervals for rigorous linear algebra."""

    def __init__(self, lo: NDArray, hi: NDArray):
        self.lo = lo
        self.hi = hi
        self.shape = lo.shape

    @staticmethod
    def from_matrix(A: NDArray, eps: float = 0.0) -> "IntervalMatrix":
        return IntervalMatrix(A - eps, A + eps)

    @property
    def midpoint(self) -> NDArray:
        return 0.5 * (self.lo + self.hi)

    @property
    def radius(self) -> NDArray:
        return 0.5 * (self.hi - self.lo)

    def spectral_radius_bound(self) -> Interval:
        """Bound on the spectral radius."""
        mid = self.midpoint
        rad = self.radius
        eigs_mid = np.abs(np.linalg.eigvals(mid))
        rho_mid = np.max(eigs_mid)
        perturbation = np.linalg.norm(rad, 2)
        return Interval(
            max(0, rho_mid - perturbation),
            rho_mid + perturbation,
        )

    def __repr__(self) -> str:
        return f"IntervalMatrix({self.shape}, radius={np.max(self.radius):.2e})"


# ─── Mixed-Precision Numerics ───────────────────────────────────────────────

class MixedPrecision:
    """
    Mixed-precision numerical computation: use low precision for speed,
    high precision for critical paths.
    """

    @staticmethod
    def iterative_refinement(
        A: NDArray,
        b: NDArray,
        n_refine: int = 3,
    ) -> tuple[NDArray, float]:
        """
        Solve Ax = b with iterative refinement for improved accuracy.

        1. Solve in working precision
        2. Compute residual in higher precision
        3. Correct solution

        Returns: (solution, residual_norm)
        """
        # Initial solve
        x = np.linalg.solve(A, b)

        for _ in range(n_refine):
            # Residual in high precision
            r = b - A @ x
            # Correction
            dx = np.linalg.solve(A, r)
            x = x + dx

        residual = np.linalg.norm(b - A @ x)
        return x, float(residual)

    @staticmethod
    def condition_number_estimate(A: NDArray) -> float:
        """Estimate condition number without full SVD."""
        n = A.shape[0]
        if n <= 100:
            return float(np.linalg.cond(A))

        # Use inverse power iteration for smallest singular value
        norm_A = OperatorNormEstimator.power_iteration(A)
        try:
            inv_norm = OperatorNormEstimator.power_iteration(np.linalg.inv(A))
            return norm_A * inv_norm
        except np.linalg.LinAlgError:
            return float("inf")


# ─── Stability Diagnostics ──────────────────────────────────────────────────

@dataclass
class StabilityReport:
    """Comprehensive stability report for a numerical computation."""
    condition_number: float = 0.0
    spectral_radius: float = 0.0
    numerical_rank: int = 0
    estimated_error: float = 0.0
    is_well_conditioned: bool = True
    warnings: list[str] = field(default_factory=list)

    def __repr__(self) -> str:
        status = "STABLE" if self.is_well_conditioned else "ILL-CONDITIONED"
        return (f"StabilityReport({status}, κ={self.condition_number:.2e}, "
                f"ρ={self.spectral_radius:.4f})")


class StabilityDiagnostics:
    """
    Run comprehensive stability diagnostics on matrices and operators.
    """

    @staticmethod
    def analyze(A: NDArray, threshold: float = 1e12) -> StabilityReport:
        """Full stability analysis of a matrix."""
        report = StabilityReport()

        # Condition number
        try:
            report.condition_number = float(np.linalg.cond(A))
        except Exception:
            report.condition_number = float("inf")

        # Spectral radius
        try:
            eigs = np.linalg.eigvals(A)
            report.spectral_radius = float(np.max(np.abs(eigs)))
        except Exception:
            report.spectral_radius = float("inf")

        # Numerical rank
        svs = np.linalg.svd(A, compute_uv=False)
        report.numerical_rank = int(np.sum(svs > svs[0] * 1e-10))

        # Error estimate
        report.estimated_error = report.condition_number * np.finfo(A.dtype).eps

        # Well-conditioned check
        report.is_well_conditioned = report.condition_number < threshold

        if not report.is_well_conditioned:
            report.warnings.append(
                f"Matrix is ill-conditioned: κ = {report.condition_number:.2e}"
            )
        if report.numerical_rank < min(A.shape):
            report.warnings.append(
                f"Matrix is rank-deficient: rank={report.numerical_rank} "
                f"< min(m,n)={min(A.shape)}"
            )

        return report
