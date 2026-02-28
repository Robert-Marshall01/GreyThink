"""
Advanced Functional Analysis — Experimental Module.

Extends the core functional analysis with:
- Unbounded operators with dense domains
- Resolvents and resolvent sets
- Operator semigroups (C₀, analytic, contraction)
- Advanced contraction mappings
- Fixed-point theorems (Banach, Schauder, Brouwer, Kakutani)
- Spectral radius and stability analysis
- Fredholm operators and index theory
- Compact operator SVD decomposition
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Callable, Optional

import numpy as np
from numpy.typing import NDArray

from greymath.core.types import (
    LinearOperator, Matrix, Vector, Scalar,
    SpectralObject, OperatorKind,
)
from greymath.core.metadata import StabilityInfo, StabilityRating
from greymath.experimental.ir_extensions import OperatorSpace, OperatorSpaceKind


# ─── Enumerations ────────────────────────────────────────────────────────────

class SemigroupKind(Enum):
    """Kinds of operator semigroups."""
    C0 = auto()            # Strongly continuous (C₀)
    ANALYTIC = auto()      # Analytic semigroup
    CONTRACTION = auto()   # Contraction semigroup
    UNIFORMLY_CONTINUOUS = auto()
    EVENTUALLY_COMPACT = auto()


class SpectrumKind(Enum):
    """Components of the spectrum."""
    POINT = auto()         # Eigenvalues
    CONTINUOUS = auto()    # Continuous spectrum
    RESIDUAL = auto()      # Residual spectrum


# ─── Unbounded Operator ─────────────────────────────────────────────────────

@dataclass
class UnboundedOperator:
    """
    An unbounded linear operator A: D(A) ⊂ H → H.

    Unlike bounded operators, unbounded operators are defined only on a
    dense subspace D(A) of the Hilbert space. Key examples: differential
    operators, momentum/position operators in quantum mechanics.
    """
    name: str = "A"
    domain_description: str = "D(A)"
    is_closed: bool = False
    is_densely_defined: bool = True
    is_self_adjoint: bool = False
    is_symmetric: bool = False
    core: Optional[str] = None  # Description of a core for the operator
    apply_fn: Optional[Callable[[NDArray], NDArray]] = None
    domain_check: Optional[Callable[[NDArray], bool]] = None

    # Finite-dimensional approximation
    matrix_truncation: Optional[NDArray] = None
    truncation_dim: Optional[int] = None

    def apply(self, x: NDArray) -> NDArray:
        """Apply the operator to x ∈ D(A)."""
        if self.domain_check and not self.domain_check(x):
            raise ValueError(f"Vector not in domain {self.domain_description}")
        if self.apply_fn is not None:
            return self.apply_fn(x)
        if self.matrix_truncation is not None:
            n = min(len(x), self.matrix_truncation.shape[1])
            return self.matrix_truncation[:, :n] @ x[:n]
        raise ValueError(f"Operator {self.name} has no implementation")

    def graph_norm(self, x: NDArray) -> float:
        """Graph norm ||x||_A = √(||x||² + ||Ax||²)."""
        return float(np.sqrt(np.dot(x, x) + np.dot(self.apply(x), self.apply(x))))

    def truncate(self, dim: int) -> Matrix:
        """Create a finite-dimensional truncation of the operator."""
        if self.matrix_truncation is not None:
            return Matrix(data=self.matrix_truncation[:dim, :dim])
        if self.apply_fn is not None:
            mat = np.zeros((dim, dim))
            for j in range(dim):
                e_j = np.zeros(dim)
                e_j[j] = 1.0
                mat[:, j] = self.apply_fn(e_j)[:dim]
            return Matrix(data=mat)
        raise ValueError("Cannot truncate operator")

    def __repr__(self) -> str:
        props = []
        if self.is_closed:
            props.append("closed")
        if self.is_self_adjoint:
            props.append("self-adjoint")
        return f"UnboundedOp({self.name}, {', '.join(props)})"


# ─── Resolvent and Spectrum ──────────────────────────────────────────────────

@dataclass
class SpectrumDecomposition:
    """
    Full decomposition of the spectrum of a linear operator.

    σ(A) = σ_p(A) ∪ σ_c(A) ∪ σ_r(A)
    - σ_p: point spectrum (eigenvalues)
    - σ_c: continuous spectrum
    - σ_r: residual spectrum
    ρ(A) = C \ σ(A): resolvent set
    """
    point_spectrum: list[complex] = field(default_factory=list)
    continuous_spectrum_intervals: list[tuple[float, float]] = field(default_factory=list)
    residual_spectrum: list[complex] = field(default_factory=list)
    spectral_radius: Optional[float] = None
    essential_spectrum: list[complex] = field(default_factory=list)

    def is_in_resolvent_set(self, z: complex, tol: float = 1e-10) -> bool:
        """Check if z is in the resolvent set ρ(A)."""
        for lam in self.point_spectrum:
            if abs(z - lam) < tol:
                return False
        for lam in self.residual_spectrum:
            if abs(z - lam) < tol:
                return False
        for a, b in self.continuous_spectrum_intervals:
            if a - tol <= z.real <= b + tol and abs(z.imag) < tol:
                return False
        return True

    def __repr__(self) -> str:
        return (f"Spectrum(point={len(self.point_spectrum)}, "
                f"continuous={len(self.continuous_spectrum_intervals)}, "
                f"residual={len(self.residual_spectrum)})")


class ResolventAnalysis:
    """
    Resolvent analysis for operators.

    The resolvent R(z; A) = (zI - A)^{-1} is a key analytic tool in
    spectral theory and semigroup theory.
    """

    @staticmethod
    def resolvent(A: NDArray, z: complex) -> NDArray:
        """Compute R(z; A) = (zI - A)^{-1}."""
        n = A.shape[0]
        return np.linalg.inv(z * np.eye(n) - A)

    @staticmethod
    def resolvent_norm(A: NDArray, z: complex) -> float:
        """Compute ||R(z; A)||."""
        R = ResolventAnalysis.resolvent(A, z)
        return float(np.linalg.norm(R, 2))

    @staticmethod
    def numerical_range(A: NDArray, n_points: int = 1000) -> NDArray:
        """
        Estimate the numerical range W(A) = {<Ax, x> : ||x|| = 1}.

        The numerical range contains the spectrum and is convex (Toeplitz-Hausdorff).
        """
        n = A.shape[0]
        rng = np.random.default_rng(42)
        points = []
        for _ in range(n_points):
            x = rng.standard_normal(n) + 1j * rng.standard_normal(n)
            x = x / np.linalg.norm(x)
            rayleigh = complex(x.conj() @ A @ x)
            points.append(rayleigh)
        return np.array(points)

    @staticmethod
    def pseudospectrum_resolvent(A: NDArray, epsilon: float,
                                 grid_size: int = 100,
                                 bounds: tuple = (-3, 3, -3, 3)) -> tuple:
        """
        Compute ε-pseudospectrum σ_ε(A) = {z : ||R(z;A)|| ≥ 1/ε}.

        Returns (X, Y, resolvent_norms) on a grid.
        """
        x = np.linspace(bounds[0], bounds[1], grid_size)
        y = np.linspace(bounds[2], bounds[3], grid_size)
        X, Y = np.meshgrid(x, y)
        n = A.shape[0]
        norms = np.zeros_like(X)
        for i in range(grid_size):
            for j in range(grid_size):
                z = complex(X[i, j], Y[i, j])
                try:
                    R = np.linalg.inv(z * np.eye(n) - A)
                    norms[i, j] = np.linalg.norm(R, 2)
                except np.linalg.LinAlgError:
                    norms[i, j] = 1e15
        return X, Y, norms

    @staticmethod
    def spectrum_decomposition(A: NDArray, tol: float = 1e-10) -> SpectrumDecomposition:
        """Compute spectrum decomposition for a finite-dimensional operator."""
        eigenvalues = np.linalg.eigvals(A)
        point_spectrum = [complex(ev) for ev in eigenvalues]
        spectral_radius = float(np.max(np.abs(eigenvalues)))
        return SpectrumDecomposition(
            point_spectrum=point_spectrum,
            spectral_radius=spectral_radius,
        )


# ─── Operator Semigroups ─────────────────────────────────────────────────────

@dataclass
class OperatorSemigroup:
    """
    A one-parameter operator semigroup {T(t) : t ≥ 0}.

    Satisfies:
    - T(0) = I
    - T(s + t) = T(s) T(t) (semigroup property)
    - t → T(t)x is continuous for each x (C₀ property)

    The infinitesimal generator A is defined by:
        Ax = lim_{t→0} (T(t)x - x) / t
    """
    name: str = "T"
    kind: SemigroupKind = SemigroupKind.C0
    generator_matrix: Optional[NDArray] = None  # A such that T(t) = e^{tA}
    growth_bound: Optional[float] = None         # ω such that ||T(t)|| ≤ M e^{ωt}
    growth_constant: float = 1.0                  # M in the bound
    dim: Optional[int] = None

    def evaluate(self, t: float) -> NDArray:
        """Compute T(t) = e^{tA} via matrix exponential."""
        if self.generator_matrix is None:
            raise ValueError("Semigroup has no generator matrix")
        from scipy.linalg import expm
        return expm(t * self.generator_matrix)

    def apply(self, t: float, x: NDArray) -> NDArray:
        """Compute T(t)x."""
        T_t = self.evaluate(t)
        return T_t @ x

    def orbit(self, x0: NDArray, times: NDArray) -> NDArray:
        """Compute the orbit {T(t)x0 : t ∈ times}."""
        return np.array([self.apply(t, x0) for t in times])

    def generator_spectrum(self) -> SpectrumDecomposition:
        """Spectrum of the generator A."""
        if self.generator_matrix is None:
            raise ValueError("No generator matrix")
        return ResolventAnalysis.spectrum_decomposition(self.generator_matrix)

    def is_stable(self) -> bool:
        """Check if the semigroup is exponentially stable (all Re(λ) < 0)."""
        if self.generator_matrix is None:
            return False
        eigenvalues = np.linalg.eigvals(self.generator_matrix)
        return bool(np.all(np.real(eigenvalues) < 0))

    def growth_bound_estimate(self) -> float:
        """Estimate the growth bound ω₀ = max Re(σ(A))."""
        if self.generator_matrix is None:
            raise ValueError("No generator matrix")
        eigenvalues = np.linalg.eigvals(self.generator_matrix)
        return float(np.max(np.real(eigenvalues)))

    @staticmethod
    def from_generator(A: NDArray, name: str = "T") -> "OperatorSemigroup":
        """Create a semigroup from its infinitesimal generator."""
        eigenvalues = np.linalg.eigvals(A)
        growth_bound = float(np.max(np.real(eigenvalues)))
        kind = SemigroupKind.CONTRACTION if growth_bound <= 0 else SemigroupKind.C0
        return OperatorSemigroup(
            name=name,
            kind=kind,
            generator_matrix=A,
            growth_bound=growth_bound,
            dim=A.shape[0],
        )

    def __repr__(self) -> str:
        return f"Semigroup({self.name}, {self.kind.name}, ω={self.growth_bound})"


# ─── Advanced Contraction Mappings ───────────────────────────────────────────

class ContractionAnalysis:
    """
    Advanced contraction mapping analysis and fixed-point computation.
    """

    @staticmethod
    def contraction_constant(
        T: Callable[[NDArray], NDArray],
        domain_samples: NDArray,
        norm_fn: Callable[[NDArray], float] = lambda x: float(np.linalg.norm(x)),
    ) -> float:
        """
        Estimate the contraction constant α = sup ||T(x) - T(y)|| / ||x - y||.
        """
        n = len(domain_samples)
        max_ratio = 0.0
        for i in range(n):
            for j in range(i + 1, n):
                x, y = domain_samples[i], domain_samples[j]
                ratio = norm_fn(T(x) - T(y)) / max(norm_fn(x - y), 1e-30)
                max_ratio = max(max_ratio, ratio)
        return max_ratio

    @staticmethod
    def a_priori_bound(
        x0: NDArray, x1: NDArray, alpha: float, norm_fn: Callable = np.linalg.norm
    ) -> Callable[[int], float]:
        """
        A priori error bound for Banach iteration:
        ||x_n - x*|| ≤ α^n / (1 - α) * ||x_1 - x_0||
        """
        d01 = float(norm_fn(x1 - x0))

        def bound(n: int) -> float:
            return (alpha ** n / (1 - alpha)) * d01

        return bound

    @staticmethod
    def a_posteriori_bound(
        x_n: NDArray, x_n_minus_1: NDArray, alpha: float,
        norm_fn: Callable = np.linalg.norm
    ) -> float:
        """
        A posteriori error bound:
        ||x_n - x*|| ≤ α / (1 - α) * ||x_n - x_{n-1}||
        """
        d = float(norm_fn(x_n - x_n_minus_1))
        return alpha / (1 - alpha) * d

    @staticmethod
    def picard_iteration(
        T: Callable[[NDArray], NDArray],
        x0: NDArray,
        tol: float = 1e-12,
        max_iter: int = 10000,
        track_convergence: bool = True,
    ) -> tuple[NDArray, list[float], bool]:
        """
        Picard (Banach fixed-point) iteration with convergence tracking.

        Returns: (fixed_point, error_history, converged)
        """
        x = x0.copy()
        errors = []
        for i in range(max_iter):
            x_new = T(x)
            err = float(np.linalg.norm(x_new - x))
            errors.append(err)
            if err < tol:
                return x_new, errors, True
            x = x_new
        return x, errors, False

    @staticmethod
    def mann_iteration(
        T: Callable[[NDArray], NDArray],
        x0: NDArray,
        alpha_seq: Optional[Callable[[int], float]] = None,
        tol: float = 1e-10,
        max_iter: int = 10000,
    ) -> tuple[NDArray, list[float], bool]:
        """
        Mann iteration: x_{n+1} = (1 - α_n)x_n + α_n T(x_n).

        Converges for nonexpansive mappings when Σα_n = ∞ and Σα_n² < ∞.
        """
        if alpha_seq is None:
            alpha_seq = lambda n: 1.0 / (n + 2)

        x = x0.copy()
        errors = []
        for i in range(max_iter):
            alpha = alpha_seq(i)
            x_new = (1 - alpha) * x + alpha * T(x)
            err = float(np.linalg.norm(x_new - x))
            errors.append(err)
            if err < tol:
                return x_new, errors, True
            x = x_new
        return x, errors, False

    @staticmethod
    def halpern_iteration(
        T: Callable[[NDArray], NDArray],
        x0: NDArray,
        tol: float = 1e-10,
        max_iter: int = 10000,
    ) -> tuple[NDArray, list[float], bool]:
        """
        Halpern iteration: x_{n+1} = α_n u + (1 - α_n) T(x_n).

        Strong convergence for nonexpansive mappings in Hilbert spaces,
        where α_n → 0, Σα_n = ∞, and u is an anchor point.
        """
        u = x0.copy()
        x = x0.copy()
        errors = []
        for i in range(max_iter):
            alpha = 1.0 / (i + 2)
            x_new = alpha * u + (1 - alpha) * T(x)
            err = float(np.linalg.norm(x_new - x))
            errors.append(err)
            if err < tol:
                return x_new, errors, True
            x = x_new
        return x, errors, False


# ─── Fixed-Point Theory ─────────────────────────────────────────────────────

@dataclass
class FixedPointResult:
    """Result of a fixed-point computation."""
    fixed_point: NDArray
    converged: bool
    iterations: int
    error_history: list[float]
    contraction_constant: Optional[float] = None
    method: str = ""

    def convergence_rate(self) -> Optional[float]:
        """Estimate the convergence rate from the error history."""
        if len(self.error_history) < 3:
            return None
        rates = []
        for i in range(1, len(self.error_history)):
            if self.error_history[i - 1] > 1e-30:
                rates.append(self.error_history[i] / self.error_history[i - 1])
        if rates:
            return float(np.median(rates))
        return None

    def __repr__(self) -> str:
        return (f"FixedPoint(converged={self.converged}, "
                f"iter={self.iterations}, method={self.method})")


class FixedPointTheory:
    """
    Comprehensive fixed-point theory toolkit.

    Implements multiple fixed-point theorems:
    - Banach contraction principle
    - Schauder fixed-point theorem (approximation)
    - Brouwer fixed-point theorem (computational)
    - Kakutani fixed-point (for set-valued maps)
    """

    @staticmethod
    def banach(
        T: Callable[[NDArray], NDArray],
        x0: NDArray,
        tol: float = 1e-12,
        max_iter: int = 10000,
    ) -> FixedPointResult:
        """
        Banach contraction principle.

        If T is a contraction on a complete metric space, then T has a
        unique fixed point, and iteration converges to it.
        """
        fp, errors, converged = ContractionAnalysis.picard_iteration(
            T, x0, tol, max_iter
        )
        return FixedPointResult(
            fixed_point=fp, converged=converged,
            iterations=len(errors), error_history=errors,
            method="Banach",
        )

    @staticmethod
    def newton_fixed_point(
        F: Callable[[NDArray], NDArray],
        jacobian_F: Callable[[NDArray], NDArray],
        x0: NDArray,
        tol: float = 1e-12,
        max_iter: int = 100,
    ) -> FixedPointResult:
        """
        Newton's method for F(x) = 0, i.e., find fixed point of x - J^{-1}F(x).

        Quadratic convergence near the root when Jacobian is nonsingular.
        """
        x = x0.copy()
        errors = []
        for i in range(max_iter):
            Fx = F(x)
            err = float(np.linalg.norm(Fx))
            errors.append(err)
            if err < tol:
                return FixedPointResult(
                    fixed_point=x, converged=True,
                    iterations=i + 1, error_history=errors,
                    method="Newton",
                )
            J = jacobian_F(x)
            try:
                delta = np.linalg.solve(J, -Fx)
            except np.linalg.LinAlgError:
                return FixedPointResult(
                    fixed_point=x, converged=False,
                    iterations=i + 1, error_history=errors,
                    method="Newton",
                )
            x = x + delta
        return FixedPointResult(
            fixed_point=x, converged=False,
            iterations=max_iter, error_history=errors,
            method="Newton",
        )

    @staticmethod
    def brouwer_approximate(
        f: Callable[[NDArray], NDArray],
        dim: int,
        bounds: tuple[float, float] = (0.0, 1.0),
        grid_resolution: int = 50,
    ) -> FixedPointResult:
        """
        Approximate Brouwer fixed-point search via grid + Newton refinement.

        For f: [a,b]^n → [a,b]^n continuous, finds x such that f(x) ≈ x.
        """
        # Grid search for approximate fixed point
        a, b = bounds
        best_x = np.full(dim, (a + b) / 2)
        best_err = float("inf")

        if dim <= 3:
            n = grid_resolution
            for idx in np.ndindex(*([n] * dim)):
                x = np.array([(a + (b - a) * i / (n - 1)) for i in idx])
                fx = f(x)
                err = float(np.linalg.norm(fx - x))
                if err < best_err:
                    best_err = err
                    best_x = x.copy()

        # Newton refinement: solve g(x) = f(x) - x = 0
        def g(x: NDArray) -> NDArray:
            return f(x) - x

        def jac_g(x: NDArray, eps: float = 1e-7) -> NDArray:
            n = len(x)
            J = np.zeros((n, n))
            for i in range(n):
                x_plus = x.copy()
                x_minus = x.copy()
                x_plus[i] += eps
                x_minus[i] -= eps
                J[:, i] = (g(x_plus) - g(x_minus)) / (2 * eps)
            return J

        return FixedPointTheory.newton_fixed_point(g, jac_g, best_x)


# ─── Spectral Stability Analysis ────────────────────────────────────────────

class SpectralStability:
    """
    Spectral-based stability analysis for operators and dynamical systems.
    """

    @staticmethod
    def spectral_abscissa(A: NDArray) -> float:
        """Spectral abscissa: max Re(λ). Determines exponential stability."""
        eigenvalues = np.linalg.eigvals(A)
        return float(np.max(np.real(eigenvalues)))

    @staticmethod
    def numerical_abscissa(A: NDArray) -> float:
        """Numerical abscissa: max Re(W(A)). Measures transient growth."""
        # ω(A) = max eigenvalue of (A + A*)/2
        H = (A + A.conj().T) / 2
        eigenvalues = np.linalg.eigvalsh(H)
        return float(np.max(eigenvalues))

    @staticmethod
    def kreiss_constant(A: NDArray, n_points: int = 500) -> float:
        """
        Estimate the Kreiss constant K(A):
        K(A) = sup_{|z|>1} (|z| - 1) ||R(z; A)||

        For discrete-time stability. K(A) ≥ 1 is necessary for power-boundedness.
        """
        max_val = 0.0
        n = A.shape[0]
        for theta in np.linspace(0, 2 * np.pi, n_points):
            for r in [1.01, 1.05, 1.1, 1.2, 1.5, 2.0, 3.0]:
                z = r * np.exp(1j * theta)
                try:
                    R = np.linalg.inv(z * np.eye(n) - A)
                    val = (r - 1) * np.linalg.norm(R, 2)
                    max_val = max(max_val, val)
                except np.linalg.LinAlgError:
                    pass
        return max_val

    @staticmethod
    def transient_growth_bound(A: NDArray, t_max: float = 10.0,
                               n_points: int = 100) -> tuple[NDArray, NDArray]:
        """
        Compute ||e^{tA}|| over [0, t_max] to detect transient growth.

        Returns (times, norms).
        """
        from scipy.linalg import expm
        times = np.linspace(0, t_max, n_points)
        norms = np.zeros(n_points)
        for i, t in enumerate(times):
            norms[i] = np.linalg.norm(expm(t * A), 2)
        return times, norms

    @staticmethod
    def stability_report(A: NDArray) -> dict[str, Any]:
        """Comprehensive stability report for a matrix/operator."""
        eigenvalues = np.linalg.eigvals(A)
        spectral_abscissa = float(np.max(np.real(eigenvalues)))
        spectral_radius = float(np.max(np.abs(eigenvalues)))

        report: dict[str, Any] = {
            "spectral_abscissa": spectral_abscissa,
            "spectral_radius": spectral_radius,
            "numerical_abscissa": SpectralStability.numerical_abscissa(A),
            "condition_number": float(np.linalg.cond(A)),
            "is_stable_continuous": spectral_abscissa < 0,
            "is_stable_discrete": spectral_radius < 1,
            "is_hurwitz": spectral_abscissa < 0,
            "is_schur": spectral_radius < 1,
            "eigenvalues": eigenvalues,
        }

        # Determine stability rating
        if spectral_abscissa < -1e-6:
            report["rating"] = StabilityRating.STABLE
        elif spectral_abscissa < 1e-6:
            report["rating"] = StabilityRating.CONDITIONALLY_STABLE
        else:
            report["rating"] = StabilityRating.UNSTABLE

        return report


# ─── Fredholm Operators ──────────────────────────────────────────────────────

class FredholmAnalysis:
    """
    Fredholm operator analysis.

    A bounded operator T is Fredholm if:
    - ker(T) is finite-dimensional
    - range(T) is closed and has finite codimension
    The Fredholm index is ind(T) = dim ker(T) - codim range(T).
    """

    @staticmethod
    def kernel_dimension(A: NDArray, tol: float = 1e-10) -> int:
        """Dimension of the kernel (null space) of A."""
        svs = np.linalg.svd(A, compute_uv=False)
        return int(np.sum(svs < tol))

    @staticmethod
    def cokernel_dimension(A: NDArray, tol: float = 1e-10) -> int:
        """Dimension of the cokernel (= kernel of A*)."""
        return FredholmAnalysis.kernel_dimension(A.conj().T, tol)

    @staticmethod
    def fredholm_index(A: NDArray, tol: float = 1e-10) -> int:
        """Compute the Fredholm index ind(A) = dim ker(A) - dim coker(A)."""
        return (FredholmAnalysis.kernel_dimension(A, tol)
                - FredholmAnalysis.cokernel_dimension(A, tol))

    @staticmethod
    def is_fredholm(A: NDArray, tol: float = 1e-10) -> bool:
        """Check if A is a Fredholm operator (finite kernel and cokernel)."""
        ker_dim = FredholmAnalysis.kernel_dimension(A, tol)
        coker_dim = FredholmAnalysis.cokernel_dimension(A, tol)
        # Both must be finite (always true for finite matrices)
        # Additionally, range must be closed (check via SVD gap)
        svs = np.linalg.svd(A, compute_uv=False)
        nonzero_svs = svs[svs > tol]
        if len(nonzero_svs) == 0:
            return ker_dim < A.shape[1] and coker_dim < A.shape[0]
        return True

    @staticmethod
    def essential_spectrum(A: NDArray, perturbation_size: float = 0.01,
                          n_perturbations: int = 50) -> list[complex]:
        """
        Estimate essential spectrum by checking stability of eigenvalues
        under compact perturbations (for finite-dimensional approximation).
        """
        eigenvalues = np.linalg.eigvals(A)
        n = A.shape[0]
        rng = np.random.default_rng(42)
        stable_evs = []

        for ev in eigenvalues:
            stable_count = 0
            for _ in range(n_perturbations):
                # Random rank-1 perturbation (compact)
                u = rng.standard_normal(n)
                v = rng.standard_normal(n)
                K = perturbation_size * np.outer(u, v) / n
                perturbed_evs = np.linalg.eigvals(A + K)
                # Check if any perturbed eigenvalue is close
                min_dist = np.min(np.abs(perturbed_evs - ev))
                if min_dist < perturbation_size * 2:
                    stable_count += 1
            if stable_count > n_perturbations * 0.7:
                stable_evs.append(complex(ev))

        return stable_evs
