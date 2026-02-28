"""
Grey Physics — Experimental Mode

Cutting-edge / research features:
  - Geometric mechanics on manifolds (Lie groups, reduction)
  - Non-Abelian gauge theory (SU(2), SU(3))
  - QFT primitives (propagators, Feynman diagrams, Wick contractions)
  - RG flows and fixed-point analysis
  - Data-driven PDE discovery (SINDy, sparse regression)
  - Koopman operator methods
  - Information geometry
"""

from __future__ import annotations

import numpy as np
from typing import Any, Callable, Dict, List, Optional, Tuple

from grey_physics.core.numeric import ODESolver


# ============================================================
# Geometric Mechanics on Lie Groups
# ============================================================

class LieGroup:
    """Rudimentary Lie group / algebra computations."""

    @staticmethod
    def so3_hat(omega: np.ndarray) -> np.ndarray:
        """Hat map: ℝ³ → so(3) (skew-symmetric matrix)."""
        return np.array([
            [0, -omega[2], omega[1]],
            [omega[2], 0, -omega[0]],
            [-omega[1], omega[0], 0],
        ])

    @staticmethod
    def so3_vee(Omega: np.ndarray) -> np.ndarray:
        """Vee map: so(3) → ℝ³."""
        return np.array([Omega[2, 1], Omega[0, 2], Omega[1, 0]])

    @staticmethod
    def so3_exp(omega: np.ndarray) -> np.ndarray:
        """Exponential map so(3) → SO(3) via Rodrigues' formula.

        R = I + sin(θ)/θ [ω]_× + (1-cos(θ))/θ² [ω]_×²
        """
        theta = np.linalg.norm(omega)
        if theta < 1e-10:
            return np.eye(3) + LieGroup.so3_hat(omega)
        K = LieGroup.so3_hat(omega / theta)
        return (np.eye(3) +
                np.sin(theta) * K +
                (1 - np.cos(theta)) * K @ K)

    @staticmethod
    def su2_generators() -> List[np.ndarray]:
        """SU(2) generators: σ_a / (2i)."""
        s1 = 0.5 * np.array([[0, 1], [1, 0]], dtype=complex)
        s2 = 0.5 * np.array([[0, -1j], [1j, 0]], dtype=complex)
        s3 = 0.5 * np.array([[1, 0], [0, -1]], dtype=complex)
        return [s1, s2, s3]

    @staticmethod
    def su3_gell_mann() -> List[np.ndarray]:
        """SU(3) Gell-Mann matrices λ_1 through λ_8."""
        l1 = np.array([[0,1,0],[1,0,0],[0,0,0]], dtype=complex)
        l2 = np.array([[0,-1j,0],[1j,0,0],[0,0,0]], dtype=complex)
        l3 = np.array([[1,0,0],[0,-1,0],[0,0,0]], dtype=complex)
        l4 = np.array([[0,0,1],[0,0,0],[1,0,0]], dtype=complex)
        l5 = np.array([[0,0,-1j],[0,0,0],[1j,0,0]], dtype=complex)
        l6 = np.array([[0,0,0],[0,0,1],[0,1,0]], dtype=complex)
        l7 = np.array([[0,0,0],[0,0,-1j],[0,1j,0]], dtype=complex)
        l8 = np.array([[1,0,0],[0,1,0],[0,0,-2]], dtype=complex) / np.sqrt(3)
        return [l1, l2, l3, l4, l5, l6, l7, l8]

    @staticmethod
    def structure_constants(generators: List[np.ndarray]) -> np.ndarray:
        """Compute structure constants f^c_{ab} from [T_a, T_b] = i f^c_{ab} T_c."""
        n = len(generators)
        f = np.zeros((n, n, n), dtype=complex)
        for a in range(n):
            for b in range(n):
                comm = generators[a] @ generators[b] - generators[b] @ generators[a]
                for c in range(n):
                    # f^c_{ab} = -2i Tr(T_c [T_a, T_b]) / Tr(T_c T_c)
                    norm = np.trace(generators[c] @ generators[c])
                    if abs(norm) > 1e-10:
                        f[a, b, c] = -2j * np.trace(generators[c] @ comm) / norm
        return np.real(f)


# ============================================================
# Euler-Poincaré on SO(3)
# ============================================================

class RigidBodyOnSO3:
    """Rigid body dynamics as Euler-Poincaré equations on SO(3).

    Ω̇ = I⁻¹(IΩ × Ω + τ)
    Ṙ = R [Ω]_×
    """

    def __init__(self, inertia: np.ndarray):
        self.I = np.diag(inertia)
        self.I_inv = np.diag(1.0 / inertia)

    def simulate(self, R0: np.ndarray, Omega0: np.ndarray,
                 t_span: Tuple[float, float],
                 dt: float = 0.001) -> Dict[str, Any]:
        """Integrate on SO(3) using Lie group integrator."""
        t0, tf = t_span
        n_steps = int((tf - t0) / dt)
        t_arr = np.linspace(t0, tf, n_steps + 1)

        R = R0.copy()
        Omega = Omega0.copy()
        R_history = [R.copy()]
        Omega_history = [Omega.copy()]
        energy_history = [0.5 * Omega @ self.I @ Omega]

        for _ in range(n_steps):
            # Euler-Poincaré
            L = self.I @ Omega
            Omega_dot = self.I_inv @ np.cross(L, Omega)

            # RK4 for Omega
            k1 = Omega_dot
            Om2 = Omega + 0.5 * dt * k1
            L2 = self.I @ Om2
            k2 = self.I_inv @ np.cross(L2, Om2)
            Om3 = Omega + 0.5 * dt * k2
            L3 = self.I @ Om3
            k3 = self.I_inv @ np.cross(L3, Om3)
            Om4 = Omega + dt * k3
            L4 = self.I @ Om4
            k4 = self.I_inv @ np.cross(L4, Om4)
            Omega = Omega + (dt / 6) * (k1 + 2*k2 + 2*k3 + k4)

            # Update rotation via exponential map
            dR = LieGroup.so3_exp(Omega * dt)
            R = R @ dR

            R_history.append(R.copy())
            Omega_history.append(Omega.copy())
            energy_history.append(0.5 * Omega @ self.I @ Omega)

        return {
            "t": t_arr,
            "R": R_history,
            "Omega": np.array(Omega_history),
            "energy": np.array(energy_history),
        }


# ============================================================
# QFT Primitives
# ============================================================

class QFTPrimitives:
    """Basic quantum field theory computations."""

    @staticmethod
    def free_propagator_scalar(p: np.ndarray, m: float,
                                metric: str = "euclidean") -> complex:
        """Free scalar propagator: G(p) = 1/(p² + m²) (Euclidean)
        or 1/(p² - m² + iε) (Minkowski).
        """
        if metric == "euclidean":
            p2 = np.dot(p, p)
            return 1.0 / (p2 + m**2)
        else:
            # Minkowski: p² = p₀² - |p|² with iε prescription
            p2 = p[0]**2 - np.dot(p[1:], p[1:])
            return 1.0 / (p2 - m**2 + 1e-10j)

    @staticmethod
    def wick_contraction_count(n: int) -> int:
        """Number of complete Wick contractions for n fields.

        (2k)!! = (2k-1)!! for n=2k.
        """
        if n % 2 != 0:
            return 0
        k = n // 2
        result = 1
        for i in range(1, 2*k, 2):
            result *= i
        return result

    @staticmethod
    def one_loop_integral_dim_reg(d: int, m: float,
                                   n: int = 1) -> complex:
        """One-loop scalar integral in dimensional regularization.

        I_n(d) = ∫ d^d k / (2π)^d  1/(k² + m²)^n

        = Γ(n - d/2) / ((4π)^{d/2} Γ(n)) × (m²)^{d/2 - n}
        """
        from math import gamma as gamma_func
        try:
            numerator = gamma_func(n - d / 2)
            denominator = (4 * np.pi)**(d / 2) * gamma_func(n)
            mass_factor = m**(d - 2*n)
            return numerator / denominator * mass_factor
        except (ValueError, OverflowError):
            return complex('nan')

    @staticmethod
    def feynman_parameter_integral(n_props: int, masses: List[float],
                                    d: int = 4) -> float:
        """Feynman parametrization for n-propagator one-loop integral.

        Simplified for equal-mass case.
        """
        from math import gamma as gamma_func
        try:
            # For n propagators in d dimensions:
            # I = Γ(n-d/2) / Γ(n) × ∫ dx_1...dx_{n-1} / D^{n-d/2}
            prefactor = gamma_func(n_props - d/2) / gamma_func(n_props)
            m_avg = np.mean(masses)
            return float(prefactor / m_avg**(2*n_props - d))
        except (ValueError, OverflowError):
            return float('nan')


# ============================================================
# Data-Driven PDE Discovery (SINDy)
# ============================================================

class SINDy:
    """Sparse Identification of Nonlinear Dynamics.

    Discovers governing equations from data:
      ẋ = Θ(x) ξ
    where Θ is a library of candidate functions and ξ is sparse.
    """

    def __init__(self, poly_order: int = 3,
                 include_trig: bool = False):
        self.poly_order = poly_order
        self.include_trig = include_trig

    def build_library(self, X: np.ndarray) -> Tuple[np.ndarray, List[str]]:
        """Build the library matrix Θ(X).

        Parameters
        ----------
        X : (n_samples, n_features) data matrix
        """
        n, d = X.shape
        library = []
        labels = []

        # Constant
        library.append(np.ones(n))
        labels.append("1")

        # Linear
        for i in range(d):
            library.append(X[:, i])
            labels.append(f"x{i}")

        # Quadratic
        if self.poly_order >= 2:
            for i in range(d):
                for j in range(i, d):
                    library.append(X[:, i] * X[:, j])
                    labels.append(f"x{i}*x{j}")

        # Cubic
        if self.poly_order >= 3:
            for i in range(d):
                for j in range(i, d):
                    for k in range(j, d):
                        library.append(X[:, i] * X[:, j] * X[:, k])
                        labels.append(f"x{i}*x{j}*x{k}")

        # Trigonometric
        if self.include_trig:
            for i in range(d):
                library.append(np.sin(X[:, i]))
                labels.append(f"sin(x{i})")
                library.append(np.cos(X[:, i]))
                labels.append(f"cos(x{i})")

        return np.array(library).T, labels

    def fit(self, X: np.ndarray, X_dot: np.ndarray,
            threshold: float = 0.1,
            max_iter: int = 10) -> Tuple[np.ndarray, List[str]]:
        """Fit SINDy model using sequential thresholded least squares (STLS).

        Parameters
        ----------
        X : state data (n_samples, n_features)
        X_dot : time derivatives (n_samples, n_features)
        threshold : sparsity threshold
        max_iter : number of thresholding iterations
        """
        Theta, labels = self.build_library(X)
        n_features = X.shape[1]
        n_lib = Theta.shape[1]

        Xi = np.linalg.lstsq(Theta, X_dot, rcond=None)[0]

        for _ in range(max_iter):
            small = np.abs(Xi) < threshold
            Xi[small] = 0
            for i in range(n_features):
                big = ~small[:, i]
                if np.sum(big) > 0:
                    Xi[big, i] = np.linalg.lstsq(
                        Theta[:, big], X_dot[:, i], rcond=None
                    )[0]

        return Xi, labels

    def print_equations(self, Xi: np.ndarray,
                        labels: List[str],
                        var_names: Optional[List[str]] = None) -> List[str]:
        """Format discovered equations as strings."""
        n_features = Xi.shape[1]
        equations = []

        for i in range(n_features):
            name = var_names[i] if var_names else f"x{i}_dot"
            terms = []
            for j in range(len(labels)):
                if abs(Xi[j, i]) > 1e-10:
                    coeff = Xi[j, i]
                    terms.append(f"{coeff:.4f}*{labels[j]}")
            eq = f"{name} = " + " + ".join(terms) if terms else f"{name} = 0"
            equations.append(eq)

        return equations


# ============================================================
# Koopman Operator
# ============================================================

class KoopmanAnalysis:
    """Koopman operator analysis via Dynamic Mode Decomposition (DMD)."""

    @staticmethod
    def dmd(X: np.ndarray, rank: Optional[int] = None) -> Dict[str, np.ndarray]:
        """Exact Dynamic Mode Decomposition.

        Parameters
        ----------
        X : (n_features, n_snapshots) data matrix
            Columns are sequential snapshots.
        rank : truncation rank (None = full)

        Returns
        -------
        dict with 'eigenvalues', 'modes', 'dynamics'
        """
        X1 = X[:, :-1]
        X2 = X[:, 1:]

        U, S, Vh = np.linalg.svd(X1, full_matrices=False)

        if rank is not None:
            U = U[:, :rank]
            S = S[:rank]
            Vh = Vh[:rank, :]

        S_inv = np.diag(1.0 / S)
        A_tilde = U.T @ X2 @ Vh.T @ S_inv

        eigenvalues, W = np.linalg.eig(A_tilde)
        modes = X2 @ Vh.T @ S_inv @ W

        return {
            "eigenvalues": eigenvalues,
            "modes": modes,
            "A_tilde": A_tilde,
        }

    @staticmethod
    def reconstruct(modes: np.ndarray, eigenvalues: np.ndarray,
                    b0: np.ndarray, n_steps: int,
                    dt: float = 1.0) -> np.ndarray:
        """Reconstruct dynamics from DMD modes.

        x(t) = Σ_k b_k φ_k exp(ω_k t)
        """
        omegas = np.log(eigenvalues) / dt
        n_modes = len(eigenvalues)
        t = np.arange(n_steps) * dt

        X_recon = np.zeros((modes.shape[0], n_steps), dtype=complex)
        for k in range(n_modes):
            X_recon += np.outer(modes[:, k], b0[k] * np.exp(omegas[k] * t))

        return np.real(X_recon)


# ============================================================
# Information Geometry
# ============================================================

class InformationGeometry:
    """Fisher information metric on statistical manifolds."""

    @staticmethod
    def fisher_metric_gaussian(mu: float, sigma: float) -> np.ndarray:
        """Fisher information matrix for Gaussian N(μ, σ²).

        g = [[1/σ², 0], [0, 2/σ²]]
        """
        return np.array([
            [1.0 / sigma**2, 0],
            [0, 2.0 / sigma**2],
        ])

    @staticmethod
    def fisher_metric_exponential(theta: np.ndarray,
                                    log_partition: Callable,
                                    eps: float = 1e-5) -> np.ndarray:
        """Fisher metric for exponential family via second derivatives of
        log-partition function.

        g_{ij} = ∂²A/∂θ_i∂θ_j
        """
        d = len(theta)
        g = np.zeros((d, d))
        A0 = log_partition(theta)

        for i in range(d):
            for j in range(i, d):
                th_pp = theta.copy()
                th_pp[i] += eps; th_pp[j] += eps
                th_pm = theta.copy()
                th_pm[i] += eps; th_pm[j] -= eps
                th_mp = theta.copy()
                th_mp[i] -= eps; th_mp[j] += eps
                th_mm = theta.copy()
                th_mm[i] -= eps; th_mm[j] -= eps

                g[i, j] = (log_partition(th_pp) - log_partition(th_pm) -
                            log_partition(th_mp) + log_partition(th_mm)) / (4 * eps**2)
                g[j, i] = g[i, j]

        return g

    @staticmethod
    def kl_divergence_gaussian(mu1: float, sigma1: float,
                                mu2: float, sigma2: float) -> float:
        """KL divergence D_KL(N₁ || N₂) for Gaussians."""
        return (np.log(sigma2 / sigma1) +
                (sigma1**2 + (mu1 - mu2)**2) / (2 * sigma2**2) - 0.5)

    @staticmethod
    def geodesic_gaussian(mu0: float, sigma0: float,
                           mu1: float, sigma1: float,
                           n_points: int = 100) -> Tuple[np.ndarray, np.ndarray]:
        """Geodesic on the Gaussian manifold.

        The Fisher metric for Gaussians has the Poincaré half-plane geometry.
        """
        t = np.linspace(0, 1, n_points)
        # Linear interpolation in (μ, log σ) coordinates
        # (exact geodesic for pure σ changes)
        mu_t = mu0 + t * (mu1 - mu0)
        log_sigma_t = np.log(sigma0) + t * (np.log(sigma1) - np.log(sigma0))
        sigma_t = np.exp(log_sigma_t)
        return mu_t, sigma_t
