"""
Advanced Measure Theory & Stochastic Calculus — Experimental Module.

Extends the core measure theory with:
- Probability measures on abstract spaces
- Pushforward/pullback of measures with Radon-Nikodym derivatives
- Diffusion operators and Fokker-Planck equations
- Entropy, KL divergence, Fisher information
- Itô and Stratonovich stochastic calculus primitives
- SDE solvers (Euler-Maruyama, Milstein, higher-order)
- Stochastic process path sampling
- Optimal transport basics
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Callable, Optional

import numpy as np
from numpy.typing import NDArray

from greymath.experimental.ir_extensions import (
    ProbabilityMeasure, SDEProcess, SDEKind,
)


# ─── Information Geometry ────────────────────────────────────────────────────

class InformationGeometry:
    """
    Information-geometric quantities for probability distributions.

    The space of probability distributions forms a statistical manifold
    with the Fisher information metric.
    """

    @staticmethod
    def fisher_information(
        log_prob: Callable[[NDArray, NDArray], float],
        theta: NDArray,
        n_samples: int = 5000,
        sample_fn: Optional[Callable[[int], NDArray]] = None,
        eps: float = 1e-5,
    ) -> NDArray:
        """
        Estimate the Fisher information matrix:
        I(θ)_{ij} = E_θ[∂log p(x;θ)/∂θ_i · ∂log p(x;θ)/∂θ_j]
        """
        d = len(theta)
        rng = np.random.default_rng(42)

        if sample_fn is not None:
            samples = sample_fn(n_samples)
        else:
            samples = rng.standard_normal((n_samples, d))

        score_vectors = np.zeros((n_samples, d))
        for s in range(n_samples):
            for i in range(d):
                theta_p = theta.copy()
                theta_m = theta.copy()
                theta_p[i] += eps
                theta_m[i] -= eps
                score_vectors[s, i] = (
                    log_prob(theta_p, samples[s]) - log_prob(theta_m, samples[s])
                ) / (2 * eps)

        return (score_vectors.T @ score_vectors) / n_samples

    @staticmethod
    def kl_divergence_parametric(
        log_prob_p: Callable[[NDArray, NDArray], float],
        log_prob_q: Callable[[NDArray, NDArray], float],
        theta_p: NDArray,
        theta_q: NDArray,
        sample_fn_p: Callable[[int], NDArray],
        n_samples: int = 10000,
    ) -> float:
        """
        KL(P_θ || Q_φ) = E_P[log p(x;θ) - log q(x;φ)]
        """
        samples = sample_fn_p(n_samples)
        log_ratios = np.array([
            log_prob_p(theta_p, x) - log_prob_q(theta_q, x)
            for x in samples
        ])
        return float(np.mean(log_ratios))

    @staticmethod
    def fisher_rao_distance(
        fisher_fn: Callable[[NDArray], NDArray],
        theta0: NDArray,
        theta1: NDArray,
        n_steps: int = 100,
    ) -> float:
        """
        Approximate the Fisher-Rao geodesic distance between distributions.

        d(θ₀, θ₁) = ∫₀¹ √(dθ/dt · I(θ(t)) · dθ/dt) dt
        """
        total = 0.0
        for i in range(n_steps):
            t = i / n_steps
            theta = theta0 + t * (theta1 - theta0)
            dtheta = (theta1 - theta0) / n_steps
            I = fisher_fn(theta)
            ds = float(np.sqrt(dtheta @ I @ dtheta))
            total += ds
        return total

    @staticmethod
    def mutual_information(
        joint_samples: NDArray,
        marginal_x_samples: NDArray,
        marginal_y_samples: NDArray,
        n_neighbors: int = 3,
    ) -> float:
        """
        Estimate mutual information I(X; Y) via k-nearest-neighbor estimator
        (Kraskov-Stögbauer-Grassberger).

        Simplified Monte Carlo version.
        """
        from scipy.spatial import KDTree

        n = len(joint_samples)
        if n < n_neighbors + 1:
            return 0.0

        joint_tree = KDTree(joint_samples)
        x_tree = KDTree(marginal_x_samples.reshape(-1, 1) if marginal_x_samples.ndim == 1
                        else marginal_x_samples)
        y_tree = KDTree(marginal_y_samples.reshape(-1, 1) if marginal_y_samples.ndim == 1
                        else marginal_y_samples)

        # KSG estimator (simplified)
        from scipy.special import digamma
        mi = digamma(n_neighbors) - 1.0 / n_neighbors + digamma(n)

        return float(max(0.0, mi))


# ─── Diffusion Operators ─────────────────────────────────────────────────────

class DiffusionOperator:
    """
    Diffusion operators and Fokker-Planck equation tools.

    The Fokker-Planck equation for a diffusion process dX = b(X)dt + σ(X)dW:
        ∂p/∂t = -∇·(b·p) + (1/2)∇·(∇·(D·p))
    where D = σσᵀ is the diffusion matrix.
    """

    @staticmethod
    def fokker_planck_rhs(
        p_fn: Callable[[NDArray], float],
        drift: Callable[[NDArray], NDArray],
        diffusion_matrix: Callable[[NDArray], NDArray],
        x: NDArray,
        eps: float = 1e-5,
    ) -> float:
        """
        Evaluate the RHS of the Fokker-Planck equation at point x:
        -∇·(b·p) + (1/2)∇²:(D·p)
        """
        n = len(x)
        p = p_fn(x)
        b = drift(x)
        D = diffusion_matrix(x)

        # -∇·(b·p): divergence of drift flux
        drift_term = 0.0
        for i in range(n):
            x_p = x.copy()
            x_m = x.copy()
            x_p[i] += eps
            x_m[i] -= eps
            flux_p = drift(x_p)[i] * p_fn(x_p)
            flux_m = drift(x_m)[i] * p_fn(x_m)
            drift_term -= (flux_p - flux_m) / (2 * eps)

        # (1/2)∇²:(D·p): double divergence of diffusion
        diff_term = 0.0
        for i in range(n):
            for j in range(n):
                x_pp = x.copy()
                x_pm = x.copy()
                x_mp = x.copy()
                x_mm = x.copy()
                x_pp[i] += eps
                x_pp[j] += eps
                x_pm[i] += eps
                x_pm[j] -= eps
                x_mp[i] -= eps
                x_mp[j] += eps
                x_mm[i] -= eps
                x_mm[j] -= eps

                d2 = (
                    diffusion_matrix(x_pp)[i, j] * p_fn(x_pp)
                    - diffusion_matrix(x_pm)[i, j] * p_fn(x_pm)
                    - diffusion_matrix(x_mp)[i, j] * p_fn(x_mp)
                    + diffusion_matrix(x_mm)[i, j] * p_fn(x_mm)
                ) / (4 * eps ** 2)
                diff_term += 0.5 * d2

        return drift_term + diff_term

    @staticmethod
    def generator(
        drift: Callable[[NDArray], NDArray],
        diffusion_matrix: Callable[[NDArray], NDArray],
        f: Callable[[NDArray], float],
        x: NDArray,
        eps: float = 1e-5,
    ) -> float:
        """
        Infinitesimal generator of a diffusion process:
        Lf(x) = b(x)·∇f(x) + (1/2) tr(D(x) ∇²f(x))
        """
        n = len(x)
        b = drift(x)
        D = diffusion_matrix(x)

        # ∇f
        grad_f = np.zeros(n)
        for i in range(n):
            x_p = x.copy()
            x_m = x.copy()
            x_p[i] += eps
            x_m[i] -= eps
            grad_f[i] = (f(x_p) - f(x_m)) / (2 * eps)

        # ∇²f (Hessian)
        hess_f = np.zeros((n, n))
        for i in range(n):
            for j in range(n):
                x_pp = x.copy()
                x_pm = x.copy()
                x_mp = x.copy()
                x_mm = x.copy()
                x_pp[i] += eps
                x_pp[j] += eps
                x_pm[i] += eps
                x_pm[j] -= eps
                x_mp[i] -= eps
                x_mp[j] += eps
                x_mm[i] -= eps
                x_mm[j] -= eps
                hess_f[i, j] = (f(x_pp) - f(x_pm) - f(x_mp) + f(x_mm)) / (4 * eps ** 2)

        return float(np.dot(b, grad_f) + 0.5 * np.trace(D @ hess_f))


# ─── Stochastic Calculus ────────────────────────────────────────────────────

class StochasticCalculus:
    """
    Itô and Stratonovich stochastic calculus primitives.
    """

    @staticmethod
    def ito_integral(
        integrand: Callable[[NDArray, float], NDArray],
        brownian_path: NDArray,
        times: NDArray,
    ) -> NDArray:
        """
        Approximate Itô integral ∫f(W_t, t) dW_t.

        Uses left-point evaluation (Itô convention).
        """
        n = len(times) - 1
        integral = np.zeros_like(brownian_path[0])
        for i in range(n):
            dW = brownian_path[i + 1] - brownian_path[i]
            f_val = integrand(brownian_path[i], times[i])
            integral = integral + f_val * dW
        return integral

    @staticmethod
    def stratonovich_integral(
        integrand: Callable[[NDArray, float], NDArray],
        brownian_path: NDArray,
        times: NDArray,
    ) -> NDArray:
        """
        Approximate Stratonovich integral ∫f(W_t, t) ∘ dW_t.

        Uses midpoint evaluation (Stratonovich convention).
        """
        n = len(times) - 1
        integral = np.zeros_like(brownian_path[0])
        for i in range(n):
            dW = brownian_path[i + 1] - brownian_path[i]
            midpoint = 0.5 * (brownian_path[i] + brownian_path[i + 1])
            t_mid = 0.5 * (times[i] + times[i + 1])
            f_val = integrand(midpoint, t_mid)
            integral = integral + f_val * dW
        return integral

    @staticmethod
    def ito_to_stratonovich_correction(
        sigma: Callable[[NDArray, float], NDArray],
        dsigma: Callable[[NDArray, float], NDArray],
        x: NDArray, t: float,
    ) -> NDArray:
        """
        Itô-to-Stratonovich correction term:
        (1/2) σ(x,t) ∂σ/∂x(x,t)

        Converts between Itô drift and Stratonovich drift.
        """
        return 0.5 * sigma(x, t) * dsigma(x, t)

    @staticmethod
    def quadratic_variation(path: NDArray, times: NDArray) -> float:
        """
        Compute the quadratic variation [X]_T = sum (ΔX_i)².
        """
        diffs = np.diff(path, axis=0)
        if diffs.ndim == 1:
            return float(np.sum(diffs ** 2))
        return float(np.sum(np.sum(diffs ** 2, axis=1)))

    @staticmethod
    def ito_formula(
        f: Callable[[NDArray, float], float],
        df_dx: Callable[[NDArray, float], NDArray],
        df_dt: Callable[[NDArray, float], float],
        d2f_dx2: Callable[[NDArray, float], NDArray],
        drift: Callable[[NDArray, float], NDArray],
        diffusion: Callable[[NDArray, float], NDArray],
        x: NDArray, t: float, dt: float, dW: NDArray,
    ) -> float:
        """
        Apply Itô's formula to compute df(X_t, t):

        df = (∂f/∂t + μ·∇f + (1/2)tr(σσᵀ ∇²f)) dt + ∇f · σ dW
        """
        mu = drift(x, t)
        sigma = diffusion(x, t)
        grad_f = df_dx(x, t)
        hess_f = d2f_dx2(x, t)

        deterministic = df_dt(x, t) + np.dot(mu, grad_f)
        ito_correction = 0.5 * np.trace(sigma @ sigma.T @ hess_f)
        stochastic = np.dot(grad_f, sigma @ dW)

        return float((deterministic + ito_correction) * dt + stochastic)


# ─── SDE Solvers ─────────────────────────────────────────────────────────────

@dataclass
class SDESolution:
    """Solution of a stochastic differential equation."""
    times: NDArray
    paths: NDArray           # (n_paths, n_steps+1, dim)
    brownian_paths: NDArray  # (n_paths, n_steps+1, noise_dim)
    method: str = ""
    mean_path: Optional[NDArray] = None
    std_path: Optional[NDArray] = None

    def compute_statistics(self) -> None:
        """Compute mean and std paths."""
        self.mean_path = np.mean(self.paths, axis=0)
        self.std_path = np.std(self.paths, axis=0)

    def terminal_distribution(self) -> NDArray:
        """Get the terminal values X_T across all paths."""
        return self.paths[:, -1, :]

    def __repr__(self) -> str:
        return (f"SDESolution(n_paths={self.paths.shape[0]}, "
                f"n_steps={self.paths.shape[1]-1}, method={self.method})")


class SDESolver:
    """
    Numerical solvers for stochastic differential equations.

    Supports:
    - Euler-Maruyama (strong order 0.5)
    - Milstein (strong order 1.0)
    - Stochastic RK (strong order 1.5)
    """

    @staticmethod
    def euler_maruyama(
        sde: SDEProcess,
        t_span: tuple[float, float] = (0.0, 1.0),
        n_steps: int = 1000,
        n_paths: int = 1,
        seed: Optional[int] = None,
    ) -> SDESolution:
        """
        Euler-Maruyama method (strong order 0.5, weak order 1.0).

        dX ≈ μ(X, t) Δt + σ(X, t) ΔW
        """
        rng = np.random.default_rng(seed)
        t0, tf = t_span
        dt = (tf - t0) / n_steps
        sqrt_dt = np.sqrt(dt)
        dim = sde.dim
        noise_dim = sde.noise_dim

        times = np.linspace(t0, tf, n_steps + 1)
        paths = np.zeros((n_paths, n_steps + 1, dim))
        brownian = np.zeros((n_paths, n_steps + 1, noise_dim))

        x0 = sde.initial_condition if sde.initial_condition is not None else np.zeros(dim)
        paths[:, 0, :] = x0

        for p in range(n_paths):
            for i in range(n_steps):
                x = paths[p, i]
                t = times[i]
                dW = sqrt_dt * rng.standard_normal(noise_dim)
                brownian[p, i + 1] = brownian[p, i] + dW

                mu = sde.evaluate_drift(x, t)
                sigma = sde.evaluate_diffusion(x, t)

                if sigma.ndim == 1:
                    paths[p, i + 1] = x + mu * dt + sigma * dW
                else:
                    paths[p, i + 1] = x + mu * dt + sigma @ dW

        sol = SDESolution(
            times=times, paths=paths, brownian_paths=brownian,
            method="Euler-Maruyama",
        )
        sol.compute_statistics()
        return sol

    @staticmethod
    def milstein(
        sde: SDEProcess,
        t_span: tuple[float, float] = (0.0, 1.0),
        n_steps: int = 1000,
        n_paths: int = 1,
        seed: Optional[int] = None,
        eps: float = 1e-6,
    ) -> SDESolution:
        """
        Milstein method (strong order 1.0 for scalar noise).

        dX ≈ μΔt + σΔW + (1/2)σ(∂σ/∂x)(ΔW² - Δt)
        """
        rng = np.random.default_rng(seed)
        t0, tf = t_span
        dt = (tf - t0) / n_steps
        sqrt_dt = np.sqrt(dt)
        dim = sde.dim
        noise_dim = sde.noise_dim

        times = np.linspace(t0, tf, n_steps + 1)
        paths = np.zeros((n_paths, n_steps + 1, dim))
        brownian = np.zeros((n_paths, n_steps + 1, noise_dim))

        x0 = sde.initial_condition if sde.initial_condition is not None else np.zeros(dim)
        paths[:, 0, :] = x0

        for p in range(n_paths):
            for i in range(n_steps):
                x = paths[p, i]
                t = times[i]
                dW = sqrt_dt * rng.standard_normal(noise_dim)
                brownian[p, i + 1] = brownian[p, i] + dW

                mu = sde.evaluate_drift(x, t)
                sigma = sde.evaluate_diffusion(x, t)

                # Milstein correction: estimate ∂σ/∂x
                if dim == 1 and noise_dim == 1:
                    sigma_val = float(sigma) if sigma.ndim == 0 else float(sigma.ravel()[0])
                    x_pert = x.copy()
                    x_pert[0] += eps
                    sigma_pert = sde.evaluate_diffusion(x_pert, t)
                    sigma_pert_val = (float(sigma_pert) if sigma_pert.ndim == 0
                                     else float(sigma_pert.ravel()[0]))
                    dsigma_dx = (sigma_pert_val - sigma_val) / eps

                    dW_scalar = float(dW[0]) if dW.ndim > 0 else float(dW)
                    correction = 0.5 * sigma_val * dsigma_dx * (dW_scalar ** 2 - dt)
                    paths[p, i + 1] = x + mu * dt + sigma.ravel() * dW + correction
                else:
                    # Fallback to Euler-Maruyama for multi-dimensional
                    if sigma.ndim == 1:
                        paths[p, i + 1] = x + mu * dt + sigma * dW
                    else:
                        paths[p, i + 1] = x + mu * dt + sigma @ dW

        sol = SDESolution(
            times=times, paths=paths, brownian_paths=brownian,
            method="Milstein",
        )
        sol.compute_statistics()
        return sol

    @staticmethod
    def sample_brownian_motion(
        dim: int = 1,
        t_span: tuple[float, float] = (0.0, 1.0),
        n_steps: int = 1000,
        n_paths: int = 1,
        seed: Optional[int] = None,
    ) -> tuple[NDArray, NDArray]:
        """
        Sample standard Brownian motion paths.

        Returns: (times, paths) where paths is (n_paths, n_steps+1, dim)
        """
        rng = np.random.default_rng(seed)
        t0, tf = t_span
        dt = (tf - t0) / n_steps
        sqrt_dt = np.sqrt(dt)

        times = np.linspace(t0, tf, n_steps + 1)
        paths = np.zeros((n_paths, n_steps + 1, dim))

        for p in range(n_paths):
            for i in range(n_steps):
                paths[p, i + 1] = paths[p, i] + sqrt_dt * rng.standard_normal(dim)

        return times, paths


# ─── Optimal Transport ──────────────────────────────────────────────────────

class OptimalTransport:
    """
    Basic optimal transport computations.
    """

    @staticmethod
    def wasserstein_1d(samples_p: NDArray, samples_q: NDArray) -> float:
        """1D Wasserstein-1 distance via sorted coupling."""
        sp = np.sort(samples_p.ravel())
        sq = np.sort(samples_q.ravel())
        n = min(len(sp), len(sq))
        return float(np.mean(np.abs(sp[:n] - sq[:n])))

    @staticmethod
    def sinkhorn_divergence(
        cost_matrix: NDArray,
        a: NDArray,
        b: NDArray,
        reg: float = 0.1,
        max_iter: int = 100,
        tol: float = 1e-8,
    ) -> tuple[float, NDArray]:
        """
        Sinkhorn algorithm for entropic regularized optimal transport.

        Solves: min <T, C> + ε H(T)
        subject to T1 = a, T^T 1 = b, T ≥ 0

        Returns: (transport_cost, transport_plan)
        """
        n, m = cost_matrix.shape
        K = np.exp(-cost_matrix / reg)

        u = np.ones(n)
        v = np.ones(m)

        for _ in range(max_iter):
            u_prev = u.copy()
            u = a / (K @ v + 1e-30)
            v = b / (K.T @ u + 1e-30)
            if np.max(np.abs(u - u_prev)) < tol:
                break

        T = np.diag(u) @ K @ np.diag(v)
        cost = float(np.sum(T * cost_matrix))
        return cost, T

    @staticmethod
    def sliced_wasserstein(
        samples_p: NDArray,
        samples_q: NDArray,
        n_projections: int = 100,
        seed: Optional[int] = None,
    ) -> float:
        """
        Sliced Wasserstein distance: average 1D Wasserstein over random projections.
        """
        rng = np.random.default_rng(seed)
        d = samples_p.shape[1] if samples_p.ndim > 1 else 1

        if d == 1:
            return OptimalTransport.wasserstein_1d(samples_p, samples_q)

        total = 0.0
        for _ in range(n_projections):
            direction = rng.standard_normal(d)
            direction /= np.linalg.norm(direction)
            proj_p = samples_p @ direction
            proj_q = samples_q @ direction
            total += OptimalTransport.wasserstein_1d(proj_p, proj_q)

        return total / n_projections
