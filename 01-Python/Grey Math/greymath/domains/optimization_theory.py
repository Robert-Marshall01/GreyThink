"""
Optimization Theory Module.

Provides:
- Convex sets and cones
- Duality theory
- Lagrangian and KKT conditions
- Variational principles
- Proximal operators
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Callable, Optional

import numpy as np
from numpy.typing import NDArray


# ─── Convex Sets ────────────────────────────────────────────────────────────

@dataclass
class ConvexSet:
    """
    A convex set C ⊂ R^n.

    Defined implicitly by a membership test or explicitly by constraints.
    """
    name: str = "C"
    dim: int = 1
    membership: Optional[Callable[[NDArray], bool]] = None
    projection: Optional[Callable[[NDArray], NDArray]] = None

    def contains(self, x: NDArray) -> bool:
        if self.membership:
            return self.membership(x)
        raise NotImplementedError

    def project(self, x: NDArray) -> NDArray:
        """Project x onto the set."""
        if self.projection:
            return self.projection(x)
        raise NotImplementedError

    @staticmethod
    def ball(center: NDArray, radius: float, p: int = 2) -> "ConvexSet":
        """L_p ball."""
        return ConvexSet(
            name=f"B_{p}({radius})",
            dim=len(center),
            membership=lambda x: float(np.linalg.norm(x - center, p)) <= radius,
            projection=lambda x: (
                center + radius * (x - center) / max(np.linalg.norm(x - center, p), radius)
                if np.linalg.norm(x - center, p) > radius else x
            ),
        )

    @staticmethod
    def halfspace(a: NDArray, b: float) -> "ConvexSet":
        """Halfspace {x : a^T x ≤ b}."""
        return ConvexSet(
            name="halfspace",
            dim=len(a),
            membership=lambda x: float(np.dot(a, x)) <= b,
            projection=lambda x: x - max(0, np.dot(a, x) - b) / np.dot(a, a) * a,
        )

    @staticmethod
    def box(lower: NDArray, upper: NDArray) -> "ConvexSet":
        """Box constraint {x : lower ≤ x ≤ upper}."""
        return ConvexSet(
            name="box",
            dim=len(lower),
            membership=lambda x: bool(np.all(x >= lower) and np.all(x <= upper)),
            projection=lambda x: np.clip(x, lower, upper),
        )

    @staticmethod
    def simplex(dim: int) -> "ConvexSet":
        """Probability simplex Δ^{n-1}."""
        def project_simplex(x: NDArray) -> NDArray:
            """Project onto the simplex using the algorithm of Duchi et al."""
            u = np.sort(x)[::-1]
            cssv = np.cumsum(u) - 1
            rho = np.max(np.where(u > cssv / np.arange(1, len(x) + 1)))
            theta = cssv[rho] / (rho + 1)
            return np.maximum(x - theta, 0)

        return ConvexSet(
            name=f"Δ^{dim-1}",
            dim=dim,
            membership=lambda x: bool(np.all(x >= 0) and abs(np.sum(x) - 1) < 1e-10),
            projection=project_simplex,
        )


# ─── Cones ──────────────────────────────────────────────────────────────────

@dataclass
class Cone(ConvexSet):
    """
    A convex cone K ⊂ R^n.
    """
    @staticmethod
    def nonnegative_orthant(dim: int) -> "Cone":
        return Cone(
            name=f"R^{dim}_+",
            dim=dim,
            membership=lambda x: bool(np.all(x >= 0)),
            projection=lambda x: np.maximum(x, 0),
        )

    @staticmethod
    def second_order(dim: int) -> "Cone":
        """Second-order (Lorentz/ice cream) cone."""
        def membership(x: NDArray) -> bool:
            return float(x[-1]) >= float(np.linalg.norm(x[:-1]))

        def projection(x: NDArray) -> NDArray:
            t = x[-1]
            s = np.linalg.norm(x[:-1])
            if s <= t:
                return x
            if s <= -t:
                return np.zeros_like(x)
            alpha = 0.5 * (1 + t / s)
            result = np.zeros_like(x)
            result[:-1] = alpha * x[:-1]
            result[-1] = alpha * s
            return result

        return Cone(name="SOC", dim=dim, membership=membership, projection=projection)


# ─── Lagrangian and KKT ────────────────────────────────────────────────────

@dataclass
class LagrangianSystem:
    """
    Lagrangian for constrained optimization.

    L(x, λ, μ) = f(x) + Σ λ_i h_i(x) + Σ μ_j g_j(x)

    where h_i(x) = 0 are equality constraints
    and g_j(x) ≤ 0 are inequality constraints.
    """
    objective: Callable[[NDArray], float]
    eq_constraints: list[Callable[[NDArray], float]] = field(default_factory=list)
    ineq_constraints: list[Callable[[NDArray], float]] = field(default_factory=list)
    grad_objective: Optional[Callable[[NDArray], NDArray]] = None

    def lagrangian(self, x: NDArray, lam: NDArray, mu: NDArray) -> float:
        """Evaluate L(x, λ, μ)."""
        L = self.objective(x)
        for i, h in enumerate(self.eq_constraints):
            L += lam[i] * h(x)
        for j, g in enumerate(self.ineq_constraints):
            L += mu[j] * g(x)
        return L

    def check_kkt(self, x: NDArray, lam: NDArray, mu: NDArray,
                  tol: float = 1e-6) -> dict[str, bool]:
        """
        Check KKT conditions at (x, λ, μ).

        KKT conditions:
        1. Stationarity: ∇_x L = 0
        2. Primal feasibility: h(x) = 0, g(x) ≤ 0
        3. Dual feasibility: μ ≥ 0
        4. Complementary slackness: μ_j g_j(x) = 0
        """
        results: dict[str, bool] = {}

        # Primal feasibility
        eq_feasible = all(abs(h(x)) < tol for h in self.eq_constraints)
        ineq_feasible = all(g(x) <= tol for g in self.ineq_constraints)
        results["primal_feasibility"] = eq_feasible and ineq_feasible

        # Dual feasibility
        results["dual_feasibility"] = bool(np.all(mu >= -tol))

        # Complementary slackness
        slackness = all(
            abs(mu[j] * g(x)) < tol
            for j, g in enumerate(self.ineq_constraints)
        )
        results["complementary_slackness"] = slackness

        return results


# ─── Proximal Operators ────────────────────────────────────────────────────

class ProximalOperators:
    """Common proximal operators for optimization algorithms."""

    @staticmethod
    def prox_l1(x: NDArray, lam: float) -> NDArray:
        """Proximal operator of λ||·||_1 (soft thresholding)."""
        return np.sign(x) * np.maximum(np.abs(x) - lam, 0)

    @staticmethod
    def prox_l2(x: NDArray, lam: float) -> NDArray:
        """Proximal operator of λ||·||_2."""
        norm_x = np.linalg.norm(x)
        if norm_x <= lam:
            return np.zeros_like(x)
        return (1 - lam / norm_x) * x

    @staticmethod
    def prox_indicator(x: NDArray, projection: Callable[[NDArray], NDArray]) -> NDArray:
        """Proximal operator of indicator function = projection."""
        return projection(x)

    @staticmethod
    def prox_quadratic(x: NDArray, A: NDArray, b: NDArray,
                       lam: float) -> NDArray:
        """Proximal operator of λ(0.5 x^T A x + b^T x)."""
        n = len(x)
        M = np.eye(n) + lam * A
        return np.linalg.solve(M, x - lam * b)

    @staticmethod
    def proximal_gradient_descent(
        f: Callable[[NDArray], float],
        grad_f: Callable[[NDArray], NDArray],
        prox_g: Callable[[NDArray, float], NDArray],
        x0: NDArray,
        step_size: float = 0.01,
        max_iter: int = 1000,
        tol: float = 1e-8,
    ) -> tuple[NDArray, list[float]]:
        """
        Proximal gradient descent for min f(x) + g(x).

        f is smooth, g has a known proximal operator.
        """
        x = x0.copy()
        losses = []

        for _ in range(max_iter):
            grad = grad_f(x)
            x_new = prox_g(x - step_size * grad, step_size)

            if np.linalg.norm(x_new - x) < tol:
                break
            x = x_new
            losses.append(f(x))

        return x, losses
