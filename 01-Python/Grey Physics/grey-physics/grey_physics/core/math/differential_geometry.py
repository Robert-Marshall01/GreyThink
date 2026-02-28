"""
Grey Physics Math Core — Differential Geometry

Manifolds, charts, metrics, connections, geodesics, curvature,
covariant derivatives, differential forms, and related structures.
"""

from __future__ import annotations

import itertools
from typing import Any, Callable, Dict, List, Optional, Tuple
import numpy as np
from scipy.integrate import solve_ivp

from grey_physics.core.ir.types import Expr, ExprKind, Coordinate, func
from grey_physics.core.ir.spacetime import Manifold, Metric, Connection, CurvatureTensor, Chart


# ---------------------------------------------------------------------------
# Numeric Christoffel symbol computation
# ---------------------------------------------------------------------------

def christoffel_symbols_numeric(
    metric_func: Callable[[np.ndarray], np.ndarray],
    point: np.ndarray,
    eps: float = 1e-7
) -> np.ndarray:
    """Compute Christoffel symbols Γ^λ_{μν} numerically at a point.

    metric_func(x) returns the metric tensor g_{μν} as a (dim,dim) array.
    Uses central finite differences for metric derivatives.

    Γ^λ_{μν} = ½ g^{λσ}(∂_μ g_{νσ} + ∂_ν g_{μσ} - ∂_σ g_{μν})
    """
    dim = len(point)
    g = metric_func(point)
    g_inv = np.linalg.inv(g)

    # Compute ∂_α g_{βγ} via central differences
    dg = np.zeros((dim, dim, dim))  # dg[alpha, beta, gamma] = ∂_α g_{βγ}
    for alpha in range(dim):
        point_p = point.copy()
        point_m = point.copy()
        point_p[alpha] += eps
        point_m[alpha] -= eps
        g_p = metric_func(point_p)
        g_m = metric_func(point_m)
        dg[alpha] = (g_p - g_m) / (2.0 * eps)

    # Assemble Christoffel symbols
    Gamma = np.zeros((dim, dim, dim))
    for lam in range(dim):
        for mu in range(dim):
            for nu in range(dim):
                s = 0.0
                for sigma in range(dim):
                    s += g_inv[lam, sigma] * (
                        dg[mu, nu, sigma] + dg[nu, mu, sigma] - dg[sigma, mu, nu]
                    )
                Gamma[lam, mu, nu] = 0.5 * s
    return Gamma


# ---------------------------------------------------------------------------
# Riemann curvature tensor computation
# ---------------------------------------------------------------------------

def riemann_tensor_numeric(
    metric_func: Callable[[np.ndarray], np.ndarray],
    point: np.ndarray,
    eps: float = 1e-5
) -> np.ndarray:
    """Compute the Riemann tensor R^ρ_{σμν} numerically.

    R^ρ_{σμν} = ∂_μ Γ^ρ_{νσ} - ∂_ν Γ^ρ_{μσ}
                + Γ^ρ_{μλ}Γ^λ_{νσ} - Γ^ρ_{νλ}Γ^λ_{μσ}
    """
    dim = len(point)
    Gamma = christoffel_symbols_numeric(metric_func, point, eps)

    # Derivatives of Christoffel symbols
    dGamma = np.zeros((dim, dim, dim, dim))  # dGamma[alpha, rho, mu, nu]
    for alpha in range(dim):
        p_plus = point.copy()
        p_minus = point.copy()
        p_plus[alpha] += eps
        p_minus[alpha] -= eps
        G_plus = christoffel_symbols_numeric(metric_func, p_plus, eps)
        G_minus = christoffel_symbols_numeric(metric_func, p_minus, eps)
        dGamma[alpha] = (G_plus - G_minus) / (2.0 * eps)

    R = np.zeros((dim, dim, dim, dim))
    for rho in range(dim):
        for sigma in range(dim):
            for mu in range(dim):
                for nu in range(dim):
                    R[rho, sigma, mu, nu] = (
                        dGamma[mu, rho, nu, sigma] - dGamma[nu, rho, mu, sigma]
                    )
                    for lam in range(dim):
                        R[rho, sigma, mu, nu] += (
                            Gamma[rho, mu, lam] * Gamma[lam, nu, sigma] -
                            Gamma[rho, nu, lam] * Gamma[lam, mu, sigma]
                        )
    return R


def ricci_tensor_numeric(
    metric_func: Callable[[np.ndarray], np.ndarray],
    point: np.ndarray,
    eps: float = 1e-5
) -> np.ndarray:
    """Compute Ricci tensor R_{μν} = R^λ_{μλν}."""
    R = riemann_tensor_numeric(metric_func, point, eps)
    dim = R.shape[0]
    Ric = np.zeros((dim, dim))
    for mu in range(dim):
        for nu in range(dim):
            for lam in range(dim):
                Ric[mu, nu] += R[lam, mu, lam, nu]
    return Ric


def ricci_scalar_numeric(
    metric_func: Callable[[np.ndarray], np.ndarray],
    point: np.ndarray,
    eps: float = 1e-5
) -> float:
    """Compute Ricci scalar R = g^{μν} R_{μν}."""
    g = metric_func(point)
    g_inv = np.linalg.inv(g)
    Ric = ricci_tensor_numeric(metric_func, point, eps)
    return float(np.sum(g_inv * Ric))


# ---------------------------------------------------------------------------
# Geodesic solver
# ---------------------------------------------------------------------------

def geodesic_equation(
    metric_func: Callable[[np.ndarray], np.ndarray],
    dim: int
) -> Callable[[float, np.ndarray], np.ndarray]:
    """Return the RHS of the geodesic equation as a first-order system.

    The geodesic equation: d²x^μ/dτ² + Γ^μ_{αβ} dx^α/dτ dx^β/dτ = 0

    State vector: [x^0, ..., x^{d-1}, u^0, ..., u^{d-1}]
    where u^μ = dx^μ/dτ.
    """
    def rhs(tau: float, state: np.ndarray) -> np.ndarray:
        x = state[:dim]
        u = state[dim:]
        Gamma = christoffel_symbols_numeric(metric_func, x)
        dstate = np.zeros(2 * dim)
        dstate[:dim] = u
        for mu in range(dim):
            acc = 0.0
            for alpha in range(dim):
                for beta in range(dim):
                    acc -= Gamma[mu, alpha, beta] * u[alpha] * u[beta]
            dstate[dim + mu] = acc
        return dstate
    return rhs


def solve_geodesic(
    metric_func: Callable[[np.ndarray], np.ndarray],
    x0: np.ndarray,
    u0: np.ndarray,
    tau_span: Tuple[float, float] = (0, 10),
    num_points: int = 1000,
    method: str = "RK45"
) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Solve the geodesic equation numerically.

    Args:
        metric_func: g_{μν}(x) → (dim, dim) array
        x0: initial position
        u0: initial 4-velocity
        tau_span: proper time interval
        num_points: number of output points
        method: integration method

    Returns:
        taus: array of proper times
        positions: (num_points, dim) array of positions
        velocities: (num_points, dim) array of velocities
    """
    dim = len(x0)
    rhs = geodesic_equation(metric_func, dim)
    state0 = np.concatenate([x0, u0])
    tau_eval = np.linspace(tau_span[0], tau_span[1], num_points)

    sol = solve_ivp(rhs, tau_span, state0, t_eval=tau_eval,
                    method=method, rtol=1e-10, atol=1e-12)

    positions = sol.y[:dim].T
    velocities = sol.y[dim:].T
    return sol.t, positions, velocities


# ---------------------------------------------------------------------------
# Covariant derivative
# ---------------------------------------------------------------------------

def covariant_derivative_vector(
    vector_func: Callable[[np.ndarray], np.ndarray],
    metric_func: Callable[[np.ndarray], np.ndarray],
    point: np.ndarray,
    direction: int,
    eps: float = 1e-7
) -> np.ndarray:
    """Compute the covariant derivative ∇_μ V^ν at a point.

    ∇_μ V^ν = ∂_μ V^ν + Γ^ν_{μλ} V^λ
    """
    dim = len(point)
    V = vector_func(point)
    Gamma = christoffel_symbols_numeric(metric_func, point, eps)

    # Partial derivative ∂_μ V^ν
    p_plus = point.copy()
    p_minus = point.copy()
    p_plus[direction] += eps
    p_minus[direction] -= eps
    dV = (vector_func(p_plus) - vector_func(p_minus)) / (2.0 * eps)

    # Connection term
    connection_term = np.zeros(dim)
    for nu in range(dim):
        for lam in range(dim):
            connection_term[nu] += Gamma[nu, direction, lam] * V[lam]

    return dV + connection_term


# ---------------------------------------------------------------------------
# Parallel transport
# ---------------------------------------------------------------------------

def parallel_transport(
    metric_func: Callable[[np.ndarray], np.ndarray],
    curve: np.ndarray,  # (N, dim) array of points along the curve
    initial_vector: np.ndarray,
    eps: float = 1e-7
) -> np.ndarray:
    """Parallel transport a vector along a discrete curve.

    At each step: V^μ_{n+1} = V^μ_n - Γ^μ_{αβ} V^α Δx^β.

    Returns array of transported vectors at each curve point.
    """
    N, dim = curve.shape
    vectors = np.zeros((N, dim))
    vectors[0] = initial_vector.copy()

    for i in range(N - 1):
        x = curve[i]
        dx = curve[i + 1] - curve[i]
        Gamma = christoffel_symbols_numeric(metric_func, x, eps)
        V = vectors[i]
        # Transport equation
        dV = np.zeros(dim)
        for mu in range(dim):
            for alpha in range(dim):
                for beta in range(dim):
                    dV[mu] -= Gamma[mu, alpha, beta] * V[alpha] * dx[beta]
        vectors[i + 1] = V + dV

    return vectors


# ---------------------------------------------------------------------------
# Differential forms (numeric)
# ---------------------------------------------------------------------------

class DifferentialForm:
    """A differential p-form on ℝⁿ.

    Stores the components ω_{i₁...iₚ}(x) as a callable and provides
    exterior derivative and wedge product operations.
    """

    def __init__(self, degree: int, dim: int,
                 components: Callable[[np.ndarray], np.ndarray]):
        """
        components(x) returns array of shape matching the antisymmetric
        index structure. For a 1-form: (dim,). For a 2-form: (dim, dim).
        """
        self.degree = degree
        self.dim = dim
        self.components = components

    def evaluate(self, point: np.ndarray) -> np.ndarray:
        return self.components(point)

    def exterior_derivative(self, eps: float = 1e-7) -> DifferentialForm:
        """Compute dω (exterior derivative).

        For a p-form ω, dω is a (p+1)-form:
        (dω)_{i₀ i₁...iₚ} = Σ_k (-1)^k ∂_{i_k} ω_{i₀...î_k...iₚ}
        """
        p = self.degree
        dim = self.dim
        parent = self

        if p == 0:
            # Scalar → 1-form (gradient)
            def grad_components(x: np.ndarray) -> np.ndarray:
                result = np.zeros(dim)
                f0 = parent.evaluate(x)
                for i in range(dim):
                    xp = x.copy()
                    xm = x.copy()
                    xp[i] += eps
                    xm[i] -= eps
                    result[i] = (parent.evaluate(xp) - parent.evaluate(xm)) / (2 * eps)
                return result
            return DifferentialForm(1, dim, grad_components)

        elif p == 1:
            # 1-form → 2-form (curl-like)
            def curl_components(x: np.ndarray) -> np.ndarray:
                result = np.zeros((dim, dim))
                for i in range(dim):
                    for j in range(dim):
                        xp_i = x.copy()
                        xm_i = x.copy()
                        xp_i[i] += eps
                        xm_i[i] -= eps
                        omega_j_plus = parent.evaluate(xp_i)[j]
                        omega_j_minus = parent.evaluate(xm_i)[j]
                        d_i_omega_j = (omega_j_plus - omega_j_minus) / (2 * eps)

                        xp_j = x.copy()
                        xm_j = x.copy()
                        xp_j[j] += eps
                        xm_j[j] -= eps
                        omega_i_plus = parent.evaluate(xp_j)[i]
                        omega_i_minus = parent.evaluate(xm_j)[i]
                        d_j_omega_i = (omega_i_plus - omega_i_minus) / (2 * eps)

                        result[i, j] = d_i_omega_j - d_j_omega_i
                return result
            return DifferentialForm(2, dim, curl_components)
        else:
            raise NotImplementedError(f"Exterior derivative for {p}-forms not yet implemented")

    def hodge_dual(self, metric: Optional[np.ndarray] = None) -> DifferentialForm:
        """Compute the Hodge dual ⋆ω.

        For Euclidean metric in 3D:
        ⋆(0-form) → 3-form, ⋆(1-form) → 2-form, etc.
        """
        if metric is None:
            metric = np.eye(self.dim)
        det_g = np.linalg.det(metric)
        sqrt_det_g = np.sqrt(abs(det_g))

        if self.degree == 0 and self.dim == 3:
            # ⋆f = f √|g| dx¹∧dx²∧dx³
            parent = self
            def dual_components(x: np.ndarray) -> np.ndarray:
                return np.array([sqrt_det_g * parent.evaluate(x)])
            return DifferentialForm(3, self.dim, dual_components)

        raise NotImplementedError("Hodge dual for general (p,n) not yet implemented")


# ---------------------------------------------------------------------------
# Killing vector fields
# ---------------------------------------------------------------------------

def is_killing_vector(
    vector_func: Callable[[np.ndarray], np.ndarray],
    metric_func: Callable[[np.ndarray], np.ndarray],
    point: np.ndarray,
    dim: int,
    eps: float = 1e-6,
    tolerance: float = 1e-4
) -> bool:
    """Check if a vector field X is a Killing vector at a point.

    Killing's equation: ∇_μ X_ν + ∇_ν X_μ = 0.
    """
    g = metric_func(point)
    # Lower the index: X_μ = g_{μν} X^ν
    X_up = vector_func(point)
    X_down = g @ X_up

    def X_down_func(x: np.ndarray) -> np.ndarray:
        return metric_func(x) @ vector_func(x)

    for mu in range(dim):
        for nu in range(dim):
            # ∂_μ X_ν
            p_plus = point.copy()
            p_minus = point.copy()
            p_plus[mu] += eps
            p_minus[mu] -= eps
            dmu_Xnu = (X_down_func(p_plus)[nu] - X_down_func(p_minus)[nu]) / (2 * eps)

            p_plus = point.copy()
            p_minus = point.copy()
            p_plus[nu] += eps
            p_minus[nu] -= eps
            dnu_Xmu = (X_down_func(p_plus)[mu] - X_down_func(p_minus)[mu]) / (2 * eps)

            # Christoffel corrections
            Gamma = christoffel_symbols_numeric(metric_func, point, eps)
            corr_mu = sum(Gamma[lam, mu, nu] * X_down[lam] for lam in range(dim))
            corr_nu = sum(Gamma[lam, nu, mu] * X_down[lam] for lam in range(dim))

            killing_lhs = (dmu_Xnu - corr_mu) + (dnu_Xmu - corr_nu)
            if abs(killing_lhs) > tolerance:
                return False
    return True


# ---------------------------------------------------------------------------
# Exponential map
# ---------------------------------------------------------------------------

def exponential_map(
    metric_func: Callable[[np.ndarray], np.ndarray],
    base_point: np.ndarray,
    tangent_vector: np.ndarray,
    t: float = 1.0,
    steps: int = 100
) -> np.ndarray:
    """Compute exp_p(tv) — the exponential map at p along v.

    This is the endpoint of the geodesic starting at p with velocity v,
    evaluated at parameter t.
    """
    _, positions, _ = solve_geodesic(metric_func, base_point, tangent_vector,
                                     (0, t), steps)
    return positions[-1]
