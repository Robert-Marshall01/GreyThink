"""
Grey Physics Math Core — Variational Calculus

Functionals, Euler–Lagrange equations, action principles,
first and second variations, Legendre transforms, and
constraint handling (Lagrange multipliers).
"""

from __future__ import annotations

from typing import Any, Callable, Dict, List, Optional, Tuple
import numpy as np
from scipy.integrate import quad, trapezoid
from scipy.optimize import minimize


# ---------------------------------------------------------------------------
# Functionals
# ---------------------------------------------------------------------------

class Functional:
    """A functional F[y] = ∫ L(x, y, y', y'', ...) dx.

    Supports evaluation, variation, and Euler–Lagrange equation derivation
    in both symbolic and numeric modes.
    """

    def __init__(self, name: str,
                 integrand: Callable[..., float],
                 domain: Tuple[float, float] = (0, 1),
                 order: int = 1):
        """
        Args:
            name: Descriptive name
            integrand: L(x, y, y', y'', ...) → float
                       Called as integrand(x, y, y_prime, y_double_prime, ...)
            domain: Integration interval [a, b]
            order: Highest derivative order appearing in the integrand
        """
        self.name = name
        self.integrand = integrand
        self.domain = domain
        self.order = order

    def evaluate(self, y_func: Callable[[float], float],
                 num_points: int = 1000) -> float:
        """Evaluate F[y] = ∫_a^b L(x, y(x), y'(x), ...) dx numerically."""
        a, b = self.domain
        x = np.linspace(a, b, num_points)
        dx_val = x[1] - x[0]
        y = np.array([y_func(xi) for xi in x])
        y_prime = np.gradient(y, dx_val)

        if self.order >= 2:
            y_double_prime = np.gradient(y_prime, dx_val)
            values = np.array([
                self.integrand(x[i], y[i], y_prime[i], y_double_prime[i])
                for i in range(num_points)
            ])
        else:
            values = np.array([
                self.integrand(x[i], y[i], y_prime[i])
                for i in range(num_points)
            ])
        return float(trapezoid(values, x))

    def first_variation(self, y_func: Callable[[float], float],
                        eta_func: Callable[[float], float],
                        epsilon: float = 1e-7,
                        num_points: int = 1000) -> float:
        """Compute the first variation δF[y; η] = d/dε F[y + εη] |_{ε=0}.

        Uses central finite difference in ε.
        """
        def y_plus(x: float) -> float:
            return y_func(x) + epsilon * eta_func(x)

        def y_minus(x: float) -> float:
            return y_func(x) - epsilon * eta_func(x)

        F_plus = self.evaluate(y_plus, num_points)
        F_minus = self.evaluate(y_minus, num_points)
        return (F_plus - F_minus) / (2 * epsilon)

    def second_variation(self, y_func: Callable[[float], float],
                         eta_func: Callable[[float], float],
                         epsilon: float = 1e-5,
                         num_points: int = 1000) -> float:
        """Compute δ²F[y; η] = d²/dε² F[y + εη] |_{ε=0}."""
        def y_eps(eps: float) -> Callable[[float], float]:
            return lambda x: y_func(x) + eps * eta_func(x)

        F0 = self.evaluate(y_func, num_points)
        Fp = self.evaluate(y_eps(epsilon), num_points)
        Fm = self.evaluate(y_eps(-epsilon), num_points)
        return (Fp - 2 * F0 + Fm) / epsilon ** 2

    def is_stationary(self, y_func: Callable[[float], float],
                      num_test_functions: int = 10,
                      tolerance: float = 1e-4) -> bool:
        """Check if y is a stationary point by testing δF ≈ 0 for random η."""
        a, b = self.domain
        for k in range(1, num_test_functions + 1):
            # Test functions that vanish at boundaries
            eta = lambda x, k=k: np.sin(k * np.pi * (x - a) / (b - a))
            var = self.first_variation(y_func, eta)
            if abs(var) > tolerance:
                return False
        return True


# ---------------------------------------------------------------------------
# Euler–Lagrange equation solver (numeric)
# ---------------------------------------------------------------------------

class EulerLagrangeSolver:
    """Numerically solve the Euler–Lagrange equation for a functional.

    For F[y] = ∫ L(x, y, y') dx, the E-L equation is:
        ∂L/∂y - d/dx(∂L/∂y') = 0

    with boundary conditions y(a) = y_a, y(b) = y_b.
    """

    def __init__(self, functional: Functional,
                 bc_left: float, bc_right: float,
                 num_points: int = 100):
        self.functional = functional
        self.bc_left = bc_left
        self.bc_right = bc_right
        self.num_points = num_points
        self.a, self.b = functional.domain

    def solve_direct(self) -> Tuple[np.ndarray, np.ndarray]:
        """Solve by direct minimization of the functional.

        Parameterizes y(x) = y_boundary + Σ c_k sin(kπ(x-a)/(b-a))
        and minimizes F[y] over the coefficients c_k.
        """
        a, b = self.a, self.b
        n = self.num_points
        n_terms = min(20, n // 2)
        x_grid = np.linspace(a, b, n)

        # Boundary interpolation
        y_boundary = lambda x: (self.bc_left * (b - x) + self.bc_right * (x - a)) / (b - a)

        def y_from_coeffs(coeffs: np.ndarray) -> Callable[[float], float]:
            def y(x: float) -> float:
                val = y_boundary(x)
                for k in range(len(coeffs)):
                    val += coeffs[k] * np.sin((k + 1) * np.pi * (x - a) / (b - a))
                return val
            return y

        def objective(coeffs: np.ndarray) -> float:
            y = y_from_coeffs(coeffs)
            return self.functional.evaluate(y, n)

        c0 = np.zeros(n_terms)
        result = minimize(objective, c0, method="L-BFGS-B", options={"maxiter": 500})
        y_opt = y_from_coeffs(result.x)
        y_values = np.array([y_opt(xi) for xi in x_grid])
        return x_grid, y_values

    def solve_shooting(self, y_prime_guess: float = 0.0) -> Tuple[np.ndarray, np.ndarray]:
        """Solve the E-L equation via shooting method.

        Converts to an IVP and adjusts the initial derivative
        to satisfy the right boundary condition.
        """
        from scipy.integrate import solve_ivp
        from scipy.optimize import brentq

        a, b = self.a, self.b
        n = self.num_points

        def el_rhs(x: float, y_state: np.ndarray) -> np.ndarray:
            """Numerically approximate the E-L equation RHS.
            For L(x, y, y'), the E-L equation is:
            y'' = (∂L/∂y - ∂²L/∂x∂y') / (∂²L/∂y'²)
            Approximated via finite differences.
            """
            y_val, yp_val = y_state
            L = self.functional.integrand
            eps = 1e-7

            # ∂L/∂y
            dL_dy = (L(x, y_val + eps, yp_val) - L(x, y_val - eps, yp_val)) / (2 * eps)

            # ∂²L/∂y'²
            d2L_dyp2 = (L(x, y_val, yp_val + eps) - 2 * L(x, y_val, yp_val) +
                         L(x, y_val, yp_val - eps)) / eps ** 2

            # ∂²L/∂x∂y'
            d2L_dxdyp = ((L(x + eps, y_val, yp_val + eps) - L(x + eps, y_val, yp_val - eps)) -
                          (L(x - eps, y_val, yp_val + eps) - L(x - eps, y_val, yp_val - eps))) / (4 * eps ** 2)

            # ∂²L/∂y∂y'
            d2L_dydyp = ((L(x, y_val + eps, yp_val + eps) - L(x, y_val + eps, yp_val - eps)) -
                          (L(x, y_val - eps, yp_val + eps) - L(x, y_val - eps, yp_val - eps))) / (4 * eps ** 2)

            if abs(d2L_dyp2) < 1e-15:
                ypp = 0.0
            else:
                ypp = (dL_dy - d2L_dxdyp - d2L_dydyp * yp_val) / d2L_dyp2

            return np.array([yp_val, ypp])

        def residual(yp0: float) -> float:
            sol = solve_ivp(el_rhs, (a, b), [self.bc_left, yp0],
                            t_eval=np.linspace(a, b, n), method="RK45",
                            rtol=1e-10, atol=1e-12)
            return sol.y[0, -1] - self.bc_right

        # Find the right initial slope
        try:
            yp_opt = brentq(residual, -10, 10, xtol=1e-10)
        except ValueError:
            yp_opt = y_prime_guess

        sol = solve_ivp(el_rhs, (a, b), [self.bc_left, yp_opt],
                        t_eval=np.linspace(a, b, n), method="RK45",
                        rtol=1e-10, atol=1e-12)
        return sol.t, sol.y[0]


# ---------------------------------------------------------------------------
# Legendre transform
# ---------------------------------------------------------------------------

def legendre_transform(
    L: Callable[[np.ndarray, np.ndarray], float],
    q: np.ndarray,
    q_dot: np.ndarray,
    eps: float = 1e-7
) -> Tuple[np.ndarray, float]:
    """Compute the Legendre transform L(q, q̇) → H(q, p).

    p_i = ∂L/∂q̇_i
    H = Σ p_i q̇_i - L

    Returns (p, H_value).
    """
    n = len(q_dot)
    p = np.zeros(n)
    for i in range(n):
        qd_plus = q_dot.copy()
        qd_minus = q_dot.copy()
        qd_plus[i] += eps
        qd_minus[i] -= eps
        p[i] = (L(q, qd_plus) - L(q, qd_minus)) / (2 * eps)

    H = np.dot(p, q_dot) - L(q, q_dot)
    return p, H


# ---------------------------------------------------------------------------
# Constrained variational problems
# ---------------------------------------------------------------------------

class ConstrainedFunctional:
    """Variational problem with constraints via Lagrange multipliers.

    Minimize F[y] subject to G[y] = c.
    Augmented functional: F_λ[y] = F[y] + λ(G[y] - c).
    """

    def __init__(self, objective: Functional,
                 constraint: Functional,
                 constraint_value: float = 0.0):
        self.objective = objective
        self.constraint = constraint
        self.constraint_value = constraint_value

    def augmented_functional(self, lam: float) -> Functional:
        """F_λ = F + λ(G - c)."""
        obj = self.objective
        con = self.constraint
        c_val = self.constraint_value

        def augmented_integrand(*args: Any) -> float:
            return obj.integrand(*args) + lam * (con.integrand(*args))

        return Functional(
            f"{obj.name}+{lam}·{con.name}",
            augmented_integrand,
            obj.domain,
            max(obj.order, con.order)
        )

    def solve(self, bc_left: float, bc_right: float,
              lam_range: Tuple[float, float] = (-10, 10),
              num_points: int = 100) -> Tuple[np.ndarray, np.ndarray, float]:
        """Solve the constrained problem by scanning Lagrange multipliers."""
        best_x = None
        best_y = None
        best_lam = 0.0
        best_violation = float("inf")

        for lam in np.linspace(lam_range[0], lam_range[1], 50):
            aug = self.augmented_functional(lam)
            solver = EulerLagrangeSolver(aug, bc_left, bc_right, num_points)
            try:
                x, y = solver.solve_direct()
                # Check constraint
                y_func = lambda xi, x=x, y=y: np.interp(xi, x, y)
                g_val = self.constraint.evaluate(y_func, num_points)
                violation = abs(g_val - self.constraint_value)
                if violation < best_violation:
                    best_violation = violation
                    best_x, best_y, best_lam = x, y, lam
            except Exception:
                continue

        if best_x is None:
            raise RuntimeError("Failed to solve constrained variational problem")
        return best_x, best_y, best_lam


# ---------------------------------------------------------------------------
# Brachistochrone and classic examples
# ---------------------------------------------------------------------------

def brachistochrone_functional() -> Functional:
    """The brachistochrone functional:
    T[y] = ∫ √((1 + y'²) / (2gy)) dx

    Minimizing this gives the fastest descent curve.
    """
    g = 9.81

    def integrand(x: float, y: float, yp: float) -> float:
        if y <= 0:
            y = 1e-10
        return np.sqrt((1 + yp ** 2) / (2 * g * y))

    return Functional("Brachistochrone", integrand, (0, 1), order=1)


def geodesic_functional(metric_func: Callable[[np.ndarray], float]) -> Functional:
    """Arc length functional on a surface:
    L[γ] = ∫ √(g_{ij} dx^i/dt dx^j/dt) dt.

    Simplified to 2D: ∫ √(1 + y'² · g(x,y)) dx.
    """
    def integrand(x: float, y: float, yp: float) -> float:
        g = metric_func(np.array([x, y]))
        return np.sqrt(1 + yp ** 2 * g)

    return Functional("Geodesic", integrand, (0, 1), order=1)


def elastic_energy_functional(E: float = 1.0, I: float = 1.0) -> Functional:
    """Elastic beam bending energy:
    U[y] = ½ EI ∫ y''² dx.
    """
    def integrand(x: float, y: float, yp: float, ypp: float) -> float:
        return 0.5 * E * I * ypp ** 2

    return Functional("ElasticEnergy", integrand, (0, 1), order=2)
