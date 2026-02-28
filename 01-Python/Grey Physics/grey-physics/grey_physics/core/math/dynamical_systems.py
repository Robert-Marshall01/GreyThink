"""
Grey Physics Math Core — Dynamical Systems Theory

Flows, fixed points, stability analysis, bifurcation theory,
Lyapunov analysis, and Poincaré maps.
"""

from __future__ import annotations

from typing import Any, Callable, Dict, List, Optional, Tuple
import numpy as np
from scipy.integrate import solve_ivp
from scipy.optimize import fsolve


# ---------------------------------------------------------------------------
# Phase portrait analysis
# ---------------------------------------------------------------------------

class PhasePortrait:
    """Compute and analyze the phase portrait of a 2D dynamical system.

    Provides streamlines, nullclines, and fixed-point analysis.
    """

    def __init__(self, f: Callable[[np.ndarray, float], np.ndarray],
                 x_range: Tuple[float, float] = (-5, 5),
                 y_range: Tuple[float, float] = (-5, 5),
                 resolution: int = 20):
        self.f = f
        self.x_range = x_range
        self.y_range = y_range
        self.resolution = resolution

    def vector_field(self) -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
        """Compute the vector field on a grid.

        Returns (X, Y, U, V) for quiver plotting.
        """
        x = np.linspace(self.x_range[0], self.x_range[1], self.resolution)
        y = np.linspace(self.y_range[0], self.y_range[1], self.resolution)
        X, Y = np.meshgrid(x, y)
        U = np.zeros_like(X)
        V = np.zeros_like(Y)
        for i in range(self.resolution):
            for j in range(self.resolution):
                state = np.array([X[i, j], Y[i, j]])
                dstate = self.f(state, 0)
                U[i, j] = dstate[0]
                V[i, j] = dstate[1]
        return X, Y, U, V

    def nullclines(self, component: int = 0, levels: List[float] = [0.0],
                   resolution: int = 200) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
        """Compute nullclines (where ẋ_i = 0) as contour data."""
        x = np.linspace(self.x_range[0], self.x_range[1], resolution)
        y = np.linspace(self.y_range[0], self.y_range[1], resolution)
        X, Y = np.meshgrid(x, y)
        Z = np.zeros_like(X)
        for i in range(resolution):
            for j in range(resolution):
                state = np.array([X[i, j], Y[i, j]])
                Z[i, j] = self.f(state, 0)[component]
        return X, Y, Z

    def trajectory(self, x0: np.ndarray, t_span: Tuple[float, float] = (0, 20),
                   num_points: int = 1000) -> Tuple[np.ndarray, np.ndarray]:
        """Compute a single trajectory."""
        t_eval = np.linspace(t_span[0], t_span[1], num_points)
        sol = solve_ivp(lambda t, y: self.f(y, t), t_span, x0,
                        t_eval=t_eval, method="RK45",
                        rtol=1e-8, atol=1e-10)
        return sol.t, sol.y.T

    def basin_of_attraction(self, attractor: np.ndarray,
                            t_max: float = 50.0,
                            resolution: int = 50,
                            tolerance: float = 0.5) -> np.ndarray:
        """Estimate the basin of attraction of an attractor.

        Returns a grid of booleans indicating whether each initial
        condition converges to the attractor.
        """
        x = np.linspace(self.x_range[0], self.x_range[1], resolution)
        y = np.linspace(self.y_range[0], self.y_range[1], resolution)
        basin = np.zeros((resolution, resolution), dtype=bool)
        for i in range(resolution):
            for j in range(resolution):
                try:
                    _, traj = self.trajectory(np.array([x[j], y[i]]),
                                              (0, t_max), 100)
                    endpoint = traj[-1]
                    if np.linalg.norm(endpoint - attractor) < tolerance:
                        basin[i, j] = True
                except Exception:
                    pass
        return basin


# ---------------------------------------------------------------------------
# Stability analysis
# ---------------------------------------------------------------------------

def linear_stability(jacobian: np.ndarray) -> Dict[str, Any]:
    """Analyze linear stability from a Jacobian matrix.

    Returns eigenvalues, eigenvectors, stability classification,
    and characteristic timescales.
    """
    eigenvalues = np.linalg.eigvals(jacobian)
    eigenvectors = np.linalg.eig(jacobian)[1]

    real_parts = np.real(eigenvalues)
    imag_parts = np.imag(eigenvalues)

    is_stable = np.all(real_parts < 0)
    is_unstable = np.any(real_parts > 0)
    has_oscillations = np.any(np.abs(imag_parts) > 1e-10)

    # Characteristic timescales
    timescales = []
    for ev in eigenvalues:
        if abs(ev.real) > 1e-15:
            timescales.append(1.0 / abs(ev.real))
        if abs(ev.imag) > 1e-15:
            timescales.append(2 * np.pi / abs(ev.imag))

    # Classification
    if len(eigenvalues) == 2:
        r = real_parts
        i = imag_parts
        if np.all(r < -1e-10):
            if np.any(np.abs(i) > 1e-10):
                classification = "stable_spiral"
            else:
                classification = "stable_node"
        elif np.all(r > 1e-10):
            if np.any(np.abs(i) > 1e-10):
                classification = "unstable_spiral"
            else:
                classification = "unstable_node"
        elif np.any(r > 1e-10) and np.any(r < -1e-10):
            classification = "saddle"
        elif np.all(np.abs(r) < 1e-10):
            classification = "center"
        else:
            classification = "degenerate"
    else:
        classification = "stable" if is_stable else "unstable" if is_unstable else "marginal"

    return {
        "eigenvalues": eigenvalues,
        "eigenvectors": eigenvectors,
        "is_stable": is_stable,
        "is_unstable": is_unstable,
        "has_oscillations": has_oscillations,
        "classification": classification,
        "timescales": timescales,
    }


# ---------------------------------------------------------------------------
# Bifurcation analysis
# ---------------------------------------------------------------------------

class BifurcationAnalyzer:
    """Analyze bifurcations in parameterized dynamical systems.

    For ẋ = f(x; μ), tracks fixed points and their stability
    as μ varies.
    """

    def __init__(self, f_factory: Callable[[float], Callable[[np.ndarray, float], np.ndarray]],
                 dim: int):
        """
        Args:
            f_factory: mu → f(x, t) factory function
            dim: state space dimension
        """
        self.f_factory = f_factory
        self.dim = dim

    def continuation(self, param_range: Tuple[float, float],
                     num_params: int = 200,
                     initial_guesses: Optional[List[np.ndarray]] = None) -> Dict[str, Any]:
        """Numerical continuation of fixed points over parameter range.

        Returns parameter values, fixed point locations, and eigendata.
        """
        params = np.linspace(param_range[0], param_range[1], num_params)
        if initial_guesses is None:
            initial_guesses = [np.zeros(self.dim)]

        results: Dict[str, Any] = {
            "params": params,
            "fixed_points": [],
            "eigenvalues": [],
            "stability": [],
        }

        for mu in params:
            f = self.f_factory(mu)
            fps_at_mu = []
            eigs_at_mu = []
            stab_at_mu = []

            for guess in initial_guesses:
                try:
                    x_star = fsolve(lambda x: f(x, 0), guess, full_output=False)
                    # Check it's actually a fixed point
                    if np.linalg.norm(f(x_star, 0)) < 1e-8:
                        # Compute Jacobian
                        J = np.zeros((self.dim, self.dim))
                        for j in range(self.dim):
                            e = np.zeros(self.dim)
                            e[j] = 1e-7
                            J[:, j] = (f(x_star + e, 0) - f(x_star - e, 0)) / (2e-7)
                        eigvals = np.linalg.eigvals(J)
                        fps_at_mu.append(x_star.copy())
                        eigs_at_mu.append(eigvals)
                        stab_at_mu.append(np.all(np.real(eigvals) < 0))
                        # Update guess for continuation
                        guess[:] = x_star
                except Exception:
                    pass

            results["fixed_points"].append(fps_at_mu)
            results["eigenvalues"].append(eigs_at_mu)
            results["stability"].append(stab_at_mu)

        return results

    def bifurcation_diagram_1d(self, param_range: Tuple[float, float],
                               x0: np.ndarray,
                               t_transient: float = 100.0,
                               t_record: float = 50.0,
                               dt: float = 0.01,
                               num_params: int = 500,
                               component: int = 0) -> Tuple[np.ndarray, List[np.ndarray]]:
        """Generate bifurcation diagram by sampling long-time attractor.

        Records the values of x[component] after transient decay.
        """
        params = np.linspace(param_range[0], param_range[1], num_params)
        attractors: List[np.ndarray] = []

        state = x0.copy()
        for mu in params:
            f = self.f_factory(mu)
            # Transient
            sol = solve_ivp(lambda t, y: f(y, t), (0, t_transient), state,
                            method="RK45", rtol=1e-6)
            state = sol.y[:, -1]
            # Record
            sol = solve_ivp(lambda t, y: f(y, t), (0, t_record), state,
                            t_eval=np.linspace(0, t_record, 1000),
                            method="RK45", rtol=1e-6)
            # Sample local maxima
            x_trace = sol.y[component]
            maxima = []
            for i in range(1, len(x_trace) - 1):
                if x_trace[i] > x_trace[i - 1] and x_trace[i] > x_trace[i + 1]:
                    maxima.append(x_trace[i])
            attractors.append(np.array(maxima) if maxima else np.array([x_trace[-1]]))

        return params, attractors


# ---------------------------------------------------------------------------
# Lyapunov spectrum
# ---------------------------------------------------------------------------

def lyapunov_spectrum(
    f: Callable[[np.ndarray, float], np.ndarray],
    x0: np.ndarray,
    dim: int,
    t_total: float = 200.0,
    dt: float = 0.01,
    t_transient: float = 50.0,
    eps: float = 1e-7
) -> np.ndarray:
    """Compute the full Lyapunov spectrum via continuous QR method.

    Uses Benettin's algorithm with periodic Gram-Schmidt orthogonalization.

    Returns sorted array of Lyapunov exponents (largest first).
    """
    # Skip transient
    state = x0.copy()
    n_transient = int(t_transient / dt)
    for _ in range(n_transient):
        k1 = f(state, 0)
        k2 = f(state + 0.5 * dt * k1, 0)
        k3 = f(state + 0.5 * dt * k2, 0)
        k4 = f(state + dt * k3, 0)
        state += (dt / 6) * (k1 + 2 * k2 + 2 * k3 + k4)

    # Initialize perturbation matrix
    Q = np.eye(dim)
    lyap_sum = np.zeros(dim)
    n_steps = int(t_total / dt)
    t = 0.0
    renorm_interval = max(1, int(1.0 / dt))

    for step in range(n_steps):
        # Advance state (RK4)
        k1 = f(state, t)
        k2 = f(state + 0.5 * dt * k1, t + 0.5 * dt)
        k3 = f(state + 0.5 * dt * k2, t + 0.5 * dt)
        k4 = f(state + dt * k3, t + dt)
        state += (dt / 6) * (k1 + 2 * k2 + 2 * k3 + k4)

        # Compute Jacobian
        J = np.zeros((dim, dim))
        f0 = f(state, t)
        for j in range(dim):
            e = np.zeros(dim)
            e[j] = eps
            J[:, j] = (f(state + e, t) - f0) / eps

        # Advance perturbation vectors
        Q = Q + dt * (J @ Q)

        # Periodic QR renormalization
        if (step + 1) % renorm_interval == 0:
            Q, R = np.linalg.qr(Q)
            lyap_sum += np.log(np.abs(np.diag(R)) + 1e-300)

        t += dt

    exponents = lyap_sum / t_total
    return np.sort(exponents)[::-1]


# ---------------------------------------------------------------------------
# Poincaré return map
# ---------------------------------------------------------------------------

class PoincareMap:
    """Compute and analyze Poincaré return maps."""

    def __init__(self, f: Callable[[np.ndarray, float], np.ndarray],
                 section_dim: int = 0,
                 section_value: float = 0.0,
                 crossing_direction: int = 1):
        """
        Args:
            f: RHS of the ODE ẋ = f(x, t)
            section_dim: which component defines the section
            section_value: x[section_dim] = section_value
            crossing_direction: +1 for positive crossing, -1 for negative
        """
        self.f = f
        self.section_dim = section_dim
        self.section_value = section_value
        self.crossing_direction = crossing_direction

    def compute(self, x0: np.ndarray, t_max: float = 500.0,
                dt: float = 0.001,
                max_crossings: int = 10000) -> np.ndarray:
        """Compute crossings through the Poincaré section."""
        dim = len(x0)
        crossings = []
        state = x0.copy()
        t = 0.0
        prev_val = state[self.section_dim] - self.section_value

        while t < t_max and len(crossings) < max_crossings:
            k1 = self.f(state, t)
            k2 = self.f(state + 0.5 * dt * k1, t + 0.5 * dt)
            k3 = self.f(state + 0.5 * dt * k2, t + 0.5 * dt)
            k4 = self.f(state + dt * k3, t + dt)
            new_state = state + (dt / 6) * (k1 + 2 * k2 + 2 * k3 + k4)
            new_val = new_state[self.section_dim] - self.section_value

            if self.crossing_direction > 0 and prev_val < 0 and new_val >= 0:
                # Interpolate
                frac = -prev_val / (new_val - prev_val + 1e-300)
                crossing = state + frac * (new_state - state)
                crossings.append(crossing)
            elif self.crossing_direction < 0 and prev_val > 0 and new_val <= 0:
                frac = prev_val / (prev_val - new_val + 1e-300)
                crossing = state + frac * (new_state - state)
                crossings.append(crossing)

            state = new_state
            prev_val = new_val
            t += dt

        return np.array(crossings) if crossings else np.empty((0, dim))

    def return_map_1d(self, x0: np.ndarray, component: int,
                      **kwargs: Any) -> Tuple[np.ndarray, np.ndarray]:
        """Extract 1D return map: x_{n+1} vs x_n for a given component."""
        crossings = self.compute(x0, **kwargs)
        if len(crossings) < 2:
            return np.array([]), np.array([])
        vals = crossings[:, component]
        return vals[:-1], vals[1:]
