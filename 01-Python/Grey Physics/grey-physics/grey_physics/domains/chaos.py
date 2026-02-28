"""
Grey Physics — Chaos Domain

Nonlinear dynamics and chaos:
  - Strange attractors (Lorenz, Rössler, Chua, Chen, Thomas)
  - Bifurcation analysis (discrete & continuous)
  - Lyapunov spectrum computation
  - Fractal dimension (box-counting, correlation)
  - Iterated maps (logistic, Hénon, standard)
  - Poincaré sections and return maps
  - Recurrence analysis (recurrence plots, quantification)
"""

from __future__ import annotations

import numpy as np
from typing import Callable, Dict, List, Optional, Tuple

from grey_physics.core.numeric import ODESolver


# ============================================================
# Strange Attractors
# ============================================================

class StrangeAttractor:
    """Simulate and analyse strange attractors."""

    def __init__(self, equations: Callable[[float, np.ndarray], np.ndarray],
                 dim: int, name: str = "attractor"):
        self.equations = equations
        self.dim = dim
        self.name = name
        self.solver = ODESolver()

    def simulate(self, x0: np.ndarray,
                 t_span: Tuple[float, float],
                 dt: float = 0.01,
                 method: str = "RK4") -> Dict[str, np.ndarray]:
        result = self.solver.solve(self.equations, x0, t_span, dt, method)
        return {"t": result["t"], "trajectory": result["y"]}

    # ============================================================
    # Factory methods
    # ============================================================

    @staticmethod
    def lorenz(sigma: float = 10.0, rho: float = 28.0,
               beta: float = 8.0/3) -> StrangeAttractor:
        """Lorenz system:
        ẋ = σ(y-x), ẏ = x(ρ-z)-y, ż = xy - βz
        """
        def f(t, state):
            x, y, z = state
            return np.array([
                sigma * (y - x),
                x * (rho - z) - y,
                x * y - beta * z,
            ])
        return StrangeAttractor(f, 3, "Lorenz")

    @staticmethod
    def rossler(a: float = 0.2, b: float = 0.2,
                c: float = 5.7) -> StrangeAttractor:
        """Rössler system:
        ẋ = -y-z, ẏ = x+ay, ż = b + z(x-c)
        """
        def f(t, state):
            x, y, z = state
            return np.array([
                -y - z,
                x + a * y,
                b + z * (x - c),
            ])
        return StrangeAttractor(f, 3, "Rössler")

    @staticmethod
    def chen(a: float = 35.0, b: float = 3.0,
             c: float = 28.0) -> StrangeAttractor:
        """Chen system:
        ẋ = a(y-x), ẏ = (c-a)x - xz + cy, ż = xy - bz
        """
        def f(t, state):
            x, y, z = state
            return np.array([
                a * (y - x),
                (c - a) * x - x * z + c * y,
                x * y - b * z,
            ])
        return StrangeAttractor(f, 3, "Chen")

    @staticmethod
    def chua(alpha: float = 15.6, beta: float = 28.0,
             m0: float = -1.143, m1: float = -0.714) -> StrangeAttractor:
        """Chua's circuit:
        ẋ = α(y - x - h(x)), ẏ = x - y + z, ż = -βy
        h(x) = m1*x + ½(m0-m1)(|x+1| - |x-1|)
        """
        def f(t, state):
            x, y, z = state
            h = m1 * x + 0.5 * (m0 - m1) * (abs(x + 1) - abs(x - 1))
            return np.array([
                alpha * (y - x - h),
                x - y + z,
                -beta * y,
            ])
        return StrangeAttractor(f, 3, "Chua")

    @staticmethod
    def thomas(b: float = 0.208186) -> StrangeAttractor:
        """Thomas' cyclically symmetric attractor:
        ẋ = sin(y) - bx, ẏ = sin(z) - by, ż = sin(x) - bz
        """
        def f(t, state):
            x, y, z = state
            return np.array([
                np.sin(y) - b * x,
                np.sin(z) - b * y,
                np.sin(x) - b * z,
            ])
        return StrangeAttractor(f, 3, "Thomas")

    @staticmethod
    def halvorsen(a: float = 1.89) -> StrangeAttractor:
        """Halvorsen attractor."""
        def f(t, state):
            x, y, z = state
            return np.array([
                -a*x - 4*y - 4*z - y**2,
                -a*y - 4*z - 4*x - z**2,
                -a*z - 4*x - 4*y - x**2,
            ])
        return StrangeAttractor(f, 3, "Halvorsen")

    @staticmethod
    def aizawa(a: float = 0.95, b: float = 0.7, c: float = 0.6,
               d: float = 3.5, e: float = 0.25,
               f_param: float = 0.1) -> StrangeAttractor:
        """Aizawa attractor."""
        def f(t, state):
            x, y, z = state
            return np.array([
                (z - b) * x - d * y,
                d * x + (z - b) * y,
                c + a * z - z**3/3 - (x**2 + y**2) * (1 + e*z) + f_param*z*x**3,
            ])
        return StrangeAttractor(f, 3, "Aizawa")


# ============================================================
# Lyapunov Exponent Computation
# ============================================================

class LyapunovComputer:
    """Compute Lyapunov exponents for continuous-time dynamical systems."""

    def __init__(self, equations: Callable, jacobian: Callable, dim: int):
        """
        Parameters
        ----------
        equations : f(t, x) -> dx/dt
        jacobian : J(t, x) -> (dim, dim) Jacobian matrix
        dim : phase space dimension
        """
        self.f = equations
        self.J = jacobian
        self.dim = dim

    def compute(self, x0: np.ndarray,
                t_total: float = 100.0,
                dt: float = 0.01,
                transient: float = 10.0) -> np.ndarray:
        """Compute the full Lyapunov spectrum.

        Uses QR decomposition method with intermittent orthonormalization.
        """
        d = self.dim
        n_steps = int(t_total / dt)
        n_transient = int(transient / dt)
        renorm_steps = max(1, int(1.0 / dt))

        # State: position + perturbation vectors (d columns of d×d matrix)
        x = x0.copy()
        Q = np.eye(d)  # initial perturbation basis

        # Discard transient
        solver = ODESolver()
        for _ in range(n_transient):
            k1 = np.asarray(self.f(0, x), dtype=float)
            k2 = np.asarray(self.f(0, x + 0.5*dt*k1), dtype=float)
            k3 = np.asarray(self.f(0, x + 0.5*dt*k2), dtype=float)
            k4 = np.asarray(self.f(0, x + dt*k3), dtype=float)
            x += (dt/6) * (k1 + 2*k2 + 2*k3 + k4)

        # Compute Lyapunov exponents
        lyap_sum = np.zeros(d)
        n_renorm = 0

        for step in range(n_steps):
            # Evolve state
            k1 = np.asarray(self.f(0, x), dtype=float)
            k2 = np.asarray(self.f(0, x + 0.5*dt*k1), dtype=float)
            k3 = np.asarray(self.f(0, x + 0.5*dt*k2), dtype=float)
            k4 = np.asarray(self.f(0, x + dt*k3), dtype=float)
            x += (dt/6) * (k1 + 2*k2 + 2*k3 + k4)

            # Evolve perturbation vectors
            J = self.J(0, x)
            Q = Q + dt * J @ Q

            # Periodically orthonormalize
            if (step + 1) % renorm_steps == 0:
                Q, R = np.linalg.qr(Q)
                lyap_sum += np.log(np.abs(np.diag(R)))
                n_renorm += 1

        if n_renorm == 0:
            return np.zeros(d)
        return lyap_sum / (n_renorm * renorm_steps * dt)

    @staticmethod
    def lorenz_jacobian(sigma: float = 10.0, rho: float = 28.0,
                         beta: float = 8/3) -> Callable:
        """Jacobian of the Lorenz system."""
        def J(t, state):
            x, y, z = state
            return np.array([
                [-sigma, sigma, 0],
                [rho - z, -1, -x],
                [y, x, -beta],
            ])
        return J


# ============================================================
# Iterated Maps
# ============================================================

class IteratedMap:
    """Discrete-time iterated maps."""

    @staticmethod
    def logistic(r: float, x0: float = 0.5,
                 n_iter: int = 1000,
                 n_transient: int = 200) -> np.ndarray:
        """Logistic map: x_{n+1} = r x_n (1 - x_n)."""
        x = x0
        for _ in range(n_transient):
            x = r * x * (1 - x)

        trajectory = np.zeros(n_iter)
        for n in range(n_iter):
            x = r * x * (1 - x)
            trajectory[n] = x
        return trajectory

    @staticmethod
    def henon(a: float = 1.4, b: float = 0.3,
              x0: float = 0.0, y0: float = 0.0,
              n_iter: int = 10000) -> np.ndarray:
        """Hénon map: x_{n+1} = 1 - ax_n² + y_n, y_{n+1} = bx_n."""
        trajectory = np.zeros((n_iter, 2))
        x, y = x0, y0
        for n in range(n_iter):
            x_new = 1 - a * x**2 + y
            y_new = b * x
            x, y = x_new, y_new
            trajectory[n] = [x, y]
        return trajectory

    @staticmethod
    def standard_map(K: float = 0.9, n_iter: int = 10000,
                     n_orbits: int = 20) -> np.ndarray:
        """Chirikov standard map:
        p_{n+1} = p_n + K sin(θ_n)
        θ_{n+1} = θ_n + p_{n+1}   (mod 2π)
        """
        all_points = []
        for _ in range(n_orbits):
            theta = np.random.uniform(0, 2 * np.pi)
            p = np.random.uniform(0, 2 * np.pi)
            for _ in range(n_iter):
                p = (p + K * np.sin(theta)) % (2 * np.pi)
                theta = (theta + p) % (2 * np.pi)
                all_points.append([theta, p])
        return np.array(all_points)

    @staticmethod
    def tent_map(mu: float = 2.0, x0: float = 0.4,
                 n_iter: int = 1000) -> np.ndarray:
        """Tent map: x_{n+1} = μ min(x_n, 1-x_n)."""
        trajectory = np.zeros(n_iter)
        x = x0
        for n in range(n_iter):
            x = mu * min(x, 1 - x)
            trajectory[n] = x
        return trajectory

    @staticmethod
    def baker_map(x0: float = 0.3, y0: float = 0.7,
                  n_iter: int = 1000) -> np.ndarray:
        """Baker's map on the unit square."""
        trajectory = np.zeros((n_iter, 2))
        x, y = x0, y0
        for n in range(n_iter):
            if x < 0.5:
                x, y = 2*x, y/2
            else:
                x, y = 2 - 2*x, 1 - y/2
            trajectory[n] = [x, y]
        return trajectory


# ============================================================
# Bifurcation Diagrams
# ============================================================

class BifurcationDiagram:
    """Generate bifurcation diagrams for maps and flows."""

    @staticmethod
    def logistic_bifurcation(r_range: Tuple[float, float] = (2.5, 4.0),
                              n_r: int = 1000,
                              n_iter: int = 300,
                              n_last: int = 100) -> Tuple[np.ndarray, np.ndarray]:
        """Bifurcation diagram of the logistic map.

        Returns (r_values, x_values) for plotting.
        """
        r_values = np.linspace(r_range[0], r_range[1], n_r)
        r_out = []
        x_out = []

        for r in r_values:
            x = 0.5
            for _ in range(n_iter - n_last):
                x = r * x * (1 - x)
            for _ in range(n_last):
                x = r * x * (1 - x)
                r_out.append(r)
                x_out.append(x)

        return np.array(r_out), np.array(x_out)

    @staticmethod
    def continuous_bifurcation(equations: Callable,
                                param_range: Tuple[float, float],
                                param_index: int,
                                base_params: Dict,
                                x0: np.ndarray,
                                n_params: int = 200,
                                t_transient: float = 50.0,
                                t_sample: float = 50.0,
                                dt: float = 0.01,
                                observable_index: int = 0) -> Tuple[np.ndarray, np.ndarray]:
        """Bifurcation diagram for continuous-time system.

        Varies one parameter and records Poincaré-like samples.
        """
        solver = ODESolver()
        p_values = np.linspace(param_range[0], param_range[1], n_params)
        p_out = []
        x_out = []

        for p in p_values:
            def f(t, state, _p=p):
                return equations(t, state, _p)

            # Transient
            result = solver.solve(f, x0, (0, t_transient), dt, "RK4")
            x_final = result["y"][-1]

            # Sample
            result = solver.solve(f, x_final, (0, t_sample), dt, "RK4")
            trajectory = result["y"][:, observable_index]

            # Find local maxima as "samples"
            for i in range(1, len(trajectory) - 1):
                if trajectory[i] > trajectory[i-1] and trajectory[i] > trajectory[i+1]:
                    p_out.append(p)
                    x_out.append(trajectory[i])

        return np.array(p_out), np.array(x_out)


# ============================================================
# Fractal Dimension
# ============================================================

class FractalDimension:
    """Compute fractal dimensions."""

    @staticmethod
    def box_counting(points: np.ndarray,
                      n_scales: int = 20) -> Tuple[float, np.ndarray, np.ndarray]:
        """Box-counting dimension.

        Returns (dimension, log_epsilons, log_counts).
        """
        # Normalize to [0,1]^d
        mins = points.min(axis=0)
        maxs = points.max(axis=0)
        ranges = maxs - mins
        ranges[ranges == 0] = 1
        normalized = (points - mins) / ranges

        epsilons = np.logspace(-3, 0, n_scales)
        counts = []

        for eps in epsilons:
            # Count non-empty boxes
            grid_indices = (normalized / eps).astype(int)
            unique = set(map(tuple, grid_indices))
            counts.append(len(unique))

        log_eps = np.log(1.0 / epsilons)
        log_N = np.log(np.array(counts, dtype=float))

        # Linear fit
        valid = np.isfinite(log_eps) & np.isfinite(log_N)
        if np.sum(valid) >= 2:
            coeffs = np.polyfit(log_eps[valid], log_N[valid], 1)
            dimension = coeffs[0]
        else:
            dimension = 0.0

        return dimension, log_eps, log_N

    @staticmethod
    def correlation_dimension(points: np.ndarray,
                               n_scales: int = 20,
                               max_pairs: int = 5000) -> float:
        """Grassberger-Procaccia correlation dimension.

        C(r) = (2/N(N-1)) #{(i,j) : |x_i - x_j| < r}
        D_2 = lim_{r→0} log C(r) / log r
        """
        N = min(len(points), max_pairs)
        subset = points[np.random.choice(len(points), N, replace=False)]

        # Compute pairwise distances
        dists = []
        for i in range(N):
            for j in range(i + 1, N):
                dists.append(np.linalg.norm(subset[i] - subset[j]))
        dists = np.array(dists)

        r_min = np.percentile(dists, 1)
        r_max = np.percentile(dists, 90)
        radii = np.logspace(np.log10(max(r_min, 1e-10)), np.log10(r_max), n_scales)

        C = np.array([np.sum(dists < r) / len(dists) for r in radii])

        # Fit in scaling region
        valid = C > 0
        if np.sum(valid) < 3:
            return 0.0
        log_r = np.log(radii[valid])
        log_C = np.log(C[valid])
        coeffs = np.polyfit(log_r, log_C, 1)
        return coeffs[0]


# ============================================================
# Poincaré Section
# ============================================================

class PoincareSection:
    """Compute Poincaré sections for continuous flows."""

    @staticmethod
    def compute(trajectory: np.ndarray,
                section_axis: int = 2,
                section_value: float = 0.0,
                direction: str = "positive") -> np.ndarray:
        """Extract Poincaré section crossings.

        Parameters
        ----------
        trajectory : (N, dim) array
        section_axis : which axis defines the section
        section_value : value at which section is taken
        direction : "positive" (crossing upward) or "both"
        """
        crossings = []
        vals = trajectory[:, section_axis]

        for i in range(len(vals) - 1):
            if direction == "positive":
                if vals[i] < section_value and vals[i+1] >= section_value:
                    # Linear interpolation
                    frac = (section_value - vals[i]) / (vals[i+1] - vals[i])
                    point = trajectory[i] + frac * (trajectory[i+1] - trajectory[i])
                    crossings.append(point)
            else:
                if (vals[i] - section_value) * (vals[i+1] - section_value) < 0:
                    frac = (section_value - vals[i]) / (vals[i+1] - vals[i])
                    point = trajectory[i] + frac * (trajectory[i+1] - trajectory[i])
                    crossings.append(point)

        return np.array(crossings) if crossings else np.array([]).reshape(0, trajectory.shape[1])


# ============================================================
# Recurrence Analysis
# ============================================================

class RecurrenceAnalysis:
    """Recurrence plots and quantification analysis (RQA)."""

    @staticmethod
    def recurrence_matrix(trajectory: np.ndarray,
                           threshold: float = 0.1) -> np.ndarray:
        """Compute recurrence matrix R_{ij} = Θ(ε - ||x_i - x_j||)."""
        N = len(trajectory)
        R = np.zeros((N, N), dtype=bool)
        for i in range(N):
            dists = np.linalg.norm(trajectory[i] - trajectory, axis=1)
            R[i] = dists < threshold
        return R

    @staticmethod
    def recurrence_rate(R: np.ndarray) -> float:
        """RR = (1/N²) Σ R_{ij}."""
        return float(np.sum(R)) / R.size

    @staticmethod
    def determinism(R: np.ndarray, l_min: int = 2) -> float:
        """DET = fraction of recurrence points forming diagonal lines ≥ l_min."""
        N = R.shape[0]
        diag_lengths = []

        for k in range(-N + 1, N):
            diag = np.diag(R, k)
            length = 0
            for val in diag:
                if val:
                    length += 1
                else:
                    if length >= l_min:
                        diag_lengths.append(length)
                    length = 0
            if length >= l_min:
                diag_lengths.append(length)

        total_recurrence = np.sum(R)
        if total_recurrence == 0:
            return 0.0
        return sum(diag_lengths) / total_recurrence
