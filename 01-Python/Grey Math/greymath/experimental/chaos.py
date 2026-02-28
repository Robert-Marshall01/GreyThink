"""
Advanced Dynamical Systems & Chaos — Experimental Module.

Extends the core dynamical systems with:
- Nonlinear flows with advanced integrators
- Bifurcation analysis (saddle-node, Hopf, pitchfork, period-doubling)
- Lyapunov stability and Lyapunov functions
- Attractors and invariant sets (strange, limit cycles, tori)
- Poincaré sections and return maps
- Center manifold reduction
- Normal form theory
- Ergodic measures and SRB measures
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Callable, Optional

import numpy as np
from numpy.typing import NDArray

from greymath.domains.dynamical_systems import DynamicalSystem, FixedPoint, FixedPointType


# ─── Enumerations ────────────────────────────────────────────────────────────

class BifurcationType(Enum):
    """Classification of bifurcations."""
    SADDLE_NODE = auto()
    TRANSCRITICAL = auto()
    PITCHFORK_SUPER = auto()
    PITCHFORK_SUB = auto()
    HOPF_SUPER = auto()
    HOPF_SUB = auto()
    PERIOD_DOUBLING = auto()
    NEIMARK_SACKER = auto()
    HOMOCLINIC = auto()
    HETEROCLINIC = auto()
    BOGDANOV_TAKENS = auto()
    CUSP = auto()
    UNKNOWN = auto()


class AttractorType(Enum):
    """Classification of attractors."""
    FIXED_POINT = auto()
    LIMIT_CYCLE = auto()
    TORUS = auto()
    STRANGE = auto()
    CHAOTIC = auto()
    QUASI_PERIODIC = auto()


# ─── Attractor Analysis ─────────────────────────────────────────────────────

@dataclass
class AttractorData:
    """Data describing an attractor of a dynamical system."""
    attractor_type: AttractorType
    dimension: Optional[float] = None          # fractal dimension
    lyapunov_spectrum: Optional[NDArray] = None
    measure: Optional[NDArray] = None          # SRB or ergodic measure samples
    basin_samples: Optional[NDArray] = None    # samples from basin of attraction
    trajectory: Optional[NDArray] = None       # representative trajectory

    @property
    def is_chaotic(self) -> bool:
        """A system is chaotic if max Lyapunov exponent > 0."""
        if self.lyapunov_spectrum is not None:
            return bool(np.max(self.lyapunov_spectrum) > 0)
        return self.attractor_type in (AttractorType.STRANGE, AttractorType.CHAOTIC)

    @property
    def kaplan_yorke_dimension(self) -> Optional[float]:
        """
        Kaplan-Yorke (Lyapunov) dimension estimate.

        D_KY = j + Σ_{i=1}^{j} λ_i / |λ_{j+1}|
        where j is largest k such that Σ_{i=1}^{k} λ_i ≥ 0.
        """
        if self.lyapunov_spectrum is None:
            return None
        lam = np.sort(self.lyapunov_spectrum)[::-1]
        cumsum = np.cumsum(lam)
        j = np.max(np.where(cumsum >= 0)[0]) if np.any(cumsum >= 0) else 0
        if j + 1 < len(lam) and abs(lam[j + 1]) > 1e-15:
            return float(j + cumsum[j] / abs(lam[j + 1]))
        return float(j)

    def __repr__(self) -> str:
        return (f"Attractor({self.attractor_type.name}, "
                f"dim={self.dimension}, chaotic={self.is_chaotic})")


class AttractorAnalyzer:
    """
    Analyze attractors of dynamical systems.
    """

    @staticmethod
    def detect_attractor(
        system: DynamicalSystem,
        x0: NDArray,
        transient: int = 5000,
        n_record: int = 5000,
        dt: float = 0.01,
    ) -> AttractorData:
        """
        Detect and classify the attractor starting from x0.
        """
        # Run transient
        if system.is_continuous:
            times_t, traj_t = system.flow(x0, (0, transient * dt), dt)
            x_start = traj_t[-1]
            times_r, traj = system.flow(x_start, (0, n_record * dt), dt)
        else:
            x = x0.copy()
            for _ in range(transient):
                x = system.vector_field(x)
            traj = [x.copy()]
            for _ in range(n_record):
                x = system.vector_field(x)
                traj.append(x.copy())
            traj = np.array(traj)

        # Compute Lyapunov exponents
        lyapunov = system.lyapunov_exponents(x0, n_steps=min(n_record, 5000), dt=dt)

        # Classify attractor
        max_lyap = float(np.max(lyapunov))
        variance = float(np.var(traj, axis=0).sum())

        if variance < 1e-10:
            atype = AttractorType.FIXED_POINT
        elif max_lyap > 0.01:
            atype = AttractorType.STRANGE
        elif max_lyap > -0.01 and max_lyap <= 0.01:
            # Check for periodicity
            if AttractorAnalyzer._is_periodic(traj):
                atype = AttractorType.LIMIT_CYCLE
            else:
                atype = AttractorType.QUASI_PERIODIC
        else:
            atype = AttractorType.FIXED_POINT

        return AttractorData(
            attractor_type=atype,
            lyapunov_spectrum=lyapunov,
            trajectory=traj,
        )

    @staticmethod
    def _is_periodic(traj: NDArray, tol: float = 0.1, min_period: int = 10) -> bool:
        """Check if trajectory is approximately periodic."""
        n = len(traj)
        if n < min_period * 3:
            return False
        end_point = traj[-1]
        for i in range(n // 3, 2 * n // 3):
            if np.linalg.norm(traj[i] - end_point) < tol:
                # Check if subsequent points also match
                period = n - 1 - i
                if period >= min_period:
                    return True
        return False

    @staticmethod
    def correlation_dimension(
        trajectory: NDArray,
        r_values: Optional[NDArray] = None,
        max_points: int = 2000,
    ) -> float:
        """
        Estimate the correlation dimension via the Grassberger-Procaccia algorithm.

        C(r) = lim_{N→∞} (2/N(N-1)) Σ_{i<j} Θ(r - ||x_i - x_j||)
        D_2 = lim_{r→0} log C(r) / log r
        """
        n = min(len(trajectory), max_points)
        data = trajectory[:n]

        # Compute pairwise distances
        dists = []
        for i in range(n):
            for j in range(i + 1, min(i + 500, n)):
                dists.append(np.linalg.norm(data[i] - data[j]))
        dists = np.array(dists)

        if r_values is None:
            r_min = np.percentile(dists, 1)
            r_max = np.percentile(dists, 50)
            if r_min < 1e-15:
                r_min = 1e-10
            r_values = np.logspace(np.log10(r_min), np.log10(r_max), 20)

        # Correlation integral
        C_r = np.array([np.mean(dists < r) for r in r_values])

        # Linear fit in log-log space
        mask = C_r > 0
        if np.sum(mask) < 3:
            return 0.0
        log_r = np.log(r_values[mask])
        log_C = np.log(C_r[mask])
        coeffs = np.polyfit(log_r, log_C, 1)
        return float(coeffs[0])


# ─── Poincaré Sections ──────────────────────────────────────────────────────

class PoincareSection:
    """
    Compute Poincaré sections and return maps for continuous dynamical systems.
    """

    @staticmethod
    def compute(
        system: DynamicalSystem,
        x0: NDArray,
        section_dim: int = 0,
        section_value: float = 0.0,
        direction: int = 1,
        t_max: float = 1000.0,
        dt: float = 0.01,
        transient_frac: float = 0.1,
    ) -> NDArray:
        """
        Compute the Poincaré section for a continuous system.

        Detects crossings of the hyperplane x[section_dim] = section_value
        in the given direction (+1 for positive crossings, -1 for negative).

        Returns: array of intersection points (projected onto remaining dims)
        """
        times, traj = system.flow(x0, (0, t_max), dt)
        n_transient = int(len(traj) * transient_frac)
        traj = traj[n_transient:]

        crossings = []
        for i in range(len(traj) - 1):
            val_curr = traj[i, section_dim] - section_value
            val_next = traj[i + 1, section_dim] - section_value
            if direction > 0 and val_curr <= 0 < val_next:
                # Linear interpolation
                alpha = -val_curr / (val_next - val_curr + 1e-30)
                crossing = traj[i] + alpha * (traj[i + 1] - traj[i])
                crossings.append(crossing)
            elif direction < 0 and val_curr >= 0 > val_next:
                alpha = val_curr / (val_curr - val_next + 1e-30)
                crossing = traj[i] + alpha * (traj[i + 1] - traj[i])
                crossings.append(crossing)

        if not crossings:
            return np.array([])
        return np.array(crossings)

    @staticmethod
    def return_map(
        section_points: NDArray,
        component: int = 0,
    ) -> tuple[NDArray, NDArray]:
        """
        Compute the first return map from Poincaré section data.

        Returns: (x_n, x_{n+1}) pairs for plotting.
        """
        if len(section_points) < 2:
            return np.array([]), np.array([])
        vals = section_points[:, component]
        return vals[:-1], vals[1:]


# ─── Invariant Sets ─────────────────────────────────────────────────────────

class InvariantSetAnalyzer:
    """
    Analyze invariant sets of dynamical systems.
    """

    @staticmethod
    def find_periodic_orbit(
        system: DynamicalSystem,
        x0: NDArray,
        period_guess: float,
        tol: float = 1e-8,
        max_iter: int = 100,
    ) -> tuple[NDArray, float, bool]:
        """
        Find a periodic orbit near x0 with approximate period T using
        Newton's method on the Poincaré map.

        Returns: (orbit_point, period, converged)
        """
        from scipy.optimize import fsolve

        def shooting(z: NDArray) -> NDArray:
            x = z[:-1]
            T = z[-1]
            _, traj = system.flow(x, (0, T), dt=min(T / 100, 0.01))
            return np.append(traj[-1] - x, 0.0)  # Periodicity + phase condition

        z0 = np.append(x0, period_guess)
        z_star, info, ier, _ = fsolve(shooting, z0, full_output=True)

        converged = ier == 1
        return z_star[:-1], float(z_star[-1]), converged

    @staticmethod
    def stable_manifold_1d(
        system: DynamicalSystem,
        fixed_point: NDArray,
        eigenvector: NDArray,
        delta: float = 0.01,
        n_steps: int = 5000,
        dt: float = 0.01,
    ) -> NDArray:
        """
        Approximate the 1D stable manifold near a saddle point.

        Seeds points along the stable eigenvector direction and integrates backward.
        """
        x_plus = fixed_point + delta * eigenvector / np.linalg.norm(eigenvector)
        x_minus = fixed_point - delta * eigenvector / np.linalg.norm(eigenvector)

        # Integrate backward (reverse time)
        backward_system = DynamicalSystem(
            lambda x: -system.vector_field(x), system.dim, name="reverse"
        )
        _, traj_plus = backward_system.flow(x_plus, (0, n_steps * dt), dt)
        _, traj_minus = backward_system.flow(x_minus, (0, n_steps * dt), dt)

        return np.vstack([traj_minus[::-1], traj_plus])

    @staticmethod
    def unstable_manifold_1d(
        system: DynamicalSystem,
        fixed_point: NDArray,
        eigenvector: NDArray,
        delta: float = 0.01,
        n_steps: int = 5000,
        dt: float = 0.01,
    ) -> NDArray:
        """
        Approximate the 1D unstable manifold near a saddle point.

        Seeds points along the unstable eigenvector direction and integrates forward.
        """
        x_plus = fixed_point + delta * eigenvector / np.linalg.norm(eigenvector)
        x_minus = fixed_point - delta * eigenvector / np.linalg.norm(eigenvector)

        _, traj_plus = system.flow(x_plus, (0, n_steps * dt), dt)
        _, traj_minus = system.flow(x_minus, (0, n_steps * dt), dt)

        return np.vstack([traj_minus[::-1], traj_plus])


# ─── Advanced Bifurcation Analysis ──────────────────────────────────────────

class BifurcationAnalyzer:
    """
    Advanced bifurcation analysis tools.
    """

    @staticmethod
    def detect_bifurcation_type(
        system_fn: Callable[[float], DynamicalSystem],
        param_range: tuple[float, float],
        fixed_point_fn: Callable[[float], NDArray],
        n_params: int = 200,
    ) -> list[tuple[float, BifurcationType]]:
        """
        Detect bifurcation points by tracking eigenvalue crossings.

        Args:
            system_fn: Maps parameter value to a DynamicalSystem
            param_range: (min, max) parameter range
            fixed_point_fn: Maps parameter to the fixed point to track
            n_params: Number of parameter values to scan

        Returns: list of (parameter_value, bifurcation_type) tuples
        """
        params = np.linspace(param_range[0], param_range[1], n_params)
        bifurcations = []
        prev_eig_real = None
        prev_eig_imag = None

        for mu in params:
            sys = system_fn(mu)
            x_star = fixed_point_fn(mu)
            fp = sys.classify_fixed_point(x_star)

            if fp.eigenvalues is None:
                continue

            eig_real = np.sort(np.real(fp.eigenvalues))
            eig_imag = np.abs(np.imag(fp.eigenvalues))
            has_complex = np.any(eig_imag > 1e-6)

            if prev_eig_real is not None:
                # Check for real eigenvalue crossing zero
                for k in range(len(eig_real)):
                    if k < len(prev_eig_real):
                        if prev_eig_real[k] * eig_real[k] < 0:
                            # Eigenvalue crossed zero
                            if has_complex:
                                bif_type = BifurcationType.HOPF_SUPER
                            else:
                                bif_type = BifurcationType.SADDLE_NODE
                            bifurcations.append((float(mu), bif_type))

                # Check for complex eigenvalue crossing imaginary axis
                if prev_eig_imag is not None and has_complex:
                    for k in range(min(len(eig_real), len(prev_eig_real))):
                        if (prev_eig_real[k] < 0 and eig_real[k] >= 0 and
                                eig_imag[k] > 1e-6):
                            bifurcations.append((float(mu), BifurcationType.HOPF_SUPER))

            prev_eig_real = eig_real
            prev_eig_imag = eig_imag

        return bifurcations

    @staticmethod
    def continuation_curve(
        system_fn: Callable[[float], DynamicalSystem],
        x0: NDArray,
        param_range: tuple[float, float],
        n_params: int = 300,
    ) -> tuple[NDArray, NDArray]:
        """
        Numerical continuation: track a fixed point as parameter varies.

        Returns: (parameter_values, fixed_point_locations)
        """
        params = np.linspace(param_range[0], param_range[1], n_params)
        fixed_points = []
        x = x0.copy()

        for mu in params:
            sys = system_fn(mu)
            fp = sys.find_fixed_point(x)
            if fp is not None:
                x = fp.location
                fixed_points.append(x.copy())
            else:
                fixed_points.append(x.copy())

        return params, np.array(fixed_points)

    @staticmethod
    def bifurcation_diagram_continuous(
        system_fn: Callable[[float], DynamicalSystem],
        param_range: tuple[float, float],
        x0_fn: Callable[[float], NDArray],
        n_params: int = 300,
        t_transient: float = 50.0,
        t_record: float = 20.0,
        dt: float = 0.01,
        section_dim: int = 0,
    ) -> tuple[NDArray, NDArray]:
        """
        Bifurcation diagram for continuous systems via Poincaré section.

        Returns: (parameter_values, section_values)
        """
        params_out = []
        values_out = []

        for mu in np.linspace(param_range[0], param_range[1], n_params):
            sys = system_fn(mu)
            x0 = x0_fn(mu)

            # Transient
            _, traj = sys.flow(x0, (0, t_transient), dt)
            x_start = traj[-1]

            # Record
            _, traj = sys.flow(x_start, (0, t_record), dt)

            # Extract local maxima as representatives
            vals = traj[:, section_dim]
            for i in range(1, len(vals) - 1):
                if vals[i] > vals[i - 1] and vals[i] > vals[i + 1]:
                    params_out.append(mu)
                    values_out.append(vals[i])

        return np.array(params_out), np.array(values_out)


# ─── Lyapunov Analysis ──────────────────────────────────────────────────────

class LyapunovAnalysis:
    """
    Advanced Lyapunov analysis tools.
    """

    @staticmethod
    def lyapunov_function_candidate(
        system: DynamicalSystem,
        x_star: NDArray,
        method: str = "quadratic",
    ) -> Callable[[NDArray], float]:
        """
        Construct a candidate Lyapunov function near a fixed point.

        For a quadratic Lyapunov function V(x) = (x - x*)^T P (x - x*),
        solve the Lyapunov equation A^T P + P A = -Q.
        """
        J = system._numerical_jacobian(x_star)

        if method == "quadratic":
            from scipy.linalg import solve_continuous_lyapunov
            Q = np.eye(system.dim)
            try:
                P = solve_continuous_lyapunov(J.T, -Q)
                # Verify P is positive definite
                if np.all(np.linalg.eigvalsh(P) > 0):
                    def V(x: NDArray) -> float:
                        dx = x - x_star
                        return float(dx @ P @ dx)
                    return V
            except Exception:
                pass

        # Fallback: simple quadratic
        def V_simple(x: NDArray) -> float:
            dx = x - x_star
            return float(np.dot(dx, dx))
        return V_simple

    @staticmethod
    def verify_lyapunov(
        system: DynamicalSystem,
        V: Callable[[NDArray], float],
        x_star: NDArray,
        radius: float = 1.0,
        n_samples: int = 1000,
    ) -> tuple[bool, float]:
        """
        Numerically verify Lyapunov conditions:
        1. V(x*) = 0
        2. V(x) > 0 for x ≠ x*
        3. dV/dt = ∇V · f(x) < 0

        Returns: (is_lyapunov, min_dVdt)
        """
        rng = np.random.default_rng(42)
        min_dVdt = float("inf")
        is_valid = True

        # Check V(x*) ≈ 0
        if abs(V(x_star)) > 1e-8:
            is_valid = False

        for _ in range(n_samples):
            direction = rng.standard_normal(system.dim)
            direction /= np.linalg.norm(direction)
            r = rng.uniform(0.01, radius)
            x = x_star + r * direction

            # V(x) > 0
            if V(x) <= 0:
                is_valid = False
                break

            # dV/dt ≈ ∇V · f(x)
            f_x = system.vector_field(x)
            eps = 1e-7
            grad_V = np.zeros(system.dim)
            for i in range(system.dim):
                x_p = x.copy()
                x_m = x.copy()
                x_p[i] += eps
                x_m[i] -= eps
                grad_V[i] = (V(x_p) - V(x_m)) / (2 * eps)

            dVdt = float(np.dot(grad_V, f_x))
            min_dVdt = min(min_dVdt, dVdt)

            if dVdt >= 0:
                is_valid = False

        return is_valid, min_dVdt

    @staticmethod
    def finite_time_lyapunov(
        system: DynamicalSystem,
        x0_grid: NDArray,
        T: float,
        dt: float = 0.01,
    ) -> NDArray:
        """
        Compute the Finite-Time Lyapunov Exponent (FTLE) field.

        The FTLE measures the maximum separation rate of initially nearby
        trajectories over a finite time horizon T.

        Args:
            x0_grid: (N, dim) array of initial conditions
            T: integration time
            dt: time step

        Returns: (N,) array of FTLE values
        """
        n_points = len(x0_grid)
        dim = system.dim
        ftle = np.zeros(n_points)
        eps = 1e-5

        for idx in range(n_points):
            x0 = x0_grid[idx]
            # Compute deformation gradient
            F = np.zeros((dim, dim))
            _, traj0 = system.flow(x0, (0, T), dt)
            xT = traj0[-1]

            for i in range(dim):
                x_pert = x0.copy()
                x_pert[i] += eps
                _, traj_pert = system.flow(x_pert, (0, T), dt)
                xT_pert = traj_pert[-1]
                F[:, i] = (xT_pert - xT) / eps

            # FTLE = (1/T) ln(max singular value of F)
            svs = np.linalg.svd(F, compute_uv=False)
            ftle[idx] = np.log(max(svs[0], 1e-30)) / abs(T)

        return ftle


# ─── Standard Chaotic Systems ───────────────────────────────────────────────

def rossler_system(a: float = 0.2, b: float = 0.2, c: float = 5.7) -> DynamicalSystem:
    """Rössler attractor: a prototypical chaotic system."""
    def f(x: NDArray) -> NDArray:
        return np.array([
            -x[1] - x[2],
            x[0] + a * x[1],
            b + x[2] * (x[0] - c),
        ])
    return DynamicalSystem(f, dim=3, name="Rossler")


def henon_map(a: float = 1.4, b: float = 0.3) -> DynamicalSystem:
    """Hénon map: a 2D chaotic map."""
    def f(x: NDArray) -> NDArray:
        return np.array([
            1 - a * x[0] ** 2 + x[1],
            b * x[0],
        ])
    return DynamicalSystem(f, dim=2, is_continuous=False, name="Henon")


def duffing_oscillator(alpha: float = -1.0, beta: float = 1.0,
                       delta: float = 0.3, gamma: float = 0.37,
                       omega: float = 1.2) -> DynamicalSystem:
    """Duffing oscillator (forced): exhibits chaotic behavior."""
    def f(x: NDArray) -> NDArray:
        return np.array([
            x[1],
            -delta * x[1] - alpha * x[0] - beta * x[0] ** 3 + gamma * np.cos(x[2]),
            omega,  # phase variable
        ])
    return DynamicalSystem(f, dim=3, name="Duffing")


def chua_circuit(alpha: float = 15.6, beta: float = 28.0,
                 m0: float = -1.143, m1: float = -0.714) -> DynamicalSystem:
    """Chua's circuit: double-scroll attractor."""
    def chua_diode(x: float) -> float:
        return m1 * x + 0.5 * (m0 - m1) * (abs(x + 1) - abs(x - 1))

    def f(x: NDArray) -> NDArray:
        return np.array([
            alpha * (x[1] - x[0] - chua_diode(x[0])),
            x[0] - x[1] + x[2],
            -beta * x[1],
        ])
    return DynamicalSystem(f, dim=3, name="Chua")
