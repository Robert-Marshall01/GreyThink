"""
Dynamical Systems Module.

Provides:
- Continuous and discrete dynamical systems
- Fixed-point analysis
- Stability and bifurcation analysis
- Lyapunov exponents
- Phase portraits
- Flows and orbits
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Callable, Optional

import numpy as np
from numpy.typing import NDArray

from greymath.numeric.ode import ODESolver, ODEMethod


class FixedPointType(Enum):
    """Classification of fixed points."""
    STABLE_NODE = auto()
    UNSTABLE_NODE = auto()
    STABLE_SPIRAL = auto()
    UNSTABLE_SPIRAL = auto()
    CENTER = auto()
    SADDLE = auto()
    STABLE_FOCUS = auto()
    UNSTABLE_FOCUS = auto()
    UNKNOWN = auto()


@dataclass
class FixedPoint:
    """A fixed point of a dynamical system with stability info."""
    location: NDArray
    jacobian: Optional[NDArray] = None
    eigenvalues: Optional[NDArray] = None
    classification: FixedPointType = FixedPointType.UNKNOWN
    is_stable: Optional[bool] = None

    def __repr__(self) -> str:
        return (f"FixedPoint(x={self.location}, type={self.classification.name}, "
                f"stable={self.is_stable})")


@dataclass
class BifurcationPoint:
    """A bifurcation point in a parameter-dependent system."""
    parameter_value: float
    parameter_name: str = "μ"
    bifurcation_type: str = "unknown"  # "saddle-node", "Hopf", "pitchfork", etc.
    fixed_points_before: list[FixedPoint] = field(default_factory=list)
    fixed_points_after: list[FixedPoint] = field(default_factory=list)

    def __repr__(self) -> str:
        return (f"Bifurcation({self.bifurcation_type} at "
                f"{self.parameter_name}={self.parameter_value})")


class DynamicalSystem:
    """
    A continuous dynamical system dx/dt = f(x) or discrete system x_{n+1} = f(x_n).
    """

    def __init__(
        self,
        vector_field: Callable[[NDArray], NDArray],
        dim: int,
        is_continuous: bool = True,
        name: str = "",
    ) -> None:
        self.vector_field = vector_field
        self.dim = dim
        self.is_continuous = is_continuous
        self.name = name
        self._ode_solver = ODESolver()

    # ── Trajectory computation ──────────────────────────────────────────

    def flow(self, x0: NDArray, t_span: tuple[float, float],
             dt: float = 0.01) -> tuple[NDArray, NDArray]:
        """
        Compute the flow φ_t(x0) for t ∈ t_span.

        Returns: (times, trajectory) where trajectory[i] = φ_{t_i}(x0)
        """
        if self.is_continuous:
            sol = self._ode_solver.solve(
                lambda t, y: self.vector_field(y),
                t_span, x0, method=ODEMethod.RK45_ADAPTIVE, dt=dt,
            )
            return sol.t, sol.y
        else:
            # Discrete: iterate
            n_steps = int((t_span[1] - t_span[0]) / dt)
            trajectory = [x0.copy()]
            x = x0.copy()
            for _ in range(n_steps):
                x = self.vector_field(x)
                trajectory.append(x.copy())
            return np.arange(n_steps + 1) * dt + t_span[0], np.array(trajectory)

    def orbit(self, x0: NDArray, n_steps: int) -> NDArray:
        """Compute discrete orbit for n_steps iterations."""
        trajectory = [x0.copy()]
        x = x0.copy()
        for _ in range(n_steps):
            x = self.vector_field(x)
            trajectory.append(x.copy())
        return np.array(trajectory)

    # ── Fixed-point analysis ────────────────────────────────────────────

    def find_fixed_point(self, x0: NDArray, tol: float = 1e-10,
                         max_iter: int = 1000) -> Optional[FixedPoint]:
        """Find a fixed point near x0 using Newton's method."""
        from scipy.optimize import fsolve

        if self.is_continuous:
            # Fixed point: f(x*) = 0
            x_star, info, ier, _ = fsolve(
                self.vector_field, x0, full_output=True
            )
        else:
            # Fixed point: g(x*) = x* where g = vector_field
            x_star, info, ier, _ = fsolve(
                lambda x: self.vector_field(x) - x, x0, full_output=True
            )

        if ier != 1:
            return None

        return self.classify_fixed_point(x_star)

    def classify_fixed_point(self, x_star: NDArray,
                              eps: float = 1e-7) -> FixedPoint:
        """Classify a fixed point by linearization (Jacobian eigenvalues)."""
        J = self._numerical_jacobian(x_star, eps)
        eigenvalues = np.linalg.eigvals(J)

        real_parts = np.real(eigenvalues)
        imag_parts = np.imag(eigenvalues)
        has_imaginary = np.any(np.abs(imag_parts) > 1e-10)

        if self.is_continuous:
            if np.all(real_parts < -1e-10):
                if has_imaginary:
                    cls = FixedPointType.STABLE_SPIRAL
                else:
                    cls = FixedPointType.STABLE_NODE
                is_stable = True
            elif np.all(real_parts > 1e-10):
                if has_imaginary:
                    cls = FixedPointType.UNSTABLE_SPIRAL
                else:
                    cls = FixedPointType.UNSTABLE_NODE
                is_stable = False
            elif np.any(real_parts > 1e-10) and np.any(real_parts < -1e-10):
                cls = FixedPointType.SADDLE
                is_stable = False
            elif np.all(np.abs(real_parts) < 1e-10) and has_imaginary:
                cls = FixedPointType.CENTER
                is_stable = None  # Lyapunov stable but not asymptotically
            else:
                cls = FixedPointType.UNKNOWN
                is_stable = None
        else:
            # Discrete: stability if |λ| < 1
            magnitudes = np.abs(eigenvalues)
            if np.all(magnitudes < 1 - 1e-10):
                cls = FixedPointType.STABLE_NODE
                is_stable = True
            elif np.all(magnitudes > 1 + 1e-10):
                cls = FixedPointType.UNSTABLE_NODE
                is_stable = False
            else:
                cls = FixedPointType.SADDLE
                is_stable = False

        return FixedPoint(
            location=x_star,
            jacobian=J,
            eigenvalues=eigenvalues,
            classification=cls,
            is_stable=is_stable,
        )

    # ── Lyapunov exponents ──────────────────────────────────────────────

    def lyapunov_exponents(self, x0: NDArray, n_steps: int = 10000,
                           dt: float = 0.01) -> NDArray:
        """
        Estimate Lyapunov exponents via QR decomposition method.
        """
        n = self.dim
        x = x0.copy()
        Q = np.eye(n)
        lyap = np.zeros(n)

        for step in range(n_steps):
            if self.is_continuous:
                J = self._numerical_jacobian(x)
                # Evolve tangent vectors
                dQ = J @ Q * dt
                Q = Q + dQ
            else:
                J = self._numerical_jacobian(x)
                Q = J @ Q

            # QR reorthogonalization
            Q, R = np.linalg.qr(Q)
            lyap += np.log(np.abs(np.diag(R)))

            # Evolve the state
            if self.is_continuous:
                x = x + self.vector_field(x) * dt
            else:
                x = self.vector_field(x)

        total_time = n_steps * dt if self.is_continuous else n_steps
        return lyap / total_time

    # ── Bifurcation analysis ───────────────────────────────────────────

    def bifurcation_diagram(
        self,
        param_range: tuple[float, float],
        n_params: int = 200,
        x0_fn: Callable[[float], NDArray] = lambda mu: np.array([0.1]),
        transient: int = 500,
        n_record: int = 100,
    ) -> tuple[NDArray, NDArray]:
        """
        Compute bifurcation diagram by parameter continuation.

        Returns: (parameter_values, attracting_values)
        """
        params = np.linspace(param_range[0], param_range[1], n_params)
        all_params = []
        all_values = []

        for mu in params:
            x = x0_fn(mu)
            # Transient
            for _ in range(transient):
                x = self.vector_field(x)
            # Record
            for _ in range(n_record):
                x = self.vector_field(x)
                all_params.append(mu)
                all_values.append(x[0] if len(x) > 0 else float(x))

        return np.array(all_params), np.array(all_values)

    # ── Utilities ──────────────────────────────────────────────────────

    def _numerical_jacobian(self, x: NDArray, eps: float = 1e-7) -> NDArray:
        """Compute Jacobian via central differences."""
        n = len(x)
        f0 = self.vector_field(x)
        m = len(f0)
        J = np.zeros((m, n))
        for i in range(n):
            x_plus = x.copy()
            x_minus = x.copy()
            x_plus[i] += eps
            x_minus[i] -= eps
            J[:, i] = (self.vector_field(x_plus) - self.vector_field(x_minus)) / (2 * eps)
        return J

    def __repr__(self) -> str:
        kind = "continuous" if self.is_continuous else "discrete"
        return f"DynamicalSystem({self.name}, dim={self.dim}, {kind})"


# ─── Standard Systems ──────────────────────────────────────────────────────

def lorenz_system(sigma: float = 10.0, rho: float = 28.0,
                  beta: float = 8 / 3) -> DynamicalSystem:
    """Create the Lorenz system."""
    def f(x: NDArray) -> NDArray:
        return np.array([
            sigma * (x[1] - x[0]),
            x[0] * (rho - x[2]) - x[1],
            x[0] * x[1] - beta * x[2],
        ])
    return DynamicalSystem(f, dim=3, name="Lorenz")


def logistic_map(r: float = 3.5) -> DynamicalSystem:
    """Create the logistic map x_{n+1} = r x_n (1 - x_n)."""
    def f(x: NDArray) -> NDArray:
        return np.array([r * x[0] * (1 - x[0])])
    return DynamicalSystem(f, dim=1, is_continuous=False, name="Logistic")


def van_der_pol(mu: float = 1.0) -> DynamicalSystem:
    """Create the Van der Pol oscillator."""
    def f(x: NDArray) -> NDArray:
        return np.array([
            x[1],
            mu * (1 - x[0] ** 2) * x[1] - x[0],
        ])
    return DynamicalSystem(f, dim=2, name="VanDerPol")
