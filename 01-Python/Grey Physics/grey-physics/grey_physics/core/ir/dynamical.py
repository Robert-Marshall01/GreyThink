"""
Grey Physics IR — Dynamical Systems, Flows, Vector Fields on Manifolds

Infrastructure for analyzing dynamical systems: fixed points, stability,
flows, bifurcations, and Lyapunov analysis.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Callable, Dict, List, Optional, Tuple
import numpy as np

from grey_physics.core.ir.types import (
    IRNode, Expr, ExprKind, Metadata, DomainType, func,
)


class FixedPointType(Enum):
    """Classification of a fixed point."""
    STABLE_NODE = auto()
    UNSTABLE_NODE = auto()
    SADDLE = auto()
    STABLE_SPIRAL = auto()
    UNSTABLE_SPIRAL = auto()
    CENTER = auto()
    DEGENERATE = auto()
    UNKNOWN = auto()


@dataclass
class FixedPoint:
    """A fixed point x* of ẋ = f(x) with stability classification."""
    location: np.ndarray
    eigenvalues: Optional[np.ndarray] = None
    classification: FixedPointType = FixedPointType.UNKNOWN
    jacobian: Optional[np.ndarray] = None

    def classify(self) -> FixedPointType:
        """Classify based on eigenvalues of the Jacobian."""
        if self.eigenvalues is None:
            return FixedPointType.UNKNOWN
        real_parts = np.real(self.eigenvalues)
        imag_parts = np.imag(self.eigenvalues)
        has_imag = np.any(np.abs(imag_parts) > 1e-10)

        if np.all(real_parts < -1e-10):
            self.classification = (FixedPointType.STABLE_SPIRAL if has_imag
                                   else FixedPointType.STABLE_NODE)
        elif np.all(real_parts > 1e-10):
            self.classification = (FixedPointType.UNSTABLE_SPIRAL if has_imag
                                   else FixedPointType.UNSTABLE_NODE)
        elif np.any(real_parts > 1e-10) and np.any(real_parts < -1e-10):
            self.classification = FixedPointType.SADDLE
        elif np.all(np.abs(real_parts) < 1e-10) and has_imag:
            self.classification = FixedPointType.CENTER
        else:
            self.classification = FixedPointType.DEGENERATE
        return self.classification


class BifurcationType(Enum):
    """Type of bifurcation."""
    SADDLE_NODE = auto()
    TRANSCRITICAL = auto()
    PITCHFORK = auto()
    HOPF = auto()
    PERIOD_DOUBLING = auto()
    CUSTOM = auto()


@dataclass
class Bifurcation:
    """A bifurcation event in a parameterized dynamical system."""
    parameter_value: float
    bifurcation_type: BifurcationType
    fixed_points_before: List[FixedPoint]
    fixed_points_after: List[FixedPoint]


class DynamicalSystem(IRNode):
    """A dynamical system ẋ = f(x; μ) where x ∈ ℝⁿ.

    Supports:
    - Fixed point finding and classification
    - Jacobian computation
    - Lyapunov exponent estimation
    - Bifurcation detection
    """

    def __init__(self, name: str,
                 dim: int,
                 rhs: Optional[List[Expr]] = None,
                 rhs_numeric: Optional[Callable[[np.ndarray, float], np.ndarray]] = None,
                 parameters: Optional[Dict[str, float]] = None,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata or Metadata(
            domain=DomainType.CHAOS
        ))
        self.dim = dim
        self.rhs = rhs or []
        self.rhs_numeric = rhs_numeric
        self.parameters = parameters or {}
        self._fixed_points: List[FixedPoint] = []

    @staticmethod
    def from_equations(name: str, equations: List[Expr],
                       variables: List[Expr]) -> DynamicalSystem:
        """Build a dynamical system from symbolic equations ẋ_i = f_i(x)."""
        return DynamicalSystem(name, len(variables), equations)

    @staticmethod
    def from_callable(name: str, dim: int,
                      f: Callable[[np.ndarray, float], np.ndarray],
                      params: Optional[Dict[str, float]] = None) -> DynamicalSystem:
        """Build from a numeric callable f(x, t) → dx/dt."""
        return DynamicalSystem(name, dim, rhs_numeric=f, parameters=params or {})

    @staticmethod
    def lorenz(sigma: float = 10.0, rho: float = 28.0,
               beta: float = 8.0 / 3.0) -> DynamicalSystem:
        """Lorenz system:
        ẋ = σ(y-x), ẏ = x(ρ-z)-y, ż = xy-βz.
        """
        def f(state: np.ndarray, t: float) -> np.ndarray:
            x, y, z = state
            return np.array([
                sigma * (y - x),
                x * (rho - z) - y,
                x * y - beta * z,
            ])
        ds = DynamicalSystem("Lorenz", 3, rhs_numeric=f,
                             parameters={"σ": sigma, "ρ": rho, "β": beta})
        # Also set symbolic
        x, y, z = Expr.symbol("x"), Expr.symbol("y"), Expr.symbol("z")
        s, r, b = Expr.constant(sigma), Expr.constant(rho), Expr.constant(beta)
        ds.rhs = [
            s * (y - x),
            x * (r - z) - y,
            x * y - b * z,
        ]
        return ds

    @staticmethod
    def rossler(a: float = 0.2, b: float = 0.2,
                c: float = 5.7) -> DynamicalSystem:
        """Rössler system: ẋ = -y-z, ẏ = x+ay, ż = b+z(x-c)."""
        def f(state: np.ndarray, t: float) -> np.ndarray:
            x, y, z = state
            return np.array([
                -y - z,
                x + a * y,
                b + z * (x - c),
            ])
        return DynamicalSystem("Rössler", 3, rhs_numeric=f,
                               parameters={"a": a, "b": b, "c": c})

    @staticmethod
    def duffing(alpha: float = 1.0, beta: float = -1.0,
                delta: float = 0.3, gamma: float = 0.37,
                omega: float = 1.2) -> DynamicalSystem:
        """Duffing oscillator: ẍ + δẋ + αx + βx³ = γcos(ωt)."""
        def f(state: np.ndarray, t: float) -> np.ndarray:
            x, v = state
            return np.array([
                v,
                -delta * v - alpha * x - beta * x ** 3 + gamma * np.cos(omega * t),
            ])
        return DynamicalSystem("Duffing", 2, rhs_numeric=f,
                               parameters={"α": alpha, "β": beta, "δ": delta,
                                            "γ": gamma, "ω": omega})

    @staticmethod
    def van_der_pol(mu: float = 1.0) -> DynamicalSystem:
        """Van der Pol oscillator: ẍ - μ(1-x²)ẋ + x = 0."""
        def f(state: np.ndarray, t: float) -> np.ndarray:
            x, y = state
            return np.array([
                y,
                mu * (1 - x ** 2) * y - x,
            ])
        return DynamicalSystem("VanDerPol", 2, rhs_numeric=f,
                               parameters={"μ": mu})

    @staticmethod
    def pendulum(length: float = 1.0, g: float = 9.81,
                 damping: float = 0.0) -> DynamicalSystem:
        """Simple pendulum: θ̈ + (γ)θ̇ + (g/l)sin(θ) = 0."""
        def f(state: np.ndarray, t: float) -> np.ndarray:
            theta, omega = state
            return np.array([
                omega,
                -damping * omega - (g / length) * np.sin(theta),
            ])
        return DynamicalSystem("Pendulum", 2, rhs_numeric=f,
                               parameters={"L": length, "g": g, "γ": damping})

    def evaluate(self, state: np.ndarray, t: float = 0.0) -> np.ndarray:
        """Evaluate ẋ = f(x, t)."""
        if self.rhs_numeric is not None:
            return self.rhs_numeric(state, t)
        raise ValueError("No numeric RHS defined")

    def jacobian_numeric(self, state: np.ndarray, t: float = 0.0,
                         eps: float = 1e-7) -> np.ndarray:
        """Compute Jacobian ∂f_i/∂x_j numerically via finite differences."""
        n = self.dim
        J = np.zeros((n, n))
        f0 = self.evaluate(state, t)
        for j in range(n):
            state_p = state.copy()
            state_p[j] += eps
            fp = self.evaluate(state_p, t)
            J[:, j] = (fp - f0) / eps
        return J

    def find_fixed_points_numeric(self, guesses: List[np.ndarray],
                                  t: float = 0.0) -> List[FixedPoint]:
        """Find fixed points f(x*) = 0 numerically from initial guesses."""
        from scipy.optimize import fsolve
        fps: List[FixedPoint] = []
        seen = set()
        for guess in guesses:
            try:
                sol = fsolve(lambda x: self.evaluate(x, t), guess, full_output=True)
                x_star = sol[0]
                info = sol[1]
                key = tuple(np.round(x_star, 6))
                if key not in seen:
                    seen.add(key)
                    J = self.jacobian_numeric(x_star, t)
                    eigvals = np.linalg.eigvals(J)
                    fp = FixedPoint(x_star, eigvals, jacobian=J)
                    fp.classify()
                    fps.append(fp)
            except Exception:
                pass
        self._fixed_points = fps
        return fps

    def lyapunov_exponents(self, x0: np.ndarray, t_total: float = 100.0,
                           dt: float = 0.01) -> np.ndarray:
        """Estimate Lyapunov exponents via QR decomposition method.

        Integrates the system and its tangent map simultaneously.
        """
        n = self.dim
        steps = int(t_total / dt)
        # Initialize orthonormal perturbation vectors
        Q = np.eye(n)
        lyap_sum = np.zeros(n)
        state = x0.copy()
        t = 0.0

        for step in range(steps):
            # Advance state (Euler for simplicity in the estimator)
            f = self.evaluate(state, t)
            state = state + dt * f
            # Advance tangent vectors
            J = self.jacobian_numeric(state, t)
            Q = Q + dt * (J @ Q)
            # QR decomposition for re-orthogonalization
            Q, R = np.linalg.qr(Q)
            lyap_sum += np.log(np.abs(np.diag(R)) + 1e-300)
            t += dt

        return lyap_sum / t_total

    def canonical_form(self) -> str:
        return f"DynamicalSystem({self.name}, dim={self.dim})"


class Flow(IRNode):
    """The flow map φ_t: M → M of a dynamical system.

    φ_t(x₀) gives the state at time t starting from x₀.
    """

    def __init__(self, name: str,
                 dynamical_system: DynamicalSystem,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata)
        self.dynamical_system = dynamical_system
        self._trajectory_cache: Dict[str, np.ndarray] = {}

    def compute(self, x0: np.ndarray, t_span: Tuple[float, float],
                dt: float = 0.01) -> Tuple[np.ndarray, np.ndarray]:
        """Compute the flow trajectory via RK4 integration.

        Returns (times, states) where states[i] = φ_{times[i]}(x0).
        """
        t0, tf = t_span
        steps = int((tf - t0) / dt)
        times = np.linspace(t0, tf, steps + 1)
        states = np.zeros((steps + 1, self.dynamical_system.dim))
        states[0] = x0

        state = x0.copy()
        for i in range(steps):
            t = times[i]
            h = dt
            k1 = self.dynamical_system.evaluate(state, t)
            k2 = self.dynamical_system.evaluate(state + 0.5 * h * k1, t + 0.5 * h)
            k3 = self.dynamical_system.evaluate(state + 0.5 * h * k2, t + 0.5 * h)
            k4 = self.dynamical_system.evaluate(state + h * k3, t + h)
            state = state + (h / 6.0) * (k1 + 2 * k2 + 2 * k3 + k4)
            states[i + 1] = state

        return times, states

    def poincare_section(self, x0: np.ndarray, t_total: float,
                         section_dim: int = 0,
                         section_value: float = 0.0,
                         dt: float = 0.01) -> np.ndarray:
        """Compute Poincaré section crossings.

        Records state when the trajectory crosses x[section_dim] = section_value
        in the positive direction.
        """
        times, states = self.compute(x0, (0, t_total), dt)
        crossings = []
        for i in range(1, len(states)):
            prev = states[i - 1, section_dim] - section_value
            curr = states[i, section_dim] - section_value
            if prev < 0 and curr >= 0:
                # Linear interpolation
                frac = -prev / (curr - prev)
                crossing = states[i - 1] + frac * (states[i] - states[i - 1])
                crossings.append(crossing)
        return np.array(crossings) if crossings else np.array([]).reshape(0, self.dynamical_system.dim)

    def canonical_form(self) -> str:
        return f"Flow({self.name})"


class VectorFieldOnManifold(IRNode):
    """A smooth vector field X on a manifold M.

    X assigns to each point p ∈ M a tangent vector X_p ∈ T_pM.
    Used for generating flows and computing Lie derivatives.
    """

    def __init__(self, name: str,
                 dim: int,
                 components: Optional[List[Expr]] = None,
                 components_numeric: Optional[Callable[[np.ndarray], np.ndarray]] = None,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata)
        self.dim = dim
        self.components = components or [
            Expr.symbol(f"{name}_{i}") for i in range(dim)
        ]
        self.components_numeric = components_numeric

    def lie_bracket(self, other: VectorFieldOnManifold) -> VectorFieldOnManifold:
        """Compute the Lie bracket [X, Y]."""
        coords = [Expr.symbol(f"x{i}") for i in range(self.dim)]
        bracket_components = []
        for i in range(self.dim):
            comp = Expr.zero()
            for j in range(self.dim):
                comp = comp + (
                    self.components[j] * other.components[i].diff(coords[j]) -
                    other.components[j] * self.components[i].diff(coords[j])
                )
            bracket_components.append(comp)
        return VectorFieldOnManifold(
            f"[{self.name},{other.name}]", self.dim, bracket_components
        )

    def lie_derivative_scalar(self, f: Expr) -> Expr:
        """Lie derivative of a scalar field: L_X f = X^i ∂f/∂x^i."""
        coords = [Expr.symbol(f"x{i}") for i in range(self.dim)]
        result = Expr.zero()
        for i in range(self.dim):
            result = result + self.components[i] * f.diff(coords[i])
        return result

    def divergence(self) -> Expr:
        """div(X) = ∂X^i/∂x^i."""
        coords = [Expr.symbol(f"x{i}") for i in range(self.dim)]
        result = Expr.zero()
        for i in range(self.dim):
            result = result + self.components[i].diff(coords[i])
        return result

    def to_dynamical_system(self) -> DynamicalSystem:
        """Convert to a DynamicalSystem ẋ = X(x)."""
        ds = DynamicalSystem(f"DS({self.name})", self.dim, self.components)
        if self.components_numeric is not None:
            ds.rhs_numeric = lambda state, t: self.components_numeric(state)
        return ds

    def canonical_form(self) -> str:
        return f"VectorFieldOnManifold({self.name}, dim={self.dim})"
