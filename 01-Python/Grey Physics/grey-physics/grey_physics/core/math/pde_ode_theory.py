"""
Grey Physics Math Core — PDE/ODE Theory

Classification, weak forms, variational forms, well-posedness analysis,
and method-of-lines discretizations.
"""

from __future__ import annotations

from enum import Enum, auto
from typing import Any, Callable, Dict, List, Optional, Tuple
import numpy as np
from scipy.sparse import diags, csr_matrix, kron, eye as speye
from scipy.sparse.linalg import spsolve


# ---------------------------------------------------------------------------
# PDE classification
# ---------------------------------------------------------------------------

class PDEClassification(Enum):
    ELLIPTIC = auto()
    PARABOLIC = auto()
    HYPERBOLIC = auto()
    ULTRAHYPERBOLIC = auto()
    MIXED = auto()
    DEGENERATE = auto()


def classify_second_order_pde(A: np.ndarray) -> PDEClassification:
    """Classify a second-order linear PDE from its principal symbol.

    For Σ A_{ij} ∂²u/∂x_i∂x_j + lower = 0, classify based on
    eigenvalues of A (the principal symbol matrix).

    Elliptic: all eigenvalues same sign
    Hyperbolic: one opposite sign, rest same
    Parabolic: one zero eigenvalue
    """
    eigvals = np.linalg.eigvalsh(A)
    positive = np.sum(eigvals > 1e-10)
    negative = np.sum(eigvals < -1e-10)
    zero = np.sum(np.abs(eigvals) < 1e-10)
    n = len(eigvals)

    if zero > 0:
        return PDEClassification.PARABOLIC
    if positive == n or negative == n:
        return PDEClassification.ELLIPTIC
    if (positive == 1 and negative == n - 1) or (negative == 1 and positive == n - 1):
        return PDEClassification.HYPERBOLIC
    if positive > 1 and negative > 1:
        return PDEClassification.ULTRAHYPERBOLIC
    return PDEClassification.MIXED


# ---------------------------------------------------------------------------
# Weak / variational forms
# ---------------------------------------------------------------------------

class WeakForm:
    """Weak formulation of a PDE.

    Transforms a strong PDE into its weak (integral) form by
    multiplying by test functions and integrating by parts.

    For -∇²u = f with Dirichlet BCs:
    Strong: -Δu = f
    Weak: ∫ ∇u·∇v dx = ∫ fv dx  for all v ∈ H¹₀
    """

    def __init__(self, name: str,
                 bilinear_form: Callable[[np.ndarray, np.ndarray, float], float],
                 linear_form: Callable[[np.ndarray, float], float],
                 space_dim: int,
                 num_basis: int):
        """
        Args:
            bilinear_form: a(u, v) = ∫ (operator acting on u) · v dx
            linear_form: F(v) = ∫ f · v dx
            space_dim: spatial dimension
            num_basis: number of basis functions
        """
        self.name = name
        self.bilinear_form = bilinear_form
        self.linear_form = linear_form
        self.space_dim = space_dim
        self.num_basis = num_basis

    def assemble_stiffness(self, basis: List[np.ndarray],
                           grid: np.ndarray) -> np.ndarray:
        """Assemble the stiffness matrix K_{ij} = a(φ_j, φ_i)."""
        n = len(basis)
        dx = grid[1] - grid[0] if len(grid) > 1 else 1.0
        K = np.zeros((n, n))
        for i in range(n):
            for j in range(n):
                K[i, j] = self.bilinear_form(basis[j], basis[i], dx)
        return K

    def assemble_load(self, basis: List[np.ndarray],
                      grid: np.ndarray) -> np.ndarray:
        """Assemble the load vector F_i = F(φ_i)."""
        n = len(basis)
        dx = grid[1] - grid[0] if len(grid) > 1 else 1.0
        F_vec = np.zeros(n)
        for i in range(n):
            F_vec[i] = self.linear_form(basis[i], dx)
        return F_vec

    def solve(self, basis: List[np.ndarray],
              grid: np.ndarray) -> np.ndarray:
        """Solve Ku = F for the coefficients."""
        K = self.assemble_stiffness(basis, grid)
        F_vec = self.assemble_load(basis, grid)
        return np.linalg.solve(K, F_vec)


# ---------------------------------------------------------------------------
# Method of lines
# ---------------------------------------------------------------------------

class MethodOfLines:
    """Discretize spatial derivatives of a PDE, leaving time continuous.

    Converts a PDE into a system of ODEs that can be integrated
    with standard ODE solvers.
    """

    def __init__(self, spatial_operator: Callable[[np.ndarray, float], np.ndarray],
                 num_points: int, dx: float):
        """
        Args:
            spatial_operator: f(u, dx) → du/dt (spatial discretization)
            num_points: number of spatial grid points
            dx: spatial grid spacing
        """
        self.spatial_operator = spatial_operator
        self.num_points = num_points
        self.dx = dx

    def rhs(self, t: float, u: np.ndarray) -> np.ndarray:
        """RHS for the ODE system: du/dt = L(u)."""
        return self.spatial_operator(u, self.dx)

    def solve(self, u0: np.ndarray, t_span: Tuple[float, float],
              num_times: int = 100,
              method: str = "RK45") -> Tuple[np.ndarray, np.ndarray]:
        """Integrate the method-of-lines ODE system.

        Returns (times, solution) where solution.shape = (num_times, num_points).
        """
        from scipy.integrate import solve_ivp
        t_eval = np.linspace(t_span[0], t_span[1], num_times)
        sol = solve_ivp(self.rhs, t_span, u0, t_eval=t_eval,
                        method=method, rtol=1e-8, atol=1e-10)
        return sol.t, sol.y.T

    @staticmethod
    def heat_1d(alpha: float = 1.0, nx: int = 50,
                L: float = 1.0) -> MethodOfLines:
        """1D heat equation: ∂u/∂t = α ∂²u/∂x²."""
        dx = L / (nx - 1)

        def spatial_op(u: np.ndarray, dx: float) -> np.ndarray:
            du = np.zeros_like(u)
            du[1:-1] = alpha * (u[2:] - 2 * u[1:-1] + u[:-2]) / dx ** 2
            return du

        return MethodOfLines(spatial_op, nx, dx)

    @staticmethod
    def wave_1d(c: float = 1.0, nx: int = 50,
                L: float = 1.0) -> MethodOfLines:
        """1D wave equation as first-order system:
        ∂u/∂t = v, ∂v/∂t = c² ∂²u/∂x².
        State vector: [u₁,...,uₙ, v₁,...,vₙ].
        """
        dx = L / (nx - 1)

        def spatial_op(state: np.ndarray, dx: float) -> np.ndarray:
            n = len(state) // 2
            u = state[:n]
            v = state[n:]
            du = v.copy()
            dv = np.zeros(n)
            dv[1:-1] = c ** 2 * (u[2:] - 2 * u[1:-1] + u[:-2]) / dx ** 2
            return np.concatenate([du, dv])

        return MethodOfLines(spatial_op, 2 * nx, dx)

    @staticmethod
    def advection_1d(c: float = 1.0, nx: int = 50,
                     L: float = 1.0) -> MethodOfLines:
        """1D advection equation: ∂u/∂t + c ∂u/∂x = 0.
        Uses upwind scheme.
        """
        dx = L / (nx - 1)

        def spatial_op(u: np.ndarray, dx: float) -> np.ndarray:
            du = np.zeros_like(u)
            if c > 0:
                du[1:] = -c * (u[1:] - u[:-1]) / dx
            else:
                du[:-1] = -c * (u[1:] - u[:-1]) / dx
            return du

        return MethodOfLines(spatial_op, nx, dx)


# ---------------------------------------------------------------------------
# CFL condition checker
# ---------------------------------------------------------------------------

def cfl_condition(c: float, dx: float, dt: float) -> Dict[str, Any]:
    """Check the CFL (Courant–Friedrichs–Lewy) condition.

    CFL number = c * dt / dx.
    For explicit methods, typically need CFL ≤ 1.
    """
    cfl = abs(c * dt / dx)
    return {
        "cfl_number": cfl,
        "is_stable": cfl <= 1.0,
        "max_dt": dx / abs(c) if abs(c) > 0 else float("inf"),
        "message": f"CFL = {cfl:.4f}" + (" (STABLE)" if cfl <= 1 else " (UNSTABLE!)"),
    }


def diffusion_stability(alpha: float, dx: float, dt: float) -> Dict[str, Any]:
    """Check stability for explicit diffusion (FTCS scheme).

    Stability requires α·dt/dx² ≤ 0.5.
    """
    r = alpha * dt / dx ** 2
    return {
        "stability_parameter": r,
        "is_stable": r <= 0.5,
        "max_dt": 0.5 * dx ** 2 / alpha if alpha > 0 else float("inf"),
        "message": f"r = {r:.4f}" + (" (STABLE)" if r <= 0.5 else " (UNSTABLE!)"),
    }


# ---------------------------------------------------------------------------
# Energy and error estimates
# ---------------------------------------------------------------------------

class EnergyMonitor:
    """Monitor energy-like quantities during time evolution.

    Tracks total energy, relative drift, and conservation violations.
    """

    def __init__(self, energy_func: Callable[[np.ndarray], float]):
        """
        Args:
            energy_func: function mapping state → energy value
        """
        self.energy_func = energy_func
        self._history: List[Tuple[float, float]] = []
        self._initial_energy: Optional[float] = None

    def record(self, state: np.ndarray, t: float) -> float:
        """Record energy at current state and time."""
        E = self.energy_func(state)
        if self._initial_energy is None:
            self._initial_energy = E
        self._history.append((t, E))
        return E

    @property
    def drift(self) -> float:
        """Relative energy drift from initial value."""
        if not self._history or self._initial_energy is None:
            return 0.0
        E_current = self._history[-1][1]
        if abs(self._initial_energy) < 1e-15:
            return abs(E_current)
        return abs((E_current - self._initial_energy) / self._initial_energy)

    @property
    def max_drift(self) -> float:
        """Maximum relative energy drift over all recorded steps."""
        if not self._history or self._initial_energy is None:
            return 0.0
        E0 = self._initial_energy
        if abs(E0) < 1e-15:
            return max(abs(h[1]) for h in self._history)
        return max(abs((h[1] - E0) / E0) for h in self._history)

    @property
    def times(self) -> np.ndarray:
        return np.array([h[0] for h in self._history])

    @property
    def energies(self) -> np.ndarray:
        return np.array([h[1] for h in self._history])

    def is_conserved(self, tolerance: float = 1e-6) -> bool:
        """Check if energy is conserved within tolerance."""
        return self.max_drift < tolerance


# ---------------------------------------------------------------------------
# Error estimators
# ---------------------------------------------------------------------------

def richardson_extrapolation(
    solver: Callable[[int], np.ndarray],
    n_coarse: int,
    order: int = 2
) -> Tuple[np.ndarray, np.ndarray]:
    """Estimate error via Richardson extrapolation.

    Runs solver at resolution n and 2n, estimates local error.

    Returns (solution, error_estimate).
    """
    sol_coarse = solver(n_coarse)
    sol_fine = solver(2 * n_coarse)
    # Restrict fine solution to coarse grid
    sol_fine_coarse = sol_fine[::2] if len(sol_fine) == 2 * len(sol_coarse) - 1 else sol_fine[:len(sol_coarse)]
    error = (sol_fine_coarse - sol_coarse) / (2 ** order - 1)
    corrected = sol_fine_coarse + error
    return corrected, error


# ---------------------------------------------------------------------------
# Measure theory / probability (conceptual stochastic processes)
# ---------------------------------------------------------------------------

class StochasticProcess:
    """A stochastic differential equation (SDE):
    dX = a(X,t)dt + b(X,t)dW

    Uses Euler-Maruyama method for simulation.
    """

    def __init__(self, name: str,
                 drift: Callable[[np.ndarray, float], np.ndarray],
                 diffusion: Callable[[np.ndarray, float], np.ndarray],
                 dim: int = 1):
        self.name = name
        self.drift = drift
        self.diffusion = diffusion
        self.dim = dim

    def sample_path(self, x0: np.ndarray, t_span: Tuple[float, float],
                    dt: float = 0.001,
                    seed: Optional[int] = None) -> Tuple[np.ndarray, np.ndarray]:
        """Generate a single sample path via Euler-Maruyama."""
        if seed is not None:
            np.random.seed(seed)
        t0, tf = t_span
        n = int((tf - t0) / dt)
        times = np.linspace(t0, tf, n + 1)
        states = np.zeros((n + 1, self.dim))
        states[0] = x0

        sqrt_dt = np.sqrt(dt)
        for i in range(n):
            t = times[i]
            x = states[i]
            dW = sqrt_dt * np.random.randn(self.dim)
            states[i + 1] = x + self.drift(x, t) * dt + self.diffusion(x, t) * dW

        return times, states

    def ensemble(self, x0: np.ndarray, t_span: Tuple[float, float],
                 num_paths: int = 100, dt: float = 0.001) -> Tuple[np.ndarray, np.ndarray]:
        """Generate an ensemble of sample paths.

        Returns (times, ensemble) where ensemble.shape = (num_paths, num_times, dim).
        """
        all_paths = []
        for i in range(num_paths):
            times, path = self.sample_path(x0, t_span, dt, seed=None)
            all_paths.append(path)
        return times, np.array(all_paths)

    def mean_and_variance(self, x0: np.ndarray, t_span: Tuple[float, float],
                          num_paths: int = 1000,
                          dt: float = 0.001) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
        """Estimate mean and variance from ensemble."""
        times, ensemble = self.ensemble(x0, t_span, num_paths, dt)
        mean = np.mean(ensemble, axis=0)
        variance = np.var(ensemble, axis=0)
        return times, mean, variance

    @staticmethod
    def brownian_motion(dim: int = 1) -> StochasticProcess:
        """Standard Brownian motion: dX = dW."""
        return StochasticProcess(
            "BrownianMotion",
            drift=lambda x, t: np.zeros(dim),
            diffusion=lambda x, t: np.ones(dim),
            dim=dim,
        )

    @staticmethod
    def ornstein_uhlenbeck(theta: float = 1.0, mu: float = 0.0,
                           sigma: float = 1.0) -> StochasticProcess:
        """Ornstein-Uhlenbeck process: dX = θ(μ - X)dt + σ dW."""
        return StochasticProcess(
            "OrnsteinUhlenbeck",
            drift=lambda x, t: theta * (mu - x),
            diffusion=lambda x, t: sigma * np.ones_like(x),
            dim=1,
        )

    @staticmethod
    def geometric_brownian_motion(mu: float = 0.05,
                                  sigma: float = 0.2) -> StochasticProcess:
        """GBM: dX = μX dt + σX dW."""
        return StochasticProcess(
            "GeometricBM",
            drift=lambda x, t: mu * x,
            diffusion=lambda x, t: sigma * x,
            dim=1,
        )
