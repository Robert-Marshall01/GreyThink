"""
PDE-Based Reasoning — Experimental Module.

Provides PDE solvers and reasoning tools:
- Heat equation solver (diffusion-based smoothing)
- Wave equation solver (signal propagation)
- Hamilton-Jacobi solvers (optimal control, viscosity solutions)
- Variational formulations (weak forms, energy minimization)
- Reaction-diffusion systems (pattern formation)
- Finite difference methods (explicit, implicit, Crank-Nicolson)
- Spectral PDE methods
- Method of lines
- PDE-driven operator evolution
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Callable, Optional

import numpy as np
from numpy.typing import NDArray

from greymath.experimental.ir_extensions import PDEOperator, PDEKind


# ─── Enumerations ────────────────────────────────────────────────────────────

class DiscretizationMethod(Enum):
    """PDE discretization methods."""
    EXPLICIT_EULER = auto()
    IMPLICIT_EULER = auto()
    CRANK_NICOLSON = auto()
    SPECTRAL = auto()
    METHOD_OF_LINES = auto()
    RK4 = auto()


class BoundaryCondition(Enum):
    """Types of boundary conditions."""
    DIRICHLET = auto()
    NEUMANN = auto()
    PERIODIC = auto()
    ROBIN = auto()
    ABSORBING = auto()


# ─── PDE Solution ────────────────────────────────────────────────────────────

@dataclass
class PDESolution:
    """Result of a PDE solve."""
    spatial_grid: NDArray          # (N,) or (N,M,...) spatial grid
    time_grid: NDArray             # (T,) time steps
    solution: NDArray              # (T, N) or (T, N, M) solution values
    method: DiscretizationMethod = DiscretizationMethod.EXPLICIT_EULER
    residual_norm: float = 0.0
    stable: bool = True
    metadata: dict[str, Any] = field(default_factory=dict)

    @property
    def final_state(self) -> NDArray:
        return self.solution[-1]

    @property
    def initial_state(self) -> NDArray:
        return self.solution[0]

    def at_time(self, t: float) -> NDArray:
        """Interpolate solution at a given time."""
        idx = np.searchsorted(self.time_grid, t)
        idx = min(idx, len(self.time_grid) - 1)
        return self.solution[idx]

    def energy(self, dx: float = 1.0) -> NDArray:
        """Compute L2 energy ∫|u|² dx at each time step."""
        return np.array([np.sum(u ** 2) * dx for u in self.solution])


# ─── 1D Heat Equation Solver ────────────────────────────────────────────────

class HeatEquationSolver:
    """
    Solve the heat equation ∂u/∂t = α ∂²u/∂x² in 1D.

    Supports explicit Euler, implicit Euler, and Crank-Nicolson schemes.
    """

    @staticmethod
    def solve(
        u0: NDArray,
        alpha: float = 1.0,
        dx: float = 0.01,
        dt: float = 0.0001,
        n_steps: int = 1000,
        bc: BoundaryCondition = BoundaryCondition.DIRICHLET,
        bc_values: tuple[float, float] = (0.0, 0.0),
        method: DiscretizationMethod = DiscretizationMethod.CRANK_NICOLSON,
    ) -> PDESolution:
        """Solve the 1D heat equation."""
        N = len(u0)
        r = alpha * dt / (dx ** 2)

        # Stability check for explicit method
        stable = True
        if method == DiscretizationMethod.EXPLICIT_EULER and r > 0.5:
            stable = False

        time_grid = np.arange(n_steps + 1) * dt
        solution = np.zeros((n_steps + 1, N))
        solution[0] = u0.copy()

        if method == DiscretizationMethod.EXPLICIT_EULER:
            solution = HeatEquationSolver._solve_explicit(
                u0, r, N, n_steps, bc, bc_values, solution
            )
        elif method == DiscretizationMethod.IMPLICIT_EULER:
            solution = HeatEquationSolver._solve_implicit(
                u0, r, N, n_steps, bc, bc_values, solution
            )
        elif method == DiscretizationMethod.CRANK_NICOLSON:
            solution = HeatEquationSolver._solve_crank_nicolson(
                u0, r, N, n_steps, bc, bc_values, solution
            )

        x_grid = np.arange(N) * dx
        return PDESolution(
            spatial_grid=x_grid,
            time_grid=time_grid,
            solution=solution,
            method=method,
            stable=stable,
            metadata={"alpha": alpha, "dx": dx, "dt": dt, "r": r},
        )

    @staticmethod
    def _solve_explicit(
        u0: NDArray, r: float, N: int, n_steps: int,
        bc: BoundaryCondition, bc_values: tuple[float, float],
        solution: NDArray,
    ) -> NDArray:
        u = u0.copy()
        for step in range(n_steps):
            u_new = u.copy()
            for i in range(1, N - 1):
                u_new[i] = u[i] + r * (u[i + 1] - 2 * u[i] + u[i - 1])

            if bc == BoundaryCondition.DIRICHLET:
                u_new[0] = bc_values[0]
                u_new[-1] = bc_values[1]
            elif bc == BoundaryCondition.PERIODIC:
                u_new[0] = u[0] + r * (u[1] - 2 * u[0] + u[-2])
                u_new[-1] = u_new[0]
            elif bc == BoundaryCondition.NEUMANN:
                u_new[0] = u_new[1]
                u_new[-1] = u_new[-2]

            u = u_new
            solution[step + 1] = u
        return solution

    @staticmethod
    def _solve_implicit(
        u0: NDArray, r: float, N: int, n_steps: int,
        bc: BoundaryCondition, bc_values: tuple[float, float],
        solution: NDArray,
    ) -> NDArray:
        """Implicit Euler: (I - r*L)u^{n+1} = u^n."""
        # Build tridiagonal matrix
        A = np.eye(N) * (1 + 2 * r)
        for i in range(1, N):
            A[i, i - 1] = -r
        for i in range(N - 1):
            A[i, i + 1] = -r

        if bc == BoundaryCondition.DIRICHLET:
            A[0, :] = 0; A[0, 0] = 1
            A[-1, :] = 0; A[-1, -1] = 1

        u = u0.copy()
        for step in range(n_steps):
            rhs = u.copy()
            if bc == BoundaryCondition.DIRICHLET:
                rhs[0] = bc_values[0]
                rhs[-1] = bc_values[1]
            u = np.linalg.solve(A, rhs)
            solution[step + 1] = u
        return solution

    @staticmethod
    def _solve_crank_nicolson(
        u0: NDArray, r: float, N: int, n_steps: int,
        bc: BoundaryCondition, bc_values: tuple[float, float],
        solution: NDArray,
    ) -> NDArray:
        """Crank-Nicolson: second-order in time and space."""
        r2 = r / 2.0
        A = np.eye(N) * (1 + r)
        B = np.eye(N) * (1 - r)
        for i in range(1, N):
            A[i, i - 1] = -r2
            B[i, i - 1] = r2
        for i in range(N - 1):
            A[i, i + 1] = -r2
            B[i, i + 1] = r2

        if bc == BoundaryCondition.DIRICHLET:
            A[0, :] = 0; A[0, 0] = 1
            A[-1, :] = 0; A[-1, -1] = 1
            B[0, :] = 0; B[0, 0] = 1
            B[-1, :] = 0; B[-1, -1] = 1

        u = u0.copy()
        for step in range(n_steps):
            rhs = B @ u
            if bc == BoundaryCondition.DIRICHLET:
                rhs[0] = bc_values[0]
                rhs[-1] = bc_values[1]
            u = np.linalg.solve(A, rhs)
            solution[step + 1] = u
        return solution


# ─── 1D Wave Equation Solver ────────────────────────────────────────────────

class WaveEquationSolver:
    """
    Solve the wave equation ∂²u/∂t² = c² ∂²u/∂x² in 1D.

    Uses the standard explicit leapfrog scheme.
    """

    @staticmethod
    def solve(
        u0: NDArray,
        v0: Optional[NDArray] = None,
        c: float = 1.0,
        dx: float = 0.01,
        dt: float = 0.005,
        n_steps: int = 1000,
        bc: BoundaryCondition = BoundaryCondition.DIRICHLET,
        bc_values: tuple[float, float] = (0.0, 0.0),
    ) -> PDESolution:
        """Solve the 1D wave equation."""
        N = len(u0)
        r = (c * dt / dx) ** 2

        # CFL stability condition
        stable = r <= 1.0

        if v0 is None:
            v0 = np.zeros(N)

        time_grid = np.arange(n_steps + 1) * dt
        solution = np.zeros((n_steps + 1, N))
        solution[0] = u0.copy()

        # First step using initial velocity
        u_prev = u0.copy()
        u_curr = u0.copy()
        for i in range(1, N - 1):
            u_curr[i] = (u0[i] + dt * v0[i]
                         + 0.5 * r * (u0[i + 1] - 2 * u0[i] + u0[i - 1]))

        WaveEquationSolver._apply_bc(u_curr, bc, bc_values)
        solution[1] = u_curr.copy()

        # Subsequent steps: leapfrog
        for step in range(1, n_steps):
            u_next = np.zeros(N)
            for i in range(1, N - 1):
                u_next[i] = (2 * u_curr[i] - u_prev[i]
                             + r * (u_curr[i + 1] - 2 * u_curr[i] + u_curr[i - 1]))

            WaveEquationSolver._apply_bc(u_next, bc, bc_values)
            u_prev = u_curr
            u_curr = u_next
            solution[step + 1] = u_curr

        x_grid = np.arange(N) * dx
        return PDESolution(
            spatial_grid=x_grid,
            time_grid=time_grid,
            solution=solution,
            method=DiscretizationMethod.EXPLICIT_EULER,
            stable=stable,
            metadata={"c": c, "dx": dx, "dt": dt, "CFL": r},
        )

    @staticmethod
    def _apply_bc(u: NDArray, bc: BoundaryCondition,
                  values: tuple[float, float]) -> None:
        if bc == BoundaryCondition.DIRICHLET:
            u[0] = values[0]
            u[-1] = values[1]
        elif bc == BoundaryCondition.NEUMANN:
            u[0] = u[1]
            u[-1] = u[-2]
        elif bc == BoundaryCondition.PERIODIC:
            u[0] = u[-2]
            u[-1] = u[1]


# ─── Reaction-Diffusion System ──────────────────────────────────────────────

class ReactionDiffusionSolver:
    """
    Solve reaction-diffusion systems:
        ∂u/∂t = D ∇²u + R(u)

    Supports multi-component systems (e.g., Gray-Scott, FitzHugh-Nagumo).
    """

    @staticmethod
    def solve(
        u0: NDArray,
        diffusion_coeffs: NDArray,
        reaction_fn: Callable[[NDArray], NDArray],
        dx: float = 0.01,
        dt: float = 0.0001,
        n_steps: int = 1000,
        bc: BoundaryCondition = BoundaryCondition.PERIODIC,
    ) -> PDESolution:
        """
        Solve a multi-component reaction-diffusion system.

        Args:
            u0: (n_components, N) initial conditions
            diffusion_coeffs: (n_components,) diffusion coefficients
            reaction_fn: maps (n_components, N) → (n_components, N)
            dx, dt: spatial and temporal step sizes
            n_steps: number of time steps
            bc: boundary condition type

        Returns: PDESolution with solution shape (n_steps+1, n_components, N)
        """
        n_comp, N = u0.shape
        r = dt / (dx ** 2)

        time_grid = np.arange(n_steps + 1) * dt
        solution = np.zeros((n_steps + 1, n_comp, N))
        solution[0] = u0.copy()

        u = u0.copy()
        for step in range(n_steps):
            # Diffusion (explicit)
            laplacian = np.zeros_like(u)
            for c in range(n_comp):
                for i in range(1, N - 1):
                    laplacian[c, i] = u[c, i + 1] - 2 * u[c, i] + u[c, i - 1]
                if bc == BoundaryCondition.PERIODIC:
                    laplacian[c, 0] = u[c, 1] - 2 * u[c, 0] + u[c, -2]
                    laplacian[c, -1] = laplacian[c, 0]

            # Time step: u^{n+1} = u^n + dt * (D * ∇²u + R(u))
            reaction = reaction_fn(u)
            for c in range(n_comp):
                u[c] = u[c] + dt * (diffusion_coeffs[c] * laplacian[c] / (dx ** 2) + reaction[c])

            solution[step + 1] = u.copy()

        x_grid = np.arange(N) * dx
        return PDESolution(
            spatial_grid=x_grid,
            time_grid=time_grid,
            solution=solution,
            method=DiscretizationMethod.EXPLICIT_EULER,
            metadata={"diffusion_coeffs": diffusion_coeffs.tolist()},
        )

    @staticmethod
    def gray_scott_reaction(
        F: float = 0.04, k: float = 0.06
    ) -> Callable[[NDArray], NDArray]:
        """Gray-Scott reaction term for 2-component system (u, v)."""
        def reaction(uv: NDArray) -> NDArray:
            u, v = uv[0], uv[1]
            du = -u * v ** 2 + F * (1 - u)
            dv = u * v ** 2 - (F + k) * v
            return np.array([du, dv])
        return reaction

    @staticmethod
    def fitzhugh_nagumo_reaction(
        a: float = 0.7, b: float = 0.8, tau: float = 12.5, I_ext: float = 0.5,
    ) -> Callable[[NDArray], NDArray]:
        """FitzHugh-Nagumo reaction term (excitable medium)."""
        def reaction(uv: NDArray) -> NDArray:
            u, v = uv[0], uv[1]
            du = u - u ** 3 / 3 - v + I_ext
            dv = (u + a - b * v) / tau
            return np.array([du, dv])
        return reaction


# ─── Hamilton-Jacobi Solver ──────────────────────────────────────────────────

class HamiltonJacobiSolver:
    """
    Solve Hamilton-Jacobi equations:
        ∂u/∂t + H(x, ∇u) = 0

    Uses Lax-Friedrichs or Godunov schemes for viscosity solutions.
    """

    @staticmethod
    def solve_lax_friedrichs(
        u0: NDArray,
        hamiltonian: Callable[[NDArray, NDArray], NDArray],
        dx: float = 0.01,
        dt: float = 0.005,
        n_steps: int = 500,
        x_grid: Optional[NDArray] = None,
        viscosity: float = 0.0,
    ) -> PDESolution:
        """
        Solve HJ equation using Lax-Friedrichs scheme.

        The Lax-Friedrichs numerical Hamiltonian uses:
        Ĥ(p⁻, p⁺) = H((p⁻+p⁺)/2) - α(p⁺-p⁻)/2
        where α is the maximum wave speed.
        """
        N = len(u0)
        if x_grid is None:
            x_grid = np.arange(N) * dx

        time_grid = np.arange(n_steps + 1) * dt
        solution = np.zeros((n_steps + 1, N))
        solution[0] = u0.copy()

        u = u0.copy()
        for step in range(n_steps):
            # Compute spatial gradients
            grad_plus = np.zeros(N)
            grad_minus = np.zeros(N)
            for i in range(1, N - 1):
                grad_plus[i] = (u[i + 1] - u[i]) / dx
                grad_minus[i] = (u[i] - u[i - 1]) / dx

            # Average gradient and Lax-Friedrichs dissipation
            grad_avg = 0.5 * (grad_plus + grad_minus)
            H_vals = hamiltonian(x_grid, grad_avg)

            # Estimate maximum wave speed
            alpha_lf = np.max(np.abs(grad_plus - grad_minus)) / (dx + 1e-30)
            alpha_lf = max(alpha_lf, 1.0)

            # Lax-Friedrichs update
            u_new = u.copy()
            for i in range(1, N - 1):
                numerical_H = H_vals[i] - 0.5 * alpha_lf * (grad_plus[i] - grad_minus[i])
                u_new[i] = u[i] - dt * numerical_H

                # Add viscosity if requested
                if viscosity > 0:
                    u_new[i] += viscosity * dt / dx ** 2 * (u[i + 1] - 2 * u[i] + u[i - 1])

            # Boundary extrapolation
            u_new[0] = u_new[1]
            u_new[-1] = u_new[-2]

            u = u_new
            solution[step + 1] = u

        return PDESolution(
            spatial_grid=x_grid,
            time_grid=time_grid,
            solution=solution,
            method=DiscretizationMethod.EXPLICIT_EULER,
            metadata={"viscosity": viscosity},
        )


# ─── Spectral PDE Solver ────────────────────────────────────────────────────

class SpectralPDESolver:
    """
    Solve PDEs using spectral methods (Fourier spectral).

    Ideal for problems with periodic boundary conditions.
    """

    @staticmethod
    def solve_heat_spectral(
        u0: NDArray,
        alpha: float = 1.0,
        L: float = 1.0,
        dt: float = 0.001,
        n_steps: int = 1000,
    ) -> PDESolution:
        """
        Solve the heat equation using Fourier spectral method.

        Uses exact integration in Fourier space:
        û_k(t) = û_k(0) exp(-α k² t)
        """
        N = len(u0)
        dx = L / N
        x_grid = np.linspace(0, L, N, endpoint=False)
        time_grid = np.arange(n_steps + 1) * dt

        # Wavenumbers
        k = np.fft.fftfreq(N, d=dx) * 2 * np.pi
        k_sq = k ** 2

        solution = np.zeros((n_steps + 1, N))
        u_hat = np.fft.fft(u0)
        solution[0] = u0.copy()

        for step in range(n_steps):
            t = (step + 1) * dt
            u_hat_t = u_hat * np.exp(-alpha * k_sq * t)
            solution[step + 1] = np.real(np.fft.ifft(u_hat_t))

        return PDESolution(
            spatial_grid=x_grid,
            time_grid=time_grid,
            solution=solution,
            method=DiscretizationMethod.SPECTRAL,
            stable=True,
            metadata={"alpha": alpha, "L": L},
        )

    @staticmethod
    def solve_kdv_spectral(
        u0: NDArray,
        L: float = 2 * np.pi,
        dt: float = 0.0001,
        n_steps: int = 10000,
    ) -> PDESolution:
        """
        Solve the KdV equation u_t + 6u u_x + u_xxx = 0
        using exponential time-differencing RK4 (ETDRK4) in spectral space.
        Simplified version using split-step method.
        """
        N = len(u0)
        dx = L / N
        x_grid = np.linspace(0, L, N, endpoint=False)
        time_grid = np.arange(n_steps + 1) * dt

        k = np.fft.fftfreq(N, d=dx) * 2 * np.pi
        ik3 = 1j * k ** 3

        solution = np.zeros((n_steps + 1, N))
        solution[0] = u0.copy()
        u = u0.copy()

        for step in range(n_steps):
            # Split step: linear part in Fourier, nonlinear in physical
            u_hat = np.fft.fft(u)

            # Half-step linear
            u_hat = u_hat * np.exp(-ik3 * dt / 2)

            # Full-step nonlinear
            u = np.real(np.fft.ifft(u_hat))
            u_x_hat = np.fft.fft(u) * 1j * k
            u_x = np.real(np.fft.ifft(u_x_hat))
            u_hat = np.fft.fft(u - 6 * dt * u * u_x)

            # Half-step linear
            u_hat = u_hat * np.exp(-ik3 * dt / 2)
            u = np.real(np.fft.ifft(u_hat))

            solution[step + 1] = u

        return PDESolution(
            spatial_grid=x_grid,
            time_grid=time_grid,
            solution=solution,
            method=DiscretizationMethod.SPECTRAL,
            stable=True,
            metadata={"L": L, "equation": "KdV"},
        )


# ─── Method of Lines ────────────────────────────────────────────────────────

class MethodOfLines:
    """
    Method of Lines: reduce a PDE to a system of ODEs by
    discretizing only spatial derivatives.

    Spatial discretization → ODE system → solve with scipy.integrate.
    """

    @staticmethod
    def solve(
        u0: NDArray,
        spatial_rhs: Callable[[NDArray, NDArray], NDArray],
        x_grid: NDArray,
        t_span: tuple[float, float],
        n_time_points: int = 500,
    ) -> PDESolution:
        """
        Solve PDE via method of lines.

        Args:
            u0: initial condition (N,) array
            spatial_rhs: function(u, x) → du/dt after spatial discretization
            x_grid: spatial grid points
            t_span: (t_start, t_end)
            n_time_points: number of output times
        """
        from scipy.integrate import solve_ivp

        t_eval = np.linspace(t_span[0], t_span[1], n_time_points)

        def ode_rhs(t: float, u: NDArray) -> NDArray:
            return spatial_rhs(u, x_grid)

        sol = solve_ivp(
            ode_rhs, t_span, u0, t_eval=t_eval,
            method="RK45", max_step=0.01,
        )

        solution = sol.y.T  # shape (n_times, N)
        return PDESolution(
            spatial_grid=x_grid,
            time_grid=sol.t,
            solution=solution,
            method=DiscretizationMethod.METHOD_OF_LINES,
            stable=sol.success,
            metadata={"message": sol.message},
        )


# ─── Variational PDE Formulation ────────────────────────────────────────────

class VariationalFormulation:
    """
    Variational (weak) formulation of elliptic/Poisson-type PDEs.

    Solves: find u such that a(u, v) = l(v) for all test functions v.
    Using Galerkin method with piecewise linear basis.
    """

    @staticmethod
    def solve_poisson_1d(
        f_rhs: Callable[[NDArray], NDArray],
        n_elements: int = 100,
        domain: tuple[float, float] = (0.0, 1.0),
        bc_left: float = 0.0,
        bc_right: float = 0.0,
    ) -> tuple[NDArray, NDArray]:
        """
        Solve -u''(x) = f(x) on [a,b] with Dirichlet BC.

        Uses piecewise linear finite elements (hat functions).

        Returns: (x_grid, u_solution)
        """
        a, b = domain
        h = (b - a) / n_elements
        x = np.linspace(a, b, n_elements + 1)

        n_interior = n_elements - 1
        # Stiffness matrix K (tridiagonal)
        K = np.zeros((n_interior, n_interior))
        for i in range(n_interior):
            K[i, i] = 2.0 / h
            if i > 0:
                K[i, i - 1] = -1.0 / h
            if i < n_interior - 1:
                K[i, i + 1] = -1.0 / h

        # Load vector (midpoint quadrature)
        F = np.zeros(n_interior)
        for i in range(n_interior):
            xi = x[i + 1]
            F[i] = f_rhs(np.array([xi]))[0] * h

        # Incorporate Dirichlet BCs
        F[0] += bc_left / h
        F[-1] += bc_right / h

        # Solve
        u_interior = np.linalg.solve(K, F)
        u = np.zeros(n_elements + 1)
        u[0] = bc_left
        u[-1] = bc_right
        u[1:-1] = u_interior

        return x, u

    @staticmethod
    def energy_functional(
        u: NDArray, f_rhs: NDArray, dx: float,
    ) -> float:
        """
        Compute the energy functional:
        J(u) = (1/2) ∫|∇u|² dx - ∫f·u dx
        """
        # Approximate gradient
        grad_u = np.gradient(u, dx)
        kinetic = 0.5 * np.sum(grad_u ** 2) * dx
        potential = np.sum(f_rhs * u) * dx
        return float(kinetic - potential)
