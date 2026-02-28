"""
Grey Physics — Fluids Domain

Computational fluid dynamics:
  - Euler equations (inviscid)
  - Navier-Stokes (incompressible, compressible)
  - Vortex dynamics (vortex sheet, vortex ring)
  - Turbulence (energy spectrum, structure functions)
  - Shallow water equations
  - MHD (magnetohydrodynamics)
  - Lattice Boltzmann method
"""

from __future__ import annotations

import numpy as np
from typing import Callable, Dict, List, Optional, Tuple

from grey_physics.core.numeric import FDMSolver, SpectralSolver


# ============================================================
# 2D Euler / Navier-Stokes (vorticity-streamfunction)
# ============================================================

class FluidSolver2D:
    """2D incompressible fluid solver in vorticity-streamfunction form.

    ∂ω/∂t + (u·∇)ω = ν∇²ω + f
    ∇²ψ = -ω
    u = ∂ψ/∂y,  v = -∂ψ/∂x
    """

    def __init__(self, Nx: int, Ny: int,
                 Lx: float = 2 * np.pi, Ly: float = 2 * np.pi,
                 nu: float = 0.001):
        self.Nx, self.Ny = Nx, Ny
        self.Lx, self.Ly = Lx, Ly
        self.nu = nu
        self.dx = Lx / Nx
        self.dy = Ly / Ny
        self.x = np.linspace(0, Lx, Nx, endpoint=False)
        self.y = np.linspace(0, Ly, Ny, endpoint=False)
        self.X, self.Y = np.meshgrid(self.x, self.y)

        # Wavenumbers for spectral Poisson solve
        kx = np.fft.fftfreq(Nx, d=self.dx) * 2 * np.pi
        ky = np.fft.fftfreq(Ny, d=self.dy) * 2 * np.pi
        self.KX, self.KY = np.meshgrid(kx, ky)
        self.K2 = self.KX**2 + self.KY**2
        self.K2[0, 0] = 1.0  # avoid division by zero

        self.omega = np.zeros((Ny, Nx))  # vorticity
        self.psi = np.zeros((Ny, Nx))    # streamfunction

    def set_vorticity(self, omega: np.ndarray) -> None:
        self.omega = omega.copy()

    def _solve_poisson(self) -> None:
        """Solve ∇²ψ = -ω spectrally."""
        omega_hat = np.fft.fft2(self.omega)
        psi_hat = -omega_hat / self.K2
        psi_hat[0, 0] = 0
        self.psi = np.real(np.fft.ifft2(psi_hat))

    def _compute_velocity(self) -> Tuple[np.ndarray, np.ndarray]:
        """u = ∂ψ/∂y, v = -∂ψ/∂x via spectral differentiation."""
        psi_hat = np.fft.fft2(self.psi)
        u = np.real(np.fft.ifft2(1j * self.KY * psi_hat))
        v = -np.real(np.fft.ifft2(1j * self.KX * psi_hat))
        return u, v

    def _advection(self, omega: np.ndarray,
                   u: np.ndarray, v: np.ndarray) -> np.ndarray:
        """(u·∇)ω via spectral derivatives."""
        omega_hat = np.fft.fft2(omega)
        domega_dx = np.real(np.fft.ifft2(1j * self.KX * omega_hat))
        domega_dy = np.real(np.fft.ifft2(1j * self.KY * omega_hat))
        return u * domega_dx + v * domega_dy

    def _diffusion(self, omega: np.ndarray) -> np.ndarray:
        """ν∇²ω via spectral Laplacian."""
        omega_hat = np.fft.fft2(omega)
        lap_omega = np.real(np.fft.ifft2(-self.K2 * omega_hat))
        return self.nu * lap_omega

    def step(self, dt: float,
             forcing: Optional[np.ndarray] = None) -> None:
        """One time step using RK4."""
        def rhs(w):
            self.omega = w
            self._solve_poisson()
            u, v = self._compute_velocity()
            dw = -self._advection(w, u, v) + self._diffusion(w)
            if forcing is not None:
                dw += forcing
            return dw

        k1 = rhs(self.omega)
        k2 = rhs(self.omega + 0.5 * dt * k1)
        k3 = rhs(self.omega + 0.5 * dt * k2)
        k4 = rhs(self.omega + dt * k3)
        self.omega += (dt / 6) * (k1 + 2*k2 + 2*k3 + k4)
        self._solve_poisson()

    def simulate(self, dt: float, n_steps: int,
                 save_every: int = 10,
                 forcing: Optional[np.ndarray] = None) -> Dict[str, Any]:
        """Run simulation for n_steps.

        Returns dict with time, enstrophy, energy histories
        and snapshots of vorticity.
        """
        times = []
        enstrophies = []
        energies = []
        snapshots = []

        for n in range(n_steps):
            self.step(dt, forcing)

            if n % save_every == 0:
                self._solve_poisson()
                u, v = self._compute_velocity()
                times.append(n * dt)
                enstrophies.append(self.enstrophy())
                energies.append(self.kinetic_energy())
                snapshots.append(self.omega.copy())

        return {
            "times": np.array(times),
            "enstrophy": np.array(enstrophies),
            "energy": np.array(energies),
            "snapshots": snapshots,
        }

    def kinetic_energy(self) -> float:
        """E = ½ ∫ |u|² dA."""
        u, v = self._compute_velocity()
        return 0.5 * float(np.mean(u**2 + v**2)) * self.Lx * self.Ly

    def enstrophy(self) -> float:
        """Ω = ½ ∫ ω² dA."""
        return 0.5 * float(np.mean(self.omega**2)) * self.Lx * self.Ly

    def energy_spectrum(self) -> Tuple[np.ndarray, np.ndarray]:
        """Compute isotropic energy spectrum E(k)."""
        u, v = self._compute_velocity()
        u_hat = np.fft.fft2(u)
        v_hat = np.fft.fft2(v)
        E_hat = 0.5 * (np.abs(u_hat)**2 + np.abs(v_hat)**2) / (self.Nx * self.Ny)**2

        k_mag = np.sqrt(self.KX**2 + self.KY**2)
        k_max = int(np.sqrt(self.Nx**2 + self.Ny**2) / 2)
        k_bins = np.arange(0.5, k_max + 0.5)
        spectrum = np.zeros(k_max)

        for i in range(k_max):
            mask = (k_mag >= k_bins[i] - 0.5) & (k_mag < k_bins[i] + 0.5)
            spectrum[i] = np.sum(E_hat[mask])

        return k_bins[:k_max], spectrum

    # ============================================================
    # Preset initial conditions
    # ============================================================

    def taylor_green_vortex(self) -> None:
        """Set Taylor-Green vortex initial condition."""
        self.omega = 2 * np.cos(self.X) * np.cos(self.Y)

    def kelvin_helmholtz(self, amplitude: float = 0.1) -> None:
        """Set Kelvin-Helmholtz instability initial condition."""
        self.omega = np.zeros((self.Ny, self.Nx))
        delta = self.Ly / 20
        y_mid = self.Ly / 2
        # Shear layer
        self.omega = (1.0 / (np.cosh((self.Y - y_mid) / delta))**2)
        # Add perturbation
        self.omega += amplitude * np.sin(2 * np.pi * self.X / self.Lx)

    def random_vorticity(self, amplitude: float = 1.0,
                          k_peak: int = 4) -> None:
        """Random vorticity field with energy peaked at wavenumber k_peak."""
        omega_hat = np.zeros((self.Ny, self.Nx), dtype=complex)
        k_mag = np.sqrt(self.KX**2 + self.KY**2)

        # Band-pass filter around k_peak
        mask = (k_mag > k_peak - 1) & (k_mag < k_peak + 1)
        phases = 2 * np.pi * np.random.random((self.Ny, self.Nx))
        omega_hat[mask] = amplitude * np.exp(1j * phases[mask])

        self.omega = np.real(np.fft.ifft2(omega_hat))


# ============================================================
# 1D Shallow Water Equations
# ============================================================

class ShallowWater1D:
    """1D shallow water equations.

    ∂h/∂t + ∂(hu)/∂x = 0
    ∂(hu)/∂t + ∂(hu² + ½gh²)/∂x = -ghS
    """

    def __init__(self, N: int, L: float, g: float = 9.81):
        self.N = N
        self.L = L
        self.g = g
        self.dx = L / N
        self.x = np.linspace(0, L, N)
        self.h = np.ones(N)  # water height
        self.hu = np.zeros(N)  # momentum

    def _flux(self, h: np.ndarray, hu: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
        """Compute fluxes: F1 = hu, F2 = hu²/h + ½gh²."""
        u = np.where(h > 1e-10, hu / h, 0.0)
        F1 = hu
        F2 = hu * u + 0.5 * self.g * h**2
        return F1, F2

    def _lax_friedrichs(self, dt: float) -> None:
        """Lax-Friedrichs scheme for one time step."""
        F1, F2 = self._flux(self.h, self.hu)

        h_new = np.zeros_like(self.h)
        hu_new = np.zeros_like(self.hu)

        for i in range(1, self.N - 1):
            h_new[i] = 0.5 * (self.h[i+1] + self.h[i-1]) - \
                        0.5 * dt / self.dx * (F1[i+1] - F1[i-1])
            hu_new[i] = 0.5 * (self.hu[i+1] + self.hu[i-1]) - \
                         0.5 * dt / self.dx * (F2[i+1] - F2[i-1])

        # Reflective BCs
        h_new[0] = h_new[1]
        h_new[-1] = h_new[-2]
        hu_new[0] = -hu_new[1]
        hu_new[-1] = -hu_new[-2]

        self.h = h_new
        self.hu = hu_new

    def simulate(self, dt: float, n_steps: int,
                 save_every: int = 10) -> Dict[str, Any]:
        times = []
        h_snapshots = []

        for n in range(n_steps):
            self._lax_friedrichs(dt)
            if n % save_every == 0:
                times.append(n * dt)
                h_snapshots.append(self.h.copy())

        return {"times": np.array(times), "h": h_snapshots,
                "x": self.x.copy()}

    def dam_break(self, h_left: float = 2.0, h_right: float = 1.0) -> None:
        """Set dam break initial condition."""
        self.h = np.where(self.x < self.L / 2, h_left, h_right)
        self.hu = np.zeros_like(self.h)


# ============================================================
# Lattice Boltzmann Method (D2Q9)
# ============================================================

class LatticeBoltzmann:
    """D2Q9 Lattice Boltzmann solver for incompressible flow."""

    # D2Q9 velocities and weights
    cx = np.array([0, 1, 0, -1,  0, 1, -1, -1,  1])
    cy = np.array([0, 0, 1,  0, -1, 1,  1, -1, -1])
    w = np.array([4/9, 1/9, 1/9, 1/9, 1/9, 1/36, 1/36, 1/36, 1/36])
    opposite = np.array([0, 3, 4, 1, 2, 7, 8, 5, 6])

    def __init__(self, Nx: int, Ny: int, tau: float = 0.55):
        """
        Parameters
        ----------
        Nx, Ny : grid dimensions
        tau : relaxation time (viscosity ~ (tau - 0.5)/3)
        """
        self.Nx, self.Ny = Nx, Ny
        self.tau = tau
        self.nu = (tau - 0.5) / 3  # kinematic viscosity
        self.f = np.zeros((9, Ny, Nx))  # distribution functions
        self.obstacle = np.zeros((Ny, Nx), dtype=bool)

    def _equilibrium(self, rho: np.ndarray,
                     ux: np.ndarray, uy: np.ndarray) -> np.ndarray:
        """Compute equilibrium distribution."""
        feq = np.zeros((9, self.Ny, self.Nx))
        u_sq = ux**2 + uy**2
        for i in range(9):
            cu = self.cx[i] * ux + self.cy[i] * uy
            feq[i] = self.w[i] * rho * (1 + 3*cu + 4.5*cu**2 - 1.5*u_sq)
        return feq

    def initialize(self, rho0: float = 1.0,
                   ux0: float = 0.0, uy0: float = 0.0) -> None:
        """Initialize with uniform density and velocity."""
        rho = rho0 * np.ones((self.Ny, self.Nx))
        ux = ux0 * np.ones((self.Ny, self.Nx))
        uy = uy0 * np.ones((self.Ny, self.Nx))
        self.f = self._equilibrium(rho, ux, uy)

    def set_obstacle(self, mask: np.ndarray) -> None:
        """Set obstacle (boolean mask)."""
        self.obstacle = mask.astype(bool)

    def step(self) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
        """One LBM time step: stream-collision."""
        Nx, Ny = self.Nx, self.Ny

        # Streaming
        for i in range(9):
            self.f[i] = np.roll(np.roll(self.f[i],
                                         self.cx[i], axis=1),
                                 self.cy[i], axis=0)

        # Bounce-back on obstacles
        for i in range(9):
            self.f[i, self.obstacle] = self.f[self.opposite[i], self.obstacle]

        # Macroscopic quantities
        rho = np.sum(self.f, axis=0)
        ux = np.sum(self.f * self.cx[:, None, None], axis=0) / rho
        uy = np.sum(self.f * self.cy[:, None, None], axis=0) / rho
        ux[self.obstacle] = 0
        uy[self.obstacle] = 0

        # Collision (BGK)
        feq = self._equilibrium(rho, ux, uy)
        self.f += -(self.f - feq) / self.tau

        return rho, ux, uy

    def simulate(self, n_steps: int,
                 save_every: int = 100) -> Dict[str, Any]:
        """Run LBM simulation."""
        snapshots = []
        for n in range(n_steps):
            rho, ux, uy = self.step()
            if n % save_every == 0:
                speed = np.sqrt(ux**2 + uy**2)
                snapshots.append({
                    "rho": rho.copy(),
                    "ux": ux.copy(),
                    "uy": uy.copy(),
                    "speed": speed.copy(),
                })
        return {"snapshots": snapshots}

    @staticmethod
    def cylinder_flow(Nx: int = 200, Ny: int = 80,
                      Re: float = 100) -> LatticeBoltzmann:
        """Preset: flow around a cylinder (von Kármán vortex street)."""
        U = 0.1  # inlet velocity
        D = Ny / 5  # cylinder diameter
        nu = U * D / Re
        tau = 3 * nu + 0.5

        lbm = LatticeBoltzmann(Nx, Ny, tau)
        lbm.initialize(rho0=1.0, ux0=U)

        # Create circular obstacle
        cx, cy = Nx // 4, Ny // 2
        Y, X = np.meshgrid(np.arange(Ny), np.arange(Nx), indexing='ij')
        mask = (X - cx)**2 + (Y - cy)**2 < (D / 2)**2
        lbm.set_obstacle(mask)

        return lbm


# ============================================================
# Vortex dynamics
# ============================================================

class PointVortex:
    """Point vortex dynamics in 2D.

    N point vortices with circulations Γ_i at positions (x_i, y_i).
    Each vortex is advected by the velocity field of all others.
    """

    def __init__(self, positions: np.ndarray,
                 circulations: np.ndarray):
        """
        Parameters
        ----------
        positions : (N, 2) array of vortex positions
        circulations : (N,) array of circulations
        """
        self.positions = np.asarray(positions, dtype=float)
        self.circulations = np.asarray(circulations, dtype=float)
        self.N = len(circulations)

    def _velocity_at(self, pos: np.ndarray,
                     exclude: int = -1) -> np.ndarray:
        """Velocity induced at pos by all vortices (excluding one)."""
        vx, vy = 0.0, 0.0
        for i in range(self.N):
            if i == exclude:
                continue
            dx = pos[0] - self.positions[i, 0]
            dy = pos[1] - self.positions[i, 1]
            r2 = dx**2 + dy**2
            if r2 < 1e-20:
                continue
            vx += -self.circulations[i] * dy / (2 * np.pi * r2)
            vy += self.circulations[i] * dx / (2 * np.pi * r2)
        return np.array([vx, vy])

    def _equations(self, t: float, state: np.ndarray) -> np.ndarray:
        """ODE for vortex positions."""
        pos = state.reshape(self.N, 2)
        vel = np.zeros((self.N, 2))
        for i in range(self.N):
            vel[i] = self._velocity_at(pos[i], exclude=i)
        return vel.flatten()

    def simulate(self, t_span: Tuple[float, float],
                 dt: float = 0.01) -> Dict[str, np.ndarray]:
        """Simulate vortex dynamics."""
        from grey_physics.core.numeric import ODESolver
        solver = ODESolver()
        y0 = self.positions.flatten()
        result = solver.solve(self._equations, y0, t_span, dt, "RK4")

        t = result["t"]
        y = result["y"]
        trajectories = y.reshape(len(t), self.N, 2)

        # Hamiltonian (conserved for point vortices)
        H = []
        for k in range(len(t)):
            h = 0.0
            for i in range(self.N):
                for j in range(i+1, self.N):
                    dx = trajectories[k, i, 0] - trajectories[k, j, 0]
                    dy = trajectories[k, i, 1] - trajectories[k, j, 1]
                    r = np.sqrt(dx**2 + dy**2)
                    if r > 1e-15:
                        h -= self.circulations[i] * self.circulations[j] * np.log(r) / (4*np.pi)
            H.append(h)

        return {"t": t, "trajectories": trajectories,
                "hamiltonian": np.array(H)}

    @staticmethod
    def co_rotating_pair(gamma: float = 1.0,
                          d: float = 1.0) -> PointVortex:
        """Two co-rotating vortices."""
        pos = np.array([[d/2, 0], [-d/2, 0]])
        circ = np.array([gamma, gamma])
        return PointVortex(pos, circ)

    @staticmethod
    def counter_rotating_pair(gamma: float = 1.0,
                               d: float = 1.0) -> PointVortex:
        """Two counter-rotating vortices (translating pair)."""
        pos = np.array([[0, d/2], [0, -d/2]])
        circ = np.array([gamma, -gamma])
        return PointVortex(pos, circ)
