"""
Grey Physics — Field Theory Domain

Classical and quantum field theory:
  - Scalar field solutions (Klein-Gordon, kink, instanton)
  - Gauge fields & Yang-Mills
  - Spontaneous symmetry breaking (Higgs mechanism)
  - Topological solitons (kinks, vortices, monopoles)
  - Renormalization group flows
  - Path integrals (lattice discretisation)
  - Noether currents and Ward identities
"""

from __future__ import annotations

import numpy as np
from typing import Callable, Dict, List, Optional, Tuple

from grey_physics.core.numeric import ODESolver, FDMSolver


# ============================================================
# Scalar Field Theory
# ============================================================

class ScalarFieldTheory:
    """Real scalar field φ in 1+1 dimensions.

    Lagrangian density: ℒ = ½(∂_t φ)² - ½(∂_x φ)² - V(φ)

    Equation of motion: φ_{tt} - φ_{xx} + V'(φ) = 0
    """

    def __init__(self, N: int, L: float,
                 potential: Callable[[np.ndarray], np.ndarray],
                 dV: Callable[[np.ndarray], np.ndarray]):
        """
        Parameters
        ----------
        N : number of spatial grid points
        L : spatial domain size [-L/2, L/2]
        potential : V(φ)
        dV : V'(φ) = dV/dφ
        """
        self.N = N
        self.L = L
        self.dx = L / N
        self.x = np.linspace(-L/2, L/2, N, endpoint=False)
        self.potential = potential
        self.dV = dV

    def _equations(self, t: float, state: np.ndarray) -> np.ndarray:
        """φ_{tt} = φ_{xx} - V'(φ)."""
        phi = state[:self.N]
        pi = state[self.N:]  # π = ∂φ/∂t

        # Spatial second derivative (periodic BC)
        phi_xx = np.zeros_like(phi)
        phi_xx[1:-1] = (phi[2:] - 2*phi[1:-1] + phi[:-2]) / self.dx**2
        phi_xx[0] = (phi[1] - 2*phi[0] + phi[-1]) / self.dx**2
        phi_xx[-1] = (phi[0] - 2*phi[-1] + phi[-2]) / self.dx**2

        dphi_dt = pi
        dpi_dt = phi_xx - self.dV(phi)

        return np.concatenate([dphi_dt, dpi_dt])

    def simulate(self, phi0: np.ndarray, pi0: np.ndarray,
                 t_span: Tuple[float, float],
                 dt: float = 0.001,
                 save_every: int = 10) -> Dict[str, np.ndarray]:
        y0 = np.concatenate([phi0, pi0])
        solver = ODESolver()
        result = solver.solve(self._equations, y0, t_span, dt, "Verlet",
                               split_index=self.N)

        t = result["t"]
        y = result["y"]
        phi = y[:, :self.N]
        pi = y[:, self.N:]

        # Energy density
        energies = []
        for i in range(0, len(t), save_every):
            E = self.energy_density(phi[i], pi[i])
            energies.append(np.sum(E) * self.dx)

        return {
            "t": t[::save_every],
            "x": self.x,
            "phi": phi[::save_every],
            "pi": pi[::save_every],
            "total_energy": np.array(energies),
        }

    def energy_density(self, phi: np.ndarray,
                        pi: np.ndarray) -> np.ndarray:
        """ε = ½π² + ½(∂_x φ)² + V(φ)."""
        phi_x = np.gradient(phi, self.dx)
        return 0.5 * pi**2 + 0.5 * phi_x**2 + self.potential(phi)

    def total_energy(self, phi: np.ndarray,
                      pi: np.ndarray) -> float:
        return float(np.sum(self.energy_density(phi, pi)) * self.dx)

    def topological_charge(self, phi: np.ndarray) -> float:
        """Q = [φ(+∞) - φ(-∞)] / Δφ_vacuum."""
        return (phi[-1] - phi[0])

    # ============================================================
    # Preset potentials
    # ============================================================

    @staticmethod
    def phi4_theory(N: int = 512, L: float = 40.0,
                    m2: float = -1.0, lam: float = 1.0) -> ScalarFieldTheory:
        """φ⁴ theory: V(φ) = ½m²φ² + ¼λφ⁴.

        For m² < 0: double-well → kink solutions.
        """
        def V(phi):
            return 0.5 * m2 * phi**2 + 0.25 * lam * phi**4

        def dV(phi):
            return m2 * phi + lam * phi**3

        return ScalarFieldTheory(N, L, V, dV)

    @staticmethod
    def sine_gordon(N: int = 512, L: float = 40.0,
                    m: float = 1.0) -> ScalarFieldTheory:
        """Sine-Gordon: V(φ) = m²(1 - cos φ).

        Admits soliton (kink/antikink) solutions:
          φ_kink(x,t) = 4 arctan(exp(γ(x - vt)))
        """
        def V(phi):
            return m**2 * (1 - np.cos(phi))

        def dV(phi):
            return m**2 * np.sin(phi)

        return ScalarFieldTheory(N, L, V, dV)

    @staticmethod
    def kink_solution(x: np.ndarray, x0: float = 0.0,
                       v: float = 0.0, m: float = 1.0) -> np.ndarray:
        """Analytic φ⁴ kink: φ = tanh(m(x-x0)/√2)."""
        gamma = 1.0 / np.sqrt(1 - v**2) if abs(v) < 1 else 1.0
        return np.tanh(m * gamma * (x - x0) / np.sqrt(2))

    @staticmethod
    def sine_gordon_kink(x: np.ndarray, x0: float = 0.0,
                          v: float = 0.0) -> np.ndarray:
        """Sine-Gordon kink: φ = 4 arctan(exp(x - x0))."""
        gamma = 1.0 / np.sqrt(1 - v**2) if abs(v) < 1 else 1.0
        return 4 * np.arctan(np.exp(gamma * (x - x0)))


# ============================================================
# Gauge Field Theory (lattice)
# ============================================================

class LatticeGaugeTheory:
    """U(1) lattice gauge theory in 2D.

    Wilson action: S = β Σ_plaquettes (1 - Re(U_p))
    where U_p = U_{μ}(x) U_{ν}(x+μ) U_{μ}†(x+ν) U_{ν}†(x)
    and U_μ(x) = exp(iθ_μ(x)).
    """

    def __init__(self, Nx: int, Ny: int, beta: float = 1.0):
        self.Nx = Nx
        self.Ny = Ny
        self.beta = beta
        # Link variables θ_μ(x) ∈ [0, 2π)
        self.theta_x = np.random.uniform(0, 2*np.pi, (Ny, Nx))
        self.theta_y = np.random.uniform(0, 2*np.pi, (Ny, Nx))

    def plaquette(self, x: int, y: int) -> complex:
        """Compute the plaquette at site (x, y).

        U_p = exp(i θ_x(x,y)) exp(i θ_y(x+1,y))
              exp(-i θ_x(x,y+1)) exp(-i θ_y(x,y))
        """
        Nx, Ny = self.Nx, self.Ny
        xp = (x + 1) % Nx
        yp = (y + 1) % Ny

        phase = (self.theta_x[y, x] + self.theta_y[y, xp] -
                 self.theta_x[yp, x] - self.theta_y[y, x])
        return np.exp(1j * phase)

    def action(self) -> float:
        """Wilson action: S = β Σ (1 - Re U_p)."""
        S = 0.0
        for y in range(self.Ny):
            for x in range(self.Nx):
                S += 1 - np.real(self.plaquette(x, y))
        return self.beta * S

    def average_plaquette(self) -> float:
        """⟨U_p⟩ — order parameter."""
        total = 0.0
        for y in range(self.Ny):
            for x in range(self.Nx):
                total += np.real(self.plaquette(x, y))
        return total / (self.Nx * self.Ny)

    def metropolis_update(self, n_sweeps: int = 1,
                           delta: float = 0.5) -> List[float]:
        """Metropolis Monte Carlo update.

        Returns action history.
        """
        actions = []
        for _ in range(n_sweeps):
            for y in range(self.Ny):
                for x in range(self.Nx):
                    for direction in [0, 1]:
                        # Propose change
                        if direction == 0:
                            old_val = self.theta_x[y, x]
                            new_val = old_val + np.random.uniform(-delta, delta)
                        else:
                            old_val = self.theta_y[y, x]
                            new_val = old_val + np.random.uniform(-delta, delta)

                        old_action = self.action()
                        if direction == 0:
                            self.theta_x[y, x] = new_val
                        else:
                            self.theta_y[y, x] = new_val
                        new_action = self.action()

                        dS = new_action - old_action
                        if dS > 0 and np.random.random() > np.exp(-dS):
                            # Reject
                            if direction == 0:
                                self.theta_x[y, x] = old_val
                            else:
                                self.theta_y[y, x] = old_val

            actions.append(self.action())
        return actions

    def wilson_loop(self, Rx: int, Ry: int,
                    x0: int = 0, y0: int = 0) -> complex:
        """Compute rectangular Wilson loop of size Rx × Ry."""
        W = 1.0 + 0j
        # Bottom edge
        for i in range(Rx):
            W *= np.exp(1j * self.theta_x[y0, (x0 + i) % self.Nx])
        # Right edge
        for j in range(Ry):
            W *= np.exp(1j * self.theta_y[(y0 + j) % self.Ny,
                                           (x0 + Rx) % self.Nx])
        # Top edge (reversed)
        for i in range(Rx - 1, -1, -1):
            W *= np.exp(-1j * self.theta_x[(y0 + Ry) % self.Ny,
                                            (x0 + i) % self.Nx])
        # Left edge (reversed)
        for j in range(Ry - 1, -1, -1):
            W *= np.exp(-1j * self.theta_y[(y0 + j) % self.Ny, x0])
        return W


# ============================================================
# Spontaneous Symmetry Breaking / Higgs Mechanism
# ============================================================

class HiggsMechanism:
    """Mexican hat potential and spontaneous symmetry breaking.

    Complex scalar field φ with:
      V(φ) = -μ²|φ|² + λ|φ|⁴

    Vacuum manifold: |φ| = v = μ/√(2λ)
    Goldstone boson: phase excitation
    Higgs boson: radial excitation, m_H = √(2)μ
    """

    def __init__(self, mu: float = 1.0, lam: float = 0.5):
        self.mu = mu
        self.lam = lam
        self.v = mu / np.sqrt(2 * lam)  # vacuum expectation value
        self.m_higgs = np.sqrt(2) * mu

    def potential(self, phi_re: np.ndarray,
                   phi_im: np.ndarray) -> np.ndarray:
        """V(φ) = -μ²|φ|² + λ|φ|⁴."""
        phi_sq = phi_re**2 + phi_im**2
        return -self.mu**2 * phi_sq + self.lam * phi_sq**2

    def potential_1d(self, phi: np.ndarray) -> np.ndarray:
        """Real field version: V(φ) = -½μ²φ² + ¼λφ⁴."""
        return -0.5 * self.mu**2 * phi**2 + 0.25 * self.lam * phi**4

    def goldstone_dispersion(self, k: np.ndarray) -> np.ndarray:
        """Goldstone boson: ω = |k| (massless)."""
        return np.abs(k)

    def higgs_dispersion(self, k: np.ndarray) -> np.ndarray:
        """Higgs boson: ω² = k² + m_H²."""
        return np.sqrt(k**2 + self.m_higgs**2)


# ============================================================
# Renormalization Group
# ============================================================

class RenormalizationGroup:
    """Simple renormalization group flow analysis.

    Tracks coupling constants g_i as a function of energy scale μ.
    """

    def __init__(self, beta_functions: Callable[[float, np.ndarray], np.ndarray],
                 initial_couplings: np.ndarray):
        """
        Parameters
        ----------
        beta_functions : β(log_mu, g) -> dg/d(log_mu)
        initial_couplings : g(μ₀)
        """
        self.beta = beta_functions
        self.g0 = initial_couplings

    def flow(self, log_mu_span: Tuple[float, float],
             n_points: int = 1000) -> Dict[str, np.ndarray]:
        """Integrate RG flow equations.

        dg_i/d(ln μ) = β_i(g)
        """
        solver = ODESolver()
        dt = (log_mu_span[1] - log_mu_span[0]) / n_points
        result = solver.solve(self.beta, self.g0, log_mu_span, dt, "RK4")

        return {
            "log_mu": result["t"],
            "couplings": result["y"],
        }

    @staticmethod
    def qed_beta(n_f: int = 1) -> RenormalizationGroup:
        """QED beta function at one loop.

        β(α) = 2α²/(3π) × n_f
        """
        def beta(log_mu, g):
            alpha = g[0]
            return np.array([2 * alpha**2 * n_f / (3 * np.pi)])

        return RenormalizationGroup(beta, np.array([1.0/137.0]))

    @staticmethod
    def qcd_beta(n_f: int = 6) -> RenormalizationGroup:
        """QCD beta function at one loop.

        β(α_s) = -α_s²/(2π) (11 - 2n_f/3)
        """
        b0 = 11 - 2 * n_f / 3

        def beta(log_mu, g):
            alpha_s = g[0]
            return np.array([-alpha_s**2 * b0 / (2 * np.pi)])

        return RenormalizationGroup(beta, np.array([0.12]))

    @staticmethod
    def phi4_rg(d: int = 4) -> RenormalizationGroup:
        """φ⁴ theory RG flow (mass and coupling).

        Near d=4 dimensions with ε = 4-d expansion.
        """
        epsilon = 4 - d

        def beta(log_mu, g):
            m2, lam = g
            dm2 = -2 * m2 + lam / (16 * np.pi**2)
            dlam = epsilon * lam - 3 * lam**2 / (16 * np.pi**2)
            return np.array([dm2, dlam])

        return RenormalizationGroup(beta, np.array([1.0, 0.1]))


# ============================================================
# Noether Current (numerical)
# ============================================================

class NoetherCurrent:
    """Compute Noether currents for scalar field theories."""

    @staticmethod
    def energy_momentum_tensor(phi: np.ndarray, pi: np.ndarray,
                                dx: float,
                                potential: Callable) -> Dict[str, np.ndarray]:
        """Compute T^{μν} for a scalar field.

        T^{00} = ½π² + ½(∂_x φ)² + V(φ)  (energy density)
        T^{01} = -π ∂_x φ  (momentum density / energy flux)
        """
        phi_x = np.gradient(phi, dx)
        T00 = 0.5 * pi**2 + 0.5 * phi_x**2 + potential(phi)
        T01 = -pi * phi_x
        return {"T00": T00, "T01": T01}

    @staticmethod
    def u1_current(phi_re: np.ndarray, phi_im: np.ndarray,
                   pi_re: np.ndarray, pi_im: np.ndarray,
                   dx: float) -> Dict[str, np.ndarray]:
        """U(1) Noether current for complex scalar field.

        j^0 = i(φ* π - π* φ) = 2(φ_re π_im - φ_im π_re)  (charge density)
        j^1 = i(φ* ∂_x φ - (∂_x φ*) φ)  (current density)
        """
        j0 = 2 * (phi_re * pi_im - phi_im * pi_re)
        phi_re_x = np.gradient(phi_re, dx)
        phi_im_x = np.gradient(phi_im, dx)
        j1 = 2 * (phi_re * phi_im_x - phi_im * phi_re_x)
        return {"j0": j0, "j1": j1}
