"""
Grey Physics — Electromagnetism Domain

Maxwell's equations, electrostatics, magnetostatics, radiation, optics:
  - Maxwell solver (FDTD, spectral)
  - Electrostatic: Poisson/Laplace, multipole expansion
  - Magnetostatics: Biot-Savart, vector potential
  - Electromagnetic waves: plane waves, waveguides, cavities
  - Radiation: Larmor, Lienard-Wiechert
  - Geometrical optics: ray tracing, Fermat's principle
"""

from __future__ import annotations

import numpy as np
from typing import Callable, Dict, List, Optional, Tuple

from grey_physics.core.numeric import ODESolver, FDMSolver


# ============================================================
# Electric and magnetic fields
# ============================================================

class EMField:
    """Electromagnetic field on a 3D grid."""

    def __init__(self, shape: Tuple[int, int, int],
                 dx: float, dy: float, dz: float):
        self.shape = shape
        self.dx, self.dy, self.dz = dx, dy, dz
        # E and B fields on Yee grid
        self.Ex = np.zeros(shape)
        self.Ey = np.zeros(shape)
        self.Ez = np.zeros(shape)
        self.Bx = np.zeros(shape)
        self.By = np.zeros(shape)
        self.Bz = np.zeros(shape)

    def energy_density(self) -> np.ndarray:
        """u = ½(ε₀|E|² + |B|²/μ₀)."""
        eps0 = 8.854e-12
        mu0 = 4 * np.pi * 1e-7
        E_sq = self.Ex**2 + self.Ey**2 + self.Ez**2
        B_sq = self.Bx**2 + self.By**2 + self.Bz**2
        return 0.5 * (eps0 * E_sq + B_sq / mu0)

    def total_energy(self) -> float:
        dV = self.dx * self.dy * self.dz
        return float(np.sum(self.energy_density()) * dV)

    def poynting_vector(self) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
        """S = E × B / μ₀."""
        mu0 = 4 * np.pi * 1e-7
        Sx = (self.Ey * self.Bz - self.Ez * self.By) / mu0
        Sy = (self.Ez * self.Bx - self.Ex * self.Bz) / mu0
        Sz = (self.Ex * self.By - self.Ey * self.Bx) / mu0
        return Sx, Sy, Sz


# ============================================================
# FDTD Maxwell solver
# ============================================================

class FDTDSolver:
    """Finite-Difference Time-Domain solver for Maxwell's equations.

    Implements the Yee algorithm on a staggered grid:
      ∂B/∂t = -∇×E
      ∂E/∂t = c²∇×B - J/ε₀

    Uses leapfrog time-stepping for B and E fields.
    """

    def __init__(self, shape: Tuple[int, int, int],
                 dx: float, dy: float, dz: float,
                 c: float = 3e8):
        self.shape = shape
        self.dx, self.dy, self.dz = dx, dy, dz
        self.c = c
        self.dt = 0.99 / (c * np.sqrt(1/dx**2 + 1/dy**2  + 1/dz**2))
        self.field = EMField(shape, dx, dy, dz)
        self.time = 0.0

    def step(self, J: Optional[Tuple[np.ndarray, np.ndarray, np.ndarray]] = None) -> None:
        """Advance one FDTD time step."""
        dt = self.dt
        dx, dy, dz = self.dx, self.dy, self.dz
        E = self.field
        c2 = self.c**2

        # Update B (half step) — Faraday's law
        E.Bx[:-1, :-1, :-1] -= dt * (
            (E.Ez[:-1, 1:, :-1] - E.Ez[:-1, :-1, :-1]) / dy -
            (E.Ey[:-1, :-1, 1:] - E.Ey[:-1, :-1, :-1]) / dz
        )
        E.By[:-1, :-1, :-1] -= dt * (
            (E.Ex[:-1, :-1, 1:] - E.Ex[:-1, :-1, :-1]) / dz -
            (E.Ez[1:, :-1, :-1] - E.Ez[:-1, :-1, :-1]) / dx
        )
        E.Bz[:-1, :-1, :-1] -= dt * (
            (E.Ey[1:, :-1, :-1] - E.Ey[:-1, :-1, :-1]) / dx -
            (E.Ex[:-1, 1:, :-1] - E.Ex[:-1, :-1, :-1]) / dy
        )

        # Update E — Ampere's law
        E.Ex[1:, 1:, 1:] += c2 * dt * (
            (E.Bz[1:, 1:, 1:] - E.Bz[1:, :-1, 1:]) / dy -
            (E.By[1:, 1:, 1:] - E.By[1:, 1:, :-1]) / dz
        )
        E.Ey[1:, 1:, 1:] += c2 * dt * (
            (E.Bx[1:, 1:, 1:] - E.Bx[1:, 1:, :-1]) / dz -
            (E.Bz[1:, 1:, 1:] - E.Bz[:-1, 1:, 1:]) / dx
        )
        E.Ez[1:, 1:, 1:] += c2 * dt * (
            (E.By[1:, 1:, 1:] - E.By[:-1, 1:, 1:]) / dx -
            (E.Bx[1:, 1:, 1:] - E.Bx[1:, :-1, 1:]) / dy
        )

        # Add current source
        if J is not None:
            eps0 = 8.854e-12
            E.Ex -= dt * J[0] / eps0
            E.Ey -= dt * J[1] / eps0
            E.Ez -= dt * J[2] / eps0

        self.time += dt

    def simulate(self, n_steps: int,
                 source: Optional[Callable] = None) -> List[float]:
        """Run n_steps of FDTD.

        Returns list of total energies at each step.
        """
        energies = []
        for step in range(n_steps):
            J = source(self.time, self.shape) if source else None
            self.step(J)
            energies.append(self.field.total_energy())
        return energies


# ============================================================
# Electrostatics
# ============================================================

class Electrostatics:
    """Electrostatic field calculations."""

    @staticmethod
    def point_charge_field(q: float, r_source: np.ndarray,
                           r_field: np.ndarray) -> np.ndarray:
        """Electric field from a point charge at r_source evaluated at r_field.

        E = kq(r - r_s) / |r - r_s|³
        """
        k = 8.988e9  # Coulomb constant
        dr = r_field - r_source
        r = np.linalg.norm(dr)
        if r < 1e-15:
            return np.zeros(3)
        return k * q * dr / r**3

    @staticmethod
    def point_charge_potential(q: float, r_source: np.ndarray,
                                r_field: np.ndarray) -> float:
        """Potential V = kq / |r - r_s|."""
        k = 8.988e9
        r = np.linalg.norm(r_field - r_source)
        if r < 1e-15:
            return 0.0
        return k * q / r

    @staticmethod
    def dipole_field(p: np.ndarray, r: np.ndarray) -> np.ndarray:
        """Electric field of a dipole.

        E = (1/4πε₀) [3(p·r̂)r̂ - p] / r³
        """
        k = 8.988e9
        r_mag = np.linalg.norm(r)
        if r_mag < 1e-15:
            return np.zeros(3)
        r_hat = r / r_mag
        return k * (3 * np.dot(p, r_hat) * r_hat - p) / r_mag**3

    @staticmethod
    def multipole_expansion(charges: List[Tuple[float, np.ndarray]],
                            r: np.ndarray, l_max: int = 2) -> float:
        """Multipole expansion of potential.

        V(r) ≈ Σ_l Σ_m (1/r^{l+1}) Q_{lm} Y_{lm}(θ,φ)

        Simplified for axial symmetry.
        """
        potential = 0.0
        k = 8.988e9
        r_mag = np.linalg.norm(r)
        if r_mag < 1e-15:
            return 0.0

        # Monopole (l=0)
        Q = sum(q for q, _ in charges)
        potential += k * Q / r_mag

        if l_max >= 1:
            # Dipole (l=1)
            p = sum(q * np.asarray(pos) for q, pos in charges)
            r_hat = r / r_mag
            potential += k * np.dot(p, r_hat) / r_mag**2

        if l_max >= 2:
            # Quadrupole (l=2)
            Q_ij = np.zeros((3, 3))
            for q, pos in charges:
                pos = np.asarray(pos)
                for i in range(3):
                    for j in range(3):
                        Q_ij[i, j] += q * (3 * pos[i] * pos[j] -
                                            np.dot(pos, pos) * (1 if i == j else 0))
            r_hat = r / r_mag
            quad_term = 0.5 * r_hat @ Q_ij @ r_hat
            potential += k * quad_term / r_mag**3

        return potential

    @staticmethod
    def solve_laplace_2d(Nx: int, Ny: int,
                          bc: Dict[str, Callable]) -> np.ndarray:
        """Solve Laplace equation ∇²V = 0 on unit square with specified BCs.

        bc keys: 'top', 'bottom', 'left', 'right' → f(x or y) → V
        """
        V = np.zeros((Ny, Nx))
        x = np.linspace(0, 1, Nx)
        y = np.linspace(0, 1, Ny)

        # Apply BCs
        if 'bottom' in bc:
            V[0, :] = np.array([bc['bottom'](xi) for xi in x])
        if 'top' in bc:
            V[-1, :] = np.array([bc['top'](xi) for xi in x])
        if 'left' in bc:
            V[:, 0] = np.array([bc['left'](yi) for yi in y])
        if 'right' in bc:
            V[:, -1] = np.array([bc['right'](yi) for yi in y])

        # Gauss-Seidel iteration
        for _ in range(10000):
            V_old = V.copy()
            for j in range(1, Ny - 1):
                for i in range(1, Nx - 1):
                    V[j, i] = 0.25 * (V[j, i+1] + V[j, i-1] +
                                       V[j+1, i] + V[j-1, i])
            if np.max(np.abs(V - V_old)) < 1e-6:
                break

        return V


# ============================================================
# Magnetostatics
# ============================================================

class Magnetostatics:
    """Magnetostatic field calculations."""

    @staticmethod
    def biot_savart(dl_segments: List[Tuple[np.ndarray, np.ndarray]],
                    I: float, r_field: np.ndarray) -> np.ndarray:
        """Biot-Savart law: B = (μ₀I/4π) ∫ dl × r̂ / r².

        Parameters
        ----------
        dl_segments : list of (position, dl_vector) tuples
        I : current
        r_field : field point
        """
        mu0 = 4 * np.pi * 1e-7
        B = np.zeros(3)
        for r_source, dl in dl_segments:
            dr = r_field - r_source
            r = np.linalg.norm(dr)
            if r < 1e-15:
                continue
            dB = np.cross(dl, dr) / r**3
            B += dB
        B *= mu0 * I / (4 * np.pi)
        return B

    @staticmethod
    def solenoid_field(n: float, I: float, R: float,
                       L: float, z: float) -> float:
        """Axial field of a finite solenoid.

        B_z ≈ (μ₀nI/2) [cos θ₁ - cos θ₂]
        where θ₁, θ₂ are angles to the ends.
        """
        mu0 = 4 * np.pi * 1e-7
        z1 = z + L / 2  # distance to one end
        z2 = z - L / 2  # distance to other end
        cos1 = z1 / np.sqrt(z1**2 + R**2)
        cos2 = z2 / np.sqrt(z2**2 + R**2)
        return 0.5 * mu0 * n * I * (cos1 - cos2)

    @staticmethod
    def magnetic_dipole_field(m: np.ndarray, r: np.ndarray) -> np.ndarray:
        """Magnetic field of a magnetic dipole.

        B = (μ₀/4π) [3(m·r̂)r̂ - m] / r³
        """
        mu0 = 4 * np.pi * 1e-7
        r_mag = np.linalg.norm(r)
        if r_mag < 1e-15:
            return np.zeros(3)
        r_hat = r / r_mag
        return (mu0 / (4 * np.pi)) * (3 * np.dot(m, r_hat) * r_hat - m) / r_mag**3


# ============================================================
# Electromagnetic Waves
# ============================================================

class EMWave:
    """Electromagnetic wave solutions."""

    @staticmethod
    def plane_wave(k: np.ndarray, omega: float,
                   E0: np.ndarray, r: np.ndarray,
                   t: float) -> Tuple[np.ndarray, np.ndarray]:
        """Monochromatic plane wave.

        E = E₀ cos(k·r - ωt)
        B = (k̂ × E₀/c) cos(k·r - ωt)
        """
        c = 3e8
        phase = np.dot(k, r) - omega * t
        k_hat = k / np.linalg.norm(k)
        E = E0 * np.cos(phase)
        B = np.cross(k_hat, E0) / c * np.cos(phase)
        return E, B

    @staticmethod
    def gaussian_beam(w0: float, wavelength: float,
                      z: float, r: float) -> complex:
        """Gaussian beam intensity profile.

        E(r,z) = E₀ (w₀/w(z)) exp(-r²/w(z)²) exp(-ikz - ikr²/(2R(z)) + iζ(z))
        """
        k = 2 * np.pi / wavelength
        z_R = np.pi * w0**2 / wavelength  # Rayleigh range
        w = w0 * np.sqrt(1 + (z / z_R)**2)  # beam radius
        R = z * (1 + (z_R / z)**2) if abs(z) > 1e-15 else float('inf')
        zeta = np.arctan2(z, z_R)  # Gouy phase

        amplitude = (w0 / w) * np.exp(-r**2 / w**2)
        if abs(z) > 1e-15:
            phase = -k * z - k * r**2 / (2 * R) + zeta
        else:
            phase = zeta
        return amplitude * np.exp(1j * phase)

    @staticmethod
    def larmor_power(q: float, a: float) -> float:
        """Larmor formula: total radiated power from accelerating charge.

        P = q²a² / (6πε₀c³)
        """
        eps0 = 8.854e-12
        c = 3e8
        return q**2 * a**2 / (6 * np.pi * eps0 * c**3)


# ============================================================
# Ray Optics
# ============================================================

class RayTracer:
    """Geometrical optics ray tracer using Fermat's principle.

    Traces rays through media with varying refractive index n(r).
    """

    def __init__(self, n_func: Callable[[np.ndarray], float]):
        """
        Parameters
        ----------
        n_func : refractive index n(r) as function of position
        """
        self.n_func = n_func
        self.solver = ODESolver()

    def trace(self, r0: np.ndarray, direction: np.ndarray,
              t_span: Tuple[float, float],
              dt: float = 0.001) -> Dict[str, np.ndarray]:
        """Trace a ray through the medium.

        Uses the ray equation: d/ds(n dr/ds) = ∇n.
        """
        direction = direction / np.linalg.norm(direction)

        def equations(t, state):
            r = state[:3]
            v = state[3:]  # n * dr/ds
            n = self.n_func(r)
            # Gradient of n (finite differences)
            eps = 1e-6
            grad_n = np.zeros(3)
            for i in range(3):
                r_plus = r.copy(); r_plus[i] += eps
                r_minus = r.copy(); r_minus[i] -= eps
                grad_n[i] = (self.n_func(r_plus) - self.n_func(r_minus)) / (2 * eps)

            drds = v / n
            dvds = grad_n
            return np.concatenate([drds, dvds])

        n0 = self.n_func(r0)
        y0 = np.concatenate([r0, n0 * direction])
        result = self.solver.solve(equations, y0, t_span, dt, "RK4")

        return {"s": result["t"],
                "position": result["y"][:, :3],
                "direction": result["y"][:, 3:]}

    @staticmethod
    def snells_law(n1: float, n2: float, theta_i: float) -> float:
        """Snell's law: n₁ sin θ₁ = n₂ sin θ₂.

        Returns angle of refraction. Returns NaN for total internal reflection.
        """
        sin_t = n1 * np.sin(theta_i) / n2
        if abs(sin_t) > 1:
            return float('nan')  # total internal reflection
        return np.arcsin(sin_t)

    @staticmethod
    def fresnel_coefficients(n1: float, n2: float,
                              theta_i: float) -> Dict[str, float]:
        """Fresnel reflection and transmission coefficients."""
        cos_i = np.cos(theta_i)
        sin_t = n1 * np.sin(theta_i) / n2
        if abs(sin_t) > 1:
            return {"r_s": 1.0, "r_p": 1.0, "t_s": 0.0, "t_p": 0.0}
        cos_t = np.sqrt(1 - sin_t**2)

        r_s = (n1 * cos_i - n2 * cos_t) / (n1 * cos_i + n2 * cos_t)
        r_p = (n2 * cos_i - n1 * cos_t) / (n2 * cos_i + n1 * cos_t)
        t_s = 2 * n1 * cos_i / (n1 * cos_i + n2 * cos_t)
        t_p = 2 * n1 * cos_i / (n2 * cos_i + n1 * cos_t)

        return {"r_s": r_s, "r_p": r_p, "t_s": t_s, "t_p": t_p}
