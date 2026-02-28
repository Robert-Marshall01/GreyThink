"""
Grey Physics — Relativity Domain

Special and general relativity:
  - Lorentz transformations, 4-vectors, relativistic kinematics
  - Geodesic equation solver (Schwarzschild, Kerr, FLRW)
  - Einstein field equations: Ricci tensor, scalar, Einstein tensor
  - Gravitational waves (linearised theory)
  - Cosmology: Friedmann equations, redshift
  - Penrose diagrams (conformal compactification)
"""

from __future__ import annotations

import numpy as np
from typing import Callable, Dict, List, Optional, Tuple

from grey_physics.core.numeric import ODESolver


# ============================================================
# Special Relativity
# ============================================================

class LorentzTransform:
    """Lorentz transformations on 4-vectors."""

    @staticmethod
    def boost(beta: np.ndarray) -> np.ndarray:
        """General Lorentz boost matrix for velocity β = v/c.

        Returns 4×4 matrix Λ^μ_ν.
        """
        beta = np.asarray(beta, dtype=float)
        b = np.linalg.norm(beta)
        if b < 1e-15:
            return np.eye(4)
        gamma = 1.0 / np.sqrt(1 - b**2)
        n = beta / b  # direction

        Lambda = np.eye(4)
        Lambda[0, 0] = gamma
        Lambda[0, 1:] = -gamma * beta
        Lambda[1:, 0] = -gamma * beta
        for i in range(3):
            for j in range(3):
                Lambda[i+1, j+1] = (gamma - 1) * n[i] * n[j] + (1 if i == j else 0)

        return Lambda

    @staticmethod
    def boost_x(v: float, c: float = 1.0) -> np.ndarray:
        """Boost along x-axis."""
        beta = v / c
        gamma = 1.0 / np.sqrt(1 - beta**2)
        return np.array([
            [gamma, -gamma*beta, 0, 0],
            [-gamma*beta, gamma, 0, 0],
            [0, 0, 1, 0],
            [0, 0, 0, 1],
        ])

    @staticmethod
    def transform(Lambda: np.ndarray, four_vector: np.ndarray) -> np.ndarray:
        """Apply Lorentz transformation to a 4-vector."""
        return Lambda @ four_vector

    @staticmethod
    def invariant_mass(four_momentum: np.ndarray, c: float = 1.0) -> float:
        """m² c² = E²/c² - |p|²."""
        eta = np.diag([1, -1, -1, -1])
        return float(np.sqrt(abs(four_momentum @ eta @ four_momentum))) / c

    @staticmethod
    def velocity_addition(v1: float, v2: float, c: float = 1.0) -> float:
        """Relativistic velocity addition: u = (v1 + v2)/(1 + v1*v2/c²)."""
        return (v1 + v2) / (1 + v1 * v2 / c**2)

    @staticmethod
    def doppler(f_source: float, v: float, c: float = 1.0) -> float:
        """Relativistic Doppler effect (approaching source)."""
        beta = v / c
        return f_source * np.sqrt((1 + beta) / (1 - beta))

    @staticmethod
    def time_dilation(tau: float, v: float, c: float = 1.0) -> float:
        """t = γτ."""
        gamma = 1.0 / np.sqrt(1 - (v/c)**2)
        return gamma * tau

    @staticmethod
    def length_contraction(L0: float, v: float, c: float = 1.0) -> float:
        """L = L₀/γ."""
        gamma = 1.0 / np.sqrt(1 - (v/c)**2)
        return L0 / gamma


# ============================================================
# FourVector
# ============================================================

class FourVector:
    """Relativistic 4-vector with Minkowski metric (+,-,-,-)."""

    def __init__(self, components: np.ndarray):
        self.x = np.asarray(components, dtype=float)

    @property
    def t(self) -> float: return self.x[0]
    @property
    def spatial(self) -> np.ndarray: return self.x[1:]

    def dot(self, other: FourVector) -> float:
        """Minkowski inner product."""
        eta = np.diag([1, -1, -1, -1])
        return float(self.x @ eta @ other.x)

    def norm_squared(self) -> float:
        return self.dot(self)

    def boost(self, beta: np.ndarray) -> FourVector:
        Lambda = LorentzTransform.boost(beta)
        return FourVector(Lambda @ self.x)

    @staticmethod
    def four_momentum(m: float, v: np.ndarray,
                       c: float = 1.0) -> FourVector:
        """p^μ = (E/c, p) = (γmc, γmv)."""
        v = np.asarray(v, dtype=float)
        beta = np.linalg.norm(v) / c
        gamma = 1.0 / np.sqrt(1 - beta**2)
        E = gamma * m * c
        p = gamma * m * v
        return FourVector(np.concatenate([[E], p]))


# ============================================================
# General Relativity — Geodesic Solver
# ============================================================

class GeodesicSolver:
    """Solve the geodesic equation on a (pseudo-)Riemannian manifold.

    d²x^μ/dτ² + Γ^μ_{αβ} dx^α/dτ dx^β/dτ = 0

    Requires Christoffel symbols as a function of position.
    """

    def __init__(self, dim: int,
                 christoffel: Callable[[np.ndarray], np.ndarray]):
        """
        Parameters
        ----------
        dim : number of spacetime dimensions
        christoffel : Γ(x) -> (dim, dim, dim) array of Γ^μ_{αβ}
        """
        self.dim = dim
        self.christoffel = christoffel
        self.solver = ODESolver()

    def _equations(self, tau: float, state: np.ndarray) -> np.ndarray:
        """Convert geodesic equation to first-order system.

        state = [x^0, ..., x^{d-1}, u^0, ..., u^{d-1}]
        where u^μ = dx^μ/dτ.
        """
        d = self.dim
        x = state[:d]
        u = state[d:]

        Gamma = self.christoffel(x)  # (d, d, d)

        dx_dtau = u
        du_dtau = np.zeros(d)
        for mu in range(d):
            for alpha in range(d):
                for beta in range(d):
                    du_dtau[mu] -= Gamma[mu, alpha, beta] * u[alpha] * u[beta]

        return np.concatenate([dx_dtau, du_dtau])

    def solve(self, x0: np.ndarray, u0: np.ndarray,
              tau_span: Tuple[float, float],
              dt: float = 0.001,
              method: str = "RK4") -> Dict[str, np.ndarray]:
        y0 = np.concatenate([x0, u0])
        result = self.solver.solve(self._equations, y0, tau_span, dt, method)

        t = result["t"]
        y = result["y"]
        return {
            "tau": t,
            "x": y[:, :self.dim],
            "u": y[:, self.dim:],
        }

    @staticmethod
    def schwarzschild(M: float = 1.0, G: float = 1.0,
                       c: float = 1.0) -> GeodesicSolver:
        """Schwarzschild geodesic solver.

        Metric: ds² = -(1-rs/r)c²dt² + (1-rs/r)^{-1}dr² + r²dΩ²

        Coordinates: (t, r, θ, φ).
        """
        rs = 2 * G * M / c**2

        def christoffel(x):
            t, r, theta, phi = x
            Gamma = np.zeros((4, 4, 4))

            f = 1 - rs / r if r > rs else 1e-10

            # Non-zero Christoffel symbols
            Gamma[0, 0, 1] = Gamma[0, 1, 0] = rs / (2 * r**2 * f)
            Gamma[1, 0, 0] = 0.5 * c**2 * rs * f / r**2
            Gamma[1, 1, 1] = -rs / (2 * r**2 * f)
            Gamma[1, 2, 2] = -(r - rs)
            Gamma[1, 3, 3] = -(r - rs) * np.sin(theta)**2
            Gamma[2, 1, 2] = Gamma[2, 2, 1] = 1.0 / r
            Gamma[2, 3, 3] = -np.sin(theta) * np.cos(theta)
            Gamma[3, 1, 3] = Gamma[3, 3, 1] = 1.0 / r
            Gamma[3, 2, 3] = Gamma[3, 3, 2] = np.cos(theta) / np.sin(theta) if abs(np.sin(theta)) > 1e-10 else 0

            return Gamma

        return GeodesicSolver(4, christoffel)

    @staticmethod
    def kerr(M: float = 1.0, a: float = 0.5,
             G: float = 1.0, c: float = 1.0) -> GeodesicSolver:
        """Kerr geodesic solver (rotating black hole).

        Boyer-Lindquist coordinates (t, r, θ, φ).
        Uses numerical differentiation for Christoffel symbols.
        """
        rs = 2 * G * M / c**2

        def metric(x):
            t, r, theta, phi = x
            Sigma = r**2 + a**2 * np.cos(theta)**2
            Delta = r**2 - rs * r + a**2
            sin2 = np.sin(theta)**2

            g = np.zeros((4, 4))
            g[0, 0] = -(1 - rs * r / Sigma) * c**2
            g[0, 3] = g[3, 0] = -rs * r * a * sin2 / Sigma * c
            g[1, 1] = Sigma / Delta
            g[2, 2] = Sigma
            g[3, 3] = (r**2 + a**2 + rs * r * a**2 * sin2 / Sigma) * sin2
            return g

        def christoffel(x):
            """Compute Christoffel symbols numerically."""
            eps = 1e-6
            d = 4
            Gamma = np.zeros((d, d, d))
            g = metric(x)

            # Compute metric derivatives numerically
            dg = np.zeros((d, d, d))
            for k in range(d):
                x_plus = x.copy(); x_plus[k] += eps
                x_minus = x.copy(); x_minus[k] -= eps
                dg[k] = (metric(x_plus) - metric(x_minus)) / (2 * eps)

            g_inv = np.linalg.inv(g)

            for mu in range(d):
                for alpha in range(d):
                    for beta in range(d):
                        for sigma in range(d):
                            Gamma[mu, alpha, beta] += 0.5 * g_inv[mu, sigma] * (
                                dg[alpha][sigma, beta] +
                                dg[beta][sigma, alpha] -
                                dg[sigma][alpha, beta]
                            )
            return Gamma

        return GeodesicSolver(4, christoffel)


# ============================================================
# Einstein Field Equations
# ============================================================

class EinsteinTensor:
    """Compute Einstein tensor from a metric.

    G_{μν} = R_{μν} - ½ g_{μν} R
    """

    @staticmethod
    def compute(metric_func: Callable[[np.ndarray], np.ndarray],
                x: np.ndarray, eps: float = 1e-5) -> np.ndarray:
        """Compute Einstein tensor numerically at point x.

        Uses finite differences for all derivatives.
        """
        d = len(x)
        g = metric_func(x)

        # Metric derivatives
        dg = np.zeros((d, d, d))
        d2g = np.zeros((d, d, d, d))

        for k in range(d):
            xp = x.copy(); xp[k] += eps
            xm = x.copy(); xm[k] -= eps
            dg[k] = (metric_func(xp) - metric_func(xm)) / (2 * eps)

            for l in range(d):
                xpp = x.copy(); xpp[k] += eps; xpp[l] += eps
                xpm = x.copy(); xpm[k] += eps; xpm[l] -= eps
                xmp = x.copy(); xmp[k] -= eps; xmp[l] += eps
                xmm = x.copy(); xmm[k] -= eps; xmm[l] -= eps
                d2g[k, l] = (metric_func(xpp) - metric_func(xpm) -
                              metric_func(xmp) + metric_func(xmm)) / (4 * eps**2)

        g_inv = np.linalg.inv(g)

        # Christoffel symbols
        Gamma = np.zeros((d, d, d))
        for mu in range(d):
            for alpha in range(d):
                for beta in range(d):
                    for sigma in range(d):
                        Gamma[mu, alpha, beta] += 0.5 * g_inv[mu, sigma] * (
                            dg[alpha][sigma, beta] +
                            dg[beta][sigma, alpha] -
                            dg[sigma][alpha, beta]
                        )

        # Riemann tensor
        R = np.zeros((d, d, d, d))
        dGamma = np.zeros((d, d, d, d))

        for k in range(d):
            xp = x.copy(); xp[k] += eps
            xm = x.copy(); xm[k] -= eps

            def gamma_at(pt):
                gp = metric_func(pt)
                dgp = np.zeros((d, d, d))
                for kk in range(d):
                    ptp = pt.copy(); ptp[kk] += eps
                    ptm = pt.copy(); ptm[kk] -= eps
                    dgp[kk] = (metric_func(ptp) - metric_func(ptm)) / (2 * eps)
                gp_inv = np.linalg.inv(gp)
                Gp = np.zeros((d, d, d))
                for mu in range(d):
                    for alpha in range(d):
                        for beta in range(d):
                            for sigma in range(d):
                                Gp[mu, alpha, beta] += 0.5 * gp_inv[mu, sigma] * (
                                    dgp[alpha][sigma, beta] +
                                    dgp[beta][sigma, alpha] -
                                    dgp[sigma][alpha, beta]
                                )
                return Gp

            Gp = gamma_at(xp)
            Gm = gamma_at(xm)
            dGamma[k] = (Gp - Gm) / (2 * eps)

        for rho in range(d):
            for sigma in range(d):
                for mu in range(d):
                    for nu in range(d):
                        R[rho, sigma, mu, nu] = (
                            dGamma[mu][rho, nu, sigma] -
                            dGamma[nu][rho, mu, sigma]
                        )
                        for lam in range(d):
                            R[rho, sigma, mu, nu] += (
                                Gamma[rho, mu, lam] * Gamma[lam, nu, sigma] -
                                Gamma[rho, nu, lam] * Gamma[lam, mu, sigma]
                            )

        # Ricci tensor: R_{σν} = R^μ_{σμν}
        Ricci = np.zeros((d, d))
        for sigma in range(d):
            for nu in range(d):
                for mu in range(d):
                    Ricci[sigma, nu] += R[mu, sigma, mu, nu]

        # Ricci scalar
        R_scalar = 0.0
        for mu in range(d):
            for nu in range(d):
                R_scalar += g_inv[mu, nu] * Ricci[mu, nu]

        # Einstein tensor
        G = Ricci - 0.5 * g * R_scalar

        return G


# ============================================================
# Cosmology — Friedmann Equations
# ============================================================

class FriedmannSolver:
    """Solve Friedmann equations for FLRW cosmology.

    (ȧ/a)² = (8πG/3)ρ - kc²/a² + Λ/3
    ä/a = -(4πG/3)(ρ + 3p/c²) + Λ/3

    Components: radiation (w=1/3), matter (w=0), dark energy (w=-1).
    """

    def __init__(self, H0: float = 70.0,
                 Omega_m: float = 0.3,
                 Omega_r: float = 9e-5,
                 Omega_Lambda: float = 0.7,
                 k: int = 0):
        """
        Parameters
        ----------
        H0 : Hubble constant [km/s/Mpc]
        Omega_m : matter density parameter
        Omega_r : radiation density parameter
        Omega_Lambda : dark energy density parameter
        k : curvature (-1, 0, +1)
        """
        self.H0 = H0
        self.Omega_m = Omega_m
        self.Omega_r = Omega_r
        self.Omega_Lambda = Omega_Lambda
        self.Omega_k = 1 - Omega_m - Omega_r - Omega_Lambda
        self.k = k

    def hubble(self, a: float) -> float:
        """Hubble parameter H(a) = H₀ E(a)."""
        E2 = (self.Omega_r / a**4 +
               self.Omega_m / a**3 +
               self.Omega_k / a**2 +
               self.Omega_Lambda)
        return self.H0 * np.sqrt(max(E2, 0))

    def solve(self, a0: float = 1e-4, a_final: float = 2.0,
              n_points: int = 10000) -> Dict[str, np.ndarray]:
        """Integrate scale factor evolution.

        da/dt = a H(a)
        """
        def f(t, state):
            a = state[0]
            if a < 1e-15:
                return np.array([0.0])
            H = self.hubble(a)
            return np.array([a * H])

        solver = ODESolver()
        t_span = (0, 1.0 / self.H0 * 30)  # cosmic time
        result = solver.solve(f, np.array([a0]), t_span,
                               dt=t_span[1] / n_points, method="RK4")

        t = result["t"]
        a = result["y"][:, 0]

        # Compute derived quantities
        H = np.array([self.hubble(ai) for ai in a])
        redshift = 1.0 / a - 1

        # Proper distance (comoving)
        d_comoving = np.cumsum(1.0 / (a * H + 1e-30) * np.gradient(t))

        return {
            "t": t,
            "a": a,
            "H": H,
            "redshift": redshift,
            "comoving_distance": d_comoving,
        }

    def age_of_universe(self) -> float:
        """Compute age of universe via t₀ = ∫₀¹ da / (a H(a))."""
        from scipy.integrate import quad
        integrand = lambda a: 1.0 / (a * self.hubble(a)) if a > 1e-10 else 0.0
        result, _ = quad(integrand, 1e-6, 1.0)
        return result  # in 1/H0 units

    def luminosity_distance(self, z: float) -> float:
        """Luminosity distance d_L = (1+z) ∫₀ᶻ dz'/H(z')."""
        from scipy.integrate import quad
        integrand = lambda zp: 1.0 / self.hubble(1.0 / (1 + zp))
        result, _ = quad(integrand, 0, z)
        return (1 + z) * result


# ============================================================
# Gravitational Waves (linearised theory)
# ============================================================

class GravitationalWave:
    """Gravitational wave solutions in linearised GR.

    Metric perturbation h_{μν} satisfying □h_{μν} = -16πG T_{μν}/c⁴
    (in Lorenz gauge).
    """

    @staticmethod
    def plus_polarization(amplitude: float, frequency: float,
                           t: float, z: float,
                           c: float = 1.0) -> np.ndarray:
        """h₊ mode at position z and time t.

        Returns 4×4 perturbation matrix.
        """
        phase = 2 * np.pi * frequency * (t - z / c)
        h = np.zeros((4, 4))
        h[1, 1] = amplitude * np.cos(phase)
        h[2, 2] = -amplitude * np.cos(phase)
        return h

    @staticmethod
    def cross_polarization(amplitude: float, frequency: float,
                            t: float, z: float,
                            c: float = 1.0) -> np.ndarray:
        """h× mode."""
        phase = 2 * np.pi * frequency * (t - z / c)
        h = np.zeros((4, 4))
        h[1, 2] = amplitude * np.cos(phase)
        h[2, 1] = amplitude * np.cos(phase)
        return h

    @staticmethod
    def quadrupole_power(I_ddot: np.ndarray, G: float = 1.0,
                          c: float = 1.0) -> float:
        """Gravitational wave power from quadrupole formula.

        P = G/(5c⁵) ⟨Ï_{ij}Ï_{ij}⟩

        I_ddot: third time derivative of quadrupole moment tensor.
        """
        return G / (5 * c**5) * np.sum(I_ddot**2)

    @staticmethod
    def inspiral_frequency(M_chirp: float, t_coalescence: float,
                            t: float, G: float = 1.0,
                            c: float = 1.0) -> float:
        """Gravitational wave frequency during inspiral (leading order).

        f(t) = (1/π) (5/(256(t_c - t)))^{3/8} (G M_c / c³)^{-5/8}
        """
        tau = t_coalescence - t
        if tau <= 0:
            return float('inf')
        return (1.0 / np.pi) * (5.0 / (256 * tau))**(3.0/8) * \
               (G * M_chirp / c**3)**(-5.0/8)

    @staticmethod
    def strain_inspiral(M_chirp: float, distance: float,
                         frequency: float,
                         G: float = 1.0, c: float = 1.0) -> float:
        """Characteristic strain from a compact binary inspiral.

        h ∝ (G M_c)^{5/3} (πf)^{2/3} / (c⁴ d)
        """
        return (4.0 / distance) * (G * M_chirp / c**2)**(5.0/3) * \
               (np.pi * frequency / c)**(2.0/3)
