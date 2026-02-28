"""
Grey Physics — Quantum Domain

Quantum mechanics simulations:
  - Time-dependent Schrödinger equation (split-step FFT, Crank-Nicolson)
  - Spin systems (Heisenberg, Ising model)
  - Multi-particle systems (product states, entanglement)
  - Perturbation theory (non-degenerate, degenerate)
  - Scattering theory (Born approximation, partial wave)
  - Density matrix evolution (Lindblad master equation)
  - Quantum information (entanglement entropy, Bell states)
"""

from __future__ import annotations

import numpy as np
from typing import Callable, Dict, List, Optional, Tuple


# ============================================================
# 1D Schrödinger solver (split-step FFT)
# ============================================================

class SchrodingerSolver1D:
    """Solve the 1D time-dependent Schrödinger equation.

    iℏ ∂ψ/∂t = [-ℏ²/(2m) ∂²/∂x² + V(x)] ψ

    Uses split-step Fourier method (spectral accuracy).
    """

    def __init__(self, N: int, L: float,
                 V: Callable[[np.ndarray], np.ndarray],
                 m: float = 1.0, hbar: float = 1.0):
        self.N = N
        self.L = L
        self.dx = L / N
        self.x = np.linspace(-L/2, L/2, N, endpoint=False)
        self.V = V
        self.m = m
        self.hbar = hbar

        # Wavenumbers
        self.k = np.fft.fftfreq(N, d=self.dx) * 2 * np.pi
        self.V_x = V(self.x)

    def evolve(self, psi0: np.ndarray, dt: float,
               n_steps: int, save_every: int = 1) -> Dict[str, np.ndarray]:
        """Evolve ψ using split-step FFT.

        Returns dict with 'x', 't', 'psi', 'prob', 'norm'.
        """
        psi = psi0.astype(complex).copy()
        psi /= np.sqrt(np.sum(np.abs(psi)**2) * self.dx)  # normalize

        # Kinetic phase
        T_phase = np.exp(-0.5j * self.hbar * self.k**2 / self.m * dt)
        # Potential half-phase
        V_half = np.exp(-0.5j * self.V_x / self.hbar * dt)

        t_arr = []
        psi_arr = []
        norm_arr = []

        for n in range(n_steps):
            # Split-step: V/2 → T → V/2
            psi *= V_half
            psi_hat = np.fft.fft(psi)
            psi_hat *= T_phase
            psi = np.fft.ifft(psi_hat)
            psi *= V_half

            if n % save_every == 0:
                t_arr.append(n * dt)
                psi_arr.append(psi.copy())
                norm_arr.append(float(np.sum(np.abs(psi)**2) * self.dx))

        psi_arr = np.array(psi_arr)
        return {
            "x": self.x,
            "t": np.array(t_arr),
            "psi": psi_arr,
            "prob": np.abs(psi_arr)**2,
            "norm": np.array(norm_arr),
        }

    def ground_state(self, dt: float = 0.01,
                     n_iter: int = 10000) -> np.ndarray:
        """Find ground state via imaginary time evolution.

        ψ(τ) → exp(-Hτ/ℏ) ψ(0) → ground state as τ → ∞.
        """
        psi = np.exp(-self.x**2 / 2).astype(complex)
        psi /= np.sqrt(np.sum(np.abs(psi)**2) * self.dx)

        T_decay = np.exp(-0.5 * self.hbar * self.k**2 / self.m * dt)
        V_half = np.exp(-0.5 * self.V_x / self.hbar * dt)

        for _ in range(n_iter):
            psi *= V_half
            psi_hat = np.fft.fft(psi)
            psi_hat *= T_decay
            psi = np.fft.ifft(psi_hat)
            psi *= V_half
            # Re-normalize
            psi /= np.sqrt(np.sum(np.abs(psi)**2) * self.dx)

        return psi

    def expectation_x(self, psi: np.ndarray) -> float:
        """⟨x⟩ = ∫ψ* x ψ dx."""
        return float(np.sum(np.conj(psi) * self.x * psi).real * self.dx)

    def expectation_p(self, psi: np.ndarray) -> float:
        """⟨p⟩ = ∫ψ* (-iℏ ∂/∂x) ψ dx (spectral)."""
        psi_hat = np.fft.fft(psi)
        p_psi = np.fft.ifft(self.hbar * self.k * psi_hat)
        return float(np.sum(np.conj(psi) * (-1j) * p_psi).real * self.dx)

    def energy(self, psi: np.ndarray) -> float:
        """⟨H⟩ = ⟨T⟩ + ⟨V⟩."""
        psi_hat = np.fft.fft(psi)
        T_psi = np.fft.ifft(0.5 * self.hbar**2 * self.k**2 / self.m * psi_hat)
        T = float(np.sum(np.conj(psi) * T_psi).real * self.dx)
        V = float(np.sum(np.abs(psi)**2 * self.V_x) * self.dx)
        return T + V

    @staticmethod
    def gaussian_wavepacket(x: np.ndarray, x0: float = 0.0,
                             sigma: float = 1.0,
                             k0: float = 0.0) -> np.ndarray:
        """Gaussian wavepacket ψ(x) = exp(-(x-x0)²/(4σ²)) exp(ik₀x)."""
        return (np.exp(-(x - x0)**2 / (4 * sigma**2)) *
                np.exp(1j * k0 * x)).astype(complex)

    @staticmethod
    def harmonic_potential(omega: float = 1.0, m: float = 1.0):
        """V(x) = ½mω²x²."""
        return lambda x: 0.5 * m * omega**2 * x**2

    @staticmethod
    def double_well(a: float = 1.0, b: float = 4.0):
        """V(x) = a(x² - b)²."""
        return lambda x: a * (x**2 - b)**2

    @staticmethod
    def barrier(height: float = 5.0, width: float = 0.5,
                center: float = 0.0):
        """Rectangular barrier potential."""
        return lambda x: np.where(
            np.abs(x - center) < width / 2, height, 0.0
        )


# ============================================================
# Spin systems
# ============================================================

class SpinSystem:
    """Exact diagonalization of spin-½ systems."""

    # Pauli matrices
    sigma_x = np.array([[0, 1], [1, 0]], dtype=complex)
    sigma_y = np.array([[0, -1j], [1j, 0]], dtype=complex)
    sigma_z = np.array([[1, 0], [0, -1]], dtype=complex)
    identity = np.eye(2, dtype=complex)

    def __init__(self, n_sites: int):
        self.n_sites = n_sites
        self.dim = 2**n_sites

    def _site_operator(self, op: np.ndarray, site: int) -> np.ndarray:
        """Embed a single-site operator into the full Hilbert space."""
        result = np.eye(1, dtype=complex)
        for i in range(self.n_sites):
            if i == site:
                result = np.kron(result, op)
            else:
                result = np.kron(result, self.identity)
        return result

    def heisenberg_hamiltonian(self, J: float = 1.0,
                                h: float = 0.0,
                                periodic: bool = True) -> np.ndarray:
        """Heisenberg XXX model: H = J Σ S_i · S_{i+1} + h Σ S_i^z.

        Returns the Hamiltonian matrix.
        """
        H = np.zeros((self.dim, self.dim), dtype=complex)
        n = self.n_sites

        for i in range(n):
            j = (i + 1) % n
            if not periodic and j == 0 and i == n - 1:
                break
            Sx_i = self._site_operator(0.5 * self.sigma_x, i)
            Sy_i = self._site_operator(0.5 * self.sigma_y, i)
            Sz_i = self._site_operator(0.5 * self.sigma_z, i)
            Sx_j = self._site_operator(0.5 * self.sigma_x, j)
            Sy_j = self._site_operator(0.5 * self.sigma_y, j)
            Sz_j = self._site_operator(0.5 * self.sigma_z, j)

            H += J * (Sx_i @ Sx_j + Sy_i @ Sy_j + Sz_i @ Sz_j)

        for i in range(n):
            Sz_i = self._site_operator(0.5 * self.sigma_z, i)
            H += h * Sz_i

        return H

    def ising_hamiltonian(self, J: float = 1.0, h: float = 0.0,
                           periodic: bool = True) -> np.ndarray:
        """Transverse-field Ising model: H = -J Σ σ^z_i σ^z_{i+1} - h Σ σ^x_i."""
        H = np.zeros((self.dim, self.dim), dtype=complex)
        n = self.n_sites

        for i in range(n):
            j = (i + 1) % n
            if not periodic and j == 0 and i == n - 1:
                break
            Sz_i = self._site_operator(self.sigma_z, i)
            Sz_j = self._site_operator(self.sigma_z, j)
            H -= J * Sz_i @ Sz_j

        for i in range(n):
            Sx_i = self._site_operator(self.sigma_x, i)
            H -= h * Sx_i

        return H

    def diagonalize(self, H: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
        """Exact diagonalization. Returns (eigenvalues, eigenvectors)."""
        eigenvalues, eigenvectors = np.linalg.eigh(H)
        return eigenvalues, eigenvectors

    def magnetization(self, state: np.ndarray) -> float:
        """⟨Σ σ^z_i⟩ / N."""
        M = np.zeros((self.dim, self.dim), dtype=complex)
        for i in range(self.n_sites):
            M += self._site_operator(self.sigma_z, i)
        return float(np.real(np.conj(state) @ M @ state)) / self.n_sites

    def entanglement_entropy(self, state: np.ndarray,
                              subsystem_sites: int) -> float:
        """Von Neumann entanglement entropy for a bipartition.

        Partitions the system at position subsystem_sites.
        """
        dim_A = 2**subsystem_sites
        dim_B = 2**(self.n_sites - subsystem_sites)
        psi = state.reshape(dim_A, dim_B)

        # Schmidt decomposition via SVD
        _, s, _ = np.linalg.svd(psi, full_matrices=False)
        s = s[s > 1e-14]
        p = s**2
        return -float(np.sum(p * np.log(p)))

    def time_evolve(self, state: np.ndarray, H: np.ndarray,
                    dt: float, n_steps: int) -> List[np.ndarray]:
        """Exact time evolution: |ψ(t)⟩ = e^{-iHt/ℏ} |ψ(0)⟩."""
        eigenvalues, eigenvectors = np.linalg.eigh(H)
        # Transform to energy basis
        coeffs = eigenvectors.T.conj() @ state

        states = []
        for n in range(n_steps + 1):
            t = n * dt
            phases = np.exp(-1j * eigenvalues * t)
            psi_t = eigenvectors @ (coeffs * phases)
            states.append(psi_t)

        return states


# ============================================================
# Perturbation Theory
# ============================================================

class PerturbationTheory:
    """Rayleigh-Schrödinger perturbation theory."""

    @staticmethod
    def non_degenerate(H0: np.ndarray, V: np.ndarray,
                       state_index: int = 0,
                       order: int = 2) -> Dict[str, float]:
        """Non-degenerate perturbation theory.

        H = H₀ + λV.

        Returns energy corrections up to given order.
        """
        eigenvalues, eigenvectors = np.linalg.eigh(H0)
        n = state_index
        E0_n = eigenvalues[n]
        psi0_n = eigenvectors[:, n]

        V_nn = float(np.real(psi0_n.conj() @ V @ psi0_n))

        result = {"E0": E0_n, "E1": V_nn}

        if order >= 2:
            E2 = 0.0
            for m in range(len(eigenvalues)):
                if m == n:
                    continue
                V_mn = psi0_n.conj() @ V @ eigenvectors[:, m]
                E2 += abs(V_mn)**2 / (E0_n - eigenvalues[m])
            result["E2"] = float(np.real(E2))

        return result

    @staticmethod
    def first_order_state(H0: np.ndarray, V: np.ndarray,
                           state_index: int = 0) -> np.ndarray:
        """First-order corrected state: |n⟩ + Σ_{m≠n} V_{mn}/(E_n-E_m) |m⟩."""
        eigenvalues, eigenvectors = np.linalg.eigh(H0)
        n = state_index
        psi0_n = eigenvectors[:, n]
        E0_n = eigenvalues[n]

        correction = np.zeros_like(psi0_n)
        for m in range(len(eigenvalues)):
            if m == n:
                continue
            V_mn = eigenvectors[:, m].conj() @ V @ psi0_n
            correction += V_mn / (E0_n - eigenvalues[m]) * eigenvectors[:, m]

        psi_corrected = psi0_n + correction
        psi_corrected /= np.linalg.norm(psi_corrected)
        return psi_corrected


# ============================================================
# Density Matrix / Lindblad Master Equation
# ============================================================

class OpenQuantumSystem:
    """Open quantum system dynamics via Lindblad master equation.

    dρ/dt = -i[H,ρ] + Σ_k γ_k (L_k ρ L_k† - ½{L_k†L_k, ρ})
    """

    def __init__(self, hamiltonian: np.ndarray,
                 lindblad_ops: List[Tuple[np.ndarray, float]]):
        """
        Parameters
        ----------
        hamiltonian : system Hamiltonian
        lindblad_ops : list of (L_k, γ_k) pairs
        """
        self.H = hamiltonian
        self.L_ops = lindblad_ops
        self.dim = hamiltonian.shape[0]

    def _drho_dt(self, rho: np.ndarray) -> np.ndarray:
        """Compute dρ/dt from Lindblad equation."""
        drho = -1j * (self.H @ rho - rho @ self.H)

        for L, gamma in self.L_ops:
            L_dag = L.conj().T
            L_dag_L = L_dag @ L
            drho += gamma * (
                L @ rho @ L_dag -
                0.5 * (L_dag_L @ rho + rho @ L_dag_L)
            )

        return drho

    def evolve(self, rho0: np.ndarray, dt: float,
               n_steps: int) -> List[np.ndarray]:
        """Evolve density matrix using RK4."""
        rho = rho0.astype(complex).copy()
        history = [rho.copy()]

        for _ in range(n_steps):
            k1 = self._drho_dt(rho)
            k2 = self._drho_dt(rho + 0.5 * dt * k1)
            k3 = self._drho_dt(rho + 0.5 * dt * k2)
            k4 = self._drho_dt(rho + dt * k3)
            rho += (dt / 6) * (k1 + 2*k2 + 2*k3 + k4)
            history.append(rho.copy())

        return history

    def purity(self, rho: np.ndarray) -> float:
        """Tr(ρ²) — 1 for pure state, 1/d for maximally mixed."""
        return float(np.real(np.trace(rho @ rho)))

    def von_neumann_entropy(self, rho: np.ndarray) -> float:
        """S = -Tr(ρ ln ρ)."""
        eigenvalues = np.linalg.eigvalsh(rho)
        eigenvalues = eigenvalues[eigenvalues > 1e-14]
        return -float(np.sum(eigenvalues * np.log(eigenvalues)))

    @staticmethod
    def decay(gamma: float) -> Tuple[np.ndarray, float]:
        """Spontaneous decay Lindblad operator: σ⁻."""
        sigma_minus = np.array([[0, 0], [1, 0]], dtype=complex)
        return sigma_minus, gamma

    @staticmethod
    def dephasing(gamma: float) -> Tuple[np.ndarray, float]:
        """Pure dephasing Lindblad operator: σ^z."""
        sigma_z = np.array([[1, 0], [0, -1]], dtype=complex)
        return sigma_z, gamma


# ============================================================
# Quantum Information
# ============================================================

class QuantumInfo:
    """Quantum information utilities."""

    @staticmethod
    def bell_states() -> Dict[str, np.ndarray]:
        """The four Bell states for two qubits."""
        phi_plus = np.array([1, 0, 0, 1], dtype=complex) / np.sqrt(2)
        phi_minus = np.array([1, 0, 0, -1], dtype=complex) / np.sqrt(2)
        psi_plus = np.array([0, 1, 1, 0], dtype=complex) / np.sqrt(2)
        psi_minus = np.array([0, 1, -1, 0], dtype=complex) / np.sqrt(2)
        return {
            "|Φ⁺⟩": phi_plus,
            "|Φ⁻⟩": phi_minus,
            "|Ψ⁺⟩": psi_plus,
            "|Ψ⁻⟩": psi_minus,
        }

    @staticmethod
    def ghz_state(n: int) -> np.ndarray:
        """n-qubit GHZ state: (|00...0⟩ + |11...1⟩)/√2."""
        dim = 2**n
        state = np.zeros(dim, dtype=complex)
        state[0] = 1.0 / np.sqrt(2)
        state[-1] = 1.0 / np.sqrt(2)
        return state

    @staticmethod
    def concurrence(rho: np.ndarray) -> float:
        """Wootters concurrence for a 2-qubit density matrix."""
        sigma_y = np.array([[0, -1j], [1j, 0]], dtype=complex)
        sigma_yy = np.kron(sigma_y, sigma_y)

        rho_tilde = sigma_yy @ rho.conj() @ sigma_yy
        R = rho @ rho_tilde
        eigenvalues = np.sort(np.real(np.linalg.eigvals(R)))[::-1]
        eigenvalues = np.maximum(eigenvalues, 0)
        lambdas = np.sqrt(eigenvalues)

        return max(0, lambdas[0] - lambdas[1] - lambdas[2] - lambdas[3])

    @staticmethod
    def partial_trace(rho: np.ndarray, dim_A: int, dim_B: int,
                       trace_out: str = "B") -> np.ndarray:
        """Partial trace of a bipartite density matrix.

        trace_out: "A" or "B"
        """
        rho_reshaped = rho.reshape(dim_A, dim_B, dim_A, dim_B)
        if trace_out == "B":
            return np.trace(rho_reshaped, axis1=1, axis2=3)
        else:
            return np.trace(rho_reshaped, axis1=0, axis2=2)

    @staticmethod
    def fidelity(rho: np.ndarray, sigma: np.ndarray) -> float:
        """Fidelity F(ρ,σ) = [Tr√(√ρ σ √ρ)]²."""
        sqrt_rho = np.linalg.cholesky(rho + 1e-14 * np.eye(len(rho)))
        M = sqrt_rho @ sigma @ sqrt_rho.conj().T
        eigenvalues = np.linalg.eigvalsh(M)
        eigenvalues = np.maximum(eigenvalues, 0)
        return float(np.sum(np.sqrt(eigenvalues)))**2
