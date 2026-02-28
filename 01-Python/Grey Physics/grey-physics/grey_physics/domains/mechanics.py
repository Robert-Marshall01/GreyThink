"""
Grey Physics — Mechanics Domain

Full-featured classical mechanics simulation:
  - Lagrangian and Hamiltonian dynamics
  - Rigid body (Euler equations, inertia tensor)
  - Constrained systems (holonomic, non-holonomic)
  - N-body gravitational dynamics
  - Symplectic integration with energy monitoring
  - Central force problems (Kepler, effective potential)
  - Small oscillations and normal modes
"""

from __future__ import annotations

import numpy as np
from typing import Any, Callable, Dict, List, Optional, Tuple

from grey_physics.core.numeric import ODESolver, StabilityMonitor


# ============================================================
# Particle / point mass
# ============================================================

class Particle:
    """A classical point particle."""

    def __init__(self, mass: float, position: np.ndarray,
                 velocity: np.ndarray, name: str = ""):
        self.mass = mass
        self.position = np.asarray(position, dtype=float)
        self.velocity = np.asarray(velocity, dtype=float)
        self.name = name or f"m={mass:.2g}"

    @property
    def momentum(self) -> np.ndarray:
        return self.mass * self.velocity

    @property
    def kinetic_energy(self) -> float:
        return 0.5 * self.mass * float(np.dot(self.velocity, self.velocity))

    def angular_momentum(self, origin: Optional[np.ndarray] = None) -> np.ndarray:
        r = self.position if origin is None else self.position - origin
        if len(r) == 2:
            r3 = np.array([r[0], r[1], 0.0])
            p3 = np.array([self.momentum[0], self.momentum[1], 0.0])
            return np.cross(r3, p3)
        return np.cross(r, self.momentum)


# ============================================================
# Lagrangian Mechanics Simulation
# ============================================================

class LagrangianSimulator:
    """Simulate Lagrangian systems numerically.

    Given L(q, q̇, t) and the derived Euler-Lagrange equations,
    or given the mass matrix M(q) and forces Q(q, q̇, t),
    integrates the equations of motion.
    """

    def __init__(self, dim: int, mass_matrix: Callable,
                 force_vector: Callable,
                 potential: Optional[Callable] = None):
        """
        Parameters
        ----------
        dim : number of generalized coordinates
        mass_matrix : M(q) -> (dim, dim) array
        force_vector : Q(q, qdot, t) -> (dim,) array
        potential : V(q) -> scalar (optional, for energy monitoring)
        """
        self.dim = dim
        self.mass_matrix = mass_matrix
        self.force_vector = force_vector
        self.potential = potential
        self.solver = ODESolver()
        self.monitor = StabilityMonitor()

    def _equations(self, t: float, state: np.ndarray) -> np.ndarray:
        """Convert M(q)q̈ = Q(q,q̇,t) to first-order system."""
        q = state[:self.dim]
        qdot = state[self.dim:]
        M = self.mass_matrix(q)
        Q = self.force_vector(q, qdot, t)
        qddot = np.linalg.solve(M, Q)
        return np.concatenate([qdot, qddot])

    def simulate(self, q0: np.ndarray, qdot0: np.ndarray,
                 t_span: Tuple[float, float],
                 dt: float = 0.01,
                 method: str = "Verlet") -> Dict[str, np.ndarray]:
        """Run the simulation.

        Returns dict with 't', 'q', 'qdot', optionally 'energy'.
        """
        y0 = np.concatenate([q0, qdot0])
        result = self.solver.solve(self._equations, y0, t_span, dt, method,
                                    split_index=self.dim)

        t = result["t"]
        y = result["y"]
        q = y[:, :self.dim]
        qdot = y[:, self.dim:]

        output = {"t": t, "q": q, "qdot": qdot}

        if self.potential is not None:
            energies = []
            for i in range(len(t)):
                M = self.mass_matrix(q[i])
                T = 0.5 * qdot[i] @ M @ qdot[i]
                V = self.potential(q[i])
                E = T + V
                self.monitor.record_energy(E)
                energies.append(E)
            output["energy"] = np.array(energies)

        return output

    @staticmethod
    def simple_pendulum(length: float = 1.0, g: float = 9.81) -> LagrangianSimulator:
        """Factory: simple pendulum."""
        def M(q):
            return np.array([[length**2]])

        def Q(q, qdot, t):
            return np.array([-g * length * np.sin(q[0])])

        def V(q):
            return -g * length * np.cos(q[0])

        return LagrangianSimulator(1, M, Q, V)

    @staticmethod
    def double_pendulum(m1: float = 1.0, m2: float = 1.0,
                        l1: float = 1.0, l2: float = 1.0,
                        g: float = 9.81) -> LagrangianSimulator:
        """Factory: double pendulum (classic chaos demo)."""
        def M(q):
            th1, th2 = q
            M11 = (m1 + m2) * l1**2
            M12 = m2 * l1 * l2 * np.cos(th1 - th2)
            M22 = m2 * l2**2
            return np.array([[M11, M12], [M12, M22]])

        def Q(q, qdot, t):
            th1, th2 = q
            w1, w2 = qdot
            s12 = np.sin(th1 - th2)
            Q1 = -m2 * l1 * l2 * w2**2 * s12 - (m1 + m2) * g * l1 * np.sin(th1)
            Q2 = m2 * l1 * l2 * w1**2 * s12 - m2 * g * l2 * np.sin(th2)
            return np.array([Q1, Q2])

        def V(q):
            th1, th2 = q
            y1 = -l1 * np.cos(th1)
            y2 = y1 - l2 * np.cos(th2)
            return m1 * g * y1 + m2 * g * y2

        return LagrangianSimulator(2, M, Q, V)

    @staticmethod
    def harmonic_oscillator(k: float = 1.0, m: float = 1.0) -> LagrangianSimulator:
        """Factory: 1D harmonic oscillator."""
        def M(q):
            return np.array([[m]])

        def Q(q, qdot, t):
            return np.array([-k * q[0]])

        def V(q):
            return 0.5 * k * q[0]**2

        return LagrangianSimulator(1, M, Q, V)


# ============================================================
# Hamiltonian Mechanics Simulation
# ============================================================

class HamiltonianSimulator:
    """Simulate Hamiltonian systems q̇=∂H/∂p, ṗ=-∂H/∂q.

    Uses symplectic integrators to preserve phase-space structure.
    """

    def __init__(self, dim: int,
                 hamiltonian: Callable[[np.ndarray, np.ndarray], float],
                 dH_dq: Callable[[np.ndarray, np.ndarray], np.ndarray],
                 dH_dp: Callable[[np.ndarray, np.ndarray], np.ndarray]):
        self.dim = dim
        self.hamiltonian = hamiltonian
        self.dH_dq = dH_dq
        self.dH_dp = dH_dp
        self.solver = ODESolver()
        self.monitor = StabilityMonitor()

    def _equations(self, t: float, state: np.ndarray) -> np.ndarray:
        q = state[:self.dim]
        p = state[self.dim:]
        dqdt = self.dH_dp(q, p)
        dpdt = -self.dH_dq(q, p)
        return np.concatenate([dqdt, dpdt])

    def simulate(self, q0: np.ndarray, p0: np.ndarray,
                 t_span: Tuple[float, float],
                 dt: float = 0.01,
                 method: str = "Verlet") -> Dict[str, np.ndarray]:
        y0 = np.concatenate([q0, p0])
        result = self.solver.solve(self._equations, y0, t_span, dt, method,
                                    split_index=self.dim)

        t = result["t"]
        y = result["y"]
        q = y[:, :self.dim]
        p = y[:, self.dim:]

        energies = np.array([self.hamiltonian(q[i], p[i]) for i in range(len(t))])
        for E in energies:
            self.monitor.record_energy(float(E))

        return {"t": t, "q": q, "p": p, "energy": energies}

    @staticmethod
    def kepler(mu: float = 1.0) -> HamiltonianSimulator:
        """Factory: 2D Kepler problem."""
        def H(q, p):
            r = np.sqrt(q[0]**2 + q[1]**2)
            return 0.5 * (p[0]**2 + p[1]**2) - mu / r

        def dH_dq(q, p):
            r = np.sqrt(q[0]**2 + q[1]**2)
            return mu * q / r**3

        def dH_dp(q, p):
            return p.copy()

        return HamiltonianSimulator(2, H, dH_dq, dH_dp)


# ============================================================
# Rigid Body Dynamics
# ============================================================

class RigidBody:
    """3D rigid body with Euler equations.

    Solves angular velocity evolution in body frame:
      I₁ω̇₁ = (I₂ - I₃)ω₂ω₃ + τ₁
      I₂ω̇₂ = (I₃ - I₁)ω₃ω₁ + τ₂
      I₃ω̇₃ = (I₁ - I₂)ω₁ω₂ + τ₃
    """

    def __init__(self, inertia: np.ndarray,
                 torque: Optional[Callable] = None):
        """
        Parameters
        ----------
        inertia : principal moments [I₁, I₂, I₃]
        torque : τ(ω, t) -> [τ₁, τ₂, τ₃]
        """
        self.I = np.asarray(inertia, dtype=float)
        self.torque = torque or (lambda omega, t: np.zeros(3))
        self.solver = ODESolver()

    def _euler_equations(self, t: float, omega: np.ndarray) -> np.ndarray:
        I1, I2, I3 = self.I
        w1, w2, w3 = omega
        tau = self.torque(omega, t)
        dw1 = ((I2 - I3) * w2 * w3 + tau[0]) / I1
        dw2 = ((I3 - I1) * w3 * w1 + tau[1]) / I2
        dw3 = ((I1 - I2) * w1 * w2 + tau[2]) / I3
        return np.array([dw1, dw2, dw3])

    def simulate(self, omega0: np.ndarray,
                 t_span: Tuple[float, float],
                 dt: float = 0.001,
                 method: str = "RK4") -> Dict[str, np.ndarray]:
        result = self.solver.solve(self._euler_equations, omega0,
                                    t_span, dt, method)
        t = result["t"]
        omega = result["y"]

        # Rotational kinetic energy
        energy = 0.5 * np.sum(self.I * omega**2, axis=1)

        # Angular momentum (body frame)
        L = omega * self.I

        return {"t": t, "omega": omega, "energy": energy,
                "angular_momentum": L}

    @staticmethod
    def symmetric_top(I_perp: float, I_par: float) -> RigidBody:
        """Symmetric top (I₁ = I₂ = I_perp, I₃ = I_par)."""
        return RigidBody(np.array([I_perp, I_perp, I_par]))

    @staticmethod
    def asymmetric_top(I1: float, I2: float, I3: float) -> RigidBody:
        """Asymmetric top with I₁ < I₂ < I₃."""
        return RigidBody(np.array([I1, I2, I3]))


# ============================================================
# N-Body Gravitational
# ============================================================

class NBodyGravity:
    """N-body gravitational simulation.

    Solves Newton's law of gravitation:
      m_i a_i = -G Σ_{j≠i} m_i m_j (r_i - r_j) / |r_i - r_j|³
    """

    def __init__(self, masses: List[float], G: float = 1.0,
                 softening: float = 1e-4):
        self.masses = np.array(masses, dtype=float)
        self.G = G
        self.softening = softening
        self.n = len(masses)
        self.solver = ODESolver()

    def _equations(self, t: float, state: np.ndarray) -> np.ndarray:
        """state = [x1, y1, z1, x2, y2, z2, ..., vx1, vy1, vz1, ...]"""
        n = self.n
        pos = state[:3*n].reshape(n, 3)
        vel = state[3*n:].reshape(n, 3)

        acc = np.zeros((n, 3))
        for i in range(n):
            for j in range(n):
                if i != j:
                    r_ij = pos[j] - pos[i]
                    dist = np.sqrt(np.dot(r_ij, r_ij) + self.softening**2)
                    acc[i] += self.G * self.masses[j] * r_ij / dist**3

        return np.concatenate([vel.flatten(), acc.flatten()])

    def simulate(self, positions: np.ndarray, velocities: np.ndarray,
                 t_span: Tuple[float, float],
                 dt: float = 0.001,
                 method: str = "Verlet") -> Dict[str, np.ndarray]:
        y0 = np.concatenate([positions.flatten(), velocities.flatten()])
        result = self.solver.solve(self._equations, y0, t_span, dt, method,
                                    split_index=3*self.n)

        t = result["t"]
        y = result["y"]
        pos = y[:, :3*self.n].reshape(len(t), self.n, 3)
        vel = y[:, 3*self.n:].reshape(len(t), self.n, 3)

        # Total energy
        energies = []
        for k in range(len(t)):
            T = sum(0.5 * self.masses[i] * np.dot(vel[k, i], vel[k, i])
                    for i in range(self.n))
            V = 0.0
            for i in range(self.n):
                for j in range(i + 1, self.n):
                    r = np.linalg.norm(pos[k, i] - pos[k, j])
                    V -= self.G * self.masses[i] * self.masses[j] / max(r, self.softening)
            energies.append(T + V)

        return {"t": t, "positions": pos, "velocities": vel,
                "energy": np.array(energies)}

    @staticmethod
    def two_body(m1: float = 1.0, m2: float = 1.0) -> NBodyGravity:
        return NBodyGravity([m1, m2])

    @staticmethod
    def three_body_figure_eight() -> Tuple[NBodyGravity, np.ndarray, np.ndarray]:
        """Famous figure-eight solution initial conditions."""
        nb = NBodyGravity([1.0, 1.0, 1.0])
        pos = np.array([
            [-0.97000436, 0.24308753, 0.0],
            [0.97000436, -0.24308753, 0.0],
            [0.0, 0.0, 0.0],
        ])
        vel = np.array([
            [0.4662036850, 0.4323657300, 0.0],
            [0.4662036850, 0.4323657300, 0.0],
            [-0.93240737, -0.86473146, 0.0],
        ])
        return nb, pos, vel


# ============================================================
# Constrained Systems
# ============================================================

class ConstrainedSystem:
    """Systems with holonomic or non-holonomic constraints.

    Uses the method of Lagrange multipliers or Baumgarte stabilization.
    """

    def __init__(self, dim: int,
                 mass_matrix: Callable,
                 forces: Callable,
                 constraints: List[Callable],
                 constraint_jacobians: List[Callable]):
        """
        Parameters
        ----------
        constraints : list of g_k(q) = 0
        constraint_jacobians : list of ∂g_k/∂q (q) -> row vector
        """
        self.dim = dim
        self.mass_matrix = mass_matrix
        self.forces = forces
        self.constraints = constraints
        self.jacobians = constraint_jacobians
        self.solver = ODESolver()

    def _equations(self, t: float, state: np.ndarray) -> np.ndarray:
        """Constrained EOM: M q̈ + G^T λ = Q, g(q) = 0."""
        q = state[:self.dim]
        qdot = state[self.dim:2*self.dim]

        M = self.mass_matrix(q)
        Q = self.forces(q, qdot, t)
        n_c = len(self.constraints)

        # Construct augmented system
        G = np.zeros((n_c, self.dim))
        for k in range(n_c):
            G[k] = self.jacobians[k](q)

        # Baumgarte stabilization
        alpha_b = 5.0
        beta_b = 5.0
        g_vals = np.array([c(q) for c in self.constraints])
        gdot_vals = G @ qdot
        stabilization = -2 * alpha_b * gdot_vals - beta_b**2 * g_vals

        # Solve [M, G^T; G, 0] [qddot; lambda] = [Q; stab]
        n_total = self.dim + n_c
        A = np.zeros((n_total, n_total))
        A[:self.dim, :self.dim] = M
        A[:self.dim, self.dim:] = G.T
        A[self.dim:, :self.dim] = G

        b = np.zeros(n_total)
        b[:self.dim] = Q
        b[self.dim:] = stabilization

        sol = np.linalg.solve(A, b)
        qddot = sol[:self.dim]

        return np.concatenate([qdot, qddot])

    def simulate(self, q0: np.ndarray, qdot0: np.ndarray,
                 t_span: Tuple[float, float],
                 dt: float = 0.001) -> Dict[str, np.ndarray]:
        y0 = np.concatenate([q0, qdot0])
        result = self.solver.solve(self._equations, y0, t_span, dt, "RK4")

        t = result["t"]
        y = result["y"]
        q = y[:, :self.dim]
        qdot = y[:, self.dim:]

        # Constraint violations
        violations = np.array([
            [c(q[i]) for c in self.constraints]
            for i in range(len(t))
        ])

        return {"t": t, "q": q, "qdot": qdot,
                "constraint_violations": violations}


# ============================================================
# Central Force
# ============================================================

class CentralForce:
    """Central force problems: F(r) r̂.

    Reduces to effective 1D problem with effective potential:
      V_eff(r) = V(r) + L²/(2mr²)
    """

    def __init__(self, potential: Callable[[float], float],
                 force: Callable[[float], float],
                 mass: float = 1.0):
        self.potential = potential
        self.force = force
        self.mass = mass

    def effective_potential(self, r: float, L: float) -> float:
        """V_eff = V(r) + L²/(2mr²)."""
        return self.potential(r) + L**2 / (2 * self.mass * r**2)

    def orbit_equation(self, r0: float, rdot0: float,
                       theta0: float, L: float,
                       t_span: Tuple[float, float],
                       dt: float = 0.001) -> Dict[str, np.ndarray]:
        """Integrate the orbit in polar coordinates."""
        def eqns(t, state):
            r, rdot, theta = state
            r_safe = max(abs(r), 1e-10)
            rddot = L**2 / (self.mass * r_safe**3) + self.force(r_safe) / self.mass
            thetadot = L / (self.mass * r_safe**2)
            return np.array([rdot, rddot, thetadot])

        y0 = np.array([r0, rdot0, theta0])
        solver = ODESolver()
        result = solver.solve(eqns, y0, t_span, dt, "RK4")

        t = result["t"]
        y = result["y"]
        r = y[:, 0]
        theta = y[:, 2]
        x = r * np.cos(theta)
        y_coord = r * np.sin(theta)

        return {"t": t, "r": r, "theta": theta, "x": x, "y": y_coord}

    @staticmethod
    def gravity(M: float = 1.0, m: float = 1.0, G: float = 1.0) -> CentralForce:
        """Newtonian gravity: V = -GMm/r, F = -GMm/r²."""
        def V(r):
            return -G * M * m / r

        def F(r):
            return -G * M * m / r**2

        return CentralForce(V, F, m)


# ============================================================
# Normal modes / small oscillations
# ============================================================

class NormalModes:
    """Compute normal modes for small oscillations about equilibrium.

    Given T = ½ q̇ᵀ M q̇ and V = ½ qᵀ K q,
    normal modes satisfy (K - ω² M) a = 0.
    """

    def __init__(self, mass_matrix: np.ndarray, stiffness_matrix: np.ndarray):
        self.M = np.asarray(mass_matrix, dtype=float)
        self.K = np.asarray(stiffness_matrix, dtype=float)

    def compute(self) -> Tuple[np.ndarray, np.ndarray]:
        """Compute normal mode frequencies and mode shapes.

        Returns (frequencies, mode_shapes) where frequencies are
        the ω values and mode_shapes columns are the eigenvectors.
        """
        # Solve generalized eigenvalue problem K a = ω² M a
        M_inv = np.linalg.inv(self.M)
        A = M_inv @ self.K
        eigenvalues, eigenvectors = np.linalg.eig(A)

        # ω = √λ (only real, positive eigenvalues)
        omega_sq = np.real(eigenvalues)
        omega_sq = np.maximum(omega_sq, 0)
        frequencies = np.sqrt(omega_sq)

        idx = np.argsort(frequencies)
        return frequencies[idx], np.real(eigenvectors[:, idx])

    def time_evolution(self, q0: np.ndarray, qdot0: np.ndarray,
                       t: np.ndarray) -> np.ndarray:
        """Evolve the system using normal mode decomposition."""
        frequencies, modes = self.compute()
        n = len(frequencies)

        # Transform to normal coordinates
        M_modes = modes.T @ self.M @ modes
        q0_normal = np.linalg.solve(M_modes, modes.T @ self.M @ q0)
        qdot0_normal = np.linalg.solve(M_modes, modes.T @ self.M @ qdot0)

        # Evolve each normal mode independently
        q_t = np.zeros((len(t), n))
        for i in range(n):
            if frequencies[i] > 1e-10:
                q_t[:, i] = (q0_normal[i] * np.cos(frequencies[i] * t) +
                              qdot0_normal[i] / frequencies[i] * np.sin(frequencies[i] * t))
            else:
                q_t[:, i] = q0_normal[i] + qdot0_normal[i] * t

        # Transform back
        return (modes @ q_t.T).T
