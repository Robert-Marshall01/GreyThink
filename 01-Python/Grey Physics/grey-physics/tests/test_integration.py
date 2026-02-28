"""Integration tests — end-to-end scenarios combining multiple modules."""
import numpy as np
import pytest


class TestMechanicsIntegration:
    """Full pipeline: define Lagrangian → simulate → analyse."""

    def test_pendulum_energy_conservation(self):
        from grey_physics.domains.mechanics import LagrangianSimulator
        sim = LagrangianSimulator.simple_pendulum(length=1.0, g=9.81)
        ts, qs, dqs = sim.simulate(
            q0=np.array([0.2]),
            dq0=np.array([0.0]),
            t_span=(0, 5),
            dt=0.0005,
        )
        # Energy E = ½ml²θ̇² + mgl(1-cosθ)
        l, g = 1.0, 9.81
        E = 0.5 * l ** 2 * dqs[:, 0] ** 2 + g * l * (1 - np.cos(qs[:, 0]))
        drift = (E.max() - E.min()) / E.mean()
        assert drift < 0.05  # < 5% energy drift


class TestEMIntegration:
    def test_coulomb_law(self):
        """Coulomb field magnitude falls as 1/r²."""
        from grey_physics.domains.electromagnetism import Electrostatics
        es = Electrostatics()
        E1 = np.linalg.norm(es.point_charge_field(1.0, np.zeros(3),
                                                    np.array([1, 0, 0])))
        E2 = np.linalg.norm(es.point_charge_field(1.0, np.zeros(3),
                                                    np.array([2, 0, 0])))
        ratio = E1 / E2
        assert abs(ratio - 4.0) < 0.01  # Should be 4 (inverse square)


class TestQuantumIntegration:
    def test_harmonic_oscillator_spectrum(self):
        """First few energy levels of quantum harmonic oscillator."""
        from grey_physics.domains.quantum import SchrodingerSolver1D
        solver = SchrodingerSolver1D.harmonic_potential(
            N=512, L=12.0, omega=1.0
        )
        # Find ground state
        E0, psi0 = solver.find_ground_state(n_steps=10000, dt_imag=0.0005)
        assert abs(E0 - 0.5) < 0.15  # ℏω/2


class TestChaosIntegration:
    def test_lorenz_full_pipeline(self):
        """Lorenz attractor → Lyapunov exponent → fractal dimension."""
        from grey_physics.domains.chaos import (
            StrangeAttractor, LyapunovComputer, FractalDimension,
        )
        # 1. Generate trajectory
        att = StrangeAttractor.lorenz()
        traj = att.integrate(y0=np.array([1, 1, 1]),
                             t_span=(0, 50), dt=0.01)
        assert traj.shape[0] > 100

        # 2. Compute Lyapunov exponents
        lc = LyapunovComputer.lorenz_jacobian()
        def lorenz_rhs(t, y):
            sigma, rho, beta = 10, 28, 8 / 3
            return np.array([
                sigma * (y[1] - y[0]),
                y[0] * (rho - y[2]) - y[1],
                y[0] * y[1] - beta * y[2],
            ])
        exps = lc.compute(lorenz_rhs, y0=np.array([1, 1, 1]), T=20, dt=0.01)
        assert max(exps) > 0.5  # λ₁ ≈ 0.91

        # 3. Fractal dimension of attractor
        fd = FractalDimension()
        dim = fd.correlation_dimension(traj[::10], r_range=(0.5, 20), n_r=15)
        assert 1.5 < dim < 3.0  # Lorenz attractor dim ≈ 2.06


class TestRelativityIntegration:
    def test_schwarzschild_geodesic(self):
        """Timelike geodesic around a black hole stays bounded."""
        from grey_physics.domains.relativity import GeodesicSolver
        geo = GeodesicSolver.schwarzschild(M=1.0)
        # Circular-ish orbit: r=10M, appropriate angular momentum
        y0 = np.array([0, 10.0, np.pi / 2, 0.0])
        # Initial velocities for near-circular orbit
        r0 = 10.0
        dphi_dtau = np.sqrt(1.0 / (r0 ** 2 * (r0 - 3)))  # approximate
        v0 = np.array([1.0, 0.0, 0.0, dphi_dtau])
        taus, trajectory = geo.solve(y0, v0, tau_span=(0, 500), n_steps=5000)
        r_vals = trajectory[:, 1]
        # Radius should stay bounded (not plunge to r=0 or escape)
        assert r_vals.min() > 2.0  # Outside event horizon
        assert r_vals.max() < 50.0


class TestFieldTheoryIntegration:
    def test_sine_gordon_kink(self):
        """Sine-Gordon kink propagation preserves shape."""
        from grey_physics.domains.field_theory import ScalarFieldTheory
        sft = ScalarFieldTheory.sine_gordon(N=200, dx=0.05)
        E0 = sft.energy()
        sft.step(n_steps=100)
        Ef = sft.energy()
        # Energy should be conserved
        assert abs(Ef - E0) / (abs(E0) + 1e-10) < 0.1
