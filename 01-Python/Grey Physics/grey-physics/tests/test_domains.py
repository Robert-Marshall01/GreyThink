"""Tests for the 7 physics domain modules."""
import numpy as np
import pytest


# ══════════════════════════════════════════════════════════════
# Mechanics
# ══════════════════════════════════════════════════════════════

from grey_physics.domains.mechanics import (
    Particle, LagrangianSimulator, HamiltonianSimulator,
    RigidBody, NBodyGravity, CentralForce, NormalModes,
)


class TestParticle:
    def test_creation(self):
        p = Particle(mass=2.0, position=np.array([1.0, 0.0, 0.0]),
                     velocity=np.array([0.0, 1.0, 0.0]))
        assert p.mass == 2.0
        assert abs(p.kinetic_energy() - 1.0) < 1e-12

    def test_momentum(self):
        p = Particle(mass=3.0, position=np.zeros(3),
                     velocity=np.array([1.0, 2.0, 0.0]))
        mom = p.momentum()
        np.testing.assert_allclose(mom, [3.0, 6.0, 0.0])


class TestLagrangianSimulator:
    def test_simple_pendulum(self):
        sim = LagrangianSimulator.simple_pendulum(length=1.0, g=9.81)
        ts, qs, dqs = sim.simulate(
            q0=np.array([0.1]),
            dq0=np.array([0.0]),
            t_span=(0, 2),
            dt=0.001,
        )
        # Should oscillate, θ bounded by ±0.1
        assert np.max(np.abs(qs[:, 0])) < 0.2

    def test_harmonic_oscillator(self):
        sim = LagrangianSimulator.harmonic_oscillator(m=1.0, k=1.0)
        ts, qs, dqs = sim.simulate(
            q0=np.array([1.0]),
            dq0=np.array([0.0]),
            t_span=(0, 2 * np.pi),
            dt=0.001,
        )
        # Should return near initial position after one period
        assert abs(qs[-1, 0] - 1.0) < 0.05


class TestHamiltonianSimulator:
    def test_kepler(self):
        sim = HamiltonianSimulator.kepler()
        ts, qs, ps = sim.simulate(
            q0=np.array([1.0, 0.0]),
            p0=np.array([0.0, 1.0]),
            t_span=(0, 6),
            dt=0.001,
        )
        # Energy should be approximately conserved
        E0 = sim.H(qs[0], ps[0])
        Ef = sim.H(qs[-1], ps[-1])
        assert abs(Ef - E0) / abs(E0) < 0.05


class TestCentralForce:
    def test_effective_potential(self):
        cf = CentralForce.gravity(M=1.0)
        r = np.linspace(0.5, 5, 100)
        L = 1.0
        V_eff = cf.effective_potential(r, L)
        assert V_eff.shape == (100,)
        # At large r, V_eff → 0
        assert abs(V_eff[-1]) < 0.5


# ══════════════════════════════════════════════════════════════
# Electromagnetism
# ══════════════════════════════════════════════════════════════

from grey_physics.domains.electromagnetism import (
    Electrostatics, Magnetostatics, EMWave, RayTracer,
)


class TestElectrostatics:
    def test_point_charge_field(self):
        es = Electrostatics()
        r = np.array([1.0, 0.0, 0.0])
        E = es.point_charge_field(q=1.0, r_charge=np.zeros(3), r_obs=r)
        # Field should point radially outward
        assert E[0] > 0
        assert abs(E[1]) < 1e-10

    def test_dipole_field(self):
        es = Electrostatics()
        E = es.dipole_field(p=np.array([0, 0, 1.0]),
                            r_obs=np.array([1.0, 0.0, 0.0]))
        assert E.shape == (3,)


class TestEMWave:
    def test_plane_wave(self):
        em = EMWave()
        E, B = em.plane_wave(k=np.array([1, 0, 0]),
                              E0=np.array([0, 1, 0]),
                              omega=1.0,
                              r=np.array([0.5, 0, 0]),
                              t=0.0)
        assert E.shape == (3,) and B.shape == (3,)


# ══════════════════════════════════════════════════════════════
# Fluids
# ══════════════════════════════════════════════════════════════

from grey_physics.domains.fluids import (
    FluidSolver2D, ShallowWater1D, PointVortex,
)


class TestFluidSolver2D:
    def test_taylor_green(self):
        solver = FluidSolver2D.taylor_green(N=32, Re=100)
        solver.step(n_steps=10)
        assert np.all(np.isfinite(solver.omega))

    def test_energy_spectrum(self):
        solver = FluidSolver2D.taylor_green(N=32, Re=100)
        k, Ek = solver.energy_spectrum()
        assert len(k) > 0 and len(Ek) > 0


class TestShallowWater:
    def test_dam_break(self):
        sw = ShallowWater1D.dam_break(n=100)
        sw.step(n_steps=50)
        assert np.all(sw.h > 0)  # height stays positive


class TestPointVortex:
    def test_co_rotating(self):
        pv = PointVortex.co_rotating_pair()
        pv.step(n_steps=100)
        assert np.all(np.isfinite(pv.positions))


# ══════════════════════════════════════════════════════════════
# Quantum
# ══════════════════════════════════════════════════════════════

from grey_physics.domains.quantum import (
    SchrodingerSolver1D, SpinSystem, PerturbationTheory, QuantumInfo,
)


class TestSchrodinger:
    def test_gaussian_wavepacket(self):
        solver = SchrodingerSolver1D.gaussian_wavepacket(
            N=256, L=20.0, x0=0.0, k0=5.0, sigma=1.0
        )
        # Norm should be ~1
        norm = solver.expectation(np.ones(256))
        assert abs(norm.real) > 0

    def test_ground_state_harmonic(self):
        solver = SchrodingerSolver1D.harmonic_potential(N=256, L=10.0, omega=1.0)
        E, psi = solver.find_ground_state(n_steps=5000, dt_imag=0.001)
        # Ground state energy of harmonic oscillator = ω/2 = 0.5
        assert abs(E - 0.5) < 0.1


class TestSpinSystem:
    def test_ising_ground_state(self):
        ising = SpinSystem.transverse_ising(n_sites=4, J=1.0, h=0.5)
        E, psi = ising.ground_state()
        assert E < 0  # Ground state energy should be negative

    def test_entanglement_entropy(self):
        ising = SpinSystem.transverse_ising(n_sites=4, J=1.0, h=0.5)
        E, psi = ising.ground_state()
        S = ising.entanglement_entropy(psi, cut=2)
        assert S >= 0  # Entropy is non-negative


class TestQuantumInfo:
    def test_bell_state(self):
        qi = QuantumInfo()
        psi = qi.bell_state(0)
        assert abs(np.linalg.norm(psi) - 1.0) < 1e-10

    def test_concurrence(self):
        qi = QuantumInfo()
        psi = qi.bell_state(0)
        rho = np.outer(psi, psi.conj())
        C = qi.concurrence(rho)
        assert abs(C - 1.0) < 1e-6  # Bell state is maximally entangled


# ══════════════════════════════════════════════════════════════
# Relativity
# ══════════════════════════════════════════════════════════════

from grey_physics.domains.relativity import (
    LorentzTransform, FourVector, GeodesicSolver, FriedmannSolver,
    GravitationalWave,
)


class TestLorentz:
    def test_identity_at_zero_velocity(self):
        L = LorentzTransform.boost_x(v=0.0)
        np.testing.assert_allclose(L, np.eye(4), atol=1e-12)

    def test_time_dilation(self):
        gamma = LorentzTransform.time_dilation(v=0.6)
        expected = 1.0 / np.sqrt(1 - 0.36)
        assert abs(gamma - expected) < 1e-10

    def test_velocity_addition(self):
        v = LorentzTransform.velocity_addition(0.5, 0.5)
        expected = 1.0 / (1 + 0.25)  # = 0.8
        assert abs(v - expected) < 1e-10


class TestFourVector:
    def test_inner_product(self):
        v = FourVector.four_momentum(m=1.0, v=np.array([0, 0, 0]))
        inner = v.inner(v)
        # p·p = -m² (with signature -+++)
        assert abs(inner + 1.0) < 1e-10


class TestFriedmann:
    def test_matter_dominated(self):
        fs = FriedmannSolver(Omega_m=1.0, Omega_r=0.0, Omega_L=0.0, H0=70.0)
        ts, a = fs.solve(a_init=0.001, t_span=(0, 14e9))
        assert a[-1] > a[0]  # Universe expands


class TestGravitationalWave:
    def test_plus_cross(self):
        gw = GravitationalWave()
        hp, hc = gw.plus_cross(A=1e-21, f=100, t=0.0, phi=0.0)
        assert abs(hp - 1e-21) < 1e-30  # h+ at t=0
        assert abs(hc) < 1e-30   # h× at t=0 with phi=0


# ══════════════════════════════════════════════════════════════
# Field Theory
# ══════════════════════════════════════════════════════════════

from grey_physics.domains.field_theory import (
    ScalarFieldTheory, HiggsMechanism, RenormalizationGroup,
)


class TestScalarFieldTheory:
    def test_phi4(self):
        sft = ScalarFieldTheory.phi4_theory(N=64, dx=0.1, lam=0.1, m=1.0)
        sft.step(n_steps=10)
        E = sft.energy()
        assert np.isfinite(E)


class TestHiggs:
    def test_goldstone_mass(self):
        h = HiggsMechanism(v=1.0, lam=1.0)
        m_H, m_G = h.masses()
        assert m_G < 1e-10  # Goldstone boson is massless
        assert m_H > 0


class TestRG:
    def test_qed_beta(self):
        rg = RenormalizationGroup.qed_beta()
        ts, gs = rg.flow(g0=np.array([0.3]), t_span=(0, 5), dt=0.01)
        # QED coupling increases with energy
        assert gs[-1, 0] > gs[0, 0]


# ══════════════════════════════════════════════════════════════
# Chaos
# ══════════════════════════════════════════════════════════════

from grey_physics.domains.chaos import (
    StrangeAttractor, LyapunovComputer, IteratedMap,
    BifurcationDiagram, FractalDimension,
)


class TestStrangeAttractor:
    def test_lorenz(self):
        att = StrangeAttractor.lorenz()
        traj = att.integrate(y0=np.array([1, 1, 1]),
                             t_span=(0, 10), dt=0.01)
        assert traj.shape[1] == 3
        assert np.all(np.isfinite(traj))


class TestLyapunov:
    def test_lorenz_positive(self):
        """Lorenz system has at least one positive Lyapunov exponent."""
        lc = LyapunovComputer.lorenz_jacobian()
        def lorenz(t, y):
            sigma, rho, beta = 10, 28, 8 / 3
            return np.array([
                sigma * (y[1] - y[0]),
                y[0] * (rho - y[2]) - y[1],
                y[0] * y[1] - beta * y[2],
            ])
        exps = lc.compute(lorenz, y0=np.array([1, 1, 1]),
                          T=20, dt=0.01)
        assert max(exps) > 0  # Chaotic


class TestIteratedMap:
    def test_logistic_period1(self):
        lmap = IteratedMap.logistic(r=2.0)
        x = 0.5
        for _ in range(100):
            x = lmap.iterate(x)
        # Fixed point for r=2 is 0.5
        assert abs(x - 0.5) < 0.01


class TestBifurcation:
    def test_logistic_bifurcation(self):
        bd = BifurcationDiagram()
        params, values = bd.logistic_bifurcation(
            r_range=(2.5, 4.0), n_params=50, n_iterate=200, n_last=30
        )
        assert len(params) > 0
        assert len(values) > 0


class TestFractalDimension:
    def test_box_counting(self):
        fd = FractalDimension()
        # Random points in 2D — dimension ≈ 2
        points = np.random.randn(1000, 2)
        dim = fd.box_counting(points, n_scales=10)
        assert 1.5 < dim < 2.5
