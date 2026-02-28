"""Tests for the Numeric physics engine."""
import numpy as np
import pytest

from grey_physics.core.numeric import NumericEngine, ODESolver, FDMSolver, SpectralSolver


class TestODESolver:
    def test_rk4_harmonic_oscillator(self):
        """ẍ + x = 0 as first-order system."""
        def rhs(t, y):
            return np.array([y[1], -y[0]])

        solver = ODESolver(method="rk4")
        y0 = np.array([1.0, 0.0])
        ts, ys = solver.solve(rhs, y0, t_span=(0, 2 * np.pi), dt=0.01)
        # After one period, should return to (1.0, 0.0)
        np.testing.assert_allclose(ys[-1], [1.0, 0.0], atol=0.02)

    def test_rk45_exponential_decay(self):
        """dy/dt = -y, y(0) = 1 → y(t) = e^{-t}."""
        def rhs(t, y):
            return -y

        solver = ODESolver(method="rk45")
        ts, ys = solver.solve(rhs, np.array([1.0]), t_span=(0, 3), dt=0.01)
        expected = np.exp(-ts[-1])
        assert abs(ys[-1][0] - expected) < 0.01

    def test_verlet_energy_conservation(self):
        """Verify Verlet conserves energy for Hamiltonian systems."""
        solver = ODESolver(method="verlet")
        # Simple harmonic: H = p²/2 + q²/2
        def rhs(t, y):
            q, p = y
            return np.array([p, -q])

        y0 = np.array([1.0, 0.0])
        ts, ys = solver.solve(rhs, y0, t_span=(0, 20), dt=0.01)
        E0 = 0.5 * y0[0] ** 2 + 0.5 * y0[1] ** 2
        Ef = 0.5 * ys[-1, 0] ** 2 + 0.5 * ys[-1, 1] ** 2
        assert abs(Ef - E0) / E0 < 0.01


class TestFDMSolver:
    def test_heat_1d_explicit(self):
        """Heat equation: smooth initial → should evolve without blowing up."""
        fdm = FDMSolver()
        n = 50
        u0 = np.sin(np.linspace(0, np.pi, n))
        result = fdm.heat_1d(u0, dx=1.0 / n, dt=0.0001, n_steps=100,
                             method="explicit")
        assert result.shape == (n,)
        assert np.all(np.isfinite(result))

    def test_poisson_2d(self):
        """Solve Poisson on unit square with uniform source, Dirichlet BC=0."""
        fdm = FDMSolver()
        n = 20
        f = np.ones((n, n))
        u = fdm.poisson_2d(f, dx=1.0 / n, method="jacobi", tol=1e-4,
                           max_iter=5000)
        assert u.shape == (n, n)
        # Solution should be non-negative (source > 0 with zero BC)
        assert u.min() >= -1e-6


class TestSpectralSolver:
    def test_spectral_derivative(self):
        """d/dx sin(x) = cos(x) via FFT."""
        spec = SpectralSolver()
        n = 64
        x = np.linspace(0, 2 * np.pi, n, endpoint=False)
        f = np.sin(x)
        df = spec.spectral_derivative(f, dx=x[1] - x[0])
        np.testing.assert_allclose(df, np.cos(x), atol=1e-10)


class TestNumericEngine:
    def test_facade_ode(self):
        engine = NumericEngine()
        def rhs(t, y):
            return -y
        ts, ys = engine.solve_ode(rhs, np.array([1.0]),
                                   t_span=(0, 1), dt=0.01, method="rk4")
        assert abs(ys[-1][0] - np.exp(-1)) < 0.01
