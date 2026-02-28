"""Tests for Grey Math numeric engine."""

import pytest
import numpy as np

from greymath.numeric.linalg import LinAlg
from greymath.numeric.spectral import SpectralSolver
from greymath.numeric.ode import ODESolver, ODEMethod
from greymath.numeric.optimize import Optimizer, OptMethod
from greymath.numeric.precision import ArbitraryPrecision, Interval, IntervalArithmetic
from greymath.numeric.stability import StabilityDiagnostics
from greymath.numeric.stochastic import StochasticSampler, SamplingMethod


class TestLinearAlgebra:
    """Tests for linear algebra operations."""

    def test_matmul(self):
        la = LinAlg()
        A = np.array([[1, 2], [3, 4]], dtype=float)
        B = np.array([[5, 6], [7, 8]], dtype=float)
        C = la.matmul(A, B)
        expected = np.array([[19, 22], [43, 50]], dtype=float)
        np.testing.assert_array_almost_equal(C, expected)

    def test_matvec(self):
        la = LinAlg()
        A = np.array([[1, 2], [3, 4]], dtype=float)
        x = np.array([1, 1], dtype=float)
        y = la.matvec(A, x)
        np.testing.assert_array_almost_equal(y, [3, 7])

    def test_solve(self):
        la = LinAlg()
        A = np.array([[2, 1], [1, 3]], dtype=float)
        b = np.array([5, 7], dtype=float)
        x = la.solve(A, b)
        np.testing.assert_array_almost_equal(A @ x, b)

    def test_lu_decomposition(self):
        la = LinAlg()
        A = np.array([[2, 1], [1, 3]], dtype=float)
        P, L, U = la.lu(A)
        np.testing.assert_array_almost_equal(P @ A, L @ U, decimal=10)

    def test_qr_decomposition(self):
        la = LinAlg()
        A = np.array([[1, 2], [3, 4], [5, 6]], dtype=float)
        Q, R = la.qr(A)
        np.testing.assert_array_almost_equal(A, Q @ R, decimal=10)
        # Q should have orthonormal columns
        np.testing.assert_array_almost_equal(Q.T @ Q, np.eye(2), decimal=10)

    def test_svd(self):
        la = LinAlg()
        A = np.array([[1, 2], [3, 4]], dtype=float)
        U, S, Vt = la.svd(A)
        reconstructed = U @ np.diag(S) @ Vt
        np.testing.assert_array_almost_equal(A, reconstructed, decimal=10)

    def test_cholesky(self):
        la = LinAlg()
        A = np.array([[4, 2], [2, 3]], dtype=float)
        L = la.cholesky(A)
        np.testing.assert_array_almost_equal(A, L @ L.T, decimal=10)

    def test_eig(self):
        la = LinAlg()
        A = np.array([[2, 1], [1, 2]], dtype=float)
        vals, vecs = la.eig(A)
        for i in range(len(vals)):
            np.testing.assert_array_almost_equal(
                A @ vecs[:, i], vals[i] * vecs[:, i], decimal=10
            )

    def test_condition_number(self):
        la = LinAlg()
        I = np.eye(3)
        cond = la.condition_number(I)
        assert abs(cond - 1.0) < 1e-10

    def test_rank(self):
        la = LinAlg()
        A = np.array([[1, 2], [2, 4]], dtype=float)  # Rank 1
        assert la.rank(A) == 1


class TestSpectral:
    """Tests for spectral solver."""

    def test_full_spectrum(self):
        solver = SpectralSolver()
        A = np.array([[2, 1], [1, 2]], dtype=float)
        vals, vecs = solver.full_spectrum(A)
        assert len(vals) == 2
        assert sorted(np.real(vals)) == pytest.approx([1.0, 3.0])

    def test_spectral_radius(self):
        solver = SpectralSolver()
        A = np.array([[2, 1], [1, 2]], dtype=float)
        radius = solver.spectral_radius(A)
        assert abs(radius - 3.0) < 1e-10

    def test_spectral_gap(self):
        solver = SpectralSolver()
        A = np.array([[3, 0], [0, 1]], dtype=float)
        gap = solver.spectral_gap(A)
        assert abs(gap - 2.0) < 1e-10


class TestODESolver:
    """Tests for ODE solvers."""

    def test_euler_simple(self):
        """dy/dt = -y, y(0) = 1 => y(t) = e^(-t)"""
        solver = ODESolver()

        def f(t, y):
            return -y

        sol = solver.solve(f, np.array([1.0]), (0, 1), dt=0.001, method=ODEMethod.EULER)
        # Euler isn't very accurate but should be in ballpark
        assert abs(sol.y[-1, 0] - np.exp(-1)) < 0.01

    def test_rk4_exponential_decay(self):
        """dy/dt = -y, y(0) = 1 => y(t) = e^(-t)"""
        solver = ODESolver()

        def f(t, y):
            return -y

        sol = solver.solve(f, np.array([1.0]), (0, 1), dt=0.01, method=ODEMethod.RK4)
        assert abs(sol.y[-1, 0] - np.exp(-1)) < 1e-6

    def test_rk4_harmonic_oscillator(self):
        """x'' + x = 0 => periodic solution"""
        solver = ODESolver()

        def harmonic(t, y):
            return np.array([y[1], -y[0]])

        y0 = np.array([1.0, 0.0])
        sol = solver.solve(harmonic, y0, (0.0, 2 * np.pi), dt=0.01, method=ODEMethod.RK4)
        # Should return to initial conditions after one period
        np.testing.assert_array_almost_equal(sol.y[-1], y0, decimal=3)


class TestOptimizer:
    """Tests for optimization."""

    def test_gradient_descent_quadratic(self):
        """Minimize f(x) = x^2, starting from x=5"""
        optimizer = Optimizer()

        def f(x):
            return x[0] ** 2

        def grad(x):
            return np.array([2 * x[0]])

        result = optimizer.optimize(f, np.array([5.0]), grad_fn=grad,
                                     method=OptMethod.GRADIENT_DESCENT)
        assert result.success
        assert abs(result.x[0]) < 0.1

    def test_nelder_mead_rosenbrock(self):
        """Minimize Rosenbrock function"""
        optimizer = Optimizer()

        def rosenbrock(x):
            return (1 - x[0]) ** 2 + 100 * (x[1] - x[0] ** 2) ** 2

        result = optimizer.optimize(rosenbrock, np.array([0.0, 0.0]),
                                     method=OptMethod.NELDER_MEAD)
        assert result.success
        np.testing.assert_array_almost_equal(result.x, [1.0, 1.0], decimal=2)


class TestIntervalArithmetic:
    """Tests for interval arithmetic."""

    def test_interval_creation(self):
        i = Interval(1.0, 2.0)
        assert i.lo == 1.0
        assert i.hi == 2.0
        assert i.midpoint == 1.5
        assert i.width == 1.0

    def test_interval_addition(self):
        a = Interval(1.0, 2.0)
        b = Interval(3.0, 4.0)
        c = a + b
        assert c.lo == 4.0
        assert c.hi == 6.0

    def test_interval_multiplication(self):
        a = Interval(1.0, 2.0)
        b = Interval(3.0, 4.0)
        c = a * b
        assert c.lo == 3.0
        assert c.hi == 8.0

    def test_interval_contains(self):
        i = Interval(1.0, 3.0)
        assert i.contains(2.0)
        assert not i.contains(4.0)

    def test_interval_neg_multiplication(self):
        a = Interval(-2.0, -1.0)
        b = Interval(3.0, 4.0)
        c = a * b
        assert c.lo == -8.0
        assert c.hi == -3.0


class TestStability:
    """Tests for stability diagnostics."""

    def test_well_conditioned_matrix(self):
        diag = StabilityDiagnostics()
        A = np.eye(3)
        report = diag.analyze_matrix(A)
        assert report.well_conditioned
        assert report.condition_number == pytest.approx(1.0)

    def test_ill_conditioned_matrix(self):
        diag = StabilityDiagnostics()
        A = np.array([[1, 1], [1, 1.0001]])
        report = diag.analyze_matrix(A)
        assert report.condition_number > 1000


class TestArbitraryPrecision:
    """Tests for arbitrary precision arithmetic."""

    def test_high_precision_pi(self):
        ap = ArbitraryPrecision(precision=50)
        pi = ap.pi()
        assert str(pi).startswith("3.14159265358979323846")

    def test_high_precision_e(self):
        ap = ArbitraryPrecision(precision=50)
        e = ap.e()
        assert str(e).startswith("2.71828182845904523536")

    def test_arbitrary_precision_sqrt(self):
        ap = ArbitraryPrecision(precision=50)
        result = ap.sqrt(ap.mpf(2))
        assert str(result).startswith("1.41421356237")
