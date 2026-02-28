"""Tests for the Math core modules."""
import numpy as np
import pytest

# ── Differential Geometry ────────────────────────────────────

from grey_physics.core.math.differential_geometry import (
    christoffel_symbols_numeric,
    riemann_tensor_numeric,
    ricci_tensor_numeric,
    ricci_scalar_numeric,
    solve_geodesic,
    DifferentialForm,
)


class TestDifferentialGeometry:
    def test_flat_christoffels(self):
        """Euclidean metric → all Christoffel symbols vanish."""
        def flat_metric(x):
            return np.eye(len(x))
        x0 = np.array([1.0, 2.0, 3.0])
        G = christoffel_symbols_numeric(flat_metric, x0, 3)
        assert G.shape == (3, 3, 3)
        assert np.allclose(G, 0.0, atol=1e-6)

    def test_spherical_christoffels(self):
        """Diagonal spherical metric g = diag(1, r², r²sin²θ)."""
        def spherical_metric(x):
            r, theta, _phi = x
            g = np.zeros((3, 3))
            g[0, 0] = 1.0
            g[1, 1] = r ** 2
            g[2, 2] = (r * np.sin(theta)) ** 2
            return g

        x0 = np.array([2.0, np.pi / 4, 0.0])
        G = christoffel_symbols_numeric(spherical_metric, x0, 3)
        # Γ^r_{θθ} = -r  →  should be ≈ -2
        assert abs(G[0, 1, 1] + 2.0) < 0.05

    def test_flat_riemann_vanishes(self):
        """Flat metric ⇒ Riemann = 0."""
        def flat(x):
            return np.eye(len(x))
        R = riemann_tensor_numeric(flat, np.array([0.0, 0.0]), 2)
        assert np.allclose(R, 0.0, atol=1e-4)

    def test_geodesic_free_particle(self):
        """Geodesic on flat space = straight line."""
        def flat(x):
            return np.eye(2)
        x0 = np.array([0.0, 0.0])
        v0 = np.array([1.0, 0.5])
        path = solve_geodesic(flat, x0, v0, dim=2, t_span=(0, 2), n_steps=100)
        # Endpoint ~ (2.0, 1.0)
        np.testing.assert_allclose(path[-1, :2], [2.0, 1.0], atol=0.1)

    def test_differential_form_wedge(self):
        omega = DifferentialForm(dim=3, degree=1)
        omega.add_component((0,), lambda x: 1.0)   # dx
        eta = DifferentialForm(dim=3, degree=1)
        eta.add_component((1,), lambda x: 1.0)      # dy
        wedge = omega.wedge(eta)
        assert wedge.degree == 2
        val = wedge.evaluate(np.zeros(3))
        # dx ∧ dy at origin has component (0,1) = 1
        assert abs(val[(0, 1)] - 1.0) < 1e-10


# ── Functional Analysis ──────────────────────────────────────

from grey_physics.core.math.functional_analysis import (
    FunctionSpace, LinearOperator,
)


class TestFunctionalAnalysis:
    def test_l2_norm(self):
        fs = FunctionSpace(domain=(0, 1), n_basis=50)
        # Constant function 1 has L2 norm = 1
        f = np.ones(50)
        n = fs.l2_norm(f)
        assert n > 0

    def test_laplacian_eigenvalues(self):
        """1D Laplacian on [0,π] has eigenvalues n² for Dirichlet BC."""
        op = LinearOperator.laplacian(n=100, dx=np.pi / 101)
        evals = op.eigenvalues(k=3)
        # First eigenvalue ≈ 1
        assert abs(evals[0] - 1.0) < 0.1

    def test_identity(self):
        I = LinearOperator.identity(n=10)
        x = np.random.randn(10)
        np.testing.assert_allclose(I.apply(x), x)


# ── Variational Calculus ─────────────────────────────────────

from grey_physics.core.math.variational_calculus import (
    Functional, legendre_transform,
)


class TestVariationalCalculus:
    def test_functional_evaluate(self):
        """∫₀¹ (y')² dx with y(x) = x  ⇒  integral = 1."""
        def integrand(x, y, yp):
            return yp ** 2

        F = Functional(integrand=integrand, domain=(0, 1))
        n = 200
        x = np.linspace(0, 1, n)
        y = x.copy()
        val = F.evaluate(x, y)
        assert abs(val - 1.0) < 0.02

    def test_legendre_transform(self):
        """Legendre of L = ½mv² → H = p²/(2m)."""
        v = np.linspace(-2, 2, 100)

        def L(vi):
            return 0.5 * vi ** 2

        p, H_vals = legendre_transform(L, v)
        # H should ≈ p²/2
        expected = p ** 2 / 2
        np.testing.assert_allclose(H_vals, expected, atol=0.1)


# ── PDE/ODE Theory ──────────────────────────────────────────

from grey_physics.core.math.pde_ode_theory import (
    classify_second_order_pde, MethodOfLines, EnergyMonitor,
    StochasticProcess,
)


class TestPDEODE:
    def test_classify_elliptic(self):
        """Laplace equation: u_xx + u_yy = 0 ⇒ elliptic (A=1, B=0, C=1)."""
        cls = classify_second_order_pde(A=1, B=0, C=1)
        assert cls == "elliptic"

    def test_classify_hyperbolic(self):
        """Wave equation: u_tt - c²u_xx = 0 ⇒ A=1, B=0, C=-c²."""
        cls = classify_second_order_pde(A=1, B=0, C=-1)
        assert cls == "hyperbolic"

    def test_classify_parabolic(self):
        """Heat: A=1, B=0, C=0."""
        cls = classify_second_order_pde(A=1, B=0, C=0)
        assert cls == "parabolic"

    def test_energy_monitor(self):
        monitor = EnergyMonitor()
        monitor.record(0.0, 1.0)
        monitor.record(1.0, 1.001)
        drift = monitor.relative_drift()
        assert abs(drift) < 0.01

    def test_brownian_motion(self):
        sp = StochasticProcess.brownian_motion()
        ts, xs = sp.sample_path(T=1.0, n_steps=1000)
        assert len(ts) == len(xs) == 1001
        # Mean displacement ~ 0, variance ~ T=1
        assert abs(xs[-1]) < 10  # very loose (stochastic)


# ── Dynamical Systems ───────────────────────────────────────

from grey_physics.core.math.dynamical_systems import (
    linear_stability, lyapunov_spectrum,
)


class TestDynamicalSystems:
    def test_linear_stability_saddle(self):
        """dx/dt = x, dy/dt = -y → eigenvalues ±1 (saddle)."""
        J = np.array([[1.0, 0.0], [0.0, -1.0]])
        evals, classification = linear_stability(J)
        assert classification == "saddle"

    def test_linear_stability_stable_node(self):
        J = np.array([[-1.0, 0.0], [0.0, -2.0]])
        evals, classification = linear_stability(J)
        assert "stable" in classification
