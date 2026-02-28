"""
Test suite for Grey Math Experimental Mode.

Covers:
- Mode activation and isolation
- IR extensions
- Functional analysis
- Differential geometry
- Measure theory & stochastic calculus
- Dynamical systems & chaos
- Category theory
- PDE solvers
- Symbolic extensions
- Numeric extensions
- Architecture builder
- Debugger
- Safety & isolation
- Plugin system
"""

import numpy as np
import pytest

# ─── Mode Tests ──────────────────────────────────────────────────────────────

class TestExperimentalMode:
    def test_context_manager_activation(self):
        from greymath.experimental.mode import (
            ExperimentalMode, get_experimental_context,
        )
        assert get_experimental_context() is None

        with ExperimentalMode() as ctx:
            assert ctx.active
            assert get_experimental_context() is ctx

        assert get_experimental_context() is None

    def test_stability_gating(self):
        from greymath.experimental.mode import (
            ExperimentalMode, StabilityLevel, ExperimentalAccessError,
        )
        with ExperimentalMode() as ctx:
            # ALPHA should produce a warning but not raise
            ctx.check_stability(StabilityLevel.ALPHA, "test_alpha")
            assert len(ctx.warnings) == 1

            # UNSTABLE should raise without flag
            with pytest.raises(ExperimentalAccessError):
                ctx.check_stability(StabilityLevel.UNSTABLE, "test_unstable")

    def test_unstable_allowed(self):
        from greymath.experimental.mode import (
            ExperimentalMode, StabilityLevel,
        )
        with ExperimentalMode(allow_unstable_features=True) as ctx:
            ctx.check_stability(StabilityLevel.UNSTABLE, "test")
            assert any("UNSTABLE" in w for w in ctx.warnings)

    def test_nesting(self):
        from greymath.experimental.mode import (
            ExperimentalMode, get_experimental_context,
        )
        with ExperimentalMode() as outer:
            assert get_experimental_context() is outer
            with ExperimentalMode() as inner:
                assert get_experimental_context() is inner
            assert get_experimental_context() is outer

    def test_cleanup_on_exit(self):
        from greymath.experimental.mode import ExperimentalMode
        cleaned = []
        with ExperimentalMode() as ctx:
            ctx.register_cleanup(lambda: cleaned.append(True))
        assert len(cleaned) == 1


# ─── IR Extensions Tests ────────────────────────────────────────────────────

class TestIRExtensions:
    def test_operator_space(self):
        from greymath.experimental.ir_extensions import (
            OperatorSpace, OperatorSpaceKind,
        )
        from greymath.core.types import LinearOperator, Matrix
        space = OperatorSpace(kind=OperatorSpaceKind.HILBERT_SCHMIDT)
        op = LinearOperator(
            name="T",
            matrix_rep=Matrix(data=np.array([[1.0, 2.0], [3.0, 4.0]])),
        )
        norm = space.norm(op)
        expected = np.linalg.norm(np.array([[1, 2], [3, 4]]), "fro")
        assert abs(norm - expected) < 1e-10

    def test_chart(self):
        from greymath.experimental.ir_extensions import Chart
        chart = Chart(
            name="φ", dim=2,
            coord_map=lambda p: p * 2,
            inverse_map=lambda c: c / 2,
        )
        p = np.array([1.0, 2.0])
        assert np.allclose(chart.apply(p), np.array([2.0, 4.0]))
        assert np.allclose(chart.inverse(np.array([2.0, 4.0])), p)

    def test_sde_process_gbm(self):
        from greymath.experimental.ir_extensions import SDEProcess
        gbm = SDEProcess.geometric_brownian_motion(mu=0.05, sigma=0.2)
        assert gbm.dim == 1
        drift = gbm.evaluate_drift(np.array([100.0]), 0.0)
        assert abs(drift[0] - 5.0) < 1e-10  # mu * S

    def test_pde_operator_heat(self):
        from greymath.experimental.ir_extensions import PDEOperator
        heat = PDEOperator.heat_equation(diffusivity=1.0, dim=1)
        assert heat.pde_kind.name == "HEAT"

    def test_flow(self):
        from greymath.experimental.ir_extensions import Flow
        flow = Flow(
            dim=2,
            flow_map=lambda x, t: x * np.exp(t),
        )
        result = flow.evaluate(np.array([1.0, 2.0]), 1.0)
        assert np.allclose(result, np.array([np.e, 2 * np.e]), atol=1e-6)

    def test_vector_field_lie_bracket(self):
        from greymath.experimental.ir_extensions import VectorFieldIR
        X = VectorFieldIR(
            dim=2,
            evaluate_fn=lambda p: np.array([p[1], 0.0]),
        )
        Y = VectorFieldIR(
            dim=2,
            evaluate_fn=lambda p: np.array([0.0, p[0]]),
        )
        bracket = X.lie_bracket(Y, np.array([1.0, 1.0]))
        assert bracket.shape == (2,)


# ─── Functional Analysis Tests ──────────────────────────────────────────────

class TestFunctionalAnalysis:
    def test_operator_semigroup(self):
        from greymath.experimental.functional_analysis import OperatorSemigroup
        A = np.array([[-1.0, 0.0], [0.0, -2.0]])
        sg = OperatorSemigroup.from_generator(A)
        T1 = sg.evaluate(1.0)
        assert T1.shape == (2, 2)
        assert T1[0, 0] < 1.0  # Decaying

    def test_resolvent(self):
        from greymath.experimental.functional_analysis import ResolventAnalysis
        A = np.diag([-1.0, -2.0, -3.0])
        R = ResolventAnalysis.resolvent(A, 0.0)
        expected = np.linalg.inv(-A)
        assert np.allclose(R, expected, atol=1e-10)

    def test_fixed_point_banach(self):
        from greymath.experimental.functional_analysis import FixedPointTheory
        def contraction(x):
            return 0.5 * x + 1.0
        result = FixedPointTheory.banach_iteration(contraction, x0=0.0)
        assert abs(result.fixed_point - 2.0) < 1e-6

    def test_fredholm_index(self):
        from greymath.experimental.functional_analysis import FredholmAnalysis
        A = np.eye(3)
        index = FredholmAnalysis.fredholm_index(A)
        assert index == 0  # Identity has trivial kernel and cokernel

    def test_spectral_stability(self):
        from greymath.experimental.functional_analysis import SpectralStability
        A = np.array([[-1.0, 0.0], [0.0, -2.0]])
        alpha = SpectralStability.spectral_abscissa(A)
        assert alpha < 0  # Stable


# ─── Differential Geometry Tests ────────────────────────────────────────────

class TestDifferentialGeometry:
    def test_curvature_flat_metric(self):
        from greymath.experimental.differential_geometry import CurvatureTensors
        # Euclidean metric: flat, all curvature = 0
        def flat_metric(x):
            return np.eye(2)
        ct = CurvatureTensors(metric_fn=flat_metric, dim=2)
        p = np.array([0.0, 0.0])
        ricci = ct.ricci_tensor(p)
        assert np.allclose(ricci, 0, atol=1e-4)

    def test_lie_algebra_bracket(self):
        from greymath.experimental.differential_geometry import LieAlgebra
        # so(3) bracket (cross product)
        basis = [
            np.array([[0, 0, 0], [0, 0, -1], [0, 1, 0]], dtype=float),
            np.array([[0, 0, 1], [0, 0, 0], [-1, 0, 0]], dtype=float),
            np.array([[0, -1, 0], [1, 0, 0], [0, 0, 0]], dtype=float),
        ]
        alg = LieAlgebra(name="so(3)", dim=3, basis=basis)
        bracket = alg.bracket(basis[0], basis[1])
        assert np.allclose(bracket, basis[2], atol=1e-10)

    def test_lie_group_so3(self):
        from greymath.experimental.differential_geometry import LieGroup
        so3 = LieGroup.SO(3)
        I = so3.identity()
        assert np.allclose(I, np.eye(3))

    def test_natural_gradient(self):
        from greymath.experimental.differential_geometry import NaturalGradient
        fisher = np.array([[2.0, 0.0], [0.0, 3.0]])
        grad = np.array([1.0, 1.0])
        nat_grad = NaturalGradient.natural_gradient_step(fisher, grad)
        expected = np.array([0.5, 1.0 / 3.0])
        assert np.allclose(nat_grad, expected, atol=1e-10)


# ─── Measure Theory & Stochastic Tests ──────────────────────────────────────

class TestMeasureStochastic:
    def test_sde_euler_maruyama(self):
        from greymath.experimental.measure_stochastic import SDESolver
        from greymath.experimental.ir_extensions import SDEProcess
        ou = SDEProcess.ornstein_uhlenbeck(theta=1.0, mu=0.0, sigma=0.1)
        sol = SDESolver.euler_maruyama(ou, t_span=(0, 1.0), dt=0.01, n_paths=5)
        assert sol.paths.shape[0] == 5
        assert sol.paths.shape[2] == 1

    def test_ito_integral(self):
        from greymath.experimental.measure_stochastic import StochasticCalculus
        # Simple integral of W dW
        np.random.seed(42)
        dW = np.random.randn(1000) * np.sqrt(0.001)
        W = np.cumsum(dW)
        integrand = W[:-1]
        result = StochasticCalculus.ito_integral(integrand, dW[:-1])
        # Should be approximately W_T^2/2 - T/2
        assert isinstance(result, float)

    def test_wasserstein_1d(self):
        from greymath.experimental.measure_stochastic import OptimalTransport
        mu = np.array([0.0, 1.0, 2.0])
        nu = np.array([0.5, 1.5, 2.5])
        w = OptimalTransport.wasserstein_1d(mu, nu)
        assert w > 0


# ─── Chaos Tests ─────────────────────────────────────────────────────────────

class TestChaos:
    def test_rossler_system(self):
        from greymath.experimental.chaos import rossler_system
        sys = rossler_system()
        assert sys.dim == 3
        x0 = np.array([1.0, 1.0, 1.0])
        dx = sys.vector_field(x0)
        assert dx.shape == (3,)

    def test_poincare_section(self):
        from greymath.experimental.chaos import PoincareSection, rossler_system
        sys = rossler_system()
        x0 = np.array([1.0, 1.0, 1.0])
        pts = PoincareSection.compute(sys, x0, section_dim=0, t_max=100, dt=0.01)
        # Should have some crossings
        assert isinstance(pts, np.ndarray)

    def test_lyapunov_function_candidate(self):
        from greymath.experimental.chaos import LyapunovAnalysis
        from greymath.domains.dynamical_systems import DynamicalSystem
        sys = DynamicalSystem(
            lambda x: np.array([-x[0], -2 * x[1]]),
            dim=2, name="stable_node",
        )
        V = LyapunovAnalysis.lyapunov_function_candidate(sys, np.zeros(2))
        assert V(np.array([1.0, 0.0])) > 0
        assert abs(V(np.zeros(2))) < 1e-10

    def test_correlation_dimension(self):
        from greymath.experimental.chaos import AttractorAnalyzer
        # Random data should have high dimension
        traj = np.random.randn(500, 3)
        dim = AttractorAnalyzer.correlation_dimension(traj)
        assert dim > 0

    def test_henon_map(self):
        from greymath.experimental.chaos import henon_map
        h = henon_map()
        x = np.array([0.5, 0.5])
        x_next = h.vector_field(x)
        assert x_next.shape == (2,)


# ─── Category Theory Tests ──────────────────────────────────────────────────

class TestCategoryTheory:
    def test_monoidal_category(self):
        from greymath.experimental.category import MonoidalCategory, MonoidalKind
        mc = MonoidalCategory(kind=MonoidalKind.SYMMETRIC)
        mc.category.add_object("A")
        mc.category.add_object("B")
        mc.set_unit("I")
        prod = mc.tensor_objects("A", "B")
        assert prod == "A⊗B"

    def test_operad(self):
        from greymath.experimental.category import Operad
        op = Operad(name="Test")
        op.add_operation("f", arity=2, output_type="C", input_types=["A", "B"])
        op.add_operation("g", arity=1, output_type="A", input_types=["D"])
        op.add_operation("h", arity=1, output_type="B", input_types=["E"])
        comp = op.compose("f", "g", "h")
        assert comp in op.operations

    def test_two_category(self):
        from greymath.experimental.category import TwoCategory
        tc = TwoCategory(name="Cat")
        tc.category.add_object("A")
        tc.category.add_object("B")
        f = tc.category.add_morphism("f", "A", "B")
        g = tc.category.add_morphism("g", "A", "B")
        h = tc.category.add_morphism("h", "A", "B")
        tc.add_2morphism("α", "f", "g")
        tc.add_2morphism("β", "g", "h")
        vert = tc.vertical_compose("α", "β")
        assert vert.source == "f"
        assert vert.target == "h"

    def test_string_diagram(self):
        from greymath.experimental.category import StringDiagram
        d = StringDiagram(name="test")
        d.add_node("f", inputs=["A"], outputs=["B"])
        d.add_node("g", inputs=["B"], outputs=["C"])
        expr = d.to_morphism_expression()
        assert "f" in expr and "g" in expr

    def test_yoneda(self):
        from greymath.experimental.category import YonedaEmbedding
        from greymath.domains.category_theory import Category
        cat = Category(name="C")
        cat.add_object("A")
        cat.add_object("B")
        cat.add_morphism("f", "A", "B")
        hom = YonedaEmbedding.representable_functor(cat, "B")
        assert "A" in hom
        assert any("f" in m for m in hom["A"])


# ─── PDE Tests ───────────────────────────────────────────────────────────────

class TestPDE:
    def test_heat_equation_crank_nicolson(self):
        from greymath.experimental.pde import HeatEquationSolver, BoundaryCondition
        N = 50
        u0 = np.zeros(N)
        u0[N // 2] = 1.0  # Delta-like initial condition
        sol = HeatEquationSolver.solve(
            u0, alpha=1.0, dx=0.02, dt=0.0001, n_steps=200,
        )
        assert sol.solution.shape == (201, N)
        # Heat equation should smooth the initial condition
        assert np.max(sol.final_state) < np.max(u0)

    def test_wave_equation(self):
        from greymath.experimental.pde import WaveEquationSolver
        N = 100
        x = np.linspace(0, 1, N)
        u0 = np.sin(np.pi * x)
        sol = WaveEquationSolver.solve(u0, c=1.0, dx=0.01, dt=0.005, n_steps=50)
        assert sol.solution.shape == (51, N)

    def test_spectral_heat(self):
        from greymath.experimental.pde import SpectralPDESolver
        N = 64
        x = np.linspace(0, 1, N, endpoint=False)
        u0 = np.sin(2 * np.pi * x)
        sol = SpectralPDESolver.solve_heat_spectral(u0, alpha=0.01, L=1.0, dt=0.01, n_steps=100)
        assert sol.stable
        assert np.max(np.abs(sol.final_state)) < np.max(np.abs(u0))

    def test_variational_poisson(self):
        from greymath.experimental.pde import VariationalFormulation
        x, u = VariationalFormulation.solve_poisson_1d(
            f_rhs=lambda x: np.ones_like(x),
            n_elements=50,
        )
        assert len(x) == 51
        assert u[0] == 0.0 and u[-1] == 0.0
        assert np.max(u) > 0  # Solution should bulge up

    def test_reaction_diffusion(self):
        from greymath.experimental.pde import ReactionDiffusionSolver
        N = 50
        u0 = np.random.rand(2, N) * 0.1
        sol = ReactionDiffusionSolver.solve(
            u0,
            diffusion_coeffs=np.array([0.01, 0.005]),
            reaction_fn=ReactionDiffusionSolver.fitzhugh_nagumo_reaction(),
            dx=0.02, dt=0.0001, n_steps=100,
        )
        assert sol.solution.shape == (101, 2, N)


# ─── Symbolic Extensions Tests ──────────────────────────────────────────────

class TestSymbolicExtensions:
    def test_commutator(self):
        from greymath.experimental.symbolic_ext import OperatorAlgebra
        assert OperatorAlgebra.commutator("A", "B") == "[A, B]"
        assert OperatorAlgebra.adjoint("A") == "A†"
        assert OperatorAlgebra.adjoint("A†") == "A"

    def test_bch(self):
        from greymath.experimental.symbolic_ext import OperatorAlgebra
        result = OperatorAlgebra.bch_first_order("X", "Y")
        assert "X" in result and "Y" in result

    def test_tensor_indices(self):
        from greymath.experimental.symbolic_ext import (
            SymbolicTensor, TensorIndex, IndexKind,
        )
        T = SymbolicTensor(
            name="R",
            indices=[
                TensorIndex("i", IndexKind.UPPER),
                TensorIndex("j", IndexKind.LOWER),
                TensorIndex("k", IndexKind.LOWER),
                TensorIndex("l", IndexKind.LOWER),
            ],
        )
        free = T.free_indices
        assert len(free) == 4
        assert len(T.contracted_indices) == 0

    def test_ito_formula(self):
        from greymath.experimental.symbolic_ext import SymbolicStochastic
        result = SymbolicStochastic.ito_formula("f", "X")
        assert "dW" in result

    def test_categorical_rewrites(self):
        from greymath.experimental.symbolic_ext import CategoricalRewrites
        result = CategoricalRewrites.apply_all("id ∘ f")
        assert result == "f"

    def test_fokker_planck(self):
        from greymath.experimental.symbolic_ext import SymbolicStochastic
        fp = SymbolicStochastic.fokker_planck()
        assert "∂ρ/∂t" in fp


# ─── Numeric Extensions Tests ───────────────────────────────────────────────

class TestNumericExtensions:
    def test_riemannian_gradient_descent(self):
        from greymath.experimental.numeric_ext import RiemannianOptimizer
        # Minimize ||x||^2 on the sphere
        def objective(x):
            return x[0]  # Minimize first coordinate
        def grad(x):
            g = np.zeros_like(x)
            g[0] = 1.0
            return g

        x0 = np.array([0.0, 0.0, 1.0])
        x_opt, history = RiemannianOptimizer.gradient_descent(
            objective, grad,
            retraction=RiemannianOptimizer.sphere_retraction,
            x0=x0, lr=0.1, max_iter=100,
        )
        assert np.abs(np.linalg.norm(x_opt) - 1.0) < 1e-6  # On sphere

    def test_power_iteration(self):
        from greymath.experimental.numeric_ext import OperatorNormEstimator
        A = np.array([[3.0, 1.0], [1.0, 2.0]])
        norm = OperatorNormEstimator.power_iteration(A)
        true_norm = np.linalg.norm(A, 2)
        assert abs(norm - true_norm) < 0.1

    def test_interval_arithmetic(self):
        from greymath.experimental.numeric_ext import Interval
        a = Interval(1.0, 2.0)
        b = Interval(3.0, 4.0)
        c = a + b
        assert c.lo == 4.0 and c.hi == 6.0
        d = a * b
        assert d.lo == 3.0 and d.hi == 8.0

    def test_stability_diagnostics(self):
        from greymath.experimental.numeric_ext import StabilityDiagnostics
        A = np.eye(3) * 2.0
        report = StabilityDiagnostics.analyze(A)
        assert report.is_well_conditioned
        assert report.condition_number < 2.0

    def test_mixed_precision_refinement(self):
        from greymath.experimental.numeric_ext import MixedPrecision
        A = np.array([[1.0, 1e-10], [1e-10, 1.0]])
        b = np.array([1.0, 1.0])
        x, residual = MixedPrecision.iterative_refinement(A, b)
        assert residual < 1e-12

    def test_pseudospectrum(self):
        from greymath.experimental.numeric_ext import PseudospectralAnalysis
        A = np.array([[0, 1], [-1, 0]])  # Rotation
        re, im, smin = PseudospectralAnalysis.pseudospectrum(
            A, real_range=(-2, 2), imag_range=(-2, 2), n_grid=20,
        )
        assert smin.shape == (20, 20)


# ─── Architecture Tests ─────────────────────────────────────────────────────

class TestArchitecture:
    def test_architecture_composition(self):
        from greymath.experimental.architecture import (
            Architecture, ArchitectureLayer, ArchitectureKind,
        )
        layer1 = ArchitectureLayer(
            name="L1", input_shape=(10,), output_shape=(5,),
            forward_fn=lambda x: x[:5],
        )
        layer2 = ArchitectureLayer(
            name="L2", input_shape=(5,), output_shape=(3,),
            forward_fn=lambda x: x[:3],
        )
        arch = Architecture(name="test")
        arch.add_layer(layer1)
        arch.add_layer(layer2)
        assert arch.input_shape == (10,)
        assert arch.output_shape == (3,)
        errors = arch.validate()
        assert len(errors) == 0

    def test_semigroup_layer(self):
        from greymath.experimental.architecture import OperatorArchitectureBuilder
        A = np.array([[-1.0, 0.0], [0.0, -2.0]])
        layer = OperatorArchitectureBuilder.semigroup_layer(A, t=1.0)
        x = np.array([1.0, 1.0])
        y = layer.forward(x)
        assert y.shape == (2,)
        assert y[0] < 1.0  # Decaying

    def test_normalizing_flow_layer(self):
        from greymath.experimental.architecture import MeasureArchitectureBuilder
        mask = np.array([True, True, False, False])
        layer = MeasureArchitectureBuilder.affine_coupling_layer(
            mask=mask,
            scale_fn=lambda x: np.zeros(2),
            translate_fn=lambda x: np.ones(2),
        )
        x = np.array([1.0, 2.0, 3.0, 4.0])
        y = layer.forward(x)
        x_rec = layer.inverse(y)
        assert np.allclose(x, x_rec, atol=1e-10)


# ─── Debugger Tests ──────────────────────────────────────────────────────────

class TestDebugger:
    def test_spectral_debugger(self):
        from greymath.experimental.debugger import SpectralDebugger
        A = np.diag([1.0, 2.0, 3.0])
        report = SpectralDebugger.full_report(A)
        assert report.is_healthy

    def test_dynamics_debugger(self):
        from greymath.experimental.debugger import DynamicsDebugger
        J = np.array([[-1.0, 0.0], [0.0, -2.0]])
        result = DynamicsDebugger.stability_at_fixed_point(J)
        assert result.status == "ok"
        assert "stable" in result.value

    def test_pde_debugger_conservation(self):
        from greymath.experimental.debugger import PDEDebugger
        solution = np.ones((10, 50))  # Constant solution
        result = PDEDebugger.conservation_check(solution, dx=0.02)
        assert result.status == "ok"

    def test_convergence_monitor(self):
        from greymath.experimental.debugger import ConvergenceMonitor
        monitor = ConvergenceMonitor(tol=1e-6)
        for i in range(100):
            val = 1.0 / (i + 1)
            if monitor.record(val):
                break
        assert monitor.converged

    def test_math_inspector(self):
        from greymath.experimental.debugger import MathInspector
        A = np.eye(3)
        report = MathInspector.inspect_matrix(A)
        assert report.is_healthy


# ─── Safety Tests ────────────────────────────────────────────────────────────

class TestSafety:
    def test_anomaly_detection_nan(self):
        from greymath.experimental.safety import AnomalyDetector, AnomalyKind
        arr = np.array([1.0, float("nan"), 3.0])
        anomalies = AnomalyDetector.check_array(arr)
        assert any(a.kind == AnomalyKind.NAN for a in anomalies)

    def test_anomaly_detection_ill_conditioned(self):
        from greymath.experimental.safety import AnomalyDetector, AnomalyKind
        A = np.array([[1.0, 1.0], [1.0, 1.0 + 1e-15]])
        anomalies = AnomalyDetector.check_matrix(A)
        assert any(a.kind == AnomalyKind.ILL_CONDITIONED for a in anomalies)

    def test_safe_solve(self):
        from greymath.experimental.safety import SafeCompute
        A = np.array([[2.0, 1.0], [1.0, 2.0]])
        b = np.array([3.0, 3.0])
        x, anomalies = SafeCompute.safe_solve(A, b)
        assert np.allclose(A @ x, b, atol=1e-10)

    def test_deterministic_mode(self):
        from greymath.experimental.safety import DeterministicMode
        def stochastic_fn():
            return np.random.randn(5)
        is_det, results = DeterministicMode.verify_deterministic(
            stochastic_fn, n_runs=3, seed=42,
        )
        assert is_det
        assert np.allclose(results[0], results[1])

    def test_checkpoint_rollback(self):
        from greymath.experimental.safety import Checkpoint
        ckpt = Checkpoint()
        state = np.array([1.0, 2.0, 3.0])
        ckpt.save("x", state)
        state[0] = 99.0
        restored = ckpt.restore("x")
        assert restored[0] == 1.0  # Original value

    def test_type_validator_metric(self):
        from greymath.experimental.safety import TypeValidator
        good_metric = np.eye(3)
        assert len(TypeValidator.validate_metric(good_metric)) == 0
        bad_metric = np.array([[1, 0], [1, 1]])  # Not symmetric
        assert len(TypeValidator.validate_metric(bad_metric)) > 0

    def test_execution_audit(self):
        from greymath.experimental.safety import ExecutionAudit, StabilityLevel
        audit = ExecutionAudit()
        audit.begin("test_op", StabilityLevel.ALPHA)
        audit.end(result=42)
        assert len(audit.entries) == 1
        summary = audit.summary()
        assert summary["total_operations"] == 1


# ─── Plugin System Tests ────────────────────────────────────────────────────

class TestPluginSystem:
    def test_plugin_registration(self):
        from greymath.experimental.plugin_ext import (
            ExperimentalPlugin,
            ExperimentalPluginDescriptor,
            PluginCapability,
            PluginRegistry,
        )
        from greymath.experimental.mode import StabilityLevel

        class MyPlugin(ExperimentalPlugin):
            def describe(self):
                return ExperimentalPluginDescriptor(
                    name="TestPlugin",
                    version="1.0.0",
                    stability_level=StabilityLevel.BETA,
                    capabilities=[PluginCapability.MANIFOLD],
                )
            def get_exports(self):
                return {"my_fn": lambda x: x * 2}

        registry = PluginRegistry()
        plugin = MyPlugin()
        registry.register(plugin)
        assert registry.n_registered == 1

        exports = registry.get_exports("TestPlugin")
        assert "my_fn" in exports

    def test_plugin_capability_query(self):
        from greymath.experimental.plugin_ext import (
            ExperimentalPlugin,
            ExperimentalPluginDescriptor,
            PluginCapability,
            PluginRegistry,
        )
        from greymath.experimental.mode import StabilityLevel

        class ManifoldPlugin(ExperimentalPlugin):
            def describe(self):
                return ExperimentalPluginDescriptor(
                    name="ManifoldPlugin",
                    capabilities=[PluginCapability.MANIFOLD],
                )

        class SolverPlugin(ExperimentalPlugin):
            def describe(self):
                return ExperimentalPluginDescriptor(
                    name="SolverPlugin",
                    capabilities=[PluginCapability.PDE_SOLVER],
                )

        registry = PluginRegistry()
        registry.register(ManifoldPlugin())
        registry.register(SolverPlugin())

        manifold_plugins = registry.query_by_capability(PluginCapability.MANIFOLD)
        assert len(manifold_plugins) == 1
        assert manifold_plugins[0].descriptor.name == "ManifoldPlugin"

    def test_plugin_dependency(self):
        from greymath.experimental.plugin_ext import (
            ExperimentalPlugin,
            ExperimentalPluginDescriptor,
            PluginRegistry,
        )

        class BasePlugin(ExperimentalPlugin):
            def describe(self):
                return ExperimentalPluginDescriptor(name="Base")

        class DependentPlugin(ExperimentalPlugin):
            def describe(self):
                return ExperimentalPluginDescriptor(
                    name="Dependent", dependencies=["Base"],
                )

        registry = PluginRegistry()
        registry.register(BasePlugin())
        registry.register(DependentPlugin())
        assert registry.n_registered == 2

    def test_plugin_activation_lifecycle(self):
        from greymath.experimental.plugin_ext import (
            ExperimentalPlugin,
            ExperimentalPluginDescriptor,
            PluginRegistry,
        )

        events: list[str] = []

        class LifecyclePlugin(ExperimentalPlugin):
            def describe(self):
                return ExperimentalPluginDescriptor(name="Lifecycle")
            def on_activate(self):
                events.append("activated")
            def on_deactivate(self):
                events.append("deactivated")

        registry = PluginRegistry()
        registry.register(LifecyclePlugin())
        registry.activate("Lifecycle")
        registry.deactivate("Lifecycle")
        assert events == ["activated", "deactivated"]

    def test_global_registry(self):
        from greymath.experimental.plugin_ext import (
            get_plugin_registry, reset_plugin_registry,
        )
        reset_plugin_registry()
        reg = get_plugin_registry()
        assert reg.n_registered == 0
        reset_plugin_registry()
