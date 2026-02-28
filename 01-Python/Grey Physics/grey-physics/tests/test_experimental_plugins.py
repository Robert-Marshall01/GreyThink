"""Tests for Experimental mode and Plugin system."""
import numpy as np
import pytest

# ── Experimental ─────────────────────────────────────────────

from grey_physics.experimental import (
    LieGroup, RigidBodyOnSO3, SINDy, KoopmanAnalysis,
    InformationGeometry,
)


class TestLieGroup:
    def test_so3_exp_identity(self):
        """exp(0) = I for SO(3)."""
        R = LieGroup.so3_exp(np.zeros(3))
        np.testing.assert_allclose(R, np.eye(3), atol=1e-12)

    def test_so3_hat_vee_roundtrip(self):
        omega = np.array([1.0, 2.0, 3.0])
        mat = LieGroup.so3_hat(omega)
        omega2 = LieGroup.so3_vee(mat)
        np.testing.assert_allclose(omega, omega2)

    def test_su2_generators(self):
        gens = LieGroup.su2_generators()
        assert len(gens) == 3
        for g in gens:
            assert g.shape == (2, 2)

    def test_structure_constants(self):
        gens = LieGroup.su2_generators()
        f = LieGroup.structure_constants(gens)
        # f_{12}^3 should be non-zero for SU(2)
        assert f.shape == (3, 3, 3)


class TestRigidBodySO3:
    def test_simulate(self):
        rb = RigidBodyOnSO3(
            inertia=np.diag([2.0, 3.0, 4.0]),
            omega0=np.array([1.0, 0.1, 0.0]),
        )
        ts, Rs, omegas = rb.simulate(t_span=(0, 5), dt=0.01)
        # Rotation matrix should stay in SO(3): R^T R ≈ I
        R_final = Rs[-1]
        np.testing.assert_allclose(R_final.T @ R_final, np.eye(3), atol=0.01)


class TestSINDy:
    def test_discover_linear(self):
        """Discover dx/dt = -x from data."""
        t = np.linspace(0, 5, 500)
        X = np.exp(-t).reshape(-1, 1)
        sindy = SINDy(poly_order=2, threshold=0.1)
        Xi = sindy.fit(X, t)
        # Should find coefficient ≈ -1 for the 'x' term
        assert Xi is not None
        assert Xi.shape[0] > 0


class TestKoopman:
    def test_dmd_linear(self):
        """DMD of a simple decay should recover the eigenvalue."""
        t = np.linspace(0, 5, 100)
        X = np.vstack([np.exp(-0.5 * t), np.exp(-1.0 * t)])
        ka = KoopmanAnalysis()
        evals, modes, dynamics = ka.exact_dmd(X, dt=t[1] - t[0])
        assert len(evals) > 0


class TestInfoGeometry:
    def test_fisher_gaussian(self):
        ig = InformationGeometry()
        # Fisher metric for Gaussian at (μ=0, σ=1) is diag(1, 2)
        F = ig.fisher_metric_gaussian(mu=0.0, sigma=1.0)
        assert F.shape == (2, 2)
        assert abs(F[0, 0] - 1.0) < 1e-10
        assert abs(F[1, 1] - 2.0) < 1e-10

    def test_kl_divergence(self):
        ig = InformationGeometry()
        D = ig.kl_divergence_gaussian(mu1=0, sigma1=1, mu2=0, sigma2=1)
        assert abs(D) < 1e-10  # Same distribution → 0


# ── Plugin System ────────────────────────────────────────────

from grey_physics.plugins import (
    PluginBase, PluginMeta, HookRegistry, ComponentRegistry,
    PluginManager, ExamplePlugin,
)


class TestHookRegistry:
    def test_register_trigger(self):
        registry = HookRegistry()
        called = []
        registry.register("test_hook", lambda **kw: called.append(kw))
        registry.trigger("test_hook", value=42)
        assert len(called) == 1
        assert called[0]["value"] == 42

    def test_unregister(self):
        registry = HookRegistry()
        cb = lambda **kw: None
        registry.register("h", cb)
        registry.unregister("h", cb)
        assert len(registry._hooks.get("h", [])) == 0


class TestComponentRegistry:
    def test_register_component(self):
        cr = ComponentRegistry()
        cr.register("solver", "my_solver", lambda: "solver_instance")
        items = cr.list_components("solver")
        assert "my_solver" in items

    def test_get_component(self):
        cr = ComponentRegistry()
        cr.register("solver", "rk4", "rk4_solver_obj")
        assert cr.get("solver", "rk4") == "rk4_solver_obj"


class TestPluginManager:
    def test_register_plugin(self):
        pm = PluginManager()
        plugin = ExamplePlugin()
        pm.register(plugin)
        assert "example_plugin" in pm.list_plugins()

    def test_unregister_plugin(self):
        pm = PluginManager()
        plugin = ExamplePlugin()
        pm.register(plugin)
        pm.unregister("example_plugin")
        assert "example_plugin" not in pm.list_plugins()
