"""Tests for Core IR spacetime module."""
import numpy as np
import pytest

from grey_physics.core.ir.spacetime import Metric, Manifold, Spacetime


class TestMetric:
    def test_minkowski(self):
        g = Metric.minkowski(dim=4)
        m = g.components(np.zeros(4))
        assert m.shape == (4, 4)
        assert m[0, 0] == -1
        assert m[1, 1] == 1

    def test_euclidean(self):
        g = Metric.euclidean(dim=3)
        m = g.components(np.zeros(3))
        np.testing.assert_allclose(m, np.eye(3))

    def test_schwarzschild(self):
        g = Metric.schwarzschild(M=1.0)
        r = 10.0
        m = g.components(np.array([0.0, r, np.pi / 2, 0.0]))
        # g_tt = -(1 - 2M/r)
        assert m[0, 0] < 0
        # g_rr = 1/(1 - 2M/r)
        assert m[1, 1] > 0


class TestManifold:
    def test_creation(self):
        from grey_physics.core.ir.spacetime import Chart
        chart = Chart(
            name="cartesian",
            coord_names=["x", "y"],
            coord_ranges=[(-10, 10), (-10, 10)],
        )
        manifold = Manifold(
            name="R2",
            dimension=2,
            charts=[chart],
        )
        assert manifold.dimension == 2
