"""Tests for Core IR fields and spacetime modules."""
import numpy as np
import pytest

from grey_physics.core.ir.fields import (
    ScalarField, VectorField, TensorField, SpinorField,
)


class TestScalarField:
    def test_creation(self):
        sf = ScalarField(
            name="phi",
            dimension=3,
            expression=None,
        )
        assert sf.name == "phi"
        assert sf.dimension == 3

    def test_evaluate(self):
        # ScalarField with a callable expression
        sf = ScalarField(
            name="phi",
            dimension=2,
            expression=lambda x: x[0]**2 + x[1]**2,
        )
        val = sf.evaluate(np.array([3.0, 4.0]))
        assert abs(val - 25.0) < 1e-10


class TestVectorField:
    def test_creation(self):
        vf = VectorField(
            name="v",
            dimension=3,
            components=[
                lambda x: x[0],
                lambda x: x[1],
                lambda x: x[2],
            ],
        )
        assert vf.dimension == 3
        val = vf.evaluate(np.array([1.0, 2.0, 3.0]))
        np.testing.assert_allclose(val, [1.0, 2.0, 3.0])


class TestTensorField:
    def test_metric_like(self):
        """Test that a TensorField can represent a 2D diagonal metric."""
        tf = TensorField(
            name="g",
            dimension=2,
            rank=(0, 2),
            components=lambda x: np.diag([1.0, x[0]**2]),
        )
        g = tf.evaluate(np.array([2.0, 0.0]))
        expected = np.diag([1.0, 4.0])
        np.testing.assert_allclose(g, expected)
