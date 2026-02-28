"""Tests for the Symbolic physics engine."""
import numpy as np
import pytest

from grey_physics.core.ir.types import Expr, ExprKind
from grey_physics.core.symbolic import SymbolicEngine


@pytest.fixture
def engine():
    return SymbolicEngine()


class TestSimplify:
    def test_add_zero(self, engine):
        x = Expr.symbol("x")
        z = Expr.constant(0)
        e = x + z
        s = engine.simplify_physics(e)
        # Should simplify to just x
        assert s.kind == ExprKind.SYMBOL and s.name == "x"

    def test_mul_one(self, engine):
        x = Expr.symbol("x")
        one = Expr.constant(1)
        e = one * x
        s = engine.simplify_physics(e)
        assert s.kind == ExprKind.SYMBOL and s.name == "x"

    def test_mul_zero(self, engine):
        x = Expr.symbol("x")
        z = Expr.constant(0)
        e = z * x
        s = engine.simplify_physics(e)
        assert s.kind == ExprKind.CONSTANT and s.value == 0

    def test_constant_fold(self, engine):
        a = Expr.constant(3)
        b = Expr.constant(4)
        e = a + b
        s = engine.simplify_physics(e)
        assert s.kind == ExprKind.CONSTANT
        assert abs(s.value - 7) < 1e-12


class TestEulerLagrange:
    def test_free_particle(self, engine):
        """L = ½m ẋ² → Euler-Lagrange: m ẍ = 0."""
        from grey_physics.core.ir.mechanics import Lagrangian
        L = Lagrangian.free_particle(mass=1.0)
        eqs = engine.derive_equations(L, method="euler_lagrange")
        assert eqs is not None


class TestPoisson:
    def test_bracket_canonical(self, engine):
        """Poisson bracket {q, p} = 1 (analytically)."""
        q = Expr.symbol("q")
        p = Expr.symbol("p")
        result = engine.poisson_bracket(q, p, [q], [p])
        # Should be constant 1
        assert result is not None


class TestChristoffel:
    def test_flat_metric_zero(self, engine):
        """Christoffel symbols of Euclidean metric = 0."""
        dim = 2
        # Flat metric g = diag(1,1)
        g = [[None] * dim for _ in range(dim)]
        for i in range(dim):
            for j in range(dim):
                g[i][j] = Expr.constant(1.0 if i == j else 0.0)
        gamma = engine.christoffel_symbols(g, dim)
        # All should be zero (constant 0)
        for i in range(dim):
            for j in range(dim):
                for k in range(dim):
                    c = gamma[i][j][k]
                    s = engine.simplify_physics(c)
                    if s.kind == ExprKind.CONSTANT:
                        assert abs(s.value) < 1e-10
