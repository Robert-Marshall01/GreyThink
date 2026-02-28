"""Tests for the Core IR type system."""
import math
import uuid
import pytest

from grey_physics.core.ir.types import (
    BaseUnit, Units, Metadata, StabilityLevel, DomainType,
    ExprKind, Expr, IRNode, Coordinate, IndexSlot, TensorRank,
    NumericTensor,
)


# ── Units ────────────────────────────────────────────────────

class TestUnits:
    def test_dimensionless(self):
        u = Units.dimensionless()
        assert u.length == 0 and u.mass == 0 and u.time == 0

    def test_mul(self):
        m = Units(mass=1)
        a = Units(length=1, time=-2)
        force = m * a
        assert force.mass == 1
        assert force.length == 1
        assert force.time == -2

    def test_div(self):
        dist = Units(length=1)
        t = Units(time=1)
        vel = dist / t
        assert vel.length == 1
        assert vel.time == -1

    def test_pow(self):
        L = Units(length=1)
        area = L ** 2
        assert area.length == 2

    def test_is_compatible(self):
        a = Units(length=1, time=-1)
        b = Units(length=1, time=-1)
        assert a.is_compatible(b)
        c = Units(length=1)
        assert not a.is_compatible(c)


# ── Metadata ─────────────────────────────────────────────────

class TestMetadata:
    def test_defaults(self):
        m = Metadata()
        assert m.stability == StabilityLevel.UNKNOWN
        assert m.domain == DomainType.GENERAL

    def test_custom(self):
        u = Units(length=1, time=-2)
        m = Metadata(units=u, stability=StabilityLevel.STABLE,
                     domain=DomainType.QUANTUM, tags={"angular"})
        assert m.units.length == 1
        assert "angular" in m.tags


# ── Expr DAG ─────────────────────────────────────────────────

class TestExpr:
    def test_symbol(self):
        x = Expr.symbol("x")
        assert x.kind == ExprKind.SYMBOL
        assert x.name == "x"
        assert repr(x) == "x"

    def test_constant(self):
        c = Expr.constant(3.14)
        assert c.kind == ExprKind.CONSTANT
        assert abs(c.value - 3.14) < 1e-12

    def test_add(self):
        x = Expr.symbol("x")
        y = Expr.symbol("y")
        s = x + y
        assert s.kind == ExprKind.ADD

    def test_mul(self):
        x = Expr.symbol("x")
        c = Expr.constant(2)
        p = c * x
        assert p.kind == ExprKind.MUL

    def test_pow(self):
        x = Expr.symbol("x")
        p = x ** 3
        assert p.kind == ExprKind.POW

    def test_neg(self):
        x = Expr.symbol("x")
        n = -x
        assert n.kind == ExprKind.NEG

    def test_sub(self):
        x = Expr.symbol("x")
        y = Expr.symbol("y")
        d = x - y
        # x - y is x + (-y)
        assert d.kind == ExprKind.ADD

    def test_div(self):
        x = Expr.symbol("x")
        y = Expr.symbol("y")
        d = x / y
        assert d.kind == ExprKind.DIV

    def test_derivative(self):
        x = Expr.symbol("x")
        f = Expr.func("sin", x)
        df = f.diff(x)
        assert df.kind == ExprKind.DERIVATIVE

    def test_substitute(self):
        x = Expr.symbol("x")
        y = Expr.symbol("y")
        e = x + y
        e2 = e.substitute(x, Expr.constant(1))
        # After substitution x → 1, the tree should contain constant(1)
        assert any(
            c.kind == ExprKind.CONSTANT and c.value == 1
            for c in e2.children
        )

    def test_free_symbols(self):
        x = Expr.symbol("x")
        y = Expr.symbol("y")
        e = x * y + x
        syms = e.free_symbols()
        assert "x" in syms and "y" in syms


# ── IRNode ───────────────────────────────────────────────────

class TestIRNode:
    def test_concrete_subclass(self):
        class TestNode(IRNode):
            def canonical_form(self):
                return "test_canonical"
        node = TestNode(name="T")
        assert node.name == "T"
        assert isinstance(node.id, uuid.UUID)
        assert node.canonical_form() == "test_canonical"


# ── Coordinate ───────────────────────────────────────────────

class TestCoordinate:
    def test_creation(self):
        c = Coordinate(name="x", index=0)
        assert c.name == "x"
        assert c.index == 0


# ── TensorRank & NumericTensor ───────────────────────────────

class TestNumericTensor:
    def test_creation(self):
        import numpy as np
        rank = TensorRank(
            contravariant=1, covariant=0,
            slots=[IndexSlot(name="i", position="up")]
        )
        data = np.array([1.0, 2.0, 3.0])
        t = NumericTensor(components=data, rank=rank, name="v")
        assert t.components.shape == (3,)
        assert t.rank.contravariant == 1
