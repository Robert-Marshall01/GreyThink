"""Tests for Grey Math core IR types and expressions."""

import pytest
import numpy as np

from greymath.core.types import (
    Scalar, Vector, Matrix, Tensor, MathFunction,
    Functional, LinearOperator, ScalarField, NormType,
)
from greymath.core.expr import ExprNode, ExprKind, ExprDAG, Expr
from greymath.core.metadata import Precision, PrecisionMode, Metadata


class TestMathTypes:
    """Tests for mathematical type system."""

    def test_scalar_creation(self):
        s = Scalar(value=3.14, field=ScalarField.REAL)
        assert s.value == 3.14
        assert s.field == ScalarField.REAL
        assert s.type_name() == "Scalar"

    def test_complex_scalar(self):
        s = Scalar(value=1 + 2j, field=ScalarField.COMPLEX)
        assert s.value == 1 + 2j
        assert s.field == ScalarField.COMPLEX

    def test_vector_creation(self):
        v = Vector(components=[1.0, 2.0, 3.0])
        assert len(v.components) == 3
        assert v.dimension == 3
        assert v.type_name() == "Vector"

    def test_matrix_creation(self):
        m = Matrix(entries=[[1, 0], [0, 1]], rows=2, cols=2)
        assert m.rows == 2
        assert m.cols == 2
        assert m.type_name() == "Matrix"

    def test_tensor_creation(self):
        t = Tensor(components=[1, 2, 3, 4], shape=(2, 2), rank=2)
        assert t.shape == (2, 2)
        assert t.rank == 2

    def test_math_function(self):
        f = MathFunction(name="sin", domain="R", codomain="[-1,1]")
        assert f.name == "sin"
        assert f.type_name() == "Function"

    def test_type_canonical(self):
        s = Scalar(value=42)
        canonical = s.canonical()
        assert canonical is not None


class TestExprNode:
    """Tests for expression DAG nodes."""

    def test_literal_creation(self):
        node = ExprNode(kind=ExprKind.LITERAL, value=42)
        assert node.kind == ExprKind.LITERAL
        assert node.value == 42
        assert node.to_string() == "42"

    def test_symbol_creation(self):
        node = ExprNode(kind=ExprKind.SYMBOL, name="x")
        assert node.kind == ExprKind.SYMBOL
        assert node.name == "x"
        assert node.to_string() == "x"

    def test_variable_creation(self):
        node = ExprNode(kind=ExprKind.VARIABLE, name="y")
        assert node.kind == ExprKind.VARIABLE

    def test_addition(self):
        a = ExprNode(kind=ExprKind.LITERAL, value=3)
        b = ExprNode(kind=ExprKind.LITERAL, value=4)
        c = a + b
        assert c.kind == ExprKind.ADD
        assert len(c.children) == 2

    def test_multiplication(self):
        a = ExprNode(kind=ExprKind.LITERAL, value=3)
        b = ExprNode(kind=ExprKind.LITERAL, value=4)
        c = a * b
        assert c.kind == ExprKind.MUL

    def test_subtraction(self):
        a = ExprNode(kind=ExprKind.LITERAL, value=10)
        b = ExprNode(kind=ExprKind.LITERAL, value=3)
        c = a - b
        assert c.kind == ExprKind.SUB

    def test_division(self):
        a = ExprNode(kind=ExprKind.LITERAL, value=10)
        b = ExprNode(kind=ExprKind.LITERAL, value=2)
        c = a / b
        assert c.kind == ExprKind.DIV

    def test_power(self):
        a = ExprNode(kind=ExprKind.LITERAL, value=2)
        b = ExprNode(kind=ExprKind.LITERAL, value=3)
        c = a ** b
        assert c.kind == ExprKind.POW

    def test_negation(self):
        a = ExprNode(kind=ExprKind.LITERAL, value=5)
        c = -a
        assert c.kind == ExprKind.NEG

    def test_collect_variables(self):
        x = ExprNode(kind=ExprKind.VARIABLE, name="x")
        y = ExprNode(kind=ExprKind.VARIABLE, name="y")
        expr = x + y
        variables = expr.collect_variables()
        assert variables == {"x", "y"}

    def test_collect_symbols(self):
        x = ExprNode(kind=ExprKind.SYMBOL, name="alpha")
        y = ExprNode(kind=ExprKind.SYMBOL, name="beta")
        expr = x * y
        symbols = expr.collect_symbols()
        assert "alpha" in symbols
        assert "beta" in symbols

    def test_to_string_complex(self):
        x = ExprNode(kind=ExprKind.VARIABLE, name="x")
        two = ExprNode(kind=ExprKind.LITERAL, value=2)
        expr = x ** two + x + ExprNode(kind=ExprKind.LITERAL, value=1)
        s = expr.to_string()
        assert "x" in s

    def test_substitute(self):
        x = ExprNode(kind=ExprKind.VARIABLE, name="x")
        three = ExprNode(kind=ExprKind.LITERAL, value=3)
        expr = x + ExprNode(kind=ExprKind.LITERAL, value=1)
        result = expr.substitute("x", three)
        assert result.children[0].value == 3

    def test_diff_shortcut(self):
        x = ExprNode(kind=ExprKind.VARIABLE, name="x")
        expr = x ** ExprNode(kind=ExprKind.LITERAL, value=2)
        deriv = expr.diff("x")
        assert deriv.kind == ExprKind.DIFF


class TestExprConvenience:
    """Tests for the Expr convenience class."""

    def test_lit(self):
        node = Expr.lit(42)
        assert node.kind == ExprKind.LITERAL
        assert node.value == 42

    def test_sym(self):
        node = Expr.sym("alpha")
        assert node.kind == ExprKind.SYMBOL
        assert node.name == "alpha"

    def test_var(self):
        node = Expr.var("x")
        assert node.kind == ExprKind.VARIABLE
        assert node.name == "x"


class TestExprDAG:
    """Tests for expression DAG with structural hashing."""

    def test_canonicalize(self):
        dag = ExprDAG()
        x = ExprNode(kind=ExprKind.VARIABLE, name="x")
        y = ExprNode(kind=ExprKind.VARIABLE, name="x")
        c1 = dag.canonicalize(x)
        c2 = dag.canonicalize(y)
        assert c1 is c2  # Same object due to hash-consing

    def test_topological_order(self):
        dag = ExprDAG()
        x = ExprNode(kind=ExprKind.VARIABLE, name="x")
        two = ExprNode(kind=ExprKind.LITERAL, value=2)
        expr = x + two
        order = dag.topological_order(expr)
        assert len(order) == 3  # x, 2, x+2


class TestMetadata:
    """Tests for precision and metadata."""

    def test_precision_creation(self):
        p = Precision(mode=PrecisionMode.FLOAT64, bits=64)
        assert p.mode == PrecisionMode.FLOAT64
        assert p.bits == 64

    def test_metadata_creation(self):
        m = Metadata()
        assert m.precision is None
        assert m.stability is None
