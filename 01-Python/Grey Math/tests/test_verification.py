"""Tests for Grey Math verification layer."""

import pytest
import numpy as np
import math

from greymath.verify.properties import (
    PropertyChecker,
    MathProperty,
    PropertyKind,
    DomainGenerator,
)
from greymath.verify.equivalence import SymbolicEquivalence
from greymath.verify.bounds import BoundsChecker
from greymath.verify.export import ProofExporter, ExportFormat, LaTeXDocument
from greymath.core.expr import ExprNode, ExprKind, Expr
from greymath.numeric.precision import Interval


class TestPropertyChecker:
    """Tests for property-based verification."""

    def test_identity_sin_squared_plus_cos_squared(self):
        """sin²(x) + cos²(x) = 1"""
        checker = PropertyChecker(n_samples=500, seed=42)
        prop = MathProperty(
            name="pythagorean_identity",
            kind=PropertyKind.IDENTITY,
            function=lambda x: math.sin(x) ** 2 + math.cos(x) ** 2,
            reference=lambda x: 1.0,
            tolerance=1e-10,
        )
        result = checker.check(prop)
        assert result.verified
        assert result.n_failures == 0

    def test_involution_negation(self):
        """--x = x"""
        checker = PropertyChecker(n_samples=500, seed=42)
        prop = MathProperty(
            name="double_negation",
            kind=PropertyKind.INVOLUTION,
            function=lambda x: -x,
        )
        result = checker.check(prop)
        assert result.verified

    def test_commutative_addition(self):
        """x + y = y + x"""
        checker = PropertyChecker(n_samples=500, seed=42)
        prop = MathProperty(
            name="addition_commutativity",
            kind=PropertyKind.COMMUTATIVE,
            function=lambda x, y: x + y,
        )
        result = checker.check(prop)
        assert result.verified

    def test_associative_addition(self):
        """(x + y) + z = x + (y + z)"""
        checker = PropertyChecker(n_samples=500, seed=42)
        prop = MathProperty(
            name="addition_associativity",
            kind=PropertyKind.ASSOCIATIVE,
            function=lambda x, y: x + y,
        )
        result = checker.check(prop)
        assert result.verified

    def test_monotone_exp(self):
        """exp is monotonically increasing"""
        checker = PropertyChecker(n_samples=500, seed=42)
        prop = MathProperty(
            name="exp_monotone",
            kind=PropertyKind.MONOTONE,
            function=math.exp,
            domain_generator=lambda n: sorted(DomainGenerator.real_scalars(n, -10, 10)),
        )
        result = checker.check(prop)
        assert result.verified

    def test_bounded_sin(self):
        """|sin(x)| <= 1"""
        checker = PropertyChecker(n_samples=500, seed=42)
        prop = MathProperty(
            name="sin_bounded",
            kind=PropertyKind.BOUNDED,
            function=math.sin,
            bound=1.0,
        )
        result = checker.check(prop)
        assert result.verified

    def test_failing_property(self):
        """x^2 is NOT monotone on all of R"""
        checker = PropertyChecker(n_samples=500, seed=42)
        prop = MathProperty(
            name="square_not_monotone",
            kind=PropertyKind.MONOTONE,
            function=lambda x: x ** 2,
        )
        result = checker.check(prop)
        assert not result.verified


class TestDomainGenerator:
    """Tests for domain sample generation."""

    def test_real_scalars(self):
        samples = DomainGenerator.real_scalars(100)
        assert len(samples) == 100
        assert all(isinstance(x, float) for x in samples)

    def test_positive_reals(self):
        samples = DomainGenerator.positive_reals(100)
        assert all(x > 0 for x in samples)

    def test_vectors(self):
        samples = DomainGenerator.vectors(10, dim=5)
        assert len(samples) == 10
        assert all(v.shape == (5,) for v in samples)

    def test_symmetric_matrices(self):
        samples = DomainGenerator.symmetric_matrices(10, dim=3)
        for m in samples:
            np.testing.assert_array_almost_equal(m, m.T)

    def test_positive_definite_matrices(self):
        samples = DomainGenerator.positive_definite_matrices(10, dim=3)
        for m in samples:
            eigenvalues = np.linalg.eigvalsh(m)
            assert all(v > 0 for v in eigenvalues)


class TestBoundsChecker:
    """Tests for bounds verification."""

    def test_verify_scalar_with_exact(self):
        checker = BoundsChecker()
        result = checker.verify_scalar(3.14159, exact=math.pi)
        assert result.contains_exact
        assert result.width < 0.001

    def test_verify_matrix_multiply(self):
        checker = BoundsChecker()
        A = np.random.randn(5, 5)
        B = np.random.randn(5, 5)
        C = A @ B
        result = checker.verify_matrix_multiply(A, B, C)
        assert result.certified
        assert result.contains_exact

    def test_verify_linear_solve(self):
        checker = BoundsChecker()
        A = np.array([[4, 1], [1, 3]], dtype=float)
        b = np.array([1, 2], dtype=float)
        x = np.linalg.solve(A, b)
        result = checker.verify_linear_solve(A, b, x)
        assert result.certified

    def test_verify_eigendecomposition(self):
        checker = BoundsChecker()
        A = np.array([[2, 1], [1, 2]], dtype=float)
        vals, vecs = np.linalg.eigh(A)
        results = checker.verify_eigendecomposition(A, vals, vecs)
        assert len(results) == 2
        for r in results:
            assert r.certified


class TestSymbolicEquivalence:
    """Tests for symbolic equivalence checking."""

    def test_structural_equality(self):
        eq = SymbolicEquivalence()
        x = Expr.var("x")
        a = x + Expr.lit(1)
        b = x + Expr.lit(1)
        result = eq.check(a, b)
        assert result.equivalent

    def test_different_expressions(self):
        eq = SymbolicEquivalence()
        x = Expr.var("x")
        a = x + Expr.lit(1)
        b = x + Expr.lit(2)
        result = eq.check(a, b)
        assert not result.equivalent


class TestProofExporter:
    """Tests for proof export."""

    def test_latex_export(self):
        exporter = ProofExporter()
        x = Expr.var("x")
        expr = x ** Expr.lit(2) + Expr.lit(1)
        result = exporter.export(expr, ExportFormat.LATEX)
        assert result.format == ExportFormat.LATEX
        assert "x" in result.content

    def test_lean4_export(self):
        exporter = ProofExporter()
        x = Expr.var("x")
        expr = x + Expr.lit(1)
        result = exporter.export(expr, ExportFormat.LEAN4)
        assert "Mathlib" in result.content

    def test_coq_export(self):
        exporter = ProofExporter()
        x = Expr.var("x")
        expr = x + Expr.lit(1)
        result = exporter.export(expr, ExportFormat.COQ)
        assert "Require Import Reals" in result.content

    def test_latex_document(self):
        doc = LaTeXDocument(title="Test")
        x = Expr.var("x")
        doc.add_expression("Test Expression", x ** Expr.lit(2))
        doc.add_text("Notes", "This is a test.")
        content = doc.render()
        assert "\\documentclass" in content
        assert "Test" in content
