"""
Grey Math — Example: Symbolic Differentiation & Series Expansion.

Demonstrates symbolic expression construction, differentiation,
simplification, and Taylor series expansion.
"""

from greymath.core.expr import ExprNode, ExprKind, Expr
from greymath.symbolic.differentiation import SymbolicDiff
from greymath.symbolic.simplify import Simplifier
from greymath.symbolic.series import SeriesExpander
from greymath.symbolic.analysis import StructuralAnalyzer
from greymath.verify.export import ProofExporter, ExportFormat


def main():
    print("═══ Grey Math: Symbolic Computation Example ═══\n")

    diff = SymbolicDiff()
    simp = Simplifier()
    analyzer = StructuralAnalyzer()
    exporter = ProofExporter()

    # ── Build a polynomial expression: x^3 + 2x^2 - 5x + 3 ────────────
    print("── Polynomial Expression ──")
    x = Expr.var("x")
    poly = x ** 3 + Expr.lit(2) * x ** 2 - Expr.lit(5) * x + Expr.lit(3)
    print(f"f(x) = {poly.to_string()}")

    # Structural analysis
    degree = analyzer.polynomial_degree(poly)
    print(f"Polynomial degree: {degree}")
    print(f"Is linear: {analyzer.is_linear(poly, 'x')}")

    # ── Differentiation ────────────────────────────────────────────────────
    print("\n── Differentiation ──")
    df = diff.differentiate(poly, "x")
    df_simplified = simp.simplify(df)
    print(f"f'(x) = {df.to_string()}")
    print(f"f'(x) simplified = {df_simplified.to_string()}")

    d2f = diff.differentiate(df, "x")
    d2f_simplified = simp.simplify(d2f)
    print(f"f''(x) = {d2f_simplified.to_string()}")

    # ── Multi-variable expression ──────────────────────────────────────────
    print("\n── Multivariate Expression ──")
    y = Expr.var("y")
    g = x ** 2 * y + Expr.lit(3) * x * y ** 2
    print(f"g(x,y) = {g.to_string()}")

    # Gradient
    grad = diff.gradient(g, ["x", "y"])
    print("∇g:")
    for var_name, partial in zip(["x", "y"], grad):
        partial_simp = simp.simplify(partial)
        print(f"  ∂g/∂{var_name} = {partial_simp.to_string()}")

    # ── Series Expansion ───────────────────────────────────────────────────
    print("\n── Series Expansion ──")
    expander = SeriesExpander()

    # Taylor series of a simple function
    f_expr = x ** 2 + x + Expr.lit(1)
    series = expander.taylor(f_expr, "x", center=0.0, order=5)
    print(f"Taylor series of {f_expr.to_string()}:")
    for term in series.terms:
        if abs(term.coefficient) > 1e-15:
            print(f"  order {term.order}: {term.coefficient:.6g}")

    # ── LaTeX Export ───────────────────────────────────────────────────────
    print("\n── LaTeX Export ──")
    latex_result = exporter.export(poly, ExportFormat.LATEX)
    print(f"LaTeX: {latex_result.content}")

    # ── Lean 4 Export ──────────────────────────────────────────────────────
    print("\n── Lean 4 Export ──")
    lean_result = exporter.export(poly, ExportFormat.LEAN4)
    print(lean_result.content[:200] + "...")

    print("\n✓ Symbolic computation complete.")


if __name__ == "__main__":
    main()
