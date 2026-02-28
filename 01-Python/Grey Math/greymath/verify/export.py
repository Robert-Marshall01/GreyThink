"""
Grey Math — Proof Assistant Export.

Exports mathematical computations and verified properties to
proof assistant formats for formal verification.

Supports:
- Lean 4
- Coq
- LaTeX
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum, auto
from typing import Any

from greymath.core.expr import ExprNode, ExprKind


class ExportFormat(Enum):
    LEAN4 = auto()
    COQ = auto()
    LATEX = auto()


@dataclass
class ExportResult:
    """Result of exporting to a proof assistant or typesetting format."""
    format: ExportFormat
    content: str
    verified_properties: list[str]
    warnings: list[str]


class ProofExporter:
    """Export Grey Math expressions and proofs to external formats."""

    def export(
        self,
        expr: ExprNode,
        fmt: ExportFormat = ExportFormat.LATEX,
        properties: list[str] | None = None,
    ) -> ExportResult:
        """Export an expression to the given format."""
        dispatch = {
            ExportFormat.LEAN4: self._to_lean4,
            ExportFormat.COQ: self._to_coq,
            ExportFormat.LATEX: self._to_latex,
        }
        converter = dispatch[fmt]
        content = converter(expr)
        return ExportResult(
            format=fmt,
            content=content,
            verified_properties=properties or [],
            warnings=[],
        )

    # ── LaTeX ──────────────────────────────────────────────────────────────

    def _to_latex(self, expr: ExprNode) -> str:
        """Convert expression to LaTeX."""
        if expr.kind == ExprKind.LITERAL:
            if isinstance(expr.value, float):
                return f"{expr.value:.6g}"
            return str(expr.value)

        if expr.kind in (ExprKind.SYMBOL, ExprKind.VARIABLE):
            name = expr.name or "?"
            # Greek letter mapping
            greek = {
                "alpha": r"\alpha", "beta": r"\beta", "gamma": r"\gamma",
                "delta": r"\delta", "epsilon": r"\epsilon", "zeta": r"\zeta",
                "eta": r"\eta", "theta": r"\theta", "iota": r"\iota",
                "kappa": r"\kappa", "lambda": r"\lambda", "mu": r"\mu",
                "nu": r"\nu", "xi": r"\xi", "pi": r"\pi", "rho": r"\rho",
                "sigma": r"\sigma", "tau": r"\tau", "upsilon": r"\upsilon",
                "phi": r"\phi", "chi": r"\chi", "psi": r"\psi", "omega": r"\omega",
                "Gamma": r"\Gamma", "Delta": r"\Delta", "Theta": r"\Theta",
                "Lambda": r"\Lambda", "Xi": r"\Xi", "Pi": r"\Pi",
                "Sigma": r"\Sigma", "Phi": r"\Phi", "Psi": r"\Psi", "Omega": r"\Omega",
            }
            return greek.get(name, name)

        if expr.kind == ExprKind.ADD:
            parts = [self._to_latex(c) for c in expr.children]
            return " + ".join(parts)

        if expr.kind == ExprKind.SUB:
            return f"{self._to_latex(expr.children[0])} - {self._to_latex(expr.children[1])}"

        if expr.kind == ExprKind.MUL:
            parts = [self._to_latex(c) for c in expr.children]
            return r" \cdot ".join(parts)

        if expr.kind == ExprKind.DIV:
            num = self._to_latex(expr.children[0])
            den = self._to_latex(expr.children[1])
            return rf"\frac{{{num}}}{{{den}}}"

        if expr.kind == ExprKind.POW:
            base = self._to_latex(expr.children[0])
            exp = self._to_latex(expr.children[1])
            return f"{{{base}}}^{{{exp}}}"

        if expr.kind == ExprKind.NEG:
            return f"-{self._to_latex(expr.children[0])}"

        if expr.kind == ExprKind.DIFF:
            if len(expr.children) >= 2:
                body = self._to_latex(expr.children[0])
                var = self._to_latex(expr.children[1])
                return rf"\frac{{d}}{{d{var}}} {body}"
            return rf"\frac{{d}}{{dx}} {self._to_latex(expr.children[0])}"

        if expr.kind == ExprKind.INTEGRAL:
            body = self._to_latex(expr.children[0])
            return rf"\int {body} \, dx"

        if expr.kind == ExprKind.SUM:
            return rf"\sum {self._to_latex(expr.children[0])}"

        if expr.kind == ExprKind.PRODUCT:
            return rf"\prod {self._to_latex(expr.children[0])}"

        if expr.kind == ExprKind.TRANSPOSE:
            return f"{self._to_latex(expr.children[0])}^{{\\top}}"

        if expr.kind == ExprKind.INVERSE:
            return f"{self._to_latex(expr.children[0])}^{{-1}}"

        if expr.kind == ExprKind.DET:
            return rf"\det\left({self._to_latex(expr.children[0])}\right)"

        if expr.kind == ExprKind.TRACE:
            return rf"\mathrm{{tr}}\left({self._to_latex(expr.children[0])}\right)"

        if expr.kind == ExprKind.NORM:
            return rf"\left\| {self._to_latex(expr.children[0])} \right\|"

        if expr.kind == ExprKind.APPLY:
            func = self._to_latex(expr.children[0])
            args = ", ".join(self._to_latex(c) for c in expr.children[1:])
            return rf"{func}\left({args}\right)"

        # Fallback
        return expr.to_string()

    # ── Lean 4 ─────────────────────────────────────────────────────────────

    def _to_lean4(self, expr: ExprNode) -> str:
        """Convert expression to Lean 4 stub."""
        lines = [
            "-- Auto-generated by Grey Math",
            "-- Requires Mathlib",
            "",
            "import Mathlib.Analysis.SpecialFunctions.Trigonometric.Basic",
            "import Mathlib.LinearAlgebra.Matrix.Determinant",
            "",
        ]

        variables = expr.collect_variables()
        if variables:
            var_decls = " ".join(f"({v} : ℝ)" for v in sorted(variables))
            lines.append(f"variable {var_decls}")
            lines.append("")

        lean_expr = self._expr_to_lean4(expr)
        lines.append(f"-- Expression: {expr.to_string()}")
        lines.append(f"#check {lean_expr}")
        lines.append("")
        lines.append("-- TODO: Add proof obligations")
        lines.append(f"theorem grey_math_result : {lean_expr} = sorry := by")
        lines.append("  sorry")

        return "\n".join(lines)

    def _expr_to_lean4(self, expr: ExprNode) -> str:
        if expr.kind == ExprKind.LITERAL:
            v = expr.value
            if isinstance(v, int):
                return str(v)
            if isinstance(v, float):
                return f"({v} : ℝ)"
            return str(v)

        if expr.kind in (ExprKind.SYMBOL, ExprKind.VARIABLE):
            return expr.name or "x"

        if expr.kind == ExprKind.ADD:
            parts = [self._expr_to_lean4(c) for c in expr.children]
            return " + ".join(parts)

        if expr.kind == ExprKind.SUB:
            return f"{self._expr_to_lean4(expr.children[0])} - {self._expr_to_lean4(expr.children[1])}"

        if expr.kind == ExprKind.MUL:
            parts = [self._expr_to_lean4(c) for c in expr.children]
            return " * ".join(parts)

        if expr.kind == ExprKind.DIV:
            return f"{self._expr_to_lean4(expr.children[0])} / {self._expr_to_lean4(expr.children[1])}"

        if expr.kind == ExprKind.POW:
            return f"{self._expr_to_lean4(expr.children[0])} ^ {self._expr_to_lean4(expr.children[1])}"

        if expr.kind == ExprKind.NEG:
            return f"-{self._expr_to_lean4(expr.children[0])}"

        return f"sorry /- {expr.to_string()} -/"

    # ── Coq ────────────────────────────────────────────────────────────────

    def _to_coq(self, expr: ExprNode) -> str:
        """Convert expression to Coq stub."""
        lines = [
            "(* Auto-generated by Grey Math *)",
            "Require Import Reals.",
            "Require Import Lra.",
            "Open Scope R_scope.",
            "",
        ]

        variables = expr.collect_variables()
        if variables:
            for v in sorted(variables):
                lines.append(f"Variable {v} : R.")
            lines.append("")

        coq_expr = self._expr_to_coq(expr)
        lines.append(f"(* Expression: {expr.to_string()} *)")
        lines.append(f"Check ({coq_expr}).")
        lines.append("")
        lines.append(f"Lemma grey_math_result : {coq_expr} = {coq_expr}.")
        lines.append("Proof.")
        lines.append("  reflexivity.")
        lines.append("Qed.")

        return "\n".join(lines)

    def _expr_to_coq(self, expr: ExprNode) -> str:
        if expr.kind == ExprKind.LITERAL:
            v = expr.value
            if isinstance(v, int):
                return f"(IZR {v})"
            if isinstance(v, float):
                return f"({v}%R)"
            return str(v)

        if expr.kind in (ExprKind.SYMBOL, ExprKind.VARIABLE):
            return expr.name or "x"

        if expr.kind == ExprKind.ADD:
            parts = [self._expr_to_coq(c) for c in expr.children]
            return " + ".join(parts)

        if expr.kind == ExprKind.SUB:
            return f"({self._expr_to_coq(expr.children[0])} - {self._expr_to_coq(expr.children[1])})"

        if expr.kind == ExprKind.MUL:
            parts = [self._expr_to_coq(c) for c in expr.children]
            return " * ".join(parts)

        if expr.kind == ExprKind.DIV:
            return f"({self._expr_to_coq(expr.children[0])} / {self._expr_to_coq(expr.children[1])})"

        if expr.kind == ExprKind.NEG:
            return f"(- {self._expr_to_coq(expr.children[0])})"

        return f"(* {expr.to_string()} *) 0"


class LaTeXDocument:
    """Build a complete LaTeX document from Grey Math results."""

    def __init__(self, title: str = "Grey Math Export") -> None:
        self.title = title
        self._sections: list[tuple[str, str]] = []
        self._exporter = ProofExporter()

    def add_expression(self, label: str, expr: ExprNode) -> None:
        result = self._exporter.export(expr, ExportFormat.LATEX)
        self._sections.append((label, f"$$ {result.content} $$"))

    def add_text(self, label: str, text: str) -> None:
        self._sections.append((label, text))

    def add_equation(self, label: str, lhs: ExprNode, rhs: ExprNode) -> None:
        lhs_tex = self._exporter.export(lhs, ExportFormat.LATEX).content
        rhs_tex = self._exporter.export(rhs, ExportFormat.LATEX).content
        self._sections.append((label, f"$$ {lhs_tex} = {rhs_tex} $$"))

    def render(self) -> str:
        lines = [
            r"\documentclass{article}",
            r"\usepackage{amsmath, amssymb, amsthm}",
            r"\title{" + self.title + "}",
            r"\author{Grey Math IDE}",
            r"\begin{document}",
            r"\maketitle",
            "",
        ]

        for label, content in self._sections:
            lines.append(rf"\section{{{label}}}")
            lines.append(content)
            lines.append("")

        lines.append(r"\end{document}")
        return "\n".join(lines)
