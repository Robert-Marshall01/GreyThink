"""
Symbolic Engine — Compiler layer for Grey Math.

Provides symbolic reasoning through:
- Pattern matching and rewrite rules
- Algebraic simplification
- Symbolic differentiation (scalar, vector, higher-order)
- Variational calculus
- Series expansions
- Tensor calculus
- Category-theoretic transformations
- Structural analysis (linearity, convexity, symmetry)
"""

from greymath.symbolic.rewrite import RewriteRule, RuleRegistry, RewriteEngine  # noqa: F401
from greymath.symbolic.pattern import Pattern, PatternMatcher  # noqa: F401
from greymath.symbolic.simplify import Simplifier  # noqa: F401
from greymath.symbolic.differentiation import SymbolicDiff  # noqa: F401
from greymath.symbolic.series import SeriesExpander  # noqa: F401
from greymath.symbolic.analysis import StructuralAnalyzer  # noqa: F401
