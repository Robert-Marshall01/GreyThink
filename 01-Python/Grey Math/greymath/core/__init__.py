"""
Core Mathematical IR (Intermediate Representation).

This package defines the unified type system that represents all mathematical
objects used in advanced AI research: scalars, tensors, operators, manifolds,
algebraic structures, categories, and more.
"""

from greymath.core.types import *  # noqa: F401,F403
from greymath.core.expr import Expr, ExprNode, ExprDAG  # noqa: F401
from greymath.core.metadata import Metadata, Precision, StabilityInfo  # noqa: F401
