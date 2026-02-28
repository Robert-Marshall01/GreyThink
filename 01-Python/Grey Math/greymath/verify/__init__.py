"""
Grey Math — Verification Layer.

Provides:
- Property-based testing for mathematical identities
- Symbolic equivalence checking
- Interval arithmetic validation
- Deterministic execution verification
- Proof assistant export (Lean, Coq stubs)
"""

from greymath.verify.properties import PropertyChecker, MathProperty
from greymath.verify.equivalence import SymbolicEquivalence
from greymath.verify.bounds import BoundsChecker
from greymath.verify.export import ProofExporter

__all__ = [
    "PropertyChecker",
    "MathProperty",
    "SymbolicEquivalence",
    "BoundsChecker",
    "ProofExporter",
]
