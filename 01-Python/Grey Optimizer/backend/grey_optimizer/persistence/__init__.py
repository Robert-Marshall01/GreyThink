"""
Persistence Package

SQLite storage for audit logs and proof artifacts.
Provides append-only audit logging and signed proof generation.
"""

from .database import Database
from .proof_generator import ProofGenerator

__all__ = ["Database", "ProofGenerator"]
