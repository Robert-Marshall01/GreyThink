"""
Safety & Isolation Layer — Experimental Module.

Provides deep safety enforcement beyond the basic ExperimentalMode:
- Sandboxed execution with resource tracking
- Deterministic mode enforcement
- Error-bounded numerics (condition number gating)
- Symbolic verification hooks
- Numerical anomaly detection (NaN, Inf, ill-conditioning)
- Execution audit trail
- Rollback support
- Type safety validators for experimental IR types
"""

from __future__ import annotations

import hashlib
import logging
import time
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Callable, Optional, TypeVar

import numpy as np
from numpy.typing import NDArray

from greymath.experimental.mode import (
    ExperimentalContext,
    StabilityLevel,
    get_experimental_context,
    require_experimental_context,
)

logger = logging.getLogger(__name__)

T = TypeVar("T")


# ─── Execution Audit ────────────────────────────────────────────────────────

@dataclass
class AuditEntry:
    """A single entry in the execution audit trail."""
    timestamp: float
    operation: str
    stability_level: StabilityLevel
    duration_ms: float = 0.0
    result_hash: Optional[str] = None
    warnings: list[str] = field(default_factory=list)
    error: Optional[str] = None

    def __repr__(self) -> str:
        status = "ERROR" if self.error else "OK"
        return (f"Audit({self.operation}, {self.stability_level.name}, "
                f"{self.duration_ms:.1f}ms, {status})")


class ExecutionAudit:
    """
    Audit trail for all experimental operations.
    Records what was executed, stability level, timing, and results.
    """

    def __init__(self) -> None:
        self.entries: list[AuditEntry] = []
        self._active_entry: Optional[AuditEntry] = None

    def begin(self, operation: str,
              stability_level: StabilityLevel = StabilityLevel.ALPHA) -> None:
        """Begin tracking an operation."""
        self._active_entry = AuditEntry(
            timestamp=time.time(),
            operation=operation,
            stability_level=stability_level,
        )

    def end(self, result: Any = None, error: Optional[str] = None) -> None:
        """Complete tracking an operation."""
        if self._active_entry is None:
            return
        self._active_entry.duration_ms = (
            (time.time() - self._active_entry.timestamp) * 1000
        )
        if error:
            self._active_entry.error = error
        elif result is not None:
            try:
                if isinstance(result, np.ndarray):
                    h = hashlib.md5(result.tobytes()).hexdigest()[:12]
                else:
                    h = hashlib.md5(str(result).encode()).hexdigest()[:12]
                self._active_entry.result_hash = h
            except Exception:
                pass

        self.entries.append(self._active_entry)
        self._active_entry = None

    def summary(self) -> dict[str, Any]:
        """Summary of the audit trail."""
        total_ms = sum(e.duration_ms for e in self.entries)
        n_errors = sum(1 for e in self.entries if e.error)
        by_stability = {}
        for e in self.entries:
            name = e.stability_level.name
            by_stability[name] = by_stability.get(name, 0) + 1
        return {
            "total_operations": len(self.entries),
            "total_time_ms": total_ms,
            "errors": n_errors,
            "by_stability_level": by_stability,
        }


# ─── Numerical Anomaly Detection ────────────────────────────────────────────

class AnomalyKind(Enum):
    """Types of numerical anomalies."""
    NAN = auto()
    INF = auto()
    ILL_CONDITIONED = auto()
    NEAR_SINGULAR = auto()
    LARGE_GRADIENT = auto()
    NUMERICAL_INSTABILITY = auto()


@dataclass
class NumericalAnomaly:
    """A detected numerical anomaly."""
    kind: AnomalyKind
    location: str
    value: Any = None
    severity: str = "warning"  # "info", "warning", "error"
    suggestion: str = ""

    def __repr__(self) -> str:
        return f"Anomaly({self.kind.name} at {self.location}, {self.severity})"


class AnomalyDetector:
    """
    Detect numerical anomalies in arrays, matrices, and computations.
    """

    @staticmethod
    def check_array(arr: NDArray, name: str = "array") -> list[NumericalAnomaly]:
        """Check an array for numerical anomalies."""
        anomalies: list[NumericalAnomaly] = []

        if np.any(np.isnan(arr)):
            n_nan = int(np.sum(np.isnan(arr)))
            anomalies.append(NumericalAnomaly(
                kind=AnomalyKind.NAN,
                location=name,
                value=f"{n_nan} NaN values",
                severity="error",
                suggestion="Check for division by zero or log of negative numbers",
            ))

        if np.any(np.isinf(arr)):
            n_inf = int(np.sum(np.isinf(arr)))
            anomalies.append(NumericalAnomaly(
                kind=AnomalyKind.INF,
                location=name,
                value=f"{n_inf} Inf values",
                severity="error",
                suggestion="Check for overflow or division by near-zero",
            ))

        if arr.size > 0 and not np.any(np.isnan(arr)):
            max_val = float(np.max(np.abs(arr)))
            if max_val > 1e15:
                anomalies.append(NumericalAnomaly(
                    kind=AnomalyKind.LARGE_GRADIENT,
                    location=name,
                    value=max_val,
                    severity="warning",
                    suggestion="Values are very large; consider normalization",
                ))

        return anomalies

    @staticmethod
    def check_matrix(A: NDArray, name: str = "matrix") -> list[NumericalAnomaly]:
        """Check a matrix for numerical anomalies."""
        anomalies = AnomalyDetector.check_array(A, name)

        if A.shape[0] == A.shape[1] and A.shape[0] > 0:
            try:
                kappa = float(np.linalg.cond(A))
                if kappa > 1e12:
                    anomalies.append(NumericalAnomaly(
                        kind=AnomalyKind.ILL_CONDITIONED,
                        location=name,
                        value=kappa,
                        severity="error" if kappa > 1e15 else "warning",
                        suggestion="Matrix is ill-conditioned; use regularization",
                    ))
            except Exception:
                pass

            try:
                det = float(np.linalg.det(A))
                if abs(det) < 1e-15:
                    anomalies.append(NumericalAnomaly(
                        kind=AnomalyKind.NEAR_SINGULAR,
                        location=name,
                        value=det,
                        severity="warning",
                        suggestion="Matrix is near-singular",
                    ))
            except Exception:
                pass

        return anomalies

    @staticmethod
    def check_convergence(
        sequence: list[float],
        name: str = "iteration",
    ) -> list[NumericalAnomaly]:
        """Check if a numerical sequence is converging."""
        anomalies = []
        if len(sequence) < 3:
            return anomalies

        # Check for divergence
        if sequence[-1] > 10 * sequence[0] and sequence[-1] > 1.0:
            anomalies.append(NumericalAnomaly(
                kind=AnomalyKind.NUMERICAL_INSTABILITY,
                location=name,
                value=f"grew from {sequence[0]:.2e} to {sequence[-1]:.2e}",
                severity="error",
                suggestion="Iteration is diverging; reduce step size or check formulation",
            ))

        # Check for NaN in sequence
        if any(np.isnan(v) for v in sequence):
            anomalies.append(NumericalAnomaly(
                kind=AnomalyKind.NAN,
                location=name,
                severity="error",
                suggestion="NaN appeared during iteration",
            ))

        return anomalies


# ─── Safe Computation Wrappers ───────────────────────────────────────────────

class SafeCompute:
    """
    Wrappers for safe numerical computation with error detection.
    """

    @staticmethod
    def safe_solve(
        A: NDArray,
        b: NDArray,
        condition_threshold: float = 1e12,
        regularize: bool = True,
        reg_lambda: float = 1e-10,
    ) -> tuple[NDArray, list[NumericalAnomaly]]:
        """
        Solve Ax = b with anomaly detection and optional regularization.
        """
        anomalies = AnomalyDetector.check_matrix(A, "coefficient_matrix")

        if any(a.kind == AnomalyKind.ILL_CONDITIONED for a in anomalies):
            if regularize:
                A_reg = A + reg_lambda * np.eye(A.shape[0])
                x = np.linalg.solve(A_reg, b)
                anomalies.append(NumericalAnomaly(
                    kind=AnomalyKind.ILL_CONDITIONED,
                    location="safe_solve",
                    value=f"Regularized with λ={reg_lambda}",
                    severity="info",
                ))
                return x, anomalies

        try:
            x = np.linalg.solve(A, b)
        except np.linalg.LinAlgError:
            if regularize:
                A_reg = A + reg_lambda * np.eye(A.shape[0])
                x = np.linalg.solve(A_reg, b)
            else:
                raise

        result_anomalies = AnomalyDetector.check_array(x, "solution")
        return x, anomalies + result_anomalies

    @staticmethod
    def safe_eigendecomposition(
        A: NDArray,
    ) -> tuple[NDArray, NDArray, list[NumericalAnomaly]]:
        """Eigendecomposition with anomaly detection."""
        anomalies = AnomalyDetector.check_matrix(A, "matrix")
        try:
            if np.allclose(A, A.T):
                vals, vecs = np.linalg.eigh(A)
            else:
                vals, vecs = np.linalg.eig(A)
        except np.linalg.LinAlgError as e:
            anomalies.append(NumericalAnomaly(
                kind=AnomalyKind.NUMERICAL_INSTABILITY,
                location="eigendecomposition",
                value=str(e),
                severity="error",
            ))
            return np.array([]), np.array([[]]), anomalies

        return vals, vecs, anomalies

    @staticmethod
    def safe_inverse(
        A: NDArray,
        reg_lambda: float = 1e-10,
    ) -> tuple[NDArray, list[NumericalAnomaly]]:
        """Matrix inverse with anomaly detection and regularization."""
        anomalies = AnomalyDetector.check_matrix(A, "matrix")
        try:
            A_inv = np.linalg.inv(A)
        except np.linalg.LinAlgError:
            A_reg = A + reg_lambda * np.eye(A.shape[0])
            A_inv = np.linalg.inv(A_reg)
            anomalies.append(NumericalAnomaly(
                kind=AnomalyKind.NEAR_SINGULAR,
                location="safe_inverse",
                value=f"Regularized with λ={reg_lambda}",
                severity="warning",
            ))
        return A_inv, anomalies


# ─── Deterministic Execution ────────────────────────────────────────────────

class DeterministicMode:
    """
    Enforce deterministic execution for reproducibility.
    """

    @staticmethod
    def seed_all(seed: int = 42) -> None:
        """Seed all random number generators."""
        np.random.seed(seed)

    @staticmethod
    def verify_deterministic(
        fn: Callable[[], Any],
        n_runs: int = 3,
        seed: int = 42,
    ) -> tuple[bool, list[Any]]:
        """
        Verify that a function produces deterministic results.

        Runs the function n_runs times with the same seed
        and checks that all results are identical.
        """
        results = []
        for _ in range(n_runs):
            np.random.seed(seed)
            result = fn()
            if isinstance(result, np.ndarray):
                results.append(result.copy())
            else:
                results.append(result)

        if not results:
            return True, results

        is_deterministic = True
        for i in range(1, len(results)):
            if isinstance(results[0], np.ndarray):
                if not np.allclose(results[0], results[i]):
                    is_deterministic = False
                    break
            elif results[0] != results[i]:
                is_deterministic = False
                break

        return is_deterministic, results


# ─── Rollback Support ───────────────────────────────────────────────────────

class Checkpoint:
    """Save and restore state for rollback support."""

    def __init__(self) -> None:
        self._snapshots: dict[str, list[Any]] = {}

    def save(self, name: str, state: Any) -> None:
        """Save a snapshot of state."""
        if name not in self._snapshots:
            self._snapshots[name] = []
        if isinstance(state, np.ndarray):
            self._snapshots[name].append(state.copy())
        else:
            self._snapshots[name].append(state)

    def restore(self, name: str, index: int = -1) -> Any:
        """Restore a previously saved snapshot."""
        if name not in self._snapshots or not self._snapshots[name]:
            raise ValueError(f"No snapshots for '{name}'")
        return self._snapshots[name][index]

    def clear(self, name: Optional[str] = None) -> None:
        """Clear snapshots."""
        if name is not None:
            self._snapshots.pop(name, None)
        else:
            self._snapshots.clear()

    def list_snapshots(self) -> dict[str, int]:
        """List all snapshot names and their counts."""
        return {name: len(snaps) for name, snaps in self._snapshots.items()}


# ─── Type Safety Validators ──────────────────────────────────────────────────

class TypeValidator:
    """Validate experimental IR types at runtime."""

    @staticmethod
    def validate_metric(g: NDArray) -> list[str]:
        """Validate that a matrix is a valid Riemannian metric."""
        errors: list[str] = []
        if g.ndim != 2 or g.shape[0] != g.shape[1]:
            errors.append("Metric must be a square matrix")
            return errors
        if not np.allclose(g, g.T):
            errors.append(f"Metric must be symmetric (asymmetry: {np.linalg.norm(g - g.T):.2e})")
        eigvals = np.linalg.eigvalsh(g)
        if np.any(eigvals <= 0):
            errors.append(f"Metric must be positive definite (min eigenvalue: {np.min(eigvals):.2e})")
        return errors

    @staticmethod
    def validate_probability_measure(density: NDArray, dx: float = 1.0) -> list[str]:
        """Validate that a density represents a probability measure."""
        errors: list[str] = []
        if np.any(density < 0):
            errors.append("Density must be non-negative")
        total = float(np.sum(density) * dx)
        if abs(total - 1.0) > 0.01:
            errors.append(f"Density must integrate to 1 (got {total:.6f})")
        return errors

    @staticmethod
    def validate_symplectic(omega: NDArray) -> list[str]:
        """Validate that a matrix is a valid symplectic form."""
        errors: list[str] = []
        n = omega.shape[0]
        if n % 2 != 0:
            errors.append("Symplectic form requires even dimension")
        if not np.allclose(omega, -omega.T):
            errors.append("Symplectic form must be antisymmetric")
        if abs(np.linalg.det(omega)) < 1e-10:
            errors.append("Symplectic form must be non-degenerate")
        return errors

    @staticmethod
    def validate_stochastic_matrix(P: NDArray) -> list[str]:
        """Validate that a matrix is a valid stochastic (transition) matrix."""
        errors: list[str] = []
        if np.any(P < 0):
            errors.append("Stochastic matrix must have non-negative entries")
        row_sums = np.sum(P, axis=1)
        if not np.allclose(row_sums, 1.0):
            max_err = float(np.max(np.abs(row_sums - 1.0)))
            errors.append(f"Rows must sum to 1 (max error: {max_err:.2e})")
        return errors
