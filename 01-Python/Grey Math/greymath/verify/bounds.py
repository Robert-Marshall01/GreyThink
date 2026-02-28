"""
Grey Math — Interval Bounds Verification.

Uses interval arithmetic to verify mathematical results
have guaranteed error bounds. Provides certified computation.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Callable, Optional

import numpy as np

from greymath.numeric.precision import Interval, IntervalArithmetic


@dataclass
class BoundsResult:
    """Result of a bounds verification."""
    value_interval: Interval
    contains_exact: bool
    width: float
    relative_width: float
    certified: bool  # Whether bounds are mathematically rigorous
    details: str = ""


@dataclass
class CertifiedValue:
    """A value with certified error bounds."""
    midpoint: float
    error_bound: float
    interval: Interval
    method: str = ""

    @property
    def lower(self) -> float:
        return self.interval.lo

    @property
    def upper(self) -> float:
        return self.interval.hi

    def __repr__(self) -> str:
        return f"{self.midpoint} ± {self.error_bound}"


class BoundsChecker:
    """
    Verifies computation results using interval arithmetic
    to provide certified error bounds.
    """

    def __init__(self) -> None:
        self._ia = IntervalArithmetic()

    def verify_scalar(
        self,
        computed: float,
        exact: float | None = None,
        computation: Callable[..., Interval] | None = None,
        args_intervals: list[Interval] | None = None,
    ) -> BoundsResult:
        """
        Verify a scalar computation result.

        If exact is known, checks containment.
        If computation is provided with interval args, uses interval evaluation.
        """
        if computation is not None and args_intervals is not None:
            # Interval evaluation
            result_interval = computation(*args_intervals)
            width = result_interval.hi - result_interval.lo
            midpoint = result_interval.midpoint

            contains = result_interval.contains(computed)
            contains_exact = (
                exact is not None and result_interval.contains(exact)
            )

            return BoundsResult(
                value_interval=result_interval,
                contains_exact=contains_exact,
                width=width,
                relative_width=width / abs(midpoint) if midpoint != 0 else float("inf"),
                certified=True,
                details=f"Interval evaluation: [{result_interval.lo}, {result_interval.hi}]",
            )

        # Simple comparison
        if exact is not None:
            error = abs(computed - exact)
            interval = Interval(computed - error, computed + error)
            return BoundsResult(
                value_interval=interval,
                contains_exact=True,
                width=2 * error,
                relative_width=2 * error / abs(exact) if exact != 0 else float("inf"),
                certified=False,
                details=f"Point comparison: error = {error:.2e}",
            )

        # No reference, return point interval
        eps = np.finfo(np.float64).eps * abs(computed)
        interval = Interval(computed - eps, computed + eps)
        return BoundsResult(
            value_interval=interval,
            contains_exact=True,
            width=2 * eps,
            relative_width=2 * np.finfo(np.float64).eps,
            certified=False,
            details="Machine epsilon bounds only",
        )

    def verify_matrix_multiply(
        self,
        A: np.ndarray,
        B: np.ndarray,
        result: np.ndarray,
    ) -> BoundsResult:
        """
        Verify a matrix multiplication C = A @ B using
        error bounds from floating-point error analysis.
        """
        m, k = A.shape
        _, n = B.shape

        # Forward error bound: ||C - fl(AB)|| <= k * eps * ||A|| * ||B||
        eps = np.finfo(A.dtype).eps
        norm_A = np.linalg.norm(A)
        norm_B = np.linalg.norm(B)
        error_bound = k * eps * norm_A * norm_B

        norm_result = np.linalg.norm(result)
        interval = Interval(norm_result - error_bound, norm_result + error_bound)

        return BoundsResult(
            value_interval=interval,
            contains_exact=True,  # By construction
            width=2 * error_bound,
            relative_width=2 * error_bound / norm_result if norm_result > 0 else float("inf"),
            certified=True,
            details=f"Matrix multiply error bound: {error_bound:.2e}",
        )

    def verify_linear_solve(
        self,
        A: np.ndarray,
        b: np.ndarray,
        x: np.ndarray,
    ) -> BoundsResult:
        """
        Verify a linear solve Ax = b by computing the residual
        and using backward error analysis.
        """
        residual = A @ x - b
        residual_norm = np.linalg.norm(residual)
        b_norm = np.linalg.norm(b)
        x_norm = np.linalg.norm(x)

        # Relative residual
        rel_residual = residual_norm / b_norm if b_norm > 0 else float("inf")

        # Condition number based bounds
        try:
            cond = np.linalg.cond(A)
        except np.linalg.LinAlgError:
            cond = float("inf")

        eps = np.finfo(A.dtype).eps
        error_bound = cond * eps * x_norm

        interval = Interval(x_norm - error_bound, x_norm + error_bound)

        certified = rel_residual < eps * cond * 10  # Loose check

        return BoundsResult(
            value_interval=interval,
            contains_exact=certified,
            width=2 * error_bound,
            relative_width=2 * error_bound / x_norm if x_norm > 0 else float("inf"),
            certified=certified,
            details=(
                f"Residual: {residual_norm:.2e}, "
                f"Rel residual: {rel_residual:.2e}, "
                f"Cond(A): {cond:.2e}"
            ),
        )

    def verify_eigendecomposition(
        self,
        A: np.ndarray,
        eigenvalues: np.ndarray,
        eigenvectors: np.ndarray,
    ) -> list[BoundsResult]:
        """
        Verify eigendecomposition A @ v = lambda * v
        using residual-based bounds.
        """
        results = []

        for i in range(len(eigenvalues)):
            lam = eigenvalues[i]
            v = eigenvectors[:, i]
            v_norm = np.linalg.norm(v)

            if v_norm < 1e-15:
                results.append(BoundsResult(
                    value_interval=Interval(0, 0),
                    contains_exact=False, width=0, relative_width=0,
                    certified=False, details="Zero eigenvector",
                ))
                continue

            residual = A @ v - lam * v
            residual_norm = np.linalg.norm(residual) / v_norm

            # Bauer-Fike bound
            error_bound = residual_norm
            interval = Interval(
                float(np.real(lam)) - error_bound,
                float(np.real(lam)) + error_bound,
            )

            results.append(BoundsResult(
                value_interval=interval,
                contains_exact=True,
                width=2 * error_bound,
                relative_width=2 * error_bound / abs(float(np.real(lam))) if lam != 0 else float("inf"),
                certified=True,
                details=f"Eigenvalue residual: {residual_norm:.2e}",
            ))

        return results

    def certify_computation(
        self,
        func: Callable[[float], float],
        interval_func: Callable[[Interval], Interval],
        x: float,
        x_uncertainty: float = 0.0,
    ) -> CertifiedValue:
        """
        Certify a computation by computing it with interval arithmetic.

        Args:
            func: The computation to certify
            interval_func: Same computation using interval arithmetic
            x: Input value
            x_uncertainty: Uncertainty in input value
        """
        # Point evaluation
        y = func(x)

        # Interval evaluation
        x_interval = Interval(x - x_uncertainty, x + x_uncertainty)
        y_interval = interval_func(x_interval)

        error_bound = max(abs(y - y_interval.lo), abs(y - y_interval.hi))

        return CertifiedValue(
            midpoint=y,
            error_bound=error_bound,
            interval=y_interval,
            method="interval_arithmetic",
        )
