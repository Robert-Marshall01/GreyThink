"""
Numerical stability diagnostics.

Provides:
- Condition number analysis
- Sensitivity analysis
- Error propagation estimation
- Stability classification
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Callable, Optional

import numpy as np
from numpy.typing import NDArray

from greymath.core.metadata import StabilityInfo, StabilityRating
from greymath.core.types import Matrix


@dataclass
class StabilityReport:
    """Comprehensive stability report for a computation."""
    overall_rating: StabilityRating = StabilityRating.UNKNOWN
    condition_number: Optional[float] = None
    relative_forward_error: Optional[float] = None
    backward_error: Optional[float] = None
    warnings: list[str] = field(default_factory=list)
    recommendations: list[str] = field(default_factory=list)

    def __repr__(self) -> str:
        return (f"StabilityReport(rating={self.overall_rating.name}, "
                f"κ={self.condition_number}, warnings={len(self.warnings)})")


class StabilityDiagnostics:
    """
    Numerical stability analysis tools.
    """

    @staticmethod
    def analyze_matrix(A: Matrix) -> StabilityReport:
        """Analyze stability of a matrix (for linear system solving)."""
        if A.data is None:
            return StabilityReport(overall_rating=StabilityRating.UNKNOWN)

        report = StabilityReport()

        # Condition number
        try:
            kappa = float(np.linalg.cond(A.data))
            report.condition_number = kappa

            if kappa < 1e4:
                report.overall_rating = StabilityRating.STABLE
            elif kappa < 1e10:
                report.overall_rating = StabilityRating.CONDITIONALLY_STABLE
                report.warnings.append(
                    f"Moderate condition number κ = {kappa:.2e}. "
                    "Results may lose some digits of accuracy."
                )
            else:
                report.overall_rating = StabilityRating.UNSTABLE
                report.warnings.append(
                    f"Large condition number κ = {kappa:.2e}. "
                    "Results are unreliable in floating-point arithmetic."
                )
                report.recommendations.append(
                    "Consider using arbitrary-precision arithmetic or "
                    "regularization techniques."
                )
        except np.linalg.LinAlgError:
            report.overall_rating = StabilityRating.UNSTABLE
            report.warnings.append("Matrix is singular or near-singular.")

        # Check for near-zero eigenvalues
        try:
            eigenvalues = np.linalg.eigvals(A.data)
            min_eig = np.min(np.abs(eigenvalues))
            if min_eig < 1e-14:
                report.warnings.append(
                    f"Near-zero eigenvalue detected: |λ_min| = {min_eig:.2e}"
                )
        except Exception:
            pass

        return report

    @staticmethod
    def analyze_computation(
        f: Callable[[NDArray], NDArray],
        x: NDArray,
        perturbation: float = 1e-7,
    ) -> StabilityReport:
        """
        Analyze stability of a computation f(x) by measuring sensitivity
        to small perturbations.
        """
        report = StabilityReport()
        x = np.asarray(x, dtype=float)
        fx = f(x)

        # Perturb each component and measure output change
        max_sensitivity = 0.0
        for i in range(len(x)):
            x_pert = x.copy()
            x_pert[i] += perturbation
            fx_pert = f(x_pert)

            rel_input = perturbation / max(abs(x[i]), 1e-30)
            rel_output = np.max(np.abs(fx_pert - fx)) / max(np.max(np.abs(fx)), 1e-30)
            sensitivity = rel_output / rel_input if rel_input > 0 else 0

            max_sensitivity = max(max_sensitivity, sensitivity)

        report.condition_number = max_sensitivity

        if max_sensitivity < 1e2:
            report.overall_rating = StabilityRating.STABLE
        elif max_sensitivity < 1e8:
            report.overall_rating = StabilityRating.CONDITIONALLY_STABLE
            report.warnings.append(
                f"Moderate sensitivity detected: {max_sensitivity:.2e}"
            )
        else:
            report.overall_rating = StabilityRating.UNSTABLE
            report.warnings.append(
                f"High sensitivity detected: {max_sensitivity:.2e}. "
                "Small input perturbations cause large output changes."
            )

        return report

    @staticmethod
    def mixed_precision_recommendation(
        condition_number: float,
    ) -> str:
        """Recommend a precision level based on condition number."""
        if condition_number < 1e4:
            return "float32 is sufficient"
        elif condition_number < 1e8:
            return "float64 recommended"
        elif condition_number < 1e16:
            return "float128 or arbitrary precision recommended"
        else:
            return "arbitrary precision required; consider reformulating the problem"
