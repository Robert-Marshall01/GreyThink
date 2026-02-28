"""
Experimental Debugger — Diagnostics and visualization for mathematical objects.

Provides:
- Spectral diagnostics (eigenvalue maps, spectral portraits)
- Curvature diagnostics (sectional, Ricci, scalar)
- Geodesic tracing and visualization data
- Stability maps (Lyapunov, spectral abscissa)
- Bifurcation diagrams
- Attractor visualization data
- Stochastic path sampling diagnostics
- PDE solution diagnostics
- Convergence monitors
- Mathematical object inspectors
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Callable, Optional

import numpy as np
from numpy.typing import NDArray


# ─── Diagnostic Result ──────────────────────────────────────────────────────

@dataclass
class DiagnosticResult:
    """A single diagnostic measurement."""
    name: str
    value: Any
    status: str = "ok"    # "ok", "warning", "error"
    details: str = ""
    data: Optional[NDArray] = None

    def __repr__(self) -> str:
        return f"Diag({self.name}: {self.status}, value={self.value})"


@dataclass
class DiagnosticReport:
    """Comprehensive diagnostic report."""
    title: str
    results: list[DiagnosticResult] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)

    def add(self, result: DiagnosticResult) -> None:
        self.results.append(result)

    @property
    def warnings(self) -> list[DiagnosticResult]:
        return [r for r in self.results if r.status == "warning"]

    @property
    def errors(self) -> list[DiagnosticResult]:
        return [r for r in self.results if r.status == "error"]

    @property
    def is_healthy(self) -> bool:
        return len(self.errors) == 0

    def summary(self) -> str:
        lines = [f"=== {self.title} ==="]
        for r in self.results:
            icon = {"ok": "✓", "warning": "⚠", "error": "✗"}.get(r.status, "?")
            lines.append(f"  {icon} {r.name}: {r.value}")
            if r.details:
                lines.append(f"    {r.details}")
        n_ok = sum(1 for r in self.results if r.status == "ok")
        n_warn = len(self.warnings)
        n_err = len(self.errors)
        lines.append(f"  Total: {n_ok} ok, {n_warn} warnings, {n_err} errors")
        return "\n".join(lines)

    def __repr__(self) -> str:
        return f"DiagReport({self.title}, {len(self.results)} checks)"


# ─── Spectral Debugger ──────────────────────────────────────────────────────

class SpectralDebugger:
    """Diagnostics for spectral properties of operators and matrices."""

    @staticmethod
    def eigenvalue_map(A: NDArray) -> DiagnosticResult:
        """Compute and classify eigenvalues."""
        eigs = np.linalg.eigvals(A)
        real_parts = np.real(eigs)
        imag_parts = np.imag(eigs)

        n_positive = int(np.sum(real_parts > 0))
        n_negative = int(np.sum(real_parts < 0))
        n_zero = int(np.sum(np.abs(real_parts) < 1e-10))
        spectral_radius = float(np.max(np.abs(eigs)))

        status = "ok"
        if n_positive > 0 and n_negative > 0:
            status = "warning"
            details = "Eigenvalues on both sides of imaginary axis (saddle-type)"
        elif np.max(np.abs(real_parts)) > 100:
            status = "warning"
            details = "Large eigenvalue magnitudes detected"
        else:
            details = f"{n_positive} positive, {n_negative} negative, {n_zero} zero"

        return DiagnosticResult(
            name="Eigenvalue Map",
            value={"spectral_radius": spectral_radius,
                   "n_positive": n_positive,
                   "n_negative": n_negative},
            status=status,
            details=details,
            data=eigs,
        )

    @staticmethod
    def condition_number(A: NDArray) -> DiagnosticResult:
        """Diagnose the condition number."""
        kappa = float(np.linalg.cond(A))
        if kappa > 1e12:
            status = "error"
            details = "Severely ill-conditioned"
        elif kappa > 1e6:
            status = "warning"
            details = "Moderately ill-conditioned"
        else:
            status = "ok"
            details = "Well-conditioned"

        return DiagnosticResult(
            name="Condition Number",
            value=kappa,
            status=status,
            details=details,
        )

    @staticmethod
    def spectral_gap(A: NDArray) -> DiagnosticResult:
        """Compute the spectral gap (ratio of top two eigenvalues)."""
        eigs = np.sort(np.abs(np.linalg.eigvals(A)))[::-1]
        if len(eigs) < 2 or abs(eigs[0]) < 1e-15:
            return DiagnosticResult(
                name="Spectral Gap", value=0.0,
                status="warning", details="Cannot compute spectral gap",
            )
        gap = float(eigs[0] / (eigs[1] + 1e-30))
        status = "ok" if gap > 1.1 else "warning"
        return DiagnosticResult(
            name="Spectral Gap",
            value=gap,
            status=status,
            details=f"λ₁/λ₂ = {gap:.4f}",
        )

    @staticmethod
    def full_report(A: NDArray) -> DiagnosticReport:
        """Complete spectral diagnostic report."""
        report = DiagnosticReport(title="Spectral Diagnostics")
        report.add(SpectralDebugger.eigenvalue_map(A))
        report.add(SpectralDebugger.condition_number(A))
        report.add(SpectralDebugger.spectral_gap(A))

        # Symmetry check
        sym_err = float(np.linalg.norm(A - A.T))
        report.add(DiagnosticResult(
            name="Symmetry",
            value=sym_err,
            status="ok" if sym_err < 1e-10 else "warning",
            details=f"||A - A^T|| = {sym_err:.2e}",
        ))

        # Positive definiteness
        if sym_err < 1e-10:
            min_eig = float(np.min(np.linalg.eigvalsh(A)))
            report.add(DiagnosticResult(
                name="Positive Definiteness",
                value=min_eig > 0,
                status="ok" if min_eig > 0 else "warning",
                details=f"min eigenvalue = {min_eig:.6e}",
            ))

        return report


# ─── Dynamical Systems Debugger ──────────────────────────────────────────────

class DynamicsDebugger:
    """Diagnostics for dynamical systems."""

    @staticmethod
    def stability_at_fixed_point(
        jacobian: NDArray,
    ) -> DiagnosticResult:
        """Classify stability at a fixed point from its Jacobian."""
        eigs = np.linalg.eigvals(jacobian)
        max_real = float(np.max(np.real(eigs)))

        if max_real < -1e-6:
            status = "ok"
            classification = "asymptotically stable"
        elif abs(max_real) < 1e-6:
            status = "warning"
            classification = "marginally stable (center)"
        else:
            status = "error"
            classification = "unstable"

        return DiagnosticResult(
            name="Fixed Point Stability",
            value=classification,
            status=status,
            details=f"max Re(λ) = {max_real:.6f}",
            data=eigs,
        )

    @staticmethod
    def lyapunov_report(
        spectrum: NDArray,
    ) -> DiagnosticReport:
        """Analyze a Lyapunov exponent spectrum."""
        report = DiagnosticReport(title="Lyapunov Analysis")

        max_lyap = float(np.max(spectrum))
        report.add(DiagnosticResult(
            name="Max Lyapunov Exponent",
            value=max_lyap,
            status="warning" if max_lyap > 0 else "ok",
            details="Chaotic" if max_lyap > 0 else "Non-chaotic",
        ))

        # Lyapunov dimension
        sorted_spec = np.sort(spectrum)[::-1]
        cumsum = np.cumsum(sorted_spec)
        j_vals = np.where(cumsum >= 0)[0]
        if len(j_vals) > 0:
            j = int(j_vals[-1])
            if j + 1 < len(sorted_spec) and abs(sorted_spec[j + 1]) > 1e-15:
                dim = j + cumsum[j] / abs(sorted_spec[j + 1])
            else:
                dim = float(j)
        else:
            dim = 0.0

        report.add(DiagnosticResult(
            name="Kaplan-Yorke Dimension",
            value=dim,
            status="ok",
            details=f"D_KY = {dim:.4f}",
        ))

        # Sum of exponents (should be negative for dissipative systems)
        total = float(np.sum(spectrum))
        report.add(DiagnosticResult(
            name="Sum of Exponents",
            value=total,
            status="ok" if total < 0 else "warning",
            details="Dissipative" if total < 0 else "Conservative or expanding",
        ))

        report.add(DiagnosticResult(
            name="Full Spectrum",
            value=spectrum.tolist(),
            status="ok",
            data=spectrum,
        ))

        return report


# ─── PDE Debugger ───────────────────────────────────────────────────────────

class PDEDebugger:
    """Diagnostics for PDE solutions."""

    @staticmethod
    def conservation_check(
        solution: NDArray,
        dx: float = 1.0,
    ) -> DiagnosticResult:
        """Check if total mass is conserved."""
        mass = np.array([np.sum(u) * dx for u in solution])
        mass_variation = float(np.max(mass) - np.min(mass))
        relative_var = mass_variation / (abs(mass[0]) + 1e-30)

        return DiagnosticResult(
            name="Mass Conservation",
            value=relative_var,
            status="ok" if relative_var < 1e-6 else "warning",
            details=f"Relative mass variation: {relative_var:.2e}",
            data=mass,
        )

    @staticmethod
    def energy_monotonicity(
        solution: NDArray,
        dx: float = 1.0,
    ) -> DiagnosticResult:
        """Check if energy is monotonically decreasing (dissipative PDE)."""
        energy = np.array([np.sum(u ** 2) * dx for u in solution])
        dE = np.diff(energy)
        n_increasing = int(np.sum(dE > 1e-10))

        return DiagnosticResult(
            name="Energy Monotonicity",
            value=n_increasing == 0,
            status="ok" if n_increasing == 0 else "warning",
            details=f"{n_increasing} non-decreasing steps out of {len(dE)}",
            data=energy,
        )

    @staticmethod
    def cfl_check(
        dt: float, dx: float, speed: float,
    ) -> DiagnosticResult:
        """Check CFL condition for explicit schemes."""
        cfl = speed * dt / dx
        return DiagnosticResult(
            name="CFL Condition",
            value=cfl,
            status="ok" if cfl <= 1.0 else "error",
            details=f"CFL = {cfl:.4f} (must be ≤ 1 for stability)",
        )

    @staticmethod
    def convergence_rate(
        errors: list[float],
        dx_values: list[float],
    ) -> DiagnosticResult:
        """Estimate convergence rate from grid refinement study."""
        if len(errors) < 2:
            return DiagnosticResult(
                name="Convergence Rate", value=None,
                status="warning", details="Need at least 2 data points",
            )
        log_e = np.log(np.array(errors))
        log_h = np.log(np.array(dx_values))
        coeffs = np.polyfit(log_h, log_e, 1)
        rate = float(coeffs[0])

        return DiagnosticResult(
            name="Convergence Rate",
            value=rate,
            status="ok" if rate > 0.9 else "warning",
            details=f"Order ≈ {rate:.2f}",
        )


# ─── Convergence Monitor ────────────────────────────────────────────────────

class ConvergenceMonitor:
    """Monitor convergence of iterative algorithms."""

    def __init__(self, name: str = "Iteration", tol: float = 1e-8,
                 max_iter: int = 10000):
        self.name = name
        self.tol = tol
        self.max_iter = max_iter
        self.history: list[float] = []
        self.converged: bool = False

    def record(self, value: float) -> bool:
        """Record a value and check convergence. Returns True if converged."""
        self.history.append(value)
        if len(self.history) > 1:
            change = abs(self.history[-1] - self.history[-2])
            if change < self.tol:
                self.converged = True
                return True
        if len(self.history) >= self.max_iter:
            return True
        return False

    def convergence_rate(self) -> Optional[float]:
        """Estimate asymptotic convergence rate."""
        if len(self.history) < 3:
            return None
        # Ratio of consecutive differences
        diffs = np.abs(np.diff(self.history))
        diffs = diffs[diffs > 1e-30]
        if len(diffs) < 2:
            return None
        ratios = diffs[1:] / diffs[:-1]
        return float(np.median(ratios))

    def report(self) -> DiagnosticReport:
        """Generate convergence report."""
        rep = DiagnosticReport(title=f"Convergence: {self.name}")
        rep.add(DiagnosticResult(
            name="Converged",
            value=self.converged,
            status="ok" if self.converged else "warning",
        ))
        rep.add(DiagnosticResult(
            name="Iterations",
            value=len(self.history),
            status="ok",
        ))
        if self.history:
            rep.add(DiagnosticResult(
                name="Final Value",
                value=self.history[-1],
                status="ok",
            ))
        rate = self.convergence_rate()
        if rate is not None:
            rep.add(DiagnosticResult(
                name="Asymptotic Rate",
                value=rate,
                status="ok" if rate < 1 else "warning",
                details="Linear" if rate < 1 else "Divergent/stagnant",
            ))
        return rep


# ─── Mathematical Object Inspector ──────────────────────────────────────────

class MathInspector:
    """
    Inspect mathematical objects and provide human-readable diagnostics.
    """

    @staticmethod
    def inspect_matrix(A: NDArray) -> DiagnosticReport:
        """Full inspection of a matrix."""
        report = DiagnosticReport(title=f"Matrix Inspection ({A.shape})")

        # Basic properties
        report.add(DiagnosticResult("Shape", A.shape, "ok"))
        report.add(DiagnosticResult("Dtype", str(A.dtype), "ok"))
        report.add(DiagnosticResult("Norm (Frobenius)", float(np.linalg.norm(A, "fro")), "ok"))
        report.add(DiagnosticResult("Rank", int(np.linalg.matrix_rank(A)), "ok"))

        if A.shape[0] == A.shape[1]:
            report.add(DiagnosticResult("Trace", float(np.trace(A)), "ok"))
            try:
                det = float(np.linalg.det(A))
                report.add(DiagnosticResult(
                    "Determinant", det,
                    "ok" if abs(det) > 1e-15 else "warning",
                ))
            except Exception:
                report.add(DiagnosticResult("Determinant", "error", "error"))

            # Add spectral diagnostics
            spectral = SpectralDebugger.full_report(A)
            for r in spectral.results:
                report.add(r)

        # NaN/Inf check
        has_nan = bool(np.any(np.isnan(A)))
        has_inf = bool(np.any(np.isinf(A)))
        if has_nan or has_inf:
            report.add(DiagnosticResult(
                "Numerical Health", "UNHEALTHY", "error",
                details=f"NaN: {has_nan}, Inf: {has_inf}",
            ))
        else:
            report.add(DiagnosticResult("Numerical Health", "HEALTHY", "ok"))

        return report
