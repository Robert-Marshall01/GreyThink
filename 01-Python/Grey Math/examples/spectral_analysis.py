"""
Grey Math — Example: Spectral Analysis Workflow.

Demonstrates eigendecomposition, stability analysis, and verification
on a matrix arising from a finite-difference discretization.
"""

import numpy as np

from greymath.numeric.linalg import LinAlg
from greymath.numeric.spectral import SpectralSolver
from greymath.numeric.stability import StabilityDiagnostics
from greymath.verify.bounds import BoundsChecker


def build_laplacian_1d(n: int) -> np.ndarray:
    """Build 1D discrete Laplacian (tridiagonal)."""
    L = np.zeros((n, n))
    for i in range(n):
        L[i, i] = 2.0
        if i > 0:
            L[i, i - 1] = -1.0
        if i < n - 1:
            L[i, i + 1] = -1.0
    return L


def main():
    print("═══ Grey Math: Spectral Analysis Example ═══\n")

    # Build a 10x10 discrete Laplacian
    n = 10
    L = build_laplacian_1d(n)
    print(f"Built {n}x{n} discrete Laplacian matrix")

    # Initialize engines
    la = LinAlg()
    spectral = SpectralSolver()
    stability = StabilityDiagnostics()
    bounds = BoundsChecker()

    # ── Eigendecomposition ─────────────────────────────────────────────────
    print("\n── Eigendecomposition ──")
    eigenvalues, eigenvectors = la.eigh(L)
    print(f"Eigenvalues: {np.round(eigenvalues, 6)}")

    # Analytic eigenvalues: 2 - 2*cos(k*pi/(n+1)), k=1,...,n
    analytic = np.array([2 - 2 * np.cos(k * np.pi / (n + 1)) for k in range(1, n + 1)])
    max_error = np.max(np.abs(eigenvalues - analytic))
    print(f"Max error vs analytic: {max_error:.2e}")

    # ── Spectral Analysis ──────────────────────────────────────────────────
    print("\n── Spectral Data ──")
    spectrum = spectral.full_spectrum(L)
    print(f"Spectral radius: {spectral.spectral_radius(L):.6f}")
    print(f"Spectral gap: {spectral.spectral_gap(L):.6f}")

    # ── Stability Diagnostics ──────────────────────────────────────────────
    print("\n── Stability Analysis ──")
    report = stability.analyze_matrix(L)
    print(f"Condition number: {report.condition_number:.4f}")
    print(f"Well-conditioned: {report.well_conditioned}")
    if report.warnings:
        for w in report.warnings:
            print(f"  ⚠ {w}")
    for r in report.recommendations:
        print(f"  → {r}")

    # ── Bounds Verification ────────────────────────────────────────────────
    print("\n── Bounds Verification ──")
    eig_bounds = bounds.verify_eigendecomposition(L, eigenvalues, eigenvectors)
    for i, b in enumerate(eig_bounds[:3]):
        print(f"  λ_{i}: [{b.value_interval.lo:.8f}, {b.value_interval.hi:.8f}] "
              f"width={b.width:.2e} certified={b.certified}")

    # ── Linear Solve ───────────────────────────────────────────────────────
    print("\n── Linear Solve Verification ──")
    b = np.ones(n)
    x = la.solve(L, b)
    solve_result = bounds.verify_linear_solve(L, b, x)
    print(f"Residual check: {solve_result.details}")
    print(f"Certified: {solve_result.certified}")

    print("\n✓ Spectral analysis complete.")


if __name__ == "__main__":
    main()
