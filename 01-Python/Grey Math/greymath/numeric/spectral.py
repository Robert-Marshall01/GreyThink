"""
Spectral computation engine.

Provides:
- Eigenvalue/eigenvector computation
- Spectral decomposition
- Spectral radius and condition number
- Resolvent computation
- Spectral gap analysis
"""

from __future__ import annotations

from typing import Optional, Union

import numpy as np
from numpy.typing import NDArray

from greymath.core.types import Matrix, LinearOperator, SpectralObject, Scalar


class SpectralSolver:
    """
    Spectral analysis and decomposition engine.
    """

    @staticmethod
    def full_spectrum(A: Matrix) -> SpectralObject:
        """Compute full spectral decomposition."""
        if A.data is None:
            raise ValueError("Requires numeric matrix")

        if "symmetric" in A.properties or np.allclose(A.data, A.data.T):
            eigenvalues, eigenvectors = np.linalg.eigh(A.data)
        else:
            eigenvalues, eigenvectors = np.linalg.eig(A.data)

        spectral_radius = float(np.max(np.abs(eigenvalues)))
        cond = float(np.linalg.cond(A.data))

        return SpectralObject(
            operator=LinearOperator(matrix_rep=A),
            eigenvalues=eigenvalues,
            eigenvectors=eigenvectors,
            spectral_radius=spectral_radius,
            condition_number=cond,
        )

    @staticmethod
    def spectral_radius(A: Matrix) -> float:
        """Compute spectral radius ρ(A) = max|λ_i|."""
        if A.data is None:
            raise ValueError("Requires numeric matrix")
        eigenvalues = np.linalg.eigvals(A.data)
        return float(np.max(np.abs(eigenvalues)))

    @staticmethod
    def spectral_gap(A: Matrix) -> float:
        """Compute spectral gap: difference between two largest eigenvalue magnitudes."""
        if A.data is None:
            raise ValueError("Requires numeric matrix")
        eigenvalues = np.sort(np.abs(np.linalg.eigvals(A.data)))[::-1]
        if len(eigenvalues) < 2:
            return float(eigenvalues[0])
        return float(eigenvalues[0] - eigenvalues[1])

    @staticmethod
    def resolvent(A: Matrix, z: complex) -> Matrix:
        """Compute resolvent R(z) = (A - zI)^{-1}."""
        if A.data is None:
            raise ValueError("Requires numeric matrix")
        n = A.data.shape[0]
        R = np.linalg.inv(A.data - z * np.eye(n))
        return Matrix(data=R)

    @staticmethod
    def singular_values(A: Matrix) -> NDArray:
        """Compute singular values."""
        if A.data is None:
            raise ValueError("Requires numeric matrix")
        return np.linalg.svd(A.data, compute_uv=False)

    @staticmethod
    def pseudospectrum(A: Matrix, grid_size: int = 100,
                       x_range: tuple[float, float] = (-3, 3),
                       y_range: tuple[float, float] = (-3, 3)) -> tuple[NDArray, NDArray, NDArray]:
        """
        Compute pseudospectrum σ_ε(A) on a grid.

        Returns (X, Y, sigma_min) where sigma_min[i,j] is the
        smallest singular value of (A - z_{ij} I).
        """
        if A.data is None:
            raise ValueError("Requires numeric matrix")

        x = np.linspace(x_range[0], x_range[1], grid_size)
        y = np.linspace(y_range[0], y_range[1], grid_size)
        X, Y = np.meshgrid(x, y)
        n = A.data.shape[0]

        sigma_min = np.zeros_like(X)
        for i in range(grid_size):
            for j in range(grid_size):
                z = complex(X[i, j], Y[i, j])
                M = A.data - z * np.eye(n)
                sigma_min[i, j] = np.min(np.linalg.svd(M, compute_uv=False))

        return X, Y, sigma_min
