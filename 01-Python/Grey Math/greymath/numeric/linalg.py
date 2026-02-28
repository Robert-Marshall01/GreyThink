"""
Linear algebra backend.

Provides:
- Dense and sparse matrix operations
- Decompositions (LU, QR, SVD, Cholesky, Schur)
- Solvers for linear systems
- Matrix functions (exponential, logarithm, square root)
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Optional, Union

import numpy as np
from numpy.typing import NDArray

from greymath.core.types import Matrix, Vector, Scalar, SpectralObject


class LinAlg:
    """
    Linear algebra operations on Grey Math IR types.

    Wraps numpy/scipy operations with stability diagnostics
    and support for both dense and sparse matrices.
    """

    # ── Basic operations ─────────────────────────────────────────────────

    @staticmethod
    def matmul(A: Matrix, B: Matrix) -> Matrix:
        """Matrix multiplication."""
        if A.data is None or B.data is None:
            raise ValueError("Requires numeric matrices")
        return Matrix(data=A.data @ B.data)

    @staticmethod
    def matvec(A: Matrix, v: Vector) -> Vector:
        """Matrix-vector multiplication."""
        if A.data is None or v.components is None:
            raise ValueError("Requires numeric matrix and vector")
        return Vector(components=A.data @ v.components)

    @staticmethod
    def solve(A: Matrix, b: Vector) -> Vector:
        """Solve Ax = b."""
        if A.data is None or b.components is None:
            raise ValueError("Requires numeric matrix and vector")
        x = np.linalg.solve(A.data, b.components)
        return Vector(components=x)

    @staticmethod
    def lstsq(A: Matrix, b: Vector) -> Vector:
        """Least-squares solution to Ax ≈ b."""
        if A.data is None or b.components is None:
            raise ValueError("Requires numeric matrix and vector")
        x, _, _, _ = np.linalg.lstsq(A.data, b.components, rcond=None)
        return Vector(components=x)

    # ── Decompositions ───────────────────────────────────────────────────

    @staticmethod
    def lu(A: Matrix) -> tuple[Matrix, Matrix, NDArray]:
        """LU decomposition with partial pivoting: PA = LU."""
        from scipy.linalg import lu as scipy_lu
        if A.data is None:
            raise ValueError("Requires numeric matrix")
        P, L, U = scipy_lu(A.data)
        return Matrix(data=L), Matrix(data=U), P

    @staticmethod
    def qr(A: Matrix) -> tuple[Matrix, Matrix]:
        """QR decomposition: A = QR."""
        if A.data is None:
            raise ValueError("Requires numeric matrix")
        Q, R = np.linalg.qr(A.data)
        return Matrix(data=Q, properties={"orthogonal"}), Matrix(data=R)

    @staticmethod
    def svd(A: Matrix) -> tuple[Matrix, NDArray, Matrix]:
        """Singular value decomposition: A = U Σ V^T."""
        if A.data is None:
            raise ValueError("Requires numeric matrix")
        U, s, Vt = np.linalg.svd(A.data, full_matrices=False)
        return Matrix(data=U), s, Matrix(data=Vt)

    @staticmethod
    def cholesky(A: Matrix) -> Matrix:
        """Cholesky decomposition: A = L L^T (A must be positive definite)."""
        if A.data is None:
            raise ValueError("Requires numeric matrix")
        L = np.linalg.cholesky(A.data)
        return Matrix(data=L, properties={"lower_triangular"})

    @staticmethod
    def schur(A: Matrix) -> tuple[Matrix, Matrix]:
        """Schur decomposition: A = Q T Q^T."""
        from scipy.linalg import schur as scipy_schur
        if A.data is None:
            raise ValueError("Requires numeric matrix")
        T, Q = scipy_schur(A.data)
        return Matrix(data=T), Matrix(data=Q, properties={"orthogonal"})

    @staticmethod
    def eig(A: Matrix) -> SpectralObject:
        """Compute eigenvalues and eigenvectors."""
        if A.data is None:
            raise ValueError("Requires numeric matrix")
        eigenvalues, eigenvectors = np.linalg.eig(A.data)
        spectral_radius = float(np.max(np.abs(eigenvalues)))
        condition_number = float(np.linalg.cond(A.data))
        from greymath.core.types import LinearOperator
        return SpectralObject(
            operator=LinearOperator(matrix_rep=A),
            eigenvalues=eigenvalues,
            eigenvectors=eigenvectors,
            spectral_radius=spectral_radius,
            condition_number=condition_number,
        )

    @staticmethod
    def eigh(A: Matrix) -> SpectralObject:
        """Eigendecomposition for symmetric/Hermitian matrices."""
        if A.data is None:
            raise ValueError("Requires numeric matrix")
        eigenvalues, eigenvectors = np.linalg.eigh(A.data)
        spectral_radius = float(np.max(np.abs(eigenvalues)))
        condition_number = float(np.linalg.cond(A.data))
        from greymath.core.types import LinearOperator
        return SpectralObject(
            operator=LinearOperator(matrix_rep=A),
            eigenvalues=eigenvalues,
            eigenvectors=eigenvectors,
            spectral_radius=spectral_radius,
            condition_number=condition_number,
        )

    # ── Matrix functions ─────────────────────────────────────────────────

    @staticmethod
    def expm(A: Matrix) -> Matrix:
        """Matrix exponential exp(A)."""
        from scipy.linalg import expm as scipy_expm
        if A.data is None:
            raise ValueError("Requires numeric matrix")
        return Matrix(data=scipy_expm(A.data))

    @staticmethod
    def logm(A: Matrix) -> Matrix:
        """Matrix logarithm log(A)."""
        from scipy.linalg import logm as scipy_logm
        if A.data is None:
            raise ValueError("Requires numeric matrix")
        return Matrix(data=scipy_logm(A.data))

    @staticmethod
    def sqrtm(A: Matrix) -> Matrix:
        """Matrix square root."""
        from scipy.linalg import sqrtm as scipy_sqrtm
        if A.data is None:
            raise ValueError("Requires numeric matrix")
        result = scipy_sqrtm(A.data)
        return Matrix(data=np.real(result) if np.allclose(result.imag, 0) else result)

    # ── Norms and conditioning ───────────────────────────────────────────

    @staticmethod
    def condition_number(A: Matrix, p: Union[int, float, str] = 2) -> float:
        """Compute condition number κ(A) = ||A|| · ||A^{-1}||."""
        if A.data is None:
            raise ValueError("Requires numeric matrix")
        return float(np.linalg.cond(A.data, p))

    @staticmethod
    def rank(A: Matrix, tol: Optional[float] = None) -> int:
        """Compute numerical rank."""
        if A.data is None:
            raise ValueError("Requires numeric matrix")
        return int(np.linalg.matrix_rank(A.data, tol=tol))

    @staticmethod
    def nullspace(A: Matrix, tol: float = 1e-10) -> Matrix:
        """Compute nullspace basis."""
        if A.data is None:
            raise ValueError("Requires numeric matrix")
        _, s, Vt = np.linalg.svd(A.data)
        null_mask = s < tol
        null_space = Vt[null_mask] if any(null_mask) else np.empty((0, A.cols or 0))
        return Matrix(data=null_space.T)

    # ── Sparse operations ────────────────────────────────────────────────

    @staticmethod
    def sparse_solve(A_sparse: "Any", b: NDArray) -> NDArray:
        """Solve sparse linear system."""
        from scipy.sparse.linalg import spsolve
        return spsolve(A_sparse, b)

    @staticmethod
    def sparse_eigs(A_sparse: "Any", k: int = 6,
                    which: str = "LM") -> tuple[NDArray, NDArray]:
        """Compute k eigenvalues of a sparse matrix."""
        from scipy.sparse.linalg import eigs
        return eigs(A_sparse, k=k, which=which)
