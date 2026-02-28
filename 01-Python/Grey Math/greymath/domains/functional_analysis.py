"""
Functional Analysis Module.

Provides:
- Operators on Hilbert and Banach spaces
- Norms, spectral radius, resolvents
- Contraction mappings and fixed-point operators
- Dual spaces and weak topologies
- Compact operator theory
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Callable, Optional

import numpy as np
from numpy.typing import NDArray

from greymath.core.types import (
    LinearOperator, Matrix, Vector, Scalar,
    SpectralObject, OperatorKind, NormType,
)


# ─── Hilbert Space ──────────────────────────────────────────────────────────

@dataclass
class HilbertSpace:
    """
    A Hilbert space: a complete inner-product space.

    Can represent finite-dimensional (R^n, C^n) or infinite-dimensional
    (L^2, ℓ^2) spaces. For infinite-dimensional spaces, operations are
    defined on finite-dimensional approximations.
    """
    name: str = "H"
    dim: Optional[int] = None  # None for infinite-dimensional
    is_real: bool = True
    inner_product: Optional[Callable[[NDArray, NDArray], float]] = None

    def default_inner_product(self, x: NDArray, y: NDArray) -> float:
        """Standard inner product <x, y>."""
        if self.is_real:
            return float(np.dot(x, y))
        return float(np.vdot(x, y))  # conjugate-linear in first arg

    def norm_from_inner_product(self, x: NDArray) -> float:
        """||x|| = sqrt(<x, x>)."""
        ip = self.inner_product or self.default_inner_product
        return float(np.sqrt(ip(x, x)))

    def is_orthogonal(self, x: NDArray, y: NDArray, tol: float = 1e-10) -> bool:
        """Check if <x, y> = 0."""
        ip = self.inner_product or self.default_inner_product
        return abs(ip(x, y)) < tol

    def projection(self, x: NDArray, subspace_basis: NDArray) -> NDArray:
        """Orthogonal projection of x onto a subspace."""
        # Gram-Schmidt-style projection
        Q, _ = np.linalg.qr(subspace_basis)
        return Q @ (Q.T @ x)

    def __repr__(self) -> str:
        dim_str = str(self.dim) if self.dim else "∞"
        return f"HilbertSpace({self.name}, dim={dim_str})"


@dataclass
class BanachSpace:
    """
    A Banach space: a complete normed vector space.
    """
    name: str = "B"
    dim: Optional[int] = None
    norm_fn: Optional[Callable[[NDArray], float]] = None
    norm_type: NormType = NormType.L2

    def norm(self, x: NDArray) -> float:
        if self.norm_fn:
            return self.norm_fn(x)
        if self.norm_type == NormType.L1:
            return float(np.sum(np.abs(x)))
        elif self.norm_type == NormType.L2:
            return float(np.linalg.norm(x))
        elif self.norm_type == NormType.LINF:
            return float(np.max(np.abs(x)))
        return float(np.linalg.norm(x))

    def __repr__(self) -> str:
        return f"BanachSpace({self.name}, norm={self.norm_type.name})"


# ─── Operator Theory ────────────────────────────────────────────────────────

class OperatorTheory:
    """
    Tools for analyzing operators on Hilbert/Banach spaces.
    """

    @staticmethod
    def operator_norm(A: Matrix, p: int = 2) -> float:
        """Compute the operator norm ||A||_op = sup ||Ax|| / ||x||."""
        if A.data is None:
            raise ValueError("Requires numeric matrix")
        return float(np.linalg.norm(A.data, ord=p))

    @staticmethod
    def spectral_radius(A: Matrix) -> float:
        """Compute ρ(A) = max |λ_i|."""
        if A.data is None:
            raise ValueError("Requires numeric matrix")
        eigenvalues = np.linalg.eigvals(A.data)
        return float(np.max(np.abs(eigenvalues)))

    @staticmethod
    def is_contraction(A: Matrix, norm_type: int = 2) -> bool:
        """Check if A is a contraction mapping: ||A|| < 1."""
        return OperatorTheory.operator_norm(A, norm_type) < 1.0

    @staticmethod
    def is_self_adjoint(A: Matrix, tol: float = 1e-10) -> bool:
        """Check if A = A*."""
        if A.data is None:
            return False
        return bool(np.allclose(A.data, A.data.conj().T, atol=tol))

    @staticmethod
    def is_positive_definite(A: Matrix) -> bool:
        """Check if A is positive definite (all eigenvalues > 0)."""
        if A.data is None:
            return False
        try:
            eigenvalues = np.linalg.eigvalsh(A.data)
            return bool(np.all(eigenvalues > 0))
        except np.linalg.LinAlgError:
            return False

    @staticmethod
    def is_unitary(A: Matrix, tol: float = 1e-10) -> bool:
        """Check if A* A = I."""
        if A.data is None:
            return False
        n = A.data.shape[0]
        product = A.data.conj().T @ A.data
        return bool(np.allclose(product, np.eye(n), atol=tol))

    @staticmethod
    def is_normal(A: Matrix, tol: float = 1e-10) -> bool:
        """Check if A*A = AA*."""
        if A.data is None:
            return False
        return bool(np.allclose(
            A.data.conj().T @ A.data,
            A.data @ A.data.conj().T,
            atol=tol,
        ))

    @staticmethod
    def resolvent(A: Matrix, z: complex) -> Matrix:
        """Compute resolvent R(z;A) = (zI - A)^{-1}."""
        if A.data is None:
            raise ValueError("Requires numeric matrix")
        n = A.data.shape[0]
        return Matrix(data=np.linalg.inv(z * np.eye(n) - A.data))

    @staticmethod
    def resolvent_norm(A: Matrix, z: complex) -> float:
        """Compute ||R(z;A)||."""
        R = OperatorTheory.resolvent(A, z)
        return float(np.linalg.norm(R.data, 2))

    @staticmethod
    def commutator(A: Matrix, B: Matrix) -> Matrix:
        """Compute [A, B] = AB - BA."""
        if A.data is None or B.data is None:
            raise ValueError("Requires numeric matrices")
        return Matrix(data=A.data @ B.data - B.data @ A.data)

    @staticmethod
    def anticommutator(A: Matrix, B: Matrix) -> Matrix:
        """Compute {A, B} = AB + BA."""
        if A.data is None or B.data is None:
            raise ValueError("Requires numeric matrices")
        return Matrix(data=A.data @ B.data + B.data @ A.data)


# ─── Fixed-Point Theory ─────────────────────────────────────────────────────

class FixedPointSolver:
    """
    Fixed-point iteration methods.

    Given a contraction mapping T, finds x such that T(x) = x.
    """

    @staticmethod
    def banach_iteration(
        T: Callable[[NDArray], NDArray],
        x0: NDArray,
        tol: float = 1e-10,
        max_iter: int = 10000,
    ) -> tuple[NDArray, int, bool]:
        """
        Banach fixed-point iteration.

        By the Banach fixed-point theorem, if T is a contraction on a
        complete metric space, iteration x_{n+1} = T(x_n) converges
        to the unique fixed point.

        Returns: (fixed_point, iterations, converged)
        """
        x = np.asarray(x0, dtype=float)
        for i in range(max_iter):
            x_new = T(x)
            if np.max(np.abs(x_new - x)) < tol:
                return x_new, i + 1, True
            x = x_new
        return x, max_iter, False

    @staticmethod
    def krasnoselskii_mann(
        T: Callable[[NDArray], NDArray],
        x0: NDArray,
        alpha: float = 0.5,
        tol: float = 1e-10,
        max_iter: int = 10000,
    ) -> tuple[NDArray, int, bool]:
        """
        Krasnoselskii-Mann iteration: x_{n+1} = (1-α)x_n + αT(x_n).

        Converges for nonexpansive mappings in Hilbert spaces.
        """
        x = np.asarray(x0, dtype=float)
        for i in range(max_iter):
            Tx = T(x)
            x_new = (1 - alpha) * x + alpha * Tx
            if np.max(np.abs(x_new - x)) < tol:
                return x_new, i + 1, True
            x = x_new
        return x, max_iter, False
