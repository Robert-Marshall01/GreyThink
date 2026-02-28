"""
Grey Physics Math Core — Functional Analysis

Operators on function spaces, spectral theory, resolvents,
semigroups, and Hilbert space infrastructure.
"""

from __future__ import annotations

from typing import Any, Callable, Dict, List, Optional, Tuple, Union
import numpy as np
from scipy.sparse import csr_matrix, diags, eye as speye
from scipy.sparse.linalg import eigsh, eigs, LinearOperator as ScipyLinOp


# ---------------------------------------------------------------------------
# Function spaces
# ---------------------------------------------------------------------------

class FunctionSpace:
    """A discretized function space on a grid.

    Represents L²(Ω) or subspaces thereof on a uniform grid.
    """

    def __init__(self, domain: Tuple[float, float], num_points: int,
                 dimension: int = 1):
        self.domain = domain
        self.num_points = num_points
        self.dimension = dimension
        self.grid = np.linspace(domain[0], domain[1], num_points)
        self.dx = (domain[1] - domain[0]) / (num_points - 1)

    def inner_product(self, f: np.ndarray, g: np.ndarray) -> complex:
        """⟨f, g⟩ = ∫ f̄(x)g(x) dx via trapezoidal rule."""
        integrand = np.conj(f) * g
        return complex(np.trapz(integrand, self.grid))

    def norm(self, f: np.ndarray) -> float:
        """||f|| = √⟨f,f⟩."""
        return np.sqrt(abs(self.inner_product(f, f)))

    def normalize(self, f: np.ndarray) -> np.ndarray:
        """Return f/||f||."""
        n = self.norm(f)
        return f / n if n > 0 else f

    def project(self, f: np.ndarray, basis: List[np.ndarray]) -> np.ndarray:
        """Project f onto span of basis vectors."""
        result = np.zeros_like(f)
        for b in basis:
            coeff = self.inner_product(b, f) / self.inner_product(b, b)
            result += coeff * b
        return result


# ---------------------------------------------------------------------------
# Linear operators on function spaces
# ---------------------------------------------------------------------------

class LinearOperator:
    """A linear operator A: V → V on a function space.

    Can be defined by a matrix, a callable, or a sparse matrix.
    Provides spectral analysis, resolvent, and semigroup operations.
    """

    def __init__(self, name: str,
                 matrix: Optional[np.ndarray] = None,
                 sparse_matrix: Optional[csr_matrix] = None,
                 apply_func: Optional[Callable[[np.ndarray], np.ndarray]] = None,
                 space: Optional[FunctionSpace] = None,
                 is_hermitian: bool = False,
                 is_positive: bool = False):
        self.name = name
        self._matrix = matrix
        self._sparse = sparse_matrix
        self._apply = apply_func
        self.space = space
        self.is_hermitian = is_hermitian
        self.is_positive = is_positive
        self._dim = (matrix.shape[0] if matrix is not None
                     else sparse_matrix.shape[0] if sparse_matrix is not None
                     else None)

    @property
    def dim(self) -> Optional[int]:
        return self._dim

    def apply(self, v: np.ndarray) -> np.ndarray:
        """Apply the operator: Av."""
        if self._matrix is not None:
            return self._matrix @ v
        if self._sparse is not None:
            return self._sparse @ v
        if self._apply is not None:
            return self._apply(v)
        raise ValueError("No representation available")

    def __matmul__(self, v: np.ndarray) -> np.ndarray:
        return self.apply(v)

    def compose(self, other: LinearOperator) -> LinearOperator:
        """Compose operators: (A ∘ B)v = A(B(v))."""
        if self._matrix is not None and other._matrix is not None:
            return LinearOperator(f"{self.name}∘{other.name}",
                                  matrix=self._matrix @ other._matrix,
                                  space=self.space)

        def composed(v: np.ndarray) -> np.ndarray:
            return self.apply(other.apply(v))
        return LinearOperator(f"{self.name}∘{other.name}",
                              apply_func=composed, space=self.space)

    def add(self, other: LinearOperator, alpha: float = 1.0,
            beta: float = 1.0) -> LinearOperator:
        """Compute αA + βB."""
        if self._matrix is not None and other._matrix is not None:
            return LinearOperator(f"{alpha}·{self.name}+{beta}·{other.name}",
                                  matrix=alpha * self._matrix + beta * other._matrix,
                                  space=self.space)

        def added(v: np.ndarray) -> np.ndarray:
            return alpha * self.apply(v) + beta * other.apply(v)
        return LinearOperator(f"{alpha}·{self.name}+{beta}·{other.name}",
                              apply_func=added, space=self.space)

    # ------ Spectral theory ------

    def eigenvalues(self, k: int = 10, which: str = "SM") -> np.ndarray:
        """Compute k eigenvalues.

        which: 'SM' = smallest magnitude, 'LM' = largest magnitude,
               'SR' = smallest real, 'LR' = largest real.
        """
        if self._matrix is not None:
            if self.is_hermitian:
                return np.sort(np.linalg.eigvalsh(self._matrix))[:k]
            return np.sort(np.linalg.eigvals(self._matrix).real)[:k]
        if self._sparse is not None:
            if self.is_hermitian:
                return eigsh(self._sparse, k=min(k, self._dim - 2),
                             which=which, return_eigenvectors=False)
            return eigs(self._sparse, k=min(k, self._dim - 2),
                        which=which, return_eigenvectors=False)
        raise ValueError("Need matrix or sparse representation for eigenvalues")

    def eigenstates(self, k: int = 10, which: str = "SM") -> Tuple[np.ndarray, np.ndarray]:
        """Compute k eigenvalues and eigenvectors.

        Returns (eigenvalues, eigenvectors) where eigenvectors[:,i]
        corresponds to eigenvalues[i].
        """
        if self._matrix is not None:
            if self.is_hermitian:
                vals, vecs = np.linalg.eigh(self._matrix)
                idx = np.argsort(vals)[:k]
                return vals[idx], vecs[:, idx]
            vals, vecs = np.linalg.eig(self._matrix)
            idx = np.argsort(vals.real)[:k]
            return vals[idx], vecs[:, idx]
        if self._sparse is not None:
            if self.is_hermitian:
                return eigsh(self._sparse, k=min(k, self._dim - 2), which=which)
            return eigs(self._sparse, k=min(k, self._dim - 2), which=which)
        raise ValueError("Need matrix or sparse for eigenstates")

    def spectrum(self) -> np.ndarray:
        """Full spectrum (all eigenvalues)."""
        if self._matrix is not None:
            if self.is_hermitian:
                return np.linalg.eigvalsh(self._matrix)
            return np.linalg.eigvals(self._matrix)
        raise ValueError("Full spectrum requires dense matrix")

    def spectral_radius(self) -> float:
        """ρ(A) = max |λ_i|."""
        eigs_val = self.spectrum() if self._matrix is not None else self.eigenvalues(1, "LM")
        return float(np.max(np.abs(eigs_val)))

    def condition_number(self) -> float:
        """Condition number κ(A) = ||A|| · ||A⁻¹||."""
        if self._matrix is not None:
            return float(np.linalg.cond(self._matrix))
        raise ValueError("Need dense matrix for condition number")

    # ------ Resolvent ------

    def resolvent(self, z: complex) -> LinearOperator:
        """R(z; A) = (zI - A)⁻¹."""
        if self._matrix is not None:
            n = self._matrix.shape[0]
            R = np.linalg.inv(z * np.eye(n) - self._matrix)
            return LinearOperator(f"R({z};{self.name})", matrix=R,
                                  space=self.space)
        raise NotImplementedError("Resolvent needs dense matrix")

    def resolvent_set_sample(self, z_values: np.ndarray) -> np.ndarray:
        """Compute ||R(z;A)|| for a grid of z values (resolvent norm)."""
        norms = np.zeros(len(z_values))
        for i, z in enumerate(z_values):
            try:
                R = self.resolvent(z)
                norms[i] = np.linalg.norm(R._matrix, ord=2)
            except np.linalg.LinAlgError:
                norms[i] = np.inf
        return norms

    # ------ Semigroup ------

    def semigroup(self, t: float) -> LinearOperator:
        """Compute e^{tA} (operator semigroup / matrix exponential).

        Uses scipy's matrix exponential for dense matrices.
        """
        from scipy.linalg import expm
        if self._matrix is not None:
            expA = expm(t * self._matrix)
            return LinearOperator(f"exp({t}·{self.name})", matrix=expA,
                                  space=self.space)
        raise NotImplementedError("Semigroup needs dense matrix")

    def semigroup_trajectory(self, v0: np.ndarray,
                             times: np.ndarray) -> np.ndarray:
        """Compute e^{tA}v₀ for multiple t values.

        Returns array of shape (len(times), dim).
        """
        trajectory = np.zeros((len(times), len(v0)), dtype=complex)
        for i, t in enumerate(times):
            S = self.semigroup(t)
            trajectory[i] = S.apply(v0)
        return trajectory

    # ------ Factory methods ------

    @staticmethod
    def laplacian_1d(n: int, dx: float, bc: str = "dirichlet") -> LinearOperator:
        """1D discrete Laplacian Δ = d²/dx².

        Uses second-order finite differences:
        Δf_i ≈ (f_{i+1} - 2f_i + f_{i-1}) / dx²
        """
        diag_main = -2.0 * np.ones(n) / dx ** 2
        diag_off = np.ones(n - 1) / dx ** 2
        if bc == "periodic":
            L = diags([diag_off, diag_main, diag_off], [-1, 0, 1],
                      shape=(n, n), format="csr")
            L = L.tolil()
            L[0, n - 1] = 1.0 / dx ** 2
            L[n - 1, 0] = 1.0 / dx ** 2
            L = L.tocsr()
        else:  # Dirichlet
            L = diags([diag_off, diag_main, diag_off], [-1, 0, 1],
                      shape=(n, n), format="csr")
        return LinearOperator("Δ₁ᴅ", sparse_matrix=L, is_hermitian=True)

    @staticmethod
    def laplacian_2d(nx: int, ny: int, dx: float, dy: float) -> LinearOperator:
        """2D discrete Laplacian using Kronecker products."""
        Lx = LinearOperator.laplacian_1d(nx, dx)
        Ly = LinearOperator.laplacian_1d(ny, dy)
        from scipy.sparse import kron
        Ix = speye(nx)
        Iy = speye(ny)
        L2d = kron(Ix, Ly._sparse) + kron(Lx._sparse, Iy)
        return LinearOperator("Δ₂ᴅ", sparse_matrix=L2d.tocsr(),
                              is_hermitian=True)

    @staticmethod
    def identity(n: int) -> LinearOperator:
        """n×n identity operator."""
        return LinearOperator("I", matrix=np.eye(n), is_hermitian=True,
                              is_positive=True)

    @staticmethod
    def from_potential(V: np.ndarray, dx: float) -> LinearOperator:
        """Schrödinger-type operator: H = -Δ + V(x).

        Constructs -d²/dx² + V as a sparse matrix.
        """
        n = len(V)
        kinetic = LinearOperator.laplacian_1d(n, dx)
        # H = -Δ + V
        H_sparse = -kinetic._sparse + diags(V, 0, shape=(n, n), format="csr")
        return LinearOperator("H", sparse_matrix=H_sparse, is_hermitian=True)


# ---------------------------------------------------------------------------
# Sobolev-type norms
# ---------------------------------------------------------------------------

def sobolev_norm(f: np.ndarray, dx: float, order: int = 1) -> float:
    """Compute the H^s Sobolev norm ||f||_{H^s}.

    ||f||²_{H^s} = Σ_{k=0}^{s} ||f^{(k)}||²_{L²}
    """
    total = np.trapz(np.abs(f) ** 2, dx=dx)
    deriv = f.copy()
    for k in range(1, order + 1):
        deriv = np.gradient(deriv, dx)
        total += np.trapz(np.abs(deriv) ** 2, dx=dx)
    return np.sqrt(total)


# ---------------------------------------------------------------------------
# Green's functions
# ---------------------------------------------------------------------------

def greens_function_1d(
    operator: LinearOperator,
    source_index: int,
    n: int
) -> np.ndarray:
    """Compute the Green's function G(x, x') for a 1D operator.

    Solves AG = δ(x - x') using matrix inversion.
    """
    if operator._matrix is not None:
        A_inv = np.linalg.inv(operator._matrix)
        return A_inv[:, source_index]
    elif operator._sparse is not None:
        from scipy.sparse.linalg import spsolve
        rhs = np.zeros(n)
        rhs[source_index] = 1.0
        return spsolve(operator._sparse, rhs)
    raise ValueError("Need matrix representation")


# ---------------------------------------------------------------------------
# Compact operator approximation
# ---------------------------------------------------------------------------

def singular_values(operator: LinearOperator, k: int = 10) -> np.ndarray:
    """Compute the k largest singular values of an operator.

    For compact operators, singular values → 0.
    """
    if operator._matrix is not None:
        return np.linalg.svd(operator._matrix, compute_uv=False)[:k]
    raise NotImplementedError("SVD needs dense matrix")
