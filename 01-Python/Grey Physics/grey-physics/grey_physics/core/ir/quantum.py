"""
Grey Physics IR — Quantum Mechanics Types

QuantumState, Observable, OperatorAlgebra, Commutator
for representing quantum-mechanical objects in the IR.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Callable, Dict, List, Optional, Tuple
import numpy as np

from grey_physics.core.ir.types import (
    IRNode, Expr, ExprKind, Metadata, DomainType, commutator, func,
)


class StateType(Enum):
    """Type of quantum state."""
    KET = auto()
    BRA = auto()
    DENSITY_MATRIX = auto()
    WAVEFUNCTION = auto()
    FOCK = auto()
    COHERENT = auto()


class QuantumState(IRNode):
    """A quantum state |ψ⟩ in a Hilbert space.

    Can represent:
      - Abstract kets/bras
      - Wavefunctions ψ(x) on L²(ℝⁿ)
      - Finite-dimensional state vectors
      - Density matrices ρ
    """

    def __init__(self, name: str, state_type: StateType = StateType.KET,
                 dimension: Optional[int] = None,
                 coefficients: Optional[np.ndarray] = None,
                 wavefunction: Optional[Expr] = None,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata or Metadata(
            domain=DomainType.QUANTUM_MECHANICS
        ))
        self.state_type = state_type
        self.dimension = dimension
        self.coefficients = coefficients  # for finite-dim Hilbert spaces
        self.wavefunction = wavefunction  # symbolic ψ(x)

    @staticmethod
    def ket(name: str, dim: Optional[int] = None,
            coeffs: Optional[np.ndarray] = None) -> QuantumState:
        """Create a ket state |name⟩."""
        return QuantumState(name, StateType.KET, dim, coeffs)

    @staticmethod
    def bra(name: str, dim: Optional[int] = None,
            coeffs: Optional[np.ndarray] = None) -> QuantumState:
        """Create a bra state ⟨name|."""
        if coeffs is not None:
            coeffs = np.conj(coeffs)
        return QuantumState(name, StateType.BRA, dim, coeffs)

    @staticmethod
    def from_wavefunction(name: str, psi_expr: Expr) -> QuantumState:
        """Create a state from a symbolic wavefunction ψ(x)."""
        return QuantumState(name, StateType.WAVEFUNCTION,
                            wavefunction=psi_expr)

    @staticmethod
    def basis_state(n: int, dim: int) -> QuantumState:
        """Create the n-th basis state |n⟩ in a dim-dimensional space."""
        coeffs = np.zeros(dim, dtype=complex)
        coeffs[n] = 1.0
        return QuantumState(f"|{n}⟩", StateType.KET, dim, coeffs)

    @staticmethod
    def superposition(states: List[QuantumState],
                      weights: List[complex]) -> QuantumState:
        """Create |ψ⟩ = Σ_i c_i |i⟩."""
        if states[0].coefficients is not None:
            dim = len(states[0].coefficients)
            total = np.zeros(dim, dtype=complex)
            for s, w in zip(states, weights):
                if s.coefficients is not None:
                    total += w * s.coefficients
            return QuantumState("superposition", StateType.KET, dim, total)
        return QuantumState("superposition", StateType.KET)

    @staticmethod
    def coherent(alpha: complex, dim: int = 20) -> QuantumState:
        """Create a coherent state |α⟩ = e^{-|α|²/2} Σ αⁿ/√(n!) |n⟩."""
        coeffs = np.zeros(dim, dtype=complex)
        for n in range(dim):
            from math import factorial, sqrt
            coeffs[n] = (alpha ** n) / sqrt(factorial(n))
        coeffs *= np.exp(-0.5 * abs(alpha) ** 2)
        return QuantumState(f"|α={alpha}⟩", StateType.COHERENT, dim, coeffs)

    def inner_product(self, other: QuantumState) -> complex:
        """Compute ⟨self|other⟩."""
        if self.coefficients is not None and other.coefficients is not None:
            return complex(np.dot(np.conj(self.coefficients), other.coefficients))
        raise ValueError("Cannot compute inner product without coefficients")

    def norm(self) -> float:
        """||ψ|| = √⟨ψ|ψ⟩."""
        if self.coefficients is not None:
            return float(np.linalg.norm(self.coefficients))
        return 1.0

    def normalize(self) -> QuantumState:
        """Return normalized state."""
        if self.coefficients is not None:
            n = self.norm()
            if n > 0:
                return QuantumState(self.name, self.state_type,
                                    self.dimension, self.coefficients / n)
        return self

    def density_matrix(self) -> np.ndarray:
        """Compute ρ = |ψ⟩⟨ψ| for a pure state."""
        if self.coefficients is not None:
            c = self.coefficients.reshape(-1, 1)
            return c @ np.conj(c.T)
        raise ValueError("Need coefficients for density matrix")

    def probabilities(self) -> np.ndarray:
        """Compute |c_i|² for each basis component."""
        if self.coefficients is not None:
            return np.abs(self.coefficients) ** 2
        raise ValueError("Need coefficients")

    def expectation_value(self, operator: Observable) -> complex:
        """Compute ⟨ψ|Â|ψ⟩."""
        if self.coefficients is not None and operator.matrix is not None:
            bra = np.conj(self.coefficients)
            ket = operator.matrix @ self.coefficients
            return complex(bra @ ket)
        raise ValueError("Need both coefficients and operator matrix")

    def canonical_form(self) -> str:
        return f"QuantumState({self.name}, {self.state_type.name})"


class Observable(IRNode):
    """A quantum-mechanical observable (Hermitian operator).

    Can be represented as:
      - A symbolic expression (differential operator on wavefunctions)
      - A finite-dimensional matrix
    """

    def __init__(self, name: str,
                 symbolic: Optional[Expr] = None,
                 matrix: Optional[np.ndarray] = None,
                 is_hermitian: bool = True,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata or Metadata(
            domain=DomainType.QUANTUM_MECHANICS
        ))
        self.symbolic = symbolic or Expr.symbol(name)
        self.matrix = matrix
        self.is_hermitian = is_hermitian

    @staticmethod
    def position(dim: int = 1) -> Observable:
        """Position operator x̂."""
        return Observable("x̂", Expr.symbol("x"))

    @staticmethod
    def momentum(dim: int = 1) -> Observable:
        """Momentum operator p̂ = -iℏ ∂/∂x."""
        hbar = Expr.symbol("ℏ")
        i = Expr.imaginary_unit()
        x = Expr.symbol("x")
        return Observable("p̂", Expr.constant(-1) * i * hbar * Expr.symbol("∂/∂x"))

    @staticmethod
    def hamiltonian_matrix(H: np.ndarray) -> Observable:
        """Create a Hamiltonian observable from a matrix."""
        return Observable("Ĥ", matrix=H)

    @staticmethod
    def pauli_x() -> Observable:
        """Pauli σ_x matrix."""
        return Observable("σ_x", matrix=np.array([[0, 1], [1, 0]], dtype=complex))

    @staticmethod
    def pauli_y() -> Observable:
        """Pauli σ_y matrix."""
        return Observable("σ_y", matrix=np.array([[0, -1j], [1j, 0]], dtype=complex))

    @staticmethod
    def pauli_z() -> Observable:
        """Pauli σ_z matrix."""
        return Observable("σ_z", matrix=np.array([[1, 0], [0, -1]], dtype=complex))

    @staticmethod
    def number_operator(dim: int) -> Observable:
        """Number operator N̂ = â†â for a truncated Fock space."""
        mat = np.diag(np.arange(dim, dtype=complex))
        return Observable("N̂", matrix=mat)

    def eigenvalues(self) -> np.ndarray:
        """Compute eigenvalues of the operator (from matrix representation)."""
        if self.matrix is not None:
            return np.linalg.eigvalsh(self.matrix) if self.is_hermitian else np.linalg.eigvals(self.matrix)
        raise ValueError("Need matrix representation")

    def eigenstates(self) -> Tuple[np.ndarray, List[QuantumState]]:
        """Compute eigenvalues and eigenstates."""
        if self.matrix is not None:
            if self.is_hermitian:
                eigenvalues, eigenvectors = np.linalg.eigh(self.matrix)
            else:
                eigenvalues, eigenvectors = np.linalg.eig(self.matrix)
            states = []
            for i in range(len(eigenvalues)):
                states.append(QuantumState(
                    f"|{self.name}={eigenvalues[i]:.3f}⟩",
                    StateType.KET,
                    len(eigenvalues),
                    eigenvectors[:, i],
                ))
            return eigenvalues, states
        raise ValueError("Need matrix representation")

    def commutator_with(self, other: Observable) -> Observable:
        """Compute [self, other] = self·other - other·self."""
        if self.matrix is not None and other.matrix is not None:
            comm = self.matrix @ other.matrix - other.matrix @ self.matrix
            return Observable(f"[{self.name},{other.name}]", matrix=comm,
                              is_hermitian=False)
        return Observable(f"[{self.name},{other.name}]",
                          symbolic=commutator(self.symbolic, other.symbolic))

    def anticommutator_with(self, other: Observable) -> Observable:
        """Compute {self, other} = self·other + other·self."""
        if self.matrix is not None and other.matrix is not None:
            anti = self.matrix @ other.matrix + other.matrix @ self.matrix
            return Observable(f"{{{self.name},{other.name}}}", matrix=anti)
        raise ValueError("Need matrices for anticommutator")

    def canonical_form(self) -> str:
        return f"Observable({self.name})"


class OperatorAlgebra(IRNode):
    """An algebra of quantum operators with commutation relations.

    Encodes the algebraic structure [A_i, A_j] = c_{ij}^k A_k.
    """

    def __init__(self, name: str,
                 generators: Optional[List[Observable]] = None,
                 commutation_relations: Optional[Dict[Tuple[str, str], Expr]] = None,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata)
        self.generators = generators or []
        self.commutation_relations = commutation_relations or {}

    def add_generator(self, gen: Observable) -> None:
        self.generators.append(gen)

    def set_commutator(self, a_name: str, b_name: str, result: Expr) -> None:
        """Set [A, B] = result."""
        self.commutation_relations[(a_name, b_name)] = result

    def get_commutator(self, a_name: str, b_name: str) -> Optional[Expr]:
        """Look up [A, B]."""
        key = (a_name, b_name)
        if key in self.commutation_relations:
            return self.commutation_relations[key]
        # Try antisymmetry: [B, A] = -[A, B]
        rev_key = (b_name, a_name)
        if rev_key in self.commutation_relations:
            return -self.commutation_relations[rev_key]
        return None

    @staticmethod
    def heisenberg(dim: int = 1) -> OperatorAlgebra:
        """Heisenberg algebra: [x̂, p̂] = iℏ."""
        algebra = OperatorAlgebra("Heisenberg")
        x_hat = Observable.position()
        p_hat = Observable.momentum()
        algebra.add_generator(x_hat)
        algebra.add_generator(p_hat)
        ihbar = Expr.imaginary_unit() * Expr.symbol("ℏ")
        algebra.set_commutator("x̂", "p̂", ihbar)
        return algebra

    @staticmethod
    def angular_momentum() -> OperatorAlgebra:
        """Angular momentum algebra: [L_i, L_j] = iℏε_{ijk}L_k."""
        algebra = OperatorAlgebra("AngularMomentum")
        Lx = Observable("L_x")
        Ly = Observable("L_y")
        Lz = Observable("L_z")
        algebra.add_generator(Lx)
        algebra.add_generator(Ly)
        algebra.add_generator(Lz)
        ihbar = Expr.imaginary_unit() * Expr.symbol("ℏ")
        algebra.set_commutator("L_x", "L_y", ihbar * Expr.symbol("L_z"))
        algebra.set_commutator("L_y", "L_z", ihbar * Expr.symbol("L_x"))
        algebra.set_commutator("L_z", "L_x", ihbar * Expr.symbol("L_y"))
        return algebra

    @staticmethod
    def creation_annihilation() -> OperatorAlgebra:
        """Bosonic creation/annihilation algebra: [â, â†] = 1."""
        algebra = OperatorAlgebra("Fock")
        a = Observable("â")
        a_dag = Observable("â†")
        algebra.add_generator(a)
        algebra.add_generator(a_dag)
        algebra.set_commutator("â", "â†", Expr.constant(1))
        return algebra

    @staticmethod
    def su2() -> OperatorAlgebra:
        """SU(2) algebra from Pauli matrices: [σ_i, σ_j] = 2iε_{ijk}σ_k."""
        algebra = OperatorAlgebra("SU(2)")
        sx = Observable.pauli_x()
        sy = Observable.pauli_y()
        sz = Observable.pauli_z()
        algebra.add_generator(sx)
        algebra.add_generator(sy)
        algebra.add_generator(sz)
        two_i = Expr.constant(2) * Expr.imaginary_unit()
        algebra.set_commutator("σ_x", "σ_y", two_i * Expr.symbol("σ_z"))
        algebra.set_commutator("σ_y", "σ_z", two_i * Expr.symbol("σ_x"))
        algebra.set_commutator("σ_z", "σ_x", two_i * Expr.symbol("σ_y"))
        return algebra

    def canonical_form(self) -> str:
        gens = ",".join(g.name for g in self.generators)
        return f"OperatorAlgebra({self.name},[{gens}])"


class Commutator(IRNode):
    """Explicit commutator [A, B] as an IR node."""

    def __init__(self, a: Observable, b: Observable,
                 result: Optional[Expr] = None,
                 metadata: Optional[Metadata] = None):
        super().__init__(f"[{a.name},{b.name}]", metadata)
        self.a = a
        self.b = b
        self.result = result

    def evaluate_symbolic(self) -> Expr:
        """Evaluate [A, B] symbolically."""
        if self.result is not None:
            return self.result
        return commutator(self.a.symbolic, self.b.symbolic)

    def evaluate_matrix(self) -> Optional[np.ndarray]:
        """Evaluate [A, B] = AB - BA as matrices."""
        if self.a.matrix is not None and self.b.matrix is not None:
            return self.a.matrix @ self.b.matrix - self.b.matrix @ self.a.matrix
        return None

    def canonical_form(self) -> str:
        return f"Commutator([{self.a.name},{self.b.name}])"
