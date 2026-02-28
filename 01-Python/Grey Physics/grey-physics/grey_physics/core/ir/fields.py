"""
Grey Physics IR — Field Types

ScalarField, VectorField, TensorField, SpinorField
defined on a background spacetime / manifold.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Callable, Dict, List, Optional, Sequence, Tuple
import numpy as np

from grey_physics.core.ir.types import (
    IRNode, Expr, Metadata, Units, NumericTensor, TensorRank,
    IndexSlot, Coordinate, DomainType, ExprKind,
)


class FieldBase(IRNode):
    """Abstract base for all physical fields."""

    def __init__(self, name: str, spacetime: Any = None,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata)
        self.spacetime = spacetime
        self._symbolic: Optional[Expr] = Expr.symbol(name)
        self._numeric: Optional[NumericTensor] = None

    @property
    def symbolic(self) -> Expr:
        assert self._symbolic is not None
        return self._symbolic

    @symbolic.setter
    def symbolic(self, expr: Expr) -> None:
        self._symbolic = expr

    @property
    def numeric(self) -> Optional[NumericTensor]:
        return self._numeric

    @numeric.setter
    def numeric(self, val: NumericTensor) -> None:
        self._numeric = val


class ScalarField(FieldBase):
    """A scalar field φ(x) on a spacetime manifold.

    Represents a smooth map M → ℝ (or ℂ). Supports both symbolic expression
    and numeric grid data.
    """

    def __init__(self, name: str, spacetime: Any = None,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, spacetime, metadata)
        self.rank = TensorRank(0, 0)

    def canonical_form(self) -> str:
        return f"ScalarField({self.name})"

    def gradient(self) -> VectorField:
        """Compute the gradient ∇φ, returning a covector (1-form) field."""
        grad_name = f"∇{self.name}"
        vf = VectorField(grad_name, self.spacetime, dim=self._get_dim(),
                         metadata=self.metadata)
        if self.spacetime is not None:
            dim = self._get_dim()
            coords = self.spacetime.coordinates
            components = []
            for i in range(dim):
                components.append(self.symbolic.diff(coords[i].to_expr()))
            vf._symbolic_components = components
        return vf

    def laplacian(self) -> ScalarField:
        """Compute Δφ = ∇²φ (flat-space Laplacian)."""
        lap_name = f"Δ{self.name}"
        result = ScalarField(lap_name, self.spacetime, self.metadata)
        if self.spacetime is not None:
            dim = self._get_dim()
            coords = self.spacetime.coordinates
            total = Expr.zero()
            for i in range(dim):
                xi = coords[i].to_expr()
                total = total + self.symbolic.diff(xi).diff(xi)
            result.symbolic = total
        return result

    def evaluate(self, point: np.ndarray) -> complex:
        """Evaluate field at a point (requires numeric data or callable)."""
        if self._numeric is not None:
            # Nearest-grid-point interpolation for simplicity
            idx = tuple(int(round(p)) for p in point)
            return complex(self._numeric.data[idx])
        raise ValueError("No numeric data available; set .numeric first")

    def set_from_function(self, fn: Callable[..., float],
                          grid: np.ndarray) -> None:
        """Initialize numeric data from a callable on a grid."""
        data = np.vectorize(fn)(*[grid[..., i] for i in range(grid.shape[-1])])
        self._numeric = NumericTensor(data)

    def _get_dim(self) -> int:
        if self.spacetime is not None:
            return self.spacetime.dimension
        return 3

    def __repr__(self) -> str:
        return f"ScalarField('{self.name}')"


class VectorField(FieldBase):
    """A vector field V^μ(x) on a spacetime manifold.

    Stores dim symbolic components and optional numeric grid data.
    """

    def __init__(self, name: str, spacetime: Any = None, dim: int = 3,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, spacetime, metadata)
        self.dim = dim
        self.rank = TensorRank(1, 0)
        self._symbolic_components: List[Expr] = [
            Expr.symbol(f"{name}_{i}") for i in range(dim)
        ]
        self._numeric_components: Optional[np.ndarray] = None

    @property
    def components(self) -> List[Expr]:
        return self._symbolic_components

    @components.setter
    def components(self, comps: List[Expr]) -> None:
        if len(comps) != self.dim:
            raise ValueError(f"Expected {self.dim} components, got {len(comps)}")
        self._symbolic_components = comps

    def divergence(self) -> ScalarField:
        """Compute ∇·V = ∂_i V^i (flat-space divergence)."""
        result = ScalarField(f"∇·{self.name}", self.spacetime, self.metadata)
        if self.spacetime is not None:
            coords = self.spacetime.coordinates
            total = Expr.zero()
            for i in range(self.dim):
                total = total + self._symbolic_components[i].diff(
                    coords[i].to_expr()
                )
            result.symbolic = total
        return result

    def curl(self) -> VectorField:
        """Compute ∇×V (only in 3D flat space)."""
        if self.dim != 3:
            raise ValueError("Curl is defined only in 3D")
        result = VectorField(f"∇×{self.name}", self.spacetime, 3, self.metadata)
        if self.spacetime is not None:
            coords = self.spacetime.coordinates
            c = self._symbolic_components
            x, y, z = [coords[i].to_expr() for i in range(3)]
            result._symbolic_components = [
                c[2].diff(y) - c[1].diff(z),
                c[0].diff(z) - c[2].diff(x),
                c[1].diff(x) - c[0].diff(y),
            ]
        return result

    def dot(self, other: VectorField) -> ScalarField:
        """Flat-space dot product V·W = V^i W_i."""
        result = ScalarField(f"{self.name}·{other.name}", self.spacetime)
        total = Expr.zero()
        for i in range(self.dim):
            total = total + self._symbolic_components[i] * other._symbolic_components[i]
        result.symbolic = total
        return result

    def canonical_form(self) -> str:
        return f"VectorField({self.name}, dim={self.dim})"

    def __repr__(self) -> str:
        return f"VectorField('{self.name}', dim={self.dim})"


class TensorField(FieldBase):
    """A general (p,q)-tensor field T^{μ₁…μₚ}_{ν₁…νq}(x).

    Components stored as a multidimensional symbolic array and optional
    numeric array.
    """

    def __init__(self, name: str, rank: TensorRank, spacetime: Any = None,
                 dim: int = 4, metadata: Optional[Metadata] = None):
        super().__init__(name, spacetime, metadata)
        self.tensor_rank = rank
        self.dim = dim
        total = rank.total
        shape = tuple(dim for _ in range(total))
        # Initialize symbolic components
        self._components: Dict[Tuple[int, ...], Expr] = {}
        self._init_components(shape)

    def _init_components(self, shape: Tuple[int, ...]) -> None:
        """Create default symbolic components."""
        import itertools
        if not shape:
            self._components[()] = Expr.symbol(self.name)
            return
        for idx in itertools.product(*[range(s) for s in shape]):
            idx_str = "".join(str(i) for i in idx)
            self._components[idx] = Expr.symbol(f"{self.name}_{idx_str}")

    def __getitem__(self, idx: Tuple[int, ...]) -> Expr:
        return self._components[idx]

    def __setitem__(self, idx: Tuple[int, ...], value: Expr) -> None:
        self._components[idx] = value

    def contract(self, upper_idx: int, lower_idx: int) -> TensorField:
        """Contract an upper and lower index to produce a lower-rank tensor."""
        new_rank = TensorRank(
            self.tensor_rank.contravariant - 1,
            self.tensor_rank.covariant - 1
        )
        result = TensorField(f"tr({self.name})", new_rank,
                             self.spacetime, self.dim, self.metadata)
        import itertools
        total_new = new_rank.total
        if total_new == 0:
            # Full trace
            total = Expr.zero()
            for i in range(self.dim):
                total = total + self._components[(i, i)]
            result._components[()] = total
        else:
            for out_idx in itertools.product(*[range(self.dim)] * total_new):
                total = Expr.zero()
                for k in range(self.dim):
                    full_idx = list(out_idx)
                    full_idx.insert(upper_idx, k)
                    full_idx.insert(lower_idx, k)
                    total = total + self._components[tuple(full_idx)]
                result._components[out_idx] = total
        return result

    def symmetrize(self) -> TensorField:
        """Symmetrize over all indices: T_{(μν…)}."""
        import itertools
        result = TensorField(f"sym({self.name})", self.tensor_rank,
                             self.spacetime, self.dim, self.metadata)
        total_rank = self.tensor_rank.total
        for idx in itertools.product(*[range(self.dim)] * total_rank):
            perms = list(itertools.permutations(idx))
            n = len(perms)
            total = Expr.zero()
            for p in perms:
                total = total + self._components[p]
            result._components[idx] = total / Expr.constant(n)
        return result

    def antisymmetrize(self) -> TensorField:
        """Antisymmetrize over all indices: T_{[μν…]}."""
        import itertools
        result = TensorField(f"asym({self.name})", self.tensor_rank,
                             self.spacetime, self.dim, self.metadata)
        total_rank = self.tensor_rank.total

        def parity(perm: Tuple[int, ...], orig: Tuple[int, ...]) -> int:
            """Compute parity of permutation."""
            lst = list(perm)
            orig_lst = list(orig)
            sign = 1
            for i in range(len(lst)):
                if lst[i] != orig_lst[i]:
                    j = lst.index(orig_lst[i], i)
                    lst[i], lst[j] = lst[j], lst[i]
                    sign *= -1
            return sign

        for idx in itertools.product(*[range(self.dim)] * total_rank):
            perms = list(itertools.permutations(idx))
            n = len(perms)
            total = Expr.zero()
            for p in perms:
                sgn = parity(p, idx)
                total = total + Expr.constant(sgn) * self._components[p]
            result._components[idx] = total / Expr.constant(n)
        return result

    def to_numeric(self) -> NumericTensor:
        """Convert to NumericTensor (requires all components to be numeric constants)."""
        import itertools
        total_rank = self.tensor_rank.total
        shape = tuple(self.dim for _ in range(total_rank)) if total_rank > 0 else (1,)
        data = np.zeros(shape, dtype=complex)
        for idx, expr in self._components.items():
            if expr.kind == ExprKind.CONSTANT and expr.value is not None:
                if total_rank == 0:
                    data[0] = expr.value
                else:
                    data[idx] = expr.value
        indices = ([IndexSlot(f"i{k}", upper=True)
                    for k in range(self.tensor_rank.contravariant)] +
                   [IndexSlot(f"j{k}", upper=False)
                    for k in range(self.tensor_rank.covariant)])
        return NumericTensor(data, indices)

    def canonical_form(self) -> str:
        return f"TensorField({self.name}, rank={self.tensor_rank}, dim={self.dim})"

    def __repr__(self) -> str:
        return f"TensorField('{self.name}', {self.tensor_rank})"


class SpinorField(FieldBase):
    """A spinor field ψ(x) — used in quantum mechanics and field theory.

    Stores a multi-component complex field (Dirac spinor = 4 components in 4D,
    Pauli spinor = 2 components in 3D).
    """

    def __init__(self, name: str, spacetime: Any = None, num_components: int = 2,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, spacetime, metadata)
        self.num_components = num_components
        self._symbolic_components: List[Expr] = [
            Expr.symbol(f"{name}_{i}") for i in range(num_components)
        ]
        self._numeric_data: Optional[np.ndarray] = None

    @property
    def components(self) -> List[Expr]:
        return self._symbolic_components

    @components.setter
    def components(self, comps: List[Expr]) -> None:
        self._symbolic_components = comps

    def conjugate(self) -> SpinorField:
        """Return the Dirac conjugate ψ̄ = ψ†γ⁰."""
        conj = SpinorField(f"{self.name}†", self.spacetime,
                           self.num_components, self.metadata)
        return conj

    def norm_squared(self) -> ScalarField:
        """ψ†ψ = Σ|ψ_i|² — probability density."""
        result = ScalarField(f"|{self.name}|²", self.spacetime, self.metadata)
        total = Expr.zero()
        for comp in self._symbolic_components:
            # |ψ_i|² represented symbolically
            total = total + comp * comp  # simplified: real case
        result.symbolic = total
        return result

    def canonical_form(self) -> str:
        return f"SpinorField({self.name}, components={self.num_components})"

    def __repr__(self) -> str:
        return f"SpinorField('{self.name}', {self.num_components})"
