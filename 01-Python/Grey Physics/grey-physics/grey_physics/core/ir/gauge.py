"""
Grey Physics IR — Gauge Fields and Gauge Transformations

Implements gauge field infrastructure for electromagnetism
and simplified non-Abelian gauge theories.
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum, auto
from typing import Any, Dict, List, Optional, Tuple
import numpy as np

from grey_physics.core.ir.types import (
    IRNode, Expr, ExprKind, Metadata, DomainType, func,
)


class GaugeGroup(Enum):
    """Gauge symmetry group."""
    U1 = auto()       # Electromagnetism
    SU2 = auto()      # Weak force (simplified)
    SU3 = auto()      # Strong force (simplified)
    SO3 = auto()      # Rotation gauge
    CUSTOM = auto()


class GaugeField(IRNode):
    """A gauge field A_μ (connection on a principal bundle).

    For U(1): A_μ is the electromagnetic 4-potential.
    For SU(N): A_μ = A_μ^a T_a (Lie-algebra valued).
    """

    def __init__(self, name: str,
                 gauge_group: GaugeGroup = GaugeGroup.U1,
                 dim: int = 4,
                 num_generators: int = 1,
                 components: Optional[List[Expr]] = None,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata or Metadata(
            domain=DomainType.ELECTROMAGNETISM
        ))
        self.gauge_group = gauge_group
        self.dim = dim
        self.num_generators = num_generators
        self.components = components or [
            Expr.symbol(f"{name}_{mu}") for mu in range(dim)
        ]

    @staticmethod
    def electromagnetic(dim: int = 4) -> GaugeField:
        """U(1) gauge field: the electromagnetic 4-potential A_μ."""
        return GaugeField("A", GaugeGroup.U1, dim, 1)

    @staticmethod
    def yang_mills(group: GaugeGroup = GaugeGroup.SU2,
                   dim: int = 4) -> GaugeField:
        """Non-Abelian gauge field A_μ^a."""
        num_gen = {GaugeGroup.SU2: 3, GaugeGroup.SU3: 8,
                   GaugeGroup.SO3: 3}.get(group, 1)
        return GaugeField("A", group, dim, num_gen)

    def field_strength_tensor(self) -> Dict[Tuple[int, int], Expr]:
        """Compute F_{μν} = ∂_μ A_ν - ∂_ν A_μ (Abelian case).

        For non-Abelian: F_{μν} = ∂_μ A_ν - ∂_ν A_μ + g[A_μ, A_ν]
        (represented symbolically).
        """
        coords = [Expr.symbol(f"x{mu}") for mu in range(self.dim)]
        F: Dict[Tuple[int, int], Expr] = {}
        for mu in range(self.dim):
            for nu in range(self.dim):
                if mu == nu:
                    F[(mu, nu)] = Expr.zero()
                else:
                    dmu_Anu = self.components[nu].diff(coords[mu])
                    dnu_Amu = self.components[mu].diff(coords[nu])
                    f_mn = dmu_Anu - dnu_Amu
                    if self.gauge_group != GaugeGroup.U1:
                        g = Expr.symbol("g_coupling")
                        comm = Expr(ExprKind.COMMUTATOR,
                                    children=(self.components[mu],
                                              self.components[nu]))
                        f_mn = f_mn + g * comm
                    F[(mu, nu)] = f_mn
        return F

    def maxwell_lagrangian(self) -> Expr:
        """Compute ℒ = -¼ F_{μν}F^{μν} (U(1) case)."""
        F = self.field_strength_tensor()
        quarter = Expr.constant(0.25)
        result = Expr.zero()
        # In Minkowski: F^{μν} = η^{μα}η^{νβ}F_{αβ}
        # For simplicity, use flat Minkowski: η = diag(-1,1,1,1)
        eta = [-1.0] + [1.0] * (self.dim - 1)
        for mu in range(self.dim):
            for nu in range(self.dim):
                for alpha in range(self.dim):
                    for beta in range(self.dim):
                        coeff = eta[mu] * eta[nu]  # simplified raise
                        if mu == alpha and nu == beta and abs(coeff) > 1e-15:
                            result = result + Expr.constant(coeff) * F.get((mu, nu), Expr.zero()) * F.get((alpha, beta), Expr.zero())
        return Expr.constant(-1) * quarter * result

    def electric_field(self) -> List[Expr]:
        """E_i = -∂A_0/∂x_i - ∂A_i/∂t (electromagnetic case)."""
        if self.dim < 2:
            return []
        t = Expr.symbol("x0")  # time coordinate
        coords = [Expr.symbol(f"x{i}") for i in range(1, self.dim)]
        E = []
        for i in range(len(coords)):
            Ei = (Expr.constant(-1) * self.components[0].diff(coords[i]) -
                  self.components[i + 1].diff(t))
            E.append(Ei)
        return E

    def magnetic_field(self) -> List[Expr]:
        """B_i = ε_{ijk} ∂_j A_k (3D electromagnetic case)."""
        if self.dim < 4:
            return []
        coords = [Expr.symbol(f"x{i}") for i in range(1, 4)]
        A = self.components[1:4]
        return [
            A[2].diff(coords[1]) - A[1].diff(coords[2]),
            A[0].diff(coords[2]) - A[2].diff(coords[0]),
            A[1].diff(coords[0]) - A[0].diff(coords[1]),
        ]

    def canonical_form(self) -> str:
        return f"GaugeField({self.name}, {self.gauge_group.name})"


class GaugeTransformation(IRNode):
    """A gauge transformation acting on gauge fields and matter fields.

    U(1): A_μ → A_μ + ∂_μ λ,  ψ → e^{iλ} ψ
    SU(N): A_μ → U A_μ U† + (i/g) U(∂_μ U†)
    """

    def __init__(self, name: str,
                 gauge_group: GaugeGroup = GaugeGroup.U1,
                 parameter: Optional[Expr] = None,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata)
        self.gauge_group = gauge_group
        self.parameter = parameter or Expr.symbol("λ")

    def transform_gauge_field(self, field: GaugeField) -> List[Expr]:
        """Apply gauge transformation to the gauge field components."""
        if self.gauge_group == GaugeGroup.U1:
            coords = [Expr.symbol(f"x{mu}") for mu in range(field.dim)]
            return [
                field.components[mu] + self.parameter.diff(coords[mu])
                for mu in range(field.dim)
            ]
        else:
            # Non-Abelian: symbolic representation
            U = Expr.symbol("U")
            U_dag = Expr.symbol("U†")
            g = Expr.symbol("g_coupling")
            return [
                U * field.components[mu] * U_dag
                for mu in range(field.dim)
            ]

    def transform_matter_field(self, field_expr: Expr) -> Expr:
        """Apply gauge transformation to a matter field."""
        if self.gauge_group == GaugeGroup.U1:
            i = Expr.imaginary_unit()
            phase = func("exp", i * self.parameter)
            return phase * field_expr
        else:
            U = Expr.symbol("U")
            return U * field_expr

    @staticmethod
    def lorenz_gauge_condition(field: GaugeField) -> Expr:
        """Lorenz gauge: ∂_μ A^μ = 0."""
        coords = [Expr.symbol(f"x{mu}") for mu in range(field.dim)]
        eta = [-1.0] + [1.0] * (field.dim - 1)
        result = Expr.zero()
        for mu in range(field.dim):
            result = result + Expr.constant(eta[mu]) * field.components[mu].diff(coords[mu])
        return result

    @staticmethod
    def coulomb_gauge_condition(field: GaugeField) -> Expr:
        """Coulomb gauge: ∇·A = 0 (spatial divergence only)."""
        coords = [Expr.symbol(f"x{i}") for i in range(1, field.dim)]
        result = Expr.zero()
        for i, xi in enumerate(coords):
            result = result + field.components[i + 1].diff(xi)
        return result

    def canonical_form(self) -> str:
        return f"GaugeTransformation({self.name}, {self.gauge_group.name})"
