"""
Grey Physics IR — Symmetry, Noether Charges, Conservation Laws

Implements the connection between continuous symmetries and conserved
quantities via Noether's theorem.
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum, auto
from typing import Any, Dict, List, Optional, Tuple
import numpy as np

from grey_physics.core.ir.types import (
    IRNode, Expr, ExprKind, Metadata, DomainType,
)


class SymmetryType(Enum):
    """Classification of symmetries."""
    TRANSLATION = auto()         # spatial or temporal translation
    ROTATION = auto()            # SO(n) rotation
    BOOST = auto()               # Lorentz boost
    GAUGE = auto()               # internal gauge symmetry
    SCALE = auto()               # dilatation / scale
    DISCRETE = auto()            # parity, time reversal, charge conjugation
    CONFORMAL = auto()           # conformal transformations
    SUPERSYMMETRY = auto()       # SUSY (experimental)
    CUSTOM = auto()


class Symmetry(IRNode):
    """A symmetry transformation of a physical system.

    Encodes the generator, type, and transformation rule.
    Can act on fields, coordinates, and Lagrangians.
    """

    def __init__(self, name: str, sym_type: SymmetryType,
                 generator: Optional[Expr] = None,
                 parameter: Optional[Expr] = None,
                 transformation_rules: Optional[Dict[str, Expr]] = None,
                 is_continuous: bool = True,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata)
        self.sym_type = sym_type
        self.generator = generator or Expr.symbol(f"G_{name}")
        self.parameter = parameter or Expr.symbol("ε")
        self.transformation_rules = transformation_rules or {}
        self.is_continuous = is_continuous

    def transform(self, expr: Expr) -> Expr:
        """Apply the symmetry transformation to an expression.

        For infinitesimal transformations: φ → φ + ε·δφ.
        """
        result = expr
        for old_name, new_expr in self.transformation_rules.items():
            old = Expr.symbol(old_name)
            result = result.substitute(old, new_expr)
        return result

    def infinitesimal_variation(self, field_name: str) -> Expr:
        """Compute δφ = the infinitesimal variation of a field."""
        if field_name in self.transformation_rules:
            original = Expr.symbol(field_name)
            transformed = self.transformation_rules[field_name]
            return transformed - original
        return Expr.zero()

    @staticmethod
    def time_translation() -> Symmetry:
        """t → t + ε (energy conservation)."""
        eps = Expr.symbol("ε")
        t = Expr.symbol("t")
        return Symmetry("time_translation", SymmetryType.TRANSLATION,
                        generator=Expr.symbol("∂/∂t"),
                        parameter=eps,
                        transformation_rules={"t": t + eps})

    @staticmethod
    def space_translation(direction: int = 0) -> Symmetry:
        """x_i → x_i + ε (momentum conservation)."""
        coord = f"x{direction}"
        eps = Expr.symbol("ε")
        x = Expr.symbol(coord)
        return Symmetry(f"translation_{coord}", SymmetryType.TRANSLATION,
                        generator=Expr.symbol(f"∂/∂{coord}"),
                        parameter=eps,
                        transformation_rules={coord: x + eps})

    @staticmethod
    def rotation(plane: Tuple[int, int] = (0, 1)) -> Symmetry:
        """Rotation in the (x_i, x_j) plane (angular momentum conservation)."""
        i, j = plane
        xi, xj = f"x{i}", f"x{j}"
        eps = Expr.symbol("ε")
        x_i = Expr.symbol(xi)
        x_j = Expr.symbol(xj)
        # Infinitesimal: x_i → x_i - ε·x_j, x_j → x_j + ε·x_i
        return Symmetry(f"rotation_{xi}{xj}", SymmetryType.ROTATION,
                        generator=Expr.symbol(f"L_{xi}{xj}"),
                        parameter=eps,
                        transformation_rules={
                            xi: x_i - eps * x_j,
                            xj: x_j + eps * x_i,
                        })

    @staticmethod
    def u1_gauge(field_name: str = "ψ") -> Symmetry:
        """U(1) gauge: ψ → e^{iα}ψ ≈ ψ + iαψ."""
        alpha = Expr.symbol("α")
        psi = Expr.symbol(field_name)
        i = Expr.imaginary_unit()
        return Symmetry("U(1)", SymmetryType.GAUGE,
                        generator=Expr.symbol("Q"),
                        parameter=alpha,
                        transformation_rules={
                            field_name: psi + i * alpha * psi,
                        })

    @staticmethod
    def lorentz_boost(direction: int = 0) -> Symmetry:
        """Lorentz boost along x_direction."""
        coord = f"x{direction}"
        xi = Expr.symbol(coord)
        t = Expr.symbol("t")
        beta = Expr.symbol("β")
        return Symmetry(f"boost_{coord}", SymmetryType.BOOST,
                        parameter=beta,
                        transformation_rules={
                            "t": t - beta * xi,
                            coord: xi - beta * t,
                        })

    def canonical_form(self) -> str:
        return f"Symmetry({self.name}, {self.sym_type.name})"


class NoetherCharge(IRNode):
    """A conserved charge derived from Noether's theorem.

    Given a continuous symmetry and a Lagrangian, Noether's theorem
    produces a conserved current j^μ and a conserved charge Q = ∫ j⁰ d³x.
    """

    def __init__(self, name: str, charge_expr: Expr,
                 current: Optional[List[Expr]] = None,
                 symmetry: Optional[Symmetry] = None,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata)
        self.charge_expr = charge_expr
        self.current = current or []
        self.symmetry = symmetry

    @staticmethod
    def energy(hamiltonian_expr: Expr) -> NoetherCharge:
        """Energy: conserved charge from time translation symmetry."""
        return NoetherCharge("Energy", hamiltonian_expr,
                             symmetry=Symmetry.time_translation())

    @staticmethod
    def momentum(component: int, momentum_expr: Expr) -> NoetherCharge:
        """Linear momentum: conserved from spatial translation."""
        return NoetherCharge(f"p_{component}", momentum_expr,
                             symmetry=Symmetry.space_translation(component))

    @staticmethod
    def angular_momentum(plane: Tuple[int, int],
                         ang_mom_expr: Expr) -> NoetherCharge:
        """Angular momentum: conserved from rotational symmetry."""
        return NoetherCharge(f"L_{plane[0]}{plane[1]}", ang_mom_expr,
                             symmetry=Symmetry.rotation(plane))

    @staticmethod
    def electric_charge(charge_expr: Expr) -> NoetherCharge:
        """Electric charge: conserved from U(1) gauge symmetry."""
        return NoetherCharge("Q_electric", charge_expr,
                             symmetry=Symmetry.u1_gauge())

    def is_conserved_numerically(self, values: np.ndarray,
                                 tolerance: float = 1e-10) -> bool:
        """Check if a time series of charge values is conserved."""
        if len(values) < 2:
            return True
        drift = np.max(np.abs(values - values[0]))
        return drift < tolerance

    def canonical_form(self) -> str:
        return f"NoetherCharge({self.name})"


class ConservationLaw(IRNode):
    """A conservation law: ∂_μ j^μ = 0 (local) or dQ/dt = 0 (global).

    Links a symmetry, a conserved current, and a conserved charge.
    """

    def __init__(self, name: str,
                 symmetry: Optional[Symmetry] = None,
                 charge: Optional[NoetherCharge] = None,
                 continuity_equation: Optional[Expr] = None,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata)
        self.symmetry = symmetry
        self.charge = charge
        self.continuity_equation = continuity_equation

    @staticmethod
    def energy_conservation() -> ConservationLaw:
        """dE/dt = 0 from time translation invariance."""
        E = Expr.symbol("E")
        t = Expr.symbol("t")
        return ConservationLaw(
            "energy_conservation",
            symmetry=Symmetry.time_translation(),
            charge=NoetherCharge.energy(E),
            continuity_equation=E.diff(t),
        )

    @staticmethod
    def momentum_conservation(direction: int = 0) -> ConservationLaw:
        """dp_i/dt = 0 from space translation invariance."""
        p = Expr.symbol(f"p_{direction}")
        t = Expr.symbol("t")
        return ConservationLaw(
            f"momentum_conservation_{direction}",
            symmetry=Symmetry.space_translation(direction),
            charge=NoetherCharge.momentum(direction, p),
            continuity_equation=p.diff(t),
        )

    @staticmethod
    def charge_conservation() -> ConservationLaw:
        """∂_μ j^μ = 0 from U(1) gauge invariance."""
        j = Expr.symbol("j")
        return ConservationLaw(
            "charge_conservation",
            symmetry=Symmetry.u1_gauge(),
            charge=NoetherCharge.electric_charge(Expr.symbol("Q")),
        )

    def verify_numeric(self, time_series: np.ndarray,
                       tolerance: float = 1e-10) -> bool:
        """Verify the conservation law holds numerically over a time series."""
        if self.charge is not None:
            return self.charge.is_conserved_numerically(time_series, tolerance)
        return True

    def canonical_form(self) -> str:
        return f"ConservationLaw({self.name})"
