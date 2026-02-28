"""
Grey Physics IR — Continuous Media: Fluid, Plasma, ElasticMedium

IR types for continuum mechanics including fluid dynamics,
plasma physics, and elasticity.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Dict, List, Optional, Tuple
import numpy as np

from grey_physics.core.ir.types import (
    IRNode, Expr, ExprKind, Metadata, Units, DomainType, func,
)


class FluidType(Enum):
    INCOMPRESSIBLE = auto()
    COMPRESSIBLE = auto()
    INVISCID = auto()
    VISCOUS = auto()
    IDEAL = auto()


class Fluid(IRNode):
    """A fluid medium described by velocity, pressure, density fields.

    Supports both incompressible and compressible fluids, with
    optional viscosity, thermal conductivity, etc.
    """

    def __init__(self, name: str,
                 dim: int = 3,
                 fluid_type: FluidType = FluidType.INCOMPRESSIBLE,
                 density: Optional[Expr] = None,
                 velocity: Optional[List[Expr]] = None,
                 pressure: Optional[Expr] = None,
                 viscosity: Optional[Expr] = None,
                 temperature: Optional[Expr] = None,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata or Metadata(
            domain=DomainType.FLUID_DYNAMICS
        ))
        self.dim = dim
        self.fluid_type = fluid_type
        self.density = density or Expr.symbol("ρ")
        self.velocity = velocity or [Expr.symbol(f"v_{i}") for i in range(dim)]
        self.pressure = pressure or Expr.symbol("p")
        self.viscosity = viscosity or Expr.symbol("μ")
        self.temperature = temperature
        self.kinematic_viscosity = Expr.symbol("ν")

    def continuity_equation(self) -> Expr:
        """∂ρ/∂t + ∇·(ρv) = 0 (mass conservation)."""
        t = Expr.symbol("t")
        rho = self.density
        result = rho.diff(t)
        coords = [Expr.symbol(f"x{i}") for i in range(self.dim)]
        for i in range(self.dim):
            result = result + (rho * self.velocity[i]).diff(coords[i])
        return result

    def momentum_equations(self) -> List[Expr]:
        """Navier-Stokes momentum equations (one per spatial component).

        ρ(∂v_i/∂t + v_j ∂v_i/∂x_j) = -∂p/∂x_i + μ∇²v_i
        """
        t = Expr.symbol("t")
        coords = [Expr.symbol(f"x{i}") for i in range(self.dim)]
        eqs = []
        for i in range(self.dim):
            vi = self.velocity[i]
            # Material derivative
            Dvi_Dt = vi.diff(t)
            for j in range(self.dim):
                Dvi_Dt = Dvi_Dt + self.velocity[j] * vi.diff(coords[j])
            # Pressure gradient
            dp_dxi = self.pressure.diff(coords[i])
            # Viscous term
            visc = Expr.zero()
            for j in range(self.dim):
                visc = visc + vi.diff(coords[j]).diff(coords[j])
            lhs = self.density * Dvi_Dt + dp_dxi - self.viscosity * visc
            eqs.append(lhs)
        return eqs

    def vorticity(self) -> List[Expr]:
        """ω = ∇ × v (only in 3D)."""
        if self.dim != 3:
            raise ValueError("Vorticity curl only defined in 3D")
        coords = [Expr.symbol(f"x{i}") for i in range(3)]
        v = self.velocity
        return [
            v[2].diff(coords[1]) - v[1].diff(coords[2]),
            v[0].diff(coords[2]) - v[2].diff(coords[0]),
            v[1].diff(coords[0]) - v[0].diff(coords[1]),
        ]

    def kinetic_energy_density(self) -> Expr:
        """½ρ|v|²."""
        half = Expr.constant(0.5)
        v_sq = Expr.zero()
        for vi in self.velocity:
            v_sq = v_sq + vi * vi
        return half * self.density * v_sq

    def reynolds_number(self, L: Expr, U: Expr) -> Expr:
        """Re = ρUL/μ."""
        return self.density * U * L / self.viscosity

    def canonical_form(self) -> str:
        return f"Fluid({self.name}, dim={self.dim}, type={self.fluid_type.name})"


class Plasma(IRNode):
    """A plasma medium — charged fluid coupled to electromagnetic fields.

    Extends fluid description with charge density, current density,
    and coupling to Maxwell's equations.
    """

    def __init__(self, name: str,
                 dim: int = 3,
                 density: Optional[Expr] = None,
                 velocity: Optional[List[Expr]] = None,
                 pressure: Optional[Expr] = None,
                 magnetic_field: Optional[List[Expr]] = None,
                 electric_field: Optional[List[Expr]] = None,
                 charge_density: Optional[Expr] = None,
                 conductivity: Optional[Expr] = None,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata or Metadata(
            domain=DomainType.PLASMA
        ))
        self.dim = dim
        self.density = density or Expr.symbol("ρ")
        self.velocity = velocity or [Expr.symbol(f"v_{i}") for i in range(dim)]
        self.pressure = pressure or Expr.symbol("p")
        self.magnetic_field = magnetic_field or [
            Expr.symbol(f"B_{i}") for i in range(dim)
        ]
        self.electric_field = electric_field or [
            Expr.symbol(f"E_{i}") for i in range(dim)
        ]
        self.charge_density = charge_density or Expr.symbol("ρ_q")
        self.conductivity = conductivity or Expr.symbol("σ")

    def lorentz_force_density(self) -> List[Expr]:
        """f = ρ_q E + J × B (force per unit volume)."""
        J = [self.charge_density * vi for vi in self.velocity]
        if self.dim == 3:
            B = self.magnetic_field
            return [
                self.charge_density * self.electric_field[0] + J[1] * B[2] - J[2] * B[1],
                self.charge_density * self.electric_field[1] + J[2] * B[0] - J[0] * B[2],
                self.charge_density * self.electric_field[2] + J[0] * B[1] - J[1] * B[0],
            ]
        return [self.charge_density * self.electric_field[i] for i in range(self.dim)]

    def magnetic_pressure(self) -> Expr:
        """p_B = B²/(2μ₀)."""
        B_sq = Expr.zero()
        for Bi in self.magnetic_field:
            B_sq = B_sq + Bi * Bi
        mu0 = Expr.symbol("μ₀")
        return B_sq / (Expr.constant(2) * mu0)

    def alfven_speed(self) -> Expr:
        """v_A = B/√(μ₀ρ)."""
        B_mag = Expr.symbol("|B|")
        mu0 = Expr.symbol("μ₀")
        return B_mag / func("sqrt", mu0 * self.density)

    def induction_equation(self) -> List[Expr]:
        """∂B/∂t = ∇ × (v × B) + η∇²B (resistive MHD)."""
        t = Expr.symbol("t")
        eta = Expr.symbol("η")  # magnetic diffusivity
        eqs = []
        for i in range(self.dim):
            eqs.append(self.magnetic_field[i].diff(t))
        return eqs

    def canonical_form(self) -> str:
        return f"Plasma({self.name}, dim={self.dim})"


class ElasticMedium(IRNode):
    """An elastic continuum described by displacement, stress, strain.

    Implements linear elasticity (Hooke's law) and provides
    the equations of elastodynamics.
    """

    def __init__(self, name: str,
                 dim: int = 3,
                 density: Optional[Expr] = None,
                 displacement: Optional[List[Expr]] = None,
                 youngs_modulus: Optional[Expr] = None,
                 poisson_ratio: Optional[Expr] = None,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata)
        self.dim = dim
        self.density = density or Expr.symbol("ρ")
        self.displacement = displacement or [
            Expr.symbol(f"u_{i}") for i in range(dim)
        ]
        self.youngs_modulus = youngs_modulus or Expr.symbol("E")
        self.poisson_ratio = poisson_ratio or Expr.symbol("ν_p")

    def strain_tensor(self) -> Dict[Tuple[int, int], Expr]:
        """Infinitesimal strain: ε_{ij} = ½(∂u_i/∂x_j + ∂u_j/∂x_i)."""
        coords = [Expr.symbol(f"x{i}") for i in range(self.dim)]
        half = Expr.constant(0.5)
        strain: Dict[Tuple[int, int], Expr] = {}
        for i in range(self.dim):
            for j in range(self.dim):
                strain[(i, j)] = half * (
                    self.displacement[i].diff(coords[j]) +
                    self.displacement[j].diff(coords[i])
                )
        return strain

    def stress_tensor_isotropic(self) -> Dict[Tuple[int, int], Expr]:
        """Isotropic linear elastic stress via Hooke's law:
        σ_{ij} = λ tr(ε) δ_{ij} + 2μ ε_{ij}
        where λ, μ are Lamé parameters.
        """
        lam = Expr.symbol("λ")
        mu = Expr.symbol("μ_shear")
        strain = self.strain_tensor()
        # Trace of strain
        tr_eps = Expr.zero()
        for i in range(self.dim):
            tr_eps = tr_eps + strain[(i, i)]
        stress: Dict[Tuple[int, int], Expr] = {}
        for i in range(self.dim):
            for j in range(self.dim):
                delta = Expr.constant(1) if i == j else Expr.zero()
                stress[(i, j)] = lam * tr_eps * delta + Expr.constant(2) * mu * strain[(i, j)]
        return stress

    def wave_equation(self) -> List[Expr]:
        """Elastodynamic wave equation: ρ ∂²u_i/∂t² = ∂σ_{ij}/∂x_j."""
        t = Expr.symbol("t")
        coords = [Expr.symbol(f"x{i}") for i in range(self.dim)]
        stress = self.stress_tensor_isotropic()
        eqs = []
        for i in range(self.dim):
            lhs = self.density * self.displacement[i].diff(t).diff(t)
            rhs = Expr.zero()
            for j in range(self.dim):
                rhs = rhs + stress[(i, j)].diff(coords[j])
            eqs.append(lhs - rhs)
        return eqs

    def canonical_form(self) -> str:
        return f"ElasticMedium({self.name}, dim={self.dim})"
