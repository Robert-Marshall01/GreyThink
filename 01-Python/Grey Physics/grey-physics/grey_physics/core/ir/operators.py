"""
Grey Physics IR — Differential Operators, PDE/ODE Operators, Boundary/Initial Conditions

Provides the operator infrastructure for expressing and manipulating
partial and ordinary differential equations.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Callable, Dict, List, Optional, Sequence, Tuple, Union
import numpy as np

from grey_physics.core.ir.types import (
    IRNode, Expr, ExprKind, Metadata, Units, NumericTensor,
    Coordinate, DomainType, func,
)


class OperatorKind(Enum):
    """Classification of differential operators."""
    GRADIENT = auto()
    DIVERGENCE = auto()
    CURL = auto()
    LAPLACIAN = auto()
    DALEMBERTIAN = auto()
    COVARIANT_DERIVATIVE = auto()
    LIE_DERIVATIVE = auto()
    EXTERIOR_DERIVATIVE = auto()
    CUSTOM = auto()


class DifferentialOperator(IRNode):
    """A general differential operator acting on fields.

    Can represent ∇, Δ, □, covariant derivative, Lie derivative, etc.
    """

    def __init__(self, name: str, kind: OperatorKind,
                 order: int = 1,
                 symbol: Optional[Expr] = None,
                 coordinates: Optional[List[Coordinate]] = None,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata)
        self.kind = kind
        self.order = order
        self.symbol = symbol or Expr.symbol(name)
        self.coordinates = coordinates or []

    def apply(self, field_expr: Expr) -> Expr:
        """Apply this operator to a symbolic expression."""
        if self.kind == OperatorKind.GRADIENT:
            return self._apply_gradient(field_expr)
        elif self.kind == OperatorKind.LAPLACIAN:
            return self._apply_laplacian(field_expr)
        elif self.kind == OperatorKind.DALEMBERTIAN:
            return self._apply_dalembertian(field_expr)
        else:
            # Generic: represent as operator application
            return Expr(ExprKind.FUNC, name=self.name,
                        children=(field_expr,))

    def _apply_gradient(self, f: Expr) -> Expr:
        """∇f = (∂f/∂x₁, ..., ∂f/∂xₙ)."""
        if not self.coordinates:
            return f.diff(Expr.symbol("x"))
        parts = [f.diff(c.to_expr()) for c in self.coordinates]
        # Return as a function-style expression
        return Expr(ExprKind.FUNC, name="grad", children=tuple(parts))

    def _apply_laplacian(self, f: Expr) -> Expr:
        """Δf = Σ ∂²f/∂x_i²."""
        total = Expr.zero()
        for c in self.coordinates:
            xi = c.to_expr()
            total = total + f.diff(xi).diff(xi)
        return total

    def _apply_dalembertian(self, f: Expr) -> Expr:
        """□f = -∂²f/∂t² + Δf (wave operator, mostly plus signature)."""
        if len(self.coordinates) < 2:
            return f.diff(Expr.symbol("t")).diff(Expr.symbol("t"))
        t = self.coordinates[0].to_expr()
        spatial = self.coordinates[1:]
        result = Expr.constant(-1) * f.diff(t).diff(t)
        for c in spatial:
            xi = c.to_expr()
            result = result + f.diff(xi).diff(xi)
        return result

    def compose(self, other: DifferentialOperator) -> DifferentialOperator:
        """Compose two operators: (A ∘ B)(f) = A(B(f))."""
        return DifferentialOperator(
            f"{self.name}∘{other.name}",
            OperatorKind.CUSTOM,
            self.order + other.order,
            coordinates=self.coordinates or other.coordinates,
        )

    @staticmethod
    def gradient(coords: List[Coordinate]) -> DifferentialOperator:
        return DifferentialOperator("∇", OperatorKind.GRADIENT, 1,
                                   coordinates=coords)

    @staticmethod
    def laplacian(coords: List[Coordinate]) -> DifferentialOperator:
        return DifferentialOperator("Δ", OperatorKind.LAPLACIAN, 2,
                                   coordinates=coords)

    @staticmethod
    def dalembertian(coords: List[Coordinate]) -> DifferentialOperator:
        return DifferentialOperator("□", OperatorKind.DALEMBERTIAN, 2,
                                   coordinates=coords)

    def canonical_form(self) -> str:
        return f"DiffOp({self.name}, order={self.order})"


class PDEType(Enum):
    """Classification of PDEs."""
    ELLIPTIC = auto()
    PARABOLIC = auto()
    HYPERBOLIC = auto()
    MIXED = auto()
    UNKNOWN = auto()


class PDEOperator(IRNode):
    """A partial differential equation operator.

    Represents a PDE of the form F(u, ∂u, ∂²u, ..., x, t) = 0
    where u is the unknown field.
    """

    def __init__(self, name: str, lhs: Expr, rhs: Expr = Expr.zero(),
                 unknown: Optional[Expr] = None,
                 independent_vars: Optional[List[Expr]] = None,
                 pde_type: PDEType = PDEType.UNKNOWN,
                 order: int = 2,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata)
        self.lhs = lhs
        self.rhs = rhs
        self.unknown = unknown or Expr.symbol("u")
        self.independent_vars = independent_vars or []
        self.pde_type = pde_type
        self.order = order
        self._boundary_conditions: List[BoundaryCondition] = []
        self._initial_conditions: List[InitialCondition] = []

    def add_bc(self, bc: BoundaryCondition) -> None:
        self._boundary_conditions.append(bc)

    def add_ic(self, ic: InitialCondition) -> None:
        self._initial_conditions.append(ic)

    @property
    def boundary_conditions(self) -> List[BoundaryCondition]:
        return list(self._boundary_conditions)

    @property
    def initial_conditions(self) -> List[InitialCondition]:
        return list(self._initial_conditions)

    @property
    def residual(self) -> Expr:
        """Residual F = LHS - RHS (should be zero for a solution)."""
        return self.lhs - self.rhs

    @staticmethod
    def wave_equation(dim: int = 1, c: float = 1.0) -> PDEOperator:
        """∂²u/∂t² = c² ∇²u."""
        u = Expr.symbol("u")
        t = Expr.symbol("t")
        c_expr = Expr.constant(c)
        lhs = u.diff(t).diff(t)
        rhs = Expr.zero()
        coords = [Expr.symbol(f"x{i}") for i in range(dim)]
        for x in coords:
            rhs = rhs + c_expr * c_expr * u.diff(x).diff(x)
        return PDEOperator("WaveEq", lhs, rhs, u,
                           [t] + coords, PDEType.HYPERBOLIC, 2)

    @staticmethod
    def heat_equation(dim: int = 1, alpha: float = 1.0) -> PDEOperator:
        """∂u/∂t = α ∇²u."""
        u = Expr.symbol("u")
        t = Expr.symbol("t")
        a = Expr.constant(alpha)
        lhs = u.diff(t)
        rhs = Expr.zero()
        coords = [Expr.symbol(f"x{i}") for i in range(dim)]
        for x in coords:
            rhs = rhs + a * u.diff(x).diff(x)
        return PDEOperator("HeatEq", lhs, rhs, u,
                           [t] + coords, PDEType.PARABOLIC, 2)

    @staticmethod
    def poisson_equation(dim: int = 2) -> PDEOperator:
        """∇²u = f."""
        u = Expr.symbol("u")
        f = Expr.symbol("f")
        lhs = Expr.zero()
        coords = [Expr.symbol(f"x{i}") for i in range(dim)]
        for x in coords:
            lhs = lhs + u.diff(x).diff(x)
        return PDEOperator("PoissonEq", lhs, f, u,
                           coords, PDEType.ELLIPTIC, 2)

    @staticmethod
    def schrodinger_equation(dim: int = 1) -> PDEOperator:
        """iℏ ∂ψ/∂t = -ℏ²/(2m) ∇²ψ + V(x)ψ."""
        psi = Expr.symbol("ψ")
        t = Expr.symbol("t")
        hbar = Expr.symbol("ℏ")
        m = Expr.symbol("m")
        V = Expr.symbol("V")
        i = Expr.imaginary_unit()
        two = Expr.constant(2)

        lhs = i * hbar * psi.diff(t)
        kinetic = Expr.zero()
        coords = [Expr.symbol(f"x{j}") for j in range(dim)]
        for x in coords:
            kinetic = kinetic + psi.diff(x).diff(x)
        kinetic = (Expr.constant(-1) * hbar * hbar / (two * m)) * kinetic
        potential = V * psi
        rhs = kinetic + potential

        return PDEOperator("SchrödingerEq", lhs, rhs, psi,
                           [t] + coords, PDEType.PARABOLIC, 2,
                           metadata=Metadata(domain=DomainType.QUANTUM_MECHANICS))

    @staticmethod
    def navier_stokes_simplified(dim: int = 2) -> PDEOperator:
        """Simplified incompressible Navier-Stokes:
        ∂u/∂t + (u·∇)u = -∇p/ρ + ν∇²u
        (single-component representation)."""
        u = Expr.symbol("u")
        t = Expr.symbol("t")
        p = Expr.symbol("p")
        rho = Expr.symbol("ρ")
        nu = Expr.symbol("ν")
        coords = [Expr.symbol(f"x{i}") for i in range(dim)]
        lhs = u.diff(t)
        for x in coords:
            lhs = lhs + u * u.diff(x)
        rhs = Expr.zero()
        for x in coords:
            rhs = rhs + nu * u.diff(x).diff(x)
        # Pressure gradient
        rhs = rhs - p.diff(coords[0]) / rho
        return PDEOperator("NavierStokes", lhs, rhs, u,
                           [t] + coords, PDEType.PARABOLIC, 2,
                           metadata=Metadata(domain=DomainType.FLUID_DYNAMICS))

    def canonical_form(self) -> str:
        return f"PDE({self.name}, type={self.pde_type.name}, order={self.order})"


class ODEOperator(IRNode):
    """An ordinary differential equation operator.

    Represents an ODE of the form F(y, y', y'', ..., t) = 0.
    """

    def __init__(self, name: str, lhs: Expr, rhs: Expr = Expr.zero(),
                 unknown: Optional[Expr] = None,
                 independent_var: Optional[Expr] = None,
                 order: int = 2,
                 is_autonomous: bool = False,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata)
        self.lhs = lhs
        self.rhs = rhs
        self.unknown = unknown or Expr.symbol("y")
        self.independent_var = independent_var or Expr.symbol("t")
        self.order = order
        self.is_autonomous = is_autonomous
        self._initial_conditions: List[InitialCondition] = []

    def add_ic(self, ic: InitialCondition) -> None:
        self._initial_conditions.append(ic)

    @property
    def initial_conditions(self) -> List[InitialCondition]:
        return list(self._initial_conditions)

    @staticmethod
    def harmonic_oscillator(omega: float = 1.0, damping: float = 0.0) -> ODEOperator:
        """ẍ + 2γẋ + ω²x = 0."""
        x = Expr.symbol("x")
        t = Expr.symbol("t")
        w = Expr.constant(omega)
        g = Expr.constant(damping)
        lhs = x.diff(t).diff(t) + Expr.constant(2) * g * x.diff(t) + w * w * x
        return ODEOperator("SHO", lhs, Expr.zero(), x, t, 2,
                           is_autonomous=True)

    @staticmethod
    def newton_second_law(force_expr: Expr, mass: float = 1.0) -> ODEOperator:
        """m ẍ = F(x, ẋ, t)."""
        x = Expr.symbol("x")
        t = Expr.symbol("t")
        m = Expr.constant(mass)
        lhs = m * x.diff(t).diff(t)
        return ODEOperator("Newton2", lhs, force_expr, x, t, 2)

    def to_first_order_system(self) -> List[Tuple[Expr, Expr]]:
        """Convert high-order ODE to first-order system.

        y'' = f(y, y', t) → { y' = v, v' = f(y, v, t) }.
        """
        if self.order == 1:
            return [(self.unknown.diff(self.independent_var), self.rhs)]
        elif self.order == 2:
            y = self.unknown
            t = self.independent_var
            v = Expr.symbol(f"{y.name}_dot")
            return [
                (y.diff(t), v),
                (v.diff(t), self.rhs),
            ]
        raise NotImplementedError(f"Order {self.order} reduction not yet implemented")

    def canonical_form(self) -> str:
        return f"ODE({self.name}, order={self.order})"


class BCType(Enum):
    """Boundary condition type."""
    DIRICHLET = auto()
    NEUMANN = auto()
    ROBIN = auto()
    PERIODIC = auto()
    CAUCHY = auto()


@dataclass
class BoundaryCondition(IRNode):
    """A boundary condition for a PDE.

    Specifies the value or derivative condition on a boundary.
    """

    def __init__(self, name: str, bc_type: BCType,
                 boundary: str = "∂Ω",
                 value: Optional[Expr] = None,
                 normal_derivative: Optional[Expr] = None,
                 alpha: Optional[Expr] = None,
                 beta: Optional[Expr] = None,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata)
        self.bc_type = bc_type
        self.boundary = boundary
        self.value = value or Expr.zero()
        self.normal_derivative = normal_derivative
        self.alpha = alpha  # Robin: αu + β∂u/∂n = g
        self.beta = beta

    @staticmethod
    def dirichlet(value: Expr, boundary: str = "∂Ω") -> BoundaryCondition:
        return BoundaryCondition("Dirichlet", BCType.DIRICHLET,
                                 boundary, value)

    @staticmethod
    def neumann(normal_deriv: Expr, boundary: str = "∂Ω") -> BoundaryCondition:
        return BoundaryCondition("Neumann", BCType.NEUMANN,
                                 boundary, normal_derivative=normal_deriv)

    @staticmethod
    def periodic(boundary: str = "∂Ω") -> BoundaryCondition:
        return BoundaryCondition("Periodic", BCType.PERIODIC, boundary)

    def canonical_form(self) -> str:
        return f"BC({self.bc_type.name}, on={self.boundary})"


class InitialCondition(IRNode):
    """An initial condition for a time-dependent PDE or ODE."""

    def __init__(self, name: str,
                 field: Optional[Expr] = None,
                 value: Optional[Expr] = None,
                 derivative_value: Optional[Expr] = None,
                 t0: float = 0.0,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata)
        self.field = field or Expr.symbol("u")
        self.value = value or Expr.zero()
        self.derivative_value = derivative_value
        self.t0 = t0

    @staticmethod
    def from_function(fn_expr: Expr, t0: float = 0.0) -> InitialCondition:
        return InitialCondition("IC", value=fn_expr, t0=t0)

    @staticmethod
    def with_velocity(pos_expr: Expr, vel_expr: Expr,
                      t0: float = 0.0) -> InitialCondition:
        return InitialCondition("IC_pv", value=pos_expr,
                                derivative_value=vel_expr, t0=t0)

    def canonical_form(self) -> str:
        return f"IC(t0={self.t0})"
