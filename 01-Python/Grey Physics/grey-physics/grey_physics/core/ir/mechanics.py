"""
Grey Physics IR — Lagrangian, Hamiltonian, ActionFunctional

Variational mechanics infrastructure: Lagrangians, Hamiltonians,
action functionals, and Legendre transforms.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Callable, Dict, List, Optional, Tuple
import numpy as np

from grey_physics.core.ir.types import (
    IRNode, Expr, ExprKind, Metadata, Units, DomainType, func,
)


class Lagrangian(IRNode):
    """A Lagrangian L(q, q̇, t) or Lagrangian density ℒ(φ, ∂φ, x).

    Encodes the kinetic, potential, and interaction terms symbolically.
    Supports both particle and field Lagrangians.
    """

    def __init__(self, name: str, expr: Expr,
                 generalized_coords: Optional[List[Expr]] = None,
                 velocities: Optional[List[Expr]] = None,
                 fields: Optional[List[Any]] = None,
                 time_var: Optional[Expr] = None,
                 is_density: bool = False,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata or Metadata(
            domain=DomainType.CLASSICAL_MECHANICS
        ))
        self.expr = expr
        self.generalized_coords = generalized_coords or []
        self.velocities = velocities or []
        self.fields = fields or []
        self.time_var = time_var or Expr.symbol("t")
        self.is_density = is_density

    @staticmethod
    def particle(kinetic: Expr, potential: Expr,
                 coords: List[Expr], velocities: List[Expr],
                 name: str = "L") -> Lagrangian:
        """L = T - V for a particle system."""
        return Lagrangian(
            name, kinetic - potential,
            generalized_coords=coords,
            velocities=velocities,
            is_density=False,
        )

    @staticmethod
    def free_particle(mass: float, dim: int = 3) -> Lagrangian:
        """L = ½m(ẋ² + ẏ² + ż²) for a free particle."""
        m = Expr.constant(mass)
        half = Expr.constant(0.5)
        coords = [Expr.symbol(f"q{i}") for i in range(dim)]
        vels = [Expr.symbol(f"q{i}_dot") for i in range(dim)]
        T = Expr.zero()
        for v in vels:
            T = T + half * m * v * v
        return Lagrangian("L_free", T, coords, vels)

    @staticmethod
    def harmonic_oscillator(mass: float = 1.0, omega: float = 1.0,
                            dim: int = 1) -> Lagrangian:
        """L = ½m q̇² - ½mω²q² for a harmonic oscillator."""
        m = Expr.constant(mass)
        w = Expr.constant(omega)
        half = Expr.constant(0.5)
        coords = [Expr.symbol(f"q{i}") for i in range(dim)]
        vels = [Expr.symbol(f"q{i}_dot") for i in range(dim)]
        T = Expr.zero()
        V = Expr.zero()
        for i in range(dim):
            T = T + half * m * vels[i] * vels[i]
            V = V + half * m * w * w * coords[i] * coords[i]
        return Lagrangian("L_SHO", T - V, coords, vels)

    @staticmethod
    def klein_gordon(field: Any, mass: float = 0.0) -> Lagrangian:
        """Klein-Gordon Lagrangian density:
        ℒ = ½(∂_μφ)(∂^μφ) - ½m²φ²
        """
        phi = Expr.symbol(field.name if hasattr(field, 'name') else "φ")
        m = Expr.constant(mass)
        half = Expr.constant(0.5)
        # Symbolic: ½ (∂_μ φ)(∂^μ φ) − ½ m² φ²
        dphi = Expr.symbol(f"∂{phi.name}")  # placeholder for ∂_μφ
        kinetic = half * dphi * dphi
        mass_term = half * m * m * phi * phi
        expr = kinetic - mass_term
        return Lagrangian("ℒ_KG", expr, fields=[field], is_density=True)

    @staticmethod
    def electromagnetic(A_field: Any = None) -> Lagrangian:
        """Maxwell Lagrangian density: ℒ = -¼ F_{μν}F^{μν}."""
        F = Expr.symbol("F_μν")
        quarter = Expr.constant(0.25)
        expr = Expr.constant(-1) * quarter * F * F
        return Lagrangian("ℒ_EM", expr, fields=[A_field] if A_field else [],
                          is_density=True,
                          metadata=Metadata(domain=DomainType.ELECTROMAGNETISM))

    @staticmethod
    def gravitational_einstein_hilbert(spacetime: Any = None) -> Lagrangian:
        """Einstein-Hilbert Lagrangian density: ℒ = (1/16πG) R √(-g)."""
        R = Expr.symbol("R")  # Ricci scalar
        sqrt_neg_g = Expr.symbol("√(-g)")
        G = Expr.symbol("G")
        pi = Expr.constant(np.pi)
        coeff = Expr.constant(1) / (Expr.constant(16) * pi * G)
        expr = coeff * R * sqrt_neg_g
        return Lagrangian("ℒ_EH", expr, is_density=True,
                          metadata=Metadata(domain=DomainType.RELATIVITY))

    def kinetic_energy(self) -> Optional[Expr]:
        """Extract kinetic energy (if Lagrangian is T - V form)."""
        if self.expr.kind == ExprKind.ADD:
            return self.expr.children[0]
        return None

    def potential_energy(self) -> Optional[Expr]:
        """Extract potential energy (if Lagrangian is T - V form)."""
        if self.expr.kind == ExprKind.ADD and len(self.expr.children) > 1:
            v_term = self.expr.children[1]
            if v_term.kind == ExprKind.NEG:
                return v_term.children[0]
            return Expr.constant(-1) * v_term
        return None

    def canonical_form(self) -> str:
        return f"Lagrangian({self.name}, density={self.is_density})"


class Hamiltonian(IRNode):
    """A Hamiltonian H(q, p, t).

    Obtained from a Lagrangian via Legendre transform:
      H = Σ_i p_i q̇_i - L
    """

    def __init__(self, name: str, expr: Expr,
                 generalized_coords: Optional[List[Expr]] = None,
                 momenta: Optional[List[Expr]] = None,
                 time_var: Optional[Expr] = None,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata or Metadata(
            domain=DomainType.CLASSICAL_MECHANICS
        ))
        self.expr = expr
        self.generalized_coords = generalized_coords or []
        self.momenta = momenta or []
        self.time_var = time_var or Expr.symbol("t")

    @staticmethod
    def from_lagrangian(lagrangian: Lagrangian) -> Hamiltonian:
        """Legendre transform: H = Σ p_i q̇_i - L.

        Constructs canonical momenta p_i = ∂L/∂q̇_i and expresses H
        in terms of (q, p).
        """
        coords = lagrangian.generalized_coords
        vels = lagrangian.velocities
        n = len(coords)
        momenta = [Expr.symbol(f"p{i}") for i in range(n)]

        # H = Σ p_i q̇_i - L (symbolic)
        h_expr = Expr.zero()
        for i in range(n):
            h_expr = h_expr + momenta[i] * vels[i]
        h_expr = h_expr - lagrangian.expr

        return Hamiltonian(f"H_from_{lagrangian.name}", h_expr,
                           coords, momenta)

    @staticmethod
    def harmonic_oscillator(mass: float = 1.0, omega: float = 1.0) -> Hamiltonian:
        """H = p²/(2m) + ½mω²q²."""
        q = Expr.symbol("q")
        p = Expr.symbol("p")
        m = Expr.constant(mass)
        w = Expr.constant(omega)
        half = Expr.constant(0.5)
        h = p * p / (Expr.constant(2) * m) + half * m * w * w * q * q
        return Hamiltonian("H_SHO", h, [q], [p])

    @staticmethod
    def kepler(mass: float = 1.0, k: float = 1.0) -> Hamiltonian:
        """H = p²/(2m) - k/|r| (2D polar: r, θ)."""
        r = Expr.symbol("r")
        pr = Expr.symbol("p_r")
        ptheta = Expr.symbol("p_θ")
        m_expr = Expr.constant(mass)
        k_expr = Expr.constant(k)
        two = Expr.constant(2)
        T = (pr * pr + ptheta * ptheta / (r * r)) / (two * m_expr)
        V = Expr.constant(-1) * k_expr / r
        h = T + V
        return Hamiltonian("H_Kepler", h, [r, Expr.symbol("θ")],
                           [pr, ptheta])

    def hamilton_equations(self) -> List[Tuple[Expr, Expr]]:
        """Generate Hamilton's equations of motion:
          dq_i/dt = ∂H/∂p_i
          dp_i/dt = -∂H/∂q_i
        Returns list of (lhs, rhs) pairs.
        """
        eqs: List[Tuple[Expr, Expr]] = []
        for i, (q, p) in enumerate(zip(self.generalized_coords, self.momenta)):
            q_dot = Expr.symbol(f"q{i}_dot")
            p_dot = Expr.symbol(f"p{i}_dot")
            dH_dp = self.expr.diff(p)
            dH_dq = self.expr.diff(q)
            eqs.append((q_dot, dH_dp))
            eqs.append((p_dot, -dH_dq))
        return eqs

    def is_autonomous(self) -> bool:
        """Check if H is time-independent."""
        return self.time_var.name not in self.expr.free_symbols()

    def canonical_form(self) -> str:
        return f"Hamiltonian({self.name})"


class ActionFunctional(IRNode):
    """The action functional S[q] = ∫ L(q, q̇, t) dt.

    The variational principle δS = 0 yields the Euler–Lagrange equations.
    """

    def __init__(self, name: str, lagrangian: Lagrangian,
                 integration_domain: Optional[Tuple[Expr, Expr]] = None,
                 metadata: Optional[Metadata] = None):
        super().__init__(name, metadata)
        self.lagrangian = lagrangian
        self.integration_domain = integration_domain or (
            Expr.symbol("t_i"), Expr.symbol("t_f")
        )

    @property
    def expr(self) -> Expr:
        """S = ∫ L dt."""
        L = self.lagrangian.expr
        t = self.lagrangian.time_var
        return L.integrate(t, self.integration_domain[0],
                           self.integration_domain[1])

    def variation(self) -> Expr:
        """Symbolic variation δS."""
        return Expr.symbol(f"δ{self.name}")

    def euler_lagrange_lhs(self) -> List[Expr]:
        """Compute the left-hand side of Euler-Lagrange equations:
          ∂L/∂q_i - d/dt(∂L/∂q̇_i) = 0

        Returns list of expressions that should each equal zero.
        """
        L = self.lagrangian.expr
        coords = self.lagrangian.generalized_coords
        vels = self.lagrangian.velocities
        t = self.lagrangian.time_var
        eqs: List[Expr] = []
        for q, qdot in zip(coords, vels):
            dL_dq = L.diff(q)
            dL_dqdot = L.diff(qdot)
            d_dt_dL_dqdot = dL_dqdot.diff(t)
            eqs.append(dL_dq - d_dt_dL_dqdot)
        return eqs

    def canonical_form(self) -> str:
        return f"Action({self.name}, L={self.lagrangian.name})"
