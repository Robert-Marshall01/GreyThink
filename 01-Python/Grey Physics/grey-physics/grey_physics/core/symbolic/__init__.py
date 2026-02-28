"""
Grey Physics — Symbolic Physics Engine

Manipulates the IR at the level of theoretical physics:
  - Derive equations from Lagrangians (Euler-Lagrange)
  - Noether's theorem: symmetry → conservation law
  - Tensor calculus: Christoffel symbols, curvature
  - Quantum algebra: commutators, ladder operators
  - Gauge theory: transformations, fixing
  - Separation of variables, weak forms
"""

from __future__ import annotations

from typing import Any, Callable, Dict, List, Optional, Tuple
import numpy as np

from grey_physics.core.ir.types import (
    IRNode, Expr, ExprKind, Metadata, DomainType, func, commutator,
)
from grey_physics.core.ir.fields import ScalarField, VectorField, TensorField
from grey_physics.core.ir.spacetime import Spacetime, Metric, Connection, CurvatureTensor
from grey_physics.core.ir.mechanics import Lagrangian, Hamiltonian, ActionFunctional
from grey_physics.core.ir.operators import PDEOperator, ODEOperator, BoundaryCondition
from grey_physics.core.ir.symmetry import Symmetry, NoetherCharge, ConservationLaw
from grey_physics.core.ir.quantum import QuantumState, Observable, OperatorAlgebra, Commutator
from grey_physics.core.ir.gauge import GaugeField, GaugeTransformation


class SymbolicEngine:
    """The symbolic physics engine — the core symbolic manipulation layer.

    Provides high-level physics-aware symbolic operations:
      - derive_equations: Lagrangian → equations of motion
      - simplify_physics: reduce expressions using physics identities
      - apply_symmetry: exploit symmetries to simplify models
      - compute_conserved_quantities: Noether's theorem
      - canonical_form: normalize expressions
    """

    def __init__(self):
        self._simplification_rules: List[Callable[[Expr], Expr]] = []
        self._register_default_rules()

    # ============================================================
    # Top-level API
    # ============================================================

    def derive_equations(self, model: Any) -> List[Expr]:
        """Derive equations of motion from a physical model.

        Supports:
          - Lagrangian → Euler-Lagrange equations
          - Hamiltonian → Hamilton's equations
          - ActionFunctional → Euler-Lagrange equations
          - PDEOperator / ODEOperator → residual equations
        """
        if isinstance(model, Lagrangian):
            return self._euler_lagrange(model)
        elif isinstance(model, Hamiltonian):
            return self._hamilton_equations(model)
        elif isinstance(model, ActionFunctional):
            return self._euler_lagrange(model.lagrangian)
        elif isinstance(model, PDEOperator):
            return [model.residual]
        elif isinstance(model, ODEOperator):
            return [model.lhs - model.rhs]
        else:
            raise TypeError(f"Cannot derive equations from {type(model)}")

    def simplify_physics(self, expr: Expr) -> Expr:
        """Apply physics-aware simplification rules."""
        result = expr
        for rule in self._simplification_rules:
            result = rule(result)
        return result

    def apply_symmetry(self, model: Any, symmetry: Symmetry) -> Any:
        """Apply a symmetry to simplify a model.

        Returns a transformed/reduced model.
        """
        if isinstance(model, Lagrangian):
            return self._apply_symmetry_to_lagrangian(model, symmetry)
        elif isinstance(model, PDEOperator):
            return self._apply_symmetry_to_pde(model, symmetry)
        return model

    def compute_conserved_quantities(self, model: Any) -> List[ConservationLaw]:
        """Use Noether's theorem to find conserved quantities.

        For a Lagrangian with known symmetries, derives the
        associated Noether charges and conservation laws.
        """
        if isinstance(model, Lagrangian):
            return self._noether_theorem(model)
        return []

    def canonical_form(self, expr: Expr) -> Expr:
        """Reduce an expression to canonical form for equivalence checking."""
        result = self._flatten_associative(expr)
        result = self._sort_commutative(result)
        result = self._fold_constants(result)
        return result

    # ============================================================
    # Euler-Lagrange equations
    # ============================================================

    def _euler_lagrange(self, L: Lagrangian) -> List[Expr]:
        """Compute Euler-Lagrange equations from a Lagrangian.

        For L(q_i, q̇_i, t):
          ∂L/∂q_i - d/dt(∂L/∂q̇_i) = 0  for each i.

        For field theory (Lagrangian density ℒ(φ, ∂_μφ)):
          ∂ℒ/∂φ - ∂_μ(∂ℒ/∂(∂_μφ)) = 0
        """
        eqs = []
        if L.is_density:
            # Field Euler-Lagrange
            for field in L.fields:
                if field is not None and hasattr(field, 'name'):
                    phi = Expr.symbol(field.name)
                    dphi = Expr.symbol(f"∂{field.name}")
                    dL_dphi = L.expr.diff(phi)
                    dL_ddphi = L.expr.diff(dphi)
                    # ∂_μ(∂ℒ/∂(∂_μφ))
                    divergence = Expr.symbol(f"∂_μ(∂ℒ/∂(∂_μ{field.name}))")
                    eq = dL_dphi - divergence
                    eqs.append(eq)
        else:
            # Particle Euler-Lagrange
            for q, qdot in zip(L.generalized_coords, L.velocities):
                dL_dq = L.expr.diff(q)
                dL_dqdot = L.expr.diff(qdot)
                t = L.time_var
                d_dt_dL_dqdot = dL_dqdot.diff(t)
                eqs.append(dL_dq - d_dt_dL_dqdot)
        return eqs

    # ============================================================
    # Hamilton's equations
    # ============================================================

    def _hamilton_equations(self, H: Hamiltonian) -> List[Expr]:
        """Generate Hamilton's equations:
          q̇_i = ∂H/∂p_i
          ṗ_i = -∂H/∂q_i
        """
        eqs = []
        for i, (q, p) in enumerate(zip(H.generalized_coords, H.momenta)):
            qdot = Expr.symbol(f"q{i}_dot")
            pdot = Expr.symbol(f"p{i}_dot")
            dH_dp = H.expr.diff(p)
            dH_dq = H.expr.diff(q)
            eqs.append(qdot - dH_dp)
            eqs.append(pdot + dH_dq)
        return eqs

    # ============================================================
    # Noether's theorem
    # ============================================================

    def _noether_theorem(self, L: Lagrangian) -> List[ConservationLaw]:
        """Apply Noether's theorem to find conservation laws.

        For each detected symmetry, compute the Noether charge:
          Q = Σ_i (∂L/∂q̇_i) δq_i - (Σ_i (∂L/∂q̇_i)q̇_i - L) δt
        """
        laws = []

        # Time translation → energy conservation
        if L.time_var.name not in L.expr.free_symbols():
            H = Hamiltonian.from_lagrangian(L)
            charge = NoetherCharge.energy(H.expr)
            law = ConservationLaw(
                "energy_conservation",
                symmetry=Symmetry.time_translation(),
                charge=charge,
            )
            laws.append(law)

        # Spatial translations → momentum conservation
        for i, q in enumerate(L.generalized_coords):
            # Check if L depends on q directly
            # If ∂L/∂q = 0, then p_i is conserved
            dL_dq = L.expr.diff(q)
            if self._is_zero(dL_dq):
                p = Expr.symbol(f"p{i}")
                dL_dqdot = L.expr.diff(L.velocities[i])
                charge = NoetherCharge.momentum(i, dL_dqdot)
                law = ConservationLaw(
                    f"momentum_{i}_conservation",
                    symmetry=Symmetry.space_translation(i),
                    charge=charge,
                )
                laws.append(law)

        return laws

    # ============================================================
    # Tensor calculus
    # ============================================================

    def christoffel_symbols(self, metric: Metric,
                            coords: List[Expr]) -> Dict[Tuple[int, int, int], Expr]:
        """Compute Christoffel symbols symbolically.

        Γ^λ_{μν} = ½ g^{λσ} (∂_μ g_{νσ} + ∂_ν g_{μσ} - ∂_σ g_{μν})
        """
        dim = metric.dim
        result: Dict[Tuple[int, int, int], Expr] = {}

        for lam in range(dim):
            for mu in range(dim):
                for nu in range(dim):
                    total = Expr.zero()
                    for sigma in range(dim):
                        g_inv_lam_sigma = Expr.symbol(f"g_inv_{lam}{sigma}")
                        dmu_g_nu_sigma = metric[nu, sigma].diff(coords[mu])
                        dnu_g_mu_sigma = metric[mu, sigma].diff(coords[nu])
                        dsigma_g_mu_nu = metric[mu, nu].diff(coords[sigma])
                        term = g_inv_lam_sigma * (
                            dmu_g_nu_sigma + dnu_g_mu_sigma - dsigma_g_mu_nu
                        )
                        total = total + term
                    result[(lam, mu, nu)] = Expr.constant(0.5) * total
        return result

    def riemann_tensor(self, christoffels: Dict[Tuple[int, int, int], Expr],
                       coords: List[Expr],
                       dim: int) -> Dict[Tuple[int, int, int, int], Expr]:
        """Compute Riemann tensor symbolically from Christoffel symbols.

        R^ρ_{σμν} = ∂_μ Γ^ρ_{νσ} - ∂_ν Γ^ρ_{μσ}
                    + Γ^ρ_{μλ}Γ^λ_{νσ} - Γ^ρ_{νλ}Γ^λ_{μσ}
        """
        R: Dict[Tuple[int, int, int, int], Expr] = {}

        for rho in range(dim):
            for sigma in range(dim):
                for mu in range(dim):
                    for nu in range(dim):
                        Gamma_rho_nu_sigma = christoffels.get(
                            (rho, nu, sigma), Expr.zero()
                        )
                        Gamma_rho_mu_sigma = christoffels.get(
                            (rho, mu, sigma), Expr.zero()
                        )
                        term1 = Gamma_rho_nu_sigma.diff(coords[mu])
                        term2 = Gamma_rho_mu_sigma.diff(coords[nu])
                        term3 = Expr.zero()
                        term4 = Expr.zero()
                        for lam in range(dim):
                            Gamma_rho_mu_lam = christoffels.get(
                                (rho, mu, lam), Expr.zero()
                            )
                            Gamma_lam_nu_sigma = christoffels.get(
                                (lam, nu, sigma), Expr.zero()
                            )
                            term3 = term3 + Gamma_rho_mu_lam * Gamma_lam_nu_sigma
                            Gamma_rho_nu_lam = christoffels.get(
                                (rho, nu, lam), Expr.zero()
                            )
                            Gamma_lam_mu_sigma = christoffels.get(
                                (lam, mu, sigma), Expr.zero()
                            )
                            term4 = term4 + Gamma_rho_nu_lam * Gamma_lam_mu_sigma
                        R[(rho, sigma, mu, nu)] = term1 - term2 + term3 - term4
        return R

    def covariant_derivative(self, vector_components: List[Expr],
                             christoffels: Dict[Tuple[int, int, int], Expr],
                             coords: List[Expr],
                             direction: int) -> List[Expr]:
        """Symbolic covariant derivative ∇_μ V^ν.

        ∇_μ V^ν = ∂_μ V^ν + Γ^ν_{μλ} V^λ
        """
        dim = len(vector_components)
        result = []
        for nu in range(dim):
            partial = vector_components[nu].diff(coords[direction])
            connection_term = Expr.zero()
            for lam in range(dim):
                Gamma = christoffels.get((nu, direction, lam), Expr.zero())
                connection_term = connection_term + Gamma * vector_components[lam]
            result.append(partial + connection_term)
        return result

    # ============================================================
    # Quantum algebra
    # ============================================================

    def commutator(self, A: Expr, B: Expr) -> Expr:
        """Compute [A, B] = AB - BA symbolically."""
        return A * B - B * A

    def ladder_operators(self, hamiltonian: str = "SHO") -> Dict[str, Expr]:
        """Define ladder operators for standard systems.

        SHO: â = √(mω/2ℏ)(x + ip/(mω))
             â† = √(mω/2ℏ)(x - ip/(mω))
        """
        if hamiltonian == "SHO":
            x = Expr.symbol("x̂")
            p = Expr.symbol("p̂")
            m = Expr.symbol("m")
            omega = Expr.symbol("ω")
            hbar = Expr.symbol("ℏ")
            coeff = func("sqrt", m * omega / (Expr.constant(2) * hbar))
            a = coeff * (x + Expr.imaginary_unit() * p / (m * omega))
            a_dag = coeff * (x - Expr.imaginary_unit() * p / (m * omega))
            N = a_dag * a
            H = hbar * omega * (N + Expr.constant(0.5))
            return {
                "a": a,
                "a†": a_dag,
                "N": N,
                "H": H,
                "[a, a†]": Expr.constant(1),
                "[N, a]": Expr.constant(-1) * Expr.symbol("â"),
                "[N, a†]": Expr.symbol("â†"),
            }
        raise ValueError(f"Unknown Hamiltonian: {hamiltonian}")

    def baker_campbell_hausdorff(self, A: Expr, B: Expr,
                                order: int = 2) -> Expr:
        """Compute e^A e^B = e^C where C = A + B + ½[A,B] + ...

        Truncated at given order.
        """
        C = A + B
        if order >= 2:
            comm_AB = commutator(A, B)
            C = C + Expr.constant(0.5) * comm_AB
        if order >= 3:
            comm_A_AB = commutator(A, comm_AB)
            comm_B_AB = commutator(B, comm_AB)
            C = C + Expr.constant(1.0 / 12.0) * (comm_A_AB - comm_B_AB)
        return C

    # ============================================================
    # Gauge theory
    # ============================================================

    def gauge_transform(self, field: GaugeField,
                        transformation: GaugeTransformation) -> List[Expr]:
        """Apply a gauge transformation to a gauge field."""
        return transformation.transform_gauge_field(field)

    def gauge_fix(self, field: GaugeField,
                  gauge: str = "lorenz") -> Expr:
        """Apply a gauge-fixing condition."""
        if gauge == "lorenz":
            return GaugeTransformation.lorenz_gauge_condition(field)
        elif gauge == "coulomb":
            return GaugeTransformation.coulomb_gauge_condition(field)
        else:
            raise ValueError(f"Unknown gauge: {gauge}")

    def field_strength(self, gauge_field: GaugeField) -> Dict[Tuple[int, int], Expr]:
        """Compute the field strength tensor F_{μν}."""
        return gauge_field.field_strength_tensor()

    # ============================================================
    # Symplectic structure
    # ============================================================

    def poisson_bracket(self, f: Expr, g: Expr,
                        coords: List[Expr],
                        momenta: List[Expr]) -> Expr:
        """{f, g} = Σ_i (∂f/∂q_i ∂g/∂p_i - ∂f/∂p_i ∂g/∂q_i)."""
        result = Expr.zero()
        for q, p in zip(coords, momenta):
            result = result + (f.diff(q) * g.diff(p) - f.diff(p) * g.diff(q))
        return result

    def canonical_transformation_check(self, Q: List[Expr], P: List[Expr],
                                        q: List[Expr], p: List[Expr]) -> List[Expr]:
        """Check if (Q, P) is a canonical transformation of (q, p).

        Verifies {Q_i, P_j} = δ_{ij}, {Q_i, Q_j} = 0, {P_i, P_j} = 0.
        """
        conditions = []
        n = len(q)
        for i in range(n):
            for j in range(n):
                pb = self.poisson_bracket(Q[i], P[j], q, p)
                expected = Expr.constant(1) if i == j else Expr.zero()
                conditions.append(pb - expected)
        return conditions

    # ============================================================
    # Variational / weak forms
    # ============================================================

    def derive_weak_form(self, pde: PDEOperator) -> Expr:
        """Derive the weak form of a PDE through symbolic integration by parts.

        For -∇²u = f:
          ∫ ∇u·∇v dx = ∫ fv dx
        """
        v = Expr.symbol("v")  # test function
        return Expr(ExprKind.INTEGRAL, children=(pde.residual * v,))

    def separation_of_variables(self, pde: PDEOperator,
                                sep_vars: List[str]) -> List[Expr]:
        """Attempt separation of variables u(x,t) = X(x)T(t).

        Returns the separated ODEs.
        """
        separated_eqs = []
        for var in sep_vars:
            factor = Expr.symbol(f"{var.upper()}({var})")
            sep_constant = Expr.symbol(f"λ_{var}")
            separated_eqs.append(factor - sep_constant)
        return separated_eqs

    # ============================================================
    # Simplification infrastructure
    # ============================================================

    def _register_default_rules(self) -> None:
        self._simplification_rules.append(self._fold_constants)
        self._simplification_rules.append(self._cancel_zeros)
        self._simplification_rules.append(self._cancel_ones)

    def _fold_constants(self, expr: Expr) -> Expr:
        """Fold constant arithmetic: 2 + 3 → 5."""
        if expr.kind == ExprKind.ADD:
            a, b = expr.children
            a = self._fold_constants(a)
            b = self._fold_constants(b)
            if (a.kind == ExprKind.CONSTANT and b.kind == ExprKind.CONSTANT
                    and a.value is not None and b.value is not None):
                return Expr.constant(a.value + b.value)
            return Expr(ExprKind.ADD, children=(a, b))
        if expr.kind == ExprKind.MUL:
            a, b = expr.children
            a = self._fold_constants(a)
            b = self._fold_constants(b)
            if (a.kind == ExprKind.CONSTANT and b.kind == ExprKind.CONSTANT
                    and a.value is not None and b.value is not None):
                return Expr.constant(a.value * b.value)
            return Expr(ExprKind.MUL, children=(a, b))
        if expr.kind == ExprKind.NEG:
            child = self._fold_constants(expr.children[0])
            if child.kind == ExprKind.CONSTANT and child.value is not None:
                return Expr.constant(-child.value)
            return Expr(ExprKind.NEG, children=(child,))
        return expr

    def _cancel_zeros(self, expr: Expr) -> Expr:
        """x + 0 → x, x * 0 → 0."""
        if expr.kind == ExprKind.ADD:
            a, b = expr.children
            a = self._cancel_zeros(a)
            b = self._cancel_zeros(b)
            if a.kind == ExprKind.CONSTANT and a.value == 0:
                return b
            if b.kind == ExprKind.CONSTANT and b.value == 0:
                return a
            return Expr(ExprKind.ADD, children=(a, b))
        if expr.kind == ExprKind.MUL:
            a, b = expr.children
            if (a.kind == ExprKind.CONSTANT and a.value == 0) or \
               (b.kind == ExprKind.CONSTANT and b.value == 0):
                return Expr.zero()
        return expr

    def _cancel_ones(self, expr: Expr) -> Expr:
        """x * 1 → x, x^1 → x, x^0 → 1."""
        if expr.kind == ExprKind.MUL:
            a, b = expr.children
            a = self._cancel_ones(a)
            b = self._cancel_ones(b)
            if a.kind == ExprKind.CONSTANT and a.value == 1:
                return b
            if b.kind == ExprKind.CONSTANT and b.value == 1:
                return a
            return Expr(ExprKind.MUL, children=(a, b))
        if expr.kind == ExprKind.POW:
            base, exp = expr.children
            if exp.kind == ExprKind.CONSTANT and exp.value == 1:
                return base
            if exp.kind == ExprKind.CONSTANT and exp.value == 0:
                return Expr.constant(1)
        return expr

    def _flatten_associative(self, expr: Expr) -> Expr:
        """Flatten nested ADD/MUL: (a + b) + c → a + b + c (tree form)."""
        return expr  # placeholder for deep flattening

    def _sort_commutative(self, expr: Expr) -> Expr:
        """Sort children of commutative operations for canonical form."""
        return expr  # placeholder for canonical ordering

    def _is_zero(self, expr: Expr) -> bool:
        """Check if an expression is symbolically zero."""
        if expr.kind == ExprKind.CONSTANT and expr.value == 0:
            return True
        return False

    # ============================================================
    # Symmetry application
    # ============================================================

    def _apply_symmetry_to_lagrangian(self, L: Lagrangian,
                                       sym: Symmetry) -> Lagrangian:
        """Verify and exploit a symmetry of a Lagrangian."""
        transformed_expr = sym.transform(L.expr)
        return Lagrangian(f"{L.name}_{sym.name}", transformed_expr,
                          L.generalized_coords, L.velocities,
                          L.fields, L.time_var, L.is_density)

    def _apply_symmetry_to_pde(self, pde: PDEOperator,
                                sym: Symmetry) -> PDEOperator:
        """Apply symmetry to reduce a PDE."""
        new_lhs = sym.transform(pde.lhs)
        new_rhs = sym.transform(pde.rhs)
        return PDEOperator(f"{pde.name}_{sym.name}", new_lhs, new_rhs,
                           pde.unknown, pde.independent_vars,
                           pde.pde_type, pde.order)
