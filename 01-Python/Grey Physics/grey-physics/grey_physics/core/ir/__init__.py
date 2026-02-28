"""
Grey Physics — Intermediate Representation (IR)
Strongly-typed, graph-based, compositional IR for physics and mathematics.
"""

from grey_physics.core.ir.types import *
from grey_physics.core.ir.fields import ScalarField, VectorField, TensorField, SpinorField
from grey_physics.core.ir.spacetime import Spacetime, Manifold, Chart, Metric, Connection, CurvatureTensor
from grey_physics.core.ir.mechanics import Lagrangian, Hamiltonian, ActionFunctional
from grey_physics.core.ir.operators import (
    PDEOperator, ODEOperator, DifferentialOperator,
    BoundaryCondition, InitialCondition
)
from grey_physics.core.ir.symmetry import Symmetry, NoetherCharge, ConservationLaw
from grey_physics.core.ir.quantum import QuantumState, Observable, OperatorAlgebra, Commutator
from grey_physics.core.ir.media import Fluid, Plasma, ElasticMedium
from grey_physics.core.ir.gauge import GaugeField, GaugeTransformation
from grey_physics.core.ir.dynamical import DynamicalSystem, Flow, VectorFieldOnManifold

__all__ = [
    "ScalarField", "VectorField", "TensorField", "SpinorField",
    "Spacetime", "Manifold", "Chart", "Metric", "Connection", "CurvatureTensor",
    "Lagrangian", "Hamiltonian", "ActionFunctional",
    "PDEOperator", "ODEOperator", "DifferentialOperator",
    "BoundaryCondition", "InitialCondition",
    "Symmetry", "NoetherCharge", "ConservationLaw",
    "QuantumState", "Observable", "OperatorAlgebra", "Commutator",
    "Fluid", "Plasma", "ElasticMedium",
    "GaugeField", "GaugeTransformation",
    "DynamicalSystem", "Flow", "VectorFieldOnManifold",
]
