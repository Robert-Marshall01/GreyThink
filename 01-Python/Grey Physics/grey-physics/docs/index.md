# Grey Physics Documentation

## Architecture

```
grey_physics/
├── core/
│   ├── ir/           # Intermediate Representation
│   │   ├── types.py      — Expr DAG, IRNode, Units, Metadata, Tensors
│   │   ├── fields.py     — Scalar/Vector/Tensor/SpinorField
│   │   ├── spacetime.py  — Metric, Manifold, Curvature, Connection
│   │   ├── mechanics.py  — Lagrangian, Hamiltonian, Action
│   │   ├── operators.py  — PDE/ODE operators, boundary conditions
│   │   ├── symmetry.py   — Symmetry, NoetherCharge, ConservationLaw
│   │   ├── quantum.py    — QuantumState, Observable, OperatorAlgebra
│   │   ├── media.py      — Fluid, Plasma, ElasticMedium
│   │   ├── gauge.py      — GaugeField, GaugeTransformation
│   │   └── dynamical.py  — DynamicalSystem, FixedPoint, Flow
│   ├── math/         # Mathematical foundations
│   │   ├── differential_geometry.py — Christoffel, Riemann, geodesics, forms
│   │   ├── functional_analysis.py   — FunctionSpace, LinearOperator, spectra
│   │   ├── variational_calculus.py  — Functionals, Euler-Lagrange, Legendre
│   │   ├── pde_ode_theory.py        — Classification, weak forms, stochastic
│   │   └── dynamical_systems.py     — Phase portraits, bifurcation, Poincaré
│   ├── symbolic/     # Symbolic physics engine
│   │   └── __init__.py — SymbolicEngine: Euler-Lagrange, Noether, tensor calc
│   └── numeric/      # Numerical solvers
│       └── __init__.py — ODE (RK4/RK45/Verlet/BDF), PDE (FDM/FEM/Spectral)
├── domains/          # Physics domains
│   ├── mechanics.py        — Particles, Lagrangian/Hamiltonian simulators
│   ├── electromagnetism.py — FDTD, electrostatics, EM waves, ray tracing
│   ├── fluids.py           — Navier-Stokes, shallow water, lattice Boltzmann
│   ├── quantum.py          — Schrödinger, spin systems, open quantum systems
│   ├── relativity.py       — Lorentz, geodesics, Friedmann, grav. waves
│   ├── field_theory.py     — Lattice gauge, Higgs, RG flow, Noether currents
│   └── chaos.py            — Strange attractors, Lyapunov, bifurcation, fractals
├── experimental/     # Cutting-edge modules
│   └── __init__.py — Lie groups, SINDy, Koopman, information geometry
├── plugins/          # Plugin system
│   └── __init__.py — PluginManager, hooks, component registry
└── ide/              # Terminal interface
    └── __init__.py — REPL, inspector, text visualizations
```

## Quick Start

```python
import numpy as np
from grey_physics.domains.mechanics import LagrangianSimulator

# Double pendulum
sim = LagrangianSimulator.double_pendulum(m1=1, m2=1, l1=1, l2=1, g=9.81)
ts, qs, dqs = sim.simulate(
    q0=np.array([np.pi/4, np.pi/4]),
    dq0=np.array([0.0, 0.0]),
    t_span=(0, 20), dt=0.001,
)
```

## Tutorials

See individual domain docs below.

## API Reference

Each module has comprehensive docstrings — use `help(module)` in Python.
