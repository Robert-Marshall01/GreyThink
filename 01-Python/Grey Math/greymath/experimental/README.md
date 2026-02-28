# Grey Math — Experimental Mode

> A sandboxed mathematical research environment for advanced, cutting-edge
> mathematical reasoning.

## Overview

The Experimental Mode provides opt-in access to advanced mathematical tools that
extend Grey Math's core capabilities. All experimental features are:

- **Isolated** — activated via a context manager, thread-local state
- **Safe** — resource limits, anomaly detection, determinism verification
- **Explicitly opt-in** — no experimental code runs unless you request it
- **Stability-gated** — every feature has a stability level (STABLE → DANGEROUS)

## Quick Start

```python
from greymath.experimental import ExperimentalMode

with ExperimentalMode() as ctx:
    # All experimental features available inside the context
    from greymath.experimental.functional_analysis import OperatorSemigroup
    import numpy as np

    A = np.array([[-1.0, 0.0], [0.0, -2.0]])
    sg = OperatorSemigroup.from_generator(A)
    T = sg.evaluate(1.0)
    print(T)  # Matrix exponential e^{tA}
```

## Modules

| Module | Description | Stability |
|--------|-------------|-----------|
| `mode.py` | Core activation, context management, stability gating | STABLE |
| `ir_extensions.py` | Extended IR types: operator spaces, charts, atlases, connections, bundles, SDEs, PDEs, flows | BETA |
| `functional_analysis.py` | Unbounded operators, spectral theory, resolvents, semigroups, fixed-point theory, Fredholm analysis | BETA |
| `differential_geometry.py` | Tangent/cotangent bundles, curvature tensors, geodesics, Lie groups, geometric flows | BETA |
| `measure_stochastic.py` | Information geometry, diffusion operators, Itô/Milstein SDE solvers, optimal transport | BETA |
| `chaos.py` | Attractor analysis, Poincaré sections, bifurcation analysis, Lyapunov analysis | ALPHA |
| `category.py` | Monoidal categories, enriched categories, operads, 2-categories, string diagrams, Yoneda embedding | ALPHA |
| `pde.py` | Heat/wave/reaction-diffusion/Hamilton-Jacobi solvers, spectral methods, FEM | BETA |
| `symbolic_ext.py` | Operator algebra, tensor calculus, stochastic symbolic manipulation, categorical rewrites | ALPHA |
| `numeric_ext.py` | Riemannian optimization, pseudospectra, interval arithmetic, mixed-precision, stability diagnostics | BETA |
| `architecture.py` | Composable mathematical architecture layers (semigroup, spectral, neural ODE, normalizing flow) | ALPHA |
| `debugger.py` | Spectral/dynamics/PDE diagnostics, convergence monitoring, matrix inspection | STABLE |
| `safety.py` | Anomaly detection, safe compute, determinism verification, checkpoints, type validators | STABLE |
| `plugin_ext.py` | Plugin registration, lifecycle, capability queries, dependency resolution | ALPHA |

## Mathematical Domains

### Functional Analysis
- Unbounded operators with domain specifications
- Resolvent analysis and spectral decomposition
- Operator semigroups (C₀ semigroups via matrix exponential)
- Contraction analysis and fixed-point theorems (Banach iteration)
- Fredholm analysis (kernel/cokernel, index computation)

### Differential Geometry
- Tangent and cotangent bundles with dual pairing
- Curvature tensors (Riemann, Ricci, scalar, Weyl) via numerical differentiation
- Geodesic solvers using the geodesic equation
- Lie algebras and Lie groups (SO(n), SU(n), GL(n)) with exponential maps
- Levi-Civita connection, natural gradient, geometric flows (Ricci, mean curvature)

### Measure Theory & Stochastic Calculus
- Information geometry (Fisher information, KL divergence, natural gradient)
- Diffusion operators (generator, Fokker-Planck, spectral gap)
- SDE solvers: Euler-Maruyama, Milstein schemes
- Optimal transport: 1D Wasserstein, Sinkhorn, sliced Wasserstein

### Dynamical Systems & Chaos
- Attractor detection and classification (fixed point, limit cycle, chaotic)
- Correlation dimension estimation
- Poincaré sections and return maps
- Invariant sets: periodic orbits, stable/unstable manifolds
- Bifurcation analysis (saddle-node, pitchfork, Hopf detection)
- Lyapunov analysis (candidate functions, verification, FTLE)
- Built-in systems: Rössler, Hénon, Duffing, Chua

### Category Theory
- Monoidal categories (symmetric, braided) with tensor products
- Enriched categories and enriched Hom objects
- Operads with multi-input compositions and operad algebras
- 2-categories with vertical/horizontal composition
- String diagrams (graphical calculus)
- Kan extensions and Yoneda embedding

### PDE Solvers
- Heat equation: explicit, implicit, Crank-Nicolson
- Wave equation: leapfrog scheme
- Reaction-diffusion: Gray-Scott, FitzHugh-Nagumo patterns
- Hamilton-Jacobi: Lax-Friedrichs scheme
- Spectral methods: Fourier heat solver, KdV split-step
- Method of lines (scipy.integrate backend)
- Variational formulation: 1D Poisson FEM

## Safety & Diagnostics

The experimental mode includes comprehensive safety features:

```python
from greymath.experimental.safety import AnomalyDetector, SafeCompute

# Automatic anomaly detection
anomalies = AnomalyDetector.check_matrix(A)

# Safe linear algebra with regularization
x, anomalies = SafeCompute.safe_solve(A, b)

# Determinism verification
is_det, results = DeterministicMode.verify_deterministic(my_fn, seed=42)

# Checkpoint & rollback
ckpt = Checkpoint()
ckpt.save("state", current_state)
restored = ckpt.restore("state")
```

## Plugin System

Extend the experimental subsystem with custom plugins:

```python
from greymath.experimental.plugin_ext import (
    ExperimentalPlugin, ExperimentalPluginDescriptor,
    PluginCapability, get_plugin_registry,
)

class MyPlugin(ExperimentalPlugin):
    def describe(self):
        return ExperimentalPluginDescriptor(
            name="MyPlugin",
            version="0.1.0",
            capabilities=[PluginCapability.MANIFOLD],
        )

    def get_exports(self):
        return {"my_manifold_fn": ...}

registry = get_plugin_registry()
registry.register(MyPlugin())
```

## Running Tests

```bash
cd "Grey Math"
python -m pytest tests/test_experimental.py -v
```

## Architecture

```
greymath/experimental/
├── __init__.py           # Package init, exports core types
├── mode.py               # Context manager, stability gating
├── ir_extensions.py      # Extended IR types
├── functional_analysis.py
├── differential_geometry.py
├── measure_stochastic.py
├── chaos.py
├── category.py
├── pde.py
├── symbolic_ext.py
├── numeric_ext.py
├── architecture.py       # Composable math layers
├── debugger.py           # Diagnostics & visualization
├── safety.py             # Safety enforcement
├── plugin_ext.py         # Plugin system
└── README.md
```

## Version

`0.1.0-experimental`
