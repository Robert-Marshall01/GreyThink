# Core IR — Intermediate Representation

The IR is the universal language of Grey Physics. Every physical or
mathematical object is an `IRNode` with:

- **UUID identity** — every node is uniquely identifiable
- **Metadata** — units, stability, domain, symmetries, tags
- **Children** — a tree/DAG of substructures
- **`canonical_form()`** — normalised representation for comparison

## Expression DAG (`Expr`)

The expression system builds symbolic math trees without external CAS:

```python
from grey_physics.core.ir.types import Expr

x = Expr.symbol("x")
t = Expr.symbol("t")

# Build expressions
f = x**2 + Expr.constant(3) * x + Expr.constant(1)

# Derivatives
df = f.diff(x)

# Substitution
f_at_2 = f.substitute(x, Expr.constant(2))

# Free symbols
print(f.free_symbols())  # {'x'}
```

### ExprKind nodes

| Kind | Description |
|------|-------------|
| SYMBOL | Named variable |
| CONSTANT | Numeric literal |
| ADD | Sum of two exprs |
| MUL | Product |
| POW | Exponentiation |
| DIV | Division |
| NEG | Unary minus |
| FUNC | Named function application |
| DERIVATIVE | ∂f/∂x |
| INTEGRAL | ∫f dx |
| INDEXED | Tensor component |
| DELTA | Kronecker/Dirac δ |
| COMMUTATOR | [A, B] |
| TENSOR_PRODUCT | A ⊗ B |

## Units System

Dimensional analysis via 7 SI base dimensions:

```python
from grey_physics.core.ir.types import Units

force = Units(mass=1, length=1, time=-2)
energy = force * Units(length=1)
assert energy.is_compatible(Units(mass=1, length=2, time=-2))
```

## Fields

```python
from grey_physics.core.ir.fields import ScalarField, VectorField
import numpy as np

phi = ScalarField(name="phi", dimension=3,
                  expression=lambda x: np.exp(-np.sum(x**2)))

v = VectorField(name="v", dimension=3,
                components=[lambda x: -x[1], lambda x: x[0], lambda x: 0])
```

## Spacetime

```python
from grey_physics.core.ir.spacetime import Metric, Spacetime

g = Metric.schwarzschild(M=1.0)
components = g.components(np.array([0, 10, np.pi/2, 0]))  # 4×4 matrix
```

## Mechanics IR

```python
from grey_physics.core.ir.mechanics import Lagrangian, Hamiltonian

L = Lagrangian.harmonic_oscillator(mass=1.0, omega=2.0)
H = Hamiltonian.from_lagrangian(L)
```
