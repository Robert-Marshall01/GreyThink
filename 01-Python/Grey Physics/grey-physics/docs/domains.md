# Domain Modules

Grey Physics provides 7 physics domain modules, each with production-grade
simulators and analysis tools.

---

## 1. Classical Mechanics (`grey_physics.domains.mechanics`)

### Lagrangian Mechanics

```python
from grey_physics.domains.mechanics import LagrangianSimulator
import numpy as np

# Pre-built systems
sim = LagrangianSimulator.double_pendulum(m1=1, m2=1, l1=1, l2=1, g=9.81)

ts, qs, dqs = sim.simulate(
    q0=np.array([np.pi/4, np.pi/6]),
    dq0=np.array([0, 0]),
    t_span=(0, 20), dt=0.001,
)
```

### Hamiltonian Mechanics

```python
from grey_physics.domains.mechanics import HamiltonianSimulator

sim = HamiltonianSimulator.kepler()
ts, qs, ps = sim.simulate(
    q0=np.array([1.0, 0.0]),
    p0=np.array([0.0, 1.0]),
    t_span=(0, 10), dt=0.001,
)
```

### N-Body Gravity

```python
from grey_physics.domains.mechanics import NBodyGravity

nbody = NBodyGravity.three_body_figure_eight()
ts, positions, velocities = nbody.simulate(t_span=(0, 10), dt=0.001)
```

---

## 2. Electromagnetism (`grey_physics.domains.electromagnetism`)

### FDTD (Finite-Difference Time-Domain)

```python
from grey_physics.domains.electromagnetism import FDTDSolver

fdtd = FDTDSolver(nx=100, ny=100, nz=1, dx=0.01)
fdtd.add_source(50, 50, 0, frequency=1e9)
for _ in range(500):
    fdtd.step()
```

### Electrostatics

```python
from grey_physics.domains.electromagnetism import Electrostatics

es = Electrostatics()
E = es.point_charge_field(q=1.0, r_charge=np.zeros(3),
                           r_obs=np.array([1, 0, 0]))
```

---

## 3. Fluid Dynamics (`grey_physics.domains.fluids`)

### 2D Navier-Stokes (vorticity-streamfunction)

```python
from grey_physics.domains.fluids import FluidSolver2D

solver = FluidSolver2D.taylor_green(N=128, Re=1000)
for _ in range(1000):
    solver.step()
k, Ek = solver.energy_spectrum()
```

### Lattice Boltzmann

```python
from grey_physics.domains.fluids import LatticeBoltzmann

lb = LatticeBoltzmann.cylinder_flow(nx=200, ny=80, Re=100)
for _ in range(5000):
    lb.step()
```

---

## 4. Quantum Mechanics (`grey_physics.domains.quantum`)

### Split-Step Schrödinger

```python
from grey_physics.domains.quantum import SchrodingerSolver1D

solver = SchrodingerSolver1D.gaussian_wavepacket(N=512, L=20, x0=-5, k0=5)
for _ in range(500):
    solver.step(dt=0.01)
prob = np.abs(solver.psi)**2
```

### Spin Chains

```python
from grey_physics.domains.quantum import SpinSystem

ising = SpinSystem.transverse_ising(n_sites=8, J=1.0, h=0.5)
E, psi = ising.ground_state()
S = ising.entanglement_entropy(psi, cut=4)
```

---

## 5. General Relativity (`grey_physics.domains.relativity`)

### Schwarzschild Geodesics

```python
from grey_physics.domains.relativity import GeodesicSolver

geo = GeodesicSolver.schwarzschild(M=1.0)
taus, traj = geo.solve(
    y0=np.array([0, 10, np.pi/2, 0]),
    v0=np.array([1, 0, 0, 0.03]),
    tau_span=(0, 1000), n_steps=10000,
)
```

### FLRW Cosmology

```python
from grey_physics.domains.relativity import FriedmannSolver

fs = FriedmannSolver(Omega_m=0.3, Omega_r=1e-4, Omega_L=0.7, H0=70)
ts, a = fs.solve(a_init=0.001, t_span=(0, 14e9))
```

---

## 6. Field Theory (`grey_physics.domains.field_theory`)

### Lattice Gauge Theory

```python
from grey_physics.domains.field_theory import LatticeGaugeTheory

lgt = LatticeGaugeTheory(Nx=16, Ny=16, beta=2.0)
lgt.thermalize(n_sweeps=1000)
W = lgt.wilson_loop(Rt=4, Rx=4)
```

### Renormalization Group

```python
from grey_physics.domains.field_theory import RenormalizationGroup

rg = RenormalizationGroup.qcd_beta(Nf=6)
ts, gs = rg.flow(g0=np.array([0.3]), t_span=(0, 10), dt=0.01)
```

---

## 7. Chaos & Nonlinear Dynamics (`grey_physics.domains.chaos`)

### Strange Attractors

```python
from grey_physics.domains.chaos import StrangeAttractor, LyapunovComputer

att = StrangeAttractor.lorenz()
traj = att.integrate(y0=np.array([1, 1, 1]), t_span=(0, 100), dt=0.01)
```

### Bifurcation Diagrams

```python
from grey_physics.domains.chaos import BifurcationDiagram

bd = BifurcationDiagram()
params, values = bd.logistic_bifurcation(r_range=(2.5, 4), n_params=500)
```

### Lyapunov Exponents

```python
from grey_physics.domains.chaos import LyapunovComputer

lc = LyapunovComputer.lorenz_jacobian()
exps = lc.compute(lorenz_rhs, y0=np.array([1,1,1]), T=50, dt=0.01)
# → [0.91, 0.0, -14.57]  (Lorenz exponents)
```
