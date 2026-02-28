# Experimental & Plugin System

## Experimental Modules

Cutting-edge features under `grey_physics.experimental`:

### Lie Groups & Geometric Mechanics

```python
from grey_physics.experimental import LieGroup, RigidBodyOnSO3
import numpy as np

# SO(3) operations
omega = np.array([0, 0, np.pi/4])
R = LieGroup.so3_exp(omega)  # Rodrigues rotation

# Rigid body on SO(3) with Lie group integrator
rb = RigidBodyOnSO3(
    inertia=np.diag([2, 3, 4]),
    omega0=np.array([1, 0.1, 0]),
)
ts, Rs, omegas = rb.simulate(t_span=(0, 10), dt=0.01)
```

### Data-Driven Discovery (SINDy)

```python
from grey_physics.experimental import SINDy

# Discover governing equations from data
sindy = SINDy(poly_order=3, threshold=0.05)
Xi = sindy.fit(X_data, t)
sindy.print_equations(Xi, feature_names=["x", "y", "z"])
```

### Koopman Analysis (DMD)

```python
from grey_physics.experimental import KoopmanAnalysis

ka = KoopmanAnalysis()
evals, modes, dynamics = ka.exact_dmd(X_snapshots, dt=0.01)
X_reconstructed = ka.reconstruct(evals, modes, dynamics, t_predict)
```

### Information Geometry

```python
from grey_physics.experimental import InformationGeometry

ig = InformationGeometry()
F = ig.fisher_metric_gaussian(mu=0, sigma=1)
D_KL = ig.kl_divergence_gaussian(0, 1, 1, 2)
```

---

## Plugin System

Extend Grey Physics with custom solvers, field types, and analysis tools.

### Creating a Plugin

```python
from grey_physics.plugins import PluginBase, PluginMeta

class MyPlugin(PluginBase):
    meta = PluginMeta(
        name="my_plugin",
        version="1.0.0",
        author="You",
        description="Custom solver plugin",
    )

    def on_load(self, manager):
        manager.components.register("solver", "my_solver", MySolver())
        manager.hooks.register("pre_simulate", self.on_pre_sim)

    def on_unload(self, manager):
        manager.components.unregister("solver", "my_solver")

    def on_pre_sim(self, **kwargs):
        print("Simulation starting with:", kwargs)
```

### Using the Plugin Manager

```python
from grey_physics.plugins import PluginManager

pm = PluginManager()
pm.register(MyPlugin())

# Plugins are auto-discovered from directories:
pm.load_from_directory("./my_plugins/")

# Trigger hooks
pm.hooks.trigger("pre_simulate", system="pendulum")

# Access registered components
solver = pm.components.get("solver", "my_solver")
```

### Built-in Hooks

| Hook | Triggered When |
|------|---------------|
| `pre_simulate` | Before simulation starts |
| `post_simulate` | After simulation completes |
| `pre_solve` | Before solver step |
| `post_solve` | After solver step |
| `on_error` | When an error occurs |
| `on_energy_drift` | When energy conservation fails |
