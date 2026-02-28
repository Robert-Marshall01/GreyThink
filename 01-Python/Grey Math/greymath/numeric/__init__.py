"""
Numeric Engine — High-precision numerical computation backend.

Provides:
- Arbitrary precision arithmetic
- Interval arithmetic for error bounds
- Dense and sparse linear algebra
- Eigenvalue / spectral decomposition
- ODE solvers (explicit, implicit, stiff)
- PDE discretization hooks
- Optimization (convex, non-convex, constrained)
- Stochastic sampling (MCMC, SMC, variational inference)
- Conditioning and stability diagnostics
- Mixed-precision execution strategies
"""

from greymath.numeric.precision import ArbitraryPrecision, IntervalArithmetic  # noqa: F401
from greymath.numeric.linalg import LinAlg  # noqa: F401
from greymath.numeric.spectral import SpectralSolver  # noqa: F401
from greymath.numeric.ode import ODESolver  # noqa: F401
from greymath.numeric.optimize import Optimizer  # noqa: F401
from greymath.numeric.stochastic import StochasticSampler  # noqa: F401
from greymath.numeric.stability import StabilityDiagnostics  # noqa: F401
