# Grey Math Architecture

## System Overview

Grey Math is a research-grade mathematical IDE designed for building intelligence
using PhD-level mathematics. It provides a complete stack from symbolic computation
through numerical solvers to an interactive IDE interface.

## Architecture Diagram

```
┌──────────────────────────────────────────────────────────────────────┐
│                        Grey Math IDE                                 │
│  ┌──────────┐ ┌──────────┐ ┌───────────┐ ┌───────────────────────┐  │
│  │  Editor   │ │  Graph   │ │  Console  │ │     Inspector         │  │
│  │  (Cells)  │ │  View    │ │  (REPL)   │ │  (Types/Stability)   │  │
│  └──────────┘ └──────────┘ └───────────┘ └───────────────────────┘  │
├──────────────────────────────────────────────────────────────────────┤
│                     FastAPI + WebSocket Server                       │
├──────────────────────────────────────────────────────────────────────┤
│  ┌─────────────────────────────────────────────────────────────────┐ │
│  │                      Session Manager                            │ │
│  │  Variables │ History │ DAG │ Evaluation │ State                 │ │
│  └─────────────────────────────────────────────────────────────────┘ │
├────────────┬───────────────┬──────────────┬──────────────────────────┤
│  Symbolic  │   Numeric     │   Domain     │   Verification          │
│  Engine    │   Engine      │   Modules    │   Layer                 │
│            │               │              │                         │
│  Pattern   │  Precision    │  Functional  │  Property-Based Testing │
│  Matching  │  (Arbitrary)  │  Analysis    │  Symbolic Equivalence   │
│            │               │              │                         │
│  Rewrite   │  LinAlg       │  Diff        │  Interval Bounds        │
│  Rules     │  (Dense/Sprs) │  Geometry    │                         │
│            │               │              │  Proof Export           │
│  Simplify  │  Spectral     │  Measure     │  (Lean 4 / Coq /       │
│            │  Solver       │  Theory      │   LaTeX)                │
│  Diff      │               │              │                         │
│  (Symbolic)│  ODE Solvers  │  Optimization│                         │
│            │  (RK4/BDF)    │  Theory      │                         │
│  Series    │               │              │                         │
│  Expansion │  Optimizer    │  Dynamical   │                         │
│            │  (GD/L-BFGS)  │  Systems     │                         │
│  Structural│               │              │                         │
│  Analysis  │  Stochastic   │  Category    │                         │
│            │  (MCMC/HMC)   │  Theory      │                         │
│            │               │              │                         │
│            │  Stability    │              │                         │
│            │  Diagnostics  │              │                         │
├────────────┴───────────────┴──────────────┴──────────────────────────┤
│                         Core IR Layer                                │
│  ┌──────────────┐  ┌──────────────┐  ┌────────────────────────────┐ │
│  │  Type System  │  │  Expression  │  │  Metadata (Precision,     │ │
│  │  (15+ types)  │  │  DAG + Hash  │  │  Stability, Provenance)   │ │
│  └──────────────┘  └──────────────┘  └────────────────────────────┘ │
├──────────────────────────────────────────────────────────────────────┤
│                        Runtime Layer                                 │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌───────────────────┐   │
│  │  Cache    │  │  Tracer  │  │ Compiler │  │ Parallel Executor │   │
│  │  (LRU)   │  │ +Profile │  │  (CSE)   │  │  (Thread/Process) │   │
│  └──────────┘  └──────────┘  └──────────┘  └───────────────────┘   │
├──────────────────────────────────────────────────────────────────────┤
│                     Integration Layer                                │
│  ┌──────────────┐  ┌──────────────┐  ┌────────────────────────────┐ │
│  │  JSON-RPC     │  │ LLM Tool     │  │  Sandboxed Evaluator      │ │
│  │  Server       │  │ Schemas      │  │  (Safe Math Execution)    │ │
│  └──────────────┘  └──────────────┘  └────────────────────────────┘ │
├──────────────────────────────────────────────────────────────────────┤
│                       Plugin System                                  │
│  ┌──────────────┐  ┌──────────────┐  ┌────────────────────────────┐ │
│  │  Plugin Base  │  │  Plugin Mgr  │  │  Hot-Reload Loader        │ │
│  │  (ABC + Meta) │  │  (Lifecycle) │  │  (Entry Points / Dir)     │ │
│  └──────────────┘  └──────────────┘  └────────────────────────────┘ │
└──────────────────────────────────────────────────────────────────────┘
```

## Module Descriptions

### Core IR (`greymath/core/`)
The foundational representation layer:
- **types.py**: 15+ mathematical types (Scalar, Vector, Matrix, Tensor, Function, Operator, Measure, Manifold, Category objects, etc.) with Protocol-based type classes
- **expr.py**: Expression DAG with 40+ operator kinds, structural hash-consing, operator overloading, symbolic manipulation
- **metadata.py**: Precision tracking, stability ratings, domain assumptions, provenance

### Symbolic Engine (`greymath/symbolic/`)
Pure symbolic computation:
- **pattern.py**: Structural pattern matching (exact, wildcard, typed, predicate, alternative)
- **rewrite.py**: Rule-based term rewriting with strategies (normalize, greedy, exhaustive, bottom-up, top-down)
- **simplify.py**: 15+ algebraic simplification rules
- **differentiation.py**: Symbolic differentiation (product rule, chain rule, etc.), gradient, Jacobian, Hessian
- **series.py**: Taylor, Laurent, asymptotic series expansion
- **analysis.py**: Structural analysis (linearity, convexity, symmetry, polynomial degree)

### Numeric Engine (`greymath/numeric/`)
Numerical computation:
- **precision.py**: Arbitrary precision (mpmath), interval arithmetic
- **linalg.py**: Complete linear algebra (solve, decompositions, matrix functions)
- **spectral.py**: Spectral analysis (eigenvalues, pseudospectrum, resolvent)
- **ode.py**: ODE solvers (Euler, RK4, RK45 adaptive, backward Euler, BDF)
- **optimize.py**: Optimization (gradient descent, Newton, L-BFGS, Nelder-Mead, constrained)
- **stochastic.py**: MCMC sampling (Metropolis-Hastings, HMC with leapfrog)
- **stability.py**: Numerical stability diagnostics and recommendations

### Domain Modules (`greymath/domains/`)
Advanced mathematical domains:
- **functional_analysis.py**: Hilbert/Banach spaces, operator theory, fixed-point solvers
- **differential_geometry.py**: Manifolds (Euclidean, Sphere, Stiefel), Riemannian geometry, geodesics
- **measure_theory.py**: σ-algebras, distributions, KL divergence, Wasserstein distance
- **optimization_theory.py**: Convex sets, cones, Lagrangian systems, proximal operators
- **dynamical_systems.py**: Fixed points, bifurcation, Lyapunov exponents, Lorenz system
- **category_theory.py**: Categories, functors, adjunctions, monads

### IDE (`greymath/ide/`)
Interactive development environment:
- **server.py**: FastAPI backend with REST + WebSocket
- **session.py**: Session management with variable binding and history
- **frontend.py**: Single-page HTML/CSS/JS IDE with multi-pane layout

### API Layer (`greymath/api/`)
LLM and agent integration:
- **rpc.py**: JSON-RPC 2.0 server with full method dispatch
- **tools.py**: Tool definitions for OpenAI/Anthropic function calling
- **sandbox.py**: AST-validated safe expression evaluation

### Plugin System (`greymath/plugins/`)
Extensibility framework:
- **base.py**: Plugin ABC, metadata, capabilities, lifecycle management
- **loader.py**: Discovery from modules, directories, entry points; hot-reload

### Verification (`greymath/verify/`)
Mathematical verification:
- **properties.py**: Property-based testing (identity, commutativity, monotonicity, etc.)
- **equivalence.py**: Symbolic equivalence via normalization + numerical testing
- **bounds.py**: Interval arithmetic verification, certified computation
- **export.py**: Proof export to Lean 4, Coq, LaTeX

### Runtime (`greymath/runtime/`)
Execution infrastructure:
- **cache.py**: LRU expression cache + persistent result cache
- **tracer.py**: Execution tracing, profiling, span-based instrumentation
- **compiler.py**: Expression compilation with CSE, constant folding, NumPy codegen
- **parallel.py**: Thread/process pool execution, task graphs, parameter sweeps

## Design Principles

1. **Separation of Concerns**: Symbolic and numeric layers are independent
2. **Dependency Inversion**: Core types define interfaces; engines implement them
3. **Extensibility**: Plugin system allows adding types, operators, solvers at runtime
4. **Safety**: Sandboxed evaluation prevents arbitrary code execution
5. **Verifiability**: Every computation can be bounds-checked and exported to proof assistants
6. **Performance**: Compilation, caching, and parallelism for production workloads
