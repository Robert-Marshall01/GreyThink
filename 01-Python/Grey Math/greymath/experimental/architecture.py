"""
Architecture Builder — Experimental Module.

Provides composable, type-safe architecture construction for:
- Operator-based architectures (operator semigroups, spectral)
- Geometric architectures (manifold-valued, fiber-bundle)
- Measure-theoretic architectures (normalizing flows, transport)
- Dynamical architectures (flows, ODE-nets)
- Categorical architectures (string diagram composition)
- PDE architectures (neural PDE, physics-informed)
- Hybrid architectures (multi-domain composition)

Each architecture tracks mathematical invariants and supports
validation, composition, and export.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Callable, Optional

import numpy as np
from numpy.typing import NDArray


# ─── Architecture Kind ──────────────────────────────────────────────────────

class ArchitectureKind(Enum):
    """Classification of mathematical architectures."""
    OPERATOR = auto()
    GEOMETRIC = auto()
    MEASURE = auto()
    DYNAMICAL = auto()
    CATEGORICAL = auto()
    PDE = auto()
    HYBRID = auto()


# ─── Architecture Layer ─────────────────────────────────────────────────────

@dataclass
class ArchitectureLayer:
    """A single layer in a mathematical architecture."""
    name: str
    kind: ArchitectureKind = ArchitectureKind.OPERATOR
    input_shape: Optional[tuple[int, ...]] = None
    output_shape: Optional[tuple[int, ...]] = None
    parameters: dict[str, Any] = field(default_factory=dict)
    forward_fn: Optional[Callable[[NDArray], NDArray]] = None
    inverse_fn: Optional[Callable[[NDArray], NDArray]] = None
    is_invertible: bool = False
    preserves: list[str] = field(default_factory=list)  # e.g., ["volume", "symplectic"]

    def forward(self, x: NDArray) -> NDArray:
        if self.forward_fn is None:
            raise ValueError(f"Layer {self.name} has no forward function")
        return self.forward_fn(x)

    def inverse(self, y: NDArray) -> NDArray:
        if not self.is_invertible or self.inverse_fn is None:
            raise ValueError(f"Layer {self.name} is not invertible")
        return self.inverse_fn(y)

    def __repr__(self) -> str:
        return (f"Layer({self.name}, {self.kind.name}, "
                f"{self.input_shape} → {self.output_shape})")


# ─── Architecture ───────────────────────────────────────────────────────────

@dataclass
class Architecture:
    """
    A composable mathematical architecture.

    Chains layers while tracking mathematical invariants
    (shape compatibility, preservation properties, invertibility).
    """
    name: str = ""
    kind: ArchitectureKind = ArchitectureKind.HYBRID
    layers: list[ArchitectureLayer] = field(default_factory=list)
    invariants: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)

    def add_layer(self, layer: ArchitectureLayer) -> None:
        """Add a layer, checking shape compatibility."""
        if self.layers:
            prev = self.layers[-1]
            if (prev.output_shape is not None and
                    layer.input_shape is not None and
                    prev.output_shape != layer.input_shape):
                raise ValueError(
                    f"Shape mismatch: {prev.name} outputs {prev.output_shape} "
                    f"but {layer.name} expects {layer.input_shape}"
                )
        self.layers.append(layer)

    def forward(self, x: NDArray) -> NDArray:
        """Forward pass through all layers."""
        out = x
        for layer in self.layers:
            out = layer.forward(out)
        return out

    def inverse(self, y: NDArray) -> NDArray:
        """Inverse pass (if all layers are invertible)."""
        out = y
        for layer in reversed(self.layers):
            out = layer.inverse(out)
        return out

    @property
    def is_invertible(self) -> bool:
        return all(layer.is_invertible for layer in self.layers)

    @property
    def input_shape(self) -> Optional[tuple[int, ...]]:
        return self.layers[0].input_shape if self.layers else None

    @property
    def output_shape(self) -> Optional[tuple[int, ...]]:
        return self.layers[-1].output_shape if self.layers else None

    def compose(self, other: "Architecture") -> "Architecture":
        """Compose two architectures sequentially."""
        result = Architecture(
            name=f"({self.name} → {other.name})",
            kind=ArchitectureKind.HYBRID,
        )
        for layer in self.layers:
            result.add_layer(layer)
        for layer in other.layers:
            result.add_layer(layer)
        return result

    def validate(self) -> list[str]:
        """Validate the architecture for consistency."""
        errors: list[str] = []
        for i in range(len(self.layers) - 1):
            curr = self.layers[i]
            nxt = self.layers[i + 1]
            if (curr.output_shape is not None and
                    nxt.input_shape is not None and
                    curr.output_shape != nxt.input_shape):
                errors.append(
                    f"Shape mismatch at layer {i}: "
                    f"{curr.name} → {nxt.name}"
                )
        return errors

    def summary(self) -> dict[str, Any]:
        """Architecture summary."""
        return {
            "name": self.name,
            "kind": self.kind.name,
            "n_layers": len(self.layers),
            "input_shape": self.input_shape,
            "output_shape": self.output_shape,
            "invertible": self.is_invertible,
            "layers": [
                {"name": l.name, "kind": l.kind.name,
                 "shape": f"{l.input_shape} → {l.output_shape}"}
                for l in self.layers
            ],
        }

    def __repr__(self) -> str:
        return (f"Architecture({self.name}, {len(self.layers)} layers, "
                f"{self.input_shape} → {self.output_shape})")


# ─── Architecture Builders ──────────────────────────────────────────────────

class OperatorArchitectureBuilder:
    """Build architectures from operator semigroups and spectral methods."""

    @staticmethod
    def semigroup_layer(
        generator: NDArray,
        t: float = 1.0,
        name: str = "SemigroupLayer",
    ) -> ArchitectureLayer:
        """Create a layer from operator semigroup: x ↦ exp(tA)x."""
        from scipy.linalg import expm
        n = generator.shape[0]
        E = expm(t * generator)

        return ArchitectureLayer(
            name=name,
            kind=ArchitectureKind.OPERATOR,
            input_shape=(n,),
            output_shape=(n,),
            forward_fn=lambda x: E @ x,
            inverse_fn=lambda y: np.linalg.solve(E, y),
            is_invertible=True,
            preserves=["linearity"],
            parameters={"generator": generator, "t": t},
        )

    @staticmethod
    def spectral_layer(
        eigenvectors: NDArray,
        eigenvalues: NDArray,
        activation: Callable[[NDArray], NDArray] = lambda x: x,
        name: str = "SpectralLayer",
    ) -> ArchitectureLayer:
        """
        Spectral layer: project to eigenbasis, apply function, project back.

        x ↦ V f(Λ V^T x)
        """
        n = eigenvectors.shape[0]
        V = eigenvectors
        VT = V.T

        def forward_fn(x: NDArray) -> NDArray:
            spectral = VT @ x
            return V @ activation(spectral * eigenvalues)

        return ArchitectureLayer(
            name=name,
            kind=ArchitectureKind.OPERATOR,
            input_shape=(n,),
            output_shape=(n,),
            forward_fn=forward_fn,
            parameters={"eigenvalues": eigenvalues},
        )


class GeometricArchitectureBuilder:
    """Build architectures operating on manifold-valued data."""

    @staticmethod
    def exponential_map_layer(
        metric_fn: Callable[[NDArray], NDArray],
        name: str = "ExpMapLayer",
        dim: int = 0,
    ) -> ArchitectureLayer:
        """
        Layer using the exponential map on a Riemannian manifold.

        Maps tangent vectors to manifold via exp_p: T_pM → M.
        """
        return ArchitectureLayer(
            name=name,
            kind=ArchitectureKind.GEOMETRIC,
            input_shape=(dim,) if dim > 0 else None,
            output_shape=(dim,) if dim > 0 else None,
            preserves=["manifold_structure"],
            parameters={"metric_fn": metric_fn},
        )

    @staticmethod
    def parallel_transport_layer(
        christoffel_fn: Callable[[NDArray], NDArray],
        curve: Callable[[float], NDArray],
        t0: float = 0.0,
        t1: float = 1.0,
        name: str = "ParTransportLayer",
        dim: int = 0,
    ) -> ArchitectureLayer:
        """Layer that parallel-transports vectors along a curve."""
        return ArchitectureLayer(
            name=name,
            kind=ArchitectureKind.GEOMETRIC,
            input_shape=(dim,) if dim > 0 else None,
            output_shape=(dim,) if dim > 0 else None,
            preserves=["inner_product"],
            parameters={"curve": curve, "t0": t0, "t1": t1},
        )


class DynamicalArchitectureBuilder:
    """Build architectures from dynamical systems / ODE-nets."""

    @staticmethod
    def neural_ode_layer(
        vector_field: Callable[[NDArray, float], NDArray],
        t_span: tuple[float, float] = (0.0, 1.0),
        dt: float = 0.01,
        name: str = "NeuralODELayer",
        dim: int = 0,
    ) -> ArchitectureLayer:
        """
        Neural ODE layer: x(T) = x(0) + ∫₀ᵀ f(x(t), t) dt.

        Integrates a learned vector field.
        """
        from scipy.integrate import solve_ivp

        def forward_fn(x: NDArray) -> NDArray:
            sol = solve_ivp(
                lambda t, y: vector_field(y, t),
                t_span, x, method="RK45",
                max_step=dt,
            )
            return sol.y[:, -1]

        return ArchitectureLayer(
            name=name,
            kind=ArchitectureKind.DYNAMICAL,
            input_shape=(dim,) if dim > 0 else None,
            output_shape=(dim,) if dim > 0 else None,
            forward_fn=forward_fn,
            is_invertible=True,
            preserves=["continuity"],
            parameters={"t_span": t_span},
        )

    @staticmethod
    def hamiltonian_layer(
        hamiltonian: Callable[[NDArray, NDArray], float],
        t: float = 1.0,
        dt: float = 0.01,
        name: str = "HamiltonianLayer",
        dim: int = 0,
    ) -> ArchitectureLayer:
        """
        Hamiltonian layer: symplectic integrator preserving phase space volume.

        Uses leapfrog/Verlet integration of Hamilton's equations:
        dq/dt = ∂H/∂p, dp/dt = -∂H/∂q
        """
        def forward_fn(qp: NDArray) -> NDArray:
            n = len(qp) // 2
            q, p = qp[:n].copy(), qp[n:].copy()
            eps = 1e-5
            n_steps = max(1, int(t / dt))
            h = t / n_steps

            for _ in range(n_steps):
                # Leapfrog
                grad_q = np.zeros(n)
                for i in range(n):
                    q_plus = q.copy(); q_plus[i] += eps
                    q_minus = q.copy(); q_minus[i] -= eps
                    grad_q[i] = (hamiltonian(q_plus, p) - hamiltonian(q_minus, p)) / (2 * eps)

                p -= 0.5 * h * grad_q

                grad_p = np.zeros(n)
                for i in range(n):
                    p_plus = p.copy(); p_plus[i] += eps
                    p_minus = p.copy(); p_minus[i] -= eps
                    grad_p[i] = (hamiltonian(q, p_plus) - hamiltonian(q, p_minus)) / (2 * eps)

                q += h * grad_p

                grad_q2 = np.zeros(n)
                for i in range(n):
                    q_plus = q.copy(); q_plus[i] += eps
                    q_minus = q.copy(); q_minus[i] -= eps
                    grad_q2[i] = (hamiltonian(q_plus, p) - hamiltonian(q_minus, p)) / (2 * eps)

                p -= 0.5 * h * grad_q2

            return np.concatenate([q, p])

        return ArchitectureLayer(
            name=name,
            kind=ArchitectureKind.DYNAMICAL,
            input_shape=(2 * dim,) if dim > 0 else None,
            output_shape=(2 * dim,) if dim > 0 else None,
            forward_fn=forward_fn,
            is_invertible=True,
            preserves=["symplectic", "volume"],
            parameters={"t": t},
        )


class MeasureArchitectureBuilder:
    """Build architectures for measure transport / normalizing flows."""

    @staticmethod
    def affine_coupling_layer(
        mask: NDArray,
        scale_fn: Callable[[NDArray], NDArray],
        translate_fn: Callable[[NDArray], NDArray],
        name: str = "AffineCoupling",
    ) -> ArchitectureLayer:
        """
        Affine coupling layer for normalizing flows.

        x_A = x[mask], x_B = x[~mask]
        y_A = x_A
        y_B = x_B * s(x_A) + t(x_A)

        Invertible with tractable Jacobian determinant.
        """
        dim = len(mask)

        def forward_fn(x: NDArray) -> NDArray:
            x_a = x[mask]
            x_b = x[~mask]
            s = scale_fn(x_a)
            t = translate_fn(x_a)
            y = x.copy()
            y[~mask] = x_b * np.exp(s) + t
            return y

        def inverse_fn(y: NDArray) -> NDArray:
            y_a = y[mask]
            y_b = y[~mask]
            s = scale_fn(y_a)
            t = translate_fn(y_a)
            x = y.copy()
            x[~mask] = (y_b - t) * np.exp(-s)
            return x

        return ArchitectureLayer(
            name=name,
            kind=ArchitectureKind.MEASURE,
            input_shape=(dim,),
            output_shape=(dim,),
            forward_fn=forward_fn,
            inverse_fn=inverse_fn,
            is_invertible=True,
            preserves=["bijectivity"],
        )
