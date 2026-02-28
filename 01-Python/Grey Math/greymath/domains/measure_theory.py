"""
Measure Theory & Probability Module.

Provides:
- σ-algebras and measurable spaces
- Measures and probability measures
- Probability distributions (parametric families)
- Pushforward and pullback of measures
- Stochastic processes
- Information-theoretic quantities (entropy, KL divergence)
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Callable, Optional

import numpy as np
from numpy.typing import NDArray


# ─── Measurable Space ──────────────────────────────────────────────────────

@dataclass
class SigmaAlgebra:
    """
    A σ-algebra on a set: a collection of subsets closed under
    complement, countable union, and containing the empty set.
    """
    name: str = "F"
    base_set: str = "Ω"
    generators: list[str] = field(default_factory=list)
    is_borel: bool = False

    @staticmethod
    def borel(space: str = "R") -> "SigmaAlgebra":
        return SigmaAlgebra(name=f"B({space})", base_set=space, is_borel=True)

    def __repr__(self) -> str:
        return f"σ-algebra({self.name} on {self.base_set})"


@dataclass
class MeasurableSpace:
    """A pair (Ω, F) of a set and a σ-algebra."""
    omega: str = "Ω"
    sigma_algebra: SigmaAlgebra = field(default_factory=SigmaAlgebra)

    def __repr__(self) -> str:
        return f"({self.omega}, {self.sigma_algebra.name})"


# ─── Probability Distributions ─────────────────────────────────────────────

@dataclass
class Distribution:
    """
    A probability distribution, either parametric or defined by density.
    """
    name: str = ""
    parameters: dict[str, float] = field(default_factory=dict)
    dim: int = 1
    log_density: Optional[Callable[[NDArray], float]] = None
    sample_fn: Optional[Callable[[int], NDArray]] = None

    def log_prob(self, x: NDArray) -> float:
        if self.log_density:
            return self.log_density(x)
        raise NotImplementedError(f"No density for {self.name}")

    def sample(self, n: int, rng: Optional[np.random.Generator] = None) -> NDArray:
        if self.sample_fn:
            return self.sample_fn(n)
        raise NotImplementedError(f"No sampler for {self.name}")

    # ── Standard distributions ──────────────────────────────────────────

    @staticmethod
    def normal(mean: float = 0.0, std: float = 1.0) -> "Distribution":
        return Distribution(
            name="Normal",
            parameters={"mean": mean, "std": std},
            log_density=lambda x: float(
                -0.5 * ((x - mean) / std) ** 2
                - np.log(std) - 0.5 * np.log(2 * np.pi)
            ),
            sample_fn=lambda n: np.random.default_rng().normal(mean, std, n),
        )

    @staticmethod
    def multivariate_normal(mean: NDArray, cov: NDArray) -> "Distribution":
        dim = len(mean)
        L = np.linalg.cholesky(cov)
        cov_inv = np.linalg.inv(cov)
        log_det = np.linalg.slogdet(cov)[1]

        def log_density(x: NDArray) -> float:
            diff = x - mean
            return float(
                -0.5 * diff @ cov_inv @ diff
                - 0.5 * log_det
                - 0.5 * dim * np.log(2 * np.pi)
            )

        def sample_fn(n: int) -> NDArray:
            z = np.random.default_rng().standard_normal((n, dim))
            return z @ L.T + mean

        return Distribution(
            name="MultivariateNormal",
            parameters={"mean": mean.tolist(), "cov": "matrix"},
            dim=dim,
            log_density=log_density,
            sample_fn=sample_fn,
        )

    @staticmethod
    def uniform(a: float = 0.0, b: float = 1.0) -> "Distribution":
        return Distribution(
            name="Uniform",
            parameters={"a": a, "b": b},
            log_density=lambda x: float(
                -np.log(b - a) if a <= x <= b else -np.inf
            ),
            sample_fn=lambda n: np.random.default_rng().uniform(a, b, n),
        )

    @staticmethod
    def exponential(rate: float = 1.0) -> "Distribution":
        return Distribution(
            name="Exponential",
            parameters={"rate": rate},
            log_density=lambda x: float(
                np.log(rate) - rate * x if x >= 0 else -np.inf
            ),
            sample_fn=lambda n: np.random.default_rng().exponential(1 / rate, n),
        )

    def __repr__(self) -> str:
        return f"Distribution({self.name}, {self.parameters})"


# ─── Measure Operations ────────────────────────────────────────────────────

class MeasureOps:
    """Operations on measures and distributions."""

    @staticmethod
    def pushforward(dist: Distribution,
                    transform: Callable[[NDArray], NDArray],
                    dim_out: Optional[int] = None) -> Distribution:
        """
        Pushforward measure: T_# μ.

        Given μ and T: X → Y, compute the distribution of T(X) when X ~ μ.
        """
        def sample_fn(n: int) -> NDArray:
            x_samples = dist.sample(n)
            if x_samples.ndim == 1:
                return np.array([transform(x) for x in x_samples])
            return np.array([transform(x) for x in x_samples])

        return Distribution(
            name=f"Pushforward({dist.name})",
            dim=dim_out or dist.dim,
            sample_fn=sample_fn,
        )

    @staticmethod
    def kl_divergence(p: Distribution, q: Distribution,
                      n_samples: int = 10000) -> float:
        """
        Estimate KL(P || Q) = E_P[log P(x) - log Q(x)] via Monte Carlo.
        """
        samples = p.sample(n_samples)
        if samples.ndim == 1:
            log_ratios = np.array([
                p.log_prob(x) - q.log_prob(x) for x in samples
            ])
        else:
            log_ratios = np.array([
                p.log_prob(x) - q.log_prob(x) for x in samples
            ])
        return float(np.mean(log_ratios))

    @staticmethod
    def entropy(dist: Distribution, n_samples: int = 10000) -> float:
        """Estimate H(P) = -E_P[log P(x)] via Monte Carlo."""
        samples = dist.sample(n_samples)
        if samples.ndim == 1:
            log_probs = np.array([dist.log_prob(x) for x in samples])
        else:
            log_probs = np.array([dist.log_prob(x) for x in samples])
        return float(-np.mean(log_probs))

    @staticmethod
    def wasserstein_1d(samples_p: NDArray, samples_q: NDArray) -> float:
        """1D Wasserstein distance via sorted samples."""
        sp = np.sort(samples_p)
        sq = np.sort(samples_q)
        n = min(len(sp), len(sq))
        return float(np.mean(np.abs(sp[:n] - sq[:n])))

    @staticmethod
    def mixture(distributions: list[Distribution],
                weights: list[float]) -> Distribution:
        """Create a mixture distribution."""
        weights_arr = np.array(weights)
        weights_arr = weights_arr / weights_arr.sum()

        def sample_fn(n: int) -> NDArray:
            rng = np.random.default_rng()
            indices = rng.choice(len(distributions), size=n, p=weights_arr)
            samples = []
            for idx in indices:
                s = distributions[idx].sample(1)
                samples.append(s[0] if s.ndim > 0 else s)
            return np.array(samples)

        return Distribution(
            name=f"Mixture({len(distributions)} components)",
            sample_fn=sample_fn,
        )
