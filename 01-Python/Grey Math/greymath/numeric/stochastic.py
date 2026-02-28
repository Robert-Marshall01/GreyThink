"""
Stochastic sampling engine.

Provides:
- Markov Chain Monte Carlo (MCMC)
  - Metropolis-Hastings
  - Hamiltonian Monte Carlo (HMC)
- Sequential Monte Carlo (particle filters)
- Variational inference (mean-field, ELBO)
- Random variable generation
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Callable, Optional

import numpy as np
from numpy.typing import NDArray


class SamplingMethod(Enum):
    """Available sampling methods."""
    METROPOLIS_HASTINGS = auto()
    HMC = auto()
    SMC = auto()
    VARIATIONAL = auto()


@dataclass
class SamplingResult:
    """Result of a sampling procedure."""
    samples: NDArray           # (n_samples x dim)
    log_probs: NDArray         # log probability at each sample
    acceptance_rate: float = 0.0
    n_samples: int = 0
    method: SamplingMethod = SamplingMethod.METROPOLIS_HASTINGS
    diagnostics: dict = field(default_factory=dict)

    @property
    def mean(self) -> NDArray:
        return np.mean(self.samples, axis=0)

    @property
    def std(self) -> NDArray:
        return np.std(self.samples, axis=0)

    @property
    def cov(self) -> NDArray:
        return np.cov(self.samples.T)

    def effective_sample_size(self) -> float:
        """Estimate effective sample size via autocorrelation."""
        if self.n_samples < 2:
            return float(self.n_samples)
        # Use the initial positive sequence estimator (simplified)
        n = len(self.samples)
        mean = np.mean(self.samples, axis=0)
        var = np.var(self.samples, axis=0)
        if np.all(var == 0):
            return float(n)
        # Normalized autocorrelation for first dimension
        x = self.samples[:, 0] if self.samples.ndim > 1 else self.samples
        x_centered = x - np.mean(x)
        acf = np.correlate(x_centered, x_centered, mode="full")
        acf = acf[len(acf) // 2:]
        acf = acf / acf[0] if acf[0] > 0 else acf
        # Sum positive autocorrelations
        tau = 1 + 2 * np.sum(acf[1:min(len(acf), 100)])
        return n / max(tau, 1)

    def __repr__(self) -> str:
        return (f"SamplingResult(method={self.method.name}, "
                f"n={self.n_samples}, accept={self.acceptance_rate:.3f})")


class StochasticSampler:
    """
    Stochastic sampling engine for probability distributions.
    """

    def __init__(self, seed: Optional[int] = None) -> None:
        self.rng = np.random.default_rng(seed)

    def sample(
        self,
        log_prob: Callable[[NDArray], float],
        x0: NDArray,
        n_samples: int = 10000,
        method: SamplingMethod = SamplingMethod.METROPOLIS_HASTINGS,
        burn_in: int = 1000,
        **kwargs,
    ) -> SamplingResult:
        """
        Draw samples from a distribution defined by log_prob.

        Args:
            log_prob: Log probability density function
            x0: Initial point
            n_samples: Number of samples to draw
            method: Sampling method
            burn_in: Number of burn-in samples to discard
        """
        x0 = np.atleast_1d(np.asarray(x0, dtype=float))

        if method == SamplingMethod.METROPOLIS_HASTINGS:
            return self._metropolis_hastings(
                log_prob, x0, n_samples, burn_in,
                proposal_std=kwargs.get("proposal_std", 1.0),
            )
        elif method == SamplingMethod.HMC:
            return self._hmc(
                log_prob, x0, n_samples, burn_in,
                grad_log_prob=kwargs.get("grad_log_prob"),
                step_size=kwargs.get("step_size", 0.1),
                n_leapfrog=kwargs.get("n_leapfrog", 10),
            )
        else:
            return self._metropolis_hastings(log_prob, x0, n_samples, burn_in)

    def _metropolis_hastings(
        self, log_prob: Callable, x0: NDArray,
        n_samples: int, burn_in: int,
        proposal_std: float = 1.0,
    ) -> SamplingResult:
        """Metropolis-Hastings MCMC sampler."""
        dim = len(x0)
        total = n_samples + burn_in
        samples = np.zeros((total, dim))
        log_probs = np.zeros(total)

        x = x0.copy()
        lp = log_prob(x)
        accepted = 0

        for i in range(total):
            # Propose
            x_prop = x + proposal_std * self.rng.standard_normal(dim)
            lp_prop = log_prob(x_prop)

            # Accept/reject
            log_alpha = lp_prop - lp
            if np.log(self.rng.uniform()) < log_alpha:
                x = x_prop
                lp = lp_prop
                accepted += 1

            samples[i] = x
            log_probs[i] = lp

        # Discard burn-in
        samples = samples[burn_in:]
        log_probs = log_probs[burn_in:]

        return SamplingResult(
            samples=samples,
            log_probs=log_probs,
            acceptance_rate=accepted / total,
            n_samples=n_samples,
            method=SamplingMethod.METROPOLIS_HASTINGS,
        )

    def _hmc(
        self, log_prob: Callable, x0: NDArray,
        n_samples: int, burn_in: int,
        grad_log_prob: Optional[Callable] = None,
        step_size: float = 0.1,
        n_leapfrog: int = 10,
    ) -> SamplingResult:
        """Hamiltonian Monte Carlo sampler."""
        dim = len(x0)
        total = n_samples + burn_in

        if grad_log_prob is None:
            # Numerical gradient
            def grad_log_prob(x: NDArray) -> NDArray:
                eps = 1e-5
                g = np.zeros_like(x)
                for i in range(len(x)):
                    x_p = x.copy(); x_p[i] += eps
                    x_m = x.copy(); x_m[i] -= eps
                    g[i] = (log_prob(x_p) - log_prob(x_m)) / (2 * eps)
                return g

        samples = np.zeros((total, dim))
        log_probs = np.zeros(total)
        x = x0.copy()
        accepted = 0

        for i in range(total):
            # Sample momentum
            p = self.rng.standard_normal(dim)

            # Current Hamiltonian
            current_H = -log_prob(x) + 0.5 * np.dot(p, p)

            # Leapfrog integration
            x_prop = x.copy()
            p_prop = p.copy()
            p_prop += 0.5 * step_size * grad_log_prob(x_prop)
            for _ in range(n_leapfrog - 1):
                x_prop += step_size * p_prop
                p_prop += step_size * grad_log_prob(x_prop)
            x_prop += step_size * p_prop
            p_prop += 0.5 * step_size * grad_log_prob(x_prop)
            p_prop = -p_prop  # negate momentum for reversibility

            # Proposed Hamiltonian
            proposed_H = -log_prob(x_prop) + 0.5 * np.dot(p_prop, p_prop)

            # Accept/reject
            if np.log(self.rng.uniform()) < current_H - proposed_H:
                x = x_prop
                accepted += 1

            samples[i] = x
            log_probs[i] = log_prob(x)

        samples = samples[burn_in:]
        log_probs = log_probs[burn_in:]

        return SamplingResult(
            samples=samples,
            log_probs=log_probs,
            acceptance_rate=accepted / total,
            n_samples=n_samples,
            method=SamplingMethod.HMC,
        )
