"""
Experimental Mode controller — activation, context management, and isolation.

ExperimentalMode is the top-level entry point for the experimental subsystem.
It manages opt-in activation, resource limits, and provides a context manager
that guarantees teardown and isolation from the main IDE.
"""

from __future__ import annotations

import logging
import threading
import time
from contextlib import contextmanager
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Callable, Optional

logger = logging.getLogger(__name__)


class StabilityLevel(Enum):
    """Stability classification for experimental features."""
    STABLE = auto()       # Well-tested, reliable
    BETA = auto()         # Mostly stable, edge cases possible
    ALPHA = auto()        # Experimental, may have bugs
    UNSTABLE = auto()     # Research-grade, correctness not guaranteed
    DANGEROUS = auto()    # May produce incorrect results without warning


@dataclass
class ExperimentalConfig:
    """Configuration for an experimental mode session."""
    # Resource limits
    max_memory_mb: int = 2048
    max_cpu_seconds: float = 300.0
    max_matrix_dim: int = 10000
    max_iterations: int = 1_000_000

    # Precision
    default_precision_bits: int = 64
    enable_arbitrary_precision: bool = True
    enable_interval_arithmetic: bool = True

    # Safety
    allow_unstable_features: bool = False
    allow_dangerous_features: bool = False
    deterministic_mode: bool = False
    random_seed: Optional[int] = None

    # Error handling
    error_bounded_numerics: bool = True
    symbolic_verification: bool = True
    warn_on_ill_conditioned: bool = True
    condition_number_threshold: float = 1e12

    # GPU
    enable_gpu: bool = False
    gpu_device_id: int = 0


@dataclass
class ExperimentalContext:
    """
    Runtime context for an active experimental session.

    Tracks resources, warnings, and provides the gateway to all
    experimental subsystems.
    """
    config: ExperimentalConfig = field(default_factory=ExperimentalConfig)
    active: bool = False
    start_time: Optional[float] = None
    warnings: list[str] = field(default_factory=list)
    _resource_usage: dict[str, float] = field(default_factory=dict)
    _registered_cleanups: list[Callable] = field(default_factory=list)

    def warn(self, message: str) -> None:
        """Record a warning in the experimental context."""
        self.warnings.append(message)
        logger.warning(f"[Experimental] {message}")

    def check_stability(self, level: StabilityLevel, feature_name: str) -> None:
        """Gate access to features based on stability level."""
        if level == StabilityLevel.DANGEROUS and not self.config.allow_dangerous_features:
            raise ExperimentalAccessError(
                f"Feature '{feature_name}' is classified as DANGEROUS. "
                f"Set allow_dangerous_features=True to use it."
            )
        if level == StabilityLevel.UNSTABLE and not self.config.allow_unstable_features:
            raise ExperimentalAccessError(
                f"Feature '{feature_name}' is classified as UNSTABLE. "
                f"Set allow_unstable_features=True to use it."
            )
        if level in (StabilityLevel.ALPHA, StabilityLevel.UNSTABLE, StabilityLevel.DANGEROUS):
            self.warn(
                f"Using {level.name} feature: {feature_name}. "
                f"Results may not be mathematically verified."
            )

    def check_resource_limit(self, resource: str, value: float) -> None:
        """Check that a resource usage is within configured limits."""
        self._resource_usage[resource] = self._resource_usage.get(resource, 0) + value
        if resource == "memory_mb" and self._resource_usage[resource] > self.config.max_memory_mb:
            raise ExperimentalResourceError(
                f"Memory limit exceeded: {self._resource_usage[resource]:.1f}MB "
                f"> {self.config.max_memory_mb}MB"
            )
        if resource == "cpu_seconds":
            elapsed = time.time() - (self.start_time or time.time())
            if elapsed > self.config.max_cpu_seconds:
                raise ExperimentalResourceError(
                    f"CPU time limit exceeded: {elapsed:.1f}s "
                    f"> {self.config.max_cpu_seconds}s"
                )

    def register_cleanup(self, fn: Callable) -> None:
        """Register a cleanup function to be called on teardown."""
        self._registered_cleanups.append(fn)

    def elapsed_seconds(self) -> float:
        """Time elapsed since session start."""
        if self.start_time is None:
            return 0.0
        return time.time() - self.start_time


class ExperimentalAccessError(Exception):
    """Raised when accessing a gated experimental feature."""
    pass


class ExperimentalResourceError(Exception):
    """Raised when resource limits are exceeded."""
    pass


# Thread-local storage for the active experimental context
_thread_local = threading.local()


def get_experimental_context() -> Optional[ExperimentalContext]:
    """Get the currently active experimental context, or None."""
    return getattr(_thread_local, "experimental_context", None)


def require_experimental_context() -> ExperimentalContext:
    """Get the active context, raising if not in experimental mode."""
    ctx = get_experimental_context()
    if ctx is None or not ctx.active:
        raise ExperimentalAccessError(
            "This feature requires Experimental Mode. "
            "Use 'with ExperimentalMode() as em:' to activate."
        )
    return ctx


class ExperimentalMode:
    """
    Top-level controller for Grey Math Experimental Mode.

    Use as a context manager to activate experimental features:

        with ExperimentalMode(allow_unstable_features=True) as em:
            # experimental features available
            from greymath.experimental.pde import PDEOperator
            ...

    All resources are cleaned up on exit. The main IDE is unaffected.
    """

    def __init__(self, **config_kwargs: Any) -> None:
        self.config = ExperimentalConfig(**config_kwargs)
        self._context: Optional[ExperimentalContext] = None
        self._previous_context: Optional[ExperimentalContext] = None

    def __enter__(self) -> ExperimentalContext:
        # Save any existing context (for nesting)
        self._previous_context = get_experimental_context()

        self._context = ExperimentalContext(
            config=self.config,
            active=True,
            start_time=time.time(),
        )
        _thread_local.experimental_context = self._context
        logger.info("[Experimental Mode] Activated")

        if self.config.deterministic_mode and self.config.random_seed is not None:
            import numpy as np
            np.random.seed(self.config.random_seed)

        return self._context

    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> bool:
        if self._context is not None:
            # Run cleanups in reverse order
            for cleanup in reversed(self._context._registered_cleanups):
                try:
                    cleanup()
                except Exception as e:
                    logger.error(f"[Experimental] Cleanup error: {e}")

            elapsed = self._context.elapsed_seconds()
            n_warnings = len(self._context.warnings)
            self._context.active = False
            logger.info(
                f"[Experimental Mode] Deactivated after {elapsed:.2f}s "
                f"({n_warnings} warnings)"
            )

        # Restore previous context
        _thread_local.experimental_context = self._previous_context
        return False  # Don't suppress exceptions
