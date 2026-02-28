"""
Grey Math — Experimental Mode.

A sandboxed mathematical research environment that exposes bleeding-edge
mathematical structures and operators beyond what current LLMs use.

Intended for:
- Designing new forms of intelligence
- Exploring unsupported mathematical structures
- Prototyping architectures based on advanced mathematics
- Enabling PhD-level mathematical reasoning

Experimental Mode is isolated, safe, and explicitly opt-in.

Usage:
    from greymath.experimental import ExperimentalMode

    with ExperimentalMode() as em:
        # All experimental features available here
        ...
"""

__version__ = "0.1.0-experimental"

from greymath.experimental.mode import (
    ExperimentalMode,
    ExperimentalContext,
    ExperimentalConfig,
    StabilityLevel,
    get_experimental_context,
    require_experimental_context,
)

__all__ = [
    # Core
    "ExperimentalMode",
    "ExperimentalContext",
    "ExperimentalConfig",
    "StabilityLevel",
    "get_experimental_context",
    "require_experimental_context",
]
