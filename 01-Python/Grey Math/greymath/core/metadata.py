"""
Metadata system for the Grey Math IR.

Every mathematical object can carry metadata about:
- Precision (floating-point, arbitrary, interval)
- Numerical stability information
- Domain assumptions and constraints
- Provenance and lineage
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Optional


class PrecisionMode(Enum):
    """Precision modes for numerical computation."""
    FLOAT32 = auto()
    FLOAT64 = auto()
    FLOAT128 = auto()
    ARBITRARY = auto()
    INTERVAL = auto()
    EXACT = auto()


class StabilityRating(Enum):
    """Numerical stability classification."""
    STABLE = auto()
    CONDITIONALLY_STABLE = auto()
    UNSTABLE = auto()
    UNKNOWN = auto()


@dataclass
class Precision:
    """Precision specification for a mathematical object."""
    mode: PrecisionMode = PrecisionMode.FLOAT64
    bits: Optional[int] = None  # for arbitrary precision
    relative_error: Optional[float] = None
    absolute_error: Optional[float] = None

    def __repr__(self) -> str:
        if self.mode == PrecisionMode.ARBITRARY:
            return f"Precision(arbitrary, {self.bits} bits)"
        return f"Precision({self.mode.name})"


@dataclass
class StabilityInfo:
    """Numerical stability information."""
    rating: StabilityRating = StabilityRating.UNKNOWN
    condition_number: Optional[float] = None
    sensitivity: Optional[float] = None
    notes: list[str] = field(default_factory=list)

    def is_well_conditioned(self, threshold: float = 1e10) -> bool:
        if self.condition_number is None:
            return self.rating == StabilityRating.STABLE
        return self.condition_number < threshold

    def __repr__(self) -> str:
        return f"Stability({self.rating.name}, κ={self.condition_number})"


@dataclass
class DomainAssumption:
    """A domain assumption or constraint on a mathematical object."""
    name: str = ""
    description: str = ""
    is_verified: bool = False
    expression: Optional[str] = None  # symbolic constraint

    def __repr__(self) -> str:
        verified = "✓" if self.is_verified else "?"
        return f"Assumption({self.name} [{verified}])"


@dataclass
class Provenance:
    """Tracks how a mathematical object was derived."""
    source: Optional[str] = None
    operation: Optional[str] = None
    timestamp: Optional[str] = None
    parent_ids: list[str] = field(default_factory=list)

    def __repr__(self) -> str:
        return f"Provenance(op={self.operation}, from={self.parent_ids})"


@dataclass
class Metadata:
    """
    Comprehensive metadata for any mathematical IR object.

    Tracks precision, stability, domain assumptions, provenance,
    and arbitrary key-value annotations.
    """
    precision: Precision = field(default_factory=Precision)
    stability: StabilityInfo = field(default_factory=StabilityInfo)
    assumptions: list[DomainAssumption] = field(default_factory=list)
    provenance: Optional[Provenance] = None
    tags: set[str] = field(default_factory=set)
    annotations: dict[str, Any] = field(default_factory=dict)

    def add_assumption(self, name: str, description: str = "",
                       verified: bool = False) -> None:
        self.assumptions.append(DomainAssumption(
            name=name, description=description, is_verified=verified
        ))

    def add_tag(self, tag: str) -> None:
        self.tags.add(tag)

    def annotate(self, key: str, value: Any) -> None:
        self.annotations[key] = value

    def __repr__(self) -> str:
        parts = [f"precision={self.precision.mode.name}"]
        if self.stability.rating != StabilityRating.UNKNOWN:
            parts.append(f"stability={self.stability.rating.name}")
        if self.tags:
            parts.append(f"tags={self.tags}")
        return f"Metadata({', '.join(parts)})"
