"""
Grey Math — Property-Based Testing for Mathematical Identities.

Uses random sampling and Hypothesis-style generation to verify
mathematical properties hold across domains.
"""

from __future__ import annotations

import random
import math
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Callable, Optional

import numpy as np


class PropertyKind(Enum):
    """Kinds of mathematical properties to verify."""
    IDENTITY = auto()         # f(x) == g(x) for all x
    INEQUALITY = auto()       # f(x) <= g(x) for all x
    IDEMPOTENT = auto()       # f(f(x)) == f(x)
    INVOLUTION = auto()       # f(f(x)) == x
    COMMUTATIVE = auto()      # f(x,y) == f(y,x)
    ASSOCIATIVE = auto()      # f(f(x,y),z) == f(x,f(y,z))
    DISTRIBUTIVE = auto()     # f(x, g(y,z)) == g(f(x,y), f(x,z))
    MONOTONE = auto()         # x <= y => f(x) <= f(y)
    POSITIVE_DEFINITE = auto()  # f(x) > 0 for x != 0
    BOUNDED = auto()          # |f(x)| <= M
    FIXED_POINT = auto()      # f(x*) == x*
    CONVERGENT = auto()       # lim f^n(x) exists


@dataclass
class MathProperty:
    """A mathematical property to verify."""
    name: str
    kind: PropertyKind
    function: Callable
    reference: Optional[Callable] = None  # For identity checks
    domain_generator: Optional[Callable] = None  # Custom domain sampling
    tolerance: float = 1e-10
    bound: Optional[float] = None  # For BOUNDED checks
    description: str = ""


@dataclass
class VerificationResult:
    """Result of a property verification."""
    property_name: str
    verified: bool
    n_samples: int
    n_failures: int
    counterexample: Any = None
    max_error: float = 0.0
    mean_error: float = 0.0
    details: str = ""


class DomainGenerator:
    """Generates samples from standard mathematical domains."""

    @staticmethod
    def real_scalars(n: int, low: float = -100, high: float = 100) -> list[float]:
        return [random.uniform(low, high) for _ in range(n)]

    @staticmethod
    def positive_reals(n: int, low: float = 1e-6, high: float = 100) -> list[float]:
        return [random.uniform(low, high) for _ in range(n)]

    @staticmethod
    def unit_interval(n: int) -> list[float]:
        return [random.random() for _ in range(n)]

    @staticmethod
    def integers(n: int, low: int = -100, high: int = 100) -> list[int]:
        return [random.randint(low, high) for _ in range(n)]

    @staticmethod
    def vectors(n: int, dim: int = 3, low: float = -10, high: float = 10) -> list[np.ndarray]:
        return [np.random.uniform(low, high, dim) for _ in range(n)]

    @staticmethod
    def matrices(n: int, rows: int = 3, cols: int = 3) -> list[np.ndarray]:
        return [np.random.randn(rows, cols) for _ in range(n)]

    @staticmethod
    def symmetric_matrices(n: int, dim: int = 3) -> list[np.ndarray]:
        result = []
        for _ in range(n):
            A = np.random.randn(dim, dim)
            result.append((A + A.T) / 2)
        return result

    @staticmethod
    def positive_definite_matrices(n: int, dim: int = 3) -> list[np.ndarray]:
        result = []
        for _ in range(n):
            A = np.random.randn(dim, dim)
            result.append(A @ A.T + np.eye(dim) * 0.1)
        return result

    @staticmethod
    def orthogonal_matrices(n: int, dim: int = 3) -> list[np.ndarray]:
        result = []
        for _ in range(n):
            A = np.random.randn(dim, dim)
            Q, _ = np.linalg.qr(A)
            result.append(Q)
        return result

    @staticmethod
    def unit_vectors(n: int, dim: int = 3) -> list[np.ndarray]:
        result = []
        for _ in range(n):
            v = np.random.randn(dim)
            result.append(v / np.linalg.norm(v))
        return result


class PropertyChecker:
    """
    Verifies mathematical properties using randomized testing.
    """

    def __init__(self, n_samples: int = 1000, seed: int | None = None) -> None:
        self.n_samples = n_samples
        if seed is not None:
            random.seed(seed)
            np.random.seed(seed)

    def check(self, prop: MathProperty) -> VerificationResult:
        """Check a mathematical property."""
        dispatch = {
            PropertyKind.IDENTITY: self._check_identity,
            PropertyKind.INEQUALITY: self._check_inequality,
            PropertyKind.IDEMPOTENT: self._check_idempotent,
            PropertyKind.INVOLUTION: self._check_involution,
            PropertyKind.COMMUTATIVE: self._check_commutative,
            PropertyKind.ASSOCIATIVE: self._check_associative,
            PropertyKind.MONOTONE: self._check_monotone,
            PropertyKind.POSITIVE_DEFINITE: self._check_positive_definite,
            PropertyKind.BOUNDED: self._check_bounded,
        }

        checker = dispatch.get(prop.kind)
        if checker is None:
            return VerificationResult(
                property_name=prop.name,
                verified=False,
                n_samples=0,
                n_failures=0,
                details=f"Unsupported property kind: {prop.kind}",
            )

        return checker(prop)

    def check_all(self, properties: list[MathProperty]) -> list[VerificationResult]:
        """Check multiple properties."""
        return [self.check(p) for p in properties]

    def _generate_samples(self, prop: MathProperty) -> list:
        if prop.domain_generator is not None:
            return prop.domain_generator(self.n_samples)
        return DomainGenerator.real_scalars(self.n_samples)

    def _check_identity(self, prop: MathProperty) -> VerificationResult:
        """Check f(x) == g(x) for all x."""
        if prop.reference is None:
            return VerificationResult(
                property_name=prop.name, verified=False,
                n_samples=0, n_failures=0,
                details="Identity check requires a reference function",
            )

        samples = self._generate_samples(prop)
        failures = 0
        max_error = 0.0
        errors = []
        counterexample = None

        for x in samples:
            try:
                a = prop.function(x)
                b = prop.reference(x)
                err = self._compute_error(a, b)
                errors.append(err)
                if err > max_error:
                    max_error = err
                if err > prop.tolerance:
                    failures += 1
                    if counterexample is None:
                        counterexample = {"x": x, "f(x)": a, "g(x)": b, "error": err}
            except Exception:
                pass

        return VerificationResult(
            property_name=prop.name,
            verified=(failures == 0),
            n_samples=len(samples),
            n_failures=failures,
            counterexample=counterexample,
            max_error=max_error,
            mean_error=np.mean(errors) if errors else 0.0,
        )

    def _check_idempotent(self, prop: MathProperty) -> VerificationResult:
        """Check f(f(x)) == f(x)."""
        samples = self._generate_samples(prop)
        failures = 0
        max_error = 0.0
        counterexample = None

        for x in samples:
            try:
                fx = prop.function(x)
                ffx = prop.function(fx)
                err = self._compute_error(fx, ffx)
                if err > max_error:
                    max_error = err
                if err > prop.tolerance:
                    failures += 1
                    if counterexample is None:
                        counterexample = {"x": x, "f(x)": fx, "f(f(x))": ffx}
            except Exception:
                pass

        return VerificationResult(
            property_name=prop.name,
            verified=(failures == 0),
            n_samples=len(samples),
            n_failures=failures,
            counterexample=counterexample,
            max_error=max_error,
        )

    def _check_involution(self, prop: MathProperty) -> VerificationResult:
        """Check f(f(x)) == x."""
        samples = self._generate_samples(prop)
        failures = 0
        max_error = 0.0
        counterexample = None

        for x in samples:
            try:
                ffx = prop.function(prop.function(x))
                err = self._compute_error(x, ffx)
                if err > max_error:
                    max_error = err
                if err > prop.tolerance:
                    failures += 1
                    if counterexample is None:
                        counterexample = {"x": x, "f(f(x))": ffx}
            except Exception:
                pass

        return VerificationResult(
            property_name=prop.name,
            verified=(failures == 0),
            n_samples=len(samples),
            n_failures=failures,
            counterexample=counterexample,
            max_error=max_error,
        )

    def _check_commutative(self, prop: MathProperty) -> VerificationResult:
        """Check f(x,y) == f(y,x)."""
        samples = self._generate_samples(prop)
        failures = 0
        max_error = 0.0
        counterexample = None
        n = len(samples)

        for i in range(min(n - 1, self.n_samples)):
            x, y = samples[i], samples[(i + 1) % n]
            try:
                a = prop.function(x, y)
                b = prop.function(y, x)
                err = self._compute_error(a, b)
                if err > max_error:
                    max_error = err
                if err > prop.tolerance:
                    failures += 1
                    if counterexample is None:
                        counterexample = {"x": x, "y": y, "f(x,y)": a, "f(y,x)": b}
            except Exception:
                pass

        return VerificationResult(
            property_name=prop.name,
            verified=(failures == 0),
            n_samples=min(n - 1, self.n_samples),
            n_failures=failures,
            counterexample=counterexample,
            max_error=max_error,
        )

    def _check_associative(self, prop: MathProperty) -> VerificationResult:
        """Check f(f(x,y),z) == f(x,f(y,z))."""
        samples = self._generate_samples(prop)
        failures = 0
        max_error = 0.0
        counterexample = None
        n = len(samples)

        for i in range(min(n - 2, self.n_samples)):
            x, y, z = samples[i], samples[(i + 1) % n], samples[(i + 2) % n]
            try:
                left = prop.function(prop.function(x, y), z)
                right = prop.function(x, prop.function(y, z))
                err = self._compute_error(left, right)
                if err > max_error:
                    max_error = err
                if err > prop.tolerance:
                    failures += 1
                    if counterexample is None:
                        counterexample = {"x": x, "y": y, "z": z}
            except Exception:
                pass

        return VerificationResult(
            property_name=prop.name,
            verified=(failures == 0),
            n_samples=min(n - 2, self.n_samples),
            n_failures=failures,
            counterexample=counterexample,
            max_error=max_error,
        )

    def _check_inequality(self, prop: MathProperty) -> VerificationResult:
        """Check f(x) <= g(x) for all x."""
        if prop.reference is None:
            return VerificationResult(
                property_name=prop.name, verified=False,
                n_samples=0, n_failures=0,
                details="Inequality check requires a reference function",
            )

        samples = self._generate_samples(prop)
        failures = 0
        counterexample = None

        for x in samples:
            try:
                a = prop.function(x)
                b = prop.reference(x)
                if isinstance(a, np.ndarray):
                    if not np.all(a <= b + prop.tolerance):
                        failures += 1
                        if counterexample is None:
                            counterexample = {"x": x, "f(x)": a, "g(x)": b}
                else:
                    if a > b + prop.tolerance:
                        failures += 1
                        if counterexample is None:
                            counterexample = {"x": x, "f(x)": a, "g(x)": b}
            except Exception:
                pass

        return VerificationResult(
            property_name=prop.name,
            verified=(failures == 0),
            n_samples=len(samples),
            n_failures=failures,
            counterexample=counterexample,
        )

    def _check_monotone(self, prop: MathProperty) -> VerificationResult:
        """Check x <= y => f(x) <= f(y)."""
        samples = sorted(self._generate_samples(prop))
        failures = 0
        counterexample = None

        for i in range(len(samples) - 1):
            x, y = samples[i], samples[i + 1]
            try:
                fx = prop.function(x)
                fy = prop.function(y)
                if fx > fy + prop.tolerance:
                    failures += 1
                    if counterexample is None:
                        counterexample = {"x": x, "y": y, "f(x)": fx, "f(y)": fy}
            except Exception:
                pass

        return VerificationResult(
            property_name=prop.name,
            verified=(failures == 0),
            n_samples=len(samples) - 1,
            n_failures=failures,
            counterexample=counterexample,
        )

    def _check_positive_definite(self, prop: MathProperty) -> VerificationResult:
        """Check f(x) > 0 for x != 0."""
        samples = self._generate_samples(prop)
        failures = 0
        counterexample = None

        for x in samples:
            try:
                is_zero = (np.linalg.norm(x) < prop.tolerance
                           if isinstance(x, np.ndarray)
                           else abs(x) < prop.tolerance)
                if is_zero:
                    continue
                val = prop.function(x)
                if val <= 0:
                    failures += 1
                    if counterexample is None:
                        counterexample = {"x": x, "f(x)": val}
            except Exception:
                pass

        return VerificationResult(
            property_name=prop.name,
            verified=(failures == 0),
            n_samples=len(samples),
            n_failures=failures,
            counterexample=counterexample,
        )

    def _check_bounded(self, prop: MathProperty) -> VerificationResult:
        """Check |f(x)| <= M."""
        if prop.bound is None:
            return VerificationResult(
                property_name=prop.name, verified=False,
                n_samples=0, n_failures=0,
                details="Bounded check requires a bound value",
            )

        samples = self._generate_samples(prop)
        failures = 0
        max_val = 0.0
        counterexample = None

        for x in samples:
            try:
                val = prop.function(x)
                absval = abs(val) if not isinstance(val, np.ndarray) else np.linalg.norm(val)
                if absval > max_val:
                    max_val = absval
                if absval > prop.bound + prop.tolerance:
                    failures += 1
                    if counterexample is None:
                        counterexample = {"x": x, "|f(x)|": absval, "bound": prop.bound}
            except Exception:
                pass

        return VerificationResult(
            property_name=prop.name,
            verified=(failures == 0),
            n_samples=len(samples),
            n_failures=failures,
            counterexample=counterexample,
            max_error=max_val,
        )

    @staticmethod
    def _compute_error(a: Any, b: Any) -> float:
        """Compute error between two values."""
        if isinstance(a, np.ndarray) and isinstance(b, np.ndarray):
            return float(np.linalg.norm(a - b))
        try:
            return abs(float(a) - float(b))
        except (TypeError, ValueError):
            return 0.0 if a == b else float("inf")
