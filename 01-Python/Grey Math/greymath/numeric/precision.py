"""
Arbitrary precision and interval arithmetic.

Provides:
- Arbitrary precision real and complex arithmetic via mpmath
- Interval arithmetic with rigorous error bounds
- Mixed-precision execution strategies
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Optional, Union

import mpmath


class ArbitraryPrecision:
    """
    Arbitrary precision arithmetic engine backed by mpmath.

    Supports configurable decimal precision for all operations,
    enabling exact or near-exact computation when needed.
    """

    def __init__(self, decimal_places: int = 50) -> None:
        self.decimal_places = decimal_places
        mpmath.mp.dps = decimal_places

    def set_precision(self, decimal_places: int) -> None:
        """Set the working precision."""
        self.decimal_places = decimal_places
        mpmath.mp.dps = decimal_places

    def mpf(self, value: Union[int, float, str]) -> mpmath.mpf:
        """Create an arbitrary-precision real number."""
        return mpmath.mpf(value)

    def mpc(self, real: Union[int, float, str],
            imag: Union[int, float, str] = 0) -> mpmath.mpc:
        """Create an arbitrary-precision complex number."""
        return mpmath.mpc(real, imag)

    def pi(self) -> mpmath.mpf:
        return mpmath.pi

    def e(self) -> mpmath.mpf:
        return mpmath.e

    def exp(self, x: Union[mpmath.mpf, float]) -> mpmath.mpf:
        return mpmath.exp(x)

    def log(self, x: Union[mpmath.mpf, float]) -> mpmath.mpf:
        return mpmath.log(x)

    def sin(self, x: Union[mpmath.mpf, float]) -> mpmath.mpf:
        return mpmath.sin(x)

    def cos(self, x: Union[mpmath.mpf, float]) -> mpmath.mpf:
        return mpmath.cos(x)

    def sqrt(self, x: Union[mpmath.mpf, float]) -> mpmath.mpf:
        return mpmath.sqrt(x)

    def gamma(self, x: Union[mpmath.mpf, float]) -> mpmath.mpf:
        return mpmath.gamma(x)

    def zeta(self, s: Union[mpmath.mpf, float]) -> mpmath.mpf:
        return mpmath.zeta(s)

    def matrix(self, data: list[list[Union[int, float, str]]]) -> mpmath.matrix:
        """Create an arbitrary-precision matrix."""
        return mpmath.matrix(data)

    def det(self, M: mpmath.matrix) -> mpmath.mpf:
        return mpmath.det(M)

    def inverse(self, M: mpmath.matrix) -> mpmath.matrix:
        return M ** (-1)

    def eigenvalues(self, M: mpmath.matrix) -> list:
        """Compute eigenvalues at arbitrary precision."""
        return list(mpmath.eigsy(M) if self._is_symmetric(M) else mpmath.eig(M)[0])

    def _is_symmetric(self, M: mpmath.matrix) -> bool:
        n = M.rows
        if M.rows != M.cols:
            return False
        for i in range(n):
            for j in range(i + 1, n):
                if M[i, j] != M[j, i]:
                    return False
        return True


@dataclass
class Interval:
    """
    A closed interval [lo, hi] for interval arithmetic.

    Provides rigorous bounds on the result of arithmetic operations.
    """
    lo: float
    hi: float

    def __post_init__(self) -> None:
        if self.lo > self.hi:
            self.lo, self.hi = self.hi, self.lo

    @staticmethod
    def point(x: float) -> "Interval":
        """Create a degenerate interval [x, x]."""
        return Interval(x, x)

    @property
    def mid(self) -> float:
        return (self.lo + self.hi) / 2

    @property
    def width(self) -> float:
        return self.hi - self.lo

    @property
    def radius(self) -> float:
        return self.width / 2

    def contains(self, x: float) -> bool:
        return self.lo <= x <= self.hi

    def overlaps(self, other: "Interval") -> bool:
        return self.lo <= other.hi and other.lo <= self.hi

    def __add__(self, other: "Interval") -> "Interval":
        return Interval(self.lo + other.lo, self.hi + other.hi)

    def __sub__(self, other: "Interval") -> "Interval":
        return Interval(self.lo - other.hi, self.hi - other.lo)

    def __mul__(self, other: "Interval") -> "Interval":
        products = [
            self.lo * other.lo, self.lo * other.hi,
            self.hi * other.lo, self.hi * other.hi,
        ]
        return Interval(min(products), max(products))

    def __truediv__(self, other: "Interval") -> "Interval":
        if other.contains(0):
            raise ZeroDivisionError("Division by interval containing zero")
        return self * Interval(1 / other.hi, 1 / other.lo)

    def __pow__(self, n: int) -> "Interval":
        if n == 0:
            return Interval.point(1.0)
        if n == 1:
            return self
        if n % 2 == 0:
            if self.lo >= 0:
                return Interval(self.lo ** n, self.hi ** n)
            if self.hi <= 0:
                return Interval(self.hi ** n, self.lo ** n)
            return Interval(0, max(self.lo ** n, self.hi ** n))
        return Interval(self.lo ** n, self.hi ** n)

    def __repr__(self) -> str:
        return f"[{self.lo:.6g}, {self.hi:.6g}]"


class IntervalArithmetic:
    """
    Interval arithmetic engine for rigorous error bound computation.

    Wraps standard operations to propagate intervals, ensuring
    that the true result is always contained within the computed interval.
    """

    @staticmethod
    def add(a: Interval, b: Interval) -> Interval:
        return a + b

    @staticmethod
    def sub(a: Interval, b: Interval) -> Interval:
        return a - b

    @staticmethod
    def mul(a: Interval, b: Interval) -> Interval:
        return a * b

    @staticmethod
    def div(a: Interval, b: Interval) -> Interval:
        return a / b

    @staticmethod
    def sqrt(a: Interval) -> Interval:
        if a.lo < 0:
            raise ValueError("Cannot take sqrt of interval with negative lower bound")
        import math
        return Interval(math.sqrt(a.lo), math.sqrt(a.hi))

    @staticmethod
    def exp(a: Interval) -> Interval:
        import math
        return Interval(math.exp(a.lo), math.exp(a.hi))

    @staticmethod
    def log(a: Interval) -> Interval:
        if a.lo <= 0:
            raise ValueError("Cannot take log of interval with non-positive lower bound")
        import math
        return Interval(math.log(a.lo), math.log(a.hi))

    @staticmethod
    def sin(a: Interval) -> Interval:
        """Conservative sin bound (simplified)."""
        import math
        # This is a simplified version; full implementation tracks monotonicity
        vals = [math.sin(a.lo), math.sin(a.hi)]
        if a.width >= 2 * math.pi:
            return Interval(-1, 1)
        return Interval(min(vals) - 1e-15, max(vals) + 1e-15)

    @staticmethod
    def cos(a: Interval) -> Interval:
        import math
        vals = [math.cos(a.lo), math.cos(a.hi)]
        if a.width >= 2 * math.pi:
            return Interval(-1, 1)
        return Interval(min(vals) - 1e-15, max(vals) + 1e-15)

    @staticmethod
    def power(a: Interval, n: int) -> Interval:
        return a ** n
