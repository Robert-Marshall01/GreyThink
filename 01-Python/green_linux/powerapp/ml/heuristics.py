"""Lightweight heuristics for per-app power estimation.

These functions provide a deterministic baseline that maps per-app CPU shares to an
estimated total power (watts) and splits the estimated power across apps.
"""
from typing import Dict


def estimate_total_power(per_app_cpu: Dict[str, float], base_power: float = 4.0, app_coeffs: Dict[str, float] = None) -> float:
    """Estimate total system power (W) from per-app CPU percentages.

    Args:
        per_app_cpu: mapping app -> CPU percent (0-100)
        base_power: baseline idle power (W)
        app_coeffs: optional mapping app -> watts per CPU-percent; if None a sensible default is used.

    Returns:
        estimated total power in watts (float)
    """
    if app_coeffs is None:
        # default coefficients: heavier apps cost more per CPU percent
        app_coeffs = {a: 0.04 for a in per_app_cpu.keys()}
    total = float(base_power)
    for a, cpu in per_app_cpu.items():
        # clamp CPU percentages to sensible range
        try:
            c = float(cpu)
        except Exception:
            c = 0.0
        if c < 0.0:
            c = 0.0
        if c > 100.0:
            c = 100.0
        coeff = float(app_coeffs.get(a, 0.04))
        total += (c * coeff)
    # minimal rounding for stability
    return round(total, 4)


def split_app_power(estimated_total: float, per_app_cpu: Dict[str, float], base_power: float = 4.0, include_base: bool = False) -> Dict[str, float]:
    """Split `estimated_total` across apps proportional to CPU shares.

    By default this returns the allocation of the *dynamic* portion (i.e. total - base_power)
    across apps according to CPU proportions. If `include_base` is True, the base_power is
    added back to one app named '__base__' to make it explicit.

    Args:
        estimated_total: total watts estimate
        per_app_cpu: mapping app -> CPU percent
        base_power: baseline idle power (W) used to compute dynamic portion (default 4.0)
        include_base: whether to include base power in the returned mapping (as key '__base__')
    """
    if not per_app_cpu:
        return {}

    # clamp CPU values and compute sum
    clamped = {}
    total_cpu = 0.0
    for a, cpu in per_app_cpu.items():
        try:
            c = float(cpu)
        except Exception:
            c = 0.0
        if c < 0.0:
            c = 0.0
        if c > 100.0:
            c = 100.0
        clamped[a] = c
        total_cpu += c

    if total_cpu <= 0.0:
        # nothing running
        return {a: 0.0 for a in per_app_cpu}

    dynamic = float(estimated_total) - float(base_power)
    if dynamic < 0.0:
        dynamic = 0.0

    # allocate dynamically and ensure sums to dynamic via last-item correction
    allocation = {}
    apps = list(clamped.items())
    acc = 0.0
    for idx, (a, cpu) in enumerate(apps):
        if idx < len(apps) - 1:
            val = round((cpu / total_cpu) * dynamic, 4)
            allocation[a] = val
            acc += val
        else:
            # last app gets remainder to avoid rounding drift
            allocation[a] = round(max(0.0, dynamic - acc), 4)

    if include_base:
        allocation['__base__'] = round(float(base_power), 4)

    return allocation
