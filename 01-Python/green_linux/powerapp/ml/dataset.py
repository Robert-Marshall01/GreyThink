"""Simulated dataset generator for on-device ML prototyping.

The simulated data mimics hourly samples with per-app CPU contributions and a measured
"total_power_w" value derived from a simple linear generative model plus noise. This
is intentionally simple and deterministic enough for unit tests and initial experiments.
"""
from datetime import datetime, timedelta, timezone
import random
from typing import List, Dict


def generate_simulated_series(n_series: int = 100, length: int = 48, apps: List[str] = None, base_power: float = 4.0, seed: int = None):
    """Generate a list of time series samples.

    Returns a list of dictionaries, each representing one hourly series (length entries).
    Each series is a dict with keys:
      - "series": list of sample dicts {timestamp, forecast_intensity, per_app_cpu, total_power_w}
      - "meta": optional meta info

    Args:
        n_series: number of series to produce
        length: number of hourly points per series
        apps: list of app names to produce per-app CPU shares
        base_power: baseline system power in watts
        seed: optional int to deterministically seed the generator for reproducible output
    """
    if apps is None:
        apps = ['chrome', 'editor', 'background']

    # If a seed is provided, use a local Random instance and a deterministic time base
    if seed is not None:
        rnd = random.Random(seed)
        # Fixed anchor date for reproducible timestamps
        anchor = datetime(2025, 1, 1, tzinfo=timezone.utc)
    else:
        rnd = random

    out = []
    for s in range(n_series):
        if seed is not None:
            # deterministic offset in [-24,0]
            start = anchor + timedelta(hours=rnd.randint(-24, 0))
        else:
            now = datetime.now(timezone.utc).replace(minute=0, second=0, microsecond=0)
            # Random diurnal offset
            start = now + timedelta(hours=random.randint(-24, 0))

        # Random forecast baseline (gCO2/kWh)
        fc_base = rnd.choice([100, 200, 300, 400])
        series = []
        # Assign per-app coefficients (how many watts per CPU percent)
        app_coeffs = {a: rnd.uniform(0.02, 0.12) for a in apps}
        for i in range(length):
            ts = (start + timedelta(hours=i)).isoformat()
            forecast_intensity = max(0.0, fc_base + rnd.gauss(0, 10))
            # Simulate per-app CPU (sum <= 100)
            per_app = {}
            remaining = 100.0
            for a in apps[:-1]:
                val = round(rnd.uniform(0, min(20, remaining)), 2)
                per_app[a] = val
                remaining -= val
            per_app[apps[-1]] = round(rnd.uniform(0, remaining), 2)
            # Generate measured total power using base + linear combination + noise
            total_w = base_power + sum(per_app[a] * app_coeffs[a] for a in apps) + rnd.gauss(0, 2.0)
            # Ensure measurement does not drop below the baseline system power
            if total_w < base_power:
                total_w = base_power
            sample = {
                'timestamp': ts,
                'forecast_intensity': forecast_intensity,
                'per_app_cpu': per_app,
                'total_power_w': total_w,
            }
            series.append(sample)
        out.append({'series': series, 'meta': {'app_coeffs': app_coeffs}})
    return out


def extract_window_examples(series_list: List[Dict], window_hours: int = 2):
    """Extract fixed-size window training examples from simulated series.

    Returns tuple (X, y, app_keys) where:
      - X is a list of feature lists: [start_hour, avg_forecast, mean_power, per_app_means...]
      - y is a list of labels (avg_forecast for the window)
      - app_keys is a sorted list of per-app keys seen in the series

    The function is deterministic for a given series_list and suitable for lightweight
    training or exporting to disk for evaluation.
    """
    X = []
    y = []
    # determine app keys from first non-empty series
    app_keys = []
    for s in series_list:
        ser = s.get('series', [])
        if ser:
            app_keys = sorted(ser[0].get('per_app_cpu', {}).keys())
            break
    if not app_keys:
        app_keys = []

    for s in series_list:
        ser = s.get('series', [])
        n = len(ser)
        for i in range(0, max(0, n - window_hours + 1)):
            window = ser[i:i+window_hours]
            start_hour = int(window[0]['timestamp'].split('T')[1].split(':')[0])
            avg_forecast = float(sum(p['forecast_intensity'] for p in window) / len(window))
            mean_power = float(sum(p['total_power_w'] for p in window) / len(window))
            per_app_means = [float(sum(p['per_app_cpu'].get(k, 0.0) for p in window) / len(window)) for k in app_keys]
            feat = [start_hour, avg_forecast, mean_power] + per_app_means
            X.append(feat)
            y.append(avg_forecast)
    return X, y, app_keys


def save_dataset_npz(path: str, X, y, app_keys=None):
    """Save extracted examples to a .npz file for training/evaluation."""
    import numpy as np
    arrX = np.array(X, dtype=float)
    arry = np.array(y, dtype=float)
    meta = {'app_keys': app_keys or []}
    import json
    # ensure parent dir exists
    from pathlib import Path
    p = Path(path)
    p.parent.mkdir(parents=True, exist_ok=True)
    # Save arrays and meta
    np.savez_compressed(str(p), X=arrX, y=arry, meta=json.dumps(meta))
    return str(p)
