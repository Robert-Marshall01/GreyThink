"""Minimal ML and heuristic prototypes for PowerApp.

Module contents are intentionally lightweight: a simulated dataset generator and simple
heuristics to estimate per-app power usage. A scikit-learn model training pipeline can be
added later under `powerapp/ml/model.py` and `scripts/train.py`.
"""

from .dataset import generate_simulated_series
from .heuristics import estimate_total_power, split_app_power

__all__ = [
    'generate_simulated_series',
    'estimate_total_power',
    'split_app_power',
]
