"""Simple deterministic explainability helpers for window suggestions.

This module returns a small list of feature contributions (feature, score) that is
sufficient for UI explanation overlays and unit testing. It is intentionally
lightweight and deterministic for on-device use.
"""
from typing import List, Dict, Optional


def explain_window_choice(suggestion: Dict, forecast: List[Dict], window_start: Optional[str], per_app_series: Optional[List[Dict]] = None, top_k: int = 3) -> List[Dict]:
    """Return a list of feature explanations sorted by descending absolute score.

    Each item: {'feature': str, 'score': float}
    Heuristics used (simple and deterministic):
    - 'avg_intensity_delta': difference between current intensity and chosen window avg (positive means savings)
    - 'app:<name>': normalized mean cpu contribution for apps present in per_app_series
    """
    res = []
    if not forecast or not window_start:
        return res
    # find window avg
    times = [p['timestamp'] for p in forecast]
    try:
        idx = times.index(window_start)
    except ValueError:
        idx = None
        for i, t in enumerate(times):
            if t.startswith(window_start):
                idx = i
                break
    if idx is None:
        return res
    # compute window avg over suggestion window_hours if provided
    window_hours = int(suggestion.get('window_hours', 2))
    slice_ = forecast[idx:idx + window_hours]
    if not slice_:
        return res
    avg = sum(float(p['intensity']) for p in slice_) / len(slice_)
    cur_int = float(suggestion.get('current_intensity_g', 0.0) or 0.0)
    delta = cur_int - avg
    res.append({'feature': 'avg_intensity_delta', 'score': float(delta)})

    # per-app contributions: compute mean CPU in window
    app_means = {}
    try:
        ts_map = {p['timestamp']: p.get('per_app_cpu', {}) for p in (per_app_series or [])}
        apps = set()
        for p in slice_:
            apps.update(ts_map.get(p['timestamp'], {}).keys())
        for a in sorted(apps):
            vals = []
            for p in slice_:
                try:
                    v = float(ts_map.get(p['timestamp'], {}).get(a, 0.0) or 0.0)
                except Exception:
                    v = 0.0
                vals.append(v)
            app_means[a] = (sum(vals) / len(vals)) if vals else 0.0
    except Exception:
        app_means = {}

    # normalize app means to 0..1 and add as features
    if app_means:
        max_mean = max(app_means.values()) or 1.0
        for a, mean in app_means.items():
            res.append({'feature': f'app:{a}', 'score': float(mean / max_mean)})

    # sort by absolute score desc and return top_k
    res.sort(key=lambda it: abs(it.get('score', 0.0)), reverse=True)
    return res[:top_k]
