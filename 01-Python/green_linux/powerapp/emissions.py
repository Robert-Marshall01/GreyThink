"""Emissions and carbon-aware suggestion utilities.

This module provides a pluggable way to fetch current carbon intensity and
produce simple postponement suggestions for deferrable tasks (UI-only, no
automation). The implementation includes a mock provider and optional
ElectricityMap support if you set ELECTRICITYMAP_TOKEN in the environment.
"""
from datetime import datetime, timezone, timedelta
import os
import json
from typing import List, Dict, Optional
from pathlib import Path


def _parse_iso_to_dt(ts):
    """Parse various timestamp representations into a timezone-aware datetime.

    - Accepts ISO strings with 'Z', offsets, or naive datetimes.
    - If the parsed datetime is naive, assumes system local timezone (fallback to UTC).
    """
    try:
        if isinstance(ts, str):
            s = ts
            if s.endswith('Z'):
                s = s.replace('Z', '+00:00')
            try:
                dt = datetime.fromisoformat(s)
            except Exception:
                try:
                    dt = datetime.fromisoformat(s)
                except Exception:
                    dt = datetime.now(timezone.utc)
        else:
            dt = ts
    except Exception:
        dt = datetime.now(timezone.utc)
    if getattr(dt, 'tzinfo', None) is None:
        try:
            local_tz = datetime.now(timezone.utc).astimezone().tzinfo
            dt = dt.replace(tzinfo=local_tz)
        except Exception:
            dt = dt.replace(tzinfo=timezone.utc)
    return dt


def _now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def fetch_current_intensity(zone: Optional[str] = None, token: Optional[str] = None, max_retries: int = 3, backoff_factor: float = 1.0) -> Dict:
    """Fetch current carbon intensity (gCO2/kWh).

    Returns a dict: {
      'intensity': float (gCO2/kWh),
      'unit': 'gCO2/kWh', 'timestamp': iso,
      'source': 'mock'|'electricitymap', 'note': str
    }

    If `provider` in persisted settings is 'electricitymap' and a token is available
    (either passed via `token`, environment or config), attempts to query the
    ElectricityMap API for the zone. Implements simple retry/backoff for HTTP 429
    or transient errors. Falls back to synthetic mock provider on error.
    """
    # Resolve token and provider from env or config
    try:
        from powerapp.config import load_settings
        cfg = load_settings()
    except Exception:
        cfg = {}

    provider = cfg.get('provider') or os.environ.get('POWERAPP_PROVIDER') or 'mock'
    token = token or os.environ.get('ELECTRICITYMAP_TOKEN') or cfg.get('token')

    if provider == 'electricitymap' and token and zone:
        import urllib.request
        import urllib.error
        import time
        url = f'https://api.electricitymap.org/v3/carbon-intensity/latest?zone={zone}'
        req = urllib.request.Request(url, headers={'auth-token': token})
        attempt = 0
        while attempt < max_retries:
            try:
                with urllib.request.urlopen(req, timeout=8) as resp:
                    data = json.load(resp)
                    ci = data.get('data', {}).get('carbonIntensity')
                    if ci is not None:
                        return {'intensity': float(ci), 'unit': 'gCO2/kWh', 'timestamp': _now_iso(), 'source': 'electricitymap', 'note': f'zone={zone}'}
                    break
            except urllib.error.HTTPError as he:
                # Honor Retry-After header on 429
                if he.code == 429:
                    headers = getattr(he, 'headers', None)
                    retry_after = headers.get('Retry-After') if headers else None
                    try:
                        ra = int(retry_after) if retry_after else None
                        sleep_t = ra if ra else backoff_factor * (2 ** attempt)
                    except Exception:
                        sleep_t = backoff_factor * (2 ** attempt)
                    time.sleep(sleep_t)
                    attempt += 1
                    continue
                # other HTTP errors -> break and fallback
                break
            except Exception:
                # transient error: backoff and retry
                sleep_t = backoff_factor * (2 ** attempt)
                time.sleep(sleep_t)
                attempt += 1
                continue
        # if we get here, electricitymap failed; fall back to mock with note
        return {'intensity': 400.0, 'unit': 'gCO2/kWh', 'timestamp': _now_iso(), 'source': 'mock', 'note': f'electricitymap failure after {attempt} attempts'}

    # Mock provider: produce a synthetic value based on hour of day
    hour = datetime.now(timezone.utc).hour
    # peak hours → higher intensity
    base = 200 + (hour % 24 >= 6 and hour % 24 <= 20) * 150
    return {'intensity': float(base), 'unit': 'gCO2/kWh', 'timestamp': _now_iso(), 'source': 'mock', 'note': 'synthetic hour-based'}


def suggest_postponements(tasks: List[Dict], current_intensity: float, threshold: float = 300.0) -> List[Dict]:
    """Given a list of candidate tasks and current intensity, return postponement suggestions.

    Each task is a dict: {'id': str, 'name': str, 'urgency': 'low'|'medium'|'high', 'duration_min': int, 'power_w': float (optional)}
    We suggest postponement for low-urgency tasks when current_intensity > threshold.

    Estimation: use task provided power (power_w) or default 50 W.
    Returns list with estimated_saving_kgCO2_if_postponed (based on current intensity)
    """
    suggestions = []
    default_power_w = 50.0
    for t in tasks:
        urgency = t.get('urgency', 'low')
        if urgency == 'high':
            continue
        if current_intensity <= threshold:
            # no suggestion needed; show possible optimization
            continue
        dur_h = max(1, t.get('duration_min', 30)) / 60.0
        power_w = float(t.get('power_w', default_power_w))
        kwh = (power_w / 1000.0) * dur_h
        est_co2_kg = (kwh * current_intensity) / 1000.0
        # suggest postpone by 1-6 hours depending on urgency
        postpone_hours = 6 if urgency == 'low' else 1
        suggested_time = (datetime.now(timezone.utc) + timedelta(hours=postpone_hours)).isoformat()
        suggestions.append({
            'task_id': t.get('id'),
            'task_name': t.get('name'),
            'urgency': urgency,
            'duration_min': t.get('duration_min'),
            'power_w': power_w,
            'current_intensity_g': current_intensity,
            'estimated_saving_kgCO2_if_postponed': round(est_co2_kg, 6),
            'suggested_postpone_until': suggested_time,
            'reason': f'Current intensity {current_intensity:.0f} gCO2/kWh > threshold {threshold:.0f}'
        })
    return suggestions


def suggestions_to_csv(suggestions: List[Dict], user_zone: Optional[str] = None) -> str:
    """Return CSV text for a list of suggestions.
    
    Args:
        suggestions: List of suggestion dictionaries
        user_zone: Optional IANA timezone name (e.g., 'America/New_York') to convert timestamps
    """
    import io
    from datetime import datetime
    from zoneinfo import ZoneInfo
    
    def convert_timestamp(ts_str: str) -> str:
        """Convert UTC timestamp string to user's timezone."""
        if not ts_str or not user_zone:
            return ts_str
        try:
            # Parse the timestamp (assuming ISO format with timezone)
            dt = datetime.fromisoformat(ts_str)
            # Convert to user's timezone
            user_dt = dt.astimezone(ZoneInfo(user_zone))
            # Return in ISO format
            return user_dt.isoformat()
        except Exception:
            return ts_str
    
    out = io.StringIO()
    out.write('task_id,task_name,urgency,duration_min,current_intensity_g,saving_kgCO2,saving_kgCO2_best_window,best_window_start,suggested_postpone_until,reason\n')
    for s in suggestions:
        # Convert timestamp fields to user's timezone
        best_window = convert_timestamp(s.get('best_window_start', ''))
        postpone_until = convert_timestamp(s['suggested_postpone_until'])
        out.write(f"{s['task_id']},{s['task_name']},{s['urgency']},{s['duration_min']},{s.get('current_intensity_g','')},{s.get('estimated_saving_kgCO2_if_postponed','')},{s.get('estimated_saving_kgCO2_best_window','')},{best_window},{postpone_until},{s['reason']}\n")
    return out.getvalue()


def write_suggestions_to_file(suggestions: List[Dict], path: str, user_zone: Optional[str] = None) -> None:
    """Write suggestions to CSV file.
    
    Args:
        suggestions: List of suggestion dictionaries
        path: File path to write to
        user_zone: Optional IANA timezone name to convert timestamps
    """
    with open(path, 'w', encoding='utf-8') as fh:
        fh.write(suggestions_to_csv(suggestions, user_zone=user_zone))


def augment_suggestions_with_best_window(suggestions: List[Dict], forecast: List[Dict], window_hours: int = 2, windows: Optional[List[Dict]] = None, use_model: bool = False, model_path: Optional[str] = None) -> List[Dict]:
    """Given suggestions and a forecast, compute best low-carbon window and estimated savings.

    Adds fields to each suggestion: 'best_window_start', 'best_window_avg_intensity', 'estimated_saving_kgCO2_best_window'
    Returns a new list (shallow-copied dicts) with added fields.

    Parameters:
      use_model: when True, attempts to use a trained model via `predict_best_windows` to select windows.
      model_path: optional path to the joblib model to pass-through to the predictor.
    """
    if not suggestions:
        return []
    if not forecast:
        # No forecast: pass through with None fields
        out = []
        for s in suggestions:
            ns = s.copy()
            ns['best_window_start'] = None
            ns['best_window_avg_intensity'] = None
            ns['estimated_saving_kgCO2_best_window'] = None
            out.append(ns)
        return out

    # find lowest avg window (allow caller-provided windows after calendar filtering)
    per_suggestion_best = None
    if windows is None:
        if use_model:
            # If suggestions include per-app series data, compute a per-suggestion best window using the model.
            try:
                any_series = any(s.get('per_app_series') for s in suggestions)
                if any_series:
                    per_suggestion_best = {}
                    for idx, s in enumerate(suggestions):
                        pas = s.get('per_app_series')
                        try:
                            res = predict_best_windows(forecast, window_hours=window_hours, top_k=1, model_path=model_path, use_model=True, per_app_series=pas)
                            per_suggestion_best[idx] = res[0] if res else None
                        except Exception:
                            per_suggestion_best[idx] = None
                    windows = None
                else:
                    # Attempt to use a model-based predictor; falls back internally if model not available
                    try:
                        windows = predict_best_windows(forecast, window_hours=window_hours, top_k=1, model_path=model_path, use_model=True)
                    except Exception:
                        windows = find_low_carbon_windows(forecast, window_hours=window_hours, top_k=1)
            except Exception:
                windows = find_low_carbon_windows(forecast, window_hours=window_hours, top_k=1)
        else:
            windows = find_low_carbon_windows(forecast, window_hours=window_hours, top_k=1)
    best = None if per_suggestion_best is not None else (windows[0] if windows else None)

    out = []
    avg_power_kw = 0.05
    for idx, s in enumerate(suggestions):
        ns = s.copy()
        dur_h = max(1, s.get('duration_min', 30)) / 60.0
        kwh = avg_power_kw * dur_h
        current_intensity = float(s.get('current_intensity_g', 0.0))
        # select per-suggestion best if computed, otherwise the common best
        sel_best = None
        if per_suggestion_best is not None:
            sel_best = per_suggestion_best.get(idx)
        else:
            sel_best = best
        ns['best_window_start'] = sel_best['start'] if sel_best else None
        ns['best_window_avg_intensity'] = float(sel_best['avg_intensity']) if sel_best else None
        if sel_best:
            delta_g = current_intensity - ns['best_window_avg_intensity']
            est_kg = (kwh * delta_g) / 1000.0 if delta_g > 0 else 0.0
            ns['estimated_saving_kgCO2_best_window'] = round(est_kg, 6)
        else:
            ns['estimated_saving_kgCO2_best_window'] = None
        out.append(ns)
    return out


def simulate_postponement(task: Dict, forecast: List[Dict], window_start: Optional[str], window_hours: int = 2, current_intensity: Optional[float] = None, per_app_series: Optional[List[Dict]] = None) -> Dict:
    """Simulate postponing `task` to a forecast window.

    task: dict with 'duration_min' and 'power_w'.
    forecast: list of {'timestamp': iso, 'intensity': g}
    window_start: ISO timestamp string identifying start of chosen window (or None to use best window)
    window_hours: length of window in hours
    current_intensity: optional current intensity (gCO2/kWh). If not provided, falls back to task current_intensity in task dict or 0.
    per_app_series: optional list of samples with 'timestamp' and 'per_app_cpu' dicts to estimate per-app impacts.

    Returns a dict with keys: 'task_kwh', 'co2_now_kg', 'co2_in_window_kg', 'savings_kg', 'window_avg_intensity', 'window_start', 'series' and optionally 'per_app_impacts' (mapping app -> {'power_w','kwh','co2_kg'})
    """
    # compute task energy (kWh)
    dur_h = max(1, int(task.get('duration_min', 30))) / 60.0
    power_w = float(task.get('power_w', 50.0))
    kwh = (power_w / 1000.0) * dur_h

    # determine current intensity
    cur_int = current_intensity if current_intensity is not None else float(task.get('current_intensity_g', 0.0))
    co2_now = (kwh * float(cur_int)) / 1000.0 if cur_int else 0.0

    # find chosen window in forecast
    if not forecast:
        return {
            'task_kwh': kwh,
            'co2_now_kg': round(co2_now, 6),
            'co2_in_window_kg': None,
            'savings_kg': None,
            'window_avg_intensity': None,
            'window_start': None,
            'series': []
        }

    # normalize timestamps in forecast to iso strings
    # build an index map from timestamp -> intensity
    idx = {p['timestamp']: float(p['intensity']) for p in forecast}

    chosen = None
    if window_start and window_start in idx:
        chosen = window_start
    else:
        # attempt to choose the best (lowest average) window of size window_hours
        windows = find_low_carbon_windows(forecast, window_hours=window_hours, top_k=1)
        if windows:
            chosen = windows[0]['start']

    if not chosen:
        # could not pick a window
        return {
            'task_kwh': kwh,
            'co2_now_kg': round(co2_now, 6),
            'co2_in_window_kg': None,
            'savings_kg': None,
            'window_avg_intensity': None,
            'window_start': None,
            'series': []
        }

    # compute average intensity across window
    # find index of chosen timestamp in forecast
    times = [p['timestamp'] for p in forecast]
    try:
        start_idx = times.index(chosen)
    except ValueError:
        # try to match by prefix / truncated iso
        start_idx = None
        for i, t in enumerate(times):
            if t.startswith(chosen):
                start_idx = i
                break
    if start_idx is None:
        return {
            'task_kwh': kwh,
            'co2_now_kg': round(co2_now, 6),
            'co2_in_window_kg': None,
            'savings_kg': None,
            'window_avg_intensity': None,
            'window_start': None,
            'series': []
        }

    slice_ = forecast[start_idx:start_idx + window_hours]
    if not slice_:
        return {
            'task_kwh': kwh,
            'co2_now_kg': round(co2_now, 6),
            'co2_in_window_kg': None,
            'savings_kg': None,
            'window_avg_intensity': None,
            'window_start': None,
            'series': []
        }
    avg = sum(float(p['intensity']) for p in slice_) / len(slice_)
    co2_window = (kwh * avg) / 1000.0
    savings = max(0.0, co2_now - co2_window)

    # Build a small series around the window for plotting context (6h before/after)
    ctx_before = 6
    ctx_after = 6
    start_ctx = max(0, start_idx - ctx_before)
    end_ctx = min(len(forecast), start_idx + window_hours + ctx_after)
    series = [{'timestamp': p['timestamp'], 'intensity': float(p['intensity'])} for p in forecast[start_ctx:end_ctx]]

    result = {
        'task_kwh': kwh,
        'co2_now_kg': round(co2_now, 6),
        'co2_in_window_kg': round(co2_window, 6),
        'savings_kg': round(savings, 6),
        'window_avg_intensity': round(avg, 6),
        'window_start': chosen,
        'series': series
    }

    # If per_app_series provided, estimate per-app impacts for the chosen window
    try:
        if per_app_series:
            # build timestamp -> per_app_cpu map
            ts_map = {p['timestamp']: p.get('per_app_cpu', {}) for p in per_app_series}
            # compute mean per-app cpu across the window slice
            apps = set()
            for p in slice_:
                apps.update(ts_map.get(p['timestamp'], {}).keys())
            per_app_means = {}
            for a in sorted(apps):
                vals = []
                for p in slice_:
                    v = ts_map.get(p['timestamp'], {})
                    if v and a in v:
                        try:
                            vals.append(float(v.get(a, 0.0)))
                        except Exception:
                            vals.append(0.0)
                per_app_means[a] = float(sum(vals) / len(vals)) if vals else 0.0

            from powerapp.ml.heuristics import estimate_total_power, split_app_power
            base_power = 4.0
            est_total = estimate_total_power(per_app_means, base_power=base_power)
            alloc = split_app_power(est_total, per_app_means, base_power=base_power, include_base=False)
            dynamic_total = sum(alloc.values())
            per_app_impacts = {}
            for a, w in alloc.items():
                if dynamic_total > 0:
                    frac = float(w) / dynamic_total
                else:
                    frac = 0.0
                app_kwh = round(kwh * frac, 6)
                app_co2 = round((app_kwh * avg) / 1000.0, 6)
                per_app_impacts[a] = {'power_w': round(w, 4), 'kwh': app_kwh, 'co2_kg': app_co2}
            result['per_app_impacts'] = per_app_impacts
    except Exception:
        # per-app estimation must not crash simulation
        pass

    return result


def _cache_path_for_zone(zone: str):
    # simple per-zone cache file under XDG cache. Only use caching when XDG_CACHE_HOME
    # is explicitly set to avoid accidentally using global caches during tests.
    from pathlib import Path
    xdg = os.environ.get('XDG_CACHE_HOME')
    if not xdg:
        return None
    d = Path(xdg) / 'powerapp'
    d.mkdir(parents=True, exist_ok=True)
    # sanitize zone
    safe_zone = zone.replace('/', '_')
    return d / f'forecast_{safe_zone}.json'


def _load_cached_forecast(zone: str, ttl: int) -> Optional[List[Dict]]:
    p = _cache_path_for_zone(zone)
    if not p:
        return None
    if not p.exists():
        return None
    try:
        import time
        with p.open('r', encoding='utf-8') as fh:
            data = json.load(fh)
        ts = data.get('fetched_at')
        if ts is None:
            return None
        fetched = float(ts)
        if (time.time() - fetched) > ttl:
            return None
        return data.get('forecast')
    except Exception:
        return None


def _save_cached_forecast(zone: str, forecast: List[Dict]) -> None:
    p = _cache_path_for_zone(zone)
    if not p:
        return
    try:
        import time
        with p.open('w', encoding='utf-8') as fh:
            json.dump({'fetched_at': time.time(), 'forecast': forecast}, fh)
    except Exception:
        # best-effort
        pass


def fetch_forecast(zone: Optional[str] = None, hours: int = 24, token: Optional[str] = None, max_retries: int = 3, backoff_factor: float = 1.0, cache_ttl: Optional[int] = None) -> List[Dict]:
    """Return a list of hourly forecast points for the next `hours` hours.

    Each point: {'timestamp': iso, 'intensity': gCO2/kWh}
    Uses ElectricityMap if persisted settings provider is 'electricitymap' and token/zone are provided; otherwise returns
    a synthetic sinusoidal forecast (local testing). Implements simple retry/backoff on transient errors.
    Caches forecasts for `cache_ttl` seconds when zone/provider is used.
    """
    # Resolve token and provider from env or config
    try:
        from powerapp.config import load_settings
        cfg = load_settings()
    except Exception:
        cfg = {}
    provider = cfg.get('provider') or os.environ.get('POWERAPP_PROVIDER') or 'mock'
    token = token or os.environ.get('ELECTRICITYMAP_TOKEN') or cfg.get('token')

    # determine TTL for cache
    if cache_ttl is None:
        cache_ttl = int(cfg.get('forecast_cache_ttl', 0)) if cfg.get('forecast_cache_ttl') is not None else 0

    # attempt to return cached forecast if available and not expired
    if provider == 'electricitymap' and zone and cache_ttl > 0:
        cached = _load_cached_forecast(zone, cache_ttl)
        if cached is not None:
            return cached

    if provider == 'electricitymap' and token and zone:
        import urllib.request
        import urllib.error
        import time
        url = f'https://api.electricitymap.org/v3/carbon-intensity/forecast?zone={zone}'
        req = urllib.request.Request(url, headers={'auth-token': token})
        attempt = 0
        while attempt < max_retries:
            try:
                with urllib.request.urlopen(req, timeout=8) as resp:
                    data = json.load(resp)
                    series = data.get('data', {}).get('data') or []
                    res = []
                    for item in series[:hours]:
                        ts = item.get('datetime') or item.get('timestamp')
                        ci = item.get('carbonIntensity') or item.get('carbon_intensity') or item.get('intensity')
                        if ci is not None and ts is not None:
                            res.append({'timestamp': ts, 'intensity': float(ci), 'source': 'electricitymap'})
                    if res:
                        # cache successful fetch
                        if zone and cache_ttl > 0:
                            _save_cached_forecast(zone, res)
                        return res
                    break
            except urllib.error.HTTPError as he:
                if he.code == 429:
                    headers = getattr(he, 'headers', None)
                    retry_after = headers.get('Retry-After') if headers else None
                    try:
                        ra = int(retry_after) if retry_after else None
                        sleep_t = ra if ra else backoff_factor * (2 ** attempt)
                    except Exception:
                        sleep_t = backoff_factor * (2 ** attempt)
                    time.sleep(sleep_t)
                    attempt += 1
                    continue
                break
            except Exception:
                sleep_t = backoff_factor * (2 ** attempt)
                time.sleep(sleep_t)
                attempt += 1
                continue
        # fallback to synthetic
    # Synthetic forecast: hourly values with day/night pattern and some noise
    from math import sin, pi
    now = datetime.now(timezone.utc).replace(minute=0, second=0, microsecond=0)
    res = []
    for h in range(hours):
        dt = now + timedelta(hours=h)
        hour = dt.hour
        # synthetic: lower at night, higher during day
        base = 180 + 120 * (1 + sin((hour / 24.0) * 2 * pi - pi/2)) / 2
        # add small deterministic variation
        variation = (h % 5) * 3
        ci = base + variation
        res.append({'timestamp': dt.isoformat(), 'intensity': float(ci), 'source': 'mock'})
    return res

def find_low_carbon_windows(forecast: List[Dict], window_hours: int = 1, top_k: int = 3, percentile: Optional[float] = None, threshold: Optional[float] = None) -> List[Dict]:
    """Find low-carbon windows in the forecast.

    - forecast: list of {'timestamp', 'intensity'} hourly ordered ascending
    - window_hours: size of window in hours
    Returns up to top_k windows as dicts: {'start': iso, 'end': iso, 'avg_intensity': float}
    Selection is either bottom percentile (if percentile provided) or below threshold if provided, otherwise returns bottom top_k windows.
    """
    if not forecast:
        return []

    # Normalize forecast timestamps into timezone-aware datetimes (UTC) and sort by time
    norm_fc = []
    for p in forecast:
        ts = p.get('timestamp')
        try:
            if isinstance(ts, str):
                s = ts
                if s.endswith('Z'):
                    s = s.replace('Z', '+00:00')
                dt = datetime.fromisoformat(s)
            else:
                dt = ts
        except Exception:
            dt = datetime.now(timezone.utc)
        if getattr(dt, 'tzinfo', None) is None:
            # If a timestamp lacks timezone info, assume it's in the system local timezone
            try:
                local_tz = datetime.now(timezone.utc).astimezone().tzinfo
                dt = dt.replace(tzinfo=local_tz)
            except Exception:
                dt = dt.replace(tzinfo=timezone.utc)
        norm_fc.append({'timestamp': dt, 'intensity': float(p.get('intensity', 0.0))})

    norm_fc.sort(key=lambda p: p['timestamp'])
    n = len(norm_fc)

    # compute window averages
    windows = []
    for i in range(0, max(0, n - window_hours + 1)):
        slice_ = norm_fc[i:i+window_hours]
        avg = sum(p['intensity'] for p in slice_) / len(slice_)
        start_dt = slice_[0]['timestamp']
        end_dt = slice_[-1]['timestamp'] + timedelta(hours=1)
        windows.append({'start': start_dt.isoformat(), 'end': end_dt.isoformat(), 'avg_intensity': avg, 'duration_h': int(window_hours)})
    # sort by avg_intensity ascending
    windows.sort(key=lambda w: w['avg_intensity'])
    if percentile is not None:
        # select windows below percentile
        vals = [w['avg_intensity'] for w in windows]
        idx = max(0, min(len(vals)-1, int(len(vals) * (percentile/100.0))))
        thresh = vals[idx]
        selected = [w for w in windows if w['avg_intensity'] <= thresh]
        return selected[:top_k]
    if threshold is not None:
        selected = [w for w in windows if w['avg_intensity'] <= threshold]
        return selected[:top_k]
    return windows[:top_k]


# Simple in-memory LRU-style cache for per-app predictions
from collections import OrderedDict
import threading
import hashlib
import time

_PER_APP_PRED_CACHE = OrderedDict()
_PER_APP_PRED_CACHE_MAX = int(os.environ.get('POWERAPP_PRED_CACHE_MAX', '256'))
_PER_APP_PRED_LOCK = threading.Lock()

# simple cache metrics (hit/miss counters)
_PER_APP_PRED_CACHE_HITS = 0
_PER_APP_PRED_CACHE_MISSES = 0
_PER_APP_PRED_CACHE_EVICTIONS = 0
_PER_APP_PRED_LAST_REPORT = 0
_PER_APP_PRED_CACHE_TELEMETRY_INTERVAL = int(os.environ.get('POWERAPP_PRED_CACHE_TELEMETRY_INTERVAL_SEC', '3600'))


def _cache_get(key: str):
    global _PER_APP_PRED_CACHE_HITS, _PER_APP_PRED_CACHE_MISSES
    with _PER_APP_PRED_LOCK:
        val = _PER_APP_PRED_CACHE.get(key)
        if val is not None:
            # move to end as recently used
            _PER_APP_PRED_CACHE.move_to_end(key)
            _PER_APP_PRED_CACHE_HITS += 1
        else:
            _PER_APP_PRED_CACHE_MISSES += 1
        return val


def _cache_set(key: str, value):
    global _PER_APP_PRED_CACHE_EVICTIONS
    with _PER_APP_PRED_LOCK:
        _PER_APP_PRED_CACHE[key] = value
        _PER_APP_PRED_CACHE.move_to_end(key)
        if len(_PER_APP_PRED_CACHE) > _PER_APP_PRED_CACHE_MAX:
            # pop oldest
            _PER_APP_PRED_CACHE.popitem(last=False)
            _PER_APP_PRED_CACHE_EVICTIONS += 1
    # consider reporting telemetry when cache changes
    try:
        _maybe_report_cache_metrics()
    except Exception:
        # telemetry must never crash the app
        pass


def _make_cache_key(forecast_candidates, per_app_series, window_hours, model_path):
    payload = {
        'cands': [{'start': c['start'], 'avg': c['avg_intensity']} for c in forecast_candidates],
        'per_app': per_app_series or None,
        'wh': int(window_hours),
        'model': model_path or ''
    }
    ser = json.dumps(payload, sort_keys=True, default=str, separators=(',', ':'))
    return hashlib.sha256(ser.encode('utf-8')).hexdigest()


def _send_telemetry_event(name: str, payload: dict) -> bool:
    """Send telemetry event if telemetry is enabled via settings. If not available, log.
    Currently: write a JSON-line to $XDG_CACHE_HOME/powerapp/telemetry_events.jsonl when XDG is set.
    Returns True when an event file was written, False otherwise.
    """
    try:
        from powerapp.config import load_settings
        cfg = load_settings()
    except Exception:
        cfg = {}
    if not bool(cfg.get('telemetry_opt_in', False)):
        return False
    xdg = os.environ.get('XDG_CACHE_HOME')
    ev = {'event': name, 'payload': payload, 'ts': int(time.time())}
    try:
        if xdg:
            p = Path(xdg) / 'powerapp'
            p.mkdir(parents=True, exist_ok=True)
            f = p / 'telemetry_events.jsonl'
            with f.open('a', encoding='utf-8') as fh:
                fh.write(json.dumps(ev) + "\n")
            return True
        else:
            # fallback to writing into repo-local telemetry dir (useful for test environments
            # where XDG_CACHE_HOME may not be set). This still requires opt-in and will not
            # send anything externally.
            try:
                p = Path(__file__).resolve().parent.parent / 'run-telemetry'
                p.mkdir(parents=True, exist_ok=True)
                f = p / 'telemetry_events.jsonl'
                with f.open('a', encoding='utf-8') as fh:
                    fh.write(json.dumps(ev) + "\n")
                return True
            except Exception:
                import logging
                logging.exception('telemetry write fallback failed')
                logging.info('telemetry event (log): %s %s', name, json.dumps(payload))
                return False
    except Exception:
        import logging
        logging.exception('telemetry write failed')
        return False


def _maybe_report_cache_metrics():
    """Rate-limited report of cache stats to telemetry (if opted-in)."""
    global _PER_APP_PRED_LAST_REPORT
    now = time.time()
    if (_PER_APP_PRED_LAST_REPORT == 0) or (now - _PER_APP_PRED_LAST_REPORT >= _PER_APP_PRED_CACHE_TELEMETRY_INTERVAL):
        stats = get_prediction_cache_stats()
        # include evictions too
        with _PER_APP_PRED_LOCK:
            ev = _PER_APP_PRED_CACHE_EVICTIONS
        payload = {
            'cache_size': stats['size'],
            'cache_max': stats['max_size'],
            'hits': stats['hits'],
            'misses': stats['misses'],
            'evictions': ev,
        }
        # enqueue via telemetry uploader (respects opt-in, sampling, and endpoint config)
        try:
            from powerapp.telemetry import enqueue_event
            queued = enqueue_event('pred_cache_stats', payload)
            if queued:
                _PER_APP_PRED_LAST_REPORT = now
        except Exception:
            # fallback to file write if telemetry infrastructure isn't available
            if _send_telemetry_event('pred_cache_stats', payload):
                _PER_APP_PRED_LAST_REPORT = now


def get_prediction_cache_stats():
    """Return dict with cache statistics: size, max_size, hits, misses."""
    with _PER_APP_PRED_LOCK:
        return {
            'size': len(_PER_APP_PRED_CACHE),
            'max_size': _PER_APP_PRED_CACHE_MAX,
            'hits': _PER_APP_PRED_CACHE_HITS,
            'misses': _PER_APP_PRED_CACHE_MISSES,
        }


def reset_prediction_cache_stats():
    """Reset cache and metrics (useful for tests)."""
    global _PER_APP_PRED_CACHE_HITS, _PER_APP_PRED_CACHE_MISSES, _PER_APP_PRED_CACHE
    with _PER_APP_PRED_LOCK:
        _PER_APP_PRED_CACHE.clear()
        _PER_APP_PRED_CACHE_HITS = 0
        _PER_APP_PRED_CACHE_MISSES = 0


def predict_best_windows(forecast: List[Dict], window_hours: int = 2, top_k: int = 3, model_path: Optional[str] = None, use_model: bool = False, per_app_series: Optional[List[Dict]] = None) -> List[Dict]:
    """Predict best windows, optionally using a trained model.

    - If `use_model` is False or no model is available, falls back to `find_low_carbon_windows`.
    - If `use_model` is True, attempts to load model from `model_path` or common locations and predict a score for each candidate window.

    The model is expected to be a scikit-learn regressor that predicts average intensity for a window given features.

    New parameter:
      per_app_series: optional list of samples with 'timestamp' and 'per_app_cpu' dicts; when provided, per-app mean features will be computed per window and passed to the model.
    """
    # fallback behavior: return deterministic windows
    if not use_model:
        return find_low_carbon_windows(forecast, window_hours=window_hours, top_k=top_k, percentile=None, threshold=None)

    # attempt to load model
    candidates = []
    n = len(forecast)
    for i in range(0, max(0, n - window_hours + 1)):
        slice_ = forecast[i:i+window_hours]
        avg = sum(p['intensity'] for p in slice_) / len(slice_)
        start_dt = _parse_iso_to_dt(slice_[0]['timestamp'])
        end_dt = _parse_iso_to_dt(slice_[-1]['timestamp']) + timedelta(hours=1)
        candidates.append({'start': start_dt.isoformat(), 'end': end_dt.isoformat(), 'avg_intensity': avg, 'slice': slice_})

    # try loading model from provided path or default locations
    paths_to_try = []
    if model_path:
        paths_to_try.append(model_path)
    # look under XDG cache
    xdg = os.environ.get('XDG_CACHE_HOME')
    if xdg:
        paths_to_try.append(os.path.join(xdg, 'powerapp', 'window_predictor.joblib'))
    # look in repo models/ dir
    paths_to_try.append(os.path.join(os.path.dirname(__file__), '..', 'models', 'window_predictor.joblib'))
    paths_to_try.append(os.path.join(os.getcwd(), 'models', 'window_predictor.joblib'))

    # Build candidate list (start,end,avg,slice)
    candidates = []
    n = len(forecast)
    for i in range(0, max(0, n - window_hours + 1)):
        slice_ = forecast[i:i+window_hours]
        avg = sum(p['intensity'] for p in slice_) / len(slice_)
        start_dt = _parse_iso_to_dt(slice_[0]['timestamp'])
        end_dt = _parse_iso_to_dt(slice_[-1]['timestamp']) + timedelta(hours=1)
        candidates.append({'start': start_dt.isoformat(), 'end': end_dt.isoformat(), 'avg_intensity': avg, 'slice': slice_})

    # If no model requested, fallback quickly
    if not use_model:
        return [{'start': c['start'], 'end': c['end'], 'avg_intensity': c['avg_intensity']} for c in find_low_carbon_windows(forecast, window_hours=window_hours, top_k=top_k, percentile=None, threshold=None)]

    # compute a deterministic cache key for these inputs
    cache_key = _make_cache_key(candidates, per_app_series, window_hours, model_path)
    cached = _cache_get(cache_key)
    if cached is not None:
        return cached[:top_k]

    model = None
    app_keys = None
    for p in paths_to_try:
        try:
            import joblib
            data = joblib.load(p)
            model = data.get('model') if isinstance(data, dict) else data
            app_keys = data.get('app_keys') if isinstance(data, dict) else None
            break
        except Exception:
            continue

    if not model:
        # could not load model; fallback
        res = find_low_carbon_windows(forecast, window_hours=window_hours, top_k=top_k, percentile=None, threshold=None)
        _cache_set(cache_key, res)
        return res

    # build feature matrix compatible with training script: [start_hour, avg_forecast, mean_power, per_app_means...]
    import numpy as np
    X = []
    for c in candidates:
        start_hour = int(c['start'].split('T')[1].split(':')[0])
        avg_forecast = float(c['avg_intensity'])
        # mean_power is not available in forecast-only inputs; use avg_forecast as proxy or 0
        mean_power = 0.0
        per_app_means = [0.0 for _ in (app_keys or [])]
        # If per_app_series provided, compute mean per app over this window slice
        if per_app_series and app_keys:
            # build a mapping from timestamp -> per_app_cpu dict for quick lookup
            ts_map = {p['timestamp']: p.get('per_app_cpu', {}) for p in per_app_series}
            for idx, k in enumerate(app_keys):
                vals = []
                for p in c['slice']:
                    v = ts_map.get(p['timestamp'])
                    if v is not None and k in v:
                        try:
                            vals.append(float(v.get(k, 0.0)))
                        except Exception:
                            vals.append(0.0)
                if vals:
                    per_app_means[idx] = float(sum(vals) / len(vals))
                else:
                    per_app_means[idx] = 0.0
        feat = [start_hour, avg_forecast, mean_power] + per_app_means
        X.append(feat)
    if not X:
        return []
    X = np.array(X)
    try:
        preds = model.predict(X)
    except Exception:
        res = find_low_carbon_windows(forecast, window_hours=window_hours, top_k=top_k, percentile=None, threshold=None)
        _cache_set(cache_key, res)
        return res

    # attach predicted avg intensity and sort
    for c, pval in zip(candidates, preds):
        c['predicted_avg'] = float(pval)
    candidates.sort(key=lambda w: w['predicted_avg'])
    res = [{'start': c['start'], 'end': c['end'], 'avg_intensity': c['predicted_avg']} for c in candidates[:top_k]]
    _cache_set(cache_key, res)
    return res


def windows_to_csv(windows: List[Dict]) -> str:
    import io
    out = io.StringIO()
    out.write('start,end,avg_intensity_gCO2pkWh\n')
    for w in windows:
        out.write(f"{w['start']},{w['end']},{w['avg_intensity']:.2f}\n")
    return out.getvalue()


def write_windows_to_file(windows: List[Dict], path: str) -> None:
    with open(path, 'w', encoding='utf-8') as fh:
        fh.write(windows_to_csv(windows))


def compute_emissions_from_history(history: List, last_minutes: Optional[int] = None, intensity: Optional[float] = None, intensity_series: Optional[List[Dict]] = None) -> Dict:
    """Estimate energy (kWh) and CO2 (kg) from a history list.

    history: list of (timestamp_iso, power_w_or_None)
    last_minutes: if provided, only consider samples within the last N minutes
    intensity: single value gCO2/kWh to apply uniformly
    intensity_series: list of {'timestamp': iso, 'intensity': gCO2/kWh} ordered ascending (hourly forecast/historical). If provided, uses the intensity at the start of each energy segment.

    Returns dict: {'kwh': float, 'kg_co2': float, 'duration_h': float, 'sample_count': int (valid samples with power values)}
    """
    from datetime import datetime, timezone, timedelta

    # filter by last_minutes
    samples = []
    for ts, pw in history:
        try:
            dt = _parse_iso_to_dt(ts)
        except Exception:
            continue
        samples.append((dt, pw))

    if last_minutes is not None:
        cutoff = datetime.now(timezone.utc) - timedelta(minutes=int(last_minutes))
        samples = [(dt, pw) for (dt, pw) in samples if dt >= cutoff]

    # Filter out samples with missing power and count valid samples
    valid_samples = [(dt, pw) for (dt, pw) in samples if pw is not None]
    sample_count = len(valid_samples)
    if sample_count < 2:
        # return zero totals when there are fewer than two valid samples
        return {'kwh': 0.0, 'kg_co2': 0.0, 'duration_h': 0.0, 'sample_count': sample_count}

    # helper to find intensity at a datetime from intensity_series
    def _intensity_at(dt: datetime) -> Optional[float]:
        if not intensity_series:
            return None
        # assume intensity_series entries have 'timestamp' ISO strings ordered ascending
        last = None
        for e in intensity_series:
            try:
                edt = _parse_iso_to_dt(e['timestamp'])
            except Exception:
                continue
            if edt <= dt:
                last = e
            else:
                break
        if last is not None:
            return float(last.get('intensity'))
        # fallback to first
        return float(intensity_series[0].get('intensity')) if intensity_series else None

    total_wh = 0.0
    total_seconds = 0.0
    # iterate over consecutive pairs of valid samples
    for (dt1, p1), (dt2, p2) in zip(valid_samples, valid_samples[1:]):
        delta = (dt2 - dt1).total_seconds()
        if delta <= 0:
            continue
        # average power (W) * seconds -> Wh = W * s / 3600
        avg_w = (float(p1) + float(p2)) / 2.0
        wh = avg_w * (delta / 3600.0)
        total_wh += wh
        total_seconds += delta

    kwh = total_wh / 1000.0
    duration_h = total_seconds / 3600.0

    # compute kgCO2
    kg = 0.0
    if intensity_series:
        # compute with per-segment intensity using valid samples
        total_kg = 0.0
        total_wh2 = 0.0
        for (dt1, p1), (dt2, p2) in zip(valid_samples, valid_samples[1:]):
            delta = (dt2 - dt1).total_seconds()
            if delta <= 0:
                continue
            avg_w = (float(p1) + float(p2)) / 2.0
            wh = avg_w * (delta / 3600.0)
            int_at = _intensity_at(dt1)
            if int_at is None:
                int_at = intensity or 0.0
            total_kg += (wh / 1000.0) * float(int_at)
            total_wh2 += wh
        kg = total_kg / 1000.0  # g -> kg
    elif intensity is not None:
        kg = (kwh * float(intensity)) / 1000.0

    return {'kwh': kwh, 'kg_co2': kg, 'duration_h': duration_h, 'sample_count': sample_count}
