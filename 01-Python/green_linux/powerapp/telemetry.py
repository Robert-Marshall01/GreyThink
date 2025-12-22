"""Simple opt-in telemetry uploader with sampling, batching and secure POST.

Design notes:
- Only sends aggregated, non-identifying metrics and only when `telemetry_opt_in` is True.
- Endpoint and API key are configured via env vars:
  - POWERAPP_TELEMETRY_ENDPOINT (https URL)
  - POWERAPP_TELEMETRY_API_KEY (optional; sent as ``Authorization: ApiKey <key>``)
  - POWERAPP_TELEMETRY_SAMPLE_RATE (0.0-1.0, default 1.0)
  - POWERAPP_TELEMETRY_BATCH_SIZE (default 20)
  - POWERAPP_TELEMETRY_UPLOAD_INTERVAL_SEC (default 30)
- For testability and robustness we provide `flush()` to force a synchronous upload.
- Uses `requests` when available; falls back to a no-op sender if not.
"""
from __future__ import annotations

import os
import time
import json
import logging
import threading
import queue
import random
from typing import Dict, Any, List

# Config defaults
_ENDPOINT = os.environ.get('POWERAPP_TELEMETRY_ENDPOINT')
_API_KEY = os.environ.get('POWERAPP_TELEMETRY_API_KEY')
_SAMPLE_RATE = float(os.environ.get('POWERAPP_TELEMETRY_SAMPLE_RATE', '1.0'))
_BATCH_SIZE = int(os.environ.get('POWERAPP_TELEMETRY_BATCH_SIZE', '20'))
_UPLOAD_INTERVAL = int(os.environ.get('POWERAPP_TELEMETRY_UPLOAD_INTERVAL_SEC', '30'))

# internal queue & worker
_queue: "queue.Queue[Dict[str, Any]]" = queue.Queue()
_worker: threading.Thread | None = None
_worker_stop = threading.Event()

# attempts to import requests
try:
    import requests
except Exception:
    requests = None


def _start_worker():
    global _worker
    if _worker and _worker.is_alive():
        return

    def _worker_fn():
        while not _worker_stop.is_set():
            try:
                batch: List[Dict] = []
                # collect up to BATCH_SIZE items, but wait at most UPLOAD_INTERVAL
                start = time.time()
                while len(batch) < _BATCH_SIZE and (time.time() - start) < _UPLOAD_INTERVAL:
                    try:
                        item = _queue.get(timeout=max(0.1, _UPLOAD_INTERVAL - (time.time() - start)))
                        batch.append(item)
                    except queue.Empty:
                        break
                if batch:
                    _send_batch(batch)
            except Exception:
                logging.exception('telemetry worker error')
                time.sleep(1)

    _worker_stop.clear()
    _worker = threading.Thread(target=_worker_fn, daemon=True)
    _worker.start()


def _send_batch(batch: List[Dict]) -> bool:
    """Attempt to POST the batch to the endpoint. Return True on success."""
    global _ENDPOINT, _API_KEY
    payload = {'events': batch, 'ts': int(time.time())}
    data = json.dumps(payload)
    # If no endpoint configured, write to XDG cache file as a local debug artifact (opt-in still required)
    if not _ENDPOINT or requests is None:
        try:
            xdg = os.environ.get('XDG_CACHE_HOME')
            if xdg:
                p = os.path.join(xdg, 'powerapp')
                os.makedirs(p, exist_ok=True)
                fpath = os.path.join(p, 'telemetry_upload.jsonl')
                with open(fpath, 'a', encoding='utf-8') as fh:
                    fh.write(json.dumps(payload) + "\n")
                return True
        except Exception:
            logging.exception('telemetry local write failed')
            return False
    # POST to endpoint
    headers = {'Content-Type': 'application/json'}
    if _API_KEY:
        headers['Authorization'] = f'ApiKey {_API_KEY}'
    try:
        resp = requests.post(_ENDPOINT, data=data, headers=headers, timeout=5)
        resp.raise_for_status()
        return True
    except Exception:
        logging.exception('telemetry POST failed')
        return False


def enqueue_event(name: str, payload: Dict[str, Any]) -> bool:
    """Enqueue an event for potential upload.

    Returns True if the event was accepted (sampled in and queued or written locally), False if sampled out or telemetry disabled.
    """
    # Check opt-in
    try:
        from powerapp.config import load_settings
        cfg = load_settings()
    except Exception:
        cfg = {}
    if not bool(cfg.get('telemetry_opt_in', False)):
        return False

    # Sampling (read sample rate dynamically to avoid stale module-level values)
    try:
        sample_rate = float(os.environ.get('POWERAPP_TELEMETRY_SAMPLE_RATE', str(_SAMPLE_RATE)))
    except Exception:
        sample_rate = _SAMPLE_RATE
    if sample_rate < 1.0 and random.random() >= sample_rate:
        return False

    ev = {'event': name, 'payload': payload, 'ts': int(time.time())}

    # If no remote endpoint or requests not available, write synchronously to local file for tests and local-debug
    if not _ENDPOINT or requests is None:
        try:
            return _send_batch([ev])
        except Exception:
            # Fall back to queueing to preserve behavior
            pass

    _queue.put(ev)
    # ensure worker is running
    _start_worker()
    return True


def flush(timeout: float = 5.0) -> int:
    """Force-upload queued events synchronously. Returns number of events sent."""
    items: List[Dict] = []
    try:
        while True:
            items.append(_queue.get_nowait())
    except queue.Empty:
        pass
    if not items:
        return 0
    ok = _send_batch(items)
    return len(items) if ok else 0


def stop_worker():
    _worker_stop.set()
    if _worker:
        _worker.join(timeout=1)


__all__ = ['enqueue_event', 'flush', 'stop_worker']
