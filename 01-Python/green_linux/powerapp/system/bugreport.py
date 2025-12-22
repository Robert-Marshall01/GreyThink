"""Privacy-first bug report anonymizer and upload helpers."""
from __future__ import annotations

import json
import re
from typing import Any, Dict, Optional

# conservative regexes
_EMAIL_RE = re.compile(r"[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}")
_IPV4_RE = re.compile(r"\b(?:\d{1,3}\.){3}\d{1,3}\b")
_IPV6_RE = re.compile(r"\b(?:[0-9a-fA-F]{1,4}:){2,7}[0-9a-fA-F]{1,4}\b")
_MAC_RE = re.compile(r"\b(?:[0-9A-Fa-f]{2}:){5}[0-9A-Fa-f]{2}\b")
_UUID_RE = re.compile(r"\b[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}\b")
_HOME_PATH_RE = re.compile(r"(/home/)[^/\\]+")

REDACT_EMAIL = "<REDACTED_EMAIL>"
REDACT_IP = "<REDACTED_IP>"
REDACT = "<REDACTED>"
REDACT_USER = "<REDACTED_USER>"


def _sanitize_value(val: str) -> str:
    if not isinstance(val, str):
        return val
    v = _EMAIL_RE.sub(REDACT_EMAIL, val)
    v = _IPv4_ipv6_mask(v)
    v = _MAC_RE.sub(REDACT, v)
    v = _UUID_RE.sub(REDACT, v)
    v = _HOME_PATH_RE.sub(r"\1<REDACTED>", v)
    # Limit token-like long strings
    if len(v) > 200:
        return v[:80] + "..." + REDACT
    return v


def _IPv4_ipv6_mask(s: str) -> str:
    s = _IPV4_RE.sub(REDACT_IP, s)
    s = _IPV6_RE.sub(REDACT_IP, s)
    return s


def anonymize_diagnostics(data: Dict[str, Any]) -> Dict[str, Any]:
    """Deep-copy and anonymize diagnostics payload conservatively.

    Rules are intentionally conservative (safer to redact more).
    """
    if data is None:
        return {}

    def _an(value: Any) -> Any:
        if isinstance(value, dict):
            out = {}
            for k, v in value.items():
                lk = k.lower()
                if lk in ("password", "token", "secret"):
                    out[k] = "<REDACTED>"
                elif lk in ("username", "user") and isinstance(v, str):
                    out[k] = REDACT_USER
                else:
                    out[k] = _an(v)
            return out
        if isinstance(value, list):
            return [_an(v) for v in value]
        if isinstance(value, str):
            return _sanitize_value(value)
        return value

    return _an(data)


# small abstraction so tests can patch network behavior
def _http_post(url: str, json_data: Dict[str, Any], headers: Optional[Dict[str, str]] = None) -> Dict[str, Any]:
    try:
        import urllib.request
        req = urllib.request.Request(url, data=json.dumps(json_data).encode("utf-8"), headers=headers or {}, method="POST")
        with urllib.request.urlopen(req, timeout=10) as resp:
            b = resp.read()
            return json.loads(b.decode("utf-8"))
    except Exception:
        # bubble error to caller
        raise


def upload_bug_report(diagnostics: Dict[str, Any], url: Optional[str], headers: Optional[Dict[str, str]] = None) -> Dict[str, Any]:
    """Upload anonymized diagnostics to a configured URL.

    Returns server JSON on success or raises on network error.
    Caller should ensure opt-in and that url is set.
    """
    if not url:
        raise ValueError("No upload URL configured")

    payload = {
        "diagnostics": anonymize_diagnostics(diagnostics),
        "meta": {"uploader": "powerapp", "version": "0.0.0"},
    }
    return _http_post(url, payload, headers=headers)
