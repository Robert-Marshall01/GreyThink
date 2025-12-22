"""Safe, test-friendly wrappers for system integrations used by PowerApp.

This module provides lightweight helpers to:
- schedule/cancel user-systemd timers for delayed actions
- detect availability of systemd user instance
- set / get GNOME/PowerProfile using `powerprofilesctl` or a D-Bus fallback

All external commands are invoked via `_run_cmd` which is easy to stub in tests.
"""
from __future__ import annotations

import shlex
import subprocess
import shutil
from pathlib import Path
from typing import Optional, Tuple


def _run_cmd(cmd: list[str], capture_output: bool = True) -> Tuple[int, str, str]:
    """Run the external command; returns (returncode, stdout, stderr)."""
    try:
        res = subprocess.run(cmd, capture_output=capture_output, text=True, check=False)
        return res.returncode, (res.stdout or '').strip(), (res.stderr or '').strip()
    except FileNotFoundError:
        return 127, '', f'Command not found: {cmd[0] if cmd else None}'


def has_systemd_user() -> bool:
    """Return True if a systemd --user instance appears available."""
    # Check for `systemctl --user` availability
    rc, out, err = _run_cmd(['systemctl', '--user', '--version'])
    return rc == 0


def schedule_user_timer(unit_name: str, when: str, command: list[str]) -> bool:
    """Schedule a one-shot transient unit via `systemd-run --user --on-calendar`.

    Args:
        unit_name: short identifier (no spaces) used as unit name prefix
        when: calendar time string accepted by systemd -- e.g. '2025-12-17 08:00:00'
        command: list of command parts to execute when timer fires

    Returns True on success; False otherwise.
    """
    if not has_systemd_user():
        return False
    cmd_str = ' '.join(shlex.quote(p) for p in command)
    unit = f"powerapp-{unit_name}".replace(' ', '_')
    run_cmd = ['systemd-run', '--user', f'--on-calendar={when}', f'--unit={unit}', 'sh', '-c', cmd_str]
    rc, out, err = _run_cmd(run_cmd)
    return rc == 0


def cancel_user_timer(unit_name: str) -> bool:
    """Stop and disable a user unit created by schedule_user_timer.

    Returns True if a stop command was issued successfully (or unit not present), False on error.
    """
    if not has_systemd_user():
        return False
    unit = f"powerapp-{unit_name}".replace(' ', '_')
    # stop unit (best-effort)
    rc1, out1, err1 = _run_cmd(['systemctl', '--user', 'stop', unit])
    rc2, out2, err2 = _run_cmd(['systemctl', '--user', 'disable', unit])
    return rc1 in (0, 5, 1) and rc2 in (0, 5, 1)


def powerprofilesctl_available() -> bool:
    """Return True if `powerprofilesctl` CLI is present."""
    return shutil.which('powerprofilesctl') is not None


# Distro detection & candidate selection helpers
_DISTRO_CACHE: dict | None = None

def _detect_distro() -> tuple[str, str]:
    """Return a tuple of (id, id_like) from /etc/os-release when available.

    This is intentionally lightweight and defensive (no external deps).
    """
    global _DISTRO_CACHE
    if _DISTRO_CACHE is not None:
        return _DISTRO_CACHE.get('id', ''), _DISTRO_CACHE.get('id_like', '')
    try:
        data = {}
        with open('/etc/os-release', 'r', encoding='utf-8') as fh:
            for ln in fh:
                if '=' in ln:
                    k, v = ln.rstrip('\n').split('=', 1)
                    data[k.lower()] = v.strip('"')
        _DISTRO_CACHE = data
        return data.get('id', ''), data.get('id_like', '')
    except Exception:
        _DISTRO_CACHE = {}
        return '', ''


def _candidate_profiles_for_distro() -> list[tuple[str, str, str, str]]:
    """Return an ordered list of D-Bus candidate tuples (dest, path, interface, prop/method).

    Order is tuned for known distributions when possible (e.g., Fedora vs Ubuntu).
    """
    # Broader pool of known candidates
    ubuntu_candidates = [
        ('org.freedesktop.powerprofiles', '/org/freedesktop/powerprofiles', 'org.freedesktop.powerprofiles', 'ActiveProfile'),
        ('org.freedesktop.PowerProfiles', '/org/freedesktop/PowerProfiles', 'org.freedesktop.PowerProfiles', 'ActiveProfile'),
        ('net.hadess.PowerProfiles', '/net/hadess/PowerProfiles', 'net.hadess.PowerProfiles', 'ActiveProfile'),
    ]
    fedora_candidates = [
        ('org.freedesktop.PowerProfiles', '/org/freedesktop/PowerProfiles', 'org.freedesktop.PowerProfiles', 'ActiveProfile'),
        ('org.freedesktop.powerprofiles', '/org/freedesktop/powerprofiles', 'org.freedesktop.powerprofiles', 'ActiveProfile'),
    ]
    generic_candidates = [
        ('org.freedesktop.PowerProfiles', '/org/freedesktop/PowerProfiles', 'org.freedesktop.PowerProfiles', 'ActiveProfile'),
        ('org.freedesktop.powerprofiles', '/org/freedesktop/powerprofiles', 'org.freedesktop.powerprofiles', 'ActiveProfile'),
        ('net.hadess.PowerProfiles', '/net/hadess/PowerProfiles', 'net.hadess.PowerProfiles', 'ActiveProfile'),
    ]

    id, id_like = _detect_distro()
    id = id.lower() if id else ''

    if 'ubuntu' in id or 'debian' in id or 'ubuntu' in (id_like or '') or 'debian' in (id_like or ''):
        return ubuntu_candidates + [c for c in generic_candidates if c not in ubuntu_candidates]
    if 'fedora' in id or 'rhel' in id or 'fedora' in (id_like or ''):
        return fedora_candidates + [c for c in generic_candidates if c not in fedora_candidates]
    return generic_candidates


def _gdbus_get_property(dest: str, path: str, interface: str, prop: str) -> Optional[str]:
    """Try to read a D-Bus property via gdbus and return a string value if found."""
    if shutil.which('gdbus') is None:
        return None
    cmd = ['gdbus', 'call', '--session', '--dest', dest, '--object-path', path,
           '--method', 'org.freedesktop.DBus.Properties.Get', interface, prop]
    rc, out, err = _run_cmd(cmd)
    if rc != 0 or not out:
        return None
    # gdbus typically returns a tuple, e.g. "('balanced',)" or "(<'balanced'>,)" — extract first quoted string
    import re
    m = re.search(r"['\"]([^'\"]+)['\"]", out)
    if m:
        return m.group(1)
    return None


def _busctl_get_property(dest: str, path: str, interface: str, prop: str) -> Optional[str]:
    """Try to read property via busctl --user (systemd) and parse string output."""
    if shutil.which('busctl') is None:
        return None
    # busctl --user get-property <dest> <path> <interface> <property>
    cmd = ['busctl', '--user', 'get-property', dest, path, interface, prop]
    rc, out, err = _run_cmd(cmd)
    if rc != 0 or not out:
        return None
    # busctl often prints like: "s \"balanced\"" or "s 'balanced'"
    import re
    m = re.search(r"['\"]([^'\"]+)['\"]", out)
    if m:
        return m.group(1)
    # last resort: split and return last token
    parts = out.strip().split()
    if parts:
        return parts[-1].strip('"\'')
    return None


def get_current_power_profile() -> Optional[str]:
    """Return current power profile name (e.g., 'performance', 'balanced', 'power-saver') or None."""
    if powerprofilesctl_available():
        rc, out, err = _run_cmd(['powerprofilesctl', 'get'])
        if rc == 0 and out:
            return out.strip()
    # Try gdbus/busctl fallbacks (best-effort)
    candidates = _candidate_profiles_for_distro()
    for dest, path, interface, prop in candidates:
        val = _gdbus_get_property(dest, path, interface, prop)
        if val:
            return val
        val = _busctl_get_property(dest, path, interface, prop)
        if val:
            return val
    return None


def _gdbus_set_active(dest: str, path: str, interface: str, method: str, profile: str) -> bool:
    if shutil.which('gdbus') is None:
        return False
    cmd = ['gdbus', 'call', '--session', '--dest', dest, '--object-path', path, '--method', method, profile]
    rc, out, err = _run_cmd(cmd)
    return rc == 0


def _busctl_set_active(dest: str, path: str, interface: str, method: str, profile: str) -> bool:
    if shutil.which('busctl') is None:
        return False
    # busctl --user call <dest> <path> <interface> <method> s "profile"
    cmd = ['busctl', '--user', 'call', dest, path, interface, method, 's', profile]
    rc, out, err = _run_cmd(cmd)
    return rc == 0


def set_power_profile(profile: str) -> bool:
    """Set the system power profile via powerprofilesctl if available.

    Returns True on success.
    """
    if powerprofilesctl_available():
        rc, out, err = _run_cmd(['powerprofilesctl', 'set', profile])
        return rc == 0

    # Attempt best-effort D-Bus calls
    candidates = [
        ('org.freedesktop.PowerProfiles', '/org/freedesktop/PowerProfiles', 'org.freedesktop.PowerProfiles', 'SetActiveProfile'),
        ('org.freedesktop.PowerProfiles', '/org/freedesktop/PowerProfiles', 'org.freedesktop.PowerProfiles', 'Set'),
        ('org.freedesktop.powerprofiles', '/org/freedesktop/powerprofiles', 'org.freedesktop.powerprofiles', 'SetActiveProfile'),
    ]
    # Reorder based on distro preference when powerprofilesctl is not present
    if not powerprofilesctl_available():
        ordered = []
        for c in _candidate_profiles_for_distro():
            # convert ActiveProfile candidate into a set-method tuple, preserving path/interface
            dest, path, interface, _ = c
            # prefer canonical SetActiveProfile method first
            ordered.append((dest, path, interface, 'SetActiveProfile'))
        candidates = ordered + [c for c in candidates if c not in ordered]

    for dest, path, interface, method in candidates:
        ok = _gdbus_set_active(dest, path, interface, method, profile)
        if ok:
            return True
        ok = _busctl_set_active(dest, path, interface, method, profile)
        if ok:
            return True
    return False


def run_diagnostics() -> dict:
    """Run a set of non-destructive diagnostics useful for troubleshooting power profile integration.

    Returns a simple dictionary summarizing detected tools, available backends, and outcomes.
    """
    out = {}
    out['powerprofilesctl_installed'] = powerprofilesctl_available()
    try:
        if out['powerprofilesctl_installed']:
            rc, stdout, stderr = _run_cmd(['powerprofilesctl', 'get'])
            out['powerprofilesctl_get'] = {'rc': rc, 'out': stdout, 'err': stderr}
        else:
            out['powerprofilesctl_get'] = None
    except Exception as e:
        out['powerprofilesctl_get'] = {'rc': 1, 'out': '', 'err': str(e)}

    out['systemd_user'] = has_systemd_user()
    out['gdbus_present'] = shutil.which('gdbus') is not None
    out['busctl_present'] = shutil.which('busctl') is not None
    try:
        out['current_profile'] = get_current_power_profile()
    except Exception as e:
        out['current_profile'] = None
        out['current_profile_error'] = str(e)

    # best-guess candidate used for reads/writes
    if out['powerprofilesctl_installed'] and out.get('powerprofilesctl_get', {}).get('rc') == 0:
        out['candidate_used'] = 'powerprofilesctl'
    elif out['gdbus_present']:
        out['candidate_used'] = 'gdbus'
    elif out['busctl_present']:
        out['candidate_used'] = 'busctl'
    else:
        out['candidate_used'] = None

    # add simple distro hint to help UI show tailored suggestions
    try:
        id, id_like = _detect_distro()
        out['distro_id'] = id or ''
        out['distro_like'] = id_like or ''
    except Exception:
        out['distro_id'] = ''
        out['distro_like'] = ''

    return out


def save_diagnostics(results: dict, path: str | None = None) -> str | None:
    """Persist diagnostics results as JSON and return the path written.

    If `path` is not provided, write to $XDG_STATE_HOME/powerapp/diagnostics-YYYYmmdd-HHMMSS.json.
    Returns the absolute path on success, or None on failure.
    """
    import json
    import os
    import datetime
    try:
        if not path:
            base = os.environ.get('XDG_STATE_HOME') or str(Path.home() / '.local' / 'state')
            d = Path(base) / 'powerapp'
            d.mkdir(parents=True, exist_ok=True)
            ts = datetime.datetime.now(datetime.timezone.utc).strftime('%Y%m%d-%H%M%SZ')
            path = str(d / f'diagnostics-{ts}.json')
        with open(path, 'w', encoding='utf-8') as fh:
            json.dump(results, fh, indent=2, sort_keys=True)
        return os.path.abspath(path)
    except Exception:
        return None


def generate_bug_report(results: dict, include_logs: bool = True, path: str | None = None) -> str | None:
    """Bundle diagnostics and logs into a single tar.gz archive for bug reporting.

    If `path` is None, writes to $XDG_STATE_HOME/powerapp/bugreport-YYYYmmdd-HHMMSS.tar.gz and returns the path.
    Returns the archive path or None on failure.
    """
    import tarfile
    import os
    import datetime
    try:
        # ensure diagnostics JSON exists
        diag_path = save_diagnostics(results)
        if not diag_path:
            return None
        if not path:
            base = os.environ.get('XDG_STATE_HOME') or str(Path.home() / '.local' / 'state')
            d = Path(base) / 'powerapp'
            d.mkdir(parents=True, exist_ok=True)
            ts = datetime.datetime.now(datetime.timezone.utc).strftime('%Y%m%d-%H%M%SZ')
            path = str(d / f'bugreport-{ts}.tar.gz')
        # create tar.gz containing diagnostics and optional logs and config
        with tarfile.open(path, 'w:gz') as tf:
            tf.add(diag_path, arcname=Path(diag_path).name)
            # include config if exists
            cfg = Path(os.environ.get('XDG_CONFIG_HOME') or str(Path.home() / '.config')) / 'powerapp' / 'config.json'
            if cfg.exists():
                tf.add(str(cfg), arcname='config.json')
            if include_logs:
                state_dir = Path(os.environ.get('XDG_STATE_HOME') or str(Path.home() / '.local' / 'state')) / 'powerapp'
                if state_dir.exists():
                    for f in state_dir.glob('*.log'):
                        tf.add(str(f), arcname=f.name)
        return os.path.abspath(path)
    except Exception:
        return None


class TempPowerProfile:
    """Context manager to temporarily set a power profile and restore the previous value on exit.

    Example:
        with TempPowerProfile('power-saver'):
            # do work with lowered profile
            pass
    """

    def __init__(self, profile: str):
        self._desired = profile
        self._prior: Optional[str] = None

    def __enter__(self):
        try:
            self._prior = get_current_power_profile()
            set_power_profile(self._desired)
        except Exception:
            pass
        return self

    def __exit__(self, exc_type, exc, tb):
        try:
            if self._prior:
                set_power_profile(self._prior)
        except Exception:
            pass
        return False
