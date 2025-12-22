#!/usr/bin/env python3
"""Improved CLI prototype to report instantaneous system power (W).

Features added:
 - Proper upower battery detection (ignores peripherals)
 - Robust RAPL sampling with wrap-around handling and max_range support
 - Verbose logging and --watch mode for periodic sampling
 - Better JSON output with timezone-aware timestamps

License: MIT
"""

import argparse
import json
import logging
import os
import subprocess
import sys
import time
from datetime import datetime, timezone


LOG = logging.getLogger("power_reader")


def run_cmd(cmd):
    try:
        out = subprocess.check_output(cmd, shell=True, stderr=subprocess.DEVNULL, text=True)
        return out.strip()
    except Exception:
        return ""


def upower_power_w():
    """Return instantaneous power in W from upower if a real battery is present.
    Returns (power_w, details) or (None, None) if not available.
    """
    devs = run_cmd('upower -e').splitlines()
    for d in devs:
        info = run_cmd(f'upower -i {d}')
        if not info:
            continue
        # parse lines robustly to find 'power supply' and its value
        lines = [l.strip() for l in info.splitlines() if l.strip()]
        is_power_supply = False
        for line in lines:
            if line.lower().startswith('power supply'):
                # line like 'power supply: yes' (multiple spaces possible)
                parts = [p.strip() for p in line.split(':', 1)]
                if len(parts) == 2 and parts[1].lower() == 'yes':
                    is_power_supply = True
                    break
        if not is_power_supply:
            continue
        # find energy-rate or power line
        for line in lines:
            l = line.lower()
            if 'energy-rate' in l or l.startswith('power:') or 'power:' in l:
                parts = line.split(':', 1)
                if len(parts) == 2:
                    val = parts[1].strip().split()[0]
                    try:
                        return float(val), {'device': d}
                    except Exception:
                        pass
    return None, None


def find_rapl_energy_paths():
    base = '/sys/devices/virtual/powercap'
    if not os.path.isdir(base):
        return []
    paths = []
    for root, dirs, files in os.walk(base):
        for f in files:
            if f == 'energy_uj':
                paths.append(os.path.join(root, f))
    return sorted(set(paths))


def read_int_file(path):
    try:
        with open(path, 'r') as fh:
            return int(fh.read().strip())
    except Exception:
        return None


def sample_rapl_power(interval=0.5):
    """Sample all energy_uj counters and return (power_w, details) or (None, details)"""
    paths = find_rapl_energy_paths()
    if not paths:
        return None, {'reason': 'no rapl energy_uj paths found'}
    # Read max ranges if available to handle wrap-around
    max_ranges = []
    for p in paths:
        mr = None
        mr_path = os.path.join(os.path.dirname(p), 'max_energy_range_uj')
        mr = read_int_file(mr_path)
        max_ranges.append(mr)
    t0 = time.time()
    e0 = [read_int_file(p) for p in paths]
    if any(x is None for x in e0):
        return None, {'reason': 'unable to read some energy_uj at t0', 'paths': paths}
    time.sleep(interval)
    t1 = time.time()
    e1 = [read_int_file(p) for p in paths]
    if any(x is None for x in e1):
        return None, {'reason': 'unable to read some energy_uj at t1', 'paths': paths}
    de_uj_total = 0
    for i, (a, b) in enumerate(zip(e0, e1)):
        if b >= a:
            delta = b - a
        else:
            # counter wrapped; estimate using max_ranges if available
            mr = max_ranges[i]
            if mr and mr > 0:
                delta = (mr - a) + b
            else:
                # best-effort: assume small wrap (treat as unreadable)
                return None, {'reason': 'wrap-around detected but no max range', 'path': paths[i]}
        de_uj_total += delta
    de_j = de_uj_total / 1e6
    dt = t1 - t0
    power_w = de_j / dt
    details = {'rapl_paths': paths, 'interval': interval}
    return power_w, details


def format_result(result, pretty=False):
    if pretty and result.get('method'):
        if result['method'] == 'upower':
            print(f"{result['power_w']:.2f} W  (method: upower)")
        elif result['method'] == 'rapl':
            print(f"{result['power_w']:.2f} W  (method: rapl, interval={result.get('details', {}).get('interval')})")
        else:
            print(result.get('notes', ['No measurement available'])[0])
    else:
        print(json.dumps(result))


def main():
    p = argparse.ArgumentParser(prog='power_reader', description='Report instantaneous system power (W)')
    p.add_argument('--method', choices=['auto', 'upower', 'rapl'], default='auto')
    p.add_argument('--interval', type=float, default=0.5, help='sampling interval for RAPL (seconds)')
    p.add_argument('--json', action='store_true', help='print JSON output (compact)')
    p.add_argument('--pretty', action='store_true', help='human-friendly output')
    p.add_argument('--watch', type=float, metavar='S', help='sample repeatedly every S seconds')
    p.add_argument('-v', '--verbose', action='store_true', help='verbose logging')
    args = p.parse_args()

    logging.basicConfig(level=logging.DEBUG if args.verbose else logging.INFO, format='%(levelname)s: %(message)s')

def get_sample(method='auto', interval=0.5):
    """Return a sample dict with keys: timestamp, method, power_w, notes, details

    This function is safe to call from other modules (e.g., the GTK UI) and
    encapsulates the sampling logic used by the CLI.
    """
    result = {
        'timestamp': datetime.now(timezone.utc).isoformat(),
        'method': None,
        'power_w': None,
        'notes': [],
        'details': {}
    }

    # Check for mock mode via environment variable
    if method == 'mock' or os.environ.get('POWERAPP_MOCK_POWER'):
        import random
        # Generate realistic-looking mock power values (15-45W typical for a laptop)
        base_power = 25.0
        variation = random.uniform(-10.0, 20.0)
        mock_power = max(5.0, base_power + variation)
        result['method'] = 'mock'
        result['power_w'] = round(mock_power, 2)
        result['notes'].append('using mock power values for testing')
        return result

    if method in ('auto', 'upower'):
        pw, details = upower_power_w()
        if pw is not None:
            result['method'] = 'upower'
            result['power_w'] = float(pw)
            result['notes'].append('read energy-rate / battery via upower')
            result['details'].update(details or {})
            return result
        elif method == 'upower':
            result['notes'].append('upower not available or no suitable battery')

    if method in ('auto', 'rapl'):
        pw, details = sample_rapl_power(interval=interval)
        if pw is not None:
            result['method'] = 'rapl'
            result['power_w'] = float(pw)
            result['notes'].append('sampled energy_uj RAPL counters')
            result['details'].update(details or {})
            return result
        else:
            LOG.debug('rapl sampling info: %s', details)
            result['notes'].append('rapl not available or unreadable (requires root or helper)')
            result['details'].update(details or {})

    result['method'] = 'none'
    result['notes'].append(
        'no available measurement; '
        'consider enabling privileged helper or use estimates'
    )
    return result


if __name__ == '__main__':
    main()

