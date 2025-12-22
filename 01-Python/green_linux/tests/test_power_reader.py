import os
import time
from types import SimpleNamespace

import pytest

from powerapp.cli import power_reader as pr


def test_upower_parsing(monkeypatch):
    # Simulate upower -e returning one device
    def fake_run(cmd):
        if cmd.strip() == 'upower -e':
            return '/org/freedesktop/UPower/devices/battery_BAT0'
        if cmd.startswith('upower -i'):
            # Include 'power supply: yes' and an energy-rate line
            return (
                '  native-path:          BAT0\n'
                '  model:                Battery\n'
                '  power supply:         yes\n'
                '  energy-rate:          6.23 W\n'
            )
        return ''

    monkeypatch.setattr(pr, 'run_cmd', fake_run)
    pw, details = pr.upower_power_w()
    assert pw == pytest.approx(6.23)
    assert 'device' in details


def create_rapl_files(tmp_path, values, max_range=None):
    # Create files that mimic RAPL energy_uj and optionally max_energy_range_uj
    paths = []
    base = tmp_path / 'intel-rapl:0'
    base.mkdir()
    for i, v in enumerate(values):
        sub = base / f'intel-rapl:0:{i}'
        sub.mkdir()
        energy = sub / 'energy_uj'
        energy.write_text(str(v))
        if max_range is not None:
            (sub / 'max_energy_range_uj').write_text(str(max_range))
        paths.append(str(energy))
    # Also add a top-level energy_uj
    top = base / 'energy_uj'
    top.write_text('0')
    paths.append(str(top))
    return paths


def test_sample_rapl_power_normal(monkeypatch, tmp_path):
    # Simulate two counters with monotonic increases
    p1 = tmp_path / 'a'
    p2 = tmp_path / 'b'
    p1.write_text('1000000')
    p2.write_text('2000000')

    paths = [str(p1), str(p2)]
    monkeypatch.setattr(pr, 'find_rapl_energy_paths', lambda: paths)

    # Replace read_int_file to return controlled values at t0 and t1
    seq = {'calls': 0}

    def fake_read(path):
        # on first call (t0): return baseline
        # on second call (t1): return baseline + delta
        seq['calls'] += 1
        if seq['calls'] <= 2:
            return 1000000 if 'a' in path else 2000000
        else:
            return 1100000 if 'a' in path else 2100000

    monkeypatch.setattr(pr, 'read_int_file', fake_read)
    # sample_rapl_power will call read_int_file 4 times (two files twice)
    pw, details = pr.sample_rapl_power(interval=0.01)
    assert pw is not None
    assert 'rapl_paths' in details


def test_sample_rapl_power_wraparound(monkeypatch, tmp_path):
    # Test wrap-around behavior using max_energy_range_uj
    paths = create_rapl_files(tmp_path, [4294967290, 50], max_range=4294967295)
    # monkeypatch finder to return our paths in order (first element will be top-level), so trim
    # we only want the two detailed paths
    detailed = paths[:2]
    monkeypatch.setattr(pr, 'find_rapl_energy_paths', lambda: detailed)

    # Simulate read_int_file returning values that wrap using explicit mapping
    paths_map = {detailed[0]: 4294967290, detailed[1]: 50}
    next_map = {detailed[0]: 10, detailed[1]: 100}
    # add max range entries for the expected mr_path reads
    for p in detailed:
        mr = os.path.join(os.path.dirname(p), 'max_energy_range_uj')
        paths_map[mr] = 4294967295
        next_map[mr] = 4294967295

    state = {'phase': 0}

    def fake_read(path):
        # phase 0: t0 reads, phase 1: t1 reads
        if state['phase'] == 0:
            return paths_map.get(path, 0)
        else:
            return next_map.get(path, 0)

    # We need to toggle phase between the two sampling times: monkeypatch the sleep to flip phase
    def fake_sleep(s):
        state['phase'] = 1

    monkeypatch.setattr(pr, 'read_int_file', fake_read)
    monkeypatch.setattr(pr, 'time', SimpleNamespace(time=time.time, sleep=fake_sleep))

    pw, details = pr.sample_rapl_power(interval=0.01)
    assert pw is not None
    # delta for first counter: (max - 4294967290) + 10 = 5 + 10 = 15
    # second counter: 100 - 50 = 50
    # total de_uj = 65 -> 65e-6 J, dt will be computed from time.time calls
    assert pw > 0


if __name__ == '__main__':
    pytest.main([__file__])
