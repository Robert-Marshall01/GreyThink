import json


def _write(tmp_path, data, name):
    p = tmp_path / name
    p.write_text(json.dumps(data))
    return p


def test_compare_visual_meta_ok(tmp_path):
    e = {'per_app_bars': [{'app': 'appA', 'fraction': 0.7, 'color': [0.4,0.4,0.4]}, {'app': 'appB', 'fraction': 0.3, 'color': [0.5,0.5,0.5]}]}
    a = {'per_app_bars': [{'app': 'appB', 'fraction': 0.31, 'color': [0.49,0.5,0.5]}, {'app': 'appA', 'fraction': 0.69, 'color': [0.4,0.4,0.4]}]}
    ep = _write(tmp_path, e, 'exp.json')
    ap = _write(tmp_path, a, 'act.json')
    # call compare script; expect exit 0
    import subprocess
    subprocess.check_call(['python', 'scripts/compare_visual_meta.py', str(ep), str(ap), '--frac-tol', '0.05', '--color-tol', '0.12'])


def test_compare_visual_meta_fraction_fail(tmp_path):
    e = {'per_app_bars': [{'app': 'appA', 'fraction': 0.7, 'color': [0.4,0.4,0.4]}]}
    a = {'per_app_bars': [{'app': 'appA', 'fraction': 0.5, 'color': [0.4,0.4,0.4]}]}
    ep = _write(tmp_path, e, 'exp.json')
    ap = _write(tmp_path, a, 'act.json')
    import subprocess
    try:
        subprocess.check_call(['python', 'scripts/compare_visual_meta.py', str(ep), str(ap), '--frac-tol', '0.05'])
        assert False, 'Should have failed for fraction difference'
    except subprocess.CalledProcessError:
        pass
