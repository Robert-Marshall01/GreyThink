import io
from pathlib import Path
from PIL import Image, ImageChops


from powerapp.emissions import simulate_postponement
from powerapp.gtk.main import draw_simulation


def _make_input():
    suggestion = {'task_name': 'VisualGolden', 'duration_min': 60, 'power_w': 100.0, 'current_intensity_g': 400.0}
    forecast = [{'timestamp': f'2025-12-17T0{i}:00:00+00:00', 'intensity': v} for i, v in enumerate([200,180,100,90,120,160])]
    per_app_series = []
    for i, p in enumerate(forecast):
        if i in (2,3):
            per_app_series.append({'timestamp': p['timestamp'], 'per_app_cpu': {'appA': 70.0, 'appB': 30.0}})
        else:
            per_app_series.append({'timestamp': p['timestamp'], 'per_app_cpu': {'appA': 10.0, 'appB': 10.0}})
    suggestion['per_app_series'] = per_app_series
    res = simulate_postponement(suggestion, forecast, forecast[2]['timestamp'], window_hours=2, current_intensity=400.0, per_app_series=per_app_series)
    return res


def _render_image_for_res(res, width=520, height=160):
    import cairo
    surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, width, height)
    cr = cairo.Context(surface)
    class FakeArea:
        pass
    area = FakeArea()
    area._sim_series = res.get('series', [])
    area._sim_window_start = res.get('window_start')
    area._sim_window_hours = 2
    area._sim_busy = getattr(res, 'get', lambda k, d=None: d)('busy', []) or []
    # ensure per_app_bars exist
    if not getattr(res, 'get', lambda k, d=None: d)('per_app_bars'):
        impacts = res.get('per_app_impacts', {})
        per_app_bars = []
        total_power = sum(float(dd.get('power_w', 0.0)) for dd in impacts.values()) or 1.0
        for a, dd in impacts.items():
            frac = float(dd.get('power_w', 0.0)) / total_power
            from powerapp.gtk.main import color_for_app
            per_app_bars.append({'app': a, 'fraction': frac, 'color': color_for_app(a)})
        area._per_app_bars = per_app_bars
    else:
        area._per_app_bars = res.get('per_app_bars')

    draw_simulation(area, cr, width, height, settings={'sim_gridlines': 4, 'sim_grid_opacity': 0.06})
    buf = io.BytesIO()
    surface.write_to_png(buf)
    buf.seek(0)
    return Image.open(buf).convert('RGBA')


def test_simulator_visual_regression_matches_golden():
    res = _make_input()
    img = _render_image_for_res(res)
    golden_path = Path('tests/golden/simulator_per_app_bars_golden.png')
    assert golden_path.exists(), 'Golden image missing; generate with scripts/generate_golden_simulator_image.py'
    golden = Image.open(golden_path).convert('RGBA')

    # compute difference and accept small pixel-level differences
    diff = ImageChops.difference(img, golden)
    # convert to grayscale and sum pixel values
    bw = diff.convert('L')
    stat = sum(bw.getdata())
    # threshold chosen conservatively; small antialiasing differences allowed
    assert stat < 5000, f'Visual regression detected: pixel diff sum={stat}'


def test_simulator_side_by_side_visual_regression_matches_golden():
    res = _make_input()
    # create baseline (run-now)
    forecast = [{'timestamp': f'2025-12-17T0{i}:00:00+00:00', 'intensity': v} for i, v in enumerate([200,180,100,90,120,160])]
    baseline = simulate_postponement({'task_name': 'VisualGolden', 'duration_min': 60, 'power_w': 100.0, 'current_intensity_g': 400.0}, forecast, forecast[0]['timestamp'], window_hours=2, current_intensity=400.0)

    import cairo
    surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, 520, 160)
    cr = cairo.Context(surface)
    class FakeArea:
        pass
    area = FakeArea()
    area._sim_series = res.get('series', [])
    area._sim_window_start = res.get('window_start')
    area._sim_window_hours = 2
    area._sim_busy = getattr(res, 'get', lambda k, d=None: d)('busy', []) or []
    area._per_app_bars = res.get('per_app_bars')
    # ensure per_app_bars exist like generator would
    if not getattr(area, '_per_app_bars', None):
        impacts = res.get('per_app_impacts', {})
        per_app_bars = []
        total_power = sum(float(dd.get('power_w', 0.0)) for dd in impacts.values()) or 1.0
        for a, dd in impacts.items():
            frac = float(dd.get('power_w', 0.0)) / total_power
            from powerapp.gtk.main import color_for_app
            per_app_bars.append({'app': a, 'fraction': frac, 'color': color_for_app(a)})
        area._per_app_bars = per_app_bars

    # attach baseline
    area._sim_series_b = baseline.get('series', [])
    area._per_app_bars_b = baseline.get('per_app_bars', [])
    if not getattr(area, '_per_app_bars_b', None):
        impacts = baseline.get('per_app_impacts', {})
        per_app_bars_b = []
        total_power = sum(float(dd.get('power_w', 0.0)) for dd in impacts.values()) or 1.0
        for a, dd in impacts.items():
            frac = float(dd.get('power_w', 0.0)) / total_power
            from powerapp.gtk.main import color_for_app
            per_app_bars_b.append({'app': a, 'fraction': frac, 'color': color_for_app(a)})
        area._per_app_bars_b = per_app_bars_b

    draw_simulation(area, cr, 520, 160, series_b=area._sim_series_b, side_by_side=True, scrub_frac=0.5, settings={'sim_gridlines': 4, 'sim_grid_opacity': 0.06})
    buf = io.BytesIO()
    surface.write_to_png(buf)
    buf.seek(0)
    img = Image.open(buf).convert('RGBA')

    golden_path = Path('tests/golden/simulator_side_by_side_golden.png')
    assert golden_path.exists(), 'Side-by-side golden missing; generate with scripts/generate_golden_simulator_image.py'
    golden = Image.open(golden_path).convert('RGBA')

    # Prefer structural metadata-first comparison to avoid pixel-level flakiness
    import json
    meta_path = Path('tests/golden/simulator_side_by_side_golden.json')
    assert meta_path.exists(), 'Side-by-side golden metadata missing; generate with scripts/generate_golden_simulator_image.py'
    meta = json.load(open(meta_path, 'r', encoding='utf-8'))
    left_meta = meta.get('left_pixel_bars', [])
    right_meta = meta.get('right_pixel_bars', [])

    left_actual = getattr(area, '_per_app_pixel_bars_left', []) or []
    right_actual = getattr(area, '_per_app_pixel_bars_right', []) or []

    # basic checks: same number of bars
    assert len(left_actual) == len(left_meta)
    assert len(right_actual) == len(right_meta)

    # In this headless path we don't have the dialog; just assert structural metadata checks.
    # explanation should be attached to the area under test
    expl = getattr(area, '_explain', None)
    assert expl is not None and isinstance(expl, list)
    if expl:
        assert 'feature' in expl[0] and 'score' in expl[0]

    # compare centroids and fractions with tolerances
    def centroid(pb):
        try:
            return (float(pb.get('start_px', 0)) + float(pb.get('width_px', 0)) / 2.0) / float(520)
        except Exception:
            return None

    for exp, act in zip(left_meta, left_actual):
        exp_c = exp.get('centroid_frac')
        act_c = centroid(act)
        assert exp.get('app') == act.get('app')
        assert exp_c is not None and act_c is not None and abs(exp_c - act_c) < 0.06

    for exp, act in zip(right_meta, right_actual):
        exp_c = exp.get('centroid_frac')
        act_c = centroid(act)
        assert exp.get('app') == act.get('app')
        assert exp_c is not None and act_c is not None and abs(exp_c - act_c) < 0.06

