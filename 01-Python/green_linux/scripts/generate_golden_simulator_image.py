#!/usr/bin/env python3
"""Generate a golden PNG for the simulator per-app bars visual test.

This script runs headless (requires Xvfb) and writes the PNG to tests/golden/simulator_per_app_bars_golden.png
"""
import sys
from pathlib import Path
sys.path.insert(0, str(Path.cwd()))

# Ensure system-installed gi bindings are importable from inside virtualenv
import sys
sys.path.append('/usr/lib/python3/dist-packages')
import gi
gi.require_version('Gtk', '4.0')
import cairo

from powerapp.gtk.main import draw_simulation
from powerapp.emissions import simulate_postponement

out = Path('tests/golden')
out.mkdir(parents=True, exist_ok=True)

# Build suggestion/forecast/per_app_series
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

# also compute a run-now baseline for side-by-side sample
baseline = simulate_postponement(suggestion, forecast, forecast[0]['timestamp'], window_hours=2, current_intensity=400.0, per_app_series=per_app_series)

# Render with Cairo
WIDTH, HEIGHT = 520, 160
surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, WIDTH, HEIGHT)
cr = cairo.Context(surface)

# attach properties to a fake area object for compatibility
class FakeArea:
    pass
area = FakeArea()
area._sim_series = res.get('series', [])
area._sim_window_start = res.get('window_start')
area._sim_window_hours = 2
area._sim_busy = getattr(res, 'get', lambda k, d=None: d)('busy', []) or []
area._per_app_bars = res.get('per_app_bars') or []
# baseline attachments
area._sim_series_b = baseline.get('series', [])
area._sim_window_start_b = baseline.get('window_start')
area._per_app_bars_b = baseline.get('per_app_bars') or []

from powerapp.gtk.main import color_for_app
# ensure per_app_bars exist: if not produced by simulate, build from res['per_app_impacts']
if getattr(area, '_per_app_bars', None) is None or not area._per_app_bars:
    impacts = res.get('per_app_impacts', {})
    per_app_bars = []
    total_power = sum(float(dd.get('power_w', 0.0)) for dd in impacts.values()) or 1.0
    for a, dd in impacts.items():
        frac = float(dd.get('power_w', 0.0)) / total_power
        per_app_bars.append({'app': a, 'fraction': frac, 'color': color_for_app(a)})
    area._per_app_bars = per_app_bars

# baseline per-app bars
if getattr(area, '_per_app_bars_b', None) is None or not area._per_app_bars_b:
    impacts = baseline.get('per_app_impacts', {})
    per_app_bars_b = []
    total_power = sum(float(dd.get('power_w', 0.0)) for dd in impacts.values()) or 1.0
    for a, dd in impacts.items():
        frac = float(dd.get('power_w', 0.0)) / total_power
        per_app_bars_b.append({'app': a, 'fraction': frac, 'color': color_for_app(a)})
    area._per_app_bars_b = per_app_bars_b


# render side-by-side with scrub at 50%
draw_simulation(area, cr, WIDTH, HEIGHT, series_b=area._sim_series_b, side_by_side=True, settings={'sim_gridlines': 4, 'sim_grid_opacity': 0.06}, scrub_frac=0.5)

out_file = out / 'simulator_side_by_side_golden.png'
surface.write_to_png(str(out_file))

# Write metadata (per_app_bars + pixel info + centroids) for structural comparisons
import json
meta = {
    'left_per_app_bars': getattr(area, '_per_app_bars', []),
    'right_per_app_bars': getattr(area, '_per_app_bars_b', [])
}
# attach pixel bars if present and compute centroid fractions
pixel_left = getattr(area, '_per_app_pixel_bars_left', []) or []
for pb in pixel_left:
    try:
        start = float(pb.get('start_px', 0))
        w = float(pb.get('width_px', 0))
        centroid = (start + (w / 2.0)) / float(WIDTH)
        pb['centroid_frac'] = centroid
    except Exception:
        pb['centroid_frac'] = None
pixel_right = getattr(area, '_per_app_pixel_bars_right', []) or []
for pb in pixel_right:
    try:
        start = float(pb.get('start_px', 0))
        w = float(pb.get('width_px', 0))
        centroid = (start + (w / 2.0)) / float(WIDTH)
        pb['centroid_frac'] = centroid
    except Exception:
        pb['centroid_frac'] = None
meta['left_pixel_bars'] = pixel_left
meta['right_pixel_bars'] = pixel_right
with open(out / 'simulator_side_by_side_golden.json', 'w', encoding='utf-8') as fh:
    json.dump(meta, fh, sort_keys=True, indent=2)

print('Wrote golden to', out_file)
print('Wrote metadata to', out / 'simulator_side_by_side_golden.json')
