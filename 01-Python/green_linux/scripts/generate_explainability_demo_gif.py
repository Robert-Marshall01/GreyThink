#!/usr/bin/env python3
"""Generate a small demo GIF showing the Explainability panel 'Preview' flow.

This script renders two frames using the existing simulator draw helper and composes a small GIF demonstrating how "Preview" changes the chosen window and estimated savings.
"""
from pathlib import Path
import sys
sys.path.insert(0, str(Path.cwd()))

# allow system gi imports when running under virtualenv
sys.path.append('/usr/lib/python3/dist-packages')
import gi
gi.require_version('Gtk', '4.0')
import cairo
from PIL import Image

from powerapp.gtk.main import draw_simulation
from powerapp.emissions import simulate_postponement, find_low_carbon_windows

OUT = Path('docs/assets')
OUT.mkdir(parents=True, exist_ok=True)

# Sample forecast & suggestion (deterministic)
forecast = [{'timestamp': f'2025-12-17T0{i}:00:00+00:00', 'intensity': v} for i, v in enumerate([200,180,100,90,120,160])]
suggestion = {'task_name': 'DemoTask', 'duration_min': 60, 'power_w': 100.0, 'current_intensity_g': 400.0}

# Choose two windows to demonstrate: baseline (first) and a low-carbon candidate
window_hours = 2
cand_windows = find_low_carbon_windows(forecast, window_hours=window_hours, top_k=3)
if cand_windows:
    cf_start = cand_windows[0]['start']
else:
    cf_start = forecast[2]['timestamp']

baseline_start = forecast[0]['timestamp']

# Render two frames
WIDTH, HEIGHT = 640, 240
frames = []
labels = []
for label, start in (('Baseline', baseline_start), ('Preview', cf_start)):
    res = simulate_postponement(suggestion, forecast, start, window_hours=window_hours, current_intensity=suggestion.get('current_intensity_g'), per_app_series=suggestion.get('per_app_series'))

    # Create a Cairo surface and draw timeline
    surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, WIDTH, HEIGHT - 40)
    cr = cairo.Context(surface)

    # Attach a minimal fake area with expected attributes for draw_simulation
    class FakeArea:
        pass
    area = FakeArea()
    area._sim_series = res.get('series', [])
    area._sim_window_start = res.get('window_start')
    area._sim_window_hours = window_hours
    # per-app bars from result
    impacts = res.get('per_app_impacts', {})
    per_app_bars = []
    from powerapp.gtk.main import color_for_app
    total_power = sum(float(dd.get('power_w', 0.0)) for dd in impacts.values()) or 1.0
    for a, dd in impacts.items():
        frac = float(dd.get('power_w', 0.0)) / total_power if total_power else 0.0
        per_app_bars.append({'app': a, 'fraction': frac, 'color': color_for_app(a), 'kwh': dd.get('kwh'), 'co2_kg': dd.get('co2_kg')})
    area._per_app_bars = per_app_bars

    draw_simulation(area, cr, WIDTH, HEIGHT - 40, settings={'sim_gridlines': 4, 'sim_grid_opacity': 0.06}, scrub_frac=0.5)

    # write Cairo surface to temporary PNG
    tmp_png = OUT / f'_tmp_{label}.png'
    surface.write_to_png(str(tmp_png))

    # open with PIL and compose a taller canvas with label text
    img = Image.open(str(tmp_png)).convert('RGBA')
    canvas = Image.new('RGBA', (WIDTH, HEIGHT), (255, 255, 255, 255))
    canvas.paste(img, (0, 0))

    # Draw a simple caption using PIL (Pillow ImageDraw)
    from PIL import ImageDraw, ImageFont
    draw = ImageDraw.Draw(canvas)
    # Use a safe default bitmap font to avoid large-truetype issues in minimal environments
    font = ImageFont.load_default()
    caption = f"{label}: window={start}, saves={res.get('savings_kg') or '-'} kg CO2"
    draw.rectangle([(0, HEIGHT - 40), (WIDTH, HEIGHT)], fill=(250, 250, 250, 255))
    draw.text((8, HEIGHT - 32), caption, fill=(20, 20, 20), font=font)

    frames.append(canvas)
    labels.append(caption)

# Save animated GIF
out_gif = OUT / 'explainability_preview_demo.gif'
frames[0].save(str(out_gif), format='GIF', save_all=True, append_images=frames[1:], loop=0, duration=1000, disposal=2)
print('Wrote demo GIF to', out_gif)
