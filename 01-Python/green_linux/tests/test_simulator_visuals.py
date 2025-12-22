import cairo
from powerapp.gtk.main import draw_simulation


def _make_area_with_series():
    class FakeArea:
        pass
    area = FakeArea()
    # simple series and window
    area._sim_series = [{'timestamp': '2025-12-17T00:00:00+00:00', 'intensity': 200.0}, {'timestamp': '2025-12-17T01:00:00+00:00', 'intensity': 150.0}, {'timestamp': '2025-12-17T02:00:00+00:00', 'intensity': 90.0}, {'timestamp': '2025-12-17T03:00:00+00:00', 'intensity': 80.0}]
    area._sim_window_start = area._sim_series[2]['timestamp']
    area._sim_window_hours = 2
    # per_app_bars with fractions
    area._per_app_bars = [{'app': 'appA', 'fraction': 0.75, 'color': (0.4,0.5,0.6), 'kwh':0.1, 'co2_kg':0.01}, {'app':'appB','fraction':0.25,'color':(0.6,0.4,0.3),'kwh':0.033,'co2_kg':0.003}]
    return area


def test_draw_animation_changes_bar_widths():
    area = _make_area_with_series()
    WIDTH, HEIGHT = 520, 160
    def render(area, anim):
        surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, WIDTH, HEIGHT)
        cr = cairo.Context(surface)
        draw_simulation(area, cr, WIDTH, HEIGHT, settings={'sim_gridlines':4,'sim_grid_opacity':0.06}, anim_progress=anim)
        return getattr(area, '_per_app_pixel_bars', [])

    pb0 = render(area, 0.0)
    pb1 = render(area, 1.0)
    assert pb0 and pb1
    # widths should differ and be larger at anim_progress=1.0
    sum0 = sum(int(p.get('width_px',0)) for p in pb0)
    sum1 = sum(int(p.get('width_px',0)) for p in pb1)
    assert sum1 >= sum0


def test_draw_side_by_side_produces_left_and_right_pixel_bars():
    area = _make_area_with_series()
    # create a 'baseline' series with a shifted set
    baseline = list(area._sim_series)
    # tweak baseline intensities to simulate 'now' view
    for p in baseline:
        p['intensity'] = p['intensity'] + 20
    WIDTH, HEIGHT = 520, 160
    surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, WIDTH, HEIGHT)
    cr = cairo.Context(surface)
    draw_simulation(area, cr, WIDTH, HEIGHT, series_b=baseline, side_by_side=True, settings={'sim_gridlines':4,'sim_grid_opacity':0.06})
    left = getattr(area, '_per_app_pixel_bars_left', [])
    right = getattr(area, '_per_app_pixel_bars_right', [])
    assert left and right
    # combined metadata convenience
    assert isinstance(getattr(area, '_per_app_pixel_bars_combined', {}), dict)


def test_focus_sets_focused_flag_in_pixel_bars():
    area = _make_area_with_series()
    WIDTH, HEIGHT = 520, 160
    # focus appA and render
    area._focused_app = 'appA'
    surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, WIDTH, HEIGHT)
    cr = cairo.Context(surface)
    draw_simulation(area, cr, WIDTH, HEIGHT, settings={'sim_gridlines':4,'sim_grid_opacity':0.06})
    pb = getattr(area, '_per_app_pixel_bars', [])
    assert any(p.get('focused') for p in pb)
    focused = [p for p in pb if p.get('focused')]
    assert focused and focused[0]['app'] == 'appA'


def test_scrubber_sets_last_frac_on_panels():
    area = _make_area_with_series()
    WIDTH, HEIGHT = 520, 160
    surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, WIDTH, HEIGHT)
    cr = cairo.Context(surface)
    draw_simulation(area, cr, WIDTH, HEIGHT, scrub_frac=0.25, settings={'sim_gridlines':4,'sim_grid_opacity':0.06})
    assert abs(getattr(area, '_last_scrub_frac_left', 0.0) - 0.25) < 1e-6
    # right panel when series_b provided
    baseline = list(area._sim_series)
    draw_simulation(area, cr, WIDTH, HEIGHT, series_b=baseline, side_by_side=True, scrub_frac=0.4, settings={'sim_gridlines':4,'sim_grid_opacity':0.06})
    assert abs(getattr(area, '_last_scrub_frac_left', 0.0) - 0.4) < 1e-6
    assert abs(getattr(area, '_last_scrub_frac_right', 0.0) - 0.4) < 1e-6


def test_compute_animation_sequence_converges():
    from powerapp.gtk.main import compute_animation_sequence
    seq = compute_animation_sequence(0.0, 1.0, steps=6)
    assert len(seq) == 6
    assert abs(seq[-1] - 1.0) < 1e-6
    # monotonic increase
    assert all(x < y for x, y in zip(seq, seq[1:]))


def test_compute_animation_sequence_partial_target():
    from powerapp.gtk.main import compute_animation_sequence
    seq = compute_animation_sequence(0.2, 0.6, steps=5)
    assert abs(seq[-1] - 0.6) < 1e-6
    assert seq[0] > 0.2 and seq[-1] >= seq[0]


def test_keyboard_scrub_left_right():
    try:
        from powerapp.gtk.main import PowerWindow
        from gi.repository import Gdk
    except Exception:
        import pytest; pytest.skip('GTK/Gdk not available')
    try:
        win = PowerWindow(app=None)
    except Exception:
        import pytest; pytest.skip('Cannot instantiate PowerWindow in this environment')
    assert hasattr(win, '_scrub')
    win._scrub.set_value(0.5)
    class Ev: pass
    ev = Ev(); ev.keyval = Gdk.KEY_Left; ev.state = 0
    win._on_keypress(win, ev)
    assert win._scrub.get_value() < 0.5
    ev2 = Ev(); ev2.keyval = Gdk.KEY_Right; ev2.state = 0
    before = win._scrub.get_value()
    win._on_keypress(win, ev2)
    assert win._scrub.get_value() > before


def test_keyboard_scrub_home_end_and_page():
    try:
        from powerapp.gtk.main import PowerWindow
        from gi.repository import Gdk
    except Exception:
        import pytest; pytest.skip('GTK/Gdk not available')
    try:
        win = PowerWindow(app=None)
    except Exception:
        import pytest; pytest.skip('Cannot instantiate PowerWindow in this environment')
    win._scrub.set_value(0.5)
    class Ev: pass
    ev = Ev(); ev.keyval = Gdk.KEY_Home; ev.state = 0
    win._on_keypress(win, ev)
    assert abs(win._scrub.get_value() - 0.0) < 1e-6
    ev.keyval = Gdk.KEY_End
    win._on_keypress(win, ev)
    assert abs(win._scrub.get_value() - 1.0) < 1e-6
    ev.keyval = Gdk.KEY_Page_Up
    win._scrub.set_value(0.2)
    win._on_keypress(win, ev)
    assert win._scrub.get_value() > 0.2
    ev.keyval = Gdk.KEY_Page_Down
    win._scrub.set_value(0.8)
    win._on_keypress(win, ev)
    assert win._scrub.get_value() < 0.8
