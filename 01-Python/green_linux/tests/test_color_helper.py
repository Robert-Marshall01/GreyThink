from powerapp.gtk.main import color_for_app


def test_color_for_app_deterministic_and_range():
    # Deterministic for same name
    c1 = color_for_app('myapp')
    c2 = color_for_app('myapp')
    assert c1 == c2
    # tuple of three floats
    assert isinstance(c1, tuple) and len(c1) == 3
    for v in c1:
        assert isinstance(v, float)
        # pastel bias 0.2 .. 0.8
        assert 0.19 <= v <= 0.81


def test_color_for_app_differs_between_names():
    a = color_for_app('appA')
    b = color_for_app('appB')
    assert a != b, 'Different names should produce different colors (very likely)'


def test_palette_color_variants():
    from powerapp.gtk.main import palette_color_for_app
    d = palette_color_for_app('myapp', 'default')
    hc = palette_color_for_app('myapp', 'high_contrast')
    cb = palette_color_for_app('myapp', 'colorblind')
    assert isinstance(d, tuple) and isinstance(hc, tuple) and isinstance(cb, tuple)
    assert d != hc or d != cb
    # ensure values are float triples
    for v in (d, hc, cb):
        assert len(v) == 3 and all(isinstance(x, float) for x in v)
