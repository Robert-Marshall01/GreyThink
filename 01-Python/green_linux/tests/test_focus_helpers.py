from powerapp.gtk.main import focus_app, focus_next_app, focus_prev_app

class FakeArea:
    pass


def test_focus_app_sets_attribute_and_anim():
    a = FakeArea()
    a._per_app_bars = [{'app':'appA'},{'app':'appB'}]
    focus_app(a, 'appA')
    assert getattr(a, '_focused_app', None) == 'appA'
    assert getattr(a, '_focus_anim', None) is not None


def test_focus_next_and_prev_cycle():
    a = FakeArea()
    a._per_app_bars = [{'app':'appA'},{'app':'appB'},{'app':'appC'}]
    assert focus_next_app(a) == 'appA'  # was None -> first
    assert focus_next_app(a) == 'appB'
    assert focus_prev_app(a) == 'appA'
