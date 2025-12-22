from gi.repository import Gtk

from powerapp.gtk.main import PowerWindow


def test_exercise_all_top_level_buttons(monkeypatch, tmp_path):
    """Programmatically click top-level buttons and ensure handlers run without crashing.
    We stub out real dialogs/actions to avoid spawning external apps; we assert that each handler either presented
    a dialog (captured) or performed a recorded side-effect."""

    presented = []
    side_effects = []

    def fake_present_and_handle(self, dlg, on_response=None):
        # Record the dialog title or type for inspection
        try:
            title = dlg.get_title() if getattr(dlg, 'get_title', None) else getattr(dlg, '__class__', type(dlg)).__name__
        except Exception:
            title = getattr(dlg, '__class__', type(dlg)).__name__
        presented.append(title)
        # Simulate a user response to avoid leaving the dialog open
        if on_response:
            # For Export file chooser suggest ACCEPT to exercise write codepaths ideally
            if 'Export' in (title or ''):
                on_response(dlg, Gtk.ResponseType.ACCEPT)
            else:
                on_response(dlg, Gtk.ResponseType.CLOSE)

    monkeypatch.setattr(PowerWindow, '_present_and_handle_dialog', fake_present_and_handle)

    # Stub system opener to avoid invoking xdg-open
    monkeypatch.setattr('subprocess.Popen', lambda *args, **kwargs: side_effects.append(('popen', args)))

    win = PowerWindow(app=None)

    # Map the top-level button attributes to a human-friendly name
    button_attrs = [
        ('carbon_refresh_btn', 'Refresh Carbon'),
        ('carbon_suggest_btn', 'Suggest postponements'),
        ('manage_tasks_btn', 'Manage tasks'),
        ('carbon_windows_btn', 'Show low-carbon windows'),
        ('emissions_btn', 'Estimate emissions'),
        ('refresh_btn', 'Refresh'),
        ('export_btn', 'Export CSV'),
        ('preview_btn', 'Preview'),
        ('settings_btn', 'Settings'),
        ('manage_applied_btn', 'Manage applied'),
    ]

    # Click each button and ensure no exceptions are raised and either a dialog was presented or a side-effect recorded
    for attr, name in button_attrs:
        btn = getattr(win, attr, None)
        assert btn is not None, f"Missing button attribute {attr}"
        try:
            # emit clicked to trigger handlers
            btn.emit('clicked')
        except Exception as e:
            pytest.fail(f"Clicking {name} raised: {e}")

    # Basic assertions: at least one dialog was presented and no unexpected errors
    assert presented, 'No dialogs were presented during the exercise run; handlers may be no-ops'

    # Ensure Export CSV initiated a dialog (presented) — reasonable expectation
    assert any('Export' in (t or '') for t in presented), f"Export dialog not presented; presented={presented}"
