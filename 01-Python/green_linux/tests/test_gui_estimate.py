def test_on_estimate_no_crash():
    """Simple smoke test: instantiate PowerWindow and call _on_estimate to ensure it doesn't throw."""
    from powerapp.gtk.main import PowerWindow
    w = PowerWindow(None)
    w._on_estimate()
    # If no exception raised we consider this a pass
