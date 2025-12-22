import importlib
import json


def test_uploader_posts_when_opted_in_and_endpoint(monkeypatch, tmp_path):
    # set endpoint and API key; patch load_settings to opt-in
    monkeypatch.setenv('POWERAPP_TELEMETRY_ENDPOINT', 'https://example.invalid/telemetry')
    monkeypatch.setenv('POWERAPP_TELEMETRY_API_KEY', 'fakekey')
    monkeypatch.setenv('POWERAPP_TELEMETRY_SAMPLE_RATE', '1.0')

    import powerapp.config as cfg
    monkeypatch.setattr(cfg, 'load_settings', lambda: {'telemetry_opt_in': True})

    # reload telemetry module to pick up env
    import powerapp.telemetry as t
    importlib.reload(t)

    # monkeypatch requests to capture payload (telemetry may have 'requests = None')
    from types import SimpleNamespace

    class DummyResp:
        def raise_for_status(self):
            return None

    calls = {}

    def fake_post(url, data=None, headers=None, timeout=None):
        calls['url'] = url
        calls['data'] = data
        calls['headers'] = headers
        return DummyResp()

    monkeypatch.setattr(t, 'requests', SimpleNamespace(post=fake_post), raising=False)
    # prevent background worker race by disabling _start_worker (we'll call flush() to upload)
    monkeypatch.setattr(t, '_start_worker', lambda: None, raising=False)

    # enqueue and flush
    ok = t.enqueue_event('test_event', {'a': 1})
    assert ok is True
    sent = t.flush()
    assert sent == 1
    assert 'url' in calls and calls['url'] == 'https://example.invalid/telemetry'
    payload = json.loads(calls['data'])
    assert 'events' in payload
    assert payload['events'][0]['event'] == 'test_event'


def test_uploader_respects_sampling(monkeypatch):
    monkeypatch.setenv('POWERAPP_TELEMETRY_ENDPOINT', 'https://example.invalid/telemetry')
    monkeypatch.setenv('POWERAPP_TELEMETRY_SAMPLE_RATE', '0.0')
    import powerapp.config as cfg
    monkeypatch.setattr(cfg, 'load_settings', lambda: {'telemetry_opt_in': True})
    import importlib
    import powerapp.telemetry as t
    importlib.reload(t)

    # monkeypatch requests.post should not be called (patch whole requests if module set to None)
    from types import SimpleNamespace
    called = {'n': 0}
    def fake_post(url, data=None, headers=None, timeout=None):
        called['n'] += 1
        class Dummy: 
            def raise_for_status(self):
                return None
        return Dummy()
    monkeypatch.setattr(t, 'requests', SimpleNamespace(post=fake_post), raising=False)
    monkeypatch.setattr(t, '_start_worker', lambda: None, raising=False)

    ok = t.enqueue_event('ev', {'x': 2})
    assert ok is False  # sampled out
    sent = t.flush()
    assert sent == 0
    assert called['n'] == 0
