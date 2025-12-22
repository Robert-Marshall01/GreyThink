import json
import io
import urllib.error

from powerapp.config import save_settings
import powerapp.emissions as emissions


class DummyResponse:
    def __init__(self, data_bytes):
        self._buf = io.BytesIO(data_bytes)
    def read(self):
        return self._buf.read()
    def __enter__(self):
        return self
    def __exit__(self, exc_type, exc, tb):
        return False


def test_fetch_current_with_electricitymap_success(monkeypatch, tmp_path):
    # write config to use electricitymap and token
    monkeypatch.setenv('XDG_CONFIG_HOME', str(tmp_path))
    save_settings({'provider': 'electricitymap', 'token': 'tok', 'zone': 'GB'})

    # stub urlopen to return expected JSON
    data = json.dumps({'data': {'carbonIntensity': 123}}).encode('utf-8')
    def fake_urlopen(req, timeout=8):
        return DummyResponse(data)
    monkeypatch.setattr('urllib.request.urlopen', fake_urlopen)

    r = emissions.fetch_current_intensity(zone='GB')
    assert r['source'] == 'electricitymap'
    assert r['intensity'] == 123.0


def test_fetch_current_electricitymap_rate_limit_falls_back(monkeypatch, tmp_path):
    monkeypatch.setenv('XDG_CONFIG_HOME', str(tmp_path))
    save_settings({'provider': 'electricitymap', 'token': 'tok', 'zone': 'GB'})

    # stub urlopen to raise HTTPError 429 consistently
    class FakeHTTPError(urllib.error.HTTPError):
        def __init__(self):
            super().__init__('url', 429, 'Too Many Requests', hdrs=None, fp=None)
    def fake_urlopen(req, timeout=8):
        raise FakeHTTPError()
    monkeypatch.setattr('urllib.request.urlopen', fake_urlopen)

    r = emissions.fetch_current_intensity(zone='GB', max_retries=2, backoff_factor=0.001)
    assert r['source'] == 'mock'
    assert 'failure' in r['note'] or 'error' in r['note']


def test_fetch_forecast_with_electricitymap_success(monkeypatch, tmp_path):
    monkeypatch.setenv('XDG_CONFIG_HOME', str(tmp_path))
    save_settings({'provider': 'electricitymap', 'token': 'tok', 'zone': 'GB'})

    # craft series
    series = [{'datetime': '2025-12-15T00:00:00+00:00', 'carbonIntensity': 100},
              {'datetime': '2025-12-15T01:00:00+00:00', 'carbonIntensity': 110}]
    data = json.dumps({'data': {'data': series}}).encode('utf-8')

    def fake_urlopen(req, timeout=8):
        return DummyResponse(data)
    monkeypatch.setattr('urllib.request.urlopen', fake_urlopen)

    res = emissions.fetch_forecast(zone='GB', hours=2)
    assert len(res) == 2
    assert res[0]['intensity'] == 100.0
    assert res[1]['intensity'] == 110.0


def test_fetch_forecast_rate_limit_backoff(monkeypatch, tmp_path):
    monkeypatch.setenv('XDG_CONFIG_HOME', str(tmp_path))
    save_settings({'provider': 'electricitymap', 'token': 'tok', 'zone': 'GB'})

    class FakeHTTPError(urllib.error.HTTPError):
        def __init__(self):
            super().__init__('url', 429, 'Too Many Requests', hdrs=None, fp=None)
    def fake_urlopen(req, timeout=8):
        raise FakeHTTPError()
    monkeypatch.setattr('urllib.request.urlopen', fake_urlopen)

    res = emissions.fetch_forecast(zone='GB', hours=2, max_retries=2, backoff_factor=0.001)
    # when provider fails we get synthetic forecast
    assert len(res) == 2
    assert all('intensity' in p for p in res)
    assert all(p.get('source') == 'mock' for p in res)
