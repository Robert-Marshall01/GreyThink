import json
import io

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


def test_forecast_cache_used(monkeypatch, tmp_path):
    monkeypatch.setenv('XDG_CONFIG_HOME', str(tmp_path))
    monkeypatch.setenv('XDG_CACHE_HOME', str(tmp_path))
    save_settings({'provider': 'electricitymap', 'token': 'tok', 'zone': 'GB', 'forecast_cache_ttl': 600})

    series = [{'datetime': '2025-12-15T00:00:00+00:00', 'carbonIntensity': 100},
              {'datetime': '2025-12-15T01:00:00+00:00', 'carbonIntensity': 110}]
    data = json.dumps({'data': {'data': series}}).encode('utf-8')

    call = {'count': 0}
    def fake_urlopen(req, timeout=8):
        call['count'] += 1
        return DummyResponse(data)
    monkeypatch.setattr('urllib.request.urlopen', fake_urlopen)

    # first call should fetch
    res1 = emissions.fetch_forecast(zone='GB', hours=2)
    assert call['count'] == 1
    assert len(res1) == 2

    # second call should use cache and not increase urlopen calls
    res2 = emissions.fetch_forecast(zone='GB', hours=2)
    assert call['count'] == 1
    assert res1 == res2


def test_forecast_cache_expires(monkeypatch, tmp_path):
    monkeypatch.setenv('XDG_CONFIG_HOME', str(tmp_path))
    monkeypatch.setenv('XDG_CACHE_HOME', str(tmp_path))
    save_settings({'provider': 'electricitymap', 'token': 'tok', 'zone': 'GB', 'forecast_cache_ttl': 0})

    series = [{'datetime': '2025-12-15T00:00:00+00:00', 'carbonIntensity': 100},
              {'datetime': '2025-12-15T01:00:00+00:00', 'carbonIntensity': 110}]
    data = json.dumps({'data': {'data': series}}).encode('utf-8')

    call = {'count': 0}
    def fake_urlopen(req, timeout=8):
        call['count'] += 1
        return DummyResponse(data)
    monkeypatch.setattr('urllib.request.urlopen', fake_urlopen)

    res1 = emissions.fetch_forecast(zone='GB', hours=2)
    assert call['count'] == 1
    # cache_ttl == 0 => always fetch
    res2 = emissions.fetch_forecast(zone='GB', hours=2)
    assert call['count'] == 2
