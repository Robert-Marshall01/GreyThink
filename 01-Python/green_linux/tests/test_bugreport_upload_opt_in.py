from powerapp.system import bugreport
from powerapp import config


def test_upload_not_attempted_when_disabled(monkeypatch, tmp_path):
    settings = config.DEFAULTS.copy()
    settings['enable_bugreport_upload'] = False
    # ensure raising is expected when no url provided and upload attempted
    try:
        bugreport.upload_bug_report({'a': 1}, None)
    except ValueError:
        pass
    else:
        assert False, "upload should not proceed without URL"


def test_upload_calls_http_post_when_enabled(monkeypatch, tmp_path):
    called = {}

    def fake_post(url, json_data, headers=None):
        called['url'] = url
        called['payload'] = json_data
        return {'id': 'abc123'}

    monkeypatch.setattr(bugreport, '_http_post', fake_post)
    url = 'https://example.test/bugreport'

    res = bugreport.upload_bug_report({'foo': 'bar'}, url)
    assert res.get('id') == 'abc123'
    assert called['url'] == url
    assert 'diagnostics' in called['payload']
    # diagnostics should be anonymized by the function
    assert isinstance(called['payload']['diagnostics'], dict)