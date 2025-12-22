from powerapp.system.bugreport import anonymize_diagnostics


def test_anonymize_replaces_email_ip_and_home_path():
    data = {
        'user': 'alice',
        'email': 'alice@example.com',
        'last_ip': '192.168.1.55',
        'home': '/home/alice/.config/somefile',
        'details': 'contact me at bob@company.org or 10.0.0.1',
    }

    out = anonymize_diagnostics(data)
    assert out['user'] == '<REDACTED_USER>'
    assert '<REDACTED_EMAIL>' in out['email'] or out['email'].startswith('<REDACTED')
    assert '<REDACTED_IP>' in out['last_ip']
    assert '/home/<REDACTED>' in out['home']
    assert '<REDACTED_EMAIL>' in out['details'] or '<REDACTED_IP>' in out['details']


def test_anonymize_preserves_other_fields():
    data = {'candidate_used': 'gdbus', 'current_profile': 'balanced'}
    out = anonymize_diagnostics(data)
    assert out['candidate_used'] == 'gdbus'
    assert out['current_profile'] == 'balanced'