import subprocess
import shutil
from datetime import datetime, timezone

from powerapp.gtk.main import PowerWindow


def test_schedule_job_uses_user_timezone(monkeypatch):
    win = PowerWindow(None)

    # make _get_user_zone return a fixed ZoneInfo-like tz via a small shim
    try:
        from zoneinfo import ZoneInfo
        tzobj = ZoneInfo('America/Los_Angeles')
    except Exception:
        # fallback shim if zoneinfo not available in test env
        class _TZ:
            def utcoffset(self, dt):
                from datetime import timedelta
                return timedelta(hours=-8)
            def tzname(self, dt):
                return 'PST'
            def dst(self, dt):
                from datetime import timedelta
                return timedelta(0)
        tzobj = _TZ()

    monkeypatch.setattr(win, '_get_user_zone', lambda: tzobj)

    captured = {}

    # pretend systemd-run exists
    monkeypatch.setattr(shutil, 'which', lambda name: '/bin/systemd-run' if name == 'systemd-run' else None)

    def fake_check_output(args, stderr=None):
        captured['args'] = list(args)
        return b'Running as unit powerapp-schedule-1234'

    monkeypatch.setattr(subprocess, 'check_output', fake_check_output)

    when_dt = datetime(2025, 1, 1, 12, 0, tzinfo=timezone.utc)
    res = win._schedule_job(when_dt, ['echo', 'hello'])

    assert captured, 'subprocess.check_output was not invoked'
    # join args to a single string to find the on-calendar time
    args_str = ' '.join(captured['args'])
    assert '2025-01-01 04:00:00' in args_str or '20250101040000' in args_str, f'Expected local Pacific time in args, got: {args_str}'
    assert isinstance(res, dict) and res.get('backend') == 'systemd'
