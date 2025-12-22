
from powerapp.config import _config_path, load_settings, save_settings, DEFAULTS


def test_config_save_and_load(tmp_path, monkeypatch):
    # point XDG_CONFIG_HOME to tmp
    monkeypatch.setenv('XDG_CONFIG_HOME', str(tmp_path))
    p = _config_path()
    assert p.exists() is False

    cfg = DEFAULTS.copy()
    cfg['zone'] = 'GB'  # example
    cfg['provider'] = 'electricitymap'
    cfg['token'] = 'secret-token'
    cfg['forecast_cache_ttl'] = 600
    cfg['forecast_refresh_cooldown'] = 12
    cfg['sim_gridlines'] = 6
    cfg['sim_grid_opacity'] = 0.12
    save_settings(cfg)

    # file should exist
    assert p.exists()
    loaded = load_settings()
    assert loaded['zone'] == 'GB'
    assert loaded['provider'] == 'electricitymap'
    assert loaded['token'] == 'secret-token'
    assert loaded['window_hours'] == DEFAULTS['window_hours']
    assert loaded['forecast_cache_ttl'] == 600
    assert loaded['forecast_refresh_cooldown'] == 12
    assert loaded['sim_gridlines'] == 6
    assert abs(loaded['sim_grid_opacity'] - 0.12) < 1e-9
