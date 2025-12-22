import sys
from pathlib import Path
import pytest

# Ensure repository root is on sys.path for imports
ROOT = Path(__file__).resolve().parents[1]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

# Make system-installed Python dist-packages available in virtualenvs so 'gi' can be imported
# (PyGObject is often installed into /usr/lib/python3/dist-packages by the system package manager)
SYSTEM_DIST = '/usr/lib/python3/dist-packages'
if Path(SYSTEM_DIST).exists() and SYSTEM_DIST not in sys.path:
    sys.path.insert(0, SYSTEM_DIST)


# Isolate XDG config and cache directories per-test to avoid cross-test state leakage
# Tests that read/write config.json rely on XDG_CONFIG_HOME; use tmp_path to create
# a unique directory for each test run so settings are isolated and deterministic.
@pytest.fixture(autouse=True)
def isolate_xdg(monkeypatch, tmp_path):
    cfg = tmp_path / 'xdg_config'
    cfg.mkdir()
    monkeypatch.setenv('XDG_CONFIG_HOME', str(cfg))
    monkeypatch.setenv('XDG_CACHE_HOME', str(tmp_path / 'xdg_cache'))
    # monkeypatch will automatically undo env var changes after the test
    yield

