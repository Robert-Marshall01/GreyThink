"""
Grey Optimizer - Test Suite Configuration
"""
import pytest
import asyncio
import tempfile
import os
from pathlib import Path


@pytest.fixture(scope="session")
def event_loop():
    """Create an instance of the default event loop for each test case."""
    loop = asyncio.get_event_loop_policy().new_event_loop()
    yield loop
    loop.close()


@pytest.fixture
def temp_dir():
    """Create a temporary directory for tests."""
    with tempfile.TemporaryDirectory() as tmpdir:
        yield Path(tmpdir)


@pytest.fixture
def mock_config(temp_dir):
    """Create a mock configuration for testing."""
    from grey_optimizer.config import (
        Config, TelemetryConfig, CPUEnforcementConfig,
        RAMEnforcementConfig, DiskEnforcementConfig,
        SafetyConfig, APIConfig, PersistenceConfig
    )
    
    return Config(
        telemetry=TelemetryConfig(
            interval_seconds=0.1,
            history_size=10
        ),
        cpu_enforcement=CPUEnforcementConfig(
            enabled=True,
            target_percent=10,
            min_percent=1
        ),
        ram_enforcement=RAMEnforcementConfig(
            enabled=True,
            target_mb=64,
            min_mb=64
        ),
        disk_enforcement=DiskEnforcementConfig(
            enabled=True,
            target_iops=100
        ),
        safety=SafetyConfig(
            simulation_mode=True,  # Always simulate in tests
            protected_patterns=["test_*", "pytest*"],
            rollback_on_failure=True,
            max_enforcement_percent=90
        ),
        api=APIConfig(
            host="127.0.0.1",
            port=0,  # Random port
            enable_websocket=True
        ),
        persistence=PersistenceConfig(
            database_path=str(temp_dir / "test_audit.db"),
            proof_dir=str(temp_dir / "proofs"),
            retention_days=1
        )
    )


@pytest.fixture
def mock_metrics():
    """Create mock telemetry metrics."""
    return {
        'cpu': {
            'percent': 45.0,
            'per_cpu': [40.0, 50.0, 45.0, 45.0],
            'load_avg': (1.5, 1.2, 1.0)
        },
        'ram': {
            'percent': 60.0,
            'total_mb': 16384,
            'available_mb': 6553,
            'used_mb': 9830
        },
        'disk': {
            'read_bytes_per_sec': 50_000_000,
            'write_bytes_per_sec': 25_000_000,
            'read_iops': 500,
            'write_iops': 250
        }
    }
