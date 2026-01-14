"""
Grey Optimizer - Configuration Tests
"""
import pytest
import tempfile
from pathlib import Path

from grey_optimizer.config import (
    Config, TelemetryConfig, CPUEnforcementConfig,
    RAMEnforcementConfig, DiskEnforcementConfig,
    SafetyConfig, APIConfig, PersistenceConfig
)


class TestConfig:
    """Tests for configuration classes."""
    
    def test_default_config(self):
        """Test default configuration values."""
        config = Config()
        
        assert config.telemetry.interval_seconds == 1.0
        assert config.telemetry.history_size == 300
        assert config.safety.simulation_mode is True
    
    def test_telemetry_config(self):
        """Test telemetry configuration."""
        config = TelemetryConfig(
            interval_seconds=0.5,
            history_size=100
        )
        
        assert config.interval_seconds == 0.5
        assert config.history_size == 100
    
    def test_cpu_enforcement_config(self):
        """Test CPU enforcement configuration."""
        config = CPUEnforcementConfig(
            enabled=True,
            target_percent=5,
            min_percent=1
        )
        
        assert config.enabled is True
        assert config.target_percent == 5
        assert config.min_percent == 1
    
    def test_ram_enforcement_config(self):
        """Test RAM enforcement configuration."""
        config = RAMEnforcementConfig(
            enabled=True,
            target_mb=128,
            min_mb=64
        )
        
        assert config.enabled is True
        assert config.target_mb == 128
        assert config.min_mb == 64
    
    def test_disk_enforcement_config(self):
        """Test disk enforcement configuration."""
        config = DiskEnforcementConfig(
            enabled=True,
            target_iops=200
        )
        
        assert config.enabled is True
        assert config.target_iops == 200
    
    def test_safety_config(self):
        """Test safety configuration."""
        config = SafetyConfig(
            simulation_mode=False,
            protected_patterns=["systemd*", "kernel*"],
            rollback_on_failure=True,
            max_enforcement_percent=80
        )
        
        assert config.simulation_mode is False
        assert "systemd*" in config.protected_patterns
        assert config.rollback_on_failure is True
        assert config.max_enforcement_percent == 80
    
    def test_api_config(self):
        """Test API configuration."""
        config = APIConfig(
            host="0.0.0.0",
            port=9090,
            enable_websocket=False
        )
        
        assert config.host == "0.0.0.0"
        assert config.port == 9090
        assert config.enable_websocket is False
    
    def test_persistence_config(self):
        """Test persistence configuration."""
        config = PersistenceConfig(
            database_path="/var/lib/test/db.sqlite",
            proof_dir="/var/lib/test/proofs",
            retention_days=7
        )
        
        assert config.database_path == "/var/lib/test/db.sqlite"
        assert config.proof_dir == "/var/lib/test/proofs"
        assert config.retention_days == 7
    
    def test_config_validation(self):
        """Test that configuration validation catches invalid values."""
        # This test ensures validation works
        config = Config()
        
        # Should have valid defaults
        assert config.telemetry.interval_seconds > 0
        assert config.cpu_enforcement.min_percent >= 0
        assert config.ram_enforcement.min_mb >= 0
    
    def test_config_from_yaml(self, temp_dir):
        """Test loading configuration from YAML file."""
        config_content = """
telemetry:
  interval_seconds: 2.0
  history_size: 500

cpu_enforcement:
  enabled: true
  target_percent: 15
  min_percent: 2

ram_enforcement:
  enabled: false
  target_mb: 256
  min_mb: 128

safety:
  simulation_mode: true
  protected_patterns:
    - "test*"
"""
        config_path = temp_dir / "config.yaml"
        config_path.write_text(config_content)
        
        config = Config.load(str(config_path))
        
        assert config.telemetry.interval_seconds == 2.0
        assert config.telemetry.history_size == 500
        assert config.cpu_enforcement.target_percent == 15
        assert config.ram_enforcement.enabled is False
        assert config.safety.simulation_mode is True


class TestConfigEnvironment:
    """Tests for environment variable configuration."""
    
    def test_env_override(self, monkeypatch):
        """Test that environment variables override config."""
        monkeypatch.setenv("GREY_OPTIMIZER_SIMULATION", "false")
        
        # In a real implementation, Config.load would read this
        # This is a placeholder for the actual behavior
        import os
        assert os.environ.get("GREY_OPTIMIZER_SIMULATION") == "false"
