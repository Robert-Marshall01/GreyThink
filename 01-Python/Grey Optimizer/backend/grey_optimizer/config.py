"""
Grey Optimizer Configuration Module

Manages all configuration settings with sensible defaults and
environment variable overrides for production deployment.

Safety: Configuration values have hard limits to prevent
dangerous settings from being applied.
"""

import os
import json
import logging
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional, Dict, Any, List

try:
    import yaml
    HAS_YAML = True
except ImportError:
    HAS_YAML = False

logger = logging.getLogger(__name__)


@dataclass
class TelemetryConfig:
    """Configuration for telemetry collection."""
    # Sampling interval in seconds
    sample_interval: float = 1.0
    # Warm-up period before enforcement (seconds)
    warmup_period: float = 10.0
    # History retention for metrics (seconds)
    history_retention: int = 3600
    # Enable per-process tracking
    track_processes: bool = True
    # Maximum processes to track
    max_tracked_processes: int = 100


@dataclass
class CPUEnforcementConfig:
    """Configuration for CPU enforcement."""
    enabled: bool = True
    # Target CPU quota as percentage (10 = 10% of a core)
    target_quota_percent: int = 10
    # cgroup cpu.max period in microseconds
    cpu_period_us: int = 100000
    # Use SCHED_IDLE for non-critical processes
    use_sched_idle: bool = True
    # Minimum CPU weight (1-10000)
    min_cpu_weight: int = 1
    # Process name patterns to target
    target_patterns: List[str] = field(default_factory=lambda: ["stress", "cpu-burn"])
    # Process name patterns to never touch
    protected_patterns: List[str] = field(default_factory=lambda: [
        "systemd", "init", "sshd", "kernel", "grey-optimizer"
    ])


@dataclass
class RAMEnforcementConfig:
    """Configuration for RAM enforcement."""
    enabled: bool = True
    # Default memory limit per process (MB)
    default_limit_mb: int = 512
    # Minimum memory limit (safety floor)
    min_limit_mb: int = 64
    # Enable KSM (Kernel Same-page Merging)
    enable_ksm: bool = True
    # KSM sleep interval (milliseconds)
    ksm_sleep_ms: int = 200
    # KSM pages to scan
    ksm_pages_to_scan: int = 100
    # Enable cache drops (requires root)
    enable_cache_drops: bool = False
    # Only drop caches when free memory below this threshold (MB)
    cache_drop_threshold_mb: int = 256
    # Target patterns for memory limits
    target_patterns: List[str] = field(default_factory=lambda: ["memory-hog"])
    # Protected patterns
    protected_patterns: List[str] = field(default_factory=lambda: [
        "systemd", "init", "sshd", "kernel", "grey-optimizer", "Xorg", "gnome"
    ])


@dataclass
class DiskEnforcementConfig:
    """Configuration for disk enforcement."""
    enabled: bool = True
    # Enable log compression
    compress_logs: bool = True
    # Log directories to compress
    log_dirs: List[str] = field(default_factory=lambda: ["/var/log"])
    # Minimum file age for compression (days)
    compress_min_age_days: int = 1
    # Enable deduplication
    enable_dedup: bool = False  # Disabled by default - requires consent
    # Dedup directories
    dedup_dirs: List[str] = field(default_factory=list)
    # Minimum file size for dedup (bytes)
    dedup_min_size: int = 1024 * 1024  # 1MB
    # Enable sparse file conversion
    enable_sparse: bool = False  # Disabled by default - requires consent
    # IO nice level for heavy writers (0-7)
    ionice_level: int = 7
    # Heavy writer threshold (MB/s)
    heavy_writer_threshold_mbs: float = 50.0


@dataclass
class SafetyConfig:
    """Configuration for safety mechanisms."""
    # Enable watchdog
    watchdog_enabled: bool = True
    # Watchdog check interval (seconds)
    watchdog_interval: float = 5.0
    # OOM score threshold for rollback
    oom_score_threshold: int = 900
    # Maximum enforcement actions per minute
    max_actions_per_minute: int = 60
    # Auto-rollback timeout (seconds, 0 = disabled)
    auto_rollback_timeout: int = 300
    # Require consent for destructive actions
    require_consent: bool = True
    # Backup directory for file operations
    backup_dir: str = "/var/lib/grey-optimizer/backups"


@dataclass
class APIConfig:
    """Configuration for the REST/WebSocket API."""
    host: str = "127.0.0.1"
    port: int = 8080
    # Enable CORS for frontend development
    cors_enabled: bool = True
    cors_origins: List[str] = field(default_factory=lambda: ["http://localhost:5173"])
    # WebSocket ping interval (seconds)
    ws_ping_interval: float = 30.0
    # API rate limit (requests per minute)
    rate_limit: int = 120


@dataclass
class PersistenceConfig:
    """Configuration for data persistence."""
    # SQLite database path
    db_path: str = "data/grey_optimizer.db"
    # Proof artifacts directory
    proofs_dir: str = "proofs"
    # HMAC key for signing proofs (auto-generated if not set)
    hmac_key: Optional[str] = None
    # Log retention days
    log_retention_days: int = 30
    # Maximum proof artifacts to keep
    max_proofs: int = 1000


@dataclass
class Config:
    """
    Main configuration container for Grey Optimizer.
    
    Loads configuration from:
    1. Default values
    2. Configuration file (config.json)
    3. Environment variables (GREY_OPTIMIZER_*)
    """
    # Operation mode
    simulation_mode: bool = True  # Safe default
    verbose: bool = False
    
    # Sub-configurations
    telemetry: TelemetryConfig = field(default_factory=TelemetryConfig)
    cpu: CPUEnforcementConfig = field(default_factory=CPUEnforcementConfig)
    ram: RAMEnforcementConfig = field(default_factory=RAMEnforcementConfig)
    disk: DiskEnforcementConfig = field(default_factory=DiskEnforcementConfig)
    safety: SafetyConfig = field(default_factory=SafetyConfig)
    api: APIConfig = field(default_factory=APIConfig)
    persistence: PersistenceConfig = field(default_factory=PersistenceConfig)
    
    @classmethod
    def load(cls, config_path: Optional[str] = None) -> "Config":
        """
        Load configuration from file and environment.
        
        Priority (highest to lowest):
        1. Environment variables
        2. Config file
        3. Defaults
        """
        config = cls()
        
        # Load from file if exists
        if config_path is None:
            config_path = os.environ.get("GREY_OPTIMIZER_CONFIG")
        
        # Search paths for config file
        search_paths = []
        if config_path:
            search_paths.append(config_path)
        
        # Add common config locations
        search_paths.extend([
            "config.yaml",
            "config.json",
            "config/config.yaml",
            "config/config.json",
            "../config/config.yaml",
            "../config.yaml",
            "/etc/grey-optimizer/config.yaml",
            os.path.expanduser("~/.config/grey-optimizer/config.yaml"),
        ])
        
        config_loaded = False
        for path in search_paths:
            if Path(path).exists():
                try:
                    with open(path, "r") as f:
                        if path.endswith(".yaml") or path.endswith(".yml"):
                            if HAS_YAML:
                                file_config = yaml.safe_load(f) or {}
                            else:
                                logger.warning(f"YAML support not available, skipping {path}")
                                continue
                        else:
                            file_config = json.load(f)
                    config = cls._apply_dict(config, file_config)
                    logger.info(f"Loaded configuration from {path}")
                    config_loaded = True
                    break
                except Exception as e:
                    logger.warning(f"Failed to load config file {path}: {e}")
        
        if not config_loaded:
            logger.info("No config file found, using defaults")
        
        # Apply environment overrides
        config = cls._apply_env(config)
        
        # Validate configuration
        config._validate()
        
        return config
    
    @classmethod
    def _apply_dict(cls, config: "Config", data: Dict[str, Any]) -> "Config":
        """Apply dictionary values to configuration."""
        if "simulation_mode" in data:
            config.simulation_mode = bool(data["simulation_mode"])
        if "verbose" in data:
            config.verbose = bool(data["verbose"])
        
        # Apply sub-configurations
        if "telemetry" in data:
            for key, value in data["telemetry"].items():
                if hasattr(config.telemetry, key):
                    setattr(config.telemetry, key, value)
        
        if "cpu" in data:
            for key, value in data["cpu"].items():
                if hasattr(config.cpu, key):
                    setattr(config.cpu, key, value)
        
        if "ram" in data:
            for key, value in data["ram"].items():
                if hasattr(config.ram, key):
                    setattr(config.ram, key, value)
        
        if "disk" in data:
            for key, value in data["disk"].items():
                if hasattr(config.disk, key):
                    setattr(config.disk, key, value)
        
        if "safety" in data:
            for key, value in data["safety"].items():
                if hasattr(config.safety, key):
                    setattr(config.safety, key, value)
        
        if "api" in data:
            for key, value in data["api"].items():
                if hasattr(config.api, key):
                    setattr(config.api, key, value)
        
        if "persistence" in data:
            for key, value in data["persistence"].items():
                if hasattr(config.persistence, key):
                    setattr(config.persistence, key, value)
        
        return config
    
    @classmethod
    def _apply_env(cls, config: "Config") -> "Config":
        """Apply environment variable overrides."""
        env_map = {
            "GREY_OPTIMIZER_SIMULATION": ("simulation_mode", bool),
            "GREY_OPTIMIZER_VERBOSE": ("verbose", bool),
            "GREY_OPTIMIZER_API_HOST": ("api.host", str),
            "GREY_OPTIMIZER_API_PORT": ("api.port", int),
            "GREY_OPTIMIZER_DB_PATH": ("persistence.db_path", str),
            "GREY_OPTIMIZER_HMAC_KEY": ("persistence.hmac_key", str),
        }
        
        for env_var, (path, type_fn) in env_map.items():
            value = os.environ.get(env_var)
            if value is not None:
                try:
                    if type_fn == bool:
                        value = value.lower() in ("true", "1", "yes")
                    else:
                        value = type_fn(value)
                    
                    # Navigate to nested attribute
                    parts = path.split(".")
                    obj = config
                    for part in parts[:-1]:
                        obj = getattr(obj, part)
                    setattr(obj, parts[-1], value)
                    
                except Exception as e:
                    logger.warning(f"Failed to apply env {env_var}: {e}")
        
        return config
    
    def _validate(self) -> None:
        """
        Validate configuration values for safety.
        
        Raises ValueError if any setting is dangerous.
        """
        # CPU validation
        if self.cpu.target_quota_percent < 1:
            raise ValueError("CPU quota must be at least 1%")
        if self.cpu.target_quota_percent > 100:
            raise ValueError("CPU quota cannot exceed 100%")
        
        # RAM validation
        if self.ram.default_limit_mb < self.ram.min_limit_mb:
            raise ValueError(
                f"Default RAM limit ({self.ram.default_limit_mb}MB) must be >= "
                f"minimum ({self.ram.min_limit_mb}MB)"
            )
        if self.ram.min_limit_mb < 32:
            raise ValueError("Minimum RAM limit must be at least 32MB for safety")
        
        # Safety validation
        if self.safety.max_actions_per_minute > 1000:
            raise ValueError("Max actions per minute capped at 1000 for safety")
        
        # API validation
        if self.api.port < 1024 and self.api.port != 80 and self.api.port != 443:
            logger.warning(f"API port {self.api.port} requires root privileges")
        
        logger.info("Configuration validated successfully")
    
    def to_dict(self) -> Dict[str, Any]:
        """Export configuration as dictionary (for API)."""
        return {
            "simulation_mode": self.simulation_mode,
            "verbose": self.verbose,
            "telemetry": {
                "sample_interval": self.telemetry.sample_interval,
                "warmup_period": self.telemetry.warmup_period,
                "track_processes": self.telemetry.track_processes,
            },
            "cpu": {
                "enabled": self.cpu.enabled,
                "target_quota_percent": self.cpu.target_quota_percent,
                "use_sched_idle": self.cpu.use_sched_idle,
            },
            "ram": {
                "enabled": self.ram.enabled,
                "default_limit_mb": self.ram.default_limit_mb,
                "enable_ksm": self.ram.enable_ksm,
            },
            "disk": {
                "enabled": self.disk.enabled,
                "compress_logs": self.disk.compress_logs,
                "enable_dedup": self.disk.enable_dedup,
            },
            "safety": {
                "watchdog_enabled": self.safety.watchdog_enabled,
                "require_consent": self.safety.require_consent,
            },
        }
    
    def save(self, path: str) -> None:
        """Save configuration to file."""
        Path(path).parent.mkdir(parents=True, exist_ok=True)
        with open(path, "w") as f:
            json.dump(self.to_dict(), f, indent=2)
        logger.info(f"Configuration saved to {path}")
