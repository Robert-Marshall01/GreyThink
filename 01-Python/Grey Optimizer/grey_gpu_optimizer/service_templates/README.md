# Grey GPU Optimizer Service Templates

This directory contains systemd service files for running the Grey GPU Optimizer daemon as a system service.

## Available Templates

### grey_optimizer.service (Safe Mode)
- **Purpose**: Monitoring-only mode with dry-run enforcement
- **Interval**: 30 seconds
- **Actions**: Logs recommendations, does not modify processes
- **Use case**: Production monitoring, telemetry gathering

### grey_optimizer_aggressive.service (Aggressive Mode)
- **Purpose**: Active enforcement mode
- **Interval**: 15 seconds
- **Actions**: Actively throttles processes, manages VRAM, controls thermals
- **Use case**: Dedicated GPU servers, ML training clusters
- **WARNING**: Will suspend/resume processes to enforce limits

## Installation

### 1. Copy service file to systemd directory

```bash
# For safe monitoring mode
sudo cp grey_optimizer.service /etc/systemd/system/

# For aggressive enforcement mode
sudo cp grey_optimizer_aggressive.service /etc/systemd/system/grey_optimizer.service
```

### 2. Create required directories

```bash
sudo mkdir -p /var/lib/grey_optimizer/artifacts
sudo mkdir -p /var/log/grey_optimizer
sudo mkdir -p /etc/grey_optimizer
```

### 3. Install the package

```bash
cd /path/to/grey_gpu_optimizer
pip install -e .
```

### 4. Enable and start the service

```bash
sudo systemctl daemon-reload
sudo systemctl enable grey_optimizer
sudo systemctl start grey_optimizer
```

### 5. Check status

```bash
sudo systemctl status grey_optimizer
journalctl -u grey_optimizer -f
```

## Configuration

Create `/etc/grey_optimizer/config.yaml` to customize behavior:

```yaml
# Monitoring interval in seconds
interval: 30

# Optimization mode: safe or aggressive
mode: safe

# Thermal thresholds (Celsius)
thermal:
  cooldown_threshold_c: 83
  resume_threshold_c: 70

# VRAM limits
vram:
  per_process_cap_mb: 8192
  deduplication_enabled: false

# Process management
processes:
  max_concurrent_gpu: 4
  priority_boost: false
  cpu_affinity_enabled: true

# Logging
logging:
  level: INFO
  json_enabled: true
  artifact_retention_days: 30
```

## Troubleshooting

### Permission denied errors
Ensure the service runs as root or with appropriate capabilities:
```bash
sudo setcap cap_sys_nice,cap_kill+ep /usr/bin/python3
```

### GPU not detected
Check that NVIDIA drivers are loaded:
```bash
nvidia-smi
lspci | grep -i nvidia
```

### Service won't start
Check the journal for errors:
```bash
journalctl -u grey_optimizer -e
```

## Security Considerations

- The aggressive mode requires root privileges to manage processes
- Cgroup integration requires write access to /sys/fs/cgroup
- Consider using separate user accounts with limited capabilities
- Enable audit logging for compliance requirements

## Uninstalling

```bash
sudo systemctl stop grey_optimizer
sudo systemctl disable grey_optimizer
sudo rm /etc/systemd/system/grey_optimizer.service
sudo systemctl daemon-reload
```
