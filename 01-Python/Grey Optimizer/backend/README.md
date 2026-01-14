# Grey Optimizer - Backend Package

This is the Python backend for Grey Optimizer. It provides:

- **Daemon**: Main orchestration loop that coordinates all subsystems
- **Telemetry**: Real-time collection of CPU, RAM, and Disk metrics
- **Policy Engine**: Decision-making for enforcement actions
- **Enforcement Manager**: Applies optimizations via cgroups, scheduler, KSM
- **API Server**: REST API and WebSocket for dashboard communication
- **Persistence**: SQLite audit log with hash chain, proof generation

## Installation

### Development

```bash
# Create virtual environment
python3 -m venv venv
source venv/bin/activate

# Install in development mode
pip install -e ".[dev]"
```

### Production

```bash
# Install with production dependencies
pip install .
```

## Usage

### Run in Simulation Mode (Development)

```bash
export GREY_OPTIMIZER_CONFIG=../config.dev.yaml
python -m grey_optimizer.daemon
```

### Run with Custom Config

```bash
export GREY_OPTIMIZER_CONFIG=/path/to/config.yaml
python -m grey_optimizer.daemon
```

## Configuration

See `config.yaml.example` for all configuration options.

Key settings:

```yaml
safety:
  simulation_mode: true  # Set to false for live enforcement
  protected_patterns:
    - "systemd*"
    - "kernel*"
```

## Testing

```bash
# Run all tests
pytest

# Run with coverage
pytest --cov=grey_optimizer --cov-report=html

# Run specific test file
pytest tests/test_policy.py -v
```

## Architecture

```
grey_optimizer/
├── __init__.py          # Package initialization
├── config.py            # Configuration dataclasses
├── daemon.py            # Main daemon entry point
├── telemetry/           # Metrics collection
│   ├── __init__.py
│   ├── collector.py     # Aggregates all telemetry
│   ├── cpu.py           # CPU metrics
│   ├── ram.py           # RAM metrics
│   └── disk.py          # Disk I/O metrics
├── policy/              # Decision engine
│   ├── __init__.py
│   └── engine.py        # Policy evaluation
├── enforcement/         # Action application
│   ├── __init__.py
│   └── manager.py       # Coordinates controllers
├── api/                 # HTTP/WebSocket server
│   ├── __init__.py
│   └── server.py        # aiohttp server
└── persistence/         # Data storage
    ├── __init__.py
    ├── database.py      # SQLite audit log
    └── proof_generator.py  # Signed proof artifacts
```
