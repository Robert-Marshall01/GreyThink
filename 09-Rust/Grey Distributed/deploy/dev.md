# Development Deployment Guide

This guide covers local development setup for Grey Distributed.

## Prerequisites

- Rust 1.70+ (stable)
- Go 1.21+ (for control plane)
- Docker (for local cluster simulation)
- Make

## Quick Start

```bash
# Clone and build
git clone https://github.com/grey-distributed/grey.git
cd grey-distributed
make build

# Run single node
./deploy/dev.sh single

# Run 3-node local cluster
./deploy/dev.sh cluster
```

## Development Modes

### Single Node Mode

Runs a single Grey node for development:

```bash
./deploy/dev.sh single --port 8080 --data-dir /tmp/grey
```

**Use cases:**
- Unit testing
- Feature development
- Debugging

### Local Cluster Mode

Runs multiple nodes in Docker:

```bash
./deploy/dev.sh cluster --nodes 3 --start-port 8080
```

**Ports:**
- Node 1: 8080 (API), 9080 (Raft), 10080 (metrics)
- Node 2: 8081 (API), 9081 (Raft), 10081 (metrics)
- Node 3: 8082 (API), 9082 (Raft), 10082 (metrics)

## Configuration

### Environment Variables

```bash
# Core settings
GREY_NODE_ID=node-1
GREY_DATA_DIR=/var/lib/grey
GREY_LOG_LEVEL=info

# Network
GREY_API_ADDR=0.0.0.0:8080
GREY_RAFT_ADDR=0.0.0.0:9080
GREY_GOSSIP_ADDR=0.0.0.0:7946

# Cluster
GREY_BOOTSTRAP_PEERS=node-1:9080,node-2:9080,node-3:9080

# Consensus
GREY_ELECTION_TIMEOUT_MS=300
GREY_HEARTBEAT_INTERVAL_MS=100
```

### Config File

```yaml
# grey.yaml
node:
  id: node-1
  data_dir: /var/lib/grey

network:
  api_addr: 0.0.0.0:8080
  raft_addr: 0.0.0.0:9080
  gossip_addr: 0.0.0.0:7946

consensus:
  election_timeout: 300ms
  heartbeat_interval: 100ms

storage:
  engine: rocksdb
  shards: 16
  replication_factor: 3

observability:
  tracing:
    enabled: true
    exporter: jaeger
    endpoint: localhost:6831
  metrics:
    enabled: true
    port: 10080
```

## Debugging

### Enable Debug Logging

```bash
GREY_LOG_LEVEL=debug ./deploy/dev.sh single
```

### Tracing

Local Jaeger for distributed tracing:

```bash
docker run -d --name jaeger \
  -p 6831:6831/udp \
  -p 16686:16686 \
  jaegertracing/all-in-one:latest

# View traces at http://localhost:16686
```

### Profiling

```bash
# CPU profiling
cargo build --release --features profiling
./target/release/greyd --profile cpu --output profile.pb

# Memory profiling
MALLOC_CONF=prof:true ./target/release/greyd
```

## Testing

### Unit Tests

```bash
cargo test --lib
```

### Integration Tests

```bash
# Requires local cluster
./deploy/dev.sh cluster --nodes 3
cargo test --test integration
```

### Chaos Testing

```bash
# Enable chaos features
cargo build --features chaos

# Inject failures
./deploy/dev.sh cluster --chaos --partition-rate 0.1
```

## IDE Setup

### VS Code

Install extensions:
- rust-analyzer
- CodeLLDB (debugging)
- TOML Language Support

`.vscode/settings.json`:
```json
{
  "rust-analyzer.cargo.features": ["full"],
  "rust-analyzer.checkOnSave.command": "clippy"
}
```

### IntelliJ/CLion

1. Install Rust plugin
2. Open project as Cargo project
3. Configure toolchain in Settings → Rust

## Common Issues

### Port Conflicts

```bash
# Check port usage
lsof -i :8080

# Kill process
kill -9 $(lsof -t -i :8080)
```

### Build Failures

```bash
# Clean build
cargo clean
cargo build
```

### Cluster Not Forming

1. Check all nodes started
2. Verify network connectivity
3. Check firewall rules
4. Review logs for Raft errors

## Next Steps

- [Cluster Deployment](cluster.md) - Multi-node production setup
- [Cloud Deployment](cloud.md) - AWS/GCP/Azure deployment
- [Hybrid Deployment](hybrid.md) - Multi-cloud and edge
