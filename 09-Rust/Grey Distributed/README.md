# Grey Distributed

## ⚠️ Warning: Experimental & Unstable

Grey Distributed is an **experimental system** under active development.  
Features, APIs, and behaviors may change without notice, and stability is **not guaranteed**.  
Do not rely on this project for production workloads or critical infrastructure.  
Use at your own risk, and always validate results before deployment.

**Specialist-Depth Distributed Systems Architecture**

Grey Distributed is the distributed execution, coordination, and governance layer for the Grey ecosystem (GreyAV, Grey Optimizer, Grey Multi-Tenant, Grey Computer, Grey AI Internal).

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                      Grey Distributed                            │
├─────────────────────────────────────────────────────────────────┤
│  ┌───────────────┐  ┌───────────────┐  ┌───────────────┐       │
│  │   Consensus   │  │   Scheduler   │  │   Governance  │       │
│  │     (Raft)    │  │ (Deterministic)│  │   (Quotas)    │       │
│  └───────┬───────┘  └───────┬───────┘  └───────┬───────┘       │
│          │                  │                  │                │
│  ┌───────▼──────────────────▼──────────────────▼───────┐       │
│  │              Cluster State (Event-Sourced)           │       │
│  └───────┬──────────────────┬──────────────────┬───────┘       │
│          │                  │                  │                │
│  ┌───────▼───────┐  ┌───────▼───────┐  ┌───────▼───────┐       │
│  │    Storage    │  │   Networking  │  │  Observability│       │
│  │   (Sharded)   │  │ (Grey Proto)  │  │   (Tracing)   │       │
│  └───────────────┘  └───────────────┘  └───────────────┘       │
├─────────────────────────────────────────────────────────────────┤
│  ┌───────────────┐  ┌───────────────┐  ┌───────────────┐       │
│  │Fault Tolerance│  │    Security   │  │  Deployment   │       │
│  │ (Self-Heal)   │  │ (Attestation) │  │   (Hybrid)    │       │
│  └───────────────┘  └───────────────┘  └───────────────┘       │
└─────────────────────────────────────────────────────────────────┘
```

## Project Structure

```
grey-distributed/
├── pkg/
│   ├── consensus/      # Raft consensus implementation
│   ├── state/          # Event-sourced cluster state
│   ├── scheduler/      # Deterministic task scheduler
│   ├── governance/     # Resource quotas and throttling
│   ├── storage/        # Sharding, replication, quorum
│   ├── network/        # Grey Protocol v2, backpressure
│   ├── fault/          # Failure detection, self-healing
│   ├── observability/  # Distributed tracing, replay
│   ├── security/       # Node attestation, signed state
│   └── types/          # Shared type definitions
├── cmd/
│   ├── greyd/          # Main daemon
│   └── greyctl/        # CLI controller
├── deploy/
│   ├── dev/            # Single-node development
│   ├── local/          # Local multi-node cluster
│   ├── cloud/          # Cloud-scale deployment
│   └── hybrid/         # Edge + cloud federation
└── docs/
    └── ARCHITECTURE.md # Detailed architecture decisions
```

## Quick Start

```bash
# Development mode (single node)
go run ./cmd/greyd --mode=dev

# Local cluster (3 nodes)
./deploy/local/start-cluster.sh

# Controller
go run ./cmd/greyctl cluster status
```

## License

Proprietary - Grey Systems
