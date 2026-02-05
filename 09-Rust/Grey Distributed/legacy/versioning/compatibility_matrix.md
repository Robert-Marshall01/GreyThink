# Grey Distributed — Compatibility Matrix

This document defines compatibility relationships across versions, components, and protocols.

## Table of Contents

1. [Version Compatibility](#version-compatibility)
2. [Protocol Compatibility](#protocol-compatibility)
3. [API Compatibility](#api-compatibility)
4. [Storage Compatibility](#storage-compatibility)
5. [Federation Compatibility](#federation-compatibility)
6. [Upgrade Paths](#upgrade-paths)

---

## Version Compatibility

### Core Version Matrix

| Client Version | Server v1.0 | Server v1.1 | Server v1.2 | Server v2.0 | Server v2.1 |
|----------------|-------------|-------------|-------------|-------------|-------------|
| greyctl v1.0   | ✅ Full     | ✅ Full     | ⚠️ Limited  | ❌ None     | ❌ None     |
| greyctl v1.1   | ✅ Full     | ✅ Full     | ✅ Full     | ⚠️ Limited  | ⚠️ Limited  |
| greyctl v1.2   | ⚠️ Limited  | ✅ Full     | ✅ Full     | ✅ Full     | ✅ Full     |
| greyctl v2.0   | ❌ None     | ⚠️ Limited  | ✅ Full     | ✅ Full     | ✅ Full     |
| greyctl v2.1   | ❌ None     | ❌ None     | ⚠️ Limited  | ✅ Full     | ✅ Full     |

**Legend**:
- ✅ **Full**: All features work as expected
- ⚠️ **Limited**: Core features work; new features unavailable
- ❌ **None**: Incompatible; upgrade required

### SDK Version Matrix

| SDK | Grey v1.0 | Grey v1.1 | Grey v1.2 | Grey v2.0 |
|-----|-----------|-----------|-----------|-----------|
| Python SDK 0.1.x | ✅ Full | ✅ Full | ⚠️ Limited | ❌ None |
| Python SDK 0.2.x | ⚠️ Limited | ✅ Full | ✅ Full | ✅ Full |
| Python SDK 1.0.x | ❌ None | ⚠️ Limited | ✅ Full | ✅ Full |
| JS SDK 0.1.x | ✅ Full | ✅ Full | ⚠️ Limited | ❌ None |
| JS SDK 0.2.x | ⚠️ Limited | ✅ Full | ✅ Full | ✅ Full |
| JS SDK 1.0.x | ❌ None | ⚠️ Limited | ✅ Full | ✅ Full |

---

## Protocol Compatibility

### Consensus Protocol Matrix

| Cluster A Version | Cluster B v1.0 | Cluster B v1.1 | Cluster B v1.2 | Cluster B v2.0 |
|-------------------|----------------|----------------|----------------|----------------|
| v1.0 (Raft) | ✅ Compatible | ✅ Compatible | ✅ Compatible | ⚠️ Bridge mode |
| v1.1 (Raft) | ✅ Compatible | ✅ Compatible | ✅ Compatible | ⚠️ Bridge mode |
| v1.2 (Raft) | ✅ Compatible | ✅ Compatible | ✅ Compatible | ✅ Dual mode |
| v2.0 (Horizon) | ⚠️ Bridge mode | ⚠️ Bridge mode | ✅ Dual mode | ✅ Compatible |

### Protocol Version Details

```
Consensus Protocols:
━━━━━━━━━━━━━━━━━━━

  Raft v1 (v1.0.0 - v1.2.x)
  ├─ Leader election: Standard Raft
  ├─ Log replication: Batched
  ├─ Snapshot: LZ4 compressed
  └─ Max nodes: 100

  Horizon v1 (v2.0.0+)
  ├─ Consensus: BFT (PBFT-derived)
  ├─ Finality: 2-round
  ├─ Attestation: Cryptographic
  └─ Max nodes: 1,000

  Bridge Mode:
  ├─ Translates between Raft and Horizon
  ├─ Adds ~50ms latency overhead
  ├─ Read-only for cross-protocol queries
  └─ Write operations require protocol match
```

### Network Protocol Matrix

| Protocol | v1.0 | v1.1 | v1.2 | v2.0 | Notes |
|----------|------|------|------|------|-------|
| gRPC | v1 | v1 | v1.1 | v2 | v2 adds streaming |
| REST | v1 | v1 | v1 | v1 | Stable across versions |
| WebSocket | — | v1 | v1 | v1 | Added in v1.1 |
| QUIC | — | — | — | v1 | Added in v2.0 |

---

## API Compatibility

### REST API Versions

| API Version | Introduced | Deprecated | Removed | LTS Support Until |
|-------------|------------|------------|---------|-------------------|
| /api/v1 | v1.0.0 | v2.0.0 | v3.0.0 | 2028-06 |
| /api/v2 | v2.0.0 | — | — | 2031-06 |

### API Compatibility Guarantees

```
API Stability Levels:
━━━━━━━━━━━━━━━━━━━━

  stable/v1/*
  ├─ Breaking changes: Never (within major version)
  ├─ New fields: Added with defaults
  ├─ Removed fields: Deprecated first, removed in next major
  └─ Backward compatible: Always

  beta/v1/*
  ├─ Breaking changes: Possible with 3-month notice
  ├─ Stability: API shape stable, behavior may change
  └─ Production use: Allowed with caution

  alpha/v1/*
  ├─ Breaking changes: Any time without notice
  ├─ Stability: None guaranteed
  └─ Production use: Not recommended
```

### gRPC Service Compatibility

| Service | v1.0 | v1.1 | v1.2 | v2.0 | Wire Compatible |
|---------|------|------|------|------|-----------------|
| ClusterService | ✅ | ✅ | ✅ | ⚠️ | v1.x ↔ v1.x only |
| FederationService | — | ✅ | ✅ | ✅ | v1.1+ ↔ v2.0+ |
| GovernanceService | — | — | ✅ | ✅ | v1.2 ↔ v2.0+ |
| TokenService | — | — | — | ✅ | v2.0+ only |

---

## Storage Compatibility

### Data Format Matrix

| Data Type | v1.0 Format | v1.1 Format | v1.2 Format | v2.0 Format |
|-----------|-------------|-------------|-------------|-------------|
| State snapshots | Protobuf v1 | Protobuf v1 | Protobuf v2 | Protobuf v2 |
| Audit logs | JSON | JSON | CBOR | CBOR |
| Attestations | — | — | JWT | JWT+ZKP |
| Token ledger | — | — | — | Merkle-CBOR |

### Migration Compatibility

```
Data Migration Paths:
━━━━━━━━━━━━━━━━━━━━━

  v1.0 → v1.1: Automatic (no migration needed)
  v1.1 → v1.2: Automatic (audit log format upgrade)
  v1.2 → v2.0: Migration tool required

  Migration Tool Support:
  ┌─────────────────────────────────────────────────────┐
  │ grey-migrate --from v1.2 --to v2.0 --data /var/grey │
  │                                                      │
  │ Steps:                                              │
  │   1. Validate source data integrity                 │
  │   2. Create backup snapshot                         │
  │   3. Transform state snapshots                      │
  │   4. Upgrade audit log format                       │
  │   5. Initialize token ledger                        │
  │   6. Verify migration                               │
  │                                                      │
  │ Duration: ~1 hour per TB of data                    │
  └─────────────────────────────────────────────────────┘
```

### Storage Backend Compatibility

| Backend | v1.0 | v1.1 | v1.2 | v2.0 | Notes |
|---------|------|------|------|------|-------|
| RocksDB | ✅ | ✅ | ✅ | ✅ | Primary backend |
| PostgreSQL | ✅ | ✅ | ✅ | ✅ | Enterprise backend |
| SQLite | ✅ | ✅ | ⚠️ | ❌ | Development only |
| TiKV | — | ✅ | ✅ | ✅ | Distributed backend |
| FoundationDB | — | — | ✅ | ✅ | High-scale backend |

---

## Federation Compatibility

### Cross-Version Federation

| Federation Initiator | Target v1.1 | Target v1.2 | Target v2.0 | Target v2.1 |
|---------------------|-------------|-------------|-------------|-------------|
| v1.1 | ✅ Full | ✅ Full | ⚠️ Read-only | ⚠️ Read-only |
| v1.2 | ✅ Full | ✅ Full | ✅ Full | ✅ Full |
| v2.0 | ⚠️ Read-only | ✅ Full | ✅ Full | ✅ Full |
| v2.1 | ⚠️ Read-only | ✅ Full | ✅ Full | ✅ Full |

### Federation Protocol Versions

```
Federation Protocol Evolution:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  FedProto v1 (v1.1.0 - v1.2.x)
  ├─ Discovery: Manual peering
  ├─ Authentication: mTLS
  ├─ Authorization: ACL-based
  └─ Data sync: Pull-based

  FedProto v2 (v2.0.0+)
  ├─ Discovery: Automatic (gossip + DNS)
  ├─ Authentication: mTLS + attestation
  ├─ Authorization: Policy-based (OPA)
  └─ Data sync: Push + pull (bidirectional)

  Cross-Protocol Compatibility:
  ├─ FedProto v1 ↔ v2: Via compatibility adapter
  ├─ Latency overhead: ~10ms per request
  └─ Feature parity: v1 features only
```

### Governance Compatibility

| Governance Feature | v1.2 | v2.0 | v2.1 | Cross-Version |
|--------------------|------|------|------|---------------|
| Proposal submission | ✅ | ✅ | ✅ | ✅ v1.2+ |
| Voting | ✅ | ✅ | ✅ | ✅ v1.2+ |
| Delegation | — | ✅ | ✅ | ❌ v2.0+ only |
| Token staking | — | ✅ | ✅ | ❌ v2.0+ only |
| Quadratic voting | — | — | ✅ | ❌ v2.1+ only |

---

## Upgrade Paths

### Supported Upgrade Paths

```
Direct Upgrade Paths:
━━━━━━━━━━━━━━━━━━━━━

  v1.0.x → v1.1.x  ✅ Rolling upgrade
  v1.1.x → v1.2.x  ✅ Rolling upgrade
  v1.2.x → v2.0.x  ✅ Rolling upgrade (with migration tool)
  v2.0.x → v2.1.x  ✅ Rolling upgrade

  Skip-Version Upgrades:

  v1.0.x → v1.2.x  ✅ Supported (via v1.1.x shim)
  v1.1.x → v2.0.x  ✅ Supported
  v1.0.x → v2.0.x  ⚠️ Requires intermediate upgrade to v1.2.x first

  LTS-to-LTS Upgrades:

  v2.0-LTS → v3.0-LTS  ✅ Direct upgrade supported
  v3.0-LTS → v4.0-LTS  ✅ Direct upgrade supported
```

### Upgrade Compatibility Checklist

| Check | Tool | Required For |
|-------|------|--------------|
| Version compatibility | `greyctl upgrade check` | All upgrades |
| Data format compatibility | `grey-migrate validate` | Major upgrades |
| API compatibility | `greyctl api diff` | Applications |
| Federation compatibility | `greyctl federation check` | Federated clusters |
| Plugin compatibility | `greyctl plugin check` | Plugins |

### Rollback Support

| Upgrade Path | Rollback Support | Data Preserved |
|--------------|------------------|----------------|
| v1.x → v1.x | ✅ Full rollback | ✅ Complete |
| v1.x → v2.0 | ⚠️ Limited (24h window) | ⚠️ Pre-migration snapshot |
| v2.x → v2.x | ✅ Full rollback | ✅ Complete |
| v2.x → v3.0 | ⚠️ Limited (24h window) | ⚠️ Pre-migration snapshot |

---

## Compatibility Testing

### CI/CD Matrix

```yaml
# .github/workflows/compatibility.yml
name: Compatibility Matrix

on: [push, pull_request]

jobs:
  compatibility:
    strategy:
      matrix:
        server_version: [v1.1, v1.2, v2.0, v2.1]
        client_version: [v1.1, v1.2, v2.0, v2.1]
        exclude:
          - server_version: v1.1
            client_version: v2.1
          - server_version: v2.1
            client_version: v1.1
    
    steps:
      - name: Test compatibility
        run: |
          ./scripts/test-compatibility.sh \
            --server ${{ matrix.server_version }} \
            --client ${{ matrix.client_version }}
```

### Compatibility Test Suite

| Test Category | Tests | Frequency |
|---------------|-------|-----------|
| API compatibility | 500+ | Every commit |
| Protocol compatibility | 200+ | Every commit |
| Storage compatibility | 100+ | Every commit |
| Federation compatibility | 150+ | Daily |
| Upgrade path testing | 50+ | Weekly |

---

## Related Documents

- [Version Roadmap](roadmap.md)
- [Deprecation Policy](deprecation_policy.md)
- [Upgrade Guide](/docs/upgrading.md)

---

*Last updated: February 2026*
