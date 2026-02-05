# Grey Distributed — Tokenization

This document describes the Grey Token (GRT) system, distributed ledger design, and attestation proof mechanisms that power Grey Distributed's economic layer.

## Table of Contents

1. [Token Overview](#token-overview)
2. [Token Design](#token-design)
3. [Ledger Architecture](#ledger-architecture)
4. [Attestation System](#attestation-system)
5. [Federation Token Protocol](#federation-token-protocol)
6. [Security Considerations](#security-considerations)
7. [Operations Guide](#operations-guide)

---

## Token Overview

### What Grey Tokens Are

Grey Tokens (GRT) are **internal accounting units** used to:

- Track resource consumption across tenants
- Distribute rewards for good behavior
- Enable cross-cluster resource accounting
- Provide a consistent pricing unit

### What Grey Tokens Are NOT

- **Not cryptocurrency**: No external trading, no blockchain
- **Not investment**: No financial speculation
- **Not transferable outside system**: Cannot be withdrawn

### Token Properties

| Property | Value | Rationale |
|----------|-------|-----------|
| Symbol | GRT | Grey Token |
| Divisibility | 10⁶ micro-tokens | Fine-grained accounting |
| Expiration | 365 days | Prevent hoarding |
| Transferability | Optional (per-deployment) | Security vs. flexibility |
| Backing | None (internal unit) | Simplicity |

---

## Token Design

### Token Lifecycle

```
                    ┌─────────────────────────────────────────┐
                    │              TOKEN LIFECYCLE            │
                    └─────────────────────────────────────────┘

    CREATION                CIRCULATION               RETIREMENT
    ────────               ────────────               ──────────
    
    ┌──────────┐          ┌──────────────┐          ┌───────────┐
    │  Minting │─────────►│   Active     │─────────►│ Consumed  │
    │ (Alloc)  │          │  (Balance)   │          │ (Spent)   │
    └──────────┘          └──────────────┘          └───────────┘
         │                      │    │                    │
         │                      │    │                    │
         │    ┌─────────────────┘    └──────────┐        │
         │    │                                  │        │
         │    ▼                                  ▼        │
         │  ┌──────────┐                  ┌──────────┐   │
         │  │ Reserved │                  │ Expired  │   │
         │  │  (Held)  │                  │          │   │
         │  └──────────┘                  └──────────┘   │
         │        │                                      │
         │        │ release                              │
         │        ▼                                      │
         │  ┌──────────┐                                │
         └──│ Released │─────────────────────────────────┘
              └──────────┘        (back to active)
```

### Balance Structure

```rust
struct TokenBalance {
    available: u64,   // Ready to spend
    reserved: u64,    // Committed but not spent
    consumed: u64,    // Already spent (cumulative)
    expired: u64,     // Lost to expiration (cumulative)
    earned: u64,      // Received as rewards (cumulative)
}
```

### Allocation Model

#### Allocation Sources

| Source | Description | Expiration |
|--------|-------------|------------|
| Initial | Subscription credit | 365 days |
| TopUp | Additional purchase | 365 days |
| Reward | Earned through behavior | 90 days |
| Transfer | From another tenant | Inherits source |
| FederationGrant | Cross-cluster credit | 30 days |
| Promotional | Free trial/promo | 30 days |

#### Allocation Example

```
Tenant A receives:
  2024-01-01: 10,000 GRT (Initial, expires 2025-01-01)
  2024-03-15: 1,000 GRT (Reward, expires 2024-06-13)
  2024-04-01: 5,000 GRT (TopUp, expires 2025-04-01)

Total available: 16,000 GRT
First to expire: 1,000 GRT on 2024-06-13 (Reward)
```

### Reservation System

Reservations provide **atomic holds** for multi-step operations:

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│   Request   │────►│  Reserve    │────►│  Consume    │
│  (Intent)   │     │   (Hold)    │     │  (Finalize) │
└─────────────┘     └─────────────┘     └─────────────┘
                          │
                          │ timeout/cancel
                          ▼
                    ┌─────────────┐
                    │   Release   │
                    │ (Return)    │
                    └─────────────┘
```

#### Reservation Properties

| Property | Value | Purpose |
|----------|-------|---------|
| Max duration | 1 hour | Prevent indefinite holds |
| Auto-release | On timeout | Recover stuck tokens |
| Atomic | Yes | All-or-nothing |
| Auditable | Yes | Full trace in ledger |

#### Reservation Example

```
1. Task scheduled, estimated cost: 500 GRT
2. Reserve 500 GRT (available: 10000 → 9500, reserved: 0 → 500)
3. Task executes, actual cost: 450 GRT
4. Consume 450 GRT (reserved: 500 → 50, consumed: +450)
5. Release 50 GRT (reserved: 50 → 0, available: 9500 → 9550)
```

---

## Ledger Architecture

### Design Goals

1. **Append-only**: Entries cannot be modified
2. **Causally ordered**: Vector clocks for distributed ordering
3. **Verifiable**: Cryptographic hashes for integrity
4. **Federation-aware**: Supports cross-cluster entries

### Entry Structure

```
┌────────────────────────────────────────────────────────────────┐
│                         Ledger Entry                          │
├─────────────────┬──────────────────────────────────────────────┤
│ entry_id        │ Unique identifier (cluster-sequence)        │
│ previous_hash   │ Hash of previous entry (chain)              │
│ entry_hash      │ SHA-256 of this entry                       │
│ vector_clock    │ Logical timestamp {cluster: counter}        │
│ timestamp       │ Physical timestamp (wall clock)              │
│ cluster_id      │ Originating cluster                         │
│ sequence_number │ Local sequence number                        │
│ entry_type      │ Type-specific payload (see below)           │
│ signature       │ Cluster's cryptographic signature            │
└─────────────────┴──────────────────────────────────────────────┘
```

### Entry Types

#### TokenTransaction

```rust
struct TokenTransactionEntry {
    transaction_id: String,
    from_tenant: Option<TenantId>,  // None for minting
    to_tenant: TenantId,
    amount: u64,
    transaction_type: TransactionType,  // Allocation, Consumption, etc.
    reference: Option<String>,  // Task ID, invoice, etc.
}
```

#### ResourceUsage

```rust
struct ResourceUsageEntry {
    tenant_id: TenantId,
    resource_type: ResourceType,
    usage_amount: f64,
    usage_unit: String,
    period_start: Timestamp,
    period_end: Timestamp,
    cost: u64,  // Tokens charged
}
```

#### RewardDistribution

```rust
struct RewardDistributionEntry {
    batch_id: String,
    reward_type: RewardType,
    distributions: Map<TenantId, Amount>,
    total_amount: u64,
    rationale: String,
}
```

#### Attestation

```rust
struct AttestationEntry {
    attestation_id: String,
    attested_cluster: ClusterId,
    verifier_clusters: Vec<ClusterId>,
    result: AttestationResult,
    proof_hash: Hash,
}
```

### Chain Integrity

```
Entry 0 (Genesis)          Entry 1                    Entry 2
┌──────────────────┐      ┌──────────────────┐      ┌──────────────────┐
│ prev: 0x000...   │      │ prev: hash(E0)   │      │ prev: hash(E1)   │
│ hash: 0xabc...   │─────►│ hash: 0xdef...   │─────►│ hash: 0x123...   │
│ seq: 0           │      │ seq: 1           │      │ seq: 2           │
└──────────────────┘      └──────────────────┘      └──────────────────┘
```

Verification:
```
for each entry E[i] where i > 0:
    assert E[i].previous_hash == hash(E[i-1])
    assert E[i].entry_hash == compute_hash(E[i])
```

### Vector Clocks

Vector clocks enable causal ordering across federated clusters:

```
Cluster A: {A: 5, B: 3}
Cluster B: {A: 4, B: 7}

Event ordering:
  A's event with {A:3, B:2} happened-before B's event with {A:4, B:5}
  B's event with {A:4, B:6} is concurrent with A's event with {A:5, B:3}
```

Merge on sync:
```
merged_clock[c] = max(local_clock[c], remote_clock[c]) for all c
```

### Checkpointing

Periodic checkpoints enable:
- Faster state recovery
- Ledger compaction
- Audit snapshots

```rust
struct CheckpointEntry {
    checkpoint_sequence: u64,
    state_hash: Hash,            // Merkle root of balances
    balances: Map<TenantId, u64>,
    entries_included: u64,
}

Checkpoint interval: every 10,000 entries or 1 hour
```

### Merkle Tree

Balance Merkle tree for efficient verification:

```
                    ┌───────────────┐
                    │  Root Hash    │
                    │   0xabcd...   │
                    └───────┬───────┘
                ┌───────────┴───────────┐
          ┌─────┴─────┐           ┌─────┴─────┐
          │ Hash(L,R) │           │ Hash(L,R) │
          └─────┬─────┘           └─────┬─────┘
        ┌───────┴───────┐       ┌───────┴───────┐
   ┌────┴────┐     ┌────┴────┐ ┌────┴────┐  ┌────┴────┐
   │Tenant A │     │Tenant B │ │Tenant C │  │Tenant D │
   │10,000   │     │5,000    │ │8,000    │  │3,000    │
   └─────────┘     └─────────┘ └─────────┘  └─────────┘
```

---

## Attestation System

### Attestation Purpose

Attestations provide cryptographic proof of claims:

- **Resource usage**: "Tenant A used 100 CPU-hours"
- **Token balance**: "Tenant B has 5,000 GRT"
- **State integrity**: "Cluster C's state hash is 0xabc..."
- **Federation sharing**: "Cluster D shared 50GB with Cluster E"

### Claim Types

| Type | Claimant | Verifiers | Evidence |
|------|----------|-----------|----------|
| ResourceUsage | Tenant | System + Federation | Metrics logs |
| TokenBalance | Tenant | Ledger + Witnesses | Ledger entries |
| StateIntegrity | Cluster | Federation peers | State snapshot |
| FederationSharing | Provider | Consumer + Peers | Transaction logs |
| RewardEligibility | Tenant | System | Task records |
| PenaltyAppeal | Tenant | Governance | Supporting docs |

### Proof Structure

```
┌──────────────────────────────────────────────────────────────────┐
│                     Attestation Proof                            │
├──────────────────┬───────────────────────────────────────────────┤
│ attestation_id   │ Unique proof identifier                       │
│ claim            │ The claim being attested                      │
│ evidence         │ Supporting data with hash                     │
│ merkle_proof     │ Proof of inclusion in state tree              │
│ witness_sigs     │ Multi-party signatures (quorum required)      │
│ result           │ Verified / Rejected / Pending                 │
│ valid_until      │ Expiration timestamp                          │
└──────────────────┴───────────────────────────────────────────────┘
```

### Verification Process

```
┌─────────┐     ┌─────────┐     ┌─────────┐     ┌─────────┐
│ Submit  │────►│ Collect │────►│ Verify  │────►│ Finalize│
│ Claim   │     │Evidence │     │Witnesses│     │ Result  │
└─────────┘     └─────────┘     └─────────┘     └─────────┘
     │               │               │               │
     │               │               │               │
     ▼               ▼               ▼               ▼
  Claim ID      Evidence         Signatures      Attestation
  generated     collected        gathered        recorded
```

### Quorum Requirements

```
required_witnesses = max(
    MIN_WITNESSES,                           // 3 minimum
    total_witnesses × QUORUM_PERCENTAGE      // 67%
)

Attestation passes when:
  approvals > rejections AND
  approvals + rejections >= required_witnesses
```

### Witness Selection

```python
def select_witnesses(claim, count):
    eligible = [w for w in witnesses 
                if w.active 
                and w.cluster != claim.claimant_cluster
                and w.reputation >= 0.5]
    
    # Weighted random selection by reputation
    weights = [w.reputation for w in eligible]
    return weighted_sample(eligible, weights, count)
```

### Evidence Types

| Type | Description | Source |
|------|-------------|--------|
| Metrics | Time-series resource data | Monitoring system |
| Logs | Audit trail entries | System logs |
| LedgerEntries | Transaction records | Distributed ledger |
| StateSnapshot | Point-in-time state | Storage system |
| ExternalVerification | Third-party confirmation | Oracle |

---

## Federation Token Protocol

### Cross-Cluster Transactions

#### Protocol Flow

```
Cluster A                    Cluster B
─────────                    ─────────
    │                            │
    │  1. Initiate Transfer      │
    │──────────────────────────► │
    │                            │
    │  2. Reserve Tokens         │
    │◄─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ │
    │                            │
    │  3. Attestation Request    │
    │──────────────────────────► │
    │                            │
    │  4. Witness Signatures     │
    │◄────────────────────────── │
    │                            │
    │  5. Commit Transaction     │
    │──────────────────────────► │
    │                            │
    │  6. Acknowledgment         │
    │◄────────────────────────── │
```

#### Message Format

```rust
struct FederationTransfer {
    transfer_id: String,
    source_cluster: ClusterId,
    target_cluster: ClusterId,
    source_tenant: TenantId,
    target_tenant: TenantId,
    amount: u64,
    purpose: TransferPurpose,
    created_at: Timestamp,
    expires_at: Timestamp,
    attestation_id: Option<AttestationId>,
}
```

### Ledger Synchronization

#### Sync Protocol

```
1. Exchange latest vector clocks
2. Identify missing entries (clock comparison)
3. Request missing entries
4. Validate received entries
5. Merge into local ledger
6. Update vector clock
```

#### Conflict Resolution

| Conflict Type | Resolution |
|---------------|------------|
| Duplicate entry | Keep first seen, reject duplicate |
| Invalid hash | Reject entry, request re-sync |
| Clock drift | Use physical time as tiebreaker |
| Network partition | Merge on reconnection |

### Federation Grants

```
grant_amount = min(
    requested_amount,
    source_cluster_budget,
    federation_daily_limit
)

grant_expiration = min(
    30 days,
    source_allocation_expiration
)
```

---

## Security Considerations

### Threat Model

| Threat | Mitigation |
|--------|------------|
| Double-spending | Reservation system + ledger verification |
| Balance forgery | Cryptographic signatures + Merkle proofs |
| Attestation fraud | Multi-witness quorum requirement |
| Sybil attack | Account restrictions + reputation system |
| Ledger tampering | Hash chains + distributed copies |

### Cryptographic Primitives

| Purpose | Algorithm | Key Size |
|---------|-----------|----------|
| Entry hashing | SHA-256 | 256 bits |
| Signatures | Ed25519 | 256 bits |
| Merkle tree | SHA-256 | 256 bits |
| Secure RNG | ChaCha20 | 256 bits |

### Key Management

```
Cluster keys:
  - Master key: HSM-protected, offline
  - Signing key: Rotated monthly
  - Session keys: Per-connection, ephemeral

Key rotation:
  1. Generate new key pair
  2. Publish new public key
  3. Wait for propagation (24 hours)
  4. Begin signing with new key
  5. Accept both keys during transition (7 days)
  6. Revoke old key
```

### Audit Trail

All token operations logged with:
- Actor identity
- Operation type
- Amount
- Timestamp
- Correlation ID
- Result (success/failure)

---

## Operations Guide

### Monitoring Metrics

#### Token Metrics

| Metric | Description | Alert Threshold |
|--------|-------------|-----------------|
| `grey_token_total_supply` | Total minted tokens | N/A |
| `grey_token_active_balance` | Available tokens | < 10% of supply |
| `grey_token_consumed_rate` | Consumption velocity | > 1000/sec |
| `grey_token_reservation_active` | Active reservations | > 10,000 |

#### Ledger Metrics

| Metric | Description | Alert Threshold |
|--------|-------------|-----------------|
| `grey_ledger_entry_count` | Total entries | N/A |
| `grey_ledger_sync_lag_seconds` | Federation sync delay | > 60s |
| `grey_ledger_verification_failures` | Chain integrity issues | > 0 |

### Common Operations

#### Token Allocation

```bash
# Allocate tokens to tenant
greyctl token allocate \
  --tenant tenant-123 \
  --amount 10000 \
  --source initial \
  --expiration-days 365
```

#### Balance Check

```bash
# Check tenant balance
greyctl token balance --tenant tenant-123

# Output:
# Tenant: tenant-123
# Available: 8,500 GRT
# Reserved: 500 GRT
# Consumed: 1,000 GRT (total)
# Expired: 0 GRT
```

#### Ledger Verification

```bash
# Verify ledger integrity
greyctl ledger verify --full

# Output:
# Entries verified: 1,234,567
# Chain integrity: OK
# Merkle root: 0xabcd1234...
# Last checkpoint: 2024-01-15 10:30:00
```

### Troubleshooting

#### Token Reservation Stuck

```bash
# List stuck reservations
greyctl token reservations list --status stuck --max-age 1h

# Force release (requires admin)
greyctl token reservation release --id rsv-12345 --force --reason "timeout"
```

#### Ledger Sync Issues

```bash
# Check sync status
greyctl ledger sync status

# Force re-sync
greyctl ledger sync --cluster cluster-b --from-checkpoint latest
```

#### Attestation Failures

```bash
# List failed attestations
greyctl attestation list --status failed --last 24h

# Retry attestation
greyctl attestation retry --id attest-12345
```

---

## Appendix: Configuration Reference

### Token Configuration

```yaml
token:
  default_expiration_days: 365
  max_reservation_duration: 3600  # seconds
  allow_transfers: true
  min_transfer_amount: 1
  allow_overdraft: false
  max_overdraft: 0
```

### Ledger Configuration

```yaml
ledger:
  max_in_memory_entries: 100000
  checkpoint_interval_entries: 10000
  checkpoint_interval_seconds: 3600
  sync_interval_seconds: 60
  max_sync_batch_size: 1000
```

### Attestation Configuration

```yaml
attestation:
  min_witnesses: 3
  quorum_percentage: 0.67
  validity_period_seconds: 86400
  challenge_window_seconds: 3600
  cross_cluster_enabled: true
```

---

*Last updated: 2024*
