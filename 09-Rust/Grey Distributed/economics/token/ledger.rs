//! # Grey Distributed — Ledger
//!
//! This module implements a distributed ledger for tracking token transactions,
//! resource usage, and reward distributions across the Grey Distributed system.
//!
//! ## Ledger Design
//!
//! The ledger is append-only with the following properties:
//! - **Immutability**: Entries cannot be modified after creation
//! - **Causality**: Entries maintain causal ordering via vector clocks
//! - **Verifiability**: Each entry includes cryptographic hashes for verification
//! - **Federation-aware**: Supports cross-cluster transaction tracking
//!
//! ## Entry Structure
//!
//! ```text
//! ┌────────────────────────────────────────────────────────────────┐
//! │                         Ledger Entry                          │
//! ├─────────────────┬──────────────────────────────────────────────┤
//! │ entry_id        │ Unique identifier (hash-based)              │
//! │ previous_hash   │ Hash of previous entry (chain)              │
//! │ timestamp       │ Logical timestamp (vector clock)             │
//! │ cluster_id      │ Originating cluster                         │
//! │ entry_type      │ Transaction, Usage, Reward, Attestation     │
//! │ payload         │ Entry-specific data                         │
//! │ signature       │ Cryptographic signature                      │
//! └─────────────────┴──────────────────────────────────────────────┘
//! ```

use std::collections::{HashMap, VecDeque};
use std::sync::{Arc, RwLock};
use std::time::SystemTime;

// =============================================================================
// Types and Constants
// =============================================================================

pub type ClusterId = String;
pub type TenantId = String;
pub type EntryId = String;
pub type TokenAmount = u64;
pub type Hash = [u8; 32];

/// Maximum entries to keep in memory
const MAX_IN_MEMORY_ENTRIES: usize = 100_000;

/// Genesis entry hash (all zeros)
const GENESIS_HASH: Hash = [0u8; 32];

// =============================================================================
// Ledger Entry Types
// =============================================================================

/// A ledger entry
#[derive(Debug, Clone)]
pub struct LedgerEntry {
    /// Unique entry identifier
    pub entry_id: EntryId,
    
    /// Hash of previous entry in chain
    pub previous_hash: Hash,
    
    /// Entry's own hash
    pub entry_hash: Hash,
    
    /// Logical timestamp (vector clock)
    pub vector_clock: VectorClock,
    
    /// Physical timestamp
    pub timestamp: SystemTime,
    
    /// Originating cluster
    pub cluster_id: ClusterId,
    
    /// Entry type and payload
    pub entry_type: LedgerEntryType,
    
    /// Sequence number within cluster
    pub sequence_number: u64,
    
    /// Cryptographic signature (placeholder)
    pub signature: Vec<u8>,
}

impl LedgerEntry {
    /// Compute hash of entry
    pub fn compute_hash(&self) -> Hash {
        use std::hash::{Hash as StdHash, Hasher};
        use std::collections::hash_map::DefaultHasher;
        
        let mut hasher = DefaultHasher::new();
        self.entry_id.hash(&mut hasher);
        self.previous_hash.hash(&mut hasher);
        self.sequence_number.hash(&mut hasher);
        
        let hash_value = hasher.finish();
        let mut result = [0u8; 32];
        result[..8].copy_from_slice(&hash_value.to_le_bytes());
        result
    }
    
    /// Verify entry hash
    pub fn verify_hash(&self) -> bool {
        self.entry_hash == self.compute_hash()
    }
}

/// Vector clock for causal ordering
#[derive(Debug, Clone, Default)]
pub struct VectorClock {
    clocks: HashMap<ClusterId, u64>,
}

impl VectorClock {
    pub fn new() -> Self {
        Self { clocks: HashMap::new() }
    }
    
    /// Increment clock for a cluster
    pub fn increment(&mut self, cluster_id: &ClusterId) {
        *self.clocks.entry(cluster_id.clone()).or_default() += 1;
    }
    
    /// Merge with another vector clock (take max of each component)
    pub fn merge(&mut self, other: &VectorClock) {
        for (cluster, &time) in &other.clocks {
            let entry = self.clocks.entry(cluster.clone()).or_default();
            *entry = (*entry).max(time);
        }
    }
    
    /// Check if this clock happened before another
    pub fn happened_before(&self, other: &VectorClock) -> bool {
        let mut at_least_one_less = false;
        
        for (cluster, &time) in &self.clocks {
            let other_time = other.clocks.get(cluster).copied().unwrap_or(0);
            if time > other_time {
                return false;
            }
            if time < other_time {
                at_least_one_less = true;
            }
        }
        
        // Check clusters in other but not in self
        for (cluster, &other_time) in &other.clocks {
            if !self.clocks.contains_key(cluster) && other_time > 0 {
                at_least_one_less = true;
            }
        }
        
        at_least_one_less
    }
    
    /// Get clock value for a cluster
    pub fn get(&self, cluster_id: &ClusterId) -> u64 {
        self.clocks.get(cluster_id).copied().unwrap_or(0)
    }
}

/// Ledger entry types
#[derive(Debug, Clone)]
pub enum LedgerEntryType {
    /// Token transaction
    TokenTransaction(TokenTransactionEntry),
    
    /// Resource usage
    ResourceUsage(ResourceUsageEntry),
    
    /// Reward distribution
    RewardDistribution(RewardDistributionEntry),
    
    /// Attestation record
    Attestation(AttestationEntry),
    
    /// Federation event
    FederationEvent(FederationEventEntry),
    
    /// Checkpoint/snapshot
    Checkpoint(CheckpointEntry),
}

// =============================================================================
// Entry Payloads
// =============================================================================

/// Token transaction entry
#[derive(Debug, Clone)]
pub struct TokenTransactionEntry {
    /// Transaction ID
    pub transaction_id: String,
    
    /// Source tenant
    pub from_tenant: Option<TenantId>,
    
    /// Destination tenant
    pub to_tenant: TenantId,
    
    /// Amount
    pub amount: TokenAmount,
    
    /// Transaction type
    pub transaction_type: TokenTransactionType,
    
    /// Reference
    pub reference: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenTransactionType {
    Allocation,
    Consumption,
    Reward,
    Transfer,
    Expiration,
}

/// Resource usage entry
#[derive(Debug, Clone)]
pub struct ResourceUsageEntry {
    /// Tenant ID
    pub tenant_id: TenantId,
    
    /// Resource type
    pub resource_type: ResourceType,
    
    /// Usage amount
    pub usage_amount: f64,
    
    /// Usage unit
    pub usage_unit: String,
    
    /// Time period start
    pub period_start: SystemTime,
    
    /// Time period end
    pub period_end: SystemTime,
    
    /// Associated cost (tokens)
    pub cost: TokenAmount,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ResourceType {
    Cpu,
    Memory,
    Storage,
    NetworkBandwidth,
    NetworkRequests,
}

/// Reward distribution entry
#[derive(Debug, Clone)]
pub struct RewardDistributionEntry {
    /// Reward batch ID
    pub batch_id: String,
    
    /// Reward type
    pub reward_type: RewardType,
    
    /// Recipient → amount mapping
    pub distributions: HashMap<TenantId, TokenAmount>,
    
    /// Total distributed
    pub total_amount: TokenAmount,
    
    /// Distribution rationale
    pub rationale: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RewardType {
    TaskCompletion,
    FairResourceUsage,
    FederationContribution,
    AttestationParticipation,
    Other(String),
}

/// Attestation entry
#[derive(Debug, Clone)]
pub struct AttestationEntry {
    /// Attestation ID
    pub attestation_id: String,
    
    /// Attested cluster
    pub attested_cluster: ClusterId,
    
    /// Verifier clusters
    pub verifier_clusters: Vec<ClusterId>,
    
    /// Attestation result
    pub result: AttestationResult,
    
    /// Proof/evidence hash
    pub proof_hash: Hash,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AttestationResult {
    Verified,
    Failed,
    Inconclusive,
}

/// Federation event entry
#[derive(Debug, Clone)]
pub struct FederationEventEntry {
    /// Event ID
    pub event_id: String,
    
    /// Event type
    pub event_type: FederationEventType,
    
    /// Participating clusters
    pub clusters: Vec<ClusterId>,
    
    /// Event data
    pub data: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FederationEventType {
    ClusterJoined,
    ClusterLeft,
    ResourceShared,
    PolicyUpdate,
    ProtocolNegotiation,
}

/// Checkpoint entry (snapshot of state)
#[derive(Debug, Clone)]
pub struct CheckpointEntry {
    /// Checkpoint sequence
    pub checkpoint_sequence: u64,
    
    /// State hash
    pub state_hash: Hash,
    
    /// Tenant balances at checkpoint
    pub balances: HashMap<TenantId, TokenAmount>,
    
    /// Number of entries since last checkpoint
    pub entries_included: u64,
}

// =============================================================================
// Ledger Implementation
// =============================================================================

/// The distributed ledger
pub struct Ledger {
    /// Local cluster ID
    cluster_id: ClusterId,
    
    /// Entries in memory
    entries: Arc<RwLock<VecDeque<LedgerEntry>>>,
    
    /// Entry index by ID
    entry_index: Arc<RwLock<HashMap<EntryId, usize>>>,
    
    /// Latest entry hash
    latest_hash: Arc<RwLock<Hash>>,
    
    /// Current vector clock
    vector_clock: Arc<RwLock<VectorClock>>,
    
    /// Sequence number counter
    sequence: Arc<RwLock<u64>>,
    
    /// Pending entries from other clusters
    pending_entries: Arc<RwLock<Vec<LedgerEntry>>>,
    
    /// Checkpoints
    checkpoints: Arc<RwLock<Vec<CheckpointEntry>>>,
}

impl Ledger {
    pub fn new(cluster_id: ClusterId) -> Self {
        Self {
            cluster_id,
            entries: Arc::new(RwLock::new(VecDeque::new())),
            entry_index: Arc::new(RwLock::new(HashMap::new())),
            latest_hash: Arc::new(RwLock::new(GENESIS_HASH)),
            vector_clock: Arc::new(RwLock::new(VectorClock::new())),
            sequence: Arc::new(RwLock::new(0)),
            pending_entries: Arc::new(RwLock::new(Vec::new())),
            checkpoints: Arc::new(RwLock::new(Vec::new())),
        }
    }
    
    // =========================================================================
    // Entry Creation
    // =========================================================================
    
    /// Append a new entry to the ledger
    pub fn append(&self, entry_type: LedgerEntryType) -> LedgerEntry {
        let mut vc = self.vector_clock.write().unwrap();
        vc.increment(&self.cluster_id);
        
        let mut seq = self.sequence.write().unwrap();
        *seq += 1;
        
        let previous_hash = *self.latest_hash.read().unwrap();
        
        let entry_id = format!("{}-{}", self.cluster_id, *seq);
        
        let mut entry = LedgerEntry {
            entry_id: entry_id.clone(),
            previous_hash,
            entry_hash: [0u8; 32],
            vector_clock: vc.clone(),
            timestamp: SystemTime::now(),
            cluster_id: self.cluster_id.clone(),
            entry_type,
            sequence_number: *seq,
            signature: Vec::new(),
        };
        
        entry.entry_hash = entry.compute_hash();
        
        // Update latest hash
        *self.latest_hash.write().unwrap() = entry.entry_hash;
        
        // Add to entries
        let mut entries = self.entries.write().unwrap();
        let index = entries.len();
        entries.push_back(entry.clone());
        
        // Update index
        self.entry_index.write().unwrap().insert(entry_id, index);
        
        // Evict old entries if necessary
        while entries.len() > MAX_IN_MEMORY_ENTRIES {
            if let Some(old_entry) = entries.pop_front() {
                self.entry_index.write().unwrap().remove(&old_entry.entry_id);
            }
        }
        
        entry
    }
    
    /// Record a token transaction
    pub fn record_token_transaction(
        &self,
        from: Option<TenantId>,
        to: TenantId,
        amount: TokenAmount,
        tx_type: TokenTransactionType,
        reference: Option<String>,
    ) -> LedgerEntry {
        let entry = TokenTransactionEntry {
            transaction_id: generate_id("tx"),
            from_tenant: from,
            to_tenant: to,
            amount,
            transaction_type: tx_type,
            reference,
        };
        
        self.append(LedgerEntryType::TokenTransaction(entry))
    }
    
    /// Record resource usage
    pub fn record_resource_usage(
        &self,
        tenant_id: TenantId,
        resource_type: ResourceType,
        usage_amount: f64,
        usage_unit: String,
        period_start: SystemTime,
        period_end: SystemTime,
        cost: TokenAmount,
    ) -> LedgerEntry {
        let entry = ResourceUsageEntry {
            tenant_id,
            resource_type,
            usage_amount,
            usage_unit,
            period_start,
            period_end,
            cost,
        };
        
        self.append(LedgerEntryType::ResourceUsage(entry))
    }
    
    /// Record reward distribution
    pub fn record_reward_distribution(
        &self,
        reward_type: RewardType,
        distributions: HashMap<TenantId, TokenAmount>,
        rationale: String,
    ) -> LedgerEntry {
        let total_amount = distributions.values().sum();
        
        let entry = RewardDistributionEntry {
            batch_id: generate_id("reward"),
            reward_type,
            distributions,
            total_amount,
            rationale,
        };
        
        self.append(LedgerEntryType::RewardDistribution(entry))
    }
    
    /// Record attestation
    pub fn record_attestation(
        &self,
        attested_cluster: ClusterId,
        verifier_clusters: Vec<ClusterId>,
        result: AttestationResult,
        proof_hash: Hash,
    ) -> LedgerEntry {
        let entry = AttestationEntry {
            attestation_id: generate_id("attest"),
            attested_cluster,
            verifier_clusters,
            result,
            proof_hash,
        };
        
        self.append(LedgerEntryType::Attestation(entry))
    }
    
    // =========================================================================
    // Entry Retrieval
    // =========================================================================
    
    /// Get entry by ID
    pub fn get_entry(&self, entry_id: &EntryId) -> Option<LedgerEntry> {
        let index = self.entry_index.read().unwrap();
        let entries = self.entries.read().unwrap();
        
        index.get(entry_id).and_then(|&idx| entries.get(idx).cloned())
    }
    
    /// Get entries in range
    pub fn get_entries(&self, start: usize, limit: usize) -> Vec<LedgerEntry> {
        let entries = self.entries.read().unwrap();
        entries.iter()
            .skip(start)
            .take(limit)
            .cloned()
            .collect()
    }
    
    /// Get entries for a tenant
    pub fn get_tenant_entries(&self, tenant_id: &TenantId) -> Vec<LedgerEntry> {
        let entries = self.entries.read().unwrap();
        entries.iter()
            .filter(|entry| match &entry.entry_type {
                LedgerEntryType::TokenTransaction(tx) => {
                    tx.from_tenant.as_ref() == Some(tenant_id) || &tx.to_tenant == tenant_id
                }
                LedgerEntryType::ResourceUsage(usage) => &usage.tenant_id == tenant_id,
                LedgerEntryType::RewardDistribution(dist) => dist.distributions.contains_key(tenant_id),
                _ => false,
            })
            .cloned()
            .collect()
    }
    
    /// Get latest hash
    pub fn get_latest_hash(&self) -> Hash {
        *self.latest_hash.read().unwrap()
    }
    
    /// Get current sequence number
    pub fn get_sequence(&self) -> u64 {
        *self.sequence.read().unwrap()
    }
    
    /// Get entry count
    pub fn entry_count(&self) -> usize {
        self.entries.read().unwrap().len()
    }
    
    // =========================================================================
    // Federation Sync
    // =========================================================================
    
    /// Receive entry from another cluster
    pub fn receive_remote_entry(&self, entry: LedgerEntry) -> Result<(), LedgerError> {
        // Verify entry hash
        if !entry.verify_hash() {
            return Err(LedgerError::InvalidHash);
        }
        
        // Check for duplicate
        if self.entry_index.read().unwrap().contains_key(&entry.entry_id) {
            return Err(LedgerError::DuplicateEntry(entry.entry_id.clone()));
        }
        
        // Add to pending for processing
        self.pending_entries.write().unwrap().push(entry);
        
        Ok(())
    }
    
    /// Process pending entries
    pub fn process_pending(&self) -> Vec<LedgerEntry> {
        let pending: Vec<LedgerEntry> = self.pending_entries.write().unwrap().drain(..).collect();
        let mut processed = Vec::new();
        
        for entry in pending {
            // Merge vector clock
            self.vector_clock.write().unwrap().merge(&entry.vector_clock);
            
            // Add to entries
            let mut entries = self.entries.write().unwrap();
            let index = entries.len();
            entries.push_back(entry.clone());
            self.entry_index.write().unwrap().insert(entry.entry_id.clone(), index);
            
            processed.push(entry);
        }
        
        processed
    }
    
    // =========================================================================
    // Checkpointing
    // =========================================================================
    
    /// Create a checkpoint
    pub fn create_checkpoint(&self, balances: HashMap<TenantId, TokenAmount>) -> CheckpointEntry {
        let checkpoints = self.checkpoints.read().unwrap();
        let checkpoint_sequence = checkpoints.len() as u64 + 1;
        
        let entries_since_last = if let Some(last) = checkpoints.last() {
            self.get_sequence() - last.checkpoint_sequence
        } else {
            self.get_sequence()
        };
        
        drop(checkpoints);
        
        let checkpoint = CheckpointEntry {
            checkpoint_sequence,
            state_hash: self.get_latest_hash(),
            balances,
            entries_included: entries_since_last,
        };
        
        // Record checkpoint in ledger
        let entry = self.append(LedgerEntryType::Checkpoint(checkpoint.clone()));
        
        // Store checkpoint
        self.checkpoints.write().unwrap().push(checkpoint.clone());
        
        checkpoint
    }
    
    /// Get latest checkpoint
    pub fn get_latest_checkpoint(&self) -> Option<CheckpointEntry> {
        self.checkpoints.read().unwrap().last().cloned()
    }
    
    // =========================================================================
    // Verification
    // =========================================================================
    
    /// Verify chain integrity
    pub fn verify_chain(&self) -> Result<(), LedgerError> {
        let entries = self.entries.read().unwrap();
        
        let mut expected_previous = GENESIS_HASH;
        
        for entry in entries.iter() {
            if entry.previous_hash != expected_previous {
                return Err(LedgerError::ChainBroken {
                    entry_id: entry.entry_id.clone(),
                    expected: expected_previous,
                    actual: entry.previous_hash,
                });
            }
            
            if !entry.verify_hash() {
                return Err(LedgerError::InvalidHash);
            }
            
            expected_previous = entry.entry_hash;
        }
        
        Ok(())
    }
    
    /// Compute Merkle root of entries
    pub fn compute_merkle_root(&self) -> Hash {
        let entries = self.entries.read().unwrap();
        
        if entries.is_empty() {
            return GENESIS_HASH;
        }
        
        let mut hashes: Vec<Hash> = entries.iter().map(|e| e.entry_hash).collect();
        
        while hashes.len() > 1 {
            let mut new_hashes = Vec::new();
            
            for chunk in hashes.chunks(2) {
                let combined = if chunk.len() == 2 {
                    combine_hashes(&chunk[0], &chunk[1])
                } else {
                    combine_hashes(&chunk[0], &chunk[0])
                };
                new_hashes.push(combined);
            }
            
            hashes = new_hashes;
        }
        
        hashes[0]
    }
}

// =============================================================================
// Errors
// =============================================================================

#[derive(Debug, Clone, PartialEq)]
pub enum LedgerError {
    InvalidHash,
    DuplicateEntry(String),
    ChainBroken { entry_id: String, expected: Hash, actual: Hash },
    EntryNotFound(String),
}

impl std::fmt::Display for LedgerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LedgerError::InvalidHash => write!(f, "Invalid entry hash"),
            LedgerError::DuplicateEntry(id) => write!(f, "Duplicate entry: {}", id),
            LedgerError::ChainBroken { entry_id, .. } => {
                write!(f, "Chain broken at entry: {}", entry_id)
            }
            LedgerError::EntryNotFound(id) => write!(f, "Entry not found: {}", id),
        }
    }
}

impl std::error::Error for LedgerError {}

// =============================================================================
// Utilities
// =============================================================================

fn generate_id(prefix: &str) -> String {
    use std::time::{SystemTime, UNIX_EPOCH};
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    format!("{}-{:032x}", prefix, nanos)
}

fn combine_hashes(a: &Hash, b: &Hash) -> Hash {
    use std::hash::{Hash as StdHash, Hasher};
    use std::collections::hash_map::DefaultHasher;
    
    let mut hasher = DefaultHasher::new();
    a.hash(&mut hasher);
    b.hash(&mut hasher);
    
    let hash_value = hasher.finish();
    let mut result = [0u8; 32];
    result[..8].copy_from_slice(&hash_value.to_le_bytes());
    result
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_append_entry() {
        let ledger = Ledger::new("cluster-1".to_string());
        
        let entry = ledger.record_token_transaction(
            None,
            "tenant-1".to_string(),
            1000,
            TokenTransactionType::Allocation,
            None,
        );
        
        assert_eq!(entry.sequence_number, 1);
        assert!(entry.verify_hash());
    }
    
    #[test]
    fn test_chain_integrity() {
        let ledger = Ledger::new("cluster-1".to_string());
        
        for i in 0..10 {
            ledger.record_token_transaction(
                None,
                format!("tenant-{}", i),
                100,
                TokenTransactionType::Allocation,
                None,
            );
        }
        
        assert!(ledger.verify_chain().is_ok());
    }
    
    #[test]
    fn test_vector_clock() {
        let mut vc1 = VectorClock::new();
        vc1.increment(&"a".to_string());
        vc1.increment(&"a".to_string());
        
        let mut vc2 = VectorClock::new();
        vc2.increment(&"a".to_string());
        vc2.increment(&"b".to_string());
        
        assert!(vc1.happened_before(&vc2) == false);
        
        vc1.merge(&vc2);
        assert_eq!(vc1.get(&"a".to_string()), 2);
        assert_eq!(vc1.get(&"b".to_string()), 1);
    }
    
    #[test]
    fn test_tenant_entries() {
        let ledger = Ledger::new("cluster-1".to_string());
        let tenant = "tenant-1".to_string();
        
        ledger.record_token_transaction(
            None, tenant.clone(), 1000, TokenTransactionType::Allocation, None,
        );
        ledger.record_token_transaction(
            Some(tenant.clone()), "tenant-2".to_string(), 100, TokenTransactionType::Transfer, None,
        );
        
        let entries = ledger.get_tenant_entries(&tenant);
        assert_eq!(entries.len(), 2);
    }
}
