//! # Data Replication
//!
//! Replication strategies for fault tolerance and read scalability.
//!
//! ## Replication Models
//!
//! 1. **Synchronous**: All replicas updated before acknowledging
//!    - Strong consistency
//!    - Higher latency
//!    - Availability suffers during partitions
//!
//! 2. **Asynchronous**: Primary acknowledges, replicas lag
//!    - Better latency
//!    - Eventual consistency
//!    - Risk of data loss on failure
//!
//! 3. **Semi-synchronous**: Wait for K of N replicas
//!    - Tunable consistency/latency tradeoff
//!    - Quorum-based durability
//!
//! ## Replica Placement
//!
//! Strategic placement for:
//! - Failure domain isolation (racks, zones, regions)
//! - Read latency (geo-distribution)
//! - Cost optimization (different storage tiers)

use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use parking_lot::{Mutex, RwLock};
use tokio::sync::mpsc;

use super::sharding::ShardId;
use super::StorageError;
use crate::types::NodeId;

// ============================================================================
// Core Types
// ============================================================================

/// Replica identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ReplicaId {
    pub shard_id: ShardId,
    pub node_id: NodeId,
}

/// Replication factor configuration
#[derive(Debug, Clone)]
pub struct ReplicationConfig {
    /// Total number of replicas
    pub replication_factor: u32,
    
    /// Minimum replicas for write success
    pub min_write_replicas: u32,
    
    /// Mode of replication
    pub mode: ReplicationMode,
    
    /// Placement constraints
    pub placement: PlacementPolicy,
    
    /// Maximum replication lag before alarm
    pub max_lag: Duration,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ReplicationMode {
    /// Wait for all replicas
    Synchronous,
    /// Don't wait for any replica
    Asynchronous,
    /// Wait for min_write_replicas
    SemiSynchronous,
}

#[derive(Debug, Clone)]
pub enum PlacementPolicy {
    /// Any available nodes
    Any,
    /// Spread across failure domains
    FailureDomainSpread { domains: Vec<String> },
    /// Specific rack/zone requirements
    RackAware { min_racks: u32 },
    /// Geographic requirements
    GeoSpread { min_regions: u32 },
}

impl Default for ReplicationConfig {
    fn default() -> Self {
        Self {
            replication_factor: 3,
            min_write_replicas: 2,
            mode: ReplicationMode::SemiSynchronous,
            placement: PlacementPolicy::Any,
            max_lag: Duration::from_secs(30),
        }
    }
}

// ============================================================================
// Replica State
// ============================================================================

/// State of a single replica
#[derive(Debug, Clone)]
pub struct ReplicaState {
    /// Replica identity
    pub id: ReplicaId,
    
    /// Role
    pub role: ReplicaRole,
    
    /// Status
    pub status: ReplicaStatus,
    
    /// Last sequence number applied
    pub last_applied: u64,
    
    /// Last sequence number committed
    pub last_committed: u64,
    
    /// Last heartbeat time
    pub last_heartbeat: Instant,
    
    /// Replication lag
    pub lag: Duration,
    
    /// Failure domain (rack, zone, etc.)
    pub failure_domain: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ReplicaRole {
    /// Primary (handles writes)
    Primary,
    /// Synchronous replica
    SyncReplica,
    /// Asynchronous replica
    AsyncReplica,
    /// Witness (voting only, no data)
    Witness,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ReplicaStatus {
    /// Up and caught up
    Online,
    /// Online but behind
    CatchingUp,
    /// Not responding
    Offline,
    /// Permanently removed
    Decommissioned,
}

// ============================================================================
// Replication Log
// ============================================================================

/// Entry in the replication log
#[derive(Debug, Clone)]
pub struct ReplicationEntry {
    /// Sequence number
    pub sequence: u64,
    
    /// Operation type
    pub operation: ReplicatedOperation,
    
    /// Timestamp
    pub timestamp: u64,
    
    /// Checksum for integrity
    pub checksum: u64,
}

#[derive(Debug, Clone)]
pub enum ReplicatedOperation {
    /// Key-value write
    Write { key: Vec<u8>, value: Vec<u8> },
    /// Key deletion
    Delete { key: Vec<u8> },
    /// Batch write
    BatchWrite { entries: Vec<(Vec<u8>, Vec<u8>)> },
    /// Checkpoint marker
    Checkpoint { checkpoint_id: u64 },
}

/// Replication log for a shard
pub struct ReplicationLog {
    shard_id: ShardId,
    
    /// Log entries
    entries: RwLock<Vec<ReplicationEntry>>,
    
    /// Last committed sequence
    committed: AtomicU64,
    
    /// Last applied sequence per replica
    replica_progress: RwLock<HashMap<NodeId, u64>>,
    
    /// Log compaction watermark
    compacted_up_to: AtomicU64,
}

impl ReplicationLog {
    pub fn new(shard_id: ShardId) -> Self {
        Self {
            shard_id,
            entries: RwLock::new(Vec::new()),
            committed: AtomicU64::new(0),
            replica_progress: RwLock::new(HashMap::new()),
            compacted_up_to: AtomicU64::new(0),
        }
    }
    
    /// Append an entry to the log
    pub fn append(&self, operation: ReplicatedOperation) -> u64 {
        let mut entries = self.entries.write();
        let sequence = entries.len() as u64 + 1 + self.compacted_up_to.load(Ordering::SeqCst);
        
        let entry = ReplicationEntry {
            sequence,
            operation,
            timestamp: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_micros() as u64,
            checksum: 0, // TODO: Compute checksum
        };
        
        entries.push(entry);
        sequence
    }
    
    /// Get entries from a sequence number
    pub fn get_entries_from(&self, from_sequence: u64) -> Vec<ReplicationEntry> {
        let entries = self.entries.read();
        let compacted = self.compacted_up_to.load(Ordering::SeqCst);
        
        if from_sequence <= compacted {
            // Client needs a full sync
            return Vec::new();
        }
        
        let start_idx = (from_sequence - compacted - 1) as usize;
        entries[start_idx..].to_vec()
    }
    
    /// Mark entries as committed up to sequence
    pub fn commit(&self, up_to: u64) {
        self.committed.fetch_max(up_to, Ordering::SeqCst);
    }
    
    /// Record replica progress
    pub fn record_progress(&self, node_id: NodeId, sequence: u64) {
        self.replica_progress.write().insert(node_id, sequence);
    }
    
    /// Get current committed sequence
    pub fn committed_sequence(&self) -> u64 {
        self.committed.load(Ordering::SeqCst)
    }
    
    /// Compact log up to sequence
    pub fn compact(&self, up_to: u64) {
        let mut entries = self.entries.write();
        let current_compacted = self.compacted_up_to.load(Ordering::SeqCst);
        
        let entries_to_remove = (up_to - current_compacted) as usize;
        if entries_to_remove > 0 && entries_to_remove <= entries.len() {
            entries.drain(0..entries_to_remove);
            self.compacted_up_to.store(up_to, Ordering::SeqCst);
        }
    }
}

// ============================================================================
// Replication Manager
// ============================================================================

/// Manages replication for a shard group
pub struct ReplicationManager {
    /// Configuration
    config: ReplicationConfig,
    
    /// Replication logs per shard
    logs: RwLock<HashMap<ShardId, Arc<ReplicationLog>>>,
    
    /// Replica states
    replicas: RwLock<HashMap<ReplicaId, ReplicaState>>,
    
    /// Primary nodes per shard
    primaries: RwLock<HashMap<ShardId, NodeId>>,
    
    /// Pending writes awaiting acknowledgment
    pending_writes: Mutex<HashMap<u64, PendingWrite>>,
    
    /// Next write ID
    next_write_id: AtomicU64,
}

struct PendingWrite {
    shard_id: ShardId,
    sequence: u64,
    required_acks: u32,
    received_acks: HashSet<NodeId>,
    deadline: Instant,
}

impl ReplicationManager {
    pub fn new(config: ReplicationConfig) -> Self {
        Self {
            config,
            logs: RwLock::new(HashMap::new()),
            replicas: RwLock::new(HashMap::new()),
            primaries: RwLock::new(HashMap::new()),
            pending_writes: Mutex::new(HashMap::new()),
            next_write_id: AtomicU64::new(1),
        }
    }
    
    /// Initialize replication for a shard
    pub fn init_shard(&self, shard_id: ShardId, nodes: Vec<NodeId>) {
        // Create replication log
        let log = Arc::new(ReplicationLog::new(shard_id));
        self.logs.write().insert(shard_id, log);
        
        // Initialize replica states
        let mut replicas = self.replicas.write();
        for (i, node_id) in nodes.iter().enumerate() {
            let role = if i == 0 {
                ReplicaRole::Primary
            } else if i < self.config.min_write_replicas as usize {
                ReplicaRole::SyncReplica
            } else {
                ReplicaRole::AsyncReplica
            };
            
            let replica_id = ReplicaId { shard_id, node_id: *node_id };
            replicas.insert(replica_id, ReplicaState {
                id: replica_id,
                role,
                status: ReplicaStatus::Online,
                last_applied: 0,
                last_committed: 0,
                last_heartbeat: Instant::now(),
                lag: Duration::ZERO,
                failure_domain: None,
            });
        }
        
        // Set primary
        if let Some(first) = nodes.first() {
            self.primaries.write().insert(shard_id, *first);
        }
    }
    
    /// Replicate a write to all replicas
    pub async fn replicate_write(
        &self,
        shard_id: ShardId,
        operation: ReplicatedOperation,
    ) -> Result<u64, StorageError> {
        let log = self.logs.read().get(&shard_id).cloned()
            .ok_or(StorageError::ShardNotFound(shard_id.0))?;
        
        // Append to log
        let sequence = log.append(operation);
        
        // Determine required acknowledgments based on mode
        let required_acks = match self.config.mode {
            ReplicationMode::Synchronous => self.config.replication_factor,
            ReplicationMode::Asynchronous => 1, // Primary only
            ReplicationMode::SemiSynchronous => self.config.min_write_replicas,
        };
        
        // For async mode, return immediately
        if self.config.mode == ReplicationMode::Asynchronous {
            log.commit(sequence);
            return Ok(sequence);
        }
        
        // Register pending write
        let write_id = self.next_write_id.fetch_add(1, Ordering::SeqCst);
        {
            let mut pending = self.pending_writes.lock();
            pending.insert(write_id, PendingWrite {
                shard_id,
                sequence,
                required_acks,
                received_acks: HashSet::new(),
                deadline: Instant::now() + Duration::from_secs(5),
            });
        }
        
        // In real implementation: send to replicas and wait
        // For now, simulate successful replication
        log.commit(sequence);
        
        Ok(sequence)
    }
    
    /// Handle acknowledgment from replica
    pub fn handle_ack(&self, write_id: u64, node_id: NodeId) -> bool {
        let mut pending = self.pending_writes.lock();
        
        if let Some(write) = pending.get_mut(&write_id) {
            write.received_acks.insert(node_id);
            
            if write.received_acks.len() as u32 >= write.required_acks {
                // Commit the write
                if let Some(log) = self.logs.read().get(&write.shard_id) {
                    log.commit(write.sequence);
                }
                pending.remove(&write_id);
                return true;
            }
        }
        
        false
    }
    
    /// Get entries for a replica to catch up
    pub fn get_catchup_entries(&self, shard_id: ShardId, from_sequence: u64) -> Vec<ReplicationEntry> {
        self.logs
            .read()
            .get(&shard_id)
            .map(|log| log.get_entries_from(from_sequence))
            .unwrap_or_default()
    }
    
    /// Update replica progress
    pub fn update_replica_progress(&self, replica_id: ReplicaId, sequence: u64) {
        // Update replica state
        if let Some(state) = self.replicas.write().get_mut(&replica_id) {
            state.last_applied = sequence;
            state.last_heartbeat = Instant::now();
            state.status = ReplicaStatus::Online;
        }
        
        // Update log progress
        if let Some(log) = self.logs.read().get(&replica_id.shard_id) {
            log.record_progress(replica_id.node_id, sequence);
        }
    }
    
    /// Check replica health and detect lag
    pub fn check_health(&self) -> Vec<ReplicaId> {
        let mut lagging = Vec::new();
        let now = Instant::now();
        let mut replicas = self.replicas.write();
        
        for (id, state) in replicas.iter_mut() {
            let since_heartbeat = now.duration_since(state.last_heartbeat);
            
            if since_heartbeat > Duration::from_secs(10) {
                state.status = ReplicaStatus::Offline;
            } else if since_heartbeat > self.config.max_lag {
                state.status = ReplicaStatus::CatchingUp;
                lagging.push(*id);
            }
            
            state.lag = since_heartbeat;
        }
        
        lagging
    }
    
    /// Elect new primary if current fails
    pub fn elect_primary(&self, shard_id: ShardId) -> Option<NodeId> {
        let replicas = self.replicas.read();
        let primaries = self.primaries.read();
        
        let current_primary = primaries.get(&shard_id)?;
        
        // Find best candidate: online sync replica with highest applied sequence
        let candidate = replicas
            .iter()
            .filter(|(id, state)| {
                id.shard_id == shard_id
                    && id.node_id != *current_primary
                    && state.status == ReplicaStatus::Online
                    && (state.role == ReplicaRole::SyncReplica || state.role == ReplicaRole::AsyncReplica)
            })
            .max_by_key(|(_, state)| state.last_applied)?;
        
        Some(candidate.0.node_id)
    }
    
    /// Promote replica to primary
    pub fn promote(&self, shard_id: ShardId, new_primary: NodeId) {
        let mut replicas = self.replicas.write();
        let mut primaries = self.primaries.write();
        
        // Demote old primary
        if let Some(old) = primaries.get(&shard_id) {
            let old_id = ReplicaId { shard_id, node_id: *old };
            if let Some(state) = replicas.get_mut(&old_id) {
                state.role = ReplicaRole::AsyncReplica;
            }
        }
        
        // Promote new primary
        let new_id = ReplicaId { shard_id, node_id: new_primary };
        if let Some(state) = replicas.get_mut(&new_id) {
            state.role = ReplicaRole::Primary;
        }
        
        primaries.insert(shard_id, new_primary);
    }
    
    /// Get replication status for a shard
    pub fn status(&self, shard_id: ShardId) -> ReplicationStatus {
        let replicas = self.replicas.read();
        let log = self.logs.read();
        
        let shard_replicas: Vec<_> = replicas
            .iter()
            .filter(|(id, _)| id.shard_id == shard_id)
            .map(|(_, state)| state.clone())
            .collect();
        
        let online_count = shard_replicas
            .iter()
            .filter(|r| r.status == ReplicaStatus::Online)
            .count() as u32;
        
        let committed = log
            .get(&shard_id)
            .map(|l| l.committed_sequence())
            .unwrap_or(0);
        
        ReplicationStatus {
            shard_id,
            replicas: shard_replicas,
            committed_sequence: committed,
            online_replicas: online_count,
            healthy: online_count >= self.config.min_write_replicas,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ReplicationStatus {
    pub shard_id: ShardId,
    pub replicas: Vec<ReplicaState>,
    pub committed_sequence: u64,
    pub online_replicas: u32,
    pub healthy: bool,
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_replication_log() {
        let log = ReplicationLog::new(ShardId(1));
        
        let seq1 = log.append(ReplicatedOperation::Write {
            key: b"key1".to_vec(),
            value: b"value1".to_vec(),
        });
        
        let seq2 = log.append(ReplicatedOperation::Write {
            key: b"key2".to_vec(),
            value: b"value2".to_vec(),
        });
        
        assert_eq!(seq1, 1);
        assert_eq!(seq2, 2);
        
        log.commit(2);
        assert_eq!(log.committed_sequence(), 2);
    }
    
    #[tokio::test]
    async fn test_replication_manager() {
        let manager = ReplicationManager::new(ReplicationConfig {
            mode: ReplicationMode::Asynchronous,
            ..Default::default()
        });
        
        let shard = ShardId(1);
        manager.init_shard(shard, vec![NodeId(1), NodeId(2), NodeId(3)]);
        
        let seq = manager
            .replicate_write(shard, ReplicatedOperation::Write {
                key: b"test".to_vec(),
                value: b"value".to_vec(),
            })
            .await
            .unwrap();
        
        assert_eq!(seq, 1);
    }
}
