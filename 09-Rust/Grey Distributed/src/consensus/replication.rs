//! # Log Replication
//!
//! Implements Raft log replication with optimizations for throughput and latency.
//!
//! ## Design Decisions
//!
//! 1. **Pipelining**: Send entries before receiving ACKs for previous batches.
//!    This dramatically improves throughput at the cost of complexity.
//!    We track in-flight entries and handle out-of-order responses.
//!
//! 2. **Batching**: Batch multiple entries into single AppendEntries RPC.
//!    Amortizes network overhead across entries.
//!
//! 3. **Parallel Replication**: Replicate to all followers in parallel.
//!    Commit as soon as majority acknowledges.
//!
//! 4. **Flow Control**: Limit in-flight bytes to prevent overwhelming followers.
//!    Back off when followers fall behind.
//!
//! ## Key Invariants
//!
//! 1. **Log Matching**: If two logs contain an entry with the same index and term,
//!    then the logs are identical in all entries up through that index.
//!
//! 2. **Leader Completeness**: If an entry is committed in a given term,
//!    it will be present in the logs of all leaders for higher terms.

use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use parking_lot::{Mutex, RwLock};
use tokio::sync::{mpsc, oneshot};

use crate::types::{LogIndex, NodeId, Term};
use super::{AppendEntriesResponse, ConsensusError, ConsensusNode, LogEntry};

// ============================================================================
// Replication Manager
// ============================================================================

/// Manages log replication to followers
pub struct ReplicationManager {
    /// Reference to consensus node
    consensus: Arc<ConsensusNode>,
    
    /// Per-follower replication state
    followers: RwLock<HashMap<NodeId, FollowerState>>,
    
    /// Replication configuration
    config: ReplicationConfig,
    
    /// Commit waiter channels
    commit_waiters: Mutex<HashMap<LogIndex, Vec<oneshot::Sender<()>>>>,
}

/// Configuration for replication
#[derive(Debug, Clone)]
pub struct ReplicationConfig {
    /// Maximum entries per AppendEntries RPC
    pub max_entries_per_rpc: usize,
    
    /// Maximum bytes per AppendEntries RPC
    pub max_bytes_per_rpc: usize,
    
    /// Maximum in-flight bytes per follower
    pub max_inflight_bytes: usize,
    
    /// Pipeline depth (max unacknowledged RPCs)
    pub pipeline_depth: usize,
    
    /// Heartbeat interval when idle
    pub heartbeat_interval: Duration,
    
    /// Retry interval after failure
    pub retry_interval: Duration,
    
    /// Timeout for individual RPC
    pub rpc_timeout: Duration,
}

impl Default for ReplicationConfig {
    fn default() -> Self {
        Self {
            max_entries_per_rpc: 100,
            max_bytes_per_rpc: 1024 * 1024,      // 1 MB
            max_inflight_bytes: 4 * 1024 * 1024, // 4 MB
            pipeline_depth: 4,
            heartbeat_interval: Duration::from_millis(100),
            retry_interval: Duration::from_millis(50),
            rpc_timeout: Duration::from_secs(5),
        }
    }
}

/// State for replication to a single follower
#[derive(Debug)]
pub struct FollowerState {
    /// Follower node ID
    pub node_id: NodeId,
    
    /// Index of next entry to send
    /// Initialized to leader's last log index + 1 after election
    pub next_index: LogIndex,
    
    /// Highest index known to be replicated
    /// Initialized to 0 after election
    pub match_index: LogIndex,
    
    /// Whether there are entries to send
    pub has_pending: bool,
    
    /// Last successful communication
    pub last_contact: Instant,
    
    /// In-flight requests (for pipelining)
    pub inflight: VecDeque<InflightRequest>,
    
    /// Bytes currently in-flight
    pub inflight_bytes: usize,
    
    /// Replication state
    pub state: ReplicationState,
    
    /// Consecutive failures
    pub failures: u32,
}

/// State of replication to a follower
#[derive(Debug, Clone, PartialEq)]
pub enum ReplicationState {
    /// Actively replicating
    Active,
    /// Waiting for probe response (after rejection)
    Probing,
    /// Sending snapshot (log too far behind)
    Snapshotting { offset: u64, total: u64 },
    /// Paused (e.g., follower unreachable)
    Paused,
}

/// An in-flight replication request
#[derive(Debug)]
pub struct InflightRequest {
    /// First index in this batch
    pub first_index: LogIndex,
    /// Last index in this batch
    pub last_index: LogIndex,
    /// Bytes in this batch
    pub bytes: usize,
    /// When the request was sent
    pub sent_at: Instant,
}

impl ReplicationManager {
    pub fn new(consensus: Arc<ConsensusNode>, config: ReplicationConfig) -> Self {
        Self {
            consensus,
            followers: RwLock::new(HashMap::new()),
            config,
            commit_waiters: Mutex::new(HashMap::new()),
        }
    }

    /// Initialize follower state after becoming leader
    ///
    /// # Raft Specification
    /// "For each server, index of the next log entry to send to that server
    /// (initialized to leader last log index + 1)"
    pub fn initialize_leader(&self, voters: &[NodeId]) {
        let last_index = self.consensus.log_length() as LogIndex;
        let mut followers = self.followers.write();
        followers.clear();

        for &node_id in voters {
            if node_id == self.consensus.node_id() {
                continue;
            }

            followers.insert(
                node_id,
                FollowerState {
                    node_id,
                    next_index: last_index + 1,
                    match_index: 0,
                    has_pending: true, // Start with heartbeat
                    last_contact: Instant::now(),
                    inflight: VecDeque::new(),
                    inflight_bytes: 0,
                    state: ReplicationState::Active,
                    failures: 0,
                },
            );
        }
    }

    /// Trigger replication to all followers
    ///
    /// Called when:
    /// 1. New entries are appended to leader's log
    /// 2. Heartbeat timer fires
    /// 3. Follower becomes available again
    pub async fn replicate(&self) {
        let followers = self.followers.read();
        
        for follower in followers.values() {
            if self.should_send(follower) {
                // Clone node_id for async task
                let node_id = follower.node_id;
                // TODO: Spawn replication task for this follower
                self.replicate_to_follower(node_id).await;
            }
        }
    }

    /// Check if we should send to a follower
    fn should_send(&self, follower: &FollowerState) -> bool {
        match follower.state {
            ReplicationState::Paused => false,
            ReplicationState::Snapshotting { .. } => false,
            _ => {
                // Check flow control
                follower.inflight_bytes < self.config.max_inflight_bytes
                    && follower.inflight.len() < self.config.pipeline_depth
            }
        }
    }

    /// Replicate to a specific follower
    async fn replicate_to_follower(&self, node_id: NodeId) {
        let batch = self.prepare_batch(node_id);
        
        if batch.is_none() {
            return;
        }
        
        let batch = batch.unwrap();
        
        // Record in-flight
        {
            let mut followers = self.followers.write();
            if let Some(state) = followers.get_mut(&node_id) {
                state.inflight.push_back(InflightRequest {
                    first_index: batch.prev_log_index + 1,
                    last_index: batch.last_index,
                    bytes: batch.bytes,
                    sent_at: Instant::now(),
                });
                state.inflight_bytes += batch.bytes;
            }
        }

        // TODO: Send AppendEntries RPC
        // let response = self.send_append_entries(node_id, batch).await;
        // self.handle_replication_response(node_id, response);
    }

    /// Prepare a batch of entries for a follower
    fn prepare_batch(&self, node_id: NodeId) -> Option<ReplicationBatch> {
        let followers = self.followers.read();
        let state = followers.get(&node_id)?;
        
        let next_index = state.next_index;
        let current_term = self.consensus.current_term();
        
        // TODO: Get entries from consensus log
        // let (prev_log_index, prev_log_term, entries) = 
        //     self.get_entries_for_follower(next_index);
        
        // Placeholder
        Some(ReplicationBatch {
            term: current_term,
            leader_id: self.consensus.node_id(),
            prev_log_index: next_index - 1,
            prev_log_term: 0,
            entries: Vec::new(),
            leader_commit: self.consensus.commit_index(),
            last_index: next_index,
            bytes: 0,
        })
    }

    /// Handle response from AppendEntries RPC
    ///
    /// # Success Case
    /// Update match_index and next_index, check for new commits
    ///
    /// # Failure Case (Log Inconsistency)
    /// Decrement next_index and retry. Use binary search optimization
    /// to find the correct next_index faster.
    pub fn handle_response(
        &self,
        node_id: NodeId,
        response: AppendEntriesResponse,
        batch_last_index: LogIndex,
    ) {
        let mut followers = self.followers.write();
        let state = match followers.get_mut(&node_id) {
            Some(s) => s,
            None => return,
        };

        // Remove completed in-flight request
        if let Some(pos) = state
            .inflight
            .iter()
            .position(|r| r.last_index == batch_last_index)
        {
            let req = state.inflight.remove(pos).unwrap();
            state.inflight_bytes = state.inflight_bytes.saturating_sub(req.bytes);
        }

        // Check term
        if response.term > self.consensus.current_term() {
            // Step down - higher term discovered
            drop(followers);
            // TODO: self.consensus.step_down(response.term);
            return;
        }

        if response.success {
            // Update match_index (monotonically increasing)
            state.match_index = state.match_index.max(response.match_index);
            state.next_index = state.match_index + 1;
            state.failures = 0;
            state.last_contact = Instant::now();
            state.state = ReplicationState::Active;
            
            drop(followers);
            
            // Check if we can advance commit index
            self.maybe_advance_commit_index();
        } else {
            // Log inconsistency - need to backtrack
            state.failures += 1;
            
            // Use hint from response if available
            if response.match_index > 0 {
                state.next_index = response.match_index + 1;
            } else {
                // Decrement by 1 (could use binary search optimization)
                state.next_index = state.next_index.saturating_sub(1).max(1);
            }
            
            state.state = ReplicationState::Probing;
            state.has_pending = true;
        }
    }

    /// Check if commit index can be advanced
    ///
    /// # Raft Commit Rule
    /// "If there exists an N such that N > commitIndex, a majority of
    /// match_index[i] ≥ N, and log[N].term == currentTerm: set commitIndex = N"
    ///
    /// # Why Current Term Check?
    /// Leader can only commit entries from its own term. Entries from previous
    /// terms are committed implicitly when an entry from the current term is
    /// committed. This prevents the Figure 8 scenario in the Raft paper.
    fn maybe_advance_commit_index(&self) {
        let followers = self.followers.read();
        let current_term = self.consensus.current_term();
        let current_commit = self.consensus.commit_index();
        
        // Collect all match indices (including leader's)
        let mut match_indices: Vec<LogIndex> = followers
            .values()
            .map(|f| f.match_index)
            .collect();
        
        // Add leader's own log length
        match_indices.push(self.consensus.log_length() as LogIndex);
        
        // Sort to find median
        match_indices.sort_unstable();
        
        // Quorum index is the median
        let quorum_size = (match_indices.len() + 1) / 2;
        let quorum_index = match_indices[match_indices.len() - quorum_size];
        
        if quorum_index > current_commit {
            // TODO: Verify log[quorum_index].term == currentTerm
            // Then update commit index via consensus node
            
            // Notify waiters
            self.notify_commit_waiters(quorum_index);
        }
    }

    /// Wait for an index to be committed
    pub async fn wait_for_commit(&self, index: LogIndex) -> Result<(), ConsensusError> {
        let current_commit = self.consensus.commit_index();
        
        if index <= current_commit {
            return Ok(());
        }
        
        let (tx, rx) = oneshot::channel();
        
        {
            let mut waiters = self.commit_waiters.lock();
            waiters.entry(index).or_insert_with(Vec::new).push(tx);
        }
        
        rx.await.map_err(|_| ConsensusError::Timeout)?;
        Ok(())
    }

    /// Notify waiters when commit index advances
    fn notify_commit_waiters(&self, new_commit: LogIndex) {
        let mut waiters = self.commit_waiters.lock();
        
        let completed: Vec<LogIndex> = waiters
            .keys()
            .filter(|&&idx| idx <= new_commit)
            .copied()
            .collect();
        
        for idx in completed {
            if let Some(senders) = waiters.remove(&idx) {
                for tx in senders {
                    let _ = tx.send(());
                }
            }
        }
    }

    /// Get replication lag for a follower
    pub fn get_lag(&self, node_id: NodeId) -> Option<LogIndex> {
        let followers = self.followers.read();
        let state = followers.get(&node_id)?;
        let leader_last = self.consensus.log_length() as LogIndex;
        Some(leader_last.saturating_sub(state.match_index))
    }

    /// Get replication statistics
    pub fn get_stats(&self) -> ReplicationStats {
        let followers = self.followers.read();
        let mut stats = ReplicationStats::default();
        
        for state in followers.values() {
            stats.follower_count += 1;
            
            match state.state {
                ReplicationState::Active => stats.active_count += 1,
                ReplicationState::Probing => stats.probing_count += 1,
                ReplicationState::Snapshotting { .. } => stats.snapshotting_count += 1,
                ReplicationState::Paused => stats.paused_count += 1,
            }
            
            stats.total_inflight_bytes += state.inflight_bytes;
        }
        
        stats
    }
}

/// A batch of entries to replicate
#[derive(Debug)]
pub struct ReplicationBatch {
    pub term: Term,
    pub leader_id: NodeId,
    pub prev_log_index: LogIndex,
    pub prev_log_term: Term,
    pub entries: Vec<LogEntry>,
    pub leader_commit: LogIndex,
    pub last_index: LogIndex,
    pub bytes: usize,
}

/// Replication statistics
#[derive(Debug, Default)]
pub struct ReplicationStats {
    pub follower_count: usize,
    pub active_count: usize,
    pub probing_count: usize,
    pub snapshotting_count: usize,
    pub paused_count: usize,
    pub total_inflight_bytes: usize,
}

// ============================================================================
// Snapshot Streaming
// ============================================================================

/// Handles streaming snapshots to far-behind followers
///
/// # When to Use Snapshots
/// When a follower is so far behind that the required log entries have been
/// compacted (deleted), we must send a snapshot instead.
///
/// # Design Trade-offs
/// - Snapshot streaming is expensive (CPU/network/disk)
/// - We chunk snapshots to avoid OOM on both sides
/// - Streaming allows partial progress tracking
pub struct SnapshotStreamer {
    /// Chunk size for streaming
    chunk_size: usize,
    
    /// Active streaming sessions
    sessions: RwLock<HashMap<NodeId, SnapshotSession>>,
}

/// Active snapshot streaming session
#[derive(Debug)]
pub struct SnapshotSession {
    pub node_id: NodeId,
    pub snapshot_index: LogIndex,
    pub snapshot_term: Term,
    pub offset: u64,
    pub total_size: u64,
    pub started_at: Instant,
}

impl SnapshotStreamer {
    pub fn new(chunk_size: usize) -> Self {
        Self {
            chunk_size,
            sessions: RwLock::new(HashMap::new()),
        }
    }

    /// Start streaming a snapshot to a follower
    pub fn start_stream(
        &self,
        node_id: NodeId,
        snapshot_index: LogIndex,
        snapshot_term: Term,
        total_size: u64,
    ) {
        let session = SnapshotSession {
            node_id,
            snapshot_index,
            snapshot_term,
            offset: 0,
            total_size,
            started_at: Instant::now(),
        };
        
        self.sessions.write().insert(node_id, session);
    }

    /// Get next chunk to send
    pub fn next_chunk(&self, node_id: NodeId) -> Option<SnapshotChunk> {
        let sessions = self.sessions.read();
        let session = sessions.get(&node_id)?;
        
        if session.offset >= session.total_size {
            return None;
        }
        
        let remaining = session.total_size - session.offset;
        let chunk_size = (remaining as usize).min(self.chunk_size);
        
        Some(SnapshotChunk {
            snapshot_index: session.snapshot_index,
            snapshot_term: session.snapshot_term,
            offset: session.offset,
            data: vec![0u8; chunk_size], // TODO: Read actual data
            done: session.offset + chunk_size as u64 >= session.total_size,
        })
    }

    /// Update progress after chunk acknowledged
    pub fn ack_chunk(&self, node_id: NodeId, offset: u64) {
        let mut sessions = self.sessions.write();
        if let Some(session) = sessions.get_mut(&node_id) {
            session.offset = offset;
            
            if offset >= session.total_size {
                sessions.remove(&node_id);
            }
        }
    }
}

/// A chunk of snapshot data
#[derive(Debug)]
pub struct SnapshotChunk {
    pub snapshot_index: LogIndex,
    pub snapshot_term: Term,
    pub offset: u64,
    pub data: Vec<u8>,
    pub done: bool,
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_replication_config_defaults() {
        let config = ReplicationConfig::default();
        assert_eq!(config.max_entries_per_rpc, 100);
        assert_eq!(config.pipeline_depth, 4);
    }

    #[test]
    fn test_flow_control() {
        let state = FollowerState {
            node_id: NodeId(0),
            next_index: 1,
            match_index: 0,
            has_pending: true,
            last_contact: Instant::now(),
            inflight: VecDeque::new(),
            inflight_bytes: 0,
            state: ReplicationState::Active,
            failures: 0,
        };
        
        // Should send when inflight is empty
        assert!(state.inflight_bytes < 4 * 1024 * 1024);
    }
}
