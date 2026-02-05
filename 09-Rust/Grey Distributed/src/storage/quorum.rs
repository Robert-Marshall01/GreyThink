//! # Quorum Protocols
//!
//! Quorum-based consistency for distributed reads and writes.
//!
//! ## Quorum Basics
//!
//! A quorum is the minimum number of replicas that must agree
//! for an operation to succeed.
//!
//! For N replicas:
//! - Write quorum W: replicas that must acknowledge write
//! - Read quorum R: replicas that must respond to read
//! - Consistency: R + W > N ensures overlap
//!
//! ## Quorum Configurations
//!
//! | W | R | Behavior |
//! |---|---|----------|
//! | N | 1 | Strong writes, fast reads |
//! | 1 | N | Fast writes, strong reads |
//! | ⌈(N+1)/2⌉ | ⌈(N+1)/2⌉ | Balanced (majority) |
//!
//! ## Trade-offs
//!
//! - Higher W = more durable writes, higher write latency
//! - Higher R = stronger read consistency, higher read latency
//! - Lower W + R = faster but eventually consistent

use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use parking_lot::{Mutex, RwLock};
use tokio::sync::oneshot;

use super::StorageError;
use crate::types::NodeId;

// ============================================================================
// Quorum Configuration
// ============================================================================

/// Quorum configuration
#[derive(Debug, Clone)]
pub struct QuorumConfig {
    /// Total number of replicas
    pub replication_factor: u32,
    
    /// Write quorum
    pub write_quorum: u32,
    
    /// Read quorum
    pub read_quorum: u32,
    
    /// Timeout for quorum operations
    pub timeout: Duration,
    
    /// Whether to prefer local reads
    pub prefer_local: bool,
    
    /// Required replicas for strong consistency
    pub strong_consistency: bool,
}

impl QuorumConfig {
    /// Create majority quorum
    pub fn majority(replication_factor: u32) -> Self {
        let quorum = replication_factor / 2 + 1;
        Self {
            replication_factor,
            write_quorum: quorum,
            read_quorum: quorum,
            timeout: Duration::from_secs(5),
            prefer_local: true,
            strong_consistency: true,
        }
    }
    
    /// Create one-all quorum (fast writes)
    pub fn one_all(replication_factor: u32) -> Self {
        Self {
            replication_factor,
            write_quorum: 1,
            read_quorum: replication_factor,
            timeout: Duration::from_secs(5),
            prefer_local: false,
            strong_consistency: true,
        }
    }
    
    /// Create all-one quorum (fast reads)
    pub fn all_one(replication_factor: u32) -> Self {
        Self {
            replication_factor,
            write_quorum: replication_factor,
            read_quorum: 1,
            timeout: Duration::from_secs(5),
            prefer_local: true,
            strong_consistency: true,
        }
    }
    
    /// Verify quorum configuration
    pub fn is_valid(&self) -> bool {
        self.write_quorum + self.read_quorum > self.replication_factor
    }
}

// ============================================================================
// Quorum Tracker
// ============================================================================

/// Operation being tracked for quorum
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OperationId(pub u64);

/// Tracks responses for quorum operations
pub struct QuorumTracker {
    /// Configuration
    config: QuorumConfig,
    
    /// Pending write operations
    pending_writes: RwLock<HashMap<OperationId, PendingQuorumOp<WriteResponse>>>,
    
    /// Pending read operations
    pending_reads: RwLock<HashMap<OperationId, PendingQuorumOp<ReadResponse>>>,
    
    /// Next operation ID
    next_op_id: AtomicU64,
    
    /// Operation metrics
    metrics: QuorumMetrics,
}

struct PendingQuorumOp<T> {
    required: u32,
    responses: Vec<(NodeId, T)>,
    failed_nodes: HashSet<NodeId>,
    start_time: Instant,
    deadline: Instant,
}

#[derive(Debug, Clone)]
pub struct WriteResponse {
    pub node_id: NodeId,
    pub success: bool,
    pub latency: Duration,
}

#[derive(Debug, Clone)]
pub struct ReadResponse {
    pub node_id: NodeId,
    pub version: u64,
    pub value: Option<Vec<u8>>,
    pub latency: Duration,
}

#[derive(Default)]
struct QuorumMetrics {
    writes_started: AtomicU64,
    writes_succeeded: AtomicU64,
    writes_failed: AtomicU64,
    reads_started: AtomicU64,
    reads_succeeded: AtomicU64,
    reads_failed: AtomicU64,
    total_write_latency_us: AtomicU64,
    total_read_latency_us: AtomicU64,
}

impl QuorumTracker {
    pub fn new(config: QuorumConfig) -> Self {
        Self {
            config,
            pending_writes: RwLock::new(HashMap::new()),
            pending_reads: RwLock::new(HashMap::new()),
            next_op_id: AtomicU64::new(1),
            metrics: QuorumMetrics::default(),
        }
    }
    
    /// Start a write operation
    pub fn start_write(&self) -> OperationId {
        let id = OperationId(self.next_op_id.fetch_add(1, Ordering::SeqCst));
        let now = Instant::now();
        
        self.pending_writes.write().insert(id, PendingQuorumOp {
            required: self.config.write_quorum,
            responses: Vec::new(),
            failed_nodes: HashSet::new(),
            start_time: now,
            deadline: now + self.config.timeout,
        });
        
        self.metrics.writes_started.fetch_add(1, Ordering::Relaxed);
        id
    }
    
    /// Record a write response
    pub fn record_write_response(&self, op_id: OperationId, response: WriteResponse) -> QuorumResult {
        let mut pending = self.pending_writes.write();
        
        let Some(op) = pending.get_mut(&op_id) else {
            return QuorumResult::NotFound;
        };
        
        if response.success {
            op.responses.push((response.node_id, response));
        } else {
            op.failed_nodes.insert(response.node_id);
        }
        
        let success_count = op.responses.len() as u32;
        let fail_count = op.failed_nodes.len() as u32;
        let remaining = self.config.replication_factor - success_count - fail_count;
        let required = op.required;
        let latency = op.start_time.elapsed();
        let deadline = op.deadline;
        
        // Determine result
        let result = if success_count >= required {
            self.metrics.writes_succeeded.fetch_add(1, Ordering::Relaxed);
            self.metrics.total_write_latency_us.fetch_add(
                latency.as_micros() as u64,
                Ordering::Relaxed,
            );
            
            Some(QuorumResult::Achieved { 
                count: success_count,
                latency,
            })
        } else if fail_count > self.config.replication_factor - required {
            self.metrics.writes_failed.fetch_add(1, Ordering::Relaxed);
            
            Some(QuorumResult::Failed {
                reason: format!(
                    "Too many failures: {} of {} failed",
                    fail_count,
                    self.config.replication_factor
                ),
            })
        } else if Instant::now() > deadline {
            self.metrics.writes_failed.fetch_add(1, Ordering::Relaxed);
            
            Some(QuorumResult::Timeout {
                received: success_count,
                required,
            })
        } else {
            None
        };
        
        if let Some(r) = result {
            pending.remove(&op_id);
            r
        } else {
            QuorumResult::Pending {
                received: success_count,
                required,
                remaining,
            }
        }
    }
    
    /// Start a read operation
    pub fn start_read(&self) -> OperationId {
        let id = OperationId(self.next_op_id.fetch_add(1, Ordering::SeqCst));
        let now = Instant::now();
        
        self.pending_reads.write().insert(id, PendingQuorumOp {
            required: self.config.read_quorum,
            responses: Vec::new(),
            failed_nodes: HashSet::new(),
            start_time: now,
            deadline: now + self.config.timeout,
        });
        
        self.metrics.reads_started.fetch_add(1, Ordering::Relaxed);
        id
    }
    
    /// Record a read response
    pub fn record_read_response(&self, op_id: OperationId, response: ReadResponse) -> QuorumReadResult {
        let mut pending = self.pending_reads.write();
        
        let Some(op) = pending.get_mut(&op_id) else {
            return QuorumReadResult::NotFound;
        };
        
        op.responses.push((response.node_id, response));
        
        let count = op.responses.len() as u32;
        let required = op.required;
        let latency = op.start_time.elapsed();
        let deadline = op.deadline;
        let responses: Vec<_> = op.responses.iter().map(|(_, r)| r.clone()).collect();
        
        // Determine result
        let result = if count >= required {
            self.metrics.reads_succeeded.fetch_add(1, Ordering::Relaxed);
            self.metrics.total_read_latency_us.fetch_add(
                latency.as_micros() as u64,
                Ordering::Relaxed,
            );
            
            // Resolve to latest version
            let resolved = Self::resolve_read(&responses);
            
            Some(QuorumReadResult::Achieved {
                value: resolved,
                count,
                latency,
            })
        } else if Instant::now() > deadline {
            self.metrics.reads_failed.fetch_add(1, Ordering::Relaxed);
            
            Some(QuorumReadResult::Timeout {
                received: count,
                required,
            })
        } else {
            None
        };
        
        if let Some(r) = result {
            pending.remove(&op_id);
            r
        } else {
            QuorumReadResult::Pending {
                received: count,
                required,
            }
        }
    }
    
    /// Resolve conflicting read responses
    ///
    /// Uses latest version (highest version number wins)
    fn resolve_read(responses: &[ReadResponse]) -> Option<Vec<u8>> {
        responses
            .iter()
            .filter(|r| r.value.is_some())
            .max_by_key(|r| r.version)
            .and_then(|r| r.value.clone())
    }
    
    /// Get quorum metrics
    pub fn get_metrics(&self) -> QuorumStats {
        let writes_succeeded = self.metrics.writes_succeeded.load(Ordering::Relaxed);
        let reads_succeeded = self.metrics.reads_succeeded.load(Ordering::Relaxed);
        
        QuorumStats {
            writes_started: self.metrics.writes_started.load(Ordering::Relaxed),
            writes_succeeded,
            writes_failed: self.metrics.writes_failed.load(Ordering::Relaxed),
            reads_started: self.metrics.reads_started.load(Ordering::Relaxed),
            reads_succeeded,
            reads_failed: self.metrics.reads_failed.load(Ordering::Relaxed),
            avg_write_latency: if writes_succeeded > 0 {
                Duration::from_micros(
                    self.metrics.total_write_latency_us.load(Ordering::Relaxed) / writes_succeeded
                )
            } else {
                Duration::ZERO
            },
            avg_read_latency: if reads_succeeded > 0 {
                Duration::from_micros(
                    self.metrics.total_read_latency_us.load(Ordering::Relaxed) / reads_succeeded
                )
            } else {
                Duration::ZERO
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum QuorumResult {
    Achieved { count: u32, latency: Duration },
    Pending { received: u32, required: u32, remaining: u32 },
    Failed { reason: String },
    Timeout { received: u32, required: u32 },
    NotFound,
}

#[derive(Debug, Clone)]
pub enum QuorumReadResult {
    Achieved { value: Option<Vec<u8>>, count: u32, latency: Duration },
    Pending { received: u32, required: u32 },
    Timeout { received: u32, required: u32 },
    NotFound,
}

#[derive(Debug, Clone)]
pub struct QuorumStats {
    pub writes_started: u64,
    pub writes_succeeded: u64,
    pub writes_failed: u64,
    pub reads_started: u64,
    pub reads_succeeded: u64,
    pub reads_failed: u64,
    pub avg_write_latency: Duration,
    pub avg_read_latency: Duration,
}

// ============================================================================
// Sloppy Quorum
// ============================================================================

/// Sloppy quorum with hinted handoff
///
/// When preferred nodes are unavailable, temporarily use
/// any available node with a "hint" to forward data later.
///
/// ## Hinted Handoff Flow
///
/// 1. Write to preferred replica fails
/// 2. Write to any available node with hint
/// 3. Available node stores hint
/// 4. When preferred node recovers, forward data
/// 5. Delete hint after successful handoff
pub struct SloppyQuorum {
    /// Base quorum configuration
    config: QuorumConfig,
    
    /// Hints awaiting handoff
    hints: RwLock<HashMap<NodeId, Vec<Hint>>>,
    
    /// Current node status
    node_status: RwLock<HashMap<NodeId, bool>>,
}

#[derive(Debug, Clone)]
pub struct Hint {
    /// Intended recipient
    pub target_node: NodeId,
    
    /// Key that was written
    pub key: Vec<u8>,
    
    /// Value that was written
    pub value: Vec<u8>,
    
    /// Version/timestamp
    pub version: u64,
    
    /// When hint was created
    pub created_at: Instant,
    
    /// Handoff attempts
    pub attempts: u32,
}

impl SloppyQuorum {
    pub fn new(config: QuorumConfig) -> Self {
        Self {
            config,
            hints: RwLock::new(HashMap::new()),
            node_status: RwLock::new(HashMap::new()),
        }
    }
    
    /// Update node status
    pub fn set_node_status(&self, node_id: NodeId, available: bool) {
        self.node_status.write().insert(node_id, available);
    }
    
    /// Check if node is available
    pub fn is_available(&self, node_id: NodeId) -> bool {
        *self.node_status.read().get(&node_id).unwrap_or(&true)
    }
    
    /// Get available nodes from preferred list
    pub fn get_available(&self, preferred: &[NodeId]) -> Vec<NodeId> {
        let status = self.node_status.read();
        preferred
            .iter()
            .filter(|id| *status.get(id).unwrap_or(&true))
            .copied()
            .collect()
    }
    
    /// Get substitute nodes when preferred are unavailable
    pub fn get_substitutes(&self, preferred: &[NodeId], all_nodes: &[NodeId], count: usize) -> Vec<NodeId> {
        let status = self.node_status.read();
        let preferred_set: HashSet<_> = preferred.iter().collect();
        
        all_nodes
            .iter()
            .filter(|id| !preferred_set.contains(id))
            .filter(|id| *status.get(id).unwrap_or(&true))
            .take(count)
            .copied()
            .collect()
    }
    
    /// Store a hint for later handoff
    pub fn store_hint(&self, target: NodeId, key: Vec<u8>, value: Vec<u8>, version: u64) {
        let hint = Hint {
            target_node: target,
            key,
            value,
            version,
            created_at: Instant::now(),
            attempts: 0,
        };
        
        self.hints.write()
            .entry(target)
            .or_insert_with(Vec::new)
            .push(hint);
    }
    
    /// Get hints for a node (for handoff)
    pub fn get_hints(&self, target: NodeId) -> Vec<Hint> {
        self.hints.read()
            .get(&target)
            .cloned()
            .unwrap_or_default()
    }
    
    /// Remove hint after successful handoff
    pub fn remove_hint(&self, target: NodeId, key: &[u8]) {
        if let Some(hints) = self.hints.write().get_mut(&target) {
            hints.retain(|h| h.key != key);
        }
    }
    
    /// Get pending hint count
    pub fn hint_count(&self) -> usize {
        self.hints.read().values().map(|v| v.len()).sum()
    }
    
    /// Clean up old hints
    pub fn cleanup_old_hints(&self, max_age: Duration) {
        let now = Instant::now();
        let mut hints = self.hints.write();
        
        for hints_list in hints.values_mut() {
            hints_list.retain(|h| now.duration_since(h.created_at) < max_age);
        }
        
        hints.retain(|_, v| !v.is_empty());
    }
}

// ============================================================================
// Read Repair
// ============================================================================

/// Repairs stale replicas during reads
///
/// When a quorum read reveals inconsistent versions:
/// 1. Return the latest version to client
/// 2. Asynchronously update stale replicas
///
/// This is "pull-based" anti-entropy - repairs happen
/// opportunistically during normal reads.
pub struct ReadRepair {
    /// Node update functions (in real system, would be RPC)
    pending_repairs: Mutex<Vec<RepairTask>>,
    
    /// Metrics
    repairs_initiated: AtomicU64,
    repairs_completed: AtomicU64,
}

#[derive(Debug, Clone)]
pub struct RepairTask {
    pub node_id: NodeId,
    pub key: Vec<u8>,
    pub value: Vec<u8>,
    pub version: u64,
    pub created_at: Instant,
}

impl ReadRepair {
    pub fn new() -> Self {
        Self {
            pending_repairs: Mutex::new(Vec::new()),
            repairs_initiated: AtomicU64::new(0),
            repairs_completed: AtomicU64::new(0),
        }
    }
    
    /// Check read responses and initiate repairs
    pub fn check_and_repair(&self, responses: &[ReadResponse], key: &[u8]) -> Option<Vec<u8>> {
        if responses.is_empty() {
            return None;
        }
        
        // Find the latest version
        let latest = responses
            .iter()
            .filter(|r| r.value.is_some())
            .max_by_key(|r| r.version)?;
        
        let latest_version = latest.version;
        let latest_value = latest.value.clone()?;
        
        // Find stale replicas
        let stale: Vec<_> = responses
            .iter()
            .filter(|r| {
                r.value.as_ref().map(|_| r.version < latest_version).unwrap_or(true)
            })
            .collect();
        
        // Schedule repairs
        if !stale.is_empty() {
            let mut repairs = self.pending_repairs.lock();
            
            for response in stale {
                self.repairs_initiated.fetch_add(1, Ordering::Relaxed);
                repairs.push(RepairTask {
                    node_id: response.node_id,
                    key: key.to_vec(),
                    value: latest_value.clone(),
                    version: latest_version,
                    created_at: Instant::now(),
                });
            }
        }
        
        Some(latest_value)
    }
    
    /// Get pending repairs
    pub fn drain_repairs(&self) -> Vec<RepairTask> {
        std::mem::take(&mut *self.pending_repairs.lock())
    }
    
    /// Mark repair completed
    pub fn complete_repair(&self) {
        self.repairs_completed.fetch_add(1, Ordering::Relaxed);
    }
    
    /// Get repair stats
    pub fn stats(&self) -> (u64, u64) {
        (
            self.repairs_initiated.load(Ordering::Relaxed),
            self.repairs_completed.load(Ordering::Relaxed),
        )
    }
}

impl Default for ReadRepair {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_quorum_config() {
        let config = QuorumConfig::majority(3);
        assert_eq!(config.write_quorum, 2);
        assert_eq!(config.read_quorum, 2);
        assert!(config.is_valid());
        
        let weak = QuorumConfig {
            replication_factor: 3,
            write_quorum: 1,
            read_quorum: 1,
            ..QuorumConfig::majority(3)
        };
        assert!(!weak.is_valid()); // R + W = 2, not > 3
    }
    
    #[test]
    fn test_quorum_tracker() {
        let tracker = QuorumTracker::new(QuorumConfig::majority(3));
        
        let op = tracker.start_write();
        
        // First response
        let result = tracker.record_write_response(op, WriteResponse {
            node_id: NodeId(1),
            success: true,
            latency: Duration::from_millis(10),
        });
        assert!(matches!(result, QuorumResult::Pending { .. }));
        
        // Second response - achieves quorum
        let result = tracker.record_write_response(op, WriteResponse {
            node_id: NodeId(2),
            success: true,
            latency: Duration::from_millis(15),
        });
        assert!(matches!(result, QuorumResult::Achieved { count: 2, .. }));
    }
    
    #[test]
    fn test_sloppy_quorum() {
        let quorum = SloppyQuorum::new(QuorumConfig::majority(3));
        
        quorum.set_node_status(NodeId(1), true);
        quorum.set_node_status(NodeId(2), false);
        quorum.set_node_status(NodeId(3), true);
        
        let preferred = vec![NodeId(1), NodeId(2), NodeId(3)];
        let available = quorum.get_available(&preferred);
        
        assert_eq!(available.len(), 2);
        assert!(available.contains(&NodeId(1)));
        assert!(available.contains(&NodeId(3)));
    }
    
    #[test]
    fn test_read_repair() {
        let repair = ReadRepair::new();
        
        let responses = vec![
            ReadResponse {
                node_id: NodeId(1),
                version: 5,
                value: Some(b"latest".to_vec()),
                latency: Duration::from_millis(10),
            },
            ReadResponse {
                node_id: NodeId(2),
                version: 3,
                value: Some(b"stale".to_vec()),
                latency: Duration::from_millis(12),
            },
        ];
        
        let result = repair.check_and_repair(&responses, b"key");
        assert_eq!(result, Some(b"latest".to_vec()));
        
        let tasks = repair.drain_repairs();
        assert_eq!(tasks.len(), 1);
        assert_eq!(tasks[0].node_id, NodeId(2));
    }
}
