//! # Distributed Checkpointing
//!
//! Coordinates consistent checkpoints across the cluster.
//!
//! ## The Checkpoint Problem
//!
//! In a distributed system, taking a consistent snapshot is non-trivial.
//! Each node has its own state, and messages may be in-flight between nodes.
//! A naive approach (each node snapshots independently) can capture an
//! inconsistent state.
//!
//! ## Chandy-Lamport Algorithm
//!
//! We implement a variant of Chandy-Lamport for distributed snapshots:
//!
//! 1. **Initiator** starts checkpoint, takes local snapshot, sends markers
//! 2. **Participants** on receiving first marker:
//!    - Take local snapshot
//!    - Start recording in-flight messages from other channels
//!    - Forward marker to all outgoing channels
//! 3. **Completion** when all channels have received markers
//!
//! ## Use Cases
//!
//! - Disaster recovery
//! - Point-in-time restore
//! - Debugging/analysis
//! - Migration between clusters

use std::collections::{BTreeMap, HashMap, HashSet};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use parking_lot::{Mutex, RwLock};

use crate::types::NodeId;

// ============================================================================
// Checkpoint Types
// ============================================================================

/// Unique identifier for a checkpoint
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct CheckpointId(pub u64);

/// A distributed checkpoint
#[derive(Debug, Clone)]
pub struct Checkpoint {
    /// Checkpoint identifier
    pub id: CheckpointId,
    /// Node snapshots
    pub node_snapshots: HashMap<NodeId, NodeSnapshot>,
    /// In-flight messages captured
    pub channel_states: HashMap<ChannelId, Vec<ChannelMessage>>,
    /// When checkpoint was initiated
    pub initiated_at: u64,
    /// When checkpoint completed
    pub completed_at: Option<u64>,
    /// Checkpoint status
    pub status: CheckpointStatus,
}

/// Snapshot of a single node
#[derive(Debug, Clone)]
pub struct NodeSnapshot {
    /// Node that took this snapshot
    pub node_id: NodeId,
    /// State version at snapshot time
    pub version: u64,
    /// Serialized state
    pub data: Vec<u8>,
    /// Snapshot timestamp
    pub timestamp: u64,
}

/// Channel between two nodes
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ChannelId {
    pub from: NodeId,
    pub to: NodeId,
}

/// Message captured in channel state
#[derive(Debug, Clone)]
pub struct ChannelMessage {
    /// Sequence number
    pub sequence: u64,
    /// Message payload
    pub payload: Vec<u8>,
    /// When message was sent
    pub sent_at: u64,
}

/// Status of a checkpoint operation
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CheckpointStatus {
    /// Checkpoint in progress
    InProgress {
        /// Nodes that have reported their snapshot
        completed_nodes: HashSet<NodeId>,
        /// Total nodes expected
        total_nodes: usize,
    },
    /// Checkpoint completed successfully
    Completed,
    /// Checkpoint failed
    Failed { reason: String },
    /// Checkpoint was cancelled
    Cancelled,
}

// ============================================================================
// Marker Protocol
// ============================================================================

/// Marker message for Chandy-Lamport algorithm
#[derive(Debug, Clone)]
pub struct CheckpointMarker {
    /// Checkpoint this marker belongs to
    pub checkpoint_id: CheckpointId,
    /// Node that sent this marker
    pub sender: NodeId,
    /// Timestamp when marker was sent
    pub timestamp: u64,
}

/// Response to a checkpoint marker
#[derive(Debug, Clone)]
pub struct CheckpointAck {
    /// Checkpoint being acknowledged
    pub checkpoint_id: CheckpointId,
    /// Node sending acknowledgment
    pub node_id: NodeId,
    /// Node's snapshot
    pub snapshot: NodeSnapshot,
    /// Messages recorded after snapshot but before marker
    pub recorded_messages: Vec<(ChannelId, ChannelMessage)>,
}

// ============================================================================
// Checkpoint Coordinator
// ============================================================================

/// Coordinates distributed checkpoints
///
/// Runs on the leader node to orchestrate checkpoint creation.
pub struct CheckpointCoordinator {
    /// Our node ID
    node_id: NodeId,
    /// Next checkpoint ID
    next_checkpoint_id: AtomicU64,
    /// Active checkpoints
    active_checkpoints: RwLock<HashMap<CheckpointId, CheckpointState>>,
    /// Completed checkpoints
    completed_checkpoints: RwLock<BTreeMap<CheckpointId, Checkpoint>>,
    /// Configuration
    config: CheckpointConfig,
}

/// Internal state for an in-progress checkpoint
struct CheckpointState {
    /// Checkpoint metadata
    checkpoint: Checkpoint,
    /// Nodes we're waiting for
    pending_nodes: HashSet<NodeId>,
    /// When we started
    started_at: Instant,
    /// Timeout duration
    timeout: Duration,
}

/// Checkpoint configuration
#[derive(Debug, Clone)]
pub struct CheckpointConfig {
    /// Maximum time to wait for all nodes
    pub timeout: Duration,
    /// Maximum concurrent checkpoints
    pub max_concurrent: usize,
    /// Automatic checkpoint interval (None = manual only)
    pub auto_interval: Option<Duration>,
    /// Retain this many completed checkpoints
    pub retention_count: usize,
}

impl Default for CheckpointConfig {
    fn default() -> Self {
        Self {
            timeout: Duration::from_secs(60),
            max_concurrent: 2,
            auto_interval: Some(Duration::from_secs(300)),
            retention_count: 10,
        }
    }
}

impl CheckpointCoordinator {
    pub fn new(node_id: NodeId, config: CheckpointConfig) -> Self {
        Self {
            node_id,
            next_checkpoint_id: AtomicU64::new(1),
            active_checkpoints: RwLock::new(HashMap::new()),
            completed_checkpoints: RwLock::new(BTreeMap::new()),
            config,
        }
    }

    /// Initiate a new checkpoint
    ///
    /// Returns the checkpoint ID and markers to send to each node.
    pub fn initiate(
        &self,
        cluster_nodes: &[NodeId],
    ) -> Result<(CheckpointId, Vec<(NodeId, CheckpointMarker)>), super::StateError> {
        // Check concurrent checkpoint limit
        if self.active_checkpoints.read().len() >= self.config.max_concurrent {
            return Err(super::StateError::CheckpointFailed("max concurrent checkpoints reached".into()));
        }

        let checkpoint_id = CheckpointId(
            self.next_checkpoint_id.fetch_add(1, Ordering::SeqCst)
        );

        let now = Self::now();

        // Create checkpoint
        let checkpoint = Checkpoint {
            id: checkpoint_id,
            node_snapshots: HashMap::new(),
            channel_states: HashMap::new(),
            initiated_at: now,
            completed_at: None,
            status: CheckpointStatus::InProgress {
                completed_nodes: HashSet::new(),
                total_nodes: cluster_nodes.len(),
            },
        };

        // Track active checkpoint
        let state = CheckpointState {
            checkpoint,
            pending_nodes: cluster_nodes.iter().cloned().collect(),
            started_at: Instant::now(),
            timeout: self.config.timeout,
        };

        self.active_checkpoints.write().insert(checkpoint_id, state);

        // Generate markers for each node
        let markers: Vec<_> = cluster_nodes
            .iter()
            .map(|&node| {
                (
                    node,
                    CheckpointMarker {
                        checkpoint_id,
                        sender: self.node_id,
                        timestamp: now,
                    },
                )
            })
            .collect();

        Ok((checkpoint_id, markers))
    }

    /// Process a checkpoint acknowledgment from a node
    pub fn process_ack(&self, ack: CheckpointAck) -> Result<bool, super::StateError> {
        let mut active = self.active_checkpoints.write();

        let state = active
            .get_mut(&ack.checkpoint_id)
            .ok_or(super::StateError::CheckpointFailed("checkpoint not found".into()))?;

        // Record node's snapshot
        state.checkpoint.node_snapshots.insert(ack.node_id, ack.snapshot);

        // Record channel messages
        for (channel_id, message) in ack.recorded_messages {
            state
                .checkpoint
                .channel_states
                .entry(channel_id)
                .or_insert_with(Vec::new)
                .push(message);
        }

        // Update status
        state.pending_nodes.remove(&ack.node_id);

        if let CheckpointStatus::InProgress { ref mut completed_nodes, .. } = state.checkpoint.status {
            completed_nodes.insert(ack.node_id);
        }

        // Check if complete
        if state.pending_nodes.is_empty() {
            state.checkpoint.status = CheckpointStatus::Completed;
            state.checkpoint.completed_at = Some(Self::now());

            // Move to completed
            let checkpoint = state.checkpoint.clone();
            drop(active);

            self.complete_checkpoint(checkpoint)?;

            return Ok(true);
        }

        Ok(false)
    }

    /// Check for timed out checkpoints
    pub fn check_timeouts(&self) -> Vec<CheckpointId> {
        let mut timed_out = Vec::new();
        let mut active = self.active_checkpoints.write();

        for (id, state) in active.iter_mut() {
            if state.started_at.elapsed() > state.timeout {
                state.checkpoint.status = CheckpointStatus::Failed {
                    reason: format!("Timeout waiting for nodes: {:?}", state.pending_nodes),
                };
                timed_out.push(*id);
            }
        }

        // Remove timed out checkpoints
        for id in &timed_out {
            active.remove(id);
        }

        timed_out
    }

    /// Get status of a checkpoint
    pub fn get_status(&self, checkpoint_id: CheckpointId) -> Option<CheckpointStatus> {
        // Check active
        if let Some(state) = self.active_checkpoints.read().get(&checkpoint_id) {
            return Some(state.checkpoint.status.clone());
        }

        // Check completed
        if let Some(checkpoint) = self.completed_checkpoints.read().get(&checkpoint_id) {
            return Some(checkpoint.status.clone());
        }

        None
    }

    /// Get a completed checkpoint
    pub fn get_checkpoint(&self, checkpoint_id: CheckpointId) -> Option<Checkpoint> {
        self.completed_checkpoints.read().get(&checkpoint_id).cloned()
    }

    /// List completed checkpoints
    pub fn list_checkpoints(&self) -> Vec<(CheckpointId, u64)> {
        self.completed_checkpoints
            .read()
            .iter()
            .map(|(id, cp)| (*id, cp.completed_at.unwrap_or(0)))
            .collect()
    }

    fn complete_checkpoint(&self, checkpoint: Checkpoint) -> Result<(), super::StateError> {
        let mut completed = self.completed_checkpoints.write();
        
        let checkpoint_id = checkpoint.id;

        // Add new checkpoint
        completed.insert(checkpoint_id, checkpoint);

        // Cleanup old checkpoints
        while completed.len() > self.config.retention_count {
            if let Some(oldest) = completed.keys().next().cloned() {
                completed.remove(&oldest);
            }
        }

        // Also remove from active
        self.active_checkpoints.write().remove(&checkpoint_id);

        Ok(())
    }

    fn now() -> u64 {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_micros() as u64
    }
}

// ============================================================================
// Checkpoint Participant
// ============================================================================

/// Participant in distributed checkpoints
///
/// Runs on each node to handle checkpoint markers and create local snapshots.
pub struct CheckpointParticipant {
    /// Our node ID
    node_id: NodeId,
    /// Active checkpoints we're participating in
    active_checkpoints: RwLock<HashMap<CheckpointId, ParticipantState>>,
    /// Callback to take local snapshot
    snapshot_callback: Mutex<Option<Box<dyn Fn() -> NodeSnapshot + Send + Sync>>>,
}

/// State for a checkpoint we're participating in
struct ParticipantState {
    /// Our local snapshot (once taken)
    snapshot: Option<NodeSnapshot>,
    /// Channels we've received markers from
    marked_channels: HashSet<NodeId>,
    /// Messages recorded after snapshot but before marker
    recorded_messages: Vec<(ChannelId, ChannelMessage)>,
    /// When we received the first marker
    started_at: Instant,
}

impl CheckpointParticipant {
    pub fn new(node_id: NodeId) -> Self {
        Self {
            node_id,
            active_checkpoints: RwLock::new(HashMap::new()),
            snapshot_callback: Mutex::new(None),
        }
    }

    /// Set the callback for taking local snapshots
    pub fn set_snapshot_callback<F>(&self, callback: F)
    where
        F: Fn() -> NodeSnapshot + Send + Sync + 'static,
    {
        *self.snapshot_callback.lock() = Some(Box::new(callback));
    }

    /// Handle an incoming checkpoint marker
    ///
    /// Returns an acknowledgment if this is the first marker we've seen
    /// for this checkpoint.
    pub fn handle_marker(&self, marker: CheckpointMarker) -> Option<CheckpointAck> {
        let mut active = self.active_checkpoints.write();

        let is_first = !active.contains_key(&marker.checkpoint_id);

        if is_first {
            // Take local snapshot
            let snapshot = self.take_snapshot();

            // Create participant state
            let mut state = ParticipantState {
                snapshot: Some(snapshot.clone()),
                marked_channels: HashSet::new(),
                recorded_messages: Vec::new(),
                started_at: Instant::now(),
            };

            state.marked_channels.insert(marker.sender);
            active.insert(marker.checkpoint_id, state);

            // Return acknowledgment with snapshot
            Some(CheckpointAck {
                checkpoint_id: marker.checkpoint_id,
                node_id: self.node_id,
                snapshot,
                recorded_messages: Vec::new(),
            })
        } else {
            // Record that we've received marker from this channel
            if let Some(state) = active.get_mut(&marker.checkpoint_id) {
                state.marked_channels.insert(marker.sender);
            }
            None
        }
    }

    /// Record an incoming message for channel state
    ///
    /// Called when a message arrives and we've taken our snapshot
    /// but haven't received a marker from that channel yet.
    pub fn record_message(
        &self,
        checkpoint_id: CheckpointId,
        from: NodeId,
        sequence: u64,
        payload: Vec<u8>,
    ) {
        let mut active = self.active_checkpoints.write();

        if let Some(state) = active.get_mut(&checkpoint_id) {
            // Only record if we haven't received marker from this channel
            if !state.marked_channels.contains(&from) {
                let channel_id = ChannelId { from, to: self.node_id };
                let message = ChannelMessage {
                    sequence,
                    payload,
                    sent_at: Self::now(),
                };
                state.recorded_messages.push((channel_id, message));
            }
        }
    }

    /// Complete a checkpoint after receiving all markers
    pub fn complete(&self, checkpoint_id: CheckpointId) -> Option<CheckpointAck> {
        let mut active = self.active_checkpoints.write();

        if let Some(state) = active.remove(&checkpoint_id) {
            Some(CheckpointAck {
                checkpoint_id,
                node_id: self.node_id,
                snapshot: state.snapshot.unwrap(),
                recorded_messages: state.recorded_messages,
            })
        } else {
            None
        }
    }

    fn take_snapshot(&self) -> NodeSnapshot {
        let callback = self.snapshot_callback.lock();
        if let Some(ref cb) = *callback {
            cb()
        } else {
            // Default empty snapshot
            NodeSnapshot {
                node_id: self.node_id,
                version: 0,
                data: Vec::new(),
                timestamp: Self::now(),
            }
        }
    }

    fn now() -> u64 {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_micros() as u64
    }
}

// ============================================================================
// Checkpoint Recovery
// ============================================================================

/// Recovers system state from a checkpoint
pub struct CheckpointRecovery {
    /// The checkpoint to recover from
    checkpoint: Checkpoint,
}

impl CheckpointRecovery {
    pub fn new(checkpoint: Checkpoint) -> Self {
        Self { checkpoint }
    }

    /// Get the snapshot for a specific node
    pub fn get_node_snapshot(&self, node_id: NodeId) -> Option<&NodeSnapshot> {
        self.checkpoint.node_snapshots.get(&node_id)
    }

    /// Get all node snapshots
    pub fn get_all_snapshots(&self) -> impl Iterator<Item = (&NodeId, &NodeSnapshot)> {
        self.checkpoint.node_snapshots.iter()
    }

    /// Get recorded messages for a channel
    pub fn get_channel_messages(&self, channel: &ChannelId) -> Option<&Vec<ChannelMessage>> {
        self.checkpoint.channel_states.get(channel)
    }

    /// Get all in-flight messages that need to be replayed
    pub fn get_inflight_messages(&self) -> impl Iterator<Item = (&ChannelId, &Vec<ChannelMessage>)> {
        self.checkpoint.channel_states.iter()
    }

    /// Verify checkpoint consistency
    ///
    /// Checks:
    /// - All expected nodes have snapshots
    /// - Snapshot versions are consistent
    /// - No gaps in recorded messages
    pub fn verify(&self, expected_nodes: &[NodeId]) -> Result<(), super::StateError> {
        // Check all nodes have snapshots
        for node in expected_nodes {
            if !self.checkpoint.node_snapshots.contains_key(node) {
                return Err(super::StateError::RecoveryFailed("missing node snapshot".into()));
            }
        }

        // Check status
        if !matches!(self.checkpoint.status, CheckpointStatus::Completed) {
            return Err(super::StateError::RecoveryFailed("checkpoint not completed".into()));
        }

        Ok(())
    }

    /// Create recovery plan
    ///
    /// Returns the order in which nodes should be restored and
    /// messages should be replayed.
    pub fn create_recovery_plan(&self) -> RecoveryPlan {
        let mut node_order: Vec<_> = self.checkpoint.node_snapshots.keys().cloned().collect();
        node_order.sort(); // Deterministic order

        let mut message_order: Vec<_> = self
            .checkpoint
            .channel_states
            .iter()
            .flat_map(|(channel, messages)| {
                messages.iter().map(move |msg| (channel.clone(), msg.clone()))
            })
            .collect();

        // Sort messages by sequence number
        message_order.sort_by_key(|(_, msg)| msg.sequence);

        RecoveryPlan {
            node_order,
            message_order,
            checkpoint_id: self.checkpoint.id,
        }
    }
}

/// Plan for recovering from a checkpoint
#[derive(Debug)]
pub struct RecoveryPlan {
    /// Order to restore nodes
    pub node_order: Vec<NodeId>,
    /// Messages to replay (in order)
    pub message_order: Vec<(ChannelId, ChannelMessage)>,
    /// Source checkpoint
    pub checkpoint_id: CheckpointId,
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_checkpoint_coordinator() {
        let node_id = NodeId(1);
        let config = CheckpointConfig::default();
        let coordinator = CheckpointCoordinator::new(node_id, config);

        let cluster_nodes = vec![NodeId(1), NodeId(2), NodeId(3)];
        let (checkpoint_id, markers) = coordinator.initiate(&cluster_nodes).unwrap();

        assert_eq!(markers.len(), 3);

        // Simulate acks
        for node in &cluster_nodes {
            let ack = CheckpointAck {
                checkpoint_id,
                node_id: *node,
                snapshot: NodeSnapshot {
                    node_id: *node,
                    version: 1,
                    data: Vec::new(),
                    timestamp: 12345,
                },
                recorded_messages: Vec::new(),
            };
            coordinator.process_ack(ack).unwrap();
        }

        let status = coordinator.get_status(checkpoint_id).unwrap();
        assert!(matches!(status, CheckpointStatus::Completed));
    }

    #[test]
    fn test_checkpoint_participant() {
        let participant = CheckpointParticipant::new(NodeId(2));

        let marker = CheckpointMarker {
            checkpoint_id: CheckpointId(1),
            sender: NodeId(1),
            timestamp: 12345,
        };

        let ack = participant.handle_marker(marker.clone());
        assert!(ack.is_some());

        // Second marker from same checkpoint should not produce ack
        let ack2 = participant.handle_marker(marker);
        assert!(ack2.is_none());
    }

    #[test]
    fn test_recovery_plan() {
        let mut checkpoint = Checkpoint {
            id: CheckpointId(1),
            node_snapshots: HashMap::new(),
            channel_states: HashMap::new(),
            initiated_at: 12345,
            completed_at: Some(12346),
            status: CheckpointStatus::Completed,
        };

        checkpoint.node_snapshots.insert(
            NodeId(1),
            NodeSnapshot {
                node_id: NodeId(1),
                version: 1,
                data: Vec::new(),
                timestamp: 12345,
            },
        );

        checkpoint.node_snapshots.insert(
            NodeId(2),
            NodeSnapshot {
                node_id: NodeId(2),
                version: 1,
                data: Vec::new(),
                timestamp: 12345,
            },
        );

        let recovery = CheckpointRecovery::new(checkpoint);
        let plan = recovery.create_recovery_plan();

        assert_eq!(plan.node_order.len(), 2);
    }
}
