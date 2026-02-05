//! # Cluster Membership Management
//!
//! Handles dynamic membership changes in the Raft cluster using joint consensus.
//!
//! ## Design Decisions
//!
//! 1. **Joint Consensus**: We use Raft's joint consensus approach for safe
//!    membership changes. This allows adding/removing multiple nodes atomically
//!    while maintaining safety (no split-brain).
//!
//! 2. **Two-Phase Changes**: Membership changes happen in two phases:
//!    - C_old → C_old,new (joint configuration)
//!    - C_old,new → C_new (new configuration)
//!
//! 3. **One Change at a Time**: Only one membership change can be in progress.
//!    This simplifies reasoning and prevents complex failure modes.
//!
//! 4. **Learners First**: New nodes join as learners (non-voting) first,
//!    catch up with the log, then get promoted to voters.

use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::time::{Duration, Instant};

use parking_lot::RwLock;

use crate::types::{LogIndex, NodeId, Term};
use super::{ClusterConfig, ConsensusError, ConsensusNode};

// ============================================================================
// Membership Manager
// ============================================================================

/// Manages cluster membership and configuration changes
pub struct MembershipManager {
    /// Reference to consensus node
    consensus: Arc<ConsensusNode>,

    /// Pending membership change (if any)
    pending_change: RwLock<Option<PendingChange>>,

    /// Learner progress tracking
    learner_progress: RwLock<HashMap<NodeId, LearnerProgress>>,

    /// Configuration for membership changes
    config: MembershipConfig,
}

/// Configuration for membership operations
#[derive(Debug, Clone)]
pub struct MembershipConfig {
    /// Minimum time a learner must catch up before promotion
    pub learner_warmup: Duration,
    
    /// Maximum lag (in log entries) before learner can be promoted
    pub max_learner_lag: u64,
    
    /// Timeout for membership change completion
    pub change_timeout: Duration,
    
    /// How often to check learner progress
    pub progress_check_interval: Duration,
}

impl Default for MembershipConfig {
    fn default() -> Self {
        Self {
            learner_warmup: Duration::from_secs(10),
            max_learner_lag: 100,
            change_timeout: Duration::from_secs(60),
            progress_check_interval: Duration::from_secs(1),
        }
    }
}

/// A pending membership change
#[derive(Debug, Clone)]
pub struct PendingChange {
    /// Type of change
    pub change_type: ChangeType,
    /// Target node
    pub node_id: NodeId,
    /// When the change was initiated
    pub started_at: Instant,
    /// Current phase
    pub phase: ChangePhase,
    /// Log index of the configuration entry
    pub config_index: Option<LogIndex>,
}

/// Types of membership changes
#[derive(Debug, Clone, PartialEq)]
pub enum ChangeType {
    /// Add a new voting member
    AddVoter,
    /// Add a new learner (non-voting)
    AddLearner,
    /// Remove a member (voter or learner)
    Remove,
    /// Promote learner to voter
    PromoteLearner,
    /// Demote voter to learner
    DemoteVoter,
}

/// Phase of a membership change
#[derive(Debug, Clone, PartialEq)]
pub enum ChangePhase {
    /// Waiting for learner to catch up
    WaitingForCatchUp,
    /// Joint configuration committed, waiting for new config
    JointCommitted,
    /// New configuration committed
    Completed,
    /// Change failed
    Failed { reason: String },
}

/// Progress tracking for learners
#[derive(Debug, Clone)]
pub struct LearnerProgress {
    pub node_id: NodeId,
    pub match_index: LogIndex,
    pub last_contact: Instant,
    pub replication_rate: f64,  // entries per second
    pub joined_at: Instant,
}

impl MembershipManager {
    pub fn new(consensus: Arc<ConsensusNode>, config: MembershipConfig) -> Self {
        Self {
            consensus,
            pending_change: RwLock::new(None),
            learner_progress: RwLock::new(HashMap::new()),
            config,
        }
    }

    /// Add a new node to the cluster
    ///
    /// The node joins as a learner first, then gets promoted to voter
    /// once it has caught up with the log.
    ///
    /// # Why Learners First?
    /// If we added a voter directly, it would have an empty log and could
    /// cause availability issues (quorum harder to reach). By adding as a
    /// learner first, the node catches up without affecting quorum.
    pub async fn add_node(
        &self,
        node_id: NodeId,
        address: String,
    ) -> Result<(), ConsensusError> {
        // Check no change in progress
        if self.pending_change.read().is_some() {
            return Err(ConsensusError::MembershipChangeInProgress);
        }

        // Check node not already in cluster
        // (would check cluster config here)

        // Start as learner
        let change = PendingChange {
            change_type: ChangeType::AddLearner,
            node_id,
            started_at: Instant::now(),
            phase: ChangePhase::WaitingForCatchUp,
            config_index: None,
        };

        *self.pending_change.write() = Some(change);

        // Initialize learner progress tracking
        self.learner_progress.write().insert(
            node_id,
            LearnerProgress {
                node_id,
                match_index: 0,
                last_contact: Instant::now(),
                replication_rate: 0.0,
                joined_at: Instant::now(),
            },
        );

        // Propose configuration change to add learner
        self.propose_config_change(ConfigurationChange::AddLearner { node_id, address })
            .await?;

        Ok(())
    }

    /// Remove a node from the cluster
    ///
    /// # Removing the Leader
    /// If the leader removes itself, it should:
    /// 1. Commit the configuration change
    /// 2. Step down after the new config is committed
    /// 3. Allow another node to become leader
    pub async fn remove_node(&self, node_id: NodeId) -> Result<(), ConsensusError> {
        if self.pending_change.read().is_some() {
            return Err(ConsensusError::MembershipChangeInProgress);
        }

        let change = PendingChange {
            change_type: ChangeType::Remove,
            node_id,
            started_at: Instant::now(),
            phase: ChangePhase::JointCommitted,
            config_index: None,
        };

        *self.pending_change.write() = Some(change);

        // Use joint consensus for safe removal
        self.propose_joint_config_change(node_id, false).await?;

        Ok(())
    }

    /// Promote a learner to a voting member
    ///
    /// Only succeeds if:
    /// 1. Node is a learner
    /// 2. Node has caught up sufficiently (within max_learner_lag)
    /// 3. Node has been a learner for at least learner_warmup duration
    pub async fn promote_learner(&self, node_id: NodeId) -> Result<(), ConsensusError> {
        // Check learner is ready
        let progress = self
            .learner_progress
            .read()
            .get(&node_id)
            .cloned()
            .ok_or(ConsensusError::NodeNotInCluster(node_id))?;

        // Check warmup period
        if progress.joined_at.elapsed() < self.config.learner_warmup {
            return Err(ConsensusError::Internal(
                "Learner has not completed warmup period".into(),
            ));
        }

        // Check lag
        let leader_index = self.consensus.log_length() as LogIndex;
        if leader_index - progress.match_index > self.config.max_learner_lag {
            return Err(ConsensusError::Internal(format!(
                "Learner too far behind: {} entries",
                leader_index - progress.match_index
            )));
        }

        // Propose promotion
        self.propose_config_change(ConfigurationChange::PromoteLearner { node_id })
            .await?;

        Ok(())
    }

    /// Update learner progress (called when AppendEntries response received)
    pub fn update_learner_progress(&self, node_id: NodeId, match_index: LogIndex) {
        let mut progress = self.learner_progress.write();
        if let Some(p) = progress.get_mut(&node_id) {
            let elapsed = p.last_contact.elapsed().as_secs_f64();
            if elapsed > 0.0 {
                let entries_replicated = match_index.saturating_sub(p.match_index);
                p.replication_rate = entries_replicated as f64 / elapsed;
            }
            p.match_index = match_index;
            p.last_contact = Instant::now();
        }
    }

    /// Check if all learners are ready for promotion
    pub fn learners_ready_for_promotion(&self) -> Vec<NodeId> {
        let leader_index = self.consensus.log_length() as LogIndex;
        let progress = self.learner_progress.read();

        progress
            .values()
            .filter(|p| {
                p.joined_at.elapsed() >= self.config.learner_warmup
                    && leader_index - p.match_index <= self.config.max_learner_lag
            })
            .map(|p| p.node_id)
            .collect()
    }

    /// Propose a configuration change via consensus
    async fn propose_config_change(
        &self,
        change: ConfigurationChange,
    ) -> Result<(), ConsensusError> {
        // Serialize and propose through consensus
        let _payload = self.serialize_config_change(&change);
        
        // TODO: Call consensus.propose() with config change
        // The config change is applied when the entry is committed

        Ok(())
    }

    /// Propose a joint consensus configuration change
    ///
    /// Joint consensus ensures safety during membership changes:
    /// 1. Create C_old,new configuration requiring majorities from both
    /// 2. Once committed, create C_new configuration
    /// 3. Once C_new committed, the change is complete
    async fn propose_joint_config_change(
        &self,
        node_id: NodeId,
        adding: bool,
    ) -> Result<(), ConsensusError> {
        // TODO: Implement joint consensus
        // 1. Create joint configuration
        // 2. Propose and wait for commit
        // 3. Create new configuration
        // 4. Propose and wait for commit
        
        Ok(())
    }

    fn serialize_config_change(&self, _change: &ConfigurationChange) -> Vec<u8> {
        // TODO: Proper serialization
        Vec::new()
    }

    /// Get current cluster members
    pub fn get_members(&self) -> ClusterMembers {
        // TODO: Get from consensus node's cluster config
        ClusterMembers {
            voters: HashSet::new(),
            learners: HashSet::new(),
        }
    }

    /// Get pending change status
    pub fn pending_change_status(&self) -> Option<PendingChange> {
        self.pending_change.read().clone()
    }
}

/// Cluster members summary
#[derive(Debug, Clone)]
pub struct ClusterMembers {
    pub voters: HashSet<NodeId>,
    pub learners: HashSet<NodeId>,
}

impl ClusterMembers {
    pub fn quorum_size(&self) -> usize {
        self.voters.len() / 2 + 1
    }

    pub fn is_voter(&self, node_id: &NodeId) -> bool {
        self.voters.contains(node_id)
    }

    pub fn is_member(&self, node_id: &NodeId) -> bool {
        self.voters.contains(node_id) || self.learners.contains(node_id)
    }
}

/// Configuration change types (serialized in log entries)
#[derive(Debug, Clone)]
pub enum ConfigurationChange {
    AddLearner { node_id: NodeId, address: String },
    RemoveLearner { node_id: NodeId },
    PromoteLearner { node_id: NodeId },
    DemoteVoter { node_id: NodeId },
    JointConfig { old: HashSet<NodeId>, new: HashSet<NodeId> },
    FinalConfig { members: HashSet<NodeId> },
}

// ============================================================================
// Health Checks
// ============================================================================

/// Health status for membership
#[derive(Debug, Clone)]
pub struct MembershipHealth {
    /// Number of healthy voters
    pub healthy_voters: usize,
    /// Total voters
    pub total_voters: usize,
    /// Number of healthy learners
    pub healthy_learners: usize,
    /// Total learners
    pub total_learners: usize,
    /// Whether quorum is reachable
    pub has_quorum: bool,
    /// Unhealthy nodes
    pub unhealthy_nodes: Vec<NodeId>,
}

impl MembershipHealth {
    pub fn is_healthy(&self) -> bool {
        self.has_quorum && self.unhealthy_nodes.is_empty()
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_quorum_calculation() {
        let mut members = ClusterMembers {
            voters: HashSet::new(),
            learners: HashSet::new(),
        };

        // 1 node cluster
        members.voters.insert(NodeId(0));
        assert_eq!(members.quorum_size(), 1);

        // 3 node cluster
        members.voters.insert(NodeId(1));
        members.voters.insert(NodeId(2));
        assert_eq!(members.quorum_size(), 2);

        // 5 node cluster
        members.voters.insert(NodeId(3));
        members.voters.insert(NodeId(4));
        assert_eq!(members.quorum_size(), 3);
    }
}
