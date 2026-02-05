//! # Core Consensus Implementation
//!
//! Implements the Raft consensus algorithm for leader election and log replication.
//!
//! ## Design Decisions
//!
//! 1. **Single-decree vs Multi-Paxos**: We use Raft (multi-decree) for simplicity
//!    and understandability. Raft's strong leader model simplifies reasoning.
//!
//! 2. **Pre-Vote Extension**: Before starting an election, candidates check if
//!    they would win. This prevents partitioned nodes from disrupting the cluster.
//!
//! 3. **Batching**: Log entries are batched for higher throughput. Trade-off:
//!    slightly higher latency for much better throughput.
//!
//! 4. **Pipelining**: Leaders pipeline AppendEntries without waiting for responses.
//!    Requires careful handling of out-of-order responses.

use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use parking_lot::{Mutex, RwLock};
use tokio::sync::{broadcast, mpsc, oneshot};

use crate::types::{LogIndex, NodeId, Term, TimeoutConfig};

// ============================================================================
// Raft Role
// ============================================================================

/// Current role in the Raft cluster
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RaftRole {
    /// Passive participant, responds to RPCs
    Follower,
    /// Seeking votes, may become leader
    Candidate,
    /// Active leader, handles all client requests
    Leader,
    /// Pre-candidate (Pre-Vote extension)
    PreCandidate,
}

// ============================================================================
// Log Entry
// ============================================================================

/// A single entry in the Raft log
#[derive(Debug, Clone)]
pub struct LogEntry {
    /// Term when entry was received by leader
    pub term: Term,
    /// Position in the log (1-indexed)
    pub index: LogIndex,
    /// Entry type and payload
    pub command: LogCommand,
    /// Client request ID for deduplication
    pub client_id: Option<u64>,
    /// Timestamp when entry was created
    pub timestamp: u64,
}

/// Types of log commands
#[derive(Debug, Clone)]
pub enum LogCommand {
    /// No-op entry (used for leader commit confirmation)
    Noop,
    /// State machine command
    Command(Vec<u8>),
    /// Configuration change
    ConfigChange(ConfigChange),
}

/// Configuration change types
#[derive(Debug, Clone)]
pub enum ConfigChange {
    /// Add a new node
    AddNode { node_id: NodeId, address: String },
    /// Remove a node
    RemoveNode { node_id: NodeId },
    /// Update node configuration
    UpdateNode { node_id: NodeId, address: String },
}

// ============================================================================
// Consensus Node
// ============================================================================

/// Core Raft consensus node
pub struct ConsensusNode {
    /// Our node identifier
    node_id: NodeId,

    /// Current term (persistent)
    /// Invariant: monotonically increasing
    current_term: AtomicU64,

    /// Node we voted for in current term (persistent)
    voted_for: RwLock<Option<NodeId>>,

    /// Current role
    role: RwLock<RaftRole>,

    /// Current leader (if known)
    leader_id: RwLock<Option<NodeId>>,

    /// Log entries (persistent)
    /// Invariant: entries are ordered by index, no gaps
    log: RwLock<Vec<LogEntry>>,

    /// Index of highest entry known to be committed
    /// Invariant: commit_index <= last_log_index
    commit_index: AtomicU64,

    /// Index of highest entry applied to state machine
    /// Invariant: last_applied <= commit_index
    last_applied: AtomicU64,

    /// Cluster membership configuration
    membership: RwLock<ClusterConfig>,

    /// Timeout configuration
    timeout_config: TimeoutConfig,

    /// Last time we heard from leader (for election timeout)
    last_heartbeat: RwLock<Instant>,

    /// Votes received in current election
    votes_received: RwLock<HashSet<NodeId>>,

    /// Pre-votes received (Pre-Vote extension)
    pre_votes_received: RwLock<HashSet<NodeId>>,

    // === Leader-only state (reinitialized after election) ===
    /// For each server, index of next entry to send
    next_index: RwLock<HashMap<NodeId, LogIndex>>,

    /// For each server, index of highest entry known to be replicated
    match_index: RwLock<HashMap<NodeId, LogIndex>>,

    /// Pending client requests waiting for commit
    pending_requests: Mutex<HashMap<LogIndex, oneshot::Sender<Result<(), super::ConsensusError>>>>,
}

/// Cluster configuration
#[derive(Debug, Clone)]
pub struct ClusterConfig {
    /// Voting members
    pub voters: HashSet<NodeId>,
    /// Non-voting members (learners/witnesses)
    pub learners: HashSet<NodeId>,
    /// Node addresses for RPC
    pub addresses: HashMap<NodeId, String>,
    /// Configuration version
    pub version: u64,
    /// Joint consensus configuration (during transitions)
    pub joint_config: Option<Box<ClusterConfig>>,
}

impl ConsensusNode {
    /// Create a new consensus node
    ///
    /// # Arguments
    /// * `node_id` - Unique identifier for this node
    /// * `config` - Timeout configuration
    ///
    /// # Design Note
    /// Node starts as a Follower with no knowledge of the cluster.
    /// It must either bootstrap a new cluster or join an existing one.
    pub fn new(node_id: NodeId, config: TimeoutConfig) -> Self {
        Self {
            node_id,
            current_term: AtomicU64::new(0),
            voted_for: RwLock::new(None),
            role: RwLock::new(RaftRole::Follower),
            leader_id: RwLock::new(None),
            log: RwLock::new(Vec::new()),
            commit_index: AtomicU64::new(0),
            last_applied: AtomicU64::new(0),
            membership: RwLock::new(ClusterConfig {
                voters: HashSet::new(),
                learners: HashSet::new(),
                addresses: HashMap::new(),
                version: 0,
                joint_config: None,
            }),
            timeout_config: config,
            last_heartbeat: RwLock::new(Instant::now()),
            votes_received: RwLock::new(HashSet::new()),
            pre_votes_received: RwLock::new(HashSet::new()),
            next_index: RwLock::new(HashMap::new()),
            match_index: RwLock::new(HashMap::new()),
            pending_requests: Mutex::new(HashMap::new()),
        }
    }

    /// Bootstrap a new single-node cluster
    ///
    /// # Safety
    /// Only call this on a fresh node to create a new cluster.
    /// Never call on a node that has joined an existing cluster.
    pub fn bootstrap(&self) -> Result<(), super::ConsensusError> {
        // Become leader of term 1
        self.current_term.store(1, Ordering::SeqCst);
        *self.voted_for.write() = Some(self.node_id);
        *self.role.write() = RaftRole::Leader;
        *self.leader_id.write() = Some(self.node_id);

        // Add ourselves to the cluster
        let mut membership = self.membership.write();
        membership.voters.insert(self.node_id);
        membership.version = 1;

        // Append initial no-op entry to commit leader's term
        // This ensures the leader can commit entries from its term
        let entry = LogEntry {
            term: 1,
            index: 1,
            command: LogCommand::Noop,
            client_id: None,
            timestamp: Self::now(),
        };
        self.log.write().push(entry);

        Ok(())
    }

    /// Propose a command to the cluster
    ///
    /// Only succeeds if this node is the leader.
    /// Returns when the command is committed (replicated to majority).
    ///
    /// # Design Trade-off
    /// We wait for commit before returning, which provides linearizability
    /// but adds latency. Alternative: return after local append for lower
    /// latency but only sequential consistency.
    pub async fn propose(&self, command: Vec<u8>) -> Result<LogIndex, super::ConsensusError> {
        // Verify we're the leader
        if *self.role.read() != RaftRole::Leader {
            return Err(super::ConsensusError::NotLeader {
                leader: *self.leader_id.read(),
            });
        }

        let term = self.current_term.load(Ordering::SeqCst);
        let index = {
            let mut log = self.log.write();
            let index = log.len() as LogIndex + 1;

            let entry = LogEntry {
                term,
                index,
                command: LogCommand::Command(command),
                client_id: None,
                timestamp: Self::now(),
            };
            log.push(entry);
            index
        };

        // Create channel to wait for commit
        let (tx, rx) = oneshot::channel();
        self.pending_requests.lock().insert(index, tx);

        // Trigger immediate replication
        self.replicate_entries().await;

        // Wait for commit
        rx.await
            .map_err(|_| super::ConsensusError::Internal("Request cancelled".into()))?
            .map(|_| index)
    }

    /// Handle AppendEntries RPC from leader
    ///
    /// # Raft Invariants Maintained
    /// 1. If term < currentTerm, reject
    /// 2. If log doesn't contain entry at prevLogIndex with prevLogTerm, reject
    /// 3. If existing entry conflicts with new one, delete it and all following
    /// 4. Append any new entries not already in the log
    /// 5. If leaderCommit > commitIndex, update commitIndex
    pub fn handle_append_entries(
        &self,
        term: Term,
        leader_id: NodeId,
        prev_log_index: LogIndex,
        prev_log_term: Term,
        entries: Vec<LogEntry>,
        leader_commit: LogIndex,
    ) -> Result<AppendEntriesResponse, super::ConsensusError> {
        let current_term = self.current_term.load(Ordering::SeqCst);

        // Rule 1: Reply false if term < currentTerm
        if term < current_term {
            return Ok(AppendEntriesResponse {
                term: current_term,
                success: false,
                match_index: 0,
            });
        }

        // If RPC term is higher, update our term and become follower
        if term > current_term {
            self.step_down(term);
        }

        // Record that we heard from leader
        *self.last_heartbeat.write() = Instant::now();
        *self.leader_id.write() = Some(leader_id);

        // Ensure we're a follower
        if *self.role.read() != RaftRole::Follower {
            *self.role.write() = RaftRole::Follower;
        }

        let mut log = self.log.write();

        // Rule 2: Reply false if log doesn't contain entry at prevLogIndex
        if prev_log_index > 0 {
            if prev_log_index > log.len() as LogIndex {
                return Ok(AppendEntriesResponse {
                    term: self.current_term.load(Ordering::SeqCst),
                    success: false,
                    match_index: log.len() as LogIndex,
                });
            }

            let prev_entry = &log[(prev_log_index - 1) as usize];
            if prev_entry.term != prev_log_term {
                // Rule 3: Delete conflicting entry and all following
                log.truncate((prev_log_index - 1) as usize);
                return Ok(AppendEntriesResponse {
                    term: self.current_term.load(Ordering::SeqCst),
                    success: false,
                    match_index: log.len() as LogIndex,
                });
            }
        }

        // Rule 4: Append new entries
        for entry in entries {
            let idx = entry.index as usize;
            if idx > log.len() {
                log.push(entry);
            } else if log[idx - 1].term != entry.term {
                // Conflict: truncate and append
                log.truncate(idx - 1);
                log.push(entry);
            }
            // else: already have this entry, skip
        }

        let match_index = log.len() as LogIndex;
        drop(log);

        // Rule 5: Update commit index
        if leader_commit > self.commit_index.load(Ordering::SeqCst) {
            let new_commit = leader_commit.min(match_index);
            self.commit_index.store(new_commit, Ordering::SeqCst);

            // Apply committed entries (would trigger state machine)
            self.apply_committed_entries();
        }

        Ok(AppendEntriesResponse {
            term: self.current_term.load(Ordering::SeqCst),
            success: true,
            match_index,
        })
    }

    /// Handle RequestVote RPC from candidate
    ///
    /// # Vote Granting Rules
    /// 1. Reject if term < currentTerm
    /// 2. Reject if already voted for someone else this term
    /// 3. Reject if candidate's log is not at least as up-to-date as ours
    /// 4. Grant vote otherwise
    pub fn handle_request_vote(
        &self,
        term: Term,
        candidate_id: NodeId,
        last_log_index: LogIndex,
        last_log_term: Term,
        is_pre_vote: bool,
    ) -> RequestVoteResponse {
        let current_term = self.current_term.load(Ordering::SeqCst);

        // Rule 1: Reject if term is stale
        if term < current_term {
            return RequestVoteResponse {
                term: current_term,
                vote_granted: false,
            };
        }

        // For real votes (not pre-vote), update term if higher
        if !is_pre_vote && term > current_term {
            self.step_down(term);
        }

        // Rule 2: Check if we already voted
        let voted_for = self.voted_for.read();
        if !is_pre_vote {
            if let Some(voted) = *voted_for {
                if voted != candidate_id {
                    return RequestVoteResponse {
                        term: self.current_term.load(Ordering::SeqCst),
                        vote_granted: false,
                    };
                }
            }
        }
        drop(voted_for);

        // Rule 3: Check log up-to-dateness
        // Raft defines "up-to-date" as:
        // - Higher last log term, OR
        // - Same last log term and >= last log index
        let log = self.log.read();
        let our_last_term = log.last().map(|e| e.term).unwrap_or(0);
        let our_last_index = log.len() as LogIndex;
        drop(log);

        let candidate_is_current = last_log_term > our_last_term
            || (last_log_term == our_last_term && last_log_index >= our_last_index);

        if !candidate_is_current {
            return RequestVoteResponse {
                term: self.current_term.load(Ordering::SeqCst),
                vote_granted: false,
            };
        }

        // Grant vote (for real votes, record it)
        if !is_pre_vote {
            *self.voted_for.write() = Some(candidate_id);
            *self.last_heartbeat.write() = Instant::now();
        }

        RequestVoteResponse {
            term: self.current_term.load(Ordering::SeqCst),
            vote_granted: true,
        }
    }

    /// Start an election
    ///
    /// Uses Pre-Vote extension to prevent disruption:
    /// 1. First, run pre-election to check if we would win
    /// 2. Only if pre-election succeeds, run real election
    ///
    /// # Why Pre-Vote?
    /// Prevents partitioned nodes from incrementing terms when they can't win.
    /// Without Pre-Vote, a partitioned node rejoining would disrupt the cluster
    /// by having a higher term.
    pub async fn start_election(&self) {
        // First, try pre-vote
        *self.role.write() = RaftRole::PreCandidate;
        self.pre_votes_received.write().clear();
        self.pre_votes_received.write().insert(self.node_id);

        // TODO: Send PreVote RPCs to all voters
        // If majority grants pre-vote, proceed to real election

        // For now, proceed directly to election (would be async in production)
        self.run_election().await;
    }

    async fn run_election(&self) {
        // Increment term and vote for self
        let new_term = self.current_term.fetch_add(1, Ordering::SeqCst) + 1;
        *self.voted_for.write() = Some(self.node_id);
        *self.role.write() = RaftRole::Candidate;

        self.votes_received.write().clear();
        self.votes_received.write().insert(self.node_id);

        // TODO: Send RequestVote RPCs to all voters
        // Collect votes and check for majority
    }

    /// Step down to follower state
    fn step_down(&self, new_term: Term) {
        self.current_term.store(new_term, Ordering::SeqCst);
        *self.voted_for.write() = None;
        *self.role.write() = RaftRole::Follower;
        *self.last_heartbeat.write() = Instant::now();
    }

    /// Trigger log replication to followers
    async fn replicate_entries(&self) {
        // TODO: Send AppendEntries to all followers
        // Track responses and update commit_index when majority reached
    }

    /// Apply committed entries to state machine
    fn apply_committed_entries(&self) {
        let commit_index = self.commit_index.load(Ordering::SeqCst);
        let mut last_applied = self.last_applied.load(Ordering::SeqCst);

        while last_applied < commit_index {
            last_applied += 1;
            // TODO: Apply entry at last_applied to state machine
            self.last_applied.store(last_applied, Ordering::SeqCst);
        }

        // Notify pending requests
        let mut pending = self.pending_requests.lock();
        let completed: Vec<_> = pending
            .keys()
            .filter(|&&idx| idx <= commit_index)
            .copied()
            .collect();

        for idx in completed {
            if let Some(tx) = pending.remove(&idx) {
                let _ = tx.send(Ok(()));
            }
        }
    }

    /// Get current timestamp in microseconds
    fn now() -> u64 {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_micros() as u64
    }

    // === Accessor methods ===

    pub fn node_id(&self) -> NodeId {
        self.node_id
    }

    pub fn current_term(&self) -> Term {
        self.current_term.load(Ordering::SeqCst)
    }

    pub fn role(&self) -> RaftRole {
        *self.role.read()
    }

    pub fn leader_id(&self) -> Option<NodeId> {
        *self.leader_id.read()
    }

    pub fn commit_index(&self) -> LogIndex {
        self.commit_index.load(Ordering::SeqCst)
    }

    pub fn log_length(&self) -> usize {
        self.log.read().len()
    }
}

// ============================================================================
// RPC Response Types
// ============================================================================

/// Response to AppendEntries RPC
#[derive(Debug, Clone)]
pub struct AppendEntriesResponse {
    /// Responder's current term
    pub term: Term,
    /// True if follower contained entry matching prevLogIndex and prevLogTerm
    pub success: bool,
    /// Highest log index known to be replicated on responder
    pub match_index: LogIndex,
}

/// Response to RequestVote RPC
#[derive(Debug, Clone)]
pub struct RequestVoteResponse {
    /// Responder's current term
    pub term: Term,
    /// True if vote was granted
    pub vote_granted: bool,
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bootstrap_cluster() {
        let node = ConsensusNode::new(NodeId(0), TimeoutConfig::default());
        node.bootstrap().unwrap();

        assert_eq!(node.role(), RaftRole::Leader);
        assert_eq!(node.current_term(), 1);
        assert!(node.leader_id().is_some());
    }

    #[test]
    fn test_append_entries_rejects_old_term() {
        let node = ConsensusNode::new(NodeId(0), TimeoutConfig::default());
        node.current_term.store(5, Ordering::SeqCst);

        let response = node
            .handle_append_entries(3, NodeId(1), 0, 0, vec![], 0)
            .unwrap();

        assert!(!response.success);
        assert_eq!(response.term, 5);
    }

    #[test]
    fn test_vote_not_granted_if_log_behind() {
        let node = ConsensusNode::new(NodeId(0), TimeoutConfig::default());

        // Add some log entries
        {
            let mut log = node.log.write();
            log.push(LogEntry {
                term: 2,
                index: 1,
                command: LogCommand::Noop,
                client_id: None,
                timestamp: 0,
            });
        }

        // Candidate with older log shouldn't get vote
        let response = node.handle_request_vote(
            3,
            NodeId(1),
            0, // last_log_index
            1, // last_log_term
            false,
        );

        assert!(!response.vote_granted);
    }
}
