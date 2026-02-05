//! # Grey Distributed — Inter-Cluster Consensus Protocol
//!
//! This module implements consensus across federated Grey clusters.
//!
//! ## Architecture
//!
//! Federation consensus operates at two levels:
//! 1. **Local Consensus**: Each cluster runs independent Raft for internal operations
//! 2. **Global Consensus**: Federated clusters coordinate via a lighter-weight protocol
//!
//! ## Design Tradeoffs
//!
//! - **Consistency vs Latency**: Use eventual consistency for cross-region operations
//! - **Autonomy vs Coordination**: Each cluster retains sovereignty, global decisions are advisory
//! - **Availability vs Partition Tolerance**: Prefer availability during network partitions
//!
//! ## Protocol: Federated Raft (F-Raft)
//!
//! F-Raft adapts Raft for high-latency, partition-prone inter-cluster links:
//! - Longer election timeouts (seconds, not milliseconds)
//! - Hierarchical log replication (batch updates)
//! - Conflict resolution via vector clocks + lamport timestamps

use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::{mpsc, RwLock};

// =============================================================================
// Types and Constants
// =============================================================================

/// Unique identifier for a federated cluster
pub type ClusterId = String;

/// Unique identifier for a federation member
pub type MemberId = String;

/// Term number for federation leader election
pub type FederationTerm = u64;

/// Index into the federation log
pub type FederationLogIndex = u64;

/// Federation-wide election timeout (much longer than local Raft)
const FEDERATION_ELECTION_TIMEOUT_MIN: Duration = Duration::from_secs(10);
const FEDERATION_ELECTION_TIMEOUT_MAX: Duration = Duration::from_secs(30);

/// Heartbeat interval for federation leader
const FEDERATION_HEARTBEAT_INTERVAL: Duration = Duration::from_secs(5);

/// Maximum entries per federation append
const MAX_FEDERATION_BATCH_SIZE: usize = 100;

// =============================================================================
// Federation Member State
// =============================================================================

/// State of a federation member
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FederationState {
    /// Following the current federation leader
    Follower,
    /// Seeking election as federation leader
    Candidate,
    /// Leading the federation (coordinating global decisions)
    Leader,
    /// Disconnected from federation (operating autonomously)
    Disconnected,
}

/// Health status of a federated cluster
#[derive(Debug, Clone)]
pub struct ClusterHealth {
    pub cluster_id: ClusterId,
    pub last_heartbeat: Instant,
    pub latency_ms: u64,
    pub local_leader: Option<String>,
    pub node_count: u32,
    pub healthy_nodes: u32,
    pub queue_depth: u64,
    pub cpu_utilization: f32,
    pub memory_utilization: f32,
}

/// Entry in the federation log (global decisions)
#[derive(Debug, Clone)]
pub struct FederationLogEntry {
    pub term: FederationTerm,
    pub index: FederationLogIndex,
    pub timestamp: u64,
    pub command: FederationCommand,
    /// Vector clock for conflict detection
    pub vector_clock: HashMap<ClusterId, u64>,
}

/// Commands that require federation-wide consensus
#[derive(Debug, Clone)]
pub enum FederationCommand {
    /// Add a new cluster to the federation
    AddCluster {
        cluster_id: ClusterId,
        endpoint: String,
        region: String,
        attestation: SecurityAttestation,
    },
    /// Remove a cluster from the federation
    RemoveCluster {
        cluster_id: ClusterId,
        reason: String,
    },
    /// Update resource quotas for a tenant across clusters
    UpdateGlobalQuota {
        tenant_id: String,
        cpu_quota: u64,
        memory_quota: u64,
        storage_quota: u64,
    },
    /// Migrate workload between clusters
    MigrateWorkload {
        workload_id: String,
        source_cluster: ClusterId,
        target_cluster: ClusterId,
        reason: String,
    },
    /// Record a dispute resolution outcome
    DisputeResolution {
        dispute_id: String,
        outcome: DisputeOutcome,
    },
    /// Update federation policy
    PolicyUpdate {
        policy_id: String,
        policy_version: u64,
        policy_hash: String,
    },
    /// Attestation refresh for a cluster
    AttestationRefresh {
        cluster_id: ClusterId,
        attestation: SecurityAttestation,
    },
}

/// Security attestation for cross-cluster trust
#[derive(Debug, Clone)]
pub struct SecurityAttestation {
    pub issued_at: u64,
    pub expires_at: u64,
    pub issuer: String,
    pub subject: ClusterId,
    pub claims: HashMap<String, String>,
    pub signature: Vec<u8>,
}

/// Outcome of a dispute resolution
#[derive(Debug, Clone)]
pub enum DisputeOutcome {
    TenantFavored { tenant_id: String, remedy: String },
    ClusterFavored { cluster_id: ClusterId, reason: String },
    Compromise { terms: String },
    Escalated { to: String },
}

// =============================================================================
// Federation Consensus Node
// =============================================================================

/// A node participating in federation consensus
pub struct FederationNode {
    /// This cluster's ID
    cluster_id: ClusterId,
    
    /// Current federation state
    state: FederationState,
    
    /// Current federation term
    current_term: FederationTerm,
    
    /// ID of cluster we voted for in current term
    voted_for: Option<ClusterId>,
    
    /// Current federation leader
    leader_id: Option<ClusterId>,
    
    /// Federation log (global decisions)
    log: Vec<FederationLogEntry>,
    
    /// Index of highest log entry known to be committed
    commit_index: FederationLogIndex,
    
    /// Index of highest log entry applied to state machine
    last_applied: FederationLogIndex,
    
    /// Known federation members
    members: HashMap<ClusterId, ClusterHealth>,
    
    /// Vector clock for this cluster
    vector_clock: HashMap<ClusterId, u64>,
    
    /// Pending commands awaiting consensus
    pending_commands: VecDeque<FederationCommand>,
    
    /// Last time we heard from the leader
    last_leader_contact: Instant,
    
    /// Election timeout (randomized)
    election_timeout: Duration,
}

impl FederationNode {
    /// Create a new federation node
    pub fn new(cluster_id: ClusterId, initial_members: Vec<ClusterId>) -> Self {
        let mut members = HashMap::new();
        for member in initial_members {
            members.insert(member.clone(), ClusterHealth {
                cluster_id: member,
                last_heartbeat: Instant::now(),
                latency_ms: 0,
                local_leader: None,
                node_count: 0,
                healthy_nodes: 0,
                queue_depth: 0,
                cpu_utilization: 0.0,
                memory_utilization: 0.0,
            });
        }
        
        let mut vector_clock = HashMap::new();
        vector_clock.insert(cluster_id.clone(), 0);
        
        Self {
            cluster_id,
            state: FederationState::Follower,
            current_term: 0,
            voted_for: None,
            leader_id: None,
            log: Vec::new(),
            commit_index: 0,
            last_applied: 0,
            members,
            vector_clock,
            pending_commands: VecDeque::new(),
            last_leader_contact: Instant::now(),
            election_timeout: random_election_timeout(),
        }
    }
    
    // =========================================================================
    // Step 1: Election Timeout Detection
    // =========================================================================
    
    /// Check if election timeout has elapsed
    /// 
    /// Federation uses longer timeouts due to WAN latency.
    /// Timeout is randomized to prevent split votes.
    pub fn should_start_election(&self) -> bool {
        // Only followers and candidates can start elections
        if self.state == FederationState::Leader {
            return false;
        }
        
        // Check if we've exceeded election timeout
        self.last_leader_contact.elapsed() > self.election_timeout
    }
    
    // =========================================================================
    // Step 2: Start Election
    // =========================================================================
    
    /// Transition to candidate and request votes from all members
    /// 
    /// Returns vote requests to send to each member.
    pub fn start_election(&mut self) -> Vec<(ClusterId, VoteRequest)> {
        // Increment term
        self.current_term += 1;
        
        // Vote for self
        self.voted_for = Some(self.cluster_id.clone());
        
        // Become candidate
        self.state = FederationState::Candidate;
        
        // Reset election timeout (randomize for next attempt)
        self.election_timeout = random_election_timeout();
        self.last_leader_contact = Instant::now();
        
        // Prepare vote requests for all other members
        let last_log_index = self.log.len() as FederationLogIndex;
        let last_log_term = self.log.last().map(|e| e.term).unwrap_or(0);
        
        self.members
            .keys()
            .filter(|id| *id != &self.cluster_id)
            .map(|id| {
                (
                    id.clone(),
                    VoteRequest {
                        term: self.current_term,
                        candidate_id: self.cluster_id.clone(),
                        last_log_index,
                        last_log_term,
                        // Include attestation for security
                        attestation_hash: self.get_attestation_hash(),
                    },
                )
            })
            .collect()
    }
    
    // =========================================================================
    // Step 3: Handle Vote Request
    // =========================================================================
    
    /// Process a vote request from another cluster
    /// 
    /// Grant vote if:
    /// 1. Candidate's term >= our term
    /// 2. We haven't voted for someone else this term
    /// 3. Candidate's log is at least as up-to-date as ours
    /// 4. Candidate has valid attestation
    pub fn handle_vote_request(&mut self, request: VoteRequest) -> VoteResponse {
        // Step 3a: Check term
        if request.term < self.current_term {
            return VoteResponse {
                term: self.current_term,
                vote_granted: false,
                reason: "Stale term".to_string(),
            };
        }
        
        // Step 3b: Update term if necessary
        if request.term > self.current_term {
            self.current_term = request.term;
            self.state = FederationState::Follower;
            self.voted_for = None;
            self.leader_id = None;
        }
        
        // Step 3c: Check if we already voted for someone else
        if let Some(ref voted_for) = self.voted_for {
            if voted_for != &request.candidate_id {
                return VoteResponse {
                    term: self.current_term,
                    vote_granted: false,
                    reason: format!("Already voted for {}", voted_for),
                };
            }
        }
        
        // Step 3d: Check log up-to-date-ness
        let our_last_log_index = self.log.len() as FederationLogIndex;
        let our_last_log_term = self.log.last().map(|e| e.term).unwrap_or(0);
        
        let candidate_log_ok = request.last_log_term > our_last_log_term
            || (request.last_log_term == our_last_log_term
                && request.last_log_index >= our_last_log_index);
        
        if !candidate_log_ok {
            return VoteResponse {
                term: self.current_term,
                vote_granted: false,
                reason: "Candidate log not up-to-date".to_string(),
            };
        }
        
        // Step 3e: Verify attestation (security check)
        if !self.verify_attestation(&request.candidate_id, &request.attestation_hash) {
            return VoteResponse {
                term: self.current_term,
                vote_granted: false,
                reason: "Invalid attestation".to_string(),
            };
        }
        
        // Step 3f: Grant vote
        self.voted_for = Some(request.candidate_id.clone());
        self.last_leader_contact = Instant::now();
        
        VoteResponse {
            term: self.current_term,
            vote_granted: true,
            reason: "Vote granted".to_string(),
        }
    }
    
    // =========================================================================
    // Step 4: Process Vote Responses
    // =========================================================================
    
    /// Process vote responses and potentially become leader
    /// 
    /// Becomes leader if we receive votes from a majority of clusters.
    pub fn process_votes(&mut self, votes: Vec<(ClusterId, VoteResponse)>) -> bool {
        if self.state != FederationState::Candidate {
            return false;
        }
        
        // Count votes (including our own vote for ourselves)
        let mut vote_count = 1; // Self-vote
        
        for (cluster_id, response) in votes {
            // Check for higher term
            if response.term > self.current_term {
                self.current_term = response.term;
                self.state = FederationState::Follower;
                self.voted_for = None;
                return false;
            }
            
            if response.vote_granted {
                vote_count += 1;
            }
        }
        
        // Check if we have majority
        let majority = (self.members.len() / 2) + 1;
        
        if vote_count >= majority {
            self.become_leader();
            return true;
        }
        
        false
    }
    
    // =========================================================================
    // Step 5: Become Leader
    // =========================================================================
    
    /// Transition to leader state
    fn become_leader(&mut self) {
        self.state = FederationState::Leader;
        self.leader_id = Some(self.cluster_id.clone());
        
        // Increment vector clock for this event
        self.increment_vector_clock();
        
        // Leader immediately sends heartbeats to establish authority
        // (Handled by caller)
    }
    
    // =========================================================================
    // Step 6: Leader Heartbeat
    // =========================================================================
    
    /// Generate heartbeat (empty append entries) for all followers
    /// 
    /// Heartbeats maintain leader authority and replicate log entries.
    pub fn generate_heartbeats(&mut self) -> Vec<(ClusterId, AppendEntries)> {
        if self.state != FederationState::Leader {
            return Vec::new();
        }
        
        self.members
            .keys()
            .filter(|id| *id != &self.cluster_id)
            .map(|id| {
                (
                    id.clone(),
                    AppendEntries {
                        term: self.current_term,
                        leader_id: self.cluster_id.clone(),
                        prev_log_index: self.log.len() as FederationLogIndex,
                        prev_log_term: self.log.last().map(|e| e.term).unwrap_or(0),
                        entries: Vec::new(), // Empty for heartbeat
                        leader_commit: self.commit_index,
                        leader_vector_clock: self.vector_clock.clone(),
                    },
                )
            })
            .collect()
    }
    
    // =========================================================================
    // Step 7: Handle Append Entries (Follower)
    // =========================================================================
    
    /// Process append entries from leader
    /// 
    /// This is how followers receive global decisions.
    pub fn handle_append_entries(&mut self, request: AppendEntries) -> AppendEntriesResponse {
        // Step 7a: Check term
        if request.term < self.current_term {
            return AppendEntriesResponse {
                term: self.current_term,
                success: false,
                match_index: 0,
                reason: "Stale term".to_string(),
            };
        }
        
        // Step 7b: Recognize leader
        if request.term > self.current_term {
            self.current_term = request.term;
            self.voted_for = None;
        }
        
        self.state = FederationState::Follower;
        self.leader_id = Some(request.leader_id.clone());
        self.last_leader_contact = Instant::now();
        
        // Step 7c: Merge vector clocks
        self.merge_vector_clock(&request.leader_vector_clock);
        
        // Step 7d: Check log consistency
        if request.prev_log_index > 0 {
            if request.prev_log_index > self.log.len() as FederationLogIndex {
                return AppendEntriesResponse {
                    term: self.current_term,
                    success: false,
                    match_index: self.log.len() as FederationLogIndex,
                    reason: "Log gap".to_string(),
                };
            }
            
            let prev_entry = &self.log[(request.prev_log_index - 1) as usize];
            if prev_entry.term != request.prev_log_term {
                // Conflict: remove this and all following entries
                self.log.truncate((request.prev_log_index - 1) as usize);
                return AppendEntriesResponse {
                    term: self.current_term,
                    success: false,
                    match_index: self.log.len() as FederationLogIndex,
                    reason: "Log conflict".to_string(),
                };
            }
        }
        
        // Step 7e: Append new entries
        for entry in request.entries {
            let entry_index = entry.index as usize;
            if entry_index > self.log.len() {
                self.log.push(entry);
            } else if entry_index == self.log.len() {
                self.log.push(entry);
            }
            // If entry already exists with same index and term, skip
        }
        
        // Step 7f: Update commit index
        if request.leader_commit > self.commit_index {
            self.commit_index = std::cmp::min(
                request.leader_commit,
                self.log.len() as FederationLogIndex,
            );
        }
        
        AppendEntriesResponse {
            term: self.current_term,
            success: true,
            match_index: self.log.len() as FederationLogIndex,
            reason: "Success".to_string(),
        }
    }
    
    // =========================================================================
    // Step 8: Apply Committed Entries
    // =========================================================================
    
    /// Apply committed entries to the federation state machine
    /// 
    /// This executes global decisions locally.
    pub fn apply_committed(&mut self) -> Vec<FederationLogEntry> {
        let mut applied = Vec::new();
        
        while self.last_applied < self.commit_index {
            self.last_applied += 1;
            let entry = self.log[(self.last_applied - 1) as usize].clone();
            
            // Apply the command to local state
            self.apply_command(&entry.command);
            
            applied.push(entry);
        }
        
        applied
    }
    
    /// Apply a federation command to local state
    fn apply_command(&mut self, command: &FederationCommand) {
        match command {
            FederationCommand::AddCluster { cluster_id, .. } => {
                self.members.insert(cluster_id.clone(), ClusterHealth {
                    cluster_id: cluster_id.clone(),
                    last_heartbeat: Instant::now(),
                    latency_ms: 0,
                    local_leader: None,
                    node_count: 0,
                    healthy_nodes: 0,
                    queue_depth: 0,
                    cpu_utilization: 0.0,
                    memory_utilization: 0.0,
                });
                self.vector_clock.insert(cluster_id.clone(), 0);
            }
            FederationCommand::RemoveCluster { cluster_id, .. } => {
                self.members.remove(cluster_id);
                self.vector_clock.remove(cluster_id);
            }
            // Other commands are handled by higher-level modules
            _ => {}
        }
    }
    
    // =========================================================================
    // Step 9: Propose Commands (Leader Only)
    // =========================================================================
    
    /// Propose a new command for federation consensus
    /// 
    /// Only the leader can propose commands.
    pub fn propose(&mut self, command: FederationCommand) -> Result<FederationLogIndex, String> {
        if self.state != FederationState::Leader {
            return Err(format!(
                "Not leader. Current leader: {:?}",
                self.leader_id
            ));
        }
        
        // Increment vector clock
        self.increment_vector_clock();
        
        // Create log entry
        let entry = FederationLogEntry {
            term: self.current_term,
            index: (self.log.len() + 1) as FederationLogIndex,
            timestamp: current_timestamp(),
            command,
            vector_clock: self.vector_clock.clone(),
        };
        
        let index = entry.index;
        self.log.push(entry);
        
        Ok(index)
    }
    
    // =========================================================================
    // Helper Functions
    // =========================================================================
    
    fn increment_vector_clock(&mut self) {
        let counter = self.vector_clock.entry(self.cluster_id.clone()).or_insert(0);
        *counter += 1;
    }
    
    fn merge_vector_clock(&mut self, other: &HashMap<ClusterId, u64>) {
        for (cluster, counter) in other {
            let our_counter = self.vector_clock.entry(cluster.clone()).or_insert(0);
            *our_counter = std::cmp::max(*our_counter, *counter);
        }
    }
    
    fn get_attestation_hash(&self) -> String {
        // In production, this would be a cryptographic hash of the attestation
        format!("attestation-{}", self.cluster_id)
    }
    
    fn verify_attestation(&self, _cluster_id: &ClusterId, _hash: &str) -> bool {
        // In production, verify against trusted attestation authority
        true
    }
    
    // =========================================================================
    // Public Accessors
    // =========================================================================
    
    pub fn cluster_id(&self) -> &ClusterId {
        &self.cluster_id
    }
    
    pub fn state(&self) -> &FederationState {
        &self.state
    }
    
    pub fn current_term(&self) -> FederationTerm {
        self.current_term
    }
    
    pub fn leader_id(&self) -> Option<&ClusterId> {
        self.leader_id.as_ref()
    }
    
    pub fn commit_index(&self) -> FederationLogIndex {
        self.commit_index
    }
    
    pub fn members(&self) -> &HashMap<ClusterId, ClusterHealth> {
        &self.members
    }
}

// =============================================================================
// Protocol Messages
// =============================================================================

/// Vote request message
#[derive(Debug, Clone)]
pub struct VoteRequest {
    pub term: FederationTerm,
    pub candidate_id: ClusterId,
    pub last_log_index: FederationLogIndex,
    pub last_log_term: FederationTerm,
    pub attestation_hash: String,
}

/// Vote response message
#[derive(Debug, Clone)]
pub struct VoteResponse {
    pub term: FederationTerm,
    pub vote_granted: bool,
    pub reason: String,
}

/// Append entries request (log replication)
#[derive(Debug, Clone)]
pub struct AppendEntries {
    pub term: FederationTerm,
    pub leader_id: ClusterId,
    pub prev_log_index: FederationLogIndex,
    pub prev_log_term: FederationTerm,
    pub entries: Vec<FederationLogEntry>,
    pub leader_commit: FederationLogIndex,
    pub leader_vector_clock: HashMap<ClusterId, u64>,
}

/// Append entries response
#[derive(Debug, Clone)]
pub struct AppendEntriesResponse {
    pub term: FederationTerm,
    pub success: bool,
    pub match_index: FederationLogIndex,
    pub reason: String,
}

// =============================================================================
// Utility Functions
// =============================================================================

fn random_election_timeout() -> Duration {
    use std::time::SystemTime;
    let nanos = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .subsec_nanos();
    
    let range = FEDERATION_ELECTION_TIMEOUT_MAX.as_millis()
        - FEDERATION_ELECTION_TIMEOUT_MIN.as_millis();
    let offset = (nanos as u128 % range) as u64;
    
    FEDERATION_ELECTION_TIMEOUT_MIN + Duration::from_millis(offset)
}

fn current_timestamp() -> u64 {
    use std::time::SystemTime;
    SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_secs()
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_election_starts_after_timeout() {
        let mut node = FederationNode::new(
            "cluster-1".to_string(),
            vec!["cluster-2".to_string(), "cluster-3".to_string()],
        );
        
        // Initially should not start election
        assert!(!node.should_start_election());
        
        // After timeout, should start election
        node.last_leader_contact = Instant::now() - Duration::from_secs(60);
        assert!(node.should_start_election());
    }
    
    #[test]
    fn test_vote_granted_for_higher_term() {
        let mut node = FederationNode::new(
            "cluster-1".to_string(),
            vec!["cluster-2".to_string()],
        );
        
        let request = VoteRequest {
            term: 5,
            candidate_id: "cluster-2".to_string(),
            last_log_index: 0,
            last_log_term: 0,
            attestation_hash: "attestation-cluster-2".to_string(),
        };
        
        let response = node.handle_vote_request(request);
        assert!(response.vote_granted);
        assert_eq!(node.current_term, 5);
    }
    
    #[test]
    fn test_leader_can_propose() {
        let mut node = FederationNode::new(
            "cluster-1".to_string(),
            vec!["cluster-2".to_string(), "cluster-3".to_string()],
        );
        
        // Make node a leader
        node.state = FederationState::Leader;
        node.current_term = 1;
        
        let result = node.propose(FederationCommand::AddCluster {
            cluster_id: "cluster-4".to_string(),
            endpoint: "https://cluster-4.example.com".to_string(),
            region: "us-west-2".to_string(),
            attestation: SecurityAttestation {
                issued_at: 0,
                expires_at: 0,
                issuer: "".to_string(),
                subject: "".to_string(),
                claims: HashMap::new(),
                signature: Vec::new(),
            },
        });
        
        assert!(result.is_ok());
        assert_eq!(node.log.len(), 1);
    }
}
