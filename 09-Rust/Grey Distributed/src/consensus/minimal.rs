//! Grey Distributed — Minimal Consensus Implementation
//!
//! A minimal but runnable Raft-style consensus implementation demonstrating:
//! - Leader election with randomized timeouts
//! - Log replication with quorum acknowledgment
//! - Membership tracking
//!
//! # Design Tradeoffs
//!
//! - **Simplicity over optimization**: Single-threaded state machine for clarity
//! - **In-memory log**: No persistence (add WAL for production)
//! - **No snapshots**: Log grows unbounded (add compaction for production)
//! - **No pre-vote**: Simpler elections, but more disruption during partitions

use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::{mpsc, RwLock};
use tokio::time::interval;

// ============================================================================
// Types
// ============================================================================

pub type NodeId = u64;
pub type Term = u64;
pub type LogIndex = u64;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Role {
    Follower,
    Candidate,
    Leader,
}

#[derive(Debug, Clone)]
pub struct LogEntry {
    pub term: Term,
    pub index: LogIndex,
    pub command: Command,
}

#[derive(Debug, Clone)]
pub enum Command {
    /// No-op for leader establishment
    Noop,
    /// Client command with arbitrary payload
    Data(Vec<u8>),
    /// Membership change
    AddNode(NodeId),
    RemoveNode(NodeId),
}

#[derive(Debug, Clone)]
pub enum Message {
    // Vote requests
    RequestVote {
        term: Term,
        candidate_id: NodeId,
        last_log_index: LogIndex,
        last_log_term: Term,
    },
    RequestVoteResponse {
        term: Term,
        vote_granted: bool,
    },
    
    // Log replication
    AppendEntries {
        term: Term,
        leader_id: NodeId,
        prev_log_index: LogIndex,
        prev_log_term: Term,
        entries: Vec<LogEntry>,
        leader_commit: LogIndex,
    },
    AppendEntriesResponse {
        term: Term,
        success: bool,
        match_index: LogIndex,
    },
}

#[derive(Debug)]
pub struct ConsensusConfig {
    pub node_id: NodeId,
    pub peers: HashSet<NodeId>,
    pub election_timeout_min: Duration,
    pub election_timeout_max: Duration,
    pub heartbeat_interval: Duration,
}

impl Default for ConsensusConfig {
    fn default() -> Self {
        Self {
            node_id: 1,
            peers: HashSet::new(),
            election_timeout_min: Duration::from_millis(150),
            election_timeout_max: Duration::from_millis(300),
            heartbeat_interval: Duration::from_millis(50),
        }
    }
}

// ============================================================================
// Consensus State
// ============================================================================

pub struct ConsensusState {
    // Persistent state (would be on disk in production)
    pub current_term: Term,
    pub voted_for: Option<NodeId>,
    pub log: Vec<LogEntry>,
    
    // Volatile state
    pub commit_index: LogIndex,
    pub last_applied: LogIndex,
    
    // Leader state (reinitialized after election)
    pub next_index: HashMap<NodeId, LogIndex>,
    pub match_index: HashMap<NodeId, LogIndex>,
    
    // Node state
    pub role: Role,
    pub leader_id: Option<NodeId>,
    pub votes_received: HashSet<NodeId>,
    pub last_heartbeat: Instant,
}

impl ConsensusState {
    pub fn new() -> Self {
        Self {
            current_term: 0,
            voted_for: None,
            log: Vec::new(),
            commit_index: 0,
            last_applied: 0,
            next_index: HashMap::new(),
            match_index: HashMap::new(),
            role: Role::Follower,
            leader_id: None,
            votes_received: HashSet::new(),
            last_heartbeat: Instant::now(),
        }
    }
    
    pub fn last_log_index(&self) -> LogIndex {
        self.log.last().map(|e| e.index).unwrap_or(0)
    }
    
    pub fn last_log_term(&self) -> Term {
        self.log.last().map(|e| e.term).unwrap_or(0)
    }
    
    pub fn get_entry(&self, index: LogIndex) -> Option<&LogEntry> {
        if index == 0 || index as usize > self.log.len() {
            return None;
        }
        self.log.get((index - 1) as usize)
    }
    
    pub fn term_at(&self, index: LogIndex) -> Term {
        self.get_entry(index).map(|e| e.term).unwrap_or(0)
    }
}

// ============================================================================
// Consensus Node
// ============================================================================

pub struct ConsensusNode {
    pub config: ConsensusConfig,
    pub state: Arc<RwLock<ConsensusState>>,
    outbound_tx: mpsc::Sender<(NodeId, Message)>,
    commit_tx: mpsc::Sender<LogEntry>,
}

impl ConsensusNode {
    pub fn new(
        config: ConsensusConfig,
        outbound_tx: mpsc::Sender<(NodeId, Message)>,
        commit_tx: mpsc::Sender<LogEntry>,
    ) -> Self {
        Self {
            config,
            state: Arc::new(RwLock::new(ConsensusState::new())),
            outbound_tx,
            commit_tx,
        }
    }
    
    /// Start the consensus node event loop.
    pub async fn run(
        self: Arc<Self>,
        mut inbound_rx: mpsc::Receiver<(NodeId, Message)>,
        mut shutdown_rx: mpsc::Receiver<()>,
    ) {
        let mut tick_interval = interval(Duration::from_millis(10));
        
        loop {
            tokio::select! {
                _ = shutdown_rx.recv() => {
                    break;
                }
                _ = tick_interval.tick() => {
                    self.tick().await;
                }
                Some((from, msg)) = inbound_rx.recv() => {
                    self.handle_message(from, msg).await;
                }
            }
        }
    }
    
    /// Periodic tick for election timeouts and heartbeats.
    async fn tick(&self) {
        let mut state = self.state.write().await;
        
        match state.role {
            Role::Follower | Role::Candidate => {
                // Check election timeout
                let timeout = self.random_election_timeout();
                if state.last_heartbeat.elapsed() > timeout {
                    drop(state);
                    self.start_election().await;
                }
            }
            Role::Leader => {
                // Send heartbeats
                if state.last_heartbeat.elapsed() > self.config.heartbeat_interval {
                    state.last_heartbeat = Instant::now();
                    drop(state);
                    self.send_heartbeats().await;
                }
            }
        }
    }
    
    fn random_election_timeout(&self) -> Duration {
        use rand::Rng;
        let min = self.config.election_timeout_min.as_millis() as u64;
        let max = self.config.election_timeout_max.as_millis() as u64;
        let ms = rand::thread_rng().gen_range(min..=max);
        Duration::from_millis(ms)
    }
    
    /// Start a new election.
    async fn start_election(&self) {
        let mut state = self.state.write().await;
        
        state.current_term += 1;
        state.role = Role::Candidate;
        state.voted_for = Some(self.config.node_id);
        state.votes_received.clear();
        state.votes_received.insert(self.config.node_id);
        state.last_heartbeat = Instant::now();
        
        let term = state.current_term;
        let last_log_index = state.last_log_index();
        let last_log_term = state.last_log_term();
        
        drop(state);
        
        // Request votes from all peers
        let msg = Message::RequestVote {
            term,
            candidate_id: self.config.node_id,
            last_log_index,
            last_log_term,
        };
        
        for &peer in &self.config.peers {
            let _ = self.outbound_tx.send((peer, msg.clone())).await;
        }
    }
    
    /// Send heartbeats (empty AppendEntries) to all peers.
    async fn send_heartbeats(&self) {
        let state = self.state.read().await;
        
        if state.role != Role::Leader {
            return;
        }
        
        let term = state.current_term;
        let leader_commit = state.commit_index;
        
        for &peer in &self.config.peers {
            let next_idx = *state.next_index.get(&peer).unwrap_or(&1);
            let prev_log_index = next_idx.saturating_sub(1);
            let prev_log_term = state.term_at(prev_log_index);
            
            // Collect entries to send
            let entries: Vec<LogEntry> = state.log
                .iter()
                .filter(|e| e.index >= next_idx)
                .cloned()
                .collect();
            
            let msg = Message::AppendEntries {
                term,
                leader_id: self.config.node_id,
                prev_log_index,
                prev_log_term,
                entries,
                leader_commit,
            };
            
            let _ = self.outbound_tx.send((peer, msg)).await;
        }
    }
    
    /// Handle an incoming message.
    async fn handle_message(&self, from: NodeId, msg: Message) {
        match msg {
            Message::RequestVote { term, candidate_id, last_log_index, last_log_term } => {
                self.handle_request_vote(from, term, candidate_id, last_log_index, last_log_term).await;
            }
            Message::RequestVoteResponse { term, vote_granted } => {
                self.handle_vote_response(from, term, vote_granted).await;
            }
            Message::AppendEntries { term, leader_id, prev_log_index, prev_log_term, entries, leader_commit } => {
                self.handle_append_entries(from, term, leader_id, prev_log_index, prev_log_term, entries, leader_commit).await;
            }
            Message::AppendEntriesResponse { term, success, match_index } => {
                self.handle_append_response(from, term, success, match_index).await;
            }
        }
    }
    
    async fn handle_request_vote(
        &self,
        from: NodeId,
        term: Term,
        candidate_id: NodeId,
        last_log_index: LogIndex,
        last_log_term: Term,
    ) {
        let mut state = self.state.write().await;
        
        // Update term if stale
        if term > state.current_term {
            state.current_term = term;
            state.voted_for = None;
            state.role = Role::Follower;
        }
        
        let vote_granted = if term < state.current_term {
            false
        } else if state.voted_for.is_some() && state.voted_for != Some(candidate_id) {
            false
        } else {
            // Check if candidate's log is at least as up-to-date
            let our_last_term = state.last_log_term();
            let our_last_index = state.last_log_index();
            
            let log_ok = last_log_term > our_last_term
                || (last_log_term == our_last_term && last_log_index >= our_last_index);
            
            if log_ok {
                state.voted_for = Some(candidate_id);
                state.last_heartbeat = Instant::now();
                true
            } else {
                false
            }
        };
        
        let response = Message::RequestVoteResponse {
            term: state.current_term,
            vote_granted,
        };
        
        drop(state);
        let _ = self.outbound_tx.send((from, response)).await;
    }
    
    async fn handle_vote_response(&self, from: NodeId, term: Term, vote_granted: bool) {
        let mut state = self.state.write().await;
        
        if term > state.current_term {
            state.current_term = term;
            state.voted_for = None;
            state.role = Role::Follower;
            return;
        }
        
        if state.role != Role::Candidate || term != state.current_term {
            return;
        }
        
        if vote_granted {
            state.votes_received.insert(from);
            
            // Check for majority
            let total_nodes = self.config.peers.len() + 1;
            let votes = state.votes_received.len();
            
            if votes > total_nodes / 2 {
                // Become leader
                state.role = Role::Leader;
                state.leader_id = Some(self.config.node_id);
                
                // Initialize leader state
                let last_log = state.last_log_index();
                for &peer in &self.config.peers {
                    state.next_index.insert(peer, last_log + 1);
                    state.match_index.insert(peer, 0);
                }
                
                // Append no-op entry
                let noop = LogEntry {
                    term: state.current_term,
                    index: last_log + 1,
                    command: Command::Noop,
                };
                state.log.push(noop);
                
                state.last_heartbeat = Instant::now();
                drop(state);
                
                // Send immediate heartbeat
                self.send_heartbeats().await;
            }
        }
    }
    
    async fn handle_append_entries(
        &self,
        from: NodeId,
        term: Term,
        leader_id: NodeId,
        prev_log_index: LogIndex,
        prev_log_term: Term,
        entries: Vec<LogEntry>,
        leader_commit: LogIndex,
    ) {
        let mut state = self.state.write().await;
        
        // Update term if stale
        if term > state.current_term {
            state.current_term = term;
            state.voted_for = None;
            state.role = Role::Follower;
        }
        
        if term < state.current_term {
            let response = Message::AppendEntriesResponse {
                term: state.current_term,
                success: false,
                match_index: 0,
            };
            drop(state);
            let _ = self.outbound_tx.send((from, response)).await;
            return;
        }
        
        // Valid leader - reset election timer
        state.last_heartbeat = Instant::now();
        state.leader_id = Some(leader_id);
        state.role = Role::Follower;
        
        // Check log consistency
        let log_ok = prev_log_index == 0 
            || (prev_log_index <= state.last_log_index() 
                && state.term_at(prev_log_index) == prev_log_term);
        
        if !log_ok {
            let response = Message::AppendEntriesResponse {
                term: state.current_term,
                success: false,
                match_index: state.last_log_index(),
            };
            drop(state);
            let _ = self.outbound_tx.send((from, response)).await;
            return;
        }
        
        // Append entries
        for entry in entries {
            if entry.index <= state.log.len() as LogIndex {
                // Check for conflict
                if let Some(existing) = state.get_entry(entry.index) {
                    if existing.term != entry.term {
                        // Truncate conflicting entries
                        state.log.truncate((entry.index - 1) as usize);
                        state.log.push(entry);
                    }
                }
            } else {
                state.log.push(entry);
            }
        }
        
        // Update commit index
        if leader_commit > state.commit_index {
            state.commit_index = std::cmp::min(leader_commit, state.last_log_index());
        }
        
        let match_index = state.last_log_index();
        let response = Message::AppendEntriesResponse {
            term: state.current_term,
            success: true,
            match_index,
        };
        
        // Apply committed entries
        self.apply_committed(&mut state).await;
        
        drop(state);
        let _ = self.outbound_tx.send((from, response)).await;
    }
    
    async fn handle_append_response(
        &self,
        from: NodeId,
        term: Term,
        success: bool,
        match_index: LogIndex,
    ) {
        let mut state = self.state.write().await;
        
        if term > state.current_term {
            state.current_term = term;
            state.voted_for = None;
            state.role = Role::Follower;
            return;
        }
        
        if state.role != Role::Leader {
            return;
        }
        
        if success {
            state.match_index.insert(from, match_index);
            state.next_index.insert(from, match_index + 1);
            
            // Try to advance commit index
            self.try_advance_commit(&mut state).await;
        } else {
            // Decrement next_index and retry
            let next = state.next_index.entry(from).or_insert(1);
            *next = next.saturating_sub(1).max(1);
        }
    }
    
    async fn try_advance_commit(&self, state: &mut ConsensusState) {
        // Find the highest index replicated on a majority
        let mut indices: Vec<LogIndex> = state.match_index.values().copied().collect();
        indices.push(state.last_log_index()); // Leader's own log
        indices.sort_unstable();
        
        let majority_idx = indices.len() / 2;
        let potential_commit = indices[majority_idx];
        
        // Only commit entries from current term
        if potential_commit > state.commit_index 
            && state.term_at(potential_commit) == state.current_term 
        {
            state.commit_index = potential_commit;
            self.apply_committed(state).await;
        }
    }
    
    async fn apply_committed(&self, state: &mut ConsensusState) {
        while state.last_applied < state.commit_index {
            state.last_applied += 1;
            if let Some(entry) = state.get_entry(state.last_applied).cloned() {
                let _ = self.commit_tx.send(entry).await;
            }
        }
    }
    
    // ========================================================================
    // Client API
    // ========================================================================
    
    /// Propose a command to the cluster. Returns error if not leader.
    pub async fn propose(&self, command: Command) -> Result<LogIndex, &'static str> {
        let mut state = self.state.write().await;
        
        if state.role != Role::Leader {
            return Err("not leader");
        }
        
        let index = state.last_log_index() + 1;
        let entry = LogEntry {
            term: state.current_term,
            index,
            command,
        };
        
        state.log.push(entry);
        drop(state);
        
        // Trigger immediate replication
        self.send_heartbeats().await;
        
        Ok(index)
    }
    
    /// Check if this node is the leader.
    pub async fn is_leader(&self) -> bool {
        self.state.read().await.role == Role::Leader
    }
    
    /// Get the current leader ID.
    pub async fn leader_id(&self) -> Option<NodeId> {
        self.state.read().await.leader_id
    }
    
    /// Get current state snapshot.
    pub async fn get_state(&self) -> (Role, Term, LogIndex) {
        let state = self.state.read().await;
        (state.role.clone(), state.current_term, state.commit_index)
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_leader_election() {
        let (outbound_tx, _outbound_rx) = mpsc::channel(100);
        let (commit_tx, _commit_rx) = mpsc::channel(100);
        
        let config = ConsensusConfig {
            node_id: 1,
            peers: HashSet::from([2, 3]),
            election_timeout_min: Duration::from_millis(50),
            election_timeout_max: Duration::from_millis(100),
            heartbeat_interval: Duration::from_millis(25),
        };
        
        let node = Arc::new(ConsensusNode::new(config, outbound_tx, commit_tx));
        
        // Initially a follower
        assert_eq!(node.state.read().await.role, Role::Follower);
        
        // After starting election
        node.start_election().await;
        assert_eq!(node.state.read().await.role, Role::Candidate);
        assert_eq!(node.state.read().await.current_term, 1);
    }
    
    #[tokio::test]
    async fn test_log_replication() {
        let (outbound_tx, _outbound_rx) = mpsc::channel(100);
        let (commit_tx, _commit_rx) = mpsc::channel(100);
        
        let config = ConsensusConfig {
            node_id: 1,
            peers: HashSet::new(), // Single node cluster
            ..Default::default()
        };
        
        let node = Arc::new(ConsensusNode::new(config, outbound_tx, commit_tx));
        
        // Become leader (single node = instant majority)
        node.start_election().await;
        
        {
            let mut state = node.state.write().await;
            state.role = Role::Leader;
            state.votes_received.insert(1);
        }
        
        // Propose a command
        let result = node.propose(Command::Data(vec![1, 2, 3])).await;
        assert!(result.is_ok());
    }
}
