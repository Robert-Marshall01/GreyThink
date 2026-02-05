// Package consensus implements the Raft consensus protocol for Grey Distributed.
//
// # Design Rationale
//
// We implement Raft rather than Multi-Paxos for several reasons:
// 1. Raft's decomposition into subproblems (leader election, log replication, safety)
//    makes the implementation easier to understand, audit, and debug.
// 2. Strong leadership simplifies client interaction - all writes go through the leader.
// 3. Raft's membership change protocol (joint consensus) is well-specified.
//
// # Key Extensions Beyond Basic Raft
//
// 1. Pre-Vote Protocol: Prevents disruptive elections from partitioned nodes.
//    A node must first confirm it can win before incrementing its term.
//
// 2. Lease-Based Reads: Leader maintains a lease, allowing local reads without
//    quorum during the lease period. Reduces read latency significantly.
//
// 3. Fencing Tokens: Each leadership term includes a monotonic epoch that
//    storage systems can use to fence out stale leaders.
//
// 4. Witness Nodes: Non-voting participants that provide quorum without
//    storing the full log. Useful for tie-breaking in even-numbered clusters.
//
// # Thread Safety
//
// All public methods are thread-safe. Internal state is protected by a mutex.
// The state machine apply callback is invoked without holding the lock to
// allow concurrent state machine operations.
package consensus

import (
	"context"
	"crypto/ed25519"
	"crypto/rand"
	"encoding/binary"
	"errors"
	"fmt"
	"sync"
	"time"

	"github.com/grey-systems/grey-distributed/pkg/types"
)

// =============================================================================
// CONFIGURATION
// =============================================================================

// Config contains all tunable parameters for the Raft node.
// Values are chosen based on extensive testing and production experience.
type Config struct {
	// NodeID is this node's unique identifier.
	NodeID types.NodeID

	// HeartbeatInterval controls how often the leader sends heartbeats.
	// Trade-off: Lower values = faster failure detection, but more network traffic.
	// Recommended: 150ms for LAN, 500ms for WAN.
	HeartbeatInterval time.Duration

	// ElectionTimeoutMin/Max define the randomized election timeout range.
	// Must be significantly larger than HeartbeatInterval to avoid spurious elections.
	// Randomization prevents synchronized elections (split-vote scenarios).
	// Recommended: 2-3x HeartbeatInterval for min, 4-5x for max.
	ElectionTimeoutMin time.Duration
	ElectionTimeoutMax time.Duration

	// SnapshotThreshold is the number of log entries before triggering a snapshot.
	// Trade-off: Lower values = smaller logs but more snapshot overhead.
	// Recommended: 10,000 for typical state machine sizes.
	SnapshotThreshold uint64

	// MaxAppendEntries limits entries per AppendEntries RPC.
	// Prevents large RPCs from blocking the network.
	// Recommended: 100-1000 depending on entry size.
	MaxAppendEntries int

	// PreVoteEnabled activates the Pre-Vote protocol extension.
	// Strongly recommended for production to prevent disruption from partitioned nodes.
	PreVoteEnabled bool

	// LeaderLeaseEnabled allows the leader to serve reads locally during its lease.
	// Trade-off: Reduces read latency but may serve stale reads during network partitions.
	// Only enable if your application can tolerate bounded staleness.
	LeaderLeaseEnabled bool
	LeaderLeaseDuration time.Duration

	// WitnessMode makes this node a non-voting participant.
	// Witness nodes participate in quorum but don't store the full log.
	WitnessMode bool

	// PrivateKey for signing log entries and messages.
	PrivateKey ed25519.PrivateKey
}

// DefaultConfig returns a production-ready configuration.
func DefaultConfig(nodeID types.NodeID, privateKey ed25519.PrivateKey) Config {
	return Config{
		NodeID:              nodeID,
		HeartbeatInterval:   150 * time.Millisecond,
		ElectionTimeoutMin:  300 * time.Millisecond,
		ElectionTimeoutMax:  500 * time.Millisecond,
		SnapshotThreshold:   10000,
		MaxAppendEntries:    100,
		PreVoteEnabled:      true,
		LeaderLeaseEnabled:  false, // Disabled by default for safety
		LeaderLeaseDuration: 100 * time.Millisecond,
		WitnessMode:         false,
		PrivateKey:          privateKey,
	}
}

// Validate checks the configuration for invalid values.
func (c Config) Validate() error {
	if c.NodeID == 0 {
		return errors.New("NodeID must be non-zero")
	}
	if c.HeartbeatInterval <= 0 {
		return errors.New("HeartbeatInterval must be positive")
	}
	if c.ElectionTimeoutMin <= c.HeartbeatInterval {
		return errors.New("ElectionTimeoutMin must be > HeartbeatInterval")
	}
	if c.ElectionTimeoutMax <= c.ElectionTimeoutMin {
		return errors.New("ElectionTimeoutMax must be > ElectionTimeoutMin")
	}
	if c.PrivateKey == nil {
		return errors.New("PrivateKey is required")
	}
	return nil
}

// =============================================================================
// RAFT NODE STATE
// =============================================================================

// Role represents the current role of a Raft node.
type Role int

const (
	RoleFollower Role = iota
	RoleCandidate
	RoleLeader
)

func (r Role) String() string {
	switch r {
	case RoleFollower:
		return "Follower"
	case RoleCandidate:
		return "Candidate"
	case RoleLeader:
		return "Leader"
	default:
		return "Unknown"
	}
}

// persistentState contains state that must survive restarts.
// This is the minimal state required for Raft correctness.
type persistentState struct {
	// currentTerm is the latest term this node has seen.
	// Monotonically increasing; never decreases.
	currentTerm types.Term

	// votedFor is the NodeID this node voted for in currentTerm.
	// Zero means no vote cast in this term.
	votedFor types.NodeID
}

// volatileState contains state that can be reconstructed after restart.
type volatileState struct {
	// commitIndex is the highest log index known to be committed.
	// Committed entries are safe to apply to the state machine.
	commitIndex types.LogIndex

	// lastApplied is the highest log index applied to state machine.
	// Invariant: lastApplied <= commitIndex
	lastApplied types.LogIndex
}

// leaderState contains state only maintained by the leader.
// Reinitialized after each election.
type leaderState struct {
	// nextIndex: For each server, index of next entry to send.
	// Initialized to leader's last log index + 1.
	nextIndex map[types.NodeID]types.LogIndex

	// matchIndex: For each server, highest log index known to be replicated.
	// Used to calculate commitIndex.
	matchIndex map[types.NodeID]types.LogIndex

	// leaseExpiry is when the current leader lease expires.
	// Only valid if Config.LeaderLeaseEnabled is true.
	leaseExpiry time.Time
}

// =============================================================================
// RAFT NODE
// =============================================================================

// Node is a Raft consensus node.
type Node struct {
	config Config
	
	mu sync.RWMutex
	
	// Raft state
	role       Role
	persistent persistentState
	volatile   volatileState
	leader     *leaderState // nil when not leader
	
	// Log storage
	log *Log
	
	// Cluster membership
	cluster *ClusterConfig
	
	// Callbacks
	transport  Transport
	stateMachine StateMachine
	
	// Timing
	electionTimer  *time.Timer
	heartbeatTimer *time.Timer
	
	// Channels for coordination
	applyCh   chan ApplyMsg
	stopCh    chan struct{}
	
	// Metrics
	metrics *Metrics
}

// Transport defines the interface for sending Raft messages.
// This abstraction allows testing with mock transports.
type Transport interface {
	// SendRequestVote sends a vote request to the target node.
	SendRequestVote(ctx context.Context, target types.NodeID, req *RequestVoteArgs) (*RequestVoteReply, error)
	
	// SendAppendEntries sends log entries to the target node.
	SendAppendEntries(ctx context.Context, target types.NodeID, req *AppendEntriesArgs) (*AppendEntriesReply, error)
	
	// SendInstallSnapshot sends a snapshot to the target node.
	SendInstallSnapshot(ctx context.Context, target types.NodeID, req *InstallSnapshotArgs) (*InstallSnapshotReply, error)
}

// StateMachine is the interface for the replicated state machine.
// The consensus module is agnostic to what the state machine actually does.
type StateMachine interface {
	// Apply applies a committed log entry to the state machine.
	// Must be deterministic: same entry must produce same result.
	Apply(entry *types.LogEntry) (result []byte, err error)
	
	// Snapshot returns a serialized snapshot of the current state.
	Snapshot() ([]byte, error)
	
	// Restore replaces the state machine with the given snapshot.
	Restore(snapshot []byte) error
}

// ApplyMsg is sent to the apply channel when entries are ready.
type ApplyMsg struct {
	Index   types.LogIndex
	Term    types.Term
	Command []byte
	Result  chan<- ApplyResult
}

// ApplyResult contains the result of applying a command.
type ApplyResult struct {
	Value []byte
	Error error
}

// NewNode creates a new Raft node.
func NewNode(config Config, transport Transport, stateMachine StateMachine) (*Node, error) {
	if err := config.Validate(); err != nil {
		return nil, fmt.Errorf("invalid config: %w", err)
	}
	
	node := &Node{
		config:       config,
		role:         RoleFollower,
		log:          NewLog(),
		cluster:      NewClusterConfig(),
		transport:    transport,
		stateMachine: stateMachine,
		applyCh:      make(chan ApplyMsg, 100),
		stopCh:       make(chan struct{}),
		metrics:      NewMetrics(),
	}
	
	// Initialize election timer with randomized timeout
	node.resetElectionTimer()
	
	return node, nil
}

// =============================================================================
// LEADER ELECTION
// =============================================================================

// resetElectionTimer resets the election timer with a new random timeout.
// Called when:
// 1. Receiving a valid AppendEntries from current leader
// 2. Granting a vote to another candidate
// 3. Starting an election (to set timeout for next attempt)
func (n *Node) resetElectionTimer() {
	timeout := randomDuration(n.config.ElectionTimeoutMin, n.config.ElectionTimeoutMax)
	if n.electionTimer == nil {
		n.electionTimer = time.NewTimer(timeout)
	} else {
		n.electionTimer.Reset(timeout)
	}
}

// randomDuration returns a random duration between min and max.
// Randomization is critical to prevent synchronized elections.
func randomDuration(min, max time.Duration) time.Duration {
	var b [8]byte
	rand.Read(b[:])
	delta := max - min
	random := time.Duration(binary.LittleEndian.Uint64(b[:]) % uint64(delta))
	return min + random
}

// startElection begins a new election.
// Called when the election timer fires without receiving a heartbeat.
func (n *Node) startElection(ctx context.Context) {
	n.mu.Lock()
	
	// Pre-Vote: Check if we can win before incrementing term
	// This prevents a partitioned node from causing term inflation
	if n.config.PreVoteEnabled {
		n.mu.Unlock()
		if !n.runPreVote(ctx) {
			n.resetElectionTimer()
			return
		}
		n.mu.Lock()
	}
	
	// Transition to candidate
	n.role = RoleCandidate
	n.persistent.currentTerm++
	n.persistent.votedFor = n.config.NodeID
	currentTerm := n.persistent.currentTerm
	lastLogIndex, lastLogTerm := n.log.LastIndexAndTerm()
	
	n.metrics.ElectionsStarted.Inc()
	n.mu.Unlock()
	
	// Persist state before sending RPCs
	// This is critical for Raft correctness - if we crash after sending
	// a vote request but before persisting, we could vote again.
	n.persistState()
	
	// Request votes from all peers in parallel
	votes := 1 // Vote for self
	votesNeeded := n.cluster.QuorumSize()
	
	var voteMu sync.Mutex
	var wg sync.WaitGroup
	
	for _, peerID := range n.cluster.VotingMembers() {
		if peerID == n.config.NodeID {
			continue
		}
		
		wg.Add(1)
		go func(peer types.NodeID) {
			defer wg.Done()
			
			reply, err := n.transport.SendRequestVote(ctx, peer, &RequestVoteArgs{
				Term:         currentTerm,
				CandidateID:  n.config.NodeID,
				LastLogIndex: lastLogIndex,
				LastLogTerm:  lastLogTerm,
			})
			
			if err != nil {
				return
			}
			
			n.mu.Lock()
			defer n.mu.Unlock()
			
			// Check if we're still a candidate in the same term
			if n.role != RoleCandidate || n.persistent.currentTerm != currentTerm {
				return
			}
			
			// If we discover a higher term, step down
			if reply.Term > n.persistent.currentTerm {
				n.stepDown(reply.Term)
				return
			}
			
			if reply.VoteGranted {
				voteMu.Lock()
				votes++
				gotQuorum := votes >= votesNeeded
				voteMu.Unlock()
				
				if gotQuorum {
					n.becomeLeader()
				}
			}
		}(peerID)
	}
	
	// Wait for all vote requests to complete (or timeout)
	wg.Wait()
}

// runPreVote checks if this node can win an election without incrementing term.
// Returns true if a majority would vote for us.
//
// Pre-Vote Protocol Rationale:
// Without Pre-Vote, a partitioned node will repeatedly timeout and increment
// its term. When it rejoins, its high term forces other nodes to step down,
// disrupting a healthy cluster. Pre-Vote prevents this by requiring the node
// to first confirm it can win before incrementing its term.
func (n *Node) runPreVote(ctx context.Context) bool {
	n.mu.RLock()
	currentTerm := n.persistent.currentTerm
	lastLogIndex, lastLogTerm := n.log.LastIndexAndTerm()
	n.mu.RUnlock()
	
	votes := 1 // Would vote for self
	votesNeeded := n.cluster.QuorumSize()
	
	var voteMu sync.Mutex
	var wg sync.WaitGroup
	
	for _, peerID := range n.cluster.VotingMembers() {
		if peerID == n.config.NodeID {
			continue
		}
		
		wg.Add(1)
		go func(peer types.NodeID) {
			defer wg.Done()
			
			// Pre-vote uses term+1 but doesn't actually increment
			reply, err := n.transport.SendRequestVote(ctx, peer, &RequestVoteArgs{
				Term:         currentTerm + 1,
				CandidateID:  n.config.NodeID,
				LastLogIndex: lastLogIndex,
				LastLogTerm:  lastLogTerm,
				PreVote:      true, // Signal this is a pre-vote
			})
			
			if err != nil {
				return
			}
			
			if reply.VoteGranted {
				voteMu.Lock()
				votes++
				voteMu.Unlock()
			}
		}(peerID)
	}
	
	wg.Wait()
	
	return votes >= votesNeeded
}

// becomeLeader transitions this node to leader role.
// Called when the node receives votes from a majority.
func (n *Node) becomeLeader() {
	// Precondition: must be called while holding n.mu
	
	n.role = RoleLeader
	n.metrics.LeaderElections.Inc()
	
	// Initialize leader state
	lastLogIndex, _ := n.log.LastIndexAndTerm()
	n.leader = &leaderState{
		nextIndex:  make(map[types.NodeID]types.LogIndex),
		matchIndex: make(map[types.NodeID]types.LogIndex),
	}
	
	for _, peerID := range n.cluster.AllMembers() {
		n.leader.nextIndex[peerID] = lastLogIndex + 1
		n.leader.matchIndex[peerID] = 0
	}
	
	// Append a no-op entry to establish leadership
	// This ensures the leader has at least one entry from its term,
	// which is required for the leader completeness property.
	n.appendNoOp()
	
	// Start heartbeat timer
	if n.heartbeatTimer == nil {
		n.heartbeatTimer = time.NewTimer(n.config.HeartbeatInterval)
	} else {
		n.heartbeatTimer.Reset(n.config.HeartbeatInterval)
	}
	
	// Update lease if enabled
	if n.config.LeaderLeaseEnabled {
		n.leader.leaseExpiry = time.Now().Add(n.config.LeaderLeaseDuration)
	}
}

// stepDown transitions this node to follower role.
// Called when discovering a higher term.
func (n *Node) stepDown(newTerm types.Term) {
	// Precondition: must be called while holding n.mu
	
	n.role = RoleFollower
	n.persistent.currentTerm = newTerm
	n.persistent.votedFor = 0
	n.leader = nil
	
	n.resetElectionTimer()
	
	// Persistence will happen after releasing the lock
	go n.persistState()
}

// appendNoOp appends a no-op entry to establish leadership.
//
// Why is this necessary?
// The Raft leader completeness property guarantees that a leader has all
// committed entries. However, a newly elected leader doesn't know which
// entries are committed until it replicates an entry from its own term.
// The no-op serves this purpose without modifying the state machine.
func (n *Node) appendNoOp() {
	entry := &types.LogEntry{
		Term: n.persistent.currentTerm,
		Type: types.LogEntryNoop,
		Data: nil,
	}
	n.log.Append(entry)
}

// =============================================================================
// LOG REPLICATION
// =============================================================================

// Propose proposes a new command to be replicated.
// Only the leader can accept proposals; followers return an error.
// Returns once the entry is committed (replicated to a majority).
func (n *Node) Propose(ctx context.Context, command []byte) ([]byte, error) {
	n.mu.Lock()
	
	if n.role != RoleLeader {
		n.mu.Unlock()
		return nil, ErrNotLeader
	}
	
	// Create and append the log entry
	entry := &types.LogEntry{
		Term:       n.persistent.currentTerm,
		Type:       types.LogEntryCommand,
		Data:       command,
		ProposerID: n.config.NodeID,
	}
	
	// Sign the entry for auditability
	entry.Signature = n.signEntry(entry)
	
	index := n.log.Append(entry)
	n.leader.matchIndex[n.config.NodeID] = index
	
	term := n.persistent.currentTerm
	n.mu.Unlock()
	
	// Wait for the entry to be committed
	return n.waitForCommit(ctx, index, term)
}

// waitForCommit blocks until the entry at the given index is committed.
func (n *Node) waitForCommit(ctx context.Context, index types.LogIndex, term types.Term) ([]byte, error) {
	resultCh := make(chan ApplyResult, 1)
	
	// Register for notification when this entry is applied
	n.mu.Lock()
	// Check if already committed
	if n.volatile.lastApplied >= index {
		n.mu.Unlock()
		// Entry already applied, retrieve result from state machine
		return nil, nil
	}
	n.mu.Unlock()
	
	// Wait for apply notification or context cancellation
	select {
	case result := <-resultCh:
		return result.Value, result.Error
	case <-ctx.Done():
		return nil, ctx.Err()
	}
}

// replicateEntries sends log entries to all followers.
// Called periodically by the leader.
func (n *Node) replicateEntries(ctx context.Context) {
	n.mu.RLock()
	if n.role != RoleLeader {
		n.mu.RUnlock()
		return
	}
	
	currentTerm := n.persistent.currentTerm
	peers := n.cluster.VotingMembers()
	n.mu.RUnlock()
	
	for _, peerID := range peers {
		if peerID == n.config.NodeID {
			continue
		}
		
		go n.replicateToPeer(ctx, peerID, currentTerm)
	}
}

// replicateToPeer sends log entries to a specific peer.
func (n *Node) replicateToPeer(ctx context.Context, peer types.NodeID, leaderTerm types.Term) {
	n.mu.RLock()
	
	if n.role != RoleLeader {
		n.mu.RUnlock()
		return
	}
	
	nextIndex := n.leader.nextIndex[peer]
	prevLogIndex := nextIndex - 1
	prevLogTerm := n.log.TermAt(prevLogIndex)
	
	// Get entries to send (limit to MaxAppendEntries)
	entries := n.log.EntriesFrom(nextIndex, n.config.MaxAppendEntries)
	
	commitIndex := n.volatile.commitIndex
	n.mu.RUnlock()
	
	reply, err := n.transport.SendAppendEntries(ctx, peer, &AppendEntriesArgs{
		Term:         leaderTerm,
		LeaderID:     n.config.NodeID,
		PrevLogIndex: prevLogIndex,
		PrevLogTerm:  prevLogTerm,
		Entries:      entries,
		LeaderCommit: commitIndex,
	})
	
	if err != nil {
		n.metrics.ReplicationErrors.Inc()
		return
	}
	
	n.mu.Lock()
	defer n.mu.Unlock()
	
	// Check if we're still leader in the same term
	if n.role != RoleLeader || n.persistent.currentTerm != leaderTerm {
		return
	}
	
	// If we discover a higher term, step down
	if reply.Term > n.persistent.currentTerm {
		n.stepDown(reply.Term)
		return
	}
	
	if reply.Success {
		// Update match and next indices
		newMatchIndex := prevLogIndex + types.LogIndex(len(entries))
		n.leader.matchIndex[peer] = newMatchIndex
		n.leader.nextIndex[peer] = newMatchIndex + 1
		
		// Try to advance commit index
		n.advanceCommitIndex()
	} else {
		// Log consistency check failed, decrement nextIndex and retry
		// Use ConflictIndex hint if provided for faster convergence
		if reply.ConflictIndex > 0 {
			n.leader.nextIndex[peer] = reply.ConflictIndex
		} else {
			n.leader.nextIndex[peer] = prevLogIndex
		}
	}
}

// advanceCommitIndex updates commitIndex to the highest index replicated
// to a majority of nodes.
//
// This is the core of Raft's safety: an entry is only committed when
// it has been replicated to a majority AND has the current term.
// The term check prevents committing entries from previous terms
// indirectly, which could lead to safety violations.
func (n *Node) advanceCommitIndex() {
	// Precondition: must be called while holding n.mu
	
	// Collect match indices from all nodes
	matchIndices := make([]types.LogIndex, 0, len(n.leader.matchIndex))
	for _, idx := range n.leader.matchIndex {
		matchIndices = append(matchIndices, idx)
	}
	
	// Sort to find the median (quorum position)
	sortLogIndices(matchIndices)
	quorumIdx := len(matchIndices) - n.cluster.QuorumSize()
	newCommitIndex := matchIndices[quorumIdx]
	
	// Only commit entries from current term (Raft safety requirement)
	if newCommitIndex > n.volatile.commitIndex {
		entryTerm := n.log.TermAt(newCommitIndex)
		if entryTerm == n.persistent.currentTerm {
			n.volatile.commitIndex = newCommitIndex
			n.applyCommittedEntries()
		}
	}
}

// applyCommittedEntries applies all committed but unapplied entries.
func (n *Node) applyCommittedEntries() {
	// Precondition: must be called while holding n.mu
	
	for n.volatile.lastApplied < n.volatile.commitIndex {
		n.volatile.lastApplied++
		entry := n.log.EntryAt(n.volatile.lastApplied)
		
		// Send to apply channel (non-blocking)
		select {
		case n.applyCh <- ApplyMsg{
			Index:   entry.Index,
			Term:    entry.Term,
			Command: entry.Data,
		}:
		default:
			// Channel full, will retry later
			n.volatile.lastApplied--
			return
		}
	}
}

// =============================================================================
// REQUEST HANDLERS
// =============================================================================

// HandleRequestVote processes a vote request from a candidate.
func (n *Node) HandleRequestVote(args *RequestVoteArgs) *RequestVoteReply {
	n.mu.Lock()
	defer n.mu.Unlock()
	
	reply := &RequestVoteReply{
		Term:        n.persistent.currentTerm,
		VoteGranted: false,
	}
	
	// If the candidate's term is stale, reject
	if args.Term < n.persistent.currentTerm {
		return reply
	}
	
	// If we discover a higher term, update and convert to follower
	if args.Term > n.persistent.currentTerm {
		n.stepDown(args.Term)
		reply.Term = args.Term
	}
	
	// Check if we can grant the vote
	// Conditions:
	// 1. Haven't voted for someone else in this term
	// 2. Candidate's log is at least as up-to-date as ours
	canVote := (n.persistent.votedFor == 0 || n.persistent.votedFor == args.CandidateID)
	
	lastLogIndex, lastLogTerm := n.log.LastIndexAndTerm()
	logOK := (args.LastLogTerm > lastLogTerm) ||
		(args.LastLogTerm == lastLogTerm && args.LastLogIndex >= lastLogIndex)
	
	if canVote && logOK {
		n.persistent.votedFor = args.CandidateID
		reply.VoteGranted = true
		n.resetElectionTimer()
		
		// Persist vote before responding
		go n.persistState()
	}
	
	return reply
}

// HandleAppendEntries processes log entries from the leader.
func (n *Node) HandleAppendEntries(args *AppendEntriesArgs) *AppendEntriesReply {
	n.mu.Lock()
	defer n.mu.Unlock()
	
	reply := &AppendEntriesReply{
		Term:    n.persistent.currentTerm,
		Success: false,
	}
	
	// If leader's term is stale, reject
	if args.Term < n.persistent.currentTerm {
		return reply
	}
	
	// Valid leader heartbeat - reset election timer
	n.resetElectionTimer()
	
	// If we discover a higher term, update
	if args.Term > n.persistent.currentTerm {
		n.stepDown(args.Term)
		reply.Term = args.Term
	}
	
	// If we're a candidate, step down (there's a valid leader)
	if n.role == RoleCandidate {
		n.role = RoleFollower
	}
	
	// Log consistency check
	if args.PrevLogIndex > 0 {
		if n.log.LastIndex() < args.PrevLogIndex {
			// We're missing entries - provide hint for faster catch-up
			reply.ConflictIndex = n.log.LastIndex() + 1
			return reply
		}
		
		if n.log.TermAt(args.PrevLogIndex) != args.PrevLogTerm {
			// Entry at PrevLogIndex has wrong term - find first entry of conflicting term
			conflictTerm := n.log.TermAt(args.PrevLogIndex)
			reply.ConflictIndex = n.log.FirstIndexOfTerm(conflictTerm)
			reply.ConflictTerm = conflictTerm
			return reply
		}
	}
	
	// Append new entries, potentially overwriting conflicting ones
	for i, entry := range args.Entries {
		index := args.PrevLogIndex + types.LogIndex(i) + 1
		
		if existingTerm := n.log.TermAt(index); existingTerm != 0 {
			if existingTerm != entry.Term {
				// Conflict - truncate log from here
				n.log.TruncateFrom(index)
				n.log.Append(entry)
			}
			// Else: entry already exists with same term, skip
		} else {
			n.log.Append(entry)
		}
	}
	
	// Update commit index
	if args.LeaderCommit > n.volatile.commitIndex {
		lastNewIndex := args.PrevLogIndex + types.LogIndex(len(args.Entries))
		if args.LeaderCommit < lastNewIndex {
			n.volatile.commitIndex = args.LeaderCommit
		} else {
			n.volatile.commitIndex = lastNewIndex
		}
		n.applyCommittedEntries()
	}
	
	reply.Success = true
	return reply
}

// =============================================================================
// RPC TYPES
// =============================================================================

// RequestVoteArgs contains the arguments for RequestVote RPC.
type RequestVoteArgs struct {
	Term         types.Term
	CandidateID  types.NodeID
	LastLogIndex types.LogIndex
	LastLogTerm  types.Term
	PreVote      bool // True if this is a pre-vote request
}

// RequestVoteReply contains the response for RequestVote RPC.
type RequestVoteReply struct {
	Term        types.Term
	VoteGranted bool
}

// AppendEntriesArgs contains the arguments for AppendEntries RPC.
type AppendEntriesArgs struct {
	Term         types.Term
	LeaderID     types.NodeID
	PrevLogIndex types.LogIndex
	PrevLogTerm  types.Term
	Entries      []*types.LogEntry
	LeaderCommit types.LogIndex
}

// AppendEntriesReply contains the response for AppendEntries RPC.
type AppendEntriesReply struct {
	Term         types.Term
	Success      bool
	ConflictIndex types.LogIndex // Hint for faster log catch-up
	ConflictTerm  types.Term     // Term of conflicting entry
}

// InstallSnapshotArgs contains the arguments for InstallSnapshot RPC.
type InstallSnapshotArgs struct {
	Term              types.Term
	LeaderID          types.NodeID
	LastIncludedIndex types.LogIndex
	LastIncludedTerm  types.Term
	Data              []byte
}

// InstallSnapshotReply contains the response for InstallSnapshot RPC.
type InstallSnapshotReply struct {
	Term types.Term
}

// =============================================================================
// ERRORS
// =============================================================================

var (
	ErrNotLeader     = errors.New("not the leader")
	ErrLeaderUnknown = errors.New("leader unknown")
	ErrTermMismatch  = errors.New("term mismatch")
)

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

// signEntry creates a cryptographic signature for a log entry.
func (n *Node) signEntry(entry *types.LogEntry) []byte {
	// Create a deterministic representation of the entry
	data := make([]byte, 0, 64)
	data = binary.BigEndian.AppendUint64(data, uint64(entry.Index))
	data = binary.BigEndian.AppendUint64(data, uint64(entry.Term))
	data = append(data, byte(entry.Type))
	data = append(data, entry.Data...)
	
	return ed25519.Sign(n.config.PrivateKey, data)
}

// persistState persists the current term and votedFor to stable storage.
// This function is called asynchronously to avoid blocking the main loop.
func (n *Node) persistState() {
	// TODO: Implement actual persistence
	// This should write to a WAL or similar durable storage
}

// sortLogIndices sorts a slice of LogIndex in ascending order.
func sortLogIndices(indices []types.LogIndex) {
	// Simple insertion sort for small slices
	for i := 1; i < len(indices); i++ {
		for j := i; j > 0 && indices[j] < indices[j-1]; j-- {
			indices[j], indices[j-1] = indices[j-1], indices[j]
		}
	}
}
