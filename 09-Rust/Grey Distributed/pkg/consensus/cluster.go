// Package consensus - Cluster configuration and membership management.
//
// This file implements dynamic cluster membership changes using joint consensus,
// the safe approach described in the Raft paper.
//
// # The Problem with Single-Step Membership Changes
//
// If we change from configuration C_old to C_new in a single step, there can
// be a moment where two disjoint majorities exist, allowing two leaders to
// be elected simultaneously. This violates Raft's safety guarantees.
//
// Example: Going from {A, B, C} to {A, B, C, D, E}
// - Old majority: 2 of 3
// - New majority: 3 of 5
// - {A, B} could form old majority while {C, D, E} forms new majority
//
// # Joint Consensus Solution
//
// We use a two-phase approach:
// 1. First, enter joint configuration C_old,new where decisions require
//    majorities from BOTH configurations
// 2. Then, move to C_new where only the new configuration matters
//
// This ensures there's no point where two independent majorities can exist.
package consensus

import (
	"sync"

	"github.com/grey-systems/grey-distributed/pkg/types"
)

// ClusterConfig represents the current cluster membership.
type ClusterConfig struct {
	mu sync.RWMutex
	
	// current is the primary configuration.
	current *Configuration
	
	// joint is non-nil during a configuration change.
	// When set, decisions require majority from BOTH current AND joint.
	joint *Configuration
	
	// pendingChange tracks an in-progress configuration change.
	pendingChange *ConfigurationChange
}

// Configuration represents a set of cluster members.
type Configuration struct {
	// Voters participate in leader election and log commitment.
	Voters map[types.NodeID]bool
	
	// Learners receive log entries but don't vote.
	// Useful for catching up new nodes before promoting them to voters.
	Learners map[types.NodeID]bool
	
	// Witnesses participate in quorum but store minimal state.
	// Used for tie-breaking in even-numbered clusters.
	Witnesses map[types.NodeID]bool
}

// ConfigurationChange represents an in-progress membership change.
type ConfigurationChange struct {
	// Type of change being performed.
	Type ConfigChangeType
	
	// NodeID being added or removed.
	NodeID types.NodeID
	
	// Role the node should have after the change.
	Role NodeRole
	
	// LogIndex where this change was proposed.
	LogIndex types.LogIndex
}

// ConfigChangeType identifies the type of membership change.
type ConfigChangeType int

const (
	ConfigChangeAddVoter ConfigChangeType = iota
	ConfigChangeRemoveVoter
	ConfigChangeAddLearner
	ConfigChangePromoteLearner
	ConfigChangeAddWitness
	ConfigChangeRemoveNode
)

// NodeRole identifies a node's role in the cluster.
type NodeRole int

const (
	NodeRoleVoter NodeRole = iota
	NodeRoleLearner
	NodeRoleWitness
)

// NewClusterConfig creates an empty cluster configuration.
func NewClusterConfig() *ClusterConfig {
	return &ClusterConfig{
		current: &Configuration{
			Voters:    make(map[types.NodeID]bool),
			Learners:  make(map[types.NodeID]bool),
			Witnesses: make(map[types.NodeID]bool),
		},
	}
}

// BootstrapCluster initializes a new cluster with the given nodes as voters.
// This should only be called once when forming a new cluster.
func (c *ClusterConfig) BootstrapCluster(nodes []types.NodeID) {
	c.mu.Lock()
	defer c.mu.Unlock()
	
	c.current = &Configuration{
		Voters:    make(map[types.NodeID]bool),
		Learners:  make(map[types.NodeID]bool),
		Witnesses: make(map[types.NodeID]bool),
	}
	
	for _, node := range nodes {
		c.current.Voters[node] = true
	}
}

// VotingMembers returns all nodes that can vote in elections.
func (c *ClusterConfig) VotingMembers() []types.NodeID {
	c.mu.RLock()
	defer c.mu.RUnlock()
	
	// During joint consensus, voting members come from both configurations
	members := make(map[types.NodeID]bool)
	
	for node := range c.current.Voters {
		members[node] = true
	}
	
	if c.joint != nil {
		for node := range c.joint.Voters {
			members[node] = true
		}
	}
	
	result := make([]types.NodeID, 0, len(members))
	for node := range members {
		result = append(result, node)
	}
	
	return result
}

// AllMembers returns all nodes in the cluster (voters, learners, witnesses).
func (c *ClusterConfig) AllMembers() []types.NodeID {
	c.mu.RLock()
	defer c.mu.RUnlock()
	
	members := make(map[types.NodeID]bool)
	
	addFromConfig := func(cfg *Configuration) {
		for node := range cfg.Voters {
			members[node] = true
		}
		for node := range cfg.Learners {
			members[node] = true
		}
		for node := range cfg.Witnesses {
			members[node] = true
		}
	}
	
	addFromConfig(c.current)
	if c.joint != nil {
		addFromConfig(c.joint)
	}
	
	result := make([]types.NodeID, 0, len(members))
	for node := range members {
		result = append(result, node)
	}
	
	return result
}

// QuorumSize returns the number of votes needed for a majority.
//
// During joint consensus, we need a majority from BOTH configurations.
// This is handled by HasQuorum, not by adjusting QuorumSize.
func (c *ClusterConfig) QuorumSize() int {
	c.mu.RLock()
	defer c.mu.RUnlock()
	
	voterCount := len(c.current.Voters) + len(c.current.Witnesses)
	return voterCount/2 + 1
}

// HasQuorum checks if the given set of nodes constitutes a quorum.
//
// During joint consensus, a quorum exists only if we have a majority
// in BOTH the current AND joint configurations.
func (c *ClusterConfig) HasQuorum(nodes map[types.NodeID]bool) bool {
	c.mu.RLock()
	defer c.mu.RUnlock()
	
	// Check current configuration
	if !c.hasQuorumInConfig(c.current, nodes) {
		return false
	}
	
	// During joint consensus, also check joint configuration
	if c.joint != nil {
		if !c.hasQuorumInConfig(c.joint, nodes) {
			return false
		}
	}
	
	return true
}

// hasQuorumInConfig checks if nodes form a majority in the given configuration.
func (c *ClusterConfig) hasQuorumInConfig(cfg *Configuration, nodes map[types.NodeID]bool) bool {
	total := len(cfg.Voters) + len(cfg.Witnesses)
	needed := total/2 + 1
	
	count := 0
	for node := range nodes {
		if cfg.Voters[node] || cfg.Witnesses[node] {
			count++
		}
	}
	
	return count >= needed
}

// IsInJointConsensus returns true if a configuration change is in progress.
func (c *ClusterConfig) IsInJointConsensus() bool {
	c.mu.RLock()
	defer c.mu.RUnlock()
	
	return c.joint != nil
}

// ProposeChange initiates a configuration change.
// Returns an error if a change is already in progress.
//
// The change goes through two phases:
// 1. Enter joint consensus (C_old,new)
// 2. Exit to new configuration (C_new)
//
// Each phase requires the new configuration to be replicated to a majority.
func (c *ClusterConfig) ProposeChange(change ConfigurationChange) error {
	c.mu.Lock()
	defer c.mu.Unlock()
	
	if c.joint != nil {
		return ErrChangeInProgress
	}
	
	// Create the joint configuration
	c.joint = c.createJointConfig(change)
	c.pendingChange = &change
	
	return nil
}

// createJointConfig creates the joint configuration for the given change.
func (c *ClusterConfig) createJointConfig(change ConfigurationChange) *Configuration {
	joint := &Configuration{
		Voters:    make(map[types.NodeID]bool),
		Learners:  make(map[types.NodeID]bool),
		Witnesses: make(map[types.NodeID]bool),
	}
	
	// Copy current configuration
	for node := range c.current.Voters {
		joint.Voters[node] = true
	}
	for node := range c.current.Learners {
		joint.Learners[node] = true
	}
	for node := range c.current.Witnesses {
		joint.Witnesses[node] = true
	}
	
	// Apply the change
	switch change.Type {
	case ConfigChangeAddVoter:
		joint.Voters[change.NodeID] = true
		delete(joint.Learners, change.NodeID)
		
	case ConfigChangeRemoveVoter:
		delete(joint.Voters, change.NodeID)
		
	case ConfigChangeAddLearner:
		joint.Learners[change.NodeID] = true
		
	case ConfigChangePromoteLearner:
		delete(joint.Learners, change.NodeID)
		joint.Voters[change.NodeID] = true
		
	case ConfigChangeAddWitness:
		joint.Witnesses[change.NodeID] = true
		
	case ConfigChangeRemoveNode:
		delete(joint.Voters, change.NodeID)
		delete(joint.Learners, change.NodeID)
		delete(joint.Witnesses, change.NodeID)
	}
	
	return joint
}

// CommitJointPhase transitions from joint consensus to the new configuration.
// Called when the joint configuration entry is committed.
func (c *ClusterConfig) CommitJointPhase() {
	c.mu.Lock()
	defer c.mu.Unlock()
	
	if c.joint == nil {
		return
	}
	
	// The joint configuration becomes the new current configuration
	c.current = c.joint
	c.joint = nil
	c.pendingChange = nil
}

// AbortChange aborts an in-progress configuration change.
// This should only be called if the change entry fails to commit.
func (c *ClusterConfig) AbortChange() {
	c.mu.Lock()
	defer c.mu.Unlock()
	
	c.joint = nil
	c.pendingChange = nil
}

// IsVoter returns true if the node is a voter in the current configuration.
func (c *ClusterConfig) IsVoter(nodeID types.NodeID) bool {
	c.mu.RLock()
	defer c.mu.RUnlock()
	
	return c.current.Voters[nodeID] || (c.joint != nil && c.joint.Voters[nodeID])
}

// IsLearner returns true if the node is a learner.
func (c *ClusterConfig) IsLearner(nodeID types.NodeID) bool {
	c.mu.RLock()
	defer c.mu.RUnlock()
	
	return c.current.Learners[nodeID] || (c.joint != nil && c.joint.Learners[nodeID])
}

// IsWitness returns true if the node is a witness.
func (c *ClusterConfig) IsWitness(nodeID types.NodeID) bool {
	c.mu.RLock()
	defer c.mu.RUnlock()
	
	return c.current.Witnesses[nodeID] || (c.joint != nil && c.joint.Witnesses[nodeID])
}

// Serialize returns a serialized representation of the cluster configuration.
// Used for snapshotting and replication.
func (c *ClusterConfig) Serialize() []byte {
	c.mu.RLock()
	defer c.mu.RUnlock()
	
	// TODO: Implement serialization using MessagePack
	return nil
}

// Deserialize restores the cluster configuration from serialized data.
func (c *ClusterConfig) Deserialize(data []byte) error {
	c.mu.Lock()
	defer c.mu.Unlock()
	
	// TODO: Implement deserialization
	return nil
}

// =============================================================================
// ERRORS
// =============================================================================

var (
	ErrChangeInProgress = ConfigError("configuration change already in progress")
	ErrNodeNotFound     = ConfigError("node not found in configuration")
	ErrInvalidChange    = ConfigError("invalid configuration change")
)

// ConfigError is a configuration-related error.
type ConfigError string

func (e ConfigError) Error() string {
	return string(e)
}
