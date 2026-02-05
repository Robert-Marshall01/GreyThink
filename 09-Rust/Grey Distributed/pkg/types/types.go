// Package types defines shared type definitions for Grey Distributed.
// These types form the foundation of the entire system and are designed
// for serialization efficiency, type safety, and clear semantics.
package types

import (
	"crypto/ed25519"
	"encoding/binary"
	"time"
)

// =============================================================================
// NODE IDENTITY
// =============================================================================

// NodeID uniquely identifies a node in the cluster.
// We use a 64-bit ID rather than UUIDs for:
// 1. Compact wire format (8 bytes vs 16)
// 2. Efficient map keys (single comparison)
// 3. Human-readable in logs when formatted as hex
type NodeID uint64

// NodeState represents the current operational state of a node.
// State transitions follow a strict state machine to prevent invalid states.
type NodeState int

const (
	// NodeStateUnknown is the zero value, indicating uninitialized state.
	NodeStateUnknown NodeState = iota

	// NodeStateBooting indicates the node is starting up and loading state.
	NodeStateBooting

	// NodeStateJoining indicates the node is attempting to join the cluster.
	NodeStateJoining

	// NodeStateActive indicates the node is fully operational.
	NodeStateActive

	// NodeStateDraining indicates the node is gracefully shutting down.
	// New work will not be scheduled, but existing work continues.
	NodeStateDraining

	// NodeStateQuarantined indicates the node is suspected faulty.
	// No work is scheduled; the node is under investigation.
	NodeStateQuarantined

	// NodeStateRemoved indicates the node has left the cluster.
	NodeStateRemoved
)

// NodeInfo contains metadata about a cluster node.
type NodeInfo struct {
	ID        NodeID
	Address   string    // host:port for Grey Protocol connections
	State     NodeState
	JoinedAt  time.Time
	LastSeen  time.Time
	
	// Capabilities advertised by this node
	Capabilities NodeCapabilities
	
	// Resources available on this node
	Resources ResourceCapacity
	
	// PublicKey for verifying signed messages from this node
	PublicKey ed25519.PublicKey
}

// NodeCapabilities describes what features a node supports.
// Used for capability-based routing and scheduling decisions.
type NodeCapabilities struct {
	// CanBeLeader indicates if this node can participate in leader election.
	// Witness nodes set this to false.
	CanBeLeader bool

	// CanStoreData indicates if this node can hold shard replicas.
	// Compute-only nodes set this to false.
	CanStoreData bool

	// HasGPU indicates if GPU workloads can be scheduled here.
	HasGPU bool

	// AvailabilityZone for rack-aware replica placement.
	AvailabilityZone string

	// Region for multi-region deployments.
	Region string
}

// =============================================================================
// RESOURCE TYPES
// =============================================================================

// ResourceCapacity represents the total resources available on a node.
// All values are in base units (cores, bytes) for precision.
type ResourceCapacity struct {
	// CPUMillicores: 1000 = 1 CPU core. Allows fractional allocation.
	CPUMillicores int64

	// MemoryBytes: Total RAM in bytes.
	MemoryBytes int64

	// DiskBytes: Total disk space in bytes.
	DiskBytes int64

	// NetworkBandwidthBps: Network capacity in bits per second.
	NetworkBandwidthBps int64
}

// ResourceUsage represents current resource consumption.
type ResourceUsage struct {
	CPUMillicores       int64
	MemoryBytes         int64
	DiskBytes           int64
	NetworkBandwidthBps int64
}

// Subtract returns the remaining capacity after usage.
func (c ResourceCapacity) Subtract(u ResourceUsage) ResourceCapacity {
	return ResourceCapacity{
		CPUMillicores:       c.CPUMillicores - u.CPUMillicores,
		MemoryBytes:         c.MemoryBytes - u.MemoryBytes,
		DiskBytes:           c.DiskBytes - u.DiskBytes,
		NetworkBandwidthBps: c.NetworkBandwidthBps - u.NetworkBandwidthBps,
	}
}

// CanFit returns true if the capacity can accommodate the request.
func (c ResourceCapacity) CanFit(request ResourceRequest) bool {
	return c.CPUMillicores >= request.CPUMillicores &&
		c.MemoryBytes >= request.MemoryBytes &&
		c.DiskBytes >= request.DiskBytes
}

// ResourceRequest specifies resources needed for a task.
type ResourceRequest struct {
	CPUMillicores int64
	MemoryBytes   int64
	DiskBytes     int64
}

// =============================================================================
// TASK TYPES
// =============================================================================

// TaskID uniquely identifies a task across the cluster.
// Format: [8 bytes epoch][8 bytes sequence]
// This allows chronological ordering and partitioning.
type TaskID [16]byte

// NewTaskID creates a TaskID from epoch and sequence number.
func NewTaskID(epoch uint64, sequence uint64) TaskID {
	var id TaskID
	binary.BigEndian.PutUint64(id[:8], epoch)
	binary.BigEndian.PutUint64(id[8:], sequence)
	return id
}

// Epoch extracts the epoch portion of the TaskID.
func (t TaskID) Epoch() uint64 {
	return binary.BigEndian.Uint64(t[:8])
}

// Sequence extracts the sequence portion of the TaskID.
func (t TaskID) Sequence() uint64 {
	return binary.BigEndian.Uint64(t[8:])
}

// TaskPriority determines scheduling order within the queue.
// Lower values = higher priority (processed first).
type TaskPriority int

const (
	// PriorityCritical: System-level tasks that must run immediately.
	// Examples: leader election, health checks, emergency compaction.
	PriorityCritical TaskPriority = 0

	// PriorityHigh: User-facing latency-sensitive workloads.
	// Examples: API requests, real-time processing.
	PriorityHigh TaskPriority = 100

	// PriorityNormal: Standard batch processing workloads.
	// Examples: ETL jobs, report generation.
	PriorityNormal TaskPriority = 200

	// PriorityLow: Background maintenance tasks.
	// Examples: compaction, garbage collection, metrics aggregation.
	PriorityLow TaskPriority = 300

	// PriorityPreemptible: Opportunistic workloads that can be killed.
	// Examples: speculative execution, cache warming.
	PriorityPreemptible TaskPriority = 400
)

// TaskState represents the lifecycle state of a task.
type TaskState int

const (
	TaskStatePending TaskState = iota
	TaskStateScheduled
	TaskStateRunning
	TaskStateCompleted
	TaskStateFailed
	TaskStateCancelled
	TaskStateRetrying
)

// Task represents a unit of work in the cluster.
type Task struct {
	ID        TaskID
	TenantID  TenantID
	Priority  TaskPriority
	State     TaskState
	
	// Payload is the serialized task definition.
	// Opaque to the scheduler; interpreted by the executor.
	Payload []byte
	
	// Resources required to run this task.
	Resources ResourceRequest
	
	// Constraints for scheduling (e.g., must run in specific zone).
	Constraints SchedulingConstraints
	
	// Metadata for tracking and debugging.
	SubmittedAt   time.Time
	ScheduledAt   time.Time
	StartedAt     time.Time
	CompletedAt   time.Time
	AssignedNode  NodeID
	
	// Execution tracking
	Attempt       int
	MaxAttempts   int
	LastError     string
}

// SchedulingConstraints limit where a task can be placed.
type SchedulingConstraints struct {
	// RequiredZones: Task must run in one of these zones.
	RequiredZones []string
	
	// ExcludedNodes: Task must not run on these nodes.
	ExcludedNodes []NodeID
	
	// Affinity: Prefer co-locating with tasks having these labels.
	Affinity map[string]string
	
	// AntiAffinity: Avoid co-locating with tasks having these labels.
	AntiAffinity map[string]string
}

// =============================================================================
// TENANT TYPES
// =============================================================================

// TenantID uniquely identifies a tenant in the multi-tenant system.
type TenantID uint64

// Tenant represents an isolated organizational unit.
type Tenant struct {
	ID   TenantID
	Name string
	
	// Quota defines resource limits for this tenant.
	Quota ResourceQuota
	
	// Settings for tenant-specific behavior.
	Settings TenantSettings
}

// ResourceQuota defines hard and soft limits for a tenant.
type ResourceQuota struct {
	// Hard limits that cannot be exceeded.
	MaxCPUMillicores int64
	MaxMemoryBytes   int64
	MaxDiskBytes     int64
	MaxTasks         int64
	
	// Soft limits that trigger warnings and throttling.
	WarnCPUMillicores int64
	WarnMemoryBytes   int64
}

// TenantSettings contains tenant-specific configuration.
type TenantSettings struct {
	// MaxTaskRetries before a task is marked as failed.
	MaxTaskRetries int
	
	// TaskTimeoutSeconds: Default timeout for tasks.
	TaskTimeoutSeconds int
	
	// AllowPreemption: Can this tenant's tasks be preempted?
	AllowPreemption bool
}

// =============================================================================
// CONSENSUS TYPES
// =============================================================================

// Term is a logical clock that increases with each election.
// Used to detect stale leaders and log entries.
type Term uint64

// LogIndex is the position of an entry in the Raft log.
// Starts at 1; 0 indicates no log entry.
type LogIndex uint64

// LogEntry represents a single entry in the Raft log.
type LogEntry struct {
	Index   LogIndex
	Term    Term
	Type    LogEntryType
	Data    []byte
	
	// Signature from the proposing node for auditability.
	ProposerID NodeID
	Signature  []byte
}

// LogEntryType distinguishes different log entry purposes.
type LogEntryType int

const (
	// LogEntryCommand: Normal state machine command.
	LogEntryCommand LogEntryType = iota
	
	// LogEntryConfiguration: Cluster membership change.
	LogEntryConfiguration
	
	// LogEntryNoop: Empty entry used for leader establishment.
	LogEntryNoop
)

// =============================================================================
// PROOF & AUDIT TYPES
// =============================================================================

// ProofBundle contains cryptographic evidence of task execution.
// Used for compliance, debugging, and dispute resolution.
type ProofBundle struct {
	TaskID        TaskID
	ExecutingNode NodeID
	
	// Hash of the input data.
	InputHash [32]byte
	
	// Hash of the output data.
	OutputHash [32]byte
	
	// StateTransitions: Ordered list of state changes during execution.
	StateTransitions []StateTransition
	
	// Timestamp when execution completed.
	CompletedAt time.Time
	
	// Signature from the executing node.
	Signature []byte
}

// StateTransition records a single state change.
type StateTransition struct {
	Timestamp   time.Time
	FromState   TaskState
	ToState     TaskState
	Reason      string
	
	// Optional: Vector clock for causal ordering.
	VectorClock map[NodeID]uint64
}

// =============================================================================
// NETWORK TYPES
// =============================================================================

// MessageType identifies the purpose of a Grey Protocol message.
type MessageType uint16

const (
	// Consensus messages
	MessageTypeVoteRequest MessageType = iota + 1
	MessageTypeVoteResponse
	MessageTypeAppendEntries
	MessageTypeAppendEntriesResponse
	MessageTypeInstallSnapshot
	MessageTypeInstallSnapshotResponse
	
	// Cluster management
	MessageTypeJoinRequest
	MessageTypeJoinResponse
	MessageTypeHeartbeat
	MessageTypeHeartbeatResponse
	
	// Task management
	MessageTypeTaskSubmit
	MessageTypeTaskStatus
	MessageTypeTaskResult
	
	// Data replication
	MessageTypeReplicateData
	MessageTypeReplicateAck
	MessageTypeAntiEntropySync
)

// Message is the base structure for all Grey Protocol messages.
type Message struct {
	Type      MessageType
	MessageID uint64
	SenderID  NodeID
	Term      Term
	Timestamp time.Time
	Payload   []byte
	
	// Flags for message handling.
	Flags MessageFlags
}

// MessageFlags control message processing behavior.
type MessageFlags uint16

const (
	// FlagCompressed indicates the payload is LZ4 compressed.
	FlagCompressed MessageFlags = 1 << 0
	
	// FlagEncrypted indicates the payload is encrypted.
	FlagEncrypted MessageFlags = 1 << 1
	
	// FlagHighPriority bypasses normal queue ordering.
	FlagHighPriority MessageFlags = 1 << 2
	
	// FlagIdempotent allows safe retry without deduplication.
	FlagIdempotent MessageFlags = 1 << 3
)
