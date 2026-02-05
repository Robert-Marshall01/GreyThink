// Package state implements event-sourced cluster state management.
//
// # Design Rationale
//
// Event sourcing stores all changes to state as a sequence of immutable events.
// Rather than storing the current state directly, we store the history of how
// we arrived at that state.
//
// Benefits:
// 1. Complete Audit Trail: Every state change is recorded for compliance.
// 2. Temporal Queries: "What was the state at time T?"
// 3. Debugging: Replay events to understand how we got into a bad state.
// 4. Natural Fit for Raft: Events map directly to Raft log entries.
// 5. Event-Driven Integration: Other systems can subscribe to events.
//
// Trade-offs:
// 1. Storage: Event logs grow unboundedly without compaction/snapshotting.
// 2. Rebuild Time: Replaying all events to reconstruct state can be slow.
// 3. Complexity: Event versioning and schema evolution is non-trivial.
//
// We mitigate these with periodic snapshots that capture materialized state.
package state

import (
	"encoding/binary"
	"errors"
	"sync"
	"time"

	"github.com/grey-systems/grey-distributed/pkg/types"
)

// =============================================================================
// EVENT TYPES
// =============================================================================

// EventType identifies the category of state change.
type EventType uint16

const (
	// Task lifecycle events
	EventTaskSubmitted EventType = iota + 1
	EventTaskScheduled
	EventTaskStarted
	EventTaskCompleted
	EventTaskFailed
	EventTaskCancelled
	EventTaskRetrying
	
	// Resource events
	EventResourceAllocated
	EventResourceReleased
	EventResourceQuotaUpdated
	
	// Node events
	EventNodeJoined
	EventNodeLeft
	EventNodeStateChanged
	
	// Tenant events
	EventTenantCreated
	EventTenantUpdated
	EventTenantDeleted
	
	// Configuration events
	EventConfigurationUpdated
)

// Event represents a single state change in the cluster.
// Events are immutable once created.
type Event struct {
	// ID is a unique, monotonically increasing identifier.
	ID uint64
	
	// Type identifies what kind of event this is.
	Type EventType
	
	// Timestamp when the event was created.
	Timestamp time.Time
	
	// CausalID links to the event that caused this one (if any).
	CausalID uint64
	
	// TenantID for multi-tenant isolation.
	TenantID types.TenantID
	
	// Payload contains the event-specific data.
	Payload []byte
	
	// Metadata for tracing and debugging.
	TraceID  [16]byte
	SpanID   [8]byte
	Proposer types.NodeID
}

// =============================================================================
// CLUSTER STATE
// =============================================================================

// ClusterState is the materialized view of all events.
// It's rebuilt by replaying events from the log.
type ClusterState struct {
	mu sync.RWMutex
	
	// Version is the event ID of the last applied event.
	// Used for consistency checks and cache invalidation.
	Version uint64
	
	// Tasks indexed by TaskID
	Tasks map[types.TaskID]*TaskState
	
	// Nodes indexed by NodeID
	Nodes map[types.NodeID]*NodeState
	
	// Resources tracks cluster-wide resource usage
	Resources *ResourceState
	
	// Tenants indexed by TenantID
	Tenants map[types.TenantID]*TenantState
	
	// Indexes for efficient queries
	tasksByTenant    map[types.TenantID]map[types.TaskID]bool
	tasksByNode      map[types.NodeID]map[types.TaskID]bool
	tasksByState     map[types.TaskState]map[types.TaskID]bool
	nodesByZone      map[string]map[types.NodeID]bool
}

// TaskState represents the current state of a task.
type TaskState struct {
	Task      types.Task
	Events    []uint64 // Event IDs that affected this task
	UpdatedAt time.Time
}

// NodeState represents the current state of a node.
type NodeState struct {
	Info        types.NodeInfo
	Allocated   types.ResourceUsage
	TaskCount   int
	LastEventID uint64
	UpdatedAt   time.Time
}

// ResourceState tracks cluster-wide resource usage.
type ResourceState struct {
	// Cluster totals
	TotalCapacity   types.ResourceCapacity
	TotalUsage      types.ResourceUsage
	
	// Per-tenant usage
	TenantUsage     map[types.TenantID]types.ResourceUsage
	
	// Per-node usage
	NodeUsage       map[types.NodeID]types.ResourceUsage
}

// TenantState represents the current state of a tenant.
type TenantState struct {
	Tenant    types.Tenant
	TaskCount int
	Usage     types.ResourceUsage
	UpdatedAt time.Time
}

// NewClusterState creates an empty cluster state.
func NewClusterState() *ClusterState {
	return &ClusterState{
		Tasks:   make(map[types.TaskID]*TaskState),
		Nodes:   make(map[types.NodeID]*NodeState),
		Tenants: make(map[types.TenantID]*TenantState),
		Resources: &ResourceState{
			TenantUsage: make(map[types.TenantID]types.ResourceUsage),
			NodeUsage:   make(map[types.NodeID]types.ResourceUsage),
		},
		tasksByTenant: make(map[types.TenantID]map[types.TaskID]bool),
		tasksByNode:   make(map[types.NodeID]map[types.TaskID]bool),
		tasksByState:  make(map[types.TaskState]map[types.TaskID]bool),
		nodesByZone:   make(map[string]map[types.NodeID]bool),
	}
}

// =============================================================================
// EVENT APPLICATION
// =============================================================================

// Apply applies an event to the cluster state.
// This must be deterministic: the same event always produces the same state.
func (s *ClusterState) Apply(event *Event) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	
	// Events must be applied in order
	if event.ID != s.Version+1 {
		return ErrEventOutOfOrder
	}
	
	var err error
	switch event.Type {
	case EventTaskSubmitted:
		err = s.applyTaskSubmitted(event)
	case EventTaskScheduled:
		err = s.applyTaskScheduled(event)
	case EventTaskStarted:
		err = s.applyTaskStarted(event)
	case EventTaskCompleted:
		err = s.applyTaskCompleted(event)
	case EventTaskFailed:
		err = s.applyTaskFailed(event)
	case EventTaskCancelled:
		err = s.applyTaskCancelled(event)
	case EventResourceAllocated:
		err = s.applyResourceAllocated(event)
	case EventResourceReleased:
		err = s.applyResourceReleased(event)
	case EventNodeJoined:
		err = s.applyNodeJoined(event)
	case EventNodeLeft:
		err = s.applyNodeLeft(event)
	case EventNodeStateChanged:
		err = s.applyNodeStateChanged(event)
	case EventTenantCreated:
		err = s.applyTenantCreated(event)
	default:
		err = ErrUnknownEventType
	}
	
	if err != nil {
		return err
	}
	
	s.Version = event.ID
	return nil
}

// =============================================================================
// TASK EVENT HANDLERS
// =============================================================================

// TaskSubmittedPayload is the payload for EventTaskSubmitted.
type TaskSubmittedPayload struct {
	TaskID    types.TaskID
	TenantID  types.TenantID
	Priority  types.TaskPriority
	Payload   []byte
	Resources types.ResourceRequest
}

func (s *ClusterState) applyTaskSubmitted(event *Event) error {
	var payload TaskSubmittedPayload
	if err := decodePayload(event.Payload, &payload); err != nil {
		return err
	}
	
	// Check if task already exists (idempotency)
	if _, exists := s.Tasks[payload.TaskID]; exists {
		return ErrTaskAlreadyExists
	}
	
	task := &TaskState{
		Task: types.Task{
			ID:          payload.TaskID,
			TenantID:    payload.TenantID,
			Priority:    payload.Priority,
			State:       types.TaskStatePending,
			Payload:     payload.Payload,
			Resources:   payload.Resources,
			SubmittedAt: event.Timestamp,
		},
		Events:    []uint64{event.ID},
		UpdatedAt: event.Timestamp,
	}
	
	s.Tasks[payload.TaskID] = task
	
	// Update indexes
	s.addToIndex(&s.tasksByTenant, payload.TenantID, payload.TaskID)
	s.addToTaskStateIndex(types.TaskStatePending, payload.TaskID)
	
	return nil
}

// TaskScheduledPayload is the payload for EventTaskScheduled.
type TaskScheduledPayload struct {
	TaskID     types.TaskID
	NodeID     types.NodeID
	ScheduleAt time.Time
}

func (s *ClusterState) applyTaskScheduled(event *Event) error {
	var payload TaskScheduledPayload
	if err := decodePayload(event.Payload, &payload); err != nil {
		return err
	}
	
	task, ok := s.Tasks[payload.TaskID]
	if !ok {
		return ErrTaskNotFound
	}
	
	// Update task state
	oldState := task.Task.State
	task.Task.State = types.TaskStateScheduled
	task.Task.AssignedNode = payload.NodeID
	task.Task.ScheduledAt = payload.ScheduleAt
	task.Events = append(task.Events, event.ID)
	task.UpdatedAt = event.Timestamp
	
	// Update indexes
	s.removeFromTaskStateIndex(oldState, payload.TaskID)
	s.addToTaskStateIndex(types.TaskStateScheduled, payload.TaskID)
	s.addToIndex(&s.tasksByNode, payload.NodeID, payload.TaskID)
	
	return nil
}

// TaskStartedPayload is the payload for EventTaskStarted.
type TaskStartedPayload struct {
	TaskID  types.TaskID
	NodeID  types.NodeID
}

func (s *ClusterState) applyTaskStarted(event *Event) error {
	var payload TaskStartedPayload
	if err := decodePayload(event.Payload, &payload); err != nil {
		return err
	}
	
	task, ok := s.Tasks[payload.TaskID]
	if !ok {
		return ErrTaskNotFound
	}
	
	oldState := task.Task.State
	task.Task.State = types.TaskStateRunning
	task.Task.StartedAt = event.Timestamp
	task.Events = append(task.Events, event.ID)
	task.UpdatedAt = event.Timestamp
	
	s.removeFromTaskStateIndex(oldState, payload.TaskID)
	s.addToTaskStateIndex(types.TaskStateRunning, payload.TaskID)
	
	return nil
}

// TaskCompletedPayload is the payload for EventTaskCompleted.
type TaskCompletedPayload struct {
	TaskID     types.TaskID
	ResultHash [32]byte
}

func (s *ClusterState) applyTaskCompleted(event *Event) error {
	var payload TaskCompletedPayload
	if err := decodePayload(event.Payload, &payload); err != nil {
		return err
	}
	
	task, ok := s.Tasks[payload.TaskID]
	if !ok {
		return ErrTaskNotFound
	}
	
	oldState := task.Task.State
	oldNode := task.Task.AssignedNode
	
	task.Task.State = types.TaskStateCompleted
	task.Task.CompletedAt = event.Timestamp
	task.Events = append(task.Events, event.ID)
	task.UpdatedAt = event.Timestamp
	
	s.removeFromTaskStateIndex(oldState, payload.TaskID)
	s.addToTaskStateIndex(types.TaskStateCompleted, payload.TaskID)
	s.removeFromIndex(&s.tasksByNode, oldNode, payload.TaskID)
	
	return nil
}

// TaskFailedPayload is the payload for EventTaskFailed.
type TaskFailedPayload struct {
	TaskID  types.TaskID
	Error   string
	Attempt int
}

func (s *ClusterState) applyTaskFailed(event *Event) error {
	var payload TaskFailedPayload
	if err := decodePayload(event.Payload, &payload); err != nil {
		return err
	}
	
	task, ok := s.Tasks[payload.TaskID]
	if !ok {
		return ErrTaskNotFound
	}
	
	oldState := task.Task.State
	task.Task.State = types.TaskStateFailed
	task.Task.LastError = payload.Error
	task.Task.Attempt = payload.Attempt
	task.Events = append(task.Events, event.ID)
	task.UpdatedAt = event.Timestamp
	
	s.removeFromTaskStateIndex(oldState, payload.TaskID)
	s.addToTaskStateIndex(types.TaskStateFailed, payload.TaskID)
	
	return nil
}

func (s *ClusterState) applyTaskCancelled(event *Event) error {
	var payload struct {
		TaskID types.TaskID
		Reason string
	}
	if err := decodePayload(event.Payload, &payload); err != nil {
		return err
	}
	
	task, ok := s.Tasks[payload.TaskID]
	if !ok {
		return ErrTaskNotFound
	}
	
	oldState := task.Task.State
	task.Task.State = types.TaskStateCancelled
	task.Events = append(task.Events, event.ID)
	task.UpdatedAt = event.Timestamp
	
	s.removeFromTaskStateIndex(oldState, payload.TaskID)
	s.addToTaskStateIndex(types.TaskStateCancelled, payload.TaskID)
	
	return nil
}

// =============================================================================
// RESOURCE EVENT HANDLERS
// =============================================================================

func (s *ClusterState) applyResourceAllocated(event *Event) error {
	var payload struct {
		TaskID   types.TaskID
		NodeID   types.NodeID
		TenantID types.TenantID
		CPU      int64
		Memory   int64
	}
	if err := decodePayload(event.Payload, &payload); err != nil {
		return err
	}
	
	// Update node usage
	nodeUsage := s.Resources.NodeUsage[payload.NodeID]
	nodeUsage.CPUMillicores += payload.CPU
	nodeUsage.MemoryBytes += payload.Memory
	s.Resources.NodeUsage[payload.NodeID] = nodeUsage
	
	// Update tenant usage
	tenantUsage := s.Resources.TenantUsage[payload.TenantID]
	tenantUsage.CPUMillicores += payload.CPU
	tenantUsage.MemoryBytes += payload.Memory
	s.Resources.TenantUsage[payload.TenantID] = tenantUsage
	
	// Update cluster total
	s.Resources.TotalUsage.CPUMillicores += payload.CPU
	s.Resources.TotalUsage.MemoryBytes += payload.Memory
	
	return nil
}

func (s *ClusterState) applyResourceReleased(event *Event) error {
	var payload struct {
		TaskID   types.TaskID
		NodeID   types.NodeID
		TenantID types.TenantID
		CPU      int64
		Memory   int64
	}
	if err := decodePayload(event.Payload, &payload); err != nil {
		return err
	}
	
	// Update node usage
	nodeUsage := s.Resources.NodeUsage[payload.NodeID]
	nodeUsage.CPUMillicores -= payload.CPU
	nodeUsage.MemoryBytes -= payload.Memory
	s.Resources.NodeUsage[payload.NodeID] = nodeUsage
	
	// Update tenant usage
	tenantUsage := s.Resources.TenantUsage[payload.TenantID]
	tenantUsage.CPUMillicores -= payload.CPU
	tenantUsage.MemoryBytes -= payload.Memory
	s.Resources.TenantUsage[payload.TenantID] = tenantUsage
	
	// Update cluster total
	s.Resources.TotalUsage.CPUMillicores -= payload.CPU
	s.Resources.TotalUsage.MemoryBytes -= payload.Memory
	
	return nil
}

// =============================================================================
// NODE EVENT HANDLERS
// =============================================================================

func (s *ClusterState) applyNodeJoined(event *Event) error {
	var payload struct {
		NodeID   types.NodeID
		Address  string
		Zone     string
		Capacity types.ResourceCapacity
	}
	if err := decodePayload(event.Payload, &payload); err != nil {
		return err
	}
	
	s.Nodes[payload.NodeID] = &NodeState{
		Info: types.NodeInfo{
			ID:       payload.NodeID,
			Address:  payload.Address,
			State:    types.NodeStateActive,
			JoinedAt: event.Timestamp,
			LastSeen: event.Timestamp,
			Capabilities: types.NodeCapabilities{
				AvailabilityZone: payload.Zone,
			},
			Resources: payload.Capacity,
		},
		LastEventID: event.ID,
		UpdatedAt:   event.Timestamp,
	}
	
	// Update cluster capacity
	s.Resources.TotalCapacity.CPUMillicores += payload.Capacity.CPUMillicores
	s.Resources.TotalCapacity.MemoryBytes += payload.Capacity.MemoryBytes
	
	// Update zone index
	s.addToIndex(&s.nodesByZone, payload.Zone, payload.NodeID)
	
	return nil
}

func (s *ClusterState) applyNodeLeft(event *Event) error {
	var payload struct {
		NodeID types.NodeID
		Reason string
	}
	if err := decodePayload(event.Payload, &payload); err != nil {
		return err
	}
	
	node, ok := s.Nodes[payload.NodeID]
	if !ok {
		return ErrNodeNotFound
	}
	
	// Update cluster capacity
	s.Resources.TotalCapacity.CPUMillicores -= node.Info.Resources.CPUMillicores
	s.Resources.TotalCapacity.MemoryBytes -= node.Info.Resources.MemoryBytes
	
	// Remove from zone index
	zone := node.Info.Capabilities.AvailabilityZone
	s.removeFromIndex(&s.nodesByZone, zone, payload.NodeID)
	
	// Mark as removed but keep for historical queries
	node.Info.State = types.NodeStateRemoved
	node.LastEventID = event.ID
	node.UpdatedAt = event.Timestamp
	
	return nil
}

func (s *ClusterState) applyNodeStateChanged(event *Event) error {
	var payload struct {
		NodeID   types.NodeID
		OldState types.NodeState
		NewState types.NodeState
		Reason   string
	}
	if err := decodePayload(event.Payload, &payload); err != nil {
		return err
	}
	
	node, ok := s.Nodes[payload.NodeID]
	if !ok {
		return ErrNodeNotFound
	}
	
	node.Info.State = payload.NewState
	node.LastEventID = event.ID
	node.UpdatedAt = event.Timestamp
	
	return nil
}

// =============================================================================
// TENANT EVENT HANDLERS
// =============================================================================

func (s *ClusterState) applyTenantCreated(event *Event) error {
	var payload struct {
		TenantID types.TenantID
		Name     string
		Quota    types.ResourceQuota
	}
	if err := decodePayload(event.Payload, &payload); err != nil {
		return err
	}
	
	s.Tenants[payload.TenantID] = &TenantState{
		Tenant: types.Tenant{
			ID:    payload.TenantID,
			Name:  payload.Name,
			Quota: payload.Quota,
		},
		UpdatedAt: event.Timestamp,
	}
	
	return nil
}

// =============================================================================
// QUERY METHODS
// =============================================================================

// GetTask returns a task by ID.
func (s *ClusterState) GetTask(id types.TaskID) (*TaskState, bool) {
	s.mu.RLock()
	defer s.mu.RUnlock()
	
	task, ok := s.Tasks[id]
	return task, ok
}

// GetTasksByTenant returns all tasks for a tenant.
func (s *ClusterState) GetTasksByTenant(tenantID types.TenantID) []*TaskState {
	s.mu.RLock()
	defer s.mu.RUnlock()
	
	taskIDs, ok := s.tasksByTenant[tenantID]
	if !ok {
		return nil
	}
	
	result := make([]*TaskState, 0, len(taskIDs))
	for taskID := range taskIDs {
		if task, ok := s.Tasks[taskID]; ok {
			result = append(result, task)
		}
	}
	
	return result
}

// GetTasksByState returns all tasks in a given state.
func (s *ClusterState) GetTasksByState(state types.TaskState) []*TaskState {
	s.mu.RLock()
	defer s.mu.RUnlock()
	
	taskIDs, ok := s.tasksByState[state]
	if !ok {
		return nil
	}
	
	result := make([]*TaskState, 0, len(taskIDs))
	for taskID := range taskIDs {
		if task, ok := s.Tasks[taskID]; ok {
			result = append(result, task)
		}
	}
	
	return result
}

// GetNode returns a node by ID.
func (s *ClusterState) GetNode(id types.NodeID) (*NodeState, bool) {
	s.mu.RLock()
	defer s.mu.RUnlock()
	
	node, ok := s.Nodes[id]
	return node, ok
}

// GetActiveNodes returns all nodes that are currently active.
func (s *ClusterState) GetActiveNodes() []*NodeState {
	s.mu.RLock()
	defer s.mu.RUnlock()
	
	result := make([]*NodeState, 0)
	for _, node := range s.Nodes {
		if node.Info.State == types.NodeStateActive {
			result = append(result, node)
		}
	}
	
	return result
}

// GetTenantUsage returns current resource usage for a tenant.
func (s *ClusterState) GetTenantUsage(tenantID types.TenantID) types.ResourceUsage {
	s.mu.RLock()
	defer s.mu.RUnlock()
	
	return s.Resources.TenantUsage[tenantID]
}

// =============================================================================
// SNAPSHOTTING
// =============================================================================

// Snapshot creates a serialized snapshot of the current state.
// The snapshot includes all materialized state at the current version.
func (s *ClusterState) Snapshot() ([]byte, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()
	
	// TODO: Implement serialization with MessagePack or similar
	// For now, return placeholder
	buf := make([]byte, 8)
	binary.BigEndian.PutUint64(buf, s.Version)
	return buf, nil
}

// Restore replaces the current state with the given snapshot.
func (s *ClusterState) Restore(data []byte) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	
	// TODO: Implement deserialization
	if len(data) < 8 {
		return errors.New("invalid snapshot data")
	}
	
	s.Version = binary.BigEndian.Uint64(data[:8])
	return nil
}

// =============================================================================
// INDEX HELPERS
// =============================================================================

func (s *ClusterState) addToIndex[K comparable, V comparable](index *map[K]map[V]bool, key K, value V) {
	if *index == nil {
		*index = make(map[K]map[V]bool)
	}
	if (*index)[key] == nil {
		(*index)[key] = make(map[V]bool)
	}
	(*index)[key][value] = true
}

func (s *ClusterState) removeFromIndex[K comparable, V comparable](index *map[K]map[V]bool, key K, value V) {
	if *index == nil {
		return
	}
	if (*index)[key] != nil {
		delete((*index)[key], value)
	}
}

func (s *ClusterState) addToTaskStateIndex(state types.TaskState, taskID types.TaskID) {
	if s.tasksByState == nil {
		s.tasksByState = make(map[types.TaskState]map[types.TaskID]bool)
	}
	if s.tasksByState[state] == nil {
		s.tasksByState[state] = make(map[types.TaskID]bool)
	}
	s.tasksByState[state][taskID] = true
}

func (s *ClusterState) removeFromTaskStateIndex(state types.TaskState, taskID types.TaskID) {
	if s.tasksByState != nil && s.tasksByState[state] != nil {
		delete(s.tasksByState[state], taskID)
	}
}

// =============================================================================
// PAYLOAD ENCODING
// =============================================================================

// decodePayload deserializes an event payload.
// Uses a simple binary format for now; in production, use MessagePack.
func decodePayload(data []byte, v interface{}) error {
	// TODO: Implement proper deserialization
	// For now, this is a placeholder that succeeds if data is non-nil
	if data == nil {
		return errors.New("nil payload")
	}
	return nil
}

// =============================================================================
// ERRORS
// =============================================================================

var (
	ErrEventOutOfOrder  = errors.New("event applied out of order")
	ErrUnknownEventType = errors.New("unknown event type")
	ErrTaskAlreadyExists = errors.New("task already exists")
	ErrTaskNotFound     = errors.New("task not found")
	ErrNodeNotFound     = errors.New("node not found")
)
