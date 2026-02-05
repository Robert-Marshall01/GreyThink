// Package scheduler implements deterministic task scheduling.
//
// # Design Goals
//
// 1. Determinism: Given the same inputs (task queue, cluster state, time),
//    the scheduler MUST produce identical scheduling decisions. This enables:
//    - Replay debugging of scheduling decisions
//    - State machine replication (all replicas schedule identically)
//    - Predictable capacity planning
//
// 2. Fairness: No tenant should be starved while others run tasks.
//    We use Weighted Fair Queuing (WFQ) within each priority lane.
//
// 3. Efficiency: Maximize cluster utilization while respecting constraints.
//
// 4. Priority: Critical system tasks must preempt lower-priority work.
//
// # Scheduling Algorithm
//
// The scheduler uses a multi-level queue with priority lanes:
// - Lane 0 (CRITICAL): System tasks, health checks
// - Lane 1 (HIGH): Latency-sensitive user tasks
// - Lane 2 (NORMAL): Standard batch processing
// - Lane 3 (LOW): Background maintenance
// - Lane 4 (PREEMPTIBLE): Opportunistic tasks
//
// Within each lane, we use Deficit Round Robin (DRR) to ensure fairness
// between tenants. Each tenant accumulates "deficit" which determines
// scheduling priority within the lane.
package scheduler

import (
	"container/heap"
	"context"
	"errors"
	"sort"
	"sync"
	"time"

	"github.com/grey-systems/grey-distributed/pkg/state"
	"github.com/grey-systems/grey-distributed/pkg/types"
)

// =============================================================================
// CONFIGURATION
// =============================================================================

// Config contains scheduler configuration.
type Config struct {
	// SchedulingInterval is how often the scheduler runs.
	// Lower values = lower latency, but more CPU overhead.
	SchedulingInterval time.Duration

	// MaxSchedulePerCycle limits tasks scheduled per cycle.
	// Prevents long scheduling cycles from blocking other operations.
	MaxSchedulePerCycle int

	// DefaultBurstMultiplier allows temporary overage of quota.
	// A value of 2.0 allows 2x quota for short bursts.
	DefaultBurstMultiplier float64

	// PreemptionEnabled allows higher-priority tasks to preempt lower.
	PreemptionEnabled bool

	// BinPackingEnabled enables bin packing optimization.
	// Trades scheduling latency for better resource utilization.
	BinPackingEnabled bool
}

// DefaultConfig returns sensible defaults.
func DefaultConfig() Config {
	return Config{
		SchedulingInterval:    100 * time.Millisecond,
		MaxSchedulePerCycle:   100,
		DefaultBurstMultiplier: 2.0,
		PreemptionEnabled:     true,
		BinPackingEnabled:     true,
	}
}

// =============================================================================
// SCHEDULER
// =============================================================================

// Scheduler manages task placement on cluster nodes.
type Scheduler struct {
	config Config
	mu     sync.RWMutex

	// State references
	clusterState *state.ClusterState

	// Priority lanes (0 = highest priority)
	lanes [5]*PriorityLane

	// Per-tenant deficit tracking for fair queuing
	tenantDeficits map[types.TenantID]int64

	// Pending scheduling decisions (deterministic order)
	pendingDecisions []SchedulingDecision

	// Metrics
	metrics *SchedulerMetrics
}

// PriorityLane is a queue for tasks at a specific priority level.
type PriorityLane struct {
	Priority types.TaskPriority
	Tasks    *TaskHeap
}

// SchedulingDecision represents a task-to-node assignment.
type SchedulingDecision struct {
	TaskID     types.TaskID
	TenantID   types.TenantID
	NodeID     types.NodeID
	Priority   types.TaskPriority
	Resources  types.ResourceRequest
	ScheduleAt time.Time
}

// NewScheduler creates a new scheduler instance.
func NewScheduler(config Config, clusterState *state.ClusterState) *Scheduler {
	s := &Scheduler{
		config:         config,
		clusterState:   clusterState,
		tenantDeficits: make(map[types.TenantID]int64),
		metrics:        NewSchedulerMetrics(),
	}

	// Initialize priority lanes
	s.lanes[0] = &PriorityLane{Priority: types.PriorityCritical, Tasks: &TaskHeap{}}
	s.lanes[1] = &PriorityLane{Priority: types.PriorityHigh, Tasks: &TaskHeap{}}
	s.lanes[2] = &PriorityLane{Priority: types.PriorityNormal, Tasks: &TaskHeap{}}
	s.lanes[3] = &PriorityLane{Priority: types.PriorityLow, Tasks: &TaskHeap{}}
	s.lanes[4] = &PriorityLane{Priority: types.PriorityPreemptible, Tasks: &TaskHeap{}}

	for i := range s.lanes {
		heap.Init(s.lanes[i].Tasks)
	}

	return s
}

// =============================================================================
// PUBLIC API
// =============================================================================

// Submit adds a task to the scheduling queue.
// The task is placed in the appropriate priority lane.
func (s *Scheduler) Submit(task *types.Task) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	// Validate task
	if err := s.validateTask(task); err != nil {
		return err
	}

	// Determine lane based on priority
	laneIdx := s.priorityToLane(task.Priority)
	
	entry := &TaskEntry{
		Task:        task,
		SubmittedAt: time.Now(),
		Deficit:     s.tenantDeficits[task.TenantID],
	}

	heap.Push(s.lanes[laneIdx].Tasks, entry)
	s.metrics.TasksQueued.Inc()

	return nil
}

// Schedule runs one scheduling cycle.
// This should be called by the leader node periodically.
// Returns scheduling decisions that should be applied to cluster state.
//
// CRITICAL: This method must be deterministic. All nodes running
// this method with the same inputs must produce the same output.
func (s *Scheduler) Schedule(ctx context.Context, now time.Time) ([]SchedulingDecision, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	start := time.Now()
	defer func() {
		s.metrics.SchedulingLatency.Set(time.Since(start).Nanoseconds())
	}()

	decisions := make([]SchedulingDecision, 0, s.config.MaxSchedulePerCycle)

	// Get available nodes sorted deterministically
	nodes := s.getAvailableNodes()
	if len(nodes) == 0 {
		return nil, ErrNoAvailableNodes
	}

	// Process lanes in priority order (critical first)
	for laneIdx := 0; laneIdx < len(s.lanes); laneIdx++ {
		lane := s.lanes[laneIdx]
		if lane.Tasks.Len() == 0 {
			continue
		}

		// Schedule tasks from this lane
		for lane.Tasks.Len() > 0 && len(decisions) < s.config.MaxSchedulePerCycle {
			// Peek at the next task
			entry := (*lane.Tasks)[0]

			// Check tenant quota
			if !s.checkTenantQuota(entry.Task) {
				// Tenant over quota, skip this task
				heap.Pop(lane.Tasks)
				s.requeueWithDelay(entry, laneIdx)
				continue
			}

			// Find best node for this task
			nodeID, err := s.selectNode(entry.Task, nodes)
			if err != nil {
				if errors.Is(err, ErrNoFittingNode) {
					// No node can fit this task right now
					// Try preemption if enabled and this is high priority
					if s.config.PreemptionEnabled && laneIdx <= 1 {
						if preempted := s.tryPreemption(entry.Task, nodes); preempted {
							nodeID, err = s.selectNode(entry.Task, nodes)
						}
					}
				}
				if err != nil {
					// Still can't schedule, try next task
					heap.Pop(lane.Tasks)
					s.requeueWithDelay(entry, laneIdx)
					continue
				}
			}

			// Create scheduling decision
			decision := SchedulingDecision{
				TaskID:     entry.Task.ID,
				TenantID:   entry.Task.TenantID,
				NodeID:     nodeID,
				Priority:   entry.Task.Priority,
				Resources:  entry.Task.Resources,
				ScheduleAt: now,
			}
			decisions = append(decisions, decision)

			// Update node availability (in-memory, for this cycle)
			s.reserveResources(nodeID, entry.Task.Resources)

			// Update tenant deficit
			s.updateTenantDeficit(entry.Task.TenantID, entry.Task.Resources)

			// Remove from queue
			heap.Pop(lane.Tasks)

			s.metrics.TasksScheduled.Inc()
		}
	}

	// Sort decisions for deterministic ordering
	sort.Slice(decisions, func(i, j int) bool {
		return decisions[i].TaskID.Sequence() < decisions[j].TaskID.Sequence()
	})

	return decisions, nil
}

// Cancel removes a task from the scheduling queue.
func (s *Scheduler) Cancel(taskID types.TaskID) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	for _, lane := range s.lanes {
		for i, entry := range *lane.Tasks {
			if entry.Task.ID == taskID {
				heap.Remove(lane.Tasks, i)
				s.metrics.TasksCancelled.Inc()
				return nil
			}
		}
	}

	return ErrTaskNotQueued
}

// =============================================================================
// NODE SELECTION
// =============================================================================

// selectNode finds the best node for a task.
// Uses bin packing when enabled to improve utilization.
//
// Selection criteria (in order):
// 1. Node must have sufficient resources
// 2. Node must satisfy constraints (zone, affinity, etc.)
// 3. Prefer nodes with most available resources (spread) or least (pack)
func (s *Scheduler) selectNode(task *types.Task, nodes []*NodeCandidate) (types.NodeID, error) {
	var candidates []*NodeCandidate

	for _, node := range nodes {
		// Check if node has sufficient resources
		if !node.Available.CanFit(task.Resources) {
			continue
		}

		// Check constraints
		if !s.checkConstraints(task, node) {
			continue
		}

		candidates = append(candidates, node)
	}

	if len(candidates) == 0 {
		return 0, ErrNoFittingNode
	}

	// Score candidates
	if s.config.BinPackingEnabled {
		// Bin packing: prefer nodes with LEAST available resources
		// This consolidates workloads and allows other nodes to be drained
		sort.Slice(candidates, func(i, j int) bool {
			return candidates[i].score() < candidates[j].score()
		})
	} else {
		// Spread: prefer nodes with MOST available resources
		// This distributes load evenly
		sort.Slice(candidates, func(i, j int) bool {
			return candidates[i].score() > candidates[j].score()
		})
	}

	return candidates[0].NodeID, nil
}

// NodeCandidate represents a node being considered for scheduling.
type NodeCandidate struct {
	NodeID    types.NodeID
	Zone      string
	Available types.ResourceCapacity
}

// score returns a numeric score for node selection.
// Higher score = more available resources.
func (n *NodeCandidate) score() int64 {
	// Weight CPU more heavily (typically bottleneck)
	return n.Available.CPUMillicores*2 + n.Available.MemoryBytes/(1024*1024)
}

// getAvailableNodes returns nodes available for scheduling.
// Nodes are sorted by ID for deterministic ordering.
func (s *Scheduler) getAvailableNodes() []*NodeCandidate {
	activeNodes := s.clusterState.GetActiveNodes()
	
	candidates := make([]*NodeCandidate, 0, len(activeNodes))
	for _, node := range activeNodes {
		usage := s.clusterState.GetTenantUsage(0) // System usage
		available := node.Info.Resources.Subtract(types.ResourceUsage{
			CPUMillicores: node.Allocated.CPUMillicores,
			MemoryBytes:   node.Allocated.MemoryBytes,
		})
		
		candidates = append(candidates, &NodeCandidate{
			NodeID:    node.Info.ID,
			Zone:      node.Info.Capabilities.AvailabilityZone,
			Available: available,
		})
		_ = usage // Suppress unused variable warning
	}

	// Sort by NodeID for determinism
	sort.Slice(candidates, func(i, j int) bool {
		return candidates[i].NodeID < candidates[j].NodeID
	})

	return candidates
}

// checkConstraints verifies a node satisfies task constraints.
func (s *Scheduler) checkConstraints(task *types.Task, node *NodeCandidate) bool {
	constraints := task.Constraints

	// Check required zones
	if len(constraints.RequiredZones) > 0 {
		found := false
		for _, zone := range constraints.RequiredZones {
			if zone == node.Zone {
				found = true
				break
			}
		}
		if !found {
			return false
		}
	}

	// Check excluded nodes
	for _, excluded := range constraints.ExcludedNodes {
		if excluded == node.NodeID {
			return false
		}
	}

	// TODO: Check affinity/anti-affinity rules

	return true
}

// =============================================================================
// QUOTA MANAGEMENT
// =============================================================================

// checkTenantQuota verifies a tenant has available quota.
func (s *Scheduler) checkTenantQuota(task *types.Task) bool {
	tenantState, ok := s.clusterState.Tenants[task.TenantID]
	if !ok {
		return false
	}

	quota := tenantState.Tenant.Quota
	usage := s.clusterState.GetTenantUsage(task.TenantID)

	// Check if adding this task would exceed quota
	// Allow burst up to multiplier
	maxCPU := int64(float64(quota.MaxCPUMillicores) * s.config.DefaultBurstMultiplier)
	maxMem := int64(float64(quota.MaxMemoryBytes) * s.config.DefaultBurstMultiplier)

	return (usage.CPUMillicores+task.Resources.CPUMillicores <= maxCPU &&
		usage.MemoryBytes+task.Resources.MemoryBytes <= maxMem)
}

// updateTenantDeficit updates deficit counter for fair queuing.
// Deficit increases when a tenant uses resources, decreases over time.
func (s *Scheduler) updateTenantDeficit(tenantID types.TenantID, resources types.ResourceRequest) {
	// Deficit is proportional to resources consumed
	deficit := resources.CPUMillicores + resources.MemoryBytes/(1024*1024)
	s.tenantDeficits[tenantID] += deficit
}

// =============================================================================
// PREEMPTION
// =============================================================================

// tryPreemption attempts to free resources by preempting lower-priority tasks.
// Returns true if preemption was successful.
//
// Preemption Policy:
// 1. Only preempt tasks with lower priority
// 2. Prefer preempting single tasks over multiple
// 3. Never preempt tasks from the same tenant (unless CRITICAL)
// 4. Preempted tasks are re-queued for scheduling
func (s *Scheduler) tryPreemption(task *types.Task, nodes []*NodeCandidate) bool {
	// Get running tasks that could be preempted
	preemptibleTasks := s.clusterState.GetTasksByState(types.TaskStateRunning)

	// Sort by priority (lowest first = best preemption candidates)
	sort.Slice(preemptibleTasks, func(i, j int) bool {
		return preemptibleTasks[i].Task.Priority > preemptibleTasks[j].Task.Priority
	})

	for _, candidate := range preemptibleTasks {
		// Don't preempt higher or equal priority
		if candidate.Task.Priority <= task.Priority {
			continue
		}

		// Don't preempt same tenant unless this is CRITICAL
		if candidate.Task.TenantID == task.TenantID && task.Priority != types.PriorityCritical {
			continue
		}

		// Check if preempting this task would free enough resources
		// For simplicity, we just check if the resources match
		if candidate.Task.Resources.CPUMillicores >= task.Resources.CPUMillicores &&
			candidate.Task.Resources.MemoryBytes >= task.Resources.MemoryBytes {
			
			// TODO: Actually preempt the task
			// This would involve:
			// 1. Sending a preemption signal to the node
			// 2. Waiting for the task to be killed
			// 3. Re-queuing the preempted task

			s.metrics.TasksPreempted.Inc()
			return true
		}
	}

	return false
}

// =============================================================================
// HELPER METHODS
// =============================================================================

func (s *Scheduler) validateTask(task *types.Task) error {
	if task.ID == (types.TaskID{}) {
		return ErrInvalidTaskID
	}
	if task.Resources.CPUMillicores <= 0 {
		return ErrInvalidResources
	}
	return nil
}

func (s *Scheduler) priorityToLane(priority types.TaskPriority) int {
	switch {
	case priority <= types.PriorityCritical:
		return 0
	case priority <= types.PriorityHigh:
		return 1
	case priority <= types.PriorityNormal:
		return 2
	case priority <= types.PriorityLow:
		return 3
	default:
		return 4
	}
}

func (s *Scheduler) requeueWithDelay(entry *TaskEntry, laneIdx int) {
	// Add delay penalty to prevent tight retry loops
	entry.Deficit += 1000
	heap.Push(s.lanes[laneIdx].Tasks, entry)
}

func (s *Scheduler) reserveResources(nodeID types.NodeID, resources types.ResourceRequest) {
	// This is a temporary reservation for the current scheduling cycle
	// The actual resource allocation happens when the scheduling decision
	// is committed through the state machine
	// TODO: Track temporary reservations
}

// =============================================================================
// TASK HEAP (Priority Queue)
// =============================================================================

// TaskEntry is an entry in the scheduling queue.
type TaskEntry struct {
	Task        *types.Task
	SubmittedAt time.Time
	Deficit     int64 // For fair queuing
	Index       int   // heap.Interface
}

// TaskHeap implements heap.Interface for task scheduling.
// Tasks are ordered by: (Priority, Deficit, SubmittedAt)
type TaskHeap []*TaskEntry

func (h TaskHeap) Len() int { return len(h) }

func (h TaskHeap) Less(i, j int) bool {
	// Lower priority value = higher priority (processed first)
	if h[i].Task.Priority != h[j].Task.Priority {
		return h[i].Task.Priority < h[j].Task.Priority
	}
	// Lower deficit = more deserving (fair queuing)
	if h[i].Deficit != h[j].Deficit {
		return h[i].Deficit < h[j].Deficit
	}
	// Earlier submission = higher priority (FIFO tiebreaker)
	return h[i].SubmittedAt.Before(h[j].SubmittedAt)
}

func (h TaskHeap) Swap(i, j int) {
	h[i], h[j] = h[j], h[i]
	h[i].Index = i
	h[j].Index = j
}

func (h *TaskHeap) Push(x interface{}) {
	entry := x.(*TaskEntry)
	entry.Index = len(*h)
	*h = append(*h, entry)
}

func (h *TaskHeap) Pop() interface{} {
	old := *h
	n := len(old)
	entry := old[n-1]
	old[n-1] = nil
	entry.Index = -1
	*h = old[0 : n-1]
	return entry
}

// =============================================================================
// METRICS
// =============================================================================

// SchedulerMetrics tracks scheduler performance.
type SchedulerMetrics struct {
	TasksQueued       Counter
	TasksScheduled    Counter
	TasksCancelled    Counter
	TasksPreempted    Counter
	SchedulingLatency Gauge
	QueueDepth        Gauge
}

// Counter is a monotonically increasing metric.
type Counter struct {
	value int64
	mu    sync.Mutex
}

func (c *Counter) Inc() {
	c.mu.Lock()
	c.value++
	c.mu.Unlock()
}

func (c *Counter) Value() int64 {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.value
}

// Gauge is a metric that can increase or decrease.
type Gauge struct {
	value int64
	mu    sync.Mutex
}

func (g *Gauge) Set(v int64) {
	g.mu.Lock()
	g.value = v
	g.mu.Unlock()
}

func (g *Gauge) Value() int64 {
	g.mu.Lock()
	defer g.mu.Unlock()
	return g.value
}

func NewSchedulerMetrics() *SchedulerMetrics {
	return &SchedulerMetrics{}
}

// =============================================================================
// ERRORS
// =============================================================================

var (
	ErrNoAvailableNodes = errors.New("no available nodes in cluster")
	ErrNoFittingNode    = errors.New("no node can fit the task")
	ErrTaskNotQueued    = errors.New("task not in scheduling queue")
	ErrInvalidTaskID    = errors.New("invalid task ID")
	ErrInvalidResources = errors.New("invalid resource request")
)
