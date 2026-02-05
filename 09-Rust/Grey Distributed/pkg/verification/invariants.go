// Package verification implements runtime invariant checking for Grey Distributed.
//
// # Design Philosophy
//
// Formal verification (TLA+) proves correctness of the design, but implementation
// can still have bugs. Runtime invariant checking provides defense in depth:
//
//   1. TLA+ proves the algorithm is correct
//   2. Runtime checks verify the implementation matches the algorithm
//   3. Production monitoring detects violations in real deployments
//
// # Invariant Categories
//
//   Safety Invariants (checked continuously):
//   - Violated? Stop processing, alert, dump state
//   - Examples: quota limits, capacity bounds, single leader per term
//
//   Consistency Invariants (checked periodically):
//   - Violated? Trigger repair, log warning
//   - Examples: replica convergence, log matching
//
//   Performance Invariants (monitored):
//   - Violated? Trigger scaling, rebalancing
//   - Examples: latency bounds, queue depths
//
// # Integration
//
// Invariants are checked at state transitions:
//
//	Before: Preconditions must hold
//	After:  Postconditions must hold
//	Always: Invariants must hold at all times
//
// This mirrors TLA+ temporal logic:
//   - []P  (always P)     → Safety invariants
//   - <>P  (eventually P) → Liveness properties (checked via timeouts)
package verification

import (
	"context"
	"errors"
	"fmt"
	"runtime"
	"sync"
	"sync/atomic"
	"time"

	"github.com/grey-systems/grey-distributed/pkg/types"
)

// =============================================================================
// INVARIANT TYPES
// =============================================================================

// InvariantKind categorizes invariants by severity and checking frequency.
type InvariantKind int

const (
	// SafetyInvariant: If violated, system is in undefined state.
	// Action: Halt processing, alert ops, dump diagnostics.
	SafetyInvariant InvariantKind = iota

	// ConsistencyInvariant: System may self-repair.
	// Action: Trigger repair, warn, continue processing.
	ConsistencyInvariant

	// PerformanceInvariant: Performance degradation detected.
	// Action: Scale, rebalance, alert if sustained.
	PerformanceInvariant
)

func (k InvariantKind) String() string {
	switch k {
	case SafetyInvariant:
		return "SAFETY"
	case ConsistencyInvariant:
		return "CONSISTENCY"
	case PerformanceInvariant:
		return "PERFORMANCE"
	default:
		return "UNKNOWN"
	}
}

// Invariant defines a checkable property.
type Invariant struct {
	// Name identifies the invariant (should match TLA+ spec)
	Name string

	// Kind determines handling of violations
	Kind InvariantKind

	// Description explains what the invariant checks
	Description string

	// TLAReference points to the TLA+ specification
	TLAReference string

	// Check is the actual verification function
	// Returns nil if invariant holds, error describing violation otherwise
	Check func(state *SystemState) error
}

// Violation records an invariant violation.
type Violation struct {
	Invariant   *Invariant
	Error       error
	Timestamp   time.Time
	State       *SystemState
	StackTrace  string
}

// =============================================================================
// SYSTEM STATE SNAPSHOT
// =============================================================================

// SystemState captures the system state for invariant checking.
// This is a consistent snapshot of all relevant state.
type SystemState struct {
	// Timestamp when snapshot was taken
	Timestamp time.Time

	// Consensus state
	Consensus ConsensusState

	// Scheduler state
	Scheduler SchedulerState

	// Storage state
	Storage StorageState

	// Security state
	Security SecurityState
}

// ConsensusState captures Raft consensus state.
type ConsensusState struct {
	NodeID       types.NodeID
	CurrentTerm  uint64
	Role         string // "Leader", "Follower", "Candidate"
	VotedFor     *types.NodeID
	CommitIndex  uint64
	LastApplied  uint64
	LogLength    uint64
	LeaderID     *types.NodeID

	// Cluster membership
	Peers        []types.NodeID
	VoterCount   int
	QuorumSize   int

	// For checking Agreement (single leader per term)
	KnownLeaders map[uint64]types.NodeID // term -> leader
}

// SchedulerState captures scheduler state.
type SchedulerState struct {
	// Queue depths per lane
	QueueDepths map[string]int

	// Resource usage per tenant
	TenantUsage map[types.TenantID]ResourceUsage

	// Node capacity
	NodeCapacity    int64
	NodeUsed        int64

	// Tasks in flight
	RunningTasks    int
	PreemptedTasks  int
}

// ResourceUsage tracks a tenant's resource consumption.
type ResourceUsage struct {
	CPUUsed       int64
	CPUQuota      int64
	MemoryUsed    int64
	MemoryQuota   int64
	TasksRunning  int
	TasksQueued   int
}

// StorageState captures storage layer state.
type StorageState struct {
	// Replication health
	ReplicationFactor int
	ReadQuorum        int
	WriteQuorum       int

	// Data distribution
	TotalKeys         int64
	ReplicatedKeys    int64
	UnderReplicated   int64

	// Per-node data counts (for balance checking)
	NodeDataCounts    map[types.NodeID]int64
}

// SecurityState captures security state.
type SecurityState struct {
	// Active certificates
	ValidCerts        int
	ExpiringSoon      int // Expiring within 24h
	Expired           int

	// Authentication state
	ActiveSessions    int
	FailedAuthLast5m  int
}

// =============================================================================
// INVARIANT CHECKER
// =============================================================================

// InvariantChecker manages invariant registration and checking.
type InvariantChecker struct {
	mu         sync.RWMutex
	invariants []*Invariant
	violations chan *Violation
	metrics    *InvariantMetrics

	// Handlers
	onSafetyViolation func(*Violation)
	onViolation       func(*Violation)

	// State
	enabled     atomic.Bool
	checkCount  atomic.Uint64
	violationCount atomic.Uint64
}

// InvariantMetrics tracks invariant checking statistics.
type InvariantMetrics struct {
	ChecksPerformed   atomic.Uint64
	SafetyViolations  atomic.Uint64
	ConsistencyViolations atomic.Uint64
	PerformanceViolations atomic.Uint64
	LastCheckDuration atomic.Int64 // nanoseconds
}

// NewInvariantChecker creates a new invariant checker.
func NewInvariantChecker(bufferSize int) *InvariantChecker {
	ic := &InvariantChecker{
		invariants: make([]*Invariant, 0),
		violations: make(chan *Violation, bufferSize),
		metrics:    &InvariantMetrics{},
	}
	ic.enabled.Store(true)

	// Register built-in invariants
	ic.registerBuiltinInvariants()

	return ic
}

// Register adds an invariant to the checker.
func (ic *InvariantChecker) Register(inv *Invariant) {
	ic.mu.Lock()
	defer ic.mu.Unlock()
	ic.invariants = append(ic.invariants, inv)
}

// SetSafetyViolationHandler sets the handler for safety violations.
// This should halt processing and alert operators.
func (ic *InvariantChecker) SetSafetyViolationHandler(handler func(*Violation)) {
	ic.mu.Lock()
	defer ic.mu.Unlock()
	ic.onSafetyViolation = handler
}

// SetViolationHandler sets the general violation handler.
func (ic *InvariantChecker) SetViolationHandler(handler func(*Violation)) {
	ic.mu.Lock()
	defer ic.mu.Unlock()
	ic.onViolation = handler
}

// Check verifies all invariants against the given state.
func (ic *InvariantChecker) Check(state *SystemState) []*Violation {
	if !ic.enabled.Load() {
		return nil
	}

	start := time.Now()
	defer func() {
		ic.metrics.LastCheckDuration.Store(time.Since(start).Nanoseconds())
	}()

	ic.mu.RLock()
	invariants := make([]*Invariant, len(ic.invariants))
	copy(invariants, ic.invariants)
	ic.mu.RUnlock()

	var violations []*Violation

	for _, inv := range invariants {
		ic.checkCount.Add(1)
		ic.metrics.ChecksPerformed.Add(1)

		if err := inv.Check(state); err != nil {
			ic.violationCount.Add(1)

			v := &Violation{
				Invariant:  inv,
				Error:      err,
				Timestamp:  time.Now(),
				State:      state,
				StackTrace: captureStackTrace(),
			}
			violations = append(violations, v)

			// Update metrics by kind
			switch inv.Kind {
			case SafetyInvariant:
				ic.metrics.SafetyViolations.Add(1)
			case ConsistencyInvariant:
				ic.metrics.ConsistencyViolations.Add(1)
			case PerformanceInvariant:
				ic.metrics.PerformanceViolations.Add(1)
			}

			// Send to channel (non-blocking)
			select {
			case ic.violations <- v:
			default:
				// Channel full, drop (metrics captured above)
			}

			// Call handlers
			ic.mu.RLock()
			if inv.Kind == SafetyInvariant && ic.onSafetyViolation != nil {
				go ic.onSafetyViolation(v)
			}
			if ic.onViolation != nil {
				go ic.onViolation(v)
			}
			ic.mu.RUnlock()
		}
	}

	return violations
}

// CheckInvariant checks a single invariant by name.
func (ic *InvariantChecker) CheckInvariant(name string, state *SystemState) error {
	ic.mu.RLock()
	defer ic.mu.RUnlock()

	for _, inv := range ic.invariants {
		if inv.Name == name {
			return inv.Check(state)
		}
	}
	return fmt.Errorf("invariant not found: %s", name)
}

// Violations returns the violation channel for async processing.
func (ic *InvariantChecker) Violations() <-chan *Violation {
	return ic.violations
}

// Enable enables invariant checking.
func (ic *InvariantChecker) Enable() {
	ic.enabled.Store(true)
}

// Disable disables invariant checking (for performance-critical paths).
func (ic *InvariantChecker) Disable() {
	ic.enabled.Store(false)
}

// Metrics returns current metrics.
func (ic *InvariantChecker) Metrics() InvariantMetrics {
	return InvariantMetrics{
		ChecksPerformed:      ic.metrics.ChecksPerformed,
		SafetyViolations:     ic.metrics.SafetyViolations,
		ConsistencyViolations: ic.metrics.ConsistencyViolations,
		PerformanceViolations: ic.metrics.PerformanceViolations,
		LastCheckDuration:    ic.metrics.LastCheckDuration,
	}
}

// =============================================================================
// BUILT-IN INVARIANTS
// =============================================================================

func (ic *InvariantChecker) registerBuiltinInvariants() {
	// -----------------------------------------
	// CONSENSUS INVARIANTS
	// -----------------------------------------

	ic.Register(&Invariant{
		Name:        "Agreement",
		Kind:        SafetyInvariant,
		Description: "At most one leader per term",
		TLAReference: "GreyConsensus.tla:Agreement",
		Check: func(state *SystemState) error {
			// Check if we know of multiple leaders in any term
			for term, leader := range state.Consensus.KnownLeaders {
				if state.Consensus.CurrentTerm == term &&
					state.Consensus.Role == "Leader" &&
					state.Consensus.NodeID != leader {
					return fmt.Errorf("multiple leaders in term %d: %s and %s",
						term, leader, state.Consensus.NodeID)
				}
			}
			return nil
		},
	})

	ic.Register(&Invariant{
		Name:        "CommitMonotonicity",
		Kind:        SafetyInvariant,
		Description: "Commit index never decreases",
		TLAReference: "GreyConsensus.tla:CommittedEntriesNeverRollback",
		Check: func(state *SystemState) error {
			// This requires comparing with previous state
			// In practice, maintain lastCommitIndex and compare
			return nil // Placeholder
		},
	})

	ic.Register(&Invariant{
		Name:        "LogBoundedByCommit",
		Kind:        SafetyInvariant,
		Description: "Commit index does not exceed log length",
		TLAReference: "GreyConsensus.tla",
		Check: func(state *SystemState) error {
			if state.Consensus.CommitIndex > state.Consensus.LogLength {
				return fmt.Errorf("commit index %d exceeds log length %d",
					state.Consensus.CommitIndex, state.Consensus.LogLength)
			}
			return nil
		},
	})

	// -----------------------------------------
	// SCHEDULER INVARIANTS
	// -----------------------------------------

	ic.Register(&Invariant{
		Name:        "NoQuotaViolation",
		Kind:        SafetyInvariant,
		Description: "Tenants never exceed allocated quota",
		TLAReference: "GreyScheduler.tla:NoQuotaViolation",
		Check: func(state *SystemState) error {
			for tenantID, usage := range state.Scheduler.TenantUsage {
				if usage.CPUUsed > usage.CPUQuota {
					return fmt.Errorf("tenant %s CPU usage %d exceeds quota %d",
						tenantID, usage.CPUUsed, usage.CPUQuota)
				}
				if usage.MemoryUsed > usage.MemoryQuota {
					return fmt.Errorf("tenant %s memory usage %d exceeds quota %d",
						tenantID, usage.MemoryUsed, usage.MemoryQuota)
				}
			}
			return nil
		},
	})

	ic.Register(&Invariant{
		Name:        "CapacityLimits",
		Kind:        SafetyInvariant,
		Description: "Node capacity never exceeded",
		TLAReference: "GreyScheduler.tla:CapacityLimits",
		Check: func(state *SystemState) error {
			if state.Scheduler.NodeUsed > state.Scheduler.NodeCapacity {
				return fmt.Errorf("node usage %d exceeds capacity %d",
					state.Scheduler.NodeUsed, state.Scheduler.NodeCapacity)
			}
			return nil
		},
	})

	ic.Register(&Invariant{
		Name:        "ResourceConservation",
		Kind:        ConsistencyInvariant,
		Description: "Total tenant usage equals total node usage",
		TLAReference: "GreyScheduler.tla:ResourceConservation",
		Check: func(state *SystemState) error {
			var totalTenantUsage int64
			for _, usage := range state.Scheduler.TenantUsage {
				totalTenantUsage += usage.CPUUsed
			}
			// Allow small epsilon for concurrency
			if abs(totalTenantUsage-state.Scheduler.NodeUsed) > 2 {
				return fmt.Errorf("resource accounting mismatch: tenant=%d, node=%d",
					totalTenantUsage, state.Scheduler.NodeUsed)
			}
			return nil
		},
	})

	// -----------------------------------------
	// STORAGE INVARIANTS
	// -----------------------------------------

	ic.Register(&Invariant{
		Name:        "QuorumIntersection",
		Kind:        SafetyInvariant,
		Description: "Read and write quorums overlap",
		TLAReference: "GreyReplication.tla:QuorumIntersection",
		Check: func(state *SystemState) error {
			R := state.Storage.ReadQuorum
			W := state.Storage.WriteQuorum
			N := state.Storage.ReplicationFactor
			if R+W <= N {
				return fmt.Errorf("quorum intersection violated: R=%d, W=%d, N=%d (R+W must > N)",
					R, W, N)
			}
			return nil
		},
	})

	ic.Register(&Invariant{
		Name:        "ReplicationHealth",
		Kind:        ConsistencyInvariant,
		Description: "Data is sufficiently replicated",
		TLAReference: "GreyReplication.tla",
		Check: func(state *SystemState) error {
			if state.Storage.TotalKeys > 0 {
				underReplicatedRatio := float64(state.Storage.UnderReplicated) /
					float64(state.Storage.TotalKeys)
				if underReplicatedRatio > 0.1 { // More than 10% under-replicated
					return fmt.Errorf("%.1f%% of keys under-replicated",
						underReplicatedRatio*100)
				}
			}
			return nil
		},
	})

	// -----------------------------------------
	// SECURITY INVARIANTS
	// -----------------------------------------

	ic.Register(&Invariant{
		Name:        "NoExpiredCertificates",
		Kind:        SafetyInvariant,
		Description: "No nodes operating with expired certificates",
		TLAReference: "GreySecurity.tla",
		Check: func(state *SystemState) error {
			if state.Security.Expired > 0 {
				return fmt.Errorf("%d expired certificates in use",
					state.Security.Expired)
			}
			return nil
		},
	})

	ic.Register(&Invariant{
		Name:        "AuthFailureRateLimit",
		Kind:        PerformanceInvariant,
		Description: "Authentication failures below threshold",
		TLAReference: "GreySecurity.tla",
		Check: func(state *SystemState) error {
			if state.Security.FailedAuthLast5m > 100 {
				return fmt.Errorf("high auth failure rate: %d in last 5m",
					state.Security.FailedAuthLast5m)
			}
			return nil
		},
	})
}

// =============================================================================
// CONTINUOUS CHECKER
// =============================================================================

// ContinuousChecker runs invariant checks periodically.
type ContinuousChecker struct {
	checker      *InvariantChecker
	stateFunc    func() *SystemState
	interval     time.Duration
	cancel       context.CancelFunc
	wg           sync.WaitGroup
}

// NewContinuousChecker creates a background invariant checker.
func NewContinuousChecker(
	checker *InvariantChecker,
	stateFunc func() *SystemState,
	interval time.Duration,
) *ContinuousChecker {
	return &ContinuousChecker{
		checker:   checker,
		stateFunc: stateFunc,
		interval:  interval,
	}
}

// Start begins periodic invariant checking.
func (cc *ContinuousChecker) Start(ctx context.Context) {
	ctx, cc.cancel = context.WithCancel(ctx)
	cc.wg.Add(1)

	go func() {
		defer cc.wg.Done()
		ticker := time.NewTicker(cc.interval)
		defer ticker.Stop()

		for {
			select {
			case <-ctx.Done():
				return
			case <-ticker.C:
				state := cc.stateFunc()
				cc.checker.Check(state)
			}
		}
	}()
}

// Stop halts periodic checking.
func (cc *ContinuousChecker) Stop() {
	if cc.cancel != nil {
		cc.cancel()
	}
	cc.wg.Wait()
}

// =============================================================================
// HELPERS
// =============================================================================

func captureStackTrace() string {
	buf := make([]byte, 4096)
	n := runtime.Stack(buf, false)
	return string(buf[:n])
}

func abs(x int64) int64 {
	if x < 0 {
		return -x
	}
	return x
}

// =============================================================================
// ERRORS
// =============================================================================

var (
	ErrInvariantViolation = errors.New("invariant violation")
)
