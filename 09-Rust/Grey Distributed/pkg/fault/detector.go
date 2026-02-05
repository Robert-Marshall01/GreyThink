// Package fault implements failure detection and self-healing mechanisms.
//
// # Design Philosophy
//
// Distributed systems must assume failure is the norm, not the exception.
// This package provides:
// 1. Accurate failure detection with low false-positive rate
// 2. Automatic quarantine of suspected nodes
// 3. Self-healing through rebalancing and recovery
// 4. Chaos-resilience patterns for testing
//
// # Φ-Accrual Failure Detector
//
// Traditional failure detectors use a binary alive/dead model based on
// timeout thresholds. This approach has problems:
// - Fixed timeout doesn't adapt to varying network conditions
// - High false-positive rate during GC pauses or network congestion
// - No confidence level for cascading decisions
//
// We implement the Φ-accrual failure detector which:
// - Computes a suspicion level (φ) rather than binary state
// - Adapts to network variance automatically
// - Provides a confidence level for downstream decisions
// - Reduces false positives during transient slowdowns
//
// # Failure Handling Pipeline
//
//   Detection → Quarantine → Diagnosis → Recovery/Removal
//      │            │           │              │
//      ▼            ▼           ▼              ▼
//   φ-accrual   Fence all   Run health     If healthy:
//   threshold   traffic     checks          reintegrate
//               from node                  If not: remove
package fault

import (
	"context"
	"math"
	"sync"
	"time"

	"github.com/grey-systems/grey-distributed/pkg/types"
)

// =============================================================================
// CONFIGURATION
// =============================================================================

// Config contains failure detection configuration.
type Config struct {
	// HeartbeatInterval is how often nodes send heartbeats.
	HeartbeatInterval time.Duration

	// PhiThreshold is the φ value above which a node is suspected.
	// Higher values = fewer false positives, longer detection time.
	// Recommended: 8 for LAN, 12 for WAN.
	PhiThreshold float64

	// PhiConvictionThreshold is the φ value above which a node is considered dead.
	// Should be significantly higher than PhiThreshold.
	PhiConvictionThreshold float64

	// HistorySize is how many heartbeat samples to keep for variance calculation.
	HistorySize int

	// MinStdDeviation is the minimum standard deviation to use.
	// Prevents overly sensitive detection when variance is low.
	MinStdDeviation time.Duration

	// QuarantineProbeInterval is how often to probe quarantined nodes.
	QuarantineProbeInterval time.Duration

	// QuarantineRecoveryThreshold: successful probes needed to leave quarantine.
	QuarantineRecoveryThreshold int

	// MaxQuarantineTime: if node doesn't recover in this time, remove it.
	MaxQuarantineTime time.Duration
}

// DefaultConfig returns production defaults.
func DefaultConfig() Config {
	return Config{
		HeartbeatInterval:           1 * time.Second,
		PhiThreshold:                8.0,
		PhiConvictionThreshold:      16.0,
		HistorySize:                 1000,
		MinStdDeviation:             100 * time.Millisecond,
		QuarantineProbeInterval:     5 * time.Second,
		QuarantineRecoveryThreshold: 3,
		MaxQuarantineTime:           10 * time.Minute,
	}
}

// =============================================================================
// Φ-ACCRUAL FAILURE DETECTOR
// =============================================================================

// FailureDetector implements the Φ-accrual failure detector.
type FailureDetector struct {
	config Config
	mu     sync.RWMutex

	// Per-node heartbeat tracking
	nodeStates map[types.NodeID]*NodeState

	// Callback for state changes
	callback StateChangeCallback

	// Metrics
	metrics *DetectorMetrics
}

// NodeState tracks heartbeat history for a single node.
type NodeState struct {
	NodeID types.NodeID

	// Heartbeat interval samples (for variance calculation)
	intervals *SamplingWindow

	// Last heartbeat timestamp
	lastHeartbeat time.Time

	// Current state
	state     HealthState
	stateTime time.Time

	// Quarantine tracking
	quarantinedAt    time.Time
	successfulProbes int

	// Statistics
	phi float64

	mu sync.Mutex
}

// HealthState represents a node's health status.
type HealthState int

const (
	StateHealthy HealthState = iota
	StateSuspect
	StateQuarantined
	StateDead
)

func (s HealthState) String() string {
	switch s {
	case StateHealthy:
		return "HEALTHY"
	case StateSuspect:
		return "SUSPECT"
	case StateQuarantined:
		return "QUARANTINED"
	case StateDead:
		return "DEAD"
	default:
		return "UNKNOWN"
	}
}

// StateChangeCallback is called when a node's state changes.
type StateChangeCallback func(nodeID types.NodeID, oldState, newState HealthState)

// NewFailureDetector creates a new Φ-accrual failure detector.
func NewFailureDetector(config Config, callback StateChangeCallback) *FailureDetector {
	return &FailureDetector{
		config:     config,
		nodeStates: make(map[types.NodeID]*NodeState),
		callback:   callback,
		metrics:    NewDetectorMetrics(),
	}
}

// RegisterNode adds a node to be monitored.
func (fd *FailureDetector) RegisterNode(nodeID types.NodeID) {
	fd.mu.Lock()
	defer fd.mu.Unlock()

	if _, exists := fd.nodeStates[nodeID]; exists {
		return
	}

	fd.nodeStates[nodeID] = &NodeState{
		NodeID:        nodeID,
		intervals:     NewSamplingWindow(fd.config.HistorySize),
		lastHeartbeat: time.Now(),
		state:         StateHealthy,
		stateTime:     time.Now(),
	}
}

// UnregisterNode removes a node from monitoring.
func (fd *FailureDetector) UnregisterNode(nodeID types.NodeID) {
	fd.mu.Lock()
	defer fd.mu.Unlock()
	delete(fd.nodeStates, nodeID)
}

// RecordHeartbeat records a heartbeat from a node.
func (fd *FailureDetector) RecordHeartbeat(nodeID types.NodeID) {
	fd.mu.RLock()
	state, ok := fd.nodeStates[nodeID]
	fd.mu.RUnlock()

	if !ok {
		return
	}

	state.mu.Lock()
	defer state.mu.Unlock()

	now := time.Now()

	// Record interval since last heartbeat
	if !state.lastHeartbeat.IsZero() {
		interval := now.Sub(state.lastHeartbeat)
		state.intervals.Add(float64(interval.Nanoseconds()))
	}

	state.lastHeartbeat = now

	// If node was suspect or quarantined, check for recovery
	if state.state == StateSuspect {
		fd.transitionState(state, StateHealthy)
	} else if state.state == StateQuarantined {
		state.successfulProbes++
		if state.successfulProbes >= fd.config.QuarantineRecoveryThreshold {
			fd.transitionState(state, StateHealthy)
		}
	}

	fd.metrics.HeartbeatsReceived.Inc()
}

// GetPhi returns the current φ value for a node.
// Higher φ = more suspicious that the node has failed.
//
// The calculation is based on the probability that we would NOT have received
// a heartbeat by now if the node were alive. This probability is computed
// using the normal distribution of heartbeat intervals.
//
// Interpretation:
//   φ = 1: 10% probability of being a false positive
//   φ = 2: 1% probability
//   φ = 3: 0.1% probability
//   ...and so on (exponentially decreasing)
func (fd *FailureDetector) GetPhi(nodeID types.NodeID) float64 {
	fd.mu.RLock()
	state, ok := fd.nodeStates[nodeID]
	fd.mu.RUnlock()

	if !ok {
		return 0
	}

	state.mu.Lock()
	defer state.mu.Unlock()

	if state.intervals.Count() < 2 {
		// Not enough samples, assume healthy
		return 0
	}

	now := time.Now()
	timeSinceLastHB := now.Sub(state.lastHeartbeat)

	mean := state.intervals.Mean()
	stdDev := state.intervals.StdDev()

	// Enforce minimum standard deviation
	minStdDev := float64(fd.config.MinStdDeviation.Nanoseconds())
	if stdDev < minStdDev {
		stdDev = minStdDev
	}

	// Calculate the probability that the heartbeat is late by this much
	// Using the cumulative distribution function (CDF) of normal distribution
	y := (float64(timeSinceLastHB.Nanoseconds()) - mean) / stdDev
	phi := -math.Log10(1.0 - cdf(y))

	// Clamp to reasonable bounds
	if phi < 0 {
		phi = 0
	}
	if phi > 100 {
		phi = 100
	}

	state.phi = phi
	return phi
}

// cdf computes the cumulative distribution function of the standard normal distribution.
// Uses an approximation that's accurate to ~10^-7.
func cdf(x float64) float64 {
	// Approximation using the error function
	return 0.5 * (1.0 + erf(x/math.Sqrt2))
}

// erf approximates the error function.
func erf(x float64) float64 {
	// Horner's method for polynomial approximation
	t := 1.0 / (1.0 + 0.5*math.Abs(x))

	tau := t * math.Exp(-x*x-1.26551223+
		t*(1.00002368+
			t*(0.37409196+
				t*(0.09678418+
					t*(-0.18628806+
						t*(0.27886807+
							t*(-1.13520398+
								t*(1.48851587+
									t*(-0.82215223+
										t*0.17087277)))))))))

	if x >= 0 {
		return 1 - tau
	}
	return tau - 1
}

// CheckNodes evaluates all nodes and updates their states.
// Should be called periodically (e.g., every second).
func (fd *FailureDetector) CheckNodes() []StateChange {
	fd.mu.RLock()
	nodes := make([]*NodeState, 0, len(fd.nodeStates))
	for _, state := range fd.nodeStates {
		nodes = append(nodes, state)
	}
	fd.mu.RUnlock()

	var changes []StateChange

	for _, state := range nodes {
		phi := fd.GetPhi(state.NodeID)

		state.mu.Lock()
		oldState := state.state

		switch state.state {
		case StateHealthy:
			if phi >= fd.config.PhiConvictionThreshold {
				// Direct to quarantine if very high phi
				fd.transitionState(state, StateQuarantined)
			} else if phi >= fd.config.PhiThreshold {
				fd.transitionState(state, StateSuspect)
			}

		case StateSuspect:
			if phi < fd.config.PhiThreshold {
				fd.transitionState(state, StateHealthy)
			} else if phi >= fd.config.PhiConvictionThreshold {
				fd.transitionState(state, StateQuarantined)
			}

		case StateQuarantined:
			// Check if max quarantine time exceeded
			if time.Since(state.quarantinedAt) > fd.config.MaxQuarantineTime {
				fd.transitionState(state, StateDead)
			}
		}

		if state.state != oldState {
			changes = append(changes, StateChange{
				NodeID:    state.NodeID,
				OldState:  oldState,
				NewState:  state.state,
				Timestamp: time.Now(),
				Phi:       phi,
			})
		}

		state.mu.Unlock()
	}

	return changes
}

// StateChange records a node state transition.
type StateChange struct {
	NodeID    types.NodeID
	OldState  HealthState
	NewState  HealthState
	Timestamp time.Time
	Phi       float64
}

func (fd *FailureDetector) transitionState(state *NodeState, newState HealthState) {
	oldState := state.state
	state.state = newState
	state.stateTime = time.Now()

	if newState == StateQuarantined {
		state.quarantinedAt = time.Now()
		state.successfulProbes = 0
	}

	if fd.callback != nil {
		go fd.callback(state.NodeID, oldState, newState)
	}

	fd.metrics.StateTransitions.Inc()
}

// GetNodeState returns the current state of a node.
func (fd *FailureDetector) GetNodeState(nodeID types.NodeID) (HealthState, bool) {
	fd.mu.RLock()
	state, ok := fd.nodeStates[nodeID]
	fd.mu.RUnlock()

	if !ok {
		return StateHealthy, false
	}

	state.mu.Lock()
	defer state.mu.Unlock()
	return state.state, true
}

// =============================================================================
// SAMPLING WINDOW
// =============================================================================

// SamplingWindow maintains a fixed-size window of samples for statistics.
type SamplingWindow struct {
	samples []float64
	head    int
	count   int
	sum     float64
	sumSq   float64
	mu      sync.Mutex
}

// NewSamplingWindow creates a new sampling window.
func NewSamplingWindow(size int) *SamplingWindow {
	return &SamplingWindow{
		samples: make([]float64, size),
	}
}

// Add adds a new sample to the window.
func (w *SamplingWindow) Add(value float64) {
	w.mu.Lock()
	defer w.mu.Unlock()

	// If window is full, remove oldest value from stats
	if w.count >= len(w.samples) {
		old := w.samples[w.head]
		w.sum -= old
		w.sumSq -= old * old
	} else {
		w.count++
	}

	// Add new value
	w.samples[w.head] = value
	w.sum += value
	w.sumSq += value * value
	w.head = (w.head + 1) % len(w.samples)
}

// Mean returns the mean of samples in the window.
func (w *SamplingWindow) Mean() float64 {
	w.mu.Lock()
	defer w.mu.Unlock()

	if w.count == 0 {
		return 0
	}
	return w.sum / float64(w.count)
}

// StdDev returns the standard deviation of samples.
func (w *SamplingWindow) StdDev() float64 {
	w.mu.Lock()
	defer w.mu.Unlock()

	if w.count < 2 {
		return 0
	}

	n := float64(w.count)
	variance := (w.sumSq - (w.sum*w.sum)/n) / (n - 1)
	if variance < 0 {
		variance = 0 // Handle floating point errors
	}
	return math.Sqrt(variance)
}

// Count returns the number of samples in the window.
func (w *SamplingWindow) Count() int {
	w.mu.Lock()
	defer w.mu.Unlock()
	return w.count
}

// =============================================================================
// SELF-HEALING MANAGER
// =============================================================================

// SelfHealingManager coordinates recovery from failures.
type SelfHealingManager struct {
	config   Config
	detector *FailureDetector

	// Callback for healing actions
	onQuarantine func(nodeID types.NodeID)
	onRecover    func(nodeID types.NodeID)
	onRemove     func(nodeID types.NodeID)

	// Queue of nodes needing rebalancing
	rebalanceQueue chan types.NodeID

	// Metrics
	metrics *HealingMetrics

	mu      sync.Mutex
	running bool
	stopCh  chan struct{}
}

// NewSelfHealingManager creates a new self-healing manager.
func NewSelfHealingManager(
	config Config,
	detector *FailureDetector,
	onQuarantine, onRecover, onRemove func(types.NodeID),
) *SelfHealingManager {
	return &SelfHealingManager{
		config:         config,
		detector:       detector,
		onQuarantine:   onQuarantine,
		onRecover:      onRecover,
		onRemove:       onRemove,
		rebalanceQueue: make(chan types.NodeID, 100),
		metrics:        NewHealingMetrics(),
		stopCh:         make(chan struct{}),
	}
}

// Start begins the self-healing loop.
func (m *SelfHealingManager) Start(ctx context.Context) {
	m.mu.Lock()
	if m.running {
		m.mu.Unlock()
		return
	}
	m.running = true
	m.mu.Unlock()

	go m.checkLoop(ctx)
	go m.rebalanceLoop(ctx)
	go m.probeQuarantinedNodes(ctx)
}

// checkLoop periodically checks all nodes.
func (m *SelfHealingManager) checkLoop(ctx context.Context) {
	ticker := time.NewTicker(1 * time.Second)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			return
		case <-m.stopCh:
			return
		case <-ticker.C:
			changes := m.detector.CheckNodes()
			for _, change := range changes {
				m.handleStateChange(change)
			}
		}
	}
}

func (m *SelfHealingManager) handleStateChange(change StateChange) {
	switch change.NewState {
	case StateQuarantined:
		if m.onQuarantine != nil {
			m.onQuarantine(change.NodeID)
		}
		// Queue for rebalancing
		select {
		case m.rebalanceQueue <- change.NodeID:
		default:
		}
		m.metrics.NodesQuarantined.Inc()

	case StateHealthy:
		if change.OldState == StateQuarantined {
			if m.onRecover != nil {
				m.onRecover(change.NodeID)
			}
			m.metrics.NodesRecovered.Inc()
		}

	case StateDead:
		if m.onRemove != nil {
			m.onRemove(change.NodeID)
		}
		m.metrics.NodesRemoved.Inc()
	}
}

// rebalanceLoop handles rebalancing when nodes are quarantined.
func (m *SelfHealingManager) rebalanceLoop(ctx context.Context) {
	for {
		select {
		case <-ctx.Done():
			return
		case <-m.stopCh:
			return
		case nodeID := <-m.rebalanceQueue:
			m.rebalanceFromNode(ctx, nodeID)
		}
	}
}

func (m *SelfHealingManager) rebalanceFromNode(ctx context.Context, nodeID types.NodeID) {
	// TODO: Implement actual rebalancing logic:
	// 1. Get all shards/tasks assigned to this node
	// 2. Reassign to healthy nodes
	// 3. Ensure replication factor is maintained
	m.metrics.RebalancesTriggered.Inc()
}

// probeQuarantinedNodes periodically probes quarantined nodes.
func (m *SelfHealingManager) probeQuarantinedNodes(ctx context.Context) {
	ticker := time.NewTicker(m.config.QuarantineProbeInterval)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			return
		case <-m.stopCh:
			return
		case <-ticker.C:
			m.probeAll(ctx)
		}
	}
}

func (m *SelfHealingManager) probeAll(ctx context.Context) {
	// Get all quarantined nodes
	// TODO: Get list from detector
	// For each, try to ping
	// Success counts toward recovery threshold
}

// Stop stops the self-healing manager.
func (m *SelfHealingManager) Stop() {
	m.mu.Lock()
	defer m.mu.Unlock()

	if !m.running {
		return
	}

	m.running = false
	close(m.stopCh)
}

// =============================================================================
// CHAOS PATTERNS
// =============================================================================

// ChaosInjector injects failures for chaos testing.
type ChaosInjector struct {
	mu       sync.Mutex
	failures map[string]ChaosFailure
}

// ChaosFailure describes an injected failure.
type ChaosFailure struct {
	Type       FailureType
	Target     string
	Probability float64
	Duration   time.Duration
	StartTime  time.Time
}

// FailureType identifies the kind of injected failure.
type FailureType int

const (
	FailureNetworkDelay FailureType = iota
	FailureNetworkDrop
	FailureNetworkPartition
	FailureCPUStress
	FailureMemoryPressure
	FailureDiskSlow
	FailureProcessCrash
)

// NewChaosInjector creates a chaos injector.
func NewChaosInjector() *ChaosInjector {
	return &ChaosInjector{
		failures: make(map[string]ChaosFailure),
	}
}

// InjectFailure starts injecting a failure.
func (c *ChaosInjector) InjectFailure(id string, failure ChaosFailure) {
	c.mu.Lock()
	defer c.mu.Unlock()

	failure.StartTime = time.Now()
	c.failures[id] = failure
}

// RemoveFailure stops an injected failure.
func (c *ChaosInjector) RemoveFailure(id string) {
	c.mu.Lock()
	defer c.mu.Unlock()
	delete(c.failures, id)
}

// ShouldFail checks if an operation should fail.
func (c *ChaosInjector) ShouldFail(target string) *ChaosFailure {
	c.mu.Lock()
	defer c.mu.Unlock()

	for _, failure := range c.failures {
		if failure.Target != target && failure.Target != "*" {
			continue
		}

		// Check if failure has expired
		if failure.Duration > 0 && time.Since(failure.StartTime) > failure.Duration {
			continue
		}

		// Probabilistic failure
		// In real implementation, use a proper RNG
		return &failure
	}

	return nil
}

// =============================================================================
// METRICS
// =============================================================================

// DetectorMetrics tracks failure detector metrics.
type DetectorMetrics struct {
	HeartbeatsReceived AtomicCounter
	StateTransitions   AtomicCounter
	FalsePositives     AtomicCounter
	FalseNegatives     AtomicCounter
}

func NewDetectorMetrics() *DetectorMetrics {
	return &DetectorMetrics{}
}

// HealingMetrics tracks self-healing metrics.
type HealingMetrics struct {
	NodesQuarantined    AtomicCounter
	NodesRecovered      AtomicCounter
	NodesRemoved        AtomicCounter
	RebalancesTriggered AtomicCounter
	RebalancesCompleted AtomicCounter
}

func NewHealingMetrics() *HealingMetrics {
	return &HealingMetrics{}
}

type AtomicCounter struct {
	value int64
	mu    sync.Mutex
}

func (c *AtomicCounter) Inc() {
	c.mu.Lock()
	c.value++
	c.mu.Unlock()
}

func (c *AtomicCounter) Value() int64 {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.value
}
