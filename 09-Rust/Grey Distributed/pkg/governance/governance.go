// Package governance implements resource management and quota enforcement.
//
// # Design Philosophy
//
// Resource governance in Grey Distributed serves three purposes:
// 1. Protection: Prevent any single tenant from monopolizing cluster resources
// 2. Fairness: Ensure each tenant gets their fair share based on quotas
// 3. Efficiency: Maximize utilization while respecting constraints
//
// # Integration with Grey Optimizer
//
// This module integrates with Grey Optimizer for:
// - Predictive scaling based on historical patterns
// - Hotspot detection and proactive rebalancing
// - Cost optimization across deployment regions
//
// # Throttling Model
//
// Rather than hard rejections (which create poor user experience), we use
// graduated throttling:
// - Green Zone (<70% quota): No throttling
// - Yellow Zone (70-90%): Introduce scheduling delays
// - Red Zone (90-100%): Queue requests, shed load gracefully
// - Emergency (>100%): Preempt lowest-priority work
package governance

import (
	"context"
	"errors"
	"math"
	"sync"
	"time"

	"github.com/grey-systems/grey-distributed/pkg/types"
)

// =============================================================================
// CONFIGURATION
// =============================================================================

// Config contains governance configuration.
type Config struct {
	// Threshold percentages for throttling zones
	YellowThreshold float64 // Default: 0.70 (70%)
	RedThreshold    float64 // Default: 0.90 (90%)
	
	// MaxThrottleDelay caps the scheduling delay in yellow zone
	MaxThrottleDelay time.Duration // Default: 5s
	
	// RedZoneQueueLimit is max requests queued in red zone before shedding
	RedZoneQueueLimit int // Default: 1000
	
	// PreemptionEnabled allows emergency preemption when over quota
	PreemptionEnabled bool
	
	// HotspotDetectionWindow is the window for hotspot analysis
	HotspotDetectionWindow time.Duration // Default: 5m
	
	// HotspotThreshold: Node is "hot" if usage exceeds this for the window
	HotspotThreshold float64 // Default: 0.85 (85%)
	
	// PredictiveScalingEnabled enables Grey Optimizer integration
	PredictiveScalingEnabled bool
}

// DefaultConfig returns production defaults.
func DefaultConfig() Config {
	return Config{
		YellowThreshold:          0.70,
		RedThreshold:             0.90,
		MaxThrottleDelay:         5 * time.Second,
		RedZoneQueueLimit:        1000,
		PreemptionEnabled:        true,
		HotspotDetectionWindow:   5 * time.Minute,
		HotspotThreshold:         0.85,
		PredictiveScalingEnabled: false,
	}
}

// =============================================================================
// RESOURCE GOVERNOR
// =============================================================================

// Governor manages cluster-wide resource governance.
type Governor struct {
	config Config
	mu     sync.RWMutex
	
	// Cluster-wide limits
	clusterLimits ResourceLimits
	
	// Per-tenant state
	tenants map[types.TenantID]*TenantGovernance
	
	// Hotspot detection
	nodeMetrics map[types.NodeID]*NodeMetricsWindow
	
	// Throttling state
	throttleState map[types.TenantID]*ThrottleState
	
	// Metrics
	metrics *GovernanceMetrics
}

// ResourceLimits defines absolute cluster-wide resource ceilings.
type ResourceLimits struct {
	// Maximum resources available cluster-wide
	MaxCPUMillicores       int64
	MaxMemoryBytes         int64
	MaxDiskBytes           int64
	MaxConcurrentTasks     int64
	
	// Reserved for system operations (scheduling, consensus, etc.)
	ReservedCPUMillicores  int64
	ReservedMemoryBytes    int64
}

// TenantGovernance tracks governance state for a tenant.
type TenantGovernance struct {
	TenantID     types.TenantID
	Quota        types.ResourceQuota
	CurrentUsage types.ResourceUsage
	
	// Burst tracking
	BurstBudget  types.ResourceUsage
	BurstExpiry  time.Time
	
	// Historical usage for trending
	UsageHistory *UsageHistoryRing
	
	// Throttle state
	Zone         ThrottleZone
	LastThrottle time.Time
}

// ThrottleZone indicates current throttling level.
type ThrottleZone int

const (
	ZoneGreen ThrottleZone = iota
	ZoneYellow
	ZoneRed
	ZoneEmergency
)

func (z ThrottleZone) String() string {
	switch z {
	case ZoneGreen:
		return "GREEN"
	case ZoneYellow:
		return "YELLOW"
	case ZoneRed:
		return "RED"
	case ZoneEmergency:
		return "EMERGENCY"
	default:
		return "UNKNOWN"
	}
}

// ThrottleState tracks current throttling for a tenant.
type ThrottleState struct {
	Zone          ThrottleZone
	QueuedCount   int
	LastUpdated   time.Time
	ThrottleUntil time.Time
}

// NewGovernor creates a new resource governor.
func NewGovernor(config Config) *Governor {
	return &Governor{
		config:        config,
		tenants:       make(map[types.TenantID]*TenantGovernance),
		nodeMetrics:   make(map[types.NodeID]*NodeMetricsWindow),
		throttleState: make(map[types.TenantID]*ThrottleState),
		metrics:       NewGovernanceMetrics(),
	}
}

// =============================================================================
// PUBLIC API
// =============================================================================

// CheckQuota determines if a resource request should be allowed.
// Returns a QuotaDecision with throttling information.
func (g *Governor) CheckQuota(ctx context.Context, tenantID types.TenantID, request types.ResourceRequest) (*QuotaDecision, error) {
	g.mu.Lock()
	defer g.mu.Unlock()
	
	tenant, ok := g.tenants[tenantID]
	if !ok {
		return nil, ErrTenantNotFound
	}
	
	decision := &QuotaDecision{
		TenantID:  tenantID,
		Allowed:   false,
		Request:   request,
		Timestamp: time.Now(),
	}
	
	// Calculate usage percentage after this request
	futureUsage := types.ResourceUsage{
		CPUMillicores: tenant.CurrentUsage.CPUMillicores + request.CPUMillicores,
		MemoryBytes:   tenant.CurrentUsage.MemoryBytes + request.MemoryBytes,
	}
	
	cpuPct := float64(futureUsage.CPUMillicores) / float64(tenant.Quota.MaxCPUMillicores)
	memPct := float64(futureUsage.MemoryBytes) / float64(tenant.Quota.MaxMemoryBytes)
	maxPct := math.Max(cpuPct, memPct)
	
	// Determine zone based on usage
	zone := g.determineZone(maxPct)
	decision.Zone = zone
	tenant.Zone = zone
	
	switch zone {
	case ZoneGreen:
		decision.Allowed = true
		decision.Delay = 0
		g.metrics.GreenZoneRequests.Inc()
		
	case ZoneYellow:
		// Allow with delay proportional to overage
		decision.Allowed = true
		overage := maxPct - g.config.YellowThreshold
		delayPct := overage / (g.config.RedThreshold - g.config.YellowThreshold)
		decision.Delay = time.Duration(float64(g.config.MaxThrottleDelay) * delayPct)
		g.metrics.YellowZoneRequests.Inc()
		
	case ZoneRed:
		// Check queue limit
		state := g.getOrCreateThrottleState(tenantID)
		if state.QueuedCount < g.config.RedZoneQueueLimit {
			decision.Allowed = true
			decision.Queued = true
			state.QueuedCount++
			g.metrics.RedZoneRequests.Inc()
		} else {
			decision.Allowed = false
			decision.Reason = "queue limit exceeded"
			g.metrics.RejectedRequests.Inc()
		}
		
	case ZoneEmergency:
		// Only allow if preemption is possible
		if g.config.PreemptionEnabled {
			decision.Allowed = true
			decision.RequiresPreemption = true
			g.metrics.EmergencyPreemptions.Inc()
		} else {
			decision.Allowed = false
			decision.Reason = "over quota, preemption disabled"
			g.metrics.RejectedRequests.Inc()
		}
	}
	
	return decision, nil
}

// QuotaDecision contains the result of a quota check.
type QuotaDecision struct {
	TenantID           types.TenantID
	Allowed            bool
	Request            types.ResourceRequest
	Zone               ThrottleZone
	Delay              time.Duration
	Queued             bool
	RequiresPreemption bool
	Reason             string
	Timestamp          time.Time
}

// RecordUsage updates resource usage for a tenant.
func (g *Governor) RecordUsage(tenantID types.TenantID, delta types.ResourceUsage) error {
	g.mu.Lock()
	defer g.mu.Unlock()
	
	tenant, ok := g.tenants[tenantID]
	if !ok {
		return ErrTenantNotFound
	}
	
	tenant.CurrentUsage.CPUMillicores += delta.CPUMillicores
	tenant.CurrentUsage.MemoryBytes += delta.MemoryBytes
	tenant.CurrentUsage.DiskBytes += delta.DiskBytes
	
	// Record in history for trending
	if tenant.UsageHistory != nil {
		tenant.UsageHistory.Record(time.Now(), tenant.CurrentUsage)
	}
	
	return nil
}

// ReleaseUsage decrements resource usage for a tenant.
func (g *Governor) ReleaseUsage(tenantID types.TenantID, delta types.ResourceUsage) error {
	g.mu.Lock()
	defer g.mu.Unlock()
	
	tenant, ok := g.tenants[tenantID]
	if !ok {
		return ErrTenantNotFound
	}
	
	tenant.CurrentUsage.CPUMillicores -= delta.CPUMillicores
	tenant.CurrentUsage.MemoryBytes -= delta.MemoryBytes
	tenant.CurrentUsage.DiskBytes -= delta.DiskBytes
	
	// Ensure we don't go negative (shouldn't happen, but safety first)
	if tenant.CurrentUsage.CPUMillicores < 0 {
		tenant.CurrentUsage.CPUMillicores = 0
	}
	if tenant.CurrentUsage.MemoryBytes < 0 {
		tenant.CurrentUsage.MemoryBytes = 0
	}
	
	// Check if we can dequeue throttled requests
	state := g.throttleState[tenantID]
	if state != nil && state.QueuedCount > 0 {
		state.QueuedCount--
	}
	
	return nil
}

// RegisterTenant adds a new tenant with their quota.
func (g *Governor) RegisterTenant(tenant types.Tenant) {
	g.mu.Lock()
	defer g.mu.Unlock()
	
	g.tenants[tenant.ID] = &TenantGovernance{
		TenantID:     tenant.ID,
		Quota:        tenant.Quota,
		UsageHistory: NewUsageHistoryRing(1000), // Keep last 1000 samples
		Zone:         ZoneGreen,
	}
}

// UpdateQuota changes a tenant's resource quota.
func (g *Governor) UpdateQuota(tenantID types.TenantID, quota types.ResourceQuota) error {
	g.mu.Lock()
	defer g.mu.Unlock()
	
	tenant, ok := g.tenants[tenantID]
	if !ok {
		return ErrTenantNotFound
	}
	
	tenant.Quota = quota
	return nil
}

// =============================================================================
// HOTSPOT DETECTION
// =============================================================================

// RecordNodeMetrics records metrics for hotspot detection.
func (g *Governor) RecordNodeMetrics(nodeID types.NodeID, metrics NodeMetricsSample) {
	g.mu.Lock()
	defer g.mu.Unlock()
	
	window, ok := g.nodeMetrics[nodeID]
	if !ok {
		window = NewNodeMetricsWindow(g.config.HotspotDetectionWindow)
		g.nodeMetrics[nodeID] = window
	}
	
	window.Record(metrics)
}

// NodeMetricsSample is a point-in-time node resource measurement.
type NodeMetricsSample struct {
	Timestamp        time.Time
	CPUUtilization   float64 // 0.0 to 1.0
	MemoryUtilization float64
	DiskIOPS         int64
	NetworkBandwidth int64
}

// DetectHotspots returns nodes that are overloaded.
func (g *Governor) DetectHotspots() []HotspotReport {
	g.mu.RLock()
	defer g.mu.RUnlock()
	
	var hotspots []HotspotReport
	
	for nodeID, window := range g.nodeMetrics {
		avg := window.AverageUtilization()
		
		if avg.CPU > g.config.HotspotThreshold || avg.Memory > g.config.HotspotThreshold {
			hotspots = append(hotspots, HotspotReport{
				NodeID:            nodeID,
				CPUUtilization:    avg.CPU,
				MemoryUtilization: avg.Memory,
				Duration:          g.config.HotspotDetectionWindow,
				Severity:          g.calculateSeverity(avg),
			})
		}
	}
	
	g.metrics.HotspotsDetected.Add(int64(len(hotspots)))
	return hotspots
}

// HotspotReport describes an overloaded node.
type HotspotReport struct {
	NodeID            types.NodeID
	CPUUtilization    float64
	MemoryUtilization float64
	Duration          time.Duration
	Severity          HotspotSeverity
}

// HotspotSeverity indicates how severe the hotspot is.
type HotspotSeverity int

const (
	SeverityLow HotspotSeverity = iota
	SeverityMedium
	SeverityHigh
	SeverityCritical
)

func (g *Governor) calculateSeverity(avg UtilizationAverage) HotspotSeverity {
	maxUtil := math.Max(avg.CPU, avg.Memory)
	switch {
	case maxUtil > 0.95:
		return SeverityCritical
	case maxUtil > 0.90:
		return SeverityHigh
	case maxUtil > 0.85:
		return SeverityMedium
	default:
		return SeverityLow
	}
}

// =============================================================================
// PREDICTIVE SCALING
// =============================================================================

// PredictResourceNeeds uses historical data to predict future resource needs.
// This integrates with Grey Optimizer for ML-based predictions.
func (g *Governor) PredictResourceNeeds(tenantID types.TenantID, horizon time.Duration) (*ResourcePrediction, error) {
	g.mu.RLock()
	tenant, ok := g.tenants[tenantID]
	g.mu.RUnlock()
	
	if !ok {
		return nil, ErrTenantNotFound
	}
	
	if tenant.UsageHistory == nil || tenant.UsageHistory.Count() < 10 {
		return nil, ErrInsufficientData
	}
	
	// Simple linear regression for prediction
	// In production, this would call Grey Optimizer ML service
	history := tenant.UsageHistory.GetAll()
	
	// Calculate trend
	cpuTrend := g.calculateLinearTrend(history, func(u types.ResourceUsage) float64 {
		return float64(u.CPUMillicores)
	})
	memTrend := g.calculateLinearTrend(history, func(u types.ResourceUsage) float64 {
		return float64(u.MemoryBytes)
	})
	
	// Project forward
	hoursAhead := horizon.Hours()
	
	prediction := &ResourcePrediction{
		TenantID:  tenantID,
		Horizon:   horizon,
		Timestamp: time.Now(),
		PredictedUsage: types.ResourceUsage{
			CPUMillicores: tenant.CurrentUsage.CPUMillicores + int64(cpuTrend*hoursAhead),
			MemoryBytes:   tenant.CurrentUsage.MemoryBytes + int64(memTrend*hoursAhead),
		},
		Confidence: 0.7, // Placeholder confidence
	}
	
	// Determine recommended action
	pctOfQuotaCPU := float64(prediction.PredictedUsage.CPUMillicores) / float64(tenant.Quota.MaxCPUMillicores)
	pctOfQuotaMem := float64(prediction.PredictedUsage.MemoryBytes) / float64(tenant.Quota.MaxMemoryBytes)
	maxPct := math.Max(pctOfQuotaCPU, pctOfQuotaMem)
	
	if maxPct > 0.90 {
		prediction.Action = ActionScaleUp
		prediction.RecommendedIncrease = types.ResourceUsage{
			CPUMillicores: int64(float64(tenant.Quota.MaxCPUMillicores) * 0.25),
			MemoryBytes:   int64(float64(tenant.Quota.MaxMemoryBytes) * 0.25),
		}
	} else if maxPct < 0.30 {
		prediction.Action = ActionScaleDown
	} else {
		prediction.Action = ActionNone
	}
	
	return prediction, nil
}

// ResourcePrediction contains predicted resource needs.
type ResourcePrediction struct {
	TenantID            types.TenantID
	Horizon             time.Duration
	Timestamp           time.Time
	PredictedUsage      types.ResourceUsage
	Confidence          float64
	Action              ScalingAction
	RecommendedIncrease types.ResourceUsage
}

// ScalingAction is a recommended scaling action.
type ScalingAction int

const (
	ActionNone ScalingAction = iota
	ActionScaleUp
	ActionScaleDown
)

// calculateLinearTrend calculates the hourly change rate.
func (g *Governor) calculateLinearTrend(history []UsageHistoryEntry, getValue func(types.ResourceUsage) float64) float64 {
	if len(history) < 2 {
		return 0
	}
	
	// Simple linear regression
	n := float64(len(history))
	var sumX, sumY, sumXY, sumX2 float64
	
	for i, entry := range history {
		x := float64(i)
		y := getValue(entry.Usage)
		sumX += x
		sumY += y
		sumXY += x * y
		sumX2 += x * x
	}
	
	slope := (n*sumXY - sumX*sumY) / (n*sumX2 - sumX*sumX)
	
	// Convert to hourly rate
	if len(history) >= 2 {
		duration := history[len(history)-1].Timestamp.Sub(history[0].Timestamp)
		if duration > 0 {
			samplesPerHour := float64(len(history)) / duration.Hours()
			slope *= samplesPerHour
		}
	}
	
	return slope
}

// =============================================================================
// HELPER DATA STRUCTURES
// =============================================================================

// NodeMetricsWindow maintains a sliding window of node metrics.
type NodeMetricsWindow struct {
	windowDuration time.Duration
	samples        []NodeMetricsSample
	mu             sync.Mutex
}

func NewNodeMetricsWindow(duration time.Duration) *NodeMetricsWindow {
	return &NodeMetricsWindow{
		windowDuration: duration,
		samples:        make([]NodeMetricsSample, 0, 100),
	}
}

func (w *NodeMetricsWindow) Record(sample NodeMetricsSample) {
	w.mu.Lock()
	defer w.mu.Unlock()
	
	w.samples = append(w.samples, sample)
	
	// Trim old samples
	cutoff := time.Now().Add(-w.windowDuration)
	startIdx := 0
	for i, s := range w.samples {
		if s.Timestamp.After(cutoff) {
			startIdx = i
			break
		}
	}
	w.samples = w.samples[startIdx:]
}

// UtilizationAverage holds average utilization values.
type UtilizationAverage struct {
	CPU    float64
	Memory float64
}

func (w *NodeMetricsWindow) AverageUtilization() UtilizationAverage {
	w.mu.Lock()
	defer w.mu.Unlock()
	
	if len(w.samples) == 0 {
		return UtilizationAverage{}
	}
	
	var cpuSum, memSum float64
	for _, s := range w.samples {
		cpuSum += s.CPUUtilization
		memSum += s.MemoryUtilization
	}
	
	n := float64(len(w.samples))
	return UtilizationAverage{
		CPU:    cpuSum / n,
		Memory: memSum / n,
	}
}

// UsageHistoryRing is a ring buffer for usage history.
type UsageHistoryRing struct {
	entries []UsageHistoryEntry
	head    int
	count   int
	mu      sync.Mutex
}

// UsageHistoryEntry is a single usage sample.
type UsageHistoryEntry struct {
	Timestamp time.Time
	Usage     types.ResourceUsage
}

func NewUsageHistoryRing(capacity int) *UsageHistoryRing {
	return &UsageHistoryRing{
		entries: make([]UsageHistoryEntry, capacity),
	}
}

func (r *UsageHistoryRing) Record(t time.Time, usage types.ResourceUsage) {
	r.mu.Lock()
	defer r.mu.Unlock()
	
	r.entries[r.head] = UsageHistoryEntry{Timestamp: t, Usage: usage}
	r.head = (r.head + 1) % len(r.entries)
	if r.count < len(r.entries) {
		r.count++
	}
}

func (r *UsageHistoryRing) GetAll() []UsageHistoryEntry {
	r.mu.Lock()
	defer r.mu.Unlock()
	
	result := make([]UsageHistoryEntry, r.count)
	for i := 0; i < r.count; i++ {
		idx := (r.head - r.count + i + len(r.entries)) % len(r.entries)
		result[i] = r.entries[idx]
	}
	return result
}

func (r *UsageHistoryRing) Count() int {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.count
}

func (g *Governor) determineZone(usagePct float64) ThrottleZone {
	switch {
	case usagePct < g.config.YellowThreshold:
		return ZoneGreen
	case usagePct < g.config.RedThreshold:
		return ZoneYellow
	case usagePct <= 1.0:
		return ZoneRed
	default:
		return ZoneEmergency
	}
}

func (g *Governor) getOrCreateThrottleState(tenantID types.TenantID) *ThrottleState {
	state, ok := g.throttleState[tenantID]
	if !ok {
		state = &ThrottleState{Zone: ZoneGreen, LastUpdated: time.Now()}
		g.throttleState[tenantID] = state
	}
	return state
}

// =============================================================================
// METRICS
// =============================================================================

// GovernanceMetrics tracks governance system metrics.
type GovernanceMetrics struct {
	GreenZoneRequests    AtomicCounter
	YellowZoneRequests   AtomicCounter
	RedZoneRequests      AtomicCounter
	RejectedRequests     AtomicCounter
	EmergencyPreemptions AtomicCounter
	HotspotsDetected     AtomicCounter
}

func NewGovernanceMetrics() *GovernanceMetrics {
	return &GovernanceMetrics{}
}

// AtomicCounter is a thread-safe counter.
type AtomicCounter struct {
	value int64
	mu    sync.Mutex
}

func (c *AtomicCounter) Inc() {
	c.mu.Lock()
	c.value++
	c.mu.Unlock()
}

func (c *AtomicCounter) Add(n int64) {
	c.mu.Lock()
	c.value += n
	c.mu.Unlock()
}

func (c *AtomicCounter) Value() int64 {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.value
}

// =============================================================================
// ERRORS
// =============================================================================

var (
	ErrTenantNotFound   = errors.New("tenant not found")
	ErrQuotaExceeded    = errors.New("quota exceeded")
	ErrInsufficientData = errors.New("insufficient historical data for prediction")
)
