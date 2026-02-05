// Package observability implements distributed tracing, causal logging, and replay.
//
// # Design Philosophy
//
// In distributed systems, understanding what happened and why is crucial.
// Traditional logging fails because:
// - Clocks are not synchronized across nodes
// - Events interleave in complex ways
// - Causality is not captured
//
// This package provides:
// 1. Distributed Tracing: Track requests across service boundaries
// 2. Causal Logging: Vector clocks establish happened-before relationships
// 3. Replay Engine: Reproduce execution for debugging
// 4. Proof Bundles: Cryptographic evidence of execution
//
// # Trace Context Propagation
//
// Every request carries a trace context:
// - Trace ID: 128-bit globally unique identifier
// - Span ID: 64-bit unique within trace
// - Parent Span ID: Links causal relationships
// - Baggage: Key-value pairs propagated through system
//
// This follows the W3C Trace Context standard for interoperability.
//
// # Vector Clocks
//
// Vector clocks establish causal ordering across nodes:
// - Each node maintains a counter
// - On send: increment own counter, attach vector
// - On receive: merge vectors (max each component), increment own
// - Comparison: e1 → e2 if all e1 components ≤ e2 (with at least one <)
package observability

import (
	"context"
	"crypto/rand"
	"encoding/binary"
	"encoding/hex"
	"encoding/json"
	"errors"
	"sync"
	"time"

	"github.com/grey-systems/grey-distributed/pkg/types"
)

// =============================================================================
// TRACE CONTEXT
// =============================================================================

// TraceID is a 128-bit globally unique trace identifier.
type TraceID [16]byte

// SpanID is a 64-bit span identifier, unique within a trace.
type SpanID [8]byte

// TraceContext carries distributed tracing information.
type TraceContext struct {
	// TraceID identifies the entire trace
	TraceID TraceID

	// SpanID identifies this specific span
	SpanID SpanID

	// ParentSpanID links to the parent span (zero if root)
	ParentSpanID SpanID

	// Flags for trace behavior
	Flags TraceFlags

	// Baggage: propagated key-value pairs
	Baggage map[string]string
}

// TraceFlags control tracing behavior.
type TraceFlags uint8

const (
	// FlagSampled indicates this trace should be sampled
	FlagSampled TraceFlags = 1 << 0

	// FlagDebug enables detailed logging
	FlagDebug TraceFlags = 1 << 1
)

// NewTraceContext creates a new root trace context.
func NewTraceContext() *TraceContext {
	var traceID TraceID
	var spanID SpanID
	rand.Read(traceID[:])
	rand.Read(spanID[:])

	return &TraceContext{
		TraceID: traceID,
		SpanID:  spanID,
		Flags:   FlagSampled,
		Baggage: make(map[string]string),
	}
}

// CreateChild creates a child span context.
func (tc *TraceContext) CreateChild() *TraceContext {
	var spanID SpanID
	rand.Read(spanID[:])

	// Copy baggage
	baggage := make(map[string]string, len(tc.Baggage))
	for k, v := range tc.Baggage {
		baggage[k] = v
	}

	return &TraceContext{
		TraceID:      tc.TraceID,
		SpanID:       spanID,
		ParentSpanID: tc.SpanID,
		Flags:        tc.Flags,
		Baggage:      baggage,
	}
}

// String returns the W3C traceparent format.
func (tc *TraceContext) String() string {
	return "00-" + hex.EncodeToString(tc.TraceID[:]) + "-" +
		hex.EncodeToString(tc.SpanID[:]) + "-" +
		hex.EncodeToString([]byte{byte(tc.Flags)})
}

// =============================================================================
// SPAN
// =============================================================================

// Span represents a unit of work within a trace.
type Span struct {
	// Context for this span
	Context *TraceContext

	// Name describes the operation
	Name string

	// Start and end times
	StartTime time.Time
	EndTime   time.Time

	// Status indicates success/failure
	Status SpanStatus

	// Attributes: key-value metadata
	Attributes map[string]interface{}

	// Events: timestamped annotations
	Events []SpanEvent

	// Links: references to other spans
	Links []SpanLink

	// Resource: describes the entity producing this span
	Resource *Resource

	mu sync.Mutex
}

// SpanStatus indicates the outcome of the operation.
type SpanStatus int

const (
	StatusUnset SpanStatus = iota
	StatusOK
	StatusError
)

// SpanEvent is a timestamped annotation within a span.
type SpanEvent struct {
	Timestamp  time.Time
	Name       string
	Attributes map[string]interface{}
}

// SpanLink references a related span.
type SpanLink struct {
	TraceID    TraceID
	SpanID     SpanID
	Attributes map[string]interface{}
}

// Resource describes the entity producing telemetry.
type Resource struct {
	ServiceName    string
	ServiceVersion string
	NodeID         types.NodeID
	Environment    string
}

// NewSpan creates a new span.
func NewSpan(ctx *TraceContext, name string, resource *Resource) *Span {
	return &Span{
		Context:    ctx,
		Name:       name,
		StartTime:  time.Now(),
		Status:     StatusUnset,
		Attributes: make(map[string]interface{}),
		Events:     make([]SpanEvent, 0),
		Links:      make([]SpanLink, 0),
		Resource:   resource,
	}
}

// SetAttribute adds an attribute to the span.
func (s *Span) SetAttribute(key string, value interface{}) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.Attributes[key] = value
}

// AddEvent adds a timestamped event.
func (s *Span) AddEvent(name string, attrs map[string]interface{}) {
	s.mu.Lock()
	defer s.mu.Unlock()

	s.Events = append(s.Events, SpanEvent{
		Timestamp:  time.Now(),
		Name:       name,
		Attributes: attrs,
	})
}

// End completes the span.
func (s *Span) End() {
	s.mu.Lock()
	defer s.mu.Unlock()

	s.EndTime = time.Now()
	if s.Status == StatusUnset {
		s.Status = StatusOK
	}
}

// SetError marks the span as failed.
func (s *Span) SetError(err error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	s.Status = StatusError
	s.Attributes["error"] = true
	s.Attributes["error.message"] = err.Error()
}

// Duration returns the span duration.
func (s *Span) Duration() time.Duration {
	if s.EndTime.IsZero() {
		return time.Since(s.StartTime)
	}
	return s.EndTime.Sub(s.StartTime)
}

// =============================================================================
// TRACER
// =============================================================================

// Tracer manages span creation and export.
type Tracer struct {
	resource *Resource
	sampler  Sampler
	exporter SpanExporter
	mu       sync.Mutex

	// Active spans for context propagation
	activeSpans map[TraceID][]*Span
}

// Sampler decides which traces to record.
type Sampler interface {
	ShouldSample(traceID TraceID, name string) bool
}

// SpanExporter exports completed spans.
type SpanExporter interface {
	Export(spans []*Span) error
	Shutdown(ctx context.Context) error
}

// NewTracer creates a new tracer.
func NewTracer(resource *Resource, sampler Sampler, exporter SpanExporter) *Tracer {
	return &Tracer{
		resource:    resource,
		sampler:     sampler,
		exporter:    exporter,
		activeSpans: make(map[TraceID][]*Span),
	}
}

// StartSpan starts a new span.
func (t *Tracer) StartSpan(ctx context.Context, name string) (context.Context, *Span) {
	// Get parent context if exists
	parentCtx := TraceContextFromContext(ctx)

	var traceCtx *TraceContext
	if parentCtx != nil {
		traceCtx = parentCtx.CreateChild()
	} else {
		traceCtx = NewTraceContext()
	}

	// Check sampling
	if !t.sampler.ShouldSample(traceCtx.TraceID, name) {
		traceCtx.Flags &^= FlagSampled
	}

	span := NewSpan(traceCtx, name, t.resource)

	// Store in active spans
	t.mu.Lock()
	t.activeSpans[traceCtx.TraceID] = append(t.activeSpans[traceCtx.TraceID], span)
	t.mu.Unlock()

	// Create new context with trace
	newCtx := ContextWithTraceContext(ctx, traceCtx)
	return newCtx, span
}

// EndSpan completes a span and exports it.
func (t *Tracer) EndSpan(span *Span) {
	span.End()

	// Export if sampled
	if span.Context.Flags&FlagSampled != 0 {
		t.exporter.Export([]*Span{span})
	}
}

// Context key for trace context
type traceContextKey struct{}

// TraceContextFromContext retrieves trace context from a Go context.
func TraceContextFromContext(ctx context.Context) *TraceContext {
	if tc, ok := ctx.Value(traceContextKey{}).(*TraceContext); ok {
		return tc
	}
	return nil
}

// ContextWithTraceContext adds trace context to a Go context.
func ContextWithTraceContext(ctx context.Context, tc *TraceContext) context.Context {
	return context.WithValue(ctx, traceContextKey{}, tc)
}

// =============================================================================
// VECTOR CLOCK
// =============================================================================

// VectorClock implements Lamport's vector clock for causal ordering.
type VectorClock struct {
	mu     sync.RWMutex
	clocks map[types.NodeID]uint64
}

// NewVectorClock creates a new vector clock.
func NewVectorClock() *VectorClock {
	return &VectorClock{
		clocks: make(map[types.NodeID]uint64),
	}
}

// Increment increments this node's clock component.
func (vc *VectorClock) Increment(nodeID types.NodeID) {
	vc.mu.Lock()
	defer vc.mu.Unlock()
	vc.clocks[nodeID]++
}

// Merge merges another vector clock into this one (component-wise max).
// Used when receiving a message.
func (vc *VectorClock) Merge(other *VectorClock) {
	vc.mu.Lock()
	defer vc.mu.Unlock()

	other.mu.RLock()
	defer other.mu.RUnlock()

	for nodeID, clock := range other.clocks {
		if clock > vc.clocks[nodeID] {
			vc.clocks[nodeID] = clock
		}
	}
}

// Get returns the clock value for a node.
func (vc *VectorClock) Get(nodeID types.NodeID) uint64 {
	vc.mu.RLock()
	defer vc.mu.RUnlock()
	return vc.clocks[nodeID]
}

// Copy creates a copy of the vector clock.
func (vc *VectorClock) Copy() *VectorClock {
	vc.mu.RLock()
	defer vc.mu.RUnlock()

	copy := NewVectorClock()
	for nodeID, clock := range vc.clocks {
		copy.clocks[nodeID] = clock
	}
	return copy
}

// HappenedBefore returns true if this clock happened before other.
// e1 → e2 if ∀i: e1[i] ≤ e2[i] and ∃j: e1[j] < e2[j]
func (vc *VectorClock) HappenedBefore(other *VectorClock) bool {
	vc.mu.RLock()
	defer vc.mu.RUnlock()

	other.mu.RLock()
	defer other.mu.RUnlock()

	hasStrict := false

	// Check all components in this clock
	for nodeID, clock := range vc.clocks {
		otherClock := other.clocks[nodeID]
		if clock > otherClock {
			return false
		}
		if clock < otherClock {
			hasStrict = true
		}
	}

	// Check components only in other clock
	for nodeID, otherClock := range other.clocks {
		if _, exists := vc.clocks[nodeID]; !exists && otherClock > 0 {
			hasStrict = true
		}
	}

	return hasStrict
}

// Concurrent returns true if neither clock happened before the other.
func (vc *VectorClock) Concurrent(other *VectorClock) bool {
	return !vc.HappenedBefore(other) && !other.HappenedBefore(vc)
}

// ToMap returns the vector clock as a map.
func (vc *VectorClock) ToMap() map[types.NodeID]uint64 {
	vc.mu.RLock()
	defer vc.mu.RUnlock()

	result := make(map[types.NodeID]uint64, len(vc.clocks))
	for k, v := range vc.clocks {
		result[k] = v
	}
	return result
}

// =============================================================================
// CAUSAL LOG
// =============================================================================

// CausalLog is a log with vector clock ordering.
type CausalLog struct {
	mu       sync.Mutex
	nodeID   types.NodeID
	clock    *VectorClock
	entries  []*CausalLogEntry
	exporter LogExporter
}

// CausalLogEntry is a log entry with causal ordering.
type CausalLogEntry struct {
	Timestamp   time.Time
	VectorClock map[types.NodeID]uint64
	NodeID      types.NodeID
	Level       LogLevel
	Message     string
	Fields      map[string]interface{}

	// Trace context for correlation
	TraceID TraceID
	SpanID  SpanID
}

// LogLevel indicates log severity.
type LogLevel int

const (
	LogDebug LogLevel = iota
	LogInfo
	LogWarn
	LogError
)

func (l LogLevel) String() string {
	switch l {
	case LogDebug:
		return "DEBUG"
	case LogInfo:
		return "INFO"
	case LogWarn:
		return "WARN"
	case LogError:
		return "ERROR"
	default:
		return "UNKNOWN"
	}
}

// LogExporter exports log entries.
type LogExporter interface {
	Export(entries []*CausalLogEntry) error
}

// NewCausalLog creates a new causal log.
func NewCausalLog(nodeID types.NodeID, exporter LogExporter) *CausalLog {
	return &CausalLog{
		nodeID:   nodeID,
		clock:    NewVectorClock(),
		entries:  make([]*CausalLogEntry, 0),
		exporter: exporter,
	}
}

// Log records an entry with the current vector clock.
func (l *CausalLog) Log(level LogLevel, message string, fields map[string]interface{}) {
	l.LogWithContext(nil, level, message, fields)
}

// LogWithContext logs with trace context.
func (l *CausalLog) LogWithContext(ctx context.Context, level LogLevel, message string, fields map[string]interface{}) {
	l.mu.Lock()
	defer l.mu.Unlock()

	// Increment our clock
	l.clock.Increment(l.nodeID)

	entry := &CausalLogEntry{
		Timestamp:   time.Now(),
		VectorClock: l.clock.ToMap(),
		NodeID:      l.nodeID,
		Level:       level,
		Message:     message,
		Fields:      fields,
	}

	// Add trace context if available
	if ctx != nil {
		if tc := TraceContextFromContext(ctx); tc != nil {
			entry.TraceID = tc.TraceID
			entry.SpanID = tc.SpanID
		}
	}

	l.entries = append(l.entries, entry)

	// Export asynchronously
	if l.exporter != nil {
		go l.exporter.Export([]*CausalLogEntry{entry})
	}
}

// Merge updates our clock with an incoming message's clock.
func (l *CausalLog) Merge(incoming *VectorClock) {
	l.mu.Lock()
	defer l.mu.Unlock()

	l.clock.Merge(incoming)
	l.clock.Increment(l.nodeID)
}

// GetClock returns a copy of the current vector clock.
func (l *CausalLog) GetClock() *VectorClock {
	l.mu.Lock()
	defer l.mu.Unlock()
	return l.clock.Copy()
}

// =============================================================================
// REPLAY ENGINE
// =============================================================================

// ReplayEngine allows replaying events for debugging.
type ReplayEngine struct {
	events   []*ReplayEvent
	index    int
	handlers map[string]EventHandler
	mu       sync.Mutex
}

// ReplayEvent is an event that can be replayed.
type ReplayEvent struct {
	Timestamp   time.Time
	VectorClock *VectorClock
	Type        string
	NodeID      types.NodeID
	Data        []byte

	// Parsed data (cached)
	parsed interface{}
}

// EventHandler processes replayed events.
type EventHandler func(event *ReplayEvent) error

// NewReplayEngine creates a replay engine.
func NewReplayEngine() *ReplayEngine {
	return &ReplayEngine{
		events:   make([]*ReplayEvent, 0),
		handlers: make(map[string]EventHandler),
	}
}

// LoadEvents loads events for replay.
func (r *ReplayEngine) LoadEvents(events []*ReplayEvent) {
	r.mu.Lock()
	defer r.mu.Unlock()

	r.events = events
	r.index = 0

	// Sort by causal order
	r.sortByCausality()
}

// sortByCausality orders events respecting causality.
func (r *ReplayEngine) sortByCausality() {
	// Topological sort based on vector clock ordering
	// Simplified: sort by timestamp as a fallback
	for i := 0; i < len(r.events)-1; i++ {
		for j := i + 1; j < len(r.events); j++ {
			if r.events[j].VectorClock.HappenedBefore(r.events[i].VectorClock) {
				r.events[i], r.events[j] = r.events[j], r.events[i]
			}
		}
	}
}

// RegisterHandler registers a handler for an event type.
func (r *ReplayEngine) RegisterHandler(eventType string, handler EventHandler) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.handlers[eventType] = handler
}

// Step replays the next event.
func (r *ReplayEngine) Step() (*ReplayEvent, error) {
	r.mu.Lock()
	defer r.mu.Unlock()

	if r.index >= len(r.events) {
		return nil, ErrNoMoreEvents
	}

	event := r.events[r.index]
	r.index++

	// Call handler if registered
	if handler, ok := r.handlers[event.Type]; ok {
		if err := handler(event); err != nil {
			return event, err
		}
	}

	return event, nil
}

// StepUntil replays events until a condition is met.
func (r *ReplayEngine) StepUntil(predicate func(*ReplayEvent) bool) ([]*ReplayEvent, error) {
	var replayed []*ReplayEvent

	for {
		event, err := r.Step()
		if err != nil {
			if errors.Is(err, ErrNoMoreEvents) {
				break
			}
			return replayed, err
		}

		replayed = append(replayed, event)

		if predicate(event) {
			break
		}
	}

	return replayed, nil
}

// Reset restarts replay from the beginning.
func (r *ReplayEngine) Reset() {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.index = 0
}

// =============================================================================
// PROOF BUNDLE
// =============================================================================

// ProofBundle contains cryptographic evidence of task execution.
type ProofBundle struct {
	// Task identification
	TaskID    types.TaskID
	TenantID  types.TenantID
	NodeID    types.NodeID

	// Timing
	StartTime time.Time
	EndTime   time.Time

	// Input/output hashes
	InputHash  [32]byte
	OutputHash [32]byte

	// Execution trace
	StateTransitions []StateTransitionRecord
	LogHash          [32]byte

	// Signatures
	NodeSignature    []byte
	ClusterSignature []byte // From consensus

	// Metadata
	Version   uint32
	CreatedAt time.Time
}

// StateTransitionRecord records a state transition.
type StateTransitionRecord struct {
	Timestamp   time.Time
	FromState   string
	ToState     string
	Reason      string
	VectorClock map[types.NodeID]uint64
}

// Serialize returns the proof bundle as JSON.
func (p *ProofBundle) Serialize() ([]byte, error) {
	return json.Marshal(p)
}

// Deserialize parses a proof bundle from JSON.
func DeserializeProofBundle(data []byte) (*ProofBundle, error) {
	var bundle ProofBundle
	if err := json.Unmarshal(data, &bundle); err != nil {
		return nil, err
	}
	return &bundle, nil
}

// ProofBundleBuilder helps construct proof bundles.
type ProofBundleBuilder struct {
	bundle *ProofBundle
	mu     sync.Mutex
}

// NewProofBundleBuilder creates a builder.
func NewProofBundleBuilder(taskID types.TaskID, tenantID types.TenantID, nodeID types.NodeID) *ProofBundleBuilder {
	return &ProofBundleBuilder{
		bundle: &ProofBundle{
			TaskID:           taskID,
			TenantID:         tenantID,
			NodeID:           nodeID,
			StartTime:        time.Now(),
			StateTransitions: make([]StateTransitionRecord, 0),
			Version:          1,
		},
	}
}

// SetInputHash sets the input hash.
func (b *ProofBundleBuilder) SetInputHash(hash [32]byte) {
	b.mu.Lock()
	defer b.mu.Unlock()
	b.bundle.InputHash = hash
}

// SetOutputHash sets the output hash.
func (b *ProofBundleBuilder) SetOutputHash(hash [32]byte) {
	b.mu.Lock()
	defer b.mu.Unlock()
	b.bundle.OutputHash = hash
}

// RecordTransition records a state transition.
func (b *ProofBundleBuilder) RecordTransition(from, to, reason string, clock *VectorClock) {
	b.mu.Lock()
	defer b.mu.Unlock()

	b.bundle.StateTransitions = append(b.bundle.StateTransitions, StateTransitionRecord{
		Timestamp:   time.Now(),
		FromState:   from,
		ToState:     to,
		Reason:      reason,
		VectorClock: clock.ToMap(),
	})
}

// Build finalizes and signs the proof bundle.
func (b *ProofBundleBuilder) Build(signFunc func([]byte) []byte) *ProofBundle {
	b.mu.Lock()
	defer b.mu.Unlock()

	b.bundle.EndTime = time.Now()
	b.bundle.CreatedAt = time.Now()

	// Create signable content
	content := b.createSignableContent()
	b.bundle.NodeSignature = signFunc(content)

	return b.bundle
}

func (b *ProofBundleBuilder) createSignableContent() []byte {
	buf := make([]byte, 0, 256)

	// Task ID
	buf = append(buf, b.bundle.TaskID[:]...)

	// Timing
	buf = binary.BigEndian.AppendUint64(buf, uint64(b.bundle.StartTime.UnixNano()))
	buf = binary.BigEndian.AppendUint64(buf, uint64(b.bundle.EndTime.UnixNano()))

	// Hashes
	buf = append(buf, b.bundle.InputHash[:]...)
	buf = append(buf, b.bundle.OutputHash[:]...)

	return buf
}

// =============================================================================
// SAMPLERS
// =============================================================================

// AlwaysSample samples every trace.
type AlwaysSample struct{}

func (s AlwaysSample) ShouldSample(traceID TraceID, name string) bool {
	return true
}

// ProbabilitySampler samples based on probability.
type ProbabilitySampler struct {
	probability float64
}

func NewProbabilitySampler(probability float64) *ProbabilitySampler {
	return &ProbabilitySampler{probability: probability}
}

func (s *ProbabilitySampler) ShouldSample(traceID TraceID, name string) bool {
	// Use trace ID for determinism
	// High bytes of trace ID as random source
	threshold := uint64(s.probability * float64(^uint64(0)))
	traceValue := binary.BigEndian.Uint64(traceID[:8])
	return traceValue < threshold
}

// =============================================================================
// ERRORS
// =============================================================================

var (
	ErrNoMoreEvents = errors.New("no more events to replay")
)
