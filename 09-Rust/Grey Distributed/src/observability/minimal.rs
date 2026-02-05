//! Grey Distributed — Minimal Observability Implementation
//!
//! Distributed tracing with causal ordering and structured logging.
//! Implements W3C Trace Context compatible spans.

use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime};
use tokio::sync::{mpsc, Mutex, RwLock};

// ============================================================================
// Trace Context
// ============================================================================

/// Unique trace identifier.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TraceId(pub u128);

/// Unique span identifier.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SpanId(pub u64);

impl TraceId {
    pub fn new() -> Self {
        Self(rand::random())
    }

    pub fn to_hex(&self) -> String {
        format!("{:032x}", self.0)
    }
}

impl SpanId {
    pub fn new() -> Self {
        Self(rand::random())
    }

    pub fn to_hex(&self) -> String {
        format!("{:016x}", self.0)
    }
}

/// Trace context propagated across services.
#[derive(Clone, Debug)]
pub struct TraceContext {
    pub trace_id: TraceId,
    pub span_id: SpanId,
    pub parent_span_id: Option<SpanId>,
    pub trace_flags: u8,
    pub baggage: HashMap<String, String>,
}

impl TraceContext {
    pub fn new() -> Self {
        Self {
            trace_id: TraceId::new(),
            span_id: SpanId::new(),
            parent_span_id: None,
            trace_flags: 0x01, // Sampled.
            baggage: HashMap::new(),
        }
    }

    pub fn child(&self) -> Self {
        Self {
            trace_id: self.trace_id,
            span_id: SpanId::new(),
            parent_span_id: Some(self.span_id),
            trace_flags: self.trace_flags,
            baggage: self.baggage.clone(),
        }
    }

    /// Serialize to W3C traceparent header format.
    pub fn to_traceparent(&self) -> String {
        format!(
            "00-{}-{}-{:02x}",
            self.trace_id.to_hex(),
            self.span_id.to_hex(),
            self.trace_flags
        )
    }

    /// Parse from W3C traceparent header.
    pub fn from_traceparent(header: &str) -> Option<Self> {
        let parts: Vec<&str> = header.split('-').collect();
        if parts.len() != 4 || parts[0] != "00" {
            return None;
        }

        let trace_id = u128::from_str_radix(parts[1], 16).ok()?;
        let span_id = u64::from_str_radix(parts[2], 16).ok()?;
        let flags = u8::from_str_radix(parts[3], 16).ok()?;

        Some(Self {
            trace_id: TraceId(trace_id),
            span_id: SpanId(span_id),
            parent_span_id: None,
            trace_flags: flags,
            baggage: HashMap::new(),
        })
    }
}

// ============================================================================
// Span
// ============================================================================

/// Span status.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SpanStatus {
    Unset,
    Ok,
    Error,
}

/// Span kind (client, server, internal, etc.).
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SpanKind {
    Internal,
    Client,
    Server,
    Producer,
    Consumer,
}

/// Span event (log within span).
#[derive(Clone, Debug)]
pub struct SpanEvent {
    pub name: String,
    pub timestamp: Instant,
    pub attributes: HashMap<String, AttributeValue>,
}

/// Attribute value types.
#[derive(Clone, Debug)]
pub enum AttributeValue {
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    StringArray(Vec<String>),
}

/// A span representing a unit of work.
#[derive(Clone, Debug)]
pub struct Span {
    pub context: TraceContext,
    pub name: String,
    pub kind: SpanKind,
    pub start_time: Instant,
    pub end_time: Option<Instant>,
    pub status: SpanStatus,
    pub status_message: Option<String>,
    pub attributes: HashMap<String, AttributeValue>,
    pub events: Vec<SpanEvent>,
}

impl Span {
    pub fn new(name: impl Into<String>, kind: SpanKind, context: TraceContext) -> Self {
        Self {
            context,
            name: name.into(),
            kind,
            start_time: Instant::now(),
            end_time: None,
            status: SpanStatus::Unset,
            status_message: None,
            attributes: HashMap::new(),
            events: Vec::new(),
        }
    }

    /// Set an attribute.
    pub fn set_attribute(&mut self, key: impl Into<String>, value: AttributeValue) {
        self.attributes.insert(key.into(), value);
    }

    /// Add an event.
    pub fn add_event(&mut self, name: impl Into<String>) {
        self.events.push(SpanEvent {
            name: name.into(),
            timestamp: Instant::now(),
            attributes: HashMap::new(),
        });
    }

    /// Add an event with attributes.
    pub fn add_event_with_attrs(
        &mut self,
        name: impl Into<String>,
        attrs: HashMap<String, AttributeValue>,
    ) {
        self.events.push(SpanEvent {
            name: name.into(),
            timestamp: Instant::now(),
            attributes: attrs,
        });
    }

    /// Set status to OK.
    pub fn set_ok(&mut self) {
        self.status = SpanStatus::Ok;
    }

    /// Set status to error.
    pub fn set_error(&mut self, message: impl Into<String>) {
        self.status = SpanStatus::Error;
        self.status_message = Some(message.into());
    }

    /// End the span.
    pub fn end(&mut self) {
        self.end_time = Some(Instant::now());
    }

    /// Get duration.
    pub fn duration(&self) -> Duration {
        let end = self.end_time.unwrap_or_else(Instant::now);
        end.duration_since(self.start_time)
    }
}

// ============================================================================
// Tracer
// ============================================================================

/// Tracer for creating and managing spans.
pub struct Tracer {
    service_name: String,
    node_id: u64,
    span_tx: mpsc::Sender<Span>,
    active_spans: RwLock<HashMap<SpanId, Span>>,
    current_context: RwLock<Option<TraceContext>>,
}

impl Tracer {
    pub fn new(service_name: impl Into<String>, node_id: u64, span_tx: mpsc::Sender<Span>) -> Self {
        Self {
            service_name: service_name.into(),
            node_id,
            span_tx,
            active_spans: RwLock::new(HashMap::new()),
            current_context: RwLock::new(None),
        }
    }

    /// Start a new root span.
    pub async fn start_span(&self, name: impl Into<String>, kind: SpanKind) -> Span {
        let current = self.current_context.read().await;
        let context = match &*current {
            Some(ctx) => ctx.child(),
            None => TraceContext::new(),
        };
        drop(current);

        let mut span = Span::new(name, kind, context.clone());
        span.set_attribute(
            "service.name",
            AttributeValue::String(self.service_name.clone()),
        );
        span.set_attribute("node.id", AttributeValue::Int(self.node_id as i64));

        let mut active = self.active_spans.write().await;
        active.insert(context.span_id, span.clone());

        let mut current = self.current_context.write().await;
        *current = Some(context);

        span
    }

    /// Start a child span.
    pub async fn start_child_span(
        &self,
        parent: &TraceContext,
        name: impl Into<String>,
        kind: SpanKind,
    ) -> Span {
        let context = parent.child();
        let mut span = Span::new(name, kind, context.clone());
        span.set_attribute(
            "service.name",
            AttributeValue::String(self.service_name.clone()),
        );
        span.set_attribute("node.id", AttributeValue::Int(self.node_id as i64));

        let mut active = self.active_spans.write().await;
        active.insert(context.span_id, span.clone());

        span
    }

    /// End a span and export it.
    pub async fn end_span(&self, mut span: Span) {
        span.end();

        let mut active = self.active_spans.write().await;
        active.remove(&span.context.span_id);

        // Restore parent context.
        if let Some(parent_id) = span.context.parent_span_id {
            if let Some(parent_span) = active.get(&parent_id) {
                let mut current = self.current_context.write().await;
                *current = Some(parent_span.context.clone());
            }
        } else {
            let mut current = self.current_context.write().await;
            *current = None;
        }

        drop(active);

        // Export span.
        let _ = self.span_tx.send(span).await;
    }

    /// Get current trace context.
    pub async fn current_context(&self) -> Option<TraceContext> {
        let current = self.current_context.read().await;
        current.clone()
    }
}

// ============================================================================
// Causal Log
// ============================================================================

/// Logical clock for causal ordering.
#[derive(Debug)]
pub struct LamportClock {
    time: AtomicU64,
    node_id: u64,
}

impl LamportClock {
    pub fn new(node_id: u64) -> Self {
        Self {
            time: AtomicU64::new(0),
            node_id,
        }
    }

    /// Increment and get timestamp.
    pub fn tick(&self) -> u64 {
        self.time.fetch_add(1, Ordering::SeqCst) + 1
    }

    /// Update from received timestamp.
    pub fn update(&self, received: u64) -> u64 {
        loop {
            let current = self.time.load(Ordering::SeqCst);
            let new_time = current.max(received) + 1;
            if self
                .time
                .compare_exchange(current, new_time, Ordering::SeqCst, Ordering::SeqCst)
                .is_ok()
            {
                return new_time;
            }
        }
    }

    /// Get current time.
    pub fn now(&self) -> u64 {
        self.time.load(Ordering::SeqCst)
    }
}

/// Log level.
#[derive(Clone, Copy, Debug, PartialEq, Ord, PartialOrd, Eq)]
pub enum LogLevel {
    Trace,
    Debug,
    Info,
    Warn,
    Error,
}

/// Causal log entry.
#[derive(Clone, Debug)]
pub struct LogEntry {
    pub lamport_time: u64,
    pub node_id: u64,
    pub level: LogLevel,
    pub message: String,
    pub trace_id: Option<TraceId>,
    pub span_id: Option<SpanId>,
    pub timestamp: SystemTime,
    pub attributes: HashMap<String, String>,
}

/// Causally ordered log.
pub struct CausalLog {
    clock: LamportClock,
    entries: Mutex<VecDeque<LogEntry>>,
    max_entries: usize,
    log_tx: mpsc::Sender<LogEntry>,
}

impl CausalLog {
    pub fn new(node_id: u64, max_entries: usize, log_tx: mpsc::Sender<LogEntry>) -> Self {
        Self {
            clock: LamportClock::new(node_id),
            entries: Mutex::new(VecDeque::new()),
            max_entries,
            log_tx,
        }
    }

    /// Log a message.
    pub async fn log(
        &self,
        level: LogLevel,
        message: impl Into<String>,
        context: Option<&TraceContext>,
    ) {
        let lamport_time = self.clock.tick();

        let entry = LogEntry {
            lamport_time,
            node_id: self.clock.node_id,
            level,
            message: message.into(),
            trace_id: context.map(|c| c.trace_id),
            span_id: context.map(|c| c.span_id),
            timestamp: SystemTime::now(),
            attributes: HashMap::new(),
        };

        let mut entries = self.entries.lock().await;
        entries.push_back(entry.clone());

        if entries.len() > self.max_entries {
            entries.pop_front();
        }

        drop(entries);

        let _ = self.log_tx.send(entry).await;
    }

    /// Log with attributes.
    pub async fn log_with_attrs(
        &self,
        level: LogLevel,
        message: impl Into<String>,
        context: Option<&TraceContext>,
        attrs: HashMap<String, String>,
    ) {
        let lamport_time = self.clock.tick();

        let entry = LogEntry {
            lamport_time,
            node_id: self.clock.node_id,
            level,
            message: message.into(),
            trace_id: context.map(|c| c.trace_id),
            span_id: context.map(|c| c.span_id),
            timestamp: SystemTime::now(),
            attributes: attrs,
        };

        let mut entries = self.entries.lock().await;
        entries.push_back(entry.clone());

        if entries.len() > self.max_entries {
            entries.pop_front();
        }

        drop(entries);

        let _ = self.log_tx.send(entry).await;
    }

    /// Update clock from received message.
    pub fn update_clock(&self, received_time: u64) -> u64 {
        self.clock.update(received_time)
    }

    /// Get recent entries.
    pub async fn recent_entries(&self, count: usize) -> Vec<LogEntry> {
        let entries = self.entries.lock().await;
        entries.iter().rev().take(count).cloned().collect()
    }

    /// Get entries for a trace.
    pub async fn entries_for_trace(&self, trace_id: TraceId) -> Vec<LogEntry> {
        let entries = self.entries.lock().await;
        entries
            .iter()
            .filter(|e| e.trace_id == Some(trace_id))
            .cloned()
            .collect()
    }

    /// Get current lamport time.
    pub fn current_time(&self) -> u64 {
        self.clock.now()
    }
}

// ============================================================================
// Metrics
// ============================================================================

/// Counter metric.
pub struct Counter {
    name: String,
    value: AtomicU64,
    labels: HashMap<String, String>,
}

impl Counter {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            value: AtomicU64::new(0),
            labels: HashMap::new(),
        }
    }

    pub fn with_labels(name: impl Into<String>, labels: HashMap<String, String>) -> Self {
        Self {
            name: name.into(),
            value: AtomicU64::new(0),
            labels,
        }
    }

    pub fn inc(&self) {
        self.value.fetch_add(1, Ordering::Relaxed);
    }

    pub fn add(&self, value: u64) {
        self.value.fetch_add(value, Ordering::Relaxed);
    }

    pub fn get(&self) -> u64 {
        self.value.load(Ordering::Relaxed)
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

/// Gauge metric.
pub struct Gauge {
    name: String,
    value: AtomicU64,
    labels: HashMap<String, String>,
}

impl Gauge {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            value: AtomicU64::new(0),
            labels: HashMap::new(),
        }
    }

    pub fn set(&self, value: u64) {
        self.value.store(value, Ordering::Relaxed);
    }

    pub fn inc(&self) {
        self.value.fetch_add(1, Ordering::Relaxed);
    }

    pub fn dec(&self) {
        self.value.fetch_sub(1, Ordering::Relaxed);
    }

    pub fn get(&self) -> u64 {
        self.value.load(Ordering::Relaxed)
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

/// Histogram metric.
pub struct Histogram {
    name: String,
    buckets: Vec<f64>,
    counts: Vec<AtomicU64>,
    sum: AtomicU64,
    count: AtomicU64,
}

impl Histogram {
    pub fn new(name: impl Into<String>, buckets: Vec<f64>) -> Self {
        let counts = buckets.iter().map(|_| AtomicU64::new(0)).collect();
        Self {
            name: name.into(),
            buckets,
            counts,
            sum: AtomicU64::new(0),
            count: AtomicU64::new(0),
        }
    }

    pub fn observe(&self, value: f64) {
        // Update sum and count.
        self.sum
            .fetch_add((value * 1_000_000.0) as u64, Ordering::Relaxed);
        self.count.fetch_add(1, Ordering::Relaxed);

        // Update bucket.
        for (i, &bound) in self.buckets.iter().enumerate() {
            if value <= bound {
                self.counts[i].fetch_add(1, Ordering::Relaxed);
                break;
            }
        }
    }

    pub fn get_count(&self) -> u64 {
        self.count.load(Ordering::Relaxed)
    }

    pub fn get_sum(&self) -> f64 {
        self.sum.load(Ordering::Relaxed) as f64 / 1_000_000.0
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_trace_context() {
        let ctx = TraceContext::new();
        let child = ctx.child();

        assert_eq!(child.trace_id, ctx.trace_id);
        assert_ne!(child.span_id, ctx.span_id);
        assert_eq!(child.parent_span_id, Some(ctx.span_id));
    }

    #[test]
    fn test_traceparent_roundtrip() {
        let ctx = TraceContext::new();
        let header = ctx.to_traceparent();
        let parsed = TraceContext::from_traceparent(&header).unwrap();

        assert_eq!(parsed.trace_id, ctx.trace_id);
        assert_eq!(parsed.span_id, ctx.span_id);
        assert_eq!(parsed.trace_flags, ctx.trace_flags);
    }

    #[test]
    fn test_lamport_clock() {
        let clock = LamportClock::new(1);

        assert_eq!(clock.tick(), 1);
        assert_eq!(clock.tick(), 2);
        assert_eq!(clock.now(), 2);

        // Update from higher timestamp.
        assert_eq!(clock.update(10), 11);
        assert_eq!(clock.now(), 11);
    }

    #[tokio::test]
    async fn test_tracer() {
        let (span_tx, mut span_rx) = mpsc::channel(100);
        let tracer = Tracer::new("test-service", 1, span_tx);

        let mut span = tracer.start_span("test-operation", SpanKind::Internal).await;
        span.set_attribute("key", AttributeValue::String("value".into()));
        span.add_event("event-1");
        span.set_ok();
        tracer.end_span(span).await;

        let received = span_rx.recv().await.unwrap();
        assert_eq!(received.name, "test-operation");
        assert_eq!(received.status, SpanStatus::Ok);
        assert_eq!(received.events.len(), 1);
    }

    #[tokio::test]
    async fn test_causal_log() {
        let (log_tx, mut log_rx) = mpsc::channel(100);
        let log = CausalLog::new(1, 1000, log_tx);

        log.log(LogLevel::Info, "test message", None).await;

        let entry = log_rx.recv().await.unwrap();
        assert_eq!(entry.level, LogLevel::Info);
        assert_eq!(entry.message, "test message");
        assert_eq!(entry.lamport_time, 1);
    }

    #[test]
    fn test_counter() {
        let counter = Counter::new("test_counter");

        counter.inc();
        counter.inc();
        counter.add(5);

        assert_eq!(counter.get(), 7);
    }

    #[test]
    fn test_histogram() {
        let hist = Histogram::new("test_hist", vec![0.1, 0.5, 1.0, 5.0, 10.0]);

        hist.observe(0.05);
        hist.observe(0.3);
        hist.observe(2.5);

        assert_eq!(hist.get_count(), 3);
    }
}
