//! # Distributed Tracing
//!
//! Request tracing across distributed system boundaries.
//!
//! ## Core Concepts
//!
//! - **Trace**: End-to-end request journey
//! - **Span**: Unit of work within a trace
//! - **Context**: Propagated metadata
//! - **Baggage**: User-defined key-value pairs
//!
//! ## Trace Structure
//!
//! ```text
//! Trace: abc123
//! ├── Span: gateway (12ms)
//! │   ├── Span: auth (3ms)
//! │   └── Span: route (8ms)
//! │       ├── Span: service-a (4ms)
//! │       │   └── Span: database (2ms)
//! │       └── Span: service-b (3ms)
//! ```
//!
//! ## W3C Trace Context
//!
//! Follows W3C Trace Context standard for interoperability:
//! - traceparent: version-traceId-spanId-flags
//! - tracestate: vendor-specific key-value pairs

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use parking_lot::{Mutex, RwLock};

// ============================================================================
// Core Types
// ============================================================================

/// Unique trace identifier (128-bit)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TraceId(pub u64, pub u64);

impl TraceId {
    pub fn generate() -> Self {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        
        static COUNTER: AtomicU64 = AtomicU64::new(0);
        
        let mut hasher = DefaultHasher::new();
        std::time::SystemTime::now().hash(&mut hasher);
        std::process::id().hash(&mut hasher);
        COUNTER.fetch_add(1, Ordering::Relaxed).hash(&mut hasher);
        
        let high = hasher.finish();
        
        hasher = DefaultHasher::new();
        high.hash(&mut hasher);
        let low = hasher.finish();
        
        TraceId(high, low)
    }
    
    pub fn to_hex(&self) -> String {
        format!("{:016x}{:016x}", self.0, self.1)
    }
    
    pub fn from_hex(s: &str) -> Option<Self> {
        if s.len() != 32 {
            return None;
        }
        let high = u64::from_str_radix(&s[0..16], 16).ok()?;
        let low = u64::from_str_radix(&s[16..32], 16).ok()?;
        Some(TraceId(high, low))
    }
}

/// Unique span identifier (64-bit)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SpanId(pub u64);

impl SpanId {
    pub fn generate() -> Self {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        
        static COUNTER: AtomicU64 = AtomicU64::new(0);
        
        let mut hasher = DefaultHasher::new();
        std::time::SystemTime::now().hash(&mut hasher);
        COUNTER.fetch_add(1, Ordering::Relaxed).hash(&mut hasher);
        
        SpanId(hasher.finish())
    }
    
    pub fn to_hex(&self) -> String {
        format!("{:016x}", self.0)
    }
    
    pub fn from_hex(s: &str) -> Option<Self> {
        if s.len() != 16 {
            return None;
        }
        u64::from_str_radix(s, 16).ok().map(SpanId)
    }
}

/// Trace flags
#[derive(Debug, Clone, Copy, Default)]
pub struct TraceFlags {
    /// Whether this trace is sampled
    pub sampled: bool,
}

impl TraceFlags {
    pub fn to_byte(&self) -> u8 {
        if self.sampled { 0x01 } else { 0x00 }
    }
    
    pub fn from_byte(b: u8) -> Self {
        Self {
            sampled: (b & 0x01) != 0,
        }
    }
}

// ============================================================================
// Span
// ============================================================================

/// A span represents a unit of work
#[derive(Debug, Clone)]
pub struct Span {
    /// Span context
    pub context: SpanContext,
    
    /// Span name
    pub name: String,
    
    /// Span kind
    pub kind: SpanKind,
    
    /// Start time
    pub start_time: SystemTime,
    
    /// End time (None if still active)
    pub end_time: Option<SystemTime>,
    
    /// Duration (computed when ended)
    pub duration: Option<Duration>,
    
    /// Status
    pub status: SpanStatus,
    
    /// Attributes
    pub attributes: HashMap<String, AttributeValue>,
    
    /// Events/logs within the span
    pub events: Vec<SpanEvent>,
    
    /// Links to other spans
    pub links: Vec<SpanLink>,
}

#[derive(Debug, Clone)]
pub struct SpanContext {
    pub trace_id: TraceId,
    pub span_id: SpanId,
    pub parent_span_id: Option<SpanId>,
    pub flags: TraceFlags,
    pub trace_state: HashMap<String, String>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SpanKind {
    /// Internal operation
    Internal,
    /// Server receiving a request
    Server,
    /// Client sending a request
    Client,
    /// Message producer
    Producer,
    /// Message consumer
    Consumer,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SpanStatus {
    Unset,
    Ok,
    Error,
}

#[derive(Debug, Clone)]
pub enum AttributeValue {
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    StringArray(Vec<String>),
    IntArray(Vec<i64>),
}

#[derive(Debug, Clone)]
pub struct SpanEvent {
    pub name: String,
    pub timestamp: SystemTime,
    pub attributes: HashMap<String, AttributeValue>,
}

#[derive(Debug, Clone)]
pub struct SpanLink {
    pub context: SpanContext,
    pub attributes: HashMap<String, AttributeValue>,
}

impl Span {
    pub fn new(name: impl Into<String>, context: SpanContext, kind: SpanKind) -> Self {
        Self {
            context,
            name: name.into(),
            kind,
            start_time: SystemTime::now(),
            end_time: None,
            duration: None,
            status: SpanStatus::Unset,
            attributes: HashMap::new(),
            events: Vec::new(),
            links: Vec::new(),
        }
    }
    
    pub fn set_attribute(&mut self, key: impl Into<String>, value: AttributeValue) {
        self.attributes.insert(key.into(), value);
    }
    
    pub fn add_event(&mut self, name: impl Into<String>) {
        self.events.push(SpanEvent {
            name: name.into(),
            timestamp: SystemTime::now(),
            attributes: HashMap::new(),
        });
    }
    
    pub fn set_status(&mut self, status: SpanStatus) {
        self.status = status;
    }
    
    pub fn end(&mut self) {
        let end = SystemTime::now();
        self.end_time = Some(end);
        self.duration = end.duration_since(self.start_time).ok();
    }
    
    pub fn is_recording(&self) -> bool {
        self.end_time.is_none()
    }
}

// ============================================================================
// Tracer
// ============================================================================

/// Creates and manages spans
pub struct Tracer {
    /// Service name
    service_name: String,
    
    /// Active spans (in current thread context, simplified)
    active_spans: RwLock<HashMap<SpanId, Span>>,
    
    /// Completed spans awaiting export
    completed_spans: Mutex<Vec<Span>>,
    
    /// Sampler
    sampler: Arc<dyn Sampler + Send + Sync>,
    
    /// Exporters
    exporters: RwLock<Vec<Box<dyn SpanExporter + Send + Sync>>>,
    
    /// Max completed spans buffer
    max_buffer_size: usize,
}

impl Tracer {
    pub fn new(service_name: impl Into<String>) -> Self {
        Self {
            service_name: service_name.into(),
            active_spans: RwLock::new(HashMap::new()),
            completed_spans: Mutex::new(Vec::new()),
            sampler: Arc::new(AlwaysOnSampler),
            exporters: RwLock::new(Vec::new()),
            max_buffer_size: 10000,
        }
    }
    
    /// Start a new root span
    pub fn start_span(&self, name: impl Into<String>, kind: SpanKind) -> SpanId {
        let name_str = name.into();
        let trace_id = TraceId::generate();
        let span_id = SpanId::generate();
        
        let flags = TraceFlags {
            sampled: self.sampler.should_sample(&trace_id, &name_str),
        };
        
        let context = SpanContext {
            trace_id,
            span_id,
            parent_span_id: None,
            flags,
            trace_state: HashMap::new(),
        };
        
        let span = Span::new(name_str, context, kind);
        self.active_spans.write().insert(span_id, span);
        
        span_id
    }
    
    /// Start a child span
    pub fn start_child_span(&self, parent: &SpanContext, name: impl Into<String>, kind: SpanKind) -> SpanId {
        let span_id = SpanId::generate();
        
        let context = SpanContext {
            trace_id: parent.trace_id,
            span_id,
            parent_span_id: Some(parent.span_id),
            flags: parent.flags,
            trace_state: parent.trace_state.clone(),
        };
        
        let span = Span::new(name, context, kind);
        self.active_spans.write().insert(span_id, span);
        
        span_id
    }
    
    /// End a span
    pub fn end_span(&self, span_id: SpanId) {
        if let Some(mut span) = self.active_spans.write().remove(&span_id) {
            span.end();
            
            if span.context.flags.sampled {
                let mut completed = self.completed_spans.lock();
                if completed.len() < self.max_buffer_size {
                    completed.push(span);
                }
            }
        }
    }
    
    /// Get a reference to a span for modification
    pub fn with_span<F, R>(&self, span_id: SpanId, f: F) -> Option<R>
    where
        F: FnOnce(&mut Span) -> R,
    {
        self.active_spans.write().get_mut(&span_id).map(f)
    }
    
    /// Get span context for propagation
    pub fn span_context(&self, span_id: SpanId) -> Option<SpanContext> {
        self.active_spans.read().get(&span_id).map(|s| s.context.clone())
    }
    
    /// Export completed spans
    pub fn export(&self) {
        let spans = std::mem::take(&mut *self.completed_spans.lock());
        
        if spans.is_empty() {
            return;
        }
        
        let exporters = self.exporters.read();
        for exporter in exporters.iter() {
            exporter.export(&spans);
        }
    }
    
    /// Register an exporter
    pub fn add_exporter<E: SpanExporter + 'static>(&self, exporter: E) {
        self.exporters.write().push(Box::new(exporter));
    }
}

// ============================================================================
// Sampling
// ============================================================================

/// Decides whether to sample a trace
pub trait Sampler: Send + Sync {
    fn should_sample(&self, trace_id: &TraceId, span_name: &str) -> bool;
}

/// Always sample
pub struct AlwaysOnSampler;
impl Sampler for AlwaysOnSampler {
    fn should_sample(&self, _: &TraceId, _: &str) -> bool {
        true
    }
}

/// Never sample
pub struct AlwaysOffSampler;
impl Sampler for AlwaysOffSampler {
    fn should_sample(&self, _: &TraceId, _: &str) -> bool {
        false
    }
}

/// Sample a percentage of traces
pub struct RatioSampler {
    ratio: f64,
    threshold: u64,
}

impl RatioSampler {
    pub fn new(ratio: f64) -> Self {
        Self {
            ratio,
            threshold: (u64::MAX as f64 * ratio) as u64,
        }
    }
}

impl Sampler for RatioSampler {
    fn should_sample(&self, trace_id: &TraceId, _: &str) -> bool {
        trace_id.0 < self.threshold
    }
}

// ============================================================================
// Export
// ============================================================================

/// Exports completed spans
pub trait SpanExporter: Send + Sync {
    fn export(&self, spans: &[Span]);
}

/// Logs spans to stdout (for debugging)
pub struct StdoutExporter;

impl SpanExporter for StdoutExporter {
    fn export(&self, spans: &[Span]) {
        for span in spans {
            println!(
                "[TRACE] {} {} {:?} {:?}",
                span.context.trace_id.to_hex(),
                span.name,
                span.kind,
                span.duration,
            );
        }
    }
}

/// Buffers spans for batch export
pub struct BatchExporter {
    spans: Mutex<Vec<Span>>,
    batch_size: usize,
    inner: Box<dyn SpanExporter + Send + Sync>,
}

impl BatchExporter {
    pub fn new(inner: impl SpanExporter + 'static, batch_size: usize) -> Self {
        Self {
            spans: Mutex::new(Vec::new()),
            batch_size,
            inner: Box::new(inner),
        }
    }
    
    pub fn flush(&self) {
        let spans = std::mem::take(&mut *self.spans.lock());
        if !spans.is_empty() {
            self.inner.export(&spans);
        }
    }
}

impl SpanExporter for BatchExporter {
    fn export(&self, spans: &[Span]) {
        let mut buffer = self.spans.lock();
        buffer.extend(spans.iter().cloned());
        
        if buffer.len() >= self.batch_size {
            let batch = std::mem::take(&mut *buffer);
            drop(buffer);
            self.inner.export(&batch);
        }
    }
}

// ============================================================================
// Context Propagation
// ============================================================================

/// Propagates trace context across boundaries
pub struct ContextPropagator;

impl ContextPropagator {
    /// Inject context into headers (W3C Trace Context format)
    pub fn inject(context: &SpanContext, headers: &mut HashMap<String, String>) {
        // traceparent: {version}-{trace-id}-{span-id}-{flags}
        let traceparent = format!(
            "00-{}-{}-{:02x}",
            context.trace_id.to_hex(),
            context.span_id.to_hex(),
            context.flags.to_byte()
        );
        headers.insert("traceparent".into(), traceparent);
        
        // tracestate: key1=value1,key2=value2
        if !context.trace_state.is_empty() {
            let tracestate: String = context.trace_state
                .iter()
                .map(|(k, v)| format!("{}={}", k, v))
                .collect::<Vec<_>>()
                .join(",");
            headers.insert("tracestate".into(), tracestate);
        }
    }
    
    /// Extract context from headers
    pub fn extract(headers: &HashMap<String, String>) -> Option<SpanContext> {
        let traceparent = headers.get("traceparent")?;
        let parts: Vec<&str> = traceparent.split('-').collect();
        
        if parts.len() != 4 || parts[0] != "00" {
            return None;
        }
        
        let trace_id = TraceId::from_hex(parts[1])?;
        let span_id = SpanId::from_hex(parts[2])?;
        let flags = TraceFlags::from_byte(u8::from_str_radix(parts[3], 16).ok()?);
        
        let mut trace_state = HashMap::new();
        if let Some(ts) = headers.get("tracestate") {
            for pair in ts.split(',') {
                if let Some((k, v)) = pair.split_once('=') {
                    trace_state.insert(k.trim().into(), v.trim().into());
                }
            }
        }
        
        Some(SpanContext {
            trace_id,
            span_id,
            parent_span_id: None, // Unknown from headers
            flags,
            trace_state,
        })
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_trace_id() {
        let id = TraceId::generate();
        let hex = id.to_hex();
        assert_eq!(hex.len(), 32);
        
        let parsed = TraceId::from_hex(&hex);
        assert_eq!(parsed, Some(id));
    }
    
    #[test]
    fn test_span_id() {
        let id = SpanId::generate();
        let hex = id.to_hex();
        assert_eq!(hex.len(), 16);
        
        let parsed = SpanId::from_hex(&hex);
        assert_eq!(parsed, Some(id));
    }
    
    #[test]
    fn test_tracer() {
        let tracer = Tracer::new("test-service");
        
        let span_id = tracer.start_span("test-operation", SpanKind::Internal);
        
        tracer.with_span(span_id, |span| {
            span.set_attribute("key", AttributeValue::String("value".into()));
            span.add_event("something-happened");
        });
        
        let context = tracer.span_context(span_id);
        assert!(context.is_some());
        
        tracer.end_span(span_id);
    }
    
    #[test]
    fn test_context_propagation() {
        let context = SpanContext {
            trace_id: TraceId::generate(),
            span_id: SpanId::generate(),
            parent_span_id: None,
            flags: TraceFlags { sampled: true },
            trace_state: HashMap::new(),
        };
        
        let mut headers = HashMap::new();
        ContextPropagator::inject(&context, &mut headers);
        
        assert!(headers.contains_key("traceparent"));
        
        let extracted = ContextPropagator::extract(&headers).unwrap();
        assert_eq!(extracted.trace_id, context.trace_id);
        assert_eq!(extracted.span_id, context.span_id);
        assert!(extracted.flags.sampled);
    }
    
    #[test]
    fn test_ratio_sampler() {
        let sampler = RatioSampler::new(0.5);
        
        // With enough samples, roughly half should be sampled
        let mut sampled = 0;
        for _ in 0..1000 {
            if sampler.should_sample(&TraceId::generate(), "test") {
                sampled += 1;
            }
        }
        
        // Should be roughly 50% (with some variance)
        assert!(sampled > 300 && sampled < 700);
    }
}
