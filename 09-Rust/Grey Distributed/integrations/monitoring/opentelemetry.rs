//! OpenTelemetry Integration for Grey Distributed
//!
//! Provides distributed tracing and metrics export via OpenTelemetry protocol.
//! Supports both OTLP/gRPC and OTLP/HTTP exporters.
//!
//! # Architecture
//!
//! Grey uses OpenTelemetry for:
//! 1. **Distributed Tracing**: Track tasks across scheduler, workers, storage
//! 2. **Metrics**: Export to OTLP-compatible backends (alternative to Prometheus)
//! 3. **Logs**: Structured logging with trace context
//!
//! # Trace Hierarchy
//!
//! Task traces follow this structure:
//! ```
//! task.submission (root)
//! ├── scheduler.assign
//! │   └── scheduler.find_node
//! ├── worker.execute
//! │   ├── worker.load_state
//! │   ├── worker.run
//! │   └── worker.save_state
//! └── task.completion
//! ```
//!
//! # Context Propagation
//!
//! Trace context is propagated via:
//! - gRPC metadata headers
//! - Task payload (for async execution)
//! - HTTP headers (W3C Trace Context)

use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;

use opentelemetry::{
    global,
    trace::{Span, SpanKind, Status, TraceContextExt, Tracer, TracerProvider as _},
    Context, KeyValue,
};
use opentelemetry_otlp::{Protocol, WithExportConfig};
use opentelemetry_sdk::{
    runtime::Tokio,
    trace::{BatchConfig, RandomIdGenerator, Sampler, TracerProvider},
    Resource,
};
use opentelemetry_semantic_conventions as semconv;
use tracing::{debug, info, instrument, warn};
use tracing_opentelemetry::OpenTelemetryLayer;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, EnvFilter};

// ============================================================================
// Configuration
// ============================================================================

/// OpenTelemetry configuration for Grey.
#[derive(Debug, Clone)]
pub struct OtelConfig {
    /// Service name for resource identification
    pub service_name: String,
    
    /// Service version
    pub service_version: String,
    
    /// Deployment environment (production, staging, dev)
    pub environment: String,
    
    /// OTLP exporter endpoint
    pub otlp_endpoint: String,
    
    /// Use gRPC (true) or HTTP (false) for OTLP
    pub otlp_grpc: bool,
    
    /// Sampling rate (0.0 to 1.0)
    /// 1.0 = sample everything, 0.1 = sample 10%
    pub sampling_rate: f64,
    
    /// Enable trace context propagation
    pub propagation_enabled: bool,
    
    /// Batch export settings
    pub batch_config: BatchExportConfig,
    
    /// Additional resource attributes
    pub resource_attributes: HashMap<String, String>,
}

impl Default for OtelConfig {
    fn default() -> Self {
        Self {
            service_name: "grey-distributed".to_string(),
            service_version: env!("CARGO_PKG_VERSION").to_string(),
            environment: "production".to_string(),
            otlp_endpoint: "http://localhost:4317".to_string(),
            otlp_grpc: true,
            sampling_rate: 1.0,
            propagation_enabled: true,
            batch_config: BatchExportConfig::default(),
            resource_attributes: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BatchExportConfig {
    /// Maximum queue size for pending spans
    pub max_queue_size: usize,
    
    /// Maximum batch size for export
    pub max_export_batch_size: usize,
    
    /// Scheduled delay between exports
    pub scheduled_delay: Duration,
    
    /// Maximum time to wait for export
    pub max_export_timeout: Duration,
}

impl Default for BatchExportConfig {
    fn default() -> Self {
        Self {
            max_queue_size: 2048,
            max_export_batch_size: 512,
            scheduled_delay: Duration::from_secs(5),
            max_export_timeout: Duration::from_secs(30),
        }
    }
}

// ============================================================================
// Initialization
// ============================================================================

/// Initialize OpenTelemetry tracing for Grey.
///
/// This sets up:
/// 1. OTLP trace exporter
/// 2. Resource with service metadata
/// 3. Batch span processor
/// 4. Tracing subscriber integration
///
/// # Tradeoffs
///
/// - Sampling: Lower rates reduce overhead but lose visibility
/// - Batch size: Larger batches are efficient but delay export
/// - Queue size: Larger queues handle bursts but use more memory
pub fn init_tracing(config: &OtelConfig) -> Result<(), OtelError> {
    info!(
        service = %config.service_name,
        endpoint = %config.otlp_endpoint,
        "Initializing OpenTelemetry tracing"
    );

    // Build resource with service metadata
    let mut resource_attrs = vec![
        KeyValue::new(semconv::resource::SERVICE_NAME, config.service_name.clone()),
        KeyValue::new(semconv::resource::SERVICE_VERSION, config.service_version.clone()),
        KeyValue::new(semconv::resource::DEPLOYMENT_ENVIRONMENT, config.environment.clone()),
    ];
    
    for (key, value) in &config.resource_attributes {
        resource_attrs.push(KeyValue::new(key.clone(), value.clone()));
    }
    
    let resource = Resource::new(resource_attrs);

    // Build OTLP exporter
    let exporter = if config.otlp_grpc {
        opentelemetry_otlp::new_exporter()
            .tonic()
            .with_endpoint(&config.otlp_endpoint)
            .with_timeout(config.batch_config.max_export_timeout)
    } else {
        opentelemetry_otlp::new_exporter()
            .http()
            .with_endpoint(&config.otlp_endpoint)
            .with_timeout(config.batch_config.max_export_timeout)
    };

    // Build sampler based on rate
    let sampler = if config.sampling_rate >= 1.0 {
        Sampler::AlwaysOn
    } else if config.sampling_rate <= 0.0 {
        Sampler::AlwaysOff
    } else {
        Sampler::TraceIdRatioBased(config.sampling_rate)
    };

    // Build batch config
    let batch_config = BatchConfig::default()
        .with_max_queue_size(config.batch_config.max_queue_size)
        .with_max_export_batch_size(config.batch_config.max_export_batch_size)
        .with_scheduled_delay(config.batch_config.scheduled_delay);

    // Build tracer provider
    let tracer_provider = TracerProvider::builder()
        .with_batch_exporter(
            opentelemetry_otlp::new_pipeline()
                .tracing()
                .with_exporter(exporter)
                .build_batch_exporter(Tokio, batch_config)?,
            Tokio,
        )
        .with_resource(resource)
        .with_sampler(sampler)
        .with_id_generator(RandomIdGenerator::default())
        .build();

    // Set global tracer provider
    global::set_tracer_provider(tracer_provider.clone());

    // Set up context propagation
    if config.propagation_enabled {
        use opentelemetry::propagation::TextMapCompositePropagator;
        use opentelemetry_sdk::propagation::TraceContextPropagator;
        
        let propagator = TextMapCompositePropagator::new(vec![
            Box::new(TraceContextPropagator::new()),
        ]);
        global::set_text_map_propagator(propagator);
    }

    // Integrate with tracing crate
    let otel_layer = OpenTelemetryLayer::new(tracer_provider.tracer("grey"));
    
    tracing_subscriber::registry()
        .with(EnvFilter::from_default_env())
        .with(otel_layer)
        .with(tracing_subscriber::fmt::layer())
        .init();

    info!("OpenTelemetry tracing initialized");
    Ok(())
}

/// Shutdown OpenTelemetry gracefully.
///
/// Flushes remaining spans before shutdown.
pub fn shutdown_tracing() {
    info!("Shutting down OpenTelemetry tracing");
    global::shutdown_tracer_provider();
}

// ============================================================================
// Tracing Utilities
// ============================================================================

/// Grey-specific tracer wrapper.
pub struct GreyTracer {
    tracer: opentelemetry::global::BoxedTracer,
}

impl GreyTracer {
    pub fn new() -> Self {
        Self {
            tracer: global::tracer("grey"),
        }
    }

    /// Start a new task span.
    ///
    /// Creates root span for task lifecycle tracking.
    pub fn start_task_span(&self, task_id: &str, tenant_id: &str) -> TaskSpan {
        let span = self.tracer
            .span_builder("task.lifecycle")
            .with_kind(SpanKind::Server)
            .with_attributes(vec![
                KeyValue::new("grey.task_id", task_id.to_string()),
                KeyValue::new("grey.tenant_id", tenant_id.to_string()),
            ])
            .start(&self.tracer);

        TaskSpan {
            span,
            task_id: task_id.to_string(),
            tenant_id: tenant_id.to_string(),
        }
    }

    /// Start a scheduler span (child of task span).
    pub fn start_scheduler_span(&self, ctx: &Context, operation: &str) -> impl Span {
        let span = self.tracer
            .span_builder(format!("scheduler.{}", operation))
            .with_kind(SpanKind::Internal)
            .start_with_context(&self.tracer, ctx);
        span
    }

    /// Start a worker span (child of task span).
    pub fn start_worker_span(&self, ctx: &Context, operation: &str, node_id: &str) -> impl Span {
        let span = self.tracer
            .span_builder(format!("worker.{}", operation))
            .with_kind(SpanKind::Internal)
            .with_attributes(vec![
                KeyValue::new("grey.node_id", node_id.to_string()),
            ])
            .start_with_context(&self.tracer, ctx);
        span
    }

    /// Start a storage span.
    pub fn start_storage_span(&self, ctx: &Context, operation: &str, backend: &str) -> impl Span {
        let span = self.tracer
            .span_builder(format!("storage.{}", operation))
            .with_kind(SpanKind::Client)
            .with_attributes(vec![
                KeyValue::new("db.system", backend.to_string()),
            ])
            .start_with_context(&self.tracer, ctx);
        span
    }

    /// Start an RPC client span.
    pub fn start_rpc_span(&self, ctx: &Context, method: &str, target: &str) -> impl Span {
        let span = self.tracer
            .span_builder(format!("rpc.{}", method))
            .with_kind(SpanKind::Client)
            .with_attributes(vec![
                KeyValue::new("rpc.method", method.to_string()),
                KeyValue::new("rpc.target", target.to_string()),
            ])
            .start_with_context(&self.tracer, ctx);
        span
    }
}

/// Task span wrapper with Grey-specific functionality.
pub struct TaskSpan {
    span: opentelemetry::global::BoxedSpan,
    task_id: String,
    tenant_id: String,
}

impl TaskSpan {
    /// Mark task as submitted.
    pub fn set_submitted(&mut self) {
        self.span.add_event("task.submitted", vec![]);
    }

    /// Mark task as scheduled to a node.
    pub fn set_scheduled(&mut self, node_id: &str) {
        self.span.add_event("task.scheduled", vec![
            KeyValue::new("grey.node_id", node_id.to_string()),
        ]);
    }

    /// Mark task as running.
    pub fn set_running(&mut self) {
        self.span.add_event("task.running", vec![]);
    }

    /// Mark task as completed.
    pub fn set_completed(&mut self, duration_ms: f64) {
        self.span.add_event("task.completed", vec![
            KeyValue::new("grey.duration_ms", duration_ms),
        ]);
        self.span.set_status(Status::Ok);
    }

    /// Mark task as failed.
    pub fn set_failed(&mut self, error: &str) {
        self.span.add_event("task.failed", vec![
            KeyValue::new("grey.error", error.to_string()),
        ]);
        self.span.set_status(Status::error(error.to_string()));
    }

    /// Add proof artifact info.
    pub fn set_proof(&mut self, proof_id: &str, proof_hash: &str) {
        self.span.set_attribute(KeyValue::new("grey.proof_id", proof_id.to_string()));
        self.span.set_attribute(KeyValue::new("grey.proof_hash", proof_hash.to_string()));
    }

    /// Get context for child spans.
    pub fn context(&self) -> Context {
        Context::current_with_span(self.span.clone())
    }

    /// End the span.
    pub fn end(self) {
        self.span.end();
    }
}

// ============================================================================
// Context Propagation
// ============================================================================

/// Extract trace context from incoming request headers.
pub fn extract_context(headers: &HashMap<String, String>) -> Context {
    let propagator = global::get_text_map_propagator();
    propagator.extract(&HeaderExtractor(headers))
}

/// Inject trace context into outgoing request headers.
pub fn inject_context(ctx: &Context, headers: &mut HashMap<String, String>) {
    let propagator = global::get_text_map_propagator();
    propagator.inject_context(ctx, &mut HeaderInjector(headers));
}

struct HeaderExtractor<'a>(&'a HashMap<String, String>);

impl<'a> opentelemetry::propagation::Extractor for HeaderExtractor<'a> {
    fn get(&self, key: &str) -> Option<&str> {
        self.0.get(key).map(|s| s.as_str())
    }

    fn keys(&self) -> Vec<&str> {
        self.0.keys().map(|s| s.as_str()).collect()
    }
}

struct HeaderInjector<'a>(&'a mut HashMap<String, String>);

impl<'a> opentelemetry::propagation::Injector for HeaderInjector<'a> {
    fn set(&mut self, key: &str, value: String) {
        self.0.insert(key.to_string(), value);
    }
}

// ============================================================================
// Trace Context for Task Payloads
// ============================================================================

/// Serializable trace context for async task propagation.
///
/// When a task is queued for later execution, we need to preserve
/// the trace context. This struct can be serialized into task payloads.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct SerializableTraceContext {
    pub trace_id: String,
    pub span_id: String,
    pub trace_flags: u8,
    pub trace_state: String,
}

impl SerializableTraceContext {
    /// Capture current trace context.
    pub fn capture() -> Option<Self> {
        use opentelemetry::trace::TraceContextExt;
        
        let ctx = Context::current();
        let span_ctx = ctx.span().span_context().clone();
        
        if !span_ctx.is_valid() {
            return None;
        }
        
        Some(Self {
            trace_id: span_ctx.trace_id().to_string(),
            span_id: span_ctx.span_id().to_string(),
            trace_flags: span_ctx.trace_flags().to_u8(),
            trace_state: span_ctx.trace_state().as_str().to_string(),
        })
    }

    /// Restore trace context for continuing a trace.
    pub fn restore(&self) -> Context {
        use opentelemetry::trace::{SpanContext, TraceId, SpanId, TraceFlags, TraceState};
        
        let trace_id = TraceId::from_hex(&self.trace_id)
            .unwrap_or(TraceId::INVALID);
        let span_id = SpanId::from_hex(&self.span_id)
            .unwrap_or(SpanId::INVALID);
        let trace_flags = TraceFlags::new(self.trace_flags);
        let trace_state = TraceState::from_str(&self.trace_state)
            .unwrap_or_default();

        let span_context = SpanContext::new(
            trace_id,
            span_id,
            trace_flags,
            true,  // remote
            trace_state,
        );

        Context::current().with_remote_span_context(span_context)
    }
}

// ============================================================================
// Errors
// ============================================================================

#[derive(Debug, thiserror::Error)]
pub enum OtelError {
    #[error("Failed to initialize tracer: {0}")]
    TracerInit(String),
    
    #[error("Failed to export spans: {0}")]
    Export(String),
    
    #[error("Invalid configuration: {0}")]
    Config(String),
}

impl From<opentelemetry::trace::TraceError> for OtelError {
    fn from(e: opentelemetry::trace::TraceError) -> Self {
        OtelError::TracerInit(e.to_string())
    }
}

// ============================================================================
// Instrumentation Macros
// ============================================================================

/// Macro for instrumenting Grey operations with tracing.
///
/// Usage:
/// ```rust
/// grey_span!("scheduler.assign", task_id = %task_id, {
///     // operation code
/// })
/// ```
#[macro_export]
macro_rules! grey_span {
    ($name:expr, $($field:tt)*) => {
        tracing::info_span!($name, $($field)*, otel.name = $name)
    };
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_serializable_context_roundtrip() {
        // This would need a proper tracer setup to test fully
        // Simplified test for serialization
        let ctx = SerializableTraceContext {
            trace_id: "0123456789abcdef0123456789abcdef".to_string(),
            span_id: "0123456789abcdef".to_string(),
            trace_flags: 1,
            trace_state: "".to_string(),
        };

        let json = serde_json::to_string(&ctx).unwrap();
        let restored: SerializableTraceContext = serde_json::from_str(&json).unwrap();

        assert_eq!(ctx.trace_id, restored.trace_id);
        assert_eq!(ctx.span_id, restored.span_id);
    }

    #[test]
    fn test_config_defaults() {
        let config = OtelConfig::default();
        assert_eq!(config.service_name, "grey-distributed");
        assert_eq!(config.sampling_rate, 1.0);
        assert!(config.otlp_grpc);
    }
}
