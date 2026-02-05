//! # Observability Module
//!
//! Distributed tracing, logging, and debugging infrastructure.
//!
//! ## Module Structure
//!
//! - `tracing.rs` - Distributed tracing spans and context
//! - `logs.rs` - Structured logging infrastructure
//! - `replay.rs` - Request replay and debugging

mod tracing;
mod logs;
mod replay;

pub use self::tracing::*;
pub use logs::*;
pub use replay::*;

use thiserror::Error;

/// Errors related to observability
#[derive(Error, Debug, Clone)]
pub enum ObservabilityError {
    #[error("Trace not found: {0}")]
    TraceNotFound(String),

    #[error("Span not found: {0}")]
    SpanNotFound(String),

    #[error("Log storage full")]
    LogStorageFull,

    #[error("Replay failed: {reason}")]
    ReplayFailed { reason: String },

    #[error("Context propagation failed: {reason}")]
    ContextPropagationFailed { reason: String },

    #[error("Sampling rejected")]
    SamplingRejected,
}
