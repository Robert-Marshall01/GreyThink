//! # Fault Tolerance Module
//!
//! Fault detection, isolation, and chaos engineering.
//!
//! ## Module Structure
//!
//! - `detectors.rs` - Failure detection mechanisms
//! - `quarantine.rs` - Faulty component isolation
//! - `chaos.rs` - Chaos engineering for resilience testing

mod detectors;
mod quarantine;
mod chaos;

pub use detectors::*;
pub use quarantine::*;
pub use chaos::*;

use thiserror::Error;

/// Errors related to fault handling
#[derive(Error, Debug, Clone)]
pub enum FaultError {
    #[error("Component {component} is quarantined: {reason}")]
    Quarantined { component: String, reason: String },

    #[error("Circuit breaker open for {service}")]
    CircuitOpen { service: String },

    #[error("Failure threshold exceeded: {failures} failures in {window_secs}s")]
    ThresholdExceeded { failures: u32, window_secs: u64 },

    #[error("Health check failed: {reason}")]
    HealthCheckFailed { reason: String },

    #[error("Chaos injection active: {description}")]
    ChaosActive { description: String },

    #[error("Recovery failed: {reason}")]
    RecoveryFailed { reason: String },
}
