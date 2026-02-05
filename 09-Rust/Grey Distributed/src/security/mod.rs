//! # Security Module
//!
//! Security infrastructure for distributed systems.
//!
//! ## Module Structure
//!
//! - `identity.rs` - Node identity and authentication
//! - `attestation.rs` - Remote attestation protocols
//! - `isolation.rs` - Security isolation and sandboxing

mod identity;
mod attestation;
mod isolation;

pub use identity::*;
pub use attestation::*;
pub use isolation::*;

use thiserror::Error;

/// Errors related to security
#[derive(Error, Debug, Clone)]
pub enum SecurityError {
    #[error("Authentication failed: {reason}")]
    AuthenticationFailed { reason: String },

    #[error("Invalid certificate: {reason}")]
    InvalidCertificate { reason: String },

    #[error("Certificate expired")]
    CertificateExpired,

    #[error("Attestation failed: {reason}")]
    AttestationFailed { reason: String },

    #[error("Key derivation failed")]
    KeyDerivationFailed,

    #[error("Untrusted node: {node_id}")]
    UntrustedNode { node_id: String },

    #[error("Access denied: {resource}")]
    AccessDenied { resource: String },

    #[error("Isolation breach: {reason}")]
    IsolationBreach { reason: String },

    #[error("Sandbox violation: {reason}")]
    SandboxViolation { reason: String },
}
