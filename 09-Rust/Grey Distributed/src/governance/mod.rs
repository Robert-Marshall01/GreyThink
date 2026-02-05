//! # Governance Module
//!
//! Resource governance and multi-tenant management.
//!
//! ## Module Structure
//!
//! - `quotas.rs` - Tenant quota management and enforcement
//! - `throttling.rs` - Adaptive throttling and rate limiting
//! - `hotspot.rs` - Hotspot detection and mitigation

mod quotas;
mod throttling;
mod hotspot;

pub use quotas::*;
pub use throttling::*;
pub use hotspot::*;

use thiserror::Error;

/// Errors that can occur in governance
#[derive(Error, Debug, Clone)]
pub enum GovernanceError {
    #[error("Quota exceeded for tenant {tenant_id}: {resource}")]
    QuotaExceeded { tenant_id: u64, resource: String },

    #[error("Tenant not found: {0}")]
    TenantNotFound(u64),

    #[error("Throttled: {reason}")]
    Throttled { reason: String },

    #[error("Hotspot detected: {resource}")]
    HotspotDetected { resource: String },

    #[error("Invalid quota configuration: {0}")]
    InvalidQuotaConfig(String),

    #[error("Rate limit exceeded")]
    RateLimitExceeded,
}
