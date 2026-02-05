//! # Scheduler Module
//!
//! Multi-tenant task scheduling with fairness guarantees.
//!
//! ## Module Structure
//!
//! - `scheduler.rs` - Core scheduling algorithms
//! - `fairness.rs` - Fair share scheduling and tenant isolation
//! - `retries.rs` - Retry policies and backoff strategies

mod scheduler;
mod fairness;
mod retries;

pub use scheduler::*;
pub use fairness::*;
pub use retries::*;

use thiserror::Error;

/// Errors that can occur in the scheduler
#[derive(Error, Debug, Clone)]
pub enum SchedulerError {
    #[error("Queue is full")]
    QueueFull,

    #[error("Task not found: {0}")]
    TaskNotFound(u64),

    #[error("Tenant is throttled")]
    TenantThrottled,

    #[error("Invalid priority: {0}")]
    InvalidPriority(i32),

    #[error("Worker not available")]
    NoWorkerAvailable,

    #[error("Task already exists")]
    TaskAlreadyExists,

    #[error("Scheduling deadline missed")]
    DeadlineMissed,

    #[error("Maximum retries exceeded")]
    MaxRetriesExceeded,
}
