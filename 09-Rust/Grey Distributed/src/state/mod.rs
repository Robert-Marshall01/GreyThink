//! # State Module
//!
//! Event-sourced cluster state with snapshots and checkpoints.
//!
//! ## Module Structure
//!
//! - [`state`]: Core state machine and event processing
//! - [`snapshot`]: State snapshotting for recovery
//! - [`checkpoint`]: Distributed checkpoints for consistent recovery

mod checkpoint;
mod snapshot;
mod state;

pub use checkpoint::*;
pub use snapshot::*;
pub use state::*;

use thiserror::Error;

/// State-related errors
#[derive(Debug, Error)]
pub enum StateError {
    #[error("Invalid event: {0}")]
    InvalidEvent(String),

    #[error("State version mismatch: expected {expected}, got {actual}")]
    VersionMismatch { expected: u64, actual: u64 },

    #[error("Snapshot not found: {0}")]
    SnapshotNotFound(u64),

    #[error("Checkpoint failed: {0}")]
    CheckpointFailed(String),

    #[error("Recovery failed: {0}")]
    RecoveryFailed(String),

    #[error("Serialization error: {0}")]
    Serialization(String),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}
