//! # Storage Module
//!
//! Distributed storage layer with sharding and replication.
//!
//! ## Module Structure
//!
//! - `sharding.rs` - Data partitioning and shard management
//! - `replication.rs` - Data replication across nodes
//! - `quorum.rs` - Quorum-based consistency protocols

mod sharding;
mod replication;
mod quorum;

pub use sharding::*;
pub use replication::*;
pub use quorum::*;

use thiserror::Error;

/// Errors that can occur in storage operations
#[derive(Error, Debug, Clone)]
pub enum StorageError {
    #[error("Shard not found: {0}")]
    ShardNotFound(u64),

    #[error("Key not found: {0}")]
    KeyNotFound(String),

    #[error("Shard unavailable: {shard_id}")]
    ShardUnavailable { shard_id: u64 },

    #[error("Replication failed: {reason}")]
    ReplicationFailed { reason: String },

    #[error("Quorum not reached: required {required}, got {actual}")]
    QuorumNotReached { required: u32, actual: u32 },

    #[error("Split in progress for shard {0}")]
    SplitInProgress(u64),

    #[error("Merge in progress for shards {0} and {1}")]
    MergeInProgress(u64, u64),

    #[error("IO error: {0}")]
    IoError(String),

    #[error("Serialization error: {0}")]
    SerializationError(String),
}
