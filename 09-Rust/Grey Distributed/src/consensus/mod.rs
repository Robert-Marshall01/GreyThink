//! # Consensus Module
//!
//! Implements Raft consensus with extensions for:
//! - Pre-Vote protocol (prevents disruption from partitioned nodes)
//! - Joint consensus (safe membership changes)
//! - Pipelining (higher throughput)
//! - Witness replicas (read scaling)
//!
//! ## Module Structure
//!
//! - [`consensus`]: Core Raft state machine and leader election
//! - [`membership`]: Cluster membership and configuration changes
//! - [`replication`]: Log replication and commit protocol

mod consensus;
mod membership;
mod replication;

pub use consensus::*;
pub use membership::*;
pub use replication::*;

use thiserror::Error;

/// Consensus-related errors
#[derive(Debug, Error)]
pub enum ConsensusError {
    #[error("Not leader, leader is {leader:?}")]
    NotLeader { leader: Option<crate::types::NodeId> },

    #[error("Term mismatch: expected {expected}, got {actual}")]
    TermMismatch { expected: u64, actual: u64 },

    #[error("Log inconsistency at index {index}")]
    LogInconsistency { index: u64 },

    #[error("Membership change in progress")]
    MembershipChangeInProgress,

    #[error("Node not in cluster: {0:?}")]
    NodeNotInCluster(crate::types::NodeId),

    #[error("Quorum not reached")]
    QuorumNotReached,

    #[error("Timeout waiting for consensus")]
    Timeout,

    #[error("Internal error: {0}")]
    Internal(String),
}
