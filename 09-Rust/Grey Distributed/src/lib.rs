//! # Grey Distributed
//!
//! A production-grade distributed systems platform implementing consensus,
//! state management, scheduling, governance, storage, networking, fault tolerance,
//! observability, and security.
//!
//! ## Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────────┐
//! │                        Grey Distributed                          │
//! ├─────────────────────────────────────────────────────────────────┤
//! │  ┌───────────┐  ┌───────────┐  ┌───────────┐  ┌───────────┐    │
//! │  │ Consensus │  │   State   │  │ Scheduler │  │Governance │    │
//! │  │  (Raft)   │  │ (Events)  │  │  (Fair)   │  │ (Quotas)  │    │
//! │  └─────┬─────┘  └─────┬─────┘  └─────┬─────┘  └─────┬─────┘    │
//! │        │              │              │              │          │
//! │  ┌─────┴──────────────┴──────────────┴──────────────┴─────┐    │
//! │  │                    Storage Layer                        │    │
//! │  │            (Sharding, Replication, Quorum)             │    │
//! │  └─────────────────────────┬───────────────────────────────┘    │
//! │                            │                                    │
//! │  ┌─────────────────────────┴───────────────────────────────┐    │
//! │  │                   Network Layer                          │    │
//! │  │           (Protocol, Transport, Routing)                │    │
//! │  └──────────────────────────────────────────────────────────┘    │
//! │                                                                  │
//! │  ┌──────────┐  ┌─────────────┐  ┌──────────┐                   │
//! │  │  Fault   │  │Observability│  │ Security │                   │
//! │  │Tolerance │  │  (Tracing)  │  │  (mTLS)  │                   │
//! │  └──────────┘  └─────────────┘  └──────────┘                   │
//! └─────────────────────────────────────────────────────────────────┘
//! ```
//!
//! ## Module Organization
//!
//! - [`consensus`]: Raft consensus, leader election, log replication
//! - [`state`]: Event-sourced state, snapshots, checkpoints
//! - [`scheduler`]: Task scheduling, fairness, retries
//! - [`governance`]: Resource quotas, throttling, hotspot detection
//! - [`storage`]: Sharding, replication, quorum reads/writes
//! - [`network`]: Protocol, transport, routing
//! - [`fault`]: Failure detection, quarantine, chaos testing
//! - [`observability`]: Distributed tracing, causal logs, replay
//! - [`security`]: Identity, attestation, tenant isolation

pub mod consensus;
pub mod fault;
pub mod governance;
pub mod network;
pub mod observability;
pub mod scheduler;
pub mod security;
pub mod state;
pub mod storage;

/// Re-export commonly used types
pub mod prelude {
    // Consensus types
    pub use crate::consensus::{
        ConsensusError, ConsensusNode, RaftRole, ClusterConfig,
        LogEntry, ReplicationManager, ReplicationState,
        MembershipManager, MembershipConfig,
    };
    
    // State types
    pub use crate::state::{
        StateError, StateMachine, StoredEvent, Snapshot, 
        SnapshotManager, CheckpointCoordinator, Checkpoint,
    };
    
    // Scheduler types
    pub use crate::scheduler::{
        SchedulerError, Task, TaskId, Priority,
        Scheduler, FairShareScheduler, RetryPolicy,
    };
    
    // Governance types
    pub use crate::governance::{
        GovernanceError, TenantQuota, QuotaManager,
        TokenBucket, HotspotDetector,
    };
    
    // Storage types
    pub use crate::storage::{
        StorageError, ShardId, ConsistentHashRing,
        ReplicationManager as StorageReplicationManager,
        QuorumConfig, QuorumTracker,
    };
    
    // Network types
    pub use crate::network::{
        NetworkError, Message, MessageType, MessageCodec,
        ConnectionPool, Router, GossipProtocol,
    };
    
    // Fault tolerance types
    pub use crate::fault::{
        FaultError, HeartbeatDetector, PhiAccrualDetector,
        CircuitBreaker, QuarantineManager, ChaosController,
    };
    
    // Observability types
    pub use crate::observability::{
        ObservabilityError, TraceId, SpanId, Span, Tracer,
        Logger, LogLevel, EventRecorder, ReplayEngine,
    };
    
    // Security types
    pub use crate::security::{
        SecurityError, CertSubjectId, NodeIdentity, KeyPair,
        CertificateManager, AttestationVerifier,
        TenantManager, Sandbox, ProcessIsolator,
    };
    
    // Common types
    pub use crate::types::NodeId;
}

/// Common types used across modules
pub mod types {
    use std::time::Duration;

    /// Unique node identifier
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub struct NodeId(pub u64);

    impl NodeId {
        pub fn new(id: u64) -> Self {
            Self(id)
        }

        pub fn as_u64(&self) -> u64 {
            self.0
        }
    }

    /// Tenant identifier for multi-tenancy
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct TenantId(pub u64);

    /// Logical term number in Raft
    pub type Term = u64;

    /// Log index in Raft
    pub type LogIndex = u64;

    /// Timestamp in microseconds since epoch
    pub type Timestamp = u64;

    /// Configuration for timeouts
    #[derive(Debug, Clone)]
    pub struct TimeoutConfig {
        pub election_timeout: Duration,
        pub heartbeat_interval: Duration,
        pub rpc_timeout: Duration,
    }

    impl Default for TimeoutConfig {
        fn default() -> Self {
            Self {
                election_timeout: Duration::from_millis(300),
                heartbeat_interval: Duration::from_millis(100),
                rpc_timeout: Duration::from_millis(500),
            }
        }
    }
}

/// Error types for Grey Distributed
pub mod error {
    use thiserror::Error;

    #[derive(Debug, Error)]
    pub enum GreyError {
        #[error("Consensus error: {0}")]
        Consensus(#[from] crate::consensus::ConsensusError),

        #[error("State error: {0}")]
        State(#[from] crate::state::StateError),

        #[error("Scheduler error: {0}")]
        Scheduler(#[from] crate::scheduler::SchedulerError),

        #[error("Governance error: {0}")]
        Governance(#[from] crate::governance::GovernanceError),

        #[error("Storage error: {0}")]
        Storage(#[from] crate::storage::StorageError),

        #[error("Network error: {0}")]
        Network(#[from] crate::network::NetworkError),

        #[error("Fault error: {0}")]
        Fault(#[from] crate::fault::FaultError),

        #[error("Observability error: {0}")]
        Observability(#[from] crate::observability::ObservabilityError),

        #[error("Security error: {0}")]
        Security(#[from] crate::security::SecurityError),

        #[error("IO error: {0}")]
        Io(#[from] std::io::Error),
    }

    pub type Result<T> = std::result::Result<T, GreyError>;
}
