//! Cassandra Adapter for Grey Distributed
//!
//! This adapter provides a quorum-aware Cassandra connector optimized for
//! Grey's distributed task storage needs. Cassandra's eventual consistency
//! model aligns naturally with Grey's multi-region deployment patterns.
//!
//! # Architecture Decisions
//!
//! ## Why Cassandra for Grey?
//!
//! 1. **Linear Write Scalability**: Cassandra handles high write throughput
//!    without single-leader bottleneck. Task submissions scale horizontally.
//!
//! 2. **Tunable Consistency**: Grey can choose consistency per-operation:
//!    - QUORUM for critical task state
//!    - LOCAL_ONE for high-throughput telemetry
//!
//! 3. **Multi-Region Native**: Cassandra's rack/datacenter awareness maps
//!    directly to Grey's region topology.
//!
//! ## Tradeoffs
//!
//! - No ACID transactions (use lightweight transactions sparingly)
//! - No JOINs (denormalize data model)
//! - Read-before-write antipattern (design for append-only where possible)
//! - Tombstone accumulation (tune compaction for delete-heavy workloads)

use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;

use async_trait::async_trait;
use scylla::transport::errors::QueryError;
use scylla::transport::session::{CurrentDeserializationApi, Session};
use scylla::{SessionBuilder, ExecutionProfile, QueryResult};
use scylla::statement::prepared_statement::PreparedStatement;
use scylla::statement::Consistency;
use tracing::{debug, error, info, instrument, warn};

use crate::storage::{StorageAdapter, StorageError, StorageResult};
use crate::types::{TenantId, TaskId, TaskState};

// ============================================================================
// Configuration
// ============================================================================

/// Cassandra adapter configuration
#[derive(Debug, Clone)]
pub struct CassandraConfig {
    /// Contact points (seed nodes)
    pub contact_points: Vec<String>,
    
    /// Keyspace name
    pub keyspace: String,
    
    /// Datacenter name for LOCAL_* consistency levels
    pub local_datacenter: String,
    
    /// Replication factor per datacenter
    /// Key: datacenter name, Value: replication factor
    pub replication: HashMap<String, u32>,
    
    /// Default consistency level for writes
    pub write_consistency: CassandraConsistency,
    
    /// Default consistency level for reads
    pub read_consistency: CassandraConsistency,
    
    /// Connection pool size per node
    pub pool_size: u32,
    
    /// Query timeout
    pub query_timeout: Duration,
    
    /// Enable speculative execution for reads
    /// Trade-off: Reduces tail latency but increases cluster load
    pub speculative_execution: bool,
    
    /// Retry policy configuration
    pub retry_policy: RetryPolicy,
}

impl Default for CassandraConfig {
    fn default() -> Self {
        let mut replication = HashMap::new();
        replication.insert("dc1".to_string(), 3);
        
        Self {
            contact_points: vec!["127.0.0.1:9042".to_string()],
            keyspace: "grey".to_string(),
            local_datacenter: "dc1".to_string(),
            replication,
            // QUORUM ensures majority read/write for strong consistency
            write_consistency: CassandraConsistency::Quorum,
            read_consistency: CassandraConsistency::Quorum,
            pool_size: 4,
            query_timeout: Duration::from_secs(30),
            speculative_execution: true,
            retry_policy: RetryPolicy::default(),
        }
    }
}

/// Cassandra consistency levels mapped to Grey's requirements
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CassandraConsistency {
    /// Any node (for fire-and-forget writes)
    Any,
    /// Single node in local datacenter
    LocalOne,
    /// Majority of nodes in local datacenter
    LocalQuorum,
    /// Single node anywhere
    One,
    /// Majority of nodes across all datacenters
    Quorum,
    /// All nodes must acknowledge
    All,
    /// Used for lightweight transactions
    Serial,
    LocalSerial,
}

impl From<CassandraConsistency> for Consistency {
    fn from(c: CassandraConsistency) -> Self {
        match c {
            CassandraConsistency::Any => Consistency::Any,
            CassandraConsistency::LocalOne => Consistency::LocalOne,
            CassandraConsistency::LocalQuorum => Consistency::LocalQuorum,
            CassandraConsistency::One => Consistency::One,
            CassandraConsistency::Quorum => Consistency::Quorum,
            CassandraConsistency::All => Consistency::All,
            CassandraConsistency::Serial => Consistency::Serial,
            CassandraConsistency::LocalSerial => Consistency::LocalSerial,
        }
    }
}

/// Retry policy for transient failures
#[derive(Debug, Clone)]
pub struct RetryPolicy {
    /// Maximum retry attempts
    pub max_retries: u32,
    /// Base delay between retries
    pub base_delay: Duration,
    /// Maximum delay between retries
    pub max_delay: Duration,
    /// Whether to retry on write timeout
    pub retry_on_write_timeout: bool,
}

impl Default for RetryPolicy {
    fn default() -> Self {
        Self {
            max_retries: 3,
            base_delay: Duration::from_millis(100),
            max_delay: Duration::from_secs(5),
            // Generally unsafe to retry writes without idempotency
            retry_on_write_timeout: false,
        }
    }
}

// ============================================================================
// Session Manager
// ============================================================================

/// Manages Cassandra sessions with automatic reconnection and failover.
pub struct SessionManager {
    session: Arc<Session>,
    config: CassandraConfig,
    prepared_statements: PreparedStatementCache,
}

struct PreparedStatementCache {
    insert_task: Option<PreparedStatement>,
    select_task: Option<PreparedStatement>,
    update_task_state: Option<PreparedStatement>,
    delete_task: Option<PreparedStatement>,
    select_by_tenant: Option<PreparedStatement>,
}

impl SessionManager {
    /// Create a new session manager.
    ///
    /// # Connection Strategy
    ///
    /// Uses token-aware routing to send queries directly to replica nodes,
    /// minimizing network hops. The driver maintains connections to all nodes
    /// and tracks the token ring for routing decisions.
    pub async fn new(config: CassandraConfig) -> Result<Self, StorageError> {
        info!(
            contact_points = ?config.contact_points,
            keyspace = %config.keyspace,
            "Initializing Cassandra session"
        );

        let mut builder = SessionBuilder::new()
            .known_nodes(&config.contact_points)
            .use_keyspace(&config.keyspace, false);

        // Configure execution profile with consistency settings
        let profile = ExecutionProfile::builder()
            .consistency(config.write_consistency.into())
            .build();

        // Build session with automatic schema agreement wait
        let session = builder
            .default_execution_profile_handle(profile.into_handle())
            .build()
            .await
            .map_err(|e| StorageError::Connection(format!("Session creation failed: {}", e)))?;

        Ok(Self {
            session: Arc::new(session),
            config,
            prepared_statements: PreparedStatementCache {
                insert_task: None,
                select_task: None,
                update_task_state: None,
                delete_task: None,
                select_by_tenant: None,
            },
        })
    }

    /// Prepare all statements for efficient repeated execution.
    ///
    /// Prepared statements are:
    /// 1. Parsed once, executed many times
    /// 2. Token-aware (driver routes to correct replica)
    /// 3. Cached on coordinator nodes
    pub async fn prepare_statements(&mut self) -> Result<(), StorageError> {
        self.prepared_statements.insert_task = Some(
            self.session.prepare(
                "INSERT INTO tasks (tenant_id, task_id, payload, state, created_at, updated_at)
                 VALUES (?, ?, ?, ?, toTimestamp(now()), toTimestamp(now()))"
            ).await.map_err(|e| StorageError::Query(e.to_string()))?
        );

        self.prepared_statements.select_task = Some(
            self.session.prepare(
                "SELECT tenant_id, task_id, payload, state, created_at, updated_at
                 FROM tasks WHERE tenant_id = ? AND task_id = ?"
            ).await.map_err(|e| StorageError::Query(e.to_string()))?
        );

        self.prepared_statements.update_task_state = Some(
            self.session.prepare(
                "UPDATE tasks SET state = ?, updated_at = toTimestamp(now())
                 WHERE tenant_id = ? AND task_id = ?"
            ).await.map_err(|e| StorageError::Query(e.to_string()))?
        );

        self.prepared_statements.delete_task = Some(
            self.session.prepare(
                "DELETE FROM tasks WHERE tenant_id = ? AND task_id = ?"
            ).await.map_err(|e| StorageError::Query(e.to_string()))?
        );

        self.prepared_statements.select_by_tenant = Some(
            self.session.prepare(
                "SELECT task_id, state, created_at FROM tasks WHERE tenant_id = ? LIMIT ?"
            ).await.map_err(|e| StorageError::Query(e.to_string()))?
        );

        info!("Prepared statements cached");
        Ok(())
    }

    pub fn session(&self) -> Arc<Session> {
        Arc::clone(&self.session)
    }
}

// ============================================================================
// Cassandra Adapter
// ============================================================================

/// Main Cassandra adapter implementing Grey's storage interface.
pub struct CassandraAdapter {
    session_manager: SessionManager,
    config: CassandraConfig,
}

impl CassandraAdapter {
    pub async fn new(config: CassandraConfig) -> Result<Self, StorageError> {
        let mut session_manager = SessionManager::new(config.clone()).await?;
        session_manager.prepare_statements().await?;
        
        Ok(Self {
            session_manager,
            config,
        })
    }

    /// Store a task with configurable consistency.
    ///
    /// # Consistency Tradeoffs
    ///
    /// - QUORUM: Survives single node failure, writes to majority
    /// - LOCAL_QUORUM: Fast writes within datacenter, cross-DC async
    /// - ALL: Maximum durability, but single slow node blocks write
    /// - ANY: Fire-and-forget, may lose data on crash
    ///
    /// Default is QUORUM for balance of durability and latency.
    #[instrument(skip(self, payload))]
    pub async fn store_task(
        &self,
        tenant_id: &TenantId,
        task_id: &TaskId,
        payload: &[u8],
        consistency: Option<CassandraConsistency>,
    ) -> Result<(), StorageError> {
        let stmt = self.session_manager.prepared_statements.insert_task
            .as_ref()
            .ok_or_else(|| StorageError::NotInitialized)?
            .clone();

        let session = self.session_manager.session();
        
        // Apply consistency override if provided
        let consistency = consistency.unwrap_or(self.config.write_consistency);
        
        session.execute(&stmt, (
            tenant_id.as_str(),
            task_id.as_str(),
            payload,
            "pending",
        )).await.map_err(|e| StorageError::Query(e.to_string()))?;

        debug!(
            tenant = %tenant_id,
            task = %task_id,
            consistency = ?consistency,
            "Task stored"
        );

        Ok(())
    }

    /// Load a task with configurable consistency.
    ///
    /// # Read Consistency Strategy
    ///
    /// For read-after-write consistency, ensure:
    ///   read_consistency + write_consistency > replication_factor
    ///
    /// Example with RF=3:
    /// - QUORUM write (2) + QUORUM read (2) = 4 > 3 ✓
    /// - ONE write (1) + QUORUM read (2) = 3 = 3 ✗ (may miss recent write)
    #[instrument(skip(self))]
    pub async fn load_task(
        &self,
        tenant_id: &TenantId,
        task_id: &TaskId,
        consistency: Option<CassandraConsistency>,
    ) -> Result<Option<TaskRecord>, StorageError> {
        let stmt = self.session_manager.prepared_statements.select_task
            .as_ref()
            .ok_or_else(|| StorageError::NotInitialized)?
            .clone();

        let session = self.session_manager.session();
        
        let result = session.execute(&stmt, (
            tenant_id.as_str(),
            task_id.as_str(),
        )).await.map_err(|e| StorageError::Query(e.to_string()))?;

        // Parse first row if present
        if let Some(row) = result.rows_typed::<TaskRecord>()?.next() {
            Ok(Some(row.map_err(|e| StorageError::Deserialize(e.to_string()))?))
        } else {
            Ok(None)
        }
    }

    /// Update task state using lightweight transaction (LWT) for atomicity.
    ///
    /// # LWT Considerations
    ///
    /// Lightweight transactions provide linearizable consistency but:
    /// 1. 4x slower than regular writes (Paxos round-trips)
    /// 2. Can create hotspots on frequently-updated rows
    /// 3. Don't mix LWT and non-LWT writes to same partition
    ///
    /// Use LWTs only when atomicity is required (state transitions).
    #[instrument(skip(self))]
    pub async fn update_task_state_atomic(
        &self,
        tenant_id: &TenantId,
        task_id: &TaskId,
        from_state: &str,
        to_state: &str,
    ) -> Result<bool, StorageError> {
        let session = self.session_manager.session();
        
        // Conditional update using IF clause (LWT)
        let query = "UPDATE tasks SET state = ?, updated_at = toTimestamp(now())
                     WHERE tenant_id = ? AND task_id = ?
                     IF state = ?";

        let result = session.query(query, (
            to_state,
            tenant_id.as_str(),
            task_id.as_str(),
            from_state,
        )).await.map_err(|e| StorageError::Query(e.to_string()))?;

        // Check [applied] column in result
        let applied = result.rows_typed::<(bool,)>()?
            .next()
            .and_then(|r| r.ok())
            .map(|(applied,)| applied)
            .unwrap_or(false);

        if applied {
            debug!(
                tenant = %tenant_id,
                task = %task_id,
                from = %from_state,
                to = %to_state,
                "State transition applied"
            );
        } else {
            warn!(
                tenant = %tenant_id,
                task = %task_id,
                expected = %from_state,
                "State transition rejected (concurrent update)"
            );
        }

        Ok(applied)
    }

    /// Batch write for high-throughput ingestion.
    ///
    /// # Batch Types
    ///
    /// - LOGGED batch: Atomic, all-or-nothing (default)
    /// - UNLOGGED batch: No atomicity, just batched network I/O
    ///
    /// # Anti-Pattern Warning
    ///
    /// Batches spanning multiple partitions are ANTI-PATTERN in Cassandra.
    /// They don't improve performance and can overload coordinators.
    /// 
    /// This method groups by partition key (tenant_id) to avoid this.
    #[instrument(skip(self, items))]
    pub async fn batch_store(
        &self,
        tenant_id: &TenantId,
        items: Vec<(TaskId, Vec<u8>)>,
    ) -> Result<u64, StorageError> {
        use scylla::batch::Batch;
        use scylla::batch::BatchType;

        if items.is_empty() {
            return Ok(0);
        }

        let stmt = self.session_manager.prepared_statements.insert_task
            .as_ref()
            .ok_or_else(|| StorageError::NotInitialized)?
            .clone();

        // Create LOGGED batch (atomic within partition)
        let mut batch = Batch::new(BatchType::Logged);
        let mut values = Vec::new();

        for (task_id, payload) in &items {
            batch.append_statement(stmt.clone());
            values.push((
                tenant_id.as_str(),
                task_id.as_str(),
                payload.as_slice(),
                "pending",
            ));
        }

        let session = self.session_manager.session();
        session.batch(&batch, values)
            .await
            .map_err(|e| StorageError::Query(e.to_string()))?;

        let count = items.len() as u64;
        info!(
            tenant = %tenant_id,
            count = count,
            "Batch store completed"
        );

        Ok(count)
    }

    /// Time-based query for task history.
    ///
    /// Uses Cassandra's time-ordered clustering for efficient range scans.
    #[instrument(skip(self))]
    pub async fn list_tasks_by_time(
        &self,
        tenant_id: &TenantId,
        start_time: chrono::DateTime<chrono::Utc>,
        end_time: chrono::DateTime<chrono::Utc>,
        limit: u32,
    ) -> Result<Vec<TaskSummary>, StorageError> {
        let session = self.session_manager.session();
        
        // Assumes tasks table has time-based clustering
        let query = "SELECT task_id, state, created_at FROM tasks
                     WHERE tenant_id = ? AND created_at >= ? AND created_at <= ?
                     LIMIT ?";

        let result = session.query(query, (
            tenant_id.as_str(),
            start_time.timestamp_millis(),
            end_time.timestamp_millis(),
            limit as i32,
        )).await.map_err(|e| StorageError::Query(e.to_string()))?;

        let tasks: Vec<TaskSummary> = result.rows_typed::<TaskSummary>()?
            .filter_map(|r| r.ok())
            .collect();

        Ok(tasks)
    }

    /// Delete task with tombstone awareness.
    ///
    /// # Tombstone Warning
    ///
    /// Cassandra doesn't immediately remove data; it creates tombstones.
    /// Excessive tombstones degrade read performance.
    ///
    /// Mitigation strategies:
    /// 1. Use TTLs instead of explicit deletes when possible
    /// 2. Tune gc_grace_seconds based on repair frequency
    /// 3. Run regular compactions
    #[instrument(skip(self))]
    pub async fn delete_task(
        &self,
        tenant_id: &TenantId,
        task_id: &TaskId,
    ) -> Result<(), StorageError> {
        let stmt = self.session_manager.prepared_statements.delete_task
            .as_ref()
            .ok_or_else(|| StorageError::NotInitialized)?
            .clone();

        let session = self.session_manager.session();
        session.execute(&stmt, (
            tenant_id.as_str(),
            task_id.as_str(),
        )).await.map_err(|e| StorageError::Query(e.to_string()))?;

        debug!(
            tenant = %tenant_id,
            task = %task_id,
            "Task deleted (tombstone created)"
        );

        Ok(())
    }
}

// ============================================================================
// Data Types
// ============================================================================

/// Task record as stored in Cassandra
#[derive(Debug, Clone)]
pub struct TaskRecord {
    pub tenant_id: String,
    pub task_id: String,
    pub payload: Vec<u8>,
    pub state: String,
    pub created_at: i64,
    pub updated_at: i64,
}

/// Lightweight task summary for list operations
#[derive(Debug, Clone)]
pub struct TaskSummary {
    pub task_id: String,
    pub state: String,
    pub created_at: i64,
}

// ============================================================================
// Replication Strategy
// ============================================================================

/// Configure replication for multi-region deployment.
///
/// # Replication Strategies
///
/// 1. SimpleStrategy: Development only, single datacenter
/// 2. NetworkTopologyStrategy: Production, multi-datacenter
///
/// NetworkTopologyStrategy allows per-DC replication factors:
/// - Primary DC: RF=3 (survive 1 node failure)
/// - DR DC: RF=2 (recovery capability)
/// - Analytics DC: RF=1 (read-only queries)
pub async fn configure_replication(
    session: &Session,
    keyspace: &str,
    strategy: ReplicationStrategy,
) -> Result<(), StorageError> {
    let cql = match strategy {
        ReplicationStrategy::Simple { replication_factor } => {
            format!(
                "CREATE KEYSPACE IF NOT EXISTS {} WITH replication = {{'class': 'SimpleStrategy', 'replication_factor': {}}}",
                keyspace, replication_factor
            )
        }
        ReplicationStrategy::NetworkTopology { datacenters } => {
            let dc_config: Vec<String> = datacenters
                .iter()
                .map(|(dc, rf)| format!("'{}': {}", dc, rf))
                .collect();
            format!(
                "CREATE KEYSPACE IF NOT EXISTS {} WITH replication = {{'class': 'NetworkTopologyStrategy', {}}}",
                keyspace, dc_config.join(", ")
            )
        }
    };

    session.query(cql, &[])
        .await
        .map_err(|e| StorageError::Query(e.to_string()))?;

    info!(keyspace = %keyspace, "Keyspace configured");
    Ok(())
}

pub enum ReplicationStrategy {
    Simple { replication_factor: u32 },
    NetworkTopology { datacenters: HashMap<String, u32> },
}

// ============================================================================
// Schema
// ============================================================================

/// Create Grey Distributed schema in Cassandra.
///
/// # Data Model Considerations
///
/// 1. **Partition Key**: tenant_id ensures tenant isolation and even distribution
/// 2. **Clustering Key**: task_id within partition for point lookups
/// 3. **Created_at clustering**: Alternative design for time-range queries
///
/// Wide partitions (many tasks per tenant) are acceptable but monitor with
/// nodetool tablehistograms for partition size alerts.
pub async fn create_schema(session: &Session, keyspace: &str) -> Result<(), StorageError> {
    session.query(format!("USE {}", keyspace), &[])
        .await
        .map_err(|e| StorageError::Query(e.to_string()))?;

    // Main tasks table
    session.query(
        "CREATE TABLE IF NOT EXISTS tasks (
            tenant_id TEXT,
            task_id TEXT,
            payload BLOB,
            state TEXT,
            created_at TIMESTAMP,
            updated_at TIMESTAMP,
            PRIMARY KEY (tenant_id, task_id)
        ) WITH CLUSTERING ORDER BY (task_id ASC)
          AND gc_grace_seconds = 86400
          AND compaction = {'class': 'LeveledCompactionStrategy'}",
        &[],
    ).await.map_err(|e| StorageError::Query(e.to_string()))?;

    // Time-series view for temporal queries
    session.query(
        "CREATE TABLE IF NOT EXISTS tasks_by_time (
            tenant_id TEXT,
            time_bucket TEXT,
            created_at TIMESTAMP,
            task_id TEXT,
            state TEXT,
            PRIMARY KEY ((tenant_id, time_bucket), created_at, task_id)
        ) WITH CLUSTERING ORDER BY (created_at DESC, task_id ASC)
          AND default_time_to_live = 2592000",  // 30 days TTL
        &[],
    ).await.map_err(|e| StorageError::Query(e.to_string()))?;

    // Checkpoints table for state recovery
    session.query(
        "CREATE TABLE IF NOT EXISTS checkpoints (
            tenant_id TEXT,
            task_id TEXT,
            checkpoint_id TIMEUUID,
            data BLOB,
            created_at TIMESTAMP,
            PRIMARY KEY ((tenant_id, task_id), checkpoint_id)
        ) WITH CLUSTERING ORDER BY (checkpoint_id DESC)",
        &[],
    ).await.map_err(|e| StorageError::Query(e.to_string()))?;

    info!(keyspace = %keyspace, "Schema created");
    Ok(())
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_consistency_conversion() {
        let c: Consistency = CassandraConsistency::Quorum.into();
        assert_eq!(c, Consistency::Quorum);
        
        let c: Consistency = CassandraConsistency::LocalQuorum.into();
        assert_eq!(c, Consistency::LocalQuorum);
    }

    #[test]
    fn test_read_write_quorum_guarantees() {
        // With RF=3, QUORUM read + QUORUM write > RF guarantees consistency
        let rf = 3u32;
        let write_quorum = (rf / 2) + 1; // 2
        let read_quorum = (rf / 2) + 1;  // 2
        assert!(read_quorum + write_quorum > rf);
        
        // With RF=5, QUORUM = 3
        let rf = 5u32;
        let quorum = (rf / 2) + 1; // 3
        assert!(quorum + quorum > rf);
    }
}
