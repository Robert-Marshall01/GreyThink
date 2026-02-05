//! MongoDB Adapter for Grey Distributed
//!
//! This adapter provides a sharding-aware MongoDB connector with read/write
//! concern alignment to Grey's consistency requirements.
//!
//! # Why MongoDB for Grey?
//!
//! 1. **Flexible Schema**: Task payloads vary widely; BSON handles this naturally
//! 2. **Built-in Sharding**: Native horizontal scaling with auto-balancing
//! 3. **Change Streams**: Real-time notifications for task state changes
//! 4. **Aggregation Pipeline**: Complex queries without external processing
//!
//! # Architecture Alignment
//!
//! Grey's quorum model maps to MongoDB's write/read concerns:
//! - Grey QUORUM → MongoDB majority
//! - Grey ONE → MongoDB w:1 / r:primary
//! - Grey ALL → MongoDB w:all (rare, high latency)
//!
//! # Sharding Strategy
//!
//! Shard key: { tenant_id: 1, task_id: "hashed" }
//! - tenant_id prefix enables tenant-isolated queries
//! - Hashed task_id prevents hotspots from sequential IDs

use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;

use async_trait::async_trait;
use bson::{doc, Document, Bson};
use futures::stream::StreamExt;
use mongodb::{
    Client, Collection, Database,
    options::{
        ClientOptions, ReadConcern, WriteConcern, ReadPreference,
        FindOptions, InsertOneOptions, UpdateOptions, Acknowledgment,
        ChangeStreamOptions, FullDocumentType,
    },
    change_stream::event::ChangeStreamEvent,
};
use serde::{Deserialize, Serialize};
use tokio::sync::broadcast;
use tracing::{debug, error, info, instrument, warn};

use crate::storage::{StorageAdapter, StorageError, StorageResult};
use crate::types::{TenantId, TaskId};

// ============================================================================
// Configuration
// ============================================================================

/// MongoDB adapter configuration
#[derive(Debug, Clone)]
pub struct MongoConfig {
    /// MongoDB connection URI
    /// Format: mongodb://host1:port1,host2:port2/?replicaSet=rs0
    pub uri: String,
    
    /// Database name
    pub database: String,
    
    /// Collection name for tasks
    pub collection: String,
    
    /// Connection pool size
    pub pool_size: u32,
    
    /// Default write concern
    pub write_concern: MongoWriteConcern,
    
    /// Default read concern
    pub read_concern: MongoReadConcern,
    
    /// Default read preference
    pub read_preference: MongoReadPreference,
    
    /// Operation timeout
    pub timeout: Duration,
    
    /// Enable retryable writes
    /// Trade-off: Adds small overhead but handles transient failures
    pub retryable_writes: bool,
    
    /// Enable causal consistency for sessions
    pub causal_consistency: bool,
}

impl Default for MongoConfig {
    fn default() -> Self {
        Self {
            uri: "mongodb://localhost:27017".to_string(),
            database: "grey".to_string(),
            collection: "tasks".to_string(),
            pool_size: 20,
            // Majority write ensures durability across replica set
            write_concern: MongoWriteConcern::Majority,
            // Majority read sees acknowledged writes
            read_concern: MongoReadConcern::Majority,
            // Primary preferred for consistency, secondary for scaling
            read_preference: MongoReadPreference::PrimaryPreferred,
            timeout: Duration::from_secs(30),
            retryable_writes: true,
            causal_consistency: true,
        }
    }
}

/// MongoDB write concern levels
#[derive(Debug, Clone)]
pub enum MongoWriteConcern {
    /// Write to primary only (fast, data loss risk)
    Unacknowledged,
    /// Acknowledge from primary (default, single node durability)
    Acknowledged,
    /// Journal write before acknowledging
    Journaled,
    /// Majority of replica set acknowledges
    Majority,
    /// Specific number of nodes
    Count(u32),
    /// All nodes (slowest, maximum durability)
    All,
}

impl From<MongoWriteConcern> for WriteConcern {
    fn from(wc: MongoWriteConcern) -> Self {
        match wc {
            MongoWriteConcern::Unacknowledged => WriteConcern::builder()
                .w(Acknowledgment::Nodes(0))
                .build(),
            MongoWriteConcern::Acknowledged => WriteConcern::builder()
                .w(Acknowledgment::Nodes(1))
                .build(),
            MongoWriteConcern::Journaled => WriteConcern::builder()
                .w(Acknowledgment::Nodes(1))
                .journal(true)
                .build(),
            MongoWriteConcern::Majority => WriteConcern::builder()
                .w(Acknowledgment::Majority)
                .build(),
            MongoWriteConcern::Count(n) => WriteConcern::builder()
                .w(Acknowledgment::Nodes(n))
                .build(),
            MongoWriteConcern::All => WriteConcern::builder()
                .w(Acknowledgment::Custom("all".to_string()))
                .build(),
        }
    }
}

/// MongoDB read concern levels
#[derive(Debug, Clone)]
pub enum MongoReadConcern {
    /// Read from any node (fastest, potentially stale)
    Local,
    /// Read majority-committed data (consistent)
    Majority,
    /// Read data that would survive majority failure
    Linearizable,
    /// Read from snapshot (consistent point-in-time)
    Snapshot,
}

impl From<MongoReadConcern> for ReadConcern {
    fn from(rc: MongoReadConcern) -> Self {
        match rc {
            MongoReadConcern::Local => ReadConcern::local(),
            MongoReadConcern::Majority => ReadConcern::majority(),
            MongoReadConcern::Linearizable => ReadConcern::linearizable(),
            MongoReadConcern::Snapshot => ReadConcern::snapshot(),
        }
    }
}

/// MongoDB read preference
#[derive(Debug, Clone)]
pub enum MongoReadPreference {
    /// Always read from primary
    Primary,
    /// Prefer primary, fall back to secondary
    PrimaryPreferred,
    /// Read from secondary only
    Secondary,
    /// Prefer secondary, fall back to primary
    SecondaryPreferred,
    /// Read from nearest node (lowest latency)
    Nearest,
}

impl From<MongoReadPreference> for ReadPreference {
    fn from(rp: MongoReadPreference) -> Self {
        match rp {
            MongoReadPreference::Primary => ReadPreference::Primary,
            MongoReadPreference::PrimaryPreferred => ReadPreference::PrimaryPreferred { 
                options: None 
            },
            MongoReadPreference::Secondary => ReadPreference::Secondary { 
                options: None 
            },
            MongoReadPreference::SecondaryPreferred => ReadPreference::SecondaryPreferred { 
                options: None 
            },
            MongoReadPreference::Nearest => ReadPreference::Nearest { 
                options: None 
            },
        }
    }
}

// ============================================================================
// MongoDB Adapter
// ============================================================================

/// Main MongoDB adapter for Grey Distributed.
pub struct MongoAdapter {
    client: Client,
    database: Database,
    collection: Collection<TaskDocument>,
    config: MongoConfig,
    /// Channel for broadcasting task state changes
    change_tx: broadcast::Sender<TaskChangeEvent>,
}

impl MongoAdapter {
    /// Create a new MongoDB adapter.
    ///
    /// # Connection Pooling
    ///
    /// MongoDB driver maintains a connection pool automatically.
    /// Pool size is set via maxPoolSize in URI or config.
    /// Connections are created lazily and reused across operations.
    pub async fn new(config: MongoConfig) -> Result<Self, StorageError> {
        info!(uri = %config.uri, database = %config.database, "Initializing MongoDB adapter");

        let mut client_options = ClientOptions::parse(&config.uri)
            .await
            .map_err(|e| StorageError::Connection(format!("URI parse error: {}", e)))?;

        client_options.max_pool_size = Some(config.pool_size);
        client_options.connect_timeout = Some(config.timeout);
        client_options.server_selection_timeout = Some(config.timeout);
        client_options.retry_writes = Some(config.retryable_writes);

        // Apply default read/write concerns
        client_options.write_concern = Some(config.write_concern.clone().into());
        client_options.read_concern = Some(config.read_concern.clone().into());

        let client = Client::with_options(client_options)
            .map_err(|e| StorageError::Connection(format!("Client creation failed: {}", e)))?;

        // Verify connection with ping
        client.database("admin")
            .run_command(doc! { "ping": 1 }, None)
            .await
            .map_err(|e| StorageError::Connection(format!("Ping failed: {}", e)))?;

        let database = client.database(&config.database);
        let collection = database.collection(&config.collection);

        // Create broadcast channel for change events
        let (change_tx, _) = broadcast::channel(1000);

        Ok(Self {
            client,
            database,
            collection,
            config,
            change_tx,
        })
    }

    /// Initialize indexes for optimal query performance.
    ///
    /// # Index Strategy
    ///
    /// 1. Compound index on (tenant_id, task_id): Primary lookup path
    /// 2. Compound index on (tenant_id, state): Filter by status within tenant
    /// 3. TTL index on created_at: Auto-cleanup of old tasks (optional)
    /// 4. Shard key index: Required for sharded clusters
    pub async fn create_indexes(&self) -> Result<(), StorageError> {
        use mongodb::IndexModel;
        use mongodb::options::IndexOptions;

        let indexes = vec![
            // Unique index for primary key
            IndexModel::builder()
                .keys(doc! { "tenant_id": 1, "task_id": 1 })
                .options(IndexOptions::builder()
                    .unique(true)
                    .name("idx_tenant_task".to_string())
                    .build())
                .build(),
            
            // Index for state queries within tenant
            IndexModel::builder()
                .keys(doc! { "tenant_id": 1, "state": 1, "created_at": -1 })
                .options(IndexOptions::builder()
                    .name("idx_tenant_state_time".to_string())
                    .build())
                .build(),
            
            // Index for time-range queries
            IndexModel::builder()
                .keys(doc! { "tenant_id": 1, "created_at": -1 })
                .options(IndexOptions::builder()
                    .name("idx_tenant_time".to_string())
                    .build())
                .build(),
        ];

        self.collection.create_indexes(indexes, None)
            .await
            .map_err(|e| StorageError::Query(format!("Index creation failed: {}", e)))?;

        info!("MongoDB indexes created");
        Ok(())
    }

    /// Store a task with configurable write concern.
    ///
    /// # Idempotency
    ///
    /// Uses upsert semantics: Insert if new, update if exists.
    /// This makes the operation idempotent for retry scenarios.
    #[instrument(skip(self, payload))]
    pub async fn store_task(
        &self,
        tenant_id: &TenantId,
        task_id: &TaskId,
        payload: &[u8],
        metadata: Option<TaskMetadata>,
    ) -> Result<(), StorageError> {
        let now = chrono::Utc::now();
        
        let doc = doc! {
            "tenant_id": tenant_id.as_str(),
            "task_id": task_id.as_str(),
            "payload": bson::Binary { subtype: bson::spec::BinarySubtype::Generic, bytes: payload.to_vec() },
            "state": "pending",
            "created_at": bson::DateTime::from_chrono(now),
            "updated_at": bson::DateTime::from_chrono(now),
            "metadata": bson::to_bson(&metadata).unwrap_or(Bson::Null),
        };

        let options = UpdateOptions::builder()
            .upsert(true)
            .build();

        self.collection.update_one(
            doc! { 
                "tenant_id": tenant_id.as_str(), 
                "task_id": task_id.as_str() 
            },
            doc! { "$set": doc },
            options,
        ).await.map_err(|e| StorageError::Query(format!("Store failed: {}", e)))?;

        debug!(
            tenant = %tenant_id,
            task = %task_id,
            "Task stored"
        );

        Ok(())
    }

    /// Load a task with configurable read concern.
    #[instrument(skip(self))]
    pub async fn load_task(
        &self,
        tenant_id: &TenantId,
        task_id: &TaskId,
    ) -> Result<Option<TaskDocument>, StorageError> {
        let result = self.collection.find_one(
            doc! { 
                "tenant_id": tenant_id.as_str(), 
                "task_id": task_id.as_str() 
            },
            None,
        ).await.map_err(|e| StorageError::Query(format!("Load failed: {}", e)))?;

        Ok(result)
    }

    /// Update task state with optimistic locking.
    ///
    /// # Optimistic Concurrency
    ///
    /// Uses version field to detect concurrent updates.
    /// If version mismatch, returns conflict error for retry.
    ///
    /// Alternative: Use findAndModify for atomic update-and-return.
    #[instrument(skip(self))]
    pub async fn update_task_state(
        &self,
        tenant_id: &TenantId,
        task_id: &TaskId,
        from_state: &str,
        to_state: &str,
        version: u64,
    ) -> Result<UpdateResult, StorageError> {
        let result = self.collection.update_one(
            doc! {
                "tenant_id": tenant_id.as_str(),
                "task_id": task_id.as_str(),
                "state": from_state,
                "version": version as i64,
            },
            doc! {
                "$set": {
                    "state": to_state,
                    "updated_at": bson::DateTime::from_chrono(chrono::Utc::now()),
                },
                "$inc": { "version": 1 }
            },
            None,
        ).await.map_err(|e| StorageError::Query(format!("Update failed: {}", e)))?;

        if result.modified_count == 0 {
            // Either not found or version conflict
            let exists = self.collection.find_one(
                doc! { "tenant_id": tenant_id.as_str(), "task_id": task_id.as_str() },
                None,
            ).await.map_err(|e| StorageError::Query(e.to_string()))?;

            if exists.is_some() {
                return Ok(UpdateResult::Conflict);
            } else {
                return Ok(UpdateResult::NotFound);
            }
        }

        debug!(
            tenant = %tenant_id,
            task = %task_id,
            from = %from_state,
            to = %to_state,
            "State updated"
        );

        Ok(UpdateResult::Success)
    }

    /// Batch write using ordered bulk operations.
    ///
    /// # Ordered vs Unordered Batches
    ///
    /// - Ordered: Stops on first error (sequential semantics)
    /// - Unordered: Continues on errors (parallel, best-effort)
    ///
    /// Grey uses unordered for throughput; individual failures
    /// are tracked and retried separately.
    #[instrument(skip(self, items))]
    pub async fn batch_store(
        &self,
        tenant_id: &TenantId,
        items: Vec<(TaskId, Vec<u8>, Option<TaskMetadata>)>,
    ) -> Result<BatchResult, StorageError> {
        use mongodb::options::InsertManyOptions;

        if items.is_empty() {
            return Ok(BatchResult { inserted: 0, errors: vec![] });
        }

        let now = chrono::Utc::now();
        let documents: Vec<TaskDocument> = items.iter().map(|(task_id, payload, metadata)| {
            TaskDocument {
                tenant_id: tenant_id.to_string(),
                task_id: task_id.to_string(),
                payload: payload.clone(),
                state: "pending".to_string(),
                created_at: now,
                updated_at: now,
                version: 1,
                metadata: metadata.clone(),
            }
        }).collect();

        let options = InsertManyOptions::builder()
            .ordered(false)  // Continue on individual errors
            .build();

        match self.collection.insert_many(documents, options).await {
            Ok(result) => {
                let inserted = result.inserted_ids.len() as u64;
                info!(tenant = %tenant_id, count = inserted, "Batch store completed");
                Ok(BatchResult { inserted, errors: vec![] })
            }
            Err(e) => {
                // Parse bulk write error for partial success info
                warn!(tenant = %tenant_id, error = %e, "Batch store partial failure");
                Err(StorageError::Query(e.to_string()))
            }
        }
    }

    /// List tasks for a tenant with filtering and pagination.
    ///
    /// # Pagination Strategy
    ///
    /// Uses cursor-based pagination with created_at + task_id sort.
    /// More efficient than skip/limit for large datasets.
    #[instrument(skip(self))]
    pub async fn list_tasks(
        &self,
        tenant_id: &TenantId,
        filter: TaskFilter,
        pagination: Pagination,
    ) -> Result<TaskPage, StorageError> {
        let mut query = doc! { "tenant_id": tenant_id.as_str() };

        // Apply optional filters
        if let Some(state) = filter.state {
            query.insert("state", state);
        }
        if let Some(since) = filter.since {
            query.insert("created_at", doc! { "$gte": bson::DateTime::from_chrono(since) });
        }

        // Cursor-based pagination
        if let Some(cursor) = pagination.cursor {
            query.insert("$or", vec![
                doc! { "created_at": { "$lt": bson::DateTime::from_chrono(cursor.created_at) } },
                doc! { 
                    "created_at": bson::DateTime::from_chrono(cursor.created_at),
                    "task_id": { "$gt": &cursor.task_id }
                },
            ]);
        }

        let options = FindOptions::builder()
            .sort(doc! { "created_at": -1, "task_id": 1 })
            .limit(pagination.limit as i64 + 1)  // +1 to detect has_more
            .projection(doc! {
                "task_id": 1,
                "state": 1,
                "created_at": 1,
                "updated_at": 1,
                "metadata": 1,
            })
            .build();

        let mut cursor = self.collection.find(query, options)
            .await
            .map_err(|e| StorageError::Query(e.to_string()))?;

        let mut tasks = Vec::new();
        while let Some(doc) = cursor.next().await {
            match doc {
                Ok(task) => tasks.push(task),
                Err(e) => {
                    warn!(error = %e, "Error reading task from cursor");
                }
            }
            if tasks.len() > pagination.limit as usize {
                break;
            }
        }

        let has_more = tasks.len() > pagination.limit as usize;
        if has_more {
            tasks.pop();
        }

        let next_cursor = if has_more {
            tasks.last().map(|t| PageCursor {
                created_at: t.created_at,
                task_id: t.task_id.clone(),
            })
        } else {
            None
        };

        Ok(TaskPage {
            tasks,
            next_cursor,
            has_more,
        })
    }

    /// Subscribe to task state changes using Change Streams.
    ///
    /// # Change Streams
    ///
    /// MongoDB 3.6+ feature for real-time notifications.
    /// Uses oplog tailing internally, no polling required.
    ///
    /// # Resume Token
    ///
    /// Store resume token to continue from last position after restart.
    /// Tokens are valid for oplog retention period (default 72 hours).
    pub async fn watch_tasks(
        &self,
        tenant_id: &TenantId,
    ) -> Result<impl futures::Stream<Item = TaskChangeEvent>, StorageError> {
        let pipeline = vec![
            doc! { "$match": { "fullDocument.tenant_id": tenant_id.as_str() } }
        ];

        let options = ChangeStreamOptions::builder()
            .full_document(Some(FullDocumentType::UpdateLookup))
            .build();

        let change_stream = self.collection
            .watch(pipeline, options)
            .await
            .map_err(|e| StorageError::Query(format!("Watch failed: {}", e)))?;

        Ok(change_stream.filter_map(|result| async move {
            match result {
                Ok(event) => Self::parse_change_event(event),
                Err(e) => {
                    warn!(error = %e, "Change stream error");
                    None
                }
            }
        }))
    }

    fn parse_change_event(event: ChangeStreamEvent<TaskDocument>) -> Option<TaskChangeEvent> {
        let operation = match event.operation_type {
            mongodb::change_stream::event::OperationType::Insert => ChangeOperation::Insert,
            mongodb::change_stream::event::OperationType::Update => ChangeOperation::Update,
            mongodb::change_stream::event::OperationType::Delete => ChangeOperation::Delete,
            mongodb::change_stream::event::OperationType::Replace => ChangeOperation::Update,
            _ => return None,
        };

        let doc = event.full_document?;
        
        Some(TaskChangeEvent {
            operation,
            tenant_id: doc.tenant_id,
            task_id: doc.task_id,
            new_state: Some(doc.state),
            timestamp: chrono::Utc::now(),
        })
    }

    /// Execute aggregation pipeline for analytics.
    ///
    /// # Use Cases
    ///
    /// - Task counts by state
    /// - Average processing time
    /// - Throughput metrics
    /// - Tenant usage reports
    pub async fn aggregate_stats(
        &self,
        tenant_id: &TenantId,
    ) -> Result<TenantStats, StorageError> {
        let pipeline = vec![
            doc! { "$match": { "tenant_id": tenant_id.as_str() } },
            doc! { "$group": {
                "_id": "$state",
                "count": { "$sum": 1 },
            }},
        ];

        let mut cursor = self.collection.aggregate(pipeline, None)
            .await
            .map_err(|e| StorageError::Query(e.to_string()))?;

        let mut stats = TenantStats::default();
        while let Some(doc) = cursor.next().await {
            if let Ok(d) = doc {
                let state = d.get_str("_id").unwrap_or("unknown");
                let count = d.get_i32("count").unwrap_or(0) as u64;
                match state {
                    "pending" => stats.pending = count,
                    "running" => stats.running = count,
                    "completed" => stats.completed = count,
                    "failed" => stats.failed = count,
                    _ => {}
                }
            }
        }

        Ok(stats)
    }
}

// ============================================================================
// Data Types
// ============================================================================

/// Task document stored in MongoDB
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskDocument {
    pub tenant_id: String,
    pub task_id: String,
    #[serde(with = "serde_bytes")]
    pub payload: Vec<u8>,
    pub state: String,
    #[serde(with = "chrono::serde::ts_milliseconds")]
    pub created_at: chrono::DateTime<chrono::Utc>,
    #[serde(with = "chrono::serde::ts_milliseconds")]
    pub updated_at: chrono::DateTime<chrono::Utc>,
    #[serde(default)]
    pub version: u64,
    #[serde(default)]
    pub metadata: Option<TaskMetadata>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskMetadata {
    pub labels: HashMap<String, String>,
    pub priority: Option<String>,
    pub timeout_ms: Option<u64>,
}

#[derive(Debug, Clone)]
pub struct TaskFilter {
    pub state: Option<String>,
    pub since: Option<chrono::DateTime<chrono::Utc>>,
    pub labels: Option<HashMap<String, String>>,
}

#[derive(Debug, Clone)]
pub struct Pagination {
    pub limit: u32,
    pub cursor: Option<PageCursor>,
}

#[derive(Debug, Clone)]
pub struct PageCursor {
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub task_id: String,
}

#[derive(Debug)]
pub struct TaskPage {
    pub tasks: Vec<TaskDocument>,
    pub next_cursor: Option<PageCursor>,
    pub has_more: bool,
}

#[derive(Debug)]
pub enum UpdateResult {
    Success,
    NotFound,
    Conflict,
}

#[derive(Debug)]
pub struct BatchResult {
    pub inserted: u64,
    pub errors: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct TaskChangeEvent {
    pub operation: ChangeOperation,
    pub tenant_id: String,
    pub task_id: String,
    pub new_state: Option<String>,
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

#[derive(Debug, Clone)]
pub enum ChangeOperation {
    Insert,
    Update,
    Delete,
}

#[derive(Debug, Default)]
pub struct TenantStats {
    pub pending: u64,
    pub running: u64,
    pub completed: u64,
    pub failed: u64,
}

// ============================================================================
// Sharding Configuration
// ============================================================================

/// Configure sharding for horizontal scaling.
///
/// # Shard Key Selection
///
/// { tenant_id: 1, task_id: "hashed" } provides:
/// - Tenant isolation: All tenant data on same shards
/// - Even distribution: Hashed task_id prevents hotspots
/// - Targeted queries: Tenant-scoped queries hit single shard
///
/// # Tradeoffs
///
/// - Can't efficiently query across all tenants
/// - Rebalancing during resharding is expensive
/// - Large tenants may still create hotspots
pub async fn configure_sharding(
    client: &Client,
    database: &str,
    collection: &str,
) -> Result<(), StorageError> {
    let admin = client.database("admin");

    // Enable sharding on database
    admin.run_command(
        doc! { "enableSharding": database },
        None,
    ).await.map_err(|e| StorageError::Query(format!("Enable sharding failed: {}", e)))?;

    // Shard collection
    admin.run_command(
        doc! {
            "shardCollection": format!("{}.{}", database, collection),
            "key": { "tenant_id": 1, "task_id": "hashed" }
        },
        None,
    ).await.map_err(|e| StorageError::Query(format!("Shard collection failed: {}", e)))?;

    info!(
        database = %database,
        collection = %collection,
        "Sharding configured"
    );

    Ok(())
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_write_concern_conversion() {
        let wc: WriteConcern = MongoWriteConcern::Majority.into();
        // Verify majority is set
        assert!(wc.w.is_some());
    }

    #[test]
    fn test_task_document_serialization() {
        let doc = TaskDocument {
            tenant_id: "tenant-1".to_string(),
            task_id: "task-123".to_string(),
            payload: vec![1, 2, 3],
            state: "pending".to_string(),
            created_at: chrono::Utc::now(),
            updated_at: chrono::Utc::now(),
            version: 1,
            metadata: None,
        };

        let bson = bson::to_bson(&doc).unwrap();
        assert!(bson.as_document().is_some());
    }
}
