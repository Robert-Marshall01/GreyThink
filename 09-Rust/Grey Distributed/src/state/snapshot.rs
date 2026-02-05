//! # Snapshotting System
//!
//! Efficiently captures and restores state machine state.
//!
//! ## Why Snapshots Matter
//!
//! 1. **Faster Recovery**: Instead of replaying millions of events, load
//!    the latest snapshot and replay only recent events.
//!
//! 2. **Log Compaction**: Once a snapshot exists, older events can be
//!    garbage collected.
//!
//! 3. **Follower Catch-up**: New or lagging nodes can receive a snapshot
//!    instead of replaying the entire history.
//!
//! ## Snapshot Strategies
//!
//! - **Time-based**: Snapshot every N minutes
//! - **Event-based**: Snapshot every N events
//! - **Size-based**: Snapshot when state size exceeds threshold
//! - **Hybrid**: Combine strategies for optimal behavior

use std::collections::BTreeMap;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

use parking_lot::{Mutex, RwLock};

// ============================================================================
// Snapshot Types
// ============================================================================

/// A point-in-time capture of state
#[derive(Debug, Clone)]
pub struct Snapshot {
    /// State version at snapshot time
    pub version: u64,
    /// Serialized state data
    pub data: Vec<u8>,
    /// Creation timestamp (microseconds since epoch)
    pub created_at: u64,
    /// Aggregate ID this snapshot belongs to
    pub aggregate_id: String,
}

/// Snapshot metadata (without the data)
#[derive(Debug, Clone)]
pub struct SnapshotMeta {
    /// State version
    pub version: u64,
    /// Data size in bytes
    pub size: u64,
    /// Creation timestamp
    pub created_at: u64,
    /// Aggregate ID
    pub aggregate_id: String,
    /// Storage location
    pub location: SnapshotLocation,
}

/// Where a snapshot is stored
#[derive(Debug, Clone)]
pub enum SnapshotLocation {
    /// In-memory
    Memory,
    /// Local filesystem path
    File(PathBuf),
    /// Remote object storage (bucket, key)
    Remote { bucket: String, key: String },
}

// ============================================================================
// Snapshot Policy
// ============================================================================

/// Configuration for when to take snapshots
#[derive(Debug, Clone)]
pub struct SnapshotPolicy {
    /// Interval in events between snapshots
    pub event_interval: Option<u64>,
    /// Time interval between snapshots (microseconds)
    pub time_interval: Option<u64>,
    /// Maximum state size before forced snapshot
    pub size_threshold: Option<u64>,
    /// Maximum number of snapshots to retain
    pub retention_count: u64,
    /// Compression enabled
    pub compression: bool,
}

impl Default for SnapshotPolicy {
    fn default() -> Self {
        Self {
            event_interval: Some(10_000),
            time_interval: Some(300_000_000), // 5 minutes
            size_threshold: Some(100 * 1024 * 1024), // 100MB
            retention_count: 5,
            compression: true,
        }
    }
}

impl SnapshotPolicy {
    /// Check if a snapshot should be taken
    pub fn should_snapshot(
        &self,
        events_since_last: u64,
        time_since_last: u64,
        current_size: u64,
    ) -> bool {
        if let Some(interval) = self.event_interval {
            if events_since_last >= interval {
                return true;
            }
        }

        if let Some(interval) = self.time_interval {
            if time_since_last >= interval {
                return true;
            }
        }

        if let Some(threshold) = self.size_threshold {
            if current_size >= threshold {
                return true;
            }
        }

        false
    }
}

// ============================================================================
// Snapshot Storage
// ============================================================================

/// Trait for snapshot persistence
pub trait SnapshotStorage: Send + Sync {
    /// Save a snapshot
    fn save(&self, snapshot: &Snapshot) -> Result<SnapshotMeta, super::StateError>;

    /// Load a snapshot by version
    fn load(&self, aggregate_id: &str, version: u64) -> Result<Snapshot, super::StateError>;

    /// Load the latest snapshot
    fn load_latest(&self, aggregate_id: &str) -> Result<Option<Snapshot>, super::StateError>;

    /// List available snapshots
    fn list(&self, aggregate_id: &str) -> Result<Vec<SnapshotMeta>, super::StateError>;

    /// Delete old snapshots beyond retention count
    fn cleanup(&self, aggregate_id: &str, keep_count: usize) -> Result<u64, super::StateError>;
}

/// In-memory snapshot storage (for testing/small state)
pub struct MemorySnapshotStorage {
    /// Snapshots organized by aggregate_id -> version -> snapshot
    snapshots: RwLock<BTreeMap<String, BTreeMap<u64, Snapshot>>>,
}

impl MemorySnapshotStorage {
    pub fn new() -> Self {
        Self {
            snapshots: RwLock::new(BTreeMap::new()),
        }
    }
}

impl SnapshotStorage for MemorySnapshotStorage {
    fn save(&self, snapshot: &Snapshot) -> Result<SnapshotMeta, super::StateError> {
        let meta = SnapshotMeta {
            version: snapshot.version,
            size: snapshot.data.len() as u64,
            created_at: snapshot.created_at,
            aggregate_id: snapshot.aggregate_id.clone(),
            location: SnapshotLocation::Memory,
        };

        self.snapshots
            .write()
            .entry(snapshot.aggregate_id.clone())
            .or_insert_with(BTreeMap::new)
            .insert(snapshot.version, snapshot.clone());

        Ok(meta)
    }

    fn load(&self, aggregate_id: &str, version: u64) -> Result<Snapshot, super::StateError> {
        self.snapshots
            .read()
            .get(aggregate_id)
            .and_then(|snapshots| snapshots.get(&version))
            .cloned()
            .ok_or(super::StateError::SnapshotNotFound(version))
    }

    fn load_latest(&self, aggregate_id: &str) -> Result<Option<Snapshot>, super::StateError> {
        Ok(self
            .snapshots
            .read()
            .get(aggregate_id)
            .and_then(|snapshots| snapshots.values().last())
            .cloned())
    }

    fn list(&self, aggregate_id: &str) -> Result<Vec<SnapshotMeta>, super::StateError> {
        Ok(self
            .snapshots
            .read()
            .get(aggregate_id)
            .map(|snapshots| {
                snapshots
                    .values()
                    .map(|s| SnapshotMeta {
                        version: s.version,
                        size: s.data.len() as u64,
                        created_at: s.created_at,
                        aggregate_id: s.aggregate_id.clone(),
                        location: SnapshotLocation::Memory,
                    })
                    .collect()
            })
            .unwrap_or_default())
    }

    fn cleanup(&self, aggregate_id: &str, keep_count: usize) -> Result<u64, super::StateError> {
        let mut snapshots = self.snapshots.write();
        let mut deleted = 0u64;

        if let Some(agg_snapshots) = snapshots.get_mut(aggregate_id) {
            while agg_snapshots.len() > keep_count {
                if let Some(oldest) = agg_snapshots.keys().next().cloned() {
                    agg_snapshots.remove(&oldest);
                    deleted += 1;
                }
            }
        }

        Ok(deleted)
    }
}

// ============================================================================
// Snapshot Manager
// ============================================================================

/// Coordinates snapshot creation and recovery
pub struct SnapshotManager<S: SnapshotStorage> {
    /// Storage backend
    storage: S,
    /// Snapshot policy
    policy: SnapshotPolicy,
    /// Events since last snapshot per aggregate
    events_since: RwLock<BTreeMap<String, u64>>,
    /// Time of last snapshot per aggregate
    last_snapshot_time: RwLock<BTreeMap<String, u64>>,
}

impl<S: SnapshotStorage> SnapshotManager<S> {
    pub fn new(storage: S, policy: SnapshotPolicy) -> Self {
        Self {
            storage,
            policy,
            events_since: RwLock::new(BTreeMap::new()),
            last_snapshot_time: RwLock::new(BTreeMap::new()),
        }
    }

    /// Check if we should take a snapshot
    pub fn should_snapshot(&self, aggregate_id: &str, current_size: u64) -> bool {
        let events_since = self
            .events_since
            .read()
            .get(aggregate_id)
            .copied()
            .unwrap_or(0);

        let last_time = self
            .last_snapshot_time
            .read()
            .get(aggregate_id)
            .copied()
            .unwrap_or(0);

        let now = Self::now();
        let time_since = now.saturating_sub(last_time);

        self.policy.should_snapshot(events_since, time_since, current_size)
    }

    /// Record that an event was applied
    pub fn record_event(&self, aggregate_id: &str) {
        *self
            .events_since
            .write()
            .entry(aggregate_id.to_string())
            .or_insert(0) += 1;
    }

    /// Take a snapshot
    pub fn take_snapshot(&self, snapshot: Snapshot) -> Result<SnapshotMeta, super::StateError> {
        let aggregate_id = snapshot.aggregate_id.clone();

        // Optionally compress
        let snapshot = if self.policy.compression {
            self.compress(snapshot)?
        } else {
            snapshot
        };

        // Save
        let meta = self.storage.save(&snapshot)?;

        // Reset counters
        self.events_since.write().insert(aggregate_id.clone(), 0);
        self.last_snapshot_time.write().insert(aggregate_id.clone(), Self::now());

        // Cleanup old snapshots
        self.storage.cleanup(&aggregate_id, self.policy.retention_count as usize)?;

        Ok(meta)
    }

    /// Load latest snapshot for recovery
    pub fn load_latest(&self, aggregate_id: &str) -> Result<Option<Snapshot>, super::StateError> {
        let snapshot = self.storage.load_latest(aggregate_id)?;

        // Decompress if needed
        if let Some(s) = snapshot {
            if self.policy.compression {
                return Ok(Some(self.decompress(s)?));
            } else {
                return Ok(Some(s));
            }
        }

        Ok(None)
    }

    /// List all snapshots
    pub fn list(&self, aggregate_id: &str) -> Result<Vec<SnapshotMeta>, super::StateError> {
        self.storage.list(aggregate_id)
    }

    fn compress(&self, snapshot: Snapshot) -> Result<Snapshot, super::StateError> {
        // TODO: Implement real compression (e.g., zstd, lz4)
        Ok(snapshot)
    }

    fn decompress(&self, snapshot: Snapshot) -> Result<Snapshot, super::StateError> {
        // TODO: Implement real decompression
        Ok(snapshot)
    }

    fn now() -> u64 {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_micros() as u64
    }
}

// ============================================================================
// Incremental Snapshots
// ============================================================================

/// Delta between two snapshots
///
/// For large state, sending full snapshots is expensive.
/// Incremental snapshots send only the changes.
#[derive(Debug, Clone)]
pub struct SnapshotDelta {
    /// Base snapshot version
    pub base_version: u64,
    /// Target snapshot version
    pub target_version: u64,
    /// Changed data
    pub changes: Vec<DeltaChange>,
}

/// A single change in a delta
#[derive(Debug, Clone)]
pub enum DeltaChange {
    /// Key was inserted or updated
    Upsert { key: Vec<u8>, value: Vec<u8> },
    /// Key was deleted
    Delete { key: Vec<u8> },
}

/// Builder for incremental snapshots
pub struct IncrementalSnapshotBuilder {
    changes: Vec<DeltaChange>,
    base_version: u64,
}

impl IncrementalSnapshotBuilder {
    pub fn new(base_version: u64) -> Self {
        Self {
            changes: Vec::new(),
            base_version,
        }
    }

    pub fn upsert(&mut self, key: Vec<u8>, value: Vec<u8>) {
        self.changes.push(DeltaChange::Upsert { key, value });
    }

    pub fn delete(&mut self, key: Vec<u8>) {
        self.changes.push(DeltaChange::Delete { key });
    }

    pub fn build(self, target_version: u64) -> SnapshotDelta {
        SnapshotDelta {
            base_version: self.base_version,
            target_version,
            changes: self.changes,
        }
    }
}

// ============================================================================
// Streaming Snapshots
// ============================================================================

/// For very large state, stream snapshots in chunks
pub struct SnapshotStream {
    /// Snapshot version being streamed
    pub version: u64,
    /// Total size in bytes
    pub total_size: u64,
    /// Chunk size for streaming
    pub chunk_size: usize,
    /// Number of chunks
    pub total_chunks: usize,
}

/// A chunk of a streaming snapshot
#[derive(Debug, Clone)]
pub struct SnapshotChunk {
    /// Chunk index (0-based)
    pub index: usize,
    /// Chunk data
    pub data: Vec<u8>,
    /// Is this the last chunk?
    pub is_last: bool,
    /// Checksum of this chunk
    pub checksum: u64,
}

impl SnapshotStream {
    pub fn new(version: u64, data: &[u8], chunk_size: usize) -> Self {
        let total_size = data.len() as u64;
        let total_chunks = (data.len() + chunk_size - 1) / chunk_size;

        Self {
            version,
            total_size,
            chunk_size,
            total_chunks,
        }
    }

    /// Create chunks from data
    pub fn chunk(data: Vec<u8>, chunk_size: usize) -> Vec<SnapshotChunk> {
        let total = data.len();
        let mut chunks = Vec::new();
        let mut offset = 0;
        let mut index = 0;

        while offset < total {
            let end = (offset + chunk_size).min(total);
            let chunk_data = data[offset..end].to_vec();
            let is_last = end >= total;

            let checksum = Self::compute_checksum(&chunk_data);

            chunks.push(SnapshotChunk {
                index,
                data: chunk_data,
                is_last,
                checksum,
            });

            offset = end;
            index += 1;
        }

        chunks
    }

    /// Reassemble chunks into data
    pub fn reassemble(mut chunks: Vec<SnapshotChunk>) -> Result<Vec<u8>, super::StateError> {
        // Sort by index
        chunks.sort_by_key(|c| c.index);

        // Verify completeness
        for (i, chunk) in chunks.iter().enumerate() {
            if chunk.index != i {
                return Err(super::StateError::SnapshotNotFound(0));
            }
            // Verify checksum
            let computed = Self::compute_checksum(&chunk.data);
            if computed != chunk.checksum {
                return Err(super::StateError::CheckpointFailed("checksum mismatch".into()));
            }
        }

        // Concatenate
        let mut data = Vec::new();
        for chunk in chunks {
            data.extend(chunk.data);
        }

        Ok(data)
    }

    fn compute_checksum(data: &[u8]) -> u64 {
        // Simple checksum - use CRC32 or xxHash in production
        let mut hash = 0u64;
        for &byte in data {
            hash = hash.wrapping_mul(31).wrapping_add(byte as u64);
        }
        hash
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_memory_storage() {
        let storage = MemorySnapshotStorage::new();

        let snapshot = Snapshot {
            version: 1,
            data: b"test data".to_vec(),
            created_at: 12345,
            aggregate_id: "test".to_string(),
        };

        storage.save(&snapshot).unwrap();

        let loaded = storage.load("test", 1).unwrap();
        assert_eq!(loaded.data, b"test data");
    }

    #[test]
    fn test_snapshot_policy() {
        let policy = SnapshotPolicy {
            event_interval: Some(100),
            time_interval: None,
            size_threshold: None,
            retention_count: 5,
            compression: false,
        };

        assert!(!policy.should_snapshot(50, 0, 0));
        assert!(policy.should_snapshot(100, 0, 0));
    }

    #[test]
    fn test_snapshot_streaming() {
        let data = vec![1u8; 1000];
        let chunks = SnapshotStream::chunk(data.clone(), 100);

        assert_eq!(chunks.len(), 10);
        assert!(chunks.last().unwrap().is_last);

        let reassembled = SnapshotStream::reassemble(chunks).unwrap();
        assert_eq!(reassembled, data);
    }

    #[test]
    fn test_incremental_snapshot() {
        let mut builder = IncrementalSnapshotBuilder::new(1);
        builder.upsert(b"key1".to_vec(), b"value1".to_vec());
        builder.delete(b"key2".to_vec());

        let delta = builder.build(2);
        assert_eq!(delta.changes.len(), 2);
    }
}
