//! Grey Distributed — Minimal Storage Implementation
//!
//! Sharded key-value storage with quorum reads/writes.
//! Demonstrates partitioning, replication, and consistency guarantees.

use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::{Mutex, RwLock};

// ============================================================================
// Configuration
// ============================================================================

/// Storage configuration.
#[derive(Clone, Debug)]
pub struct StorageConfig {
    /// Number of shards.
    pub shard_count: usize,
    /// Replication factor per shard.
    pub replication_factor: usize,
    /// Quorum size for reads.
    pub read_quorum: usize,
    /// Quorum size for writes.
    pub write_quorum: usize,
    /// Operation timeout.
    pub timeout: Duration,
}

impl Default for StorageConfig {
    fn default() -> Self {
        Self {
            shard_count: 16,
            replication_factor: 3,
            read_quorum: 2,
            write_quorum: 2,
            timeout: Duration::from_secs(5),
        }
    }
}

// ============================================================================
// Data Types
// ============================================================================

/// Versioned value with vector clock.
#[derive(Clone, Debug)]
pub struct VersionedValue {
    pub data: Vec<u8>,
    pub version: u64,
    pub timestamp: Instant,
    pub node_id: u64,
}

impl VersionedValue {
    pub fn new(data: Vec<u8>, version: u64, node_id: u64) -> Self {
        Self {
            data,
            version,
            timestamp: Instant::now(),
            node_id,
        }
    }
}

/// Quorum level for operations.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum QuorumLevel {
    One,
    Quorum,
    All,
}

/// Storage operation result.
#[derive(Debug)]
pub enum StorageResult<T> {
    Ok(T),
    QuorumNotMet { achieved: usize, required: usize },
    NotFound,
    Timeout,
    Conflict { versions: Vec<VersionedValue> },
}

// ============================================================================
// Shard
// ============================================================================

/// Single shard storing a partition of keys.
pub struct Shard {
    id: usize,
    data: RwLock<HashMap<String, VersionedValue>>,
    version_counter: Mutex<u64>,
}

impl Shard {
    pub fn new(id: usize) -> Self {
        Self {
            id,
            data: RwLock::new(HashMap::new()),
            version_counter: Mutex::new(0),
        }
    }

    /// Get a value from this shard.
    pub async fn get(&self, key: &str) -> Option<VersionedValue> {
        let data = self.data.read().await;
        data.get(key).cloned()
    }

    /// Put a value into this shard.
    pub async fn put(&self, key: String, value: Vec<u8>, node_id: u64) -> VersionedValue {
        let mut counter = self.version_counter.lock().await;
        *counter += 1;
        let version = *counter;
        drop(counter);

        let versioned = VersionedValue::new(value, version, node_id);
        let mut data = self.data.write().await;
        data.insert(key, versioned.clone());
        versioned
    }

    /// Conditional put (compare-and-swap).
    pub async fn cas(
        &self,
        key: String,
        expected_version: u64,
        value: Vec<u8>,
        node_id: u64,
    ) -> Result<VersionedValue, Option<VersionedValue>> {
        let mut data = self.data.write().await;

        let current = data.get(&key);
        let current_version = current.map(|v| v.version).unwrap_or(0);

        if current_version != expected_version {
            return Err(current.cloned());
        }

        let mut counter = self.version_counter.lock().await;
        *counter += 1;
        let version = *counter;
        drop(counter);

        let versioned = VersionedValue::new(value, version, node_id);
        data.insert(key, versioned.clone());
        Ok(versioned)
    }

    /// Delete a key from this shard.
    pub async fn delete(&self, key: &str) -> Option<VersionedValue> {
        let mut data = self.data.write().await;
        data.remove(key)
    }

    /// List all keys in this shard.
    pub async fn keys(&self) -> Vec<String> {
        let data = self.data.read().await;
        data.keys().cloned().collect()
    }

    /// Get shard size.
    pub async fn len(&self) -> usize {
        let data = self.data.read().await;
        data.len()
    }
}

// ============================================================================
// Replica
// ============================================================================

/// Replica of a shard, possibly on a different node.
pub struct Replica {
    pub node_id: u64,
    pub shard: Arc<Shard>,
    pub is_primary: bool,
}

impl Replica {
    pub fn new(node_id: u64, shard: Arc<Shard>, is_primary: bool) -> Self {
        Self {
            node_id,
            shard,
            is_primary,
        }
    }
}

// ============================================================================
// Sharded Storage
// ============================================================================

/// Sharded storage with quorum operations.
pub struct ShardedStorage {
    config: StorageConfig,
    node_id: u64,
    shards: Vec<Arc<Shard>>,
    /// Simulated replicas (in real impl, these would be remote).
    replicas: RwLock<HashMap<usize, Vec<Arc<Shard>>>>,
}

impl ShardedStorage {
    /// Create new sharded storage.
    pub fn new(config: StorageConfig, node_id: u64) -> Self {
        let shards: Vec<Arc<Shard>> = (0..config.shard_count)
            .map(|id| Arc::new(Shard::new(id)))
            .collect();

        Self {
            config,
            node_id,
            shards,
            replicas: RwLock::new(HashMap::new()),
        }
    }

    /// Initialize replicas for fault tolerance.
    pub async fn initialize_replicas(&self) {
        let mut replicas = self.replicas.write().await;

        for shard_id in 0..self.config.shard_count {
            let mut shard_replicas = Vec::new();

            // Create simulated replicas (in production, connect to remote nodes).
            for r in 0..self.config.replication_factor {
                let replica_shard = Arc::new(Shard::new(shard_id * 100 + r));
                shard_replicas.push(replica_shard);
            }

            replicas.insert(shard_id, shard_replicas);
        }
    }

    /// Compute shard for a key.
    fn shard_for_key(&self, key: &str) -> usize {
        // Simple hash-based sharding.
        let mut hash: u64 = 0;
        for byte in key.bytes() {
            hash = hash.wrapping_mul(31).wrapping_add(byte as u64);
        }
        (hash as usize) % self.config.shard_count
    }

    /// Get with quorum read.
    pub async fn get(&self, key: &str, quorum: QuorumLevel) -> StorageResult<VersionedValue> {
        let shard_id = self.shard_for_key(key);
        let primary = &self.shards[shard_id];

        match quorum {
            QuorumLevel::One => {
                // Single read from primary.
                match primary.get(key).await {
                    Some(v) => StorageResult::Ok(v),
                    None => StorageResult::NotFound,
                }
            }
            QuorumLevel::Quorum | QuorumLevel::All => {
                // Read from multiple replicas.
                let replicas = self.replicas.read().await;
                let shard_replicas = replicas.get(&shard_id);

                let mut values = Vec::new();

                // Read from primary.
                if let Some(v) = primary.get(key).await {
                    values.push(v);
                }

                // Read from replicas.
                if let Some(reps) = shard_replicas {
                    let required = match quorum {
                        QuorumLevel::Quorum => self.config.read_quorum,
                        QuorumLevel::All => self.config.replication_factor,
                        _ => 1,
                    };

                    for rep in reps.iter().take(required - 1) {
                        if let Some(v) = rep.get(key).await {
                            values.push(v);
                        }
                    }
                }

                if values.is_empty() {
                    return StorageResult::NotFound;
                }

                let required = match quorum {
                    QuorumLevel::Quorum => self.config.read_quorum,
                    QuorumLevel::All => self.config.replication_factor,
                    _ => 1,
                };

                if values.len() < required {
                    return StorageResult::QuorumNotMet {
                        achieved: values.len(),
                        required,
                    };
                }

                // Return highest version (read repair would fix inconsistencies).
                let best = values.into_iter().max_by_key(|v| v.version).unwrap();
                StorageResult::Ok(best)
            }
        }
    }

    /// Put with quorum write.
    pub async fn put(
        &self,
        key: String,
        value: Vec<u8>,
        quorum: QuorumLevel,
    ) -> StorageResult<VersionedValue> {
        let shard_id = self.shard_for_key(&key);
        let primary = &self.shards[shard_id];

        // Write to primary.
        let versioned = primary.put(key.clone(), value.clone(), self.node_id).await;

        match quorum {
            QuorumLevel::One => StorageResult::Ok(versioned),
            QuorumLevel::Quorum | QuorumLevel::All => {
                let replicas = self.replicas.read().await;
                let shard_replicas = replicas.get(&shard_id);

                let mut acks = 1; // Primary counts as 1.

                if let Some(reps) = shard_replicas {
                    let required = match quorum {
                        QuorumLevel::Quorum => self.config.write_quorum,
                        QuorumLevel::All => self.config.replication_factor,
                        _ => 1,
                    };

                    for rep in reps.iter().take(required - 1) {
                        rep.put(key.clone(), value.clone(), self.node_id).await;
                        acks += 1;
                    }
                }

                let required = match quorum {
                    QuorumLevel::Quorum => self.config.write_quorum,
                    QuorumLevel::All => self.config.replication_factor,
                    _ => 1,
                };

                if acks >= required {
                    StorageResult::Ok(versioned)
                } else {
                    StorageResult::QuorumNotMet {
                        achieved: acks,
                        required,
                    }
                }
            }
        }
    }

    /// Delete with quorum.
    pub async fn delete(&self, key: &str, quorum: QuorumLevel) -> StorageResult<()> {
        let shard_id = self.shard_for_key(key);
        let primary = &self.shards[shard_id];

        primary.delete(key).await;

        match quorum {
            QuorumLevel::One => StorageResult::Ok(()),
            QuorumLevel::Quorum | QuorumLevel::All => {
                let replicas = self.replicas.read().await;
                let shard_replicas = replicas.get(&shard_id);

                let mut acks = 1;

                if let Some(reps) = shard_replicas {
                    for rep in reps {
                        rep.delete(key).await;
                        acks += 1;
                    }
                }

                let required = match quorum {
                    QuorumLevel::Quorum => self.config.write_quorum,
                    QuorumLevel::All => self.config.replication_factor,
                    _ => 1,
                };

                if acks >= required {
                    StorageResult::Ok(())
                } else {
                    StorageResult::QuorumNotMet {
                        achieved: acks,
                        required,
                    }
                }
            }
        }
    }

    /// Compare-and-swap with quorum.
    pub async fn cas(
        &self,
        key: String,
        expected_version: u64,
        value: Vec<u8>,
    ) -> StorageResult<VersionedValue> {
        let shard_id = self.shard_for_key(&key);
        let primary = &self.shards[shard_id];

        match primary
            .cas(key.clone(), expected_version, value.clone(), self.node_id)
            .await
        {
            Ok(versioned) => {
                // Replicate to other shards.
                let replicas = self.replicas.read().await;
                if let Some(reps) = replicas.get(&shard_id) {
                    for rep in reps {
                        // Force write (CAS succeeded on primary).
                        rep.put(key.clone(), value.clone(), self.node_id).await;
                    }
                }
                StorageResult::Ok(versioned)
            }
            Err(current) => match current {
                Some(v) => StorageResult::Conflict {
                    versions: vec![v],
                },
                None => StorageResult::NotFound,
            },
        }
    }

    /// Get storage statistics.
    pub async fn stats(&self) -> StorageStats {
        let mut total_keys = 0;
        let mut shard_sizes = Vec::new();

        for shard in &self.shards {
            let size = shard.len().await;
            total_keys += size;
            shard_sizes.push(size);
        }

        StorageStats {
            shard_count: self.config.shard_count,
            replication_factor: self.config.replication_factor,
            total_keys,
            shard_sizes,
        }
    }
}

/// Storage statistics.
#[derive(Debug)]
pub struct StorageStats {
    pub shard_count: usize,
    pub replication_factor: usize,
    pub total_keys: usize,
    pub shard_sizes: Vec<usize>,
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_basic_put_get() {
        let config = StorageConfig::default();
        let storage = ShardedStorage::new(config, 1);

        // Put a value.
        let result = storage
            .put("key1".to_string(), b"value1".to_vec(), QuorumLevel::One)
            .await;
        assert!(matches!(result, StorageResult::Ok(_)));

        // Get the value.
        let result = storage.get("key1", QuorumLevel::One).await;
        match result {
            StorageResult::Ok(v) => assert_eq!(v.data, b"value1"),
            _ => panic!("Expected Ok"),
        }
    }

    #[tokio::test]
    async fn test_not_found() {
        let config = StorageConfig::default();
        let storage = ShardedStorage::new(config, 1);

        let result = storage.get("nonexistent", QuorumLevel::One).await;
        assert!(matches!(result, StorageResult::NotFound));
    }

    #[tokio::test]
    async fn test_delete() {
        let config = StorageConfig::default();
        let storage = ShardedStorage::new(config, 1);

        storage
            .put("key1".to_string(), b"value1".to_vec(), QuorumLevel::One)
            .await;
        storage.delete("key1", QuorumLevel::One).await;

        let result = storage.get("key1", QuorumLevel::One).await;
        assert!(matches!(result, StorageResult::NotFound));
    }

    #[tokio::test]
    async fn test_cas_success() {
        let config = StorageConfig::default();
        let storage = ShardedStorage::new(config, 1);

        // Put initial value.
        let result = storage
            .put("key1".to_string(), b"v1".to_vec(), QuorumLevel::One)
            .await;
        let version = match result {
            StorageResult::Ok(v) => v.version,
            _ => panic!("Expected Ok"),
        };

        // CAS with correct version.
        let result = storage
            .cas("key1".to_string(), version, b"v2".to_vec())
            .await;
        assert!(matches!(result, StorageResult::Ok(_)));
    }

    #[tokio::test]
    async fn test_cas_conflict() {
        let config = StorageConfig::default();
        let storage = ShardedStorage::new(config, 1);

        storage
            .put("key1".to_string(), b"v1".to_vec(), QuorumLevel::One)
            .await;

        // CAS with wrong version.
        let result = storage.cas("key1".to_string(), 999, b"v2".to_vec()).await;
        assert!(matches!(result, StorageResult::Conflict { .. }));
    }

    #[tokio::test]
    async fn test_sharding_distribution() {
        let config = StorageConfig {
            shard_count: 4,
            ..Default::default()
        };
        let storage = ShardedStorage::new(config, 1);

        // Insert many keys.
        for i in 0..100 {
            storage
                .put(format!("key{}", i), format!("value{}", i).into_bytes(), QuorumLevel::One)
                .await;
        }

        let stats = storage.stats().await;
        assert_eq!(stats.total_keys, 100);

        // Check distribution (should be roughly even).
        for size in &stats.shard_sizes {
            assert!(*size > 10, "Shard too empty: {}", size);
            assert!(*size < 50, "Shard too full: {}", size);
        }
    }

    #[tokio::test]
    async fn test_quorum_writes() {
        let config = StorageConfig {
            shard_count: 4,
            replication_factor: 3,
            write_quorum: 2,
            read_quorum: 2,
            ..Default::default()
        };
        let storage = ShardedStorage::new(config, 1);
        storage.initialize_replicas().await;

        // Write with quorum.
        let result = storage
            .put("key1".to_string(), b"value1".to_vec(), QuorumLevel::Quorum)
            .await;
        assert!(matches!(result, StorageResult::Ok(_)));

        // Read with quorum.
        let result = storage.get("key1", QuorumLevel::Quorum).await;
        assert!(matches!(result, StorageResult::Ok(_)));
    }
}
