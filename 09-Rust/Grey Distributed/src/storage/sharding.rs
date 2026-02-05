//! # Sharding
//!
//! Data partitioning strategies for distributed storage.
//!
//! ## Design Philosophy
//!
//! Sharding divides data across nodes for:
//! - **Scalability**: Distribute load beyond single-node limits
//! - **Isolation**: Failures affect only specific shards
//! - **Locality**: Co-locate related data for efficiency
//!
//! ## Sharding Strategies
//!
//! 1. **Hash Sharding**: Uniform distribution via consistent hashing
//! 2. **Range Sharding**: Ordered data for range queries
//! 3. **Geographic Sharding**: Data locality for latency
//! 4. **Custom Sharding**: Application-specific partitioning
//!
//! ## Key Considerations
//!
//! - **Hotspots**: Avoid skewed key distributions
//! - **Resharding**: Online shard splitting/merging
//! - **Cross-shard**: Transactions spanning shards
//! - **Rebalancing**: Minimize data movement

use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

use parking_lot::RwLock;

use crate::types::NodeId;

// ============================================================================
// Core Types
// ============================================================================

/// Unique shard identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ShardId(pub u64);

/// Key used for sharding decisions
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ShardKey(pub Vec<u8>);

impl ShardKey {
    pub fn from_string(s: &str) -> Self {
        Self(s.as_bytes().to_vec())
    }
    
    pub fn from_bytes(b: &[u8]) -> Self {
        Self(b.to_vec())
    }
    
    /// Compute hash for consistent hashing
    pub fn hash_value(&self) -> u64 {
        use std::collections::hash_map::DefaultHasher;
        let mut hasher = DefaultHasher::new();
        self.0.hash(&mut hasher);
        hasher.finish()
    }
}

/// Shard metadata
#[derive(Debug, Clone)]
pub struct ShardInfo {
    pub id: ShardId,
    /// Nodes hosting this shard (primary + replicas)
    pub nodes: Vec<NodeId>,
    /// Key range for range-based sharding
    pub key_range: Option<KeyRange>,
    /// Shard status
    pub status: ShardStatus,
    /// Data size in bytes
    pub size_bytes: u64,
    /// Number of keys
    pub key_count: u64,
}

#[derive(Debug, Clone)]
pub struct KeyRange {
    pub start: Option<ShardKey>,
    pub end: Option<ShardKey>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ShardStatus {
    Active,
    Splitting,
    Merging,
    Migrating,
    Offline,
}

// ============================================================================
// Consistent Hashing
// ============================================================================

/// Consistent hash ring for shard placement
///
/// Consistent hashing minimizes key redistribution when
/// nodes are added or removed.
///
/// ## Algorithm
///
/// 1. Hash each node to multiple points on a ring
/// 2. Hash each key to a point on the ring
/// 3. Walk clockwise to find the owning node
///
/// ## Virtual Nodes
///
/// Each physical node maps to multiple virtual nodes:
/// - Improves load distribution
/// - Reduces variance with heterogeneous nodes
/// - Enables weighted placement
pub struct ConsistentHashRing {
    /// Ring points (hash -> shard)
    ring: RwLock<Vec<(u64, ShardId)>>,
    
    /// Virtual nodes per shard
    virtual_nodes: u32,
    
    /// Shard to nodes mapping
    shard_nodes: RwLock<HashMap<ShardId, Vec<NodeId>>>,
}

impl ConsistentHashRing {
    /// Create new hash ring
    ///
    /// `virtual_nodes` controls distribution quality:
    /// - Higher values = more uniform distribution
    /// - Typical values: 100-200 virtual nodes
    pub fn new(virtual_nodes: u32) -> Self {
        Self {
            ring: RwLock::new(Vec::new()),
            virtual_nodes,
            shard_nodes: RwLock::new(HashMap::new()),
        }
    }
    
    /// Add a shard to the ring
    pub fn add_shard(&self, shard_id: ShardId, nodes: Vec<NodeId>) {
        let mut ring = self.ring.write();
        
        for i in 0..self.virtual_nodes {
            let hash = self.hash_shard(shard_id, i);
            ring.push((hash, shard_id));
        }
        
        ring.sort_by_key(|(hash, _)| *hash);
        
        self.shard_nodes.write().insert(shard_id, nodes);
    }
    
    /// Remove a shard from the ring
    pub fn remove_shard(&self, shard_id: ShardId) {
        let mut ring = self.ring.write();
        ring.retain(|(_, id)| *id != shard_id);
        self.shard_nodes.write().remove(&shard_id);
    }
    
    /// Find the shard for a key
    pub fn locate(&self, key: &ShardKey) -> Option<ShardId> {
        let ring = self.ring.read();
        if ring.is_empty() {
            return None;
        }
        
        let hash = key.hash_value();
        
        // Binary search for the first hash >= key hash
        match ring.binary_search_by_key(&hash, |(h, _)| *h) {
            Ok(idx) => Some(ring[idx].1),
            Err(idx) => {
                // Wrap around to beginning if past the end
                let idx = if idx >= ring.len() { 0 } else { idx };
                Some(ring[idx].1)
            }
        }
    }
    
    /// Find N shards for replication (following ring clockwise)
    pub fn locate_n(&self, key: &ShardKey, n: usize) -> Vec<ShardId> {
        let ring = self.ring.read();
        if ring.is_empty() {
            return Vec::new();
        }
        
        let hash = key.hash_value();
        let start = match ring.binary_search_by_key(&hash, |(h, _)| *h) {
            Ok(idx) => idx,
            Err(idx) => idx % ring.len(),
        };
        
        let mut result = Vec::with_capacity(n);
        let mut seen = std::collections::HashSet::new();
        
        for i in 0..ring.len() {
            let idx = (start + i) % ring.len();
            let shard_id = ring[idx].1;
            
            if seen.insert(shard_id) {
                result.push(shard_id);
                if result.len() >= n {
                    break;
                }
            }
        }
        
        result
    }
    
    /// Get nodes for a shard
    pub fn get_nodes(&self, shard_id: ShardId) -> Vec<NodeId> {
        self.shard_nodes
            .read()
            .get(&shard_id)
            .cloned()
            .unwrap_or_default()
    }
    
    fn hash_shard(&self, shard_id: ShardId, virtual_idx: u32) -> u64 {
        use std::collections::hash_map::DefaultHasher;
        let mut hasher = DefaultHasher::new();
        shard_id.0.hash(&mut hasher);
        virtual_idx.hash(&mut hasher);
        hasher.finish()
    }
}

// ============================================================================
// Range-Based Sharding
// ============================================================================

/// Range-based shard manager
///
/// Shards data by key ranges for:
/// - Efficient range queries
/// - Natural data locality
/// - Ordered scans
///
/// ## Splitting
///
/// When a shard grows too large:
/// 1. Find median key
/// 2. Create new shard for upper half
/// 3. Move data atomically
/// 4. Update routing metadata
pub struct RangeSharder {
    /// Shard ranges (sorted by start key)
    ranges: RwLock<Vec<RangeEntry>>,
    
    /// Split threshold (bytes)
    split_threshold: u64,
    
    /// Merge threshold (bytes)
    merge_threshold: u64,
}

#[derive(Debug)]
struct RangeEntry {
    shard_id: ShardId,
    start: Option<ShardKey>,
    end: Option<ShardKey>,
    size_bytes: AtomicU64,
}

impl Clone for RangeEntry {
    fn clone(&self) -> Self {
        Self {
            shard_id: self.shard_id,
            start: self.start.clone(),
            end: self.end.clone(),
            size_bytes: AtomicU64::new(self.size_bytes.load(std::sync::atomic::Ordering::Relaxed)),
        }
    }
}

impl RangeEntry {
    fn contains(&self, key: &ShardKey) -> bool {
        let after_start = match &self.start {
            None => true,
            Some(start) => key.0 >= start.0,
        };
        let before_end = match &self.end {
            None => true,
            Some(end) => key.0 < end.0,
        };
        after_start && before_end
    }
}

impl RangeSharder {
    pub fn new(split_threshold: u64, merge_threshold: u64) -> Self {
        Self {
            ranges: RwLock::new(Vec::new()),
            split_threshold,
            merge_threshold,
        }
    }
    
    /// Add initial shard covering entire keyspace
    pub fn initialize(&self, shard_id: ShardId) {
        let mut ranges = self.ranges.write();
        ranges.clear();
        ranges.push(RangeEntry {
            shard_id,
            start: None,
            end: None,
            size_bytes: AtomicU64::new(0),
        });
    }
    
    /// Find shard for a key
    pub fn locate(&self, key: &ShardKey) -> Option<ShardId> {
        let ranges = self.ranges.read();
        ranges
            .iter()
            .find(|e| e.contains(key))
            .map(|e| e.shard_id)
    }
    
    /// Find shards for a key range
    pub fn locate_range(&self, start: &ShardKey, end: &ShardKey) -> Vec<ShardId> {
        let ranges = self.ranges.read();
        ranges
            .iter()
            .filter(|e| {
                // Shard overlaps with query range
                let shard_starts_before_end = match &e.start {
                    None => true,
                    Some(s) => s.0 < end.0,
                };
                let shard_ends_after_start = match &e.end {
                    None => true,
                    Some(e) => e.0 > start.0,
                };
                shard_starts_before_end && shard_ends_after_start
            })
            .map(|e| e.shard_id)
            .collect()
    }
    
    /// Record data size change
    pub fn update_size(&self, shard_id: ShardId, delta: i64) {
        let ranges = self.ranges.read();
        if let Some(entry) = ranges.iter().find(|e| e.shard_id == shard_id) {
            if delta >= 0 {
                entry.size_bytes.fetch_add(delta as u64, Ordering::Relaxed);
            } else {
                entry.size_bytes.fetch_sub((-delta) as u64, Ordering::Relaxed);
            }
        }
    }
    
    /// Check if any shard needs splitting
    pub fn check_splits(&self) -> Vec<ShardId> {
        let ranges = self.ranges.read();
        ranges
            .iter()
            .filter(|e| e.size_bytes.load(Ordering::Relaxed) > self.split_threshold)
            .map(|e| e.shard_id)
            .collect()
    }
    
    /// Check if any adjacent shards can be merged
    pub fn check_merges(&self) -> Vec<(ShardId, ShardId)> {
        let ranges = self.ranges.read();
        let mut candidates = Vec::new();
        
        for i in 0..ranges.len().saturating_sub(1) {
            let current = &ranges[i];
            let next = &ranges[i + 1];
            
            let combined_size = current.size_bytes.load(Ordering::Relaxed)
                + next.size_bytes.load(Ordering::Relaxed);
            
            if combined_size < self.merge_threshold {
                candidates.push((current.shard_id, next.shard_id));
            }
        }
        
        candidates
    }
    
    /// Execute a split
    pub fn split(&self, shard_id: ShardId, split_key: ShardKey, new_shard_id: ShardId) -> bool {
        let mut ranges = self.ranges.write();
        
        if let Some(idx) = ranges.iter().position(|e| e.shard_id == shard_id) {
            let old_entry = &ranges[idx];
            let old_size = old_entry.size_bytes.load(Ordering::Relaxed);
            
            // Create new entry for upper half
            let new_entry = RangeEntry {
                shard_id: new_shard_id,
                start: Some(split_key.clone()),
                end: old_entry.end.clone(),
                size_bytes: AtomicU64::new(old_size / 2),
            };
            
            // Update old entry
            let updated_entry = RangeEntry {
                shard_id: old_entry.shard_id,
                start: old_entry.start.clone(),
                end: Some(split_key),
                size_bytes: AtomicU64::new(old_size / 2),
            };
            
            ranges[idx] = updated_entry;
            ranges.insert(idx + 1, new_entry);
            
            true
        } else {
            false
        }
    }
}

// ============================================================================
// Shard Manager
// ============================================================================

/// Manages shard lifecycle and placement
pub struct ShardManager {
    /// Hash-based routing
    hash_ring: Arc<ConsistentHashRing>,
    
    /// Range-based routing (optional)
    range_sharder: Option<Arc<RangeSharder>>,
    
    /// Shard metadata
    shards: RwLock<HashMap<ShardId, ShardInfo>>,
    
    /// Next shard ID
    next_shard_id: AtomicU64,
    
    /// Sharding strategy
    strategy: ShardingStrategy,
}

#[derive(Debug, Clone, Copy)]
pub enum ShardingStrategy {
    /// Hash-based (consistent hashing)
    Hash,
    /// Range-based (for ordered access)
    Range,
    /// Hybrid (hash for distribution, range for locality)
    Hybrid,
}

impl ShardManager {
    pub fn new(strategy: ShardingStrategy) -> Self {
        let range_sharder = match strategy {
            ShardingStrategy::Range | ShardingStrategy::Hybrid => {
                Some(Arc::new(RangeSharder::new(
                    1024 * 1024 * 1024, // 1GB split threshold
                    256 * 1024 * 1024,  // 256MB merge threshold
                )))
            }
            ShardingStrategy::Hash => None,
        };
        
        Self {
            hash_ring: Arc::new(ConsistentHashRing::new(150)),
            range_sharder,
            shards: RwLock::new(HashMap::new()),
            next_shard_id: AtomicU64::new(1),
            strategy,
        }
    }
    
    /// Create a new shard
    pub fn create_shard(&self, nodes: Vec<NodeId>) -> ShardId {
        let id = ShardId(self.next_shard_id.fetch_add(1, Ordering::SeqCst));
        
        let info = ShardInfo {
            id,
            nodes: nodes.clone(),
            key_range: None,
            status: ShardStatus::Active,
            size_bytes: 0,
            key_count: 0,
        };
        
        self.shards.write().insert(id, info);
        self.hash_ring.add_shard(id, nodes);
        
        id
    }
    
    /// Route a key to its shard
    pub fn route(&self, key: &ShardKey) -> Option<ShardId> {
        match self.strategy {
            ShardingStrategy::Hash => self.hash_ring.locate(key),
            ShardingStrategy::Range => {
                self.range_sharder.as_ref()?.locate(key)
            }
            ShardingStrategy::Hybrid => {
                // Try range first, fall back to hash
                self.range_sharder
                    .as_ref()
                    .and_then(|rs| rs.locate(key))
                    .or_else(|| self.hash_ring.locate(key))
            }
        }
    }
    
    /// Get shard info
    pub fn get_shard(&self, id: ShardId) -> Option<ShardInfo> {
        self.shards.read().get(&id).cloned()
    }
    
    /// List all shards
    pub fn list_shards(&self) -> Vec<ShardInfo> {
        self.shards.read().values().cloned().collect()
    }
    
    /// Get nodes hosting a shard
    pub fn get_nodes(&self, id: ShardId) -> Vec<NodeId> {
        self.shards
            .read()
            .get(&id)
            .map(|s| s.nodes.clone())
            .unwrap_or_default()
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_consistent_hash_ring() {
        let ring = ConsistentHashRing::new(100);
        
        ring.add_shard(ShardId(1), vec![NodeId(1)]);
        ring.add_shard(ShardId(2), vec![NodeId(2)]);
        ring.add_shard(ShardId(3), vec![NodeId(3)]);
        
        // Should consistently route same keys
        let key = ShardKey::from_string("test-key");
        let shard1 = ring.locate(&key);
        let shard2 = ring.locate(&key);
        assert_eq!(shard1, shard2);
    }
    
    #[test]
    fn test_range_sharder() {
        let sharder = RangeSharder::new(1000, 100);
        sharder.initialize(ShardId(1));
        
        let key = ShardKey::from_string("test-key");
        assert_eq!(sharder.locate(&key), Some(ShardId(1)));
    }
    
    #[test]
    fn test_shard_manager() {
        let manager = ShardManager::new(ShardingStrategy::Hash);
        
        let shard1 = manager.create_shard(vec![NodeId(1), NodeId(2)]);
        let shard2 = manager.create_shard(vec![NodeId(2), NodeId(3)]);
        
        assert_ne!(shard1, shard2);
        
        let key = ShardKey::from_string("some-key");
        let routed = manager.route(&key);
        assert!(routed.is_some());
    }
}
