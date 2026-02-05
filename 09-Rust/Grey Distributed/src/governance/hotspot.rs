//! # Hotspot Detection and Mitigation
//!
//! Identifies and handles resource hotspots in distributed systems.
//!
//! ## Design Principles
//!
//! Hotspots are localized overload conditions:
//! - **Detection**: Identify hotspots before they cause failures
//! - **Mitigation**: Spread load or shed traffic intelligently
//! - **Prevention**: Design patterns to avoid hotspots
//! - **Observability**: Track hotspot metrics and patterns
//!
//! ## Hotspot Types
//!
//! 1. **Key Hotspots**: Single keys with high access frequency
//! 2. **Range Hotspots**: Data ranges with skewed access
//! 3. **Node Hotspots**: Individual nodes receiving excess load
//! 4. **Temporal Hotspots**: Time-based access spikes

use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use parking_lot::{Mutex, RwLock};

use super::{GovernanceError, TenantId};
use crate::types::NodeId;

// ============================================================================
// Hotspot Types
// ============================================================================

/// Unique resource identifier
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResourceKey {
    /// Resource type (e.g., "partition", "key", "endpoint")
    pub resource_type: String,
    /// Resource identifier
    pub id: String,
}

/// Detected hotspot
#[derive(Debug, Clone)]
pub struct Hotspot {
    /// Resource that is hot
    pub resource: ResourceKey,
    
    /// Hotspot type
    pub hotspot_type: HotspotType,
    
    /// Current load (requests per second)
    pub current_load: f64,
    
    /// Threshold that was exceeded
    pub threshold: f64,
    
    /// Severity level
    pub severity: HotspotSeverity,
    
    /// When hotspot was first detected
    pub detected_at: Instant,
    
    /// Affected tenants
    pub affected_tenants: Vec<TenantId>,
    
    /// Suggested mitigation
    pub suggested_action: MitigationAction,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HotspotType {
    /// Single key receiving excessive traffic
    KeyHotspot,
    /// Data range receiving skewed access
    RangeHotspot,
    /// Node receiving excess traffic
    NodeHotspot,
    /// Time-based traffic spike
    TemporalSpike,
    /// Write amplification hotspot
    WriteHotspot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum HotspotSeverity {
    /// Early warning, no action needed yet
    Low,
    /// Should investigate, may need action
    Medium,
    /// Immediate action recommended
    High,
    /// Critical, automatic mitigation triggered
    Critical,
}

#[derive(Debug, Clone)]
pub enum MitigationAction {
    /// No action needed
    None,
    /// Spread load across replicas
    LoadBalance,
    /// Cache hot data
    EnableCaching,
    /// Split hot partition
    SplitPartition { split_key: String },
    /// Add read replicas
    AddReplicas { count: u32 },
    /// Rate limit specific key
    RateLimitKey { rate: f64 },
    /// Shed load (reject some requests)
    LoadShed { percentage: f64 },
    /// Custom action
    Custom { action: String },
}

// ============================================================================
// Count-Min Sketch for Frequency Estimation
// ============================================================================

/// Count-Min Sketch for approximate frequency counting
///
/// Space-efficient probabilistic data structure for counting
/// frequencies with bounded error.
///
/// Properties:
/// - O(1) updates and queries
/// - Sublinear space (vs storing all keys)
/// - Overestimates frequency (never underestimates)
pub struct CountMinSketch {
    /// Width of each row
    width: usize,
    
    /// Number of hash functions (rows)
    depth: usize,
    
    /// Count matrix
    counts: Vec<Vec<AtomicU64>>,
    
    /// Hash seeds for each row
    seeds: Vec<u64>,
}

impl CountMinSketch {
    /// Create new sketch
    ///
    /// # Arguments
    /// - `width`: Number of columns (higher = more accurate)
    /// - `depth`: Number of hash functions (higher = lower error probability)
    pub fn new(width: usize, depth: usize) -> Self {
        let counts = (0..depth)
            .map(|_| (0..width).map(|_| AtomicU64::new(0)).collect())
            .collect();
        
        // Use different seeds for each row
        let seeds: Vec<u64> = (0..depth as u64).map(|i| i * 0x517cc1b727220a95).collect();
        
        Self {
            width,
            depth,
            counts,
            seeds,
        }
    }
    
    /// Increment count for a key
    pub fn increment(&self, key: &[u8]) {
        for (i, row) in self.counts.iter().enumerate() {
            let idx = self.hash(key, self.seeds[i]);
            row[idx].fetch_add(1, Ordering::Relaxed);
        }
    }
    
    /// Estimate count for a key
    pub fn estimate(&self, key: &[u8]) -> u64 {
        self.counts
            .iter()
            .enumerate()
            .map(|(i, row)| {
                let idx = self.hash(key, self.seeds[i]);
                row[idx].load(Ordering::Relaxed)
            })
            .min()
            .unwrap_or(0)
    }
    
    /// Reset all counts
    pub fn reset(&self) {
        for row in &self.counts {
            for cell in row {
                cell.store(0, Ordering::Relaxed);
            }
        }
    }
    
    fn hash(&self, key: &[u8], seed: u64) -> usize {
        use std::collections::hash_map::DefaultHasher;
        let mut hasher = DefaultHasher::new();
        seed.hash(&mut hasher);
        key.hash(&mut hasher);
        (hasher.finish() as usize) % self.width
    }
}

// ============================================================================
// Heavy Hitters Detection
// ============================================================================

/// Detects heavy hitters (keys with high frequency)
///
/// Uses Space-Saving algorithm to track top-k frequent items
/// with bounded memory.
pub struct HeavyHittersTracker {
    /// Maximum number of items to track
    capacity: usize,
    
    /// Item counts
    items: Mutex<Vec<(String, u64)>>,
    
    /// Threshold for heavy hitter (as fraction of total)
    threshold: f64,
    
    /// Total count
    total: AtomicU64,
    
    /// Count-Min Sketch for verification
    sketch: CountMinSketch,
}

impl HeavyHittersTracker {
    pub fn new(capacity: usize, threshold: f64) -> Self {
        Self {
            capacity,
            items: Mutex::new(Vec::with_capacity(capacity)),
            threshold,
            total: AtomicU64::new(0),
            sketch: CountMinSketch::new(1024, 4),
        }
    }
    
    /// Record an access to a key
    pub fn record(&self, key: &str) {
        self.total.fetch_add(1, Ordering::Relaxed);
        self.sketch.increment(key.as_bytes());
        
        let mut items = self.items.lock();
        
        // Check if key already tracked
        if let Some(pos) = items.iter().position(|(k, _)| k == key) {
            items[pos].1 += 1;
        } else if items.len() < self.capacity {
            // Add new item
            items.push((key.to_string(), 1));
        } else {
            // Replace minimum item (Space-Saving algorithm)
            if let Some(min_pos) = items.iter().enumerate().min_by_key(|(_, (_, c))| c).map(|(i, _)| i) {
                items[min_pos] = (key.to_string(), items[min_pos].1 + 1);
            }
        }
        
        // Keep sorted by count (descending)
        items.sort_by(|a, b| b.1.cmp(&a.1));
    }
    
    /// Get current heavy hitters
    pub fn get_heavy_hitters(&self) -> Vec<(String, u64, f64)> {
        let total = self.total.load(Ordering::Relaxed) as f64;
        if total == 0.0 {
            return Vec::new();
        }
        
        let items = self.items.lock();
        items
            .iter()
            .filter(|(_, count)| (*count as f64 / total) >= self.threshold)
            .map(|(key, count)| {
                let frequency = *count as f64 / total;
                (key.clone(), *count, frequency)
            })
            .collect()
    }
    
    /// Reset tracker
    pub fn reset(&self) {
        self.items.lock().clear();
        self.total.store(0, Ordering::Relaxed);
        self.sketch.reset();
    }
}

// ============================================================================
// Load Distribution Monitor
// ============================================================================

/// Monitors load distribution across nodes
pub struct LoadDistributionMonitor {
    /// Node load counters
    node_loads: RwLock<HashMap<NodeId, NodeLoadStats>>,
    
    /// Imbalance threshold (standard deviations)
    imbalance_threshold: f64,
    
    /// Window for load calculation
    window: Duration,
}

#[derive(Debug)]
struct NodeLoadStats {
    /// Request count in current window
    request_count: AtomicU64,
    /// Total latency in current window (microseconds)
    total_latency_us: AtomicU64,
    /// Window start time
    window_start: Mutex<Instant>,
    /// Historical load (for trend analysis)
    history: Mutex<Vec<f64>>,
}

impl NodeLoadStats {
    fn new() -> Self {
        Self {
            request_count: AtomicU64::new(0),
            total_latency_us: AtomicU64::new(0),
            window_start: Mutex::new(Instant::now()),
            history: Mutex::new(Vec::with_capacity(60)),
        }
    }
    
    fn requests_per_second(&self, window: Duration) -> f64 {
        let count = self.request_count.load(Ordering::Relaxed);
        count as f64 / window.as_secs_f64()
    }
    
    fn average_latency(&self) -> Duration {
        let count = self.request_count.load(Ordering::Relaxed);
        if count == 0 {
            return Duration::ZERO;
        }
        let total = self.total_latency_us.load(Ordering::Relaxed);
        Duration::from_micros(total / count)
    }
}

impl LoadDistributionMonitor {
    pub fn new(imbalance_threshold: f64, window: Duration) -> Self {
        Self {
            node_loads: RwLock::new(HashMap::new()),
            imbalance_threshold,
            window,
        }
    }
    
    /// Record a request to a node
    pub fn record_request(&self, node_id: NodeId, latency: Duration) {
        let loads = self.node_loads.read();
        
        if let Some(stats) = loads.get(&node_id) {
            stats.request_count.fetch_add(1, Ordering::Relaxed);
            stats.total_latency_us.fetch_add(latency.as_micros() as u64, Ordering::Relaxed);
        } else {
            drop(loads);
            let mut loads = self.node_loads.write();
            let stats = loads.entry(node_id).or_insert_with(NodeLoadStats::new);
            stats.request_count.fetch_add(1, Ordering::Relaxed);
            stats.total_latency_us.fetch_add(latency.as_micros() as u64, Ordering::Relaxed);
        }
    }
    
    /// Check for load imbalance
    pub fn detect_imbalance(&self) -> Option<LoadImbalance> {
        let loads = self.node_loads.read();
        
        if loads.len() < 2 {
            return None;
        }
        
        let rps_values: Vec<f64> = loads
            .values()
            .map(|s| s.requests_per_second(self.window))
            .collect();
        
        let mean = rps_values.iter().sum::<f64>() / rps_values.len() as f64;
        let variance = rps_values
            .iter()
            .map(|v| (v - mean).powi(2))
            .sum::<f64>() / rps_values.len() as f64;
        let std_dev = variance.sqrt();
        
        // Coefficient of variation
        let cv = if mean > 0.0 { std_dev / mean } else { 0.0 };
        
        // Find most loaded node
        let (hottest_node, hottest_load) = loads
            .iter()
            .map(|(id, s)| (*id, s.requests_per_second(self.window)))
            .max_by(|a, b| a.1.partial_cmp(&b.1).unwrap())?;
        
        // Check if imbalanced
        let z_score = if std_dev > 0.0 {
            (hottest_load - mean) / std_dev
        } else {
            0.0
        };
        
        if z_score > self.imbalance_threshold {
            Some(LoadImbalance {
                hottest_node,
                load: hottest_load,
                mean_load: mean,
                coefficient_of_variation: cv,
            })
        } else {
            None
        }
    }
    
    /// Get load metrics for all nodes
    pub fn get_node_loads(&self) -> HashMap<NodeId, (f64, Duration)> {
        let loads = self.node_loads.read();
        loads
            .iter()
            .map(|(id, stats)| {
                (*id, (stats.requests_per_second(self.window), stats.average_latency()))
            })
            .collect()
    }
    
    /// Rotate windows (call periodically)
    pub fn rotate_windows(&self) {
        let loads = self.node_loads.read();
        for stats in loads.values() {
            let mut start = stats.window_start.lock();
            if start.elapsed() >= self.window {
                let rps = stats.requests_per_second(self.window);
                
                // Save to history
                let mut history = stats.history.lock();
                if history.len() >= 60 {
                    history.remove(0);
                }
                history.push(rps);
                
                // Reset counters
                stats.request_count.store(0, Ordering::Relaxed);
                stats.total_latency_us.store(0, Ordering::Relaxed);
                *start = Instant::now();
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct LoadImbalance {
    pub hottest_node: NodeId,
    pub load: f64,
    pub mean_load: f64,
    pub coefficient_of_variation: f64,
}

// ============================================================================
// Hotspot Detector
// ============================================================================

/// Central hotspot detection system
pub struct HotspotDetector {
    /// Heavy hitters tracker per resource type
    heavy_hitters: RwLock<HashMap<String, Arc<HeavyHittersTracker>>>,
    
    /// Load distribution monitor
    load_monitor: Arc<LoadDistributionMonitor>,
    
    /// Configuration
    config: HotspotConfig,
    
    /// Current hotspots
    active_hotspots: RwLock<HashMap<ResourceKey, Hotspot>>,
    
    /// Hotspot handlers
    handlers: RwLock<Vec<Box<dyn HotspotHandler + Send + Sync>>>,
}

#[derive(Debug, Clone)]
pub struct HotspotConfig {
    /// Threshold for key hotspot (fraction of total)
    pub key_hotspot_threshold: f64,
    
    /// Threshold for node imbalance (std deviations)
    pub node_imbalance_threshold: f64,
    
    /// Minimum requests before hotspot detection
    pub min_requests: u64,
    
    /// Detection window
    pub window: Duration,
    
    /// Cooldown before re-detecting same hotspot
    pub cooldown: Duration,
}

impl Default for HotspotConfig {
    fn default() -> Self {
        Self {
            key_hotspot_threshold: 0.1, // 10% of traffic to single key
            node_imbalance_threshold: 2.0, // 2 std deviations
            min_requests: 100,
            window: Duration::from_secs(60),
            cooldown: Duration::from_secs(300),
        }
    }
}

impl HotspotDetector {
    pub fn new(config: HotspotConfig) -> Self {
        Self {
            heavy_hitters: RwLock::new(HashMap::new()),
            load_monitor: Arc::new(LoadDistributionMonitor::new(
                config.node_imbalance_threshold,
                config.window,
            )),
            config,
            active_hotspots: RwLock::new(HashMap::new()),
            handlers: RwLock::new(Vec::new()),
        }
    }
    
    /// Record an access to a key
    pub fn record_key_access(&self, resource_type: &str, key: &str) {
        let trackers = self.heavy_hitters.read();
        
        if let Some(tracker) = trackers.get(resource_type) {
            tracker.record(key);
        } else {
            drop(trackers);
            let mut trackers = self.heavy_hitters.write();
            let tracker = trackers
                .entry(resource_type.to_string())
                .or_insert_with(|| Arc::new(HeavyHittersTracker::new(100, self.config.key_hotspot_threshold)));
            tracker.record(key);
        }
    }
    
    /// Record a node request
    pub fn record_node_request(&self, node_id: NodeId, latency: Duration) {
        self.load_monitor.record_request(node_id, latency);
    }
    
    /// Detect hotspots
    pub fn detect(&self) -> Vec<Hotspot> {
        let mut hotspots = Vec::new();
        
        // Detect key hotspots
        let trackers = self.heavy_hitters.read();
        for (resource_type, tracker) in trackers.iter() {
            for (key, count, frequency) in tracker.get_heavy_hitters() {
                let resource = ResourceKey {
                    resource_type: resource_type.clone(),
                    id: key,
                };
                
                let severity = if frequency > 0.5 {
                    HotspotSeverity::Critical
                } else if frequency > 0.3 {
                    HotspotSeverity::High
                } else if frequency > 0.2 {
                    HotspotSeverity::Medium
                } else {
                    HotspotSeverity::Low
                };
                
                let action = match severity {
                    HotspotSeverity::Critical => MitigationAction::LoadShed { percentage: 0.2 },
                    HotspotSeverity::High => MitigationAction::EnableCaching,
                    _ => MitigationAction::None,
                };
                
                hotspots.push(Hotspot {
                    resource,
                    hotspot_type: HotspotType::KeyHotspot,
                    current_load: count as f64,
                    threshold: self.config.key_hotspot_threshold,
                    severity,
                    detected_at: Instant::now(),
                    affected_tenants: Vec::new(),
                    suggested_action: action,
                });
            }
        }
        
        // Detect node hotspots
        if let Some(imbalance) = self.load_monitor.detect_imbalance() {
            let severity = if imbalance.coefficient_of_variation > 1.0 {
                HotspotSeverity::Critical
            } else if imbalance.coefficient_of_variation > 0.5 {
                HotspotSeverity::High
            } else {
                HotspotSeverity::Medium
            };
            
            hotspots.push(Hotspot {
                resource: ResourceKey {
                    resource_type: "node".to_string(),
                    id: imbalance.hottest_node.0.to_string(),
                },
                hotspot_type: HotspotType::NodeHotspot,
                current_load: imbalance.load,
                threshold: imbalance.mean_load * 2.0,
                severity,
                detected_at: Instant::now(),
                affected_tenants: Vec::new(),
                suggested_action: MitigationAction::LoadBalance,
            });
        }
        
        // Update active hotspots and notify handlers
        for hotspot in &hotspots {
            self.active_hotspots
                .write()
                .insert(hotspot.resource.clone(), hotspot.clone());
            
            for handler in self.handlers.read().iter() {
                handler.on_hotspot_detected(hotspot);
            }
        }
        
        hotspots
    }
    
    /// Register a hotspot handler
    pub fn on_hotspot<H: HotspotHandler + Send + Sync + 'static>(&self, handler: H) {
        self.handlers.write().push(Box::new(handler));
    }
    
    /// Get currently active hotspots
    pub fn active_hotspots(&self) -> Vec<Hotspot> {
        self.active_hotspots.read().values().cloned().collect()
    }
    
    /// Clear a resolved hotspot
    pub fn clear_hotspot(&self, resource: &ResourceKey) {
        let removed = self.active_hotspots.write().remove(resource);
        
        if let Some(hotspot) = removed {
            for handler in self.handlers.read().iter() {
                handler.on_hotspot_resolved(&hotspot);
            }
        }
    }
}

// ============================================================================
// Hotspot Handler
// ============================================================================

/// Handler for hotspot events
pub trait HotspotHandler: Send + Sync {
    /// Called when a hotspot is detected
    fn on_hotspot_detected(&self, hotspot: &Hotspot);
    
    /// Called when a hotspot is resolved
    fn on_hotspot_resolved(&self, hotspot: &Hotspot);
}

/// Logging hotspot handler
pub struct LoggingHotspotHandler;

impl HotspotHandler for LoggingHotspotHandler {
    fn on_hotspot_detected(&self, hotspot: &Hotspot) {
        tracing::warn!(
            resource_type = hotspot.resource.resource_type,
            resource_id = hotspot.resource.id,
            hotspot_type = ?hotspot.hotspot_type,
            severity = ?hotspot.severity,
            load = hotspot.current_load,
            threshold = hotspot.threshold,
            action = ?hotspot.suggested_action,
            "Hotspot detected"
        );
    }
    
    fn on_hotspot_resolved(&self, hotspot: &Hotspot) {
        tracing::info!(
            resource_type = hotspot.resource.resource_type,
            resource_id = hotspot.resource.id,
            "Hotspot resolved"
        );
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_count_min_sketch() {
        let sketch = CountMinSketch::new(1000, 4);
        
        // Increment various keys
        for _ in 0..100 {
            sketch.increment(b"hot_key");
        }
        for _ in 0..10 {
            sketch.increment(b"cold_key");
        }
        
        // Hot key should have higher count
        assert!(sketch.estimate(b"hot_key") >= 100);
        assert!(sketch.estimate(b"cold_key") >= 10);
        assert!(sketch.estimate(b"hot_key") > sketch.estimate(b"cold_key"));
    }
    
    #[test]
    fn test_heavy_hitters() {
        let tracker = HeavyHittersTracker::new(10, 0.1);
        
        // Create a heavy hitter
        for _ in 0..100 {
            tracker.record("hot_key");
        }
        for _ in 0..10 {
            tracker.record("cold_key");
        }
        
        let heavies = tracker.get_heavy_hitters();
        assert!(!heavies.is_empty());
        assert_eq!(heavies[0].0, "hot_key");
    }
    
    #[test]
    fn test_hotspot_detection() {
        let detector = HotspotDetector::new(HotspotConfig {
            key_hotspot_threshold: 0.3, // 30%
            ..Default::default()
        });
        
        // Create a clear hotspot
        for _ in 0..100 {
            detector.record_key_access("partition", "hot-partition");
        }
        for _ in 0..20 {
            detector.record_key_access("partition", "normal-partition");
        }
        
        let hotspots = detector.detect();
        // Should detect the hot partition
        assert!(hotspots.iter().any(|h| h.resource.id == "hot-partition"));
    }
}
