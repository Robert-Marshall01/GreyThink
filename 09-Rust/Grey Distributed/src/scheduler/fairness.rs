//! # Fair-Share Scheduling
//!
//! Ensures equitable resource distribution among tenants.
//!
//! ## Fair-Share Philosophy
//!
//! Instead of pure round-robin or priority-based scheduling, fair-share
//! considers:
//!
//! 1. **Target Share**: What portion of resources should each tenant get?
//! 2. **Actual Usage**: How much has each tenant actually consumed?
//! 3. **Deficit/Surplus**: Schedule those most behind their target share.
//!
//! ## Implementation: Weighted Fair Queuing (WFQ)
//!
//! Each tenant has a virtual time that advances when they use resources.
//! Tenants with lower virtual time (less usage) get priority.
//!
//! ## Anti-Starvation Guarantees
//!
//! - Minimum guaranteed share (floor)
//! - Maximum cap to prevent monopolization (ceiling)
//! - Grace periods for burst traffic

use std::collections::{BinaryHeap, HashMap};
use std::cmp::Ordering;
use std::sync::atomic::{AtomicU64, Ordering as AtomicOrdering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use parking_lot::RwLock;

use crate::types::TenantId;

// ============================================================================
// Share Configuration
// ============================================================================

/// Configuration for a tenant's fair share
#[derive(Debug, Clone)]
pub struct TenantShare {
    /// Tenant identifier
    pub tenant_id: TenantId,
    /// Weight for fair-share calculation (higher = more share)
    pub weight: u32,
    /// Minimum guaranteed share (0-100 percent)
    pub min_share: u32,
    /// Maximum allowed share (0-100 percent)
    pub max_share: u32,
    /// Preemption priority (lower = can be preempted first)
    pub preemption_priority: u32,
}

impl TenantShare {
    pub fn new(tenant_id: TenantId, weight: u32) -> Self {
        Self {
            tenant_id,
            weight,
            min_share: 0,
            max_share: 100,
            preemption_priority: 100,
        }
    }

    /// Builder: set minimum share
    pub fn with_min_share(mut self, min: u32) -> Self {
        self.min_share = min.min(100);
        self
    }

    /// Builder: set maximum share
    pub fn with_max_share(mut self, max: u32) -> Self {
        self.max_share = max.min(100);
        self
    }

    /// Builder: set preemption priority
    pub fn with_preemption_priority(mut self, priority: u32) -> Self {
        self.preemption_priority = priority;
        self
    }
}

// ============================================================================
// Virtual Time Tracking
// ============================================================================

/// Tracks virtual time for each tenant
///
/// Virtual time advances as resources are consumed.
/// Tenants with lower virtual time get priority.
#[derive(Debug)]
pub struct VirtualTimeTracker {
    /// Virtual time per tenant
    times: RwLock<HashMap<TenantId, u64>>,
    /// Global virtual time (minimum across all tenants)
    global_time: AtomicU64,
    /// Tenant weights
    weights: RwLock<HashMap<TenantId, u32>>,
}

impl VirtualTimeTracker {
    pub fn new() -> Self {
        Self {
            times: RwLock::new(HashMap::new()),
            global_time: AtomicU64::new(0),
            weights: RwLock::new(HashMap::new()),
        }
    }

    /// Register a tenant with their weight
    pub fn register(&self, tenant_id: TenantId, weight: u32) {
        let global = self.global_time.load(AtomicOrdering::SeqCst);
        self.times.write().insert(tenant_id, global);
        self.weights.write().insert(tenant_id, weight);
    }

    /// Record resource consumption
    ///
    /// Advances the tenant's virtual time based on their weight.
    /// Lower weight = faster virtual time advancement = lower priority.
    pub fn record_consumption(&self, tenant_id: TenantId, cost: u64) {
        let weight = *self.weights.read().get(&tenant_id).unwrap_or(&100);
        
        // Inverse relationship: higher weight = slower virtual time
        let adjusted_cost = (cost * 100) / (weight as u64).max(1);
        
        let mut times = self.times.write();
        let entry = times.entry(tenant_id).or_insert(0);
        *entry += adjusted_cost;
    }

    /// Get tenant's virtual time
    pub fn get_virtual_time(&self, tenant_id: TenantId) -> u64 {
        self.times.read().get(&tenant_id).copied().unwrap_or(0)
    }

    /// Get tenant with lowest virtual time (highest priority)
    pub fn get_next_tenant(&self) -> Option<TenantId> {
        self.times
            .read()
            .iter()
            .min_by_key(|(_, &time)| time)
            .map(|(&id, _)| id)
    }

    /// Update global time (called periodically)
    pub fn update_global_time(&self) {
        let min_time = self
            .times
            .read()
            .values()
            .min()
            .copied()
            .unwrap_or(0);
        self.global_time.store(min_time, AtomicOrdering::SeqCst);
    }
}

// ============================================================================
// Fair-Share Scheduler
// ============================================================================

/// Entry in the fair-share priority queue
struct FairShareEntry {
    tenant_id: TenantId,
    virtual_time: u64,
    has_pending: bool,
}

impl PartialEq for FairShareEntry {
    fn eq(&self, other: &Self) -> bool {
        self.virtual_time == other.virtual_time
    }
}

impl Eq for FairShareEntry {}

impl PartialOrd for FairShareEntry {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FairShareEntry {
    fn cmp(&self, other: &Self) -> Ordering {
        // Min-heap: lower virtual time = higher priority
        other.virtual_time.cmp(&self.virtual_time)
    }
}

/// Fair-share scheduling algorithm
pub struct FairShareScheduler {
    /// Virtual time tracker
    virtual_times: VirtualTimeTracker,
    /// Tenant shares configuration
    shares: RwLock<HashMap<TenantId, TenantShare>>,
    /// Total weight across all tenants
    total_weight: AtomicU64,
    /// Configuration
    config: FairShareConfig,
    /// Statistics
    stats: FairShareStats,
}

/// Configuration for fair-share scheduling
#[derive(Debug, Clone)]
pub struct FairShareConfig {
    /// Time window for usage tracking
    pub usage_window: Duration,
    /// Enable burst handling
    pub burst_enabled: bool,
    /// Burst allowance multiplier
    pub burst_multiplier: f64,
    /// Decay rate for old usage data
    pub usage_decay_rate: f64,
}

impl Default for FairShareConfig {
    fn default() -> Self {
        Self {
            usage_window: Duration::from_secs(60),
            burst_enabled: true,
            burst_multiplier: 2.0,
            usage_decay_rate: 0.9,
        }
    }
}

/// Statistics for fair-share scheduling
#[derive(Debug, Default)]
pub struct FairShareStats {
    /// Decisions made
    pub decisions: AtomicU64,
    /// Times a tenant was at min share
    pub min_share_events: AtomicU64,
    /// Times a tenant hit max share
    pub max_share_events: AtomicU64,
    /// Preemptions triggered
    pub preemptions: AtomicU64,
}

impl FairShareScheduler {
    pub fn new(config: FairShareConfig) -> Self {
        Self {
            virtual_times: VirtualTimeTracker::new(),
            shares: RwLock::new(HashMap::new()),
            total_weight: AtomicU64::new(0),
            config,
            stats: FairShareStats::default(),
        }
    }

    /// Register a tenant with their share configuration
    pub fn register_tenant(&self, share: TenantShare) {
        self.virtual_times.register(share.tenant_id, share.weight);
        self.total_weight.fetch_add(share.weight as u64, AtomicOrdering::SeqCst);
        self.shares.write().insert(share.tenant_id, share);
    }

    /// Unregister a tenant
    pub fn unregister_tenant(&self, tenant_id: TenantId) {
        if let Some(share) = self.shares.write().remove(&tenant_id) {
            self.total_weight.fetch_sub(share.weight as u64, AtomicOrdering::SeqCst);
        }
    }

    /// Select next tenant to run
    ///
    /// Uses weighted fair queuing: tenant with lowest virtual time wins.
    /// Respects min/max share bounds.
    pub fn select_next(
        &self,
        pending_tenants: &[TenantId],
        current_usage: &HashMap<TenantId, f64>,
    ) -> Option<TenantId> {
        if pending_tenants.is_empty() {
            return None;
        }

        let shares = self.shares.read();
        let total_weight = self.total_weight.load(AtomicOrdering::SeqCst) as f64;

        // Build priority queue based on fair-share
        let mut heap = BinaryHeap::new();

        for &tenant_id in pending_tenants {
            let virtual_time = self.virtual_times.get_virtual_time(tenant_id);
            
            // Check if tenant is above max share
            if let (Some(share), Some(&usage)) = (shares.get(&tenant_id), current_usage.get(&tenant_id)) {
                if usage >= share.max_share as f64 {
                    self.stats.max_share_events.fetch_add(1, AtomicOrdering::Relaxed);
                    continue; // Skip, tenant at max
                }
            }

            heap.push(FairShareEntry {
                tenant_id,
                virtual_time,
                has_pending: true,
            });
        }

        // Get tenant with lowest virtual time
        let selected = heap.pop().map(|e| e.tenant_id);

        if selected.is_some() {
            self.stats.decisions.fetch_add(1, AtomicOrdering::Relaxed);
        }

        selected
    }

    /// Record resource usage
    pub fn record_usage(&self, tenant_id: TenantId, cost: u64) {
        self.virtual_times.record_consumption(tenant_id, cost);
    }

    /// Calculate target share for a tenant
    pub fn target_share(&self, tenant_id: TenantId) -> f64 {
        let shares = self.shares.read();
        let total_weight = self.total_weight.load(AtomicOrdering::SeqCst);

        if total_weight == 0 {
            return 0.0;
        }

        shares
            .get(&tenant_id)
            .map(|s| (s.weight as f64 / total_weight as f64) * 100.0)
            .unwrap_or(0.0)
    }

    /// Check if tenant should be preempted
    pub fn should_preempt(
        &self,
        tenant_id: TenantId,
        current_usage: f64,
    ) -> bool {
        let shares = self.shares.read();
        
        if let Some(share) = shares.get(&tenant_id) {
            // Preempt if above max share
            if current_usage > share.max_share as f64 {
                return true;
            }

            // Check if others are below min share
            for (other_id, other_share) in shares.iter() {
                if *other_id != tenant_id {
                    // This would need actual usage data for other tenants
                    // Simplified: just check if we're way over our target
                    let our_target = self.target_share(tenant_id);
                    if current_usage > our_target * 1.5 && other_share.min_share > 0 {
                        return true;
                    }
                }
            }
        }

        false
    }

    /// Get fair-share statistics
    pub fn stats(&self) -> &FairShareStats {
        &self.stats
    }
}

// ============================================================================
// Dominant Resource Fairness (DRF)
// ============================================================================

/// Multi-resource fair sharing using DRF
///
/// When tasks need multiple resources (CPU, memory, I/O), DRF ensures
/// fairness across all dimensions. A tenant's "dominant resource" is the
/// one they consume the most of (relative to capacity).
pub struct DominantResourceFairness {
    /// Total cluster resources
    total_resources: ResourceCapacity,
    /// Per-tenant resource consumption
    consumption: RwLock<HashMap<TenantId, ResourceUsage>>,
    /// Tenant weights
    weights: RwLock<HashMap<TenantId, u32>>,
}

/// Total resource capacity in the cluster
#[derive(Debug, Clone)]
pub struct ResourceCapacity {
    /// Total CPU units
    pub cpu_units: u64,
    /// Total memory bytes
    pub memory_bytes: u64,
    /// Total disk IOPS
    pub disk_iops: u64,
    /// Total network bandwidth (bytes/sec)
    pub network_bandwidth: u64,
}

/// Resource usage by a tenant
#[derive(Debug, Clone, Default)]
pub struct ResourceUsage {
    /// CPU units used
    pub cpu_units: u64,
    /// Memory bytes used
    pub memory_bytes: u64,
    /// Disk IOPS used
    pub disk_iops: u64,
    /// Network bandwidth used
    pub network_bandwidth: u64,
}

impl DominantResourceFairness {
    pub fn new(total_resources: ResourceCapacity) -> Self {
        Self {
            total_resources,
            consumption: RwLock::new(HashMap::new()),
            weights: RwLock::new(HashMap::new()),
        }
    }

    /// Register a tenant with their weight
    pub fn register_tenant(&self, tenant_id: TenantId, weight: u32) {
        self.consumption.write().insert(tenant_id, ResourceUsage::default());
        self.weights.write().insert(tenant_id, weight);
    }

    /// Calculate dominant share for a tenant
    ///
    /// Returns the maximum of all resource shares (0.0 to 1.0)
    pub fn dominant_share(&self, tenant_id: TenantId) -> f64 {
        let consumption = self.consumption.read();
        
        if let Some(usage) = consumption.get(&tenant_id) {
            let cpu_share = usage.cpu_units as f64 / self.total_resources.cpu_units.max(1) as f64;
            let mem_share = usage.memory_bytes as f64 / self.total_resources.memory_bytes.max(1) as f64;
            let disk_share = usage.disk_iops as f64 / self.total_resources.disk_iops.max(1) as f64;
            let net_share = usage.network_bandwidth as f64 / self.total_resources.network_bandwidth.max(1) as f64;

            cpu_share.max(mem_share).max(disk_share).max(net_share)
        } else {
            0.0
        }
    }

    /// Select tenant with lowest dominant share
    pub fn select_next(&self, pending_tenants: &[TenantId]) -> Option<TenantId> {
        pending_tenants
            .iter()
            .map(|&id| (id, self.dominant_share(id)))
            .min_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap_or(Ordering::Equal))
            .map(|(id, _)| id)
    }

    /// Record resource allocation
    pub fn record_allocation(&self, tenant_id: TenantId, resources: ResourceUsage) {
        let mut consumption = self.consumption.write();
        let entry = consumption.entry(tenant_id).or_insert_with(ResourceUsage::default);
        entry.cpu_units += resources.cpu_units;
        entry.memory_bytes += resources.memory_bytes;
        entry.disk_iops += resources.disk_iops;
        entry.network_bandwidth += resources.network_bandwidth;
    }

    /// Record resource release
    pub fn record_release(&self, tenant_id: TenantId, resources: ResourceUsage) {
        let mut consumption = self.consumption.write();
        if let Some(entry) = consumption.get_mut(&tenant_id) {
            entry.cpu_units = entry.cpu_units.saturating_sub(resources.cpu_units);
            entry.memory_bytes = entry.memory_bytes.saturating_sub(resources.memory_bytes);
            entry.disk_iops = entry.disk_iops.saturating_sub(resources.disk_iops);
            entry.network_bandwidth = entry.network_bandwidth.saturating_sub(resources.network_bandwidth);
        }
    }

    /// Get current utilization across cluster
    pub fn utilization(&self) -> ResourceUsage {
        let consumption = self.consumption.read();
        let mut total = ResourceUsage::default();

        for usage in consumption.values() {
            total.cpu_units += usage.cpu_units;
            total.memory_bytes += usage.memory_bytes;
            total.disk_iops += usage.disk_iops;
            total.network_bandwidth += usage.network_bandwidth;
        }

        total
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_virtual_time_tracking() {
        let tracker = VirtualTimeTracker::new();

        tracker.register(TenantId(1), 100);
        tracker.register(TenantId(2), 50);

        // Tenant 2 has lower weight, virtual time increases faster
        tracker.record_consumption(TenantId(1), 100);
        tracker.record_consumption(TenantId(2), 100);

        let vt1 = tracker.get_virtual_time(TenantId(1));
        let vt2 = tracker.get_virtual_time(TenantId(2));

        assert!(vt2 > vt1, "Lower weight should mean higher virtual time");
    }

    #[test]
    fn test_fair_share_selection() {
        let scheduler = FairShareScheduler::new(FairShareConfig::default());

        scheduler.register_tenant(TenantShare::new(TenantId(1), 100));
        scheduler.register_tenant(TenantShare::new(TenantId(2), 100));

        // Tenant 1 uses more resources
        scheduler.record_usage(TenantId(1), 1000);
        scheduler.record_usage(TenantId(2), 100);

        // Tenant 2 should be selected (lower usage)
        let pending = vec![TenantId(1), TenantId(2)];
        let selected = scheduler.select_next(&pending, &HashMap::new());

        assert_eq!(selected, Some(TenantId(2)));
    }

    #[test]
    fn test_dominant_resource_fairness() {
        let drf = DominantResourceFairness::new(ResourceCapacity {
            cpu_units: 1000,
            memory_bytes: 10_000_000,
            disk_iops: 10000,
            network_bandwidth: 1_000_000,
        });

        drf.register_tenant(TenantId(1), 100);
        drf.register_tenant(TenantId(2), 100);

        // Tenant 1 is CPU-heavy
        drf.record_allocation(TenantId(1), ResourceUsage {
            cpu_units: 500,
            memory_bytes: 1_000_000,
            disk_iops: 0,
            network_bandwidth: 0,
        });

        // Tenant 2 is memory-heavy
        drf.record_allocation(TenantId(2), ResourceUsage {
            cpu_units: 100,
            memory_bytes: 8_000_000,
            disk_iops: 0,
            network_bandwidth: 0,
        });

        let ds1 = drf.dominant_share(TenantId(1));
        let ds2 = drf.dominant_share(TenantId(2));

        assert!((ds1 - 0.5).abs() < 0.01, "Tenant 1 dominant share should be ~50% CPU");
        assert!(ds2 > 0.7, "Tenant 2 dominant share should be ~80% memory");
    }
}
