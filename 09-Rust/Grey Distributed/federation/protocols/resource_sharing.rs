//! # Grey Distributed — Resource Sharing Protocol
//!
//! This module implements quota enforcement and resource sharing across
//! federated Grey clusters.
//!
//! ## Overview
//!
//! Resource sharing in a federation involves:
//! 1. **Global Quotas**: Organization-wide resource limits
//! 2. **Cluster Allocations**: Per-cluster resource budgets
//! 3. **Burst Sharing**: Temporary resource borrowing across clusters
//! 4. **Fair Scheduling**: Cross-cluster workload distribution
//!
//! ## Design Principles
//!
//! - **Sovereignty**: Each cluster controls its local resources
//! - **Fairness**: Tenants receive proportional resources globally
//! - **Efficiency**: Idle resources can be borrowed by others
//! - **Accountability**: All resource usage is tracked and auditable

use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};

// =============================================================================
// Types and Constants
// =============================================================================

pub type TenantId = String;
pub type ClusterId = String;
pub type OrganizationId = String;

/// Maximum time a burst allocation can be held
const MAX_BURST_DURATION: Duration = Duration::from_secs(3600); // 1 hour

/// Minimum guaranteed allocation percentage
const MIN_GUARANTEED_PERCENT: f64 = 0.5; // 50% of quota is guaranteed

/// Burst premium multiplier (cost for using borrowed resources)
const BURST_PREMIUM: f64 = 1.5;

// =============================================================================
// Resource Types
// =============================================================================

/// Resource metrics tracked across federation
#[derive(Debug, Clone, Default)]
pub struct Resources {
    pub cpu_cores: f64,
    pub memory_gb: f64,
    pub storage_gb: f64,
    pub network_gbps: f64,
    pub gpu_units: u32,
}

impl Resources {
    pub fn new(cpu: f64, memory: f64, storage: f64, network: f64, gpu: u32) -> Self {
        Self {
            cpu_cores: cpu,
            memory_gb: memory,
            storage_gb: storage,
            network_gbps: network,
            gpu_units: gpu,
        }
    }
    
    /// Check if all resources are within limits
    pub fn within(&self, limit: &Resources) -> bool {
        self.cpu_cores <= limit.cpu_cores
            && self.memory_gb <= limit.memory_gb
            && self.storage_gb <= limit.storage_gb
            && self.network_gbps <= limit.network_gbps
            && self.gpu_units <= limit.gpu_units
    }
    
    /// Add resources
    pub fn add(&mut self, other: &Resources) {
        self.cpu_cores += other.cpu_cores;
        self.memory_gb += other.memory_gb;
        self.storage_gb += other.storage_gb;
        self.network_gbps += other.network_gbps;
        self.gpu_units += other.gpu_units;
    }
    
    /// Subtract resources
    pub fn subtract(&mut self, other: &Resources) {
        self.cpu_cores = (self.cpu_cores - other.cpu_cores).max(0.0);
        self.memory_gb = (self.memory_gb - other.memory_gb).max(0.0);
        self.storage_gb = (self.storage_gb - other.storage_gb).max(0.0);
        self.network_gbps = (self.network_gbps - other.network_gbps).max(0.0);
        self.gpu_units = self.gpu_units.saturating_sub(other.gpu_units);
    }
    
    /// Calculate utilization percentage
    pub fn utilization(&self, capacity: &Resources) -> f64 {
        let cpu_util = if capacity.cpu_cores > 0.0 {
            self.cpu_cores / capacity.cpu_cores
        } else {
            0.0
        };
        let mem_util = if capacity.memory_gb > 0.0 {
            self.memory_gb / capacity.memory_gb
        } else {
            0.0
        };
        
        (cpu_util + mem_util) / 2.0
    }
}

// =============================================================================
// Quota Definitions
// =============================================================================

/// Global quota for a tenant across all clusters
#[derive(Debug, Clone)]
pub struct GlobalQuota {
    pub tenant_id: TenantId,
    pub organization_id: OrganizationId,
    
    /// Maximum resources across all clusters
    pub total_limit: Resources,
    
    /// Guaranteed minimum (always available)
    pub guaranteed: Resources,
    
    /// Burst limit (temporary overage allowed)
    pub burst_limit: Resources,
    
    /// Priority class (1 = highest, 3 = lowest)
    pub priority: u8,
    
    /// Whether tenant can borrow from others
    pub burst_enabled: bool,
    
    /// Whether tenant can lend to others
    pub lending_enabled: bool,
    
    /// Cost multiplier for this tenant
    pub cost_multiplier: f64,
}

/// Cluster-specific allocation for a tenant
#[derive(Debug, Clone)]
pub struct ClusterAllocation {
    pub tenant_id: TenantId,
    pub cluster_id: ClusterId,
    
    /// Resources allocated to this tenant on this cluster
    pub allocated: Resources,
    
    /// Currently used resources
    pub used: Resources,
    
    /// Resources borrowed from other clusters
    pub borrowed: Resources,
    
    /// Resources lent to other clusters
    pub lent: Resources,
    
    /// When this allocation was last updated
    pub last_updated: Instant,
}

impl ClusterAllocation {
    /// Calculate available resources (allocated - used + borrowed - lent)
    pub fn available(&self) -> Resources {
        let mut available = self.allocated.clone();
        available.subtract(&self.used);
        available.add(&self.borrowed);
        available.subtract(&self.lent);
        available
    }
    
    /// Calculate effective utilization
    pub fn utilization(&self) -> f64 {
        self.used.utilization(&self.allocated)
    }
}

// =============================================================================
// Burst Request and Response
// =============================================================================

/// Request to borrow resources from another cluster
#[derive(Debug, Clone)]
pub struct BurstRequest {
    pub request_id: String,
    pub tenant_id: TenantId,
    pub requesting_cluster: ClusterId,
    pub target_cluster: ClusterId,
    pub resources_needed: Resources,
    pub duration: Duration,
    pub priority: u8,
    pub reason: String,
}

/// Response to a burst request
#[derive(Debug, Clone)]
pub enum BurstResponse {
    Approved {
        request_id: String,
        granted: Resources,
        expires_at: Instant,
        cost_multiplier: f64,
    },
    PartiallyApproved {
        request_id: String,
        granted: Resources,
        expires_at: Instant,
        reason: String,
    },
    Denied {
        request_id: String,
        reason: String,
        retry_after: Option<Duration>,
    },
}

/// Active burst allocation
#[derive(Debug, Clone)]
pub struct BurstAllocation {
    pub request_id: String,
    pub tenant_id: TenantId,
    pub borrower_cluster: ClusterId,
    pub lender_cluster: ClusterId,
    pub resources: Resources,
    pub created_at: Instant,
    pub expires_at: Instant,
    pub cost_multiplier: f64,
}

// =============================================================================
// Resource Sharing Manager
// =============================================================================

/// Manages resource sharing across federated clusters
pub struct ResourceSharingManager {
    /// This cluster's ID
    cluster_id: ClusterId,
    
    /// Global quotas for all tenants
    global_quotas: HashMap<TenantId, GlobalQuota>,
    
    /// Per-cluster allocations
    cluster_allocations: HashMap<(TenantId, ClusterId), ClusterAllocation>,
    
    /// Active burst allocations
    bursts: HashMap<String, BurstAllocation>,
    
    /// Cluster capacities
    cluster_capacities: HashMap<ClusterId, Resources>,
    
    /// Cluster current usage
    cluster_usage: HashMap<ClusterId, Resources>,
}

impl ResourceSharingManager {
    pub fn new(cluster_id: ClusterId) -> Self {
        Self {
            cluster_id,
            global_quotas: HashMap::new(),
            cluster_allocations: HashMap::new(),
            bursts: HashMap::new(),
            cluster_capacities: HashMap::new(),
            cluster_usage: HashMap::new(),
        }
    }
    
    // =========================================================================
    // Step 1: Register Quotas
    // =========================================================================
    
    /// Register a global quota for a tenant
    /// 
    /// This is called when a tenant is onboarded or their quota changes.
    pub fn register_quota(&mut self, quota: GlobalQuota) {
        self.global_quotas.insert(quota.tenant_id.clone(), quota);
    }
    
    // =========================================================================
    // Step 2: Allocate to Cluster
    // =========================================================================
    
    /// Allocate a portion of a tenant's global quota to a specific cluster
    /// 
    /// This distributes the global quota across clusters based on:
    /// - Tenanthint preferences (affinity)
    /// - Cluster capacity
    /// - Historical usage patterns
    pub fn allocate_to_cluster(
        &mut self,
        tenant_id: &TenantId,
        cluster_id: &ClusterId,
        allocated: Resources,
    ) -> Result<(), String> {
        // Step 2a: Verify tenant has a global quota
        let quota = self.global_quotas.get(tenant_id)
            .ok_or_else(|| format!("No global quota for tenant {}", tenant_id))?;
        
        // Step 2b: Verify allocation doesn't exceed global limit
        let total_allocated = self.total_allocated_for_tenant(tenant_id);
        let mut new_total = total_allocated.clone();
        new_total.add(&allocated);
        
        if !new_total.within(&quota.total_limit) {
            return Err(format!(
                "Allocation would exceed global limit for tenant {}",
                tenant_id
            ));
        }
        
        // Step 2c: Create cluster allocation
        let allocation = ClusterAllocation {
            tenant_id: tenant_id.clone(),
            cluster_id: cluster_id.clone(),
            allocated,
            used: Resources::default(),
            borrowed: Resources::default(),
            lent: Resources::default(),
            last_updated: Instant::now(),
        };
        
        self.cluster_allocations.insert(
            (tenant_id.clone(), cluster_id.clone()),
            allocation,
        );
        
        Ok(())
    }
    
    // =========================================================================
    // Step 3: Request Resources
    // =========================================================================
    
    /// Request resources for a task
    /// 
    /// This is called before scheduling a task to ensure resources are available.
    pub fn request_resources(
        &mut self,
        tenant_id: &TenantId,
        cluster_id: &ClusterId,
        requested: &Resources,
    ) -> ResourceDecision {
        // Step 3a: Get current allocation
        let allocation = match self.cluster_allocations.get(&(tenant_id.clone(), cluster_id.clone())) {
            Some(a) => a,
            None => return ResourceDecision::Denied {
                reason: "No allocation for tenant on this cluster".to_string(),
            },
        };
        
        let quota = match self.global_quotas.get(tenant_id) {
            Some(q) => q,
            None => return ResourceDecision::Denied {
                reason: "No global quota for tenant".to_string(),
            },
        };
        
        // Step 3b: Check if within guaranteed allocation
        let guaranteed_available = self.guaranteed_available(tenant_id, cluster_id);
        if requested.within(&guaranteed_available) {
            return ResourceDecision::Granted {
                source: ResourceSource::Guaranteed,
                cost_multiplier: 1.0,
            };
        }
        
        // Step 3c: Check if within base allocation
        let available = allocation.available();
        if requested.within(&available) {
            return ResourceDecision::Granted {
                source: ResourceSource::Allocated,
                cost_multiplier: 1.0,
            };
        }
        
        // Step 3d: Check if burst is possible
        if !quota.burst_enabled {
            return ResourceDecision::Denied {
                reason: "Resources exceeded and burst not enabled".to_string(),
            };
        }
        
        // Step 3e: Calculate burst amount needed
        let mut burst_needed = requested.clone();
        burst_needed.subtract(&available);
        
        // Check burst limit
        let total_burst = self.total_burst_for_tenant(tenant_id);
        let mut new_burst = total_burst.clone();
        new_burst.add(&burst_needed);
        
        if !new_burst.within(&quota.burst_limit) {
            return ResourceDecision::Denied {
                reason: "Burst limit exceeded".to_string(),
            };
        }
        
        // Step 3f: Check if any cluster can lend
        let lender = self.find_lender(tenant_id, cluster_id, &burst_needed);
        match lender {
            Some(lender_cluster) => ResourceDecision::BurstRequired {
                lender_cluster,
                resources: burst_needed,
                cost_multiplier: BURST_PREMIUM,
            },
            None => ResourceDecision::Queued {
                reason: "No lender available, queued until resources free".to_string(),
                estimated_wait: Duration::from_secs(60),
            },
        }
    }
    
    // =========================================================================
    // Step 4: Execute Burst
    // =========================================================================
    
    /// Execute a burst resource transfer
    /// 
    /// Called after finding a lender and getting approval.
    pub fn execute_burst(&mut self, request: BurstRequest) -> BurstResponse {
        // Step 4a: Validate request
        if request.duration > MAX_BURST_DURATION {
            return BurstResponse::Denied {
                request_id: request.request_id,
                reason: format!(
                    "Burst duration {} exceeds maximum {}",
                    request.duration.as_secs(),
                    MAX_BURST_DURATION.as_secs()
                ),
                retry_after: None,
            };
        }
        
        // Step 4b: Check lender capacity
        let lender_allocation = match self.cluster_allocations.get(
            &(request.tenant_id.clone(), request.target_cluster.clone())
        ) {
            Some(a) => a,
            None => return BurstResponse::Denied {
                request_id: request.request_id,
                reason: "Lender has no allocation for this tenant".to_string(),
                retry_after: None,
            },
        };
        
        let available_to_lend = self.available_to_lend(
            &request.tenant_id,
            &request.target_cluster,
        );
        
        if !request.resources_needed.within(&available_to_lend) {
            // Step 4c: Partial grant if possible
            let granted = self.calculate_partial_grant(
                &request.resources_needed,
                &available_to_lend,
            );
            
            if granted.cpu_cores > 0.0 || granted.memory_gb > 0.0 {
                let expires_at = Instant::now() + request.duration;
                
                // Record burst
                let allocation = BurstAllocation {
                    request_id: request.request_id.clone(),
                    tenant_id: request.tenant_id.clone(),
                    borrower_cluster: request.requesting_cluster.clone(),
                    lender_cluster: request.target_cluster.clone(),
                    resources: granted.clone(),
                    created_at: Instant::now(),
                    expires_at,
                    cost_multiplier: BURST_PREMIUM,
                };
                
                self.bursts.insert(request.request_id.clone(), allocation);
                self.update_burst_accounting(&request, &granted);
                
                return BurstResponse::PartiallyApproved {
                    request_id: request.request_id,
                    granted,
                    expires_at,
                    reason: "Partial resources available".to_string(),
                };
            }
            
            return BurstResponse::Denied {
                request_id: request.request_id,
                reason: "Insufficient lender capacity".to_string(),
                retry_after: Some(Duration::from_secs(30)),
            };
        }
        
        // Step 4d: Full grant
        let expires_at = Instant::now() + request.duration;
        
        let allocation = BurstAllocation {
            request_id: request.request_id.clone(),
            tenant_id: request.tenant_id.clone(),
            borrower_cluster: request.requesting_cluster.clone(),
            lender_cluster: request.target_cluster.clone(),
            resources: request.resources_needed.clone(),
            created_at: Instant::now(),
            expires_at,
            cost_multiplier: BURST_PREMIUM,
        };
        
        self.bursts.insert(request.request_id.clone(), allocation);
        self.update_burst_accounting(&request, &request.resources_needed);
        
        BurstResponse::Approved {
            request_id: request.request_id,
            granted: request.resources_needed,
            expires_at,
            cost_multiplier: BURST_PREMIUM,
        }
    }
    
    // =========================================================================
    // Step 5: Release Resources
    // =========================================================================
    
    /// Release resources after task completion
    pub fn release_resources(
        &mut self,
        tenant_id: &TenantId,
        cluster_id: &ClusterId,
        released: &Resources,
    ) {
        if let Some(allocation) = self.cluster_allocations.get_mut(
            &(tenant_id.clone(), cluster_id.clone())
        ) {
            allocation.used.subtract(released);
            allocation.last_updated = Instant::now();
        }
    }
    
    /// Release a burst allocation
    pub fn release_burst(&mut self, request_id: &str) {
        if let Some(burst) = self.bursts.remove(request_id) {
            // Update borrower
            if let Some(borrower) = self.cluster_allocations.get_mut(
                &(burst.tenant_id.clone(), burst.borrower_cluster.clone())
            ) {
                borrower.borrowed.subtract(&burst.resources);
                borrower.last_updated = Instant::now();
            }
            
            // Update lender
            if let Some(lender) = self.cluster_allocations.get_mut(
                &(burst.tenant_id.clone(), burst.lender_cluster.clone())
            ) {
                lender.lent.subtract(&burst.resources);
                lender.last_updated = Instant::now();
            }
        }
    }
    
    // =========================================================================
    // Step 6: Expire Bursts
    // =========================================================================
    
    /// Check for and release expired burst allocations
    pub fn expire_bursts(&mut self) -> Vec<BurstAllocation> {
        let now = Instant::now();
        let expired: Vec<String> = self.bursts
            .iter()
            .filter(|(_, b)| b.expires_at <= now)
            .map(|(id, _)| id.clone())
            .collect();
        
        let mut released = Vec::new();
        for id in expired {
            if let Some(burst) = self.bursts.remove(&id) {
                // Clean up accounting
                self.release_burst_internal(&burst);
                released.push(burst);
            }
        }
        
        released
    }
    
    fn release_burst_internal(&mut self, burst: &BurstAllocation) {
        if let Some(borrower) = self.cluster_allocations.get_mut(
            &(burst.tenant_id.clone(), burst.borrower_cluster.clone())
        ) {
            borrower.borrowed.subtract(&burst.resources);
        }
        
        if let Some(lender) = self.cluster_allocations.get_mut(
            &(burst.tenant_id.clone(), burst.lender_cluster.clone())
        ) {
            lender.lent.subtract(&burst.resources);
        }
    }
    
    // =========================================================================
    // Helper Functions
    // =========================================================================
    
    /// Calculate total allocated resources for a tenant across all clusters
    fn total_allocated_for_tenant(&self, tenant_id: &TenantId) -> Resources {
        let mut total = Resources::default();
        for ((tid, _), allocation) in &self.cluster_allocations {
            if tid == tenant_id {
                total.add(&allocation.allocated);
            }
        }
        total
    }
    
    /// Calculate total burst usage for a tenant
    fn total_burst_for_tenant(&self, tenant_id: &TenantId) -> Resources {
        let mut total = Resources::default();
        for burst in self.bursts.values() {
            if &burst.tenant_id == tenant_id {
                total.add(&burst.resources);
            }
        }
        total
    }
    
    /// Calculate guaranteed resources available
    fn guaranteed_available(
        &self,
        tenant_id: &TenantId,
        cluster_id: &ClusterId,
    ) -> Resources {
        let allocation = match self.cluster_allocations.get(
            &(tenant_id.clone(), cluster_id.clone())
        ) {
            Some(a) => a,
            None => return Resources::default(),
        };
        
        // Guaranteed is MIN_GUARANTEED_PERCENT of allocated
        let mut guaranteed = allocation.allocated.clone();
        guaranteed.cpu_cores *= MIN_GUARANTEED_PERCENT;
        guaranteed.memory_gb *= MIN_GUARANTEED_PERCENT;
        guaranteed.storage_gb *= MIN_GUARANTEED_PERCENT;
        guaranteed.network_gbps *= MIN_GUARANTEED_PERCENT;
        
        // Subtract used, but never below 0
        guaranteed.subtract(&allocation.used);
        guaranteed
    }
    
    /// Find a cluster that can lend resources
    fn find_lender(
        &self,
        tenant_id: &TenantId,
        borrower_cluster: &ClusterId,
        needed: &Resources,
    ) -> Option<ClusterId> {
        for ((tid, cid), allocation) in &self.cluster_allocations {
            if tid != tenant_id || cid == borrower_cluster {
                continue;
            }
            
            let available = self.available_to_lend(tid, cid);
            if needed.within(&available) {
                return Some(cid.clone());
            }
        }
        None
    }
    
    /// Calculate resources available to lend from a cluster
    fn available_to_lend(
        &self,
        tenant_id: &TenantId,
        cluster_id: &ClusterId,
    ) -> Resources {
        let allocation = match self.cluster_allocations.get(
            &(tenant_id.clone(), cluster_id.clone())
        ) {
            Some(a) => a,
            None => return Resources::default(),
        };
        
        let quota = match self.global_quotas.get(tenant_id) {
            Some(q) if q.lending_enabled => q,
            _ => return Resources::default(),
        };
        
        // Available = allocated - used - lent - guaranteed_reserve
        let mut available = allocation.allocated.clone();
        available.subtract(&allocation.used);
        available.subtract(&allocation.lent);
        
        // Keep guaranteed reserve
        let mut reserve = allocation.allocated.clone();
        reserve.cpu_cores *= MIN_GUARANTEED_PERCENT;
        reserve.memory_gb *= MIN_GUARANTEED_PERCENT;
        available.subtract(&reserve);
        
        available
    }
    
    /// Calculate partial grant when full request can't be met
    fn calculate_partial_grant(
        &self,
        requested: &Resources,
        available: &Resources,
    ) -> Resources {
        Resources {
            cpu_cores: requested.cpu_cores.min(available.cpu_cores),
            memory_gb: requested.memory_gb.min(available.memory_gb),
            storage_gb: requested.storage_gb.min(available.storage_gb),
            network_gbps: requested.network_gbps.min(available.network_gbps),
            gpu_units: requested.gpu_units.min(available.gpu_units),
        }
    }
    
    /// Update accounting after burst approval
    fn update_burst_accounting(&mut self, request: &BurstRequest, granted: &Resources) {
        // Update borrower
        if let Some(borrower) = self.cluster_allocations.get_mut(
            &(request.tenant_id.clone(), request.requesting_cluster.clone())
        ) {
            borrower.borrowed.add(granted);
            borrower.last_updated = Instant::now();
        }
        
        // Update lender
        if let Some(lender) = self.cluster_allocations.get_mut(
            &(request.tenant_id.clone(), request.target_cluster.clone())
        ) {
            lender.lent.add(granted);
            lender.last_updated = Instant::now();
        }
    }
    
    // =========================================================================
    // Metrics and Reporting
    // =========================================================================
    
    /// Get federation-wide utilization for a tenant
    pub fn tenant_utilization(&self, tenant_id: &TenantId) -> TenantUtilization {
        let quota = self.global_quotas.get(tenant_id);
        let total_limit = quota.map(|q| q.total_limit.clone()).unwrap_or_default();
        
        let mut total_used = Resources::default();
        let mut total_allocated = Resources::default();
        let mut per_cluster = HashMap::new();
        
        for ((tid, cid), allocation) in &self.cluster_allocations {
            if tid != tenant_id {
                continue;
            }
            
            total_used.add(&allocation.used);
            total_allocated.add(&allocation.allocated);
            
            per_cluster.insert(cid.clone(), ClusterUtilization {
                cluster_id: cid.clone(),
                allocated: allocation.allocated.clone(),
                used: allocation.used.clone(),
                borrowed: allocation.borrowed.clone(),
                lent: allocation.lent.clone(),
                utilization: allocation.utilization(),
            });
        }
        
        TenantUtilization {
            tenant_id: tenant_id.clone(),
            total_limit,
            total_allocated,
            total_used,
            per_cluster,
            global_utilization: total_used.utilization(&total_allocated),
        }
    }
}

// =============================================================================
// Decision Types
// =============================================================================

/// Decision for a resource request
#[derive(Debug, Clone)]
pub enum ResourceDecision {
    /// Request granted from available resources
    Granted {
        source: ResourceSource,
        cost_multiplier: f64,
    },
    /// Request requires burst from another cluster
    BurstRequired {
        lender_cluster: ClusterId,
        resources: Resources,
        cost_multiplier: f64,
    },
    /// Request queued until resources available
    Queued {
        reason: String,
        estimated_wait: Duration,
    },
    /// Request denied
    Denied {
        reason: String,
    },
}

/// Source of granted resources
#[derive(Debug, Clone)]
pub enum ResourceSource {
    /// From guaranteed allocation
    Guaranteed,
    /// From base allocation
    Allocated,
    /// From burst pool
    Burst,
}

// =============================================================================
// Utilization Reports
// =============================================================================

/// Federation-wide tenant utilization
#[derive(Debug, Clone)]
pub struct TenantUtilization {
    pub tenant_id: TenantId,
    pub total_limit: Resources,
    pub total_allocated: Resources,
    pub total_used: Resources,
    pub per_cluster: HashMap<ClusterId, ClusterUtilization>,
    pub global_utilization: f64,
}

/// Per-cluster utilization for a tenant
#[derive(Debug, Clone)]
pub struct ClusterUtilization {
    pub cluster_id: ClusterId,
    pub allocated: Resources,
    pub used: Resources,
    pub borrowed: Resources,
    pub lent: Resources,
    pub utilization: f64,
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_resource_within() {
        let used = Resources::new(10.0, 20.0, 100.0, 1.0, 0);
        let limit = Resources::new(100.0, 200.0, 1000.0, 10.0, 4);
        assert!(used.within(&limit));
        
        let exceeded = Resources::new(150.0, 20.0, 100.0, 1.0, 0);
        assert!(!exceeded.within(&limit));
    }
    
    #[test]
    fn test_basic_allocation() {
        let mut manager = ResourceSharingManager::new("cluster-1".to_string());
        
        // Register quota
        manager.register_quota(GlobalQuota {
            tenant_id: "tenant-a".to_string(),
            organization_id: "org-1".to_string(),
            total_limit: Resources::new(100.0, 200.0, 1000.0, 10.0, 4),
            guaranteed: Resources::new(50.0, 100.0, 500.0, 5.0, 2),
            burst_limit: Resources::new(20.0, 40.0, 200.0, 2.0, 0),
            priority: 1,
            burst_enabled: true,
            lending_enabled: true,
            cost_multiplier: 1.0,
        });
        
        // Allocate to cluster
        let result = manager.allocate_to_cluster(
            &"tenant-a".to_string(),
            &"cluster-1".to_string(),
            Resources::new(50.0, 100.0, 500.0, 5.0, 2),
        );
        assert!(result.is_ok());
        
        // Request within allocation
        let decision = manager.request_resources(
            &"tenant-a".to_string(),
            &"cluster-1".to_string(),
            &Resources::new(10.0, 20.0, 100.0, 1.0, 0),
        );
        
        match decision {
            ResourceDecision::Granted { source, .. } => {
                assert!(matches!(source, ResourceSource::Guaranteed | ResourceSource::Allocated));
            }
            _ => panic!("Expected granted decision"),
        }
    }
}
