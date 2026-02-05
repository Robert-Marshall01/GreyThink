//! # Grey Distributed — Network Pricing Model
//!
//! Bandwidth pricing with congestion-aware dynamic pricing and
//! cross-region/cross-cloud cost modeling.
//!
//! ## Pricing Formula
//!
//! Network pricing is more nuanced than compute:
//!
//! ```text
//! Price = BasePrice × PathMultiplier × CongestionMultiplier × TierMultiplier
//!
//! Where:
//!   PathMultiplier      = f(source_region, dest_region, cloud_provider)
//!   CongestionMultiplier = 1 + γ × max(0, Utilization - CongestionThreshold)²
//!   TierMultiplier       = TierWeight[tier]
//! ```
//!
//! ## Economic Properties
//!
//! - **Locality Incentive**: Intra-region traffic is cheapest
//! - **Congestion Pricing**: Rush hour pricing reduces peak load
//! - **Cross-Cloud Costs**: Reflects actual egress costs
//! - **Federation Fairness**: Partner traffic at reduced rates

use std::collections::HashMap;
use std::time::{Duration, SystemTime};

// =============================================================================
// Types and Constants
// =============================================================================

pub type ClusterId = String;
pub type TenantId = String;
pub type TokenAmount = f64;
pub type Bytes = u64;
pub type Gbps = f64;

/// Base price per GB of data transfer (in Grey tokens)
const BASE_PRICE_PER_GB: TokenAmount = 0.01;

/// Congestion threshold (utilization above this triggers surge)
const CONGESTION_THRESHOLD: f64 = 0.70;

/// Congestion elasticity coefficient (γ)
const CONGESTION_ELASTICITY: f64 = 3.0;

/// Maximum congestion multiplier
const MAX_CONGESTION_MULTIPLIER: f64 = 4.0;

/// Bytes per GB
const BYTES_PER_GB: u64 = 1_073_741_824;

// =============================================================================
// Network Path Types
// =============================================================================

/// Represents a network path between two endpoints
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NetworkPath {
    pub source_cluster: ClusterId,
    pub source_region: String,
    pub source_cloud: CloudProvider,
    pub dest_cluster: ClusterId,
    pub dest_region: String,
    pub dest_cloud: CloudProvider,
}

impl NetworkPath {
    /// Calculate the path multiplier based on topology
    ///
    /// ## Path Cost Hierarchy
    ///
    /// 1. Same cluster: Free (internal)
    /// 2. Same region, same cloud: 1x
    /// 3. Cross-region, same cloud: 2x
    /// 4. Cross-cloud, same region: 3x
    /// 5. Cross-cloud, cross-region: 5x
    /// 6. Cross-continent: 8x
    pub fn path_multiplier(&self) -> f64 {
        // Same cluster
        if self.source_cluster == self.dest_cluster {
            return 0.0; // Free
        }
        
        // Same cloud, same region
        if self.source_cloud == self.dest_cloud && self.source_region == self.dest_region {
            return 1.0;
        }
        
        // Same cloud, different region
        if self.source_cloud == self.dest_cloud {
            if self.same_continent() {
                return 2.0;
            } else {
                return 5.0; // Cross-continent
            }
        }
        
        // Different cloud
        if self.source_region == self.dest_region {
            return 3.0; // Cross-cloud, same region
        }
        
        if self.same_continent() {
            return 5.0; // Cross-cloud, cross-region
        }
        
        8.0 // Cross-cloud, cross-continent
    }
    
    /// Check if source and destination are on the same continent
    fn same_continent(&self) -> bool {
        let src_continent = region_to_continent(&self.source_region);
        let dst_continent = region_to_continent(&self.dest_region);
        src_continent == dst_continent
    }
    
    /// Get the path type for billing purposes
    pub fn path_type(&self) -> PathType {
        if self.source_cluster == self.dest_cluster {
            PathType::IntraCluster
        } else if self.source_cloud == self.dest_cloud && self.source_region == self.dest_region {
            PathType::IntraRegion
        } else if self.source_cloud == self.dest_cloud {
            PathType::CrossRegion
        } else {
            PathType::CrossCloud
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CloudProvider {
    Aws,
    Gcp,
    Azure,
    OnPrem,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PathType {
    IntraCluster,
    IntraRegion,
    CrossRegion,
    CrossCloud,
}

impl PathType {
    pub fn description(&self) -> &'static str {
        match self {
            PathType::IntraCluster => "Within cluster (free)",
            PathType::IntraRegion => "Same region",
            PathType::CrossRegion => "Cross region",
            PathType::CrossCloud => "Cross cloud",
        }
    }
}

// =============================================================================
// Network State
// =============================================================================

/// Current network state for a path
#[derive(Debug, Clone)]
pub struct NetworkState {
    /// Path this state represents
    pub path: NetworkPath,
    
    /// Current bandwidth utilization (0.0 - 1.0)
    pub utilization: f64,
    
    /// Maximum bandwidth capacity
    pub max_bandwidth_gbps: Gbps,
    
    /// Current latency (for quality-based pricing)
    pub latency_ms: f64,
    
    /// Packet loss rate
    pub packet_loss_rate: f64,
    
    /// Is this a federation partner path?
    pub is_federation_path: bool,
}

// =============================================================================
// Network Pricing Engine
// =============================================================================

/// Dynamic network pricing engine
pub struct NetworkPricingEngine {
    /// Path state cache
    path_states: HashMap<NetworkPath, NetworkState>,
    
    /// Tenant tiers
    tenant_tiers: HashMap<TenantId, TenantTier>,
    
    /// Federation partners (get discounted rates)
    federation_partners: HashMap<ClusterId, FederationPartner>,
    
    /// Historical congestion data
    congestion_history: Vec<(SystemTime, f64)>,
    
    /// Enable congestion pricing
    congestion_pricing_enabled: bool,
}

#[derive(Debug, Clone, Copy)]
pub enum TenantTier {
    Enterprise,
    Business,
    Standard,
}

impl TenantTier {
    fn network_discount(&self) -> f64 {
        match self {
            TenantTier::Enterprise => 0.20, // 20% discount
            TenantTier::Business => 0.10,   // 10% discount
            TenantTier::Standard => 0.0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FederationPartner {
    pub cluster_id: ClusterId,
    pub discount_rate: f64,
    pub monthly_quota_gb: u64,
    pub used_quota_gb: u64,
}

/// Price quote for network transfer
#[derive(Debug, Clone)]
pub struct NetworkPriceQuote {
    pub path: NetworkPath,
    pub path_type: PathType,
    pub data_size_gb: f64,
    pub unit_price_per_gb: TokenAmount,
    pub total_price: TokenAmount,
    pub breakdown: NetworkPriceBreakdown,
    pub congestion_warning: Option<String>,
    pub valid_until: SystemTime,
}

#[derive(Debug, Clone)]
pub struct NetworkPriceBreakdown {
    pub base_price: TokenAmount,
    pub path_multiplier: f64,
    pub congestion_multiplier: f64,
    pub tier_discount: f64,
    pub federation_discount: f64,
    pub final_price_per_gb: TokenAmount,
}

impl NetworkPricingEngine {
    pub fn new() -> Self {
        Self {
            path_states: HashMap::new(),
            tenant_tiers: HashMap::new(),
            federation_partners: HashMap::new(),
            congestion_history: Vec::new(),
            congestion_pricing_enabled: true,
        }
    }
    
    /// Register tenant tier
    pub fn register_tenant(&mut self, tenant_id: TenantId, tier: TenantTier) {
        self.tenant_tiers.insert(tenant_id, tier);
    }
    
    /// Register federation partner
    pub fn register_federation_partner(&mut self, partner: FederationPartner) {
        self.federation_partners.insert(partner.cluster_id.clone(), partner);
    }
    
    /// Update network state for a path
    pub fn update_path_state(&mut self, state: NetworkState) {
        self.path_states.insert(state.path.clone(), state);
    }
    
    // =========================================================================
    // Core Pricing
    // =========================================================================
    
    /// Calculate price for data transfer
    ///
    /// ## Formula
    ///
    /// ```text
    /// Price = BasePrice × PathMultiplier × CongestionMultiplier × (1 - Discounts)
    /// ```
    pub fn calculate_price(
        &self,
        path: &NetworkPath,
        data_size_bytes: Bytes,
        tenant_id: &TenantId,
    ) -> NetworkPriceQuote {
        let data_size_gb = data_size_bytes as f64 / BYTES_PER_GB as f64;
        
        // Step 1: Get path multiplier
        let path_multiplier = path.path_multiplier();
        
        // Step 2: Calculate congestion multiplier
        let congestion_multiplier = self.calculate_congestion_multiplier(path);
        
        // Step 3: Calculate discounts
        let tier = self.tenant_tiers.get(tenant_id).copied().unwrap_or(TenantTier::Standard);
        let tier_discount = tier.network_discount();
        
        let federation_discount = self.get_federation_discount(&path.dest_cluster);
        
        // Step 4: Calculate final price
        let base_price = BASE_PRICE_PER_GB;
        let gross_price = base_price * path_multiplier * congestion_multiplier;
        let total_discount = 1.0 - ((1.0 - tier_discount) * (1.0 - federation_discount));
        let unit_price = gross_price * (1.0 - total_discount);
        let total_price = unit_price * data_size_gb;
        
        // Step 5: Generate congestion warning if needed
        let congestion_warning = self.get_congestion_warning(path);
        
        NetworkPriceQuote {
            path: path.clone(),
            path_type: path.path_type(),
            data_size_gb,
            unit_price_per_gb: unit_price,
            total_price,
            breakdown: NetworkPriceBreakdown {
                base_price,
                path_multiplier,
                congestion_multiplier,
                tier_discount,
                federation_discount,
                final_price_per_gb: unit_price,
            },
            congestion_warning,
            valid_until: SystemTime::now() + Duration::from_secs(60),
        }
    }
    
    /// Calculate congestion multiplier
    ///
    /// Uses quadratic function above threshold:
    /// ```text
    /// Multiplier = 1 + γ × max(0, Utilization - Threshold)²
    /// ```
    fn calculate_congestion_multiplier(&self, path: &NetworkPath) -> f64 {
        if !self.congestion_pricing_enabled {
            return 1.0;
        }
        
        let state = match self.path_states.get(path) {
            Some(s) => s,
            None => return 1.0, // No congestion data
        };
        
        if state.utilization <= CONGESTION_THRESHOLD {
            return 1.0;
        }
        
        let excess = state.utilization - CONGESTION_THRESHOLD;
        let multiplier = 1.0 + CONGESTION_ELASTICITY * excess * excess;
        
        multiplier.min(MAX_CONGESTION_MULTIPLIER)
    }
    
    /// Get federation discount for destination cluster
    fn get_federation_discount(&self, dest_cluster: &ClusterId) -> f64 {
        match self.federation_partners.get(dest_cluster) {
            Some(partner) => {
                // Full discount if within quota
                if partner.used_quota_gb < partner.monthly_quota_gb {
                    partner.discount_rate
                } else {
                    partner.discount_rate / 2.0 // Half discount over quota
                }
            }
            None => 0.0,
        }
    }
    
    /// Generate congestion warning message
    fn get_congestion_warning(&self, path: &NetworkPath) -> Option<String> {
        let state = self.path_states.get(path)?;
        
        if state.utilization > 0.90 {
            Some("Critical: Network path heavily congested (>90%). Consider delaying transfer.".to_string())
        } else if state.utilization > 0.80 {
            Some("Warning: Network path congested (>80%). Prices elevated.".to_string())
        } else {
            None
        }
    }
    
    // =========================================================================
    // Bandwidth Reservation
    // =========================================================================
    
    /// Calculate price for reserved bandwidth
    ///
    /// Reserved bandwidth is priced at a premium but guarantees capacity.
    pub fn calculate_reservation_price(
        &self,
        path: &NetworkPath,
        bandwidth_gbps: Gbps,
        duration_hours: f64,
        tenant_id: &TenantId,
    ) -> BandwidthReservationQuote {
        // Reservation price: 3x typical transfer, but guaranteed
        let base_hourly_rate = BASE_PRICE_PER_GB * 100.0; // Per Gbps-hour
        let path_multiplier = path.path_multiplier();
        
        let tier = self.tenant_tiers.get(tenant_id).copied().unwrap_or(TenantTier::Standard);
        let discount = tier.network_discount();
        
        let hourly_rate = base_hourly_rate * path_multiplier * bandwidth_gbps * (1.0 - discount);
        let total_price = hourly_rate * duration_hours;
        
        // Check if reservation is possible
        let available = self.check_reservation_availability(path, bandwidth_gbps);
        
        BandwidthReservationQuote {
            path: path.clone(),
            bandwidth_gbps,
            duration_hours,
            hourly_rate,
            total_price,
            available,
            guaranteed_latency_ms: self.get_guaranteed_latency(path),
        }
    }
    
    fn check_reservation_availability(&self, path: &NetworkPath, requested: Gbps) -> bool {
        match self.path_states.get(path) {
            Some(state) => {
                let available_gbps = state.max_bandwidth_gbps * (1.0 - state.utilization);
                available_gbps >= requested
            }
            None => true, // Unknown path, assume available
        }
    }
    
    fn get_guaranteed_latency(&self, path: &NetworkPath) -> Option<f64> {
        // Reserved bandwidth gets guaranteed latency based on path type
        match path.path_type() {
            PathType::IntraCluster => Some(0.5),
            PathType::IntraRegion => Some(2.0),
            PathType::CrossRegion => Some(50.0),
            PathType::CrossCloud => Some(100.0),
        }
    }
    
    // =========================================================================
    // Usage Tracking
    // =========================================================================
    
    /// Record network usage for billing
    pub fn record_usage(
        &mut self,
        path: &NetworkPath,
        bytes_transferred: Bytes,
        tenant_id: &TenantId,
    ) -> UsageRecord {
        let quote = self.calculate_price(path, bytes_transferred, tenant_id);
        
        // Update federation partner quota if applicable
        if let Some(partner) = self.federation_partners.get_mut(&path.dest_cluster) {
            let gb = bytes_transferred / BYTES_PER_GB;
            partner.used_quota_gb += gb;
        }
        
        UsageRecord {
            timestamp: SystemTime::now(),
            path: path.clone(),
            tenant_id: tenant_id.clone(),
            bytes_transferred,
            price_charged: quote.total_price,
            path_type: quote.path_type,
        }
    }
}

// =============================================================================
// Supporting Types
// =============================================================================

#[derive(Debug, Clone)]
pub struct BandwidthReservationQuote {
    pub path: NetworkPath,
    pub bandwidth_gbps: Gbps,
    pub duration_hours: f64,
    pub hourly_rate: TokenAmount,
    pub total_price: TokenAmount,
    pub available: bool,
    pub guaranteed_latency_ms: Option<f64>,
}

#[derive(Debug, Clone)]
pub struct UsageRecord {
    pub timestamp: SystemTime,
    pub path: NetworkPath,
    pub tenant_id: TenantId,
    pub bytes_transferred: Bytes,
    pub price_charged: TokenAmount,
    pub path_type: PathType,
}

// =============================================================================
// Helpers
// =============================================================================

fn region_to_continent(region: &str) -> &'static str {
    if region.starts_with("us-") || region.starts_with("northamerica") || region.starts_with("southamerica") {
        "americas"
    } else if region.starts_with("eu-") || region.starts_with("europe") {
        "europe"
    } else if region.starts_with("ap-") || region.starts_with("asia") {
        "asia-pacific"
    } else {
        "unknown"
    }
}

impl Default for NetworkPricingEngine {
    fn default() -> Self {
        Self::new()
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    fn make_path(same_region: bool, same_cloud: bool) -> NetworkPath {
        NetworkPath {
            source_cluster: "cluster-a".to_string(),
            source_region: "us-east-1".to_string(),
            source_cloud: CloudProvider::Aws,
            dest_cluster: "cluster-b".to_string(),
            dest_region: if same_region { "us-east-1".to_string() } else { "eu-west-1".to_string() },
            dest_cloud: if same_cloud { CloudProvider::Aws } else { CloudProvider::Gcp },
        }
    }
    
    #[test]
    fn test_path_multiplier() {
        let intra_region = make_path(true, true);
        let cross_region = make_path(false, true);
        let cross_cloud = make_path(false, false);
        
        assert!(intra_region.path_multiplier() < cross_region.path_multiplier());
        assert!(cross_region.path_multiplier() < cross_cloud.path_multiplier());
    }
    
    #[test]
    fn test_congestion_pricing() {
        let mut engine = NetworkPricingEngine::new();
        let path = make_path(true, true);
        
        // Low congestion
        engine.update_path_state(NetworkState {
            path: path.clone(),
            utilization: 0.5,
            max_bandwidth_gbps: 100.0,
            latency_ms: 1.0,
            packet_loss_rate: 0.0,
            is_federation_path: false,
        });
        
        let quote_low = engine.calculate_price(&path, BYTES_PER_GB, &"tenant".to_string());
        
        // High congestion
        engine.update_path_state(NetworkState {
            path: path.clone(),
            utilization: 0.9,
            max_bandwidth_gbps: 100.0,
            latency_ms: 10.0,
            packet_loss_rate: 0.01,
            is_federation_path: false,
        });
        
        let quote_high = engine.calculate_price(&path, BYTES_PER_GB, &"tenant".to_string());
        
        assert!(quote_high.unit_price_per_gb > quote_low.unit_price_per_gb);
        assert!(quote_high.congestion_warning.is_some());
    }
    
    #[test]
    fn test_federation_discount() {
        let mut engine = NetworkPricingEngine::new();
        let path = make_path(true, true);
        
        engine.register_federation_partner(FederationPartner {
            cluster_id: "cluster-b".to_string(),
            discount_rate: 0.30, // 30% discount
            monthly_quota_gb: 1000,
            used_quota_gb: 0,
        });
        
        let with_discount = engine.calculate_price(&path, BYTES_PER_GB, &"tenant".to_string());
        
        let engine_no_discount = NetworkPricingEngine::new();
        let without_discount = engine_no_discount.calculate_price(&path, BYTES_PER_GB, &"tenant".to_string());
        
        assert!(with_discount.total_price < without_discount.total_price);
    }
}
