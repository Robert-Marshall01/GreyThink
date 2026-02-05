//! # Grey Distributed — Resource Pricing Model
//!
//! Dynamic pricing for CPU and memory resources based on supply, demand,
//! and tenant tier guarantees.
//!
//! ## Pricing Formula
//!
//! The base price for resources follows a supply-demand curve:
//!
//! ```text
//! Price = BasePrice × DemandMultiplier × TierMultiplier × TimeMultiplier
//!
//! Where:
//!   DemandMultiplier = 1 + α × (Utilization - TargetUtilization)²
//!   TierMultiplier   = TierWeight[tier]
//!   TimeMultiplier   = 1 + β × PeakHourFactor
//! ```
//!
//! ## Economic Properties
//!
//! - **Efficiency**: Prices rise with scarcity, encouraging efficient use
//! - **Fairness**: Guaranteed quotas priced at base rate
//! - **Predictability**: Price caps prevent runaway costs
//! - **Sustainability**: Revenue covers infrastructure + margin

use std::collections::HashMap;
use std::time::{Duration, SystemTime};

// =============================================================================
// Types and Constants
// =============================================================================

pub type TenantId = String;
pub type ResourceUnits = f64;
pub type TokenAmount = f64;

/// Base price per CPU core per hour (in Grey tokens)
const BASE_CPU_PRICE: TokenAmount = 1.0;

/// Base price per GB RAM per hour (in Grey tokens)
const BASE_MEMORY_PRICE: TokenAmount = 0.25;

/// Target cluster utilization (70%)
const TARGET_UTILIZATION: f64 = 0.70;

/// Demand elasticity coefficient (α)
const DEMAND_ELASTICITY: f64 = 2.0;

/// Peak hour coefficient (β)
const PEAK_HOUR_COEFFICIENT: f64 = 0.5;

/// Maximum price multiplier (price cap)
const MAX_PRICE_MULTIPLIER: f64 = 5.0;

/// Minimum price multiplier (price floor)
const MIN_PRICE_MULTIPLIER: f64 = 0.5;

// =============================================================================
// Tenant Tiers
// =============================================================================

/// Tenant tier affects pricing and priority
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TenantTier {
    Enterprise,
    Business,
    Standard,
    Spot,
}

impl TenantTier {
    /// Get the price multiplier for this tier
    ///
    /// - Enterprise: Premium pricing for guaranteed resources
    /// - Business: Standard pricing
    /// - Standard: Slight discount, lower priority
    /// - Spot: Deep discount, preemptible
    pub fn price_multiplier(&self) -> f64 {
        match self {
            TenantTier::Enterprise => 1.5,
            TenantTier::Business => 1.0,
            TenantTier::Standard => 0.8,
            TenantTier::Spot => 0.3,
        }
    }
    
    /// Get preemption priority (higher = less likely to be preempted)
    pub fn priority(&self) -> u8 {
        match self {
            TenantTier::Enterprise => 100,
            TenantTier::Business => 75,
            TenantTier::Standard => 50,
            TenantTier::Spot => 10,
        }
    }
    
    /// Whether resources are guaranteed at base price
    pub fn has_guaranteed_quota(&self) -> bool {
        matches!(self, TenantTier::Enterprise | TenantTier::Business)
    }
}

// =============================================================================
// Resource Types
// =============================================================================

/// Types of compute resources
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResourceType {
    CpuCores,
    MemoryGb,
    GpuUnits,
    AcceleratorUnits,
}

impl ResourceType {
    /// Base price per unit per hour
    pub fn base_price(&self) -> TokenAmount {
        match self {
            ResourceType::CpuCores => BASE_CPU_PRICE,
            ResourceType::MemoryGb => BASE_MEMORY_PRICE,
            ResourceType::GpuUnits => 10.0,        // GPUs are expensive
            ResourceType::AcceleratorUnits => 5.0, // TPUs, FPGAs, etc.
        }
    }
    
    /// Human-readable unit name
    pub fn unit_name(&self) -> &'static str {
        match self {
            ResourceType::CpuCores => "core-hour",
            ResourceType::MemoryGb => "GB-hour",
            ResourceType::GpuUnits => "GPU-hour",
            ResourceType::AcceleratorUnits => "accelerator-hour",
        }
    }
}

// =============================================================================
// Cluster State
// =============================================================================

/// Current cluster resource state
#[derive(Debug, Clone)]
pub struct ClusterState {
    /// Total available resources
    pub total_capacity: HashMap<ResourceType, ResourceUnits>,
    
    /// Currently allocated resources
    pub allocated: HashMap<ResourceType, ResourceUnits>,
    
    /// Current time (for peak hour calculation)
    pub current_time: SystemTime,
    
    /// Cluster region (affects peak hours)
    pub region: String,
}

impl ClusterState {
    /// Calculate utilization for a resource type
    pub fn utilization(&self, resource_type: ResourceType) -> f64 {
        let total = self.total_capacity.get(&resource_type).copied().unwrap_or(1.0);
        let used = self.allocated.get(&resource_type).copied().unwrap_or(0.0);
        (used / total).min(1.0)
    }
    
    /// Check if currently in peak hours (business hours in region)
    pub fn is_peak_hours(&self) -> bool {
        // Simplified: assume peak hours are 9 AM - 6 PM local time
        // In production, would use actual timezone
        let hour = self.current_hour();
        (9..18).contains(&hour)
    }
    
    fn current_hour(&self) -> u32 {
        let secs = self.current_time
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();
        ((secs / 3600) % 24) as u32
    }
}

// =============================================================================
// Pricing Engine
// =============================================================================

/// Dynamic resource pricing engine
pub struct ResourcePricingEngine {
    /// Historical price data for smoothing
    price_history: HashMap<ResourceType, Vec<TokenAmount>>,
    
    /// Tenant quota allocations (guaranteed at base price)
    tenant_quotas: HashMap<TenantId, TenantQuota>,
    
    /// Price smoothing window size
    smoothing_window: usize,
    
    /// Enable surge pricing
    surge_pricing_enabled: bool,
}

/// Tenant's guaranteed quota
#[derive(Debug, Clone)]
pub struct TenantQuota {
    pub tier: TenantTier,
    pub guaranteed_cpu: ResourceUnits,
    pub guaranteed_memory: ResourceUnits,
    pub burst_multiplier: f64,
}

/// Price quote for a resource request
#[derive(Debug, Clone)]
pub struct PriceQuote {
    pub resource_type: ResourceType,
    pub quantity: ResourceUnits,
    pub duration_hours: f64,
    pub unit_price: TokenAmount,
    pub total_price: TokenAmount,
    pub price_breakdown: PriceBreakdown,
    pub valid_until: SystemTime,
    pub spot_available: bool,
}

/// Breakdown of price components
#[derive(Debug, Clone)]
pub struct PriceBreakdown {
    pub base_price: TokenAmount,
    pub demand_multiplier: f64,
    pub tier_multiplier: f64,
    pub time_multiplier: f64,
    pub final_multiplier: f64,
}

impl ResourcePricingEngine {
    pub fn new() -> Self {
        Self {
            price_history: HashMap::new(),
            tenant_quotas: HashMap::new(),
            smoothing_window: 10,
            surge_pricing_enabled: true,
        }
    }
    
    /// Register a tenant's quota allocation
    pub fn register_quota(&mut self, tenant_id: TenantId, quota: TenantQuota) {
        self.tenant_quotas.insert(tenant_id, quota);
    }
    
    // =========================================================================
    // Core Pricing Formula
    // =========================================================================
    
    /// Calculate the current price for a resource type
    ///
    /// ## Formula
    ///
    /// ```text
    /// Price = BasePrice × DemandMultiplier × TierMultiplier × TimeMultiplier
    /// ```
    ///
    /// Clamped to [MIN_PRICE_MULTIPLIER, MAX_PRICE_MULTIPLIER] × BasePrice
    pub fn calculate_price(
        &self,
        resource_type: ResourceType,
        tenant_id: &TenantId,
        quantity: ResourceUnits,
        cluster_state: &ClusterState,
    ) -> PriceQuote {
        let quota = self.tenant_quotas.get(tenant_id);
        let tier = quota.map(|q| q.tier).unwrap_or(TenantTier::Standard);
        
        // Step 1: Calculate demand multiplier
        // Higher utilization = higher prices
        let utilization = cluster_state.utilization(resource_type);
        let demand_multiplier = self.calculate_demand_multiplier(utilization);
        
        // Step 2: Apply tier multiplier
        let tier_multiplier = tier.price_multiplier();
        
        // Step 3: Apply time-of-day multiplier
        let time_multiplier = self.calculate_time_multiplier(cluster_state);
        
        // Step 4: Combine multipliers
        let raw_multiplier = demand_multiplier * tier_multiplier * time_multiplier;
        
        // Step 5: Apply price caps
        let final_multiplier = raw_multiplier.clamp(MIN_PRICE_MULTIPLIER, MAX_PRICE_MULTIPLIER);
        
        // Step 6: Check if within guaranteed quota (charged at base rate)
        let (effective_multiplier, in_quota) = self.check_quota_pricing(
            tenant_id,
            resource_type,
            quantity,
            final_multiplier,
        );
        
        let base_price = resource_type.base_price();
        let unit_price = base_price * effective_multiplier;
        let duration_hours = 1.0; // Default quote for 1 hour
        let total_price = unit_price * quantity * duration_hours;
        
        PriceQuote {
            resource_type,
            quantity,
            duration_hours,
            unit_price,
            total_price,
            price_breakdown: PriceBreakdown {
                base_price,
                demand_multiplier,
                tier_multiplier,
                time_multiplier,
                final_multiplier: effective_multiplier,
            },
            valid_until: SystemTime::now() + Duration::from_secs(300), // 5 min validity
            spot_available: utilization < 0.8 && !in_quota,
        }
    }
    
    /// Calculate demand multiplier based on utilization
    ///
    /// Uses quadratic relationship to create smooth price curve:
    /// - Below target: Discounted prices
    /// - At target: Base price
    /// - Above target: Premium prices (accelerating)
    fn calculate_demand_multiplier(&self, utilization: f64) -> f64 {
        if !self.surge_pricing_enabled {
            return 1.0;
        }
        
        let deviation = utilization - TARGET_UTILIZATION;
        let multiplier = 1.0 + DEMAND_ELASTICITY * deviation * deviation.abs();
        
        // Ensure minimum multiplier even at low utilization
        multiplier.max(MIN_PRICE_MULTIPLIER)
    }
    
    /// Calculate time-of-day multiplier
    fn calculate_time_multiplier(&self, cluster_state: &ClusterState) -> f64 {
        if cluster_state.is_peak_hours() {
            1.0 + PEAK_HOUR_COEFFICIENT
        } else {
            1.0
        }
    }
    
    /// Check if request is within guaranteed quota
    ///
    /// Returns (effective_multiplier, is_within_quota)
    fn check_quota_pricing(
        &self,
        tenant_id: &TenantId,
        resource_type: ResourceType,
        quantity: ResourceUnits,
        market_multiplier: f64,
    ) -> (f64, bool) {
        let quota = match self.tenant_quotas.get(tenant_id) {
            Some(q) => q,
            None => return (market_multiplier, false),
        };
        
        // Only Enterprise and Business have guaranteed quotas
        if !quota.tier.has_guaranteed_quota() {
            return (market_multiplier, false);
        }
        
        let guaranteed = match resource_type {
            ResourceType::CpuCores => quota.guaranteed_cpu,
            ResourceType::MemoryGb => quota.guaranteed_memory,
            _ => 0.0, // GPUs etc. not guaranteed
        };
        
        if quantity <= guaranteed {
            // Within quota: charge at base rate (multiplier = 1.0)
            (1.0, true)
        } else {
            // Partial: base rate for quota, market rate for excess
            // Simplified: use blended rate
            let quota_portion = guaranteed / quantity;
            let blended = quota_portion * 1.0 + (1.0 - quota_portion) * market_multiplier;
            (blended, false)
        }
    }
    
    // =========================================================================
    // Spot Pricing
    // =========================================================================
    
    /// Calculate spot price (deeply discounted, preemptible)
    ///
    /// Spot price is based on current demand with steeper discount:
    /// - High utilization: Spot unavailable or minimum discount
    /// - Low utilization: Deep discount to fill capacity
    pub fn calculate_spot_price(
        &self,
        resource_type: ResourceType,
        cluster_state: &ClusterState,
    ) -> Option<TokenAmount> {
        let utilization = cluster_state.utilization(resource_type);
        
        // No spot capacity above 85% utilization
        if utilization > 0.85 {
            return None;
        }
        
        // Spot discount increases as utilization decreases
        // At 0% utilization: 70% discount
        // At 85% utilization: 30% discount
        let discount = 0.30 + (0.85 - utilization) * (0.40 / 0.85);
        let spot_multiplier = 1.0 - discount;
        
        let base_price = resource_type.base_price();
        Some(base_price * spot_multiplier * TenantTier::Spot.price_multiplier())
    }
    
    // =========================================================================
    // Batch Pricing
    // =========================================================================
    
    /// Calculate price for a batch of resources
    pub fn calculate_batch_price(
        &self,
        requests: &[(ResourceType, ResourceUnits)],
        tenant_id: &TenantId,
        duration_hours: f64,
        cluster_state: &ClusterState,
    ) -> BatchPriceQuote {
        let mut total_price = 0.0;
        let mut line_items = Vec::new();
        
        for (resource_type, quantity) in requests {
            let quote = self.calculate_price(*resource_type, tenant_id, *quantity, cluster_state);
            let line_total = quote.unit_price * quantity * duration_hours;
            
            line_items.push(LineItem {
                resource_type: *resource_type,
                quantity: *quantity,
                unit_price: quote.unit_price,
                line_total,
            });
            
            total_price += line_total;
        }
        
        // Apply volume discount for large requests
        let volume_discount = self.calculate_volume_discount(total_price);
        let discounted_total = total_price * (1.0 - volume_discount);
        
        BatchPriceQuote {
            line_items,
            subtotal: total_price,
            volume_discount,
            total_price: discounted_total,
            duration_hours,
            valid_until: SystemTime::now() + Duration::from_secs(300),
        }
    }
    
    /// Calculate volume discount based on total spend
    fn calculate_volume_discount(&self, total: TokenAmount) -> f64 {
        // Tiered volume discounts
        match total {
            t if t >= 10000.0 => 0.15, // 15% off for very large
            t if t >= 1000.0 => 0.10,  // 10% off for large
            t if t >= 100.0 => 0.05,   // 5% off for medium
            _ => 0.0,
        }
    }
    
    // =========================================================================
    // Price Prediction
    // =========================================================================
    
    /// Predict future price based on historical trends
    pub fn predict_price(
        &self,
        resource_type: ResourceType,
        hours_ahead: u32,
    ) -> PricePrediction {
        let history = self.price_history.get(&resource_type);
        
        let (predicted, confidence) = match history {
            Some(prices) if prices.len() >= 3 => {
                // Simple moving average prediction
                let recent: f64 = prices.iter().rev().take(5).sum::<f64>() 
                    / prices.len().min(5) as f64;
                let trend = if prices.len() >= 2 {
                    prices.last().unwrap() - prices.get(prices.len() - 2).unwrap()
                } else {
                    0.0
                };
                
                let predicted = recent + (trend * hours_ahead as f64);
                let confidence = 0.8 - (hours_ahead as f64 * 0.05).min(0.5);
                
                (predicted.max(resource_type.base_price() * MIN_PRICE_MULTIPLIER), confidence)
            }
            _ => {
                // No history: use base price
                (resource_type.base_price(), 0.5)
            }
        };
        
        PricePrediction {
            resource_type,
            hours_ahead,
            predicted_price: predicted,
            confidence,
            range: (predicted * 0.8, predicted * 1.2),
        }
    }
    
    /// Record current price for history
    pub fn record_price(&mut self, resource_type: ResourceType, price: TokenAmount) {
        let history = self.price_history.entry(resource_type).or_default();
        history.push(price);
        
        // Keep only recent history
        if history.len() > 1000 {
            history.drain(0..500);
        }
    }
}

// =============================================================================
// Supporting Types
// =============================================================================

#[derive(Debug, Clone)]
pub struct LineItem {
    pub resource_type: ResourceType,
    pub quantity: ResourceUnits,
    pub unit_price: TokenAmount,
    pub line_total: TokenAmount,
}

#[derive(Debug, Clone)]
pub struct BatchPriceQuote {
    pub line_items: Vec<LineItem>,
    pub subtotal: TokenAmount,
    pub volume_discount: f64,
    pub total_price: TokenAmount,
    pub duration_hours: f64,
    pub valid_until: SystemTime,
}

#[derive(Debug, Clone)]
pub struct PricePrediction {
    pub resource_type: ResourceType,
    pub hours_ahead: u32,
    pub predicted_price: TokenAmount,
    pub confidence: f64,
    pub range: (TokenAmount, TokenAmount),
}

// =============================================================================
// Default Implementation
// =============================================================================

impl Default for ResourcePricingEngine {
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
    
    fn make_cluster_state(utilization: f64) -> ClusterState {
        let mut total = HashMap::new();
        let mut allocated = HashMap::new();
        
        total.insert(ResourceType::CpuCores, 1000.0);
        total.insert(ResourceType::MemoryGb, 4000.0);
        
        allocated.insert(ResourceType::CpuCores, 1000.0 * utilization);
        allocated.insert(ResourceType::MemoryGb, 4000.0 * utilization);
        
        ClusterState {
            total_capacity: total,
            allocated,
            current_time: SystemTime::now(),
            region: "us-east-1".to_string(),
        }
    }
    
    #[test]
    fn test_base_pricing() {
        let engine = ResourcePricingEngine::new();
        let state = make_cluster_state(0.5); // 50% utilization
        
        let quote = engine.calculate_price(
            ResourceType::CpuCores,
            &"tenant-1".to_string(),
            10.0,
            &state,
        );
        
        assert!(quote.unit_price > 0.0);
        assert!(quote.total_price == quote.unit_price * 10.0);
    }
    
    #[test]
    fn test_demand_increases_price() {
        let engine = ResourcePricingEngine::new();
        
        let low_demand = make_cluster_state(0.3);
        let high_demand = make_cluster_state(0.9);
        
        let quote_low = engine.calculate_price(
            ResourceType::CpuCores,
            &"tenant-1".to_string(),
            10.0,
            &low_demand,
        );
        
        let quote_high = engine.calculate_price(
            ResourceType::CpuCores,
            &"tenant-1".to_string(),
            10.0,
            &high_demand,
        );
        
        assert!(quote_high.unit_price > quote_low.unit_price);
    }
    
    #[test]
    fn test_tier_pricing() {
        let mut engine = ResourcePricingEngine::new();
        let state = make_cluster_state(0.7);
        
        engine.register_quota("enterprise".to_string(), TenantQuota {
            tier: TenantTier::Enterprise,
            guaranteed_cpu: 100.0,
            guaranteed_memory: 400.0,
            burst_multiplier: 2.0,
        });
        
        engine.register_quota("standard".to_string(), TenantQuota {
            tier: TenantTier::Standard,
            guaranteed_cpu: 10.0,
            guaranteed_memory: 40.0,
            burst_multiplier: 1.5,
        });
        
        let enterprise_quote = engine.calculate_price(
            ResourceType::CpuCores,
            &"enterprise".to_string(),
            50.0, // Within quota
            &state,
        );
        
        let standard_quote = engine.calculate_price(
            ResourceType::CpuCores,
            &"standard".to_string(),
            50.0, // Above quota
            &state,
        );
        
        // Enterprise within quota should get base rate
        assert!(enterprise_quote.price_breakdown.final_multiplier <= 1.0);
    }
    
    #[test]
    fn test_spot_pricing() {
        let engine = ResourcePricingEngine::new();
        
        let low_util = make_cluster_state(0.3);
        let high_util = make_cluster_state(0.9);
        
        let spot_low = engine.calculate_spot_price(ResourceType::CpuCores, &low_util);
        let spot_high = engine.calculate_spot_price(ResourceType::CpuCores, &high_util);
        
        assert!(spot_low.is_some());
        assert!(spot_high.is_none()); // No spot at 90% utilization
        
        // Spot should be significantly cheaper than base
        assert!(spot_low.unwrap() < BASE_CPU_PRICE);
    }
    
    #[test]
    fn test_volume_discount() {
        let engine = ResourcePricingEngine::new();
        let state = make_cluster_state(0.5);
        
        let small_batch = engine.calculate_batch_price(
            &[(ResourceType::CpuCores, 10.0)],
            &"tenant".to_string(),
            1.0,
            &state,
        );
        
        let large_batch = engine.calculate_batch_price(
            &[(ResourceType::CpuCores, 1000.0), (ResourceType::MemoryGb, 4000.0)],
            &"tenant".to_string(),
            1.0,
            &state,
        );
        
        assert_eq!(small_batch.volume_discount, 0.0);
        assert!(large_batch.volume_discount > 0.0);
    }
}
