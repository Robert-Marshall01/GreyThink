//! # Grey Distributed — Storage Pricing Model
//!
//! Pricing for distributed storage including sharding, replication,
//! and tiered storage cost models.
//!
//! ## Pricing Formula
//!
//! Storage pricing combines multiple factors:
//!
//! ```text
//! Price = BasePrice × SizeMultiplier × DurabilityMultiplier × TierMultiplier × AccessMultiplier
//!
//! Where:
//!   SizeMultiplier      = f(total_size, block_count)
//!   DurabilityMultiplier = f(replication_factor, erasure_coding)
//!   TierMultiplier       = TierWeight[storage_class]
//!   AccessMultiplier     = f(iops, throughput)
//! ```
//!
//! ## Economic Properties
//!
//! - **Durability Premium**: Higher replication = higher cost
//! - **Hot/Cold Tiers**: Frequently accessed data costs more
//! - **Efficient Packing**: Volume discounts for large datasets
//! - **Access Charges**: IOPS and throughput priced separately

use std::collections::HashMap;
use std::time::{Duration, SystemTime};

// =============================================================================
// Types and Constants
// =============================================================================

pub type TenantId = String;
pub type TokenAmount = f64;
pub type Bytes = u64;

/// Base price per GB-month for standard storage
const BASE_PRICE_PER_GB_MONTH: TokenAmount = 0.05;

/// Price per 1000 read operations
const READ_OP_PRICE_PER_1K: TokenAmount = 0.001;

/// Price per 1000 write operations
const WRITE_OP_PRICE_PER_1K: TokenAmount = 0.005;

/// Price per GB egress
const EGRESS_PRICE_PER_GB: TokenAmount = 0.01;

/// Bytes per GB
const BYTES_PER_GB: u64 = 1_073_741_824;

/// Hours per month (for hourly billing)
const HOURS_PER_MONTH: f64 = 730.0;

// =============================================================================
// Storage Classes
// =============================================================================

/// Storage class determines performance and durability characteristics
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StorageClass {
    /// High-performance SSD, replicated across zones
    Hot,
    /// Balanced SSD/HDD mix
    Warm,
    /// Infrequently accessed, HDD-based
    Cold,
    /// Archive tier, very infrequent access
    Archive,
    /// In-memory cache layer
    Cache,
}

impl StorageClass {
    /// Price multiplier relative to standard (Warm)
    pub fn price_multiplier(&self) -> f64 {
        match self {
            StorageClass::Hot => 2.5,     // Premium SSD
            StorageClass::Warm => 1.0,    // Standard
            StorageClass::Cold => 0.4,    // Cheap HDD
            StorageClass::Archive => 0.1, // Glacier-like
            StorageClass::Cache => 5.0,   // In-memory premium
        }
    }
    
    /// Minimum retrieval time (affects access pricing)
    pub fn retrieval_latency(&self) -> Duration {
        match self {
            StorageClass::Hot => Duration::from_millis(1),
            StorageClass::Warm => Duration::from_millis(10),
            StorageClass::Cold => Duration::from_secs(1),
            StorageClass::Archive => Duration::from_secs(3600), // Hours
            StorageClass::Cache => Duration::from_micros(100),
        }
    }
    
    /// Default durability (nines)
    pub fn default_durability_nines(&self) -> u8 {
        match self {
            StorageClass::Hot => 11,     // 99.999999999%
            StorageClass::Warm => 11,
            StorageClass::Cold => 9,     // 99.9999999%
            StorageClass::Archive => 11,
            StorageClass::Cache => 3,    // Low durability (ephemeral)
        }
    }
}

// =============================================================================
// Replication & Durability
// =============================================================================

/// Replication configuration
#[derive(Debug, Clone)]
pub struct ReplicationConfig {
    /// Number of full copies
    pub replication_factor: u8,
    
    /// Erasure coding configuration (if used)
    pub erasure_coding: Option<ErasureCoding>,
    
    /// Geographic distribution
    pub geo_distribution: GeoDistribution,
}

#[derive(Debug, Clone)]
pub struct ErasureCoding {
    /// Data shards
    pub data_shards: u8,
    /// Parity shards
    pub parity_shards: u8,
}

impl ErasureCoding {
    /// Storage overhead as multiplier
    pub fn overhead(&self) -> f64 {
        (self.data_shards + self.parity_shards) as f64 / self.data_shards as f64
    }
}

#[derive(Debug, Clone, Copy)]
pub enum GeoDistribution {
    /// All replicas in same zone
    SingleZone,
    /// Replicas across zones in same region
    MultiZone,
    /// Replicas across regions
    MultiRegion,
    /// Replicas across continents
    Global,
}

impl GeoDistribution {
    pub fn price_multiplier(&self) -> f64 {
        match self {
            GeoDistribution::SingleZone => 1.0,
            GeoDistribution::MultiZone => 1.2,
            GeoDistribution::MultiRegion => 2.0,
            GeoDistribution::Global => 3.0,
        }
    }
}

impl ReplicationConfig {
    /// Calculate storage overhead multiplier
    pub fn storage_overhead(&self) -> f64 {
        let base = if let Some(ec) = &self.erasure_coding {
            ec.overhead()
        } else {
            self.replication_factor as f64
        };
        
        base * self.geo_distribution.price_multiplier()
    }
    
    /// Calculate durability nines
    pub fn durability_nines(&self) -> u8 {
        let base_nines = match self.replication_factor {
            1 => 4,
            2 => 7,
            3 => 9,
            _ => 11,
        };
        
        // Erasure coding improves durability
        let ec_bonus = self.erasure_coding.as_ref().map(|_| 2).unwrap_or(0);
        
        // Geo distribution improves durability
        let geo_bonus = match self.geo_distribution {
            GeoDistribution::SingleZone => 0,
            GeoDistribution::MultiZone => 1,
            GeoDistribution::MultiRegion => 2,
            GeoDistribution::Global => 3,
        };
        
        (base_nines + ec_bonus + geo_bonus).min(15)
    }
}

// =============================================================================
// Storage Pricing Engine
// =============================================================================

/// Primary storage pricing engine
pub struct StoragePricingEngine {
    /// Tenant storage allocations
    tenant_allocations: HashMap<TenantId, TenantStorageAllocation>,
    
    /// Volume discount tiers
    volume_tiers: Vec<VolumeTier>,
    
    /// Current storage utilization by class
    class_utilization: HashMap<StorageClass, f64>,
    
    /// Enable lifecycle pricing (auto-tiering discounts)
    lifecycle_pricing_enabled: bool,
}

#[derive(Debug, Clone)]
pub struct TenantStorageAllocation {
    pub tenant_id: TenantId,
    pub tier: TenantTier,
    pub committed_capacity_gb: u64,
    pub current_usage_gb: u64,
    pub iops_quota: u64,
    pub throughput_mbps: u64,
}

#[derive(Debug, Clone, Copy)]
pub enum TenantTier {
    Enterprise,
    Business,
    Standard,
}

impl TenantTier {
    fn discount(&self) -> f64 {
        match self {
            TenantTier::Enterprise => 0.25,
            TenantTier::Business => 0.10,
            TenantTier::Standard => 0.0,
        }
    }
}

#[derive(Debug, Clone)]
struct VolumeTier {
    min_gb: u64,
    discount: f64,
}

impl StoragePricingEngine {
    pub fn new() -> Self {
        Self {
            tenant_allocations: HashMap::new(),
            volume_tiers: vec![
                VolumeTier { min_gb: 0, discount: 0.0 },
                VolumeTier { min_gb: 1000, discount: 0.05 },      // 5% off at 1TB
                VolumeTier { min_gb: 10000, discount: 0.10 },     // 10% off at 10TB
                VolumeTier { min_gb: 100000, discount: 0.20 },    // 20% off at 100TB
                VolumeTier { min_gb: 1000000, discount: 0.30 },   // 30% off at 1PB
            ],
            class_utilization: HashMap::new(),
            lifecycle_pricing_enabled: true,
        }
    }
    
    /// Register tenant allocation
    pub fn register_tenant(&mut self, allocation: TenantStorageAllocation) {
        self.tenant_allocations.insert(allocation.tenant_id.clone(), allocation);
    }
    
    // =========================================================================
    // Storage Pricing
    // =========================================================================
    
    /// Calculate monthly storage cost
    ///
    /// ## Formula
    ///
    /// ```text
    /// Monthly Cost = 
    ///   (Size_GB × BasePrice × ClassMultiplier × ReplicationOverhead)
    ///   × (1 - VolumeDiscount)
    ///   × (1 - TenantDiscount)
    /// ```
    pub fn calculate_storage_price(
        &self,
        tenant_id: &TenantId,
        size_bytes: Bytes,
        storage_class: StorageClass,
        replication: &ReplicationConfig,
    ) -> StoragePriceQuote {
        let size_gb = size_bytes as f64 / BYTES_PER_GB as f64;
        
        // Step 1: Base price with class multiplier
        let base_price = BASE_PRICE_PER_GB_MONTH;
        let class_multiplier = storage_class.price_multiplier();
        
        // Step 2: Apply replication overhead
        let replication_overhead = replication.storage_overhead();
        let raw_monthly = size_gb * base_price * class_multiplier * replication_overhead;
        
        // Step 3: Apply volume discount
        let volume_discount = self.get_volume_discount(size_gb);
        
        // Step 4: Apply tenant discount
        let tenant_discount = self.get_tenant_discount(tenant_id);
        
        // Step 5: Calculate final price
        let gross_monthly = raw_monthly;
        let after_volume = gross_monthly * (1.0 - volume_discount);
        let monthly_price = after_volume * (1.0 - tenant_discount);
        
        // Hourly rate for incremental billing
        let hourly_price = monthly_price / HOURS_PER_MONTH;
        
        StoragePriceQuote {
            size_gb,
            storage_class,
            replication: replication.clone(),
            monthly_price,
            hourly_price,
            breakdown: StoragePriceBreakdown {
                base_price_gb_month: base_price,
                class_multiplier,
                replication_overhead,
                volume_discount,
                tenant_discount,
                effective_price_gb_month: monthly_price / size_gb,
            },
            durability_nines: replication.durability_nines(),
            effective_capacity_gb: size_gb * replication_overhead,
        }
    }
    
    fn get_volume_discount(&self, size_gb: f64) -> f64 {
        let size_u64 = size_gb as u64;
        self.volume_tiers
            .iter()
            .rev()
            .find(|t| size_u64 >= t.min_gb)
            .map(|t| t.discount)
            .unwrap_or(0.0)
    }
    
    fn get_tenant_discount(&self, tenant_id: &TenantId) -> f64 {
        self.tenant_allocations
            .get(tenant_id)
            .map(|a| a.tier.discount())
            .unwrap_or(0.0)
    }
    
    // =========================================================================
    // Operation Pricing
    // =========================================================================
    
    /// Calculate cost for storage operations
    ///
    /// ## Operations Priced
    ///
    /// - READ: Per 1000 operations
    /// - WRITE: Per 1000 operations (5x read cost)
    /// - LIST: Per 1000 operations
    /// - DELETE: Free
    pub fn calculate_operation_price(
        &self,
        tenant_id: &TenantId,
        operation: StorageOperation,
        count: u64,
        storage_class: StorageClass,
    ) -> OperationPriceQuote {
        let base_price = operation.base_price();
        let class_multiplier = self.operation_class_multiplier(operation, storage_class);
        
        // Archive has retrieval fees
        let retrieval_fee = if storage_class == StorageClass::Archive && operation == StorageOperation::Read {
            0.01 // Extra per-request fee
        } else {
            0.0
        };
        
        let price_per_1k = (base_price * class_multiplier) + retrieval_fee;
        let total_price = price_per_1k * (count as f64 / 1000.0);
        
        OperationPriceQuote {
            operation,
            count,
            storage_class,
            price_per_1k,
            total_price,
            includes_retrieval_fee: retrieval_fee > 0.0,
        }
    }
    
    fn operation_class_multiplier(&self, op: StorageOperation, class: StorageClass) -> f64 {
        match (op, class) {
            (_, StorageClass::Hot) => 0.5,     // Fast ops are cheaper
            (_, StorageClass::Cache) => 0.1,   // Very cheap ops
            (StorageOperation::Read, StorageClass::Archive) => 10.0, // Expensive retrieval
            (_, StorageClass::Archive) => 1.0,
            _ => 1.0,
        }
    }
    
    // =========================================================================
    // Egress Pricing
    // =========================================================================
    
    /// Calculate egress cost
    pub fn calculate_egress_price(
        &self,
        tenant_id: &TenantId,
        bytes: Bytes,
        destination: EgressDestination,
    ) -> EgressPriceQuote {
        let size_gb = bytes as f64 / BYTES_PER_GB as f64;
        
        let price_per_gb = EGRESS_PRICE_PER_GB * destination.multiplier();
        let total_price = size_gb * price_per_gb;
        
        // Check if tenant has free egress quota
        let free_quota = self.get_free_egress_quota(tenant_id);
        let billable_gb = (size_gb - free_quota).max(0.0);
        let billable_price = billable_gb * price_per_gb;
        
        EgressPriceQuote {
            size_gb,
            destination,
            price_per_gb,
            total_price,
            free_quota_used: (size_gb - billable_gb).max(0.0),
            billable_price,
        }
    }
    
    fn get_free_egress_quota(&self, tenant_id: &TenantId) -> f64 {
        self.tenant_allocations
            .get(tenant_id)
            .map(|a| match a.tier {
                TenantTier::Enterprise => 100.0, // 100 GB free
                TenantTier::Business => 10.0,    // 10 GB free
                TenantTier::Standard => 1.0,     // 1 GB free
            })
            .unwrap_or(0.0)
    }
    
    // =========================================================================
    // Lifecycle Pricing
    // =========================================================================
    
    /// Calculate cost savings from lifecycle policy
    ///
    /// Auto-tiering data from Hot → Warm → Cold saves money.
    pub fn calculate_lifecycle_savings(
        &self,
        tenant_id: &TenantId,
        size_bytes: Bytes,
        current_class: StorageClass,
        target_class: StorageClass,
        access_frequency_per_month: f64,
    ) -> LifecycleSavingsEstimate {
        let size_gb = size_bytes as f64 / BYTES_PER_GB as f64;
        
        let current_replication = ReplicationConfig {
            replication_factor: 3,
            erasure_coding: None,
            geo_distribution: GeoDistribution::MultiZone,
        };
        
        // Current monthly cost
        let current_quote = self.calculate_storage_price(
            tenant_id,
            size_bytes,
            current_class,
            &current_replication,
        );
        
        // Target monthly cost (plus access costs)
        let target_quote = self.calculate_storage_price(
            tenant_id,
            size_bytes,
            target_class,
            &current_replication,
        );
        
        // Access cost difference
        let current_access_cost = self.calculate_operation_price(
            tenant_id,
            StorageOperation::Read,
            (access_frequency_per_month * 1000.0) as u64,
            current_class,
        ).total_price;
        
        let target_access_cost = self.calculate_operation_price(
            tenant_id,
            StorageOperation::Read,
            (access_frequency_per_month * 1000.0) as u64,
            target_class,
        ).total_price;
        
        let current_total = current_quote.monthly_price + current_access_cost;
        let target_total = target_quote.monthly_price + target_access_cost;
        
        let monthly_savings = current_total - target_total;
        let is_beneficial = monthly_savings > 0.0;
        
        LifecycleSavingsEstimate {
            current_class,
            target_class,
            current_monthly_cost: current_total,
            target_monthly_cost: target_total,
            monthly_savings,
            annual_savings: monthly_savings * 12.0,
            is_beneficial,
            recommendation: if is_beneficial {
                format!("Migrate to {:?} to save ${:.2}/month", target_class, monthly_savings)
            } else {
                format!("Keep in {:?} - migration would increase costs", current_class)
            },
        }
    }
}

// =============================================================================
// Supporting Types
// =============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StorageOperation {
    Read,
    Write,
    List,
    Delete,
}

impl StorageOperation {
    fn base_price(&self) -> TokenAmount {
        match self {
            StorageOperation::Read => READ_OP_PRICE_PER_1K,
            StorageOperation::Write => WRITE_OP_PRICE_PER_1K,
            StorageOperation::List => READ_OP_PRICE_PER_1K,
            StorageOperation::Delete => 0.0, // Free
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum EgressDestination {
    SameRegion,
    CrossRegion,
    Internet,
    FederationPartner,
}

impl EgressDestination {
    fn multiplier(&self) -> f64 {
        match self {
            EgressDestination::SameRegion => 0.0, // Free
            EgressDestination::CrossRegion => 1.0,
            EgressDestination::Internet => 2.0,
            EgressDestination::FederationPartner => 0.5, // Discounted
        }
    }
}

#[derive(Debug, Clone)]
pub struct StoragePriceQuote {
    pub size_gb: f64,
    pub storage_class: StorageClass,
    pub replication: ReplicationConfig,
    pub monthly_price: TokenAmount,
    pub hourly_price: TokenAmount,
    pub breakdown: StoragePriceBreakdown,
    pub durability_nines: u8,
    pub effective_capacity_gb: f64,
}

#[derive(Debug, Clone)]
pub struct StoragePriceBreakdown {
    pub base_price_gb_month: TokenAmount,
    pub class_multiplier: f64,
    pub replication_overhead: f64,
    pub volume_discount: f64,
    pub tenant_discount: f64,
    pub effective_price_gb_month: TokenAmount,
}

#[derive(Debug, Clone)]
pub struct OperationPriceQuote {
    pub operation: StorageOperation,
    pub count: u64,
    pub storage_class: StorageClass,
    pub price_per_1k: TokenAmount,
    pub total_price: TokenAmount,
    pub includes_retrieval_fee: bool,
}

#[derive(Debug, Clone)]
pub struct EgressPriceQuote {
    pub size_gb: f64,
    pub destination: EgressDestination,
    pub price_per_gb: TokenAmount,
    pub total_price: TokenAmount,
    pub free_quota_used: f64,
    pub billable_price: TokenAmount,
}

#[derive(Debug, Clone)]
pub struct LifecycleSavingsEstimate {
    pub current_class: StorageClass,
    pub target_class: StorageClass,
    pub current_monthly_cost: TokenAmount,
    pub target_monthly_cost: TokenAmount,
    pub monthly_savings: TokenAmount,
    pub annual_savings: TokenAmount,
    pub is_beneficial: bool,
    pub recommendation: String,
}

impl Default for StoragePricingEngine {
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
    
    #[test]
    fn test_storage_class_pricing() {
        let engine = StoragePricingEngine::new();
        let size = 100 * BYTES_PER_GB; // 100 GB
        let replication = ReplicationConfig {
            replication_factor: 3,
            erasure_coding: None,
            geo_distribution: GeoDistribution::MultiZone,
        };
        
        let hot = engine.calculate_storage_price(
            &"tenant".to_string(),
            size,
            StorageClass::Hot,
            &replication,
        );
        
        let cold = engine.calculate_storage_price(
            &"tenant".to_string(),
            size,
            StorageClass::Cold,
            &replication,
        );
        
        assert!(hot.monthly_price > cold.monthly_price);
    }
    
    #[test]
    fn test_volume_discount() {
        let engine = StoragePricingEngine::new();
        let replication = ReplicationConfig {
            replication_factor: 3,
            erasure_coding: None,
            geo_distribution: GeoDistribution::SingleZone,
        };
        
        let small = engine.calculate_storage_price(
            &"tenant".to_string(),
            100 * BYTES_PER_GB,
            StorageClass::Warm,
            &replication,
        );
        
        let large = engine.calculate_storage_price(
            &"tenant".to_string(),
            10000 * BYTES_PER_GB,
            StorageClass::Warm,
            &replication,
        );
        
        // Large should have lower per-GB cost
        assert!(large.breakdown.effective_price_gb_month < small.breakdown.effective_price_gb_month);
    }
    
    #[test]
    fn test_erasure_coding_overhead() {
        // 8+4 erasure coding = 1.5x overhead (not 3x for triple replication)
        let ec = ErasureCoding {
            data_shards: 8,
            parity_shards: 4,
        };
        
        assert!((ec.overhead() - 1.5).abs() < 0.001);
    }
    
    #[test]
    fn test_durability_calculation() {
        let single = ReplicationConfig {
            replication_factor: 1,
            erasure_coding: None,
            geo_distribution: GeoDistribution::SingleZone,
        };
        
        let triple_geo = ReplicationConfig {
            replication_factor: 3,
            erasure_coding: Some(ErasureCoding {
                data_shards: 8,
                parity_shards: 4,
            }),
            geo_distribution: GeoDistribution::Global,
        };
        
        assert!(triple_geo.durability_nines() > single.durability_nines());
    }
}
