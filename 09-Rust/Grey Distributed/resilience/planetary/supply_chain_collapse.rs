//! Supply Chain Collapse Orchestration
//!
//! Handles distributed system operation during global supply chain
//! disruptions affecting hardware, energy, and operational resources.
//!
//! Key challenges addressed:
//! - Hardware replacement unavailability
//! - Energy supply disruption
//! - Spare parts shortage
//! - Vendor failure
//! - Extended lead times

use std::collections::{HashMap, VecDeque};
use std::time::Duration;
use serde::{Deserialize, Serialize};

// =============================================================================
// SUPPLY CHAIN MODEL
// =============================================================================

/// Types of supply chain disruptions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DisruptionType {
    /// Semiconductor shortage
    ChipShortage {
        affected_components: Vec<ComponentType>,
        shortage_severity: ShortageLevel,
        estimated_duration: Duration,
    },
    
    /// Shipping/logistics breakdown
    LogisticsCollapse {
        affected_routes: Vec<String>,
        port_closures: Vec<String>,
        capacity_reduction_pct: f64,
    },
    
    /// Raw material shortage
    MaterialShortage {
        materials: Vec<String>,
        substitutes_available: bool,
    },
    
    /// Major vendor failure
    VendorFailure {
        vendor_id: VendorId,
        product_lines: Vec<String>,
        financial_or_operational: String,
    },
    
    /// Manufacturing disruption
    ManufacturingDisruption {
        regions: Vec<RegionId>,
        cause: String,
    },
    
    /// Energy shortage affecting supply chain
    EnergyConstraint {
        affected_regions: Vec<RegionId>,
        rationing_level: f64,
    },
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum ShortageLevel {
    Minor,     // 10-25% below demand
    Moderate,  // 25-50% below demand
    Severe,    // 50-75% below demand
    Critical,  // >75% below demand
    Complete,  // No supply
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum ComponentType {
    CPU,
    Memory,
    Storage,
    NetworkInterface,
    PowerSupply,
    Cooling,
    Chassis,
    Cable,
    Optics,
}

/// Supply chain status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SupplyChainStatus {
    pub disruptions: Vec<DisruptionType>,
    pub inventory_health: InventoryHealth,
    pub procurement_lead_times: LeadTimeStatus,
    pub vendor_health: HashMap<VendorId, VendorHealth>,
    pub capacity_outlook: CapacityOutlook,
}

// =============================================================================
// INVENTORY MANAGEMENT
// =============================================================================

/// Manages hardware and operational inventory
#[derive(Debug, Clone)]
pub struct InventoryManager {
    /// Current inventory levels
    inventory: HashMap<ComponentType, InventoryLevel>,
    
    /// Deployed equipment
    deployed: HashMap<FacilityId, Vec<DeployedEquipment>>,
    
    /// Consumption rates
    consumption: HashMap<ComponentType, ConsumptionRate>,
    
    /// Reorder points
    reorder_points: HashMap<ComponentType, u64>,
    
    /// Strategic reserves
    strategic_reserve: HashMap<ComponentType, u64>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InventoryLevel {
    pub component: ComponentType,
    pub available: u64,
    pub reserved: u64,
    pub in_transit: u64,
    pub on_order: u64,
    pub last_updated: Timestamp,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeployedEquipment {
    pub asset_id: String,
    pub component_type: ComponentType,
    pub deployed_date: Timestamp,
    pub expected_life: Duration,
    pub health_score: f64,
    pub critical: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConsumptionRate {
    pub component: ComponentType,
    pub failure_rate_per_month: f64,
    pub growth_rate_per_month: f64,
    pub seasonal_factor: f64,
}

impl InventoryManager {
    /// Calculate runway for each component type
    pub fn calculate_runway(&self) -> HashMap<ComponentType, Duration> {
        let mut runways = HashMap::new();
        
        for (component, level) in &self.inventory {
            if let Some(consumption) = self.consumption.get(component) {
                let monthly_need = consumption.failure_rate_per_month + 
                                   consumption.growth_rate_per_month;
                
                if monthly_need > 0.0 {
                    let months = level.available as f64 / monthly_need;
                    runways.insert(*component, Duration::from_secs((months * 30.0 * 86400.0) as u64));
                }
            }
        }
        
        runways
    }
    
    /// Identify critical shortages
    pub fn identify_shortages(&self) -> Vec<Shortage> {
        let mut shortages = Vec::new();
        
        for (component, level) in &self.inventory {
            let reorder_point = self.reorder_points.get(component).copied().unwrap_or(0);
            
            if level.available < reorder_point {
                shortages.push(Shortage {
                    component: *component,
                    current: level.available,
                    required: reorder_point,
                    severity: if level.available == 0 {
                        ShortageLevel::Complete
                    } else if level.available < reorder_point / 4 {
                        ShortageLevel::Critical
                    } else if level.available < reorder_point / 2 {
                        ShortageLevel::Severe
                    } else {
                        ShortageLevel::Moderate
                    },
                });
            }
        }
        
        shortages
    }
    
    /// Enable conservation mode
    pub fn enable_conservation_mode(&mut self, component: ComponentType) -> ConservationPlan {
        let level = self.inventory.get(&component);
        let consumption = self.consumption.get(&component);
        
        match (level, consumption) {
            (Some(level), Some(consumption)) => {
                ConservationPlan {
                    component,
                    current_stock: level.available,
                    conservation_measures: vec![
                        ConservationMeasure::ExtendReplacementCycles,
                        ConservationMeasure::PrioritizeCriticalSystems,
                        ConservationMeasure::CannibalizeNonCritical,
                        ConservationMeasure::ReduceRedundancy,
                    ],
                    estimated_extension: Duration::from_secs(
                        (30.0 * 86400.0 * level.available as f64 / 
                         (consumption.failure_rate_per_month * 0.5)) as u64
                    ),
                }
            }
            _ => ConservationPlan {
                component,
                current_stock: 0,
                conservation_measures: vec![],
                estimated_extension: Duration::ZERO,
            }
        }
    }
    
    /// Release from strategic reserve
    pub fn release_strategic_reserve(&mut self, component: ComponentType, amount: u64) -> ReleaseResult {
        let reserve = self.strategic_reserve.get_mut(&component);
        
        match reserve {
            Some(reserve_level) if *reserve_level >= amount => {
                *reserve_level -= amount;
                
                if let Some(inventory) = self.inventory.get_mut(&component) {
                    inventory.available += amount;
                }
                
                ReleaseResult::Released { amount }
            }
            Some(reserve_level) => {
                let available = *reserve_level;
                *reserve_level = 0;
                
                if let Some(inventory) = self.inventory.get_mut(&component) {
                    inventory.available += available;
                }
                
                ReleaseResult::PartialRelease { 
                    requested: amount,
                    released: available,
                }
            }
            None => ReleaseResult::NoReserve,
        }
    }
}

// =============================================================================
// VENDOR MANAGEMENT
// =============================================================================

/// Manages vendor relationships during disruptions
#[derive(Debug, Clone)]
pub struct VendorManager {
    /// Vendor information
    vendors: HashMap<VendorId, VendorInfo>,
    
    /// Supply agreements
    agreements: HashMap<VendorId, SupplyAgreement>,
    
    /// Alternate suppliers
    alternates: HashMap<ComponentType, Vec<VendorId>>,
    
    /// Procurement queue
    procurement_queue: VecDeque<ProcurementRequest>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VendorInfo {
    pub id: VendorId,
    pub name: String,
    pub primary_products: Vec<ComponentType>,
    pub regions: Vec<RegionId>,
    pub risk_score: f64,
    pub last_delivery: Option<Timestamp>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VendorHealth {
    pub operational: bool,
    pub accepting_orders: bool,
    pub lead_time_multiplier: f64,
    pub capacity_available_pct: f64,
    pub financial_stability: FinancialStability,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum FinancialStability {
    Stable,
    Concerns,
    AtRisk,
    Distressed,
    Insolvent,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SupplyAgreement {
    pub vendor_id: VendorId,
    pub components: Vec<ComponentType>,
    pub minimum_order: u64,
    pub lead_time_days: u32,
    pub guaranteed_capacity_units: u64,
    pub expires: Timestamp,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcurementRequest {
    pub id: String,
    pub component: ComponentType,
    pub quantity: u64,
    pub priority: ProcurementPriority,
    pub requested: Timestamp,
    pub status: ProcurementStatus,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum ProcurementPriority {
    Emergency,
    Critical,
    High,
    Normal,
    Low,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ProcurementStatus {
    Requested,
    Sourcing,
    Ordered,
    Manufacturing,
    Shipping,
    Delivered,
    Cancelled,
}

impl VendorManager {
    /// Find alternate supplier for component
    pub async fn find_alternate_supplier(&self, component: ComponentType) -> Option<VendorId> {
        let alternates = self.alternates.get(&component)?;
        
        for vendor_id in alternates {
            let info = self.vendors.get(vendor_id)?;
            let agreement = self.agreements.get(vendor_id);
            
            // Check if vendor is healthy and can supply
            if info.risk_score < 0.7 {
                if let Some(agreement) = agreement {
                    if agreement.components.contains(&component) {
                        return Some(vendor_id.clone());
                    }
                }
            }
        }
        
        None
    }
    
    /// Handle vendor failure
    pub async fn handle_vendor_failure(&mut self, vendor_id: &VendorId) -> VendorFailureResponse {
        let info = self.vendors.get(vendor_id);
        
        let affected_components: Vec<_> = info
            .map(|i| i.primary_products.clone())
            .unwrap_or_default();
        
        let mut alternates_found = HashMap::new();
        let mut components_at_risk = Vec::new();
        
        for component in &affected_components {
            if let Some(alternate) = self.find_alternate_supplier(*component).await {
                alternates_found.insert(*component, alternate);
            } else {
                components_at_risk.push(*component);
            }
        }
        
        // Cancel pending orders with failed vendor
        let cancelled = self.cancel_vendor_orders(vendor_id);
        
        VendorFailureResponse {
            vendor_id: vendor_id.clone(),
            affected_components,
            alternates_found,
            components_at_risk,
            orders_cancelled: cancelled,
        }
    }
    
    /// Activate emergency procurement
    pub async fn emergency_procurement(&mut self, request: ProcurementRequest) -> EmergencyProcurementResult {
        // Try all vendors
        let mut attempts = Vec::new();
        
        for (vendor_id, info) in &self.vendors {
            if info.primary_products.contains(&request.component) {
                attempts.push(ProcurementAttempt {
                    vendor_id: vendor_id.clone(),
                    result: ProcurementAttemptResult::Pending,
                });
            }
        }
        
        EmergencyProcurementResult {
            request_id: request.id.clone(),
            component: request.component,
            quantity: request.quantity,
            attempts,
            estimated_delivery: None,
        }
    }
    
    fn cancel_vendor_orders(&mut self, vendor_id: &VendorId) -> usize {
        // Would cancel orders in procurement queue
        0
    }
}

// =============================================================================
// CAPACITY ADAPTATION
// =============================================================================

/// Adapts system capacity during supply chain constraints
#[derive(Debug, Clone)]
pub struct CapacityAdapter {
    /// Current capacity by facility
    facility_capacity: HashMap<FacilityId, FacilityCapacity>,
    
    /// Workload requirements
    workload_requirements: WorkloadRequirements,
    
    /// Degradation rules
    degradation_rules: Vec<DegradationRule>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FacilityCapacity {
    pub facility_id: FacilityId,
    pub compute_capacity: f64,
    pub storage_capacity: f64,
    pub network_capacity: f64,
    pub projected_failures: Vec<ProjectedFailure>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectedFailure {
    pub component: ComponentType,
    pub estimated_date: Timestamp,
    pub impact_on_capacity: f64,
    pub replacement_available: bool,
}

#[derive(Debug, Clone)]
pub struct WorkloadRequirements {
    pub minimum_compute: f64,
    pub minimum_storage: f64,
    pub minimum_network: f64,
    pub priority_workloads: Vec<WorkloadId>,
}

#[derive(Debug, Clone)]
pub struct DegradationRule {
    pub trigger: DegradationTrigger,
    pub actions: Vec<DegradationAction>,
}

#[derive(Debug, Clone)]
pub enum DegradationTrigger {
    CapacityBelow(f64),
    ComponentShortage(ComponentType),
    VendorFailure(VendorId),
    LeadTimeExceeds(Duration),
}

#[derive(Debug, Clone)]
pub enum DegradationAction {
    ReduceReplication,
    ConsolidateWorkloads,
    DeferNonCritical,
    ShedLowPriority,
    RatioCapacity,
}

impl CapacityAdapter {
    /// Project future capacity based on supply chain status
    pub fn project_capacity(&self, horizon: Duration) -> CapacityProjection {
        let mut min_compute = f64::MAX;
        let mut min_storage = f64::MAX;
        let mut min_network = f64::MAX;
        
        for (_, capacity) in &self.facility_capacity {
            let mut facility_compute = capacity.compute_capacity;
            let mut facility_storage = capacity.storage_capacity;
            let mut facility_network = capacity.network_capacity;
            
            // Apply projected failures
            let horizon_end = Timestamp::now() + horizon.as_secs();
            
            for failure in &capacity.projected_failures {
                if failure.estimated_date < horizon_end && !failure.replacement_available {
                    match failure.component {
                        ComponentType::CPU => facility_compute -= failure.impact_on_capacity,
                        ComponentType::Storage => facility_storage -= failure.impact_on_capacity,
                        ComponentType::NetworkInterface => facility_network -= failure.impact_on_capacity,
                        _ => {}
                    }
                }
            }
            
            min_compute = min_compute.min(facility_compute);
            min_storage = min_storage.min(facility_storage);
            min_network = min_network.min(facility_network);
        }
        
        CapacityProjection {
            horizon,
            projected_compute: min_compute.max(0.0),
            projected_storage: min_storage.max(0.0),
            projected_network: min_network.max(0.0),
            meets_minimum: min_compute >= self.workload_requirements.minimum_compute &&
                          min_storage >= self.workload_requirements.minimum_storage &&
                          min_network >= self.workload_requirements.minimum_network,
        }
    }
    
    /// Generate adaptation plan
    pub fn generate_adaptation_plan(&self, supply_status: &SupplyChainStatus) -> AdaptationPlan {
        let mut actions = Vec::new();
        
        // Check each degradation trigger
        for rule in &self.degradation_rules {
            match &rule.trigger {
                DegradationTrigger::CapacityBelow(threshold) => {
                    if supply_status.capacity_outlook.compute_forecast < *threshold {
                        actions.extend(rule.actions.clone());
                    }
                }
                DegradationTrigger::ComponentShortage(component) => {
                    for disruption in &supply_status.disruptions {
                        if let DisruptionType::ChipShortage { affected_components, .. } = disruption {
                            if affected_components.contains(component) {
                                actions.extend(rule.actions.clone());
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        
        AdaptationPlan {
            actions: actions.into_iter().collect(),
            estimated_capacity_preserved: 0.7,
            services_affected: vec![],
        }
    }
    
    /// Execute degradation action
    pub async fn execute_degradation(&mut self, action: DegradationAction) -> DegradationResult {
        match action {
            DegradationAction::ReduceReplication => {
                DegradationResult {
                    action,
                    capacity_freed: 0.15,
                    availability_impact: 0.05,
                    success: true,
                }
            }
            DegradationAction::ConsolidateWorkloads => {
                DegradationResult {
                    action,
                    capacity_freed: 0.20,
                    availability_impact: 0.02,
                    success: true,
                }
            }
            DegradationAction::ShedLowPriority => {
                DegradationResult {
                    action,
                    capacity_freed: 0.30,
                    availability_impact: 0.0,
                    success: true,
                }
            }
            _ => DegradationResult {
                action,
                capacity_freed: 0.0,
                availability_impact: 0.0,
                success: false,
            }
        }
    }
}

// =============================================================================
// SUPPLY CHAIN COORDINATOR
// =============================================================================

/// Main coordinator for supply chain disruption response
pub struct SupplyChainCoordinator {
    /// Current supply chain status
    status: SupplyChainStatus,
    
    /// Inventory management
    inventory: InventoryManager,
    
    /// Vendor management
    vendors: VendorManager,
    
    /// Capacity adaptation
    capacity: CapacityAdapter,
}

impl SupplyChainCoordinator {
    /// Handle supply chain disruption
    pub async fn handle_disruption(&mut self, disruption: DisruptionType) -> DisruptionResponse {
        self.status.disruptions.push(disruption.clone());
        
        let mut response = DisruptionResponse::new();
        
        match &disruption {
            DisruptionType::ChipShortage { affected_components, shortage_severity, .. } => {
                // Enable conservation for affected components
                for component in affected_components {
                    let plan = self.inventory.enable_conservation_mode(*component);
                    response.conservation_plans.push(plan);
                }
                
                // Project capacity impact
                let projection = self.capacity.project_capacity(Duration::from_secs(90 * 86400));
                response.capacity_projection = Some(projection);
                
                // Activate strategic reserve if severe
                if matches!(shortage_severity, ShortageLevel::Severe | ShortageLevel::Critical | ShortageLevel::Complete) {
                    for component in affected_components {
                        let release = self.inventory.release_strategic_reserve(*component, 100);
                        response.reserve_releases.push((*component, release));
                    }
                }
            }
            
            DisruptionType::VendorFailure { vendor_id, .. } => {
                let vendor_response = self.vendors.handle_vendor_failure(vendor_id).await;
                response.vendor_failover = Some(vendor_response);
            }
            
            DisruptionType::LogisticsCollapse { capacity_reduction_pct, .. } => {
                // Adapt capacity based on expected delays
                let adaptation = self.capacity.generate_adaptation_plan(&self.status);
                response.adaptation_plan = Some(adaptation);
            }
            
            _ => {}
        }
        
        response
    }
    
    /// Get overall supply chain health
    pub fn health_check(&self) -> SupplyChainHealth {
        let inventory_runways = self.inventory.calculate_runway();
        let shortages = self.inventory.identify_shortages();
        let capacity_projection = self.capacity.project_capacity(Duration::from_secs(180 * 86400));
        
        let health_score = self.calculate_health_score(&inventory_runways, &shortages, &capacity_projection);
        
        SupplyChainHealth {
            overall_score: health_score,
            inventory_runways,
            current_shortages: shortages,
            capacity_projection,
            active_disruptions: self.status.disruptions.len(),
            recommendations: self.generate_recommendations(health_score),
        }
    }
    
    fn calculate_health_score(
        &self, 
        runways: &HashMap<ComponentType, Duration>,
        shortages: &[Shortage],
        projection: &CapacityProjection,
    ) -> f64 {
        let mut score = 1.0;
        
        // Deduct for short runways
        for (_, runway) in runways {
            if *runway < Duration::from_secs(30 * 86400) {
                score -= 0.1;
            } else if *runway < Duration::from_secs(90 * 86400) {
                score -= 0.05;
            }
        }
        
        // Deduct for shortages
        for shortage in shortages {
            match shortage.severity {
                ShortageLevel::Complete => score -= 0.2,
                ShortageLevel::Critical => score -= 0.15,
                ShortageLevel::Severe => score -= 0.1,
                ShortageLevel::Moderate => score -= 0.05,
                ShortageLevel::Minor => score -= 0.02,
            }
        }
        
        // Deduct if capacity won't meet minimum
        if !projection.meets_minimum {
            score -= 0.3;
        }
        
        score.max(0.0)
    }
    
    fn generate_recommendations(&self, health_score: f64) -> Vec<String> {
        let mut recommendations = Vec::new();
        
        if health_score < 0.5 {
            recommendations.push("Activate emergency procurement channels".to_string());
            recommendations.push("Review vendor diversification strategy".to_string());
        }
        
        if health_score < 0.7 {
            recommendations.push("Increase strategic reserve levels".to_string());
            recommendations.push("Prepare capacity degradation plans".to_string());
        }
        
        recommendations
    }
}

// =============================================================================
// TYPE DEFINITIONS
// =============================================================================

pub type VendorId = String;
pub type FacilityId = String;
pub type RegionId = String;
pub type WorkloadId = String;
pub type Timestamp = u64;

impl Timestamp {
    pub fn now() -> Self {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InventoryHealth {
    pub overall_score: f64,
    pub components_critical: Vec<ComponentType>,
    pub components_warning: Vec<ComponentType>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LeadTimeStatus {
    pub average_multiplier: f64,
    pub by_component: HashMap<ComponentType, f64>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CapacityOutlook {
    pub compute_forecast: f64,
    pub storage_forecast: f64,
    pub network_forecast: f64,
}

#[derive(Debug, Clone)]
pub struct Shortage {
    pub component: ComponentType,
    pub current: u64,
    pub required: u64,
    pub severity: ShortageLevel,
}

#[derive(Debug, Clone)]
pub struct ConservationPlan {
    pub component: ComponentType,
    pub current_stock: u64,
    pub conservation_measures: Vec<ConservationMeasure>,
    pub estimated_extension: Duration,
}

#[derive(Debug, Clone)]
pub enum ConservationMeasure {
    ExtendReplacementCycles,
    PrioritizeCriticalSystems,
    CannibalizeNonCritical,
    ReduceRedundancy,
    RatioAllocation,
}

#[derive(Debug, Clone)]
pub enum ReleaseResult {
    Released { amount: u64 },
    PartialRelease { requested: u64, released: u64 },
    NoReserve,
}

#[derive(Debug, Clone)]
pub struct VendorFailureResponse {
    pub vendor_id: VendorId,
    pub affected_components: Vec<ComponentType>,
    pub alternates_found: HashMap<ComponentType, VendorId>,
    pub components_at_risk: Vec<ComponentType>,
    pub orders_cancelled: usize,
}

#[derive(Debug, Clone)]
pub struct EmergencyProcurementResult {
    pub request_id: String,
    pub component: ComponentType,
    pub quantity: u64,
    pub attempts: Vec<ProcurementAttempt>,
    pub estimated_delivery: Option<Duration>,
}

#[derive(Debug, Clone)]
pub struct ProcurementAttempt {
    pub vendor_id: VendorId,
    pub result: ProcurementAttemptResult,
}

#[derive(Debug, Clone)]
pub enum ProcurementAttemptResult {
    Pending,
    Accepted,
    Rejected { reason: String },
    PartialFulfillment { available: u64 },
}

#[derive(Debug, Clone)]
pub struct CapacityProjection {
    pub horizon: Duration,
    pub projected_compute: f64,
    pub projected_storage: f64,
    pub projected_network: f64,
    pub meets_minimum: bool,
}

#[derive(Debug, Clone)]
pub struct AdaptationPlan {
    pub actions: HashSet<DegradationAction>,
    pub estimated_capacity_preserved: f64,
    pub services_affected: Vec<String>,
}

impl std::hash::Hash for DegradationAction {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
    }
}

impl PartialEq for DegradationAction {
    fn eq(&self, other: &Self) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}

impl Eq for DegradationAction {}

#[derive(Debug, Clone)]
pub struct DegradationResult {
    pub action: DegradationAction,
    pub capacity_freed: f64,
    pub availability_impact: f64,
    pub success: bool,
}

#[derive(Debug, Clone)]
pub struct DisruptionResponse {
    pub conservation_plans: Vec<ConservationPlan>,
    pub capacity_projection: Option<CapacityProjection>,
    pub reserve_releases: Vec<(ComponentType, ReleaseResult)>,
    pub vendor_failover: Option<VendorFailureResponse>,
    pub adaptation_plan: Option<AdaptationPlan>,
}

impl DisruptionResponse {
    pub fn new() -> Self {
        Self {
            conservation_plans: vec![],
            capacity_projection: None,
            reserve_releases: vec![],
            vendor_failover: None,
            adaptation_plan: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SupplyChainHealth {
    pub overall_score: f64,
    pub inventory_runways: HashMap<ComponentType, Duration>,
    pub current_shortages: Vec<Shortage>,
    pub capacity_projection: CapacityProjection,
    pub active_disruptions: usize,
    pub recommendations: Vec<String>,
}

use std::collections::HashSet;
