//! Global Supply Chain Orchestration
//!
//! This module implements civilization-scale supply chain coordination,
//! enabling real-time visibility, optimization, and resilience across
//! global logistics networks spanning manufacturing, shipping, and distribution.
//!
//! # Architecture Overview
//!
//! Grey Distributed provides a coordination layer for supply chain participants:
//!
//! - **Global visibility**: Real-time tracking of goods across borders
//! - **Multi-party coordination**: Synchronize actions across suppliers, carriers, customs
//! - **Demand-supply matching**: Optimize inventory and routing globally
//! - **Disruption response**: Coordinated rerouting during crises
//!
//! # Supply Chain Network
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────────────────┐
//! │                    Global Supply Chain Network                          │
//! │                                                                         │
//! │  ┌─────────────┐     ┌─────────────┐     ┌─────────────┐              │
//! │  │  Suppliers  │────►│ Manufacturers│────►│ Distribution │             │
//! │  │  (Tier 1-N) │     │             │     │   Centers    │             │
//! │  └──────┬──────┘     └──────┬──────┘     └──────┬──────┘              │
//! │         │                   │                   │                      │
//! │         └───────────────────┼───────────────────┘                      │
//! │                             │                                          │
//! │                    ┌────────┴────────┐                                 │
//! │                    ▼                 ▼                                 │
//! │              ┌──────────┐      ┌──────────┐                           │
//! │              │ Maritime │      │   Air    │                           │
//! │              │ Carriers │      │ Freight  │                           │
//! │              └────┬─────┘      └────┬─────┘                           │
//! │                   │                 │                                  │
//! │              ┌────┴─────────────────┴────┐                            │
//! │              ▼                           ▼                            │
//! │         ┌────────┐                  ┌────────┐                        │
//! │         │ Ports  │                  │Airports│                        │
//! │         │        │                  │        │                        │
//! │         └────┬───┘                  └────┬───┘                        │
//! │              │                           │                            │
//! │              └───────────┬───────────────┘                            │
//! │                          ▼                                            │
//! │                    ┌──────────┐                                       │
//! │                    │ Last Mile│                                       │
//! │                    │ Delivery │                                       │
//! │                    └──────────┘                                       │
//! └─────────────────────────────────────────────────────────────────────────┘
//! ```

use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use std::sync::Arc;
use std::time::{Duration, SystemTime};

// ============================================================================
// Core Types
// ============================================================================

/// Unique identifier for a shipment
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ShipmentId(pub String);

/// Unique identifier for an order
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OrderId(pub String);

/// SKU (Stock Keeping Unit) for products
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Sku(pub String);

/// Location identifier (facility, port, address)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LocationId(pub String);

/// Participant in the supply chain
#[derive(Debug, Clone)]
pub struct Participant {
    pub id: ParticipantId,
    pub name: String,
    pub participant_type: ParticipantType,
    pub locations: Vec<LocationId>,
    pub capabilities: Vec<Capability>,
    pub certifications: Vec<Certification>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParticipantId(pub String);

#[derive(Debug, Clone)]
pub enum ParticipantType {
    Supplier { tier: u8 },
    Manufacturer,
    Distributor,
    Carrier { modes: Vec<TransportMode> },
    PortOperator,
    CustomsBroker,
    Retailer,
    EndCustomer,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TransportMode {
    Ocean,
    Air,
    Rail,
    Road,
    Intermodal,
    Pipeline,
}

#[derive(Debug, Clone)]
pub enum Capability {
    ColdChain { min_temp: f32, max_temp: f32 },
    HazMat { classes: Vec<String> },
    Oversized,
    HighValue,
    Bonded,
    FreeTradeZone,
}

#[derive(Debug, Clone)]
pub struct Certification {
    pub name: String,
    pub issuer: String,
    pub valid_until: chrono::NaiveDate,
}

// ============================================================================
// Geographic Types
// ============================================================================

#[derive(Debug, Clone)]
pub struct GeoLocation {
    pub latitude: f64,
    pub longitude: f64,
}

#[derive(Debug, Clone)]
pub struct Facility {
    pub id: LocationId,
    pub name: String,
    pub location: GeoLocation,
    pub facility_type: FacilityType,
    pub capacity: FacilityCapacity,
    pub operating_hours: OperatingHours,
    pub customs_zone: Option<CustomsZone>,
}

#[derive(Debug, Clone)]
pub enum FacilityType {
    Factory,
    Warehouse,
    DistributionCenter,
    Port,
    Airport,
    RailTerminal,
    CrossDock,
    LastMileHub,
}

#[derive(Debug, Clone)]
pub struct FacilityCapacity {
    pub total_sqm: f64,
    pub available_sqm: f64,
    pub throughput_units_per_day: u64,
    pub dock_doors: u32,
}

#[derive(Debug, Clone)]
pub struct OperatingHours {
    pub timezone: String,
    pub schedule: HashMap<chrono::Weekday, DaySchedule>,
}

#[derive(Debug, Clone)]
pub struct DaySchedule {
    pub open: chrono::NaiveTime,
    pub close: chrono::NaiveTime,
    pub breaks: Vec<(chrono::NaiveTime, chrono::NaiveTime)>,
}

#[derive(Debug, Clone)]
pub struct CustomsZone {
    pub country: String,
    pub zone_type: String,
    pub broker_required: bool,
}

// ============================================================================
// Order Management
// ============================================================================

/// A customer order spanning multiple products and shipments
#[derive(Debug, Clone)]
pub struct Order {
    pub id: OrderId,
    pub customer: ParticipantId,
    pub lines: Vec<OrderLine>,
    pub ship_to: ShippingAddress,
    pub required_delivery: DeliveryWindow,
    pub priority: OrderPriority,
    pub status: OrderStatus,
    pub created_at: SystemTime,
}

#[derive(Debug, Clone)]
pub struct OrderLine {
    pub line_number: u32,
    pub sku: Sku,
    pub quantity: u64,
    pub unit_price: f64,
    pub currency: String,
    pub fulfilled_quantity: u64,
    pub shipments: Vec<ShipmentId>,
}

#[derive(Debug, Clone)]
pub struct ShippingAddress {
    pub location_id: Option<LocationId>,
    pub address_lines: Vec<String>,
    pub city: String,
    pub state: Option<String>,
    pub postal_code: String,
    pub country: String,
    pub coordinates: Option<GeoLocation>,
}

#[derive(Debug, Clone)]
pub struct DeliveryWindow {
    pub earliest: SystemTime,
    pub latest: SystemTime,
    pub time_slot: Option<TimeSlot>,
}

#[derive(Debug, Clone)]
pub struct TimeSlot {
    pub start: chrono::NaiveTime,
    pub end: chrono::NaiveTime,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum OrderPriority {
    Standard,
    Express,
    Priority,
    Emergency,
}

#[derive(Debug, Clone, PartialEq)]
pub enum OrderStatus {
    Pending,
    Confirmed,
    PartiallyFulfilled { fulfilled_lines: Vec<u32> },
    Fulfilled,
    Delivered,
    Cancelled { reason: String },
}

// ============================================================================
// Shipment Tracking
// ============================================================================

/// A physical shipment of goods
#[derive(Debug, Clone)]
pub struct Shipment {
    pub id: ShipmentId,
    pub orders: Vec<OrderId>,
    pub contents: Vec<ShipmentContent>,
    pub origin: LocationId,
    pub destination: LocationId,
    pub route: ShipmentRoute,
    pub current_leg: usize,
    pub status: ShipmentStatus,
    pub tracking: Vec<TrackingEvent>,
    pub documents: ShipmentDocuments,
    pub created_at: SystemTime,
}

#[derive(Debug, Clone)]
pub struct ShipmentContent {
    pub sku: Sku,
    pub quantity: u64,
    pub weight_kg: f64,
    pub volume_cbm: f64,
    pub handling: HandlingRequirements,
}

#[derive(Debug, Clone)]
pub struct HandlingRequirements {
    pub temperature_controlled: Option<TemperatureRange>,
    pub hazmat_class: Option<String>,
    pub fragile: bool,
    pub stackable: bool,
    pub orientation: Option<String>,
}

#[derive(Debug, Clone)]
pub struct TemperatureRange {
    pub min_celsius: f32,
    pub max_celsius: f32,
}

#[derive(Debug, Clone)]
pub struct ShipmentRoute {
    pub legs: Vec<RouteLeg>,
    pub total_distance_km: f64,
    pub estimated_duration: Duration,
    pub carbon_footprint_kg: f64,
}

#[derive(Debug, Clone)]
pub struct RouteLeg {
    pub leg_number: u32,
    pub origin: LocationId,
    pub destination: LocationId,
    pub carrier: ParticipantId,
    pub mode: TransportMode,
    pub vehicle: Option<VehicleInfo>,
    pub scheduled_departure: SystemTime,
    pub scheduled_arrival: SystemTime,
    pub status: LegStatus,
}

#[derive(Debug, Clone)]
pub struct VehicleInfo {
    pub vehicle_id: String,
    pub vehicle_type: String,
    pub capacity_kg: f64,
    pub capacity_cbm: f64,
    pub imo_number: Option<String>,
    pub container_ids: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LegStatus {
    Scheduled,
    InTransit { current_location: Option<GeoLocation> },
    Arrived,
    Completed,
    Delayed { reason: String, new_eta: SystemTime },
    Cancelled { reason: String },
}

#[derive(Debug, Clone, PartialEq)]
pub enum ShipmentStatus {
    Created,
    PickedUp,
    InTransit,
    AtCustoms { location: LocationId },
    CustomsCleared,
    OutForDelivery,
    Delivered { signed_by: String, timestamp: SystemTime },
    Exception { code: String, description: String },
}

#[derive(Debug, Clone)]
pub struct TrackingEvent {
    pub timestamp: SystemTime,
    pub location: LocationId,
    pub event_type: TrackingEventType,
    pub description: String,
    pub source: String,
}

#[derive(Debug, Clone)]
pub enum TrackingEventType {
    Created,
    PickedUp,
    Departed,
    Arrived,
    CustomsHold,
    CustomsReleased,
    LoadedOnVehicle,
    UnloadedFromVehicle,
    OutForDelivery,
    DeliveryAttempted { reason: String },
    Delivered,
    Exception,
}

#[derive(Debug, Clone)]
pub struct ShipmentDocuments {
    pub commercial_invoice: Option<DocumentRef>,
    pub packing_list: Option<DocumentRef>,
    pub bill_of_lading: Option<DocumentRef>,
    pub air_waybill: Option<DocumentRef>,
    pub customs_declaration: Option<DocumentRef>,
    pub certificate_of_origin: Option<DocumentRef>,
    pub dangerous_goods: Option<DocumentRef>,
}

#[derive(Debug, Clone)]
pub struct DocumentRef {
    pub document_id: String,
    pub document_type: String,
    pub hash: String,
    pub issued_at: SystemTime,
}

// ============================================================================
// Inventory Management
// ============================================================================

/// Global inventory visibility across the supply chain
pub struct InventoryManager {
    /// Inventory positions by location and SKU
    positions: HashMap<(LocationId, Sku), InventoryPosition>,
    /// In-transit inventory
    in_transit: HashMap<Sku, Vec<InTransitInventory>>,
    /// Demand forecasts
    forecasts: HashMap<Sku, DemandForecast>,
    /// Reorder policies
    policies: HashMap<Sku, ReorderPolicy>,
}

#[derive(Debug, Clone)]
pub struct InventoryPosition {
    pub location: LocationId,
    pub sku: Sku,
    pub on_hand: u64,
    pub allocated: u64,
    pub on_order: u64,
    pub last_count: SystemTime,
    pub lot_details: Vec<LotInfo>,
}

impl InventoryPosition {
    pub fn available(&self) -> u64 {
        self.on_hand.saturating_sub(self.allocated)
    }
    
    pub fn projected(&self) -> u64 {
        self.on_hand + self.on_order - self.allocated
    }
}

#[derive(Debug, Clone)]
pub struct LotInfo {
    pub lot_number: String,
    pub quantity: u64,
    pub manufacture_date: chrono::NaiveDate,
    pub expiry_date: Option<chrono::NaiveDate>,
    pub origin: String,
}

#[derive(Debug, Clone)]
pub struct InTransitInventory {
    pub shipment_id: ShipmentId,
    pub quantity: u64,
    pub destination: LocationId,
    pub eta: SystemTime,
}

#[derive(Debug, Clone)]
pub struct DemandForecast {
    pub sku: Sku,
    pub periods: Vec<ForecastPeriod>,
    pub model: ForecastModel,
    pub last_updated: SystemTime,
}

#[derive(Debug, Clone)]
pub struct ForecastPeriod {
    pub start: chrono::NaiveDate,
    pub end: chrono::NaiveDate,
    pub expected_demand: u64,
    pub confidence_low: u64,
    pub confidence_high: u64,
}

#[derive(Debug, Clone)]
pub enum ForecastModel {
    ExponentialSmoothing { alpha: f64 },
    Arima { p: u32, d: u32, q: u32 },
    MachineLearning { model_id: String },
    Ensemble { models: Vec<String> },
}

#[derive(Debug, Clone)]
pub struct ReorderPolicy {
    pub sku: Sku,
    pub location: LocationId,
    pub policy_type: ReorderPolicyType,
    pub lead_time_days: u32,
    pub safety_stock_days: u32,
}

#[derive(Debug, Clone)]
pub enum ReorderPolicyType {
    /// Reorder when inventory drops below point
    ReorderPoint { reorder_point: u64, order_quantity: u64 },
    /// Periodic review (e.g., weekly)
    Periodic { review_interval_days: u32, target_level: u64 },
    /// Min-max system
    MinMax { min: u64, max: u64 },
    /// Just-in-time (demand-driven)
    JustInTime,
}

impl InventoryManager {
    pub fn new() -> Self {
        Self {
            positions: HashMap::new(),
            in_transit: HashMap::new(),
            forecasts: HashMap::new(),
            policies: HashMap::new(),
        }
    }
    
    /// Check stock across all locations for a SKU
    pub fn global_availability(&self, sku: &Sku) -> GlobalAvailability {
        let positions: Vec<_> = self.positions.iter()
            .filter(|((_, s), _)| s == sku)
            .map(|((loc, _), pos)| (loc.clone(), pos.available()))
            .collect();
        
        let total_on_hand: u64 = positions.iter().map(|(_, q)| q).sum();
        let total_in_transit: u64 = self.in_transit.get(sku)
            .map(|v| v.iter().map(|i| i.quantity).sum())
            .unwrap_or(0);
        
        GlobalAvailability {
            sku: sku.clone(),
            total_on_hand,
            total_in_transit,
            by_location: positions,
        }
    }
    
    /// Find best fulfillment location for an order
    pub fn find_fulfillment_location(
        &self,
        sku: &Sku,
        quantity: u64,
        destination: &ShippingAddress,
    ) -> Option<FulfillmentOption> {
        let availability = self.global_availability(sku);
        
        // Find locations with sufficient stock, sorted by proximity
        let mut candidates: Vec<_> = availability.by_location.iter()
            .filter(|(_, avail)| *avail >= quantity)
            .map(|(loc, avail)| {
                // Simplified distance calculation
                let distance = self.estimate_distance(loc, destination);
                (loc.clone(), *avail, distance)
            })
            .collect();
        
        candidates.sort_by(|a, b| a.2.partial_cmp(&b.2).unwrap());
        
        candidates.first().map(|(loc, avail, dist)| FulfillmentOption {
            location: loc.clone(),
            available: *avail,
            estimated_distance_km: *dist,
            estimated_transit_days: (*dist / 500.0).ceil() as u32, // Rough estimate
        })
    }
    
    /// Check for items needing reorder
    pub fn check_reorders(&self) -> Vec<ReorderRecommendation> {
        let mut recommendations = Vec::new();
        
        for (sku, policy) in &self.policies {
            let key = (policy.location.clone(), sku.clone());
            if let Some(position) = self.positions.get(&key) {
                if let Some(rec) = self.evaluate_reorder(position, policy) {
                    recommendations.push(rec);
                }
            }
        }
        
        recommendations
    }
    
    fn evaluate_reorder(
        &self,
        position: &InventoryPosition,
        policy: &ReorderPolicy,
    ) -> Option<ReorderRecommendation> {
        match &policy.policy_type {
            ReorderPolicyType::ReorderPoint { reorder_point, order_quantity } => {
                if position.projected() < *reorder_point {
                    Some(ReorderRecommendation {
                        sku: position.sku.clone(),
                        location: position.location.clone(),
                        quantity: *order_quantity,
                        urgency: if position.available() == 0 {
                            ReorderUrgency::Critical
                        } else if position.available() < *reorder_point / 2 {
                            ReorderUrgency::High
                        } else {
                            ReorderUrgency::Normal
                        },
                    })
                } else {
                    None
                }
            }
            ReorderPolicyType::MinMax { min, max } => {
                if position.projected() < *min {
                    Some(ReorderRecommendation {
                        sku: position.sku.clone(),
                        location: position.location.clone(),
                        quantity: max - position.projected(),
                        urgency: ReorderUrgency::Normal,
                    })
                } else {
                    None
                }
            }
            _ => None,
        }
    }
    
    fn estimate_distance(&self, _from: &LocationId, _to: &ShippingAddress) -> f64 {
        // Would use actual geocoding and routing
        1000.0 // Placeholder
    }
}

#[derive(Debug)]
pub struct GlobalAvailability {
    pub sku: Sku,
    pub total_on_hand: u64,
    pub total_in_transit: u64,
    pub by_location: Vec<(LocationId, u64)>,
}

#[derive(Debug)]
pub struct FulfillmentOption {
    pub location: LocationId,
    pub available: u64,
    pub estimated_distance_km: f64,
    pub estimated_transit_days: u32,
}

#[derive(Debug)]
pub struct ReorderRecommendation {
    pub sku: Sku,
    pub location: LocationId,
    pub quantity: u64,
    pub urgency: ReorderUrgency,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ReorderUrgency {
    Normal,
    High,
    Critical,
}

// ============================================================================
// Route Optimization
// ============================================================================

/// Global route optimization engine
/// 
/// Optimizes shipping routes considering:
/// - Cost (fuel, tolls, customs, handling)
/// - Time (transit time, connection windows)
/// - Reliability (carrier performance, weather, geopolitics)
/// - Sustainability (carbon footprint)
pub struct RouteOptimizer {
    /// Network graph of locations and connections
    network: SupplyChainNetwork,
    /// Carrier performance data
    carrier_metrics: HashMap<ParticipantId, CarrierMetrics>,
    /// Current disruptions
    disruptions: Vec<Disruption>,
    /// Optimization preferences
    preferences: OptimizationPreferences,
}

#[derive(Debug)]
pub struct SupplyChainNetwork {
    /// All nodes (facilities, ports, etc.)
    nodes: HashMap<LocationId, NetworkNode>,
    /// Edges (shipping lanes, routes)
    edges: Vec<NetworkEdge>,
}

#[derive(Debug)]
pub struct NetworkNode {
    pub location: LocationId,
    pub node_type: NodeType,
    pub position: GeoLocation,
    pub handling_time_hours: f64,
    pub cost_per_unit: f64,
}

#[derive(Debug)]
pub enum NodeType {
    Origin,
    Destination,
    Hub,
    Port,
    Airport,
    BorderCrossing,
}

#[derive(Debug)]
pub struct NetworkEdge {
    pub from: LocationId,
    pub to: LocationId,
    pub mode: TransportMode,
    pub carriers: Vec<ParticipantId>,
    pub distance_km: f64,
    pub base_transit_hours: f64,
    pub base_cost_per_kg: f64,
    pub carbon_per_kg_km: f64,
    pub schedule: Option<ScheduleInfo>,
}

#[derive(Debug)]
pub struct ScheduleInfo {
    pub frequency: Frequency,
    pub departures: Vec<chrono::NaiveTime>,
    pub days: Vec<chrono::Weekday>,
}

#[derive(Debug, Clone)]
pub enum Frequency {
    Daily,
    Weekly { days_per_week: u8 },
    Monthly { sailings_per_month: u8 },
    OnDemand,
}

#[derive(Debug)]
pub struct CarrierMetrics {
    pub carrier: ParticipantId,
    pub on_time_percentage: f64,
    pub damage_rate: f64,
    pub average_delay_hours: f64,
    pub tracking_quality: f64,
    pub last_updated: SystemTime,
}

#[derive(Debug, Clone)]
pub struct Disruption {
    pub id: String,
    pub disruption_type: DisruptionType,
    pub affected_locations: Vec<LocationId>,
    pub affected_routes: Vec<(LocationId, LocationId)>,
    pub severity: DisruptionSeverity,
    pub started_at: SystemTime,
    pub expected_end: Option<SystemTime>,
}

#[derive(Debug, Clone)]
pub enum DisruptionType {
    Weather { event: String },
    PortCongestion,
    Strike,
    Geopolitical { description: String },
    Infrastructure { description: String },
    Pandemic,
    NaturalDisaster { event: String },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DisruptionSeverity {
    Minor,      // Delays expected
    Moderate,   // Significant delays, may need rerouting
    Severe,     // Route unusable, must reroute
    Critical,   // Regional shutdown
}

#[derive(Debug, Clone)]
pub struct OptimizationPreferences {
    pub cost_weight: f64,
    pub time_weight: f64,
    pub reliability_weight: f64,
    pub sustainability_weight: f64,
    pub max_transit_days: Option<u32>,
    pub max_cost: Option<f64>,
    pub preferred_carriers: Vec<ParticipantId>,
    pub avoided_countries: Vec<String>,
}

impl Default for OptimizationPreferences {
    fn default() -> Self {
        Self {
            cost_weight: 0.3,
            time_weight: 0.4,
            reliability_weight: 0.2,
            sustainability_weight: 0.1,
            max_transit_days: None,
            max_cost: None,
            preferred_carriers: vec![],
            avoided_countries: vec![],
        }
    }
}

impl RouteOptimizer {
    /// Find optimal route between two locations
    pub fn optimize_route(
        &self,
        origin: &LocationId,
        destination: &LocationId,
        cargo: &CargoProfile,
    ) -> Vec<RouteOption> {
        // Use modified Dijkstra with multiple objectives
        let paths = self.find_all_paths(origin, destination, 5);
        
        let mut options: Vec<_> = paths.into_iter()
            .map(|path| self.evaluate_route(path, cargo))
            .collect();
        
        // Sort by combined score
        options.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap());
        
        options
    }
    
    /// Check for disruptions affecting a shipment
    pub fn check_disruptions(&self, shipment: &Shipment) -> Vec<DisruptionImpact> {
        let mut impacts = Vec::new();
        
        for disruption in &self.disruptions {
            // Check if any leg is affected
            for leg in &shipment.route.legs {
                let route = (leg.origin.clone(), leg.destination.clone());
                if disruption.affected_routes.contains(&route) ||
                   disruption.affected_locations.contains(&leg.origin) ||
                   disruption.affected_locations.contains(&leg.destination) {
                    impacts.push(DisruptionImpact {
                        disruption: disruption.clone(),
                        affected_leg: leg.leg_number,
                        impact: self.estimate_impact(disruption, leg),
                    });
                }
            }
        }
        
        impacts
    }
    
    /// Generate rerouting options for disrupted shipment
    pub fn generate_reroute_options(
        &self,
        shipment: &Shipment,
        from_leg: u32,
    ) -> Vec<RerouteOption> {
        let current_location = &shipment.route.legs[from_leg as usize].origin;
        let destination = &shipment.destination;
        
        // Find alternative routes avoiding disrupted areas
        let cargo = CargoProfile {
            weight_kg: shipment.contents.iter().map(|c| c.weight_kg).sum(),
            volume_cbm: shipment.contents.iter().map(|c| c.volume_cbm).sum(),
            requirements: shipment.contents.first()
                .map(|c| c.handling.clone())
                .unwrap_or(HandlingRequirements {
                    temperature_controlled: None,
                    hazmat_class: None,
                    fragile: false,
                    stackable: true,
                    orientation: None,
                }),
        };
        
        let alternatives = self.optimize_route(current_location, destination, &cargo);
        
        alternatives.into_iter()
            .filter(|opt| !self.route_affected_by_disruptions(&opt.legs))
            .map(|opt| RerouteOption {
                new_route: opt,
                additional_cost: 0.0, // Would calculate
                delay_hours: 0.0,     // Would calculate
            })
            .collect()
    }
    
    fn find_all_paths(
        &self,
        _origin: &LocationId,
        _destination: &LocationId,
        _max_paths: usize,
    ) -> Vec<Vec<LocationId>> {
        // Would implement k-shortest paths algorithm
        vec![]
    }
    
    fn evaluate_route(&self, _path: Vec<LocationId>, _cargo: &CargoProfile) -> RouteOption {
        // Would calculate costs, times, emissions for the path
        RouteOption {
            legs: vec![],
            total_cost: 0.0,
            total_time_hours: 0.0,
            reliability_score: 0.0,
            carbon_kg: 0.0,
            score: 0.0,
        }
    }
    
    fn estimate_impact(&self, disruption: &Disruption, _leg: &RouteLeg) -> ImpactEstimate {
        match disruption.severity {
            DisruptionSeverity::Minor => ImpactEstimate::Delay { hours: 6.0 },
            DisruptionSeverity::Moderate => ImpactEstimate::Delay { hours: 24.0 },
            DisruptionSeverity::Severe => ImpactEstimate::RerouteRequired,
            DisruptionSeverity::Critical => ImpactEstimate::RerouteRequired,
        }
    }
    
    fn route_affected_by_disruptions(&self, _legs: &[RouteOptionLeg]) -> bool {
        false // Would check against active disruptions
    }
}

#[derive(Debug)]
pub struct CargoProfile {
    pub weight_kg: f64,
    pub volume_cbm: f64,
    pub requirements: HandlingRequirements,
}

#[derive(Debug)]
pub struct RouteOption {
    pub legs: Vec<RouteOptionLeg>,
    pub total_cost: f64,
    pub total_time_hours: f64,
    pub reliability_score: f64,
    pub carbon_kg: f64,
    pub score: f64,
}

#[derive(Debug)]
pub struct RouteOptionLeg {
    pub from: LocationId,
    pub to: LocationId,
    pub mode: TransportMode,
    pub carrier: ParticipantId,
    pub cost: f64,
    pub transit_hours: f64,
}

#[derive(Debug)]
pub struct DisruptionImpact {
    pub disruption: Disruption,
    pub affected_leg: u32,
    pub impact: ImpactEstimate,
}

#[derive(Debug)]
pub enum ImpactEstimate {
    Delay { hours: f64 },
    RerouteRequired,
    Blocked,
}

#[derive(Debug)]
pub struct RerouteOption {
    pub new_route: RouteOption,
    pub additional_cost: f64,
    pub delay_hours: f64,
}

// ============================================================================
// Customs and Trade Compliance
// ============================================================================

/// Trade compliance and customs management
pub struct TradeCompliance {
    /// Harmonized System codes
    hs_codes: HashMap<Sku, HsClassification>,
    /// Trade agreements
    trade_agreements: Vec<TradeAgreement>,
    /// Sanctions and restrictions
    restrictions: Vec<TradeRestriction>,
    /// Customs broker network
    brokers: HashMap<String, CustomsBroker>,
}

#[derive(Debug, Clone)]
pub struct HsClassification {
    pub sku: Sku,
    pub hs_code: String,
    pub description: String,
    pub country_variations: HashMap<String, String>,
}

#[derive(Debug, Clone)]
pub struct TradeAgreement {
    pub name: String,
    pub countries: Vec<String>,
    pub benefits: Vec<TradeBenefit>,
    pub requirements: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum TradeBenefit {
    TariffReduction { percentage: f64 },
    TariffElimination,
    QuotaIncrease { amount: u64 },
    SimplifiedClearance,
}

#[derive(Debug, Clone)]
pub struct TradeRestriction {
    pub restriction_type: RestrictionType,
    pub countries: Vec<String>,
    pub products: Vec<String>,
    pub effective_date: chrono::NaiveDate,
}

#[derive(Debug, Clone)]
pub enum RestrictionType {
    Embargo,
    ExportControl { license_required: bool },
    Quota { remaining: u64 },
    Tariff { rate: f64 },
    Antidumping { rate: f64 },
}

#[derive(Debug, Clone)]
pub struct CustomsBroker {
    pub id: String,
    pub name: String,
    pub countries: Vec<String>,
    pub specializations: Vec<String>,
    pub average_clearance_hours: f64,
}

impl TradeCompliance {
    /// Calculate duties and taxes for a shipment
    pub fn calculate_duties(
        &self,
        contents: &[ShipmentContent],
        origin_country: &str,
        destination_country: &str,
    ) -> DutyCalculation {
        let mut total_duty = 0.0;
        let mut line_details = Vec::new();
        
        for content in contents {
            if let Some(hs) = self.hs_codes.get(&content.sku) {
                let hs_code = hs.country_variations
                    .get(destination_country)
                    .unwrap_or(&hs.hs_code);
                
                // Check for trade agreement benefits
                let base_rate = self.get_base_rate(hs_code, destination_country);
                let preferential_rate = self.get_preferential_rate(
                    hs_code, origin_country, destination_country
                );
                
                let applicable_rate = preferential_rate.unwrap_or(base_rate);
                let value = content.quantity as f64 * 100.0; // Would use actual values
                let duty = value * applicable_rate;
                
                total_duty += duty;
                line_details.push(DutyLineDetail {
                    sku: content.sku.clone(),
                    hs_code: hs_code.clone(),
                    value,
                    rate: applicable_rate,
                    duty,
                    preferential: preferential_rate.is_some(),
                });
            }
        }
        
        DutyCalculation {
            total_duty,
            line_details,
            currency: "USD".into(),
        }
    }
    
    /// Check trade restrictions
    pub fn check_restrictions(
        &self,
        sku: &Sku,
        origin_country: &str,
        destination_country: &str,
    ) -> Vec<TradeRestriction> {
        self.restrictions.iter()
            .filter(|r| {
                r.countries.contains(&destination_country.to_string()) ||
                r.countries.contains(&origin_country.to_string())
            })
            .filter(|r| {
                if let Some(hs) = self.hs_codes.get(sku) {
                    r.products.iter().any(|p| hs.hs_code.starts_with(p))
                } else {
                    false
                }
            })
            .cloned()
            .collect()
    }
    
    fn get_base_rate(&self, _hs_code: &str, _country: &str) -> f64 {
        0.05 // Placeholder: 5% default rate
    }
    
    fn get_preferential_rate(
        &self,
        _hs_code: &str,
        origin: &str,
        destination: &str,
    ) -> Option<f64> {
        // Check if there's a trade agreement between countries
        for agreement in &self.trade_agreements {
            if agreement.countries.contains(&origin.to_string()) &&
               agreement.countries.contains(&destination.to_string()) {
                for benefit in &agreement.benefits {
                    if let TradeBenefit::TariffElimination = benefit {
                        return Some(0.0);
                    }
                    if let TradeBenefit::TariffReduction { percentage } = benefit {
                        return Some(0.05 * (1.0 - percentage / 100.0));
                    }
                }
            }
        }
        None
    }
}

#[derive(Debug)]
pub struct DutyCalculation {
    pub total_duty: f64,
    pub line_details: Vec<DutyLineDetail>,
    pub currency: String,
}

#[derive(Debug)]
pub struct DutyLineDetail {
    pub sku: Sku,
    pub hs_code: String,
    pub value: f64,
    pub rate: f64,
    pub duty: f64,
    pub preferential: bool,
}

// ============================================================================
// Supply Chain Metrics
// ============================================================================

/// Metrics for supply chain monitoring
#[derive(Debug, Default)]
pub struct SupplyChainMetrics {
    /// Total active shipments
    pub active_shipments: u64,
    /// On-time delivery rate
    pub on_time_rate: f64,
    /// Average transit time (hours)
    pub avg_transit_hours: f64,
    /// Total inventory value
    pub inventory_value_usd: f64,
    /// Stockout events (last 30 days)
    pub stockout_count: u64,
    /// Active disruptions
    pub active_disruptions: u64,
    /// Carbon footprint (tons CO2 this month)
    pub carbon_footprint_tons: f64,
    /// Perfect order rate
    pub perfect_order_rate: f64,
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_inventory_availability() {
        let mut manager = InventoryManager::new();
        
        let sku = Sku("TEST-001".into());
        let location = LocationId("WAREHOUSE-1".into());
        
        manager.positions.insert((location.clone(), sku.clone()), InventoryPosition {
            location: location.clone(),
            sku: sku.clone(),
            on_hand: 100,
            allocated: 30,
            on_order: 50,
            last_count: SystemTime::now(),
            lot_details: vec![],
        });
        
        let availability = manager.global_availability(&sku);
        
        assert_eq!(availability.total_on_hand, 100);
        assert_eq!(availability.by_location[0].1, 70); // 100 - 30 allocated
    }
    
    #[test]
    fn test_order_priority_ordering() {
        assert!(OrderPriority::Standard < OrderPriority::Express);
        assert!(OrderPriority::Express < OrderPriority::Priority);
        assert!(OrderPriority::Priority < OrderPriority::Emergency);
    }
    
    #[test]
    fn test_inventory_position_projected() {
        let pos = InventoryPosition {
            location: LocationId("test".into()),
            sku: Sku("test".into()),
            on_hand: 100,
            allocated: 30,
            on_order: 50,
            last_count: SystemTime::now(),
            lot_details: vec![],
        };
        
        assert_eq!(pos.available(), 70);
        assert_eq!(pos.projected(), 120); // 100 + 50 - 30
    }
}
