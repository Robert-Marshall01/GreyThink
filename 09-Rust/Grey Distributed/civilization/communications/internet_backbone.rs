//! Internet Backbone Routing and Resilience
//!
//! This module implements civilization-scale internet infrastructure coordination,
//! enabling resilient routing, traffic optimization, and global connectivity
//! management across autonomous systems and internet exchange points.
//!
//! # Architecture Overview
//!
//! Grey Distributed provides a coordination layer above traditional BGP routing,
//! enabling:
//!
//! - **Global route optimization**: Cross-AS path selection based on latency, cost, policy
//! - **DDoS mitigation coordination**: Distributed scrubbing and blackhole routing
//! - **Peering agreement enforcement**: Automated traffic engineering per agreements
//! - **Resilience orchestration**: Coordinated failover during outages
//!
//! # Network Topology Model
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────────────────┐
//! │                    Global Internet Backbone                             │
//! │                                                                         │
//! │   ┌─────────────────────┐        ┌─────────────────────┐               │
//! │   │    Tier 1 Transit   │◄──────►│    Tier 1 Transit   │               │
//! │   │     (Americas)      │        │      (Europe)       │               │
//! │   └──────────┬──────────┘        └──────────┬──────────┘               │
//! │              │                              │                          │
//! │   ┌──────────┴──────────┐        ┌──────────┴──────────┐               │
//! │   │         IXP         │        │         IXP         │               │
//! │   │     (Equinix DC)    │        │      (DE-CIX)       │               │
//! │   └──────────┬──────────┘        └──────────┬──────────┘               │
//! │              │                              │                          │
//! │   ┌──────────┼──────────┐        ┌──────────┼──────────┐               │
//! │   ▼          ▼          ▼        ▼          ▼          ▼               │
//! │ ┌───┐      ┌───┐      ┌───┐    ┌───┐      ┌───┐      ┌───┐            │
//! │ │ISP│      │CDN│      │CSP│    │ISP│      │CDN│      │ENT│            │
//! │ └───┘      └───┘      └───┘    └───┘      └───┘      └───┘            │
//! └─────────────────────────────────────────────────────────────────────────┘
//!
//! Grey Distributed nodes are deployed at:
//! - Major IXPs for peering coordination
//! - Tier 1 transit for global routing
//! - Regional PoPs for local optimization
//! ```

use std::collections::{BTreeMap, HashMap, HashSet};
use std::net::{IpAddr, Ipv4Addr, Ipv6Addr};
use std::sync::Arc;
use std::time::{Duration, SystemTime};

// ============================================================================
// Core Types
// ============================================================================

/// Autonomous System Number
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Asn(pub u32);

impl Asn {
    pub fn is_private(&self) -> bool {
        (64512..=65534).contains(&self.0) || (4200000000..=4294967294).contains(&self.0)
    }
}

/// IP prefix (CIDR notation)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IpPrefix {
    pub network: IpAddr,
    pub prefix_len: u8,
}

impl IpPrefix {
    pub fn v4(a: u8, b: u8, c: u8, d: u8, len: u8) -> Self {
        Self {
            network: IpAddr::V4(Ipv4Addr::new(a, b, c, d)),
            prefix_len: len,
        }
    }
    
    pub fn contains(&self, addr: &IpAddr) -> bool {
        match (&self.network, addr) {
            (IpAddr::V4(net), IpAddr::V4(addr)) => {
                let mask = !0u32 << (32 - self.prefix_len);
                (u32::from(*net) & mask) == (u32::from(*addr) & mask)
            }
            (IpAddr::V6(net), IpAddr::V6(addr)) => {
                let mask = !0u128 << (128 - self.prefix_len);
                (u128::from(*net) & mask) == (u128::from(*addr) & mask)
            }
            _ => false,
        }
    }
}

/// Internet Exchange Point identifier
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IxpId(pub String);

/// Physical Point of Presence
#[derive(Debug, Clone)]
pub struct PointOfPresence {
    pub id: String,
    pub location: GeoLocation,
    pub facility: String,
    pub asn: Asn,
    pub capacity_gbps: u32,
    pub connected_ixps: Vec<IxpId>,
}

#[derive(Debug, Clone)]
pub struct GeoLocation {
    pub latitude: f64,
    pub longitude: f64,
    pub city: String,
    pub country: String,
}

// ============================================================================
// BGP Route Management
// ============================================================================

/// A BGP route announcement
#[derive(Debug, Clone)]
pub struct BgpRoute {
    pub prefix: IpPrefix,
    pub origin_asn: Asn,
    pub as_path: Vec<Asn>,
    pub next_hop: IpAddr,
    pub local_pref: u32,
    pub med: u32,
    pub communities: Vec<BgpCommunity>,
    pub origin_type: OriginType,
    pub received_at: SystemTime,
    pub source: RouteSource,
}

/// BGP community tags (standard and extended)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BgpCommunity {
    /// Standard community (4 bytes)
    Standard { asn: u16, value: u16 },
    /// Extended community (8 bytes)
    Extended { type_high: u8, type_low: u8, value: [u8; 6] },
    /// Large community (12 bytes, RFC 8092)
    Large { global_admin: u32, local_data1: u32, local_data2: u32 },
}

/// Well-known communities
impl BgpCommunity {
    pub fn no_export() -> Self { Self::Standard { asn: 0xFFFF, value: 0xFF01 } }
    pub fn no_advertise() -> Self { Self::Standard { asn: 0xFFFF, value: 0xFF02 } }
    pub fn no_peer() -> Self { Self::Standard { asn: 0xFFFF, value: 0xFF04 } }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OriginType {
    Igp,
    Egp,
    Incomplete,
}

#[derive(Debug, Clone)]
pub enum RouteSource {
    Ebgp { peer_asn: Asn, peer_ip: IpAddr },
    Ibgp { router_id: IpAddr },
    Static,
    Connected,
}

/// Route validation status (RPKI)
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RpkiStatus {
    Valid,
    Invalid,
    NotFound,
    Unknown,
}

// ============================================================================
// Global Routing Table
// ============================================================================

/// Distributed global routing information base
/// 
/// Grey Distributed maintains a synchronized view of global routing across
/// all participating networks, enabling coordinated traffic engineering
/// that isn't possible with traditional decentralized BGP.
pub struct GlobalRib {
    /// All known routes indexed by prefix
    routes: HashMap<IpPrefix, Vec<BgpRoute>>,
    /// Best route per prefix
    best_routes: HashMap<IpPrefix, BgpRoute>,
    /// RPKI validation cache
    rpki_cache: RpkiCache,
    /// Route selection policy
    policy: RouteSelectionPolicy,
}

#[derive(Debug, Clone)]
pub struct RouteSelectionPolicy {
    /// Prefer routes from these ASNs
    pub preferred_origins: HashSet<Asn>,
    /// Avoid routes through these ASNs
    pub avoided_asns: HashSet<Asn>,
    /// Maximum AS path length to consider
    pub max_as_path_length: usize,
    /// Require RPKI valid routes when available
    pub prefer_rpki_valid: bool,
    /// Weight factors for route scoring
    pub weight_factors: RouteWeights,
}

#[derive(Debug, Clone)]
pub struct RouteWeights {
    pub local_pref: f64,
    pub as_path_length: f64,
    pub rpki_bonus: f64,
    pub latency: f64,
    pub cost: f64,
}

impl Default for RouteWeights {
    fn default() -> Self {
        Self {
            local_pref: 1.0,
            as_path_length: 0.5,
            rpki_bonus: 0.3,
            latency: 0.4,
            cost: 0.2,
        }
    }
}

pub struct RpkiCache {
    /// Validated ROA entries
    roas: HashMap<IpPrefix, Vec<RoaEntry>>,
    /// Last refresh time
    last_refresh: SystemTime,
}

#[derive(Debug, Clone)]
pub struct RoaEntry {
    pub prefix: IpPrefix,
    pub max_length: u8,
    pub asn: Asn,
    pub trust_anchor: String,
}

impl GlobalRib {
    /// Insert or update a route
    pub fn update_route(&mut self, route: BgpRoute) {
        let prefix = route.prefix.clone();
        
        // Add to all routes
        let routes = self.routes.entry(prefix.clone()).or_insert_with(Vec::new);
        
        // Remove existing route from same source if present
        routes.retain(|r| !self.same_source(&r.source, &route.source));
        routes.push(route);
        
        // Recalculate best route
        if let Some(best) = self.select_best_route(&prefix) {
            self.best_routes.insert(prefix, best);
        }
    }
    
    /// Select best route using scoring algorithm
    fn select_best_route(&self, prefix: &IpPrefix) -> Option<BgpRoute> {
        self.routes.get(prefix)?.iter()
            .filter(|r| self.is_route_acceptable(r))
            .max_by(|a, b| {
                let score_a = self.score_route(a);
                let score_b = self.score_route(b);
                score_a.partial_cmp(&score_b).unwrap()
            })
            .cloned()
    }
    
    fn is_route_acceptable(&self, route: &BgpRoute) -> bool {
        // Check path length
        if route.as_path.len() > self.policy.max_as_path_length {
            return false;
        }
        
        // Check for avoided ASNs
        for asn in &route.as_path {
            if self.policy.avoided_asns.contains(asn) {
                return false;
            }
        }
        
        true
    }
    
    fn score_route(&self, route: &BgpRoute) -> f64 {
        let w = &self.policy.weight_factors;
        
        let mut score = 0.0;
        
        // Local preference (higher is better)
        score += (route.local_pref as f64 / 100.0) * w.local_pref;
        
        // AS path length (shorter is better)
        let path_score = 1.0 - (route.as_path.len() as f64 / 20.0).min(1.0);
        score += path_score * w.as_path_length;
        
        // RPKI bonus
        if self.policy.prefer_rpki_valid {
            let rpki_status = self.validate_rpki(route);
            if rpki_status == RpkiStatus::Valid {
                score += w.rpki_bonus;
            }
        }
        
        // Preferred origin bonus
        if self.policy.preferred_origins.contains(&route.origin_asn) {
            score += 0.5;
        }
        
        score
    }
    
    fn validate_rpki(&self, route: &BgpRoute) -> RpkiStatus {
        if let Some(roas) = self.rpki_cache.roas.get(&route.prefix) {
            for roa in roas {
                if roa.asn == route.origin_asn && route.prefix.prefix_len <= roa.max_length {
                    return RpkiStatus::Valid;
                }
            }
            RpkiStatus::Invalid
        } else {
            RpkiStatus::NotFound
        }
    }
    
    fn same_source(&self, a: &RouteSource, b: &RouteSource) -> bool {
        match (a, b) {
            (RouteSource::Ebgp { peer_ip: a, .. }, RouteSource::Ebgp { peer_ip: b, .. }) => a == b,
            (RouteSource::Ibgp { router_id: a }, RouteSource::Ibgp { router_id: b }) => a == b,
            _ => false,
        }
    }
}

// ============================================================================
// Traffic Engineering
// ============================================================================

/// Traffic engineering controller
/// 
/// Manages traffic distribution across multiple paths to optimize for
/// latency, cost, and resilience. Works in conjunction with SDN controllers
/// and traditional BGP routers.
pub struct TrafficEngineer {
    /// Current traffic matrix (source → destination → demand)
    traffic_matrix: HashMap<(Asn, Asn), TrafficDemand>,
    /// Path options between AS pairs
    path_database: PathDatabase,
    /// Link capacities and utilization
    link_state: HashMap<LinkId, LinkState>,
    /// TE objectives
    objectives: TeObjectives,
}

#[derive(Debug, Clone)]
pub struct TrafficDemand {
    pub source: Asn,
    pub destination: Asn,
    pub avg_rate_gbps: f64,
    pub peak_rate_gbps: f64,
    pub latency_requirement_ms: Option<f64>,
    pub priority: TrafficPriority,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TrafficPriority {
    BestEffort,
    Standard,
    Premium,
    Critical,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LinkId {
    pub from: Asn,
    pub to: Asn,
    pub interface: String,
}

#[derive(Debug, Clone)]
pub struct LinkState {
    pub capacity_gbps: f64,
    pub current_utilization: f64,
    pub latency_ms: f64,
    pub packet_loss_rate: f64,
    pub cost_per_gbps: f64,
    pub status: LinkStatus,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LinkStatus {
    Up,
    Degraded,
    Down,
    Maintenance,
}

#[derive(Debug, Clone)]
pub struct TeObjectives {
    /// Maximum link utilization before spreading
    pub max_link_utilization: f64,
    /// Optimize for latency vs cost
    pub latency_weight: f64,
    pub cost_weight: f64,
    /// Minimum path diversity for resilience
    pub min_path_diversity: usize,
}

pub struct PathDatabase {
    /// All paths between AS pairs
    paths: HashMap<(Asn, Asn), Vec<TePath>>,
}

#[derive(Debug, Clone)]
pub struct TePath {
    pub id: String,
    pub hops: Vec<TeHop>,
    pub total_latency_ms: f64,
    pub total_cost: f64,
    pub available_bandwidth_gbps: f64,
}

#[derive(Debug, Clone)]
pub struct TeHop {
    pub asn: Asn,
    pub ingress_interface: String,
    pub egress_interface: String,
    pub local_latency_ms: f64,
}

impl TrafficEngineer {
    /// Compute optimal traffic distribution
    /// 
    /// Uses multi-commodity flow optimization to distribute traffic
    /// across available paths while respecting constraints.
    pub fn optimize(&self) -> TrafficAllocation {
        let mut allocation = TrafficAllocation::default();
        
        for (key, demand) in &self.traffic_matrix {
            if let Some(paths) = self.path_database.paths.get(key) {
                let selected = self.select_paths_for_demand(demand, paths);
                allocation.allocations.insert(key.clone(), selected);
            }
        }
        
        allocation
    }
    
    fn select_paths_for_demand(
        &self,
        demand: &TrafficDemand,
        paths: &[TePath],
    ) -> Vec<PathAllocation> {
        // Filter paths meeting latency requirement
        let eligible: Vec<_> = paths.iter()
            .filter(|p| {
                demand.latency_requirement_ms
                    .map(|req| p.total_latency_ms <= req)
                    .unwrap_or(true)
            })
            .filter(|p| p.available_bandwidth_gbps > 0.0)
            .collect();
        
        if eligible.is_empty() {
            return vec![];
        }
        
        // Sort by combined score (latency + cost weighted)
        let mut scored: Vec<_> = eligible.iter()
            .map(|p| {
                let score = p.total_latency_ms * self.objectives.latency_weight
                    + p.total_cost * self.objectives.cost_weight;
                (*p, score)
            })
            .collect();
        scored.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());
        
        // Allocate traffic across top N paths (for resilience)
        let mut remaining = demand.avg_rate_gbps;
        let mut allocations = Vec::new();
        
        for (path, _) in scored.iter().take(self.objectives.min_path_diversity) {
            if remaining <= 0.0 {
                break;
            }
            
            let alloc = remaining.min(path.available_bandwidth_gbps);
            allocations.push(PathAllocation {
                path_id: path.id.clone(),
                allocated_gbps: alloc,
            });
            remaining -= alloc;
        }
        
        allocations
    }
}

#[derive(Debug, Default)]
pub struct TrafficAllocation {
    pub allocations: HashMap<(Asn, Asn), Vec<PathAllocation>>,
}

#[derive(Debug, Clone)]
pub struct PathAllocation {
    pub path_id: String,
    pub allocated_gbps: f64,
}

// ============================================================================
// DDoS Mitigation Coordination
// ============================================================================

/// Coordinated DDoS mitigation across the global network
/// 
/// Grey Distributed enables rapid, coordinated response to DDoS attacks by:
/// - Sharing attack intelligence across networks
/// - Coordinating scrubbing center activation
/// - Synchronizing blackhole routes
/// - Distributing filtering rules globally
pub struct DdosMitigation {
    /// Active attack tracking
    active_attacks: HashMap<AttackId, DdosAttack>,
    /// Scrubbing centers
    scrubbing_centers: Vec<ScrubbingCenter>,
    /// Coordination state
    coordination: MitigationCoordination,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AttackId(pub [u8; 16]);

#[derive(Debug, Clone)]
pub struct DdosAttack {
    pub id: AttackId,
    pub target: IpPrefix,
    pub attack_type: AttackType,
    pub estimated_volume_gbps: f64,
    pub source_distribution: AttackSourceDist,
    pub started_at: SystemTime,
    pub mitigation: MitigationState,
}

#[derive(Debug, Clone)]
pub enum AttackType {
    VolumetricFlood { protocol: String },
    SynFlood,
    AmplificationAttack { amplifier_protocol: String },
    ApplicationLayer { target_service: String },
    MultiVector { components: Vec<String> },
}

#[derive(Debug, Clone)]
pub struct AttackSourceDist {
    /// Distribution by country
    pub by_country: HashMap<String, f64>,
    /// Distribution by ASN
    pub by_asn: HashMap<Asn, f64>,
    /// Estimated number of source IPs
    pub source_count: u64,
}

#[derive(Debug, Clone)]
pub enum MitigationState {
    Detecting,
    Analyzing,
    Mitigating { strategy: MitigationStrategy },
    Monitoring,
    Resolved,
}

#[derive(Debug, Clone)]
pub enum MitigationStrategy {
    /// Remote triggered blackhole
    Blackhole { upstream_asns: Vec<Asn> },
    /// Traffic scrubbing
    Scrubbing { center_ids: Vec<String> },
    /// FlowSpec filtering
    FlowSpec { rules: Vec<FlowSpecRule> },
    /// Rate limiting
    RateLimiting { limits: HashMap<String, u64> },
    /// Combined approach
    Combined { strategies: Vec<MitigationStrategy> },
}

#[derive(Debug, Clone)]
pub struct FlowSpecRule {
    pub destination: IpPrefix,
    pub source: Option<IpPrefix>,
    pub protocol: Option<u8>,
    pub port: Option<u16>,
    pub action: FlowSpecAction,
}

#[derive(Debug, Clone)]
pub enum FlowSpecAction {
    Discard,
    RateLimit { bps: u64 },
    Redirect { next_hop: IpAddr },
    MarkDscp { value: u8 },
}

#[derive(Debug, Clone)]
pub struct ScrubbingCenter {
    pub id: String,
    pub location: GeoLocation,
    pub capacity_gbps: f64,
    pub current_load_gbps: f64,
    pub techniques: Vec<String>,
    pub status: ScrubbingStatus,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ScrubbingStatus {
    Idle,
    Active,
    HighLoad,
    Full,
    Maintenance,
}

pub struct MitigationCoordination {
    /// Participating networks
    participants: HashSet<Asn>,
    /// Shared intelligence
    threat_intel: ThreatIntelligence,
}

pub struct ThreatIntelligence {
    /// Known bad prefixes
    known_bad_prefixes: HashSet<IpPrefix>,
    /// Known amplifiers
    known_amplifiers: HashMap<IpAddr, AmplifierInfo>,
    /// Attack signatures
    signatures: Vec<AttackSignature>,
}

#[derive(Debug, Clone)]
pub struct AmplifierInfo {
    pub address: IpAddr,
    pub protocol: String,
    pub amplification_factor: f64,
    pub last_seen: SystemTime,
}

#[derive(Debug, Clone)]
pub struct AttackSignature {
    pub id: String,
    pub pattern: String,
    pub severity: u8,
}

impl DdosMitigation {
    /// Detect and analyze potential attack
    pub fn analyze_traffic(&self, sample: &TrafficSample) -> Option<DdosAttack> {
        // Check for anomalies
        if sample.pps > sample.baseline_pps * 10.0 {
            // Potential attack detected
            Some(DdosAttack {
                id: AttackId(rand::random()),
                target: sample.destination_prefix.clone(),
                attack_type: self.classify_attack(sample),
                estimated_volume_gbps: sample.bps / 1_000_000_000.0,
                source_distribution: self.analyze_sources(sample),
                started_at: SystemTime::now(),
                mitigation: MitigationState::Analyzing,
            })
        } else {
            None
        }
    }
    
    /// Select optimal mitigation strategy
    pub fn select_strategy(&self, attack: &DdosAttack) -> MitigationStrategy {
        match &attack.attack_type {
            AttackType::VolumetricFlood { .. } if attack.estimated_volume_gbps > 100.0 => {
                // Large volumetric: use scrubbing + blackhole at source
                MitigationStrategy::Combined {
                    strategies: vec![
                        self.select_scrubbing_centers(attack),
                        MitigationStrategy::Blackhole {
                            upstream_asns: self.select_blackhole_upstreams(attack),
                        },
                    ],
                }
            }
            AttackType::ApplicationLayer { .. } => {
                // App layer: use FlowSpec for precision filtering
                MitigationStrategy::FlowSpec {
                    rules: self.generate_flowspec_rules(attack),
                }
            }
            _ => {
                // Default: scrubbing
                self.select_scrubbing_centers(attack)
            }
        }
    }
    
    fn classify_attack(&self, sample: &TrafficSample) -> AttackType {
        // Simplified classification logic
        if sample.syn_ratio > 0.9 {
            AttackType::SynFlood
        } else if sample.fragmented_ratio > 0.5 {
            AttackType::VolumetricFlood { protocol: "UDP".into() }
        } else {
            AttackType::MultiVector { components: vec!["unknown".into()] }
        }
    }
    
    fn analyze_sources(&self, _sample: &TrafficSample) -> AttackSourceDist {
        AttackSourceDist {
            by_country: HashMap::new(),
            by_asn: HashMap::new(),
            source_count: 0,
        }
    }
    
    fn select_scrubbing_centers(&self, attack: &DdosAttack) -> MitigationStrategy {
        let available: Vec<_> = self.scrubbing_centers.iter()
            .filter(|c| matches!(c.status, ScrubbingStatus::Idle | ScrubbingStatus::Active))
            .filter(|c| c.current_load_gbps + attack.estimated_volume_gbps <= c.capacity_gbps)
            .map(|c| c.id.clone())
            .collect();
        
        MitigationStrategy::Scrubbing { center_ids: available }
    }
    
    fn select_blackhole_upstreams(&self, _attack: &DdosAttack) -> Vec<Asn> {
        vec![] // Would analyze attack sources and select upstream ASNs
    }
    
    fn generate_flowspec_rules(&self, attack: &DdosAttack) -> Vec<FlowSpecRule> {
        vec![FlowSpecRule {
            destination: attack.target.clone(),
            source: None,
            protocol: None,
            port: None,
            action: FlowSpecAction::RateLimit { bps: 1_000_000 },
        }]
    }
}

#[derive(Debug)]
pub struct TrafficSample {
    pub destination_prefix: IpPrefix,
    pub bps: f64,
    pub pps: f64,
    pub baseline_pps: f64,
    pub syn_ratio: f64,
    pub fragmented_ratio: f64,
}

// ============================================================================
// Peering Management
// ============================================================================

/// Peering agreement management and enforcement
/// 
/// Automates traffic engineering based on peering agreements, ensuring
/// traffic flows according to contractual obligations.
pub struct PeeringManager {
    /// All peering agreements
    agreements: HashMap<PeeringKey, PeeringAgreement>,
    /// Traffic accounting
    accounting: TrafficAccounting,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PeeringKey {
    pub local_asn: Asn,
    pub peer_asn: Asn,
    pub ixp: Option<IxpId>,
}

#[derive(Debug, Clone)]
pub struct PeeringAgreement {
    pub key: PeeringKey,
    pub agreement_type: PeeringType,
    pub terms: PeeringTerms,
    pub status: PeeringStatus,
    pub effective_date: chrono::NaiveDate,
    pub expiry_date: Option<chrono::NaiveDate>,
}

#[derive(Debug, Clone)]
pub enum PeeringType {
    /// Free, settlement-free peering
    SettlementFree,
    /// Paid transit
    PaidTransit { monthly_cost: f64, currency: String },
    /// Partial transit (specific prefixes only)
    PartialTransit { allowed_prefixes: Vec<IpPrefix> },
    /// Route server peering at IXP
    RouteServer,
}

#[derive(Debug, Clone)]
pub struct PeeringTerms {
    /// Maximum traffic ratio (in/out)
    pub max_ratio: Option<f64>,
    /// Minimum traffic commitment
    pub min_traffic_gbps: Option<f64>,
    /// Maximum traffic limit
    pub max_traffic_gbps: Option<f64>,
    /// Prefix limits
    pub max_prefixes_v4: u32,
    pub max_prefixes_v6: u32,
    /// Community requirements
    pub required_communities: Vec<BgpCommunity>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PeeringStatus {
    Active,
    Pending,
    Suspended,
    Terminated,
}

pub struct TrafficAccounting {
    /// Hourly traffic samples
    samples: HashMap<PeeringKey, Vec<TrafficSnapshot>>,
    /// Current billing period totals
    period_totals: HashMap<PeeringKey, PeriodTotal>,
}

#[derive(Debug, Clone)]
pub struct TrafficSnapshot {
    pub timestamp: SystemTime,
    pub inbound_bytes: u64,
    pub outbound_bytes: u64,
    pub inbound_packets: u64,
    pub outbound_packets: u64,
}

#[derive(Debug, Clone)]
pub struct PeriodTotal {
    pub period_start: chrono::NaiveDate,
    pub total_inbound_tb: f64,
    pub total_outbound_tb: f64,
    pub peak_inbound_gbps: f64,
    pub peak_outbound_gbps: f64,
}

impl PeeringManager {
    /// Check if peering agreement allows specific traffic
    pub fn is_traffic_allowed(
        &self,
        local_asn: Asn,
        peer_asn: Asn,
        prefix: &IpPrefix,
        direction: TrafficDirection,
    ) -> bool {
        let key = PeeringKey { local_asn, peer_asn, ixp: None };
        
        if let Some(agreement) = self.agreements.get(&key) {
            if agreement.status != PeeringStatus::Active {
                return false;
            }
            
            // Check prefix restrictions
            if let PeeringType::PartialTransit { allowed_prefixes } = &agreement.agreement_type {
                if !allowed_prefixes.iter().any(|p| self.prefix_matches(p, prefix)) {
                    return false;
                }
            }
            
            true
        } else {
            false
        }
    }
    
    /// Check agreement compliance
    pub fn check_compliance(&self, key: &PeeringKey) -> ComplianceStatus {
        let Some(agreement) = self.agreements.get(key) else {
            return ComplianceStatus::Unknown;
        };
        
        let Some(totals) = self.accounting.period_totals.get(key) else {
            return ComplianceStatus::Unknown;
        };
        
        // Check ratio
        if let Some(max_ratio) = agreement.terms.max_ratio {
            let current_ratio = if totals.total_outbound_tb > 0.0 {
                totals.total_inbound_tb / totals.total_outbound_tb
            } else {
                0.0
            };
            
            if current_ratio > max_ratio {
                return ComplianceStatus::Violation {
                    reason: format!("Ratio {:.2} exceeds max {:.2}", current_ratio, max_ratio),
                };
            }
        }
        
        // Check traffic limits
        if let Some(max_gbps) = agreement.terms.max_traffic_gbps {
            if totals.peak_inbound_gbps > max_gbps || totals.peak_outbound_gbps > max_gbps {
                return ComplianceStatus::Warning {
                    reason: "Peak traffic approaching limit".into(),
                };
            }
        }
        
        ComplianceStatus::Compliant
    }
    
    fn prefix_matches(&self, allowed: &IpPrefix, actual: &IpPrefix) -> bool {
        allowed.contains(&actual.network) && actual.prefix_len >= allowed.prefix_len
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TrafficDirection {
    Inbound,
    Outbound,
}

#[derive(Debug)]
pub enum ComplianceStatus {
    Compliant,
    Warning { reason: String },
    Violation { reason: String },
    Unknown,
}

// ============================================================================
// Backbone Metrics
// ============================================================================

/// Metrics for backbone health monitoring
#[derive(Debug, Default)]
pub struct BackboneMetrics {
    /// Total prefixes in global RIB
    pub total_prefixes_v4: u64,
    pub total_prefixes_v6: u64,
    /// Active BGP sessions
    pub active_sessions: u64,
    /// Global traffic volume
    pub total_traffic_tbps: f64,
    /// Average global latency
    pub avg_latency_ms: f64,
    /// Active DDoS attacks
    pub active_attacks: u64,
    /// Scrubbing capacity utilization
    pub scrubbing_utilization: f64,
    /// RPKI coverage percentage
    pub rpki_coverage: f64,
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_prefix_contains() {
        let prefix = IpPrefix::v4(192, 168, 0, 0, 16);
        
        assert!(prefix.contains(&IpAddr::V4(Ipv4Addr::new(192, 168, 1, 1))));
        assert!(prefix.contains(&IpAddr::V4(Ipv4Addr::new(192, 168, 255, 255))));
        assert!(!prefix.contains(&IpAddr::V4(Ipv4Addr::new(192, 169, 0, 1))));
    }
    
    #[test]
    fn test_private_asn() {
        assert!(!Asn(64511).is_private());
        assert!(Asn(64512).is_private());
        assert!(Asn(65534).is_private());
        assert!(!Asn(65535).is_private());
    }
    
    #[test]
    fn test_peering_compliance() {
        let manager = PeeringManager {
            agreements: HashMap::new(),
            accounting: TrafficAccounting {
                samples: HashMap::new(),
                period_totals: HashMap::new(),
            },
        };
        
        let status = manager.check_compliance(&PeeringKey {
            local_asn: Asn(1),
            peer_asn: Asn(2),
            ixp: None,
        });
        
        assert!(matches!(status, ComplianceStatus::Unknown));
    }
}
