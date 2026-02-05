//! Grey Distributed — Lunar Base Coordination
//!
//! Coordination logic for Grey Distributed nodes operating in lunar bases.
//! Handles moderate latency (1.3s one-way), lunar far side isolation, and
//! extreme temperature cycling.

use std::collections::HashMap;
use std::time::Duration;

// =============================================================================
// LUNAR ENVIRONMENT CONSTRAINTS
// =============================================================================
//
// The Moon presents distinct challenges compared to Mars:
//
// 1. LATENCY
//    - Earth-Moon: ~1.3 seconds one-way (constant)
//    - Real-time interaction possible but awkward
//    - Near-real-time consensus feasible
//
// 2. FAR SIDE ISOLATION
//    - Lunar far side has no direct Earth line-of-sight
//    - Requires relay satellites or surface relays
//    - Potential for complete isolation if relays fail
//
// 3. TEMPERATURE EXTREMES
//    - Day: +127°C (253°F)
//    - Night: -173°C (-280°F)  
//    - 14-day day/night cycle at equator
//    - Permanently shadowed craters near poles
//
// 4. RESOURCE CONSIDERATIONS
//    - Solar power: 14 days on, 14 days off (equator)
//    - Polar regions: Near-constant sunlight possible
//    - Ice deposits in shadowed craters
//    - Shorter resupply (3-day transit vs 6+ months)
//
// =============================================================================

/// Lunar location classification
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LunarRegion {
    /// Near side, visible from Earth
    NearSide,
    /// Far side, requires relay
    FarSide,
    /// North polar region
    NorthPole,
    /// South polar region
    SouthPole,
}

impl LunarRegion {
    /// Does this region have direct Earth line-of-sight?
    pub fn has_earth_los(&self) -> bool {
        matches!(self, LunarRegion::NearSide)
    }
    
    /// Day/night cycle duration
    pub fn solar_cycle(&self) -> SolarCycle {
        match self {
            LunarRegion::NearSide | LunarRegion::FarSide => {
                SolarCycle::Standard { 
                    day_duration: Duration::from_secs(14 * 24 * 3600),
                    night_duration: Duration::from_secs(14 * 24 * 3600),
                }
            }
            LunarRegion::NorthPole | LunarRegion::SouthPole => {
                SolarCycle::Polar {
                    // Some locations have near-continuous sunlight
                    darkness_ratio: 0.2,
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum SolarCycle {
    Standard {
        day_duration: Duration,
        night_duration: Duration,
    },
    Polar {
        darkness_ratio: f64,
    },
}

/// Communication path to Earth
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EarthCommunicationPath {
    /// Direct line-of-sight to Earth
    Direct,
    /// Via lunar orbital relay
    OrbitalRelay,
    /// Via surface relay network
    SurfaceRelay,
    /// Via Earth-Sun L2 relay (far side)
    LagrangeRelay,
    /// No path available
    Unavailable,
}

impl EarthCommunicationPath {
    /// Additional latency introduced by this path
    pub fn additional_latency(&self) -> Duration {
        match self {
            Self::Direct => Duration::from_millis(0),
            Self::OrbitalRelay => Duration::from_millis(50),
            Self::SurfaceRelay => Duration::from_millis(100),
            Self::LagrangeRelay => Duration::from_millis(500),
            Self::Unavailable => Duration::from_secs(u64::MAX),
        }
    }
    
    /// Total one-way latency to Earth
    pub fn total_latency(&self) -> Duration {
        let base = Duration::from_millis(1300); // ~1.3 seconds
        base + self.additional_latency()
    }
}

/// Lunar base configuration
#[derive(Debug, Clone)]
pub struct LunarBaseConfig {
    /// Base identifier
    pub base_id: BaseId,
    
    /// Base name
    pub name: String,
    
    /// Lunar region
    pub region: LunarRegion,
    
    /// Precise location
    pub location: LunarLocation,
    
    /// Nodes in this base
    pub nodes: Vec<LunarNodeConfig>,
    
    /// Earth communication paths
    pub earth_paths: Vec<EarthCommunicationPath>,
    
    /// Federated bases
    pub federated_bases: Vec<BaseId>,
    
    /// Power configuration
    pub power_config: PowerConfig,
    
    /// Thermal management
    pub thermal_config: ThermalConfig,
}

pub type BaseId = String;

#[derive(Debug, Clone)]
pub struct LunarLocation {
    /// Latitude (-90 to 90)
    pub latitude: f64,
    /// Longitude (-180 to 180)
    pub longitude: f64,
    /// Elevation relative to mean radius
    pub elevation_m: f64,
    /// Named location (if any)
    pub named_location: Option<String>,
}

#[derive(Debug, Clone)]
pub struct LunarNodeConfig {
    /// Node identifier
    pub node_id: String,
    
    /// Node role
    pub role: LunarNodeRole,
    
    /// Thermal protection level
    pub thermal_protection: ThermalProtection,
    
    /// Radiation hardening
    pub rad_hardening: RadHardening,
    
    /// Storage capacity (bytes)
    pub storage_capacity: u64,
    
    /// Is this node in a permanently shadowed region?
    pub in_shadow: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LunarNodeRole {
    /// Primary base infrastructure
    Primary,
    /// Backup/redundancy
    Backup,
    /// Earth communication gateway
    EarthGateway,
    /// Far-side relay node
    FarSideRelay,
    /// Mining operations support
    MiningSupport,
    /// Research facility support
    Research,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ThermalProtection {
    /// Basic lunar-rated
    Basic,
    /// Enhanced for extreme cycling
    Enhanced,
    /// Underground/shielded
    Underground,
    /// Active cooling/heating
    Active,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RadHardening {
    Standard,
    Enhanced,
    SpaceGrade,
}

#[derive(Debug, Clone)]
pub struct PowerConfig {
    /// Primary power source
    pub primary: PowerSource,
    
    /// Backup power
    pub backup: Option<PowerSource>,
    
    /// Energy storage (watt-hours)
    pub storage_wh: u64,
    
    /// Night operation capability
    pub night_capable: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PowerSource {
    /// Solar panels
    Solar,
    /// Nuclear (RTG or reactor)
    Nuclear,
    /// Fuel cells
    FuelCell,
    /// Grid (base power grid)
    Grid,
}

#[derive(Debug, Clone)]
pub struct ThermalConfig {
    /// Operating temperature range
    pub temp_range: (f64, f64),
    
    /// Active thermal control
    pub active_control: bool,
    
    /// Radiator capacity
    pub radiator_capacity_w: u64,
}

// =============================================================================
// LUNAR CLUSTER CONSENSUS
// =============================================================================

/// Lunar-adapted consensus protocol
/// 
/// Unlike Mars, the Moon's proximity allows near-real-time Earth coordination.
/// However, far-side bases still need autonomous operation capability.
pub struct LunarConsensus {
    /// Base configuration
    config: LunarBaseConfig,
    
    /// Current Earth communication status
    earth_status: EarthConnectionStatus,
    
    /// Local consensus state
    local_state: LocalConsensusState,
    
    /// Earth sync state
    earth_sync: EarthSyncState,
    
    /// Inter-base sync state
    base_sync: HashMap<BaseId, BaseSyncState>,
    
    /// Current solar phase
    solar_phase: SolarPhase,
}

#[derive(Debug, Clone)]
pub struct EarthConnectionStatus {
    /// Current communication path
    pub path: EarthCommunicationPath,
    
    /// Connection quality
    pub quality: ConnectionQuality,
    
    /// Current measured latency
    pub measured_latency: Duration,
    
    /// Available bandwidth (bits per second)
    pub bandwidth_bps: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConnectionQuality {
    Excellent,
    Good,
    Degraded,
    Poor,
    Unavailable,
}

#[derive(Debug, Clone)]
pub struct LocalConsensusState {
    pub term: u64,
    pub leader: Option<String>,
    pub commit_index: u64,
    pub health: ConsensusHealth,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConsensusHealth {
    Healthy,
    Degraded,
    Recovering,
    Failed,
}

#[derive(Debug, Clone)]
pub struct EarthSyncState {
    pub last_sync: Option<std::time::Instant>,
    pub pending_updates: u64,
    pub status: SyncStatus,
    pub latency_history: Vec<Duration>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyncStatus {
    Synced,
    Syncing,
    Behind,
    Isolated,
    Conflict,
}

#[derive(Debug, Clone)]
pub struct BaseSyncState {
    pub base_id: BaseId,
    pub last_sync: Option<std::time::Instant>,
    pub latency: Duration,
    pub status: SyncStatus,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SolarPhase {
    /// Lunar day (solar powered)
    Day,
    /// Lunar night (battery/nuclear)
    Night,
    /// Transition (dawn/dusk)
    Transition,
    /// Polar (near-constant light)
    PolarLight,
}

impl LunarConsensus {
    /// Create new lunar consensus instance
    pub fn new(config: LunarBaseConfig) -> Self {
        let solar_phase = match config.region {
            LunarRegion::NorthPole | LunarRegion::SouthPole => SolarPhase::PolarLight,
            _ => SolarPhase::Day,
        };
        
        Self {
            config,
            earth_status: EarthConnectionStatus {
                path: EarthCommunicationPath::Direct,
                quality: ConnectionQuality::Good,
                measured_latency: Duration::from_millis(1300),
                bandwidth_bps: 10_000_000, // 10 Mbps
            },
            local_state: LocalConsensusState {
                term: 0,
                leader: None,
                commit_index: 0,
                health: ConsensusHealth::Healthy,
            },
            earth_sync: EarthSyncState {
                last_sync: None,
                pending_updates: 0,
                status: SyncStatus::Synced,
                latency_history: Vec::new(),
            },
            base_sync: HashMap::new(),
            solar_phase,
        }
    }
    
    /// Propose a decision with Earth coordination
    /// 
    /// Unlike Mars, we can often wait for Earth confirmation in real-time
    pub async fn propose_with_earth_coordination(
        &mut self, 
        decision: LocalDecision,
        require_earth_ack: bool,
    ) -> Result<DecisionResult, ConsensusError> {
        // Check Earth availability
        if require_earth_ack && !self.can_reach_earth() {
            return Err(ConsensusError::EarthUnreachable);
        }
        
        // Local consensus first
        let local_result = self.execute_local_consensus(&decision).await?;
        
        // If Earth ack required, wait for it
        if require_earth_ack {
            let earth_ack = self.wait_for_earth_ack(&local_result).await?;
            if !earth_ack.approved {
                // Earth rejected - rollback
                self.rollback_decision(&local_result).await?;
                return Err(ConsensusError::EarthRejected);
            }
        }
        
        Ok(local_result)
    }
    
    /// Propose a decision for lunar-wide consensus
    pub async fn propose_lunar_wide(&mut self, decision: LunarWideDecision) -> Result<DecisionResult, ConsensusError> {
        // Check we have quorum of lunar bases
        let reachable_bases = self.reachable_bases();
        if reachable_bases.len() < self.required_quorum() {
            return Err(ConsensusError::InsufficientQuorum);
        }
        
        // Execute multi-base consensus
        let result = self.execute_lunar_consensus(&decision, &reachable_bases).await?;
        
        Ok(result)
    }
    
    /// Enter night mode (for non-polar bases)
    pub fn enter_night_mode(&mut self) {
        self.solar_phase = SolarPhase::Night;
        
        // Reduce power consumption
        self.reduce_power_mode();
        
        // Check battery reserves
        if !self.sufficient_night_power() {
            self.enter_survival_mode();
        }
    }
    
    /// Exit night mode
    pub fn enter_day_mode(&mut self) {
        self.solar_phase = SolarPhase::Day;
        
        // Restore full power
        self.restore_full_power();
        
        // Sync any accumulated changes
        self.queue_sync();
    }
    
    /// Handle far-side isolation
    pub fn enter_far_side_isolation(&mut self) {
        // Switch to relay communication
        self.earth_status.path = EarthCommunicationPath::OrbitalRelay;
        
        if self.earth_status.path == EarthCommunicationPath::Unavailable {
            // Full isolation - operate autonomously
            self.earth_sync.status = SyncStatus::Isolated;
            self.expand_local_authority();
        }
    }
    
    // --- Helper methods ---
    
    fn can_reach_earth(&self) -> bool {
        self.earth_status.path != EarthCommunicationPath::Unavailable
    }
    
    async fn execute_local_consensus(&self, _decision: &LocalDecision) -> Result<DecisionResult, ConsensusError> {
        Ok(DecisionResult::default())
    }
    
    async fn wait_for_earth_ack(&self, _result: &DecisionResult) -> Result<EarthAck, ConsensusError> {
        // Wait ~3 seconds round trip
        tokio::time::sleep(Duration::from_secs(3)).await;
        Ok(EarthAck { approved: true })
    }
    
    async fn rollback_decision(&self, _result: &DecisionResult) -> Result<(), ConsensusError> {
        Ok(())
    }
    
    fn reachable_bases(&self) -> Vec<BaseId> {
        self.base_sync.iter()
            .filter(|(_, s)| s.status != SyncStatus::Isolated)
            .map(|(id, _)| id.clone())
            .collect()
    }
    
    fn required_quorum(&self) -> usize {
        (self.config.federated_bases.len() / 2) + 1
    }
    
    async fn execute_lunar_consensus(&self, _decision: &LunarWideDecision, _bases: &[BaseId]) -> Result<DecisionResult, ConsensusError> {
        Ok(DecisionResult::default())
    }
    
    fn reduce_power_mode(&mut self) {
        // Reduce non-essential operations
    }
    
    fn sufficient_night_power(&self) -> bool {
        self.config.power_config.night_capable
    }
    
    fn enter_survival_mode(&mut self) {
        // Minimize all operations
    }
    
    fn restore_full_power(&mut self) {
        // Resume normal operations
    }
    
    fn queue_sync(&mut self) {
        // Queue pending updates for sync
    }
    
    fn expand_local_authority(&mut self) {
        // Expand autonomous decision scope
    }
}

// =============================================================================
// SUPPORTING TYPES
// =============================================================================

#[derive(Debug, Clone)]
pub struct LocalDecision {
    pub id: String,
    pub payload: Vec<u8>,
}

#[derive(Debug, Clone)]
pub struct LunarWideDecision {
    pub id: String,
    pub payload: Vec<u8>,
}

#[derive(Debug, Clone, Default)]
pub struct DecisionResult {
    pub success: bool,
    pub decision_id: String,
    pub commit_index: u64,
}

#[derive(Debug, Clone)]
pub struct EarthAck {
    pub approved: bool,
}

#[derive(Debug, Clone)]
pub enum ConsensusError {
    EarthUnreachable,
    EarthRejected,
    InsufficientQuorum,
    Timeout,
}

// =============================================================================
// HEALTH MONITORING
// =============================================================================

/// Lunar base health for dashboard reporting
#[derive(Debug, Clone)]
pub struct LunarBaseHealth {
    pub base_id: BaseId,
    pub health_score: f64,
    pub consensus_health: ConsensusHealth,
    pub earth_connection: EarthConnectionStatus,
    pub solar_phase: SolarPhase,
    pub power_status: PowerStatus,
    pub thermal_status: ThermalStatus,
    pub node_health: NodeHealthSummary,
}

#[derive(Debug, Clone)]
pub struct PowerStatus {
    pub current_source: PowerSource,
    pub power_available_w: u64,
    pub battery_percent: f64,
    pub hours_to_sunset: Option<f64>,
    pub hours_to_sunrise: Option<f64>,
}

#[derive(Debug, Clone)]
pub struct ThermalStatus {
    pub current_temp: f64,
    pub radiator_load: f64,
    pub heater_active: bool,
}

#[derive(Debug, Clone)]
pub struct NodeHealthSummary {
    pub total_nodes: usize,
    pub healthy_nodes: usize,
    pub degraded_nodes: usize,
    pub failed_nodes: usize,
}

impl LunarConsensus {
    /// Get health status for dashboard reporting
    pub fn health_status(&self) -> LunarBaseHealth {
        LunarBaseHealth {
            base_id: self.config.base_id.clone(),
            health_score: self.calculate_health_score(),
            consensus_health: self.local_state.health,
            earth_connection: self.earth_status.clone(),
            solar_phase: self.solar_phase,
            power_status: self.power_status(),
            thermal_status: self.thermal_status(),
            node_health: self.node_health_summary(),
        }
    }
    
    fn calculate_health_score(&self) -> f64 {
        let consensus_factor = match self.local_state.health {
            ConsensusHealth::Healthy => 1.0,
            ConsensusHealth::Degraded => 0.7,
            ConsensusHealth::Recovering => 0.5,
            ConsensusHealth::Failed => 0.0,
        };
        
        let connection_factor = match self.earth_status.quality {
            ConnectionQuality::Excellent => 1.0,
            ConnectionQuality::Good => 0.9,
            ConnectionQuality::Degraded => 0.7,
            ConnectionQuality::Poor => 0.4,
            ConnectionQuality::Unavailable => 0.2,
        };
        
        consensus_factor * 0.6 + connection_factor * 0.4
    }
    
    fn power_status(&self) -> PowerStatus {
        PowerStatus {
            current_source: self.config.power_config.primary,
            power_available_w: 10000,
            battery_percent: 85.0,
            hours_to_sunset: Some(120.0),
            hours_to_sunrise: None,
        }
    }
    
    fn thermal_status(&self) -> ThermalStatus {
        ThermalStatus {
            current_temp: 25.0,
            radiator_load: 0.6,
            heater_active: false,
        }
    }
    
    fn node_health_summary(&self) -> NodeHealthSummary {
        NodeHealthSummary {
            total_nodes: self.config.nodes.len(),
            healthy_nodes: self.config.nodes.len(),
            degraded_nodes: 0,
            failed_nodes: 0,
        }
    }
}

// =============================================================================
// TESTS
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_earth_communication_latency() {
        let direct = EarthCommunicationPath::Direct;
        assert!(direct.total_latency() > Duration::from_secs(1));
        assert!(direct.total_latency() < Duration::from_secs(2));
    }
    
    #[test]
    fn test_lunar_region_earth_los() {
        assert!(LunarRegion::NearSide.has_earth_los());
        assert!(!LunarRegion::FarSide.has_earth_los());
    }
}
