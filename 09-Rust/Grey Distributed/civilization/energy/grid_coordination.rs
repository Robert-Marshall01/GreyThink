//! Grey Distributed — Global Energy Grid Coordination
//!
//! This module implements coordination logic for electricity grids at civilization scale.
//! It enables real-time balancing of supply and demand across continental power networks,
//! renewable integration, and resilient failover during grid emergencies.
//!
//! # Design Philosophy
//!
//! Power grids operate at millisecond timescales with zero tolerance for imbalance.
//! This coordination layer provides:
//! - Sub-second frequency regulation signaling
//! - Cross-border power flow optimization
//! - Renewable intermittency smoothing
//! - Cascading failure prevention
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────────────────┐
//! │                     Global Grid Coordination                            │
//! ├─────────────────────────────────────────────────────────────────────────┤
//! │                                                                         │
//! │  Continental Coordinators (5)                                          │
//! │  ├── North America (NERC regions)                                      │
//! │  ├── Europe (ENTSO-E)                                                  │
//! │  ├── Asia-Pacific (multiple ISOs)                                      │
//! │  ├── South America                                                      │
//! │  └── Africa-Middle East                                                 │
//! │                                                                         │
//! │  Regional Controllers (50+)                                             │
//! │  ├── Frequency regulation                                               │
//! │  ├── Voltage control                                                    │
//! │  └── Congestion management                                              │
//! │                                                                         │
//! │  Asset Agents (millions)                                                │
//! │  ├── Generation (thermal, nuclear, hydro, solar, wind)                 │
//! │  ├── Storage (batteries, pumped hydro, hydrogen)                       │
//! │  ├── Transmission (HVDC interties, transformers)                       │
//! │  └── Demand (industrial, commercial, residential)                      │
//! │                                                                         │
//! └─────────────────────────────────────────────────────────────────────────┘
//! ```

use std::collections::{HashMap, BTreeMap};
use std::sync::Arc;
use std::time::{Duration, Instant};

// =============================================================================
// Core Types
// =============================================================================

/// Unique identifier for a grid region (e.g., "NA-WECC", "EU-NORDIC")
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RegionId(pub String);

/// Unique identifier for a grid asset (generator, load, storage, transmission)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AssetId(pub String);

/// Power measurement in megawatts (MW)
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Power(pub f64);

impl Power {
    pub fn megawatts(mw: f64) -> Self { Power(mw) }
    pub fn gigawatts(gw: f64) -> Self { Power(gw * 1000.0) }
    pub fn terawatts(tw: f64) -> Self { Power(tw * 1_000_000.0) }
}

/// Energy measurement in megawatt-hours (MWh)
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Energy(pub f64);

/// Frequency in Hertz (nominal 50Hz or 60Hz depending on region)
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Frequency(pub f64);

impl Frequency {
    /// Deviation from nominal frequency
    pub fn deviation(&self, nominal: f64) -> f64 {
        self.0 - nominal
    }
    
    /// Check if frequency is within acceptable bounds
    pub fn is_normal(&self, nominal: f64) -> bool {
        (self.0 - nominal).abs() < 0.2  // ±200 mHz is typical normal range
    }
    
    /// Check if emergency response is needed
    pub fn is_emergency(&self, nominal: f64) -> bool {
        (self.0 - nominal).abs() > 0.5  // ±500 mHz triggers emergency
    }
}

/// Voltage in kilovolts (kV)
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Voltage(pub f64);

/// Geographic coordinates for asset location
#[derive(Debug, Clone, Copy)]
pub struct GeoLocation {
    pub latitude: f64,
    pub longitude: f64,
}

// =============================================================================
// Asset Types
// =============================================================================

/// Types of generation assets
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GenerationType {
    /// Baseload thermal (coal, gas, nuclear)
    Thermal { fuel: String, efficiency: u8 },
    /// Nuclear power
    Nuclear { reactor_type: String },
    /// Hydroelectric
    Hydro { dam_capacity_gwh: f64 },
    /// Solar photovoltaic
    Solar { tracking: bool },
    /// Wind generation
    Wind { offshore: bool },
    /// Other renewables (geothermal, biomass)
    Renewable { source: String },
}

/// Types of storage assets
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StorageType {
    /// Battery energy storage systems
    Battery { chemistry: String },
    /// Pumped hydro storage
    PumpedHydro,
    /// Compressed air energy storage
    CompressedAir,
    /// Hydrogen storage
    Hydrogen,
    /// Flywheel
    Flywheel,
}

/// Types of transmission assets
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TransmissionType {
    /// High-voltage AC transmission
    HVAC { voltage_kv: u32 },
    /// High-voltage DC transmission (for long distances, underwater)
    HVDC { voltage_kv: u32, bipolar: bool },
    /// Interconnection between regions
    Intertie { from_region: RegionId, to_region: RegionId },
}

/// Types of demand/load
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LoadType {
    /// Industrial loads (often flexible)
    Industrial { sector: String, flexible: bool },
    /// Commercial buildings
    Commercial,
    /// Residential
    Residential,
    /// Data centers (critical, some flexibility)
    DataCenter { tier: u8 },
    /// Electric vehicle charging
    EVCharging { managed: bool },
}

// =============================================================================
// Grid State
// =============================================================================

/// Real-time state of a grid asset
#[derive(Debug, Clone)]
pub struct AssetState {
    pub asset_id: AssetId,
    pub timestamp: Instant,
    
    /// Current power output/consumption (positive = generation, negative = load)
    pub power: Power,
    
    /// Maximum available power (ramp-up headroom)
    pub max_available: Power,
    
    /// Minimum power (ramp-down floor)
    pub min_power: Power,
    
    /// Ramp rate (MW/minute)
    pub ramp_rate: f64,
    
    /// Current operational status
    pub status: AssetStatus,
    
    /// For storage: current state of charge (0.0-1.0)
    pub state_of_charge: Option<f64>,
    
    /// Forecast for next hour (if available)
    pub forecast_1h: Option<Power>,
}

/// Operational status of an asset
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssetStatus {
    /// Operating normally
    Online,
    /// Starting up
    Starting { eta_minutes: u32 },
    /// Shutting down
    Stopping,
    /// Offline but available
    Standby,
    /// Under maintenance
    Maintenance { scheduled_return: Option<u64> },
    /// Faulted/tripped
    Faulted { fault_code: String },
}

/// Aggregated state of a grid region
#[derive(Debug, Clone)]
pub struct RegionState {
    pub region_id: RegionId,
    pub timestamp: Instant,
    
    /// Current system frequency
    pub frequency: Frequency,
    
    /// Nominal frequency (50Hz or 60Hz)
    pub nominal_frequency: f64,
    
    /// Total generation
    pub total_generation: Power,
    
    /// Total load
    pub total_load: Power,
    
    /// Net interchange (positive = exporting)
    pub net_interchange: Power,
    
    /// Generation by type
    pub generation_mix: HashMap<String, Power>,
    
    /// Available reserves
    pub spinning_reserve: Power,
    pub non_spinning_reserve: Power,
    
    /// Renewable curtailment (if any)
    pub curtailment: Power,
    
    /// Demand response available
    pub demand_response_available: Power,
    
    /// Carbon intensity (gCO2/kWh)
    pub carbon_intensity: f64,
    
    /// Price signals
    pub locational_marginal_price: f64,  // $/MWh
}

// =============================================================================
// Coordination Commands
// =============================================================================

/// Command issued to an asset or region
#[derive(Debug, Clone)]
pub enum GridCommand {
    /// Adjust power output/consumption to target
    SetPoint {
        asset_id: AssetId,
        target_power: Power,
        ramp_duration: Duration,
    },
    
    /// Participate in frequency response
    FrequencyResponse {
        asset_id: AssetId,
        droop_setting: f64,
        deadband_hz: f64,
    },
    
    /// Emergency load shed
    LoadShed {
        region_id: RegionId,
        amount: Power,
        priority_threshold: u8,  // Shed loads below this priority
    },
    
    /// Emergency generation dispatch
    EmergencyDispatch {
        asset_id: AssetId,
        target_power: Power,
    },
    
    /// Adjust interchange schedule
    InterchangeAdjust {
        from_region: RegionId,
        to_region: RegionId,
        adjustment: Power,
    },
    
    /// Curtail renewable generation
    Curtail {
        asset_id: AssetId,
        max_power: Power,
        reason: CurtailmentReason,
    },
    
    /// Request demand response
    DemandResponse {
        region_id: RegionId,
        reduction_target: Power,
        duration: Duration,
        compensation_rate: f64,  // $/MWh
    },
}

/// Reason for curtailment
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CurtailmentReason {
    /// Transmission congestion
    Congestion,
    /// Oversupply (negative prices)
    Oversupply,
    /// System stability (too much intermittent)
    Stability,
    /// Emergency frequency support
    Emergency,
}

// =============================================================================
// Coordination Protocol
// =============================================================================

/// Configuration for a grid coordinator
#[derive(Debug, Clone)]
pub struct CoordinatorConfig {
    /// Region this coordinator manages
    pub region_id: RegionId,
    
    /// Nominal frequency for this region
    pub nominal_frequency: f64,
    
    /// Control cycle interval
    pub control_interval: Duration,
    
    /// Frequency deviation thresholds
    pub frequency_thresholds: FrequencyThresholds,
    
    /// Reserve requirements
    pub reserve_requirements: ReserveRequirements,
    
    /// Neighboring regions for interchange
    pub neighbors: Vec<RegionId>,
    
    /// Maximum interchange capacity with each neighbor
    pub interchange_limits: HashMap<RegionId, Power>,
}

/// Frequency thresholds for control actions
#[derive(Debug, Clone)]
pub struct FrequencyThresholds {
    /// Normal operating range (±mHz from nominal)
    pub normal_band_mhz: f64,
    /// Alert threshold
    pub alert_mhz: f64,
    /// Emergency threshold (triggers load shed)
    pub emergency_mhz: f64,
    /// Critical threshold (automatic under-frequency relay)
    pub critical_mhz: f64,
}

impl Default for FrequencyThresholds {
    fn default() -> Self {
        Self {
            normal_band_mhz: 50.0,   // ±50 mHz
            alert_mhz: 200.0,         // ±200 mHz
            emergency_mhz: 500.0,     // ±500 mHz
            critical_mhz: 1000.0,     // ±1 Hz
        }
    }
}

/// Reserve requirements for reliability
#[derive(Debug, Clone)]
pub struct ReserveRequirements {
    /// Primary frequency response (seconds timescale)
    pub primary_reserve_pct: f64,    // % of peak load
    /// Secondary reserves (minutes timescale)
    pub secondary_reserve_mw: Power,
    /// Tertiary reserves (10-30 minutes)
    pub tertiary_reserve_mw: Power,
    /// Strategic reserve (offline but available)
    pub strategic_reserve_mw: Power,
}

/// The main grid coordinator
pub struct GridCoordinator {
    config: CoordinatorConfig,
    
    /// Current state of all assets in region
    asset_states: HashMap<AssetId, AssetState>,
    
    /// Current region state
    region_state: RegionState,
    
    /// Pending commands
    pending_commands: Vec<GridCommand>,
    
    /// Historical frequency data (for analysis)
    frequency_history: BTreeMap<Instant, Frequency>,
    
    /// Interchange schedules with neighbors
    interchange_schedules: HashMap<RegionId, InterchangeSchedule>,
    
    /// Automatic Generation Control (AGC) state
    agc_state: AGCState,
}

/// Interchange schedule with a neighboring region
#[derive(Debug, Clone)]
pub struct InterchangeSchedule {
    pub neighbor: RegionId,
    /// Scheduled interchange by hour (positive = export to neighbor)
    pub hourly_schedule: Vec<Power>,
    /// Current actual interchange
    pub actual: Power,
    /// Deviation from schedule
    pub deviation: Power,
}

/// Automatic Generation Control state
#[derive(Debug, Clone)]
pub struct AGCState {
    /// Area Control Error
    pub ace: f64,
    /// Integral of ACE (for control)
    pub ace_integral: f64,
    /// Participating units and their participation factors
    pub participation_factors: HashMap<AssetId, f64>,
    /// AGC setpoint adjustments
    pub setpoint_adjustments: HashMap<AssetId, Power>,
}

impl GridCoordinator {
    /// Create a new grid coordinator
    pub fn new(config: CoordinatorConfig) -> Self {
        Self {
            config: config.clone(),
            asset_states: HashMap::new(),
            region_state: RegionState {
                region_id: config.region_id.clone(),
                timestamp: Instant::now(),
                frequency: Frequency(config.nominal_frequency),
                nominal_frequency: config.nominal_frequency,
                total_generation: Power(0.0),
                total_load: Power(0.0),
                net_interchange: Power(0.0),
                generation_mix: HashMap::new(),
                spinning_reserve: Power(0.0),
                non_spinning_reserve: Power(0.0),
                curtailment: Power(0.0),
                demand_response_available: Power(0.0),
                carbon_intensity: 0.0,
                locational_marginal_price: 0.0,
            },
            pending_commands: Vec::new(),
            frequency_history: BTreeMap::new(),
            interchange_schedules: HashMap::new(),
            agc_state: AGCState {
                ace: 0.0,
                ace_integral: 0.0,
                participation_factors: HashMap::new(),
                setpoint_adjustments: HashMap::new(),
            },
        }
    }
    
    /// Update asset state from telemetry
    pub fn update_asset_state(&mut self, state: AssetState) {
        self.asset_states.insert(state.asset_id.clone(), state);
    }
    
    /// Main control loop iteration
    ///
    /// This runs every control cycle (typically 2-4 seconds) and:
    /// 1. Calculates Area Control Error (ACE)
    /// 2. Dispatches AGC signals to participating units
    /// 3. Checks for emergency conditions
    /// 4. Coordinates with neighboring regions
    pub fn control_cycle(&mut self) -> Vec<GridCommand> {
        let now = Instant::now();
        let mut commands = Vec::new();
        
        // 1. Calculate current system state
        self.calculate_region_state();
        
        // 2. Record frequency for history
        self.frequency_history.insert(now, self.region_state.frequency);
        
        // 3. Calculate Area Control Error
        let ace = self.calculate_ace();
        self.agc_state.ace = ace;
        
        // 4. Check for emergency conditions
        if let Some(emergency_cmd) = self.check_emergency_conditions() {
            commands.push(emergency_cmd);
            // In emergency, skip normal AGC
            return commands;
        }
        
        // 5. Run AGC dispatch
        let agc_commands = self.run_agc(ace);
        commands.extend(agc_commands);
        
        // 6. Check renewable curtailment needs
        if let Some(curtail_cmds) = self.check_curtailment_needs() {
            commands.extend(curtail_cmds);
        }
        
        // 7. Coordinate interchange with neighbors
        if let Some(interchange_cmds) = self.balance_interchange() {
            commands.extend(interchange_cmds);
        }
        
        commands
    }
    
    /// Calculate aggregated region state from individual assets
    fn calculate_region_state(&mut self) {
        let mut total_gen = 0.0;
        let mut total_load = 0.0;
        let mut spinning_reserve = 0.0;
        let mut gen_mix: HashMap<String, f64> = HashMap::new();
        
        for (_, state) in &self.asset_states {
            if state.power.0 > 0.0 {
                // Generation
                total_gen += state.power.0;
                spinning_reserve += state.max_available.0 - state.power.0;
            } else {
                // Load
                total_load += state.power.0.abs();
            }
        }
        
        self.region_state.total_generation = Power(total_gen);
        self.region_state.total_load = Power(total_load);
        self.region_state.spinning_reserve = Power(spinning_reserve);
        self.region_state.timestamp = Instant::now();
    }
    
    /// Calculate Area Control Error
    ///
    /// ACE = (Actual Interchange - Scheduled Interchange) + 10 * B * (Actual Freq - Scheduled Freq)
    /// where B is the frequency bias (MW/0.1Hz)
    fn calculate_ace(&self) -> f64 {
        let interchange_error: f64 = self.interchange_schedules.values()
            .map(|s| s.deviation.0)
            .sum();
        
        let frequency_error = self.region_state.frequency.0 - self.region_state.nominal_frequency;
        
        // Frequency bias: typically 1% of peak load per 0.1 Hz
        // Assuming 100 GW region, B ≈ 1000 MW/0.1Hz
        let bias_factor = 1000.0;
        
        interchange_error + 10.0 * bias_factor * frequency_error
    }
    
    /// Run Automatic Generation Control
    fn run_agc(&mut self, ace: f64) -> Vec<GridCommand> {
        let mut commands = Vec::new();
        
        // Proportional-Integral control
        let kp = 0.5;  // Proportional gain
        let ki = 0.1;  // Integral gain
        
        self.agc_state.ace_integral += ace;
        
        // Limit integral windup
        self.agc_state.ace_integral = self.agc_state.ace_integral.clamp(-5000.0, 5000.0);
        
        let total_adjustment = kp * ace + ki * self.agc_state.ace_integral;
        
        // Distribute adjustment among participating units
        for (asset_id, factor) in &self.agc_state.participation_factors {
            let adjustment = total_adjustment * factor;
            
            if let Some(state) = self.asset_states.get(asset_id) {
                let new_target = Power(state.power.0 + adjustment);
                
                // Clamp to asset limits
                let clamped_target = Power(
                    new_target.0.clamp(state.min_power.0, state.max_available.0)
                );
                
                if (clamped_target.0 - state.power.0).abs() > 1.0 {
                    commands.push(GridCommand::SetPoint {
                        asset_id: asset_id.clone(),
                        target_power: clamped_target,
                        ramp_duration: Duration::from_secs(60),
                    });
                }
            }
        }
        
        commands
    }
    
    /// Check for emergency conditions requiring immediate action
    fn check_emergency_conditions(&self) -> Option<GridCommand> {
        let freq_deviation = (self.region_state.frequency.0 - self.region_state.nominal_frequency).abs();
        
        // Under-frequency emergency (generation shortfall)
        if self.region_state.frequency.0 < self.region_state.nominal_frequency - 0.5 {
            // Calculate load shed amount needed
            // Rule of thumb: 1% frequency drop ≈ 6% power imbalance
            let imbalance_pct = freq_deviation / self.region_state.nominal_frequency * 100.0 * 6.0;
            let shed_amount = Power(self.region_state.total_load.0 * imbalance_pct / 100.0);
            
            return Some(GridCommand::LoadShed {
                region_id: self.config.region_id.clone(),
                amount: shed_amount,
                priority_threshold: 3,  // Shed low-priority loads first
            });
        }
        
        // Over-frequency emergency (generation surplus)
        if self.region_state.frequency.0 > self.region_state.nominal_frequency + 0.5 {
            // Need to curtail generation or export more
            // This is less common but can happen with high renewable penetration
        }
        
        None
    }
    
    /// Check if renewable curtailment is needed
    fn check_curtailment_needs(&self) -> Option<Vec<GridCommand>> {
        // Curtailment needed if:
        // 1. Generation exceeds load + export capacity
        // 2. Transmission is congested
        // 3. System needs more inertia (too much inverter-based)
        
        let net_surplus = self.region_state.total_generation.0 
            - self.region_state.total_load.0 
            - self.region_state.net_interchange.0;
        
        if net_surplus > 100.0 {  // 100 MW threshold
            // Find renewable assets to curtail
            let mut commands = Vec::new();
            let mut remaining = net_surplus;
            
            // Curtail in order: solar, then wind
            for (asset_id, state) in &self.asset_states {
                if remaining <= 0.0 { break; }
                
                // Only curtail if asset is generating
                if state.power.0 > 0.0 {
                    let curtail_amount = remaining.min(state.power.0);
                    commands.push(GridCommand::Curtail {
                        asset_id: asset_id.clone(),
                        max_power: Power(state.power.0 - curtail_amount),
                        reason: CurtailmentReason::Oversupply,
                    });
                    remaining -= curtail_amount;
                }
            }
            
            return Some(commands);
        }
        
        None
    }
    
    /// Balance interchange with neighboring regions
    fn balance_interchange(&self) -> Option<Vec<GridCommand>> {
        let mut commands = Vec::new();
        
        for (neighbor, schedule) in &self.interchange_schedules {
            // If deviation exceeds threshold, request adjustment
            if schedule.deviation.0.abs() > 50.0 {  // 50 MW threshold
                commands.push(GridCommand::InterchangeAdjust {
                    from_region: self.config.region_id.clone(),
                    to_region: neighbor.clone(),
                    adjustment: Power(-schedule.deviation.0),  // Correct the deviation
                });
            }
        }
        
        if commands.is_empty() { None } else { Some(commands) }
    }
    
    /// Register an asset for AGC participation
    pub fn register_agc_participant(&mut self, asset_id: AssetId, participation_factor: f64) {
        self.agc_state.participation_factors.insert(asset_id, participation_factor);
    }
    
    /// Set interchange schedule with a neighbor
    pub fn set_interchange_schedule(&mut self, neighbor: RegionId, schedule: Vec<Power>) {
        self.interchange_schedules.insert(neighbor.clone(), InterchangeSchedule {
            neighbor,
            hourly_schedule: schedule,
            actual: Power(0.0),
            deviation: Power(0.0),
        });
    }
}

// =============================================================================
// Cross-Continental Coordination
// =============================================================================

/// Global grid coordinator that manages multiple continental coordinators
pub struct GlobalGridCoordinator {
    /// Continental coordinators
    continents: HashMap<String, Arc<GridCoordinator>>,
    
    /// Inter-continental links (submarine HVDC, etc.)
    intercontinental_links: Vec<IntercontinentalLink>,
    
    /// Global carbon tracking
    global_carbon_intensity: f64,
    
    /// Global renewable generation share
    global_renewable_share: f64,
}

/// An intercontinental transmission link
#[derive(Debug, Clone)]
pub struct IntercontinentalLink {
    pub id: String,
    pub name: String,
    pub from_continent: String,
    pub to_continent: String,
    pub capacity: Power,
    pub current_flow: Power,
    pub link_type: TransmissionType,
    pub status: AssetStatus,
}

impl GlobalGridCoordinator {
    /// Optimize power flows globally
    ///
    /// This considers:
    /// - Time zone differences (solar follows the sun)
    /// - Carbon intensity arbitrage
    /// - Emergency mutual assistance
    /// - Economic dispatch across regions
    pub fn optimize_global_flows(&mut self) -> Vec<IntercontinentalFlowDecision> {
        let mut decisions = Vec::new();
        
        // Example: Europe evening, Americas afternoon, Asia morning
        // Solar surplus in Americas can serve European evening peak
        
        for link in &self.intercontinental_links {
            if link.status != AssetStatus::Online {
                continue;
            }
            
            // Get carbon intensities of connected regions
            // Flow power from low-carbon to high-carbon regions
            
            // Get price differentials
            // Flow power from low-price to high-price regions
            
            // These optimizations reduce global emissions and costs
            
            decisions.push(IntercontinentalFlowDecision {
                link_id: link.id.clone(),
                recommended_flow: Power(0.0),  // Calculated based on optimization
                reason: FlowReason::CarbonOptimization,
            });
        }
        
        decisions
    }
    
    /// Handle global emergency (major grid event)
    pub fn handle_global_emergency(&mut self, event: GridEmergency) {
        match event {
            GridEmergency::MajorGenerationLoss { region, amount } => {
                // Coordinate emergency imports from all available neighbors
                // Activate demand response globally if needed
                // Prepare for cascading failure prevention
            }
            GridEmergency::TransmissionFailure { link_id } => {
                // Reroute power flows through alternative paths
                // Activate strategic reserves in affected regions
            }
            GridEmergency::CyberAttack { severity } => {
                // Isolate affected systems
                // Switch to manual control if needed
                // Activate backup communication channels
            }
            GridEmergency::ExtremeCascade { affected_regions } => {
                // Controlled islanding to stop cascade
                // Coordinate black start preparation
            }
        }
    }
}

/// Decision for intercontinental power flow
#[derive(Debug, Clone)]
pub struct IntercontinentalFlowDecision {
    pub link_id: String,
    pub recommended_flow: Power,
    pub reason: FlowReason,
}

/// Reason for flow recommendation
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FlowReason {
    CarbonOptimization,
    Economic,
    EmergencyAssistance,
    RenewableIntegration,
    FrequencySupport,
}

/// Types of grid emergencies
#[derive(Debug, Clone)]
pub enum GridEmergency {
    MajorGenerationLoss { region: RegionId, amount: Power },
    TransmissionFailure { link_id: String },
    CyberAttack { severity: u8 },
    ExtremeCascade { affected_regions: Vec<RegionId> },
}

// =============================================================================
// Renewable Integration
// =============================================================================

/// Forecast for renewable generation
#[derive(Debug, Clone)]
pub struct RenewableForecast {
    pub asset_id: AssetId,
    pub forecast_type: ForecastType,
    /// Hourly forecasts for next 48 hours
    pub hourly_forecast: Vec<ForecastPoint>,
    /// Uncertainty bounds
    pub confidence_interval: f64,
}

#[derive(Debug, Clone)]
pub struct ForecastPoint {
    pub hour: u32,
    pub power: Power,
    pub upper_bound: Power,
    pub lower_bound: Power,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ForecastType {
    Solar { cloud_model: String },
    Wind { weather_model: String },
    Hydro { inflow_model: String },
}

/// Renewable integration coordinator
pub struct RenewableIntegrator {
    /// Forecasts by asset
    forecasts: HashMap<AssetId, RenewableForecast>,
    
    /// Storage assets available for smoothing
    storage_assets: Vec<AssetId>,
    
    /// Demand response programs
    demand_response_capacity: Power,
}

impl RenewableIntegrator {
    /// Calculate ramping requirements for the day
    ///
    /// The "duck curve" challenge: solar ramps up in morning,
    /// ramps down in evening as demand peaks.
    pub fn calculate_ramping_needs(&self, region_state: &RegionState) -> RampingRequirements {
        // Analyze forecast ramps
        let mut max_up_ramp = 0.0;
        let mut max_down_ramp = 0.0;
        
        for forecast in self.forecasts.values() {
            for i in 1..forecast.hourly_forecast.len() {
                let ramp = forecast.hourly_forecast[i].power.0 
                    - forecast.hourly_forecast[i-1].power.0;
                if ramp > max_up_ramp { max_up_ramp = ramp; }
                if ramp < max_down_ramp { max_down_ramp = ramp; }
            }
        }
        
        RampingRequirements {
            max_up_ramp: Power(max_up_ramp),
            max_down_ramp: Power(max_down_ramp.abs()),
            storage_contribution: Power(0.0),  // Calculate based on available storage
            flexible_load_contribution: Power(0.0),
        }
    }
    
    /// Smooth renewable intermittency using storage
    pub fn smooth_intermittency(&self, 
        current_renewable: Power,
        forecast_1h: Power,
    ) -> StorageDispatch {
        let expected_change = forecast_1h.0 - current_renewable.0;
        
        StorageDispatch {
            charge_discharge: Power(-expected_change / 2.0),  // Pre-emptive smoothing
            duration: Duration::from_secs(3600),
        }
    }
}

/// Ramping requirements for a time period
#[derive(Debug, Clone)]
pub struct RampingRequirements {
    pub max_up_ramp: Power,       // MW/hour
    pub max_down_ramp: Power,     // MW/hour
    pub storage_contribution: Power,
    pub flexible_load_contribution: Power,
}

/// Storage dispatch command
#[derive(Debug, Clone)]
pub struct StorageDispatch {
    /// Positive = charge, Negative = discharge
    pub charge_discharge: Power,
    pub duration: Duration,
}

// =============================================================================
// Resilience and Black Start
// =============================================================================

/// Black start coordinator for grid restoration
pub struct BlackStartCoordinator {
    /// Black start capable units
    black_start_units: Vec<AssetId>,
    
    /// Restoration sequence plans
    restoration_plans: HashMap<RegionId, RestorationPlan>,
    
    /// Cranking paths (transmission paths for restoration)
    cranking_paths: Vec<CrankingPath>,
}

/// Plan for restoring a region after blackout
#[derive(Debug, Clone)]
pub struct RestorationPlan {
    pub region_id: RegionId,
    /// Ordered list of restoration steps
    pub steps: Vec<RestorationStep>,
    /// Estimated time to full restoration
    pub estimated_duration: Duration,
}

/// A single step in grid restoration
#[derive(Debug, Clone)]
pub struct RestorationStep {
    pub step_number: u32,
    pub description: String,
    pub assets_to_energize: Vec<AssetId>,
    pub prerequisites: Vec<u32>,  // Step numbers that must complete first
    pub validation_checks: Vec<String>,
    pub estimated_duration: Duration,
}

/// Transmission path used during restoration
#[derive(Debug, Clone)]
pub struct CrankingPath {
    pub id: String,
    pub black_start_unit: AssetId,
    /// Ordered list of substations to energize
    pub substations: Vec<String>,
    /// Load blocks that can be restored via this path
    pub restorable_load: Power,
}

impl BlackStartCoordinator {
    /// Execute black start restoration
    pub fn execute_restoration(&self, affected_region: &RegionId) -> RestorationExecution {
        let plan = self.restoration_plans.get(affected_region)
            .expect("No restoration plan for region");
        
        RestorationExecution {
            plan: plan.clone(),
            current_step: 0,
            started_at: Instant::now(),
            logs: Vec::new(),
        }
    }
}

/// Ongoing restoration execution
#[derive(Debug, Clone)]
pub struct RestorationExecution {
    pub plan: RestorationPlan,
    pub current_step: u32,
    pub started_at: Instant,
    pub logs: Vec<RestorationLog>,
}

#[derive(Debug, Clone)]
pub struct RestorationLog {
    pub timestamp: Instant,
    pub step: u32,
    pub event: String,
    pub success: bool,
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_frequency_deviation() {
        let freq = Frequency(59.95);
        assert!(!freq.is_normal(60.0));  // 50 mHz deviation
        assert!(!freq.is_emergency(60.0));  // Not emergency level
        
        let emergency_freq = Frequency(59.4);
        assert!(emergency_freq.is_emergency(60.0));  // 600 mHz is emergency
    }
    
    #[test]
    fn test_ace_calculation() {
        let config = CoordinatorConfig {
            region_id: RegionId("TEST".into()),
            nominal_frequency: 60.0,
            control_interval: Duration::from_secs(4),
            frequency_thresholds: FrequencyThresholds::default(),
            reserve_requirements: ReserveRequirements {
                primary_reserve_pct: 3.0,
                secondary_reserve_mw: Power(1000.0),
                tertiary_reserve_mw: Power(2000.0),
                strategic_reserve_mw: Power(5000.0),
            },
            neighbors: vec![],
            interchange_limits: HashMap::new(),
        };
        
        let coordinator = GridCoordinator::new(config);
        // ACE should be 0 when frequency is nominal and no interchange error
        assert_eq!(coordinator.calculate_ace(), 0.0);
    }
    
    #[test]
    fn test_power_units() {
        assert_eq!(Power::gigawatts(1.0).0, 1000.0);
        assert_eq!(Power::terawatts(1.0).0, 1_000_000.0);
    }
}
