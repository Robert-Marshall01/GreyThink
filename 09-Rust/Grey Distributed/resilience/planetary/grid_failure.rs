//! Global Grid Failure Recovery
//!
//! Handles distributed system survival and recovery during cascading
//! power grid failures across multiple regions or continents.
//!
//! Key challenges addressed:
//! - Multi-region blackouts
//! - Cascading grid failures
//! - Backup power exhaustion
//! - Cold start coordination
//! - Priority-based restoration

use std::collections::{HashMap, HashSet, BinaryHeap};
use std::time::Duration;
use std::cmp::Ordering;
use serde::{Deserialize, Serialize};

// =============================================================================
// GRID FAILURE MODEL
// =============================================================================

/// Grid failure scope and severity
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GridFailure {
    pub id: FailureId,
    pub scope: FailureScope,
    pub cause: FailureCause,
    pub started: Timestamp,
    pub affected_regions: Vec<RegionId>,
    pub affected_population: u64,
    pub estimated_restoration: Option<Duration>,
    pub cascading: bool,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum FailureScope {
    /// Single facility/substation
    Local,
    
    /// City or metropolitan area
    Metropolitan,
    
    /// Multiple cities/state level
    Regional,
    
    /// Multiple states/nations
    Continental,
    
    /// Multiple continents
    Global,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FailureCause {
    /// Equipment failure
    Equipment { component: String },
    
    /// Demand exceeds supply
    Overload { shortfall_mw: u64 },
    
    /// Cascade from another grid
    Cascade { origin: RegionId },
    
    /// Cyberattack
    CyberAttack,
    
    /// Physical attack
    PhysicalAttack,
    
    /// Natural disaster
    NaturalDisaster { event_type: String },
    
    /// Geomagnetic event
    GeomagneticEvent { severity: GeomagneticSeverity },
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum GeomagneticSeverity {
    G1,  // Minor
    G2,  // Moderate
    G3,  // Strong
    G4,  // Severe
    G5,  // Extreme (Carrington-class)
}

// =============================================================================
// FACILITY POWER MANAGEMENT
// =============================================================================

/// Manages power status and backup systems for facilities
#[derive(Debug, Clone)]
pub struct FacilityPowerManager {
    /// Power status of all facilities
    facilities: HashMap<FacilityId, FacilityPowerStatus>,
    
    /// Backup power inventory
    backup_systems: HashMap<FacilityId, BackupPowerSystem>,
    
    /// Fuel reserves
    fuel_reserves: HashMap<FacilityId, FuelReserve>,
    
    /// Solar/renewable availability
    renewable_status: HashMap<FacilityId, RenewableStatus>,
    
    /// Grid connection status
    grid_connections: HashMap<FacilityId, GridConnection>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FacilityPowerStatus {
    pub facility_id: FacilityId,
    pub power_source: PowerSource,
    pub current_consumption_kw: f64,
    pub available_power_kw: f64,
    pub estimated_runtime: Option<Duration>,
    pub critical_systems_only: bool,
    pub last_updated: Timestamp,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum PowerSource {
    Grid,
    GeneratorRunning,
    GeneratorStarting,
    Battery,
    Solar,
    Multiple,
    None,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BackupPowerSystem {
    pub generator_capacity_kw: f64,
    pub battery_capacity_kwh: f64,
    pub battery_current_charge: f64,
    pub auto_transfer_switch: bool,
    pub startup_time: Duration,
    pub last_test: Timestamp,
    pub fuel_type: FuelType,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum FuelType {
    Diesel,
    NaturalGas,
    Propane,
    Hydrogen,
    None,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FuelReserve {
    pub fuel_type: FuelType,
    pub current_liters: f64,
    pub max_capacity_liters: f64,
    pub consumption_rate_lph: f64,
    pub resupply_possible: bool,
    pub last_delivery: Timestamp,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RenewableStatus {
    pub solar_available: bool,
    pub solar_capacity_kw: f64,
    pub current_solar_output_kw: f64,
    pub daylight_remaining: Duration,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GridConnection {
    pub connected: bool,
    pub voltage_normal: bool,
    pub frequency_hz: f64,
    pub last_outage: Option<Timestamp>,
}

impl FacilityPowerManager {
    /// Handle grid power loss for a facility
    pub async fn handle_grid_loss(&mut self, facility_id: &FacilityId) -> PowerTransitionResult {
        let status = self.facilities.get_mut(facility_id);
        let backup = self.backup_systems.get(facility_id);
        
        match (status, backup) {
            (Some(status), Some(backup)) if backup.auto_transfer_switch => {
                // Automatic transfer to generator
                status.power_source = PowerSource::GeneratorStarting;
                
                // Check fuel
                let fuel = self.fuel_reserves.get(facility_id);
                let runtime = fuel.map(|f| {
                    Duration::from_secs((f.current_liters / f.consumption_rate_lph * 3600.0) as u64)
                });
                
                status.estimated_runtime = runtime;
                status.power_source = PowerSource::GeneratorRunning;
                
                PowerTransitionResult::GeneratorStarted {
                    facility_id: facility_id.clone(),
                    estimated_runtime: runtime.unwrap_or(Duration::ZERO),
                }
            }
            (Some(status), Some(backup)) if backup.battery_current_charge > 0.1 => {
                // Transfer to battery
                status.power_source = PowerSource::Battery;
                let runtime = Duration::from_secs(
                    ((backup.battery_capacity_kwh * backup.battery_current_charge) / 
                     status.current_consumption_kw * 3600.0) as u64
                );
                status.estimated_runtime = Some(runtime);
                
                PowerTransitionResult::OnBattery {
                    facility_id: facility_id.clone(),
                    estimated_runtime: runtime,
                }
            }
            (Some(status), _) => {
                // No backup available
                status.power_source = PowerSource::None;
                status.estimated_runtime = Some(Duration::ZERO);
                
                PowerTransitionResult::PowerLost {
                    facility_id: facility_id.clone(),
                }
            }
            _ => PowerTransitionResult::UnknownFacility,
        }
    }
    
    /// Check all facilities for power concerns
    pub fn power_status_check(&self) -> PowerStatusReport {
        let mut report = PowerStatusReport::new();
        
        for (id, status) in &self.facilities {
            match status.power_source {
                PowerSource::None => {
                    report.add_critical(id.clone(), "No power".to_string());
                }
                PowerSource::Battery => {
                    if let Some(runtime) = status.estimated_runtime {
                        if runtime < Duration::from_secs(3600) {
                            report.add_warning(id.clone(), 
                                format!("Battery depleting: {} min remaining", runtime.as_secs() / 60));
                        }
                    }
                }
                PowerSource::GeneratorRunning => {
                    if let Some(fuel) = self.fuel_reserves.get(id) {
                        let hours = fuel.current_liters / fuel.consumption_rate_lph;
                        if hours < 24.0 {
                            report.add_warning(id.clone(),
                                format!("Low fuel: {:.1} hours remaining", hours));
                        }
                    }
                }
                _ => {}
            }
        }
        
        report
    }
    
    /// Optimize power usage during shortage
    pub async fn optimize_for_shortage(&mut self, target_reduction_percent: f64) -> OptimizationResult {
        let mut total_reduction = 0.0;
        let mut actions = Vec::new();
        
        for (id, status) in &mut self.facilities {
            // Calculate facility reduction target
            let reduction = status.current_consumption_kw * target_reduction_percent / 100.0;
            
            // Apply reductions
            if !status.critical_systems_only {
                status.critical_systems_only = true;
                status.current_consumption_kw *= 0.4; // Assume 60% reduction
                total_reduction += reduction;
                actions.push(OptimizationAction::CriticalOnlyMode(id.clone()));
            }
        }
        
        OptimizationResult {
            target_reduction_percent,
            achieved_reduction_kw: total_reduction,
            actions,
        }
    }
}

// =============================================================================
// COLD START COORDINATION
// =============================================================================

/// Coordinates system cold start after widespread grid failure
#[derive(Debug, Clone)]
pub struct ColdStartCoordinator {
    /// Facility startup priority
    startup_priority: HashMap<FacilityId, StartupPriority>,
    
    /// Dependencies between facilities
    dependencies: HashMap<FacilityId, Vec<FacilityId>>,
    
    /// Current startup status
    startup_status: HashMap<FacilityId, StartupStatus>,
    
    /// Available bootstrap resources
    bootstrap_resources: BootstrapResources,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum StartupPriority {
    /// First to start - essential for other systems
    Bootstrap = 0,
    
    /// Critical infrastructure coordination
    Critical = 1,
    
    /// Important services
    High = 2,
    
    /// Normal operations
    Normal = 3,
    
    /// Can wait until stable
    Low = 4,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StartupStatus {
    pub facility_id: FacilityId,
    pub state: StartupState,
    pub power_confirmed: bool,
    pub network_confirmed: bool,
    pub services_healthy: bool,
    pub started_at: Option<Timestamp>,
    pub ready_at: Option<Timestamp>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum StartupState {
    Offline,
    PowerRestoring,
    NetworkRestoring,
    ServicesStarting,
    HealthChecking,
    Ready,
    Failed,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BootstrapResources {
    /// Mobile generators available
    pub mobile_generators: Vec<MobileGenerator>,
    
    /// Satellite uplinks
    pub satellite_uplinks: Vec<SatelliteUplink>,
    
    /// Emergency personnel
    pub personnel: Vec<EmergencyPersonnel>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MobileGenerator {
    pub id: String,
    pub capacity_kw: f64,
    pub location: Option<FacilityId>,
    pub fuel_hours: f64,
}

impl ColdStartCoordinator {
    /// Generate cold start sequence
    pub fn generate_startup_sequence(&self) -> Vec<StartupWave> {
        let mut waves = Vec::new();
        let mut started = HashSet::new();
        
        // Wave 0: Bootstrap facilities (no dependencies)
        let bootstrap: Vec<_> = self.startup_priority.iter()
            .filter(|(_, p)| **p == StartupPriority::Bootstrap)
            .map(|(id, _)| id.clone())
            .collect();
        
        waves.push(StartupWave {
            wave_number: 0,
            facilities: bootstrap.clone(),
            estimated_duration: Duration::from_secs(1800),
            prerequisites: vec![],
        });
        started.extend(bootstrap);
        
        // Subsequent waves based on dependencies
        for priority in [StartupPriority::Critical, StartupPriority::High, 
                        StartupPriority::Normal, StartupPriority::Low] {
            let wave_facilities: Vec<_> = self.startup_priority.iter()
                .filter(|(id, p)| {
                    **p == priority && 
                    !started.contains(*id) &&
                    self.dependencies_met(id, &started)
                })
                .map(|(id, _)| id.clone())
                .collect();
            
            if !wave_facilities.is_empty() {
                waves.push(StartupWave {
                    wave_number: waves.len(),
                    facilities: wave_facilities.clone(),
                    estimated_duration: Duration::from_secs(3600),
                    prerequisites: started.iter().cloned().collect(),
                });
                started.extend(wave_facilities);
            }
        }
        
        waves
    }
    
    /// Execute cold start for a facility
    pub async fn execute_cold_start(&mut self, facility_id: &FacilityId) -> ColdStartResult {
        // Check dependencies
        if !self.dependencies_met(facility_id, &self.get_ready_facilities()) {
            return ColdStartResult::DependenciesNotMet {
                missing: self.get_missing_dependencies(facility_id),
            };
        }
        
        // Update status
        if let Some(status) = self.startup_status.get_mut(facility_id) {
            status.state = StartupState::PowerRestoring;
            status.started_at = Some(Timestamp::now());
        }
        
        // Simulate startup sequence
        self.restore_power(facility_id).await?;
        self.restore_network(facility_id).await?;
        self.start_services(facility_id).await?;
        self.health_check(facility_id).await?;
        
        if let Some(status) = self.startup_status.get_mut(facility_id) {
            status.state = StartupState::Ready;
            status.ready_at = Some(Timestamp::now());
        }
        
        ColdStartResult::Success {
            facility_id: facility_id.clone(),
            startup_duration: Duration::from_secs(1800),
        }
    }
    
    fn dependencies_met(&self, facility_id: &FacilityId, ready: &HashSet<FacilityId>) -> bool {
        self.dependencies.get(facility_id)
            .map(|deps| deps.iter().all(|d| ready.contains(d)))
            .unwrap_or(true)
    }
    
    fn get_ready_facilities(&self) -> HashSet<FacilityId> {
        self.startup_status.iter()
            .filter(|(_, s)| s.state == StartupState::Ready)
            .map(|(id, _)| id.clone())
            .collect()
    }
    
    fn get_missing_dependencies(&self, facility_id: &FacilityId) -> Vec<FacilityId> {
        let ready = self.get_ready_facilities();
        self.dependencies.get(facility_id)
            .map(|deps| deps.iter()
                .filter(|d| !ready.contains(*d))
                .cloned()
                .collect())
            .unwrap_or_default()
    }
    
    async fn restore_power(&self, _facility_id: &FacilityId) -> Result<(), ColdStartError> {
        Ok(())
    }
    
    async fn restore_network(&self, _facility_id: &FacilityId) -> Result<(), ColdStartError> {
        Ok(())
    }
    
    async fn start_services(&self, _facility_id: &FacilityId) -> Result<(), ColdStartError> {
        Ok(())
    }
    
    async fn health_check(&self, _facility_id: &FacilityId) -> Result<(), ColdStartError> {
        Ok(())
    }
}

// =============================================================================
// GRID FAILURE COORDINATOR
// =============================================================================

/// Main coordinator for grid failure response
pub struct GridFailureCoordinator {
    /// Active grid failures
    active_failures: HashMap<FailureId, GridFailure>,
    
    /// Facility power management
    power_manager: FacilityPowerManager,
    
    /// Cold start coordination
    cold_start: ColdStartCoordinator,
    
    /// Grid operator interfaces
    grid_operators: HashMap<RegionId, GridOperatorInterface>,
    
    /// Priority restoration queue
    restoration_queue: BinaryHeap<RestorationRequest>,
}

#[derive(Debug, Clone)]
pub struct GridOperatorInterface {
    pub region: RegionId,
    pub operator: String,
    pub contact: String,
    pub api_available: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RestorationRequest {
    pub facility_id: FacilityId,
    pub priority: u32,
    pub critical_load_kw: f64,
    pub requested_at: Timestamp,
}

impl Ord for RestorationRequest {
    fn cmp(&self, other: &Self) -> Ordering {
        other.priority.cmp(&self.priority)
    }
}

impl PartialOrd for RestorationRequest {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for RestorationRequest {
    fn eq(&self, other: &Self) -> bool {
        self.priority == other.priority
    }
}

impl Eq for RestorationRequest {}

impl GridFailureCoordinator {
    /// Respond to grid failure
    pub async fn handle_grid_failure(&mut self, failure: GridFailure) -> GridFailureResponse {
        let failure_id = failure.id.clone();
        self.active_failures.insert(failure_id.clone(), failure.clone());
        
        let mut response = GridFailureResponse::new(failure_id.clone());
        
        // Identify affected facilities
        let affected_facilities = self.get_affected_facilities(&failure);
        response.affected_facilities = affected_facilities.len();
        
        // Handle power loss for each facility
        for facility_id in &affected_facilities {
            let transition = self.power_manager.handle_grid_loss(facility_id).await;
            response.add_transition(transition);
        }
        
        // Optimize power usage
        if failure.scope >= FailureScope::Regional {
            let optimization = self.power_manager.optimize_for_shortage(50.0).await;
            response.optimization = Some(optimization);
        }
        
        // Request priority restoration from grid operators
        for facility_id in self.get_critical_facilities(&affected_facilities) {
            self.request_priority_restoration(&facility_id).await;
        }
        
        // If cascading, warn neighboring regions
        if failure.cascading {
            response.cascade_warnings = self.send_cascade_warnings(&failure).await;
        }
        
        response
    }
    
    /// Monitor and manage extended outage
    pub async fn manage_extended_outage(&mut self, failure_id: &FailureId) -> ExtendedOutageStatus {
        let failure = self.active_failures.get(failure_id);
        
        if let Some(failure) = failure {
            let power_status = self.power_manager.power_status_check();
            
            // Check for facilities about to lose power
            let critical = power_status.critical_issues.clone();
            let at_risk = power_status.warnings.clone();
            
            // Deploy mobile resources if needed
            let deployments = if !critical.is_empty() {
                self.deploy_emergency_resources(&critical).await
            } else {
                vec![]
            };
            
            ExtendedOutageStatus {
                failure_id: failure_id.clone(),
                duration: Duration::from_secs(Timestamp::now() - failure.started),
                facilities_powered: self.count_powered_facilities(),
                facilities_critical: critical.len(),
                facilities_at_risk: at_risk.len(),
                resource_deployments: deployments,
            }
        } else {
            ExtendedOutageStatus {
                failure_id: failure_id.clone(),
                duration: Duration::ZERO,
                facilities_powered: 0,
                facilities_critical: 0,
                facilities_at_risk: 0,
                resource_deployments: vec![],
            }
        }
    }
    
    /// Coordinate grid restoration
    pub async fn coordinate_restoration(&mut self, failure_id: &FailureId) -> RestorationPlan {
        // Generate cold start sequence
        let sequence = self.cold_start.generate_startup_sequence();
        
        RestorationPlan {
            failure_id: failure_id.clone(),
            waves: sequence,
            estimated_total_time: Duration::from_secs(86400),
            critical_facilities_first: true,
        }
    }
    
    fn get_affected_facilities(&self, failure: &GridFailure) -> Vec<FacilityId> {
        self.power_manager.facilities.iter()
            .filter(|(_, status)| failure.affected_regions.contains(&status.facility_id))
            .map(|(id, _)| id.clone())
            .collect()
    }
    
    fn get_critical_facilities(&self, facilities: &[FacilityId]) -> Vec<FacilityId> {
        facilities.iter()
            .filter(|id| {
                self.cold_start.startup_priority.get(*id)
                    .map(|p| *p <= StartupPriority::Critical)
                    .unwrap_or(false)
            })
            .cloned()
            .collect()
    }
    
    async fn request_priority_restoration(&mut self, facility_id: &FacilityId) {
        self.restoration_queue.push(RestorationRequest {
            facility_id: facility_id.clone(),
            priority: 100,
            critical_load_kw: 1000.0,
            requested_at: Timestamp::now(),
        });
    }
    
    async fn send_cascade_warnings(&self, failure: &GridFailure) -> Vec<CascadeWarning> {
        failure.affected_regions.iter()
            .map(|r| CascadeWarning {
                region: r.clone(),
                risk_level: "High".to_string(),
            })
            .collect()
    }
    
    async fn deploy_emergency_resources(&self, _critical: &[(FacilityId, String)]) -> Vec<ResourceDeployment> {
        vec![]
    }
    
    fn count_powered_facilities(&self) -> usize {
        self.power_manager.facilities.values()
            .filter(|s| s.power_source != PowerSource::None)
            .count()
    }
}

// =============================================================================
// TYPE DEFINITIONS
// =============================================================================

pub type FailureId = String;
pub type FacilityId = String;
pub type RegionId = String;
pub type Timestamp = u64;

impl Timestamp {
    pub fn now() -> Self {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs()
    }
}

#[derive(Debug, Clone)]
pub enum PowerTransitionResult {
    GeneratorStarted { facility_id: FacilityId, estimated_runtime: Duration },
    OnBattery { facility_id: FacilityId, estimated_runtime: Duration },
    PowerLost { facility_id: FacilityId },
    UnknownFacility,
}

#[derive(Debug, Clone)]
pub struct PowerStatusReport {
    pub critical_issues: Vec<(FacilityId, String)>,
    pub warnings: Vec<(FacilityId, String)>,
}

impl PowerStatusReport {
    pub fn new() -> Self {
        Self { critical_issues: vec![], warnings: vec![] }
    }
    
    pub fn add_critical(&mut self, facility: FacilityId, issue: String) {
        self.critical_issues.push((facility, issue));
    }
    
    pub fn add_warning(&mut self, facility: FacilityId, warning: String) {
        self.warnings.push((facility, warning));
    }
}

#[derive(Debug, Clone)]
pub struct OptimizationResult {
    pub target_reduction_percent: f64,
    pub achieved_reduction_kw: f64,
    pub actions: Vec<OptimizationAction>,
}

#[derive(Debug, Clone)]
pub enum OptimizationAction {
    CriticalOnlyMode(FacilityId),
    LoadShedding(FacilityId, f64),
    ScheduleShift(FacilityId, Duration),
}

#[derive(Debug, Clone)]
pub struct StartupWave {
    pub wave_number: usize,
    pub facilities: Vec<FacilityId>,
    pub estimated_duration: Duration,
    pub prerequisites: Vec<FacilityId>,
}

#[derive(Debug, Clone)]
pub struct SatelliteUplink {
    pub id: String,
    pub bandwidth_mbps: f64,
    pub assigned_to: Option<FacilityId>,
}

#[derive(Debug, Clone)]
pub struct EmergencyPersonnel {
    pub id: String,
    pub role: String,
    pub location: Option<FacilityId>,
}

#[derive(Debug, Clone)]
pub enum ColdStartResult {
    Success { facility_id: FacilityId, startup_duration: Duration },
    DependenciesNotMet { missing: Vec<FacilityId> },
    Failed(ColdStartError),
}

#[derive(Debug, Clone)]
pub struct ColdStartError {
    pub phase: String,
    pub reason: String,
}

impl From<ColdStartError> for ColdStartResult {
    fn from(err: ColdStartError) -> Self {
        ColdStartResult::Failed(err)
    }
}

#[derive(Debug, Clone)]
pub struct GridFailureResponse {
    pub failure_id: FailureId,
    pub affected_facilities: usize,
    pub transitions: Vec<PowerTransitionResult>,
    pub optimization: Option<OptimizationResult>,
    pub cascade_warnings: Vec<CascadeWarning>,
}

impl GridFailureResponse {
    pub fn new(failure_id: FailureId) -> Self {
        Self {
            failure_id,
            affected_facilities: 0,
            transitions: vec![],
            optimization: None,
            cascade_warnings: vec![],
        }
    }
    
    pub fn add_transition(&mut self, transition: PowerTransitionResult) {
        self.transitions.push(transition);
    }
}

#[derive(Debug, Clone)]
pub struct CascadeWarning {
    pub region: RegionId,
    pub risk_level: String,
}

#[derive(Debug, Clone)]
pub struct ExtendedOutageStatus {
    pub failure_id: FailureId,
    pub duration: Duration,
    pub facilities_powered: usize,
    pub facilities_critical: usize,
    pub facilities_at_risk: usize,
    pub resource_deployments: Vec<ResourceDeployment>,
}

#[derive(Debug, Clone)]
pub struct ResourceDeployment {
    pub resource_type: String,
    pub target: FacilityId,
    pub eta: Duration,
}

#[derive(Debug, Clone)]
pub struct RestorationPlan {
    pub failure_id: FailureId,
    pub waves: Vec<StartupWave>,
    pub estimated_total_time: Duration,
    pub critical_facilities_first: bool,
}
