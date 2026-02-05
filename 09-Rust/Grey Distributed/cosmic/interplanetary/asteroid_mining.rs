//! Grey Distributed — Asteroid Mining Resource Governance
//!
//! Resource governance and coordination for asteroid mining operations.
//! Handles claim management, resource extraction tracking, and fair distribution.

use std::collections::HashMap;
use std::time::Duration;

// =============================================================================
// ASTEROID MINING CONTEXT
// =============================================================================
//
// Asteroid mining presents unique governance challenges:
//
// 1. VARIABLE LATENCY
//    - Near-Earth asteroids: seconds to minutes
//    - Main belt asteroids: 15-45 minutes
//    - Trojans: hours
//    - Must support autonomous operation at all latencies
//
// 2. CLAIM MANAGEMENT
//    - Who owns what resources?
//    - How to prevent conflicts?
//    - How to enforce agreements in space?
//
// 3. RESOURCE TYPES
//    - Water ice (life support, fuel)
//    - Precious metals (economic value)
//    - Rare earths (manufacturing)
//    - Construction materials (in-space industry)
//
// 4. OPERATIONAL CHALLENGES
//    - Zero-gravity mining operations
//    - Variable rotation periods
//    - Limited communication windows
//    - Emergency response at distance
//
// =============================================================================

/// Asteroid classification for mining purposes
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AsteroidClass {
    /// Carbon-rich, contains water and organics
    C,
    /// Silicate-rich
    S,
    /// Metallic, high metal content
    M,
    /// Mixed composition
    X,
}

impl AsteroidClass {
    /// Primary resource types available
    pub fn primary_resources(&self) -> Vec<ResourceType> {
        match self {
            Self::C => vec![
                ResourceType::WaterIce,
                ResourceType::Organics,
                ResourceType::Carbon,
            ],
            Self::S => vec![
                ResourceType::Silicates,
                ResourceType::RareEarths,
            ],
            Self::M => vec![
                ResourceType::Iron,
                ResourceType::Nickel,
                ResourceType::PreciousMetals,
                ResourceType::PlatinumGroup,
            ],
            Self::X => vec![
                ResourceType::Mixed,
            ],
        }
    }
}

/// Resource types available from asteroids
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResourceType {
    WaterIce,
    Organics,
    Carbon,
    Silicates,
    RareEarths,
    Iron,
    Nickel,
    PreciousMetals,
    PlatinumGroup,
    Mixed,
}

/// Asteroid location and orbital characteristics
#[derive(Debug, Clone)]
pub struct AsteroidBody {
    /// Unique identifier
    pub id: AsteroidId,
    
    /// Official designation (e.g., "16 Psyche")
    pub designation: String,
    
    /// Asteroid class
    pub class: AsteroidClass,
    
    /// Estimated mass (kg)
    pub mass_kg: f64,
    
    /// Diameter (meters)
    pub diameter_m: f64,
    
    /// Orbital region
    pub region: OrbitalRegion,
    
    /// Current distance from Earth (AU)
    pub earth_distance_au: f64,
    
    /// Communication latency to Earth (one-way)
    pub earth_latency: Duration,
    
    /// Rotation period
    pub rotation_period: Duration,
}

pub type AsteroidId = String;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OrbitalRegion {
    /// Near-Earth objects
    NearEarth,
    /// Mars-crossing
    MarsCrossing,
    /// Main asteroid belt
    MainBelt,
    /// Jupiter Trojans
    Trojans,
    /// Trans-Neptunian
    TransNeptunian,
}

impl OrbitalRegion {
    /// Typical latency range to Earth
    pub fn latency_range(&self) -> (Duration, Duration) {
        match self {
            Self::NearEarth => (Duration::from_secs(10), Duration::from_secs(300)),
            Self::MarsCrossing => (Duration::from_secs(180), Duration::from_secs(900)),
            Self::MainBelt => (Duration::from_secs(900), Duration::from_secs(2700)),
            Self::Trojans => (Duration::from_secs(2400), Duration::from_secs(3600)),
            Self::TransNeptunian => (Duration::from_secs(14400), Duration::from_secs(28800)),
        }
    }
}

// =============================================================================
// MINING CLAIMS AND GOVERNANCE
// =============================================================================

/// A mining claim on an asteroid
#[derive(Debug, Clone)]
pub struct MiningClaim {
    /// Claim identifier
    pub claim_id: ClaimId,
    
    /// Asteroid this claim is on
    pub asteroid_id: AsteroidId,
    
    /// Claiming entity
    pub claimant: EntityId,
    
    /// Claim type
    pub claim_type: ClaimType,
    
    /// Geographic bounds on asteroid (if partial)
    pub bounds: Option<ClaimBounds>,
    
    /// Resource types covered
    pub resources: Vec<ResourceType>,
    
    /// Claim status
    pub status: ClaimStatus,
    
    /// When claim was registered
    pub registered: std::time::Instant,
    
    /// Claim expiration (if any)
    pub expires: Option<std::time::Instant>,
    
    /// Required activity to maintain claim
    pub activity_requirement: ActivityRequirement,
}

pub type ClaimId = String;
pub type EntityId = String;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClaimType {
    /// Full asteroid claim
    Full,
    /// Partial surface/volume claim
    Partial,
    /// Specific resource type only
    ResourceSpecific,
    /// Temporary exploration claim
    Exploration,
    /// Shared/consortium claim
    Shared,
}

#[derive(Debug, Clone)]
pub struct ClaimBounds {
    /// Center point (lat/lon on asteroid surface)
    pub center: (f64, f64),
    /// Radius from center (meters)
    pub radius_m: f64,
    /// Depth claim (if subsurface)
    pub depth_m: Option<f64>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClaimStatus {
    /// Pending registration
    Pending,
    /// Active and valid
    Active,
    /// Contested by another party
    Contested,
    /// Suspended due to inactivity
    Suspended,
    /// Expired or abandoned
    Expired,
    /// Revoked
    Revoked,
}

#[derive(Debug, Clone)]
pub struct ActivityRequirement {
    /// Minimum operations per period
    pub min_operations: u32,
    /// Period for minimum operations (e.g., per year)
    pub period: Duration,
    /// Minimum resource extraction
    pub min_extraction_kg: f64,
}

/// Mining operation record
#[derive(Debug, Clone)]
pub struct MiningOperation {
    /// Operation identifier
    pub operation_id: OperationId,
    
    /// Associated claim
    pub claim_id: ClaimId,
    
    /// Operating entity
    pub operator: EntityId,
    
    /// Operation type
    pub operation_type: OperationType,
    
    /// Resources extracted
    pub extraction: ResourceExtraction,
    
    /// Operation period
    pub start: std::time::Instant,
    pub end: Option<std::time::Instant>,
    
    /// Status
    pub status: OperationStatus,
}

pub type OperationId = String;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperationType {
    /// Surface mining
    SurfaceMining,
    /// Subsurface extraction
    SubsurfaceMining,
    /// Processing on-site
    Processing,
    /// Transport preparation
    TransportPrep,
}

#[derive(Debug, Clone)]
pub struct ResourceExtraction {
    /// Resources extracted by type
    pub by_type: HashMap<ResourceType, f64>, // kg
    /// Total mass extracted
    pub total_kg: f64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperationStatus {
    Planned,
    Active,
    Paused,
    Completed,
    Abandoned,
}

// =============================================================================
// RESOURCE GOVERNANCE ENGINE
// =============================================================================

/// Main governance engine for asteroid mining
pub struct AsteroidMiningGovernance {
    /// Known asteroids
    asteroids: HashMap<AsteroidId, AsteroidBody>,
    
    /// Registered claims
    claims: HashMap<ClaimId, MiningClaim>,
    
    /// Active operations
    operations: HashMap<OperationId, MiningOperation>,
    
    /// Resource tracking
    resource_ledger: ResourceLedger,
    
    /// Dispute resolution state
    disputes: Vec<Dispute>,
    
    /// Configuration
    config: GovernanceConfig,
}

#[derive(Debug, Clone)]
pub struct ResourceLedger {
    /// Total extracted by resource type
    pub total_extracted: HashMap<ResourceType, f64>,
    
    /// Extracted by entity
    pub by_entity: HashMap<EntityId, HashMap<ResourceType, f64>>,
    
    /// Extracted by asteroid
    pub by_asteroid: HashMap<AsteroidId, HashMap<ResourceType, f64>>,
}

#[derive(Debug, Clone)]
pub struct Dispute {
    pub dispute_id: String,
    pub claim_ids: Vec<ClaimId>,
    pub parties: Vec<EntityId>,
    pub dispute_type: DisputeType,
    pub status: DisputeStatus,
    pub filed: std::time::Instant,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DisputeType {
    /// Overlapping claims
    OverlappingClaims,
    /// Activity requirement not met
    InactivityChallenge,
    /// Resource theft accusation
    ResourceTheft,
    /// Boundary dispute
    BoundaryDispute,
    /// Safety violation
    SafetyViolation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DisputeStatus {
    Filed,
    UnderReview,
    Mediation,
    Arbitration,
    Resolved,
    Appealed,
}

#[derive(Debug, Clone)]
pub struct GovernanceConfig {
    /// Minimum claim validity period
    pub min_claim_period: Duration,
    
    /// Maximum claim without activity
    pub max_inactive_period: Duration,
    
    /// Dispute resolution timeout
    pub dispute_timeout: Duration,
    
    /// Consensus requirements for governance decisions
    pub consensus_requirements: ConsensusRequirements,
}

#[derive(Debug, Clone)]
pub struct ConsensusRequirements {
    /// Quorum for claim approval
    pub claim_approval_quorum: f64,
    
    /// Quorum for dispute resolution
    pub dispute_resolution_quorum: f64,
    
    /// Supermajority threshold
    pub supermajority: f64,
}

impl AsteroidMiningGovernance {
    /// Create new governance instance
    pub fn new(config: GovernanceConfig) -> Self {
        Self {
            asteroids: HashMap::new(),
            claims: HashMap::new(),
            operations: HashMap::new(),
            resource_ledger: ResourceLedger {
                total_extracted: HashMap::new(),
                by_entity: HashMap::new(),
                by_asteroid: HashMap::new(),
            },
            disputes: Vec::new(),
            config,
        }
    }
    
    /// Register a new asteroid
    pub fn register_asteroid(&mut self, asteroid: AsteroidBody) -> Result<(), GovernanceError> {
        if self.asteroids.contains_key(&asteroid.id) {
            return Err(GovernanceError::AsteroidAlreadyRegistered);
        }
        
        self.asteroids.insert(asteroid.id.clone(), asteroid);
        Ok(())
    }
    
    /// File a mining claim
    pub async fn file_claim(&mut self, claim: MiningClaim) -> Result<ClaimId, GovernanceError> {
        // Validate asteroid exists
        if !self.asteroids.contains_key(&claim.asteroid_id) {
            return Err(GovernanceError::AsteroidNotFound);
        }
        
        // Check for overlapping claims
        if self.has_overlapping_claims(&claim) {
            return Err(GovernanceError::OverlappingClaim);
        }
        
        // Validate claimant
        if !self.validate_claimant(&claim.claimant) {
            return Err(GovernanceError::InvalidClaimant);
        }
        
        let claim_id = claim.claim_id.clone();
        self.claims.insert(claim_id.clone(), claim);
        
        Ok(claim_id)
    }
    
    /// Record resource extraction
    pub fn record_extraction(
        &mut self,
        claim_id: &ClaimId,
        extraction: ResourceExtraction,
    ) -> Result<(), GovernanceError> {
        let claim = self.claims.get(claim_id)
            .ok_or(GovernanceError::ClaimNotFound)?;
        
        if claim.status != ClaimStatus::Active {
            return Err(GovernanceError::ClaimNotActive);
        }
        
        // Update ledger
        for (resource, amount) in &extraction.by_type {
            *self.resource_ledger.total_extracted
                .entry(*resource).or_insert(0.0) += amount;
            
            *self.resource_ledger.by_entity
                .entry(claim.claimant.clone())
                .or_insert_with(HashMap::new)
                .entry(*resource).or_insert(0.0) += amount;
            
            *self.resource_ledger.by_asteroid
                .entry(claim.asteroid_id.clone())
                .or_insert_with(HashMap::new)
                .entry(*resource).or_insert(0.0) += amount;
        }
        
        Ok(())
    }
    
    /// File a dispute
    pub fn file_dispute(
        &mut self,
        claim_ids: Vec<ClaimId>,
        parties: Vec<EntityId>,
        dispute_type: DisputeType,
    ) -> Result<String, GovernanceError> {
        let dispute_id = format!("DSP-{}", self.disputes.len() + 1);
        
        self.disputes.push(Dispute {
            dispute_id: dispute_id.clone(),
            claim_ids,
            parties,
            dispute_type,
            status: DisputeStatus::Filed,
            filed: std::time::Instant::now(),
        });
        
        Ok(dispute_id)
    }
    
    /// Resolve a dispute
    pub async fn resolve_dispute(
        &mut self,
        dispute_id: &str,
        resolution: DisputeResolution,
    ) -> Result<(), GovernanceError> {
        let dispute = self.disputes.iter_mut()
            .find(|d| d.dispute_id == dispute_id)
            .ok_or(GovernanceError::DisputeNotFound)?;
        
        // Apply resolution
        match &resolution.action {
            ResolutionAction::AffirmClaim(claim_id) => {
                if let Some(claim) = self.claims.get_mut(claim_id) {
                    claim.status = ClaimStatus::Active;
                }
            }
            ResolutionAction::RevokeClaim(claim_id) => {
                if let Some(claim) = self.claims.get_mut(claim_id) {
                    claim.status = ClaimStatus::Revoked;
                }
            }
            ResolutionAction::SplitClaim { original, new_claims } => {
                // Handle claim splitting
                if let Some(claim) = self.claims.get_mut(original) {
                    claim.status = ClaimStatus::Revoked;
                }
                for new_claim in new_claims {
                    self.claims.insert(new_claim.claim_id.clone(), new_claim.clone());
                }
            }
            ResolutionAction::Compensation { from, to, amount } => {
                // Handle compensation (integration with economics)
                self.process_compensation(from, to, *amount);
            }
        }
        
        dispute.status = DisputeStatus::Resolved;
        Ok(())
    }
    
    // --- Helper methods ---
    
    fn has_overlapping_claims(&self, claim: &MiningClaim) -> bool {
        for existing in self.claims.values() {
            if existing.asteroid_id == claim.asteroid_id 
                && existing.status == ClaimStatus::Active {
                // Check for overlap based on claim type and bounds
                if claim.claim_type == ClaimType::Full 
                    || existing.claim_type == ClaimType::Full {
                    return true;
                }
                // More sophisticated overlap detection would go here
            }
        }
        false
    }
    
    fn validate_claimant(&self, _entity: &EntityId) -> bool {
        true // Placeholder
    }
    
    fn process_compensation(&mut self, _from: &EntityId, _to: &EntityId, _amount: f64) {
        // Integration with economics system
    }
}

#[derive(Debug, Clone)]
pub struct DisputeResolution {
    pub action: ResolutionAction,
    pub rationale: String,
}

#[derive(Debug, Clone)]
pub enum ResolutionAction {
    AffirmClaim(ClaimId),
    RevokeClaim(ClaimId),
    SplitClaim {
        original: ClaimId,
        new_claims: Vec<MiningClaim>,
    },
    Compensation {
        from: EntityId,
        to: EntityId,
        amount: f64,
    },
}

#[derive(Debug, Clone)]
pub enum GovernanceError {
    AsteroidAlreadyRegistered,
    AsteroidNotFound,
    OverlappingClaim,
    InvalidClaimant,
    ClaimNotFound,
    ClaimNotActive,
    DisputeNotFound,
}

// =============================================================================
// HEALTH MONITORING
// =============================================================================

/// Mining governance health for dashboard reporting
#[derive(Debug, Clone)]
pub struct AsteroidMiningHealth {
    /// Total registered asteroids
    pub asteroids_registered: usize,
    
    /// Active claims
    pub active_claims: usize,
    
    /// Pending claims
    pub pending_claims: usize,
    
    /// Active disputes
    pub active_disputes: usize,
    
    /// Total extracted resources
    pub total_extraction: HashMap<ResourceType, f64>,
    
    /// Recent extraction rate (kg/day)
    pub extraction_rate: f64,
    
    /// Governance health score
    pub health_score: f64,
}

impl AsteroidMiningGovernance {
    /// Get health status for dashboard reporting
    pub fn health_status(&self) -> AsteroidMiningHealth {
        let active_claims = self.claims.values()
            .filter(|c| c.status == ClaimStatus::Active)
            .count();
        
        let pending_claims = self.claims.values()
            .filter(|c| c.status == ClaimStatus::Pending)
            .count();
        
        let active_disputes = self.disputes.iter()
            .filter(|d| !matches!(d.status, DisputeStatus::Resolved))
            .count();
        
        AsteroidMiningHealth {
            asteroids_registered: self.asteroids.len(),
            active_claims,
            pending_claims,
            active_disputes,
            total_extraction: self.resource_ledger.total_extracted.clone(),
            extraction_rate: 0.0, // Would calculate from recent operations
            health_score: self.calculate_health_score(active_claims, active_disputes),
        }
    }
    
    fn calculate_health_score(&self, active_claims: usize, active_disputes: usize) -> f64 {
        let dispute_ratio = if active_claims > 0 {
            active_disputes as f64 / active_claims as f64
        } else {
            0.0
        };
        
        (1.0 - dispute_ratio).max(0.0)
    }
}

// =============================================================================
// TESTS
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_asteroid_class_resources() {
        let m_class = AsteroidClass::M;
        let resources = m_class.primary_resources();
        assert!(resources.contains(&ResourceType::PreciousMetals));
    }
    
    #[test]
    fn test_orbital_region_latency() {
        let (min, max) = OrbitalRegion::MainBelt.latency_range();
        assert!(min < max);
        assert!(min >= Duration::from_secs(900));
    }
}
