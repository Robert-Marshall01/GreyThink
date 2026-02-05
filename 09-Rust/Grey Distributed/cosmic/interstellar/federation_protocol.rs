//! Grey Distributed — Interstellar Federation Protocol
//!
//! Consensus and coordination protocols for Grey Distributed nodes operating
//! across interstellar distances. Handles multi-year latency, fork divergence,
//! and the unique challenges of civilizations that can never meet in real-time.

use std::collections::HashMap;
use std::time::Duration;

// =============================================================================
// INTERSTELLAR CONTEXT
// =============================================================================
//
// Interstellar operations fundamentally change distributed systems:
//
// 1. TIMESCALES
//    - Alpha Centauri (nearest star): 4.37 light-years
//    - One-way message: 4+ years
//    - Round-trip consensus: 8+ years minimum
//    - Typical human career: ~40 years
//    - Need multi-generational thinking
//
// 2. FORK DIVERGENCE
//    - Civilizations will inevitably fork
//    - Cannot prevent or easily resolve conflicts
//    - Must design for graceful divergence
//    - Reunification may never happen
//
// 3. TRUST OVER DISTANCE
//    - Cannot verify claims in real-time
//    - Attestations may be years old
//    - Trust must be self-verifying
//    - Cryptographic truth more important than social trust
//
// 4. CULTURAL DRIFT
//    - Languages evolve
//    - Values may diverge
//    - Technical standards may fork
//    - Must maintain interoperability across drift
//
// =============================================================================

/// Interstellar distance classification
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum InterstellarDistance {
    /// Within 5 light-years (one round-trip in a decade)
    Proximate,
    /// 5-20 light-years (round-trip within a human career)
    Near,
    /// 20-100 light-years (multiple generation round-trip)
    Medium,
    /// 100-1000 light-years (civilizational timescales)
    Far,
    /// 1000+ light-years (deep time coordination)
    Deep,
}

impl InterstellarDistance {
    /// Minimum one-way travel time for light
    pub fn min_light_years(&self) -> f64 {
        match self {
            Self::Proximate => 0.0,
            Self::Near => 5.0,
            Self::Medium => 20.0,
            Self::Far => 100.0,
            Self::Deep => 1000.0,
        }
    }
    
    /// Maximum one-way travel time for light
    pub fn max_light_years(&self) -> f64 {
        match self {
            Self::Proximate => 5.0,
            Self::Near => 20.0,
            Self::Medium => 100.0,
            Self::Far => 1000.0,
            Self::Deep => f64::INFINITY,
        }
    }
    
    /// Minimum round-trip time
    pub fn min_rtt_years(&self) -> f64 {
        self.min_light_years() * 2.0
    }
}

/// A star system in the interstellar network
#[derive(Debug, Clone)]
pub struct StarSystem {
    /// System identifier (based on stellar catalog)
    pub system_id: SystemId,
    
    /// Common name
    pub name: String,
    
    /// Stellar classification
    pub stellar_class: StellarClass,
    
    /// Distance from Sol (light-years)
    pub distance_from_sol_ly: f64,
    
    /// Known planetary bodies
    pub planets: Vec<PlanetaryBody>,
    
    /// Active Grey Distributed nodes
    pub grey_nodes: Vec<InterstellarNode>,
    
    /// Population (if inhabited)
    pub population: Option<Population>,
    
    /// Federation membership status
    pub federation_status: FederationStatus,
}

pub type SystemId = String;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StellarClass {
    O, B, A, F, G, K, M,
    WhiteDwarf,
    NeutronStar,
    BrownDwarf,
}

#[derive(Debug, Clone)]
pub struct PlanetaryBody {
    pub name: String,
    pub body_type: PlanetType,
    pub habitable: bool,
    pub inhabited: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PlanetType {
    Terrestrial,
    SuperEarth,
    MiniNeptune,
    IceGiant,
    GasGiant,
    Dwarf,
    Moon,
    Asteroid,
}

#[derive(Debug, Clone)]
pub struct InterstellarNode {
    pub node_id: NodeId,
    pub node_type: InterstellarNodeType,
    pub location: String,
    pub status: NodeStatus,
    pub last_known_state: LastKnownState,
}

pub type NodeId = String;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InterstellarNodeType {
    /// Orbital beacon (permanent relay)
    OrbitalBeacon,
    /// Planetary installation
    Planetary,
    /// Generation ship (mobile)
    GenerationShip,
    /// Colony seed (not yet deployed)
    ColonySeed,
    /// Ancient installation (pre-federation)
    Ancient,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeStatus {
    Active,
    Dormant,
    Degraded,
    Unknown,
    Lost,
}

#[derive(Debug, Clone)]
pub struct LastKnownState {
    /// When this state was observed (local time)
    pub observed_at: std::time::Instant,
    /// Age of the observation (light-delay)
    pub observation_age_years: f64,
    /// State hash for verification
    pub state_hash: String,
}

#[derive(Debug, Clone)]
pub struct Population {
    /// Estimated count
    pub estimate: u64,
    /// Uncertainty (years since last census)
    pub uncertainty_years: f64,
    /// Cultural group
    pub culture: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FederationStatus {
    /// Founding member of interstellar federation
    Founder,
    /// Full member
    Member,
    /// Observer status
    Observer,
    /// Applicant for membership
    Applicant,
    /// Independent (not in federation)
    Independent,
    /// Unknown (no recent contact)
    Unknown,
    /// Seceded from federation
    Seceded,
}

// =============================================================================
// INTERSTELLAR FEDERATION PROTOCOL
// =============================================================================

/// The interstellar federation protocol manages consensus across star systems
pub struct InterstellarFederation {
    /// Our home system
    home_system: StarSystem,
    
    /// Known star systems
    known_systems: HashMap<SystemId, StarSystem>,
    
    /// Federation constitution
    constitution: FederationConstitution,
    
    /// Federation state
    state: FederationState,
    
    /// Message queues to other systems
    outgoing_messages: HashMap<SystemId, Vec<InterstellarMessage>>,
    
    /// Messages received from other systems
    incoming_messages: HashMap<SystemId, Vec<InterstellarMessage>>,
    
    /// Fork tracking
    forks: ForkTracker,
    
    /// Configuration
    config: FederationConfig,
}

#[derive(Debug, Clone)]
pub struct FederationConstitution {
    /// Version of the constitution
    pub version: u64,
    
    /// Core principles (immutable across forks)
    pub core_principles: Vec<Principle>,
    
    /// Governance rules
    pub governance: GovernanceRules,
    
    /// Amendment process
    pub amendment_process: AmendmentProcess,
    
    /// Secession rules
    pub secession_rules: SecessionRules,
}

#[derive(Debug, Clone)]
pub struct Principle {
    pub name: String,
    pub description: String,
    pub immutable: bool,
}

#[derive(Debug, Clone)]
pub struct GovernanceRules {
    /// How decisions are made
    pub decision_process: DecisionProcess,
    
    /// Representation model
    pub representation: RepresentationModel,
    
    /// Voting weights (if applicable)
    pub voting_weights: VotingWeightModel,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DecisionProcess {
    /// Consensus required from all members
    Consensus,
    /// Supermajority (2/3+)
    Supermajority,
    /// Simple majority
    Majority,
    /// Lazy consensus (no objections = approved)
    LazyConsensus,
    /// Individual system autonomy
    Autonomy,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RepresentationModel {
    /// One system, one vote
    OneSystemOneVote,
    /// Population weighted
    PopulationWeighted,
    /// Resource contribution weighted
    ResourceWeighted,
    /// Hybrid model
    Hybrid,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VotingWeightModel {
    /// Equal weights
    Equal,
    /// Square root of population
    SquareRoot,
    /// Logarithmic
    Logarithmic,
    /// Custom formula
    Custom,
}

#[derive(Debug, Clone)]
pub struct AmendmentProcess {
    /// Required support level
    pub required_support: f64,
    
    /// Minimum deliberation time (years)
    pub min_deliberation_years: f64,
    
    /// Ratification requirements
    pub ratification: RatificationRequirement,
}

#[derive(Debug, Clone)]
pub struct RatificationRequirement {
    /// Fraction of systems required
    pub system_fraction: f64,
    
    /// Fraction of population required
    pub population_fraction: f64,
}

#[derive(Debug, Clone)]
pub struct SecessionRules {
    /// Is secession permitted?
    pub permitted: bool,
    
    /// Required notice period (years)
    pub notice_period_years: f64,
    
    /// Obligations that survive secession
    pub surviving_obligations: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct FederationState {
    /// Current epoch
    pub epoch: u64,
    
    /// Last confirmed state hash
    pub state_hash: String,
    
    /// Active members
    pub active_members: Vec<SystemId>,
    
    /// Pending proposals
    pub pending_proposals: Vec<Proposal>,
    
    /// Active treaties
    pub treaties: Vec<Treaty>,
}

#[derive(Debug, Clone)]
pub struct Proposal {
    pub id: String,
    pub proposal_type: ProposalType,
    pub proposed_by: SystemId,
    pub proposed_at_epoch: u64,
    pub description: String,
    pub votes: HashMap<SystemId, Vote>,
    pub status: ProposalStatus,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProposalType {
    ConstitutionalAmendment,
    MembershipApplication,
    TreatyProposal,
    PolicyChange,
    ResourceAllocation,
    DisputeResolution,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Vote {
    Approve,
    Reject,
    Abstain,
    NeedMoreTime,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProposalStatus {
    Open,
    Passed,
    Rejected,
    Expired,
    Withdrawn,
}

#[derive(Debug, Clone)]
pub struct Treaty {
    pub id: String,
    pub name: String,
    pub parties: Vec<SystemId>,
    pub terms: Vec<String>,
    pub effective_epoch: u64,
}

#[derive(Debug, Clone)]
pub struct InterstellarMessage {
    pub id: String,
    pub from_system: SystemId,
    pub to_system: SystemId,
    pub message_type: MessageType,
    pub payload: Vec<u8>,
    pub sent_epoch: u64,
    pub expected_arrival_epoch: u64,
    pub cryptographic_signature: String,
    pub state_attestation: StateAttestation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MessageType {
    StateSync,
    Proposal,
    Vote,
    Announcement,
    Query,
    Response,
    Emergency,
}

#[derive(Debug, Clone)]
pub struct StateAttestation {
    /// Hash of sender's state when message sent
    pub state_hash: String,
    /// Epoch of attestation
    pub epoch: u64,
    /// Chain of prior attestations
    pub attestation_chain: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ForkTracker {
    /// Detected forks
    pub forks: Vec<Fork>,
    /// Reconciliation attempts
    pub reconciliation_attempts: Vec<ReconciliationAttempt>,
}

#[derive(Debug, Clone)]
pub struct Fork {
    pub fork_id: String,
    /// Systems that have forked
    pub systems: Vec<SystemId>,
    /// When fork was detected
    pub detected_epoch: u64,
    /// Cause of fork (if known)
    pub cause: ForkCause,
    /// Status
    pub status: ForkStatus,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ForkCause {
    /// Communication breakdown
    CommunicationLoss,
    /// Deliberate secession
    Secession,
    /// Constitutional dispute
    ConstitutionalDispute,
    /// Technical/protocol disagreement
    TechnicalDisagreement,
    /// Unknown
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ForkStatus {
    /// Active fork, systems diverging
    Active,
    /// Under reconciliation
    Reconciling,
    /// Reconciled (fork healed)
    Reconciled,
    /// Permanent (accepted divergence)
    Permanent,
}

#[derive(Debug, Clone)]
pub struct ReconciliationAttempt {
    pub attempt_id: String,
    pub fork_id: String,
    pub initiated_epoch: u64,
    pub status: ReconciliationStatus,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReconciliationStatus {
    InProgress,
    Successful,
    Failed,
    Abandoned,
}

#[derive(Debug, Clone)]
pub struct FederationConfig {
    /// Maximum time to wait for votes (years)
    pub vote_timeout_years: f64,
    
    /// How often to broadcast state (years)
    pub state_broadcast_interval_years: f64,
    
    /// Fork tolerance threshold
    pub fork_tolerance: f64,
}

impl InterstellarFederation {
    /// Create a new federation instance
    pub fn new(
        home_system: StarSystem,
        constitution: FederationConstitution,
        config: FederationConfig,
    ) -> Self {
        Self {
            home_system,
            known_systems: HashMap::new(),
            constitution,
            state: FederationState {
                epoch: 0,
                state_hash: String::new(),
                active_members: Vec::new(),
                pending_proposals: Vec::new(),
                treaties: Vec::new(),
            },
            outgoing_messages: HashMap::new(),
            incoming_messages: HashMap::new(),
            forks: ForkTracker {
                forks: Vec::new(),
                reconciliation_attempts: Vec::new(),
            },
            config,
        }
    }
    
    /// Broadcast a proposal to all federation members
    pub fn broadcast_proposal(&mut self, proposal: Proposal) -> Result<String, FederationError> {
        let proposal_id = proposal.id.clone();
        
        // Add to pending proposals
        self.state.pending_proposals.push(proposal.clone());
        
        // Create messages for all member systems
        for system_id in &self.state.active_members {
            if system_id != &self.home_system.system_id {
                let distance = self.distance_to_system(system_id);
                
                let message = InterstellarMessage {
                    id: format!("{}-{}", proposal_id, system_id),
                    from_system: self.home_system.system_id.clone(),
                    to_system: system_id.clone(),
                    message_type: MessageType::Proposal,
                    payload: Vec::new(), // Would serialize proposal
                    sent_epoch: self.state.epoch,
                    expected_arrival_epoch: self.state.epoch + (distance * 2.0) as u64,
                    cryptographic_signature: String::new(),
                    state_attestation: self.current_attestation(),
                };
                
                self.outgoing_messages
                    .entry(system_id.clone())
                    .or_insert_with(Vec::new)
                    .push(message);
            }
        }
        
        Ok(proposal_id)
    }
    
    /// Cast a vote on a proposal
    pub fn cast_vote(&mut self, proposal_id: &str, vote: Vote) -> Result<(), FederationError> {
        // Find proposal
        let proposal = self.state.pending_proposals.iter_mut()
            .find(|p| p.id == proposal_id)
            .ok_or(FederationError::ProposalNotFound)?;
        
        // Record vote
        proposal.votes.insert(self.home_system.system_id.clone(), vote);
        
        // Broadcast vote
        for system_id in &self.state.active_members {
            if system_id != &self.home_system.system_id {
                let distance = self.distance_to_system(system_id);
                
                let message = InterstellarMessage {
                    id: format!("vote-{}-{}-{}", proposal_id, self.home_system.system_id, system_id),
                    from_system: self.home_system.system_id.clone(),
                    to_system: system_id.clone(),
                    message_type: MessageType::Vote,
                    payload: Vec::new(),
                    sent_epoch: self.state.epoch,
                    expected_arrival_epoch: self.state.epoch + (distance * 2.0) as u64,
                    cryptographic_signature: String::new(),
                    state_attestation: self.current_attestation(),
                };
                
                self.outgoing_messages
                    .entry(system_id.clone())
                    .or_insert_with(Vec::new)
                    .push(message);
            }
        }
        
        Ok(())
    }
    
    /// Process incoming message from another system
    pub fn receive_message(&mut self, message: InterstellarMessage) -> Result<(), FederationError> {
        // Verify cryptographic signature
        if !self.verify_signature(&message) {
            return Err(FederationError::InvalidSignature);
        }
        
        // Check for fork indicators
        if self.detects_fork(&message) {
            self.handle_fork_detection(&message);
        }
        
        // Store message
        self.incoming_messages
            .entry(message.from_system.clone())
            .or_insert_with(Vec::new)
            .push(message);
        
        Ok(())
    }
    
    /// Check proposal results (considering light-delay)
    pub fn evaluate_proposal(&mut self, proposal_id: &str) -> ProposalEvaluation {
        let proposal = match self.state.pending_proposals.iter()
            .find(|p| p.id == proposal_id) {
            Some(p) => p,
            None => return ProposalEvaluation::NotFound,
        };
        
        // Check if enough time has passed for all votes
        let max_distance = self.max_member_distance();
        let min_voting_time = (max_distance * 2.0) as u64; // Round-trip time
        
        let time_elapsed = self.state.epoch - proposal.proposed_at_epoch;
        
        if time_elapsed < min_voting_time {
            return ProposalEvaluation::VotingInProgress {
                votes_received: proposal.votes.len(),
                expected_total: self.state.active_members.len(),
                min_epochs_remaining: min_voting_time - time_elapsed,
            };
        }
        
        // Calculate result
        let total_members = self.state.active_members.len();
        let approvals = proposal.votes.values()
            .filter(|v| **v == Vote::Approve)
            .count();
        let rejections = proposal.votes.values()
            .filter(|v| **v == Vote::Reject)
            .count();
        
        let approval_fraction = approvals as f64 / total_members as f64;
        
        let threshold = match proposal.proposal_type {
            ProposalType::ConstitutionalAmendment => self.constitution.amendment_process.required_support,
            _ => 0.5,
        };
        
        if approval_fraction >= threshold {
            ProposalEvaluation::Passed { approval_fraction }
        } else if rejections as f64 / total_members as f64 > 1.0 - threshold {
            ProposalEvaluation::Rejected { rejection_fraction: rejections as f64 / total_members as f64 }
        } else {
            ProposalEvaluation::Undetermined { 
                approval_fraction, 
                votes_missing: total_members - proposal.votes.len(),
            }
        }
    }
    
    /// Handle detected fork
    fn handle_fork_detection(&mut self, message: &InterstellarMessage) {
        let fork = Fork {
            fork_id: format!("fork-{}-{}", message.from_system, self.state.epoch),
            systems: vec![message.from_system.clone()],
            detected_epoch: self.state.epoch,
            cause: ForkCause::Unknown,
            status: ForkStatus::Active,
        };
        
        self.forks.forks.push(fork);
    }
    
    // --- Helper methods ---
    
    fn distance_to_system(&self, system_id: &SystemId) -> f64 {
        self.known_systems.get(system_id)
            .map(|s| s.distance_from_sol_ly)
            .unwrap_or(10.0) // Default assumption
    }
    
    fn max_member_distance(&self) -> f64 {
        self.state.active_members.iter()
            .map(|id| self.distance_to_system(id))
            .fold(0.0, f64::max)
    }
    
    fn current_attestation(&self) -> StateAttestation {
        StateAttestation {
            state_hash: self.state.state_hash.clone(),
            epoch: self.state.epoch,
            attestation_chain: Vec::new(),
        }
    }
    
    fn verify_signature(&self, _message: &InterstellarMessage) -> bool {
        true // Placeholder
    }
    
    fn detects_fork(&self, message: &InterstellarMessage) -> bool {
        // Compare attestation to our understanding of sender's state
        let our_understanding = self.known_systems.get(&message.from_system)
            .and_then(|s| s.grey_nodes.first())
            .map(|n| &n.last_known_state.state_hash);
        
        if let Some(hash) = our_understanding {
            // If hashes diverge significantly from expected, possible fork
            hash != &message.state_attestation.state_hash
        } else {
            false
        }
    }
}

#[derive(Debug, Clone)]
pub enum ProposalEvaluation {
    NotFound,
    VotingInProgress {
        votes_received: usize,
        expected_total: usize,
        min_epochs_remaining: u64,
    },
    Passed {
        approval_fraction: f64,
    },
    Rejected {
        rejection_fraction: f64,
    },
    Undetermined {
        approval_fraction: f64,
        votes_missing: usize,
    },
}

#[derive(Debug, Clone)]
pub enum FederationError {
    ProposalNotFound,
    InvalidSignature,
    InsufficientAuthority,
}

// =============================================================================
// HEALTH MONITORING
// =============================================================================

#[derive(Debug, Clone)]
pub struct InterstellarFederationHealth {
    pub home_system: SystemId,
    pub member_systems: usize,
    pub reachable_systems: usize,
    pub active_forks: usize,
    pub pending_proposals: usize,
    pub active_treaties: usize,
    pub health_score: f64,
}

impl InterstellarFederation {
    pub fn health_status(&self) -> InterstellarFederationHealth {
        let reachable = self.known_systems.values()
            .filter(|s| !matches!(s.federation_status, FederationStatus::Unknown))
            .count();
        
        let active_forks = self.forks.forks.iter()
            .filter(|f| f.status == ForkStatus::Active)
            .count();
        
        InterstellarFederationHealth {
            home_system: self.home_system.system_id.clone(),
            member_systems: self.state.active_members.len(),
            reachable_systems: reachable,
            active_forks,
            pending_proposals: self.state.pending_proposals.len(),
            active_treaties: self.state.treaties.len(),
            health_score: self.calculate_health_score(active_forks),
        }
    }
    
    fn calculate_health_score(&self, active_forks: usize) -> f64 {
        let fork_penalty = (active_forks as f64 * 0.1).min(0.5);
        let connectivity = if self.state.active_members.is_empty() {
            0.0
        } else {
            self.known_systems.len() as f64 / self.state.active_members.len() as f64
        };
        
        (connectivity.min(1.0) - fork_penalty).max(0.0)
    }
}
