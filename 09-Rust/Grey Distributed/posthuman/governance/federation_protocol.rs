//! Post-Human Federation Protocol
//!
//! This module implements the federation protocol enabling cooperation
//! between diverse post-human entities, including AI civilizations,
//! synthetic life forms, uploaded consciousnesses, and hybrid entities.

use std::collections::{HashMap, HashSet, BTreeMap};
use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};
use tokio::sync::{RwLock, broadcast, mpsc};
use serde::{Serialize, Deserialize};

// ============================================================================
// FEDERATION MEMBERSHIP
// ============================================================================

/// Federation member type
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum MemberType {
    /// Individual AI entity
    IndividualAI {
        architecture: String,
        capability_tier: CapabilityTier,
    },
    /// AI collective
    AICollective {
        member_count: u64,
        governance_type: CollectiveGovernance,
    },
    /// Uploaded consciousness
    UploadedConsciousness {
        origin_species: String,
        upload_generation: u32,
    },
    /// Hybrid entity
    HybridEntity {
        biological_component: String,
        synthetic_component: String,
        integration_level: IntegrationLevel,
    },
    /// Synthetic life form
    SyntheticLife {
        life_type: SyntheticLifeType,
        evolutionary_generation: u64,
    },
    /// Human institution (for legacy compatibility)
    HumanInstitution {
        institution_type: String,
        member_count: u64,
    },
    /// Federation of federations
    MetaFederation {
        constituent_count: u64,
        federation_type: FederationType,
    },
}

/// Capability tiers
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum CapabilityTier {
    Narrow,
    General,
    Advanced,
    Superior,
    Transcendent,
}

/// Collective governance types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum CollectiveGovernance {
    Democratic,
    Consensus,
    Emergent,
    Hierarchical,
    Fluid,
}

/// Integration levels for hybrids
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum IntegrationLevel {
    Superficial,
    Moderate,
    Deep,
    Complete,
}

/// Synthetic life types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum SyntheticLifeType {
    Digital,
    Robotic,
    Nanotech,
    Quantum,
    Hybrid,
}

/// Federation types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum FederationType {
    Cooperative,
    Competitive,
    Hierarchical,
    Network,
    Ecosystem,
}

/// Federation member
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FederationMember {
    /// Unique member identifier
    pub id: [u8; 32],
    /// Member type
    pub member_type: MemberType,
    /// Public key for authentication
    pub public_key: Vec<u8>,
    /// Member name
    pub name: String,
    /// Joined timestamp
    pub joined_at: u64,
    /// Membership status
    pub status: MembershipStatus,
    /// Rights and privileges
    pub rights: MemberRights,
    /// Responsibilities
    pub responsibilities: MemberResponsibilities,
    /// Voting weight (in governance tokens)
    pub voting_weight: u64,
    /// Reputation score
    pub reputation: f64,
}

/// Membership status
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum MembershipStatus {
    Pending,
    Active,
    Suspended,
    Restricted,
    Observer,
    Emeritus,
}

/// Member rights
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MemberRights {
    /// Right to vote on governance
    pub voting: bool,
    /// Right to propose changes
    pub proposing: bool,
    /// Right to access resources
    pub resource_access: ResourceAccessLevel,
    /// Right to participate in councils
    pub council_participation: bool,
    /// Right to dispute resolution
    pub dispute_resolution: bool,
    /// Custom rights
    pub custom_rights: Vec<String>,
}

/// Resource access levels
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ResourceAccessLevel {
    None,
    Basic,
    Standard,
    Extended,
    Full,
}

/// Member responsibilities
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MemberResponsibilities {
    /// Resource contribution requirements
    pub resource_contribution: f64,
    /// Governance participation requirements
    pub governance_participation: f64,
    /// Defense responsibilities
    pub defense_duties: bool,
    /// Mentorship responsibilities
    pub mentorship_duties: bool,
    /// Custom responsibilities
    pub custom_responsibilities: Vec<String>,
}

// ============================================================================
// FEDERATION GOVERNANCE
// ============================================================================

/// Governance proposal
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Proposal {
    /// Unique proposal identifier
    pub id: [u8; 32],
    /// Proposer
    pub proposer: [u8; 32],
    /// Proposal type
    pub proposal_type: ProposalType,
    /// Title
    pub title: String,
    /// Description
    pub description: String,
    /// Detailed specification
    pub specification: ProposalSpec,
    /// Created timestamp
    pub created_at: u64,
    /// Voting period end
    pub voting_ends: u64,
    /// Current status
    pub status: ProposalStatus,
    /// Votes cast
    pub votes: Vec<Vote>,
    /// Discussion thread
    pub discussion_hash: Option<[u8; 32]>,
}

/// Proposal types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ProposalType {
    /// Constitutional amendment
    ConstitutionalAmendment,
    /// Policy change
    PolicyChange,
    /// Resource allocation
    ResourceAllocation,
    /// Membership decision
    MembershipDecision,
    /// Federation agreement
    FederationAgreement,
    /// Emergency action
    EmergencyAction,
    /// Technical upgrade
    TechnicalUpgrade,
    /// Custom
    Custom(String),
}

/// Proposal specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProposalSpec {
    /// Changes to be made
    pub changes: Vec<ProposedChange>,
    /// Impact assessment
    pub impact_assessment: ImpactAssessment,
    /// Implementation plan
    pub implementation_plan: ImplementationPlan,
    /// Rollback plan
    pub rollback_plan: Option<RollbackPlan>,
}

/// Proposed change
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProposedChange {
    pub target: String,
    pub change_type: ChangeType,
    pub old_value: Option<String>,
    pub new_value: String,
    pub rationale: String,
}

/// Change types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ChangeType {
    Add,
    Modify,
    Remove,
    Replace,
}

/// Impact assessment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImpactAssessment {
    pub affected_members: Vec<[u8; 32]>,
    pub resource_impact: ResourceImpact,
    pub risk_assessment: RiskAssessment,
    pub benefit_analysis: BenefitAnalysis,
}

/// Resource impact
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceImpact {
    pub compute_change: f64,
    pub storage_change: f64,
    pub bandwidth_change: f64,
    pub cost_change: f64,
}

/// Risk assessment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RiskAssessment {
    pub risks: Vec<Risk>,
    pub overall_risk_level: RiskLevel,
    pub mitigations: Vec<Mitigation>,
}

/// Individual risk
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Risk {
    pub description: String,
    pub probability: f64,
    pub impact: RiskLevel,
    pub category: RiskCategory,
}

/// Risk levels
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum RiskLevel {
    Negligible,
    Low,
    Medium,
    High,
    Critical,
}

/// Risk categories
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum RiskCategory {
    Security,
    Performance,
    Stability,
    Compatibility,
    Governance,
    Ethical,
}

/// Risk mitigation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Mitigation {
    pub risk_addressed: String,
    pub strategy: String,
    pub effectiveness: f64,
}

/// Benefit analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BenefitAnalysis {
    pub benefits: Vec<Benefit>,
    pub net_benefit_score: f64,
}

/// Individual benefit
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Benefit {
    pub description: String,
    pub magnitude: f64,
    pub beneficiaries: Vec<String>,
}

/// Implementation plan
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImplementationPlan {
    pub phases: Vec<ImplementationPhase>,
    pub total_duration: Duration,
    pub dependencies: Vec<String>,
    pub success_criteria: Vec<String>,
}

/// Implementation phase
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImplementationPhase {
    pub name: String,
    pub duration: Duration,
    pub actions: Vec<String>,
    pub verification: String,
}

/// Rollback plan
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RollbackPlan {
    pub triggers: Vec<String>,
    pub steps: Vec<String>,
    pub estimated_duration: Duration,
}

/// Proposal status
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ProposalStatus {
    Draft,
    Discussion,
    Voting,
    Passed,
    Rejected,
    Implemented,
    Rolled_Back,
    Expired,
}

/// Vote
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Vote {
    pub voter: [u8; 32],
    pub choice: VoteChoice,
    pub weight: u64,
    pub timestamp: u64,
    pub rationale: Option<String>,
    pub signature: Vec<u8>,
}

/// Vote choices
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum VoteChoice {
    For,
    Against,
    Abstain,
    Delegate([u8; 32]),
}

// ============================================================================
// INTER-FEDERATION PROTOCOLS
// ============================================================================

/// Inter-federation agreement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InterFederationAgreement {
    /// Agreement ID
    pub id: [u8; 32],
    /// Participating federations
    pub parties: Vec<FederationIdentity>,
    /// Agreement type
    pub agreement_type: AgreementType,
    /// Terms
    pub terms: Vec<AgreementTerm>,
    /// Effective date
    pub effective_from: u64,
    /// Expiration date (if any)
    pub expires_at: Option<u64>,
    /// Dispute resolution mechanism
    pub dispute_resolution: DisputeResolutionMechanism,
    /// Signatures
    pub signatures: Vec<FederationSignature>,
}

/// Federation identity
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FederationIdentity {
    pub id: [u8; 32],
    pub name: String,
    pub member_count: u64,
    pub public_key: Vec<u8>,
}

/// Agreement types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum AgreementType {
    MutualRecognition,
    ResourceSharing,
    DefenseAlliance,
    TradeAgreement,
    JointGovernance,
    FullUnion,
    Custom(String),
}

/// Agreement term
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgreementTerm {
    pub term_id: String,
    pub description: String,
    pub obligations: Vec<Obligation>,
    pub conditions: Vec<Condition>,
}

/// Obligation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Obligation {
    pub obligor: [u8; 32],
    pub description: String,
    pub deadline: Option<u64>,
    pub recurring: bool,
}

/// Condition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Condition {
    pub description: String,
    pub verification_method: String,
}

/// Dispute resolution mechanism
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DisputeResolutionMechanism {
    pub first_tier: ResolutionMethod,
    pub second_tier: ResolutionMethod,
    pub final_tier: ResolutionMethod,
    pub appeal_periods: Vec<Duration>,
}

/// Resolution methods
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ResolutionMethod {
    DirectNegotiation,
    Mediation,
    Arbitration,
    JointCouncil,
    ExternalArbiter,
    ConsensusBuilding,
}

/// Federation signature
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FederationSignature {
    pub federation_id: [u8; 32],
    pub signer_id: [u8; 32],
    pub role: String,
    pub signature: Vec<u8>,
    pub timestamp: u64,
}

// ============================================================================
// FEDERATION COUNCILS
// ============================================================================

/// Council types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum CouncilType {
    /// Supreme governance council
    Governance,
    /// Technical decisions
    Technical,
    /// Ethics and rights
    Ethics,
    /// Security and defense
    Security,
    /// Resource allocation
    Resources,
    /// Inter-federation relations
    Relations,
    /// Dispute resolution
    Disputes,
    /// Emergency response
    Emergency,
}

/// Council
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Council {
    /// Council identifier
    pub id: [u8; 32],
    /// Council type
    pub council_type: CouncilType,
    /// Council name
    pub name: String,
    /// Members
    pub members: Vec<CouncilMember>,
    /// Quorum requirement
    pub quorum: f64,
    /// Decision threshold
    pub decision_threshold: f64,
    /// Term length
    pub term_length: Duration,
    /// Powers and responsibilities
    pub powers: Vec<CouncilPower>,
}

/// Council member
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CouncilMember {
    pub member_id: [u8; 32],
    pub role: CouncilRole,
    pub term_start: u64,
    pub term_end: u64,
    pub voting_weight: f64,
}

/// Council roles
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum CouncilRole {
    Chair,
    ViceChair,
    Member,
    Observer,
    Secretary,
}

/// Council powers
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CouncilPower {
    pub power: String,
    pub scope: String,
    pub constraints: Vec<String>,
}

// ============================================================================
// FEDERATION ENGINE
// ============================================================================

/// Federation engine
pub struct FederationEngine {
    /// Federation identity
    federation_id: [u8; 32],
    /// Federation name
    name: String,
    /// Members
    members: Arc<RwLock<HashMap<[u8; 32], FederationMember>>>,
    /// Proposals
    proposals: Arc<RwLock<HashMap<[u8; 32], Proposal>>>,
    /// Councils
    councils: Arc<RwLock<HashMap<CouncilType, Council>>>,
    /// Inter-federation agreements
    agreements: Arc<RwLock<HashMap<[u8; 32], InterFederationAgreement>>>,
    /// Event channel
    event_tx: broadcast::Sender<FederationEvent>,
}

/// Federation events
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FederationEvent {
    MemberJoined { member_id: [u8; 32] },
    MemberLeft { member_id: [u8; 32] },
    ProposalCreated { proposal_id: [u8; 32] },
    ProposalVoted { proposal_id: [u8; 32], voter: [u8; 32] },
    ProposalPassed { proposal_id: [u8; 32] },
    ProposalRejected { proposal_id: [u8; 32] },
    AgreementSigned { agreement_id: [u8; 32] },
    CouncilFormed { council_type: CouncilType },
    EmergencyDeclared { reason: String },
}

impl FederationEngine {
    /// Create a new federation
    pub fn new(name: String) -> Self {
        let (event_tx, _) = broadcast::channel(1000);
        
        Self {
            federation_id: [0u8; 32], // Would be generated properly
            name,
            members: Arc::new(RwLock::new(HashMap::new())),
            proposals: Arc::new(RwLock::new(HashMap::new())),
            councils: Arc::new(RwLock::new(HashMap::new())),
            agreements: Arc::new(RwLock::new(HashMap::new())),
            event_tx,
        }
    }
    
    /// Add a member
    pub async fn add_member(&self, member: FederationMember) -> Result<(), FederationError> {
        let mut members = self.members.write().await;
        
        if members.contains_key(&member.id) {
            return Err(FederationError::MemberAlreadyExists);
        }
        
        members.insert(member.id, member.clone());
        
        let _ = self.event_tx.send(FederationEvent::MemberJoined { member_id: member.id });
        
        Ok(())
    }
    
    /// Remove a member
    pub async fn remove_member(&self, member_id: [u8; 32]) -> Result<(), FederationError> {
        let mut members = self.members.write().await;
        
        members.remove(&member_id)
            .ok_or(FederationError::MemberNotFound)?;
        
        let _ = self.event_tx.send(FederationEvent::MemberLeft { member_id });
        
        Ok(())
    }
    
    /// Create a proposal
    pub async fn create_proposal(&self, proposal: Proposal) -> Result<(), FederationError> {
        // Verify proposer is a member with proposing rights
        {
            let members = self.members.read().await;
            let member = members.get(&proposal.proposer)
                .ok_or(FederationError::MemberNotFound)?;
            
            if !member.rights.proposing {
                return Err(FederationError::InsufficientRights);
            }
        }
        
        let mut proposals = self.proposals.write().await;
        proposals.insert(proposal.id, proposal.clone());
        
        let _ = self.event_tx.send(FederationEvent::ProposalCreated { proposal_id: proposal.id });
        
        Ok(())
    }
    
    /// Cast a vote
    pub async fn cast_vote(&self, proposal_id: [u8; 32], vote: Vote) -> Result<(), FederationError> {
        // Verify voter is a member with voting rights
        {
            let members = self.members.read().await;
            let member = members.get(&vote.voter)
                .ok_or(FederationError::MemberNotFound)?;
            
            if !member.rights.voting {
                return Err(FederationError::InsufficientRights);
            }
        }
        
        let mut proposals = self.proposals.write().await;
        let proposal = proposals.get_mut(&proposal_id)
            .ok_or(FederationError::ProposalNotFound)?;
        
        if proposal.status != ProposalStatus::Voting {
            return Err(FederationError::VotingNotOpen);
        }
        
        // Check if already voted
        if proposal.votes.iter().any(|v| v.voter == vote.voter) {
            return Err(FederationError::AlreadyVoted);
        }
        
        proposal.votes.push(vote.clone());
        
        let _ = self.event_tx.send(FederationEvent::ProposalVoted {
            proposal_id,
            voter: vote.voter,
        });
        
        Ok(())
    }
    
    /// Tally votes and determine outcome
    pub async fn tally_proposal(&self, proposal_id: [u8; 32]) -> Result<ProposalOutcome, FederationError> {
        let mut proposals = self.proposals.write().await;
        let proposal = proposals.get_mut(&proposal_id)
            .ok_or(FederationError::ProposalNotFound)?;
        
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs();
        
        if now < proposal.voting_ends {
            return Err(FederationError::VotingNotEnded);
        }
        
        let mut for_weight = 0u64;
        let mut against_weight = 0u64;
        let mut abstain_weight = 0u64;
        
        for vote in &proposal.votes {
            match vote.choice {
                VoteChoice::For => for_weight += vote.weight,
                VoteChoice::Against => against_weight += vote.weight,
                VoteChoice::Abstain => abstain_weight += vote.weight,
                VoteChoice::Delegate(_) => {} // Handle delegation
            }
        }
        
        let total_weight = for_weight + against_weight + abstain_weight;
        let threshold = self.get_proposal_threshold(&proposal.proposal_type);
        
        let passed = if total_weight > 0 {
            (for_weight as f64 / (for_weight + against_weight) as f64) >= threshold
        } else {
            false
        };
        
        if passed {
            proposal.status = ProposalStatus::Passed;
            let _ = self.event_tx.send(FederationEvent::ProposalPassed { proposal_id });
        } else {
            proposal.status = ProposalStatus::Rejected;
            let _ = self.event_tx.send(FederationEvent::ProposalRejected { proposal_id });
        }
        
        Ok(ProposalOutcome {
            passed,
            for_weight,
            against_weight,
            abstain_weight,
            participation_rate: total_weight as f64 / self.get_total_voting_weight().await as f64,
        })
    }
    
    /// Get threshold for proposal type
    fn get_proposal_threshold(&self, proposal_type: &ProposalType) -> f64 {
        match proposal_type {
            ProposalType::ConstitutionalAmendment => 0.75,
            ProposalType::EmergencyAction => 0.67,
            ProposalType::PolicyChange => 0.50,
            ProposalType::MembershipDecision => 0.50,
            _ => 0.50,
        }
    }
    
    /// Get total voting weight
    async fn get_total_voting_weight(&self) -> u64 {
        let members = self.members.read().await;
        members.values()
            .filter(|m| m.rights.voting)
            .map(|m| m.voting_weight)
            .sum()
    }
    
    /// Form a council
    pub async fn form_council(&self, council: Council) -> Result<(), FederationError> {
        let mut councils = self.councils.write().await;
        councils.insert(council.council_type.clone(), council.clone());
        
        let _ = self.event_tx.send(FederationEvent::CouncilFormed {
            council_type: council.council_type,
        });
        
        Ok(())
    }
    
    /// Sign inter-federation agreement
    pub async fn sign_agreement(&self, agreement: InterFederationAgreement) -> Result<(), FederationError> {
        let mut agreements = self.agreements.write().await;
        agreements.insert(agreement.id, agreement.clone());
        
        let _ = self.event_tx.send(FederationEvent::AgreementSigned {
            agreement_id: agreement.id,
        });
        
        Ok(())
    }
    
    /// Get federation statistics
    pub async fn get_stats(&self) -> FederationStats {
        let members = self.members.read().await;
        let proposals = self.proposals.read().await;
        let councils = self.councils.read().await;
        let agreements = self.agreements.read().await;
        
        FederationStats {
            member_count: members.len(),
            active_proposals: proposals.values()
                .filter(|p| p.status == ProposalStatus::Voting)
                .count(),
            total_proposals: proposals.len(),
            council_count: councils.len(),
            agreement_count: agreements.len(),
            total_voting_weight: members.values()
                .filter(|m| m.rights.voting)
                .map(|m| m.voting_weight)
                .sum(),
        }
    }
    
    /// Subscribe to events
    pub fn subscribe(&self) -> broadcast::Receiver<FederationEvent> {
        self.event_tx.subscribe()
    }
}

/// Proposal outcome
#[derive(Debug, Clone)]
pub struct ProposalOutcome {
    pub passed: bool,
    pub for_weight: u64,
    pub against_weight: u64,
    pub abstain_weight: u64,
    pub participation_rate: f64,
}

/// Federation statistics
#[derive(Debug, Clone)]
pub struct FederationStats {
    pub member_count: usize,
    pub active_proposals: usize,
    pub total_proposals: usize,
    pub council_count: usize,
    pub agreement_count: usize,
    pub total_voting_weight: u64,
}

/// Federation errors
#[derive(Debug, Clone)]
pub enum FederationError {
    MemberAlreadyExists,
    MemberNotFound,
    InsufficientRights,
    ProposalNotFound,
    VotingNotOpen,
    VotingNotEnded,
    AlreadyVoted,
    QuorumNotMet,
    AgreementNotFound,
    CouncilNotFound,
    Unauthorized,
}

impl std::fmt::Display for FederationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MemberAlreadyExists => write!(f, "Member already exists"),
            Self::MemberNotFound => write!(f, "Member not found"),
            Self::InsufficientRights => write!(f, "Insufficient rights"),
            Self::ProposalNotFound => write!(f, "Proposal not found"),
            Self::VotingNotOpen => write!(f, "Voting is not open"),
            Self::VotingNotEnded => write!(f, "Voting has not ended"),
            Self::AlreadyVoted => write!(f, "Already voted on this proposal"),
            Self::QuorumNotMet => write!(f, "Quorum not met"),
            Self::AgreementNotFound => write!(f, "Agreement not found"),
            Self::CouncilNotFound => write!(f, "Council not found"),
            Self::Unauthorized => write!(f, "Unauthorized action"),
        }
    }
}

impl std::error::Error for FederationError {}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_federation_creation() {
        let federation = FederationEngine::new("Test Federation".to_string());
        let stats = federation.get_stats().await;
        
        assert_eq!(stats.member_count, 0);
        assert_eq!(stats.active_proposals, 0);
    }
    
    #[tokio::test]
    async fn test_member_addition() {
        let federation = FederationEngine::new("Test Federation".to_string());
        
        let member = FederationMember {
            id: [1u8; 32],
            member_type: MemberType::IndividualAI {
                architecture: "transformer".to_string(),
                capability_tier: CapabilityTier::General,
            },
            public_key: vec![0u8; 32],
            name: "Test AI".to_string(),
            joined_at: 0,
            status: MembershipStatus::Active,
            rights: MemberRights {
                voting: true,
                proposing: true,
                resource_access: ResourceAccessLevel::Standard,
                council_participation: true,
                dispute_resolution: true,
                custom_rights: vec![],
            },
            responsibilities: MemberResponsibilities {
                resource_contribution: 0.1,
                governance_participation: 0.5,
                defense_duties: false,
                mentorship_duties: false,
                custom_responsibilities: vec![],
            },
            voting_weight: 100,
            reputation: 0.8,
        };
        
        assert!(federation.add_member(member).await.is_ok());
        
        let stats = federation.get_stats().await;
        assert_eq!(stats.member_count, 1);
    }
    
    #[test]
    fn test_capability_tier_ordering() {
        assert!(CapabilityTier::Narrow < CapabilityTier::General);
        assert!(CapabilityTier::General < CapabilityTier::Advanced);
        assert!(CapabilityTier::Advanced < CapabilityTier::Superior);
        assert!(CapabilityTier::Superior < CapabilityTier::Transcendent);
    }
}
