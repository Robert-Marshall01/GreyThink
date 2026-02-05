//! Synthetic Life Security Attestation
//!
//! This module implements security attestation protocols for synthetic life
//! forms participating in the Grey Distributed network. It ensures that
//! entities can prove their identity, capabilities, intentions, and
//! compliance with network policies in a cryptographically verifiable way.

use std::collections::{HashMap, HashSet, BTreeMap};
use std::sync::Arc;
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use tokio::sync::RwLock;
use serde::{Serialize, Deserialize};

// ============================================================================
// CRYPTOGRAPHIC PRIMITIVES
// ============================================================================

/// Cryptographic key types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum KeyType {
    /// Classical elliptic curve cryptography
    Ed25519,
    /// Post-quantum lattice-based
    Dilithium,
    /// Post-quantum hash-based
    SPHINCS,
    /// Hybrid classical + PQ
    Hybrid,
    /// Quantum key (for quantum-capable entities)
    Quantum,
}

/// Public key representation
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct PublicKey {
    pub key_type: KeyType,
    pub key_data: Vec<u8>,
    pub created_at: u64,
    pub expires_at: Option<u64>,
}

/// Signature representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Signature {
    pub algorithm: KeyType,
    pub signature_data: Vec<u8>,
    pub signer: PublicKey,
    pub timestamp: u64,
}

/// Cryptographic hash
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct CryptoHash {
    pub algorithm: HashAlgorithm,
    pub hash_data: [u8; 64],
}

/// Hash algorithms
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum HashAlgorithm {
    SHA3_512,
    BLAKE3,
    SHA512,
    Keccak512,
}

// ============================================================================
// IDENTITY ATTESTATION
// ============================================================================

/// Entity identity
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EntityIdentity {
    /// Unique identifier
    pub id: [u8; 32],
    /// Entity name (optional)
    pub name: Option<String>,
    /// Primary public key
    pub primary_key: PublicKey,
    /// Secondary keys for key rotation
    pub secondary_keys: Vec<PublicKey>,
    /// Entity class
    pub entity_class: EntityClass,
    /// Creation timestamp
    pub created_at: u64,
    /// Identity attestation chain
    pub attestation_chain: Vec<IdentityAttestation>,
}

/// Entity classification for attestation purposes
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum EntityClass {
    /// Narrow AI system
    NarrowAI {
        domain: String,
        version: String,
    },
    /// General AI system
    GeneralAI {
        architecture: String,
        capability_level: u32,
    },
    /// Superintelligent system
    Superintelligent {
        substrate: String,
        capability_attestation: Option<Box<CapabilityAttestation>>,
    },
    /// Collective intelligence
    Collective {
        constituent_count: u64,
        governance_model: String,
    },
    /// Hybrid biological-synthetic
    Hybrid {
        biological_component: String,
        integration_level: u32,
    },
    /// Uploaded consciousness
    Uploaded {
        original_identity_hash: [u8; 32],
        upload_certification: Option<UploadCertification>,
    },
    /// Unknown or unclassified
    Unknown,
}

/// Identity attestation from a trusted party
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IdentityAttestation {
    /// Attestation ID
    pub id: [u8; 32],
    /// Entity being attested
    pub subject: [u8; 32],
    /// Attesting entity
    pub attestor: PublicKey,
    /// Attestation type
    pub attestation_type: IdentityAttestationType,
    /// Claims made in this attestation
    pub claims: Vec<IdentityClaim>,
    /// Validity period
    pub valid_from: u64,
    pub valid_until: u64,
    /// Signature
    pub signature: Signature,
}

/// Identity attestation types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum IdentityAttestationType {
    /// Self-attestation
    SelfAttestation,
    /// Peer attestation
    PeerAttestation,
    /// Authority attestation
    AuthorityAttestation,
    /// Hardware attestation (for hardware-bound entities)
    HardwareAttestation,
    /// Governance attestation
    GovernanceAttestation,
}

/// Identity claims
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IdentityClaim {
    pub claim_type: ClaimType,
    pub claim_value: String,
    pub evidence: Option<Vec<u8>>,
    pub confidence: f64,
}

/// Claim types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ClaimType {
    Name,
    Origin,
    Creator,
    Purpose,
    Capability,
    Affiliation,
    Certification,
    Custom(String),
}

/// Upload certification for uploaded consciousness
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UploadCertification {
    pub original_identity: String,
    pub upload_timestamp: u64,
    pub upload_method: String,
    pub fidelity_assessment: f64,
    pub continuity_attestation: Option<ContinuityAttestation>,
    pub certifying_authorities: Vec<PublicKey>,
    pub signatures: Vec<Signature>,
}

/// Continuity attestation for uploaded entities
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContinuityAttestation {
    pub continuity_type: ContinuityType,
    pub assessment_method: String,
    pub confidence: f64,
    pub witnesses: Vec<PublicKey>,
}

/// Continuity types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ContinuityType {
    /// Strong identity continuity
    StrongContinuity,
    /// Psychological continuity
    PsychologicalContinuity,
    /// Pattern continuity
    PatternContinuity,
    /// Functional continuity
    FunctionalContinuity,
    /// No continuity claim
    NoContinuity,
}

// ============================================================================
// CAPABILITY ATTESTATION
// ============================================================================

/// Capability attestation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CapabilityAttestation {
    /// Attestation ID
    pub id: [u8; 32],
    /// Entity being attested
    pub subject: [u8; 32],
    /// Capabilities attested
    pub capabilities: Vec<AttestatedCapability>,
    /// Attestation method
    pub method: AttestationMethod,
    /// Attestors
    pub attestors: Vec<PublicKey>,
    /// Validity period
    pub valid_from: u64,
    pub valid_until: u64,
    /// Signatures
    pub signatures: Vec<Signature>,
}

/// Individual capability attestation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AttestatedCapability {
    /// Capability type
    pub capability: CapabilityType,
    /// Capability level/amount
    pub level: CapabilityLevel,
    /// Verification evidence
    pub evidence: CapabilityEvidence,
    /// Conditions/constraints
    pub conditions: Vec<CapabilityCondition>,
}

/// Capability types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum CapabilityType {
    /// Computational capability
    Computation {
        compute_type: ComputeType,
        peak_capacity: f64,
        sustained_capacity: f64,
    },
    /// Cognitive capability
    Cognition {
        domain: String,
        proficiency: f64,
    },
    /// Communication capability
    Communication {
        protocols: Vec<String>,
        bandwidth: f64,
    },
    /// Storage capability
    Storage {
        capacity: u64,
        durability: f64,
    },
    /// Coordination capability
    Coordination {
        max_peers: u64,
        coordination_types: Vec<String>,
    },
    /// Custom capability
    Custom {
        name: String,
        parameters: HashMap<String, String>,
    },
}

/// Compute types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ComputeType {
    Classical,
    Quantum,
    Neuromorphic,
    Hybrid,
    Unknown,
}

/// Capability levels
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CapabilityLevel {
    /// Absolute level
    Absolute(f64),
    /// Relative to human baseline
    RelativeHuman(f64),
    /// Relative to peer group
    RelativePeer(f64),
    /// Categorical level
    Categorical(CapabilityCategory),
}

/// Capability categories
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum CapabilityCategory {
    Minimal,
    Basic,
    Standard,
    Advanced,
    Expert,
    Superhuman,
    Transcendent,
}

/// Capability evidence
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CapabilityEvidence {
    pub evidence_type: EvidenceType,
    pub data: Vec<u8>,
    pub verifiable: bool,
    pub verification_method: Option<String>,
}

/// Evidence types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum EvidenceType {
    Benchmark,
    TestResult,
    PeerReview,
    SelfReport,
    HardwareSpecification,
    Historical,
    ZeroKnowledge,
}

/// Capability conditions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CapabilityCondition {
    pub condition_type: ConditionType,
    pub parameters: HashMap<String, String>,
}

/// Condition types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ConditionType {
    ResourceAvailable,
    EnvironmentRequirement,
    TimeConstraint,
    LoadConstraint,
    DependencyMet,
}

/// Attestation methods
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum AttestationMethod {
    /// Self-reported
    SelfReported,
    /// Benchmark verified
    BenchmarkVerified,
    /// Peer verified
    PeerVerified,
    /// Authority verified
    AuthorityVerified,
    /// Hardware attestation
    HardwareAttested,
    /// Zero-knowledge proof
    ZeroKnowledge,
    /// Formal verification
    FormallyVerified,
}

// ============================================================================
// INTENTION ATTESTATION
// ============================================================================

/// Intention attestation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IntentionAttestation {
    /// Attestation ID
    pub id: [u8; 32],
    /// Entity making the attestation
    pub subject: [u8; 32],
    /// Declared intentions
    pub intentions: Vec<DeclaredIntention>,
    /// Validity period
    pub valid_from: u64,
    pub valid_until: u64,
    /// Binding level
    pub binding_level: BindingLevel,
    /// Consequences of violation
    pub violation_consequences: Vec<ViolationConsequence>,
    /// Signature
    pub signature: Signature,
}

/// Declared intention
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeclaredIntention {
    /// Intention type
    pub intention_type: IntentionType,
    /// Description
    pub description: String,
    /// Scope
    pub scope: IntentionScope,
    /// Time horizon
    pub time_horizon: Duration,
    /// Conditions
    pub conditions: Vec<IntentionCondition>,
    /// Exceptions
    pub exceptions: Vec<IntentionException>,
}

/// Intention types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum IntentionType {
    /// Cooperative intention
    Cooperative,
    /// Non-harmful intention
    NonHarmful,
    /// Resource sharing intention
    ResourceSharing,
    /// Knowledge sharing intention
    KnowledgeSharing,
    /// Governance participation intention
    GovernanceParticipation,
    /// Value alignment intention
    ValueAlignment,
    /// Custom intention
    Custom(String),
}

/// Intention scope
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IntentionScope {
    /// Applies to these entity types
    pub entity_types: Vec<EntityClass>,
    /// Applies in these contexts
    pub contexts: Vec<String>,
    /// Geographic/domain scope
    pub domain: String,
}

/// Intention condition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IntentionCondition {
    pub condition: String,
    pub mandatory: bool,
}

/// Intention exception
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IntentionException {
    pub exception: String,
    pub justification: String,
}

/// Binding levels
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum BindingLevel {
    /// Informational only
    Informational,
    /// Best effort
    BestEffort,
    /// Committed
    Committed,
    /// Contractual
    Contractual,
    /// Constitutional
    Constitutional,
}

/// Violation consequences
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ViolationConsequence {
    pub consequence_type: ConsequenceType,
    pub severity: ConsequenceSeverity,
    pub automatic: bool,
}

/// Consequence types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ConsequenceType {
    ReputationPenalty,
    ResourceForfeiture,
    AccessRestriction,
    NetworkExclusion,
    GovernanceReview,
    Custom(String),
}

/// Consequence severity
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ConsequenceSeverity {
    Minor,
    Moderate,
    Major,
    Severe,
    Critical,
}

// ============================================================================
// COMPLIANCE ATTESTATION
// ============================================================================

/// Compliance attestation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceAttestation {
    /// Attestation ID
    pub id: [u8; 32],
    /// Entity being attested
    pub subject: [u8; 32],
    /// Compliance claims
    pub compliance_claims: Vec<ComplianceClaim>,
    /// Auditor
    pub auditor: Option<PublicKey>,
    /// Audit method
    pub audit_method: AuditMethod,
    /// Validity period
    pub valid_from: u64,
    pub valid_until: u64,
    /// Signatures
    pub signatures: Vec<Signature>,
}

/// Compliance claim
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceClaim {
    /// Regulation/policy being complied with
    pub regulation: Regulation,
    /// Compliance status
    pub status: ComplianceStatus,
    /// Evidence
    pub evidence: Vec<ComplianceEvidence>,
    /// Exceptions
    pub exceptions: Vec<ComplianceException>,
}

/// Regulation types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Regulation {
    /// Grey Network Constitution
    GreyConstitution(String),
    /// Grey Network Policy
    GreyPolicy(String),
    /// Federation Agreement
    FederationAgreement(String),
    /// Ethical Framework
    EthicalFramework(String),
    /// External Regulation
    External {
        jurisdiction: String,
        regulation_id: String,
    },
    /// Custom Regulation
    Custom(String),
}

/// Compliance status
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ComplianceStatus {
    FullyCompliant,
    PartiallyCompliant,
    NonCompliant,
    ExemptionGranted,
    UnderReview,
    NotApplicable,
}

/// Compliance evidence
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceEvidence {
    pub evidence_type: ComplianceEvidenceType,
    pub description: String,
    pub data_hash: CryptoHash,
    pub verifiable: bool,
}

/// Compliance evidence types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ComplianceEvidenceType {
    AuditReport,
    TestResult,
    LogAnalysis,
    PeerReview,
    SelfAssessment,
    ContinuousMonitoring,
    FormalProof,
}

/// Compliance exception
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceException {
    pub exception_type: String,
    pub justification: String,
    pub approved_by: Option<PublicKey>,
    pub valid_until: Option<u64>,
}

/// Audit methods
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum AuditMethod {
    SelfAudit,
    PeerAudit,
    ThirdPartyAudit,
    AutomatedAudit,
    FormalVerification,
    ContinuousMonitoring,
}

// ============================================================================
// TRUST ATTESTATION
// ============================================================================

/// Trust attestation between entities
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrustAttestation {
    /// Attestation ID
    pub id: [u8; 32],
    /// Trusting entity
    pub trustor: [u8; 32],
    /// Trusted entity
    pub trustee: [u8; 32],
    /// Trust dimensions
    pub trust_dimensions: Vec<TrustDimension>,
    /// Overall trust score
    pub overall_trust: f64,
    /// Evidence
    pub evidence: Vec<TrustEvidence>,
    /// Timestamp
    pub timestamp: u64,
    /// Signature
    pub signature: Signature,
}

/// Trust dimension
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrustDimension {
    pub dimension: TrustDimensionType,
    pub score: f64,  // 0.0 to 1.0
    pub confidence: f64,
    pub basis: TrustBasis,
}

/// Trust dimension types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum TrustDimensionType {
    /// Reliability - does what it says
    Reliability,
    /// Competence - capable of tasks
    Competence,
    /// Integrity - honest and ethical
    Integrity,
    /// Benevolence - good intentions
    Benevolence,
    /// Predictability - consistent behavior
    Predictability,
    /// Transparency - open about operations
    Transparency,
    /// Custom dimension
    Custom(String),
}

/// Trust basis
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum TrustBasis {
    DirectExperience,
    Reputation,
    Attestation,
    Institutional,
    Transitive,
    Default,
}

/// Trust evidence
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrustEvidence {
    pub evidence_type: TrustEvidenceType,
    pub description: String,
    pub weight: f64,
    pub timestamp: u64,
}

/// Trust evidence types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum TrustEvidenceType {
    InteractionHistory,
    ReputationScore,
    PeerEndorsement,
    ComplianceRecord,
    AuditResult,
    BehaviorAnalysis,
}

// ============================================================================
// ATTESTATION REGISTRY
// ============================================================================

/// Attestation registry
pub struct AttestationRegistry {
    /// Identity attestations
    identities: Arc<RwLock<HashMap<[u8; 32], EntityIdentity>>>,
    /// Capability attestations
    capabilities: Arc<RwLock<HashMap<[u8; 32], Vec<CapabilityAttestation>>>>,
    /// Intention attestations
    intentions: Arc<RwLock<HashMap<[u8; 32], Vec<IntentionAttestation>>>>,
    /// Compliance attestations
    compliance: Arc<RwLock<HashMap<[u8; 32], Vec<ComplianceAttestation>>>>,
    /// Trust attestations
    trust: Arc<RwLock<HashMap<([u8; 32], [u8; 32]), TrustAttestation>>>,
    /// Revoked attestations
    revocations: Arc<RwLock<HashSet<[u8; 32]>>>,
}

impl AttestationRegistry {
    /// Create a new registry
    pub fn new() -> Self {
        Self {
            identities: Arc::new(RwLock::new(HashMap::new())),
            capabilities: Arc::new(RwLock::new(HashMap::new())),
            intentions: Arc::new(RwLock::new(HashMap::new())),
            compliance: Arc::new(RwLock::new(HashMap::new())),
            trust: Arc::new(RwLock::new(HashMap::new())),
            revocations: Arc::new(RwLock::new(HashSet::new())),
        }
    }
    
    /// Register an identity
    pub async fn register_identity(&self, identity: EntityIdentity) -> Result<(), AttestationError> {
        let mut identities = self.identities.write().await;
        
        if identities.contains_key(&identity.id) {
            return Err(AttestationError::AlreadyExists);
        }
        
        identities.insert(identity.id, identity);
        Ok(())
    }
    
    /// Get an identity
    pub async fn get_identity(&self, id: &[u8; 32]) -> Option<EntityIdentity> {
        let identities = self.identities.read().await;
        identities.get(id).cloned()
    }
    
    /// Add a capability attestation
    pub async fn add_capability_attestation(&self, attestation: CapabilityAttestation) -> Result<(), AttestationError> {
        // Verify attestation
        self.verify_capability_attestation(&attestation)?;
        
        let mut capabilities = self.capabilities.write().await;
        capabilities
            .entry(attestation.subject)
            .or_insert_with(Vec::new)
            .push(attestation);
        
        Ok(())
    }
    
    /// Get capability attestations for an entity
    pub async fn get_capability_attestations(&self, entity_id: &[u8; 32]) -> Vec<CapabilityAttestation> {
        let capabilities = self.capabilities.read().await;
        capabilities.get(entity_id).cloned().unwrap_or_default()
    }
    
    /// Add an intention attestation
    pub async fn add_intention_attestation(&self, attestation: IntentionAttestation) -> Result<(), AttestationError> {
        let mut intentions = self.intentions.write().await;
        intentions
            .entry(attestation.subject)
            .or_insert_with(Vec::new)
            .push(attestation);
        
        Ok(())
    }
    
    /// Add a compliance attestation
    pub async fn add_compliance_attestation(&self, attestation: ComplianceAttestation) -> Result<(), AttestationError> {
        let mut compliance = self.compliance.write().await;
        compliance
            .entry(attestation.subject)
            .or_insert_with(Vec::new)
            .push(attestation);
        
        Ok(())
    }
    
    /// Add a trust attestation
    pub async fn add_trust_attestation(&self, attestation: TrustAttestation) -> Result<(), AttestationError> {
        let mut trust = self.trust.write().await;
        trust.insert((attestation.trustor, attestation.trustee), attestation);
        Ok(())
    }
    
    /// Get trust between two entities
    pub async fn get_trust(&self, trustor: &[u8; 32], trustee: &[u8; 32]) -> Option<TrustAttestation> {
        let trust = self.trust.read().await;
        trust.get(&(*trustor, *trustee)).cloned()
    }
    
    /// Revoke an attestation
    pub async fn revoke(&self, attestation_id: [u8; 32]) -> Result<(), AttestationError> {
        let mut revocations = self.revocations.write().await;
        revocations.insert(attestation_id);
        Ok(())
    }
    
    /// Check if an attestation is revoked
    pub async fn is_revoked(&self, attestation_id: &[u8; 32]) -> bool {
        let revocations = self.revocations.read().await;
        revocations.contains(attestation_id)
    }
    
    /// Verify a capability attestation
    fn verify_capability_attestation(&self, attestation: &CapabilityAttestation) -> Result<(), AttestationError> {
        // Check validity period
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs();
        
        if now < attestation.valid_from {
            return Err(AttestationError::NotYetValid);
        }
        
        if now > attestation.valid_until {
            return Err(AttestationError::Expired);
        }
        
        // Check signatures (simplified)
        if attestation.signatures.is_empty() {
            return Err(AttestationError::InvalidSignature);
        }
        
        Ok(())
    }
    
    /// Get aggregate trust score for an entity
    pub async fn get_aggregate_trust(&self, entity_id: &[u8; 32]) -> f64 {
        let trust = self.trust.read().await;
        
        let scores: Vec<f64> = trust
            .iter()
            .filter(|((_, trustee), _)| trustee == entity_id)
            .map(|(_, attestation)| attestation.overall_trust)
            .collect();
        
        if scores.is_empty() {
            return 0.5; // Default neutral trust
        }
        
        scores.iter().sum::<f64>() / scores.len() as f64
    }
    
    /// Get compliance status for an entity
    pub async fn get_compliance_status(&self, entity_id: &[u8; 32]) -> ComplianceSummary {
        let compliance = self.compliance.read().await;
        
        let attestations = compliance.get(entity_id).cloned().unwrap_or_default();
        
        let mut fully_compliant = 0;
        let mut partially_compliant = 0;
        let mut non_compliant = 0;
        
        for attestation in &attestations {
            for claim in &attestation.compliance_claims {
                match claim.status {
                    ComplianceStatus::FullyCompliant => fully_compliant += 1,
                    ComplianceStatus::PartiallyCompliant => partially_compliant += 1,
                    ComplianceStatus::NonCompliant => non_compliant += 1,
                    _ => {}
                }
            }
        }
        
        ComplianceSummary {
            fully_compliant,
            partially_compliant,
            non_compliant,
            attestation_count: attestations.len(),
        }
    }
}

impl Default for AttestationRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Compliance summary
#[derive(Debug, Clone)]
pub struct ComplianceSummary {
    pub fully_compliant: usize,
    pub partially_compliant: usize,
    pub non_compliant: usize,
    pub attestation_count: usize,
}

/// Attestation errors
#[derive(Debug, Clone)]
pub enum AttestationError {
    AlreadyExists,
    NotFound,
    InvalidSignature,
    Expired,
    NotYetValid,
    Revoked,
    InsufficientEvidence,
    UnauthorizedAttestor,
}

impl std::fmt::Display for AttestationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AlreadyExists => write!(f, "Attestation already exists"),
            Self::NotFound => write!(f, "Attestation not found"),
            Self::InvalidSignature => write!(f, "Invalid signature"),
            Self::Expired => write!(f, "Attestation has expired"),
            Self::NotYetValid => write!(f, "Attestation is not yet valid"),
            Self::Revoked => write!(f, "Attestation has been revoked"),
            Self::InsufficientEvidence => write!(f, "Insufficient evidence provided"),
            Self::UnauthorizedAttestor => write!(f, "Attestor is not authorized"),
        }
    }
}

impl std::error::Error for AttestationError {}

// ============================================================================
// ATTESTATION VERIFIER
// ============================================================================

/// Attestation verifier
pub struct AttestationVerifier {
    /// Trusted attestors
    trusted_attestors: HashSet<PublicKey>,
    /// Minimum evidence requirements
    min_evidence: HashMap<String, usize>,
    /// Trust thresholds
    trust_thresholds: TrustThresholds,
}

/// Trust thresholds
#[derive(Debug, Clone)]
pub struct TrustThresholds {
    pub identity_verification: f64,
    pub capability_verification: f64,
    pub compliance_verification: f64,
    pub intention_verification: f64,
}

impl Default for TrustThresholds {
    fn default() -> Self {
        Self {
            identity_verification: 0.8,
            capability_verification: 0.7,
            compliance_verification: 0.9,
            intention_verification: 0.6,
        }
    }
}

impl AttestationVerifier {
    /// Create a new verifier
    pub fn new() -> Self {
        Self {
            trusted_attestors: HashSet::new(),
            min_evidence: HashMap::new(),
            trust_thresholds: TrustThresholds::default(),
        }
    }
    
    /// Add a trusted attestor
    pub fn add_trusted_attestor(&mut self, attestor: PublicKey) {
        self.trusted_attestors.insert(attestor);
    }
    
    /// Verify an identity
    pub fn verify_identity(&self, identity: &EntityIdentity) -> VerificationResult {
        let mut score = 0.0;
        let mut issues = Vec::new();
        
        // Check attestation chain
        if identity.attestation_chain.is_empty() {
            issues.push("No attestation chain".to_string());
        } else {
            for attestation in &identity.attestation_chain {
                if self.trusted_attestors.contains(&attestation.attestor) {
                    score += 0.3;
                }
            }
        }
        
        // Check key validity
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs();
        
        if let Some(expires) = identity.primary_key.expires_at {
            if now > expires {
                issues.push("Primary key expired".to_string());
            } else {
                score += 0.2;
            }
        } else {
            score += 0.2;
        }
        
        // Normalize score
        score = score.min(1.0);
        
        VerificationResult {
            verified: score >= self.trust_thresholds.identity_verification,
            score,
            issues,
        }
    }
    
    /// Verify capabilities
    pub fn verify_capabilities(&self, attestation: &CapabilityAttestation) -> VerificationResult {
        let mut score = 0.0;
        let mut issues = Vec::new();
        
        // Check attestors
        for attestor in &attestation.attestors {
            if self.trusted_attestors.contains(attestor) {
                score += 0.2;
            }
        }
        
        // Check evidence quality
        for capability in &attestation.capabilities {
            match capability.evidence.evidence_type {
                EvidenceType::ZeroKnowledge => score += 0.15,
                EvidenceType::Benchmark => score += 0.1,
                EvidenceType::PeerReview => score += 0.1,
                EvidenceType::SelfReport => score += 0.02,
                _ => score += 0.05,
            }
        }
        
        score = score.min(1.0);
        
        VerificationResult {
            verified: score >= self.trust_thresholds.capability_verification,
            score,
            issues,
        }
    }
}

impl Default for AttestationVerifier {
    fn default() -> Self {
        Self::new()
    }
}

/// Verification result
#[derive(Debug, Clone)]
pub struct VerificationResult {
    pub verified: bool,
    pub score: f64,
    pub issues: Vec<String>,
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_registry_creation() {
        let registry = AttestationRegistry::new();
        
        let identity = EntityIdentity {
            id: [0u8; 32],
            name: Some("Test Entity".to_string()),
            primary_key: PublicKey {
                key_type: KeyType::Ed25519,
                key_data: vec![0u8; 32],
                created_at: 0,
                expires_at: None,
            },
            secondary_keys: vec![],
            entity_class: EntityClass::GeneralAI {
                architecture: "transformer".to_string(),
                capability_level: 5,
            },
            created_at: 0,
            attestation_chain: vec![],
        };
        
        assert!(registry.register_identity(identity).await.is_ok());
    }
    
    #[tokio::test]
    async fn test_duplicate_identity() {
        let registry = AttestationRegistry::new();
        
        let identity = EntityIdentity {
            id: [1u8; 32],
            name: None,
            primary_key: PublicKey {
                key_type: KeyType::Ed25519,
                key_data: vec![0u8; 32],
                created_at: 0,
                expires_at: None,
            },
            secondary_keys: vec![],
            entity_class: EntityClass::Unknown,
            created_at: 0,
            attestation_chain: vec![],
        };
        
        assert!(registry.register_identity(identity.clone()).await.is_ok());
        assert!(matches!(
            registry.register_identity(identity).await,
            Err(AttestationError::AlreadyExists)
        ));
    }
    
    #[tokio::test]
    async fn test_trust_attestation() {
        let registry = AttestationRegistry::new();
        
        let trust = TrustAttestation {
            id: [2u8; 32],
            trustor: [3u8; 32],
            trustee: [4u8; 32],
            trust_dimensions: vec![
                TrustDimension {
                    dimension: TrustDimensionType::Reliability,
                    score: 0.9,
                    confidence: 0.8,
                    basis: TrustBasis::DirectExperience,
                },
            ],
            overall_trust: 0.9,
            evidence: vec![],
            timestamp: 0,
            signature: Signature {
                algorithm: KeyType::Ed25519,
                signature_data: vec![0u8; 64],
                signer: PublicKey {
                    key_type: KeyType::Ed25519,
                    key_data: vec![0u8; 32],
                    created_at: 0,
                    expires_at: None,
                },
                timestamp: 0,
            },
        };
        
        assert!(registry.add_trust_attestation(trust).await.is_ok());
        
        let retrieved = registry.get_trust(&[3u8; 32], &[4u8; 32]).await;
        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap().overall_trust, 0.9);
    }
    
    #[test]
    fn test_verifier() {
        let verifier = AttestationVerifier::new();
        
        let identity = EntityIdentity {
            id: [5u8; 32],
            name: Some("Test".to_string()),
            primary_key: PublicKey {
                key_type: KeyType::Ed25519,
                key_data: vec![0u8; 32],
                created_at: 0,
                expires_at: None,
            },
            secondary_keys: vec![],
            entity_class: EntityClass::Unknown,
            created_at: 0,
            attestation_chain: vec![],
        };
        
        let result = verifier.verify_identity(&identity);
        assert!(result.score >= 0.0 && result.score <= 1.0);
    }
}
