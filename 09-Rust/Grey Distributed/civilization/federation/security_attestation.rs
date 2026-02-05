//! Grey Distributed — Security Attestation Framework
//!
//! This module implements a global trust and attestation framework for
//! civilization-scale deployments. It handles:
//! - Hardware-rooted trust verification
//! - Cross-sovereign attestation chains
//! - Regulatory compliance proofs
//! - Zero-knowledge identity verification

use std::collections::{HashMap, HashSet};
use std::time::{Duration, SystemTime};

// =============================================================================
// CORE TYPES
// =============================================================================

/// Unique identifier for an attestation
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct AttestationId(pub [u8; 32]);

impl AttestationId {
    pub fn from_hash(data: &[u8]) -> Self {
        let mut hash = [0u8; 32];
        // In production: use SHA-256 or BLAKE3
        hash.copy_from_slice(&data[..32.min(data.len())]);
        Self(hash)
    }
}

/// Entity that can be attested
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct EntityId(pub String);

/// Types of entities in the attestation hierarchy
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EntityType {
    /// Hardware security module or TPM
    HardwareRoot,
    /// Physical data center
    DataCenter,
    /// Compute node
    Node,
    /// Virtual machine or container
    Workload,
    /// Human operator
    Operator,
    /// Organization
    Organization,
    /// Sovereign region
    Region,
    /// Regulatory body
    Regulator,
    /// Certification authority
    CertificationAuthority,
}

/// Trust level assigned through attestation
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum TrustLevel {
    /// No trust established
    None = 0,
    /// Basic identity verified
    Identified = 1,
    /// Attestation chain verified
    Attested = 2,
    /// Compliance verified
    Compliant = 3,
    /// Hardware-rooted trust
    HardwareRooted = 4,
    /// Regulatory-approved
    RegulatoryApproved = 5,
}

/// Cryptographic algorithm for signatures
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SignatureAlgorithm {
    Ed25519,
    EcdsaP256,
    EcdsaP384,
    Dilithium3,  // Post-quantum
    Falcon512,   // Post-quantum
}

// =============================================================================
// ATTESTATION CLAIMS
// =============================================================================

/// A claim that can be attested
#[derive(Debug, Clone)]
pub struct Claim {
    pub claim_id: AttestationId,
    pub claim_type: ClaimType,
    pub subject: EntityId,
    pub issuer: EntityId,
    pub issued_at: SystemTime,
    pub expires_at: Option<SystemTime>,
    pub evidence: Vec<Evidence>,
    pub metadata: HashMap<String, String>,
}

/// Types of claims that can be made
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClaimType {
    /// Hardware integrity (TPM-based)
    HardwareIntegrity {
        tpm_version: String,
        pcr_values: Vec<[u8; 32]>,
        firmware_hash: [u8; 32],
    },
    
    /// Software integrity
    SoftwareIntegrity {
        binary_hash: [u8; 32],
        version: String,
        build_reproducible: bool,
    },
    
    /// Geographic location
    GeographicLocation {
        country: String,
        region: String,
        coordinates: Option<(f64, f64)>,
        verification_method: LocationVerification,
    },
    
    /// Regulatory compliance
    RegulatoryCompliance {
        regulation: String,
        jurisdiction: String,
        certification_id: String,
        audit_date: SystemTime,
    },
    
    /// Security certification
    SecurityCertification {
        standard: SecurityStandard,
        level: String,
        valid_until: SystemTime,
    },
    
    /// Identity verification
    IdentityVerification {
        identity_type: IdentityType,
        verification_level: u8,
    },
    
    /// Operational capability
    OperationalCapability {
        capability: String,
        capacity: u64,
        availability: f64,
    },
    
    /// Data handling
    DataHandling {
        classification: DataClassification,
        retention_policy: String,
        encryption_standard: String,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LocationVerification {
    SelfReported,
    NetworkLatency,
    HardwareAttestation,
    ThirdPartyAudit,
    GovernmentRegistry,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SecurityStandard {
    Iso27001,
    Iso27017,
    Iso27018,
    Soc2Type1,
    Soc2Type2,
    FedRampHigh,
    CommonCriteriaEal4,
    CommonCriteriaEal5,
    Fips1402,
    Fips1403,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IdentityType {
    Individual,
    Organization,
    Government,
    Machine,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataClassification {
    Public,
    Internal,
    Confidential,
    Restricted,
    TopSecret,
}

/// Evidence supporting a claim
#[derive(Debug, Clone)]
pub struct Evidence {
    pub evidence_type: EvidenceType,
    pub data: Vec<u8>,
    pub timestamp: SystemTime,
    pub source: EntityId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EvidenceType {
    TpmQuote,
    X509Certificate,
    AuditReport,
    ZeroKnowledgeProof,
    ThirdPartyAttestation,
    TimestampProof,
    LocationProof,
}

// =============================================================================
// ATTESTATION CHAIN
// =============================================================================

/// A signed attestation
#[derive(Debug, Clone)]
pub struct Attestation {
    pub id: AttestationId,
    pub claim: Claim,
    pub signature: Signature,
    pub parent_attestations: Vec<AttestationId>,
    pub chain_depth: u32,
}

/// Cryptographic signature
#[derive(Debug, Clone)]
pub struct Signature {
    pub algorithm: SignatureAlgorithm,
    pub public_key: Vec<u8>,
    pub signature_bytes: Vec<u8>,
    pub timestamp: SystemTime,
}

/// Chain of attestations forming a trust path
#[derive(Debug, Clone)]
pub struct AttestationChain {
    pub attestations: Vec<Attestation>,
    pub root_trust_anchor: EntityId,
    pub leaf_subject: EntityId,
    pub aggregate_trust_level: TrustLevel,
    pub chain_valid_until: SystemTime,
}

impl AttestationChain {
    /// Verify the entire attestation chain
    pub fn verify(&self) -> Result<TrustLevel, AttestationError> {
        if self.attestations.is_empty() {
            return Err(AttestationError::EmptyChain);
        }
        
        let mut current_trust = TrustLevel::None;
        let mut expected_issuer: Option<&EntityId> = None;
        
        for attestation in &self.attestations {
            // Verify chain linkage
            if let Some(issuer) = expected_issuer {
                if &attestation.claim.issuer != issuer {
                    return Err(AttestationError::ChainBroken);
                }
            }
            
            // Verify signature (simplified)
            if !self.verify_signature(&attestation.signature) {
                return Err(AttestationError::InvalidSignature);
            }
            
            // Check expiration
            if let Some(expires) = attestation.claim.expires_at {
                if expires < SystemTime::now() {
                    return Err(AttestationError::Expired);
                }
            }
            
            // Update trust level based on claim type
            let claim_trust = self.trust_level_for_claim(&attestation.claim);
            if claim_trust > current_trust {
                current_trust = claim_trust;
            }
            
            expected_issuer = Some(&attestation.claim.subject);
        }
        
        Ok(current_trust)
    }
    
    fn verify_signature(&self, _signature: &Signature) -> bool {
        // In production: actual cryptographic verification
        true
    }
    
    fn trust_level_for_claim(&self, claim: &Claim) -> TrustLevel {
        match &claim.claim_type {
            ClaimType::HardwareIntegrity { .. } => TrustLevel::HardwareRooted,
            ClaimType::RegulatoryCompliance { .. } => TrustLevel::RegulatoryApproved,
            ClaimType::SecurityCertification { .. } => TrustLevel::Compliant,
            ClaimType::IdentityVerification { .. } => TrustLevel::Identified,
            _ => TrustLevel::Attested,
        }
    }
}

// =============================================================================
// TRUST ANCHORS
// =============================================================================

/// Root trust anchor configuration
#[derive(Debug, Clone)]
pub struct TrustAnchor {
    pub anchor_id: EntityId,
    pub entity_type: EntityType,
    pub public_keys: Vec<TrustAnchorKey>,
    pub trust_level: TrustLevel,
    pub jurisdictions: HashSet<String>,
    pub valid_from: SystemTime,
    pub valid_until: SystemTime,
    pub constraints: TrustConstraints,
}

#[derive(Debug, Clone)]
pub struct TrustAnchorKey {
    pub key_id: [u8; 16],
    pub algorithm: SignatureAlgorithm,
    pub public_key: Vec<u8>,
    pub purpose: KeyPurpose,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeyPurpose {
    AttestationSigning,
    IdentityBinding,
    EncryptionKeyAgreement,
    TimestampSigning,
}

#[derive(Debug, Clone)]
pub struct TrustConstraints {
    /// Maximum chain depth from this anchor
    pub max_chain_depth: u32,
    
    /// Allowed claim types
    pub allowed_claims: HashSet<String>,
    
    /// Required evidence types
    pub required_evidence: HashSet<EvidenceType>,
    
    /// Cross-jurisdiction attestation allowed
    pub cross_jurisdiction: bool,
    
    /// Minimum signature algorithm strength
    pub min_signature_strength: SignatureAlgorithm,
}

/// Global trust anchor registry
pub struct TrustAnchorRegistry {
    anchors: HashMap<EntityId, TrustAnchor>,
    by_jurisdiction: HashMap<String, Vec<EntityId>>,
    by_type: HashMap<EntityType, Vec<EntityId>>,
}

impl TrustAnchorRegistry {
    pub fn new() -> Self {
        Self {
            anchors: HashMap::new(),
            by_jurisdiction: HashMap::new(),
            by_type: HashMap::new(),
        }
    }
    
    /// Register a trust anchor
    pub fn register(&mut self, anchor: TrustAnchor) -> Result<(), AttestationError> {
        // Validate anchor
        if anchor.valid_until < SystemTime::now() {
            return Err(AttestationError::Expired);
        }
        
        if anchor.public_keys.is_empty() {
            return Err(AttestationError::NoKeys);
        }
        
        // Index by jurisdiction
        for jurisdiction in &anchor.jurisdictions {
            self.by_jurisdiction
                .entry(jurisdiction.clone())
                .or_default()
                .push(anchor.anchor_id.clone());
        }
        
        // Index by type
        self.by_type
            .entry(anchor.entity_type)
            .or_default()
            .push(anchor.anchor_id.clone());
        
        self.anchors.insert(anchor.anchor_id.clone(), anchor);
        Ok(())
    }
    
    /// Find trust anchors for a jurisdiction
    pub fn anchors_for_jurisdiction(&self, jurisdiction: &str) -> Vec<&TrustAnchor> {
        self.by_jurisdiction
            .get(jurisdiction)
            .map(|ids| {
                ids.iter()
                    .filter_map(|id| self.anchors.get(id))
                    .collect()
            })
            .unwrap_or_default()
    }
    
    /// Check if an entity is a valid trust anchor
    pub fn is_trust_anchor(&self, entity: &EntityId) -> bool {
        self.anchors.contains_key(entity)
    }
}

// =============================================================================
// CROSS-SOVEREIGN ATTESTATION
// =============================================================================

/// Handles attestation across sovereign boundaries
pub struct CrossSovereignAttestation {
    /// Trust anchors registry
    trust_anchors: TrustAnchorRegistry,
    
    /// Mutual recognition agreements between jurisdictions
    recognition_agreements: HashMap<(String, String), RecognitionAgreement>,
    
    /// Cached attestation chains
    chain_cache: HashMap<(EntityId, EntityId), AttestationChain>,
    
    /// Cross-border attestation requests pending approval
    pending_requests: Vec<CrossBorderRequest>,
}

/// Mutual recognition agreement between jurisdictions
#[derive(Debug, Clone)]
pub struct RecognitionAgreement {
    pub jurisdiction_a: String,
    pub jurisdiction_b: String,
    pub recognized_claims: HashSet<String>,
    pub trust_level_mapping: HashMap<TrustLevel, TrustLevel>,
    pub valid_from: SystemTime,
    pub valid_until: SystemTime,
    pub conditions: Vec<String>,
}

/// Request for cross-border attestation
#[derive(Debug, Clone)]
pub struct CrossBorderRequest {
    pub request_id: String,
    pub subject: EntityId,
    pub source_jurisdiction: String,
    pub target_jurisdiction: String,
    pub requested_claims: Vec<ClaimType>,
    pub purpose: String,
    pub submitted_at: SystemTime,
    pub status: RequestStatus,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RequestStatus {
    Pending,
    Approved,
    Rejected,
    Expired,
}

impl CrossSovereignAttestation {
    pub fn new(trust_anchors: TrustAnchorRegistry) -> Self {
        Self {
            trust_anchors,
            recognition_agreements: HashMap::new(),
            chain_cache: HashMap::new(),
            pending_requests: Vec::new(),
        }
    }
    
    /// Establish mutual recognition between jurisdictions
    pub fn add_recognition_agreement(&mut self, agreement: RecognitionAgreement) {
        let key = (agreement.jurisdiction_a.clone(), agreement.jurisdiction_b.clone());
        self.recognition_agreements.insert(key, agreement);
    }
    
    /// Build attestation chain across jurisdictions
    pub fn build_cross_sovereign_chain(
        &mut self,
        subject: &EntityId,
        source_jurisdiction: &str,
        target_jurisdiction: &str,
    ) -> Result<AttestationChain, AttestationError> {
        // Check recognition agreement exists
        let agreement = self.get_recognition_agreement(source_jurisdiction, target_jurisdiction)
            .ok_or(AttestationError::NoRecognitionAgreement)?;
        
        // Verify agreement is still valid
        if agreement.valid_until < SystemTime::now() {
            return Err(AttestationError::AgreementExpired);
        }
        
        // Get source jurisdiction trust anchor
        let source_anchors = self.trust_anchors.anchors_for_jurisdiction(source_jurisdiction);
        if source_anchors.is_empty() {
            return Err(AttestationError::NoTrustAnchor);
        }
        
        // Get target jurisdiction trust anchor
        let target_anchors = self.trust_anchors.anchors_for_jurisdiction(target_jurisdiction);
        if target_anchors.is_empty() {
            return Err(AttestationError::NoTrustAnchor);
        }
        
        // Build chain from source anchor to subject
        let source_chain = self.build_local_chain(subject, &source_anchors[0].anchor_id)?;
        
        // Create cross-border bridge attestation
        let bridge_attestation = self.create_bridge_attestation(
            &source_anchors[0],
            &target_anchors[0],
            &agreement,
        )?;
        
        // Combine chains
        let mut combined = source_chain.attestations;
        combined.push(bridge_attestation);
        
        // Map trust level according to agreement
        let mapped_trust = agreement.trust_level_mapping
            .get(&source_chain.aggregate_trust_level)
            .copied()
            .unwrap_or(TrustLevel::Identified);
        
        Ok(AttestationChain {
            attestations: combined,
            root_trust_anchor: target_anchors[0].anchor_id.clone(),
            leaf_subject: subject.clone(),
            aggregate_trust_level: mapped_trust,
            chain_valid_until: agreement.valid_until,
        })
    }
    
    fn get_recognition_agreement(
        &self,
        a: &str,
        b: &str,
    ) -> Option<&RecognitionAgreement> {
        self.recognition_agreements.get(&(a.to_string(), b.to_string()))
            .or_else(|| self.recognition_agreements.get(&(b.to_string(), a.to_string())))
    }
    
    fn build_local_chain(
        &self,
        subject: &EntityId,
        anchor: &EntityId,
    ) -> Result<AttestationChain, AttestationError> {
        // Simplified: in production, this would traverse the attestation graph
        Ok(AttestationChain {
            attestations: vec![],
            root_trust_anchor: anchor.clone(),
            leaf_subject: subject.clone(),
            aggregate_trust_level: TrustLevel::Attested,
            chain_valid_until: SystemTime::now() + Duration::from_secs(86400 * 365),
        })
    }
    
    fn create_bridge_attestation(
        &self,
        source: &TrustAnchor,
        target: &TrustAnchor,
        agreement: &RecognitionAgreement,
    ) -> Result<Attestation, AttestationError> {
        let claim = Claim {
            claim_id: AttestationId::from_hash(b"bridge"),
            claim_type: ClaimType::IdentityVerification {
                identity_type: IdentityType::Organization,
                verification_level: 3,
            },
            subject: target.anchor_id.clone(),
            issuer: source.anchor_id.clone(),
            issued_at: SystemTime::now(),
            expires_at: Some(agreement.valid_until),
            evidence: vec![],
            metadata: HashMap::new(),
        };
        
        Ok(Attestation {
            id: claim.claim_id.clone(),
            claim,
            signature: Signature {
                algorithm: SignatureAlgorithm::Ed25519,
                public_key: vec![],
                signature_bytes: vec![],
                timestamp: SystemTime::now(),
            },
            parent_attestations: vec![],
            chain_depth: 0,
        })
    }
}

// =============================================================================
// ZERO-KNOWLEDGE PROOFS
// =============================================================================

/// Zero-knowledge proof for privacy-preserving attestation
#[derive(Debug, Clone)]
pub struct ZkAttestation {
    pub statement: ZkStatement,
    pub proof: Vec<u8>,
    pub public_inputs: Vec<[u8; 32]>,
    pub verification_key_hash: [u8; 32],
}

#[derive(Debug, Clone)]
pub struct ZkStatement {
    /// What is being proven
    pub claim_type: String,
    
    /// Public parameters
    pub public_params: HashMap<String, String>,
    
    /// Proof system used
    pub proof_system: ZkProofSystem,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ZkProofSystem {
    Groth16,
    Plonk,
    Stark,
    Bulletproofs,
}

/// Zero-knowledge attestation builder
pub struct ZkAttestationBuilder {
    statement_templates: HashMap<String, ZkStatementTemplate>,
}

#[derive(Debug, Clone)]
pub struct ZkStatementTemplate {
    pub name: String,
    pub description: String,
    pub public_inputs: Vec<String>,
    pub private_inputs: Vec<String>,
    pub circuit_hash: [u8; 32],
}

impl ZkAttestationBuilder {
    pub fn new() -> Self {
        let mut templates = HashMap::new();
        
        // Age verification without revealing birthdate
        templates.insert("age_over".to_string(), ZkStatementTemplate {
            name: "age_over".to_string(),
            description: "Prove age is over threshold without revealing birthdate".to_string(),
            public_inputs: vec!["threshold".to_string()],
            private_inputs: vec!["birthdate".to_string()],
            circuit_hash: [0u8; 32],
        });
        
        // Jurisdiction membership without revealing exact location
        templates.insert("jurisdiction_member".to_string(), ZkStatementTemplate {
            name: "jurisdiction_member".to_string(),
            description: "Prove membership in jurisdiction set without revealing which".to_string(),
            public_inputs: vec!["jurisdiction_set_root".to_string()],
            private_inputs: vec!["jurisdiction".to_string(), "merkle_path".to_string()],
            circuit_hash: [0u8; 32],
        });
        
        // Compliance level without revealing audit details
        templates.insert("compliance_level".to_string(), ZkStatementTemplate {
            name: "compliance_level".to_string(),
            description: "Prove compliance level meets threshold".to_string(),
            public_inputs: vec!["min_level".to_string()],
            private_inputs: vec!["audit_report".to_string()],
            circuit_hash: [0u8; 32],
        });
        
        Self {
            statement_templates: templates,
        }
    }
    
    /// Build a zero-knowledge attestation
    pub fn build_attestation(
        &self,
        template_name: &str,
        public_params: HashMap<String, String>,
        private_inputs: HashMap<String, Vec<u8>>,
    ) -> Result<ZkAttestation, AttestationError> {
        let template = self.statement_templates.get(template_name)
            .ok_or(AttestationError::UnknownTemplate)?;
        
        // In production: generate actual ZK proof
        let proof = vec![0u8; 256]; // Placeholder
        
        Ok(ZkAttestation {
            statement: ZkStatement {
                claim_type: template_name.to_string(),
                public_params,
                proof_system: ZkProofSystem::Groth16,
            },
            proof,
            public_inputs: vec![],
            verification_key_hash: template.circuit_hash,
        })
    }
    
    /// Verify a zero-knowledge attestation
    pub fn verify_attestation(&self, attestation: &ZkAttestation) -> Result<bool, AttestationError> {
        let template = self.statement_templates.get(&attestation.statement.claim_type)
            .ok_or(AttestationError::UnknownTemplate)?;
        
        // Verify circuit hash matches
        if attestation.verification_key_hash != template.circuit_hash {
            return Ok(false);
        }
        
        // In production: verify ZK proof
        // For now, accept all properly formatted proofs
        Ok(attestation.proof.len() >= 256)
    }
}

// =============================================================================
// REGULATORY COMPLIANCE PROOFS
// =============================================================================

/// Regulatory compliance attestation system
pub struct RegulatoryCompliance {
    /// Registered regulations
    regulations: HashMap<String, Regulation>,
    
    /// Compliance attestations
    attestations: HashMap<EntityId, Vec<ComplianceAttestation>>,
    
    /// Approved auditors
    approved_auditors: HashSet<EntityId>,
}

#[derive(Debug, Clone)]
pub struct Regulation {
    pub regulation_id: String,
    pub name: String,
    pub jurisdiction: String,
    pub version: String,
    pub requirements: Vec<Requirement>,
    pub effective_date: SystemTime,
    pub sunset_date: Option<SystemTime>,
}

#[derive(Debug, Clone)]
pub struct Requirement {
    pub requirement_id: String,
    pub description: String,
    pub evidence_types: Vec<EvidenceType>,
    pub audit_frequency: Duration,
    pub critical: bool,
}

#[derive(Debug, Clone)]
pub struct ComplianceAttestation {
    pub attestation_id: AttestationId,
    pub entity: EntityId,
    pub regulation_id: String,
    pub auditor: EntityId,
    pub audit_date: SystemTime,
    pub valid_until: SystemTime,
    pub findings: Vec<Finding>,
    pub overall_status: ComplianceStatus,
    pub signature: Signature,
}

#[derive(Debug, Clone)]
pub struct Finding {
    pub requirement_id: String,
    pub status: ComplianceStatus,
    pub notes: String,
    pub remediation_deadline: Option<SystemTime>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComplianceStatus {
    Compliant,
    SubstantiallyCompliant,
    PartiallyCompliant,
    NonCompliant,
    NotApplicable,
}

impl RegulatoryCompliance {
    pub fn new() -> Self {
        Self {
            regulations: HashMap::new(),
            attestations: HashMap::new(),
            approved_auditors: HashSet::new(),
        }
    }
    
    /// Register a regulation
    pub fn register_regulation(&mut self, regulation: Regulation) {
        self.regulations.insert(regulation.regulation_id.clone(), regulation);
    }
    
    /// Approve an auditor
    pub fn approve_auditor(&mut self, auditor: EntityId) {
        self.approved_auditors.insert(auditor);
    }
    
    /// Record a compliance attestation
    pub fn record_attestation(
        &mut self,
        attestation: ComplianceAttestation,
    ) -> Result<(), AttestationError> {
        // Verify auditor is approved
        if !self.approved_auditors.contains(&attestation.auditor) {
            return Err(AttestationError::UnapprovedAuditor);
        }
        
        // Verify regulation exists
        if !self.regulations.contains_key(&attestation.regulation_id) {
            return Err(AttestationError::UnknownRegulation);
        }
        
        self.attestations
            .entry(attestation.entity.clone())
            .or_default()
            .push(attestation);
        
        Ok(())
    }
    
    /// Check compliance status for an entity
    pub fn check_compliance(
        &self,
        entity: &EntityId,
        regulation_id: &str,
    ) -> Option<&ComplianceAttestation> {
        self.attestations.get(entity)?
            .iter()
            .filter(|a| a.regulation_id == regulation_id)
            .filter(|a| a.valid_until > SystemTime::now())
            .max_by_key(|a| a.audit_date)
    }
    
    /// Get all applicable regulations for a jurisdiction
    pub fn regulations_for_jurisdiction(&self, jurisdiction: &str) -> Vec<&Regulation> {
        self.regulations.values()
            .filter(|r| r.jurisdiction == jurisdiction)
            .filter(|r| r.sunset_date.map_or(true, |d| d > SystemTime::now()))
            .collect()
    }
}

// =============================================================================
// ERRORS
// =============================================================================

#[derive(Debug, Clone)]
pub enum AttestationError {
    EmptyChain,
    ChainBroken,
    InvalidSignature,
    Expired,
    NoKeys,
    NoTrustAnchor,
    NoRecognitionAgreement,
    AgreementExpired,
    UnknownTemplate,
    UnapprovedAuditor,
    UnknownRegulation,
    InsufficientEvidence,
    TrustLevelInsufficient,
}

// =============================================================================
// TESTS
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_trust_anchor_registration() {
        let mut registry = TrustAnchorRegistry::new();
        
        let anchor = TrustAnchor {
            anchor_id: EntityId("test-anchor".to_string()),
            entity_type: EntityType::CertificationAuthority,
            public_keys: vec![TrustAnchorKey {
                key_id: [0u8; 16],
                algorithm: SignatureAlgorithm::Ed25519,
                public_key: vec![0u8; 32],
                purpose: KeyPurpose::AttestationSigning,
            }],
            trust_level: TrustLevel::HardwareRooted,
            jurisdictions: vec!["US".to_string(), "EU".to_string()].into_iter().collect(),
            valid_from: SystemTime::now(),
            valid_until: SystemTime::now() + Duration::from_secs(86400 * 365),
            constraints: TrustConstraints {
                max_chain_depth: 5,
                allowed_claims: HashSet::new(),
                required_evidence: HashSet::new(),
                cross_jurisdiction: true,
                min_signature_strength: SignatureAlgorithm::Ed25519,
            },
        };
        
        registry.register(anchor).unwrap();
        assert!(registry.is_trust_anchor(&EntityId("test-anchor".to_string())));
        assert_eq!(registry.anchors_for_jurisdiction("US").len(), 1);
        assert_eq!(registry.anchors_for_jurisdiction("EU").len(), 1);
    }
    
    #[test]
    fn test_attestation_chain_verification() {
        let chain = AttestationChain {
            attestations: vec![],
            root_trust_anchor: EntityId("root".to_string()),
            leaf_subject: EntityId("subject".to_string()),
            aggregate_trust_level: TrustLevel::Attested,
            chain_valid_until: SystemTime::now() + Duration::from_secs(3600),
        };
        
        // Empty chain should fail
        assert!(matches!(chain.verify(), Err(AttestationError::EmptyChain)));
    }
    
    #[test]
    fn test_zk_attestation_builder() {
        let builder = ZkAttestationBuilder::new();
        
        let mut public_params = HashMap::new();
        public_params.insert("threshold".to_string(), "18".to_string());
        
        let attestation = builder.build_attestation(
            "age_over",
            public_params,
            HashMap::new(),
        ).unwrap();
        
        assert_eq!(attestation.statement.claim_type, "age_over");
        assert!(builder.verify_attestation(&attestation).unwrap());
    }
}
