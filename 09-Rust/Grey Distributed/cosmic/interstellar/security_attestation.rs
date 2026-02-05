//! Grey Distributed — Interstellar Security Attestation
//!
//! Security attestation and trust verification for Grey Distributed nodes
//! operating across interstellar distances. Handles trust chains, cryptographic
//! verification, and identity management across light-years.

use std::collections::HashMap;
use std::time::Duration;

// =============================================================================
// INTERSTELLAR SECURITY CONTEXT
// =============================================================================
//
// Security across interstellar distances presents novel challenges:
//
// 1. DELAYED VERIFICATION
//    - Cannot verify claims in real-time
//    - Attestations may be years old when received
//    - Must trust cryptographic proofs, not live verification
//
// 2. KEY MANAGEMENT
//    - Key rotation takes years
//    - Revocation broadcasts may arrive too late
//    - Long-lived keys are essential but risky
//
// 3. IDENTITY CONTINUITY
//    - Civilizations evolve over decades
//    - Organizational succession must be verifiable
//    - Identity claims need deep verification
//
// 4. FORK SECURITY
//    - Forked civilizations may have valid credentials
//    - Multiple valid chains may exist
//    - Must handle without central authority
//
// =============================================================================

/// Cryptographic algorithm suite for interstellar use
/// 
/// Must be extremely long-lived and resistant to future attacks
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CryptoSuite {
    /// Post-quantum lattice-based (conservative)
    LatticeBased,
    /// Hash-based signatures (stateful but proven)
    HashBased,
    /// Multi-algorithm hybrid
    Hybrid,
    /// Legacy (for compatibility)
    Legacy,
}

impl CryptoSuite {
    /// Estimated security lifetime (years)
    pub fn security_lifetime_years(&self) -> u64 {
        match self {
            Self::LatticeBased => 100,
            Self::HashBased => 200,
            Self::Hybrid => 150,
            Self::Legacy => 20,
        }
    }
    
    /// Should transition away from this suite?
    pub fn should_transition(&self, age_years: u64) -> bool {
        age_years > self.security_lifetime_years() / 2
    }
}

/// Identity types in the interstellar network
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IdentityType {
    /// Star system (governmental/civilizational)
    StarSystem,
    /// Organization within a system
    Organization,
    /// Individual (rare at interstellar scale)
    Individual,
    /// Automated system/AI
    Automated,
    /// Federation-level entity
    Federation,
    /// Unknown/unverified
    Unknown,
}

/// An interstellar identity
#[derive(Debug, Clone)]
pub struct InterstellarIdentity {
    /// Unique identifier
    pub id: IdentityId,
    
    /// Identity type
    pub identity_type: IdentityType,
    
    /// Display name
    pub name: String,
    
    /// Home system
    pub home_system: SystemId,
    
    /// Public keys
    pub public_keys: Vec<PublicKey>,
    
    /// Trust chain to root
    pub trust_chain: TrustChain,
    
    /// Attestations about this identity
    pub attestations: Vec<Attestation>,
    
    /// Succession history
    pub succession: Vec<SuccessionEvent>,
    
    /// Creation epoch
    pub created_epoch: u64,
    
    /// Last verified epoch
    pub last_verified_epoch: u64,
}

pub type IdentityId = String;
pub type SystemId = String;

#[derive(Debug, Clone)]
pub struct PublicKey {
    /// Key identifier
    pub key_id: String,
    
    /// Cryptographic suite
    pub suite: CryptoSuite,
    
    /// Key material (encoded)
    pub key_material: Vec<u8>,
    
    /// Key purpose
    pub purpose: KeyPurpose,
    
    /// Creation epoch
    pub created_epoch: u64,
    
    /// Expiration epoch (if any)
    pub expires_epoch: Option<u64>,
    
    /// Revocation status
    pub revoked: bool,
    pub revoked_epoch: Option<u64>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeyPurpose {
    /// Signing attestations and messages
    Signing,
    /// Key exchange / encryption
    Encryption,
    /// Both purposes
    General,
    /// Specifically for attestations
    Attestation,
    /// For identity succession
    Succession,
}

#[derive(Debug, Clone)]
pub struct TrustChain {
    /// Chain of trust from identity to federation root
    pub chain: Vec<TrustLink>,
    
    /// Chain verification status
    pub verified: bool,
    
    /// Last verification
    pub last_verification_epoch: u64,
}

#[derive(Debug, Clone)]
pub struct TrustLink {
    /// The entity in the chain
    pub entity: IdentityId,
    
    /// Signature by this entity
    pub signature: Signature,
    
    /// Scope of trust delegation
    pub scope: TrustScope,
    
    /// Epoch when link was created
    pub created_epoch: u64,
}

#[derive(Debug, Clone)]
pub struct Signature {
    /// Key used for signing
    pub key_id: String,
    
    /// Signature material
    pub signature: Vec<u8>,
    
    /// Algorithm used
    pub algorithm: CryptoSuite,
}

#[derive(Debug, Clone)]
pub struct TrustScope {
    /// What actions are trusted
    pub actions: Vec<TrustAction>,
    
    /// Geographic scope
    pub geographic_scope: GeographicScope,
    
    /// Time scope
    pub valid_until_epoch: Option<u64>,
    
    /// Can re-delegate trust?
    pub can_delegate: bool,
    pub max_delegation_depth: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TrustAction {
    /// Sign on behalf of delegator
    Sign,
    /// Attest to facts
    Attest,
    /// Manage resources
    ManageResources,
    /// Participate in governance
    Governance,
    /// Emergency actions
    Emergency,
    /// All actions
    All,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GeographicScope {
    /// Single system
    System,
    /// Regional cluster
    Regional,
    /// Federation-wide
    Federation,
    /// Universal (including non-federation)
    Universal,
}

/// An attestation about some fact
#[derive(Debug, Clone)]
pub struct Attestation {
    /// Unique attestation ID
    pub id: AttestationId,
    
    /// Who made the attestation
    pub attester: IdentityId,
    
    /// Subject of the attestation
    pub subject: AttestationSubject,
    
    /// The claim being attested
    pub claim: Claim,
    
    /// Cryptographic signature
    pub signature: Signature,
    
    /// When attestation was made
    pub attested_epoch: u64,
    
    /// When attestation expires
    pub expires_epoch: Option<u64>,
    
    /// State hash at time of attestation
    pub state_context: StateContext,
}

pub type AttestationId = String;

#[derive(Debug, Clone)]
pub enum AttestationSubject {
    /// Attesting about an identity
    Identity(IdentityId),
    /// Attesting about a system
    System(SystemId),
    /// Attesting about an event
    Event(String),
    /// Attesting about a state
    State(String),
}

#[derive(Debug, Clone)]
pub struct Claim {
    /// Type of claim
    pub claim_type: ClaimType,
    
    /// Claim details
    pub details: HashMap<String, String>,
    
    /// Confidence level
    pub confidence: Confidence,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClaimType {
    /// Identity is valid
    IdentityValid,
    /// Identity is successor to another
    IdentitySuccession,
    /// System is federation member
    FederationMembership,
    /// Fact about the physical world
    PhysicalFact,
    /// Technical capability
    TechnicalCapability,
    /// Custom claim
    Custom,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Confidence {
    /// Absolute certainty (cryptographic proof)
    Certain,
    /// Very high confidence
    High,
    /// Moderate confidence
    Medium,
    /// Low confidence
    Low,
    /// Unverified
    Unknown,
}

#[derive(Debug, Clone)]
pub struct StateContext {
    /// Hash of attester's state
    pub state_hash: String,
    /// Epoch of the state
    pub epoch: u64,
    /// Block/commit reference
    pub reference: String,
}

#[derive(Debug, Clone)]
pub struct SuccessionEvent {
    /// Previous identity (if any)
    pub predecessor: Option<IdentityId>,
    
    /// New identity
    pub successor: IdentityId,
    
    /// Type of succession
    pub succession_type: SuccessionType,
    
    /// Attestations proving succession
    pub proof: Vec<AttestationId>,
    
    /// Epoch of succession
    pub epoch: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SuccessionType {
    /// Planned succession (retirement, reorganization)
    Planned,
    /// Emergency succession
    Emergency,
    /// Fork/split
    Fork,
    /// Merger
    Merger,
    /// Original creation
    Genesis,
}

// =============================================================================
// SECURITY ATTESTATION ENGINE
// =============================================================================

/// Engine for managing interstellar security attestations
pub struct SecurityAttestationEngine {
    /// Our identity
    own_identity: InterstellarIdentity,
    
    /// Known identities
    known_identities: HashMap<IdentityId, InterstellarIdentity>,
    
    /// Attestation store
    attestations: HashMap<AttestationId, Attestation>,
    
    /// Trust graph
    trust_graph: TrustGraph,
    
    /// Revocation list
    revocations: RevocationList,
    
    /// Configuration
    config: SecurityConfig,
}

#[derive(Debug, Clone)]
pub struct TrustGraph {
    /// Edges: from -> (to, trust_level)
    edges: HashMap<IdentityId, Vec<(IdentityId, TrustLevel)>>,
    
    /// Cached path calculations
    path_cache: HashMap<(IdentityId, IdentityId), TrustPath>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum TrustLevel {
    /// No trust
    None,
    /// Minimal trust
    Minimal,
    /// Limited trust
    Limited,
    /// Moderate trust
    Moderate,
    /// High trust
    High,
    /// Full trust
    Full,
}

#[derive(Debug, Clone)]
pub struct TrustPath {
    /// Path from source to destination
    pub path: Vec<IdentityId>,
    /// Minimum trust along path
    pub min_trust: TrustLevel,
    /// Path age (max age of any link)
    pub max_age_epochs: u64,
}

#[derive(Debug, Clone)]
pub struct RevocationList {
    /// Revoked keys
    pub keys: Vec<RevokedKey>,
    /// Revoked identities
    pub identities: Vec<RevokedIdentity>,
    /// Revoked attestations
    pub attestations: Vec<AttestationId>,
}

#[derive(Debug, Clone)]
pub struct RevokedKey {
    pub key_id: String,
    pub revoked_epoch: u64,
    pub reason: RevocationReason,
}

#[derive(Debug, Clone)]
pub struct RevokedIdentity {
    pub identity_id: IdentityId,
    pub revoked_epoch: u64,
    pub reason: RevocationReason,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RevocationReason {
    KeyCompromise,
    Succession,
    Expiration,
    Abuse,
    Unknown,
}

#[derive(Debug, Clone)]
pub struct SecurityConfig {
    /// Minimum trust level for various actions
    pub trust_requirements: HashMap<TrustAction, TrustLevel>,
    
    /// Maximum attestation age to accept (epochs)
    pub max_attestation_age_epochs: u64,
    
    /// Require multiple attesters?
    pub multi_attestation_required: bool,
    pub min_attesters: usize,
    
    /// Accept attestations from forked chains?
    pub accept_forked_attestations: bool,
}

impl SecurityAttestationEngine {
    /// Create new engine
    pub fn new(own_identity: InterstellarIdentity, config: SecurityConfig) -> Self {
        Self {
            own_identity,
            known_identities: HashMap::new(),
            attestations: HashMap::new(),
            trust_graph: TrustGraph {
                edges: HashMap::new(),
                path_cache: HashMap::new(),
            },
            revocations: RevocationList {
                keys: Vec::new(),
                identities: Vec::new(),
                attestations: Vec::new(),
            },
            config,
        }
    }
    
    /// Create an attestation
    pub fn create_attestation(
        &mut self,
        subject: AttestationSubject,
        claim: Claim,
        key_id: &str,
    ) -> Result<Attestation, SecurityError> {
        // Find signing key
        let key = self.own_identity.public_keys.iter()
            .find(|k| k.key_id == key_id && !k.revoked)
            .ok_or(SecurityError::KeyNotFound)?;
        
        // Verify key purpose
        if !matches!(key.purpose, KeyPurpose::Signing | KeyPurpose::Attestation | KeyPurpose::General) {
            return Err(SecurityError::InvalidKeyPurpose);
        }
        
        // Create attestation
        let attestation = Attestation {
            id: format!("att-{}-{}", self.own_identity.id, self.attestations.len()),
            attester: self.own_identity.id.clone(),
            subject,
            claim,
            signature: Signature {
                key_id: key_id.to_string(),
                signature: Vec::new(), // Would be actual signature
                algorithm: key.suite,
            },
            attested_epoch: self.current_epoch(),
            expires_epoch: None,
            state_context: self.current_state_context(),
        };
        
        // Store
        self.attestations.insert(attestation.id.clone(), attestation.clone());
        
        Ok(attestation)
    }
    
    /// Verify an attestation
    pub fn verify_attestation(&self, attestation: &Attestation) -> VerificationResult {
        // Check if attestation is revoked
        if self.revocations.attestations.contains(&attestation.id) {
            return VerificationResult::Revoked;
        }
        
        // Check age
        let age = self.current_epoch() - attestation.attested_epoch;
        if age > self.config.max_attestation_age_epochs {
            return VerificationResult::Expired;
        }
        
        // Find attester
        let attester = match self.known_identities.get(&attestation.attester) {
            Some(id) => id,
            None => return VerificationResult::UnknownAttester,
        };
        
        // Find signing key
        let key = attester.public_keys.iter()
            .find(|k| k.key_id == attestation.signature.key_id);
        
        let key = match key {
            Some(k) => k,
            None => return VerificationResult::KeyNotFound,
        };
        
        // Check key revocation
        if key.revoked {
            // Check if revocation happened after attestation
            if let Some(revoked_epoch) = key.revoked_epoch {
                if revoked_epoch < attestation.attested_epoch {
                    return VerificationResult::KeyRevokedBeforeAttestation;
                }
            }
        }
        
        // Verify trust path to attester
        let trust_path = self.find_trust_path(&self.own_identity.id, &attestation.attester);
        
        match trust_path {
            Some(path) if path.min_trust >= TrustLevel::Limited => {
                // Verify cryptographic signature
                if self.verify_signature(attestation, key) {
                    VerificationResult::Valid {
                        trust_level: path.min_trust,
                        path_length: path.path.len(),
                        attestation_age_epochs: age,
                    }
                } else {
                    VerificationResult::InvalidSignature
                }
            }
            Some(_) => VerificationResult::InsufficientTrust,
            None => VerificationResult::NoTrustPath,
        }
    }
    
    /// Verify an identity
    pub fn verify_identity(&self, identity: &InterstellarIdentity) -> IdentityVerification {
        // Check revocation
        if self.revocations.identities.iter().any(|r| r.identity_id == identity.id) {
            return IdentityVerification::Revoked;
        }
        
        // Verify trust chain
        let chain_valid = self.verify_trust_chain(&identity.trust_chain);
        
        if !chain_valid {
            return IdentityVerification::InvalidTrustChain;
        }
        
        // Count valid attestations about identity
        let attestations: Vec<_> = self.attestations.values()
            .filter(|a| matches!(&a.subject, AttestationSubject::Identity(id) if id == &identity.id))
            .filter(|a| matches!(self.verify_attestation(a), VerificationResult::Valid { .. }))
            .collect();
        
        if self.config.multi_attestation_required && attestations.len() < self.config.min_attesters {
            return IdentityVerification::InsufficientAttestations {
                have: attestations.len(),
                need: self.config.min_attesters,
            };
        }
        
        // Verify succession chain if applicable
        if !identity.succession.is_empty() {
            let succession_valid = self.verify_succession_chain(&identity.succession);
            if !succession_valid {
                return IdentityVerification::InvalidSuccession;
            }
        }
        
        IdentityVerification::Valid {
            attestation_count: attestations.len(),
            trust_chain_length: identity.trust_chain.chain.len(),
        }
    }
    
    /// Handle key revocation broadcast
    pub fn process_revocation(&mut self, revocation: KeyRevocationBroadcast) -> Result<(), SecurityError> {
        // Verify revocation is properly signed
        if !self.verify_revocation_signature(&revocation) {
            return Err(SecurityError::InvalidRevocationSignature);
        }
        
        // Add to revocation list
        self.revocations.keys.push(RevokedKey {
            key_id: revocation.key_id.clone(),
            revoked_epoch: revocation.epoch,
            reason: revocation.reason,
        });
        
        // Update any affected identity
        if let Some(identity) = self.known_identities.get_mut(&revocation.identity_id) {
            if let Some(key) = identity.public_keys.iter_mut()
                .find(|k| k.key_id == revocation.key_id) {
                key.revoked = true;
                key.revoked_epoch = Some(revocation.epoch);
            }
        }
        
        // Invalidate attestations made with this key
        // (Don't remove them, just mark for re-verification)
        
        Ok(())
    }
    
    /// Calculate trust level between two identities
    pub fn trust_level(&self, from: &IdentityId, to: &IdentityId) -> TrustLevel {
        self.find_trust_path(from, to)
            .map(|p| p.min_trust)
            .unwrap_or(TrustLevel::None)
    }
    
    // --- Helper methods ---
    
    fn current_epoch(&self) -> u64 {
        0 // Placeholder
    }
    
    fn current_state_context(&self) -> StateContext {
        StateContext {
            state_hash: String::new(),
            epoch: self.current_epoch(),
            reference: String::new(),
        }
    }
    
    fn verify_signature(&self, _attestation: &Attestation, _key: &PublicKey) -> bool {
        true // Placeholder - would do actual crypto verification
    }
    
    fn find_trust_path(&self, from: &IdentityId, to: &IdentityId) -> Option<TrustPath> {
        // Check cache
        if let Some(path) = self.trust_graph.path_cache.get(&(from.clone(), to.clone())) {
            return Some(path.clone());
        }
        
        // BFS to find path
        // (Simplified - real implementation would be more sophisticated)
        if from == to {
            return Some(TrustPath {
                path: vec![from.clone()],
                min_trust: TrustLevel::Full,
                max_age_epochs: 0,
            });
        }
        
        // Check direct trust
        if let Some(edges) = self.trust_graph.edges.get(from) {
            if let Some((_, level)) = edges.iter().find(|(id, _)| id == to) {
                return Some(TrustPath {
                    path: vec![from.clone(), to.clone()],
                    min_trust: *level,
                    max_age_epochs: 0,
                });
            }
        }
        
        None // Simplified - real BFS would continue
    }
    
    fn verify_trust_chain(&self, chain: &TrustChain) -> bool {
        // Verify each link in the chain
        for (i, link) in chain.chain.iter().enumerate() {
            // Verify signature
            if !self.verify_link_signature(link) {
                return false;
            }
            
            // Verify continuity
            if i > 0 {
                let prev = &chain.chain[i - 1];
                if link.entity != prev.entity {
                    // Gap in chain
                    return false;
                }
            }
        }
        
        true
    }
    
    fn verify_link_signature(&self, _link: &TrustLink) -> bool {
        true // Placeholder
    }
    
    fn verify_succession_chain(&self, succession: &[SuccessionEvent]) -> bool {
        for event in succession {
            // Verify proofs exist and are valid
            for proof_id in &event.proof {
                if let Some(attestation) = self.attestations.get(proof_id) {
                    if !matches!(self.verify_attestation(attestation), VerificationResult::Valid { .. }) {
                        return false;
                    }
                } else {
                    return false;
                }
            }
        }
        
        true
    }
    
    fn verify_revocation_signature(&self, _revocation: &KeyRevocationBroadcast) -> bool {
        true // Placeholder
    }
}

#[derive(Debug, Clone)]
pub struct KeyRevocationBroadcast {
    pub key_id: String,
    pub identity_id: IdentityId,
    pub epoch: u64,
    pub reason: RevocationReason,
    pub signature: Signature,
}

#[derive(Debug, Clone)]
pub enum VerificationResult {
    Valid {
        trust_level: TrustLevel,
        path_length: usize,
        attestation_age_epochs: u64,
    },
    Revoked,
    Expired,
    UnknownAttester,
    KeyNotFound,
    KeyRevokedBeforeAttestation,
    InvalidSignature,
    InsufficientTrust,
    NoTrustPath,
}

#[derive(Debug, Clone)]
pub enum IdentityVerification {
    Valid {
        attestation_count: usize,
        trust_chain_length: usize,
    },
    Revoked,
    InvalidTrustChain,
    InsufficientAttestations {
        have: usize,
        need: usize,
    },
    InvalidSuccession,
}

#[derive(Debug, Clone)]
pub enum SecurityError {
    KeyNotFound,
    InvalidKeyPurpose,
    InvalidRevocationSignature,
}

// =============================================================================
// HEALTH MONITORING
// =============================================================================

#[derive(Debug, Clone)]
pub struct SecurityHealth {
    pub known_identities: usize,
    pub valid_identities: usize,
    pub total_attestations: usize,
    pub valid_attestations: usize,
    pub revoked_keys: usize,
    pub trust_graph_connectivity: f64,
    pub health_score: f64,
}

impl SecurityAttestationEngine {
    pub fn health_status(&self) -> SecurityHealth {
        let valid_identities = self.known_identities.values()
            .filter(|id| matches!(self.verify_identity(id), IdentityVerification::Valid { .. }))
            .count();
        
        let valid_attestations = self.attestations.values()
            .filter(|att| matches!(self.verify_attestation(att), VerificationResult::Valid { .. }))
            .count();
        
        let connectivity = if self.known_identities.is_empty() {
            0.0
        } else {
            self.trust_graph.edges.len() as f64 / self.known_identities.len() as f64
        };
        
        SecurityHealth {
            known_identities: self.known_identities.len(),
            valid_identities,
            total_attestations: self.attestations.len(),
            valid_attestations,
            revoked_keys: self.revocations.keys.len(),
            trust_graph_connectivity: connectivity,
            health_score: self.calculate_health_score(valid_identities, valid_attestations),
        }
    }
    
    fn calculate_health_score(&self, valid_identities: usize, valid_attestations: usize) -> f64 {
        let identity_ratio = if self.known_identities.is_empty() {
            1.0
        } else {
            valid_identities as f64 / self.known_identities.len() as f64
        };
        
        let attestation_ratio = if self.attestations.is_empty() {
            1.0
        } else {
            valid_attestations as f64 / self.attestations.len() as f64
        };
        
        (identity_ratio + attestation_ratio) / 2.0
    }
}

// =============================================================================
// TESTS
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_crypto_suite_lifetime() {
        assert!(CryptoSuite::HashBased.security_lifetime_years() > 100);
        assert!(CryptoSuite::Legacy.should_transition(15));
    }
    
    #[test]
    fn test_trust_level_ordering() {
        assert!(TrustLevel::Full > TrustLevel::High);
        assert!(TrustLevel::High > TrustLevel::None);
    }
}
