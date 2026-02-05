//! # Grey Distributed — Attestation
//!
//! This module implements cryptographic attestation proofs for verifying
//! system state, resource usage, and cross-cluster claims.
//!
//! ## Attestation Model
//!
//! Attestations provide cryptographic proof of:
//! - **State integrity**: System state at a point in time
//! - **Resource usage**: Actual resource consumption
//! - **Token balances**: Account balances verification
//! - **Federation claims**: Cross-cluster resource sharing
//!
//! ## Proof Structure
//!
//! ```text
//! ┌──────────────────────────────────────────────────────────────────┐
//! │                     Attestation Proof                            │
//! ├──────────────────┬───────────────────────────────────────────────┤
//! │ claim_hash       │ Hash of the claim being attested              │
//! │ evidence_hash    │ Hash of supporting evidence                   │
//! │ merkle_proof     │ Merkle path to root                          │
//! │ witness_sigs     │ Multi-party signatures                       │
//! │ timestamp_proof  │ Verifiable timestamp                         │
//! └──────────────────┴───────────────────────────────────────────────┘
//! ```

use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use std::time::SystemTime;

// =============================================================================
// Types and Constants
// =============================================================================

pub type ClusterId = String;
pub type TenantId = String;
pub type NodeId = String;
pub type AttestationId = String;
pub type Hash = [u8; 32];

/// Minimum witnesses for valid attestation
const MIN_WITNESSES: usize = 3;

/// Witness quorum percentage
const QUORUM_PERCENTAGE: f64 = 0.67;

// =============================================================================
// Claim Types
// =============================================================================

/// A claim to be attested
#[derive(Debug, Clone)]
pub struct Claim {
    /// Unique claim ID
    pub claim_id: String,
    
    /// Claim type
    pub claim_type: ClaimType,
    
    /// Claimant (who made the claim)
    pub claimant: Claimant,
    
    /// Claim data
    pub data: ClaimData,
    
    /// Claim timestamp
    pub timestamp: SystemTime,
    
    /// Claim hash
    pub claim_hash: Hash,
}

impl Claim {
    pub fn compute_hash(&self) -> Hash {
        use std::hash::{Hash as StdHash, Hasher};
        use std::collections::hash_map::DefaultHasher;
        
        let mut hasher = DefaultHasher::new();
        self.claim_id.hash(&mut hasher);
        
        let hash_value = hasher.finish();
        let mut result = [0u8; 32];
        result[..8].copy_from_slice(&hash_value.to_le_bytes());
        result
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Claimant {
    Tenant(TenantId),
    Cluster(ClusterId),
    Node(NodeId),
    System,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ClaimType {
    /// Resource usage claim
    ResourceUsage,
    /// Token balance claim
    TokenBalance,
    /// State integrity claim
    StateIntegrity,
    /// Federation resource sharing claim
    FederationSharing,
    /// Reward eligibility claim
    RewardEligibility,
    /// Penalty appeal claim
    PenaltyAppeal,
}

/// Claim-specific data
#[derive(Debug, Clone)]
pub enum ClaimData {
    ResourceUsage(ResourceUsageClaim),
    TokenBalance(TokenBalanceClaim),
    StateIntegrity(StateIntegrityClaim),
    FederationSharing(FederationSharingClaim),
    RewardEligibility(RewardEligibilityClaim),
    PenaltyAppeal(PenaltyAppealClaim),
}

#[derive(Debug, Clone)]
pub struct ResourceUsageClaim {
    pub tenant_id: TenantId,
    pub resource_type: String,
    pub usage_amount: f64,
    pub usage_unit: String,
    pub period_start: SystemTime,
    pub period_end: SystemTime,
}

#[derive(Debug, Clone)]
pub struct TokenBalanceClaim {
    pub tenant_id: TenantId,
    pub claimed_balance: u64,
    pub as_of: SystemTime,
}

#[derive(Debug, Clone)]
pub struct StateIntegrityClaim {
    pub cluster_id: ClusterId,
    pub state_hash: Hash,
    pub as_of: SystemTime,
}

#[derive(Debug, Clone)]
pub struct FederationSharingClaim {
    pub provider_cluster: ClusterId,
    pub consumer_cluster: ClusterId,
    pub resources_shared: HashMap<String, f64>,
    pub period_start: SystemTime,
    pub period_end: SystemTime,
}

#[derive(Debug, Clone)]
pub struct RewardEligibilityClaim {
    pub tenant_id: TenantId,
    pub reward_type: String,
    pub claimed_amount: u64,
    pub justification: String,
}

#[derive(Debug, Clone)]
pub struct PenaltyAppealClaim {
    pub tenant_id: TenantId,
    pub penalty_id: String,
    pub appeal_reason: String,
    pub evidence_hashes: Vec<Hash>,
}

// =============================================================================
// Attestation Proof
// =============================================================================

/// A cryptographic attestation proof
#[derive(Debug, Clone)]
pub struct AttestationProof {
    /// Unique attestation ID
    pub attestation_id: AttestationId,
    
    /// The claim being attested
    pub claim: Claim,
    
    /// Evidence supporting the claim
    pub evidence: Evidence,
    
    /// Merkle proof linking to root
    pub merkle_proof: MerkleProof,
    
    /// Witness signatures
    pub witness_signatures: Vec<WitnessSignature>,
    
    /// Attestation result
    pub result: AttestationResult,
    
    /// Creation timestamp
    pub created_at: SystemTime,
    
    /// Validity period
    pub valid_until: SystemTime,
}

impl AttestationProof {
    /// Check if proof has quorum
    pub fn has_quorum(&self, total_witnesses: usize) -> bool {
        let required = (total_witnesses as f64 * QUORUM_PERCENTAGE).ceil() as usize;
        self.witness_signatures.len() >= required.max(MIN_WITNESSES)
    }
    
    /// Verify all signatures
    pub fn verify_signatures(&self) -> bool {
        self.witness_signatures.iter().all(|sig| sig.verify())
    }
    
    /// Check if proof is still valid
    pub fn is_valid(&self) -> bool {
        SystemTime::now() < self.valid_until 
            && self.verify_signatures() 
            && self.merkle_proof.verify()
    }
}

/// Supporting evidence
#[derive(Debug, Clone)]
pub struct Evidence {
    /// Evidence hash
    pub evidence_hash: Hash,
    
    /// Evidence type
    pub evidence_type: EvidenceType,
    
    /// Evidence data (serialized)
    pub data: Vec<u8>,
    
    /// Data source
    pub source: EvidenceSource,
    
    /// Collection timestamp
    pub collected_at: SystemTime,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EvidenceType {
    /// Metrics data
    Metrics,
    /// Log entries
    Logs,
    /// Ledger entries
    LedgerEntries,
    /// State snapshot
    StateSnapshot,
    /// External verification
    ExternalVerification,
}

#[derive(Debug, Clone)]
pub enum EvidenceSource {
    LocalNode(NodeId),
    RemoteCluster(ClusterId),
    Ledger,
    MonitoringSystem,
    ExternalOracle(String),
}

/// Merkle proof for inclusion
#[derive(Debug, Clone)]
pub struct MerkleProof {
    /// Leaf hash
    pub leaf_hash: Hash,
    
    /// Proof path
    pub path: Vec<MerkleNode>,
    
    /// Root hash
    pub root_hash: Hash,
}

impl MerkleProof {
    /// Verify the Merkle proof
    pub fn verify(&self) -> bool {
        let mut current = self.leaf_hash;
        
        for node in &self.path {
            current = if node.is_left {
                combine_hashes(&node.hash, &current)
            } else {
                combine_hashes(&current, &node.hash)
            };
        }
        
        current == self.root_hash
    }
}

#[derive(Debug, Clone)]
pub struct MerkleNode {
    pub hash: Hash,
    pub is_left: bool,
}

/// Witness signature
#[derive(Debug, Clone)]
pub struct WitnessSignature {
    /// Witness ID (node or cluster)
    pub witness_id: String,
    
    /// Signature data
    pub signature: Vec<u8>,
    
    /// Signing timestamp
    pub signed_at: SystemTime,
    
    /// Witness verdict
    pub verdict: WitnessVerdict,
}

impl WitnessSignature {
    /// Verify signature (placeholder - real impl would use crypto)
    pub fn verify(&self) -> bool {
        // In production, this would verify cryptographic signature
        !self.signature.is_empty()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum WitnessVerdict {
    Approve,
    Reject,
    Abstain,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AttestationResult {
    Verified,
    Rejected,
    Pending,
    Expired,
    InsufficientQuorum,
}

// =============================================================================
// Attestation Service
// =============================================================================

/// Service for creating and verifying attestations
pub struct AttestationService {
    /// Local cluster ID
    cluster_id: ClusterId,
    
    /// Pending claims
    pending_claims: Arc<RwLock<HashMap<String, Claim>>>,
    
    /// Active attestations
    attestations: Arc<RwLock<HashMap<AttestationId, AttestationProof>>>,
    
    /// Witness registry
    witnesses: Arc<RwLock<Vec<WitnessInfo>>>,
    
    /// Configuration
    config: AttestationConfig,
}

#[derive(Debug, Clone)]
pub struct WitnessInfo {
    pub witness_id: String,
    pub cluster_id: ClusterId,
    pub public_key: Vec<u8>,
    pub reputation: f64,
    pub active: bool,
}

#[derive(Debug, Clone)]
pub struct AttestationConfig {
    /// Minimum witnesses required
    pub min_witnesses: usize,
    
    /// Quorum percentage
    pub quorum_percentage: f64,
    
    /// Attestation validity period (seconds)
    pub validity_period_secs: u64,
    
    /// Challenge window (seconds)
    pub challenge_window_secs: u64,
    
    /// Enable cross-cluster attestation
    pub cross_cluster_enabled: bool,
}

impl Default for AttestationConfig {
    fn default() -> Self {
        Self {
            min_witnesses: MIN_WITNESSES,
            quorum_percentage: QUORUM_PERCENTAGE,
            validity_period_secs: 86400, // 24 hours
            challenge_window_secs: 3600, // 1 hour
            cross_cluster_enabled: true,
        }
    }
}

impl AttestationService {
    pub fn new(cluster_id: ClusterId, config: AttestationConfig) -> Self {
        Self {
            cluster_id,
            pending_claims: Arc::new(RwLock::new(HashMap::new())),
            attestations: Arc::new(RwLock::new(HashMap::new())),
            witnesses: Arc::new(RwLock::new(Vec::new())),
            config,
        }
    }
    
    // =========================================================================
    // Claim Submission
    // =========================================================================
    
    /// Submit a claim for attestation
    pub fn submit_claim(&self, claim_type: ClaimType, claimant: Claimant, data: ClaimData) -> Claim {
        let claim_id = generate_id("claim");
        
        let mut claim = Claim {
            claim_id: claim_id.clone(),
            claim_type,
            claimant,
            data,
            timestamp: SystemTime::now(),
            claim_hash: [0u8; 32],
        };
        
        claim.claim_hash = claim.compute_hash();
        
        self.pending_claims.write().unwrap().insert(claim_id, claim.clone());
        
        claim
    }
    
    /// Submit resource usage claim
    pub fn submit_resource_usage_claim(
        &self,
        tenant_id: TenantId,
        resource_type: String,
        usage_amount: f64,
        usage_unit: String,
        period_start: SystemTime,
        period_end: SystemTime,
    ) -> Claim {
        let data = ClaimData::ResourceUsage(ResourceUsageClaim {
            tenant_id: tenant_id.clone(),
            resource_type,
            usage_amount,
            usage_unit,
            period_start,
            period_end,
        });
        
        self.submit_claim(ClaimType::ResourceUsage, Claimant::Tenant(tenant_id), data)
    }
    
    /// Submit token balance claim
    pub fn submit_token_balance_claim(
        &self,
        tenant_id: TenantId,
        claimed_balance: u64,
    ) -> Claim {
        let data = ClaimData::TokenBalance(TokenBalanceClaim {
            tenant_id: tenant_id.clone(),
            claimed_balance,
            as_of: SystemTime::now(),
        });
        
        self.submit_claim(ClaimType::TokenBalance, Claimant::Tenant(tenant_id), data)
    }
    
    // =========================================================================
    // Attestation Process
    // =========================================================================
    
    /// Create attestation proof for a claim
    pub fn create_attestation(
        &self,
        claim_id: &str,
        evidence: Evidence,
        merkle_proof: MerkleProof,
    ) -> Result<AttestationProof, AttestationError> {
        let claim = self.pending_claims.read().unwrap()
            .get(claim_id)
            .cloned()
            .ok_or(AttestationError::ClaimNotFound(claim_id.to_string()))?;
        
        let attestation_id = generate_id("attest");
        
        let proof = AttestationProof {
            attestation_id: attestation_id.clone(),
            claim,
            evidence,
            merkle_proof,
            witness_signatures: Vec::new(),
            result: AttestationResult::Pending,
            created_at: SystemTime::now(),
            valid_until: SystemTime::now() + std::time::Duration::from_secs(self.config.validity_period_secs),
        };
        
        self.attestations.write().unwrap().insert(attestation_id, proof.clone());
        
        Ok(proof)
    }
    
    /// Add witness signature to attestation
    pub fn add_witness_signature(
        &self,
        attestation_id: &AttestationId,
        witness_id: String,
        signature: Vec<u8>,
        verdict: WitnessVerdict,
    ) -> Result<(), AttestationError> {
        let mut attestations = self.attestations.write().unwrap();
        let proof = attestations.get_mut(attestation_id)
            .ok_or(AttestationError::AttestationNotFound(attestation_id.clone()))?;
        
        // Check for duplicate
        if proof.witness_signatures.iter().any(|s| s.witness_id == witness_id) {
            return Err(AttestationError::DuplicateWitness(witness_id));
        }
        
        proof.witness_signatures.push(WitnessSignature {
            witness_id,
            signature,
            signed_at: SystemTime::now(),
            verdict,
        });
        
        // Check for quorum
        let total_witnesses = self.witnesses.read().unwrap().len();
        if proof.has_quorum(total_witnesses) {
            self.finalize_attestation(proof);
        }
        
        Ok(())
    }
    
    /// Finalize attestation based on witness verdicts
    fn finalize_attestation(&self, proof: &mut AttestationProof) {
        let approvals = proof.witness_signatures.iter()
            .filter(|s| s.verdict == WitnessVerdict::Approve)
            .count();
        let rejections = proof.witness_signatures.iter()
            .filter(|s| s.verdict == WitnessVerdict::Reject)
            .count();
        
        proof.result = if approvals > rejections {
            AttestationResult::Verified
        } else {
            AttestationResult::Rejected
        };
        
        // Remove from pending
        self.pending_claims.write().unwrap().remove(&proof.claim.claim_id);
    }
    
    // =========================================================================
    // Verification
    // =========================================================================
    
    /// Verify an attestation proof
    pub fn verify_attestation(&self, proof: &AttestationProof) -> VerificationResult {
        let mut issues = Vec::new();
        
        // Check validity period
        if SystemTime::now() >= proof.valid_until {
            issues.push("Attestation has expired".to_string());
        }
        
        // Check signatures
        if !proof.verify_signatures() {
            issues.push("One or more signatures are invalid".to_string());
        }
        
        // Check Merkle proof
        if !proof.merkle_proof.verify() {
            issues.push("Merkle proof verification failed".to_string());
        }
        
        // Check quorum
        let total_witnesses = self.witnesses.read().unwrap().len();
        if !proof.has_quorum(total_witnesses) {
            issues.push("Insufficient witness quorum".to_string());
        }
        
        if issues.is_empty() {
            VerificationResult::Valid
        } else {
            VerificationResult::Invalid { issues }
        }
    }
    
    /// Get attestation by ID
    pub fn get_attestation(&self, attestation_id: &AttestationId) -> Option<AttestationProof> {
        self.attestations.read().unwrap().get(attestation_id).cloned()
    }
    
    /// Get attestations for a claim type
    pub fn get_attestations_by_type(&self, claim_type: ClaimType) -> Vec<AttestationProof> {
        self.attestations.read().unwrap()
            .values()
            .filter(|a| a.claim.claim_type == claim_type)
            .cloned()
            .collect()
    }
    
    // =========================================================================
    // Witness Management
    // =========================================================================
    
    /// Register a witness
    pub fn register_witness(&self, witness_info: WitnessInfo) {
        self.witnesses.write().unwrap().push(witness_info);
    }
    
    /// Get active witnesses
    pub fn get_active_witnesses(&self) -> Vec<WitnessInfo> {
        self.witnesses.read().unwrap()
            .iter()
            .filter(|w| w.active)
            .cloned()
            .collect()
    }
    
    /// Select witnesses for attestation
    pub fn select_witnesses(&self, count: usize) -> Vec<WitnessInfo> {
        let witnesses = self.witnesses.read().unwrap();
        let active: Vec<_> = witnesses.iter().filter(|w| w.active).cloned().collect();
        
        // Select by reputation (simple selection - production would use VRF)
        let mut selected: Vec<_> = active.clone();
        selected.sort_by(|a, b| b.reputation.partial_cmp(&a.reputation).unwrap());
        selected.truncate(count);
        selected
    }
}

// =============================================================================
// Result Types
// =============================================================================

#[derive(Debug, Clone)]
pub enum VerificationResult {
    Valid,
    Invalid { issues: Vec<String> },
}

// =============================================================================
// Errors
// =============================================================================

#[derive(Debug, Clone, PartialEq)]
pub enum AttestationError {
    ClaimNotFound(String),
    AttestationNotFound(String),
    DuplicateWitness(String),
    InsufficientWitnesses,
    InvalidSignature,
    ExpiredAttestation,
}

impl std::fmt::Display for AttestationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AttestationError::ClaimNotFound(id) => write!(f, "Claim not found: {}", id),
            AttestationError::AttestationNotFound(id) => write!(f, "Attestation not found: {}", id),
            AttestationError::DuplicateWitness(id) => write!(f, "Witness already signed: {}", id),
            AttestationError::InsufficientWitnesses => write!(f, "Insufficient witnesses"),
            AttestationError::InvalidSignature => write!(f, "Invalid signature"),
            AttestationError::ExpiredAttestation => write!(f, "Attestation has expired"),
        }
    }
}

impl std::error::Error for AttestationError {}

// =============================================================================
// Utilities
// =============================================================================

fn generate_id(prefix: &str) -> String {
    use std::time::{SystemTime, UNIX_EPOCH};
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    format!("{}-{:032x}", prefix, nanos)
}

fn combine_hashes(a: &Hash, b: &Hash) -> Hash {
    use std::hash::{Hash as StdHash, Hasher};
    use std::collections::hash_map::DefaultHasher;
    
    let mut hasher = DefaultHasher::new();
    a.hash(&mut hasher);
    b.hash(&mut hasher);
    
    let hash_value = hasher.finish();
    let mut result = [0u8; 32];
    result[..8].copy_from_slice(&hash_value.to_le_bytes());
    result
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_submit_claim() {
        let service = AttestationService::new("cluster-1".into(), AttestationConfig::default());
        
        let claim = service.submit_resource_usage_claim(
            "tenant-1".into(),
            "cpu".into(),
            100.0,
            "core-hours".into(),
            SystemTime::now() - std::time::Duration::from_secs(3600),
            SystemTime::now(),
        );
        
        assert!(!claim.claim_id.is_empty());
    }
    
    #[test]
    fn test_merkle_proof_verification() {
        let leaf_hash = [1u8; 32];
        let sibling = [2u8; 32];
        
        let root = combine_hashes(&leaf_hash, &sibling);
        
        let proof = MerkleProof {
            leaf_hash,
            path: vec![MerkleNode { hash: sibling, is_left: false }],
            root_hash: root,
        };
        
        assert!(proof.verify());
    }
    
    #[test]
    fn test_witness_quorum() {
        let service = AttestationService::new("cluster-1".into(), AttestationConfig::default());
        
        // Register witnesses
        for i in 0..5 {
            service.register_witness(WitnessInfo {
                witness_id: format!("witness-{}", i),
                cluster_id: format!("cluster-{}", i),
                public_key: vec![0u8; 32],
                reputation: 1.0,
                active: true,
            });
        }
        
        let witnesses = service.get_active_witnesses();
        assert_eq!(witnesses.len(), 5);
        
        let selected = service.select_witnesses(3);
        assert_eq!(selected.len(), 3);
    }
}
