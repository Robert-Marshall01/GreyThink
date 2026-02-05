//! # Security & Attestation Integration Tests
//!
//! Validates security mechanisms:
//! 1. Node identity verification
//! 2. Certificate validation
//! 3. Hardware attestation
//! 4. Join authorization
//! 5. Audit logging
//!
//! ## Why This Test Matters
//!
//! Security is non-negotiable in distributed systems. These tests verify
//! that nodes can be authenticated, malicious actors rejected, and all
//! security events are properly logged.

use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

// ============================================================================
// Mock Types
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct NodeId(u64);

#[derive(Debug, Clone)]
struct KeyPair {
    public_key: Vec<u8>,
    private_key: Vec<u8>,
    algorithm: String,
}

impl KeyPair {
    fn generate(node_id: u64) -> Self {
        // Simulated key generation
        Self {
            public_key: format!("pub-{}", node_id).into_bytes(),
            private_key: format!("priv-{}", node_id).into_bytes(),
            algorithm: "Ed25519".to_string(),
        }
    }

    fn public_key_hash(&self) -> u64 {
        // Simple hash for testing
        self.public_key.iter().map(|&b| b as u64).sum()
    }
}

#[derive(Debug, Clone)]
struct Certificate {
    subject: NodeId,
    issuer: String,
    public_key: Vec<u8>,
    not_before: u64,
    not_after: u64,
    signature: Vec<u8>,
}

impl Certificate {
    fn new(subject: NodeId, issuer: &str, key: &KeyPair, validity_secs: u64) -> Self {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs();
        
        Self {
            subject,
            issuer: issuer.to_string(),
            public_key: key.public_key.clone(),
            not_before: now,
            not_after: now + validity_secs,
            signature: format!("sig-{}", subject.0).into_bytes(),
        }
    }

    fn is_expired(&self) -> bool {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs();
        now > self.not_after
    }

    fn is_not_yet_valid(&self) -> bool {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs();
        now < self.not_before
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum AttestationType {
    Hardware,
    Software,
    None,
}

#[derive(Debug, Clone)]
struct AttestationEvidence {
    node_id: NodeId,
    attestation_type: AttestationType,
    measurement: Vec<u8>,
    signature: Vec<u8>,
    timestamp: u64,
}

impl AttestationEvidence {
    fn new(node_id: NodeId, attestation_type: AttestationType) -> Self {
        Self {
            node_id,
            attestation_type,
            measurement: format!("measure-{}", node_id.0).into_bytes(),
            signature: format!("attest-sig-{}", node_id.0).into_bytes(),
            timestamp: SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_secs(),
        }
    }
}

#[derive(Debug, Clone)]
enum SecurityEvent {
    NodeJoinRequested { node_id: NodeId },
    NodeJoinApproved { node_id: NodeId },
    NodeJoinRejected { node_id: NodeId, reason: String },
    AttestationVerified { node_id: NodeId },
    AttestationFailed { node_id: NodeId, reason: String },
    CertificateExpired { node_id: NodeId },
    SuspiciousActivity { node_id: NodeId, details: String },
    NodeBlocked { node_id: NodeId },
}

struct CertificateAuthority {
    name: String,
    trusted: bool,
}

struct SecurityManager {
    trusted_cas: Mutex<HashMap<String, CertificateAuthority>>,
    node_certificates: Mutex<HashMap<NodeId, Certificate>>,
    attestation_results: Mutex<HashMap<NodeId, bool>>,
    expected_measurements: Mutex<HashMap<NodeId, Vec<u8>>>,
    blocked_nodes: Mutex<HashSet<NodeId>>,
    audit_log: Mutex<Vec<SecurityEvent>>,
}

impl SecurityManager {
    fn new() -> Self {
        Self {
            trusted_cas: Mutex::new(HashMap::new()),
            node_certificates: Mutex::new(HashMap::new()),
            attestation_results: Mutex::new(HashMap::new()),
            expected_measurements: Mutex::new(HashMap::new()),
            blocked_nodes: Mutex::new(HashSet::new()),
            audit_log: Mutex::new(Vec::new()),
        }
    }

    fn add_trusted_ca(&self, name: &str) {
        self.trusted_cas.lock().unwrap().insert(
            name.to_string(),
            CertificateAuthority {
                name: name.to_string(),
                trusted: true,
            },
        );
    }

    fn register_certificate(&self, cert: Certificate) {
        self.node_certificates.lock().unwrap().insert(cert.subject, cert);
    }

    fn set_expected_measurement(&self, node_id: NodeId, measurement: Vec<u8>) {
        self.expected_measurements.lock().unwrap().insert(node_id, measurement);
    }

    fn validate_certificate(&self, node_id: NodeId) -> Result<(), String> {
        let certs = self.node_certificates.lock().unwrap();
        let cert = certs.get(&node_id).ok_or("Certificate not found")?;
        
        if cert.is_expired() {
            self.log_event(SecurityEvent::CertificateExpired { node_id });
            return Err("Certificate expired".to_string());
        }
        
        if cert.is_not_yet_valid() {
            return Err("Certificate not yet valid".to_string());
        }
        
        // Check if issuer is trusted
        let cas = self.trusted_cas.lock().unwrap();
        if !cas.contains_key(&cert.issuer) {
            return Err("Untrusted issuer".to_string());
        }
        
        Ok(())
    }

    fn verify_attestation(&self, evidence: &AttestationEvidence) -> Result<(), String> {
        // Check if we have expected measurement
        let measurements = self.expected_measurements.lock().unwrap();
        
        if let Some(expected) = measurements.get(&evidence.node_id) {
            if evidence.measurement != *expected {
                self.attestation_results.lock().unwrap().insert(evidence.node_id, false);
                self.log_event(SecurityEvent::AttestationFailed {
                    node_id: evidence.node_id,
                    reason: "Measurement mismatch".to_string(),
                });
                return Err("Measurement mismatch".to_string());
            }
        }
        
        self.attestation_results.lock().unwrap().insert(evidence.node_id, true);
        self.log_event(SecurityEvent::AttestationVerified { node_id: evidence.node_id });
        Ok(())
    }

    fn authorize_join(&self, node_id: NodeId) -> Result<(), String> {
        self.log_event(SecurityEvent::NodeJoinRequested { node_id });
        
        // Check if blocked
        if self.blocked_nodes.lock().unwrap().contains(&node_id) {
            self.log_event(SecurityEvent::NodeJoinRejected {
                node_id,
                reason: "Node is blocked".to_string(),
            });
            return Err("Node is blocked".to_string());
        }
        
        // Validate certificate
        if let Err(e) = self.validate_certificate(node_id) {
            self.log_event(SecurityEvent::NodeJoinRejected {
                node_id,
                reason: e.clone(),
            });
            return Err(e);
        }
        
        // Check attestation
        let attestations = self.attestation_results.lock().unwrap();
        if let Some(&passed) = attestations.get(&node_id) {
            if !passed {
                self.log_event(SecurityEvent::NodeJoinRejected {
                    node_id,
                    reason: "Attestation failed".to_string(),
                });
                return Err("Attestation failed".to_string());
            }
        }
        
        self.log_event(SecurityEvent::NodeJoinApproved { node_id });
        Ok(())
    }

    fn block_node(&self, node_id: NodeId) {
        self.blocked_nodes.lock().unwrap().insert(node_id);
        self.log_event(SecurityEvent::NodeBlocked { node_id });
    }

    fn is_blocked(&self, node_id: NodeId) -> bool {
        self.blocked_nodes.lock().unwrap().contains(&node_id)
    }

    fn report_suspicious(&self, node_id: NodeId, details: &str) {
        self.log_event(SecurityEvent::SuspiciousActivity {
            node_id,
            details: details.to_string(),
        });
    }

    fn log_event(&self, event: SecurityEvent) {
        self.audit_log.lock().unwrap().push(event);
    }

    fn audit_log_count(&self) -> usize {
        self.audit_log.lock().unwrap().len()
    }

    fn get_audit_log(&self) -> Vec<SecurityEvent> {
        self.audit_log.lock().unwrap().clone()
    }
}

// ============================================================================
// Test Fixtures
// ============================================================================

struct SecurityTestContext {
    manager: Arc<SecurityManager>,
}

impl SecurityTestContext {
    fn new() -> Self {
        let manager = Arc::new(SecurityManager::new());
        manager.add_trusted_ca("test-ca");
        Self { manager }
    }

    fn register_node(&self, node_id: u64) -> (NodeId, KeyPair, Certificate) {
        let id = NodeId(node_id);
        let key = KeyPair::generate(node_id);
        let cert = Certificate::new(id, "test-ca", &key, 3600);
        
        self.manager.register_certificate(cert.clone());
        (id, key, cert)
    }

    fn register_node_with_measurement(&self, node_id: u64) -> (NodeId, KeyPair) {
        let (id, key, _) = self.register_node(node_id);
        let measurement = format!("measure-{}", node_id).into_bytes();
        self.manager.set_expected_measurement(id, measurement);
        (id, key)
    }
}

// ============================================================================
// Tests
// ============================================================================

#[test]
fn test_key_generation() {
    let key = KeyPair::generate(1);
    
    assert!(!key.public_key.is_empty());
    assert!(!key.private_key.is_empty());
    assert_eq!(key.algorithm, "Ed25519");
}

#[test]
fn test_certificate_creation() {
    let node_id = NodeId(1);
    let key = KeyPair::generate(1);
    let cert = Certificate::new(node_id, "test-ca", &key, 3600);
    
    assert_eq!(cert.subject, node_id);
    assert_eq!(cert.issuer, "test-ca");
    assert!(!cert.is_expired());
    assert!(!cert.is_not_yet_valid());
}

#[test]
fn test_certificate_validation() {
    let ctx = SecurityTestContext::new();
    let (node_id, _, _) = ctx.register_node(1);
    
    let result = ctx.manager.validate_certificate(node_id);
    assert!(result.is_ok());
}

#[test]
fn test_untrusted_issuer_rejection() {
    let ctx = SecurityTestContext::new();
    let node_id = NodeId(999);
    let key = KeyPair::generate(999);
    let cert = Certificate::new(node_id, "untrusted-ca", &key, 3600);
    ctx.manager.register_certificate(cert);
    
    let result = ctx.manager.validate_certificate(node_id);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Untrusted issuer"));
}

#[test]
fn test_attestation_verification() {
    let ctx = SecurityTestContext::new();
    let (node_id, _) = ctx.register_node_with_measurement(1);
    
    let evidence = AttestationEvidence::new(node_id, AttestationType::Hardware);
    let result = ctx.manager.verify_attestation(&evidence);
    
    assert!(result.is_ok());
}

#[test]
fn test_attestation_measurement_mismatch() {
    let ctx = SecurityTestContext::new();
    let (node_id, _) = ctx.register_node_with_measurement(1);
    
    // Change expected measurement
    ctx.manager.set_expected_measurement(node_id, b"different".to_vec());
    
    let evidence = AttestationEvidence::new(node_id, AttestationType::Hardware);
    let result = ctx.manager.verify_attestation(&evidence);
    
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Measurement mismatch"));
}

#[test]
fn test_node_join_authorization() {
    let ctx = SecurityTestContext::new();
    let (node_id, _) = ctx.register_node_with_measurement(1);
    
    // Verify attestation first
    let evidence = AttestationEvidence::new(node_id, AttestationType::Hardware);
    ctx.manager.verify_attestation(&evidence).unwrap();
    
    // Now join
    let result = ctx.manager.authorize_join(node_id);
    assert!(result.is_ok());
}

#[test]
fn test_blocked_node_rejection() {
    let ctx = SecurityTestContext::new();
    let (node_id, _, _) = ctx.register_node(1);
    
    ctx.manager.block_node(node_id);
    assert!(ctx.manager.is_blocked(node_id));
    
    let result = ctx.manager.authorize_join(node_id);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("blocked"));
}

#[test]
fn test_audit_logging() {
    let ctx = SecurityTestContext::new();
    let (node_id, _) = ctx.register_node_with_measurement(1);
    
    let evidence = AttestationEvidence::new(node_id, AttestationType::Hardware);
    ctx.manager.verify_attestation(&evidence).unwrap();
    ctx.manager.authorize_join(node_id).unwrap();
    
    let log = ctx.manager.get_audit_log();
    
    assert!(log.len() >= 2);
    // Check for expected events
    let has_attestation = log.iter().any(|e| matches!(e, SecurityEvent::AttestationVerified { .. }));
    let has_join = log.iter().any(|e| matches!(e, SecurityEvent::NodeJoinApproved { .. }));
    
    assert!(has_attestation, "Should have attestation event");
    assert!(has_join, "Should have join event");
}

#[test]
fn test_suspicious_activity_reporting() {
    let ctx = SecurityTestContext::new();
    let node_id = NodeId(1);
    
    ctx.manager.report_suspicious(node_id, "Too many failed attempts");
    
    let log = ctx.manager.get_audit_log();
    let suspicious = log.iter().find(|e| matches!(e, SecurityEvent::SuspiciousActivity { .. }));
    
    assert!(suspicious.is_some());
}

#[test]
fn test_multiple_node_registration() {
    let ctx = SecurityTestContext::new();
    
    for i in 1..=10 {
        let (node_id, _, _) = ctx.register_node(i);
        let result = ctx.manager.validate_certificate(node_id);
        assert!(result.is_ok(), "Node {} should have valid certificate", i);
    }
}

#[test]
fn test_attestation_types() {
    let node_id = NodeId(1);
    
    let hw = AttestationEvidence::new(node_id, AttestationType::Hardware);
    let sw = AttestationEvidence::new(node_id, AttestationType::Software);
    let none = AttestationEvidence::new(node_id, AttestationType::None);
    
    assert_eq!(hw.attestation_type, AttestationType::Hardware);
    assert_eq!(sw.attestation_type, AttestationType::Software);
    assert_eq!(none.attestation_type, AttestationType::None);
}

#[test]
fn test_certificate_without_registration() {
    let ctx = SecurityTestContext::new();
    
    let result = ctx.manager.validate_certificate(NodeId(999));
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("not found"));
}

#[test]
fn test_join_without_attestation() {
    let ctx = SecurityTestContext::new();
    let (node_id, _, _) = ctx.register_node(1);
    
    // Join without attestation (should still work - attestation optional)
    let result = ctx.manager.authorize_join(node_id);
    assert!(result.is_ok());
}

#[test]
fn test_join_with_failed_attestation() {
    let ctx = SecurityTestContext::new();
    let (node_id, _) = ctx.register_node_with_measurement(1);
    
    // Set different expected measurement to fail attestation
    ctx.manager.set_expected_measurement(node_id, b"wrong".to_vec());
    
    let evidence = AttestationEvidence::new(node_id, AttestationType::Hardware);
    let _ = ctx.manager.verify_attestation(&evidence); // This will fail
    
    // Join should fail
    let result = ctx.manager.authorize_join(node_id);
    assert!(result.is_err());
}

#[test]
fn test_security_performance() {
    let ctx = SecurityTestContext::new();
    
    let start = Instant::now();
    
    for i in 1..=100 {
        let (node_id, _) = ctx.register_node_with_measurement(i);
        let evidence = AttestationEvidence::new(node_id, AttestationType::Hardware);
        ctx.manager.verify_attestation(&evidence).unwrap();
        ctx.manager.authorize_join(node_id).unwrap();
    }
    
    let elapsed = start.elapsed();
    assert!(elapsed < Duration::from_secs(1), "Security ops too slow: {:?}", elapsed);
}

#[test]
fn test_public_key_hash() {
    let key1 = KeyPair::generate(1);
    let key2 = KeyPair::generate(2);
    
    // Different nodes should have different key hashes
    assert_ne!(key1.public_key_hash(), key2.public_key_hash());
    
    // Same node should have consistent hash
    let key1_again = KeyPair::generate(1);
    assert_eq!(key1.public_key_hash(), key1_again.public_key_hash());
}
