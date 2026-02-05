// Package security implements node identity, attestation, and tenant isolation.
//
// # Design Philosophy
//
// Distributed systems face unique security challenges:
// - No central trusted authority (nodes must verify each other)
// - Byzantine failures (some nodes may be malicious)
// - Multi-tenancy (data from different tenants must not leak)
// - Audit requirements (all actions must be traceable)
//
// This package provides:
// 1. Node Identity: Ed25519 key pairs with optional TPM attestation
// 2. Signed State Transitions: Cryptographic proof of state changes
// 3. Tenant Isolation: Secure boundaries between tenant data
// 4. Anomaly Detection: Integration hooks for GreyAV
//
// # Cryptographic Choices
//
// Ed25519 for digital signatures:
// - Fast (signing/verification in microseconds)
// - Small signatures (64 bytes)
// - Well-audited implementations
// - Deterministic (no need for secure random at signing time)
//
// X25519 for key exchange (Diffie-Hellman):
// - Derived from Ed25519 keys
// - Perfect forward secrecy when used with ephemeral keys
//
// AES-256-GCM for symmetric encryption:
// - Authenticated encryption (confidentiality + integrity)
// - Hardware acceleration on modern CPUs
// - 12-byte nonce, 16-byte auth tag
//
// # Trust Model
//
// The system uses a hierarchical trust model:
// 1. Root CA (offline, HSM-protected)
// 2. Cluster Authority (per-cluster, issues node certificates)
// 3. Node Keys (generated on node, certified by Cluster Authority)
//
// Nodes verify each other via certificate chains, not a central service.
package security

import (
	"bytes"
	"crypto/aes"
	"crypto/cipher"
	"crypto/ed25519"
	"crypto/rand"
	"crypto/sha256"
	"encoding/binary"
	"encoding/hex"
	"errors"
	"sync"
	"time"

	"github.com/grey-systems/grey-distributed/pkg/types"
)

// =============================================================================
// NODE IDENTITY
// =============================================================================

// NodeIdentity represents a node's cryptographic identity.
type NodeIdentity struct {
	// NodeID is the logical node identifier
	NodeID types.NodeID

	// PublicKey for signature verification
	PublicKey ed25519.PublicKey

	// PrivateKey for signing (nil for remote nodes)
	PrivateKey ed25519.PrivateKey

	// Certificate issued by Cluster Authority
	Certificate *NodeCertificate

	// TPM attestation (optional)
	Attestation *TPMAttestation
}

// NodeCertificate certifies a node's identity.
type NodeCertificate struct {
	// Subject identifies the node
	NodeID types.NodeID

	// Subject's public key
	PublicKey ed25519.PublicKey

	// Issuer is the Cluster Authority
	Issuer string

	// Validity period
	NotBefore time.Time
	NotAfter  time.Time

	// Capabilities granted to this node
	Capabilities []NodeCapability

	// Cluster ID
	ClusterID string

	// Signature by Cluster Authority
	Signature []byte
}

// NodeCapability defines what a node is allowed to do.
type NodeCapability string

const (
	CapabilityParticipateConsensus NodeCapability = "consensus:participate"
	CapabilityStoreData            NodeCapability = "storage:store"
	CapabilityExecuteTasks         NodeCapability = "compute:execute"
	CapabilityAdminCluster         NodeCapability = "admin:cluster"
)

// TPMAttestation provides hardware-backed identity proof.
type TPMAttestation struct {
	// EK (Endorsement Key) certificate
	EKCert []byte

	// AIK (Attestation Identity Key) public key
	AIKPublic []byte

	// PCR (Platform Configuration Register) values
	PCRs map[int][]byte

	// Quote signed by AIK
	Quote []byte
}

// GenerateNodeIdentity creates a new node identity.
func GenerateNodeIdentity(nodeID types.NodeID) (*NodeIdentity, error) {
	pub, priv, err := ed25519.GenerateKey(rand.Reader)
	if err != nil {
		return nil, err
	}

	return &NodeIdentity{
		NodeID:     nodeID,
		PublicKey:  pub,
		PrivateKey: priv,
	}, nil
}

// Sign creates a signature over data.
func (n *NodeIdentity) Sign(data []byte) ([]byte, error) {
	if n.PrivateKey == nil {
		return nil, ErrNoPrivateKey
	}
	return ed25519.Sign(n.PrivateKey, data), nil
}

// Verify checks a signature.
func (n *NodeIdentity) Verify(data, signature []byte) bool {
	return ed25519.Verify(n.PublicKey, data, signature)
}

// PublicKeyHex returns the public key as hex string.
func (n *NodeIdentity) PublicKeyHex() string {
	return hex.EncodeToString(n.PublicKey)
}

// =============================================================================
// CERTIFICATE AUTHORITY
// =============================================================================

// ClusterAuthority issues and verifies node certificates.
type ClusterAuthority struct {
	mu sync.RWMutex

	// Authority identity
	identity  *NodeIdentity
	clusterID string

	// Issued certificates
	issuedCerts map[types.NodeID]*NodeCertificate

	// Revocation list
	revokedCerts map[types.NodeID]time.Time

	// Certificate validity duration
	certDuration time.Duration
}

// NewClusterAuthority creates a new cluster authority.
func NewClusterAuthority(clusterID string, certDuration time.Duration) (*ClusterAuthority, error) {
	// Generate CA key pair
	identity, err := GenerateNodeIdentity(types.NodeID{})
	if err != nil {
		return nil, err
	}

	return &ClusterAuthority{
		identity:     identity,
		clusterID:    clusterID,
		issuedCerts:  make(map[types.NodeID]*NodeCertificate),
		revokedCerts: make(map[types.NodeID]time.Time),
		certDuration: certDuration,
	}, nil
}

// IssueCertificate issues a certificate for a node.
func (ca *ClusterAuthority) IssueCertificate(
	nodeID types.NodeID,
	publicKey ed25519.PublicKey,
	capabilities []NodeCapability,
) (*NodeCertificate, error) {
	ca.mu.Lock()
	defer ca.mu.Unlock()

	now := time.Now()

	cert := &NodeCertificate{
		NodeID:       nodeID,
		PublicKey:    publicKey,
		Issuer:       ca.clusterID,
		NotBefore:    now,
		NotAfter:     now.Add(ca.certDuration),
		Capabilities: capabilities,
		ClusterID:    ca.clusterID,
	}

	// Sign the certificate
	signableData := cert.signableBytes()
	signature, err := ca.identity.Sign(signableData)
	if err != nil {
		return nil, err
	}
	cert.Signature = signature

	ca.issuedCerts[nodeID] = cert

	return cert, nil
}

// VerifyCertificate checks if a certificate is valid.
func (ca *ClusterAuthority) VerifyCertificate(cert *NodeCertificate) error {
	ca.mu.RLock()
	defer ca.mu.RUnlock()

	// Check revocation
	if _, revoked := ca.revokedCerts[cert.NodeID]; revoked {
		return ErrCertificateRevoked
	}

	// Check validity period
	now := time.Now()
	if now.Before(cert.NotBefore) || now.After(cert.NotAfter) {
		return ErrCertificateExpired
	}

	// Check cluster
	if cert.ClusterID != ca.clusterID {
		return ErrWrongCluster
	}

	// Verify signature
	signableData := cert.signableBytes()
	if !ca.identity.Verify(signableData, cert.Signature) {
		return ErrInvalidSignature
	}

	return nil
}

// RevokeCertificate revokes a node's certificate.
func (ca *ClusterAuthority) RevokeCertificate(nodeID types.NodeID) {
	ca.mu.Lock()
	defer ca.mu.Unlock()

	ca.revokedCerts[nodeID] = time.Now()
	delete(ca.issuedCerts, nodeID)
}

// GetPublicKey returns the CA's public key.
func (ca *ClusterAuthority) GetPublicKey() ed25519.PublicKey {
	return ca.identity.PublicKey
}

// signableBytes returns the bytes to sign for a certificate.
func (cert *NodeCertificate) signableBytes() []byte {
	buf := make([]byte, 0, 256)

	buf = append(buf, cert.NodeID[:]...)
	buf = append(buf, cert.PublicKey...)
	buf = append(buf, []byte(cert.Issuer)...)
	buf = binary.BigEndian.AppendUint64(buf, uint64(cert.NotBefore.Unix()))
	buf = binary.BigEndian.AppendUint64(buf, uint64(cert.NotAfter.Unix()))
	for _, cap := range cert.Capabilities {
		buf = append(buf, []byte(cap)...)
	}
	buf = append(buf, []byte(cert.ClusterID)...)

	return buf
}

// =============================================================================
// SIGNED STATE TRANSITIONS
// =============================================================================

// SignedStateTransition is a cryptographically signed state change.
type SignedStateTransition struct {
	// Transition details
	TransitionID   [16]byte
	FromState      []byte
	ToState        []byte
	StateHash      [32]byte
	TransitionType string

	// Proof
	NodeID      types.NodeID
	Term        uint64
	Timestamp   time.Time
	NodeSig     []byte
	ClusterSigs []NodeSignature
}

// NodeSignature is a signature from a specific node.
type NodeSignature struct {
	NodeID    types.NodeID
	Signature []byte
}

// StateTransitionVerifier verifies signed state transitions.
type StateTransitionVerifier struct {
	mu       sync.RWMutex
	nodes    map[types.NodeID]ed25519.PublicKey
	quorum   int
	ca       *ClusterAuthority
}

// NewStateTransitionVerifier creates a verifier.
func NewStateTransitionVerifier(quorum int, ca *ClusterAuthority) *StateTransitionVerifier {
	return &StateTransitionVerifier{
		nodes:  make(map[types.NodeID]ed25519.PublicKey),
		quorum: quorum,
		ca:     ca,
	}
}

// RegisterNode adds a node's public key.
func (v *StateTransitionVerifier) RegisterNode(nodeID types.NodeID, publicKey ed25519.PublicKey) {
	v.mu.Lock()
	defer v.mu.Unlock()
	v.nodes[nodeID] = publicKey
}

// VerifyTransition verifies a signed state transition.
func (v *StateTransitionVerifier) VerifyTransition(tx *SignedStateTransition) error {
	v.mu.RLock()
	defer v.mu.RUnlock()

	// Verify state hash
	expected := sha256.Sum256(tx.ToState)
	if !bytes.Equal(expected[:], tx.StateHash[:]) {
		return ErrStateHashMismatch
	}

	// Verify node signature
	signable := tx.signableBytes()
	if pub, ok := v.nodes[tx.NodeID]; ok {
		if !ed25519.Verify(pub, signable, tx.NodeSig) {
			return ErrInvalidSignature
		}
	} else {
		return ErrUnknownNode
	}

	// Verify quorum of cluster signatures
	validSigs := 0
	for _, sig := range tx.ClusterSigs {
		if pub, ok := v.nodes[sig.NodeID]; ok {
			if ed25519.Verify(pub, signable, sig.Signature) {
				validSigs++
			}
		}
	}

	if validSigs < v.quorum {
		return ErrInsufficientQuorum
	}

	return nil
}

// signableBytes returns bytes to sign for a transition.
func (tx *SignedStateTransition) signableBytes() []byte {
	buf := make([]byte, 0, 256)

	buf = append(buf, tx.TransitionID[:]...)
	buf = append(buf, tx.StateHash[:]...)
	buf = append(buf, []byte(tx.TransitionType)...)
	buf = binary.BigEndian.AppendUint64(buf, tx.Term)
	buf = binary.BigEndian.AppendUint64(buf, uint64(tx.Timestamp.UnixNano()))

	return buf
}

// =============================================================================
// TENANT ISOLATION
// =============================================================================

// TenantContext contains tenant security context.
type TenantContext struct {
	TenantID    types.TenantID
	Permissions []Permission
	DataKey     []byte // Derived encryption key for tenant data
	ExpiresAt   time.Time
}

// Permission defines an allowed operation.
type Permission struct {
	Resource string
	Action   string
}

// TenantIsolation enforces tenant boundaries.
type TenantIsolation struct {
	mu       sync.RWMutex
	tenants  map[types.TenantID]*TenantContext
	masterKey []byte
}

// NewTenantIsolation creates tenant isolation manager.
func NewTenantIsolation(masterKey []byte) *TenantIsolation {
	return &TenantIsolation{
		tenants:   make(map[types.TenantID]*TenantContext),
		masterKey: masterKey,
	}
}

// RegisterTenant registers a new tenant.
func (ti *TenantIsolation) RegisterTenant(
	tenantID types.TenantID,
	permissions []Permission,
	validDuration time.Duration,
) (*TenantContext, error) {
	ti.mu.Lock()
	defer ti.mu.Unlock()

	// Derive tenant-specific data key
	dataKey := ti.deriveTenantKey(tenantID)

	ctx := &TenantContext{
		TenantID:    tenantID,
		Permissions: permissions,
		DataKey:     dataKey,
		ExpiresAt:   time.Now().Add(validDuration),
	}

	ti.tenants[tenantID] = ctx

	return ctx, nil
}

// deriveTenantKey derives a unique key for tenant data encryption.
func (ti *TenantIsolation) deriveTenantKey(tenantID types.TenantID) []byte {
	// HKDF-like derivation
	h := sha256.New()
	h.Write(ti.masterKey)
	h.Write(tenantID[:])
	h.Write([]byte("tenant-data-key"))
	return h.Sum(nil)
}

// CheckPermission verifies a tenant has permission for an action.
func (ti *TenantIsolation) CheckPermission(
	tenantID types.TenantID,
	resource string,
	action string,
) error {
	ti.mu.RLock()
	defer ti.mu.RUnlock()

	ctx, ok := ti.tenants[tenantID]
	if !ok {
		return ErrUnknownTenant
	}

	if time.Now().After(ctx.ExpiresAt) {
		return ErrTenantExpired
	}

	for _, perm := range ctx.Permissions {
		if perm.Resource == resource && perm.Action == action {
			return nil
		}
		// Wildcard support
		if perm.Resource == "*" && perm.Action == "*" {
			return nil
		}
	}

	return ErrPermissionDenied
}

// EncryptForTenant encrypts data with tenant's key.
func (ti *TenantIsolation) EncryptForTenant(
	tenantID types.TenantID,
	plaintext []byte,
) ([]byte, error) {
	ti.mu.RLock()
	ctx, ok := ti.tenants[tenantID]
	ti.mu.RUnlock()

	if !ok {
		return nil, ErrUnknownTenant
	}

	return encryptAESGCM(ctx.DataKey, plaintext)
}

// DecryptForTenant decrypts data with tenant's key.
func (ti *TenantIsolation) DecryptForTenant(
	tenantID types.TenantID,
	ciphertext []byte,
) ([]byte, error) {
	ti.mu.RLock()
	ctx, ok := ti.tenants[tenantID]
	ti.mu.RUnlock()

	if !ok {
		return nil, ErrUnknownTenant
	}

	return decryptAESGCM(ctx.DataKey, ciphertext)
}

// =============================================================================
// ENCRYPTION UTILITIES
// =============================================================================

// encryptAESGCM encrypts data using AES-256-GCM.
func encryptAESGCM(key, plaintext []byte) ([]byte, error) {
	block, err := aes.NewCipher(key)
	if err != nil {
		return nil, err
	}

	gcm, err := cipher.NewGCM(block)
	if err != nil {
		return nil, err
	}

	nonce := make([]byte, gcm.NonceSize())
	if _, err := rand.Read(nonce); err != nil {
		return nil, err
	}

	// Prepend nonce to ciphertext
	ciphertext := gcm.Seal(nonce, nonce, plaintext, nil)
	return ciphertext, nil
}

// decryptAESGCM decrypts data using AES-256-GCM.
func decryptAESGCM(key, ciphertext []byte) ([]byte, error) {
	block, err := aes.NewCipher(key)
	if err != nil {
		return nil, err
	}

	gcm, err := cipher.NewGCM(block)
	if err != nil {
		return nil, err
	}

	if len(ciphertext) < gcm.NonceSize() {
		return nil, ErrCiphertextTooShort
	}

	nonce := ciphertext[:gcm.NonceSize()]
	ciphertext = ciphertext[gcm.NonceSize():]

	return gcm.Open(nil, nonce, ciphertext, nil)
}

// =============================================================================
// ANOMALY DETECTION (GREYAV INTEGRATION)
// =============================================================================

// AnomalyDetector detects suspicious behavior patterns.
type AnomalyDetector struct {
	mu        sync.Mutex
	patterns  map[string]*BehaviorPattern
	alertFunc func(alert *SecurityAlert)
}

// BehaviorPattern tracks normal behavior for deviation detection.
type BehaviorPattern struct {
	Name          string
	WindowSize    time.Duration
	Observations  []float64
	Mean          float64
	StdDev        float64
	LastUpdated   time.Time
}

// SecurityAlert represents a detected anomaly.
type SecurityAlert struct {
	Timestamp   time.Time
	Type        AlertType
	Severity    AlertSeverity
	NodeID      types.NodeID
	TenantID    types.TenantID
	Description string
	Evidence    map[string]interface{}
}

// AlertType categorizes security alerts.
type AlertType string

const (
	AlertBrute           AlertType = "brute_force"
	AlertPrivilegeEsc    AlertType = "privilege_escalation"
	AlertDataExfil       AlertType = "data_exfiltration"
	AlertAnomalousBehav  AlertType = "anomalous_behavior"
	AlertConsensusAttack AlertType = "consensus_attack"
)

// AlertSeverity indicates alert urgency.
type AlertSeverity int

const (
	SeverityLow AlertSeverity = iota
	SeverityMedium
	SeverityHigh
	SeverityCritical
)

// NewAnomalyDetector creates an anomaly detector.
func NewAnomalyDetector(alertFunc func(*SecurityAlert)) *AnomalyDetector {
	return &AnomalyDetector{
		patterns:  make(map[string]*BehaviorPattern),
		alertFunc: alertFunc,
	}
}

// RegisterPattern registers a behavior pattern to monitor.
func (ad *AnomalyDetector) RegisterPattern(name string, windowSize time.Duration) {
	ad.mu.Lock()
	defer ad.mu.Unlock()

	ad.patterns[name] = &BehaviorPattern{
		Name:         name,
		WindowSize:   windowSize,
		Observations: make([]float64, 0, 1000),
		LastUpdated:  time.Now(),
	}
}

// Observe records an observation for a pattern.
func (ad *AnomalyDetector) Observe(patternName string, value float64) {
	ad.mu.Lock()
	defer ad.mu.Unlock()

	pattern, ok := ad.patterns[patternName]
	if !ok {
		return
	}

	pattern.Observations = append(pattern.Observations, value)
	pattern.LastUpdated = time.Now()

	// Update statistics
	if len(pattern.Observations) > 10 {
		pattern.Mean, pattern.StdDev = calculateStats(pattern.Observations)

		// Check for anomaly (Z-score > 3)
		if pattern.StdDev > 0 {
			zScore := (value - pattern.Mean) / pattern.StdDev
			if zScore > 3.0 || zScore < -3.0 {
				go ad.alertFunc(&SecurityAlert{
					Timestamp:   time.Now(),
					Type:        AlertAnomalousBehav,
					Severity:    SeverityMedium,
					Description: patternName + " anomaly detected",
					Evidence: map[string]interface{}{
						"value":   value,
						"mean":    pattern.Mean,
						"stddev":  pattern.StdDev,
						"z_score": zScore,
					},
				})
			}
		}
	}

	// Trim old observations
	if len(pattern.Observations) > 10000 {
		pattern.Observations = pattern.Observations[5000:]
	}
}

// CheckRateLimit detects brute force attempts.
func (ad *AnomalyDetector) CheckRateLimit(
	key string,
	limit int,
	window time.Duration,
	current int,
) bool {
	if current > limit {
		go ad.alertFunc(&SecurityAlert{
			Timestamp:   time.Now(),
			Type:        AlertBrute,
			Severity:    SeverityHigh,
			Description: "Rate limit exceeded for " + key,
			Evidence: map[string]interface{}{
				"key":     key,
				"limit":   limit,
				"current": current,
				"window":  window.String(),
			},
		})
		return false
	}
	return true
}

// calculateStats computes mean and standard deviation.
func calculateStats(values []float64) (mean, stddev float64) {
	if len(values) == 0 {
		return 0, 0
	}

	// Mean
	sum := 0.0
	for _, v := range values {
		sum += v
	}
	mean = sum / float64(len(values))

	// Standard deviation
	sumSq := 0.0
	for _, v := range values {
		diff := v - mean
		sumSq += diff * diff
	}
	variance := sumSq / float64(len(values))
	stddev = sqrt(variance)

	return
}

// sqrt computes square root using Newton's method.
func sqrt(x float64) float64 {
	if x <= 0 {
		return 0
	}
	z := x
	for i := 0; i < 10; i++ {
		z = z - (z*z-x)/(2*z)
	}
	return z
}

// =============================================================================
// SECURE CHANNEL
// =============================================================================

// SecureChannel provides encrypted communication between nodes.
type SecureChannel struct {
	localIdentity  *NodeIdentity
	remoteIdentity *NodeIdentity
	sharedKey      []byte
	gcm            cipher.AEAD
	mu             sync.Mutex
}

// EstablishSecureChannel performs key exchange and creates a secure channel.
func EstablishSecureChannel(
	localIdentity *NodeIdentity,
	remotePublicKey ed25519.PublicKey,
	remoteNodeID types.NodeID,
) (*SecureChannel, error) {
	// For simplicity, we derive a shared key from both public keys
	// In production, you'd use X25519 with ephemeral keys
	h := sha256.New()
	h.Write(localIdentity.PublicKey)
	h.Write(remotePublicKey)
	// Sort to ensure both sides derive same key
	if bytes.Compare(localIdentity.PublicKey, remotePublicKey) > 0 {
		h.Reset()
		h.Write(remotePublicKey)
		h.Write(localIdentity.PublicKey)
	}
	sharedKey := h.Sum(nil)

	block, err := aes.NewCipher(sharedKey)
	if err != nil {
		return nil, err
	}

	gcm, err := cipher.NewGCM(block)
	if err != nil {
		return nil, err
	}

	return &SecureChannel{
		localIdentity: localIdentity,
		remoteIdentity: &NodeIdentity{
			NodeID:    remoteNodeID,
			PublicKey: remotePublicKey,
		},
		sharedKey: sharedKey,
		gcm:       gcm,
	}, nil
}

// Encrypt encrypts a message for the remote node.
func (sc *SecureChannel) Encrypt(plaintext []byte) ([]byte, error) {
	sc.mu.Lock()
	defer sc.mu.Unlock()

	nonce := make([]byte, sc.gcm.NonceSize())
	if _, err := rand.Read(nonce); err != nil {
		return nil, err
	}

	return sc.gcm.Seal(nonce, nonce, plaintext, nil), nil
}

// Decrypt decrypts a message from the remote node.
func (sc *SecureChannel) Decrypt(ciphertext []byte) ([]byte, error) {
	sc.mu.Lock()
	defer sc.mu.Unlock()

	if len(ciphertext) < sc.gcm.NonceSize() {
		return nil, ErrCiphertextTooShort
	}

	nonce := ciphertext[:sc.gcm.NonceSize()]
	ciphertext = ciphertext[sc.gcm.NonceSize():]

	return sc.gcm.Open(nil, nonce, ciphertext, nil)
}

// =============================================================================
// ERRORS
// =============================================================================

var (
	ErrNoPrivateKey       = errors.New("no private key available")
	ErrCertificateRevoked = errors.New("certificate has been revoked")
	ErrCertificateExpired = errors.New("certificate has expired")
	ErrWrongCluster       = errors.New("certificate is for a different cluster")
	ErrInvalidSignature   = errors.New("signature verification failed")
	ErrStateHashMismatch  = errors.New("state hash does not match")
	ErrUnknownNode        = errors.New("unknown node")
	ErrUnknownTenant      = errors.New("unknown tenant")
	ErrTenantExpired      = errors.New("tenant context has expired")
	ErrPermissionDenied   = errors.New("permission denied")
	ErrInsufficientQuorum = errors.New("insufficient quorum for verification")
	ErrCiphertextTooShort = errors.New("ciphertext too short")
)
