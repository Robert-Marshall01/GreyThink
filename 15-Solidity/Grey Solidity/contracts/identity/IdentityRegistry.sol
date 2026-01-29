// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/cryptography/ECDSA.sol";
import "@openzeppelin/contracts/utils/cryptography/MessageHashUtils.sol";

/**
 * @title IdentityRegistry
 * @notice Decentralized identity registry with attestations
 * @dev Implements W3C DID-compatible on-chain identity system
 * 
 * Features:
 * - Self-sovereign identity creation and management
 * - Delegated keys with different permissions
 * - Attestation issuance and verification
 * - Revocation registry
 * - Identity recovery mechanism
 * - Compatible with ERC-1056 (Ethr-DID)
 */
contract IdentityRegistry is AccessControl {
    using ECDSA for bytes32;
    using MessageHashUtils for bytes32;

    // ============================================
    // ROLES
    // ============================================

    bytes32 public constant ATTESTER_ROLE = keccak256("ATTESTER_ROLE");
    bytes32 public constant TRUSTED_ISSUER_ROLE = keccak256("TRUSTED_ISSUER_ROLE");

    // ============================================
    // TYPES
    // ============================================

    /// @notice Identity structure
    struct Identity {
        address owner;                   // Primary owner
        uint256 created;                 // Creation timestamp
        uint256 updated;                 // Last update timestamp
        bool active;                     // Is identity active
        address recoveryAddress;         // Recovery address
        uint256 recoveryDelay;           // Recovery waiting period
    }

    /// @notice Delegate key with permissions
    struct DelegateKey {
        address delegate;
        DelegateType keyType;
        uint256 validUntil;
        bool active;
    }

    /// @notice Attestation (credential)
    struct Attestation {
        bytes32 attestationId;
        address issuer;
        address subject;
        bytes32 schemaHash;              // Hash of credential schema
        bytes32 dataHash;                // Hash of attestation data
        uint256 issuedAt;
        uint256 expiresAt;
        bool revoked;
        AttestationLevel level;
    }

    /// @notice Recovery request
    struct RecoveryRequest {
        address newOwner;
        uint256 requestTime;
        bool executed;
    }

    /// @notice Service endpoint for DID Document
    struct ServiceEndpoint {
        string serviceType;              // e.g., "messaging", "storage"
        string endpoint;                 // URI
        bool active;
    }

    enum DelegateType {
        SignAuthentication,              // Can sign on behalf
        KeyAgreement,                    // For encryption
        AssertionMethod,                 // For attestation issuance
        CapabilityInvocation,            // For smart contract calls
        CapabilityDelegation             // Can delegate to others
    }

    enum AttestationLevel {
        SelfAttested,                    // Self-claimed
        PeerAttested,                    // Attested by peer
        IssuerAttested,                  // Attested by trusted issuer
        InstitutionalAttested            // Attested by institution
    }

    // ============================================
    // STATE
    // ============================================

    /// @notice Identity by address
    mapping(address => Identity) public identities;

    /// @notice Delegate keys: identity => delegate => type => DelegateKey
    mapping(address => mapping(address => mapping(DelegateType => DelegateKey))) public delegates;

    /// @notice Attestations by ID
    mapping(bytes32 => Attestation) public attestations;

    /// @notice Attestations received by subject
    mapping(address => bytes32[]) public subjectAttestations;

    /// @notice Attestations issued by issuer
    mapping(address => bytes32[]) public issuerAttestations;

    /// @notice Recovery requests
    mapping(address => RecoveryRequest) public recoveryRequests;

    /// @notice Service endpoints
    mapping(address => mapping(bytes32 => ServiceEndpoint)) public serviceEndpoints;

    /// @notice Service endpoint IDs for iteration
    mapping(address => bytes32[]) public serviceEndpointIds;

    /// @notice Schema registry
    mapping(bytes32 => bytes) public schemas;

    /// @notice Revocation registry by issuer
    mapping(address => mapping(bytes32 => bool)) public revocationRegistry;

    /// @notice Nonces for signature replay protection
    mapping(address => uint256) public nonces;

    /// @notice Changed event block for DID Document updates
    mapping(address => uint256) public changed;

    // ============================================
    // EVENTS
    // ============================================

    event IdentityCreated(
        address indexed identity,
        address indexed owner,
        uint256 timestamp
    );

    event IdentityUpdated(
        address indexed identity,
        address indexed owner,
        uint256 timestamp
    );

    event OwnerChanged(
        address indexed identity,
        address indexed previousOwner,
        address indexed newOwner
    );

    event DelegateAdded(
        address indexed identity,
        address indexed delegate,
        DelegateType keyType,
        uint256 validUntil
    );

    event DelegateRemoved(
        address indexed identity,
        address indexed delegate,
        DelegateType keyType
    );

    event AttestationIssued(
        bytes32 indexed attestationId,
        address indexed issuer,
        address indexed subject,
        bytes32 schemaHash
    );

    event AttestationRevoked(
        bytes32 indexed attestationId,
        address indexed issuer,
        address indexed subject
    );

    event RecoveryInitiated(
        address indexed identity,
        address indexed recoveryAddress,
        address newOwner
    );

    event RecoveryExecuted(
        address indexed identity,
        address indexed oldOwner,
        address indexed newOwner
    );

    event ServiceEndpointAdded(
        address indexed identity,
        bytes32 indexed serviceId,
        string serviceType
    );

    // ============================================
    // ERRORS
    // ============================================

    error IdentityAlreadyExists();
    error IdentityNotFound();
    error NotIdentityOwner();
    error NotDelegateOrOwner();
    error DelegateNotFound();
    error DelegateExpired();
    error AttestationNotFound();
    error AttestationAlreadyRevoked();
    error InvalidSignature();
    error RecoveryNotInitiated();
    error RecoveryDelayNotPassed();
    error InvalidRecoveryAddress();
    error Unauthorized();

    // ============================================
    // MODIFIERS
    // ============================================

    modifier onlyIdentityOwner(address identity) {
        if (identities[identity].owner != msg.sender) revert NotIdentityOwner();
        _;
    }

    modifier identityExists(address identity) {
        if (!identities[identity].active) revert IdentityNotFound();
        _;
    }

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor() {
        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
    }

    // ============================================
    // IDENTITY MANAGEMENT
    // ============================================

    /**
     * @notice Create a new identity
     * @param recoveryAddress Address that can recover the identity
     * @param recoveryDelay Time delay for recovery (min 1 day)
     */
    function createIdentity(
        address recoveryAddress,
        uint256 recoveryDelay
    ) external {
        if (identities[msg.sender].active) revert IdentityAlreadyExists();
        if (recoveryDelay < 1 days) recoveryDelay = 1 days;

        identities[msg.sender] = Identity({
            owner: msg.sender,
            created: block.timestamp,
            updated: block.timestamp,
            active: true,
            recoveryAddress: recoveryAddress,
            recoveryDelay: recoveryDelay
        });

        changed[msg.sender] = block.number;

        emit IdentityCreated(msg.sender, msg.sender, block.timestamp);
    }

    /**
     * @notice Transfer identity ownership
     * @param identity The identity to transfer
     * @param newOwner The new owner address
     */
    function transferOwnership(
        address identity,
        address newOwner
    ) external onlyIdentityOwner(identity) {
        address previousOwner = identities[identity].owner;
        identities[identity].owner = newOwner;
        identities[identity].updated = block.timestamp;
        changed[identity] = block.number;

        emit OwnerChanged(identity, previousOwner, newOwner);
    }

    /**
     * @notice Update recovery settings
     */
    function updateRecoverySettings(
        address identity,
        address newRecoveryAddress,
        uint256 newRecoveryDelay
    ) external onlyIdentityOwner(identity) {
        if (newRecoveryDelay < 1 days) newRecoveryDelay = 1 days;

        identities[identity].recoveryAddress = newRecoveryAddress;
        identities[identity].recoveryDelay = newRecoveryDelay;
        identities[identity].updated = block.timestamp;
        changed[identity] = block.number;

        emit IdentityUpdated(identity, msg.sender, block.timestamp);
    }

    // ============================================
    // DELEGATE MANAGEMENT
    // ============================================

    /**
     * @notice Add a delegate key
     * @param identity The identity to add delegate to
     * @param delegate The delegate address
     * @param keyType The type of delegation
     * @param validity How long the delegation is valid (seconds)
     */
    function addDelegate(
        address identity,
        address delegate,
        DelegateType keyType,
        uint256 validity
    ) public onlyIdentityOwner(identity) {
        uint256 validUntil = block.timestamp + validity;

        delegates[identity][delegate][keyType] = DelegateKey({
            delegate: delegate,
            keyType: keyType,
            validUntil: validUntil,
            active: true
        });

        changed[identity] = block.number;

        emit DelegateAdded(identity, delegate, keyType, validUntil);
    }

    /**
     * @notice Add delegate with signature (meta-transaction)
     */
    function addDelegateSigned(
        address identity,
        address delegate,
        DelegateType keyType,
        uint256 validity,
        uint8 v,
        bytes32 r,
        bytes32 s
    ) external {
        bytes32 hash = keccak256(abi.encodePacked(
            address(this),
            "addDelegate",
            identity,
            delegate,
            keyType,
            validity,
            nonces[identities[identity].owner]++
        )).toEthSignedMessageHash();

        address signer = hash.recover(v, r, s);
        if (signer != identities[identity].owner) revert InvalidSignature();

        addDelegate(identity, delegate, keyType, validity);
    }

    /**
     * @notice Remove a delegate key
     */
    function removeDelegate(
        address identity,
        address delegate,
        DelegateType keyType
    ) external onlyIdentityOwner(identity) {
        DelegateKey storage key = delegates[identity][delegate][keyType];
        if (!key.active) revert DelegateNotFound();

        key.active = false;
        changed[identity] = block.number;

        emit DelegateRemoved(identity, delegate, keyType);
    }

    /**
     * @notice Check if a delegate is valid
     */
    function isValidDelegate(
        address identity,
        address delegate,
        DelegateType keyType
    ) public view returns (bool) {
        DelegateKey memory key = delegates[identity][delegate][keyType];
        return key.active && key.validUntil > block.timestamp;
    }

    // ============================================
    // ATTESTATION FUNCTIONS
    // ============================================

    /**
     * @notice Issue an attestation
     * @param subject The subject of the attestation
     * @param schemaHash Hash of the credential schema
     * @param dataHash Hash of the attestation data (stored off-chain)
     * @param expiresIn How long until expiration (0 = never)
     */
    function issueAttestation(
        address subject,
        bytes32 schemaHash,
        bytes32 dataHash,
        uint256 expiresIn
    ) external returns (bytes32 attestationId) {
        // Determine attestation level
        AttestationLevel level;
        if (msg.sender == subject) {
            level = AttestationLevel.SelfAttested;
        } else if (hasRole(TRUSTED_ISSUER_ROLE, msg.sender)) {
            level = AttestationLevel.InstitutionalAttested;
        } else if (hasRole(ATTESTER_ROLE, msg.sender)) {
            level = AttestationLevel.IssuerAttested;
        } else {
            level = AttestationLevel.PeerAttested;
        }

        attestationId = keccak256(abi.encodePacked(
            msg.sender,
            subject,
            schemaHash,
            dataHash,
            block.timestamp
        ));

        uint256 expiresAt = expiresIn > 0 ? block.timestamp + expiresIn : 0;

        attestations[attestationId] = Attestation({
            attestationId: attestationId,
            issuer: msg.sender,
            subject: subject,
            schemaHash: schemaHash,
            dataHash: dataHash,
            issuedAt: block.timestamp,
            expiresAt: expiresAt,
            revoked: false,
            level: level
        });

        subjectAttestations[subject].push(attestationId);
        issuerAttestations[msg.sender].push(attestationId);

        emit AttestationIssued(attestationId, msg.sender, subject, schemaHash);
    }

    /**
     * @notice Revoke an attestation
     */
    function revokeAttestation(bytes32 attestationId) external {
        Attestation storage att = attestations[attestationId];

        if (att.issuer == address(0)) revert AttestationNotFound();
        if (att.issuer != msg.sender) revert NotIdentityOwner();
        if (att.revoked) revert AttestationAlreadyRevoked();

        att.revoked = true;
        revocationRegistry[msg.sender][attestationId] = true;

        emit AttestationRevoked(attestationId, msg.sender, att.subject);
    }

    /**
     * @notice Verify an attestation is valid
     */
    function verifyAttestation(bytes32 attestationId) external view returns (
        bool isValid,
        address issuer,
        address subject,
        AttestationLevel level
    ) {
        Attestation memory att = attestations[attestationId];

        bool valid = att.issuer != address(0) &&
                     !att.revoked &&
                     (att.expiresAt == 0 || att.expiresAt > block.timestamp);

        return (valid, att.issuer, att.subject, att.level);
    }

    /**
     * @notice Verify attestation with signature (for off-chain attestations)
     */
    function verifyAttestationSignature(
        address issuer,
        address subject,
        bytes32 schemaHash,
        bytes32 dataHash,
        uint256 issuedAt,
        uint256 expiresAt,
        bytes calldata signature
    ) external view returns (bool) {
        bytes32 hash = keccak256(abi.encodePacked(
            address(this),
            issuer,
            subject,
            schemaHash,
            dataHash,
            issuedAt,
            expiresAt
        )).toEthSignedMessageHash();

        address signer = hash.recover(signature);

        // Valid if signed by issuer or authorized delegate
        return signer == issuer || isValidDelegate(issuer, signer, DelegateType.AssertionMethod);
    }

    // ============================================
    // RECOVERY FUNCTIONS
    // ============================================

    /**
     * @notice Initiate identity recovery
     * @param identity The identity to recover
     * @param newOwner The new owner after recovery
     */
    function initiateRecovery(
        address identity,
        address newOwner
    ) external identityExists(identity) {
        Identity memory id = identities[identity];

        if (msg.sender != id.recoveryAddress) revert InvalidRecoveryAddress();

        recoveryRequests[identity] = RecoveryRequest({
            newOwner: newOwner,
            requestTime: block.timestamp,
            executed: false
        });

        emit RecoveryInitiated(identity, msg.sender, newOwner);
    }

    /**
     * @notice Execute recovery after delay
     */
    function executeRecovery(address identity) external identityExists(identity) {
        RecoveryRequest memory request = recoveryRequests[identity];
        Identity storage id = identities[identity];

        if (request.requestTime == 0) revert RecoveryNotInitiated();
        if (block.timestamp < request.requestTime + id.recoveryDelay) {
            revert RecoveryDelayNotPassed();
        }

        address oldOwner = id.owner;
        id.owner = request.newOwner;
        id.updated = block.timestamp;

        recoveryRequests[identity].executed = true;
        changed[identity] = block.number;

        emit RecoveryExecuted(identity, oldOwner, request.newOwner);
    }

    /**
     * @notice Cancel recovery (by current owner)
     */
    function cancelRecovery(address identity) external onlyIdentityOwner(identity) {
        delete recoveryRequests[identity];
    }

    // ============================================
    // SERVICE ENDPOINTS
    // ============================================

    /**
     * @notice Add a service endpoint to DID Document
     */
    function addServiceEndpoint(
        address identity,
        string calldata serviceType,
        string calldata endpoint
    ) external onlyIdentityOwner(identity) {
        bytes32 serviceId = keccak256(abi.encodePacked(serviceType, endpoint));

        serviceEndpoints[identity][serviceId] = ServiceEndpoint({
            serviceType: serviceType,
            endpoint: endpoint,
            active: true
        });

        serviceEndpointIds[identity].push(serviceId);
        changed[identity] = block.number;

        emit ServiceEndpointAdded(identity, serviceId, serviceType);
    }

    /**
     * @notice Remove a service endpoint
     */
    function removeServiceEndpoint(
        address identity,
        bytes32 serviceId
    ) external onlyIdentityOwner(identity) {
        serviceEndpoints[identity][serviceId].active = false;
        changed[identity] = block.number;
    }

    // ============================================
    // SCHEMA REGISTRY
    // ============================================

    /**
     * @notice Register a credential schema
     */
    function registerSchema(bytes calldata schema) external returns (bytes32 schemaHash) {
        schemaHash = keccak256(schema);
        schemas[schemaHash] = schema;
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    function getIdentity(address identity) external view returns (
        address owner,
        uint256 created,
        bool active,
        address recoveryAddress
    ) {
        Identity memory id = identities[identity];
        return (id.owner, id.created, id.active, id.recoveryAddress);
    }

    function getSubjectAttestations(address subject) external view returns (bytes32[] memory) {
        return subjectAttestations[subject];
    }

    function getAttestation(bytes32 attestationId) external view returns (
        address issuer,
        address subject,
        bytes32 schemaHash,
        uint256 issuedAt,
        uint256 expiresAt,
        bool revoked,
        AttestationLevel level
    ) {
        Attestation memory att = attestations[attestationId];
        return (
            att.issuer,
            att.subject,
            att.schemaHash,
            att.issuedAt,
            att.expiresAt,
            att.revoked,
            att.level
        );
    }

    function getServiceEndpoints(address identity) external view returns (bytes32[] memory) {
        return serviceEndpointIds[identity];
    }
}
