// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";

/**
 * @title BLSSignatureAggregator
 * @author Grey Protocol
 * @notice BLS signature aggregation patterns for efficient multi-sig verification
 * @dev Provides infrastructure for BLS12-381 signature aggregation
 * 
 * Note: Full BLS verification requires precompiles not yet available on all chains.
 * This contract provides the infrastructure and interface patterns for BLS.
 * 
 * Features:
 * - Public key registration and management
 * - Signature aggregation tracking
 * - Batch signature verification interface
 * - Threshold signature support
 * - Key rotation with commitment periods
 */
contract BLSSignatureAggregator is AccessControl {
    
    // ============================================
    // CONSTANTS
    // ============================================

    bytes32 public constant KEY_MANAGER_ROLE = keccak256("KEY_MANAGER_ROLE");

    /// @notice BLS12-381 field modulus (stored as hex string for reference; actual value exceeds uint256)
    /// @dev Full value: 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab
    bytes32 public constant BLS_MODULUS_UPPER = 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f624;

    /// @notice Minimum signers for aggregated signature
    uint256 public constant MIN_SIGNERS = 1;

    // ============================================
    // STRUCTS
    // ============================================

    /// @notice BLS public key (G1 point on BLS12-381)
    struct BLSPublicKey {
        uint256[2] x;  // Fp element (48 bytes packed into 2 uint256s)
        uint256[2] y;  // Fp element
    }

    /// @notice BLS signature (G2 point)
    struct BLSSignature {
        uint256[4] data;  // Fp2 element pair (96 bytes packed)
    }

    /// @notice Aggregated signature data
    struct AggregatedSignature {
        BLSSignature signature;
        bytes32 messageHash;
        uint256 signerBitmap;  // Bitmap of which signers are included
        uint256 timestamp;
        bool verified;
    }

    /// @notice Signer registration
    struct SignerInfo {
        BLSPublicKey publicKey;
        address ethAddress;
        uint256 weight;
        bool active;
        uint256 registeredAt;
        uint256 lastKeyRotation;
    }

    /// @notice Key rotation request
    struct KeyRotationRequest {
        uint256 signerId;
        BLSPublicKey newKey;
        uint256 requestTime;
        uint256 effectiveTime;
        bool executed;
    }

    /// @notice Threshold config
    struct ThresholdConfig {
        uint256 numerator;
        uint256 denominator;
        uint256 minSigners;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Signer ID => info
    mapping(uint256 => SignerInfo) public signers;

    /// @notice ETH address => signer ID
    mapping(address => uint256) public signerIds;

    /// @notice Message hash => aggregated signature
    mapping(bytes32 => AggregatedSignature) public aggregatedSignatures;

    /// @notice Key rotation requests
    mapping(bytes32 => KeyRotationRequest) public keyRotationRequests;

    /// @notice Total number of signers
    uint256 public signerCount;

    /// @notice Total weight of all active signers
    uint256 public totalWeight;

    /// @notice Threshold configuration
    ThresholdConfig public threshold;

    /// @notice Key rotation delay
    uint256 public keyRotationDelay;

    /// @notice Domain separator for message hashing
    bytes32 public immutable DOMAIN_SEPARATOR;

    // ============================================
    // EVENTS
    // ============================================

    event SignerRegistered(
        uint256 indexed signerId,
        address indexed ethAddress,
        uint256 weight
    );

    event SignerDeactivated(uint256 indexed signerId);

    event SignerActivated(uint256 indexed signerId);

    event SignatureAggregated(
        bytes32 indexed messageHash,
        uint256 signerBitmap,
        uint256 totalWeight
    );

    event SignatureVerified(bytes32 indexed messageHash, bool valid);

    event KeyRotationRequested(
        uint256 indexed signerId,
        bytes32 requestId,
        uint256 effectiveTime
    );

    event KeyRotationExecuted(uint256 indexed signerId, bytes32 requestId);

    event ThresholdUpdated(uint256 numerator, uint256 denominator);

    // ============================================
    // ERRORS
    // ============================================

    error InvalidPublicKey();
    error SignerNotFound();
    error SignerNotActive();
    error AlreadyRegistered();
    error InvalidWeight();
    error ThresholdNotMet();
    error KeyRotationPending();
    error KeyRotationNotReady();
    error InvalidSignature();
    error MessageAlreadySigned();

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(
        uint256 thresholdNum,
        uint256 thresholdDenom,
        uint256 minSigners,
        uint256 _keyRotationDelay
    ) {
        threshold = ThresholdConfig({
            numerator: thresholdNum,
            denominator: thresholdDenom,
            minSigners: minSigners
        });

        keyRotationDelay = _keyRotationDelay;

        DOMAIN_SEPARATOR = keccak256(abi.encode(
            keccak256("EIP712Domain(string name,string version,uint256 chainId,address verifyingContract)"),
            keccak256("BLSSignatureAggregator"),
            keccak256("1"),
            block.chainid,
            address(this)
        ));

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(KEY_MANAGER_ROLE, msg.sender);
    }

    // ============================================
    // SIGNER MANAGEMENT
    // ============================================

    /**
     * @notice Register a new BLS signer
     * @param pubKeyX X coordinate of public key
     * @param pubKeyY Y coordinate of public key
     * @param ethAddress Associated Ethereum address
     * @param weight Voting weight
     */
    function registerSigner(
        uint256[2] calldata pubKeyX,
        uint256[2] calldata pubKeyY,
        address ethAddress,
        uint256 weight
    ) external onlyRole(KEY_MANAGER_ROLE) returns (uint256 signerId) {
        if (signerIds[ethAddress] != 0) revert AlreadyRegistered();
        if (weight == 0) revert InvalidWeight();
        if (!_isValidPublicKey(pubKeyX, pubKeyY)) revert InvalidPublicKey();

        signerCount++;
        signerId = signerCount;

        signers[signerId] = SignerInfo({
            publicKey: BLSPublicKey({x: pubKeyX, y: pubKeyY}),
            ethAddress: ethAddress,
            weight: weight,
            active: true,
            registeredAt: block.timestamp,
            lastKeyRotation: 0
        });

        signerIds[ethAddress] = signerId;
        totalWeight += weight;

        emit SignerRegistered(signerId, ethAddress, weight);
    }

    /**
     * @notice Deactivate a signer
     */
    function deactivateSigner(uint256 signerId) external onlyRole(KEY_MANAGER_ROLE) {
        SignerInfo storage signer = signers[signerId];
        if (signer.ethAddress == address(0)) revert SignerNotFound();
        if (!signer.active) revert SignerNotActive();

        signer.active = false;
        totalWeight -= signer.weight;

        emit SignerDeactivated(signerId);
    }

    /**
     * @notice Reactivate a signer
     */
    function activateSigner(uint256 signerId) external onlyRole(KEY_MANAGER_ROLE) {
        SignerInfo storage signer = signers[signerId];
        if (signer.ethAddress == address(0)) revert SignerNotFound();
        if (signer.active) return;

        signer.active = true;
        totalWeight += signer.weight;

        emit SignerActivated(signerId);
    }

    /**
     * @notice Update signer weight
     */
    function updateWeight(uint256 signerId, uint256 newWeight) 
        external 
        onlyRole(KEY_MANAGER_ROLE) 
    {
        SignerInfo storage signer = signers[signerId];
        if (signer.ethAddress == address(0)) revert SignerNotFound();
        if (newWeight == 0) revert InvalidWeight();

        if (signer.active) {
            totalWeight = totalWeight - signer.weight + newWeight;
        }
        signer.weight = newWeight;
    }

    // ============================================
    // KEY ROTATION
    // ============================================

    /**
     * @notice Request key rotation for a signer
     * @param signerId Signer ID
     * @param newPubKeyX New public key X
     * @param newPubKeyY New public key Y
     */
    function requestKeyRotation(
        uint256 signerId,
        uint256[2] calldata newPubKeyX,
        uint256[2] calldata newPubKeyY
    ) external returns (bytes32 requestId) {
        SignerInfo storage signer = signers[signerId];
        if (signer.ethAddress == address(0)) revert SignerNotFound();
        if (msg.sender != signer.ethAddress && !hasRole(KEY_MANAGER_ROLE, msg.sender)) {
            revert SignerNotFound();
        }
        if (!_isValidPublicKey(newPubKeyX, newPubKeyY)) revert InvalidPublicKey();

        requestId = keccak256(abi.encode(signerId, newPubKeyX, newPubKeyY, block.timestamp));

        keyRotationRequests[requestId] = KeyRotationRequest({
            signerId: signerId,
            newKey: BLSPublicKey({x: newPubKeyX, y: newPubKeyY}),
            requestTime: block.timestamp,
            effectiveTime: block.timestamp + keyRotationDelay,
            executed: false
        });

        emit KeyRotationRequested(signerId, requestId, block.timestamp + keyRotationDelay);
    }

    /**
     * @notice Execute a pending key rotation
     */
    function executeKeyRotation(bytes32 requestId) external {
        KeyRotationRequest storage request = keyRotationRequests[requestId];
        if (request.requestTime == 0) revert SignerNotFound();
        if (request.executed) revert KeyRotationPending();
        if (block.timestamp < request.effectiveTime) revert KeyRotationNotReady();

        SignerInfo storage signer = signers[request.signerId];
        signer.publicKey = request.newKey;
        signer.lastKeyRotation = block.timestamp;
        request.executed = true;

        emit KeyRotationExecuted(request.signerId, requestId);
    }

    // ============================================
    // SIGNATURE AGGREGATION
    // ============================================

    /**
     * @notice Submit an aggregated signature
     * @param messageHash Hash of the signed message
     * @param signature Aggregated BLS signature
     * @param signerBitmap Bitmap indicating which signers are included
     */
    function submitAggregatedSignature(
        bytes32 messageHash,
        uint256[4] calldata signature,
        uint256 signerBitmap
    ) external returns (bool valid) {
        // Check if already submitted
        if (aggregatedSignatures[messageHash].timestamp != 0) {
            revert MessageAlreadySigned();
        }

        // Calculate total weight of signers
        uint256 signedWeight = _calculateSignedWeight(signerBitmap);
        uint256 requiredWeight = _requiredWeight();

        if (signedWeight < requiredWeight) revert ThresholdNotMet();

        // Count signers
        uint256 signerCountInBitmap = _popcount(signerBitmap);
        if (signerCountInBitmap < threshold.minSigners) revert ThresholdNotMet();

        // Verify signature (mock - real implementation needs precompile)
        valid = _verifyAggregatedSignature(messageHash, signature, signerBitmap);

        aggregatedSignatures[messageHash] = AggregatedSignature({
            signature: BLSSignature({data: signature}),
            messageHash: messageHash,
            signerBitmap: signerBitmap,
            timestamp: block.timestamp,
            verified: valid
        });

        emit SignatureAggregated(messageHash, signerBitmap, signedWeight);
        emit SignatureVerified(messageHash, valid);
    }

    /**
     * @notice Verify a stored aggregated signature
     */
    function verifyStoredSignature(bytes32 messageHash) 
        external 
        view 
        returns (bool valid, uint256 signedWeight) 
    {
        AggregatedSignature storage aggSig = aggregatedSignatures[messageHash];
        if (aggSig.timestamp == 0) return (false, 0);

        signedWeight = _calculateSignedWeight(aggSig.signerBitmap);
        valid = aggSig.verified;
    }

    // ============================================
    // INTERNAL FUNCTIONS
    // ============================================

    /**
     * @notice Validate public key is on curve
     */
    function _isValidPublicKey(uint256[2] calldata x, uint256[2] calldata y) 
        internal 
        pure 
        returns (bool) 
    {
        // Simplified validation - check non-zero
        // Real implementation would check point is on BLS12-381 G1
        return (x[0] != 0 || x[1] != 0) && (y[0] != 0 || y[1] != 0);
    }

    /**
     * @notice Calculate weight of signers in bitmap
     */
    function _calculateSignedWeight(uint256 bitmap) 
        internal 
        view 
        returns (uint256 weight) 
    {
        for (uint256 i = 1; i <= signerCount; i++) {
            if ((bitmap & (1 << (i - 1))) != 0) {
                SignerInfo storage signer = signers[i];
                if (signer.active) {
                    weight += signer.weight;
                }
            }
        }
    }

    /**
     * @notice Get required weight for threshold
     */
    function _requiredWeight() internal view returns (uint256) {
        return (totalWeight * threshold.numerator) / threshold.denominator;
    }

    /**
     * @notice Count set bits (population count)
     */
    function _popcount(uint256 x) internal pure returns (uint256 count) {
        while (x != 0) {
            count += x & 1;
            x >>= 1;
        }
    }

    /**
     * @notice Verify aggregated BLS signature
     * @dev Mock implementation - real version needs BLS precompile
     */
    function _verifyAggregatedSignature(
        bytes32 messageHash,
        uint256[4] calldata signature,
        uint256 signerBitmap
    ) internal view returns (bool) {
        // In a real implementation, this would:
        // 1. Aggregate public keys of signers in bitmap
        // 2. Use BLS12-381 pairing precompile
        // 3. Verify: e(aggPK, H(m)) = e(G1, sig)
        
        // For now, return true if signature is non-zero
        return (signature[0] != 0 || signature[1] != 0 || 
                signature[2] != 0 || signature[3] != 0);
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Get signer info
     */
    function getSigner(uint256 signerId) 
        external 
        view 
        returns (SignerInfo memory) 
    {
        return signers[signerId];
    }

    /**
     * @notice Get signer ID by address
     */
    function getSignerByAddress(address addr) external view returns (uint256) {
        return signerIds[addr];
    }

    /**
     * @notice Check if message has valid signature
     */
    function hasValidSignature(bytes32 messageHash) external view returns (bool) {
        return aggregatedSignatures[messageHash].verified;
    }

    /**
     * @notice Get aggregated signature for message
     */
    function getAggregatedSignature(bytes32 messageHash) 
        external 
        view 
        returns (AggregatedSignature memory) 
    {
        return aggregatedSignatures[messageHash];
    }

    /**
     * @notice Get required weight threshold
     */
    function getRequiredWeight() external view returns (uint256) {
        return _requiredWeight();
    }

    // ============================================
    // ADMIN FUNCTIONS
    // ============================================

    function setThreshold(uint256 numerator, uint256 denominator, uint256 minSigners) 
        external 
        onlyRole(DEFAULT_ADMIN_ROLE) 
    {
        require(denominator > 0 && numerator <= denominator, "Invalid threshold");
        threshold = ThresholdConfig({
            numerator: numerator,
            denominator: denominator,
            minSigners: minSigners
        });
        emit ThresholdUpdated(numerator, denominator);
    }

    function setKeyRotationDelay(uint256 delay) 
        external 
        onlyRole(DEFAULT_ADMIN_ROLE) 
    {
        keyRotationDelay = delay;
    }
}
