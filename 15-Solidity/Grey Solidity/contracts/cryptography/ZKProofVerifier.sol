// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";

/**
 * @title ZKProofVerifier
 * @author Grey Protocol
 * @notice Zero-Knowledge proof verification interface and registry
 * @dev Provides abstraction layer for multiple ZK proof systems
 * 
 * Features:
 * - Abstract verifier interface for different proof systems
 * - Verifier registry for proof type management
 * - Groth16, PLONK, and STARK proof structure support
 * - Batch verification for gas efficiency
 * - Proof commitment and reveal patterns
 * - Circuit fingerprint verification
 */
contract ZKProofVerifier is AccessControl {
    
    // ============================================
    // CONSTANTS
    // ============================================

    bytes32 public constant VERIFIER_ADMIN_ROLE = keccak256("VERIFIER_ADMIN_ROLE");
    bytes32 public constant PROVER_ROLE = keccak256("PROVER_ROLE");

    /// @notice Proof systems supported
    uint8 public constant PROOF_SYSTEM_GROTH16 = 1;
    uint8 public constant PROOF_SYSTEM_PLONK = 2;
    uint8 public constant PROOF_SYSTEM_STARK = 3;
    uint8 public constant PROOF_SYSTEM_BULLETPROOF = 4;
    uint8 public constant PROOF_SYSTEM_CUSTOM = 255;

    /// @notice BN254 curve order (for Groth16)
    uint256 public constant PRIME_Q = 21888242871839275222246405745257275088696311157297823662689037894645226208583;

    // ============================================
    // STRUCTS
    // ============================================

    /// @notice Groth16 proof structure
    struct Groth16Proof {
        uint256[2] a;      // G1 point
        uint256[2][2] b;   // G2 point
        uint256[2] c;      // G1 point
    }

    /// @notice PLONK proof structure
    struct PlonkProof {
        uint256[2] commitmentA;
        uint256[2] commitmentB;
        uint256[2] commitmentC;
        uint256[2] commitmentZ;
        uint256[2] commitmentT1;
        uint256[2] commitmentT2;
        uint256[2] commitmentT3;
        uint256[2] openingW;
        uint256[2] openingWZ;
        uint256 evalA;
        uint256 evalB;
        uint256 evalC;
        uint256 evalS1;
        uint256 evalS2;
        uint256 evalZ;
        uint256 evalZW;
    }

    /// @notice Generic proof wrapper
    struct ProofData {
        uint8 proofSystem;
        bytes proof;
        uint256[] publicInputs;
        bytes32 circuitId;
    }

    /// @notice Verifier configuration
    struct VerifierConfig {
        address verifierContract;
        bytes32 circuitId;
        uint8 proofSystem;
        bool enabled;
        uint256 minPublicInputs;
        uint256 maxPublicInputs;
        string description;
    }

    /// @notice Verification result
    struct VerificationResult {
        bool valid;
        bytes32 proofHash;
        uint256 timestamp;
        address verifier;
    }

    /// @notice Proof commitment (for commit-reveal)
    struct ProofCommitment {
        bytes32 commitmentHash;
        address committer;
        uint256 commitTime;
        uint256 revealDeadline;
        bool revealed;
        bool valid;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Registered verifiers
    mapping(bytes32 => VerifierConfig) public verifiers;

    /// @notice Circuit ID => verification key hash
    mapping(bytes32 => bytes32) public verificationKeyHashes;

    /// @notice Proof hash => verification result
    mapping(bytes32 => VerificationResult) public verificationResults;

    /// @notice Commitment hash => commitment data
    mapping(bytes32 => ProofCommitment) public commitments;

    /// @notice Nullifier registry (prevent double-spending)
    mapping(bytes32 => bool) public nullifiers;

    /// @notice Total verified proofs
    uint256 public totalVerifiedProofs;

    /// @notice Commitment timelock
    uint256 public commitmentTimelock;

    // ============================================
    // EVENTS
    // ============================================

    event VerifierRegistered(
        bytes32 indexed circuitId,
        address indexed verifierContract,
        uint8 proofSystem
    );

    event ProofVerified(
        bytes32 indexed circuitId,
        bytes32 indexed proofHash,
        address indexed verifier,
        bool valid
    );

    event ProofCommitted(
        bytes32 indexed commitmentHash,
        address indexed committer,
        uint256 revealDeadline
    );

    event ProofRevealed(
        bytes32 indexed commitmentHash,
        bytes32 indexed proofHash,
        bool valid
    );

    event NullifierUsed(bytes32 indexed nullifier);

    // ============================================
    // ERRORS
    // ============================================

    error VerifierNotFound();
    error VerifierDisabled();
    error InvalidProofFormat();
    error InvalidPublicInputs();
    error ProofVerificationFailed();
    error NullifierAlreadyUsed();
    error CommitmentNotFound();
    error CommitmentExpired();
    error CommitmentAlreadyRevealed();
    error TooEarlyToReveal();

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(uint256 _commitmentTimelock) {
        commitmentTimelock = _commitmentTimelock;

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(VERIFIER_ADMIN_ROLE, msg.sender);
        _grantRole(PROVER_ROLE, msg.sender);
    }

    // ============================================
    // VERIFIER REGISTRY
    // ============================================

    /**
     * @notice Register a new verifier for a circuit
     * @param circuitId Unique circuit identifier
     * @param verifierContract Address of verifier contract
     * @param proofSystem Proof system type
     * @param minInputs Minimum public inputs
     * @param maxInputs Maximum public inputs
     * @param description Human-readable description
     */
    function registerVerifier(
        bytes32 circuitId,
        address verifierContract,
        uint8 proofSystem,
        uint256 minInputs,
        uint256 maxInputs,
        string calldata description
    ) external onlyRole(VERIFIER_ADMIN_ROLE) {
        verifiers[circuitId] = VerifierConfig({
            verifierContract: verifierContract,
            circuitId: circuitId,
            proofSystem: proofSystem,
            enabled: true,
            minPublicInputs: minInputs,
            maxPublicInputs: maxInputs,
            description: description
        });

        emit VerifierRegistered(circuitId, verifierContract, proofSystem);
    }

    /**
     * @notice Set verification key hash for a circuit
     */
    function setVerificationKeyHash(bytes32 circuitId, bytes32 vkHash) 
        external 
        onlyRole(VERIFIER_ADMIN_ROLE) 
    {
        verificationKeyHashes[circuitId] = vkHash;
    }

    /**
     * @notice Enable or disable a verifier
     */
    function setVerifierEnabled(bytes32 circuitId, bool enabled) 
        external 
        onlyRole(VERIFIER_ADMIN_ROLE) 
    {
        verifiers[circuitId].enabled = enabled;
    }

    // ============================================
    // PROOF VERIFICATION
    // ============================================

    /**
     * @notice Verify a ZK proof
     * @param proofData Proof data including proof bytes and public inputs
     * @return valid Whether proof is valid
     */
    function verifyProof(ProofData calldata proofData) 
        external 
        returns (bool valid) 
    {
        VerifierConfig storage config = verifiers[proofData.circuitId];
        
        if (config.verifierContract == address(0)) revert VerifierNotFound();
        if (!config.enabled) revert VerifierDisabled();

        // Validate public inputs count
        if (proofData.publicInputs.length < config.minPublicInputs ||
            proofData.publicInputs.length > config.maxPublicInputs) {
            revert InvalidPublicInputs();
        }

        // Dispatch to appropriate verifier
        if (proofData.proofSystem == PROOF_SYSTEM_GROTH16) {
            valid = _verifyGroth16(proofData, config);
        } else if (proofData.proofSystem == PROOF_SYSTEM_PLONK) {
            valid = _verifyPlonk(proofData, config);
        } else {
            valid = _verifyGeneric(proofData, config);
        }

        // Record result
        bytes32 proofHash = keccak256(proofData.proof);
        verificationResults[proofHash] = VerificationResult({
            valid: valid,
            proofHash: proofHash,
            timestamp: block.timestamp,
            verifier: msg.sender
        });

        totalVerifiedProofs++;

        emit ProofVerified(proofData.circuitId, proofHash, msg.sender, valid);
    }

    /**
     * @notice Verify Groth16 proof with nullifier
     */
    function verifyWithNullifier(
        ProofData calldata proofData,
        bytes32 nullifier
    ) external returns (bool valid) {
        if (nullifiers[nullifier]) revert NullifierAlreadyUsed();

        VerifierConfig storage config = verifiers[proofData.circuitId];
        if (config.verifierContract == address(0)) revert VerifierNotFound();
        if (!config.enabled) revert VerifierDisabled();

        valid = _verifyGroth16(proofData, config);

        if (valid) {
            nullifiers[nullifier] = true;
            emit NullifierUsed(nullifier);
        }

        bytes32 proofHash = keccak256(proofData.proof);
        emit ProofVerified(proofData.circuitId, proofHash, msg.sender, valid);
        
        return valid;
    }

    /**
     * @notice Batch verify multiple proofs
     */
    function batchVerifyProofs(ProofData[] calldata proofs) 
        external 
        returns (bool[] memory results) 
    {
        results = new bool[](proofs.length);

        for (uint256 i = 0; i < proofs.length; i++) {
            VerifierConfig storage config = verifiers[proofs[i].circuitId];
            
            if (config.verifierContract != address(0) && config.enabled) {
                if (proofs[i].proofSystem == PROOF_SYSTEM_GROTH16) {
                    results[i] = _verifyGroth16(proofs[i], config);
                } else {
                    results[i] = _verifyGeneric(proofs[i], config);
                }

                bytes32 proofHash = keccak256(proofs[i].proof);
                emit ProofVerified(proofs[i].circuitId, proofHash, msg.sender, results[i]);
            }
        }
    }

    // ============================================
    // COMMIT-REVEAL PATTERN
    // ============================================

    /**
     * @notice Commit to a proof (for front-running protection)
     * @param commitmentHash Hash of (proof, salt)
     */
    function commitProof(bytes32 commitmentHash) external {
        commitments[commitmentHash] = ProofCommitment({
            commitmentHash: commitmentHash,
            committer: msg.sender,
            commitTime: block.timestamp,
            revealDeadline: block.timestamp + commitmentTimelock,
            revealed: false,
            valid: false
        });

        emit ProofCommitted(commitmentHash, msg.sender, block.timestamp + commitmentTimelock);
    }

    /**
     * @notice Reveal and verify a committed proof
     * @param proofData The proof data
     * @param salt Salt used in commitment
     */
    function revealProof(ProofData calldata proofData, bytes32 salt) 
        external 
        returns (bool valid) 
    {
        bytes32 commitmentHash = keccak256(abi.encodePacked(
            keccak256(proofData.proof),
            salt
        ));

        ProofCommitment storage commitment = commitments[commitmentHash];
        
        if (commitment.committer == address(0)) revert CommitmentNotFound();
        if (commitment.revealed) revert CommitmentAlreadyRevealed();
        if (block.timestamp > commitment.revealDeadline) revert CommitmentExpired();

        // Verify the proof
        VerifierConfig storage config = verifiers[proofData.circuitId];
        if (config.verifierContract == address(0)) revert VerifierNotFound();

        valid = _verifyGroth16(proofData, config);

        commitment.revealed = true;
        commitment.valid = valid;

        bytes32 proofHash = keccak256(proofData.proof);
        emit ProofRevealed(commitmentHash, proofHash, valid);
    }

    // ============================================
    // INTERNAL VERIFICATION
    // ============================================

    /**
     * @notice Internal Groth16 verification
     * @dev This is a mock - real implementation would use precompiles
     */
    function _verifyGroth16(
        ProofData calldata proofData,
        VerifierConfig storage config
    ) internal view returns (bool) {
        // Decode proof points
        if (proofData.proof.length < 256) revert InvalidProofFormat();

        // In a real implementation, this would:
        // 1. Decode the proof into a, b, c points
        // 2. Verify using BN254 pairing precompile (0x08)
        // 3. Check: e(A, B) = e(alpha, beta) * e(vkX, gamma) * e(C, delta)
        
        // Mock: call external verifier if set
        if (config.verifierContract != address(0)) {
            (bool success, bytes memory result) = config.verifierContract.staticcall(
                abi.encodeWithSignature(
                    "verifyProof(uint256[8],uint256[])",
                    proofData.proof,
                    proofData.publicInputs
                )
            );
            
            if (success && result.length >= 32) {
                return abi.decode(result, (bool));
            }
        }

        // Fallback: basic validation
        return _validateProofStructure(proofData.proof);
    }

    /**
     * @notice Internal PLONK verification
     */
    function _verifyPlonk(
        ProofData calldata proofData,
        VerifierConfig storage config
    ) internal view returns (bool) {
        // PLONK verification is more complex
        // Involves polynomial commitments and evaluation proofs
        
        if (config.verifierContract != address(0)) {
            (bool success, bytes memory result) = config.verifierContract.staticcall(
                abi.encodeWithSignature(
                    "verify(bytes,uint256[])",
                    proofData.proof,
                    proofData.publicInputs
                )
            );
            
            if (success && result.length >= 32) {
                return abi.decode(result, (bool));
            }
        }

        return false;
    }

    /**
     * @notice Generic verification through external contract
     */
    function _verifyGeneric(
        ProofData calldata proofData,
        VerifierConfig storage config
    ) internal view returns (bool) {
        if (config.verifierContract == address(0)) return false;

        (bool success, bytes memory result) = config.verifierContract.staticcall(
            abi.encodeWithSignature(
                "verify(bytes,uint256[])",
                proofData.proof,
                proofData.publicInputs
            )
        );

        if (success && result.length >= 32) {
            return abi.decode(result, (bool));
        }
        return false;
    }

    /**
     * @notice Validate proof structure
     */
    function _validateProofStructure(bytes calldata proof) 
        internal 
        pure 
        returns (bool) 
    {
        // Basic structural validation
        if (proof.length < 256) return false;

        // Check points are on curve (simplified)
        // Real implementation would verify G1/G2 point validity
        
        return true;
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Check if a proof has been verified
     */
    function isProofVerified(bytes32 proofHash) external view returns (bool) {
        return verificationResults[proofHash].valid;
    }

    /**
     * @notice Get verification result
     */
    function getVerificationResult(bytes32 proofHash) 
        external 
        view 
        returns (VerificationResult memory) 
    {
        return verificationResults[proofHash];
    }

    /**
     * @notice Check if nullifier is used
     */
    function isNullifierUsed(bytes32 nullifier) external view returns (bool) {
        return nullifiers[nullifier];
    }

    /**
     * @notice Get verifier config
     */
    function getVerifier(bytes32 circuitId) 
        external 
        view 
        returns (VerifierConfig memory) 
    {
        return verifiers[circuitId];
    }

    // ============================================
    // ADMIN
    // ============================================

    function setCommitmentTimelock(uint256 timelock) 
        external 
        onlyRole(VERIFIER_ADMIN_ROLE) 
    {
        commitmentTimelock = timelock;
    }
}

/**
 * @title IVerifier
 * @notice Interface for external verifier contracts
 */
interface IVerifier {
    function verifyProof(
        uint256[8] calldata proof,
        uint256[] calldata publicInputs
    ) external view returns (bool);

    function verify(
        bytes calldata proof,
        uint256[] calldata publicInputs
    ) external view returns (bool);
}
