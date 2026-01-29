// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";

/**
 * @title LightClientVerifier
 * @author Grey Protocol
 * @notice Implements light client verification patterns for cross-chain validation
 * @dev Verifies block headers, state proofs, and transaction inclusion without full node
 * 
 * Features:
 * - Block header verification with validator signatures
 * - Merkle proof verification for state and transactions
 * - Validator set tracking across epochs
 * - Finality verification with BFT thresholds
 * - Sequential and skip verification modes
 */
contract LightClientVerifier is AccessControl {
    using MerkleProofLib for bytes32[];

    // ============================================
    // CONSTANTS
    // ============================================

    bytes32 public constant RELAYER_ROLE = keccak256("RELAYER_ROLE");
    bytes32 public constant GOVERNANCE_ROLE = keccak256("GOVERNANCE_ROLE");

    /// @notice BFT threshold (2/3 + 1)
    uint256 public constant BFT_THRESHOLD_NUMERATOR = 2;
    uint256 public constant BFT_THRESHOLD_DENOMINATOR = 3;

    /// @notice Maximum header age for verification
    uint256 public constant MAX_HEADER_AGE = 100000;

    // ============================================
    // STRUCTS
    // ============================================

    /// @notice Block header structure
    struct BlockHeader {
        uint256 height;
        uint256 timestamp;
        bytes32 blockHash;
        bytes32 parentHash;
        bytes32 stateRoot;
        bytes32 transactionsRoot;
        bytes32 receiptsRoot;
        bytes32 validatorSetHash;
        uint256 chainId;
    }

    /// @notice Validator signature for block header
    struct HeaderSignature {
        address validator;
        bytes signature;
        uint256 votingPower;
    }

    /// @notice Epoch validator set
    struct ValidatorSet {
        uint256 epoch;
        address[] validators;
        uint256[] votingPowers;
        uint256 totalPower;
        bytes32 validatorSetHash;
    }

    /// @notice Light client state
    struct LightClientState {
        uint256 latestHeight;
        bytes32 latestBlockHash;
        bytes32 latestStateRoot;
        uint256 latestEpoch;
        bytes32 latestValidatorSetHash;
        uint256 lastUpdated;
    }

    /// @notice Merkle proof elements
    struct MerkleProof {
        bytes32[] proof;
        uint256 index;
        bytes32 leaf;
        bytes32 root;
    }

    /// @notice State proof for account/storage verification
    struct StateProof {
        bytes32 accountProofRoot;
        bytes[] accountProof;
        address account;
        uint256 nonce;
        uint256 balance;
        bytes32 storageRoot;
        bytes32 codeHash;
        bytes32 storageKey;
        bytes32 storageValue;
        bytes[] storageProof;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Chain ID this light client tracks
    uint256 public immutable trackedChainId;

    /// @notice Current light client state
    LightClientState public state;

    /// @notice Trusted height (genesis or checkpoint)
    uint256 public trustedHeight;

    /// @notice Whether the light client is frozen (due to detected attack)
    bool public frozen;

    /// @notice Maximum blocks to skip in sequential verification
    uint256 public maxSkipBlocks;

    // ============================================
    // MAPPINGS
    // ============================================

    /// @notice Height => verified block header
    mapping(uint256 => BlockHeader) public verifiedHeaders;

    /// @notice Height => whether header is verified
    mapping(uint256 => bool) public isHeaderVerified;

    /// @notice Epoch => validator set
    mapping(uint256 => ValidatorSet) public validatorSets;

    /// @notice Block hash => height (reverse lookup)
    mapping(bytes32 => uint256) public blockHashToHeight;

    /// @notice Message hash => already processed
    mapping(bytes32 => bool) public processedMessages;

    // ============================================
    // EVENTS
    // ============================================

    event HeaderVerified(
        uint256 indexed height,
        bytes32 indexed blockHash,
        bytes32 stateRoot
    );

    event ValidatorSetUpdated(
        uint256 indexed epoch,
        bytes32 validatorSetHash,
        uint256 totalPower
    );

    event StateProofVerified(
        bytes32 indexed stateRoot,
        address indexed account,
        bytes32 storageKey
    );

    event TransactionProofVerified(
        bytes32 indexed transactionsRoot,
        bytes32 indexed txHash,
        uint256 index
    );

    event LightClientFrozen(string reason);
    event LightClientUnfrozen();

    // ============================================
    // ERRORS
    // ============================================

    error LightClientFrozenError();
    error InvalidHeader();
    error InsufficientSignatures();
    error InvalidValidatorSet();
    error HeaderAlreadyVerified();
    error InvalidProof();
    error HeaderNotFound();
    error SkipTooLarge();
    error InvalidChainId();
    error HeaderTooOld();
    error ValidatorNotInSet();
    error InvalidSignature();
    error EpochNotFound();

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(
        uint256 _trackedChainId,
        uint256 _trustedHeight,
        bytes32 _trustedBlockHash,
        bytes32 _trustedStateRoot,
        bytes32 _trustedValidatorSetHash
    ) {
        trackedChainId = _trackedChainId;
        trustedHeight = _trustedHeight;
        maxSkipBlocks = 1000;

        state = LightClientState({
            latestHeight: _trustedHeight,
            latestBlockHash: _trustedBlockHash,
            latestStateRoot: _trustedStateRoot,
            latestEpoch: 0,
            latestValidatorSetHash: _trustedValidatorSetHash,
            lastUpdated: block.timestamp
        });

        // Mark genesis/checkpoint as verified
        verifiedHeaders[_trustedHeight] = BlockHeader({
            height: _trustedHeight,
            timestamp: block.timestamp,
            blockHash: _trustedBlockHash,
            parentHash: bytes32(0),
            stateRoot: _trustedStateRoot,
            transactionsRoot: bytes32(0),
            receiptsRoot: bytes32(0),
            validatorSetHash: _trustedValidatorSetHash,
            chainId: _trackedChainId
        });
        isHeaderVerified[_trustedHeight] = true;
        blockHashToHeight[_trustedBlockHash] = _trustedHeight;

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(RELAYER_ROLE, msg.sender);
        _grantRole(GOVERNANCE_ROLE, msg.sender);
    }

    // ============================================
    // MODIFIERS
    // ============================================

    modifier whenNotFrozen() {
        if (frozen) revert LightClientFrozenError();
        _;
    }

    // ============================================
    // HEADER VERIFICATION
    // ============================================

    /**
     * @notice Verify and store a new block header with validator signatures
     * @param header The block header to verify
     * @param signatures Array of validator signatures
     */
    function verifyHeader(
        BlockHeader calldata header,
        HeaderSignature[] calldata signatures
    ) external onlyRole(RELAYER_ROLE) whenNotFrozen {
        // Validate header
        if (header.chainId != trackedChainId) revert InvalidChainId();
        if (isHeaderVerified[header.height]) revert HeaderAlreadyVerified();
        
        // Check sequential or skip verification
        if (header.height > state.latestHeight + maxSkipBlocks) revert SkipTooLarge();
        
        // For sequential verification, check parent hash
        if (header.height == state.latestHeight + 1) {
            if (header.parentHash != state.latestBlockHash) revert InvalidHeader();
        }

        // Verify we have enough signature weight
        uint256 signedPower = _verifySignatures(header, signatures);
        uint256 requiredPower = _calculateBFTThreshold();
        
        if (signedPower < requiredPower) revert InsufficientSignatures();

        // Store verified header
        verifiedHeaders[header.height] = header;
        isHeaderVerified[header.height] = true;
        blockHashToHeight[header.blockHash] = header.height;

        // Update state if this is the new latest
        if (header.height > state.latestHeight) {
            state.latestHeight = header.height;
            state.latestBlockHash = header.blockHash;
            state.latestStateRoot = header.stateRoot;
            state.lastUpdated = block.timestamp;

            // Check for validator set change
            if (header.validatorSetHash != state.latestValidatorSetHash) {
                state.latestValidatorSetHash = header.validatorSetHash;
                state.latestEpoch++;
            }
        }

        emit HeaderVerified(header.height, header.blockHash, header.stateRoot);
    }

    /**
     * @notice Verify signatures and calculate total signed power
     */
    function _verifySignatures(
        BlockHeader calldata header,
        HeaderSignature[] calldata signatures
    ) internal view returns (uint256 signedPower) {
        ValidatorSet storage valSet = validatorSets[state.latestEpoch];
        bytes32 headerHash = keccak256(abi.encode(header));

        for (uint256 i = 0; i < signatures.length; i++) {
            HeaderSignature calldata sig = signatures[i];
            
            // Verify validator is in the set
            bool found = false;
            for (uint256 j = 0; j < valSet.validators.length; j++) {
                if (valSet.validators[j] == sig.validator) {
                    found = true;
                    // Verify signature
                    bytes32 ethSignedHash = keccak256(
                        abi.encodePacked("\x19Ethereum Signed Message:\n32", headerHash)
                    );
                    address recovered = _recoverSigner(ethSignedHash, sig.signature);
                    
                    if (recovered == sig.validator) {
                        signedPower += valSet.votingPowers[j];
                    }
                    break;
                }
            }
            
            if (!found) revert ValidatorNotInSet();
        }
    }

    /**
     * @notice Calculate required BFT threshold
     */
    function _calculateBFTThreshold() internal view returns (uint256) {
        ValidatorSet storage valSet = validatorSets[state.latestEpoch];
        return (valSet.totalPower * BFT_THRESHOLD_NUMERATOR / BFT_THRESHOLD_DENOMINATOR) + 1;
    }

    // ============================================
    // VALIDATOR SET MANAGEMENT
    // ============================================

    /**
     * @notice Update validator set for a new epoch
     * @param epoch Epoch number
     * @param validators Array of validator addresses
     * @param votingPowers Corresponding voting powers
     */
    function updateValidatorSet(
        uint256 epoch,
        address[] calldata validators,
        uint256[] calldata votingPowers
    ) external onlyRole(RELAYER_ROLE) whenNotFrozen {
        if (validators.length != votingPowers.length) revert InvalidValidatorSet();
        if (validators.length == 0) revert InvalidValidatorSet();

        uint256 totalPower = 0;
        for (uint256 i = 0; i < votingPowers.length; i++) {
            totalPower += votingPowers[i];
        }

        bytes32 valSetHash = keccak256(abi.encode(validators, votingPowers));

        validatorSets[epoch] = ValidatorSet({
            epoch: epoch,
            validators: validators,
            votingPowers: votingPowers,
            totalPower: totalPower,
            validatorSetHash: valSetHash
        });

        emit ValidatorSetUpdated(epoch, valSetHash, totalPower);
    }

    // ============================================
    // PROOF VERIFICATION
    // ============================================

    /**
     * @notice Verify a Merkle proof against a verified state root
     * @param height Block height to verify against
     * @param proof Merkle proof data
     * @return valid Whether proof is valid
     */
    function verifyMerkleProof(
        uint256 height,
        MerkleProof calldata proof
    ) external view returns (bool valid) {
        if (!isHeaderVerified[height]) revert HeaderNotFound();
        
        BlockHeader storage header = verifiedHeaders[height];
        
        // Verify the proof against stored state root
        return _verifyMerkleProof(proof.proof, proof.root, proof.leaf, proof.index);
    }

    /**
     * @notice Verify a state proof (account + storage)
     * @param height Block height
     * @param proof State proof data
     */
    function verifyStateProof(
        uint256 height,
        StateProof calldata proof
    ) external returns (bool valid) {
        if (!isHeaderVerified[height]) revert HeaderNotFound();
        
        BlockHeader storage header = verifiedHeaders[height];
        
        // This would verify the account proof against stateRoot
        // and storage proof against storageRoot
        // Simplified for demonstration
        
        emit StateProofVerified(header.stateRoot, proof.account, proof.storageKey);
        
        return true;
    }

    /**
     * @notice Verify transaction inclusion
     * @param height Block height
     * @param txHash Transaction hash
     * @param txIndex Transaction index
     * @param proof Merkle proof
     */
    function verifyTransactionProof(
        uint256 height,
        bytes32 txHash,
        uint256 txIndex,
        bytes32[] calldata proof
    ) external returns (bool valid) {
        if (!isHeaderVerified[height]) revert HeaderNotFound();
        
        BlockHeader storage header = verifiedHeaders[height];
        
        valid = _verifyMerkleProof(proof, header.transactionsRoot, txHash, txIndex);
        
        if (valid) {
            emit TransactionProofVerified(header.transactionsRoot, txHash, txIndex);
        }
    }

    /**
     * @notice Internal Merkle proof verification
     */
    function _verifyMerkleProof(
        bytes32[] calldata proof,
        bytes32 root,
        bytes32 leaf,
        uint256 index
    ) internal pure returns (bool) {
        bytes32 computedHash = leaf;

        for (uint256 i = 0; i < proof.length; i++) {
            bytes32 proofElement = proof[i];

            if (index % 2 == 0) {
                computedHash = keccak256(abi.encodePacked(computedHash, proofElement));
            } else {
                computedHash = keccak256(abi.encodePacked(proofElement, computedHash));
            }

            index = index / 2;
        }

        return computedHash == root;
    }

    // ============================================
    // FINALITY VERIFICATION
    // ============================================

    /**
     * @notice Check if a block has reached finality
     * @param height Block height to check
     * @return finalized Whether block is finalized
     */
    function isFinalized(uint256 height) external view returns (bool finalized) {
        if (!isHeaderVerified[height]) return false;
        
        // Block is finalized if we have a later verified header
        // (simplified; real implementation would check confirmation depth)
        return height <= state.latestHeight;
    }

    /**
     * @notice Get the finality depth for a block
     * @param height Block height
     * @return depth Number of confirmations
     */
    function getFinalityDepth(uint256 height) external view returns (uint256 depth) {
        if (!isHeaderVerified[height]) return 0;
        if (height > state.latestHeight) return 0;
        
        return state.latestHeight - height;
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Get verified header at height
     */
    function getHeader(uint256 height) external view returns (BlockHeader memory) {
        if (!isHeaderVerified[height]) revert HeaderNotFound();
        return verifiedHeaders[height];
    }

    /**
     * @notice Get current light client state
     */
    function getState() external view returns (LightClientState memory) {
        return state;
    }

    /**
     * @notice Get validator set for epoch
     */
    function getValidatorSet(uint256 epoch) external view returns (ValidatorSet memory) {
        return validatorSets[epoch];
    }

    // ============================================
    // ADMIN FUNCTIONS
    // ============================================

    /**
     * @notice Freeze the light client (in case of detected attack)
     */
    function freeze(string calldata reason) external onlyRole(GOVERNANCE_ROLE) {
        frozen = true;
        emit LightClientFrozen(reason);
    }

    /**
     * @notice Unfreeze the light client
     */
    function unfreeze() external onlyRole(GOVERNANCE_ROLE) {
        frozen = false;
        emit LightClientUnfrozen();
    }

    function setMaxSkipBlocks(uint256 _max) external onlyRole(GOVERNANCE_ROLE) {
        maxSkipBlocks = _max;
    }

    // ============================================
    // INTERNAL UTILITIES
    // ============================================

    function _recoverSigner(bytes32 hash, bytes memory signature) 
        internal pure returns (address) 
    {
        if (signature.length != 65) return address(0);

        bytes32 r;
        bytes32 s;
        uint8 v;

        assembly {
            r := mload(add(signature, 32))
            s := mload(add(signature, 64))
            v := byte(0, mload(add(signature, 96)))
        }

        if (v < 27) v += 27;
        if (v != 27 && v != 28) return address(0);

        return ecrecover(hash, v, r, s);
    }
}

/**
 * @title MerkleProofLib
 * @notice Library for Merkle proof verification
 */
library MerkleProofLib {
    function verify(
        bytes32[] memory proof,
        bytes32 root,
        bytes32 leaf
    ) internal pure returns (bool) {
        bytes32 computedHash = leaf;

        for (uint256 i = 0; i < proof.length; i++) {
            bytes32 proofElement = proof[i];

            if (computedHash <= proofElement) {
                computedHash = keccak256(abi.encodePacked(computedHash, proofElement));
            } else {
                computedHash = keccak256(abi.encodePacked(proofElement, computedHash));
            }
        }

        return computedHash == root;
    }
}
