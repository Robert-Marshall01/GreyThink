// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/utils/cryptography/ECDSA.sol";
import "@openzeppelin/contracts/utils/cryptography/MessageHashUtils.sol";

/**
 * @title EIP712Processor
 * @author Grey Protocol
 * @notice Comprehensive EIP-712 typed structured data signing implementation
 * @dev Provides domain separation, type hashing, and signature verification
 * 
 * Features:
 * - Full EIP-712 domain separator support
 * - Nested struct type hashing
 * - Signature verification with nonce management
 * - Permit-style approvals
 * - Meta-transaction support
 * - Batch signature verification
 */
contract EIP712Processor {
    using ECDSA for bytes32;
    using MessageHashUtils for bytes32;

    // ============================================
    // CONSTANTS
    // ============================================

    /// @notice EIP-712 domain type hash
    bytes32 public constant DOMAIN_TYPEHASH = keccak256(
        "EIP712Domain(string name,string version,uint256 chainId,address verifyingContract)"
    );

    /// @notice Permit type hash
    bytes32 public constant PERMIT_TYPEHASH = keccak256(
        "Permit(address owner,address spender,uint256 value,uint256 nonce,uint256 deadline)"
    );

    /// @notice Meta-transaction type hash
    bytes32 public constant META_TRANSACTION_TYPEHASH = keccak256(
        "MetaTransaction(uint256 nonce,address from,bytes functionSignature)"
    );

    /// @notice Delegation type hash
    bytes32 public constant DELEGATION_TYPEHASH = keccak256(
        "Delegation(address delegatee,uint256 nonce,uint256 expiry)"
    );

    /// @notice Order type hash (for trading)
    bytes32 public constant ORDER_TYPEHASH = keccak256(
        "Order(address maker,address taker,address makerToken,address takerToken,uint256 makerAmount,uint256 takerAmount,uint256 nonce,uint256 expiry)"
    );

    /// @notice Attestation type hash
    bytes32 public constant ATTESTATION_TYPEHASH = keccak256(
        "Attestation(address subject,bytes32 schemaId,bytes32 dataHash,uint256 issuedAt,uint256 expiresAt,uint256 nonce)"
    );

    // ============================================
    // STRUCTS
    // ============================================

    /// @notice EIP-712 domain data
    struct DomainData {
        string name;
        string version;
        uint256 chainId;
        address verifyingContract;
    }

    /// @notice Permit data
    struct Permit {
        address owner;
        address spender;
        uint256 value;
        uint256 nonce;
        uint256 deadline;
    }

    /// @notice Meta-transaction data
    struct MetaTransaction {
        uint256 nonce;
        address from;
        bytes functionSignature;
    }

    /// @notice Delegation data
    struct Delegation {
        address delegatee;
        uint256 nonce;
        uint256 expiry;
    }

    /// @notice Trading order data
    struct Order {
        address maker;
        address taker;
        address makerToken;
        address takerToken;
        uint256 makerAmount;
        uint256 takerAmount;
        uint256 nonce;
        uint256 expiry;
    }

    /// @notice Attestation data
    struct Attestation {
        address subject;
        bytes32 schemaId;
        bytes32 dataHash;
        uint256 issuedAt;
        uint256 expiresAt;
        uint256 nonce;
    }

    /// @notice Signature components
    struct Signature {
        uint8 v;
        bytes32 r;
        bytes32 s;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Domain separator
    bytes32 public immutable DOMAIN_SEPARATOR;

    /// @notice Cached chain ID
    uint256 private immutable INITIAL_CHAIN_ID;

    /// @notice Domain name
    string private _domainName;

    /// @notice Domain version
    string private _domainVersion;

    /// @notice User nonces for replay protection
    mapping(address => uint256) public nonces;

    /// @notice Used signatures (prevent replay)
    mapping(bytes32 => bool) public usedSignatures;

    /// @notice Approved operators (for batch operations)
    mapping(address => mapping(address => bool)) public operators;

    // ============================================
    // EVENTS
    // ============================================

    event SignatureVerified(
        address indexed signer,
        bytes32 indexed typeHash,
        bytes32 structHash
    );

    event NonceUsed(address indexed account, uint256 nonce);

    event OperatorSet(address indexed owner, address indexed operator, bool approved);

    // ============================================
    // ERRORS
    // ============================================

    error InvalidSignature();
    error ExpiredDeadline();
    error InvalidNonce();
    error SignatureAlreadyUsed();
    error InvalidChainId();

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(string memory name, string memory version) {
        _domainName = name;
        _domainVersion = version;
        INITIAL_CHAIN_ID = block.chainid;

        DOMAIN_SEPARATOR = _computeDomainSeparator();
    }

    // ============================================
    // DOMAIN SEPARATOR
    // ============================================

    /**
     * @notice Compute domain separator
     */
    function _computeDomainSeparator() internal view returns (bytes32) {
        return keccak256(
            abi.encode(
                DOMAIN_TYPEHASH,
                keccak256(bytes(_domainName)),
                keccak256(bytes(_domainVersion)),
                block.chainid,
                address(this)
            )
        );
    }

    /**
     * @notice Get current domain separator (handles chain ID change)
     */
    function domainSeparator() public view returns (bytes32) {
        if (block.chainid == INITIAL_CHAIN_ID) {
            return DOMAIN_SEPARATOR;
        }
        return _computeDomainSeparator();
    }

    /**
     * @notice Get EIP-712 domain
     */
    function eip712Domain() external view returns (
        bytes1 fields,
        string memory name,
        string memory version,
        uint256 chainId,
        address verifyingContract,
        bytes32 salt,
        uint256[] memory extensions
    ) {
        return (
            hex"0f", // 01111 - name, version, chainId, verifyingContract (no salt)
            _domainName,
            _domainVersion,
            block.chainid,
            address(this),
            bytes32(0),
            new uint256[](0)
        );
    }

    // ============================================
    // HASH FUNCTIONS
    // ============================================

    /**
     * @notice Hash a permit struct
     */
    function hashPermit(Permit memory permit) public pure returns (bytes32) {
        return keccak256(abi.encode(
            PERMIT_TYPEHASH,
            permit.owner,
            permit.spender,
            permit.value,
            permit.nonce,
            permit.deadline
        ));
    }

    /**
     * @notice Hash a meta-transaction
     */
    function hashMetaTransaction(MetaTransaction memory metaTx) 
        public 
        pure 
        returns (bytes32) 
    {
        return keccak256(abi.encode(
            META_TRANSACTION_TYPEHASH,
            metaTx.nonce,
            metaTx.from,
            keccak256(metaTx.functionSignature)
        ));
    }

    /**
     * @notice Hash a delegation
     */
    function hashDelegation(Delegation memory delegation) 
        public 
        pure 
        returns (bytes32) 
    {
        return keccak256(abi.encode(
            DELEGATION_TYPEHASH,
            delegation.delegatee,
            delegation.nonce,
            delegation.expiry
        ));
    }

    /**
     * @notice Hash an order
     */
    function hashOrder(Order memory order) public pure returns (bytes32) {
        return keccak256(abi.encode(
            ORDER_TYPEHASH,
            order.maker,
            order.taker,
            order.makerToken,
            order.takerToken,
            order.makerAmount,
            order.takerAmount,
            order.nonce,
            order.expiry
        ));
    }

    /**
     * @notice Hash an attestation
     */
    function hashAttestation(Attestation memory attestation) 
        public 
        pure 
        returns (bytes32) 
    {
        return keccak256(abi.encode(
            ATTESTATION_TYPEHASH,
            attestation.subject,
            attestation.schemaId,
            attestation.dataHash,
            attestation.issuedAt,
            attestation.expiresAt,
            attestation.nonce
        ));
    }

    /**
     * @notice Create typed data hash (combines domain separator and struct hash)
     */
    function toTypedDataHash(bytes32 structHash) public view returns (bytes32) {
        return keccak256(
            abi.encodePacked("\x19\x01", domainSeparator(), structHash)
        );
    }

    // ============================================
    // SIGNATURE VERIFICATION
    // ============================================

    /**
     * @notice Verify a permit signature
     */
    function verifyPermit(
        Permit memory permit,
        Signature memory sig
    ) public view returns (address signer) {
        if (block.timestamp > permit.deadline) revert ExpiredDeadline();

        bytes32 structHash = hashPermit(permit);
        bytes32 typedDataHash = toTypedDataHash(structHash);

        signer = ecrecover(typedDataHash, sig.v, sig.r, sig.s);
        if (signer != permit.owner || signer == address(0)) {
            revert InvalidSignature();
        }
    }

    /**
     * @notice Verify a meta-transaction signature
     */
    function verifyMetaTransaction(
        MetaTransaction memory metaTx,
        bytes memory signature
    ) public returns (address signer) {
        bytes32 structHash = hashMetaTransaction(metaTx);
        bytes32 typedDataHash = toTypedDataHash(structHash);

        signer = typedDataHash.recover(signature);
        
        if (signer != metaTx.from) revert InvalidSignature();
        if (nonces[signer] != metaTx.nonce) revert InvalidNonce();

        // Mark signature as used
        bytes32 sigHash = keccak256(signature);
        if (usedSignatures[sigHash]) revert SignatureAlreadyUsed();
        usedSignatures[sigHash] = true;

        // Increment nonce
        nonces[signer]++;
        emit NonceUsed(signer, metaTx.nonce);

        emit SignatureVerified(signer, META_TRANSACTION_TYPEHASH, structHash);
    }

    /**
     * @notice Verify an order signature
     */
    function verifyOrder(
        Order memory order,
        bytes memory signature
    ) public view returns (bool valid, address signer) {
        if (block.timestamp > order.expiry) return (false, address(0));

        bytes32 structHash = hashOrder(order);
        bytes32 typedDataHash = toTypedDataHash(structHash);

        signer = typedDataHash.recover(signature);
        valid = (signer == order.maker && signer != address(0));
    }

    /**
     * @notice Verify a delegation signature
     */
    function verifyDelegation(
        address delegator,
        Delegation memory delegation,
        bytes memory signature
    ) public view returns (bool valid) {
        if (block.timestamp > delegation.expiry) return false;

        bytes32 structHash = hashDelegation(delegation);
        bytes32 typedDataHash = toTypedDataHash(structHash);

        address signer = typedDataHash.recover(signature);
        return (signer == delegator && signer != address(0));
    }

    /**
     * @notice Batch verify multiple signatures
     */
    function batchVerify(
        bytes32[] calldata structHashes,
        address[] calldata signers,
        bytes[] calldata signatures
    ) external view returns (bool[] memory results) {
        require(
            structHashes.length == signers.length && 
            signers.length == signatures.length,
            "Length mismatch"
        );

        results = new bool[](structHashes.length);

        for (uint256 i = 0; i < structHashes.length; i++) {
            bytes32 typedDataHash = toTypedDataHash(structHashes[i]);
            address recovered = typedDataHash.recover(signatures[i]);
            results[i] = (recovered == signers[i] && recovered != address(0));
        }
    }

    // ============================================
    // NONCE MANAGEMENT
    // ============================================

    /**
     * @notice Get current nonce for an address
     */
    function getNonce(address account) external view returns (uint256) {
        return nonces[account];
    }

    /**
     * @notice Invalidate all signatures with nonce < provided value
     */
    function invalidateNonces(uint256 newNonce) external {
        require(newNonce > nonces[msg.sender], "Nonce too low");
        nonces[msg.sender] = newNonce;
        emit NonceUsed(msg.sender, newNonce);
    }

    // ============================================
    // OPERATOR MANAGEMENT
    // ============================================

    /**
     * @notice Set operator approval for batch operations
     */
    function setOperator(address operator, bool approved) external {
        operators[msg.sender][operator] = approved;
        emit OperatorSet(msg.sender, operator, approved);
    }

    /**
     * @notice Check if operator is approved
     */
    function isOperator(address owner, address operator) external view returns (bool) {
        return operators[owner][operator];
    }

    // ============================================
    // UTILITY FUNCTIONS
    // ============================================

    /**
     * @notice Split signature into components
     */
    function splitSignature(bytes memory signature) 
        public 
        pure 
        returns (Signature memory sig) 
    {
        require(signature.length == 65, "Invalid signature length");

        assembly {
            sig := mload(add(signature, 32))
            mstore(add(sig, 32), mload(add(signature, 64)))
            mstore(add(sig, 64), byte(0, mload(add(signature, 96))))
        }
    }

    /**
     * @notice Join signature components
     */
    function joinSignature(Signature memory sig) 
        public 
        pure 
        returns (bytes memory) 
    {
        return abi.encodePacked(sig.r, sig.s, sig.v);
    }

    /**
     * @notice Recover signer from typed data hash and signature
     */
    function recoverSigner(
        bytes32 structHash,
        bytes memory signature
    ) external view returns (address) {
        bytes32 typedDataHash = toTypedDataHash(structHash);
        return typedDataHash.recover(signature);
    }
}
