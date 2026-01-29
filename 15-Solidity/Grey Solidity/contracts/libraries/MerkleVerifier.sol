// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/utils/cryptography/MerkleProof.sol";

/**
 * @title MerkleVerifier
 * @author Grey Protocol Team
 * @notice Comprehensive Merkle tree verification utilities
 * @dev Provides various Merkle proof verification methods and helpers
 * 
 * Features:
 * - Standard Merkle proof verification
 * - Multi-proof verification
 * - Merkle root computation
 * - Leaf encoding helpers
 * - Proof validity checks
 * - Sparse Merkle tree support
 */
library MerkleVerifier {
    // ============================================
    // ERRORS
    // ============================================

    error InvalidProof();
    error InvalidLeaf();
    error InvalidRoot();
    error ProofLengthMismatch();
    error IndexOutOfBounds();

    // ============================================
    // STANDARD VERIFICATION
    // ============================================

    /**
     * @notice Verifies a Merkle proof for a leaf
     * @param root Merkle root
     * @param leaf Leaf to verify
     * @param proof Array of proof hashes
     * @return valid Whether the proof is valid
     */
    function verify(
        bytes32 root,
        bytes32 leaf,
        bytes32[] calldata proof
    ) internal pure returns (bool valid) {
        return MerkleProof.verify(proof, root, leaf);
    }

    /**
     * @notice Verifies a Merkle proof with revert on failure
     * @param root Merkle root
     * @param leaf Leaf to verify
     * @param proof Array of proof hashes
     */
    function verifyStrict(
        bytes32 root,
        bytes32 leaf,
        bytes32[] calldata proof
    ) internal pure {
        if (!MerkleProof.verify(proof, root, leaf)) {
            revert InvalidProof();
        }
    }

    /**
     * @notice Verifies a Merkle proof for an address
     * @param root Merkle root
     * @param account Address to verify
     * @param proof Proof array
     * @return valid Whether account is in the tree
     */
    function verifyAddress(
        bytes32 root,
        address account,
        bytes32[] calldata proof
    ) internal pure returns (bool valid) {
        bytes32 leaf = keccak256(abi.encodePacked(account));
        return MerkleProof.verify(proof, root, leaf);
    }

    /**
     * @notice Verifies a Merkle proof for an address with amount
     * @param root Merkle root
     * @param account Address to verify
     * @param amount Amount associated with address
     * @param proof Proof array
     * @return valid Whether the (address, amount) pair is in the tree
     */
    function verifyAddressAmount(
        bytes32 root,
        address account,
        uint256 amount,
        bytes32[] calldata proof
    ) internal pure returns (bool valid) {
        bytes32 leaf = keccak256(abi.encodePacked(account, amount));
        return MerkleProof.verify(proof, root, leaf);
    }

    /**
     * @notice Verifies a Merkle proof for an address with index and amount
     * @param root Merkle root
     * @param index Leaf index
     * @param account Address to verify
     * @param amount Amount associated with address
     * @param proof Proof array
     * @return valid Whether the leaf is in the tree
     */
    function verifyIndexedLeaf(
        bytes32 root,
        uint256 index,
        address account,
        uint256 amount,
        bytes32[] calldata proof
    ) internal pure returns (bool valid) {
        bytes32 leaf = keccak256(bytes.concat(keccak256(abi.encode(index, account, amount))));
        return MerkleProof.verify(proof, root, leaf);
    }

    // ============================================
    // MULTI-PROOF VERIFICATION
    // ============================================

    /**
     * @notice Verifies multiple proofs at once
     * @param root Merkle root
     * @param leaves Array of leaves
     * @param proofs Array of proof arrays
     * @return valid Whether all proofs are valid
     */
    function verifyMultiple(
        bytes32 root,
        bytes32[] calldata leaves,
        bytes32[][] calldata proofs
    ) internal pure returns (bool valid) {
        if (leaves.length != proofs.length) revert ProofLengthMismatch();
        
        for (uint256 i = 0; i < leaves.length; i++) {
            if (!MerkleProof.verify(proofs[i], root, leaves[i])) {
                return false;
            }
        }
        return true;
    }

    /**
     * @notice Verifies multiple proofs using multi-proof algorithm
     * @param root Merkle root
     * @param leaves Leaves to verify
     * @param proof Combined proof
     * @param proofFlags Flags indicating proof structure
     * @return valid Whether the multi-proof is valid
     */
    function verifyMultiProof(
        bytes32 root,
        bytes32[] calldata leaves,
        bytes32[] calldata proof,
        bool[] calldata proofFlags
    ) internal pure returns (bool valid) {
        return MerkleProof.multiProofVerify(proof, proofFlags, root, leaves);
    }

    // ============================================
    // ROOT COMPUTATION
    // ============================================

    /**
     * @notice Computes the Merkle root from a leaf and proof
     * @param leaf Leaf hash
     * @param proof Proof array
     * @return root Computed Merkle root
     */
    function computeRoot(
        bytes32 leaf,
        bytes32[] calldata proof
    ) internal pure returns (bytes32 root) {
        return MerkleProof.processProof(proof, leaf);
    }

    /**
     * @notice Computes root from multi-proof
     * @param leaves Leaf hashes
     * @param proof Proof array
     * @param proofFlags Flags for proof processing
     * @return root Computed Merkle root
     */
    function computeRootMultiProof(
        bytes32[] calldata leaves,
        bytes32[] calldata proof,
        bool[] calldata proofFlags
    ) internal pure returns (bytes32 root) {
        return MerkleProof.processMultiProof(proof, proofFlags, leaves);
    }

    /**
     * @notice Computes a Merkle root from an array of leaves
     * @param leaves Array of leaf hashes
     * @return root Computed Merkle root
     */
    function computeRootFromLeaves(
        bytes32[] memory leaves
    ) internal pure returns (bytes32 root) {
        uint256 n = leaves.length;
        if (n == 0) return bytes32(0);
        if (n == 1) return leaves[0];

        // Pad to power of 2
        uint256 size = 1;
        while (size < n) size *= 2;

        bytes32[] memory tree = new bytes32[](size);
        for (uint256 i = 0; i < n; i++) {
            tree[i] = leaves[i];
        }
        // Pad with zero hashes
        for (uint256 i = n; i < size; i++) {
            tree[i] = bytes32(0);
        }

        // Build tree bottom-up
        while (size > 1) {
            for (uint256 i = 0; i < size / 2; i++) {
                bytes32 left = tree[i * 2];
                bytes32 right = tree[i * 2 + 1];
                tree[i] = _hashPair(left, right);
            }
            size /= 2;
        }

        return tree[0];
    }

    // ============================================
    // LEAF ENCODING
    // ============================================

    /**
     * @notice Encodes an address into a Merkle leaf
     */
    function encodeLeaf(address account) internal pure returns (bytes32) {
        return keccak256(abi.encodePacked(account));
    }

    /**
     * @notice Encodes an address and amount into a Merkle leaf
     */
    function encodeLeaf(
        address account,
        uint256 amount
    ) internal pure returns (bytes32) {
        return keccak256(abi.encodePacked(account, amount));
    }

    /**
     * @notice Encodes an indexed leaf (for airdrop-style claims)
     */
    function encodeIndexedLeaf(
        uint256 index,
        address account,
        uint256 amount
    ) internal pure returns (bytes32) {
        return keccak256(bytes.concat(keccak256(abi.encode(index, account, amount))));
    }

    /**
     * @notice Encodes arbitrary data into a Merkle leaf
     */
    function encodeDataLeaf(bytes memory data) internal pure returns (bytes32) {
        return keccak256(data);
    }

    /**
     * @notice Double-hashes for OpenZeppelin standard leaf format
     */
    function encodeLeafOZ(
        address account,
        uint256 amount
    ) internal pure returns (bytes32) {
        return keccak256(bytes.concat(keccak256(abi.encode(account, amount))));
    }

    // ============================================
    // SPARSE MERKLE TREE HELPERS
    // ============================================

    /**
     * @notice Verifies a sparse Merkle tree inclusion proof
     * @param root Tree root
     * @param leaf Leaf value
     * @param index Leaf index
     * @param siblings Sibling hashes along the path
     * @return valid Whether the proof is valid
     */
    function verifySparse(
        bytes32 root,
        bytes32 leaf,
        uint256 index,
        bytes32[] calldata siblings
    ) internal pure returns (bool valid) {
        bytes32 computedHash = leaf;
        
        for (uint256 i = 0; i < siblings.length; i++) {
            if ((index >> i) & 1 == 1) {
                // Current node is right child
                computedHash = _hashPair(siblings[i], computedHash);
            } else {
                // Current node is left child
                computedHash = _hashPair(computedHash, siblings[i]);
            }
        }
        
        return computedHash == root;
    }

    /**
     * @notice Verifies a non-membership proof in sparse Merkle tree
     * @param root Tree root
     * @param key Key to prove non-membership
     * @param siblings Sibling hashes
     * @return valid Whether the key is not in the tree
     */
    function verifyNonMembership(
        bytes32 root,
        bytes32 key,
        bytes32[] calldata siblings
    ) internal pure returns (bool valid) {
        // For non-membership, we prove the leaf at the key's position is empty
        return verifySparse(root, bytes32(0), uint256(key), siblings);
    }

    // ============================================
    // INTERNAL HELPERS
    // ============================================

    /**
     * @notice Hashes a pair of nodes in sorted order
     */
    function _hashPair(bytes32 a, bytes32 b) internal pure returns (bytes32) {
        return a < b 
            ? keccak256(abi.encodePacked(a, b)) 
            : keccak256(abi.encodePacked(b, a));
    }
}

/**
 * @title MerkleDistributor
 * @author Grey Protocol Team
 * @notice Merkle tree based token distribution (airdrop) contract
 */
abstract contract MerkleDistributorBase {
    using MerkleVerifier for bytes32;

    /// @notice Merkle root for the distribution
    bytes32 public immutable merkleRoot;

    /// @notice Packed claimed bitmap (256 claims per slot)
    mapping(uint256 => uint256) private _claimedBitmap;

    event Claimed(uint256 indexed index, address indexed account, uint256 amount);

    error AlreadyClaimed(uint256 index);
    error InvalidMerkleProof();

    constructor(bytes32 _merkleRoot) {
        merkleRoot = _merkleRoot;
    }

    /**
     * @notice Checks if an index has been claimed
     */
    function isClaimed(uint256 index) public view returns (bool) {
        uint256 claimedWordIndex = index / 256;
        uint256 claimedBitIndex = index % 256;
        uint256 claimedWord = _claimedBitmap[claimedWordIndex];
        uint256 mask = (1 << claimedBitIndex);
        return claimedWord & mask == mask;
    }

    /**
     * @notice Sets an index as claimed
     */
    function _setClaimed(uint256 index) internal {
        uint256 claimedWordIndex = index / 256;
        uint256 claimedBitIndex = index % 256;
        _claimedBitmap[claimedWordIndex] = _claimedBitmap[claimedWordIndex] | (1 << claimedBitIndex);
    }

    /**
     * @notice Verifies a claim
     */
    function _verifyClaim(
        uint256 index,
        address account,
        uint256 amount,
        bytes32[] calldata merkleProof
    ) internal view returns (bool) {
        bytes32 leaf = MerkleVerifier.encodeIndexedLeaf(index, account, amount);
        return MerkleVerifier.verify(merkleRoot, leaf, merkleProof);
    }
}
