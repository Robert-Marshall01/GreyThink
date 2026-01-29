// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/utils/cryptography/ECDSA.sol";
import "@openzeppelin/contracts/utils/cryptography/MessageHashUtils.sol";
import "@openzeppelin/contracts/utils/cryptography/SignatureChecker.sol";

/**
 * @title SignatureVerifier
 * @author Grey Protocol Team
 * @notice Comprehensive signature verification utilities
 * @dev Supports ECDSA, EIP-712, and smart contract signatures
 * 
 * Features:
 * - ECDSA signature verification
 * - EIP-712 typed data signing
 * - EIP-1271 smart contract signatures
 * - Multi-signature verification
 * - Signature replay protection
 * - Signature deadline validation
 * - Recovery and verification utilities
 */
library SignatureVerifier {
    using ECDSA for bytes32;
    using MessageHashUtils for bytes32;

    // ============================================
    // CONSTANTS
    // ============================================

    /// @notice EIP-712 typehash for permits
    bytes32 public constant PERMIT_TYPEHASH = keccak256(
        "Permit(address owner,address spender,uint256 value,uint256 nonce,uint256 deadline)"
    );

    /// @notice EIP-712 typehash for delegation
    bytes32 public constant DELEGATION_TYPEHASH = keccak256(
        "Delegation(address delegatee,uint256 nonce,uint256 expiry)"
    );

    /// @notice EIP-712 typehash for generic messages
    bytes32 public constant MESSAGE_TYPEHASH = keccak256(
        "Message(address sender,bytes32 contentHash,uint256 nonce,uint256 deadline)"
    );

    /// @notice EIP-712 typehash for batch operations
    bytes32 public constant BATCH_TYPEHASH = keccak256(
        "Batch(bytes32[] operationHashes,uint256 nonce,uint256 deadline)"
    );

    // ============================================
    // ERRORS
    // ============================================

    error InvalidSignature();
    error SignatureExpired(uint256 deadline);
    error InvalidSignatureLength(uint256 length);
    error InvalidSignatureS();
    error InvalidSignatureV();
    error SignerMismatch(address expected, address recovered);
    error InsufficientSignatures(uint256 provided, uint256 required);
    error DuplicateSigner(address signer);
    error ZeroAddress();

    // ============================================
    // STRUCTS
    // ============================================

    /**
     * @notice Signature components
     */
    struct SignatureData {
        uint8 v;
        bytes32 r;
        bytes32 s;
    }

    /**
     * @notice Multi-signature verification result
     */
    struct MultiSigResult {
        bool isValid;
        uint256 validCount;
        address[] signers;
    }

    // ============================================
    // ECDSA VERIFICATION
    // ============================================

    /**
     * @notice Recovers signer from a message hash and signature
     * @param hash Message hash
     * @param signature Signature bytes
     * @return signer Recovered signer address
     */
    function recover(
        bytes32 hash,
        bytes memory signature
    ) internal pure returns (address signer) {
        return hash.recover(signature);
    }

    /**
     * @notice Recovers signer from an Ethereum signed message
     * @param hash Original message hash
     * @param signature Signature bytes
     * @return signer Recovered signer address
     */
    function recoverEthSigned(
        bytes32 hash,
        bytes memory signature
    ) internal pure returns (address signer) {
        return hash.toEthSignedMessageHash().recover(signature);
    }

    /**
     * @notice Recovers signer from signature components
     * @param hash Message hash
     * @param v Recovery byte
     * @param r R component
     * @param s S component
     * @return signer Recovered signer address
     */
    function recover(
        bytes32 hash,
        uint8 v,
        bytes32 r,
        bytes32 s
    ) internal pure returns (address signer) {
        return hash.recover(v, r, s);
    }

    /**
     * @notice Verifies a signature is from expected signer
     * @param hash Message hash
     * @param signature Signature bytes
     * @param expectedSigner Expected signer address
     * @return valid Whether signature is valid
     */
    function verify(
        bytes32 hash,
        bytes memory signature,
        address expectedSigner
    ) internal pure returns (bool valid) {
        if (expectedSigner == address(0)) return false;
        address recovered = hash.recover(signature);
        return recovered == expectedSigner;
    }

    /**
     * @notice Verifies an Ethereum signed message
     */
    function verifyEthSigned(
        bytes32 hash,
        bytes memory signature,
        address expectedSigner
    ) internal pure returns (bool valid) {
        if (expectedSigner == address(0)) return false;
        address recovered = hash.toEthSignedMessageHash().recover(signature);
        return recovered == expectedSigner;
    }

    /**
     * @notice Strict verification with revert on failure
     */
    function verifyStrict(
        bytes32 hash,
        bytes memory signature,
        address expectedSigner
    ) internal pure {
        if (expectedSigner == address(0)) revert ZeroAddress();
        address recovered = hash.recover(signature);
        if (recovered != expectedSigner) {
            revert SignerMismatch(expectedSigner, recovered);
        }
    }

    // ============================================
    // EIP-712 TYPED DATA
    // ============================================

    /**
     * @notice Computes EIP-712 domain separator
     * @param name Contract name
     * @param version Contract version
     * @param chainId Chain ID
     * @param verifyingContract Contract address
     * @return separator Domain separator hash
     */
    function computeDomainSeparator(
        string memory name,
        string memory version,
        uint256 chainId,
        address verifyingContract
    ) internal pure returns (bytes32 separator) {
        return keccak256(
            abi.encode(
                keccak256("EIP712Domain(string name,string version,uint256 chainId,address verifyingContract)"),
                keccak256(bytes(name)),
                keccak256(bytes(version)),
                chainId,
                verifyingContract
            )
        );
    }

    /**
     * @notice Computes EIP-712 typed data hash
     * @param domainSeparator Domain separator
     * @param structHash Struct hash
     * @return digest Final digest to sign
     */
    function computeTypedDataHash(
        bytes32 domainSeparator,
        bytes32 structHash
    ) internal pure returns (bytes32 digest) {
        return MessageHashUtils.toTypedDataHash(domainSeparator, structHash);
    }

    /**
     * @notice Verifies an EIP-712 typed data signature
     */
    function verifyTypedData(
        bytes32 domainSeparator,
        bytes32 structHash,
        bytes memory signature,
        address expectedSigner
    ) internal pure returns (bool valid) {
        bytes32 digest = computeTypedDataHash(domainSeparator, structHash);
        return verify(digest, signature, expectedSigner);
    }

    /**
     * @notice Computes permit struct hash
     */
    function computePermitHash(
        address owner,
        address spender,
        uint256 value,
        uint256 nonce,
        uint256 deadline
    ) internal pure returns (bytes32) {
        return keccak256(abi.encode(
            PERMIT_TYPEHASH,
            owner,
            spender,
            value,
            nonce,
            deadline
        ));
    }

    /**
     * @notice Computes delegation struct hash
     */
    function computeDelegationHash(
        address delegatee,
        uint256 nonce,
        uint256 expiry
    ) internal pure returns (bytes32) {
        return keccak256(abi.encode(
            DELEGATION_TYPEHASH,
            delegatee,
            nonce,
            expiry
        ));
    }

    // ============================================
    // EIP-1271 CONTRACT SIGNATURES
    // ============================================

    /**
     * @notice Checks if signature is valid for signer (EOA or contract)
     * @param signer Signer address
     * @param hash Message hash
     * @param signature Signature bytes
     * @return valid Whether signature is valid
     */
    function isValidSignatureNow(
        address signer,
        bytes32 hash,
        bytes memory signature
    ) internal view returns (bool valid) {
        return SignatureChecker.isValidSignatureNow(signer, hash, signature);
    }

    /**
     * @notice Checks EIP-1271 contract signature
     */
    function isValidERC1271Signature(
        address signer,
        bytes32 hash,
        bytes memory signature
    ) internal view returns (bool valid) {
        return SignatureChecker.isValidERC1271SignatureNow(signer, hash, signature);
    }

    // ============================================
    // MULTI-SIGNATURE VERIFICATION
    // ============================================

    /**
     * @notice Verifies multiple signatures
     * @param hash Message hash
     * @param signatures Array of signatures
     * @param requiredSigners Required number of valid signatures
     * @return result Multi-signature result
     */
    function verifyMultiple(
        bytes32 hash,
        bytes[] calldata signatures,
        uint256 requiredSigners
    ) internal pure returns (MultiSigResult memory result) {
        result.signers = new address[](signatures.length);
        
        for (uint256 i = 0; i < signatures.length; i++) {
            address recovered = hash.recover(signatures[i]);
            
            // Check for duplicates
            for (uint256 j = 0; j < result.validCount; j++) {
                if (result.signers[j] == recovered) {
                    revert DuplicateSigner(recovered);
                }
            }
            
            if (recovered != address(0)) {
                result.signers[result.validCount] = recovered;
                result.validCount++;
            }
        }
        
        result.isValid = result.validCount >= requiredSigners;
    }

    /**
     * @notice Verifies signatures from expected signers
     * @param hash Message hash
     * @param signatures Array of signatures
     * @param expectedSigners Array of expected signer addresses
     * @param threshold Minimum required valid signatures
     * @return valid Whether threshold is met
     */
    function verifyThreshold(
        bytes32 hash,
        bytes[] calldata signatures,
        address[] calldata expectedSigners,
        uint256 threshold
    ) internal pure returns (bool valid) {
        if (signatures.length < threshold) {
            revert InsufficientSignatures(signatures.length, threshold);
        }

        uint256 validCount = 0;
        bool[] memory used = new bool[](expectedSigners.length);

        for (uint256 i = 0; i < signatures.length && validCount < threshold; i++) {
            address recovered = hash.recover(signatures[i]);
            
            for (uint256 j = 0; j < expectedSigners.length; j++) {
                if (!used[j] && expectedSigners[j] == recovered) {
                    used[j] = true;
                    validCount++;
                    break;
                }
            }
        }

        return validCount >= threshold;
    }

    // ============================================
    // SIGNATURE VALIDATION HELPERS
    // ============================================

    /**
     * @notice Validates signature with deadline
     */
    function verifyWithDeadline(
        bytes32 hash,
        bytes memory signature,
        address expectedSigner,
        uint256 deadline
    ) internal view returns (bool valid) {
        if (block.timestamp > deadline) {
            revert SignatureExpired(deadline);
        }
        return verify(hash, signature, expectedSigner);
    }

    /**
     * @notice Splits a signature into components
     * @param signature Signature bytes
     * @return v Recovery byte
     * @return r R component
     * @return s S component
     */
    function splitSignature(
        bytes memory signature
    ) internal pure returns (uint8 v, bytes32 r, bytes32 s) {
        if (signature.length != 65) {
            revert InvalidSignatureLength(signature.length);
        }

        assembly {
            r := mload(add(signature, 0x20))
            s := mload(add(signature, 0x40))
            v := byte(0, mload(add(signature, 0x60)))
        }

        // Version of signature should be 27 or 28
        if (v < 27) {
            v += 27;
        }

        if (v != 27 && v != 28) {
            revert InvalidSignatureV();
        }
    }

    /**
     * @notice Combines signature components into bytes
     */
    function combineSignature(
        uint8 v,
        bytes32 r,
        bytes32 s
    ) internal pure returns (bytes memory signature) {
        signature = new bytes(65);
        assembly {
            mstore(add(signature, 0x20), r)
            mstore(add(signature, 0x40), s)
            mstore8(add(signature, 0x60), v)
        }
    }

    /**
     * @notice Validates signature S value (EIP-2)
     */
    function validateS(bytes32 s) internal pure returns (bool) {
        // Require s to be in lower half to prevent malleability
        return uint256(s) <= 0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5D576E7357A4501DDFE92F46681B20A0;
    }

    /**
     * @notice Creates an Ethereum signed message hash
     */
    function toEthSignedMessageHash(bytes32 hash) internal pure returns (bytes32) {
        return hash.toEthSignedMessageHash();
    }

    /**
     * @notice Creates hash from byte array message
     */
    function toEthSignedMessageHash(bytes memory message) internal pure returns (bytes32) {
        return MessageHashUtils.toEthSignedMessageHash(message);
    }
}
