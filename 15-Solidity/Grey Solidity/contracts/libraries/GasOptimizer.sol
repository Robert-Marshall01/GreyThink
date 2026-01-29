// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

/**
 * @title GasOptimizer
 * @notice Library for gas-optimized operations
 * @dev Collection of patterns for reducing gas costs
 * 
 * Optimizations included:
 * - Unchecked math for known-safe operations
 * - Efficient storage packing
 * - Calldata optimization
 * - Batch operation helpers
 * - Memory-efficient sorting
 * - Bit manipulation utilities
 */
library GasOptimizer {
    // ============================================
    // UNCHECKED MATH
    // ============================================

    /**
     * @notice Unchecked increment (saves ~50 gas per operation)
     */
    function unsafeInc(uint256 x) internal pure returns (uint256) {
        unchecked {
            return x + 1;
        }
    }

    /**
     * @notice Unchecked decrement (use only when x > 0 is guaranteed)
     */
    function unsafeDec(uint256 x) internal pure returns (uint256) {
        unchecked {
            return x - 1;
        }
    }

    /**
     * @notice Unchecked add (use only when overflow is impossible)
     */
    function unsafeAdd(uint256 a, uint256 b) internal pure returns (uint256) {
        unchecked {
            return a + b;
        }
    }

    /**
     * @notice Unchecked subtract (use only when underflow is impossible)
     */
    function unsafeSub(uint256 a, uint256 b) internal pure returns (uint256) {
        unchecked {
            return a - b;
        }
    }

    /**
     * @notice Unchecked multiply (use only when overflow is impossible)
     */
    function unsafeMul(uint256 a, uint256 b) internal pure returns (uint256) {
        unchecked {
            return a * b;
        }
    }

    /**
     * @notice Unchecked divide (use only when b != 0 is guaranteed)
     */
    function unsafeDiv(uint256 a, uint256 b) internal pure returns (uint256) {
        unchecked {
            return a / b;
        }
    }

    // ============================================
    // EFFICIENT LOOPS
    // ============================================

    /**
     * @notice Gas-efficient array sum
     */
    function sum(uint256[] memory arr) internal pure returns (uint256 result) {
        uint256 length = arr.length;
        for (uint256 i = 0; i < length;) {
            result += arr[i];
            unchecked { ++i; }
        }
    }

    /**
     * @notice Gas-efficient array sum (calldata)
     */
    function sumCalldata(uint256[] calldata arr) internal pure returns (uint256 result) {
        uint256 length = arr.length;
        for (uint256 i = 0; i < length;) {
            result += arr[i];
            unchecked { ++i; }
        }
    }

    /**
     * @notice Find max value in array efficiently
     */
    function max(uint256[] memory arr) internal pure returns (uint256 result) {
        uint256 length = arr.length;
        for (uint256 i = 0; i < length;) {
            if (arr[i] > result) {
                result = arr[i];
            }
            unchecked { ++i; }
        }
    }

    /**
     * @notice Find min value in array efficiently
     */
    function min(uint256[] memory arr) internal pure returns (uint256 result) {
        if (arr.length == 0) return 0;
        result = type(uint256).max;
        uint256 length = arr.length;
        for (uint256 i = 0; i < length;) {
            if (arr[i] < result) {
                result = arr[i];
            }
            unchecked { ++i; }
        }
    }

    // ============================================
    // BIT MANIPULATION
    // ============================================

    /**
     * @notice Check if x is power of 2
     */
    function isPowerOfTwo(uint256 x) internal pure returns (bool) {
        return x != 0 && (x & (x - 1)) == 0;
    }

    /**
     * @notice Get the position of the most significant bit
     */
    function mostSignificantBit(uint256 x) internal pure returns (uint256 r) {
        if (x >= 0x100000000000000000000000000000000) { x >>= 128; r += 128; }
        if (x >= 0x10000000000000000) { x >>= 64; r += 64; }
        if (x >= 0x100000000) { x >>= 32; r += 32; }
        if (x >= 0x10000) { x >>= 16; r += 16; }
        if (x >= 0x100) { x >>= 8; r += 8; }
        if (x >= 0x10) { x >>= 4; r += 4; }
        if (x >= 0x4) { x >>= 2; r += 2; }
        if (x >= 0x2) r += 1;
    }

    /**
     * @notice Get the position of the least significant bit
     */
    function leastSignificantBit(uint256 x) internal pure returns (uint256 r) {
        if (x == 0) return 256;
        
        r = 255;
        if (x & type(uint128).max > 0) { r -= 128; } else { x >>= 128; }
        if (x & type(uint64).max > 0) { r -= 64; } else { x >>= 64; }
        if (x & type(uint32).max > 0) { r -= 32; } else { x >>= 32; }
        if (x & type(uint16).max > 0) { r -= 16; } else { x >>= 16; }
        if (x & type(uint8).max > 0) { r -= 8; } else { x >>= 8; }
        if (x & 0xf > 0) { r -= 4; } else { x >>= 4; }
        if (x & 0x3 > 0) { r -= 2; } else { x >>= 2; }
        if (x & 0x1 > 0) { r -= 1; }
    }

    /**
     * @notice Count number of set bits (population count)
     */
    function popCount(uint256 x) internal pure returns (uint256 count) {
        unchecked {
            for (count = 0; x != 0; count++) {
                x &= x - 1;
            }
        }
    }

    /**
     * @notice Set bit at position
     */
    function setBit(uint256 x, uint256 pos) internal pure returns (uint256) {
        return x | (1 << pos);
    }

    /**
     * @notice Clear bit at position
     */
    function clearBit(uint256 x, uint256 pos) internal pure returns (uint256) {
        return x & ~(1 << pos);
    }

    /**
     * @notice Toggle bit at position
     */
    function toggleBit(uint256 x, uint256 pos) internal pure returns (uint256) {
        return x ^ (1 << pos);
    }

    /**
     * @notice Check if bit is set at position
     */
    function getBit(uint256 x, uint256 pos) internal pure returns (bool) {
        return (x >> pos) & 1 == 1;
    }

    // ============================================
    // STORAGE PACKING
    // ============================================

    /**
     * @notice Pack two uint128 into one uint256
     */
    function pack128(uint128 a, uint128 b) internal pure returns (uint256) {
        return uint256(a) | (uint256(b) << 128);
    }

    /**
     * @notice Unpack uint256 into two uint128
     */
    function unpack128(uint256 packed) internal pure returns (uint128 a, uint128 b) {
        a = uint128(packed);
        b = uint128(packed >> 128);
    }

    /**
     * @notice Pack four uint64 into one uint256
     */
    function pack64(uint64 a, uint64 b, uint64 c, uint64 d) internal pure returns (uint256) {
        return uint256(a) | (uint256(b) << 64) | (uint256(c) << 128) | (uint256(d) << 192);
    }

    /**
     * @notice Unpack uint256 into four uint64
     */
    function unpack64(uint256 packed) internal pure returns (uint64 a, uint64 b, uint64 c, uint64 d) {
        a = uint64(packed);
        b = uint64(packed >> 64);
        c = uint64(packed >> 128);
        d = uint64(packed >> 192);
    }

    /**
     * @notice Pack address and uint96 into one uint256
     */
    function packAddressUint96(address addr, uint96 value) internal pure returns (uint256) {
        return uint256(uint160(addr)) | (uint256(value) << 160);
    }

    /**
     * @notice Unpack uint256 into address and uint96
     */
    function unpackAddressUint96(uint256 packed) internal pure returns (address addr, uint96 value) {
        addr = address(uint160(packed));
        value = uint96(packed >> 160);
    }

    // ============================================
    // MEMORY OPTIMIZATION
    // ============================================

    /**
     * @notice Copy array efficiently using assembly
     */
    function copyArray(uint256[] memory source) internal pure returns (uint256[] memory dest) {
        uint256 length = source.length;
        dest = new uint256[](length);
        
        assembly {
            let srcPtr := add(source, 0x20)
            let destPtr := add(dest, 0x20)
            let byteLength := mul(length, 0x20)
            
            for { let i := 0 } lt(i, byteLength) { i := add(i, 0x20) } {
                mstore(add(destPtr, i), mload(add(srcPtr, i)))
            }
        }
    }

    /**
     * @notice Zero out array memory efficiently
     */
    function zeroArray(uint256[] memory arr) internal pure {
        assembly {
            let length := mload(arr)
            let ptr := add(arr, 0x20)
            let byteLength := mul(length, 0x20)
            
            for { let i := 0 } lt(i, byteLength) { i := add(i, 0x20) } {
                mstore(add(ptr, i), 0)
            }
        }
    }

    // ============================================
    // CONDITIONAL OPERATIONS
    // ============================================

    /**
     * @notice Branchless ternary (condition ? a : b) - saves gas on conditionals
     */
    function ternary(bool condition, uint256 a, uint256 b) internal pure returns (uint256 result) {
        assembly {
            result := xor(b, mul(xor(a, b), condition))
        }
    }

    /**
     * @notice Branchless max
     */
    function branchlessMax(uint256 a, uint256 b) internal pure returns (uint256) {
        unchecked {
            return a ^ ((a ^ b) & (uint256(int256(a - b) >> 255)));
        }
    }

    /**
     * @notice Branchless min
     */
    function branchlessMin(uint256 a, uint256 b) internal pure returns (uint256) {
        unchecked {
            return b ^ ((a ^ b) & (uint256(int256(a - b) >> 255)));
        }
    }

    /**
     * @notice Branchless absolute difference
     */
    function absDiff(uint256 a, uint256 b) internal pure returns (uint256) {
        unchecked {
            return a > b ? a - b : b - a;
        }
    }

    // ============================================
    // HASH OPTIMIZATION
    // ============================================

    /**
     * @notice Efficient keccak256 for two words
     */
    function hash2(bytes32 a, bytes32 b) internal pure returns (bytes32 result) {
        assembly {
            mstore(0x00, a)
            mstore(0x20, b)
            result := keccak256(0x00, 0x40)
        }
    }

    /**
     * @notice Efficient keccak256 for three words
     */
    function hash3(bytes32 a, bytes32 b, bytes32 c) internal pure returns (bytes32 result) {
        assembly {
            mstore(0x00, a)
            mstore(0x20, b)
            mstore(0x40, c)
            result := keccak256(0x00, 0x60)
        }
    }

    // ============================================
    // ADDRESS UTILITIES
    // ============================================

    /**
     * @notice Check if address is contract (cheaper than full code check for some cases)
     */
    function isContract(address account) internal view returns (bool) {
        uint256 size;
        assembly {
            size := extcodesize(account)
        }
        return size > 0;
    }

    /**
     * @notice Unsafe cast address to uint256
     */
    function toUint(address addr) internal pure returns (uint256) {
        return uint256(uint160(addr));
    }

    /**
     * @notice Unsafe cast uint256 to address
     */
    function toAddress(uint256 value) internal pure returns (address) {
        return address(uint160(value));
    }
}

/**
 * @title BatchExecutor
 * @notice Gas-efficient batch execution helper
 */
library BatchExecutor {
    error BatchFailed(uint256 index, bytes reason);

    /**
     * @notice Execute multiple calls in a single transaction
     */
    function batchCall(
        address[] calldata targets,
        bytes[] calldata datas
    ) internal returns (bytes[] memory results) {
        require(targets.length == datas.length, "Length mismatch");
        
        results = new bytes[](targets.length);
        
        for (uint256 i = 0; i < targets.length;) {
            (bool success, bytes memory result) = targets[i].call(datas[i]);
            if (!success) {
                revert BatchFailed(i, result);
            }
            results[i] = result;
            unchecked { ++i; }
        }
    }

    /**
     * @notice Execute multiple calls, continuing on failure
     */
    function batchCallTolerant(
        address[] calldata targets,
        bytes[] calldata datas
    ) internal returns (bool[] memory successes, bytes[] memory results) {
        require(targets.length == datas.length, "Length mismatch");
        
        successes = new bool[](targets.length);
        results = new bytes[](targets.length);
        
        for (uint256 i = 0; i < targets.length;) {
            (successes[i], results[i]) = targets[i].call(datas[i]);
            unchecked { ++i; }
        }
    }

    /**
     * @notice Execute multiple calls with value
     */
    function batchCallWithValue(
        address[] calldata targets,
        bytes[] calldata datas,
        uint256[] calldata values
    ) internal returns (bytes[] memory results) {
        require(targets.length == datas.length && targets.length == values.length, "Length mismatch");
        
        results = new bytes[](targets.length);
        
        for (uint256 i = 0; i < targets.length;) {
            (bool success, bytes memory result) = targets[i].call{value: values[i]}(datas[i]);
            if (!success) {
                revert BatchFailed(i, result);
            }
            results[i] = result;
            unchecked { ++i; }
        }
    }

    /**
     * @notice Execute multiple delegatecalls
     */
    function batchDelegateCall(
        address[] calldata targets,
        bytes[] calldata datas
    ) internal returns (bytes[] memory results) {
        require(targets.length == datas.length, "Length mismatch");
        
        results = new bytes[](targets.length);
        
        for (uint256 i = 0; i < targets.length;) {
            (bool success, bytes memory result) = targets[i].delegatecall(datas[i]);
            if (!success) {
                revert BatchFailed(i, result);
            }
            results[i] = result;
            unchecked { ++i; }
        }
    }
}
