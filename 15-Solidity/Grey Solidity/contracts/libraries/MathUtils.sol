// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

/**
 * @title MathUtils
 * @author Grey Solidity Project
 * @notice Mathematical utility functions
 * @dev Provides safe math operations and calculations
 */
library MathUtils {
    /// @notice Maximum uint256 value
    uint256 public constant MAX_UINT256 = type(uint256).max;

    /// @notice Precision for percentage calculations (100%)
    uint256 public constant PERCENTAGE_BASE = 10000; // 100% = 10000 basis points

    /// @notice Precision for fixed-point calculations
    uint256 public constant PRECISION = 1e18;

    /**
     * @notice Returns the minimum of two numbers
     * @param a First number
     * @param b Second number
     * @return The minimum
     */
    function min(uint256 a, uint256 b) internal pure returns (uint256) {
        return a < b ? a : b;
    }

    /**
     * @notice Returns the maximum of two numbers
     * @param a First number
     * @param b Second number
     * @return The maximum
     */
    function max(uint256 a, uint256 b) internal pure returns (uint256) {
        return a > b ? a : b;
    }

    /**
     * @notice Returns the average of two numbers
     * @dev Rounds down. Avoids overflow by using (a & b) + (a ^ b) / 2
     * @param a First number
     * @param b Second number
     * @return The average
     */
    function average(uint256 a, uint256 b) internal pure returns (uint256) {
        return (a & b) + (a ^ b) / 2;
    }

    /**
     * @notice Calculates ceiling of division
     * @param a The dividend
     * @param b The divisor
     * @return The ceiling of a/b
     */
    function ceilDiv(uint256 a, uint256 b) internal pure returns (uint256) {
        require(b > 0, "MathUtils: division by zero");
        return a == 0 ? 0 : (a - 1) / b + 1;
    }

    /**
     * @notice Calculates a percentage of a number
     * @param value The base value
     * @param percentage_ The percentage in basis points
     * @return The calculated percentage
     */
    function percentage(uint256 value, uint256 percentage_) internal pure returns (uint256) {
        return (value * percentage_) / PERCENTAGE_BASE;
    }

    /**
     * @notice Calculates what percentage a is of b
     * @param a The part
     * @param b The whole
     * @return The percentage in basis points
     */
    function percentageOf(uint256 a, uint256 b) internal pure returns (uint256) {
        require(b > 0, "MathUtils: division by zero");
        return (a * PERCENTAGE_BASE) / b;
    }

    /**
     * @notice Safely multiplies two numbers with precision handling
     * @param a First number
     * @param b Second number
     * @return The product divided by PRECISION
     */
    function mulPrecision(uint256 a, uint256 b) internal pure returns (uint256) {
        return (a * b) / PRECISION;
    }

    /**
     * @notice Safely divides with precision handling
     * @param a The dividend (will be multiplied by PRECISION)
     * @param b The divisor
     * @return The result with PRECISION
     */
    function divPrecision(uint256 a, uint256 b) internal pure returns (uint256) {
        require(b > 0, "MathUtils: division by zero");
        return (a * PRECISION) / b;
    }

    /**
     * @notice Calculates square root using Babylonian method
     * @param x The number
     * @return The square root
     */
    function sqrt(uint256 x) internal pure returns (uint256) {
        if (x == 0) return 0;
        
        uint256 z = (x + 1) / 2;
        uint256 y = x;
        
        while (z < y) {
            y = z;
            z = (x / z + z) / 2;
        }
        
        return y;
    }

    /**
     * @notice Calculates power with modulo (a^b mod m)
     * @param base The base
     * @param exponent The exponent
     * @param modulus The modulus
     * @return The result
     */
    function modPow(uint256 base, uint256 exponent, uint256 modulus) internal pure returns (uint256) {
        require(modulus > 0, "MathUtils: modulus is zero");
        
        if (modulus == 1) return 0;
        
        uint256 result = 1;
        base = base % modulus;
        
        while (exponent > 0) {
            if (exponent % 2 == 1) {
                result = (result * base) % modulus;
            }
            exponent = exponent / 2;
            base = (base * base) % modulus;
        }
        
        return result;
    }

    /**
     * @notice Calculates absolute difference
     * @param a First number
     * @param b Second number
     * @return The absolute difference
     */
    function absDiff(uint256 a, uint256 b) internal pure returns (uint256) {
        return a > b ? a - b : b - a;
    }

    /**
     * @notice Clamps a value between min and max
     * @param value The value to clamp
     * @param minValue The minimum
     * @param maxValue The maximum
     * @return The clamped value
     */
    function clamp(uint256 value, uint256 minValue, uint256 maxValue) internal pure returns (uint256) {
        require(minValue <= maxValue, "MathUtils: min > max");
        return min(max(value, minValue), maxValue);
    }

    /**
     * @notice Linear interpolation between two values
     * @param a Start value
     * @param b End value
     * @param t Progress (0 to PRECISION)
     * @return The interpolated value
     */
    function lerp(uint256 a, uint256 b, uint256 t) internal pure returns (uint256) {
        require(t <= PRECISION, "MathUtils: t > 1");
        if (a <= b) {
            return a + mulPrecision(b - a, t);
        } else {
            return a - mulPrecision(a - b, t);
        }
    }

    /**
     * @notice Checks if a value is within range (inclusive)
     * @param value The value to check
     * @param minValue The minimum
     * @param maxValue The maximum
     * @return True if in range
     */
    function inRange(uint256 value, uint256 minValue, uint256 maxValue) internal pure returns (bool) {
        return value >= minValue && value <= maxValue;
    }

    /**
     * @notice Calculates compound interest
     * @param principal The principal amount
     * @param rate The rate per period (in basis points)
     * @param periods Number of periods
     * @return The final amount
     */
    function compoundInterest(
        uint256 principal,
        uint256 rate,
        uint256 periods
    ) internal pure returns (uint256) {
        uint256 result = principal;
        uint256 rateMultiplier = PERCENTAGE_BASE + rate;
        
        for (uint256 i = 0; i < periods; i++) {
            result = (result * rateMultiplier) / PERCENTAGE_BASE;
        }
        
        return result;
    }

    /**
     * @notice Calculates weighted average
     * @param values Array of values
     * @param weights Array of weights
     * @return The weighted average
     */
    function weightedAverage(
        uint256[] memory values,
        uint256[] memory weights
    ) internal pure returns (uint256) {
        require(values.length == weights.length, "MathUtils: length mismatch");
        require(values.length > 0, "MathUtils: empty arrays");
        
        uint256 totalWeight;
        uint256 weightedSum;
        
        for (uint256 i = 0; i < values.length; i++) {
            weightedSum += values[i] * weights[i];
            totalWeight += weights[i];
        }
        
        require(totalWeight > 0, "MathUtils: zero total weight");
        return weightedSum / totalWeight;
    }
}
