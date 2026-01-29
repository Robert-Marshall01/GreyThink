// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

/**
 * @title StringUtils
 * @author Grey Solidity Project
 * @notice String manipulation utilities
 * @dev Provides common string operations for Solidity
 */
library StringUtils {
    bytes16 private constant HEX_DIGITS = "0123456789abcdef";

    /**
     * @notice Returns the length of a string
     * @param str The string
     * @return The length in bytes
     */
    function length(string memory str) internal pure returns (uint256) {
        return bytes(str).length;
    }

    /**
     * @notice Checks if a string is empty
     * @param str The string to check
     * @return True if empty
     */
    function isEmpty(string memory str) internal pure returns (bool) {
        return bytes(str).length == 0;
    }

    /**
     * @notice Compares two strings for equality
     * @param a First string
     * @param b Second string
     * @return True if equal
     */
    function equals(string memory a, string memory b) internal pure returns (bool) {
        return keccak256(bytes(a)) == keccak256(bytes(b));
    }

    /**
     * @notice Concatenates two strings
     * @param a First string
     * @param b Second string
     * @return The concatenated string
     */
    function concat(string memory a, string memory b) internal pure returns (string memory) {
        return string(abi.encodePacked(a, b));
    }

    /**
     * @notice Concatenates multiple strings
     * @param strings Array of strings
     * @return The concatenated string
     */
    function join(string[] memory strings) internal pure returns (string memory) {
        bytes memory result;
        for (uint256 i = 0; i < strings.length; i++) {
            result = abi.encodePacked(result, strings[i]);
        }
        return string(result);
    }

    /**
     * @notice Joins strings with a separator
     * @param strings Array of strings
     * @param separator The separator
     * @return The joined string
     */
    function joinWithSeparator(
        string[] memory strings,
        string memory separator
    ) internal pure returns (string memory) {
        if (strings.length == 0) return "";
        
        bytes memory result = bytes(strings[0]);
        for (uint256 i = 1; i < strings.length; i++) {
            result = abi.encodePacked(result, separator, strings[i]);
        }
        return string(result);
    }

    /**
     * @notice Converts uint256 to string
     * @param value The value to convert
     * @return The string representation
     */
    function toString(uint256 value) internal pure returns (string memory) {
        if (value == 0) {
            return "0";
        }
        
        uint256 temp = value;
        uint256 digits;
        
        while (temp != 0) {
            digits++;
            temp /= 10;
        }
        
        bytes memory buffer = new bytes(digits);
        
        while (value != 0) {
            digits -= 1;
            buffer[digits] = bytes1(uint8(48 + uint256(value % 10)));
            value /= 10;
        }
        
        return string(buffer);
    }

    /**
     * @notice Converts int256 to string
     * @param value The value to convert
     * @return The string representation
     */
    function toString(int256 value) internal pure returns (string memory) {
        if (value >= 0) {
            return toString(uint256(value));
        } else {
            return concat("-", toString(uint256(-value)));
        }
    }

    /**
     * @notice Converts address to string
     * @param addr The address to convert
     * @return The string representation
     */
    function toHexString(address addr) internal pure returns (string memory) {
        return toHexString(uint256(uint160(addr)), 20);
    }

    /**
     * @notice Converts bytes32 to hex string
     * @param value The bytes32 value
     * @return The hex string
     */
    function toHexString(bytes32 value) internal pure returns (string memory) {
        return toHexString(uint256(value), 32);
    }

    /**
     * @notice Converts uint256 to hex string with fixed length
     * @param value The value
     * @param byteLength The byte length
     * @return The hex string
     */
    function toHexString(uint256 value, uint256 byteLength) internal pure returns (string memory) {
        bytes memory buffer = new bytes(2 * byteLength + 2);
        buffer[0] = "0";
        buffer[1] = "x";
        
        for (uint256 i = 2 * byteLength + 1; i > 1; --i) {
            buffer[i] = HEX_DIGITS[value & 0xf];
            value >>= 4;
        }
        
        require(value == 0, "StringUtils: hex length insufficient");
        return string(buffer);
    }

    /**
     * @notice Converts bool to string
     * @param value The boolean value
     * @return "true" or "false"
     */
    function toString(bool value) internal pure returns (string memory) {
        return value ? "true" : "false";
    }

    /**
     * @notice Returns a substring
     * @param str The original string
     * @param startIndex The start index
     * @param endIndex The end index (exclusive)
     * @return The substring
     */
    function substring(
        string memory str,
        uint256 startIndex,
        uint256 endIndex
    ) internal pure returns (string memory) {
        bytes memory strBytes = bytes(str);
        require(startIndex <= endIndex, "StringUtils: invalid indices");
        require(endIndex <= strBytes.length, "StringUtils: index out of bounds");
        
        bytes memory result = new bytes(endIndex - startIndex);
        for (uint256 i = startIndex; i < endIndex; i++) {
            result[i - startIndex] = strBytes[i];
        }
        return string(result);
    }

    /**
     * @notice Finds the index of a character
     * @param str The string to search
     * @param char The character to find
     * @return The index, or max uint256 if not found
     */
    function indexOf(string memory str, bytes1 char) internal pure returns (uint256) {
        bytes memory strBytes = bytes(str);
        for (uint256 i = 0; i < strBytes.length; i++) {
            if (strBytes[i] == char) {
                return i;
            }
        }
        return type(uint256).max;
    }

    /**
     * @notice Checks if a string contains a character
     * @param str The string to search
     * @param char The character to find
     * @return True if contains
     */
    function contains(string memory str, bytes1 char) internal pure returns (bool) {
        return indexOf(str, char) != type(uint256).max;
    }

    /**
     * @notice Converts string to lowercase (ASCII only)
     * @param str The string to convert
     * @return The lowercase string
     */
    function toLower(string memory str) internal pure returns (string memory) {
        bytes memory bStr = bytes(str);
        bytes memory bLower = new bytes(bStr.length);
        
        for (uint256 i = 0; i < bStr.length; i++) {
            if ((bStr[i] >= 0x41) && (bStr[i] <= 0x5A)) {
                bLower[i] = bytes1(uint8(bStr[i]) + 32);
            } else {
                bLower[i] = bStr[i];
            }
        }
        return string(bLower);
    }

    /**
     * @notice Converts string to uppercase (ASCII only)
     * @param str The string to convert
     * @return The uppercase string
     */
    function toUpper(string memory str) internal pure returns (string memory) {
        bytes memory bStr = bytes(str);
        bytes memory bUpper = new bytes(bStr.length);
        
        for (uint256 i = 0; i < bStr.length; i++) {
            if ((bStr[i] >= 0x61) && (bStr[i] <= 0x7a)) {
                bUpper[i] = bytes1(uint8(bStr[i]) - 32);
            } else {
                bUpper[i] = bStr[i];
            }
        }
        return string(bUpper);
    }

    /**
     * @notice Pads a string on the left
     * @param str The string to pad
     * @param totalLength The desired total length
     * @param padChar The padding character
     * @return The padded string
     */
    function padLeft(
        string memory str,
        uint256 totalLength,
        bytes1 padChar
    ) internal pure returns (string memory) {
        bytes memory strBytes = bytes(str);
        if (strBytes.length >= totalLength) {
            return str;
        }
        
        uint256 padLength = totalLength - strBytes.length;
        bytes memory result = new bytes(totalLength);
        
        for (uint256 i = 0; i < padLength; i++) {
            result[i] = padChar;
        }
        for (uint256 i = 0; i < strBytes.length; i++) {
            result[padLength + i] = strBytes[i];
        }
        
        return string(result);
    }

    /**
     * @notice Pads a string on the right
     * @param str The string to pad
     * @param totalLength The desired total length
     * @param padChar The padding character
     * @return The padded string
     */
    function padRight(
        string memory str,
        uint256 totalLength,
        bytes1 padChar
    ) internal pure returns (string memory) {
        bytes memory strBytes = bytes(str);
        if (strBytes.length >= totalLength) {
            return str;
        }
        
        bytes memory result = new bytes(totalLength);
        
        for (uint256 i = 0; i < strBytes.length; i++) {
            result[i] = strBytes[i];
        }
        for (uint256 i = strBytes.length; i < totalLength; i++) {
            result[i] = padChar;
        }
        
        return string(result);
    }

    /**
     * @notice Splits a string by a delimiter
     * @param str The string to split
     * @param delimiter The delimiter character
     * @return An array of strings
     */
    function split(string memory str, bytes1 delimiter) internal pure returns (string[] memory) {
        bytes memory strBytes = bytes(str);
        
        // Count delimiters
        uint256 count = 1;
        for (uint256 i = 0; i < strBytes.length; i++) {
            if (strBytes[i] == delimiter) {
                count++;
            }
        }
        
        string[] memory parts = new string[](count);
        uint256 partIndex = 0;
        uint256 start = 0;
        
        for (uint256 i = 0; i <= strBytes.length; i++) {
            if (i == strBytes.length || strBytes[i] == delimiter) {
                bytes memory part = new bytes(i - start);
                for (uint256 j = start; j < i; j++) {
                    part[j - start] = strBytes[j];
                }
                parts[partIndex] = string(part);
                partIndex++;
                start = i + 1;
            }
        }
        
        return parts;
    }
}
