// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

/**
 * @title ArrayUtils
 * @author Grey Solidity Project
 * @notice Array manipulation utilities
 * @dev Provides common array operations for Solidity
 */
library ArrayUtils {
    /**
     * @notice Checks if an array contains a value
     * @param arr The array to search
     * @param value The value to find
     * @return True if found
     */
    function contains(uint256[] memory arr, uint256 value) internal pure returns (bool) {
        for (uint256 i = 0; i < arr.length; i++) {
            if (arr[i] == value) {
                return true;
            }
        }
        return false;
    }

    /**
     * @notice Checks if an address array contains an address
     * @param arr The array to search
     * @param value The address to find
     * @return True if found
     */
    function contains(address[] memory arr, address value) internal pure returns (bool) {
        for (uint256 i = 0; i < arr.length; i++) {
            if (arr[i] == value) {
                return true;
            }
        }
        return false;
    }

    /**
     * @notice Finds the index of a value
     * @param arr The array to search
     * @param value The value to find
     * @return The index, or max uint256 if not found
     */
    function indexOf(uint256[] memory arr, uint256 value) internal pure returns (uint256) {
        for (uint256 i = 0; i < arr.length; i++) {
            if (arr[i] == value) {
                return i;
            }
        }
        return type(uint256).max;
    }

    /**
     * @notice Finds the index of an address
     * @param arr The array to search
     * @param value The address to find
     * @return The index, or max uint256 if not found
     */
    function indexOf(address[] memory arr, address value) internal pure returns (uint256) {
        for (uint256 i = 0; i < arr.length; i++) {
            if (arr[i] == value) {
                return i;
            }
        }
        return type(uint256).max;
    }

    /**
     * @notice Removes element at index (order not preserved)
     * @param arr The array
     * @param index The index to remove
     * @return The new array
     */
    function removeAtIndexUnordered(
        uint256[] memory arr,
        uint256 index
    ) internal pure returns (uint256[] memory) {
        require(index < arr.length, "ArrayUtils: index out of bounds");
        
        uint256[] memory result = new uint256[](arr.length - 1);
        
        for (uint256 i = 0; i < index; i++) {
            result[i] = arr[i];
        }
        
        if (index < arr.length - 1) {
            result[index] = arr[arr.length - 1];
            for (uint256 i = index + 1; i < arr.length - 1; i++) {
                result[i] = arr[i];
            }
        }
        
        return result;
    }

    /**
     * @notice Removes element at index (order preserved)
     * @param arr The array
     * @param index The index to remove
     * @return The new array
     */
    function removeAtIndex(
        uint256[] memory arr,
        uint256 index
    ) internal pure returns (uint256[] memory) {
        require(index < arr.length, "ArrayUtils: index out of bounds");
        
        uint256[] memory result = new uint256[](arr.length - 1);
        
        for (uint256 i = 0; i < index; i++) {
            result[i] = arr[i];
        }
        for (uint256 i = index + 1; i < arr.length; i++) {
            result[i - 1] = arr[i];
        }
        
        return result;
    }

    /**
     * @notice Removes element at index for address array
     * @param arr The array
     * @param index The index to remove
     * @return The new array
     */
    function removeAtIndex(
        address[] memory arr,
        uint256 index
    ) internal pure returns (address[] memory) {
        require(index < arr.length, "ArrayUtils: index out of bounds");
        
        address[] memory result = new address[](arr.length - 1);
        
        for (uint256 i = 0; i < index; i++) {
            result[i] = arr[i];
        }
        for (uint256 i = index + 1; i < arr.length; i++) {
            result[i - 1] = arr[i];
        }
        
        return result;
    }

    /**
     * @notice Appends a value to an array
     * @param arr The array
     * @param value The value to append
     * @return The new array
     */
    function push(uint256[] memory arr, uint256 value) internal pure returns (uint256[] memory) {
        uint256[] memory result = new uint256[](arr.length + 1);
        
        for (uint256 i = 0; i < arr.length; i++) {
            result[i] = arr[i];
        }
        result[arr.length] = value;
        
        return result;
    }

    /**
     * @notice Appends an address to an array
     * @param arr The array
     * @param value The address to append
     * @return The new array
     */
    function push(address[] memory arr, address value) internal pure returns (address[] memory) {
        address[] memory result = new address[](arr.length + 1);
        
        for (uint256 i = 0; i < arr.length; i++) {
            result[i] = arr[i];
        }
        result[arr.length] = value;
        
        return result;
    }

    /**
     * @notice Concatenates two arrays
     * @param a First array
     * @param b Second array
     * @return The concatenated array
     */
    function concat(
        uint256[] memory a,
        uint256[] memory b
    ) internal pure returns (uint256[] memory) {
        uint256[] memory result = new uint256[](a.length + b.length);
        
        for (uint256 i = 0; i < a.length; i++) {
            result[i] = a[i];
        }
        for (uint256 i = 0; i < b.length; i++) {
            result[a.length + i] = b[i];
        }
        
        return result;
    }

    /**
     * @notice Returns a slice of an array
     * @param arr The array
     * @param start The start index
     * @param end The end index (exclusive)
     * @return The slice
     */
    function slice(
        uint256[] memory arr,
        uint256 start,
        uint256 end
    ) internal pure returns (uint256[] memory) {
        require(start <= end, "ArrayUtils: invalid range");
        require(end <= arr.length, "ArrayUtils: out of bounds");
        
        uint256[] memory result = new uint256[](end - start);
        
        for (uint256 i = start; i < end; i++) {
            result[i - start] = arr[i];
        }
        
        return result;
    }

    /**
     * @notice Reverses an array
     * @param arr The array to reverse
     * @return The reversed array
     */
    function reverse(uint256[] memory arr) internal pure returns (uint256[] memory) {
        uint256[] memory result = new uint256[](arr.length);
        
        for (uint256 i = 0; i < arr.length; i++) {
            result[i] = arr[arr.length - 1 - i];
        }
        
        return result;
    }

    /**
     * @notice Sums all elements in an array
     * @param arr The array
     * @return The sum
     */
    function sum(uint256[] memory arr) internal pure returns (uint256) {
        uint256 total;
        for (uint256 i = 0; i < arr.length; i++) {
            total += arr[i];
        }
        return total;
    }

    /**
     * @notice Finds the minimum value
     * @param arr The array
     * @return The minimum
     */
    function min(uint256[] memory arr) internal pure returns (uint256) {
        require(arr.length > 0, "ArrayUtils: empty array");
        
        uint256 minVal = arr[0];
        for (uint256 i = 1; i < arr.length; i++) {
            if (arr[i] < minVal) {
                minVal = arr[i];
            }
        }
        return minVal;
    }

    /**
     * @notice Finds the maximum value
     * @param arr The array
     * @return The maximum
     */
    function max(uint256[] memory arr) internal pure returns (uint256) {
        require(arr.length > 0, "ArrayUtils: empty array");
        
        uint256 maxVal = arr[0];
        for (uint256 i = 1; i < arr.length; i++) {
            if (arr[i] > maxVal) {
                maxVal = arr[i];
            }
        }
        return maxVal;
    }

    /**
     * @notice Removes duplicates from an array
     * @param arr The array
     * @return The deduplicated array
     */
    function unique(uint256[] memory arr) internal pure returns (uint256[] memory) {
        if (arr.length == 0) return arr;
        
        // Count unique elements
        uint256 uniqueCount = 1;
        for (uint256 i = 1; i < arr.length; i++) {
            bool found = false;
            for (uint256 j = 0; j < i; j++) {
                if (arr[i] == arr[j]) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                uniqueCount++;
            }
        }
        
        // Build result
        uint256[] memory result = new uint256[](uniqueCount);
        result[0] = arr[0];
        uint256 resultIndex = 1;
        
        for (uint256 i = 1; i < arr.length; i++) {
            bool found = false;
            for (uint256 j = 0; j < resultIndex; j++) {
                if (arr[i] == result[j]) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                result[resultIndex] = arr[i];
                resultIndex++;
            }
        }
        
        return result;
    }

    /**
     * @notice Filters elements greater than a threshold
     * @param arr The array
     * @param threshold The threshold
     * @return The filtered array
     */
    function filterGreaterThan(
        uint256[] memory arr,
        uint256 threshold
    ) internal pure returns (uint256[] memory) {
        // Count matching elements
        uint256 count;
        for (uint256 i = 0; i < arr.length; i++) {
            if (arr[i] > threshold) {
                count++;
            }
        }
        
        // Build result
        uint256[] memory result = new uint256[](count);
        uint256 resultIndex;
        
        for (uint256 i = 0; i < arr.length; i++) {
            if (arr[i] > threshold) {
                result[resultIndex] = arr[i];
                resultIndex++;
            }
        }
        
        return result;
    }

    /**
     * @notice Sorts array in ascending order (bubble sort)
     * @param arr The array to sort
     * @return The sorted array
     */
    function sort(uint256[] memory arr) internal pure returns (uint256[] memory) {
        uint256[] memory result = new uint256[](arr.length);
        for (uint256 i = 0; i < arr.length; i++) {
            result[i] = arr[i];
        }
        
        for (uint256 i = 0; i < result.length; i++) {
            for (uint256 j = i + 1; j < result.length; j++) {
                if (result[i] > result[j]) {
                    (result[i], result[j]) = (result[j], result[i]);
                }
            }
        }
        
        return result;
    }

    /**
     * @notice Binary search on sorted array
     * @param arr The sorted array
     * @param value The value to find
     * @return The index, or max uint256 if not found
     */
    function binarySearch(
        uint256[] memory arr,
        uint256 value
    ) internal pure returns (uint256) {
        if (arr.length == 0) return type(uint256).max;
        
        uint256 low = 0;
        uint256 high = arr.length - 1;
        
        while (low <= high) {
            uint256 mid = (low + high) / 2;
            
            if (arr[mid] == value) {
                return mid;
            } else if (arr[mid] < value) {
                low = mid + 1;
            } else {
                if (mid == 0) break;
                high = mid - 1;
            }
        }
        
        return type(uint256).max;
    }
}
