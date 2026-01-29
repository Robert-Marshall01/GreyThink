// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

/**
 * @title AddressUtils
 * @author Grey Solidity Project
 * @notice Address manipulation and validation utilities
 * @dev Provides common address operations for Solidity
 */
library AddressUtils {
    /**
     * @notice Checks if an address is a contract
     * @param account The address to check
     * @return True if contract
     */
    function isContract(address account) internal view returns (bool) {
        uint256 size;
        assembly {
            size := extcodesize(account)
        }
        return size > 0;
    }

    /**
     * @notice Checks if an address is not zero
     * @param account The address to check
     * @return True if not zero
     */
    function isNotZero(address account) internal pure returns (bool) {
        return account != address(0);
    }

    /**
     * @notice Checks if an address is zero
     * @param account The address to check
     * @return True if zero
     */
    function isZero(address account) internal pure returns (bool) {
        return account == address(0);
    }

    /**
     * @notice Validates that an address is not zero
     * @param account The address to validate
     * @param errorMessage The error message
     */
    function requireNonZero(address account, string memory errorMessage) internal pure {
        require(account != address(0), errorMessage);
    }

    /**
     * @notice Sends ETH to an address
     * @param to The recipient
     * @param amount The amount to send
     * @return success True if successful
     */
    function sendValue(address payable to, uint256 amount) internal returns (bool success) {
        require(address(this).balance >= amount, "AddressUtils: insufficient balance");
        
        (success, ) = to.call{value: amount}("");
    }

    /**
     * @notice Sends ETH with revert on failure
     * @param to The recipient
     * @param amount The amount to send
     */
    function sendValueOrRevert(address payable to, uint256 amount) internal {
        require(address(this).balance >= amount, "AddressUtils: insufficient balance");
        
        (bool success, ) = to.call{value: amount}("");
        require(success, "AddressUtils: ETH transfer failed");
    }

    /**
     * @notice Low-level call to an address
     * @param target The target address
     * @param data The call data
     * @return success True if successful
     * @return returnData The return data
     */
    function functionCall(
        address target,
        bytes memory data
    ) internal returns (bool success, bytes memory returnData) {
        return functionCallWithValue(target, data, 0);
    }

    /**
     * @notice Low-level call with ETH value
     * @param target The target address
     * @param data The call data
     * @param value The ETH value
     * @return success True if successful
     * @return returnData The return data
     */
    function functionCallWithValue(
        address target,
        bytes memory data,
        uint256 value
    ) internal returns (bool success, bytes memory returnData) {
        require(address(this).balance >= value, "AddressUtils: insufficient balance");
        require(isContract(target), "AddressUtils: call to non-contract");
        
        (success, returnData) = target.call{value: value}(data);
    }

    /**
     * @notice Low-level call with revert on failure
     * @param target The target address
     * @param data The call data
     * @return The return data
     */
    function functionCallOrRevert(
        address target,
        bytes memory data
    ) internal returns (bytes memory) {
        return functionCallWithValueOrRevert(target, data, 0);
    }

    /**
     * @notice Low-level call with value and revert on failure
     * @param target The target address
     * @param data The call data
     * @param value The ETH value
     * @return The return data
     */
    function functionCallWithValueOrRevert(
        address target,
        bytes memory data,
        uint256 value
    ) internal returns (bytes memory) {
        require(address(this).balance >= value, "AddressUtils: insufficient balance");
        require(isContract(target), "AddressUtils: call to non-contract");
        
        (bool success, bytes memory returnData) = target.call{value: value}(data);
        
        if (!success) {
            if (returnData.length > 0) {
                assembly {
                    let returndata_size := mload(returnData)
                    revert(add(32, returnData), returndata_size)
                }
            } else {
                revert("AddressUtils: low-level call failed");
            }
        }
        
        return returnData;
    }

    /**
     * @notice Static call to an address
     * @param target The target address
     * @param data The call data
     * @return success True if successful
     * @return returnData The return data
     */
    function functionStaticCall(
        address target,
        bytes memory data
    ) internal view returns (bool success, bytes memory returnData) {
        require(isContract(target), "AddressUtils: static call to non-contract");
        
        (success, returnData) = target.staticcall(data);
    }

    /**
     * @notice Delegate call to an address
     * @param target The target address
     * @param data The call data
     * @return success True if successful
     * @return returnData The return data
     */
    function functionDelegateCall(
        address target,
        bytes memory data
    ) internal returns (bool success, bytes memory returnData) {
        require(isContract(target), "AddressUtils: delegate call to non-contract");
        
        (success, returnData) = target.delegatecall(data);
    }

    /**
     * @notice Computes CREATE2 address
     * @param deployer The deployer address
     * @param salt The salt
     * @param bytecodeHash The bytecode hash
     * @return The predicted address
     */
    function computeCreate2Address(
        address deployer,
        bytes32 salt,
        bytes32 bytecodeHash
    ) internal pure returns (address) {
        return address(uint160(uint256(keccak256(
            abi.encodePacked(
                bytes1(0xff),
                deployer,
                salt,
                bytecodeHash
            )
        ))));
    }

    /**
     * @notice Computes CREATE address
     * @param deployer The deployer address
     * @param nonce The nonce
     * @return The predicted address
     */
    function computeCreateAddress(
        address deployer,
        uint256 nonce
    ) internal pure returns (address) {
        bytes memory data;
        
        if (nonce == 0x00) {
            data = abi.encodePacked(bytes1(0xd6), bytes1(0x94), deployer, bytes1(0x80));
        } else if (nonce <= 0x7f) {
            data = abi.encodePacked(bytes1(0xd6), bytes1(0x94), deployer, bytes1(uint8(nonce)));
        } else if (nonce <= 0xff) {
            data = abi.encodePacked(bytes1(0xd7), bytes1(0x94), deployer, bytes1(0x81), uint8(nonce));
        } else if (nonce <= 0xffff) {
            data = abi.encodePacked(bytes1(0xd8), bytes1(0x94), deployer, bytes1(0x82), uint16(nonce));
        } else if (nonce <= 0xffffff) {
            data = abi.encodePacked(bytes1(0xd9), bytes1(0x94), deployer, bytes1(0x83), uint24(nonce));
        } else {
            data = abi.encodePacked(bytes1(0xda), bytes1(0x94), deployer, bytes1(0x84), uint32(nonce));
        }
        
        return address(uint160(uint256(keccak256(data))));
    }

    /**
     * @notice Converts an address to bytes
     * @param addr The address
     * @return The bytes representation
     */
    function toBytes(address addr) internal pure returns (bytes memory) {
        return abi.encodePacked(addr);
    }

    /**
     * @notice Converts bytes to an address
     * @param data The bytes (must be 20 bytes)
     * @return The address
     */
    function toAddress(bytes memory data) internal pure returns (address) {
        require(data.length >= 20, "AddressUtils: invalid bytes length");
        
        address addr;
        assembly {
            addr := mload(add(data, 20))
        }
        return addr;
    }

    /**
     * @notice Checks if two addresses are equal
     * @param a First address
     * @param b Second address
     * @return True if equal
     */
    function equals(address a, address b) internal pure returns (bool) {
        return a == b;
    }

    /**
     * @notice Returns the balance of an address
     * @param account The address
     * @return The ETH balance
     */
    function getBalance(address account) internal view returns (uint256) {
        return account.balance;
    }

    /**
     * @notice Checks if an address has code deployed
     * @param account The address to check
     * @return True if code exists
     */
    function hasCode(address account) internal view returns (bool) {
        return account.code.length > 0;
    }

    /**
     * @notice Gets the code size of an address
     * @param account The address
     * @return size The code size
     */
    function codeSize(address account) internal view returns (uint256 size) {
        assembly {
            size := extcodesize(account)
        }
    }

    /**
     * @notice Gets the code hash of an address
     * @param account The address
     * @return hash The code hash
     */
    function codeHash(address account) internal view returns (bytes32 hash) {
        assembly {
            hash := extcodehash(account)
        }
    }
}
