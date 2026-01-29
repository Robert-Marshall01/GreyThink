// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

/**
 * @title ITimelock
 * @notice Interface for timelock controller contracts
 * @dev Queues and executes proposals after a delay
 */
interface ITimelock {
    /// @notice Operation status
    enum OperationStatus {
        Unknown,
        Pending,
        Ready,
        Done
    }

    /**
     * @notice Returns the minimum delay
     * @return The minimum delay in seconds
     */
    function getMinDelay() external view returns (uint256);

    /**
     * @notice Returns operation timestamp
     * @param id The operation ID
     * @return The timestamp (0 = unset, 1 = done)
     */
    function getTimestamp(bytes32 id) external view returns (uint256);

    /**
     * @notice Checks if an operation is pending
     * @param id The operation ID
     * @return True if pending
     */
    function isOperationPending(bytes32 id) external view returns (bool);

    /**
     * @notice Checks if an operation is ready
     * @param id The operation ID
     * @return True if ready
     */
    function isOperationReady(bytes32 id) external view returns (bool);

    /**
     * @notice Checks if an operation is done
     * @param id The operation ID
     * @return True if done
     */
    function isOperationDone(bytes32 id) external view returns (bool);

    /**
     * @notice Returns the operation status
     * @param id The operation ID
     * @return The status
     */
    function getOperationStatus(bytes32 id) external view returns (OperationStatus);

    /**
     * @notice Schedules an operation
     * @param target The target address
     * @param value The ETH value
     * @param data The call data
     * @param predecessor The predecessor operation ID
     * @param salt The salt
     * @param delay The delay in seconds
     */
    function schedule(
        address target,
        uint256 value,
        bytes calldata data,
        bytes32 predecessor,
        bytes32 salt,
        uint256 delay
    ) external;

    /**
     * @notice Schedules a batch of operations
     * @param targets Array of target addresses
     * @param values Array of ETH values
     * @param payloads Array of call data
     * @param predecessor The predecessor operation ID
     * @param salt The salt
     * @param delay The delay in seconds
     */
    function scheduleBatch(
        address[] calldata targets,
        uint256[] calldata values,
        bytes[] calldata payloads,
        bytes32 predecessor,
        bytes32 salt,
        uint256 delay
    ) external;

    /**
     * @notice Executes an operation
     * @param target The target address
     * @param value The ETH value
     * @param payload The call data
     * @param predecessor The predecessor operation ID
     * @param salt The salt
     */
    function execute(
        address target,
        uint256 value,
        bytes calldata payload,
        bytes32 predecessor,
        bytes32 salt
    ) external payable;

    /**
     * @notice Executes a batch of operations
     * @param targets Array of target addresses
     * @param values Array of ETH values
     * @param payloads Array of call data
     * @param predecessor The predecessor operation ID
     * @param salt The salt
     */
    function executeBatch(
        address[] calldata targets,
        uint256[] calldata values,
        bytes[] calldata payloads,
        bytes32 predecessor,
        bytes32 salt
    ) external payable;

    /**
     * @notice Cancels an operation
     * @param id The operation ID
     */
    function cancel(bytes32 id) external;

    /**
     * @notice Updates the minimum delay
     * @param newDelay The new minimum delay
     */
    function updateDelay(uint256 newDelay) external;

    /**
     * @notice Hashes an operation
     * @param target The target address
     * @param value The ETH value
     * @param data The call data
     * @param predecessor The predecessor operation ID
     * @param salt The salt
     * @return The operation ID
     */
    function hashOperation(
        address target,
        uint256 value,
        bytes calldata data,
        bytes32 predecessor,
        bytes32 salt
    ) external pure returns (bytes32);

    /**
     * @notice Hashes a batch of operations
     * @param targets Array of target addresses
     * @param values Array of ETH values
     * @param payloads Array of call data
     * @param predecessor The predecessor operation ID
     * @param salt The salt
     * @return The operation ID
     */
    function hashOperationBatch(
        address[] calldata targets,
        uint256[] calldata values,
        bytes[] calldata payloads,
        bytes32 predecessor,
        bytes32 salt
    ) external pure returns (bytes32);

    // Events
    event CallScheduled(
        bytes32 indexed id,
        uint256 indexed index,
        address target,
        uint256 value,
        bytes data,
        bytes32 predecessor,
        uint256 delay
    );
    event CallExecuted(bytes32 indexed id, uint256 indexed index, address target, uint256 value, bytes data);
    event Cancelled(bytes32 indexed id);
    event MinDelayChange(uint256 oldDuration, uint256 newDuration);
}
