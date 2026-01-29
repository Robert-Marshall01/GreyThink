// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/governance/TimelockController.sol";

/**
 * @title GreyTimelock
 * @author Grey Solidity Project
 * @notice Extended timelocked controller for governance
 * @dev Extends OpenZeppelin TimelockController with additional features
 */
contract GreyTimelock is TimelockController {
    /// @notice Emitted when minimum delay is updated
    event MinDelayUpdated(uint256 oldDelay, uint256 newDelay);

    /// @notice Emergency pause role
    bytes32 public constant EMERGENCY_ROLE = keccak256("EMERGENCY_ROLE");

    /// @notice Mapping of blocked operations
    mapping(bytes32 => bool) private _blockedOperations;

    /// @notice Emitted when an operation is blocked
    event OperationBlocked(bytes32 indexed id);

    /// @notice Emitted when an operation is unblocked
    event OperationUnblocked(bytes32 indexed id);

    /**
     * @notice Initializes the timelock
     * @param minDelay The minimum delay for operations
     * @param proposers Array of proposer addresses
     * @param executors Array of executor addresses
     * @param admin The admin address
     */
    constructor(
        uint256 minDelay,
        address[] memory proposers,
        address[] memory executors,
        address admin
    ) TimelockController(minDelay, proposers, executors, admin) {
        _grantRole(EMERGENCY_ROLE, admin);
    }

    /**
     * @notice Blocks an operation from execution
     * @param id The operation ID
     */
    function blockOperation(bytes32 id) external onlyRole(EMERGENCY_ROLE) {
        require(isOperationPending(id), "GreyTimelock: operation not pending");
        _blockedOperations[id] = true;
        emit OperationBlocked(id);
    }

    /**
     * @notice Unblocks an operation
     * @param id The operation ID
     */
    function unblockOperation(bytes32 id) external onlyRole(DEFAULT_ADMIN_ROLE) {
        _blockedOperations[id] = false;
        emit OperationUnblocked(id);
    }

    /**
     * @notice Checks if an operation is blocked
     * @param id The operation ID
     * @return True if blocked
     */
    function isBlocked(bytes32 id) public view returns (bool) {
        return _blockedOperations[id];
    }

    /**
     * @notice Override execute to check for blocked operations
     */
    function execute(
        address target,
        uint256 value,
        bytes calldata payload,
        bytes32 predecessor,
        bytes32 salt
    ) public payable override {
        bytes32 id = hashOperation(target, value, payload, predecessor, salt);
        require(!_blockedOperations[id], "GreyTimelock: operation blocked");
        super.execute(target, value, payload, predecessor, salt);
    }

    /**
     * @notice Override executeBatch to check for blocked operations
     */
    function executeBatch(
        address[] calldata targets,
        uint256[] calldata values,
        bytes[] calldata payloads,
        bytes32 predecessor,
        bytes32 salt
    ) public payable override {
        bytes32 id = hashOperationBatch(targets, values, payloads, predecessor, salt);
        require(!_blockedOperations[id], "GreyTimelock: operation blocked");
        super.executeBatch(targets, values, payloads, predecessor, salt);
    }

    /**
     * @notice Returns all pending operations count
     * @dev This is an approximation - actual implementation would need indexing
     */
    function getOperationsCount() external pure returns (uint256) {
        return 0; // Would need event indexing off-chain
    }
}
