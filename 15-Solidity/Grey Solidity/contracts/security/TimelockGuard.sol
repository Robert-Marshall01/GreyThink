// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/Pausable.sol";

/**
 * @title TimelockGuard
 * @author Grey Protocol Team
 * @notice Timelock protection for sensitive operations
 * @dev Implements configurable delays for different operation types
 */
contract TimelockGuard is AccessControl, ReentrancyGuard, Pausable {
    // ============================================
    // CONSTANTS & ROLES
    // ============================================

    bytes32 public constant TIMELOCK_ADMIN_ROLE = keccak256("TIMELOCK_ADMIN_ROLE");
    bytes32 public constant PROPOSER_ROLE = keccak256("PROPOSER_ROLE");
    bytes32 public constant EXECUTOR_ROLE = keccak256("EXECUTOR_ROLE");
    bytes32 public constant CANCELLER_ROLE = keccak256("CANCELLER_ROLE");

    uint256 public constant MINIMUM_DELAY = 1 hours;
    uint256 public constant MAXIMUM_DELAY = 30 days;
    uint256 public constant GRACE_PERIOD = 14 days;

    // ============================================
    // ENUMS
    // ============================================

    enum OperationState {
        Unset,
        Pending,
        Ready,
        Executed,
        Cancelled,
        Expired
    }

    enum OperationType {
        STANDARD,
        CRITICAL,
        EMERGENCY,
        GOVERNANCE,
        UPGRADE
    }

    // ============================================
    // STRUCTS
    // ============================================

    struct TimelockOperation {
        bytes32 id;
        address target;
        uint256 value;
        bytes data;
        bytes32 predecessor;
        uint256 delay;
        uint256 proposedAt;
        uint256 readyAt;
        uint256 executedAt;
        address proposer;
        address executor;
        OperationType opType;
        OperationState state;
        string description;
    }

    struct DelayConfig {
        uint256 standardDelay;
        uint256 criticalDelay;
        uint256 emergencyDelay;
        uint256 governanceDelay;
        uint256 upgradeDelay;
    }

    struct BatchOperation {
        address[] targets;
        uint256[] values;
        bytes[] datas;
        bytes32 predecessor;
        bytes32 salt;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Delay configuration
    DelayConfig public delayConfig;

    /// @notice Mapping of operation ID to operation
    mapping(bytes32 => TimelockOperation) public operations;

    /// @notice All operation IDs
    bytes32[] public operationIds;

    /// @notice Mapping of operation ID to index
    mapping(bytes32 => uint256) public operationIndex;

    /// @notice Pending operations count
    uint256 public pendingOperationsCount;

    /// @notice Total operations proposed
    uint256 public totalOperationsProposed;

    /// @notice Total operations executed
    uint256 public totalOperationsExecuted;

    /// @notice Emergency mode flag
    bool public emergencyMode;

    // ============================================
    // EVENTS
    // ============================================

    event OperationScheduled(
        bytes32 indexed id,
        address indexed target,
        uint256 value,
        bytes data,
        bytes32 predecessor,
        uint256 delay,
        address proposer
    );

    event OperationExecuted(
        bytes32 indexed id,
        address indexed target,
        uint256 value,
        bytes data,
        address executor
    );

    event OperationCancelled(bytes32 indexed id, address canceller);
    event OperationExpired(bytes32 indexed id);
    event DelayUpdated(OperationType opType, uint256 oldDelay, uint256 newDelay);
    event EmergencyModeToggled(bool enabled);

    event BatchScheduled(
        bytes32 indexed batchId,
        uint256 operationCount,
        uint256 delay
    );

    event BatchExecuted(bytes32 indexed batchId, uint256 operationCount);

    // ============================================
    // ERRORS
    // ============================================

    error OperationNotFound(bytes32 id);
    error OperationNotReady(bytes32 id);
    error OperationAlreadyExists(bytes32 id);
    error OperationAlreadyExecuted(bytes32 id);
    error OperationCancelledError(bytes32 id);
    error OperationExpiredError(bytes32 id);
    error PredecessorNotExecuted(bytes32 predecessor);
    error InvalidDelay(uint256 delay);
    error InvalidTarget();
    error ExecutionFailed(bytes32 id, bytes reason);
    error ArrayLengthMismatch();
    error EmergencyModeActive();
    error NotEmergencyMode();

    // ============================================
    // MODIFIERS
    // ============================================

    modifier operationExists(bytes32 id) {
        if (operations[id].proposedAt == 0) revert OperationNotFound(id);
        _;
    }

    modifier notEmergency() {
        if (emergencyMode) revert EmergencyModeActive();
        _;
    }

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(
        uint256 _standardDelay,
        uint256 _criticalDelay,
        address admin
    ) {
        require(_standardDelay >= MINIMUM_DELAY, "Delay too short");
        require(_criticalDelay <= MAXIMUM_DELAY, "Delay too long");

        delayConfig = DelayConfig({
            standardDelay: _standardDelay,
            criticalDelay: _criticalDelay,
            emergencyDelay: MINIMUM_DELAY,
            governanceDelay: _criticalDelay * 2,
            upgradeDelay: _criticalDelay * 3
        });

        _grantRole(DEFAULT_ADMIN_ROLE, admin);
        _grantRole(TIMELOCK_ADMIN_ROLE, admin);
        _grantRole(PROPOSER_ROLE, admin);
        _grantRole(EXECUTOR_ROLE, admin);
        _grantRole(CANCELLER_ROLE, admin);
    }

    // ============================================
    // SCHEDULING
    // ============================================

    /**
     * @notice Schedules a new operation
     * @param target Target contract address
     * @param value ETH value to send
     * @param data Calldata for the operation
     * @param predecessor Required predecessor operation
     * @param salt Unique salt for ID generation
     * @param opType Type of operation
     * @param description Human-readable description
     * @return id The operation ID
     */
    function schedule(
        address target,
        uint256 value,
        bytes calldata data,
        bytes32 predecessor,
        bytes32 salt,
        OperationType opType,
        string calldata description
    ) 
        external 
        onlyRole(PROPOSER_ROLE) 
        whenNotPaused 
        notEmergency
        returns (bytes32 id) 
    {
        if (target == address(0)) revert InvalidTarget();

        id = hashOperation(target, value, data, predecessor, salt);
        
        if (operations[id].proposedAt != 0) {
            revert OperationAlreadyExists(id);
        }

        uint256 delay = _getDelay(opType);

        operations[id] = TimelockOperation({
            id: id,
            target: target,
            value: value,
            data: data,
            predecessor: predecessor,
            delay: delay,
            proposedAt: block.timestamp,
            readyAt: block.timestamp + delay,
            executedAt: 0,
            proposer: msg.sender,
            executor: address(0),
            opType: opType,
            state: OperationState.Pending,
            description: description
        });

        operationIds.push(id);
        operationIndex[id] = operationIds.length - 1;
        pendingOperationsCount++;
        totalOperationsProposed++;

        emit OperationScheduled(id, target, value, data, predecessor, delay, msg.sender);
    }

    /**
     * @notice Schedules a batch of operations
     */
    function scheduleBatch(
        BatchOperation calldata batch,
        OperationType opType,
        string calldata description
    )
        external
        onlyRole(PROPOSER_ROLE)
        whenNotPaused
        notEmergency
        returns (bytes32 batchId)
    {
        uint256 length = batch.targets.length;
        if (length != batch.values.length || length != batch.datas.length) {
            revert ArrayLengthMismatch();
        }

        batchId = hashBatch(batch);
        uint256 delay = _getDelay(opType);

        for (uint256 i = 0; i < length; i++) {
            bytes32 opId = hashOperation(
                batch.targets[i],
                batch.values[i],
                batch.datas[i],
                i == 0 ? batch.predecessor : bytes32(0),
                keccak256(abi.encodePacked(batch.salt, i))
            );

            operations[opId] = TimelockOperation({
                id: opId,
                target: batch.targets[i],
                value: batch.values[i],
                data: batch.datas[i],
                predecessor: i == 0 ? batch.predecessor : operationIds[operationIds.length - 1],
                delay: delay,
                proposedAt: block.timestamp,
                readyAt: block.timestamp + delay,
                executedAt: 0,
                proposer: msg.sender,
                executor: address(0),
                opType: opType,
                state: OperationState.Pending,
                description: description
            });

            operationIds.push(opId);
            operationIndex[opId] = operationIds.length - 1;
            pendingOperationsCount++;
            totalOperationsProposed++;

            emit OperationScheduled(
                opId,
                batch.targets[i],
                batch.values[i],
                batch.datas[i],
                operations[opId].predecessor,
                delay,
                msg.sender
            );
        }

        emit BatchScheduled(batchId, length, delay);
    }

    // ============================================
    // EXECUTION
    // ============================================

    /**
     * @notice Executes a ready operation
     * @param id Operation ID to execute
     */
    function execute(bytes32 id)
        external
        payable
        onlyRole(EXECUTOR_ROLE)
        nonReentrant
        whenNotPaused
        operationExists(id)
    {
        TimelockOperation storage op = operations[id];

        // Check state
        if (op.state == OperationState.Executed) {
            revert OperationAlreadyExecuted(id);
        }
        if (op.state == OperationState.Cancelled) {
            revert OperationCancelledError(id);
        }

        // Check if ready
        if (block.timestamp < op.readyAt) {
            revert OperationNotReady(id);
        }

        // Check if expired
        if (block.timestamp > op.readyAt + GRACE_PERIOD) {
            op.state = OperationState.Expired;
            pendingOperationsCount--;
            emit OperationExpired(id);
            revert OperationExpiredError(id);
        }

        // Check predecessor
        if (op.predecessor != bytes32(0)) {
            if (operations[op.predecessor].state != OperationState.Executed) {
                revert PredecessorNotExecuted(op.predecessor);
            }
        }

        // Execute
        op.state = OperationState.Executed;
        op.executedAt = block.timestamp;
        op.executor = msg.sender;
        pendingOperationsCount--;
        totalOperationsExecuted++;

        (bool success, bytes memory returnData) = op.target.call{value: op.value}(op.data);
        if (!success) {
            revert ExecutionFailed(id, returnData);
        }

        emit OperationExecuted(id, op.target, op.value, op.data, msg.sender);
    }

    /**
     * @notice Executes a batch of operations
     */
    function executeBatch(bytes32[] calldata ids)
        external
        payable
        onlyRole(EXECUTOR_ROLE)
        nonReentrant
        whenNotPaused
    {
        for (uint256 i = 0; i < ids.length; i++) {
            TimelockOperation storage op = operations[ids[i]];
            
            if (op.proposedAt == 0) revert OperationNotFound(ids[i]);
            if (op.state != OperationState.Pending) continue;
            if (block.timestamp < op.readyAt) continue;

            if (block.timestamp > op.readyAt + GRACE_PERIOD) {
                op.state = OperationState.Expired;
                pendingOperationsCount--;
                emit OperationExpired(ids[i]);
                continue;
            }

            if (op.predecessor != bytes32(0)) {
                if (operations[op.predecessor].state != OperationState.Executed) {
                    continue;
                }
            }

            op.state = OperationState.Executed;
            op.executedAt = block.timestamp;
            op.executor = msg.sender;
            pendingOperationsCount--;
            totalOperationsExecuted++;

            (bool success, bytes memory returnData) = op.target.call{value: op.value}(op.data);
            if (!success) {
                // Revert state on failure
                op.state = OperationState.Pending;
                op.executedAt = 0;
                op.executor = address(0);
                pendingOperationsCount++;
                totalOperationsExecuted--;
                revert ExecutionFailed(ids[i], returnData);
            }

            emit OperationExecuted(ids[i], op.target, op.value, op.data, msg.sender);
        }
    }

    // ============================================
    // CANCELLATION
    // ============================================

    /**
     * @notice Cancels a pending operation
     * @param id Operation ID to cancel
     */
    function cancel(bytes32 id)
        external
        onlyRole(CANCELLER_ROLE)
        operationExists(id)
    {
        TimelockOperation storage op = operations[id];

        if (op.state == OperationState.Executed) {
            revert OperationAlreadyExecuted(id);
        }
        if (op.state == OperationState.Cancelled) {
            revert OperationCancelledError(id);
        }

        op.state = OperationState.Cancelled;
        pendingOperationsCount--;

        emit OperationCancelled(id, msg.sender);
    }

    /**
     * @notice Cancels multiple operations
     */
    function cancelBatch(bytes32[] calldata ids) external onlyRole(CANCELLER_ROLE) {
        for (uint256 i = 0; i < ids.length; i++) {
            if (operations[ids[i]].proposedAt == 0) continue;
            if (operations[ids[i]].state != OperationState.Pending) continue;

            operations[ids[i]].state = OperationState.Cancelled;
            pendingOperationsCount--;
            emit OperationCancelled(ids[i], msg.sender);
        }
    }

    // ============================================
    // CONFIGURATION
    // ============================================

    /**
     * @notice Updates delay for an operation type
     */
    function setDelay(
        OperationType opType,
        uint256 newDelay
    ) external onlyRole(TIMELOCK_ADMIN_ROLE) {
        if (newDelay < MINIMUM_DELAY || newDelay > MAXIMUM_DELAY) {
            revert InvalidDelay(newDelay);
        }

        uint256 oldDelay;

        if (opType == OperationType.STANDARD) {
            oldDelay = delayConfig.standardDelay;
            delayConfig.standardDelay = newDelay;
        } else if (opType == OperationType.CRITICAL) {
            oldDelay = delayConfig.criticalDelay;
            delayConfig.criticalDelay = newDelay;
        } else if (opType == OperationType.EMERGENCY) {
            oldDelay = delayConfig.emergencyDelay;
            delayConfig.emergencyDelay = newDelay;
        } else if (opType == OperationType.GOVERNANCE) {
            oldDelay = delayConfig.governanceDelay;
            delayConfig.governanceDelay = newDelay;
        } else if (opType == OperationType.UPGRADE) {
            oldDelay = delayConfig.upgradeDelay;
            delayConfig.upgradeDelay = newDelay;
        }

        emit DelayUpdated(opType, oldDelay, newDelay);
    }

    /**
     * @notice Toggles emergency mode
     */
    function setEmergencyMode(bool enabled) external onlyRole(DEFAULT_ADMIN_ROLE) {
        emergencyMode = enabled;
        emit EmergencyModeToggled(enabled);
    }

    /**
     * @notice Gets delay for operation type
     */
    function _getDelay(OperationType opType) internal view returns (uint256) {
        if (opType == OperationType.STANDARD) return delayConfig.standardDelay;
        if (opType == OperationType.CRITICAL) return delayConfig.criticalDelay;
        if (opType == OperationType.EMERGENCY) return delayConfig.emergencyDelay;
        if (opType == OperationType.GOVERNANCE) return delayConfig.governanceDelay;
        if (opType == OperationType.UPGRADE) return delayConfig.upgradeDelay;
        return delayConfig.standardDelay;
    }

    // ============================================
    // HASH FUNCTIONS
    // ============================================

    /**
     * @notice Computes operation hash
     */
    function hashOperation(
        address target,
        uint256 value,
        bytes calldata data,
        bytes32 predecessor,
        bytes32 salt
    ) public pure returns (bytes32) {
        return keccak256(abi.encode(target, value, data, predecessor, salt));
    }

    /**
     * @notice Computes batch hash
     */
    function hashBatch(BatchOperation calldata batch) public pure returns (bytes32) {
        return keccak256(abi.encode(
            batch.targets,
            batch.values,
            batch.datas,
            batch.predecessor,
            batch.salt
        ));
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Gets operation state
     */
    function getOperationState(bytes32 id) external view returns (OperationState) {
        TimelockOperation memory op = operations[id];
        
        if (op.proposedAt == 0) return OperationState.Unset;
        if (op.state == OperationState.Cancelled) return OperationState.Cancelled;
        if (op.state == OperationState.Executed) return OperationState.Executed;
        
        if (block.timestamp > op.readyAt + GRACE_PERIOD) {
            return OperationState.Expired;
        }
        
        if (block.timestamp >= op.readyAt) {
            return OperationState.Ready;
        }
        
        return OperationState.Pending;
    }

    /**
     * @notice Checks if operation is pending
     */
    function isOperationPending(bytes32 id) external view returns (bool) {
        return operations[id].state == OperationState.Pending &&
               block.timestamp < operations[id].readyAt;
    }

    /**
     * @notice Checks if operation is ready
     */
    function isOperationReady(bytes32 id) external view returns (bool) {
        TimelockOperation memory op = operations[id];
        return op.state == OperationState.Pending &&
               block.timestamp >= op.readyAt &&
               block.timestamp <= op.readyAt + GRACE_PERIOD;
    }

    /**
     * @notice Gets remaining time until operation is ready
     */
    function getTimeUntilReady(bytes32 id) external view returns (uint256) {
        TimelockOperation memory op = operations[id];
        if (op.proposedAt == 0 || block.timestamp >= op.readyAt) {
            return 0;
        }
        return op.readyAt - block.timestamp;
    }

    /**
     * @notice Gets all operation IDs
     */
    function getAllOperationIds() external view returns (bytes32[] memory) {
        return operationIds;
    }

    /**
     * @notice Gets pending operations
     */
    function getPendingOperations() external view returns (bytes32[] memory) {
        bytes32[] memory pending = new bytes32[](pendingOperationsCount);
        uint256 idx = 0;

        for (uint256 i = 0; i < operationIds.length && idx < pendingOperationsCount; i++) {
            if (operations[operationIds[i]].state == OperationState.Pending) {
                pending[idx++] = operationIds[i];
            }
        }

        return pending;
    }

    /**
     * @notice Gets operation details
     */
    function getOperation(bytes32 id) external view returns (TimelockOperation memory) {
        return operations[id];
    }

    // ============================================
    // PAUSE FUNCTIONS
    // ============================================

    function pause() external onlyRole(TIMELOCK_ADMIN_ROLE) {
        _pause();
    }

    function unpause() external onlyRole(TIMELOCK_ADMIN_ROLE) {
        _unpause();
    }

    // ============================================
    // RECEIVE
    // ============================================

    receive() external payable {}
}
