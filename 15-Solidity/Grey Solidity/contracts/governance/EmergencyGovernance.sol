// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/Pausable.sol";

/**
 * @title EmergencyGovernance
 * @notice Unified emergency and pause system for the protocol
 * @dev Provides tiered emergency response capabilities with multi-sig requirements
 *
 * Features:
 * - Tiered emergency levels (NORMAL, ELEVATED, CRITICAL, EMERGENCY)
 * - Module-specific pause controls
 * - Global pause capability
 * - Multi-sig threshold for critical actions
 * - Emergency action queue
 * - Recovery procedures
 * - Event logging for all state changes
 * - Time-locked recovery for maximum security
 */
contract EmergencyGovernance is AccessControl, ReentrancyGuard, Pausable {
    // ============ Enums ============

    enum EmergencyLevel {
        NORMAL,     // Standard operations
        ELEVATED,   // Increased monitoring, restricted operations
        CRITICAL,   // Limited operations, prepare for emergency
        EMERGENCY   // Full lockdown
    }

    enum ModuleStatus {
        ACTIVE,
        PAUSED,
        DEPRECATED
    }

    // ============ Structs ============

    struct Module {
        string name;
        address moduleAddress;
        ModuleStatus status;
        bool canPause;
        uint256 pausedAt;
        address pausedBy;
    }

    struct EmergencyAction {
        bytes32 actionId;
        string description;
        address target;
        bytes callData;
        uint256 proposedAt;
        uint256 executeAfter;
        uint256 expiresAt;
        uint256 approvalCount;
        bool executed;
        bool cancelled;
    }

    struct Guardian {
        address guardianAddress;
        bool isActive;
        uint256 addedAt;
        uint256 actionsApproved;
    }

    // ============ Constants ============

    bytes32 public constant GUARDIAN_ROLE = keccak256("GUARDIAN_ROLE");
    bytes32 public constant PAUSER_ROLE = keccak256("PAUSER_ROLE");
    bytes32 public constant RECOVERY_ROLE = keccak256("RECOVERY_ROLE");

    uint256 public constant MIN_GUARDIANS = 3;
    uint256 public constant MAX_GUARDIANS = 11;
    uint256 public constant EMERGENCY_ACTION_DELAY = 1 hours;
    uint256 public constant EMERGENCY_ACTION_EXPIRY = 7 days;
    uint256 public constant RECOVERY_DELAY = 3 days;

    // ============ State Variables ============

    /// @notice Current emergency level
    EmergencyLevel public emergencyLevel;

    /// @notice Global pause state
    bool public isGloballyPaused;

    /// @notice Registered modules
    mapping(bytes32 => Module) public modules;
    bytes32[] public moduleIds;

    /// @notice Emergency actions
    mapping(bytes32 => EmergencyAction) public emergencyActions;
    bytes32[] public actionIds;

    /// @notice Action approvals
    mapping(bytes32 => mapping(address => bool)) public actionApprovals;

    /// @notice Guardians
    mapping(address => Guardian) public guardians;
    address[] public guardianList;
    uint256 public activeGuardianCount;

    /// @notice Required approvals for each emergency level
    mapping(EmergencyLevel => uint256) public levelThresholds;

    /// @notice Timestamp of last level change
    uint256 public lastLevelChange;

    /// @notice Recovery state
    bool public recoveryInitiated;
    uint256 public recoveryInitiatedAt;
    address public recoveryInitiator;

    /// @notice Action nonce for unique IDs
    uint256 private _actionNonce;

    // ============ Events ============

    event EmergencyLevelChanged(
        EmergencyLevel indexed oldLevel,
        EmergencyLevel indexed newLevel,
        address indexed changedBy,
        string reason
    );
    event ModuleRegistered(bytes32 indexed moduleId, string name, address moduleAddress);
    event ModulePaused(bytes32 indexed moduleId, address indexed pausedBy);
    event ModuleUnpaused(bytes32 indexed moduleId, address indexed unpausedBy);
    event ModuleDeprecated(bytes32 indexed moduleId);
    event GlobalPauseActivated(address indexed activatedBy);
    event GlobalPauseDeactivated(address indexed deactivatedBy);
    event GuardianAdded(address indexed guardian);
    event GuardianRemoved(address indexed guardian);
    event EmergencyActionProposed(
        bytes32 indexed actionId,
        address indexed proposer,
        address target,
        string description
    );
    event EmergencyActionApproved(bytes32 indexed actionId, address indexed approver);
    event EmergencyActionExecuted(bytes32 indexed actionId, address indexed executor);
    event EmergencyActionCancelled(bytes32 indexed actionId, address indexed canceller);
    event RecoveryInitiated(address indexed initiator);
    event RecoveryCancelled(address indexed canceller);
    event RecoveryExecuted(address indexed executor);

    // ============ Errors ============

    error InvalidEmergencyLevel();
    error ModuleAlreadyRegistered();
    error ModuleNotFound();
    error ModuleNotPausable();
    error ModuleAlreadyPaused();
    error ModuleNotPaused();
    error GloballyPaused();
    error NotGloballyPaused();
    error InvalidGuardian();
    error GuardianAlreadyExists();
    error GuardianNotFound();
    error TooFewGuardians();
    error TooManyGuardians();
    error ActionNotFound();
    error ActionAlreadyExecuted();
    error ActionExpired();
    error ActionNotReady();
    error AlreadyApproved();
    error InsufficientApprovals();
    error RecoveryAlreadyInitiated();
    error RecoveryNotInitiated();
    error RecoveryNotReady();
    error InvalidThreshold();
    error ActionCancelled();

    // ============ Modifiers ============

    modifier whenModuleActive(bytes32 moduleId) {
        if (modules[moduleId].status != ModuleStatus.ACTIVE) revert ModuleNotFound();
        if (isGloballyPaused) revert GloballyPaused();
        _;
    }

    modifier onlyGuardian() {
        if (!guardians[msg.sender].isActive) revert InvalidGuardian();
        _;
    }

    // ============ Constructor ============

    constructor(address[] memory _initialGuardians, address _admin) {
        if (_initialGuardians.length < MIN_GUARDIANS) revert TooFewGuardians();
        if (_initialGuardians.length > MAX_GUARDIANS) revert TooManyGuardians();

        _grantRole(DEFAULT_ADMIN_ROLE, _admin);
        _grantRole(PAUSER_ROLE, _admin);
        _grantRole(RECOVERY_ROLE, _admin);

        for (uint256 i = 0; i < _initialGuardians.length; i++) {
            _addGuardian(_initialGuardians[i]);
        }

        // Set default thresholds
        levelThresholds[EmergencyLevel.NORMAL] = 1;
        levelThresholds[EmergencyLevel.ELEVATED] = 2;
        levelThresholds[EmergencyLevel.CRITICAL] = (_initialGuardians.length + 1) / 2;
        levelThresholds[EmergencyLevel.EMERGENCY] = (_initialGuardians.length * 2) / 3;

        emergencyLevel = EmergencyLevel.NORMAL;
    }

    // ============ View Functions ============

    /**
     * @notice Get all registered module IDs
     */
    function getAllModules() external view returns (bytes32[] memory) {
        return moduleIds;
    }

    /**
     * @notice Get module info
     */
    function getModuleInfo(bytes32 moduleId) external view returns (
        string memory name,
        address moduleAddress,
        ModuleStatus status,
        bool canPause,
        uint256 pausedAt,
        address pausedBy
    ) {
        Module storage m = modules[moduleId];
        return (m.name, m.moduleAddress, m.status, m.canPause, m.pausedAt, m.pausedBy);
    }

    /**
     * @notice Check if module is operational
     */
    function isModuleOperational(bytes32 moduleId) external view returns (bool) {
        if (isGloballyPaused) return false;
        if (emergencyLevel == EmergencyLevel.EMERGENCY) return false;
        return modules[moduleId].status == ModuleStatus.ACTIVE;
    }

    /**
     * @notice Get all active guardians
     */
    function getActiveGuardians() external view returns (address[] memory) {
        address[] memory active = new address[](activeGuardianCount);
        uint256 index = 0;
        for (uint256 i = 0; i < guardianList.length; i++) {
            if (guardians[guardianList[i]].isActive) {
                active[index++] = guardianList[i];
            }
        }
        return active;
    }

    /**
     * @notice Get action details
     */
    function getActionDetails(bytes32 actionId) external view returns (EmergencyAction memory) {
        return emergencyActions[actionId];
    }

    /**
     * @notice Get required threshold for current level
     */
    function getCurrentThreshold() external view returns (uint256) {
        return levelThresholds[emergencyLevel];
    }

    // ============ Module Management ============

    /**
     * @notice Register a new module
     */
    function registerModule(
        bytes32 moduleId,
        string calldata name,
        address moduleAddress,
        bool canPause
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (modules[moduleId].moduleAddress != address(0)) revert ModuleAlreadyRegistered();

        modules[moduleId] = Module({
            name: name,
            moduleAddress: moduleAddress,
            status: ModuleStatus.ACTIVE,
            canPause: canPause,
            pausedAt: 0,
            pausedBy: address(0)
        });
        moduleIds.push(moduleId);

        emit ModuleRegistered(moduleId, name, moduleAddress);
    }

    /**
     * @notice Pause a specific module
     */
    function pauseModule(bytes32 moduleId) external onlyRole(PAUSER_ROLE) {
        Module storage m = modules[moduleId];
        if (m.moduleAddress == address(0)) revert ModuleNotFound();
        if (!m.canPause) revert ModuleNotPausable();
        if (m.status == ModuleStatus.PAUSED) revert ModuleAlreadyPaused();

        m.status = ModuleStatus.PAUSED;
        m.pausedAt = block.timestamp;
        m.pausedBy = msg.sender;

        // Try to call pause on the module if it implements Pausable
        (bool success,) = m.moduleAddress.call(abi.encodeWithSignature("pause()"));
        // We don't require success - not all modules may implement this

        emit ModulePaused(moduleId, msg.sender);
    }

    /**
     * @notice Unpause a specific module
     */
    function unpauseModule(bytes32 moduleId) external onlyRole(PAUSER_ROLE) {
        Module storage m = modules[moduleId];
        if (m.moduleAddress == address(0)) revert ModuleNotFound();
        if (m.status != ModuleStatus.PAUSED) revert ModuleNotPaused();
        if (emergencyLevel == EmergencyLevel.EMERGENCY) revert InvalidEmergencyLevel();

        m.status = ModuleStatus.ACTIVE;
        m.pausedAt = 0;
        m.pausedBy = address(0);

        // Try to call unpause on the module
        (bool success,) = m.moduleAddress.call(abi.encodeWithSignature("unpause()"));

        emit ModuleUnpaused(moduleId, msg.sender);
    }

    /**
     * @notice Deprecate a module permanently
     */
    function deprecateModule(bytes32 moduleId) external onlyRole(DEFAULT_ADMIN_ROLE) {
        Module storage m = modules[moduleId];
        if (m.moduleAddress == address(0)) revert ModuleNotFound();

        m.status = ModuleStatus.DEPRECATED;
        emit ModuleDeprecated(moduleId);
    }

    // ============ Global Pause ============

    /**
     * @notice Activate global pause
     */
    function activateGlobalPause() external onlyRole(PAUSER_ROLE) {
        if (isGloballyPaused) revert GloballyPaused();
        isGloballyPaused = true;
        _pause();
        emit GlobalPauseActivated(msg.sender);
    }

    /**
     * @notice Deactivate global pause
     */
    function deactivateGlobalPause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (!isGloballyPaused) revert NotGloballyPaused();
        if (emergencyLevel == EmergencyLevel.EMERGENCY) revert InvalidEmergencyLevel();
        isGloballyPaused = false;
        _unpause();
        emit GlobalPauseDeactivated(msg.sender);
    }

    // ============ Emergency Level Management ============

    /**
     * @notice Change emergency level
     */
    function setEmergencyLevel(
        EmergencyLevel newLevel,
        string calldata reason
    ) external onlyGuardian {
        EmergencyLevel oldLevel = emergencyLevel;
        
        // Higher levels require more approvals - simplified for single guardian action
        // In production, this would use multi-sig threshold
        if (newLevel > oldLevel) {
            // Escalation - always allowed for guardians
            emergencyLevel = newLevel;
            lastLevelChange = block.timestamp;
            emit EmergencyLevelChanged(oldLevel, newLevel, msg.sender, reason);
            
            // Auto-pause all modules on EMERGENCY
            if (newLevel == EmergencyLevel.EMERGENCY && !isGloballyPaused) {
                isGloballyPaused = true;
                _pause();
                emit GlobalPauseActivated(msg.sender);
            }
        } else {
            // De-escalation requires admin
            if (!hasRole(DEFAULT_ADMIN_ROLE, msg.sender)) {
                revert InvalidEmergencyLevel();
            }
            emergencyLevel = newLevel;
            lastLevelChange = block.timestamp;
            emit EmergencyLevelChanged(oldLevel, newLevel, msg.sender, reason);
        }
    }

    // ============ Guardian Management ============

    function addGuardian(address guardian) external onlyRole(DEFAULT_ADMIN_ROLE) {
        _addGuardian(guardian);
    }

    function removeGuardian(address guardian) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (!guardians[guardian].isActive) revert GuardianNotFound();
        if (activeGuardianCount <= MIN_GUARDIANS) revert TooFewGuardians();

        guardians[guardian].isActive = false;
        activeGuardianCount--;
        _revokeRole(GUARDIAN_ROLE, guardian);

        emit GuardianRemoved(guardian);
    }

    function _addGuardian(address guardian) internal {
        if (guardian == address(0)) revert InvalidGuardian();
        if (guardians[guardian].isActive) revert GuardianAlreadyExists();
        if (activeGuardianCount >= MAX_GUARDIANS) revert TooManyGuardians();

        guardians[guardian] = Guardian({
            guardianAddress: guardian,
            isActive: true,
            addedAt: block.timestamp,
            actionsApproved: 0
        });
        guardianList.push(guardian);
        activeGuardianCount++;

        _grantRole(GUARDIAN_ROLE, guardian);
        _grantRole(PAUSER_ROLE, guardian);

        emit GuardianAdded(guardian);
    }

    // ============ Emergency Actions ============

    /**
     * @notice Propose an emergency action
     */
    function proposeEmergencyAction(
        address target,
        bytes calldata callData,
        string calldata description
    ) external onlyGuardian returns (bytes32) {
        bytes32 actionId = keccak256(abi.encodePacked(
            target,
            callData,
            block.timestamp,
            _actionNonce++
        ));

        emergencyActions[actionId] = EmergencyAction({
            actionId: actionId,
            description: description,
            target: target,
            callData: callData,
            proposedAt: block.timestamp,
            executeAfter: block.timestamp + EMERGENCY_ACTION_DELAY,
            expiresAt: block.timestamp + EMERGENCY_ACTION_EXPIRY,
            approvalCount: 1,
            executed: false,
            cancelled: false
        });
        actionIds.push(actionId);

        // Proposer automatically approves
        actionApprovals[actionId][msg.sender] = true;
        guardians[msg.sender].actionsApproved++;

        emit EmergencyActionProposed(actionId, msg.sender, target, description);
        emit EmergencyActionApproved(actionId, msg.sender);

        return actionId;
    }

    /**
     * @notice Approve an emergency action
     */
    function approveEmergencyAction(bytes32 actionId) external onlyGuardian {
        EmergencyAction storage action = emergencyActions[actionId];
        if (action.proposedAt == 0) revert ActionNotFound();
        if (action.executed) revert ActionAlreadyExecuted();
        if (action.cancelled) revert ActionCancelled();
        if (block.timestamp > action.expiresAt) revert ActionExpired();
        if (actionApprovals[actionId][msg.sender]) revert AlreadyApproved();

        actionApprovals[actionId][msg.sender] = true;
        action.approvalCount++;
        guardians[msg.sender].actionsApproved++;

        emit EmergencyActionApproved(actionId, msg.sender);
    }

    /**
     * @notice Execute an approved emergency action
     */
    function executeEmergencyAction(bytes32 actionId) external onlyGuardian nonReentrant {
        EmergencyAction storage action = emergencyActions[actionId];
        if (action.proposedAt == 0) revert ActionNotFound();
        if (action.executed) revert ActionAlreadyExecuted();
        if (action.cancelled) revert ActionCancelled();
        if (block.timestamp > action.expiresAt) revert ActionExpired();
        if (block.timestamp < action.executeAfter) revert ActionNotReady();
        if (action.approvalCount < levelThresholds[emergencyLevel]) revert InsufficientApprovals();

        action.executed = true;

        (bool success,) = action.target.call(action.callData);
        require(success, "Action execution failed");

        emit EmergencyActionExecuted(actionId, msg.sender);
    }

    /**
     * @notice Cancel an emergency action
     */
    function cancelEmergencyAction(bytes32 actionId) external onlyRole(DEFAULT_ADMIN_ROLE) {
        EmergencyAction storage action = emergencyActions[actionId];
        if (action.proposedAt == 0) revert ActionNotFound();
        if (action.executed) revert ActionAlreadyExecuted();
        if (action.cancelled) revert ActionCancelled();

        action.cancelled = true;
        emit EmergencyActionCancelled(actionId, msg.sender);
    }

    // ============ Recovery ============

    /**
     * @notice Initiate recovery process
     */
    function initiateRecovery() external onlyRole(RECOVERY_ROLE) {
        if (recoveryInitiated) revert RecoveryAlreadyInitiated();

        recoveryInitiated = true;
        recoveryInitiatedAt = block.timestamp;
        recoveryInitiator = msg.sender;

        emit RecoveryInitiated(msg.sender);
    }

    /**
     * @notice Cancel recovery
     */
    function cancelRecovery() external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (!recoveryInitiated) revert RecoveryNotInitiated();

        recoveryInitiated = false;
        recoveryInitiatedAt = 0;
        recoveryInitiator = address(0);

        emit RecoveryCancelled(msg.sender);
    }

    /**
     * @notice Execute recovery - resets to normal state
     */
    function executeRecovery() external onlyRole(RECOVERY_ROLE) nonReentrant {
        if (!recoveryInitiated) revert RecoveryNotInitiated();
        if (block.timestamp < recoveryInitiatedAt + RECOVERY_DELAY) revert RecoveryNotReady();

        // Reset emergency state
        emergencyLevel = EmergencyLevel.NORMAL;
        isGloballyPaused = false;
        if (paused()) {
            _unpause();
        }

        // Reset all paused modules
        for (uint256 i = 0; i < moduleIds.length; i++) {
            Module storage m = modules[moduleIds[i]];
            if (m.status == ModuleStatus.PAUSED) {
                m.status = ModuleStatus.ACTIVE;
                m.pausedAt = 0;
                m.pausedBy = address(0);
            }
        }

        recoveryInitiated = false;
        recoveryInitiatedAt = 0;
        recoveryInitiator = address(0);

        emit RecoveryExecuted(msg.sender);
    }

    // ============ Configuration ============

    /**
     * @notice Set threshold for emergency level
     */
    function setLevelThreshold(
        EmergencyLevel level,
        uint256 threshold
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (threshold == 0 || threshold > activeGuardianCount) revert InvalidThreshold();
        levelThresholds[level] = threshold;
    }
}
