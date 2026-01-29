// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts-upgradeable/access/AccessControlUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/PausableUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/ReentrancyGuardUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/Initializable.sol";

/**
 * @title UpgradeRollbackManager
 * @notice Manages upgradeable contract versions with rollback capability
 * @dev Provides safe upgrade paths with checkpointing and emergency rollback
 * 
 * Features:
 * - Version history tracking
 * - Implementation checkpointing
 * - Emergency rollback to previous version
 * - Staged upgrade with validation period
 * - Multi-sig requirement for upgrades
 * - Automatic rollback on critical failures
 */
contract UpgradeRollbackManager is
    Initializable,
    AccessControlUpgradeable,
    PausableUpgradeable,
    ReentrancyGuardUpgradeable
{
    // ============ Constants ============
    bytes32 public constant UPGRADER_ROLE = keccak256("UPGRADER_ROLE");
    bytes32 public constant ROLLBACK_ROLE = keccak256("ROLLBACK_ROLE");
    bytes32 public constant GUARDIAN_ROLE = keccak256("GUARDIAN_ROLE");

    uint256 public constant MAX_VERSIONS_STORED = 10;
    uint256 public constant UPGRADE_DELAY = 48 hours;
    uint256 public constant VALIDATION_PERIOD = 24 hours;

    // ============ Structs ============
    struct VersionInfo {
        address implementation;
        uint256 deployedAt;
        uint256 activatedAt;
        bytes32 codeHash;
        string description;
        bool isActive;
        bool isRolledBack;
    }

    struct PendingUpgrade {
        address newImplementation;
        bytes32 codeHash;
        uint256 scheduledAt;
        uint256 executeAfter;
        string description;
        uint256 approvalCount;
        bool executed;
        bool cancelled;
    }

    struct HealthCheck {
        uint256 lastCheck;
        bool passed;
        string failureReason;
    }

    // ============ State Variables ============
    
    /// @notice Managed proxy address
    address public managedProxy;
    
    /// @notice Current version number
    uint256 public currentVersion;
    
    /// @notice Version history
    mapping(uint256 => VersionInfo) public versions;
    
    /// @notice Pending upgrade proposals
    mapping(bytes32 => PendingUpgrade) public pendingUpgrades;
    
    /// @notice Upgrade approvals
    mapping(bytes32 => mapping(address => bool)) public upgradeApprovals;
    
    /// @notice Required approvals for upgrade
    uint256 public requiredApprovals;
    
    /// @notice Health check status per proxy
    mapping(address => HealthCheck) public healthChecks;
    
    /// @notice Automatic rollback enabled
    bool public autoRollbackEnabled;
    
    /// @notice Failed health checks count
    mapping(address => uint256) public failedHealthChecks;
    
    /// @notice Max failed checks before auto-rollback
    uint256 public maxFailedChecks;
    
    /// @notice All upgrade proposal IDs
    bytes32[] public proposalIds;

    // ============ Events ============
    
    event UpgradeScheduled(
        bytes32 indexed proposalId,
        address indexed newImplementation,
        uint256 executeAfter,
        string description
    );
    
    event UpgradeApproved(
        bytes32 indexed proposalId,
        address indexed approver,
        uint256 totalApprovals
    );
    
    event UpgradeExecuted(
        bytes32 indexed proposalId,
        uint256 newVersion,
        address implementation
    );
    
    event UpgradeCancelled(
        bytes32 indexed proposalId,
        address indexed canceller,
        string reason
    );
    
    event RollbackExecuted(
        uint256 fromVersion,
        uint256 toVersion,
        address indexed executor,
        string reason
    );
    
    event EmergencyRollback(
        uint256 fromVersion,
        uint256 toVersion,
        string reason
    );
    
    event HealthCheckPerformed(
        address indexed proxy,
        bool passed,
        string reason
    );
    
    event AutoRollbackTriggered(
        uint256 fromVersion,
        uint256 toVersion,
        uint256 failedChecks
    );

    // ============ Errors ============
    
    error InvalidImplementation();
    error UpgradeAlreadyPending();
    error UpgradeNotFound();
    error UpgradeNotReady();
    error UpgradeAlreadyExecuted();
    error UpgradeWasCancelled();
    error InsufficientApprovals();
    error AlreadyApproved();
    error NoVersionToRollback();
    error VersionNotFound();
    error RollbackDisabled();
    error HealthCheckNotRequired();
    error InvalidConfiguration();

    // ============ Initializer ============
    
    function initialize(
        address _managedProxy,
        address _admin,
        uint256 _requiredApprovals
    ) external initializer {
        __AccessControl_init();
        __Pausable_init();
        __ReentrancyGuard_init();

        if (_managedProxy == address(0)) revert InvalidImplementation();
        if (_admin == address(0)) revert InvalidImplementation();
        if (_requiredApprovals == 0) revert InvalidConfiguration();

        managedProxy = _managedProxy;
        requiredApprovals = _requiredApprovals;
        autoRollbackEnabled = true;
        maxFailedChecks = 3;

        _grantRole(DEFAULT_ADMIN_ROLE, _admin);
        _grantRole(UPGRADER_ROLE, _admin);
        _grantRole(ROLLBACK_ROLE, _admin);
        _grantRole(GUARDIAN_ROLE, _admin);

        // Record initial version
        currentVersion = 1;
        versions[1] = VersionInfo({
            implementation: _getImplementation(_managedProxy),
            deployedAt: block.timestamp,
            activatedAt: block.timestamp,
            codeHash: _getCodeHash(_getImplementation(_managedProxy)),
            description: "Initial deployment",
            isActive: true,
            isRolledBack: false
        });
    }

    // ============ Upgrade Scheduling ============
    
    /**
     * @notice Schedule a new upgrade
     * @param newImplementation Address of new implementation
     * @param description Upgrade description
     * @return proposalId Unique identifier for this upgrade proposal
     */
    function scheduleUpgrade(
        address newImplementation,
        string calldata description
    ) external onlyRole(UPGRADER_ROLE) returns (bytes32 proposalId) {
        if (newImplementation == address(0)) revert InvalidImplementation();
        if (newImplementation.code.length == 0) revert InvalidImplementation();

        bytes32 codeHash = _getCodeHash(newImplementation);
        proposalId = keccak256(abi.encode(
            newImplementation,
            codeHash,
            block.timestamp,
            description
        ));

        if (pendingUpgrades[proposalId].scheduledAt != 0) {
            revert UpgradeAlreadyPending();
        }

        uint256 executeAfter = block.timestamp + UPGRADE_DELAY;

        pendingUpgrades[proposalId] = PendingUpgrade({
            newImplementation: newImplementation,
            codeHash: codeHash,
            scheduledAt: block.timestamp,
            executeAfter: executeAfter,
            description: description,
            approvalCount: 0,
            executed: false,
            cancelled: false
        });

        proposalIds.push(proposalId);

        emit UpgradeScheduled(proposalId, newImplementation, executeAfter, description);
    }

    /**
     * @notice Approve a scheduled upgrade
     * @param proposalId The upgrade proposal to approve
     */
    function approveUpgrade(bytes32 proposalId) external onlyRole(UPGRADER_ROLE) {
        PendingUpgrade storage upgrade = pendingUpgrades[proposalId];
        
        if (upgrade.scheduledAt == 0) revert UpgradeNotFound();
        if (upgrade.executed) revert UpgradeAlreadyExecuted();
        if (upgrade.cancelled) revert UpgradeWasCancelled();
        if (upgradeApprovals[proposalId][msg.sender]) revert AlreadyApproved();

        upgradeApprovals[proposalId][msg.sender] = true;
        upgrade.approvalCount++;

        emit UpgradeApproved(proposalId, msg.sender, upgrade.approvalCount);
    }

    /**
     * @notice Execute an approved upgrade
     * @param proposalId The upgrade proposal to execute
     */
    function executeUpgrade(bytes32 proposalId) 
        external 
        onlyRole(UPGRADER_ROLE) 
        nonReentrant 
        whenNotPaused 
    {
        PendingUpgrade storage upgrade = pendingUpgrades[proposalId];
        
        if (upgrade.scheduledAt == 0) revert UpgradeNotFound();
        if (upgrade.executed) revert UpgradeAlreadyExecuted();
        if (upgrade.cancelled) revert UpgradeWasCancelled();
        if (block.timestamp < upgrade.executeAfter) revert UpgradeNotReady();
        if (upgrade.approvalCount < requiredApprovals) revert InsufficientApprovals();

        // Verify code hasn't changed
        bytes32 currentCodeHash = _getCodeHash(upgrade.newImplementation);
        if (currentCodeHash != upgrade.codeHash) revert InvalidImplementation();

        // Mark current version as inactive
        versions[currentVersion].isActive = false;

        // Increment version
        currentVersion++;

        // Store new version info
        versions[currentVersion] = VersionInfo({
            implementation: upgrade.newImplementation,
            deployedAt: upgrade.scheduledAt,
            activatedAt: block.timestamp,
            codeHash: upgrade.codeHash,
            description: upgrade.description,
            isActive: true,
            isRolledBack: false
        });

        upgrade.executed = true;

        // Perform the actual upgrade on the proxy
        _upgradeProxy(upgrade.newImplementation);

        emit UpgradeExecuted(proposalId, currentVersion, upgrade.newImplementation);
    }

    /**
     * @notice Cancel a pending upgrade
     * @param proposalId The upgrade proposal to cancel
     * @param reason Reason for cancellation
     */
    function cancelUpgrade(bytes32 proposalId, string calldata reason) 
        external 
        onlyRole(UPGRADER_ROLE) 
    {
        PendingUpgrade storage upgrade = pendingUpgrades[proposalId];
        
        if (upgrade.scheduledAt == 0) revert UpgradeNotFound();
        if (upgrade.executed) revert UpgradeAlreadyExecuted();
        if (upgrade.cancelled) revert UpgradeWasCancelled();

        upgrade.cancelled = true;

        emit UpgradeCancelled(proposalId, msg.sender, reason);
    }

    // ============ Rollback Functions ============
    
    /**
     * @notice Rollback to a previous version
     * @param targetVersion Version number to rollback to
     * @param reason Reason for rollback
     */
    function rollback(uint256 targetVersion, string calldata reason) 
        external 
        onlyRole(ROLLBACK_ROLE) 
        nonReentrant 
        whenNotPaused 
    {
        _executeRollback(targetVersion, reason);
    }

    /**
     * @notice Emergency rollback by guardian (bypasses some checks)
     * @param reason Reason for emergency rollback
     */
    function emergencyRollback(string calldata reason) 
        external 
        onlyRole(GUARDIAN_ROLE) 
        nonReentrant 
    {
        if (currentVersion <= 1) revert NoVersionToRollback();

        uint256 targetVersion = currentVersion - 1;
        
        // Find last non-rolled-back version
        while (targetVersion > 0 && versions[targetVersion].isRolledBack) {
            targetVersion--;
        }

        if (targetVersion == 0) revert NoVersionToRollback();

        _executeRollback(targetVersion, reason);

        emit EmergencyRollback(currentVersion + 1, targetVersion, reason);
    }

    /**
     * @notice Internal rollback execution
     */
    function _executeRollback(uint256 targetVersion, string calldata reason) internal {
        if (targetVersion >= currentVersion) revert VersionNotFound();
        if (targetVersion == 0) revert VersionNotFound();
        if (versions[targetVersion].isRolledBack) revert RollbackDisabled();

        VersionInfo storage targetVersionInfo = versions[targetVersion];
        
        // Verify target implementation still exists
        if (targetVersionInfo.implementation.code.length == 0) {
            revert InvalidImplementation();
        }

        // Mark current version as rolled back
        uint256 previousVersion = currentVersion;
        versions[currentVersion].isActive = false;
        versions[currentVersion].isRolledBack = true;

        // Activate target version
        currentVersion = targetVersion;
        versions[targetVersion].isActive = true;
        versions[targetVersion].activatedAt = block.timestamp;

        // Perform the actual rollback on the proxy
        _upgradeProxy(targetVersionInfo.implementation);

        emit RollbackExecuted(previousVersion, targetVersion, msg.sender, reason);
    }

    // ============ Health Check Functions ============
    
    /**
     * @notice Perform health check on managed proxy
     * @param checkData Calldata for health check
     * @return passed Whether health check passed
     */
    function performHealthCheck(bytes calldata checkData) 
        external 
        returns (bool passed) 
    {
        (bool success, bytes memory result) = managedProxy.call(checkData);
        
        passed = success && (result.length == 0 || abi.decode(result, (bool)));

        healthChecks[managedProxy] = HealthCheck({
            lastCheck: block.timestamp,
            passed: passed,
            failureReason: passed ? "" : "Health check failed"
        });

        if (!passed) {
            failedHealthChecks[managedProxy]++;
            
            // Trigger auto-rollback if enabled and threshold exceeded
            if (autoRollbackEnabled && 
                failedHealthChecks[managedProxy] >= maxFailedChecks &&
                currentVersion > 1) {
                _triggerAutoRollback();
            }
        } else {
            failedHealthChecks[managedProxy] = 0;
        }

        emit HealthCheckPerformed(managedProxy, passed, passed ? "" : "Health check failed");
    }

    /**
     * @notice Trigger automatic rollback
     */
    function _triggerAutoRollback() internal {
        uint256 targetVersion = currentVersion - 1;
        
        while (targetVersion > 0 && versions[targetVersion].isRolledBack) {
            targetVersion--;
        }

        if (targetVersion == 0) return;

        uint256 previousVersion = currentVersion;

        versions[currentVersion].isActive = false;
        versions[currentVersion].isRolledBack = true;
        currentVersion = targetVersion;
        versions[targetVersion].isActive = true;
        versions[targetVersion].activatedAt = block.timestamp;

        _upgradeProxy(versions[targetVersion].implementation);

        emit AutoRollbackTriggered(
            previousVersion, 
            targetVersion, 
            failedHealthChecks[managedProxy]
        );

        failedHealthChecks[managedProxy] = 0;
    }

    // ============ Configuration Functions ============
    
    /**
     * @notice Update required approvals
     */
    function setRequiredApprovals(uint256 _required) 
        external 
        onlyRole(DEFAULT_ADMIN_ROLE) 
    {
        if (_required == 0) revert InvalidConfiguration();
        requiredApprovals = _required;
    }

    /**
     * @notice Enable/disable automatic rollback
     */
    function setAutoRollback(bool enabled) 
        external 
        onlyRole(DEFAULT_ADMIN_ROLE) 
    {
        autoRollbackEnabled = enabled;
    }

    /**
     * @notice Set max failed health checks before auto-rollback
     */
    function setMaxFailedChecks(uint256 _max) 
        external 
        onlyRole(DEFAULT_ADMIN_ROLE) 
    {
        if (_max == 0) revert InvalidConfiguration();
        maxFailedChecks = _max;
    }

    // ============ View Functions ============
    
    /**
     * @notice Get version information
     */
    function getVersion(uint256 versionNumber) 
        external 
        view 
        returns (VersionInfo memory) 
    {
        return versions[versionNumber];
    }

    /**
     * @notice Get current active version info
     */
    function getCurrentVersion() 
        external 
        view 
        returns (VersionInfo memory) 
    {
        return versions[currentVersion];
    }

    /**
     * @notice Get pending upgrade info
     */
    function getPendingUpgrade(bytes32 proposalId) 
        external 
        view 
        returns (PendingUpgrade memory) 
    {
        return pendingUpgrades[proposalId];
    }

    /**
     * @notice Get all pending proposals count
     */
    function getPendingProposalsCount() external view returns (uint256) {
        uint256 count = 0;
        for (uint256 i = 0; i < proposalIds.length; i++) {
            if (!pendingUpgrades[proposalIds[i]].executed && 
                !pendingUpgrades[proposalIds[i]].cancelled) {
                count++;
            }
        }
        return count;
    }

    /**
     * @notice Check if rollback is available
     */
    function canRollback() external view returns (bool, uint256) {
        if (currentVersion <= 1) return (false, 0);
        
        for (uint256 v = currentVersion - 1; v > 0; v--) {
            if (!versions[v].isRolledBack) {
                return (true, v);
            }
        }
        
        return (false, 0);
    }

    // ============ Internal Functions ============
    
    /**
     * @notice Get implementation address from proxy
     */
    function _getImplementation(address proxy) internal view returns (address) {
        // EIP-1967 implementation slot
        bytes32 slot = 0x360894a13ba1a3210667c828492db98dca3e2076cc3735a920a3ca505d382bbc;
        address implementation;
        assembly {
            implementation := sload(slot)
        }
        
        // If not using EIP-1967, try to read from proxy
        if (implementation == address(0)) {
            // Fallback: use the proxy address itself for simple proxies
            implementation = proxy;
        }
        
        return implementation;
    }

    /**
     * @notice Get code hash of an address
     */
    function _getCodeHash(address addr) internal view returns (bytes32) {
        return addr.codehash;
    }

    /**
     * @notice Upgrade the managed proxy
     * @dev Override this for specific proxy types
     */
    function _upgradeProxy(address newImplementation) internal virtual {
        // This is a placeholder - actual implementation depends on proxy type
        // For TransparentUpgradeableProxy, call upgradeTo through admin
        // For UUPS, call upgradeTo on proxy directly
        
        (bool success, ) = managedProxy.call(
            abi.encodeWithSignature("upgradeTo(address)", newImplementation)
        );
        
        require(success, "Upgrade failed");
    }

    // ============ Emergency Functions ============
    
    /**
     * @notice Pause all upgrade/rollback operations
     */
    function pause() external onlyRole(GUARDIAN_ROLE) {
        _pause();
    }

    /**
     * @notice Unpause operations
     */
    function unpause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        _unpause();
    }
}
