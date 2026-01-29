// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts-upgradeable/access/AccessControlUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/PausableUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/Initializable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/UUPSUpgradeable.sol";

/**
 * @title CircuitBreaker
 * @author Grey Protocol Team
 * @notice Emergency circuit breaker for protocol protection
 * @dev Provides automatic and manual circuit breaking capabilities
 * 
 * Features:
 * - Automatic threshold-based triggers
 * - Manual emergency stops
 * - Cooldown periods
 * - Multi-level alerts (Warning, Critical, Emergency)
 * - Metrics tracking
 * - Recovery procedures
 * - Role-based access
 */
contract CircuitBreaker is
    Initializable,
    AccessControlUpgradeable,
    PausableUpgradeable,
    UUPSUpgradeable
{
    // ============================================
    // CONSTANTS & ROLES
    // ============================================

    bytes32 public constant GUARDIAN_ROLE = keccak256("GUARDIAN_ROLE");
    bytes32 public constant OPERATOR_ROLE = keccak256("OPERATOR_ROLE");
    bytes32 public constant UPGRADER_ROLE = keccak256("UPGRADER_ROLE");

    /// @notice Maximum trip count before lockdown
    uint256 public constant MAX_TRIPS_BEFORE_LOCKDOWN = 3;

    // ============================================
    // ENUMS
    // ============================================

    /**
     * @notice Alert severity levels
     */
    enum AlertLevel {
        NONE,
        WARNING,
        CRITICAL,
        EMERGENCY
    }

    /**
     * @notice Breaker states
     */
    enum BreakerState {
        CLOSED,     // Normal operation
        OPEN,       // Tripped, blocking operations
        HALF_OPEN,  // Testing if issues resolved
        LOCKED      // Requires manual intervention
    }

    /**
     * @notice Metric types for threshold checking
     */
    enum MetricType {
        VOLUME,         // Transaction volume
        VALUE,          // Total value
        RATE,           // Operations per time
        PRICE_CHANGE,   // Price movement
        GAS_PRICE,      // Network gas price
        ERROR_RATE,     // Operation failure rate
        CUSTOM          // Custom metric
    }

    // ============================================
    // STRUCTS
    // ============================================

    /**
     * @notice Threshold configuration
     * @param warningThreshold Warning level threshold
     * @param criticalThreshold Critical level threshold
     * @param emergencyThreshold Emergency level threshold
     * @param window Time window for measurement (seconds)
     * @param isEnabled Whether this threshold is active
     * @param cooldownPeriod Cooldown after trip
     */
    struct ThresholdConfig {
        uint256 warningThreshold;
        uint256 criticalThreshold;
        uint256 emergencyThreshold;
        uint256 window;
        bool isEnabled;
        uint256 cooldownPeriod;
    }

    /**
     * @notice Metric data
     * @param metricType Type of metric
     * @param value Current value
     * @param timestamp Last update time
     * @param windowStart Window start time
     * @param windowValue Value within current window
     */
    struct MetricData {
        MetricType metricType;
        uint256 value;
        uint256 timestamp;
        uint256 windowStart;
        uint256 windowValue;
    }

    /**
     * @notice Breaker status
     */
    struct BreakerStatus {
        BreakerState state;
        AlertLevel alertLevel;
        uint256 lastTrip;
        uint256 tripCount;
        uint256 cooldownEnd;
        string lastTripReason;
    }

    /**
     * @notice Trip event record
     */
    struct TripRecord {
        uint256 timestamp;
        AlertLevel level;
        MetricType metric;
        uint256 value;
        uint256 threshold;
        string reason;
        address triggeredBy;
    }

    /**
     * @notice Protected contract registration
     */
    struct ProtectedContract {
        address contractAddress;
        bool isActive;
        bool isPaused;
        uint256 lastPause;
        uint256 pauseCount;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Current breaker status
    BreakerStatus public status;

    /// @notice Threshold configurations by metric type
    mapping(MetricType => ThresholdConfig) public thresholds;

    /// @notice Current metric values
    mapping(MetricType => MetricData) public metrics;

    /// @notice Protected contracts
    mapping(address => ProtectedContract) public protectedContracts;

    /// @notice List of protected contract addresses
    address[] public protectedList;

    /// @notice Trip history
    TripRecord[] public tripHistory;

    /// @notice Auto-recovery enabled
    bool public autoRecoveryEnabled;

    /// @notice Recovery delay after cooldown
    uint256 public recoveryDelay;

    /// @notice Global pause duration
    uint256 public globalPauseDuration;

    /// @notice Total trips since deployment
    uint256 public totalTrips;

    /// @notice Storage gap
    uint256[40] private __gap;

    // ============================================
    // EVENTS
    // ============================================

    event BreakerTripped(
        AlertLevel indexed level,
        MetricType indexed metric,
        uint256 value,
        uint256 threshold,
        string reason
    );

    event BreakerReset(address indexed by, string reason);
    event AlertLevelChanged(AlertLevel indexed oldLevel, AlertLevel indexed newLevel);
    event StateChanged(BreakerState indexed oldState, BreakerState indexed newState);
    event ContractPaused(address indexed contractAddress, string reason);
    event ContractUnpaused(address indexed contractAddress);
    event ThresholdUpdated(MetricType indexed metric, uint256 warning, uint256 critical, uint256 emergency);
    event MetricUpdated(MetricType indexed metric, uint256 value);
    event ProtectedContractAdded(address indexed contractAddress);
    event ProtectedContractRemoved(address indexed contractAddress);
    event AutoRecoveryTriggered(uint256 timestamp);
    event LockdownActivated(address indexed by, string reason);

    // ============================================
    // ERRORS
    // ============================================

    error BreakerOpen();
    error BreakerLocked();
    error InvalidThreshold();
    error CooldownActive(uint256 remaining);
    error ContractNotProtected(address contractAddress);
    error AlreadyProtected(address contractAddress);
    error InvalidMetric();
    error RecoveryNotAllowed();
    error ZeroAddress();

    // ============================================
    // MODIFIERS
    // ============================================

    modifier whenBreakerClosed() {
        if (status.state == BreakerState.OPEN) revert BreakerOpen();
        if (status.state == BreakerState.LOCKED) revert BreakerLocked();
        _;
    }

    modifier afterCooldown() {
        if (block.timestamp < status.cooldownEnd) {
            revert CooldownActive(status.cooldownEnd - block.timestamp);
        }
        _;
    }

    // ============================================
    // INITIALIZER
    // ============================================

    /**
     * @notice Initializes the circuit breaker
     * @param admin Admin address
     */
    function initialize(address admin) public initializer {
        if (admin == address(0)) revert ZeroAddress();

        __AccessControl_init();
        __Pausable_init();
        __UUPSUpgradeable_init();

        status = BreakerStatus({
            state: BreakerState.CLOSED,
            alertLevel: AlertLevel.NONE,
            lastTrip: 0,
            tripCount: 0,
            cooldownEnd: 0,
            lastTripReason: ""
        });

        autoRecoveryEnabled = true;
        recoveryDelay = 1 hours;
        globalPauseDuration = 1 hours;

        _grantRole(DEFAULT_ADMIN_ROLE, admin);
        _grantRole(GUARDIAN_ROLE, admin);
        _grantRole(OPERATOR_ROLE, admin);
        _grantRole(UPGRADER_ROLE, admin);

        // Initialize default thresholds
        _initializeDefaultThresholds();
    }

    /**
     * @notice Sets up default thresholds
     */
    function _initializeDefaultThresholds() internal {
        // Volume threshold (transactions per hour)
        thresholds[MetricType.VOLUME] = ThresholdConfig({
            warningThreshold: 10000,
            criticalThreshold: 50000,
            emergencyThreshold: 100000,
            window: 1 hours,
            isEnabled: true,
            cooldownPeriod: 30 minutes
        });

        // Value threshold (wei per hour)
        thresholds[MetricType.VALUE] = ThresholdConfig({
            warningThreshold: 1000 ether,
            criticalThreshold: 5000 ether,
            emergencyThreshold: 10000 ether,
            window: 1 hours,
            isEnabled: true,
            cooldownPeriod: 1 hours
        });

        // Rate threshold (operations per minute)
        thresholds[MetricType.RATE] = ThresholdConfig({
            warningThreshold: 100,
            criticalThreshold: 500,
            emergencyThreshold: 1000,
            window: 1 minutes,
            isEnabled: true,
            cooldownPeriod: 15 minutes
        });

        // Price change threshold (basis points)
        thresholds[MetricType.PRICE_CHANGE] = ThresholdConfig({
            warningThreshold: 500,   // 5%
            criticalThreshold: 1000, // 10%
            emergencyThreshold: 2000, // 20%
            window: 5 minutes,
            isEnabled: true,
            cooldownPeriod: 30 minutes
        });

        // Error rate threshold (percentage * 100)
        thresholds[MetricType.ERROR_RATE] = ThresholdConfig({
            warningThreshold: 500,   // 5%
            criticalThreshold: 1000, // 10%
            emergencyThreshold: 2500, // 25%
            window: 10 minutes,
            isEnabled: true,
            cooldownPeriod: 15 minutes
        });
    }

    // ============================================
    // METRIC UPDATES
    // ============================================

    /**
     * @notice Records a metric value
     * @param metricType Type of metric
     * @param value Metric value
     */
    function recordMetric(
        MetricType metricType,
        uint256 value
    ) external onlyRole(OPERATOR_ROLE) {
        ThresholdConfig storage config = thresholds[metricType];
        if (!config.isEnabled) return;

        MetricData storage metric = metrics[metricType];
        
        // Check if window needs reset
        if (block.timestamp >= metric.windowStart + config.window) {
            metric.windowStart = block.timestamp;
            metric.windowValue = value;
        } else {
            metric.windowValue += value;
        }

        metric.value = value;
        metric.timestamp = block.timestamp;
        metric.metricType = metricType;

        emit MetricUpdated(metricType, value);

        // Check thresholds
        _checkThresholds(metricType, metric.windowValue);
    }

    /**
     * @notice Batch records multiple metrics
     */
    function recordMetrics(
        MetricType[] calldata metricTypes,
        uint256[] calldata values
    ) external onlyRole(OPERATOR_ROLE) {
        require(metricTypes.length == values.length, "Length mismatch");

        for (uint256 i = 0; i < metricTypes.length; i++) {
            ThresholdConfig storage config = thresholds[metricTypes[i]];
            if (!config.isEnabled) continue;

            MetricData storage metric = metrics[metricTypes[i]];
            
            if (block.timestamp >= metric.windowStart + config.window) {
                metric.windowStart = block.timestamp;
                metric.windowValue = values[i];
            } else {
                metric.windowValue += values[i];
            }

            metric.value = values[i];
            metric.timestamp = block.timestamp;
            metric.metricType = metricTypes[i];

            _checkThresholds(metricTypes[i], metric.windowValue);
        }
    }

    // ============================================
    // THRESHOLD CHECKING
    // ============================================

    /**
     * @notice Checks if value exceeds thresholds
     */
    function _checkThresholds(
        MetricType metricType,
        uint256 value
    ) internal {
        ThresholdConfig storage config = thresholds[metricType];
        AlertLevel newLevel = AlertLevel.NONE;

        if (value >= config.emergencyThreshold) {
            newLevel = AlertLevel.EMERGENCY;
        } else if (value >= config.criticalThreshold) {
            newLevel = AlertLevel.CRITICAL;
        } else if (value >= config.warningThreshold) {
            newLevel = AlertLevel.WARNING;
        }

        if (newLevel > status.alertLevel) {
            AlertLevel oldLevel = status.alertLevel;
            status.alertLevel = newLevel;
            emit AlertLevelChanged(oldLevel, newLevel);

            if (newLevel >= AlertLevel.CRITICAL) {
                _tripBreaker(newLevel, metricType, value, config.cooldownPeriod, "Threshold exceeded");
            }
        }
    }

    // ============================================
    // BREAKER CONTROL
    // ============================================

    /**
     * @notice Trips the circuit breaker
     */
    function _tripBreaker(
        AlertLevel level,
        MetricType metric,
        uint256 value,
        uint256 cooldown,
        string memory reason
    ) internal {
        BreakerState oldState = status.state;
        status.state = BreakerState.OPEN;
        status.lastTrip = block.timestamp;
        status.tripCount++;
        status.cooldownEnd = block.timestamp + cooldown;
        status.lastTripReason = reason;
        totalTrips++;

        // Record trip
        tripHistory.push(TripRecord({
            timestamp: block.timestamp,
            level: level,
            metric: metric,
            value: value,
            threshold: _getThreshold(metric, level),
            reason: reason,
            triggeredBy: msg.sender
        }));

        // Check for lockdown
        if (status.tripCount >= MAX_TRIPS_BEFORE_LOCKDOWN) {
            status.state = BreakerState.LOCKED;
            emit LockdownActivated(msg.sender, "Too many trips");
        }

        // Pause all protected contracts
        _pauseAllProtected();

        emit StateChanged(oldState, status.state);
        emit BreakerTripped(level, metric, value, _getThreshold(metric, level), reason);
    }

    /**
     * @notice Gets threshold for level
     */
    function _getThreshold(
        MetricType metric,
        AlertLevel level
    ) internal view returns (uint256) {
        ThresholdConfig storage config = thresholds[metric];
        if (level == AlertLevel.EMERGENCY) return config.emergencyThreshold;
        if (level == AlertLevel.CRITICAL) return config.criticalThreshold;
        if (level == AlertLevel.WARNING) return config.warningThreshold;
        return 0;
    }

    /**
     * @notice Manual emergency trip
     */
    function emergencyTrip(string calldata reason) external onlyRole(GUARDIAN_ROLE) {
        _tripBreaker(
            AlertLevel.EMERGENCY,
            MetricType.CUSTOM,
            0,
            globalPauseDuration,
            reason
        );
    }

    /**
     * @notice Resets the breaker after cooldown
     */
    function resetBreaker(string calldata reason) 
        external 
        onlyRole(GUARDIAN_ROLE) 
        afterCooldown 
    {
        if (status.state == BreakerState.LOCKED) {
            revert BreakerLocked();
        }

        BreakerState oldState = status.state;
        status.state = BreakerState.CLOSED;
        status.alertLevel = AlertLevel.NONE;
        status.tripCount = 0;

        // Unpause protected contracts
        _unpauseAllProtected();

        emit StateChanged(oldState, BreakerState.CLOSED);
        emit BreakerReset(msg.sender, reason);
    }

    /**
     * @notice Unlocks a locked breaker (admin only)
     */
    function unlock(string calldata reason) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (status.state != BreakerState.LOCKED) return;

        status.state = BreakerState.CLOSED;
        status.alertLevel = AlertLevel.NONE;
        status.tripCount = 0;

        _unpauseAllProtected();

        emit StateChanged(BreakerState.LOCKED, BreakerState.CLOSED);
        emit BreakerReset(msg.sender, reason);
    }

    /**
     * @notice Attempts auto-recovery if enabled
     */
    function attemptAutoRecovery() external {
        if (!autoRecoveryEnabled) revert RecoveryNotAllowed();
        if (status.state != BreakerState.OPEN) return;
        if (block.timestamp < status.cooldownEnd + recoveryDelay) {
            revert CooldownActive(status.cooldownEnd + recoveryDelay - block.timestamp);
        }

        // Move to half-open state
        status.state = BreakerState.HALF_OPEN;

        emit AutoRecoveryTriggered(block.timestamp);
    }

    // ============================================
    // PROTECTED CONTRACTS
    // ============================================

    /**
     * @notice Adds a contract to protection
     */
    function addProtectedContract(address contractAddress) external onlyRole(OPERATOR_ROLE) {
        if (contractAddress == address(0)) revert ZeroAddress();
        if (protectedContracts[contractAddress].contractAddress != address(0)) {
            revert AlreadyProtected(contractAddress);
        }

        protectedContracts[contractAddress] = ProtectedContract({
            contractAddress: contractAddress,
            isActive: true,
            isPaused: false,
            lastPause: 0,
            pauseCount: 0
        });

        protectedList.push(contractAddress);

        emit ProtectedContractAdded(contractAddress);
    }

    /**
     * @notice Removes a contract from protection
     */
    function removeProtectedContract(address contractAddress) external onlyRole(OPERATOR_ROLE) {
        if (protectedContracts[contractAddress].contractAddress == address(0)) {
            revert ContractNotProtected(contractAddress);
        }

        protectedContracts[contractAddress].isActive = false;

        emit ProtectedContractRemoved(contractAddress);
    }

    /**
     * @notice Pauses all protected contracts
     */
    function _pauseAllProtected() internal {
        for (uint256 i = 0; i < protectedList.length; i++) {
            ProtectedContract storage pc = protectedContracts[protectedList[i]];
            if (pc.isActive && !pc.isPaused) {
                pc.isPaused = true;
                pc.lastPause = block.timestamp;
                pc.pauseCount++;

                // Call pause on contract if it supports it
                (bool success,) = pc.contractAddress.call(
                    abi.encodeWithSignature("pause()")
                );
                
                if (success) {
                    emit ContractPaused(pc.contractAddress, status.lastTripReason);
                }
            }
        }
    }

    /**
     * @notice Unpauses all protected contracts
     */
    function _unpauseAllProtected() internal {
        for (uint256 i = 0; i < protectedList.length; i++) {
            ProtectedContract storage pc = protectedContracts[protectedList[i]];
            if (pc.isActive && pc.isPaused) {
                pc.isPaused = false;

                (bool success,) = pc.contractAddress.call(
                    abi.encodeWithSignature("unpause()")
                );
                
                if (success) {
                    emit ContractUnpaused(pc.contractAddress);
                }
            }
        }
    }

    // ============================================
    // CONFIGURATION
    // ============================================

    /**
     * @notice Updates threshold configuration
     */
    function setThreshold(
        MetricType metricType,
        uint256 warning,
        uint256 critical,
        uint256 emergency,
        uint256 window,
        uint256 cooldown
    ) external onlyRole(OPERATOR_ROLE) {
        if (warning >= critical || critical >= emergency) revert InvalidThreshold();

        thresholds[metricType] = ThresholdConfig({
            warningThreshold: warning,
            criticalThreshold: critical,
            emergencyThreshold: emergency,
            window: window,
            isEnabled: true,
            cooldownPeriod: cooldown
        });

        emit ThresholdUpdated(metricType, warning, critical, emergency);
    }

    /**
     * @notice Enables/disables a threshold
     */
    function setThresholdEnabled(
        MetricType metricType,
        bool enabled
    ) external onlyRole(OPERATOR_ROLE) {
        thresholds[metricType].isEnabled = enabled;
    }

    /**
     * @notice Sets auto-recovery settings
     */
    function setAutoRecovery(
        bool enabled,
        uint256 delay
    ) external onlyRole(OPERATOR_ROLE) {
        autoRecoveryEnabled = enabled;
        recoveryDelay = delay;
    }

    /**
     * @notice Sets global pause duration
     */
    function setGlobalPauseDuration(uint256 duration) external onlyRole(OPERATOR_ROLE) {
        globalPauseDuration = duration;
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    function getStatus() external view returns (BreakerStatus memory) {
        return status;
    }

    function getMetric(MetricType metricType) external view returns (MetricData memory) {
        return metrics[metricType];
    }

    function getThreshold(MetricType metricType) external view returns (ThresholdConfig memory) {
        return thresholds[metricType];
    }

    function getTripHistory(uint256 count) external view returns (TripRecord[] memory) {
        uint256 len = tripHistory.length;
        uint256 resultLen = count > len ? len : count;
        TripRecord[] memory result = new TripRecord[](resultLen);
        
        for (uint256 i = 0; i < resultLen; i++) {
            result[i] = tripHistory[len - resultLen + i];
        }
        return result;
    }

    function getProtectedContracts() external view returns (address[] memory) {
        return protectedList;
    }

    function isOpen() external view returns (bool) {
        return status.state == BreakerState.OPEN || status.state == BreakerState.LOCKED;
    }

    // ============================================
    // ADMIN
    // ============================================

    function _authorizeUpgrade(address) internal override onlyRole(UPGRADER_ROLE) {}
}
