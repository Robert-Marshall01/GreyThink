// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts-upgradeable/access/AccessControlUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/PausableUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/Initializable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/UUPSUpgradeable.sol";

/**
 * @title RateLimiter
 * @author Grey Protocol Team
 * @notice Rate limiting implementation for protecting protocol operations
 * @dev Implements multiple rate limiting strategies
 * 
 * Features:
 * - Fixed window rate limiting
 * - Sliding window rate limiting
 * - Token bucket algorithm
 * - Per-address limits
 * - Per-operation limits
 * - Global limits
 * - Configurable penalties
 * - Whitelist/bypass capability
 */
contract RateLimiter is
    Initializable,
    AccessControlUpgradeable,
    PausableUpgradeable,
    UUPSUpgradeable
{
    // ============================================
    // CONSTANTS & ROLES
    // ============================================

    bytes32 public constant OPERATOR_ROLE = keccak256("OPERATOR_ROLE");
    bytes32 public constant UPGRADER_ROLE = keccak256("UPGRADER_ROLE");
    bytes32 public constant BYPASS_ROLE = keccak256("BYPASS_ROLE");

    /// @notice Maximum limit buckets
    uint256 public constant MAX_BUCKETS = 24;

    /// @notice Maximum penalty multiplier
    uint256 public constant MAX_PENALTY_MULTIPLIER = 10;

    // ============================================
    // ENUMS
    // ============================================

    /**
     * @notice Rate limiting algorithms
     */
    enum LimitAlgorithm {
        FIXED_WINDOW,
        SLIDING_WINDOW,
        TOKEN_BUCKET,
        LEAKY_BUCKET
    }

    /**
     * @notice Limit scope
     */
    enum LimitScope {
        GLOBAL,         // Global rate limit
        PER_ADDRESS,    // Per sender address
        PER_OPERATION,  // Per operation type
        PER_TOKEN       // Per token address
    }

    // ============================================
    // STRUCTS
    // ============================================

    /**
     * @notice Rate limit configuration
     * @param id Limit ID (operation identifier)
     * @param algorithm Rate limiting algorithm
     * @param scope Limit scope
     * @param limit Maximum allowed in window
     * @param window Time window in seconds
     * @param refillRate Tokens refilled per second (for bucket algorithms)
     * @param burstCapacity Maximum burst capacity
     * @param isEnabled Whether limit is active
     * @param penaltyEnabled Whether penalties are active
     * @param penaltyMultiplier Penalty multiplier for violations
     * @param penaltyDuration Penalty duration in seconds
     */
    struct LimitConfig {
        bytes32 id;
        LimitAlgorithm algorithm;
        LimitScope scope;
        uint256 limit;
        uint256 window;
        uint256 refillRate;
        uint256 burstCapacity;
        bool isEnabled;
        bool penaltyEnabled;
        uint256 penaltyMultiplier;
        uint256 penaltyDuration;
    }

    /**
     * @notice Bucket state for tracking usage
     * @param count Current count in window
     * @param tokens Available tokens (for bucket algorithms)
     * @param windowStart Current window start
     * @param lastRefill Last token refill time
     * @param violations Number of violations
     * @param penaltyEnd Penalty end time
     */
    struct BucketState {
        uint256 count;
        uint256 tokens;
        uint256 windowStart;
        uint256 lastRefill;
        uint256 violations;
        uint256 penaltyEnd;
    }

    /**
     * @notice Sliding window bucket
     */
    struct SlidingBucket {
        uint256[] counts;
        uint256[] timestamps;
        uint256 head;
        uint256 tail;
        uint256 total;
    }

    /**
     * @notice Rate check result
     */
    struct RateCheckResult {
        bool allowed;
        uint256 remaining;
        uint256 resetAt;
        uint256 currentUsage;
        bool inPenalty;
    }

    /**
     * @notice Usage statistics
     */
    struct UsageStats {
        uint256 totalRequests;
        uint256 allowedRequests;
        uint256 blockedRequests;
        uint256 violations;
        uint256 peakUsage;
        uint256 avgUsage;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Limit configurations by ID
    mapping(bytes32 => LimitConfig) public limits;

    /// @notice All limit IDs
    bytes32[] public limitIds;

    /// @notice Bucket states: limitId => key => state
    mapping(bytes32 => mapping(bytes32 => BucketState)) public buckets;

    /// @notice Sliding window buckets
    mapping(bytes32 => mapping(bytes32 => SlidingBucket)) internal slidingBuckets;

    /// @notice Usage statistics
    mapping(bytes32 => UsageStats) public stats;

    /// @notice Whitelisted addresses (bypass rate limits)
    mapping(address => bool) public whitelist;

    /// @notice Blacklisted addresses (always blocked)
    mapping(address => bool) public blacklist;

    /// @notice Default limit for unknown operations
    LimitConfig public defaultLimit;

    /// @notice Total limits configured
    uint256 public totalLimits;

    /// @notice Storage gap
    uint256[40] private __gap;

    // ============================================
    // EVENTS
    // ============================================

    event RateLimitExceeded(
        bytes32 indexed limitId,
        bytes32 indexed key,
        address indexed account,
        uint256 count,
        uint256 limit
    );

    event LimitConfigured(bytes32 indexed limitId, LimitAlgorithm algorithm, uint256 limit, uint256 window);
    event LimitRemoved(bytes32 indexed limitId);
    event PenaltyApplied(bytes32 indexed limitId, bytes32 indexed key, uint256 until);
    event WhitelistUpdated(address indexed account, bool status);
    event BlacklistUpdated(address indexed account, bool status);
    event RateAllowed(bytes32 indexed limitId, bytes32 indexed key, uint256 remaining);

    // ============================================
    // ERRORS
    // ============================================

    error RateLimitExceededError(bytes32 limitId, uint256 count, uint256 limit);
    error AccountBlacklisted(address account);
    error LimitNotFound(bytes32 limitId);
    error LimitExists(bytes32 limitId);
    error InvalidLimit();
    error InvalidWindow();
    error InPenalty(uint256 until);
    error ZeroAddress();

    // ============================================
    // MODIFIERS
    // ============================================

    /**
     * @notice Rate limit check modifier
     * @param limitId Limit identifier
     */
    modifier rateLimit(bytes32 limitId) {
        bytes32 key = _getKey(limitId, msg.sender);
        RateCheckResult memory result = _checkAndConsume(limitId, key, 1);
        if (!result.allowed) {
            revert RateLimitExceededError(limitId, result.currentUsage, limits[limitId].limit);
        }
        _;
    }

    // ============================================
    // INITIALIZER
    // ============================================

    /**
     * @notice Initializes the rate limiter
     * @param admin Admin address
     */
    function initialize(address admin) public initializer {
        if (admin == address(0)) revert ZeroAddress();

        __AccessControl_init();
        __Pausable_init();
        __UUPSUpgradeable_init();

        // Default limit: 100 requests per minute per address
        defaultLimit = LimitConfig({
            id: keccak256("DEFAULT"),
            algorithm: LimitAlgorithm.FIXED_WINDOW,
            scope: LimitScope.PER_ADDRESS,
            limit: 100,
            window: 1 minutes,
            refillRate: 0,
            burstCapacity: 0,
            isEnabled: true,
            penaltyEnabled: true,
            penaltyMultiplier: 2,
            penaltyDuration: 5 minutes
        });

        _grantRole(DEFAULT_ADMIN_ROLE, admin);
        _grantRole(OPERATOR_ROLE, admin);
        _grantRole(UPGRADER_ROLE, admin);
        _grantRole(BYPASS_ROLE, admin);
    }

    // ============================================
    // RATE LIMITING
    // ============================================

    /**
     * @notice Checks if an operation is allowed
     * @param limitId Limit identifier
     * @param account Account to check
     * @return result Rate check result
     */
    function checkRate(
        bytes32 limitId,
        address account
    ) external view returns (RateCheckResult memory result) {
        // Check bypass
        if (hasRole(BYPASS_ROLE, account) || whitelist[account]) {
            return RateCheckResult({
                allowed: true,
                remaining: type(uint256).max,
                resetAt: 0,
                currentUsage: 0,
                inPenalty: false
            });
        }

        if (blacklist[account]) {
            return RateCheckResult({
                allowed: false,
                remaining: 0,
                resetAt: type(uint256).max,
                currentUsage: 0,
                inPenalty: false
            });
        }

        bytes32 key = _getKey(limitId, account);
        return _checkRateView(limitId, key);
    }

    /**
     * @notice Consumes rate limit quota
     * @param limitId Limit identifier
     * @param account Account consuming
     * @param amount Amount to consume
     * @return allowed Whether operation is allowed
     */
    function consume(
        bytes32 limitId,
        address account,
        uint256 amount
    ) external whenNotPaused returns (bool allowed) {
        // Check bypass
        if (hasRole(BYPASS_ROLE, account) || whitelist[account]) {
            return true;
        }

        if (blacklist[account]) {
            revert AccountBlacklisted(account);
        }

        bytes32 key = _getKey(limitId, account);
        RateCheckResult memory result = _checkAndConsume(limitId, key, amount);
        
        if (!result.allowed) {
            emit RateLimitExceeded(limitId, key, account, result.currentUsage, limits[limitId].limit);
        } else {
            emit RateAllowed(limitId, key, result.remaining);
        }

        return result.allowed;
    }

    /**
     * @notice Internal rate check with consumption
     */
    function _checkAndConsume(
        bytes32 limitId,
        bytes32 key,
        uint256 amount
    ) internal returns (RateCheckResult memory result) {
        LimitConfig storage config = limits[limitId];
        if (!config.isEnabled) {
            config = defaultLimit;
        }

        BucketState storage bucket = buckets[limitId][key];

        // Check penalty
        if (bucket.penaltyEnd > 0 && block.timestamp < bucket.penaltyEnd) {
            return RateCheckResult({
                allowed: false,
                remaining: 0,
                resetAt: bucket.penaltyEnd,
                currentUsage: bucket.count,
                inPenalty: true
            });
        }

        // Process based on algorithm
        if (config.algorithm == LimitAlgorithm.FIXED_WINDOW) {
            result = _checkFixedWindow(config, bucket, amount);
        } else if (config.algorithm == LimitAlgorithm.TOKEN_BUCKET) {
            result = _checkTokenBucket(config, bucket, amount);
        } else if (config.algorithm == LimitAlgorithm.LEAKY_BUCKET) {
            result = _checkLeakyBucket(config, bucket, amount);
        } else {
            result = _checkFixedWindow(config, bucket, amount); // Default
        }

        // Update stats
        stats[limitId].totalRequests++;
        if (result.allowed) {
            stats[limitId].allowedRequests++;
        } else {
            stats[limitId].blockedRequests++;
            bucket.violations++;
            stats[limitId].violations++;

            // Apply penalty if enabled
            if (config.penaltyEnabled) {
                uint256 penaltyDuration = config.penaltyDuration * 
                    (bucket.violations > MAX_PENALTY_MULTIPLIER ? MAX_PENALTY_MULTIPLIER : bucket.violations);
                bucket.penaltyEnd = block.timestamp + penaltyDuration;
                emit PenaltyApplied(limitId, key, bucket.penaltyEnd);
            }
        }

        // Track peak usage
        if (bucket.count > stats[limitId].peakUsage) {
            stats[limitId].peakUsage = bucket.count;
        }
    }

    /**
     * @notice Fixed window algorithm
     */
    function _checkFixedWindow(
        LimitConfig storage config,
        BucketState storage bucket,
        uint256 amount
    ) internal returns (RateCheckResult memory result) {
        // Reset window if expired
        if (block.timestamp >= bucket.windowStart + config.window) {
            bucket.windowStart = block.timestamp;
            bucket.count = 0;
        }

        result.currentUsage = bucket.count;
        result.resetAt = bucket.windowStart + config.window;

        if (bucket.count + amount <= config.limit) {
            bucket.count += amount;
            result.allowed = true;
            result.remaining = config.limit - bucket.count;
        } else {
            result.allowed = false;
            result.remaining = 0;
        }
    }

    /**
     * @notice Token bucket algorithm
     */
    function _checkTokenBucket(
        LimitConfig storage config,
        BucketState storage bucket,
        uint256 amount
    ) internal returns (RateCheckResult memory result) {
        // Refill tokens
        uint256 timePassed = block.timestamp - bucket.lastRefill;
        uint256 tokensToAdd = timePassed * config.refillRate;
        
        bucket.tokens = bucket.tokens + tokensToAdd;
        if (bucket.tokens > config.burstCapacity) {
            bucket.tokens = config.burstCapacity;
        }
        bucket.lastRefill = block.timestamp;

        result.currentUsage = config.burstCapacity - bucket.tokens;

        if (bucket.tokens >= amount) {
            bucket.tokens -= amount;
            result.allowed = true;
            result.remaining = bucket.tokens;
        } else {
            result.allowed = false;
            result.remaining = bucket.tokens;
        }

        // Estimate reset time
        if (!result.allowed && config.refillRate > 0) {
            uint256 tokensNeeded = amount - bucket.tokens;
            result.resetAt = block.timestamp + (tokensNeeded / config.refillRate);
        }
    }

    /**
     * @notice Leaky bucket algorithm
     */
    function _checkLeakyBucket(
        LimitConfig storage config,
        BucketState storage bucket,
        uint256 amount
    ) internal returns (RateCheckResult memory result) {
        // Leak tokens
        uint256 timePassed = block.timestamp - bucket.lastRefill;
        uint256 leaked = timePassed * config.refillRate;
        
        if (leaked >= bucket.count) {
            bucket.count = 0;
        } else {
            bucket.count -= leaked;
        }
        bucket.lastRefill = block.timestamp;

        result.currentUsage = bucket.count;

        if (bucket.count + amount <= config.burstCapacity) {
            bucket.count += amount;
            result.allowed = true;
            result.remaining = config.burstCapacity - bucket.count;
        } else {
            result.allowed = false;
            result.remaining = 0;
        }

        result.resetAt = block.timestamp + (bucket.count / config.refillRate);
    }

    /**
     * @notice View-only rate check
     */
    function _checkRateView(
        bytes32 limitId,
        bytes32 key
    ) internal view returns (RateCheckResult memory result) {
        LimitConfig storage config = limits[limitId];
        if (!config.isEnabled) {
            // Use default
            return RateCheckResult({
                allowed: true,
                remaining: defaultLimit.limit,
                resetAt: block.timestamp + defaultLimit.window,
                currentUsage: 0,
                inPenalty: false
            });
        }

        BucketState storage bucket = buckets[limitId][key];

        // Check penalty
        if (bucket.penaltyEnd > 0 && block.timestamp < bucket.penaltyEnd) {
            return RateCheckResult({
                allowed: false,
                remaining: 0,
                resetAt: bucket.penaltyEnd,
                currentUsage: bucket.count,
                inPenalty: true
            });
        }

        // For fixed window
        uint256 currentCount = bucket.count;
        if (block.timestamp >= bucket.windowStart + config.window) {
            currentCount = 0;
        }

        result = RateCheckResult({
            allowed: currentCount < config.limit,
            remaining: currentCount < config.limit ? config.limit - currentCount : 0,
            resetAt: bucket.windowStart + config.window,
            currentUsage: currentCount,
            inPenalty: false
        });
    }

    // ============================================
    // CONFIGURATION
    // ============================================

    /**
     * @notice Configures a rate limit
     */
    function configureLimit(
        bytes32 limitId,
        LimitAlgorithm algorithm,
        LimitScope scope,
        uint256 limit,
        uint256 window,
        uint256 refillRate,
        uint256 burstCapacity
    ) external onlyRole(OPERATOR_ROLE) {
        if (limit == 0) revert InvalidLimit();
        if (window == 0) revert InvalidWindow();

        bool isNew = limits[limitId].limit == 0;

        limits[limitId] = LimitConfig({
            id: limitId,
            algorithm: algorithm,
            scope: scope,
            limit: limit,
            window: window,
            refillRate: refillRate,
            burstCapacity: burstCapacity > 0 ? burstCapacity : limit,
            isEnabled: true,
            penaltyEnabled: true,
            penaltyMultiplier: 2,
            penaltyDuration: 5 minutes
        });

        if (isNew) {
            limitIds.push(limitId);
            totalLimits++;
        }

        emit LimitConfigured(limitId, algorithm, limit, window);
    }

    /**
     * @notice Sets penalty configuration
     */
    function configurePenalty(
        bytes32 limitId,
        bool enabled,
        uint256 multiplier,
        uint256 duration
    ) external onlyRole(OPERATOR_ROLE) {
        LimitConfig storage config = limits[limitId];
        if (config.limit == 0) revert LimitNotFound(limitId);

        config.penaltyEnabled = enabled;
        config.penaltyMultiplier = multiplier > MAX_PENALTY_MULTIPLIER ? MAX_PENALTY_MULTIPLIER : multiplier;
        config.penaltyDuration = duration;
    }

    /**
     * @notice Enables/disables a limit
     */
    function setLimitEnabled(bytes32 limitId, bool enabled) external onlyRole(OPERATOR_ROLE) {
        limits[limitId].isEnabled = enabled;
    }

    /**
     * @notice Removes a limit
     */
    function removeLimit(bytes32 limitId) external onlyRole(OPERATOR_ROLE) {
        delete limits[limitId];
        emit LimitRemoved(limitId);
    }

    // ============================================
    // WHITELIST/BLACKLIST
    // ============================================

    /**
     * @notice Updates whitelist status
     */
    function setWhitelist(address account, bool status) external onlyRole(OPERATOR_ROLE) {
        whitelist[account] = status;
        emit WhitelistUpdated(account, status);
    }

    /**
     * @notice Updates blacklist status
     */
    function setBlacklist(address account, bool status) external onlyRole(OPERATOR_ROLE) {
        blacklist[account] = status;
        emit BlacklistUpdated(account, status);
    }

    /**
     * @notice Batch whitelist update
     */
    function batchWhitelist(
        address[] calldata accounts,
        bool status
    ) external onlyRole(OPERATOR_ROLE) {
        for (uint256 i = 0; i < accounts.length; i++) {
            whitelist[accounts[i]] = status;
            emit WhitelistUpdated(accounts[i], status);
        }
    }

    /**
     * @notice Clears penalty for an account
     */
    function clearPenalty(
        bytes32 limitId,
        address account
    ) external onlyRole(OPERATOR_ROLE) {
        bytes32 key = _getKey(limitId, account);
        buckets[limitId][key].penaltyEnd = 0;
        buckets[limitId][key].violations = 0;
    }

    // ============================================
    // HELPER FUNCTIONS
    // ============================================

    /**
     * @notice Generates key for bucket storage
     */
    function _getKey(
        bytes32 limitId,
        address account
    ) internal view returns (bytes32) {
        LimitConfig storage config = limits[limitId];
        
        if (config.scope == LimitScope.GLOBAL) {
            return keccak256(abi.encodePacked(limitId, "GLOBAL"));
        } else if (config.scope == LimitScope.PER_ADDRESS) {
            return keccak256(abi.encodePacked(limitId, account));
        }
        
        return keccak256(abi.encodePacked(limitId, account));
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    function getLimit(bytes32 limitId) external view returns (LimitConfig memory) {
        return limits[limitId];
    }

    function getAllLimits() external view returns (bytes32[] memory) {
        return limitIds;
    }

    function getStats(bytes32 limitId) external view returns (UsageStats memory) {
        return stats[limitId];
    }

    function getBucket(
        bytes32 limitId,
        address account
    ) external view returns (BucketState memory) {
        bytes32 key = _getKey(limitId, account);
        return buckets[limitId][key];
    }

    // ============================================
    // ADMIN
    // ============================================

    function pause() external onlyRole(OPERATOR_ROLE) { _pause(); }
    function unpause() external onlyRole(OPERATOR_ROLE) { _unpause(); }
    function _authorizeUpgrade(address) internal override onlyRole(UPGRADER_ROLE) {}
}
