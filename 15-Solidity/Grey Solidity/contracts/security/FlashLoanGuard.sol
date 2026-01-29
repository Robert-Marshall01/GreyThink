// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/Pausable.sol";

/**
 * @title FlashLoanGuard
 * @author Grey Protocol Team
 * @notice Protection against flash loan attacks
 * @dev Implements multiple strategies to detect and prevent flash loan exploits
 * 
 * Features:
 * - Same-block transaction detection
 * - Price manipulation detection
 * - Delayed execution requirements
 * - Commit-reveal patterns
 * - Multi-block confirmation
 */
contract FlashLoanGuard is AccessControl, ReentrancyGuard, Pausable {
    // ============================================
    // CONSTANTS & ROLES
    // ============================================

    bytes32 public constant GUARD_ADMIN_ROLE = keccak256("GUARD_ADMIN_ROLE");
    bytes32 public constant BYPASS_ROLE = keccak256("BYPASS_ROLE");

    uint256 public constant MAX_DELAY_BLOCKS = 256;
    uint256 public constant MIN_DELAY_BLOCKS = 1;

    // ============================================
    // ENUMS
    // ============================================

    enum GuardMode {
        OFF,
        BLOCK_CHECK,        // Check same-block transactions
        MULTI_BLOCK,        // Require multi-block confirmation
        COMMIT_REVEAL,      // Require commit-reveal pattern
        PRICE_CHECK,        // Check for price manipulation
        FULL_PROTECTION     // All protections enabled
    }

    // ============================================
    // STRUCTS
    // ============================================

    /**
     * @notice User's block-level tracking
     */
    struct BlockTracker {
        uint256 lastBlock;
        uint256 transactionCount;
        uint256 totalValue;
        bytes32 lastActionHash;
    }

    /**
     * @notice Commit-reveal commitment
     */
    struct Commitment {
        bytes32 commitHash;
        uint256 commitBlock;
        uint256 commitTime;
        bool revealed;
        bool executed;
    }

    /**
     * @notice Price snapshot for manipulation detection
     */
    struct PriceSnapshot {
        uint256 price;
        uint256 timestamp;
        uint256 blockNumber;
    }

    /**
     * @notice Protection configuration
     */
    struct ProtectionConfig {
        GuardMode mode;
        uint256 minBlockDelay;
        uint256 minTimeDelay;
        uint256 maxValuePerBlock;
        uint256 maxTxPerBlock;
        uint256 priceDeviationThreshold; // basis points
        bool enabled;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Default protection config
    ProtectionConfig public defaultConfig;

    /// @notice Custom configs per protected contract
    mapping(address => ProtectionConfig) public contractConfigs;

    /// @notice User block tracking
    mapping(address => mapping(address => BlockTracker)) public userTrackers;

    /// @notice User commitments for commit-reveal
    mapping(address => mapping(bytes32 => Commitment)) public commitments;

    /// @notice Price snapshots for oracle checks
    mapping(address => PriceSnapshot[]) public priceHistory;

    /// @notice Last known good price per token
    mapping(address => uint256) public lastKnownPrices;

    /// @notice Trusted price oracles
    mapping(address => bool) public trustedOracles;

    /// @notice Protected contracts
    mapping(address => bool) public protectedContracts;

    /// @notice Maximum price history entries
    uint256 public maxPriceHistoryLength;

    /// @notice Commit validity period
    uint256 public commitValidityPeriod;

    // ============================================
    // EVENTS
    // ============================================

    event FlashLoanDetected(
        address indexed user,
        address indexed protectedContract,
        uint256 blockNumber,
        string reason
    );

    event ActionBlocked(
        address indexed user,
        address indexed protectedContract,
        bytes32 actionHash,
        string reason
    );

    event CommitmentMade(
        address indexed user,
        bytes32 indexed commitHash,
        uint256 commitBlock
    );

    event CommitmentRevealed(
        address indexed user,
        bytes32 indexed commitHash,
        bytes data
    );

    event PriceManipulationDetected(
        address indexed token,
        uint256 previousPrice,
        uint256 currentPrice,
        uint256 deviation
    );

    event ProtectionConfigured(
        address indexed protectedContract,
        GuardMode mode,
        uint256 minBlockDelay
    );

    event ContractProtectionToggled(address indexed protectedContract, bool enabled);
    event OracleUpdated(address indexed oracle, bool trusted);

    // ============================================
    // ERRORS
    // ============================================

    error SameBlockTransaction();
    error InsufficientBlockDelay(uint256 required, uint256 actual);
    error InsufficientTimeDelay(uint256 required, uint256 actual);
    error ValueExceedsBlockLimit(uint256 value, uint256 limit);
    error TxCountExceedsBlockLimit(uint256 count, uint256 limit);
    error CommitmentNotFound();
    error CommitmentAlreadyRevealed();
    error CommitmentExpired();
    error InvalidReveal();
    error PriceManipulationSuspected();
    error ContractNotProtected();
    error Unauthorized();

    // ============================================
    // MODIFIERS
    // ============================================

    modifier onlyProtected(address protectedContract) {
        if (!protectedContracts[protectedContract]) {
            revert ContractNotProtected();
        }
        _;
    }

    modifier withGuard(address protectedContract, uint256 value) {
        _checkGuard(msg.sender, protectedContract, value, "");
        _;
    }

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor() {
        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(GUARD_ADMIN_ROLE, msg.sender);
        _grantRole(BYPASS_ROLE, msg.sender);

        defaultConfig = ProtectionConfig({
            mode: GuardMode.BLOCK_CHECK,
            minBlockDelay: 1,
            minTimeDelay: 0,
            maxValuePerBlock: type(uint256).max,
            maxTxPerBlock: 10,
            priceDeviationThreshold: 500, // 5%
            enabled: true
        });

        maxPriceHistoryLength = 100;
        commitValidityPeriod = 1 hours;
    }

    // ============================================
    // CORE GUARD FUNCTIONS
    // ============================================

    /**
     * @notice Main guard check for flash loan protection
     * @param user Address performing action
     * @param protectedContract Contract being protected
     * @param value Transaction value
     * @param actionData Additional action data
     */
    function checkGuard(
        address user,
        address protectedContract,
        uint256 value,
        bytes calldata actionData
    ) external view returns (bool allowed, string memory reason) {
        return _checkGuardView(user, protectedContract, value, actionData);
    }

    /**
     * @notice Records action and enforces guards
     */
    function recordAndCheck(
        address user,
        address protectedContract,
        uint256 value,
        bytes calldata actionData
    ) external onlyProtected(protectedContract) returns (bool) {
        _checkGuard(user, protectedContract, value, actionData);
        _recordAction(user, protectedContract, value, actionData);
        return true;
    }

    /**
     * @notice Internal guard check with revert
     */
    function _checkGuard(
        address user,
        address protectedContract,
        uint256 value,
        bytes memory actionData
    ) internal view {
        // Bypass for privileged addresses
        if (hasRole(BYPASS_ROLE, user)) {
            return;
        }

        ProtectionConfig memory config = _getConfig(protectedContract);
        if (!config.enabled) {
            return;
        }

        BlockTracker memory tracker = userTrackers[user][protectedContract];

        if (config.mode == GuardMode.OFF) {
            return;
        }

        // Block-level checks
        if (config.mode == GuardMode.BLOCK_CHECK || 
            config.mode == GuardMode.MULTI_BLOCK ||
            config.mode == GuardMode.FULL_PROTECTION) {
            
            // Same-block detection
            if (tracker.lastBlock == block.number) {
                // Check transaction count
                if (tracker.transactionCount >= config.maxTxPerBlock) {
                    revert TxCountExceedsBlockLimit(tracker.transactionCount + 1, config.maxTxPerBlock);
                }
                
                // Check value limit
                if (tracker.totalValue + value > config.maxValuePerBlock) {
                    revert ValueExceedsBlockLimit(tracker.totalValue + value, config.maxValuePerBlock);
                }
            }
        }

        // Multi-block delay
        if (config.mode == GuardMode.MULTI_BLOCK || config.mode == GuardMode.FULL_PROTECTION) {
            if (tracker.lastBlock > 0) {
                uint256 blocksPassed = block.number - tracker.lastBlock;
                if (blocksPassed < config.minBlockDelay) {
                    revert InsufficientBlockDelay(config.minBlockDelay, blocksPassed);
                }
            }
        }

        // Price check
        if (config.mode == GuardMode.PRICE_CHECK || config.mode == GuardMode.FULL_PROTECTION) {
            _checkPriceManipulation(protectedContract, config.priceDeviationThreshold);
        }
    }

    /**
     * @notice View version of guard check
     */
    function _checkGuardView(
        address user,
        address protectedContract,
        uint256 value,
        bytes memory /* actionData */
    ) internal view returns (bool allowed, string memory reason) {
        if (hasRole(BYPASS_ROLE, user)) {
            return (true, "");
        }

        ProtectionConfig memory config = _getConfig(protectedContract);
        if (!config.enabled || config.mode == GuardMode.OFF) {
            return (true, "");
        }

        BlockTracker memory tracker = userTrackers[user][protectedContract];

        // Same-block checks
        if (tracker.lastBlock == block.number) {
            if (tracker.transactionCount >= config.maxTxPerBlock) {
                return (false, "Transaction count exceeds block limit");
            }
            if (tracker.totalValue + value > config.maxValuePerBlock) {
                return (false, "Value exceeds block limit");
            }
        }

        // Multi-block delay
        if (config.mode == GuardMode.MULTI_BLOCK || config.mode == GuardMode.FULL_PROTECTION) {
            if (tracker.lastBlock > 0) {
                uint256 blocksPassed = block.number - tracker.lastBlock;
                if (blocksPassed < config.minBlockDelay) {
                    return (false, "Insufficient block delay");
                }
            }
        }

        return (true, "");
    }

    /**
     * @notice Records an action for tracking
     */
    function _recordAction(
        address user,
        address protectedContract,
        uint256 value,
        bytes memory actionData
    ) internal {
        BlockTracker storage tracker = userTrackers[user][protectedContract];

        if (tracker.lastBlock != block.number) {
            // New block, reset counters
            tracker.lastBlock = block.number;
            tracker.transactionCount = 1;
            tracker.totalValue = value;
        } else {
            // Same block, increment
            tracker.transactionCount++;
            tracker.totalValue += value;
        }

        tracker.lastActionHash = keccak256(actionData);
    }

    /**
     * @notice Checks for price manipulation
     */
    function _checkPriceManipulation(
        address token,
        uint256 threshold
    ) internal view {
        if (priceHistory[token].length < 2) {
            return;
        }

        PriceSnapshot[] storage history = priceHistory[token];
        PriceSnapshot memory latest = history[history.length - 1];
        PriceSnapshot memory previous = history[history.length - 2];

        if (previous.price == 0) {
            return;
        }

        uint256 deviation;
        if (latest.price > previous.price) {
            deviation = ((latest.price - previous.price) * 10000) / previous.price;
        } else {
            deviation = ((previous.price - latest.price) * 10000) / previous.price;
        }

        if (deviation > threshold) {
            revert PriceManipulationSuspected();
        }
    }

    // ============================================
    // COMMIT-REVEAL PATTERN
    // ============================================

    /**
     * @notice Creates a commitment for delayed action
     * @param commitHash Hash of the action data
     */
    function commit(bytes32 commitHash) external whenNotPaused {
        Commitment storage c = commitments[msg.sender][commitHash];
        
        c.commitHash = commitHash;
        c.commitBlock = block.number;
        c.commitTime = block.timestamp;
        c.revealed = false;
        c.executed = false;

        emit CommitmentMade(msg.sender, commitHash, block.number);
    }

    /**
     * @notice Reveals a commitment
     * @param data Original data that was committed
     * @param secret Secret used in commitment
     */
    function reveal(bytes calldata data, bytes32 secret) external whenNotPaused {
        bytes32 computedHash = keccak256(abi.encodePacked(data, secret, msg.sender));
        Commitment storage c = commitments[msg.sender][computedHash];

        if (c.commitHash != computedHash) {
            revert CommitmentNotFound();
        }

        if (c.revealed) {
            revert CommitmentAlreadyRevealed();
        }

        if (block.timestamp > c.commitTime + commitValidityPeriod) {
            revert CommitmentExpired();
        }

        // Check minimum block delay
        if (block.number <= c.commitBlock + defaultConfig.minBlockDelay) {
            revert InsufficientBlockDelay(defaultConfig.minBlockDelay, block.number - c.commitBlock);
        }

        c.revealed = true;

        emit CommitmentRevealed(msg.sender, computedHash, data);
    }

    /**
     * @notice Checks if commitment can be executed
     */
    function canExecuteCommitment(
        address user,
        bytes calldata data,
        bytes32 secret
    ) external view returns (bool canExecute, string memory reason) {
        bytes32 computedHash = keccak256(abi.encodePacked(data, secret, user));
        Commitment memory c = commitments[user][computedHash];

        if (c.commitHash != computedHash) {
            return (false, "Commitment not found");
        }

        if (!c.revealed) {
            return (false, "Commitment not revealed");
        }

        if (c.executed) {
            return (false, "Already executed");
        }

        if (block.timestamp > c.commitTime + commitValidityPeriod) {
            return (false, "Commitment expired");
        }

        return (true, "");
    }

    /**
     * @notice Marks commitment as executed
     */
    function markExecuted(bytes32 commitHash) external onlyRole(GUARD_ADMIN_ROLE) {
        commitments[msg.sender][commitHash].executed = true;
    }

    // ============================================
    // PRICE ORACLE INTEGRATION
    // ============================================

    /**
     * @notice Records price snapshot
     * @param token Token address
     * @param price Current price
     */
    function recordPrice(address token, uint256 price) external {
        require(trustedOracles[msg.sender], "Not trusted oracle");

        PriceSnapshot[] storage history = priceHistory[token];

        history.push(PriceSnapshot({
            price: price,
            timestamp: block.timestamp,
            blockNumber: block.number
        }));

        // Trim history if needed
        if (history.length > maxPriceHistoryLength) {
            // Remove oldest entry
            for (uint256 i = 0; i < history.length - 1; i++) {
                history[i] = history[i + 1];
            }
            history.pop();
        }

        lastKnownPrices[token] = price;
    }

    /**
     * @notice Gets price volatility
     */
    function getPriceVolatility(
        address token,
        uint256 periods
    ) external view returns (uint256 volatility) {
        PriceSnapshot[] storage history = priceHistory[token];
        if (history.length < periods + 1) {
            return 0;
        }

        uint256 sumDeviations = 0;
        uint256 startIdx = history.length - periods - 1;

        for (uint256 i = startIdx + 1; i < history.length; i++) {
            uint256 prevPrice = history[i - 1].price;
            uint256 currPrice = history[i].price;
            
            if (prevPrice > 0) {
                uint256 deviation;
                if (currPrice > prevPrice) {
                    deviation = ((currPrice - prevPrice) * 10000) / prevPrice;
                } else {
                    deviation = ((prevPrice - currPrice) * 10000) / prevPrice;
                }
                sumDeviations += deviation;
            }
        }

        volatility = sumDeviations / periods;
    }

    // ============================================
    // CONFIGURATION
    // ============================================

    /**
     * @notice Sets protection config for a contract
     */
    function setProtectionConfig(
        address protectedContract,
        GuardMode mode,
        uint256 minBlockDelay,
        uint256 minTimeDelay,
        uint256 maxValuePerBlock,
        uint256 maxTxPerBlock,
        uint256 priceDeviationThreshold
    ) external onlyRole(GUARD_ADMIN_ROLE) {
        require(minBlockDelay <= MAX_DELAY_BLOCKS, "Delay too high");

        contractConfigs[protectedContract] = ProtectionConfig({
            mode: mode,
            minBlockDelay: minBlockDelay,
            minTimeDelay: minTimeDelay,
            maxValuePerBlock: maxValuePerBlock,
            maxTxPerBlock: maxTxPerBlock,
            priceDeviationThreshold: priceDeviationThreshold,
            enabled: true
        });

        protectedContracts[protectedContract] = true;

        emit ProtectionConfigured(protectedContract, mode, minBlockDelay);
    }

    /**
     * @notice Toggles contract protection
     */
    function setContractProtected(
        address protectedContract,
        bool enabled
    ) external onlyRole(GUARD_ADMIN_ROLE) {
        protectedContracts[protectedContract] = enabled;
        contractConfigs[protectedContract].enabled = enabled;
        emit ContractProtectionToggled(protectedContract, enabled);
    }

    /**
     * @notice Sets trusted oracle
     */
    function setTrustedOracle(
        address oracle,
        bool trusted
    ) external onlyRole(GUARD_ADMIN_ROLE) {
        trustedOracles[oracle] = trusted;
        emit OracleUpdated(oracle, trusted);
    }

    /**
     * @notice Updates default config
     */
    function setDefaultConfig(ProtectionConfig calldata config) external onlyRole(GUARD_ADMIN_ROLE) {
        defaultConfig = config;
    }

    /**
     * @notice Sets commit validity period
     */
    function setCommitValidityPeriod(uint256 period) external onlyRole(GUARD_ADMIN_ROLE) {
        require(period >= 1 minutes && period <= 1 days, "Invalid period");
        commitValidityPeriod = period;
    }

    /**
     * @notice Gets config for contract
     */
    function _getConfig(address protectedContract) internal view returns (ProtectionConfig memory) {
        ProtectionConfig memory config = contractConfigs[protectedContract];
        if (config.minBlockDelay > 0 || config.mode != GuardMode.OFF) {
            return config;
        }
        return defaultConfig;
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Gets user's tracking info
     */
    function getUserTracker(
        address user,
        address protectedContract
    ) external view returns (BlockTracker memory) {
        return userTrackers[user][protectedContract];
    }

    /**
     * @notice Gets commitment info
     */
    function getCommitment(
        address user,
        bytes32 commitHash
    ) external view returns (Commitment memory) {
        return commitments[user][commitHash];
    }

    /**
     * @notice Gets price history length
     */
    function getPriceHistoryLength(address token) external view returns (uint256) {
        return priceHistory[token].length;
    }

    /**
     * @notice Gets latest price snapshot
     */
    function getLatestPrice(address token) external view returns (PriceSnapshot memory) {
        PriceSnapshot[] storage history = priceHistory[token];
        require(history.length > 0, "No price history");
        return history[history.length - 1];
    }

    /**
     * @notice Checks if address is protected
     */
    function isProtected(address protectedContract) external view returns (bool) {
        return protectedContracts[protectedContract];
    }

    // ============================================
    // PAUSE FUNCTIONS
    // ============================================

    function pause() external onlyRole(GUARD_ADMIN_ROLE) {
        _pause();
    }

    function unpause() external onlyRole(GUARD_ADMIN_ROLE) {
        _unpause();
    }
}
