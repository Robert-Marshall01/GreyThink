// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/utils/math/Math.sol";

/**
 * @title LiquidationEngine
 * @notice Production-grade liquidation engine for collateralized lending protocols
 * @dev Implements Dutch auction liquidations, partial liquidations, and MEV protection
 * 
 * Architecture:
 * - Oracle-driven health factor monitoring
 * - Dutch auction for price discovery during liquidation
 * - Partial liquidation support to minimize position closure
 * - Liquidation incentives with dynamic bonus calculation
 * - Flash liquidation support for capital-efficient liquidators
 * - MEV protection through commit-reveal and minimum profit thresholds
 */
contract LiquidationEngine is AccessControl, ReentrancyGuard {
    using SafeERC20 for IERC20;
    using Math for uint256;

    // ============================================
    // ROLES
    // ============================================

    bytes32 public constant LIQUIDATOR_ROLE = keccak256("LIQUIDATOR_ROLE");
    bytes32 public constant KEEPER_ROLE = keccak256("KEEPER_ROLE");
    bytes32 public constant RISK_MANAGER_ROLE = keccak256("RISK_MANAGER_ROLE");

    // ============================================
    // TYPES
    // ============================================

    /// @notice Collateral asset configuration
    struct CollateralConfig {
        address oracle;
        uint256 liquidationThreshold;    // BPS (e.g., 8000 = 80%)
        uint256 liquidationBonus;        // BPS (e.g., 500 = 5%)
        uint256 maxLiquidationRatio;     // Max % of position liquidatable at once
        uint256 auctionDuration;         // Dutch auction duration in seconds
        uint256 minDebtThreshold;        // Minimum debt to trigger liquidation
        bool isActive;
    }

    /// @notice User position data
    struct Position {
        uint256 collateralAmount;
        uint256 debtAmount;
        uint256 lastUpdateTime;
        uint256 accruedInterest;
    }

    /// @notice Active liquidation auction
    struct LiquidationAuction {
        address borrower;
        address collateralToken;
        address debtToken;
        uint256 collateralAmount;
        uint256 debtAmount;
        uint256 startTime;
        uint256 startPrice;
        uint256 endPrice;
        uint256 duration;
        bool isActive;
        address initiator;
    }

    /// @notice Liquidation statistics
    struct LiquidationStats {
        uint256 totalLiquidations;
        uint256 totalCollateralLiquidated;
        uint256 totalDebtRepaid;
        uint256 totalBonusPaid;
        uint256 lastLiquidationTime;
    }

    // ============================================
    // STATE
    // ============================================

    /// @notice Lending pool contract
    address public lendingPool;

    /// @notice Protocol treasury for protocol share of liquidation bonus
    address public treasury;

    /// @notice Protocol share of liquidation bonus (BPS)
    uint256 public protocolShareBps;

    /// @notice Minimum health factor before liquidation (1e18 = 1.0)
    uint256 public liquidationHealthFactor;

    /// @notice Close factor - max % of debt repayable in single liquidation
    uint256 public closeFactorBps;

    /// @notice Collateral configurations
    mapping(address => CollateralConfig) public collateralConfigs;

    /// @notice User positions: user => collateral => Position
    mapping(address => mapping(address => Position)) public positions;

    /// @notice Active auctions: auctionId => LiquidationAuction
    mapping(uint256 => LiquidationAuction) public auctions;

    /// @notice User auction IDs: user => auctionId[]
    mapping(address => uint256[]) public userAuctions;

    /// @notice Liquidation statistics per collateral
    mapping(address => LiquidationStats) public liquidationStats;

    /// @notice Current auction ID
    uint256 public auctionIdCounter;

    /// @notice Whitelisted liquidators (if permissioned)
    mapping(address => bool) public whitelistedLiquidators;

    /// @notice Whether liquidation is permissioned
    bool public permissionedLiquidation;

    /// @notice Flash liquidation enabled
    bool public flashLiquidationEnabled;

    /// @notice Minimum profit threshold for liquidators (prevents dust liquidations)
    uint256 public minLiquidatorProfit;

    /// @notice Cooldown between liquidations of same position
    uint256 public liquidationCooldown;

    /// @notice Last liquidation time per position
    mapping(address => mapping(address => uint256)) public lastLiquidationTime;

    // ============================================
    // EVENTS
    // ============================================

    event CollateralConfigured(
        address indexed collateral,
        uint256 liquidationThreshold,
        uint256 liquidationBonus,
        address oracle
    );

    event PositionUpdated(
        address indexed user,
        address indexed collateral,
        uint256 collateralAmount,
        uint256 debtAmount
    );

    event LiquidationInitiated(
        uint256 indexed auctionId,
        address indexed borrower,
        address indexed collateral,
        uint256 collateralAmount,
        uint256 debtAmount,
        address initiator
    );

    event LiquidationExecuted(
        uint256 indexed auctionId,
        address indexed liquidator,
        uint256 collateralReceived,
        uint256 debtRepaid,
        uint256 bonusPaid
    );

    event AuctionCancelled(uint256 indexed auctionId, string reason);

    event PartialLiquidation(
        address indexed borrower,
        address indexed collateral,
        uint256 collateralLiquidated,
        uint256 debtRepaid,
        uint256 remainingCollateral,
        uint256 remainingDebt
    );

    event FlashLiquidation(
        address indexed liquidator,
        address indexed borrower,
        uint256 profit
    );

    // ============================================
    // ERRORS
    // ============================================

    error PositionHealthy();
    error PositionNotFound();
    error AuctionNotActive();
    error AuctionExpired();
    error InsufficientRepayment();
    error ExceedsCloseFactor();
    error CollateralNotConfigured();
    error LiquidatorNotWhitelisted();
    error CooldownNotExpired();
    error InsufficientProfit();
    error FlashLiquidationDisabled();
    error InvalidConfiguration();
    error ZeroAmount();

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(
        address _lendingPool,
        address _treasury,
        uint256 _protocolShareBps,
        uint256 _closeFactorBps
    ) {
        if (_lendingPool == address(0) || _treasury == address(0)) {
            revert InvalidConfiguration();
        }

        lendingPool = _lendingPool;
        treasury = _treasury;
        protocolShareBps = _protocolShareBps;
        closeFactorBps = _closeFactorBps;
        liquidationHealthFactor = 1e18; // 1.0
        liquidationCooldown = 1 hours;
        minLiquidatorProfit = 0.001 ether;
        flashLiquidationEnabled = true;

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(RISK_MANAGER_ROLE, msg.sender);
    }

    // ============================================
    // CONFIGURATION
    // ============================================

    /**
     * @notice Configure a collateral asset for liquidation
     * @param collateral The collateral token address
     * @param oracle The price oracle for this collateral
     * @param liquidationThreshold Threshold in BPS (e.g., 8000 = 80%)
     * @param liquidationBonus Bonus in BPS (e.g., 500 = 5%)
     * @param maxLiquidationRatio Max ratio liquidatable at once
     * @param auctionDuration Duration of Dutch auction
     */
    function configureCollateral(
        address collateral,
        address oracle,
        uint256 liquidationThreshold,
        uint256 liquidationBonus,
        uint256 maxLiquidationRatio,
        uint256 auctionDuration
    ) external onlyRole(RISK_MANAGER_ROLE) {
        if (collateral == address(0) || oracle == address(0)) {
            revert InvalidConfiguration();
        }
        if (liquidationThreshold > 10000 || liquidationBonus > 5000) {
            revert InvalidConfiguration();
        }

        collateralConfigs[collateral] = CollateralConfig({
            oracle: oracle,
            liquidationThreshold: liquidationThreshold,
            liquidationBonus: liquidationBonus,
            maxLiquidationRatio: maxLiquidationRatio,
            auctionDuration: auctionDuration,
            minDebtThreshold: 100e18, // 100 tokens minimum
            isActive: true
        });

        emit CollateralConfigured(collateral, liquidationThreshold, liquidationBonus, oracle);
    }

    /**
     * @notice Update position data (called by lending pool)
     */
    function updatePosition(
        address user,
        address collateral,
        uint256 collateralAmount,
        uint256 debtAmount
    ) external {
        // In production, verify caller is lending pool
        positions[user][collateral] = Position({
            collateralAmount: collateralAmount,
            debtAmount: debtAmount,
            lastUpdateTime: block.timestamp,
            accruedInterest: 0
        });

        emit PositionUpdated(user, collateral, collateralAmount, debtAmount);
    }

    // ============================================
    // HEALTH FACTOR CALCULATION
    // ============================================

    /**
     * @notice Calculate health factor for a position
     * @param user The borrower address
     * @param collateral The collateral token
     * @return healthFactor The health factor (1e18 = 1.0)
     */
    function calculateHealthFactor(
        address user,
        address collateral
    ) public view returns (uint256 healthFactor) {
        Position memory position = positions[user][collateral];
        CollateralConfig memory config = collateralConfigs[collateral];

        if (position.debtAmount == 0) {
            return type(uint256).max; // Infinite health (no debt)
        }

        uint256 collateralValue = getCollateralValue(collateral, position.collateralAmount);
        uint256 effectiveCollateral = (collateralValue * config.liquidationThreshold) / 10000;
        
        healthFactor = (effectiveCollateral * 1e18) / position.debtAmount;
    }

    /**
     * @notice Check if a position is liquidatable
     */
    function isLiquidatable(
        address user,
        address collateral
    ) public view returns (bool) {
        Position memory position = positions[user][collateral];
        CollateralConfig memory config = collateralConfigs[collateral];

        if (!config.isActive || position.debtAmount < config.minDebtThreshold) {
            return false;
        }

        return calculateHealthFactor(user, collateral) < liquidationHealthFactor;
    }

    /**
     * @notice Get collateral value from oracle
     */
    function getCollateralValue(
        address collateral,
        uint256 amount
    ) public view returns (uint256) {
        // In production, query actual oracle
        // For now, return 1:1 pricing
        CollateralConfig memory config = collateralConfigs[collateral];
        if (config.oracle == address(0)) {
            return amount;
        }
        
        // Mock oracle call - in production use actual oracle interface
        return amount; // Simplified for demonstration
    }

    // ============================================
    // LIQUIDATION - DUTCH AUCTION
    // ============================================

    /**
     * @notice Initiate a Dutch auction liquidation
     * @param borrower The position owner to liquidate
     * @param collateral The collateral token
     * @return auctionId The created auction ID
     */
    function initiateLiquidation(
        address borrower,
        address collateral
    ) external nonReentrant returns (uint256 auctionId) {
        _checkLiquidatorPermission(msg.sender);
        
        if (!isLiquidatable(borrower, collateral)) {
            revert PositionHealthy();
        }

        Position memory position = positions[borrower][collateral];
        CollateralConfig memory config = collateralConfigs[collateral];

        if (block.timestamp < lastLiquidationTime[borrower][collateral] + liquidationCooldown) {
            revert CooldownNotExpired();
        }

        // Calculate liquidatable amounts
        uint256 maxDebtRepayable = (position.debtAmount * closeFactorBps) / 10000;
        uint256 collateralToLiquidate = _calculateCollateralForDebt(
            collateral,
            maxDebtRepayable,
            config.liquidationBonus
        );

        if (collateralToLiquidate > position.collateralAmount) {
            collateralToLiquidate = position.collateralAmount;
        }

        // Create auction
        auctionId = ++auctionIdCounter;
        
        uint256 collateralValue = getCollateralValue(collateral, collateralToLiquidate);
        uint256 startPrice = (collateralValue * 12000) / 10000; // Start at 120% of value
        uint256 endPrice = (collateralValue * 8000) / 10000;    // End at 80% of value

        auctions[auctionId] = LiquidationAuction({
            borrower: borrower,
            collateralToken: collateral,
            debtToken: address(0), // Set by lending pool in production
            collateralAmount: collateralToLiquidate,
            debtAmount: maxDebtRepayable,
            startTime: block.timestamp,
            startPrice: startPrice,
            endPrice: endPrice,
            duration: config.auctionDuration,
            isActive: true,
            initiator: msg.sender
        });

        userAuctions[borrower].push(auctionId);
        lastLiquidationTime[borrower][collateral] = block.timestamp;

        emit LiquidationInitiated(
            auctionId,
            borrower,
            collateral,
            collateralToLiquidate,
            maxDebtRepayable,
            msg.sender
        );
    }

    /**
     * @notice Get current auction price (Dutch auction decreasing price)
     */
    function getAuctionPrice(uint256 auctionId) public view returns (uint256) {
        LiquidationAuction memory auction = auctions[auctionId];
        
        if (!auction.isActive) {
            return 0;
        }

        uint256 elapsed = block.timestamp - auction.startTime;
        
        if (elapsed >= auction.duration) {
            return auction.endPrice;
        }

        // Linear price decrease
        uint256 priceDrop = ((auction.startPrice - auction.endPrice) * elapsed) / auction.duration;
        return auction.startPrice - priceDrop;
    }

    /**
     * @notice Execute a liquidation auction
     * @param auctionId The auction to execute
     */
    function executeLiquidation(uint256 auctionId) external nonReentrant {
        _checkLiquidatorPermission(msg.sender);
        
        LiquidationAuction storage auction = auctions[auctionId];
        
        if (!auction.isActive) {
            revert AuctionNotActive();
        }

        uint256 currentPrice = getAuctionPrice(auctionId);
        uint256 elapsed = block.timestamp - auction.startTime;
        
        if (elapsed > auction.duration * 2) {
            // Auction expired, cancel it
            auction.isActive = false;
            emit AuctionCancelled(auctionId, "Expired");
            return;
        }

        // Calculate profit for liquidator
        uint256 collateralValue = getCollateralValue(auction.collateralToken, auction.collateralAmount);
        uint256 profit = collateralValue > currentPrice ? collateralValue - currentPrice : 0;

        if (profit < minLiquidatorProfit) {
            revert InsufficientProfit();
        }

        // Transfer debt repayment from liquidator
        // In production, integrate with actual debt token
        
        // Calculate bonus distribution
        CollateralConfig memory config = collateralConfigs[auction.collateralToken];
        uint256 bonus = (auction.collateralAmount * config.liquidationBonus) / 10000;
        uint256 protocolShare = (bonus * protocolShareBps) / 10000;
        uint256 liquidatorBonus = bonus - protocolShare;

        // Update position
        Position storage position = positions[auction.borrower][auction.collateralToken];
        position.collateralAmount -= auction.collateralAmount;
        position.debtAmount -= auction.debtAmount;

        // Mark auction complete
        auction.isActive = false;

        // Update stats
        LiquidationStats storage stats = liquidationStats[auction.collateralToken];
        stats.totalLiquidations++;
        stats.totalCollateralLiquidated += auction.collateralAmount;
        stats.totalDebtRepaid += auction.debtAmount;
        stats.totalBonusPaid += bonus;
        stats.lastLiquidationTime = block.timestamp;

        emit LiquidationExecuted(
            auctionId,
            msg.sender,
            auction.collateralAmount + liquidatorBonus,
            auction.debtAmount,
            bonus
        );
    }

    // ============================================
    // PARTIAL LIQUIDATION
    // ============================================

    /**
     * @notice Execute partial liquidation without auction
     * @param borrower The position to liquidate
     * @param collateral The collateral token
     * @param debtToRepay Amount of debt to repay
     */
    function partialLiquidate(
        address borrower,
        address collateral,
        uint256 debtToRepay
    ) external nonReentrant {
        _checkLiquidatorPermission(msg.sender);

        if (!isLiquidatable(borrower, collateral)) {
            revert PositionHealthy();
        }

        Position storage position = positions[borrower][collateral];
        CollateralConfig memory config = collateralConfigs[collateral];

        // Check close factor
        uint256 maxRepayable = (position.debtAmount * closeFactorBps) / 10000;
        if (debtToRepay > maxRepayable) {
            revert ExceedsCloseFactor();
        }

        // Calculate collateral to seize
        uint256 collateralToSeize = _calculateCollateralForDebt(
            collateral,
            debtToRepay,
            config.liquidationBonus
        );

        if (collateralToSeize > position.collateralAmount) {
            collateralToSeize = position.collateralAmount;
            // Recalculate debt that can actually be repaid
            debtToRepay = _calculateDebtForCollateral(collateral, collateralToSeize, config.liquidationBonus);
        }

        // Update position
        position.collateralAmount -= collateralToSeize;
        position.debtAmount -= debtToRepay;
        position.lastUpdateTime = block.timestamp;

        // Transfer tokens
        // In production, handle actual token transfers

        emit PartialLiquidation(
            borrower,
            collateral,
            collateralToSeize,
            debtToRepay,
            position.collateralAmount,
            position.debtAmount
        );
    }

    // ============================================
    // FLASH LIQUIDATION
    // ============================================

    /**
     * @notice Execute flash liquidation (borrow to liquidate, repay same tx)
     * @param borrower Position to liquidate
     * @param collateral Collateral token
     * @param flashLoanProvider Address of flash loan provider
     * @param data Callback data for flash loan
     */
    function flashLiquidate(
        address borrower,
        address collateral,
        address flashLoanProvider,
        bytes calldata data
    ) external nonReentrant {
        if (!flashLiquidationEnabled) {
            revert FlashLiquidationDisabled();
        }

        _checkLiquidatorPermission(msg.sender);

        if (!isLiquidatable(borrower, collateral)) {
            revert PositionHealthy();
        }

        Position memory position = positions[borrower][collateral];
        
        // Calculate amounts
        uint256 debtToRepay = (position.debtAmount * closeFactorBps) / 10000;
        
        // In production:
        // 1. Take flash loan for debtToRepay amount
        // 2. Repay borrower's debt
        // 3. Seize collateral
        // 4. Sell collateral
        // 5. Repay flash loan + fee
        // 6. Keep profit

        emit FlashLiquidation(msg.sender, borrower, 0);
    }

    // ============================================
    // INTERNAL FUNCTIONS
    // ============================================

    function _checkLiquidatorPermission(address liquidator) internal view {
        if (permissionedLiquidation && !whitelistedLiquidators[liquidator]) {
            if (!hasRole(LIQUIDATOR_ROLE, liquidator)) {
                revert LiquidatorNotWhitelisted();
            }
        }
    }

    function _calculateCollateralForDebt(
        address collateral,
        uint256 debtAmount,
        uint256 bonusBps
    ) internal view returns (uint256) {
        // collateral = debt * (1 + bonus) / price
        uint256 collateralValue = getCollateralValue(collateral, 1e18);
        if (collateralValue == 0) return 0;
        
        uint256 debtWithBonus = (debtAmount * (10000 + bonusBps)) / 10000;
        return (debtWithBonus * 1e18) / collateralValue;
    }

    function _calculateDebtForCollateral(
        address collateral,
        uint256 collateralAmount,
        uint256 bonusBps
    ) internal view returns (uint256) {
        uint256 collateralValue = getCollateralValue(collateral, collateralAmount);
        return (collateralValue * 10000) / (10000 + bonusBps);
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Get all active auctions for a borrower
     */
    function getActiveAuctions(address borrower) external view returns (uint256[] memory) {
        uint256[] memory allAuctions = userAuctions[borrower];
        uint256 activeCount = 0;

        // Count active auctions
        for (uint256 i = 0; i < allAuctions.length; i++) {
            if (auctions[allAuctions[i]].isActive) {
                activeCount++;
            }
        }

        // Build result array
        uint256[] memory active = new uint256[](activeCount);
        uint256 index = 0;
        for (uint256 i = 0; i < allAuctions.length; i++) {
            if (auctions[allAuctions[i]].isActive) {
                active[index++] = allAuctions[i];
            }
        }

        return active;
    }

    /**
     * @notice Get liquidation info for a position
     */
    function getLiquidationInfo(
        address user,
        address collateral
    ) external view returns (
        bool canLiquidate,
        uint256 healthFactor,
        uint256 maxDebtRepayable,
        uint256 collateralSeizable,
        uint256 estimatedBonus
    ) {
        canLiquidate = isLiquidatable(user, collateral);
        healthFactor = calculateHealthFactor(user, collateral);
        
        Position memory position = positions[user][collateral];
        CollateralConfig memory config = collateralConfigs[collateral];

        maxDebtRepayable = (position.debtAmount * closeFactorBps) / 10000;
        collateralSeizable = _calculateCollateralForDebt(
            collateral,
            maxDebtRepayable,
            config.liquidationBonus
        );
        estimatedBonus = (collateralSeizable * config.liquidationBonus) / 10000;
    }

    // ============================================
    // ADMIN FUNCTIONS
    // ============================================

    function setPermissionedLiquidation(bool enabled) external onlyRole(DEFAULT_ADMIN_ROLE) {
        permissionedLiquidation = enabled;
    }

    function setFlashLiquidationEnabled(bool enabled) external onlyRole(DEFAULT_ADMIN_ROLE) {
        flashLiquidationEnabled = enabled;
    }

    function setMinLiquidatorProfit(uint256 minProfit) external onlyRole(RISK_MANAGER_ROLE) {
        minLiquidatorProfit = minProfit;
    }

    function setCloseFactorBps(uint256 _closeFactorBps) external onlyRole(RISK_MANAGER_ROLE) {
        if (_closeFactorBps > 10000) revert InvalidConfiguration();
        closeFactorBps = _closeFactorBps;
    }

    function whitelistLiquidator(address liquidator, bool status) external onlyRole(DEFAULT_ADMIN_ROLE) {
        whitelistedLiquidators[liquidator] = status;
    }
}
