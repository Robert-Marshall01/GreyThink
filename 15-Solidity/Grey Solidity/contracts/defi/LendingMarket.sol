// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/Pausable.sol";
import "@openzeppelin/contracts/utils/math/Math.sol";

/**
 * @title LendingMarket
 * @author Grey Protocol Team
 * @notice Advanced lending market with isolated and cross-margin modes
 * @dev Implements comprehensive lending with advanced risk management
 * 
 * Features:
 * - Multiple collateral types per position
 * - Isolated and cross-margin modes
 * - Dynamic interest rate model
 * - Partial liquidations
 * - Soft and hard liquidation thresholds
 * - Interest rate caps
 * - Position health tracking
 */
contract LendingMarket is AccessControl, ReentrancyGuard, Pausable {
    using SafeERC20 for IERC20;
    using Math for uint256;

    // ============================================
    // CONSTANTS & ROLES
    // ============================================

    bytes32 public constant MARKET_ADMIN_ROLE = keccak256("MARKET_ADMIN_ROLE");
    bytes32 public constant RISK_MANAGER_ROLE = keccak256("RISK_MANAGER_ROLE");
    bytes32 public constant LIQUIDATOR_ROLE = keccak256("LIQUIDATOR_ROLE");
    bytes32 public constant ORACLE_UPDATER_ROLE = keccak256("ORACLE_UPDATER_ROLE");

    uint256 public constant PRECISION = 1e18;
    uint256 public constant RATE_PRECISION = 1e27;
    uint256 public constant BPS = 10000;
    uint256 public constant SECONDS_PER_YEAR = 31536000;
    uint256 public constant MAX_MARKETS = 50;
    uint256 public constant MAX_COLLATERALS_PER_POSITION = 10;

    // ============================================
    // ENUMS
    // ============================================

    enum MarginMode {
        Isolated,
        Cross
    }

    enum PositionStatus {
        Active,
        PartiallyLiquidated,
        Liquidated,
        Closed
    }

    enum LiquidationType {
        Soft,
        Hard
    }

    // ============================================
    // STRUCTS
    // ============================================

    /**
     * @notice Market configuration for an asset
     */
    struct MarketConfig {
        address asset;
        address oracle;
        uint256 maxLTV;                    // Max LTV in BPS (e.g., 7500 = 75%)
        uint256 liquidationThreshold;      // Soft liquidation threshold in BPS
        uint256 hardLiquidationThreshold;  // Hard liquidation threshold in BPS
        uint256 liquidationPenalty;        // Penalty in BPS
        uint256 borrowCap;                 // Maximum borrow amount
        uint256 supplyCap;                 // Maximum supply amount
        uint256 reserveFactor;             // Protocol fee on interest
        uint256 minBorrowAmount;           // Minimum borrow size
        uint8 decimals;
        bool isActive;
        bool isBorrowable;
        bool isCollateral;
        bool isPaused;
    }

    /**
     * @notice Market state data
     */
    struct MarketState {
        uint256 totalSupply;
        uint256 totalBorrows;
        uint256 supplyIndex;
        uint256 borrowIndex;
        uint256 supplyRate;
        uint256 borrowRate;
        uint256 lastAccrualTime;
        uint256 accumulatedFees;
        uint256 utilizationRate;
    }

    /**
     * @notice Interest rate model
     */
    struct InterestRateModel {
        uint256 baseRate;
        uint256 multiplier;
        uint256 jumpMultiplier;
        uint256 kink;
        uint256 maxRate;
    }

    /**
     * @notice User position data
     */
    struct Position {
        uint256 positionId;
        address owner;
        MarginMode mode;
        PositionStatus status;
        uint256 createdAt;
        uint256 lastUpdatedAt;
        uint256 healthFactor;
        mapping(address => uint256) collateralBalances;
        mapping(address => uint256) borrowBalances;
        mapping(address => uint256) borrowIndices;
        address[] collateralAssets;
        address[] borrowAssets;
    }

    /**
     * @notice Position snapshot for view functions
     */
    struct PositionSnapshot {
        uint256 positionId;
        address owner;
        MarginMode mode;
        PositionStatus status;
        uint256 healthFactor;
        uint256 totalCollateralValue;
        uint256 totalBorrowValue;
        uint256 availableBorrow;
        address[] collateralAssets;
        uint256[] collateralBalances;
        address[] borrowAssets;
        uint256[] borrowBalances;
    }

    /**
     * @notice Liquidation parameters
     */
    struct LiquidationParams {
        uint256 positionId;
        address collateralAsset;
        address borrowAsset;
        uint256 repayAmount;
        uint256 minCollateralReceived;
        bool receiveUnderlying;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Market configurations
    mapping(address => MarketConfig) public marketConfigs;
    
    /// @notice Market states
    mapping(address => MarketState) public marketStates;
    
    /// @notice Interest rate models
    mapping(address => InterestRateModel) public interestRateModels;
    
    /// @notice Active markets list
    address[] public activeMarkets;
    
    /// @notice Market index lookup
    mapping(address => uint256) public marketIndex;
    
    /// @notice User positions
    mapping(uint256 => Position) internal positions;
    
    /// @notice User position IDs
    mapping(address => uint256[]) public userPositions;
    
    /// @notice Position counter
    uint256 public positionCounter;
    
    /// @notice Cross margin position for users
    mapping(address => uint256) public crossMarginPosition;
    
    /// @notice User supply balances (for cross-margin)
    mapping(address => mapping(address => uint256)) public userSupplyBalance;
    
    /// @notice User supply index
    mapping(address => mapping(address => uint256)) public userSupplyIndex;
    
    /// @notice Protocol treasury
    address public treasury;
    
    /// @notice Flash loan fee (in BPS)
    uint256 public flashLoanFee = 9; // 0.09%
    
    /// @notice Insurance fund
    address public insuranceFund;
    
    /// @notice Bad debt tracked per market
    mapping(address => uint256) public badDebt;

    // ============================================
    // EVENTS
    // ============================================

    event MarketAdded(
        address indexed asset,
        uint256 maxLTV,
        uint256 liquidationThreshold,
        uint256 borrowCap,
        uint256 supplyCap
    );

    event MarketUpdated(address indexed asset, string parameter, uint256 value);

    event Supplied(
        address indexed user,
        address indexed asset,
        uint256 amount,
        uint256 shares
    );

    event Withdrawn(
        address indexed user,
        address indexed asset,
        uint256 amount,
        uint256 shares
    );

    event PositionOpened(
        uint256 indexed positionId,
        address indexed owner,
        MarginMode mode
    );

    event CollateralAdded(
        uint256 indexed positionId,
        address indexed asset,
        uint256 amount
    );

    event CollateralRemoved(
        uint256 indexed positionId,
        address indexed asset,
        uint256 amount
    );

    event Borrowed(
        uint256 indexed positionId,
        address indexed asset,
        uint256 amount,
        uint256 rate
    );

    event Repaid(
        uint256 indexed positionId,
        address indexed asset,
        uint256 amount,
        address repayer
    );

    event PositionLiquidated(
        uint256 indexed positionId,
        address indexed liquidator,
        address collateralAsset,
        address borrowAsset,
        uint256 collateralSeized,
        uint256 debtRepaid,
        LiquidationType liquidationType
    );

    event InterestAccrued(
        address indexed asset,
        uint256 supplyInterest,
        uint256 borrowInterest,
        uint256 protocolFees
    );

    event FlashLoan(
        address indexed initiator,
        address indexed asset,
        uint256 amount,
        uint256 fee
    );

    event BadDebtRealized(address indexed asset, uint256 amount);

    // ============================================
    // ERRORS
    // ============================================

    error MarketNotActive(address asset);
    error MarketPaused(address asset);
    error NotBorrowable(address asset);
    error NotCollateral(address asset);
    error PositionNotFound(uint256 positionId);
    error NotPositionOwner(uint256 positionId, address caller);
    error InsufficientCollateral();
    error InsufficientLiquidity(uint256 available, uint256 requested);
    error BorrowCapExceeded(address asset, uint256 cap, uint256 requested);
    error SupplyCapExceeded(address asset, uint256 cap, uint256 requested);
    error HealthFactorTooLow(uint256 healthFactor);
    error PositionNotLiquidatable(uint256 healthFactor);
    error InvalidAmount();
    error InvalidAsset();
    error MaxCollateralsReached();
    error FlashLoanFailed();
    error MinBorrowNotMet(uint256 minBorrow, uint256 requested);
    error CrossPositionExists();
    error WrongMarginMode();

    // ============================================
    // MODIFIERS
    // ============================================

    modifier marketActive(address asset) {
        if (!marketConfigs[asset].isActive) revert MarketNotActive(asset);
        if (marketConfigs[asset].isPaused) revert MarketPaused(asset);
        _;
    }

    modifier positionExists(uint256 positionId) {
        if (positions[positionId].owner == address(0)) revert PositionNotFound(positionId);
        _;
    }

    modifier onlyPositionOwner(uint256 positionId) {
        if (positions[positionId].owner != msg.sender) {
            revert NotPositionOwner(positionId, msg.sender);
        }
        _;
    }

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(address _treasury, address _insuranceFund) {
        treasury = _treasury;
        insuranceFund = _insuranceFund;

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(MARKET_ADMIN_ROLE, msg.sender);
        _grantRole(RISK_MANAGER_ROLE, msg.sender);
    }

    // ============================================
    // MARKET MANAGEMENT
    // ============================================

    /**
     * @notice Adds a new market
     * @param config Market configuration
     * @param rateModel Interest rate model
     */
    function addMarket(
        MarketConfig calldata config,
        InterestRateModel calldata rateModel
    ) external onlyRole(MARKET_ADMIN_ROLE) {
        require(config.asset != address(0), "Invalid asset");
        require(!marketConfigs[config.asset].isActive, "Market exists");
        require(activeMarkets.length < MAX_MARKETS, "Max markets reached");

        marketConfigs[config.asset] = config;
        interestRateModels[config.asset] = rateModel;
        
        marketStates[config.asset] = MarketState({
            totalSupply: 0,
            totalBorrows: 0,
            supplyIndex: RATE_PRECISION,
            borrowIndex: RATE_PRECISION,
            supplyRate: 0,
            borrowRate: rateModel.baseRate,
            lastAccrualTime: block.timestamp,
            accumulatedFees: 0,
            utilizationRate: 0
        });

        marketIndex[config.asset] = activeMarkets.length;
        activeMarkets.push(config.asset);

        emit MarketAdded(
            config.asset,
            config.maxLTV,
            config.liquidationThreshold,
            config.borrowCap,
            config.supplyCap
        );
    }

    /**
     * @notice Updates market configuration
     */
    function updateMarketConfig(
        address asset,
        uint256 maxLTV,
        uint256 liquidationThreshold,
        uint256 hardLiquidationThreshold,
        uint256 liquidationPenalty
    ) external onlyRole(RISK_MANAGER_ROLE) {
        MarketConfig storage config = marketConfigs[asset];
        require(config.isActive, "Market not active");
        require(maxLTV < liquidationThreshold, "Invalid LTV");
        require(liquidationThreshold < hardLiquidationThreshold, "Invalid thresholds");

        config.maxLTV = maxLTV;
        config.liquidationThreshold = liquidationThreshold;
        config.hardLiquidationThreshold = hardLiquidationThreshold;
        config.liquidationPenalty = liquidationPenalty;

        emit MarketUpdated(asset, "riskParams", maxLTV);
    }

    /**
     * @notice Updates interest rate model
     */
    function updateInterestRateModel(
        address asset,
        InterestRateModel calldata model
    ) external onlyRole(RISK_MANAGER_ROLE) {
        require(marketConfigs[asset].isActive, "Market not active");
        interestRateModels[asset] = model;
        
        // Recalculate rates
        _accrueInterest(asset);
        _updateInterestRates(asset);
    }

    /**
     * @notice Pauses a market
     */
    function pauseMarket(address asset) external onlyRole(RISK_MANAGER_ROLE) {
        marketConfigs[asset].isPaused = true;
        emit MarketUpdated(asset, "paused", 1);
    }

    /**
     * @notice Unpauses a market
     */
    function unpauseMarket(address asset) external onlyRole(RISK_MANAGER_ROLE) {
        marketConfigs[asset].isPaused = false;
        emit MarketUpdated(asset, "paused", 0);
    }

    // ============================================
    // SUPPLY FUNCTIONS
    // ============================================

    /**
     * @notice Supplies assets to the lending market
     * @param asset Asset to supply
     * @param amount Amount to supply
     */
    function supply(
        address asset,
        uint256 amount
    ) external nonReentrant whenNotPaused marketActive(asset) {
        if (amount == 0) revert InvalidAmount();

        MarketConfig storage config = marketConfigs[asset];
        MarketState storage state = marketStates[asset];

        // Accrue interest first
        _accrueInterest(asset);

        // Check supply cap
        if (config.supplyCap > 0 && state.totalSupply + amount > config.supplyCap) {
            revert SupplyCapExceeded(asset, config.supplyCap, state.totalSupply + amount);
        }

        // Calculate shares
        uint256 shares = _toShares(amount, state.supplyIndex);

        // Update user balance
        if (userSupplyIndex[msg.sender][asset] == 0) {
            userSupplyIndex[msg.sender][asset] = state.supplyIndex;
        } else {
            // Accrue user's interest
            uint256 accrued = _calculateAccruedInterest(
                userSupplyBalance[msg.sender][asset],
                userSupplyIndex[msg.sender][asset],
                state.supplyIndex
            );
            userSupplyBalance[msg.sender][asset] += accrued;
            userSupplyIndex[msg.sender][asset] = state.supplyIndex;
        }

        userSupplyBalance[msg.sender][asset] += amount;
        state.totalSupply += amount;

        // Update rates
        _updateInterestRates(asset);

        // Transfer tokens
        IERC20(asset).safeTransferFrom(msg.sender, address(this), amount);

        emit Supplied(msg.sender, asset, amount, shares);
    }

    /**
     * @notice Withdraws supplied assets
     * @param asset Asset to withdraw
     * @param amount Amount to withdraw (max uint for all)
     */
    function withdraw(
        address asset,
        uint256 amount
    ) external nonReentrant whenNotPaused {
        MarketState storage state = marketStates[asset];

        // Accrue interest
        _accrueInterest(asset);

        // Accrue user's interest
        uint256 accrued = _calculateAccruedInterest(
            userSupplyBalance[msg.sender][asset],
            userSupplyIndex[msg.sender][asset],
            state.supplyIndex
        );
        userSupplyBalance[msg.sender][asset] += accrued;
        userSupplyIndex[msg.sender][asset] = state.supplyIndex;

        uint256 maxWithdraw = userSupplyBalance[msg.sender][asset];
        uint256 toWithdraw = amount == type(uint256).max ? maxWithdraw : amount;

        if (toWithdraw == 0 || toWithdraw > maxWithdraw) revert InvalidAmount();

        // Check available liquidity
        uint256 available = state.totalSupply - state.totalBorrows;
        if (toWithdraw > available) {
            revert InsufficientLiquidity(available, toWithdraw);
        }

        // Update balances
        userSupplyBalance[msg.sender][asset] -= toWithdraw;
        state.totalSupply -= toWithdraw;

        // Update rates
        _updateInterestRates(asset);

        // Transfer tokens
        IERC20(asset).safeTransfer(msg.sender, toWithdraw);

        emit Withdrawn(msg.sender, asset, toWithdraw, _toShares(toWithdraw, state.supplyIndex));
    }

    // ============================================
    // POSITION MANAGEMENT
    // ============================================

    /**
     * @notice Opens a new isolated position
     */
    function openIsolatedPosition() external nonReentrant whenNotPaused returns (uint256 positionId) {
        positionId = ++positionCounter;

        Position storage position = positions[positionId];
        position.positionId = positionId;
        position.owner = msg.sender;
        position.mode = MarginMode.Isolated;
        position.status = PositionStatus.Active;
        position.createdAt = block.timestamp;
        position.lastUpdatedAt = block.timestamp;
        position.healthFactor = type(uint256).max;

        userPositions[msg.sender].push(positionId);

        emit PositionOpened(positionId, msg.sender, MarginMode.Isolated);
    }

    /**
     * @notice Opens or gets cross-margin position
     */
    function openCrossPosition() external nonReentrant whenNotPaused returns (uint256 positionId) {
        if (crossMarginPosition[msg.sender] != 0) {
            return crossMarginPosition[msg.sender];
        }

        positionId = ++positionCounter;

        Position storage position = positions[positionId];
        position.positionId = positionId;
        position.owner = msg.sender;
        position.mode = MarginMode.Cross;
        position.status = PositionStatus.Active;
        position.createdAt = block.timestamp;
        position.lastUpdatedAt = block.timestamp;
        position.healthFactor = type(uint256).max;

        crossMarginPosition[msg.sender] = positionId;
        userPositions[msg.sender].push(positionId);

        emit PositionOpened(positionId, msg.sender, MarginMode.Cross);
    }

    /**
     * @notice Adds collateral to a position
     */
    function addCollateral(
        uint256 positionId,
        address asset,
        uint256 amount
    ) 
        external 
        nonReentrant 
        whenNotPaused 
        positionExists(positionId)
        onlyPositionOwner(positionId)
        marketActive(asset)
    {
        if (amount == 0) revert InvalidAmount();
        if (!marketConfigs[asset].isCollateral) revert NotCollateral(asset);

        Position storage position = positions[positionId];
        
        // Check max collaterals
        if (position.collateralBalances[asset] == 0) {
            if (position.collateralAssets.length >= MAX_COLLATERALS_PER_POSITION) {
                revert MaxCollateralsReached();
            }
            position.collateralAssets.push(asset);
        }

        position.collateralBalances[asset] += amount;
        position.lastUpdatedAt = block.timestamp;

        // Update health factor
        _updatePositionHealth(positionId);

        // Transfer tokens
        IERC20(asset).safeTransferFrom(msg.sender, address(this), amount);

        emit CollateralAdded(positionId, asset, amount);
    }

    /**
     * @notice Removes collateral from a position
     */
    function removeCollateral(
        uint256 positionId,
        address asset,
        uint256 amount
    )
        external
        nonReentrant
        whenNotPaused
        positionExists(positionId)
        onlyPositionOwner(positionId)
    {
        if (amount == 0) revert InvalidAmount();

        Position storage position = positions[positionId];
        
        if (position.collateralBalances[asset] < amount) revert InvalidAmount();

        // Accrue interest on borrows first
        for (uint256 i = 0; i < position.borrowAssets.length; i++) {
            _accrueInterest(position.borrowAssets[i]);
        }

        position.collateralBalances[asset] -= amount;
        position.lastUpdatedAt = block.timestamp;

        // Calculate new health factor
        uint256 newHealth = _calculatePositionHealth(positionId);
        if (newHealth < PRECISION) {
            revert HealthFactorTooLow(newHealth);
        }

        position.healthFactor = newHealth;

        // Remove from array if zero
        if (position.collateralBalances[asset] == 0) {
            _removeAssetFromArray(position.collateralAssets, asset);
        }

        // Transfer tokens
        IERC20(asset).safeTransfer(msg.sender, amount);

        emit CollateralRemoved(positionId, asset, amount);
    }

    /**
     * @notice Borrows assets against position collateral
     */
    function borrow(
        uint256 positionId,
        address asset,
        uint256 amount
    )
        external
        nonReentrant
        whenNotPaused
        positionExists(positionId)
        onlyPositionOwner(positionId)
        marketActive(asset)
    {
        if (amount == 0) revert InvalidAmount();
        
        MarketConfig storage config = marketConfigs[asset];
        MarketState storage state = marketStates[asset];
        
        if (!config.isBorrowable) revert NotBorrowable(asset);
        if (amount < config.minBorrowAmount) revert MinBorrowNotMet(config.minBorrowAmount, amount);

        // Accrue interest
        _accrueInterest(asset);

        // Check borrow cap
        if (config.borrowCap > 0 && state.totalBorrows + amount > config.borrowCap) {
            revert BorrowCapExceeded(asset, config.borrowCap, state.totalBorrows + amount);
        }

        // Check liquidity
        uint256 available = state.totalSupply - state.totalBorrows;
        if (amount > available) {
            revert InsufficientLiquidity(available, amount);
        }

        Position storage position = positions[positionId];

        // Add to borrow tracking
        if (position.borrowBalances[asset] == 0) {
            position.borrowAssets.push(asset);
            position.borrowIndices[asset] = state.borrowIndex;
        } else {
            // Accrue existing borrow
            uint256 accrued = _calculateAccruedInterest(
                position.borrowBalances[asset],
                position.borrowIndices[asset],
                state.borrowIndex
            );
            position.borrowBalances[asset] += accrued;
            position.borrowIndices[asset] = state.borrowIndex;
        }

        position.borrowBalances[asset] += amount;
        position.lastUpdatedAt = block.timestamp;
        state.totalBorrows += amount;

        // Check health factor
        uint256 newHealth = _calculatePositionHealth(positionId);
        if (newHealth < PRECISION) {
            revert HealthFactorTooLow(newHealth);
        }
        position.healthFactor = newHealth;

        // Update rates
        _updateInterestRates(asset);

        // Transfer tokens
        IERC20(asset).safeTransfer(msg.sender, amount);

        emit Borrowed(positionId, asset, amount, state.borrowRate);
    }

    /**
     * @notice Repays borrowed assets
     */
    function repay(
        uint256 positionId,
        address asset,
        uint256 amount
    )
        external
        nonReentrant
        whenNotPaused
        positionExists(positionId)
    {
        if (amount == 0) revert InvalidAmount();

        Position storage position = positions[positionId];
        MarketState storage state = marketStates[asset];

        // Accrue interest
        _accrueInterest(asset);

        // Accrue position borrow
        uint256 accrued = _calculateAccruedInterest(
            position.borrowBalances[asset],
            position.borrowIndices[asset],
            state.borrowIndex
        );
        position.borrowBalances[asset] += accrued;
        position.borrowIndices[asset] = state.borrowIndex;

        uint256 currentBorrow = position.borrowBalances[asset];
        if (currentBorrow == 0) revert InvalidAmount();

        uint256 toRepay = amount == type(uint256).max ? currentBorrow : amount;
        if (toRepay > currentBorrow) {
            toRepay = currentBorrow;
        }

        position.borrowBalances[asset] -= toRepay;
        position.lastUpdatedAt = block.timestamp;
        state.totalBorrows -= toRepay;

        // Remove from array if fully repaid
        if (position.borrowBalances[asset] == 0) {
            _removeAssetFromArray(position.borrowAssets, asset);
        }

        // Update health
        _updatePositionHealth(positionId);

        // Update rates
        _updateInterestRates(asset);

        // Transfer tokens
        IERC20(asset).safeTransferFrom(msg.sender, address(this), toRepay);

        emit Repaid(positionId, asset, toRepay, msg.sender);
    }

    // ============================================
    // LIQUIDATION
    // ============================================

    /**
     * @notice Liquidates an undercollateralized position
     */
    function liquidate(
        LiquidationParams calldata params
    ) external nonReentrant whenNotPaused positionExists(params.positionId) {
        Position storage position = positions[params.positionId];
        
        // Accrue all interest
        _accrueAllPositionInterest(params.positionId);

        // Calculate health factor
        uint256 healthFactor = _calculatePositionHealth(params.positionId);
        
        MarketConfig storage collateralConfig = marketConfigs[params.collateralAsset];
        
        // Determine liquidation type
        LiquidationType liquidationType;
        uint256 maxRepayRatio;
        
        if (healthFactor < (collateralConfig.hardLiquidationThreshold * PRECISION) / BPS) {
            // Hard liquidation - can liquidate 100%
            liquidationType = LiquidationType.Hard;
            maxRepayRatio = BPS;
        } else if (healthFactor < PRECISION) {
            // Soft liquidation - can liquidate 50%
            liquidationType = LiquidationType.Soft;
            maxRepayRatio = 5000;
        } else {
            revert PositionNotLiquidatable(healthFactor);
        }

        MarketState storage borrowState = marketStates[params.borrowAsset];

        // Calculate max repay
        uint256 maxRepay = (position.borrowBalances[params.borrowAsset] * maxRepayRatio) / BPS;
        uint256 repayAmount = params.repayAmount > maxRepay ? maxRepay : params.repayAmount;

        // Calculate collateral to seize
        uint256 collateralToSeize = _calculateCollateralToSeize(
            params.collateralAsset,
            params.borrowAsset,
            repayAmount,
            collateralConfig.liquidationPenalty
        );

        if (collateralToSeize < params.minCollateralReceived) {
            revert InsufficientCollateral();
        }

        // Check position has enough collateral
        if (collateralToSeize > position.collateralBalances[params.collateralAsset]) {
            collateralToSeize = position.collateralBalances[params.collateralAsset];
            // Recalculate repay amount
            repayAmount = _calculateDebtForCollateral(
                params.collateralAsset,
                params.borrowAsset,
                collateralToSeize,
                collateralConfig.liquidationPenalty
            );
        }

        // Update position
        position.borrowBalances[params.borrowAsset] -= repayAmount;
        position.collateralBalances[params.collateralAsset] -= collateralToSeize;
        position.lastUpdatedAt = block.timestamp;
        
        if (position.borrowBalances[params.borrowAsset] == 0) {
            _removeAssetFromArray(position.borrowAssets, params.borrowAsset);
        }
        if (position.collateralBalances[params.collateralAsset] == 0) {
            _removeAssetFromArray(position.collateralAssets, params.collateralAsset);
        }

        // Update market state
        borrowState.totalBorrows -= repayAmount;

        // Check for bad debt
        if (position.borrowAssets.length > 0 && position.collateralAssets.length == 0) {
            // All collateral gone but debt remains - bad debt
            for (uint256 i = 0; i < position.borrowAssets.length; i++) {
                address borrowAsset = position.borrowAssets[i];
                uint256 remainingDebt = position.borrowBalances[borrowAsset];
                if (remainingDebt > 0) {
                    badDebt[borrowAsset] += remainingDebt;
                    marketStates[borrowAsset].totalBorrows -= remainingDebt;
                    position.borrowBalances[borrowAsset] = 0;
                    emit BadDebtRealized(borrowAsset, remainingDebt);
                }
            }
            position.status = PositionStatus.Liquidated;
            delete position.borrowAssets;
        } else {
            position.status = PositionStatus.PartiallyLiquidated;
        }

        // Update health
        _updatePositionHealth(params.positionId);
        _updateInterestRates(params.borrowAsset);

        // Transfer repay amount from liquidator
        IERC20(params.borrowAsset).safeTransferFrom(msg.sender, address(this), repayAmount);

        // Transfer collateral to liquidator
        IERC20(params.collateralAsset).safeTransfer(msg.sender, collateralToSeize);

        emit PositionLiquidated(
            params.positionId,
            msg.sender,
            params.collateralAsset,
            params.borrowAsset,
            collateralToSeize,
            repayAmount,
            liquidationType
        );
    }

    // ============================================
    // FLASH LOANS
    // ============================================

    /**
     * @notice Executes a flash loan
     */
    function flashLoan(
        address receiver,
        address asset,
        uint256 amount,
        bytes calldata data
    ) external nonReentrant whenNotPaused marketActive(asset) {
        MarketState storage state = marketStates[asset];
        
        uint256 available = state.totalSupply - state.totalBorrows;
        if (amount > available) {
            revert InsufficientLiquidity(available, amount);
        }

        uint256 fee = (amount * flashLoanFee) / BPS;
        uint256 amountOwed = amount + fee;

        // Transfer to receiver
        IERC20(asset).safeTransfer(receiver, amount);

        // Execute callback
        (bool success,) = receiver.call(
            abi.encodeWithSignature(
                "onFlashLoan(address,address,uint256,uint256,bytes)",
                msg.sender,
                asset,
                amount,
                fee,
                data
            )
        );
        if (!success) revert FlashLoanFailed();

        // Verify repayment
        IERC20(asset).safeTransferFrom(receiver, address(this), amountOwed);

        // Accrue fee
        state.accumulatedFees += fee;

        emit FlashLoan(msg.sender, asset, amount, fee);
    }

    // ============================================
    // INTERNAL FUNCTIONS
    // ============================================

    /**
     * @notice Accrues interest for a market
     */
    function _accrueInterest(address asset) internal {
        MarketState storage state = marketStates[asset];
        
        if (block.timestamp == state.lastAccrualTime) return;

        uint256 timeElapsed = block.timestamp - state.lastAccrualTime;
        
        if (state.totalBorrows > 0) {
            // Calculate interest
            uint256 borrowInterest = (state.borrowRate * timeElapsed) / SECONDS_PER_YEAR;
            uint256 interestAccumulated = (state.totalBorrows * borrowInterest) / RATE_PRECISION;

            // Update indices
            state.borrowIndex = state.borrowIndex + (state.borrowIndex * borrowInterest) / RATE_PRECISION;
            
            uint256 utilizationRate = state.totalSupply > 0 
                ? (state.totalBorrows * RATE_PRECISION) / state.totalSupply 
                : 0;
            uint256 supplyInterest = (borrowInterest * utilizationRate) / RATE_PRECISION;
            state.supplyIndex = state.supplyIndex + (state.supplyIndex * supplyInterest) / RATE_PRECISION;

            // Calculate protocol fees
            uint256 protocolFees = (interestAccumulated * marketConfigs[asset].reserveFactor) / BPS;
            state.accumulatedFees += protocolFees;

            emit InterestAccrued(asset, supplyInterest, borrowInterest, protocolFees);
        }

        state.lastAccrualTime = block.timestamp;
    }

    /**
     * @notice Accrues interest for all position borrows
     */
    function _accrueAllPositionInterest(uint256 positionId) internal {
        Position storage position = positions[positionId];
        
        for (uint256 i = 0; i < position.borrowAssets.length; i++) {
            address asset = position.borrowAssets[i];
            _accrueInterest(asset);
            
            uint256 accrued = _calculateAccruedInterest(
                position.borrowBalances[asset],
                position.borrowIndices[asset],
                marketStates[asset].borrowIndex
            );
            position.borrowBalances[asset] += accrued;
            position.borrowIndices[asset] = marketStates[asset].borrowIndex;
        }
    }

    /**
     * @notice Updates interest rates based on utilization
     */
    function _updateInterestRates(address asset) internal {
        MarketState storage state = marketStates[asset];
        InterestRateModel storage model = interestRateModels[asset];

        if (state.totalSupply == 0) {
            state.utilizationRate = 0;
            state.borrowRate = model.baseRate;
            state.supplyRate = 0;
            return;
        }

        state.utilizationRate = (state.totalBorrows * RATE_PRECISION) / state.totalSupply;

        uint256 borrowRate;
        if (state.utilizationRate <= model.kink) {
            borrowRate = model.baseRate + (state.utilizationRate * model.multiplier) / RATE_PRECISION;
        } else {
            uint256 normalRate = model.baseRate + (model.kink * model.multiplier) / RATE_PRECISION;
            uint256 excessUtil = state.utilizationRate - model.kink;
            borrowRate = normalRate + (excessUtil * model.jumpMultiplier) / RATE_PRECISION;
        }

        if (borrowRate > model.maxRate) {
            borrowRate = model.maxRate;
        }

        state.borrowRate = borrowRate;
        
        // Supply rate = borrow rate * utilization * (1 - reserve factor)
        uint256 reserveFactor = marketConfigs[asset].reserveFactor;
        state.supplyRate = (borrowRate * state.utilizationRate * (BPS - reserveFactor)) / (RATE_PRECISION * BPS);
    }

    /**
     * @notice Calculates position health factor
     */
    function _calculatePositionHealth(uint256 positionId) internal view returns (uint256) {
        Position storage position = positions[positionId];

        uint256 totalCollateralValue;
        uint256 weightedLiquidationThreshold;
        
        // Calculate collateral value
        for (uint256 i = 0; i < position.collateralAssets.length; i++) {
            address asset = position.collateralAssets[i];
            uint256 balance = position.collateralBalances[asset];
            uint256 price = _getAssetPrice(asset);
            uint256 value = (balance * price) / (10 ** marketConfigs[asset].decimals);
            
            totalCollateralValue += value;
            weightedLiquidationThreshold += (value * marketConfigs[asset].liquidationThreshold);
        }

        if (totalCollateralValue == 0) {
            return position.borrowAssets.length == 0 ? type(uint256).max : 0;
        }

        uint256 avgLiquidationThreshold = weightedLiquidationThreshold / totalCollateralValue;

        // Calculate borrow value
        uint256 totalBorrowValue;
        for (uint256 i = 0; i < position.borrowAssets.length; i++) {
            address asset = position.borrowAssets[i];
            uint256 balance = position.borrowBalances[asset];
            uint256 price = _getAssetPrice(asset);
            uint256 value = (balance * price) / (10 ** marketConfigs[asset].decimals);
            totalBorrowValue += value;
        }

        if (totalBorrowValue == 0) {
            return type(uint256).max;
        }

        // Health = (collateral * liquidation threshold) / debt
        return (totalCollateralValue * avgLiquidationThreshold * PRECISION) / (totalBorrowValue * BPS);
    }

    /**
     * @notice Updates position health factor
     */
    function _updatePositionHealth(uint256 positionId) internal {
        positions[positionId].healthFactor = _calculatePositionHealth(positionId);
    }

    /**
     * @notice Calculates collateral to seize for liquidation
     */
    function _calculateCollateralToSeize(
        address collateralAsset,
        address borrowAsset,
        uint256 repayAmount,
        uint256 liquidationPenalty
    ) internal view returns (uint256) {
        uint256 borrowPrice = _getAssetPrice(borrowAsset);
        uint256 collateralPrice = _getAssetPrice(collateralAsset);
        
        uint256 repayValue = (repayAmount * borrowPrice) / (10 ** marketConfigs[borrowAsset].decimals);
        uint256 seizeValue = (repayValue * (BPS + liquidationPenalty)) / BPS;
        
        return (seizeValue * (10 ** marketConfigs[collateralAsset].decimals)) / collateralPrice;
    }

    /**
     * @notice Calculates debt that can be covered by collateral
     */
    function _calculateDebtForCollateral(
        address collateralAsset,
        address borrowAsset,
        uint256 collateralAmount,
        uint256 liquidationPenalty
    ) internal view returns (uint256) {
        uint256 collateralPrice = _getAssetPrice(collateralAsset);
        uint256 borrowPrice = _getAssetPrice(borrowAsset);
        
        uint256 collateralValue = (collateralAmount * collateralPrice) / (10 ** marketConfigs[collateralAsset].decimals);
        uint256 debtValue = (collateralValue * BPS) / (BPS + liquidationPenalty);
        
        return (debtValue * (10 ** marketConfigs[borrowAsset].decimals)) / borrowPrice;
    }

    /**
     * @notice Gets asset price from oracle
     */
    function _getAssetPrice(address asset) internal view returns (uint256) {
        // In production, would call oracle
        // For now, return 1:1 price
        return PRECISION;
    }

    /**
     * @notice Converts amount to shares
     */
    function _toShares(uint256 amount, uint256 index) internal pure returns (uint256) {
        return (amount * RATE_PRECISION) / index;
    }

    /**
     * @notice Calculates accrued interest
     */
    function _calculateAccruedInterest(
        uint256 principal,
        uint256 userIndex,
        uint256 currentIndex
    ) internal pure returns (uint256) {
        if (userIndex == 0 || userIndex >= currentIndex) return 0;
        return (principal * (currentIndex - userIndex)) / userIndex;
    }

    /**
     * @notice Removes asset from array
     */
    function _removeAssetFromArray(address[] storage arr, address asset) internal {
        for (uint256 i = 0; i < arr.length; i++) {
            if (arr[i] == asset) {
                arr[i] = arr[arr.length - 1];
                arr.pop();
                break;
            }
        }
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Gets position data snapshot
     */
    function getPosition(uint256 positionId) external view returns (PositionSnapshot memory snapshot) {
        Position storage position = positions[positionId];
        
        snapshot.positionId = positionId;
        snapshot.owner = position.owner;
        snapshot.mode = position.mode;
        snapshot.status = position.status;
        snapshot.healthFactor = position.healthFactor;
        snapshot.collateralAssets = position.collateralAssets;
        snapshot.borrowAssets = position.borrowAssets;
        
        snapshot.collateralBalances = new uint256[](position.collateralAssets.length);
        for (uint256 i = 0; i < position.collateralAssets.length; i++) {
            snapshot.collateralBalances[i] = position.collateralBalances[position.collateralAssets[i]];
        }
        
        snapshot.borrowBalances = new uint256[](position.borrowAssets.length);
        for (uint256 i = 0; i < position.borrowAssets.length; i++) {
            snapshot.borrowBalances[i] = position.borrowBalances[position.borrowAssets[i]];
        }
    }

    /**
     * @notice Gets user's position IDs
     */
    function getUserPositions(address user) external view returns (uint256[] memory) {
        return userPositions[user];
    }

    /**
     * @notice Gets all active markets
     */
    function getActiveMarkets() external view returns (address[] memory) {
        return activeMarkets;
    }

    /**
     * @notice Gets current utilization rate
     */
    function getUtilizationRate(address asset) external view returns (uint256) {
        return marketStates[asset].utilizationRate;
    }

    // ============================================
    // ADMIN FUNCTIONS
    // ============================================

    /**
     * @notice Withdraws protocol fees
     */
    function withdrawFees(address asset, uint256 amount, address to) external onlyRole(MARKET_ADMIN_ROLE) {
        MarketState storage state = marketStates[asset];
        require(amount <= state.accumulatedFees, "Exceeds fees");
        
        state.accumulatedFees -= amount;
        IERC20(asset).safeTransfer(to, amount);
    }

    /**
     * @notice Updates treasury address
     */
    function setTreasury(address _treasury) external onlyRole(DEFAULT_ADMIN_ROLE) {
        treasury = _treasury;
    }

    /**
     * @notice Pauses the market
     */
    function pause() external onlyRole(RISK_MANAGER_ROLE) {
        _pause();
    }

    /**
     * @notice Unpauses the market
     */
    function unpause() external onlyRole(RISK_MANAGER_ROLE) {
        _unpause();
    }
}
