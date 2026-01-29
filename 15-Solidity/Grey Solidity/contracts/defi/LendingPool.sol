// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts-upgradeable/access/AccessControlUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/PausableUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/ReentrancyGuardUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/Initializable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/UUPSUpgradeable.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";

/**
 * @title LendingPool
 * @author Grey Protocol Team
 * @notice Decentralized lending and borrowing protocol
 * @dev Implements collateralized lending with variable interest rates
 * 
 * Features:
 * - Multi-asset lending and borrowing
 * - Dynamic interest rate model based on utilization
 * - Collateralization and liquidation system
 * - Interest accrual with per-second precision
 * - Reserve factor for protocol fees
 * - Flash loans
 * - Health factor monitoring
 */
contract LendingPool is 
    Initializable,
    AccessControlUpgradeable,
    PausableUpgradeable,
    ReentrancyGuardUpgradeable,
    UUPSUpgradeable
{
    using SafeERC20 for IERC20;

    // ============================================
    // CONSTANTS & ROLES
    // ============================================

    bytes32 public constant ASSET_MANAGER_ROLE = keccak256("ASSET_MANAGER_ROLE");
    bytes32 public constant LIQUIDATOR_ROLE = keccak256("LIQUIDATOR_ROLE");
    bytes32 public constant ORACLE_ROLE = keccak256("ORACLE_ROLE");
    bytes32 public constant PAUSER_ROLE = keccak256("PAUSER_ROLE");
    bytes32 public constant UPGRADER_ROLE = keccak256("UPGRADER_ROLE");

    /// @notice Basis points denominator
    uint256 public constant BPS = 10000;
    
    /// @notice Seconds per year for interest calculations
    uint256 public constant SECONDS_PER_YEAR = 365 days;
    
    /// @notice Maximum utilization rate (95%)
    uint256 public constant MAX_UTILIZATION_RATE = 9500;
    
    /// @notice Minimum health factor before liquidation
    uint256 public constant MIN_HEALTH_FACTOR = 1e18; // 1.0
    
    /// @notice Liquidation bonus (5%)
    uint256 public constant LIQUIDATION_BONUS = 500;
    
    /// @notice Close factor for liquidation (50%)
    uint256 public constant CLOSE_FACTOR = 5000;

    // ============================================
    // STRUCTS
    // ============================================

    /**
     * @notice Asset configuration
     * @param isActive Whether asset is enabled
     * @param canBorrow Whether borrowing is enabled
     * @param canCollateral Whether can be used as collateral
     * @param decimals Token decimals
     * @param collateralFactor Collateral factor in BPS
     * @param liquidationThreshold Liquidation threshold in BPS
     * @param reserveFactor Reserve factor in BPS
     * @param baseRate Base interest rate per year in WAD
     * @param slopeRate1 Interest rate slope below optimal utilization
     * @param slopeRate2 Interest rate slope above optimal utilization
     * @param optimalUtilization Optimal utilization rate in BPS
     */
    struct AssetConfig {
        bool isActive;
        bool canBorrow;
        bool canCollateral;
        uint8 decimals;
        uint16 collateralFactor;
        uint16 liquidationThreshold;
        uint16 reserveFactor;
        uint256 baseRate;
        uint256 slopeRate1;
        uint256 slopeRate2;
        uint256 optimalUtilization;
    }

    /**
     * @notice Asset state
     * @param totalDeposits Total deposits
     * @param totalBorrows Total borrows
     * @param totalReserves Protocol reserves
     * @param borrowIndex Accumulated borrow interest index
     * @param supplyIndex Accumulated supply interest index
     * @param lastUpdateTimestamp Last interest accrual
     * @param currentBorrowRate Current borrow rate per second
     * @param currentSupplyRate Current supply rate per second
     */
    struct AssetState {
        uint256 totalDeposits;
        uint256 totalBorrows;
        uint256 totalReserves;
        uint256 borrowIndex;
        uint256 supplyIndex;
        uint256 lastUpdateTimestamp;
        uint256 currentBorrowRate;
        uint256 currentSupplyRate;
    }

    /**
     * @notice User position for an asset
     * @param depositBalance Deposit balance (scaled by supply index)
     * @param borrowBalance Borrow balance (scaled by borrow index)
     * @param supplyIndex User's supply index at last interaction
     * @param borrowIndex User's borrow index at last interaction
     * @param isCollateral Whether deposits are used as collateral
     * @param lastUpdateTimestamp Last interaction timestamp
     */
    struct UserPosition {
        uint256 depositBalance;
        uint256 borrowBalance;
        uint256 supplyIndex;
        uint256 borrowIndex;
        bool isCollateral;
        uint256 lastUpdateTimestamp;
    }

    /**
     * @notice Account summary
     * @param totalCollateralValue Total collateral value in base currency
     * @param totalBorrowValue Total borrow value in base currency
     * @param availableBorrowValue Available borrow capacity
     * @param healthFactor Account health factor
     * @param ltv Loan-to-value ratio
     */
    struct AccountSummary {
        uint256 totalCollateralValue;
        uint256 totalBorrowValue;
        uint256 availableBorrowValue;
        uint256 healthFactor;
        uint256 ltv;
    }

    /**
     * @notice Flash loan callback data
     */
    struct FlashLoanData {
        address[] assets;
        uint256[] amounts;
        uint256[] premiums;
        address initiator;
        bytes params;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice List of supported assets
    address[] public assetList;
    
    /// @notice Asset address to configuration
    mapping(address => AssetConfig) public assetConfigs;
    
    /// @notice Asset address to state
    mapping(address => AssetState) public assetStates;
    
    /// @notice User => Asset => Position
    mapping(address => mapping(address => UserPosition)) public userPositions;
    
    /// @notice Mapping of asset to price (in 8 decimals)
    mapping(address => uint256) public assetPrices;
    
    /// @notice Price oracle address
    address public priceOracle;
    
    /// @notice Flash loan fee in BPS
    uint256 public flashLoanFee;
    
    /// @notice Treasury address for reserves
    address public treasury;
    
    /// @notice Total value locked
    uint256 public totalValueLocked;

    /// @notice Storage gap
    uint256[45] private __gap;

    // ============================================
    // EVENTS
    // ============================================

    event AssetAdded(address indexed asset, AssetConfig config);
    event AssetConfigUpdated(address indexed asset, AssetConfig config);
    event Deposit(address indexed user, address indexed asset, uint256 amount, uint256 shares);
    event Withdraw(address indexed user, address indexed asset, uint256 amount, uint256 shares);
    event Borrow(address indexed user, address indexed asset, uint256 amount);
    event Repay(address indexed user, address indexed asset, uint256 amount);
    event Liquidation(
        address indexed liquidator,
        address indexed borrower,
        address indexed debtAsset,
        address collateralAsset,
        uint256 debtRepaid,
        uint256 collateralSeized
    );
    event FlashLoan(
        address indexed initiator,
        address indexed receiver,
        address indexed asset,
        uint256 amount,
        uint256 premium
    );
    event CollateralEnabled(address indexed user, address indexed asset);
    event CollateralDisabled(address indexed user, address indexed asset);
    event InterestAccrued(address indexed asset, uint256 borrowIndex, uint256 supplyIndex);
    event PriceUpdated(address indexed asset, uint256 price);
    event ReservesWithdrawn(address indexed asset, uint256 amount);

    // ============================================
    // ERRORS
    // ============================================

    error AssetNotSupported(address asset);
    error AssetAlreadySupported(address asset);
    error AssetNotActive(address asset);
    error BorrowingNotEnabled(address asset);
    error CollateralNotEnabled(address asset);
    error InsufficientCollateral();
    error InsufficientLiquidity();
    error InsufficientBalance();
    error HealthFactorTooLow();
    error PositionNotLiquidatable();
    error ExceedsCloseFactor();
    error FlashLoanNotRepaid();
    error InvalidAmount();
    error InvalidAddress();
    error InvalidConfig();
    error TransferFailed();

    // ============================================
    // MODIFIERS
    // ============================================

    modifier assetSupported(address asset) {
        if (!assetConfigs[asset].isActive) revert AssetNotSupported(asset);
        _;
    }

    // ============================================
    // INITIALIZER
    // ============================================

    /**
     * @notice Initializes the lending pool
     * @param admin Admin address
     * @param _treasury Treasury address
     * @param _flashLoanFee Flash loan fee in BPS
     */
    function initialize(
        address admin,
        address _treasury,
        uint256 _flashLoanFee
    ) public initializer {
        if (admin == address(0)) revert InvalidAddress();
        if (_treasury == address(0)) revert InvalidAddress();

        __AccessControl_init();
        __Pausable_init();
        __ReentrancyGuard_init();
        __UUPSUpgradeable_init();

        treasury = _treasury;
        flashLoanFee = _flashLoanFee;

        _grantRole(DEFAULT_ADMIN_ROLE, admin);
        _grantRole(ASSET_MANAGER_ROLE, admin);
        _grantRole(LIQUIDATOR_ROLE, admin);
        _grantRole(ORACLE_ROLE, admin);
        _grantRole(PAUSER_ROLE, admin);
        _grantRole(UPGRADER_ROLE, admin);
    }

    // ============================================
    // ASSET MANAGEMENT
    // ============================================

    /**
     * @notice Adds a new asset to the pool
     * @param asset Asset address
     * @param config Asset configuration
     */
    function addAsset(
        address asset,
        AssetConfig calldata config
    ) external onlyRole(ASSET_MANAGER_ROLE) {
        if (asset == address(0)) revert InvalidAddress();
        if (assetConfigs[asset].isActive) revert AssetAlreadySupported(asset);
        
        _validateConfig(config);

        assetConfigs[asset] = config;
        assetStates[asset] = AssetState({
            totalDeposits: 0,
            totalBorrows: 0,
            totalReserves: 0,
            borrowIndex: 1e18,
            supplyIndex: 1e18,
            lastUpdateTimestamp: block.timestamp,
            currentBorrowRate: config.baseRate / SECONDS_PER_YEAR,
            currentSupplyRate: 0
        });

        assetList.push(asset);

        emit AssetAdded(asset, config);
    }

    /**
     * @notice Updates asset configuration
     * @param asset Asset address
     * @param config New configuration
     */
    function updateAssetConfig(
        address asset,
        AssetConfig calldata config
    ) external onlyRole(ASSET_MANAGER_ROLE) assetSupported(asset) {
        _validateConfig(config);
        
        // Accrue interest before updating
        _accrueInterest(asset);
        
        assetConfigs[asset] = config;
        
        emit AssetConfigUpdated(asset, config);
    }

    /**
     * @notice Validates asset configuration
     */
    function _validateConfig(AssetConfig calldata config) internal pure {
        if (config.collateralFactor > BPS) revert InvalidConfig();
        if (config.liquidationThreshold > BPS) revert InvalidConfig();
        if (config.liquidationThreshold < config.collateralFactor) revert InvalidConfig();
        if (config.reserveFactor > BPS) revert InvalidConfig();
        if (config.optimalUtilization > MAX_UTILIZATION_RATE) revert InvalidConfig();
    }

    // ============================================
    // DEPOSIT & WITHDRAW
    // ============================================

    /**
     * @notice Deposits assets into the pool
     * @param asset Asset to deposit
     * @param amount Amount to deposit
     */
    function deposit(
        address asset,
        uint256 amount
    ) external nonReentrant whenNotPaused assetSupported(asset) {
        if (amount == 0) revert InvalidAmount();

        // Accrue interest
        _accrueInterest(asset);

        // Transfer tokens
        IERC20(asset).safeTransferFrom(msg.sender, address(this), amount);

        // Calculate shares
        uint256 shares = _depositToShares(asset, amount);

        // Update user position
        UserPosition storage position = userPositions[msg.sender][asset];
        position.depositBalance += shares;
        position.supplyIndex = assetStates[asset].supplyIndex;
        position.lastUpdateTimestamp = block.timestamp;

        // Update asset state
        assetStates[asset].totalDeposits += amount;
        totalValueLocked += _getAssetValue(asset, amount);

        emit Deposit(msg.sender, asset, amount, shares);
    }

    /**
     * @notice Withdraws assets from the pool
     * @param asset Asset to withdraw
     * @param amount Amount to withdraw (use type(uint256).max for all)
     */
    function withdraw(
        address asset,
        uint256 amount
    ) external nonReentrant assetSupported(asset) {
        // Accrue interest
        _accrueInterest(asset);

        UserPosition storage position = userPositions[msg.sender][asset];
        
        // Calculate actual balance
        uint256 userBalance = _sharesToDeposit(asset, position.depositBalance);
        if (amount == type(uint256).max) {
            amount = userBalance;
        }
        if (amount > userBalance) revert InsufficientBalance();

        // Check liquidity
        uint256 available = assetStates[asset].totalDeposits - assetStates[asset].totalBorrows;
        if (amount > available) revert InsufficientLiquidity();

        // Check health factor if used as collateral
        if (position.isCollateral) {
            uint256 newHealthFactor = _calculateHealthFactorAfterWithdraw(msg.sender, asset, amount);
            if (newHealthFactor < MIN_HEALTH_FACTOR) revert HealthFactorTooLow();
        }

        // Calculate shares to burn
        uint256 shares = _depositToShares(asset, amount);

        // Update position
        position.depositBalance -= shares;
        position.lastUpdateTimestamp = block.timestamp;

        // Update asset state
        assetStates[asset].totalDeposits -= amount;
        totalValueLocked -= _getAssetValue(asset, amount);

        // Transfer tokens
        IERC20(asset).safeTransfer(msg.sender, amount);

        emit Withdraw(msg.sender, asset, amount, shares);
    }

    // ============================================
    // BORROW & REPAY
    // ============================================

    /**
     * @notice Borrows assets from the pool
     * @param asset Asset to borrow
     * @param amount Amount to borrow
     */
    function borrow(
        address asset,
        uint256 amount
    ) external nonReentrant whenNotPaused assetSupported(asset) {
        if (amount == 0) revert InvalidAmount();
        
        AssetConfig storage config = assetConfigs[asset];
        if (!config.canBorrow) revert BorrowingNotEnabled(asset);

        // Accrue interest
        _accrueInterest(asset);

        // Check liquidity
        uint256 available = assetStates[asset].totalDeposits - assetStates[asset].totalBorrows;
        if (amount > available) revert InsufficientLiquidity();

        // Check collateral
        AccountSummary memory summary = getAccountSummary(msg.sender);
        uint256 borrowValue = _getAssetValue(asset, amount);
        if (borrowValue > summary.availableBorrowValue) revert InsufficientCollateral();

        // Update user position
        UserPosition storage position = userPositions[msg.sender][asset];
        uint256 borrowShares = _borrowToShares(asset, amount);
        position.borrowBalance += borrowShares;
        position.borrowIndex = assetStates[asset].borrowIndex;
        position.lastUpdateTimestamp = block.timestamp;

        // Update asset state
        assetStates[asset].totalBorrows += amount;

        // Update interest rates
        _updateInterestRates(asset);

        // Transfer tokens
        IERC20(asset).safeTransfer(msg.sender, amount);

        emit Borrow(msg.sender, asset, amount);
    }

    /**
     * @notice Repays borrowed assets
     * @param asset Asset to repay
     * @param amount Amount to repay (use type(uint256).max for all)
     */
    function repay(
        address asset,
        uint256 amount
    ) external nonReentrant assetSupported(asset) {
        // Accrue interest
        _accrueInterest(asset);

        UserPosition storage position = userPositions[msg.sender][asset];
        
        // Calculate actual debt
        uint256 userDebt = _sharesToBorrow(asset, position.borrowBalance);
        if (amount == type(uint256).max) {
            amount = userDebt;
        }
        if (amount > userDebt) {
            amount = userDebt;
        }

        // Transfer tokens
        IERC20(asset).safeTransferFrom(msg.sender, address(this), amount);

        // Calculate shares to burn
        uint256 shares = _borrowToShares(asset, amount);

        // Update position
        position.borrowBalance -= shares;
        position.lastUpdateTimestamp = block.timestamp;

        // Update asset state
        assetStates[asset].totalBorrows -= amount;

        // Update interest rates
        _updateInterestRates(asset);

        emit Repay(msg.sender, asset, amount);
    }

    // ============================================
    // COLLATERAL MANAGEMENT
    // ============================================

    /**
     * @notice Enables an asset as collateral
     * @param asset Asset to enable
     */
    function enableCollateral(address asset) external assetSupported(asset) {
        if (!assetConfigs[asset].canCollateral) revert CollateralNotEnabled(asset);
        
        userPositions[msg.sender][asset].isCollateral = true;
        
        emit CollateralEnabled(msg.sender, asset);
    }

    /**
     * @notice Disables an asset as collateral
     * @param asset Asset to disable
     */
    function disableCollateral(address asset) external {
        UserPosition storage position = userPositions[msg.sender][asset];
        
        // Check if can safely disable
        if (position.borrowBalance > 0) {
            uint256 newHealthFactor = _calculateHealthFactorWithoutCollateral(msg.sender, asset);
            if (newHealthFactor < MIN_HEALTH_FACTOR) revert HealthFactorTooLow();
        }
        
        position.isCollateral = false;
        
        emit CollateralDisabled(msg.sender, asset);
    }

    // ============================================
    // LIQUIDATION
    // ============================================

    /**
     * @notice Liquidates an undercollateralized position
     * @param borrower Address of the borrower
     * @param debtAsset Asset that was borrowed
     * @param collateralAsset Asset used as collateral
     * @param debtAmount Amount of debt to repay
     */
    function liquidate(
        address borrower,
        address debtAsset,
        address collateralAsset,
        uint256 debtAmount
    ) external nonReentrant assetSupported(debtAsset) assetSupported(collateralAsset) {
        // Accrue interest on both assets
        _accrueInterest(debtAsset);
        _accrueInterest(collateralAsset);

        // Check if position is liquidatable
        AccountSummary memory summary = getAccountSummary(borrower);
        if (summary.healthFactor >= MIN_HEALTH_FACTOR) revert PositionNotLiquidatable();

        // Get user's debt
        UserPosition storage debtPosition = userPositions[borrower][debtAsset];
        uint256 userDebt = _sharesToBorrow(debtAsset, debtPosition.borrowBalance);

        // Check close factor
        uint256 maxRepay = (userDebt * CLOSE_FACTOR) / BPS;
        if (debtAmount > maxRepay) revert ExceedsCloseFactor();

        // Calculate collateral to seize
        uint256 debtValue = _getAssetValue(debtAsset, debtAmount);
        uint256 collateralValue = (debtValue * (BPS + LIQUIDATION_BONUS)) / BPS;
        uint256 collateralAmount = _valueToAsset(collateralAsset, collateralValue);

        // Check borrower has enough collateral
        UserPosition storage collateralPosition = userPositions[borrower][collateralAsset];
        uint256 borrowerCollateral = _sharesToDeposit(collateralAsset, collateralPosition.depositBalance);
        if (collateralAmount > borrowerCollateral) {
            collateralAmount = borrowerCollateral;
        }

        // Transfer debt from liquidator
        IERC20(debtAsset).safeTransferFrom(msg.sender, address(this), debtAmount);

        // Update debt position
        uint256 debtShares = _borrowToShares(debtAsset, debtAmount);
        debtPosition.borrowBalance -= debtShares;

        // Update collateral position
        uint256 collateralShares = _depositToShares(collateralAsset, collateralAmount);
        collateralPosition.depositBalance -= collateralShares;

        // Update asset states
        assetStates[debtAsset].totalBorrows -= debtAmount;
        assetStates[collateralAsset].totalDeposits -= collateralAmount;

        // Transfer collateral to liquidator
        IERC20(collateralAsset).safeTransfer(msg.sender, collateralAmount);

        emit Liquidation(msg.sender, borrower, debtAsset, collateralAsset, debtAmount, collateralAmount);
    }

    // ============================================
    // FLASH LOANS
    // ============================================

    /**
     * @notice Executes a flash loan
     * @param receiver Contract that receives the flash loan
     * @param assets Array of assets to borrow
     * @param amounts Array of amounts to borrow
     * @param params Arbitrary data to pass to receiver
     */
    function flashLoan(
        address receiver,
        address[] calldata assets,
        uint256[] calldata amounts,
        bytes calldata params
    ) external nonReentrant whenNotPaused {
        require(assets.length == amounts.length, "Array length mismatch");

        uint256[] memory premiums = new uint256[](assets.length);
        uint256[] memory balancesBefore = new uint256[](assets.length);

        // Calculate premiums and transfer assets
        for (uint256 i = 0; i < assets.length; i++) {
            if (!assetConfigs[assets[i]].isActive) revert AssetNotSupported(assets[i]);
            
            premiums[i] = (amounts[i] * flashLoanFee) / BPS;
            balancesBefore[i] = IERC20(assets[i]).balanceOf(address(this));

            IERC20(assets[i]).safeTransfer(receiver, amounts[i]);
        }

        // Call receiver
        FlashLoanData memory flashData = FlashLoanData({
            assets: assets,
            amounts: amounts,
            premiums: premiums,
            initiator: msg.sender,
            params: params
        });

        (bool success,) = receiver.call(
            abi.encodeWithSignature(
                "executeOperation(address[],uint256[],uint256[],address,bytes)",
                assets,
                amounts,
                premiums,
                msg.sender,
                params
            )
        );
        require(success, "Flash loan callback failed");

        // Verify repayment
        for (uint256 i = 0; i < assets.length; i++) {
            uint256 currentBalance = IERC20(assets[i]).balanceOf(address(this));
            uint256 amountOwed = amounts[i] + premiums[i];
            
            if (currentBalance < balancesBefore[i] + premiums[i]) {
                revert FlashLoanNotRepaid();
            }

            // Add premium to reserves
            assetStates[assets[i]].totalReserves += premiums[i];

            emit FlashLoan(msg.sender, receiver, assets[i], amounts[i], premiums[i]);
        }
    }

    // ============================================
    // INTEREST ACCRUAL
    // ============================================

    /**
     * @notice Accrues interest for an asset
     * @param asset Asset to accrue
     */
    function _accrueInterest(address asset) internal {
        AssetState storage state = assetStates[asset];
        
        uint256 timeDelta = block.timestamp - state.lastUpdateTimestamp;
        if (timeDelta == 0) return;

        if (state.totalBorrows > 0) {
            // Calculate borrow interest
            uint256 borrowInterest = (state.totalBorrows * state.currentBorrowRate * timeDelta) / 1e18;
            
            // Update borrow index
            uint256 borrowIndexDelta = (borrowInterest * 1e18) / state.totalBorrows;
            state.borrowIndex += borrowIndexDelta;
            
            // Add to borrows
            state.totalBorrows += borrowInterest;

            // Calculate reserve
            uint256 reserveAmount = (borrowInterest * assetConfigs[asset].reserveFactor) / BPS;
            state.totalReserves += reserveAmount;

            // Calculate supply interest (excluding reserves)
            uint256 supplyInterest = borrowInterest - reserveAmount;
            if (state.totalDeposits > 0) {
                uint256 supplyIndexDelta = (supplyInterest * 1e18) / state.totalDeposits;
                state.supplyIndex += supplyIndexDelta;
                state.totalDeposits += supplyInterest;
            }
        }

        state.lastUpdateTimestamp = block.timestamp;
        _updateInterestRates(asset);

        emit InterestAccrued(asset, state.borrowIndex, state.supplyIndex);
    }

    /**
     * @notice Updates interest rates based on utilization
     * @param asset Asset to update
     */
    function _updateInterestRates(address asset) internal {
        AssetConfig storage config = assetConfigs[asset];
        AssetState storage state = assetStates[asset];

        uint256 utilization = 0;
        if (state.totalDeposits > 0) {
            utilization = (state.totalBorrows * BPS) / state.totalDeposits;
        }

        uint256 borrowRate;
        if (utilization <= config.optimalUtilization) {
            // Below optimal: base + slope1 * utilization
            borrowRate = config.baseRate + 
                (config.slopeRate1 * utilization) / config.optimalUtilization;
        } else {
            // Above optimal: base + slope1 + slope2 * (utilization - optimal)
            uint256 excessUtilization = utilization - config.optimalUtilization;
            uint256 maxExcess = BPS - config.optimalUtilization;
            borrowRate = config.baseRate + config.slopeRate1 +
                (config.slopeRate2 * excessUtilization) / maxExcess;
        }

        state.currentBorrowRate = borrowRate / SECONDS_PER_YEAR;
        
        // Supply rate = borrow rate * utilization * (1 - reserve factor)
        state.currentSupplyRate = (state.currentBorrowRate * utilization * (BPS - config.reserveFactor)) / (BPS * BPS);
    }

    // ============================================
    // HELPER FUNCTIONS
    // ============================================

    function _depositToShares(address asset, uint256 amount) internal view returns (uint256) {
        uint256 supplyIndex = assetStates[asset].supplyIndex;
        return (amount * 1e18) / supplyIndex;
    }

    function _sharesToDeposit(address asset, uint256 shares) internal view returns (uint256) {
        uint256 supplyIndex = assetStates[asset].supplyIndex;
        return (shares * supplyIndex) / 1e18;
    }

    function _borrowToShares(address asset, uint256 amount) internal view returns (uint256) {
        uint256 borrowIndex = assetStates[asset].borrowIndex;
        return (amount * 1e18) / borrowIndex;
    }

    function _sharesToBorrow(address asset, uint256 shares) internal view returns (uint256) {
        uint256 borrowIndex = assetStates[asset].borrowIndex;
        return (shares * borrowIndex) / 1e18;
    }

    function _getAssetValue(address asset, uint256 amount) internal view returns (uint256) {
        return (amount * assetPrices[asset]) / (10 ** assetConfigs[asset].decimals);
    }

    function _valueToAsset(address asset, uint256 value) internal view returns (uint256) {
        return (value * (10 ** assetConfigs[asset].decimals)) / assetPrices[asset];
    }

    function _calculateHealthFactorAfterWithdraw(
        address user,
        address asset,
        uint256 amount
    ) internal view returns (uint256) {
        AccountSummary memory summary = getAccountSummary(user);
        uint256 withdrawValue = _getAssetValue(asset, amount);
        uint256 adjustedCollateral = summary.totalCollateralValue - 
            (withdrawValue * assetConfigs[asset].liquidationThreshold) / BPS;
        
        if (summary.totalBorrowValue == 0) return type(uint256).max;
        return (adjustedCollateral * 1e18) / summary.totalBorrowValue;
    }

    function _calculateHealthFactorWithoutCollateral(
        address user,
        address asset
    ) internal view returns (uint256) {
        AccountSummary memory summary = getAccountSummary(user);
        UserPosition storage position = userPositions[user][asset];
        uint256 depositValue = _getAssetValue(asset, _sharesToDeposit(asset, position.depositBalance));
        uint256 adjustedCollateral = summary.totalCollateralValue - 
            (depositValue * assetConfigs[asset].liquidationThreshold) / BPS;
        
        if (summary.totalBorrowValue == 0) return type(uint256).max;
        return (adjustedCollateral * 1e18) / summary.totalBorrowValue;
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Gets account summary
     */
    function getAccountSummary(address user) public view returns (AccountSummary memory summary) {
        for (uint256 i = 0; i < assetList.length; i++) {
            address asset = assetList[i];
            UserPosition storage position = userPositions[user][asset];
            AssetConfig storage config = assetConfigs[asset];

            // Add collateral value
            if (position.isCollateral && position.depositBalance > 0) {
                uint256 depositAmount = _sharesToDeposit(asset, position.depositBalance);
                uint256 assetValue = _getAssetValue(asset, depositAmount);
                summary.totalCollateralValue += (assetValue * config.liquidationThreshold) / BPS;
                summary.availableBorrowValue += (assetValue * config.collateralFactor) / BPS;
            }

            // Add borrow value
            if (position.borrowBalance > 0) {
                uint256 borrowAmount = _sharesToBorrow(asset, position.borrowBalance);
                summary.totalBorrowValue += _getAssetValue(asset, borrowAmount);
            }
        }

        // Subtract current borrows from available
        if (summary.totalBorrowValue < summary.availableBorrowValue) {
            summary.availableBorrowValue -= summary.totalBorrowValue;
        } else {
            summary.availableBorrowValue = 0;
        }

        // Calculate health factor
        if (summary.totalBorrowValue == 0) {
            summary.healthFactor = type(uint256).max;
        } else {
            summary.healthFactor = (summary.totalCollateralValue * 1e18) / summary.totalBorrowValue;
        }

        // Calculate LTV
        if (summary.totalCollateralValue > 0) {
            summary.ltv = (summary.totalBorrowValue * BPS) / summary.totalCollateralValue;
        }
    }

    /**
     * @notice Gets user position for an asset
     */
    function getUserPosition(
        address user,
        address asset
    ) external view returns (uint256 depositBalance, uint256 borrowBalance, bool isCollateral) {
        UserPosition storage position = userPositions[user][asset];
        depositBalance = _sharesToDeposit(asset, position.depositBalance);
        borrowBalance = _sharesToBorrow(asset, position.borrowBalance);
        isCollateral = position.isCollateral;
    }

    /**
     * @notice Gets current interest rates
     */
    function getInterestRates(address asset) external view returns (uint256 borrowRate, uint256 supplyRate) {
        AssetState storage state = assetStates[asset];
        borrowRate = state.currentBorrowRate * SECONDS_PER_YEAR;
        supplyRate = state.currentSupplyRate * SECONDS_PER_YEAR;
    }

    /**
     * @notice Gets asset utilization rate
     */
    function getUtilizationRate(address asset) external view returns (uint256) {
        AssetState storage state = assetStates[asset];
        if (state.totalDeposits == 0) return 0;
        return (state.totalBorrows * BPS) / state.totalDeposits;
    }

    // ============================================
    // ORACLE
    // ============================================

    /**
     * @notice Updates asset price
     * @param asset Asset address
     * @param price Price in 8 decimals
     */
    function updatePrice(
        address asset,
        uint256 price
    ) external onlyRole(ORACLE_ROLE) {
        assetPrices[asset] = price;
        emit PriceUpdated(asset, price);
    }

    /**
     * @notice Batch update prices
     */
    function batchUpdatePrices(
        address[] calldata assets,
        uint256[] calldata prices
    ) external onlyRole(ORACLE_ROLE) {
        require(assets.length == prices.length, "Length mismatch");
        for (uint256 i = 0; i < assets.length; i++) {
            assetPrices[assets[i]] = prices[i];
            emit PriceUpdated(assets[i], prices[i]);
        }
    }

    // ============================================
    // ADMIN
    // ============================================

    /**
     * @notice Withdraws reserves to treasury
     */
    function withdrawReserves(
        address asset,
        uint256 amount
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        AssetState storage state = assetStates[asset];
        require(amount <= state.totalReserves, "Exceeds reserves");
        
        state.totalReserves -= amount;
        IERC20(asset).safeTransfer(treasury, amount);
        
        emit ReservesWithdrawn(asset, amount);
    }

    function setFlashLoanFee(uint256 newFee) external onlyRole(DEFAULT_ADMIN_ROLE) {
        require(newFee <= 1000, "Fee too high"); // Max 10%
        flashLoanFee = newFee;
    }

    function setTreasury(address newTreasury) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (newTreasury == address(0)) revert InvalidAddress();
        treasury = newTreasury;
    }

    function pause() external onlyRole(PAUSER_ROLE) { _pause(); }
    function unpause() external onlyRole(PAUSER_ROLE) { _unpause(); }
    function _authorizeUpgrade(address) internal override onlyRole(UPGRADER_ROLE) {}
}
