// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/ERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";

/**
 * @title SyntheticAssetFactory
 * @notice Creates and manages synthetic assets backed by collateral
 * @dev Implements collateralized debt positions (CDPs) for minting synthetic assets
 * 
 * Architecture:
 * - Users deposit collateral to mint synthetic assets
 * - Collateralization ratio enforced via oracle prices
 * - Liquidation mechanism for undercollateralized positions
 * - Dynamic debt ceiling per synthetic asset
 * - Interest accrual on minted synthetics
 */
contract SyntheticAssetFactory is AccessControl, ReentrancyGuard {
    using SafeERC20 for IERC20;

    // ============================================
    // ROLES
    // ============================================

    bytes32 public constant ORACLE_ROLE = keccak256("ORACLE_ROLE");
    bytes32 public constant RISK_MANAGER_ROLE = keccak256("RISK_MANAGER_ROLE");
    bytes32 public constant MINTER_ROLE = keccak256("MINTER_ROLE");

    // ============================================
    // TYPES
    // ============================================

    /// @notice Synthetic asset configuration
    struct SyntheticConfig {
        address syntheticToken;           // The synthetic token contract
        address collateralToken;          // Accepted collateral
        address priceOracle;              // Oracle for price feed
        uint256 minCollateralRatio;       // Minimum CR (e.g., 15000 = 150%)
        uint256 liquidationRatio;         // Liquidation threshold (e.g., 12000 = 120%)
        uint256 liquidationPenalty;       // Penalty in BPS (e.g., 1300 = 13%)
        uint256 stabilityFee;             // Annual fee in BPS
        uint256 debtCeiling;              // Maximum mintable amount
        uint256 totalDebt;                // Current total minted
        bool isActive;
    }

    /// @notice Collateralized Debt Position
    struct CDP {
        uint256 collateralAmount;
        uint256 debtAmount;
        uint256 lastInterestUpdate;
        uint256 accumulatedFees;
    }

    /// @notice Price data from oracle
    struct PriceData {
        uint256 price;
        uint256 timestamp;
        uint8 decimals;
    }

    // ============================================
    // STATE
    // ============================================

    /// @notice All synthetic asset configurations
    mapping(bytes32 => SyntheticConfig) public syntheticConfigs;

    /// @notice CDP positions: syntheticId => user => CDP
    mapping(bytes32 => mapping(address => CDP)) public cdps;

    /// @notice Synthetic ID list
    bytes32[] public syntheticIds;

    /// @notice Oracle prices: asset => PriceData
    mapping(address => PriceData) public prices;

    /// @notice Interest rate model parameters
    uint256 public baseInterestRate;
    uint256 public utilizationMultiplier;

    /// @notice Protocol treasury
    address public treasury;

    /// @notice Global emergency pause
    bool public paused;

    // ============================================
    // EVENTS
    // ============================================

    event SyntheticCreated(
        bytes32 indexed syntheticId,
        address indexed syntheticToken,
        address collateralToken,
        uint256 minCollateralRatio
    );

    event CDPOpened(
        bytes32 indexed syntheticId,
        address indexed owner,
        uint256 collateralAmount,
        uint256 debtAmount
    );

    event CDPModified(
        bytes32 indexed syntheticId,
        address indexed owner,
        int256 collateralDelta,
        int256 debtDelta
    );

    event CDPClosed(
        bytes32 indexed syntheticId,
        address indexed owner,
        uint256 collateralReturned
    );

    event CDPLiquidated(
        bytes32 indexed syntheticId,
        address indexed owner,
        address indexed liquidator,
        uint256 collateralSeized,
        uint256 debtRepaid
    );

    event PriceUpdated(address indexed asset, uint256 price, uint256 timestamp);

    // ============================================
    // ERRORS
    // ============================================

    error SyntheticNotFound();
    error CDPNotFound();
    error BelowMinCollateralRatio();
    error AboveDebtCeiling();
    error NotLiquidatable();
    error InsufficientCollateral();
    error InsufficientDebt();
    error StalePrice();
    error Paused();
    error InvalidAmount();

    // ============================================
    // MODIFIERS
    // ============================================

    modifier whenNotPaused() {
        if (paused) revert Paused();
        _;
    }

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(address _treasury) {
        treasury = _treasury;
        baseInterestRate = 200; // 2% base rate
        utilizationMultiplier = 500;

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(RISK_MANAGER_ROLE, msg.sender);
        _grantRole(ORACLE_ROLE, msg.sender);
    }

    // ============================================
    // SYNTHETIC ASSET MANAGEMENT
    // ============================================

    /**
     * @notice Create a new synthetic asset type
     * @param name Name of the synthetic (e.g., "Synthetic Gold")
     * @param symbol Symbol (e.g., "sXAU")
     * @param collateralToken The collateral backing this synthetic
     * @param minCollateralRatio Minimum collateral ratio in BPS
     * @param liquidationRatio Liquidation threshold in BPS
     * @param debtCeiling Maximum total mintable
     */
    function createSynthetic(
        string memory name,
        string memory symbol,
        address collateralToken,
        address priceOracle,
        uint256 minCollateralRatio,
        uint256 liquidationRatio,
        uint256 liquidationPenalty,
        uint256 stabilityFee,
        uint256 debtCeiling
    ) external onlyRole(DEFAULT_ADMIN_ROLE) returns (bytes32 syntheticId, address syntheticToken) {
        // Deploy synthetic token
        SyntheticToken token = new SyntheticToken(name, symbol, address(this));
        syntheticToken = address(token);

        syntheticId = keccak256(abi.encodePacked(name, symbol, collateralToken));

        syntheticConfigs[syntheticId] = SyntheticConfig({
            syntheticToken: syntheticToken,
            collateralToken: collateralToken,
            priceOracle: priceOracle,
            minCollateralRatio: minCollateralRatio,
            liquidationRatio: liquidationRatio,
            liquidationPenalty: liquidationPenalty,
            stabilityFee: stabilityFee,
            debtCeiling: debtCeiling,
            totalDebt: 0,
            isActive: true
        });

        syntheticIds.push(syntheticId);

        emit SyntheticCreated(syntheticId, syntheticToken, collateralToken, minCollateralRatio);
    }

    // ============================================
    // CDP OPERATIONS
    // ============================================

    /**
     * @notice Open a new CDP by depositing collateral and minting synthetic
     * @param syntheticId The synthetic asset type
     * @param collateralAmount Amount of collateral to deposit
     * @param debtAmount Amount of synthetic to mint
     */
    function openCDP(
        bytes32 syntheticId,
        uint256 collateralAmount,
        uint256 debtAmount
    ) external nonReentrant whenNotPaused {
        SyntheticConfig storage config = syntheticConfigs[syntheticId];
        if (!config.isActive) revert SyntheticNotFound();
        if (collateralAmount == 0 || debtAmount == 0) revert InvalidAmount();

        CDP storage cdp = cdps[syntheticId][msg.sender];
        if (cdp.collateralAmount > 0) {
            // CDP exists, use modifyCDP instead
            revert("Use modifyCDP for existing positions");
        }

        // Check debt ceiling
        if (config.totalDebt + debtAmount > config.debtCeiling) {
            revert AboveDebtCeiling();
        }

        // Verify collateralization ratio
        uint256 collateralValue = getCollateralValue(syntheticId, collateralAmount);
        uint256 ratio = (collateralValue * 10000) / debtAmount;
        if (ratio < config.minCollateralRatio) {
            revert BelowMinCollateralRatio();
        }

        // Transfer collateral
        IERC20(config.collateralToken).safeTransferFrom(msg.sender, address(this), collateralAmount);

        // Create CDP
        cdps[syntheticId][msg.sender] = CDP({
            collateralAmount: collateralAmount,
            debtAmount: debtAmount,
            lastInterestUpdate: block.timestamp,
            accumulatedFees: 0
        });

        // Mint synthetic
        config.totalDebt += debtAmount;
        SyntheticToken(config.syntheticToken).mint(msg.sender, debtAmount);

        emit CDPOpened(syntheticId, msg.sender, collateralAmount, debtAmount);
    }

    /**
     * @notice Modify an existing CDP
     * @param syntheticId The synthetic asset type
     * @param collateralDelta Amount to add (positive) or remove (negative)
     * @param debtDelta Amount to mint (positive) or repay (negative)
     */
    function modifyCDP(
        bytes32 syntheticId,
        int256 collateralDelta,
        int256 debtDelta
    ) external nonReentrant whenNotPaused {
        SyntheticConfig storage config = syntheticConfigs[syntheticId];
        if (!config.isActive) revert SyntheticNotFound();

        CDP storage cdp = cdps[syntheticId][msg.sender];
        if (cdp.collateralAmount == 0 && cdp.debtAmount == 0) {
            revert CDPNotFound();
        }

        // Update accumulated fees
        _accrueInterest(syntheticId, msg.sender);

        // Calculate new values
        uint256 newCollateral;
        uint256 newDebt;

        if (collateralDelta >= 0) {
            newCollateral = cdp.collateralAmount + uint256(collateralDelta);
            if (collateralDelta > 0) {
                IERC20(config.collateralToken).safeTransferFrom(
                    msg.sender, 
                    address(this), 
                    uint256(collateralDelta)
                );
            }
        } else {
            uint256 withdrawAmount = uint256(-collateralDelta);
            if (withdrawAmount > cdp.collateralAmount) revert InsufficientCollateral();
            newCollateral = cdp.collateralAmount - withdrawAmount;
        }

        if (debtDelta >= 0) {
            newDebt = cdp.debtAmount + uint256(debtDelta);
            if (config.totalDebt + uint256(debtDelta) > config.debtCeiling) {
                revert AboveDebtCeiling();
            }
        } else {
            uint256 repayAmount = uint256(-debtDelta);
            if (repayAmount > cdp.debtAmount) revert InsufficientDebt();
            newDebt = cdp.debtAmount - repayAmount;
        }

        // Verify new collateralization ratio (if debt exists)
        if (newDebt > 0) {
            uint256 collateralValue = getCollateralValue(syntheticId, newCollateral);
            uint256 ratio = (collateralValue * 10000) / newDebt;
            if (ratio < config.minCollateralRatio) {
                revert BelowMinCollateralRatio();
            }
        }

        // Apply changes
        if (debtDelta > 0) {
            config.totalDebt += uint256(debtDelta);
            SyntheticToken(config.syntheticToken).mint(msg.sender, uint256(debtDelta));
        } else if (debtDelta < 0) {
            uint256 repayAmount = uint256(-debtDelta);
            config.totalDebt -= repayAmount;
            SyntheticToken(config.syntheticToken).burn(msg.sender, repayAmount);
        }

        if (collateralDelta < 0) {
            IERC20(config.collateralToken).safeTransfer(msg.sender, uint256(-collateralDelta));
        }

        cdp.collateralAmount = newCollateral;
        cdp.debtAmount = newDebt;

        emit CDPModified(syntheticId, msg.sender, collateralDelta, debtDelta);
    }

    /**
     * @notice Close a CDP completely
     * @param syntheticId The synthetic asset type
     */
    function closeCDP(bytes32 syntheticId) external nonReentrant whenNotPaused {
        SyntheticConfig storage config = syntheticConfigs[syntheticId];
        CDP storage cdp = cdps[syntheticId][msg.sender];

        if (cdp.collateralAmount == 0 && cdp.debtAmount == 0) {
            revert CDPNotFound();
        }

        _accrueInterest(syntheticId, msg.sender);

        // Repay all debt + fees
        uint256 totalOwed = cdp.debtAmount + cdp.accumulatedFees;
        if (totalOwed > 0) {
            SyntheticToken(config.syntheticToken).burn(msg.sender, cdp.debtAmount);
            // Fees paid separately or from collateral
        }

        // Return collateral
        uint256 collateralToReturn = cdp.collateralAmount;
        config.totalDebt -= cdp.debtAmount;

        delete cdps[syntheticId][msg.sender];

        IERC20(config.collateralToken).safeTransfer(msg.sender, collateralToReturn);

        emit CDPClosed(syntheticId, msg.sender, collateralToReturn);
    }

    // ============================================
    // LIQUIDATION
    // ============================================

    /**
     * @notice Liquidate an undercollateralized CDP
     * @param syntheticId The synthetic asset type
     * @param owner The CDP owner to liquidate
     */
    function liquidate(
        bytes32 syntheticId,
        address owner
    ) external nonReentrant whenNotPaused {
        SyntheticConfig storage config = syntheticConfigs[syntheticId];
        CDP storage cdp = cdps[syntheticId][owner];

        if (cdp.collateralAmount == 0) revert CDPNotFound();

        _accrueInterest(syntheticId, owner);

        // Check if liquidatable
        uint256 collateralValue = getCollateralValue(syntheticId, cdp.collateralAmount);
        uint256 ratio = (collateralValue * 10000) / cdp.debtAmount;
        if (ratio >= config.liquidationRatio) {
            revert NotLiquidatable();
        }

        // Calculate liquidation amounts
        uint256 debtToRepay = cdp.debtAmount;
        uint256 collateralToSeize = cdp.collateralAmount;
        uint256 penalty = (collateralToSeize * config.liquidationPenalty) / 10000;
        uint256 collateralToLiquidator = collateralToSeize - penalty;

        // Burn synthetic from liquidator
        SyntheticToken(config.syntheticToken).burn(msg.sender, debtToRepay);

        // Update state
        config.totalDebt -= debtToRepay;

        // Clear CDP
        delete cdps[syntheticId][owner];

        // Transfer collateral to liquidator
        IERC20(config.collateralToken).safeTransfer(msg.sender, collateralToLiquidator);
        
        // Transfer penalty to treasury
        if (penalty > 0) {
            IERC20(config.collateralToken).safeTransfer(treasury, penalty);
        }

        emit CDPLiquidated(syntheticId, owner, msg.sender, collateralToSeize, debtToRepay);
    }

    // ============================================
    // INTEREST ACCRUAL
    // ============================================

    function _accrueInterest(bytes32 syntheticId, address owner) internal {
        SyntheticConfig storage config = syntheticConfigs[syntheticId];
        CDP storage cdp = cdps[syntheticId][owner];

        uint256 timeElapsed = block.timestamp - cdp.lastInterestUpdate;
        if (timeElapsed == 0) return;

        // Calculate interest
        uint256 annualRate = config.stabilityFee;
        uint256 interest = (cdp.debtAmount * annualRate * timeElapsed) / (365 days * 10000);

        cdp.accumulatedFees += interest;
        cdp.lastInterestUpdate = block.timestamp;
    }

    // ============================================
    // ORACLE FUNCTIONS
    // ============================================

    /**
     * @notice Update price for an asset (oracle role)
     */
    function updatePrice(
        address asset,
        uint256 price,
        uint8 decimals
    ) external onlyRole(ORACLE_ROLE) {
        prices[asset] = PriceData({
            price: price,
            timestamp: block.timestamp,
            decimals: decimals
        });

        emit PriceUpdated(asset, price, block.timestamp);
    }

    /**
     * @notice Get collateral value in debt terms
     */
    function getCollateralValue(
        bytes32 syntheticId,
        uint256 collateralAmount
    ) public view returns (uint256) {
        SyntheticConfig memory config = syntheticConfigs[syntheticId];
        PriceData memory priceData = prices[config.collateralToken];

        // Check price freshness (1 hour max)
        if (block.timestamp - priceData.timestamp > 1 hours) {
            revert StalePrice();
        }

        return (collateralAmount * priceData.price) / (10 ** priceData.decimals);
    }

    /**
     * @notice Get collateralization ratio for a CDP
     */
    function getCollateralRatio(
        bytes32 syntheticId,
        address owner
    ) external view returns (uint256) {
        CDP memory cdp = cdps[syntheticId][owner];
        if (cdp.debtAmount == 0) return type(uint256).max;

        uint256 collateralValue = getCollateralValue(syntheticId, cdp.collateralAmount);
        return (collateralValue * 10000) / cdp.debtAmount;
    }

    /**
     * @notice Check if a CDP is liquidatable
     */
    function isLiquidatable(bytes32 syntheticId, address owner) external view returns (bool) {
        SyntheticConfig memory config = syntheticConfigs[syntheticId];
        CDP memory cdp = cdps[syntheticId][owner];

        if (cdp.debtAmount == 0) return false;

        uint256 collateralValue = getCollateralValue(syntheticId, cdp.collateralAmount);
        uint256 ratio = (collateralValue * 10000) / cdp.debtAmount;
        
        return ratio < config.liquidationRatio;
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    function getSyntheticCount() external view returns (uint256) {
        return syntheticIds.length;
    }

    function getCDPInfo(
        bytes32 syntheticId,
        address owner
    ) external view returns (
        uint256 collateralAmount,
        uint256 debtAmount,
        uint256 accumulatedFees,
        uint256 collateralRatio
    ) {
        CDP memory cdp = cdps[syntheticId][owner];
        collateralAmount = cdp.collateralAmount;
        debtAmount = cdp.debtAmount;
        accumulatedFees = cdp.accumulatedFees;
        
        if (debtAmount > 0) {
            uint256 collateralValue = getCollateralValue(syntheticId, collateralAmount);
            collateralRatio = (collateralValue * 10000) / debtAmount;
        } else {
            collateralRatio = type(uint256).max;
        }
    }

    // ============================================
    // ADMIN FUNCTIONS
    // ============================================

    function setPaused(bool _paused) external onlyRole(DEFAULT_ADMIN_ROLE) {
        paused = _paused;
    }

    function setTreasury(address _treasury) external onlyRole(DEFAULT_ADMIN_ROLE) {
        treasury = _treasury;
    }

    function updateSyntheticConfig(
        bytes32 syntheticId,
        uint256 minCollateralRatio,
        uint256 liquidationRatio,
        uint256 debtCeiling
    ) external onlyRole(RISK_MANAGER_ROLE) {
        SyntheticConfig storage config = syntheticConfigs[syntheticId];
        config.minCollateralRatio = minCollateralRatio;
        config.liquidationRatio = liquidationRatio;
        config.debtCeiling = debtCeiling;
    }
}

/**
 * @title SyntheticToken
 * @notice ERC20 token representing a synthetic asset
 */
contract SyntheticToken is ERC20 {
    address public factory;

    modifier onlyFactory() {
        require(msg.sender == factory, "Only factory");
        _;
    }

    constructor(
        string memory name,
        string memory symbol,
        address _factory
    ) ERC20(name, symbol) {
        factory = _factory;
    }

    function mint(address to, uint256 amount) external onlyFactory {
        _mint(to, amount);
    }

    function burn(address from, uint256 amount) external onlyFactory {
        _burn(from, amount);
    }
}
