// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/Pausable.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/token/ERC20/extensions/ERC20Burnable.sol";

/**
 * @title StablecoinController
 * @author Grey Team
 * @notice CDP-based algorithmic stablecoin system
 * @dev Implements collateralized debt positions (CDPs) for stablecoin minting
 *      inspired by MakerDAO, Liquity, and Frax
 * 
 * Key mechanisms:
 * - Multi-collateral CDPs with variable collateral ratios
 * - Stability fee (interest) accrual
 * - Liquidation with dynamic incentives
 * - Debt ceiling governance
 * - Price stability module (PSM)
 * - Emergency shutdown capability
 */
contract StablecoinController is AccessControl, ReentrancyGuard, Pausable {
    using SafeERC20 for IERC20;

    // ============ Roles ============
    bytes32 public constant GOVERNANCE_ROLE = keccak256("GOVERNANCE_ROLE");
    bytes32 public constant ORACLE_ROLE = keccak256("ORACLE_ROLE");
    bytes32 public constant LIQUIDATOR_ROLE = keccak256("LIQUIDATOR_ROLE");
    bytes32 public constant EMERGENCY_ROLE = keccak256("EMERGENCY_ROLE");

    // ============ Structs ============
    
    /**
     * @notice Collateral type configuration
     */
    struct CollateralType {
        address token;
        uint256 liquidationRatio;       // e.g., 15000 = 150%
        uint256 stabilityFee;           // Annual rate in basis points
        uint256 debtCeiling;            // Maximum debt for this collateral
        uint256 debtFloor;              // Minimum debt per vault
        uint256 liquidationPenalty;     // e.g., 1300 = 13%
        uint256 price;                  // USD price (18 decimals)
        uint256 lastPriceUpdate;
        uint256 totalCollateral;
        uint256 totalDebt;
        bool isActive;
    }

    /**
     * @notice Individual vault (CDP)
     */
    struct Vault {
        uint256 id;
        address owner;
        bytes32 collateralType;
        uint256 collateralAmount;
        uint256 debtAmount;             // Principal debt
        uint256 accumulatedFees;        // Accumulated stability fees
        uint256 lastFeeUpdate;
        bool isActive;
    }

    /**
     * @notice Price Stability Module configuration
     */
    struct PSMConfig {
        address pegToken;               // e.g., USDC
        uint256 mintFee;                // Fee to mint stablecoin with peg token
        uint256 redeemFee;              // Fee to redeem peg token
        uint256 debtCeiling;            // PSM debt ceiling
        uint256 currentDebt;
        bool isActive;
    }

    /**
     * @notice Global system state
     */
    struct SystemState {
        uint256 totalDebt;              // Total stablecoin debt
        uint256 globalDebtCeiling;      // Maximum total debt
        uint256 baseRate;               // Base stability fee rate
        uint256 surplusBuffer;          // Surplus before auction
        uint256 debtBuffer;             // Bad debt buffer
        bool isShutdown;
    }

    // ============ Constants ============
    uint256 public constant PRECISION = 1e18;
    uint256 public constant BPS = 10000;
    uint256 public constant SECONDS_PER_YEAR = 365 days;
    uint256 public constant MIN_COLLATERAL_RATIO = 10100; // 101%
    uint256 public constant PRICE_STALENESS = 1 hours;

    // ============ State Variables ============
    
    // Stablecoin token
    IERC20 public immutable stablecoin;
    
    // System state
    SystemState public systemState;
    
    // Collateral types
    mapping(bytes32 => CollateralType) public collateralTypes;
    bytes32[] public collateralTypeList;
    
    // Vaults
    mapping(uint256 => Vault) public vaults;
    mapping(address => uint256[]) public userVaults;
    uint256 public nextVaultId = 1;
    
    // PSM
    mapping(address => PSMConfig) public psmConfigs;
    address[] public psmTokens;
    
    // Surplus and debt buffers
    uint256 public surplus;
    uint256 public badDebt;
    
    // Treasury
    address public treasury;

    // ============ Events ============
    event CollateralTypeAdded(bytes32 indexed collateralType, address token);
    event CollateralTypeUpdated(bytes32 indexed collateralType);
    event VaultOpened(uint256 indexed vaultId, address indexed owner, bytes32 collateralType);
    event CollateralDeposited(uint256 indexed vaultId, uint256 amount);
    event CollateralWithdrawn(uint256 indexed vaultId, uint256 amount);
    event DebtMinted(uint256 indexed vaultId, uint256 amount);
    event DebtRepaid(uint256 indexed vaultId, uint256 amount);
    event VaultLiquidated(uint256 indexed vaultId, address indexed liquidator, uint256 collateralSeized);
    event PriceUpdated(bytes32 indexed collateralType, uint256 price);
    event PSMMint(address indexed user, address indexed pegToken, uint256 amount);
    event PSMRedeem(address indexed user, address indexed pegToken, uint256 amount);
    event EmergencyShutdown(address indexed triggeredBy);

    // ============ Errors ============
    error CollateralNotActive();
    error VaultNotFound();
    error VaultNotOwner();
    error InsufficientCollateral();
    error ExceedsDebtCeiling();
    error BelowDebtFloor();
    error UnsafeVault();
    error VaultIsSafe();
    error StalePrice();
    error SystemShutdown();
    error PSMNotActive();
    error ExceedsPSMCeiling();
    error ZeroAmount();

    // ============ Constructor ============
    constructor(address _stablecoin, address _treasury) {
        require(_stablecoin != address(0), "Invalid stablecoin");
        require(_treasury != address(0), "Invalid treasury");
        
        stablecoin = IERC20(_stablecoin);
        treasury = _treasury;
        
        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(GOVERNANCE_ROLE, msg.sender);
        _grantRole(EMERGENCY_ROLE, msg.sender);
        
        systemState.globalDebtCeiling = 100_000_000e18; // 100M default
        systemState.baseRate = 100; // 1% base rate
        systemState.surplusBuffer = 500_000e18; // 500k buffer
    }

    // ============ Collateral Management ============
    
    /**
     * @notice Add a new collateral type
     */
    function addCollateralType(
        address token,
        uint256 liquidationRatio,
        uint256 stabilityFee,
        uint256 debtCeiling,
        uint256 debtFloor,
        uint256 liquidationPenalty
    ) external onlyRole(GOVERNANCE_ROLE) {
        require(token != address(0), "Invalid token");
        require(liquidationRatio >= MIN_COLLATERAL_RATIO, "Ratio too low");
        
        bytes32 collateralType = keccak256(abi.encodePacked(token));
        require(collateralTypes[collateralType].token == address(0), "Already exists");
        
        collateralTypes[collateralType] = CollateralType({
            token: token,
            liquidationRatio: liquidationRatio,
            stabilityFee: stabilityFee,
            debtCeiling: debtCeiling,
            debtFloor: debtFloor,
            liquidationPenalty: liquidationPenalty,
            price: 0,
            lastPriceUpdate: 0,
            totalCollateral: 0,
            totalDebt: 0,
            isActive: true
        });
        
        collateralTypeList.push(collateralType);
        
        emit CollateralTypeAdded(collateralType, token);
    }

    /**
     * @notice Update collateral type parameters
     */
    function updateCollateralType(
        bytes32 collateralType,
        uint256 liquidationRatio,
        uint256 stabilityFee,
        uint256 debtCeiling,
        uint256 debtFloor,
        uint256 liquidationPenalty,
        bool isActive
    ) external onlyRole(GOVERNANCE_ROLE) {
        CollateralType storage ct = collateralTypes[collateralType];
        require(ct.token != address(0), "Not found");
        
        ct.liquidationRatio = liquidationRatio;
        ct.stabilityFee = stabilityFee;
        ct.debtCeiling = debtCeiling;
        ct.debtFloor = debtFloor;
        ct.liquidationPenalty = liquidationPenalty;
        ct.isActive = isActive;
        
        emit CollateralTypeUpdated(collateralType);
    }

    /**
     * @notice Update collateral price (called by oracle)
     */
    function updatePrice(bytes32 collateralType, uint256 price) external onlyRole(ORACLE_ROLE) {
        CollateralType storage ct = collateralTypes[collateralType];
        require(ct.token != address(0), "Not found");
        
        ct.price = price;
        ct.lastPriceUpdate = block.timestamp;
        
        emit PriceUpdated(collateralType, price);
    }

    // ============ Vault Operations ============
    
    /**
     * @notice Open a new vault
     */
    function openVault(bytes32 collateralType) external whenNotPaused returns (uint256 vaultId) {
        if (systemState.isShutdown) revert SystemShutdown();
        
        CollateralType storage ct = collateralTypes[collateralType];
        if (!ct.isActive) revert CollateralNotActive();
        
        vaultId = nextVaultId++;
        vaults[vaultId] = Vault({
            id: vaultId,
            owner: msg.sender,
            collateralType: collateralType,
            collateralAmount: 0,
            debtAmount: 0,
            accumulatedFees: 0,
            lastFeeUpdate: block.timestamp,
            isActive: true
        });
        
        userVaults[msg.sender].push(vaultId);
        
        emit VaultOpened(vaultId, msg.sender, collateralType);
        
        return vaultId;
    }

    /**
     * @notice Deposit collateral into a vault
     */
    function depositCollateral(
        uint256 vaultId,
        uint256 amount
    ) external nonReentrant whenNotPaused {
        if (amount == 0) revert ZeroAmount();
        
        Vault storage vault = vaults[vaultId];
        if (!vault.isActive) revert VaultNotFound();
        if (vault.owner != msg.sender) revert VaultNotOwner();
        
        CollateralType storage ct = collateralTypes[vault.collateralType];
        
        IERC20(ct.token).safeTransferFrom(msg.sender, address(this), amount);
        
        vault.collateralAmount += amount;
        ct.totalCollateral += amount;
        
        emit CollateralDeposited(vaultId, amount);
    }

    /**
     * @notice Withdraw collateral from a vault
     */
    function withdrawCollateral(
        uint256 vaultId,
        uint256 amount
    ) external nonReentrant {
        if (amount == 0) revert ZeroAmount();
        
        Vault storage vault = vaults[vaultId];
        if (!vault.isActive) revert VaultNotFound();
        if (vault.owner != msg.sender) revert VaultNotOwner();
        if (vault.collateralAmount < amount) revert InsufficientCollateral();
        
        // Accrue fees first
        _accrueFees(vaultId);
        
        // Simulate withdrawal and check if safe
        uint256 newCollateral = vault.collateralAmount - amount;
        if (!_isVaultSafe(vault.collateralType, newCollateral, vault.debtAmount + vault.accumulatedFees)) {
            revert UnsafeVault();
        }
        
        vault.collateralAmount = newCollateral;
        
        CollateralType storage ct = collateralTypes[vault.collateralType];
        ct.totalCollateral -= amount;
        
        IERC20(ct.token).safeTransfer(msg.sender, amount);
        
        emit CollateralWithdrawn(vaultId, amount);
    }

    /**
     * @notice Mint stablecoin (borrow) against vault collateral
     */
    function mintDebt(
        uint256 vaultId,
        uint256 amount
    ) external nonReentrant whenNotPaused {
        if (systemState.isShutdown) revert SystemShutdown();
        if (amount == 0) revert ZeroAmount();
        
        Vault storage vault = vaults[vaultId];
        if (!vault.isActive) revert VaultNotFound();
        if (vault.owner != msg.sender) revert VaultNotOwner();
        
        // Accrue fees first
        _accrueFees(vaultId);
        
        CollateralType storage ct = collateralTypes[vault.collateralType];
        if (!ct.isActive) revert CollateralNotActive();
        
        // Check debt ceilings
        if (ct.totalDebt + amount > ct.debtCeiling) revert ExceedsDebtCeiling();
        if (systemState.totalDebt + amount > systemState.globalDebtCeiling) revert ExceedsDebtCeiling();
        
        // Check debt floor
        uint256 newDebt = vault.debtAmount + amount;
        if (newDebt > 0 && newDebt < ct.debtFloor) revert BelowDebtFloor();
        
        // Check collateralization
        if (!_isVaultSafe(vault.collateralType, vault.collateralAmount, newDebt + vault.accumulatedFees)) {
            revert UnsafeVault();
        }
        
        vault.debtAmount = newDebt;
        ct.totalDebt += amount;
        systemState.totalDebt += amount;
        
        // Mint stablecoin to user
        // In production, would call stablecoin.mint()
        // For now, transfer from this contract (assumes pre-minted)
        stablecoin.safeTransfer(msg.sender, amount);
        
        emit DebtMinted(vaultId, amount);
    }

    /**
     * @notice Repay debt (burn stablecoin)
     */
    function repayDebt(
        uint256 vaultId,
        uint256 amount
    ) external nonReentrant {
        if (amount == 0) revert ZeroAmount();
        
        Vault storage vault = vaults[vaultId];
        if (!vault.isActive) revert VaultNotFound();
        
        // Accrue fees first
        _accrueFees(vaultId);
        
        CollateralType storage ct = collateralTypes[vault.collateralType];
        
        // Calculate total owed
        uint256 totalOwed = vault.debtAmount + vault.accumulatedFees;
        uint256 repayAmount = amount > totalOwed ? totalOwed : amount;
        
        // Transfer stablecoin from user
        stablecoin.safeTransferFrom(msg.sender, address(this), repayAmount);
        
        // Apply to fees first, then principal
        if (repayAmount <= vault.accumulatedFees) {
            vault.accumulatedFees -= repayAmount;
            surplus += repayAmount;
        } else {
            uint256 remainingRepay = repayAmount - vault.accumulatedFees;
            surplus += vault.accumulatedFees;
            vault.accumulatedFees = 0;
            vault.debtAmount -= remainingRepay;
            ct.totalDebt -= remainingRepay;
            systemState.totalDebt -= remainingRepay;
            // Burn the principal portion
            // In production: ERC20Burnable(address(stablecoin)).burn(remainingRepay);
        }
        
        // Check debt floor after repayment
        if (vault.debtAmount > 0 && vault.debtAmount < ct.debtFloor) {
            revert BelowDebtFloor();
        }
        
        emit DebtRepaid(vaultId, repayAmount);
    }

    /**
     * @notice Close vault (repay all debt and withdraw all collateral)
     */
    function closeVault(uint256 vaultId) external nonReentrant {
        Vault storage vault = vaults[vaultId];
        if (!vault.isActive) revert VaultNotFound();
        if (vault.owner != msg.sender) revert VaultNotOwner();
        
        _accrueFees(vaultId);
        
        // Repay all debt
        uint256 totalOwed = vault.debtAmount + vault.accumulatedFees;
        if (totalOwed > 0) {
            stablecoin.safeTransferFrom(msg.sender, address(this), totalOwed);
            
            CollateralType storage ct = collateralTypes[vault.collateralType];
            ct.totalDebt -= vault.debtAmount;
            systemState.totalDebt -= vault.debtAmount;
            surplus += vault.accumulatedFees;
            
            // Burn principal
            // In production: ERC20Burnable(address(stablecoin)).burn(vault.debtAmount);
        }
        
        // Return all collateral
        if (vault.collateralAmount > 0) {
            CollateralType storage ct = collateralTypes[vault.collateralType];
            ct.totalCollateral -= vault.collateralAmount;
            IERC20(ct.token).safeTransfer(msg.sender, vault.collateralAmount);
        }
        
        vault.isActive = false;
        vault.debtAmount = 0;
        vault.accumulatedFees = 0;
        vault.collateralAmount = 0;
    }

    // ============ Liquidation ============
    
    /**
     * @notice Liquidate an unsafe vault
     */
    function liquidate(uint256 vaultId) external nonReentrant {
        Vault storage vault = vaults[vaultId];
        if (!vault.isActive) revert VaultNotFound();
        
        _accrueFees(vaultId);
        
        CollateralType storage ct = collateralTypes[vault.collateralType];
        
        // Check if vault is unsafe
        uint256 totalDebt = vault.debtAmount + vault.accumulatedFees;
        if (_isVaultSafe(vault.collateralType, vault.collateralAmount, totalDebt)) {
            revert VaultIsSafe();
        }
        
        // Calculate liquidation amounts
        // Liquidator pays debt and receives collateral + penalty
        uint256 collateralValue = (vault.collateralAmount * ct.price) / PRECISION;
        uint256 debtWithPenalty = (totalDebt * (BPS + ct.liquidationPenalty)) / BPS;
        
        uint256 collateralToSeize;
        uint256 debtToCover;
        
        if (collateralValue >= debtWithPenalty) {
            // Full liquidation with surplus
            collateralToSeize = (debtWithPenalty * PRECISION) / ct.price;
            debtToCover = totalDebt;
        } else {
            // Underwater - seize all collateral
            collateralToSeize = vault.collateralAmount;
            debtToCover = (collateralValue * BPS) / (BPS + ct.liquidationPenalty);
            
            // Remaining debt is bad debt
            uint256 remainingBadDebt = totalDebt - debtToCover;
            badDebt += remainingBadDebt;
        }
        
        // Liquidator pays debt
        stablecoin.safeTransferFrom(msg.sender, address(this), debtToCover);
        
        // Update vault
        vault.collateralAmount -= collateralToSeize;
        vault.debtAmount = vault.debtAmount > debtToCover ? vault.debtAmount - debtToCover : 0;
        vault.accumulatedFees = 0;
        
        if (vault.debtAmount == 0) {
            vault.isActive = false;
        }
        
        // Update totals
        ct.totalCollateral -= collateralToSeize;
        ct.totalDebt -= debtToCover;
        systemState.totalDebt -= debtToCover;
        
        // Transfer collateral to liquidator
        IERC20(ct.token).safeTransfer(msg.sender, collateralToSeize);
        
        // Burn the debt
        // In production: ERC20Burnable(address(stablecoin)).burn(debtToCover);
        
        emit VaultLiquidated(vaultId, msg.sender, collateralToSeize);
    }

    // ============ Price Stability Module (PSM) ============
    
    /**
     * @notice Configure PSM for a peg token
     */
    function configurePSM(
        address pegToken,
        uint256 mintFee,
        uint256 redeemFee,
        uint256 debtCeiling
    ) external onlyRole(GOVERNANCE_ROLE) {
        require(pegToken != address(0), "Invalid token");
        
        if (psmConfigs[pegToken].pegToken == address(0)) {
            psmTokens.push(pegToken);
        }
        
        psmConfigs[pegToken] = PSMConfig({
            pegToken: pegToken,
            mintFee: mintFee,
            redeemFee: redeemFee,
            debtCeiling: debtCeiling,
            currentDebt: psmConfigs[pegToken].currentDebt,
            isActive: true
        });
    }

    /**
     * @notice Mint stablecoin using peg token (e.g., USDC -> stablecoin)
     */
    function psmMint(address pegToken, uint256 amount) external nonReentrant whenNotPaused {
        if (systemState.isShutdown) revert SystemShutdown();
        
        PSMConfig storage psm = psmConfigs[pegToken];
        if (!psm.isActive) revert PSMNotActive();
        if (psm.currentDebt + amount > psm.debtCeiling) revert ExceedsPSMCeiling();
        
        // Calculate output after fee
        uint256 fee = (amount * psm.mintFee) / BPS;
        uint256 outputAmount = amount - fee;
        
        // Transfer peg token from user
        IERC20(pegToken).safeTransferFrom(msg.sender, address(this), amount);
        
        // Update PSM state
        psm.currentDebt += outputAmount;
        systemState.totalDebt += outputAmount;
        surplus += fee;
        
        // Mint stablecoin to user
        stablecoin.safeTransfer(msg.sender, outputAmount);
        
        emit PSMMint(msg.sender, pegToken, outputAmount);
    }

    /**
     * @notice Redeem peg token using stablecoin (stablecoin -> USDC)
     */
    function psmRedeem(address pegToken, uint256 amount) external nonReentrant {
        PSMConfig storage psm = psmConfigs[pegToken];
        if (!psm.isActive) revert PSMNotActive();
        if (amount > psm.currentDebt) revert InsufficientCollateral();
        
        // Calculate output after fee
        uint256 fee = (amount * psm.redeemFee) / BPS;
        uint256 outputAmount = amount - fee;
        
        // Transfer stablecoin from user
        stablecoin.safeTransferFrom(msg.sender, address(this), amount);
        
        // Update PSM state
        psm.currentDebt -= amount;
        systemState.totalDebt -= amount;
        surplus += fee;
        
        // Burn stablecoin
        // In production: ERC20Burnable(address(stablecoin)).burn(amount);
        
        // Transfer peg token to user
        IERC20(pegToken).safeTransfer(msg.sender, outputAmount);
        
        emit PSMRedeem(msg.sender, pegToken, outputAmount);
    }

    // ============ Internal Functions ============
    
    function _accrueFees(uint256 vaultId) internal {
        Vault storage vault = vaults[vaultId];
        if (vault.debtAmount == 0) return;
        
        CollateralType storage ct = collateralTypes[vault.collateralType];
        
        uint256 timeElapsed = block.timestamp - vault.lastFeeUpdate;
        if (timeElapsed == 0) return;
        
        // Calculate stability fee
        uint256 annualRate = systemState.baseRate + ct.stabilityFee;
        uint256 fee = (vault.debtAmount * annualRate * timeElapsed) / (BPS * SECONDS_PER_YEAR);
        
        vault.accumulatedFees += fee;
        vault.lastFeeUpdate = block.timestamp;
    }

    function _isVaultSafe(
        bytes32 collateralType,
        uint256 collateralAmount,
        uint256 debtAmount
    ) internal view returns (bool) {
        if (debtAmount == 0) return true;
        
        CollateralType storage ct = collateralTypes[collateralType];
        
        // Check price freshness
        if (block.timestamp > ct.lastPriceUpdate + PRICE_STALENESS) {
            revert StalePrice();
        }
        
        // Calculate collateral ratio
        uint256 collateralValue = (collateralAmount * ct.price) / PRECISION;
        uint256 requiredCollateral = (debtAmount * ct.liquidationRatio) / BPS;
        
        return collateralValue >= requiredCollateral;
    }

    // ============ View Functions ============
    
    /**
     * @notice Get vault info with current fees
     */
    function getVaultInfo(uint256 vaultId) external view returns (
        address owner,
        bytes32 collateralType,
        uint256 collateralAmount,
        uint256 debtAmount,
        uint256 accumulatedFees,
        uint256 collateralRatio,
        bool isSafe
    ) {
        Vault storage vault = vaults[vaultId];
        CollateralType storage ct = collateralTypes[vault.collateralType];
        
        owner = vault.owner;
        collateralType = vault.collateralType;
        collateralAmount = vault.collateralAmount;
        debtAmount = vault.debtAmount;
        
        // Calculate current fees
        uint256 timeElapsed = block.timestamp - vault.lastFeeUpdate;
        uint256 annualRate = systemState.baseRate + ct.stabilityFee;
        accumulatedFees = vault.accumulatedFees + 
            (vault.debtAmount * annualRate * timeElapsed) / (BPS * SECONDS_PER_YEAR);
        
        // Calculate ratio
        uint256 totalDebt = debtAmount + accumulatedFees;
        if (totalDebt > 0 && ct.price > 0) {
            uint256 collateralValue = (collateralAmount * ct.price) / PRECISION;
            collateralRatio = (collateralValue * BPS) / totalDebt;
        }
        
        isSafe = collateralRatio >= ct.liquidationRatio;
    }

    /**
     * @notice Get user's vaults
     */
    function getUserVaults(address user) external view returns (uint256[] memory) {
        return userVaults[user];
    }

    /**
     * @notice Get collateral type info
     */
    function getCollateralTypeInfo(bytes32 collateralType) external view returns (
        CollateralType memory
    ) {
        return collateralTypes[collateralType];
    }

    /**
     * @notice Get all collateral types
     */
    function getAllCollateralTypes() external view returns (bytes32[] memory) {
        return collateralTypeList;
    }

    /**
     * @notice Check if vault is liquidatable
     */
    function isLiquidatable(uint256 vaultId) external view returns (bool) {
        Vault storage vault = vaults[vaultId];
        if (!vault.isActive) return false;
        
        CollateralType storage ct = collateralTypes[vault.collateralType];
        
        // Calculate current fees
        uint256 timeElapsed = block.timestamp - vault.lastFeeUpdate;
        uint256 annualRate = systemState.baseRate + ct.stabilityFee;
        uint256 currentFees = vault.accumulatedFees + 
            (vault.debtAmount * annualRate * timeElapsed) / (BPS * SECONDS_PER_YEAR);
        
        uint256 totalDebt = vault.debtAmount + currentFees;
        
        // Check if unsafe
        if (ct.price == 0) return false;
        uint256 collateralValue = (vault.collateralAmount * ct.price) / PRECISION;
        uint256 requiredCollateral = (totalDebt * ct.liquidationRatio) / BPS;
        
        return collateralValue < requiredCollateral;
    }

    // ============ Emergency Functions ============
    
    /**
     * @notice Trigger emergency shutdown
     */
    function emergencyShutdown() external onlyRole(EMERGENCY_ROLE) {
        systemState.isShutdown = true;
        _pause();
        emit EmergencyShutdown(msg.sender);
    }

    // ============ Admin Functions ============
    
    function setTreasury(address _treasury) external onlyRole(GOVERNANCE_ROLE) {
        require(_treasury != address(0), "Invalid treasury");
        treasury = _treasury;
    }

    function setGlobalDebtCeiling(uint256 ceiling) external onlyRole(GOVERNANCE_ROLE) {
        systemState.globalDebtCeiling = ceiling;
    }

    function setBaseRate(uint256 rate) external onlyRole(GOVERNANCE_ROLE) {
        systemState.baseRate = rate;
    }

    function withdrawSurplus(uint256 amount) external onlyRole(GOVERNANCE_ROLE) {
        require(amount <= surplus - systemState.surplusBuffer, "Exceeds available");
        surplus -= amount;
        stablecoin.safeTransfer(treasury, amount);
    }

    function pause() external onlyRole(EMERGENCY_ROLE) {
        _pause();
    }

    function unpause() external onlyRole(GOVERNANCE_ROLE) {
        require(!systemState.isShutdown, "System shutdown");
        _unpause();
    }
}
