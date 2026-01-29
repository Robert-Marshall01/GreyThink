// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/Pausable.sol";
import "@openzeppelin/contracts/utils/math/Math.sol";

/**
 * @title YieldAggregator
 * @author Grey Protocol Team
 * @notice Advanced yield aggregator with strategy management
 * @dev Implements multi-strategy yield optimization with auto-compounding
 * 
 * Features:
 * - Multiple yield strategies
 * - Auto-compounding
 * - Strategy rotation
 * - Risk scoring
 * - Performance fees
 * - Harvest rewards
 * - Emergency withdrawals
 * - Strategy caps
 */
contract YieldAggregator is AccessControl, ReentrancyGuard, Pausable {
    using SafeERC20 for IERC20;
    using Math for uint256;

    // ============================================
    // CONSTANTS & ROLES
    // ============================================

    bytes32 public constant STRATEGIST_ROLE = keccak256("STRATEGIST_ROLE");
    bytes32 public constant KEEPER_ROLE = keccak256("KEEPER_ROLE");
    bytes32 public constant GUARDIAN_ROLE = keccak256("GUARDIAN_ROLE");

    uint256 public constant PRECISION = 1e18;
    uint256 public constant BPS = 10000;
    uint256 public constant MAX_STRATEGIES = 20;
    uint256 public constant MAX_PERFORMANCE_FEE = 3000; // 30%
    uint256 public constant MAX_MANAGEMENT_FEE = 500; // 5%
    uint256 public constant MAX_WITHDRAWAL_FEE = 100; // 1%
    uint256 public constant MIN_HARVEST_INTERVAL = 1 hours;

    // ============================================
    // ENUMS
    // ============================================

    enum StrategyStatus {
        Inactive,
        Active,
        Deprecated,
        Emergency
    }

    enum RiskLevel {
        Conservative,
        Moderate,
        Aggressive,
        Degen
    }

    enum VaultStatus {
        Open,
        DepositOnly,
        WithdrawOnly,
        Closed,
        Emergency
    }

    // ============================================
    // STRUCTS
    // ============================================

    /**
     * @notice Strategy configuration
     */
    struct Strategy {
        address strategyAddress;
        string name;
        string protocol;
        StrategyStatus status;
        RiskLevel riskLevel;
        uint256 allocation;            // Target allocation in BPS
        uint256 maxAllocation;         // Maximum allowed allocation
        uint256 minAllocation;         // Minimum required allocation
        uint256 totalDeposited;
        uint256 totalWithdrawn;
        uint256 currentBalance;
        uint256 lastHarvest;
        uint256 totalHarvested;
        uint256 harvestCount;
        uint256 addedAt;
        uint256 riskScore;             // 1-100
        bool autoHarvest;
        bool autoCompound;
    }

    /**
     * @notice Vault configuration
     */
    struct VaultConfig {
        IERC20 asset;
        string name;
        string symbol;
        uint8 decimals;
        uint256 totalAssets;
        uint256 totalShares;
        uint256 performanceFee;
        uint256 managementFee;
        uint256 withdrawalFee;
        uint256 depositLimit;
        uint256 minDeposit;
        uint256 lastFeeCollection;
        VaultStatus status;
    }

    /**
     * @notice User deposit info
     */
    struct UserDeposit {
        uint256 shares;
        uint256 depositedAssets;
        uint256 depositTime;
        uint256 lastAction;
        uint256 harvestsClaimed;
        bool isLocked;
        uint256 lockExpiry;
    }

    /**
     * @notice Harvest record
     */
    struct HarvestRecord {
        uint256 timestamp;
        uint256 strategyId;
        uint256 profitBefore;
        uint256 profitAfter;
        uint256 feeCollected;
        address harvester;
        uint256 gasUsed;
    }

    /**
     * @notice Rebalance record
     */
    struct RebalanceRecord {
        uint256 timestamp;
        uint256[] fromStrategies;
        uint256[] toStrategies;
        uint256[] amounts;
        address initiator;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Vault configuration
    VaultConfig public vault;

    /// @notice Strategies
    mapping(uint256 => Strategy) public strategies;

    /// @notice Strategy addresses to IDs
    mapping(address => uint256) public strategyIds;

    /// @notice Active strategy count
    uint256 public activeStrategyCount;

    /// @notice Strategy counter
    uint256 public strategyCounter;

    /// @notice Active strategy IDs
    uint256[] public activeStrategies;

    /// @notice User deposits
    mapping(address => UserDeposit) public userDeposits;

    /// @notice Total users
    uint256 public totalUsers;

    /// @notice Harvest history
    HarvestRecord[] public harvestHistory;

    /// @notice Rebalance history
    RebalanceRecord[] public rebalanceHistory;

    /// @notice Treasury address
    address public treasury;

    /// @notice Harvest reward (in BPS)
    uint256 public harvestReward = 100; // 1%

    /// @notice Min time between harvests
    uint256 public minHarvestInterval = MIN_HARVEST_INTERVAL;

    /// @notice Slippage tolerance (in BPS)
    uint256 public slippageTolerance = 100; // 1%

    /// @notice Emergency mode
    bool public emergencyMode;

    /// @notice Total profit generated
    uint256 public totalProfit;

    /// @notice Total fees collected
    uint256 public totalFeesCollected;

    /// @notice Pending fees
    uint256 public pendingFees;

    // ============================================
    // EVENTS
    // ============================================

    event VaultCreated(
        address indexed asset,
        string name,
        uint256 depositLimit
    );

    event Deposited(
        address indexed user,
        uint256 assets,
        uint256 shares
    );

    event Withdrawn(
        address indexed user,
        uint256 assets,
        uint256 shares,
        uint256 fee
    );

    event StrategyAdded(
        uint256 indexed strategyId,
        address indexed strategyAddress,
        string name,
        uint256 allocation
    );

    event StrategyRemoved(
        uint256 indexed strategyId,
        address indexed strategyAddress
    );

    event StrategyUpdated(
        uint256 indexed strategyId,
        uint256 newAllocation,
        StrategyStatus newStatus
    );

    event Harvested(
        uint256 indexed strategyId,
        uint256 profit,
        uint256 fee,
        address indexed harvester
    );

    event Compounded(
        uint256 indexed strategyId,
        uint256 amount
    );

    event Rebalanced(
        uint256[] fromStrategies,
        uint256[] toStrategies,
        uint256[] amounts
    );

    event FeesCollected(
        uint256 performanceFee,
        uint256 managementFee,
        address indexed to
    );

    event EmergencyWithdrawn(
        uint256 indexed strategyId,
        uint256 amount
    );

    event EmergencyModeActivated(address indexed by);
    event EmergencyModeDeactivated(address indexed by);

    // ============================================
    // ERRORS
    // ============================================

    error VaultClosed();
    error DepositsDisabled();
    error WithdrawalsDisabled();
    error DepositLimitExceeded(uint256 limit, uint256 requested);
    error MinDepositNotMet(uint256 min, uint256 amount);
    error InsufficientShares(uint256 available, uint256 requested);
    error InsufficientAssets(uint256 available, uint256 requested);
    error StrategyNotFound(uint256 strategyId);
    error StrategyNotActive(uint256 strategyId);
    error StrategyAlreadyExists(address strategy);
    error MaxStrategiesReached();
    error InvalidAllocation();
    error AllocationExceedsMax(uint256 allocation, uint256 max);
    error HarvestTooSoon(uint256 nextHarvest);
    error NoProfit();
    error EmergencyModeActive();
    error NotEmergencyMode();
    error InvalidFee(uint256 fee, uint256 max);
    error LockedDeposit(uint256 lockExpiry);
    error ZeroAmount();
    error InvalidSlippage();

    // ============================================
    // MODIFIERS
    // ============================================

    modifier vaultOpen() {
        if (vault.status == VaultStatus.Closed) revert VaultClosed();
        _;
    }

    modifier depositsEnabled() {
        if (vault.status == VaultStatus.WithdrawOnly || vault.status == VaultStatus.Closed) {
            revert DepositsDisabled();
        }
        _;
    }

    modifier withdrawalsEnabled() {
        if (vault.status == VaultStatus.DepositOnly || vault.status == VaultStatus.Closed) {
            revert WithdrawalsDisabled();
        }
        _;
    }

    modifier notEmergency() {
        if (emergencyMode) revert EmergencyModeActive();
        _;
    }

    modifier onlyEmergency() {
        if (!emergencyMode) revert NotEmergencyMode();
        _;
    }

    modifier strategyExists(uint256 strategyId) {
        if (strategies[strategyId].strategyAddress == address(0)) {
            revert StrategyNotFound(strategyId);
        }
        _;
    }

    modifier strategyActive(uint256 strategyId) {
        if (strategies[strategyId].status != StrategyStatus.Active) {
            revert StrategyNotActive(strategyId);
        }
        _;
    }

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(
        address _asset,
        string memory _name,
        string memory _symbol,
        address _treasury,
        uint256 _depositLimit
    ) {
        vault = VaultConfig({
            asset: IERC20(_asset),
            name: _name,
            symbol: _symbol,
            decimals: 18,
            totalAssets: 0,
            totalShares: 0,
            performanceFee: 1000, // 10%
            managementFee: 200,   // 2%
            withdrawalFee: 0,
            depositLimit: _depositLimit,
            minDeposit: 1e18,
            lastFeeCollection: block.timestamp,
            status: VaultStatus.Open
        });

        treasury = _treasury;

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(STRATEGIST_ROLE, msg.sender);
        _grantRole(KEEPER_ROLE, msg.sender);
        _grantRole(GUARDIAN_ROLE, msg.sender);

        emit VaultCreated(_asset, _name, _depositLimit);
    }

    // ============================================
    // DEPOSIT & WITHDRAW
    // ============================================

    /**
     * @notice Deposits assets into the vault
     * @param assets Amount of assets to deposit
     * @param receiver Receiver of shares
     */
    function deposit(
        uint256 assets,
        address receiver
    ) 
        external 
        nonReentrant 
        whenNotPaused 
        vaultOpen 
        depositsEnabled 
        notEmergency
        returns (uint256 shares) 
    {
        if (assets == 0) revert ZeroAmount();
        if (assets < vault.minDeposit) revert MinDepositNotMet(vault.minDeposit, assets);
        if (vault.depositLimit > 0 && vault.totalAssets + assets > vault.depositLimit) {
            revert DepositLimitExceeded(vault.depositLimit, vault.totalAssets + assets);
        }

        // Calculate shares
        shares = _convertToShares(assets);

        // Update user deposit
        UserDeposit storage userDep = userDeposits[receiver];
        if (userDep.shares == 0) {
            totalUsers++;
            userDep.depositTime = block.timestamp;
        }
        userDep.shares += shares;
        userDep.depositedAssets += assets;
        userDep.lastAction = block.timestamp;

        // Update vault
        vault.totalAssets += assets;
        vault.totalShares += shares;

        // Transfer assets
        vault.asset.safeTransferFrom(msg.sender, address(this), assets);

        // Deploy to strategies
        _deployToStrategies(assets);

        emit Deposited(receiver, assets, shares);
    }

    /**
     * @notice Withdraws assets from the vault
     * @param shares Amount of shares to redeem
     * @param receiver Receiver of assets
     * @param owner Owner of shares
     */
    function withdraw(
        uint256 shares,
        address receiver,
        address owner
    ) 
        external 
        nonReentrant 
        whenNotPaused 
        vaultOpen 
        withdrawalsEnabled
        returns (uint256 assets) 
    {
        if (shares == 0) revert ZeroAmount();

        UserDeposit storage userDep = userDeposits[owner];
        if (shares > userDep.shares) revert InsufficientShares(userDep.shares, shares);
        if (userDep.isLocked && block.timestamp < userDep.lockExpiry) {
            revert LockedDeposit(userDep.lockExpiry);
        }

        // Calculate assets
        assets = _convertToAssets(shares);

        // Calculate withdrawal fee
        uint256 fee = 0;
        if (vault.withdrawalFee > 0) {
            fee = (assets * vault.withdrawalFee) / BPS;
            assets -= fee;
            pendingFees += fee;
        }

        // Withdraw from strategies if needed
        uint256 available = vault.asset.balanceOf(address(this));
        if (assets + fee > available) {
            _withdrawFromStrategies(assets + fee - available);
        }

        // Update user
        userDep.shares -= shares;
        userDep.lastAction = block.timestamp;
        if (userDep.shares == 0) {
            totalUsers--;
        }

        // Update vault
        vault.totalAssets -= assets + fee;
        vault.totalShares -= shares;

        // Transfer
        vault.asset.safeTransfer(receiver, assets);

        emit Withdrawn(owner, assets, shares, fee);
    }

    /**
     * @notice Emergency withdraw all from a user (skips strategies)
     */
    function emergencyWithdraw()
        external
        nonReentrant
        onlyEmergency
        returns (uint256 assets)
    {
        UserDeposit storage userDep = userDeposits[msg.sender];
        uint256 shares = userDep.shares;
        if (shares == 0) revert InsufficientShares(0, shares);

        assets = _convertToAssets(shares);

        // Only withdraw from vault balance
        uint256 available = vault.asset.balanceOf(address(this));
        if (assets > available) {
            assets = available;
        }

        userDep.shares = 0;
        vault.totalAssets -= assets;
        vault.totalShares -= shares;

        vault.asset.safeTransfer(msg.sender, assets);

        emit Withdrawn(msg.sender, assets, shares, 0);
    }

    // ============================================
    // STRATEGY MANAGEMENT
    // ============================================

    /**
     * @notice Adds a new strategy
     */
    function addStrategy(
        address strategyAddress,
        string calldata name,
        string calldata protocol,
        RiskLevel riskLevel,
        uint256 allocation,
        uint256 maxAllocation,
        uint256 riskScore,
        bool autoHarvest,
        bool autoCompound
    ) external onlyRole(STRATEGIST_ROLE) returns (uint256 strategyId) {
        if (strategyAddress == address(0)) revert StrategyNotFound(0);
        if (strategyIds[strategyAddress] != 0) revert StrategyAlreadyExists(strategyAddress);
        if (activeStrategyCount >= MAX_STRATEGIES) revert MaxStrategiesReached();
        if (allocation > maxAllocation) revert AllocationExceedsMax(allocation, maxAllocation);

        strategyId = ++strategyCounter;

        strategies[strategyId] = Strategy({
            strategyAddress: strategyAddress,
            name: name,
            protocol: protocol,
            status: StrategyStatus.Active,
            riskLevel: riskLevel,
            allocation: allocation,
            maxAllocation: maxAllocation,
            minAllocation: 0,
            totalDeposited: 0,
            totalWithdrawn: 0,
            currentBalance: 0,
            lastHarvest: 0,
            totalHarvested: 0,
            harvestCount: 0,
            addedAt: block.timestamp,
            riskScore: riskScore,
            autoHarvest: autoHarvest,
            autoCompound: autoCompound
        });

        strategyIds[strategyAddress] = strategyId;
        activeStrategies.push(strategyId);
        activeStrategyCount++;

        emit StrategyAdded(strategyId, strategyAddress, name, allocation);
    }

    /**
     * @notice Removes a strategy
     */
    function removeStrategy(uint256 strategyId) 
        external 
        onlyRole(STRATEGIST_ROLE) 
        strategyExists(strategyId) 
    {
        Strategy storage strategy = strategies[strategyId];

        // Withdraw all from strategy first
        if (strategy.currentBalance > 0) {
            _withdrawFromStrategy(strategyId, strategy.currentBalance);
        }

        strategy.status = StrategyStatus.Inactive;
        delete strategyIds[strategy.strategyAddress];

        // Remove from active list
        for (uint256 i = 0; i < activeStrategies.length; i++) {
            if (activeStrategies[i] == strategyId) {
                activeStrategies[i] = activeStrategies[activeStrategies.length - 1];
                activeStrategies.pop();
                break;
            }
        }
        activeStrategyCount--;

        emit StrategyRemoved(strategyId, strategy.strategyAddress);
    }

    /**
     * @notice Updates strategy allocation
     */
    function updateStrategyAllocation(
        uint256 strategyId,
        uint256 newAllocation
    ) external onlyRole(STRATEGIST_ROLE) strategyExists(strategyId) {
        Strategy storage strategy = strategies[strategyId];
        
        if (newAllocation > strategy.maxAllocation) {
            revert AllocationExceedsMax(newAllocation, strategy.maxAllocation);
        }

        strategy.allocation = newAllocation;

        emit StrategyUpdated(strategyId, newAllocation, strategy.status);
    }

    /**
     * @notice Updates strategy status
     */
    function updateStrategyStatus(
        uint256 strategyId,
        StrategyStatus newStatus
    ) external onlyRole(STRATEGIST_ROLE) strategyExists(strategyId) {
        strategies[strategyId].status = newStatus;
        emit StrategyUpdated(strategyId, strategies[strategyId].allocation, newStatus);
    }

    // ============================================
    // HARVEST & COMPOUND
    // ============================================

    /**
     * @notice Harvests profit from a strategy
     */
    function harvest(uint256 strategyId) 
        external 
        nonReentrant 
        onlyRole(KEEPER_ROLE)
        notEmergency
        strategyExists(strategyId) 
        strategyActive(strategyId)
        returns (uint256 profit)
    {
        Strategy storage strategy = strategies[strategyId];

        if (block.timestamp < strategy.lastHarvest + minHarvestInterval) {
            revert HarvestTooSoon(strategy.lastHarvest + minHarvestInterval);
        }

        // Get profit from strategy
        uint256 balanceBefore = vault.asset.balanceOf(address(this));
        
        // Call harvest on strategy
        (bool success, bytes memory result) = strategy.strategyAddress.call(
            abi.encodeWithSignature("harvest()")
        );
        
        if (success && result.length > 0) {
            profit = abi.decode(result, (uint256));
        } else {
            // Calculate profit from balance change
            uint256 balanceAfter = vault.asset.balanceOf(address(this));
            if (balanceAfter > balanceBefore) {
                profit = balanceAfter - balanceBefore;
            }
        }

        if (profit > 0) {
            // Calculate fees
            uint256 performanceFee = (profit * vault.performanceFee) / BPS;
            uint256 harvesterReward = (profit * harvestReward) / BPS;
            uint256 netProfit = profit - performanceFee - harvesterReward;

            // Update tracking
            strategy.totalHarvested += profit;
            strategy.harvestCount++;
            strategy.lastHarvest = block.timestamp;
            totalProfit += netProfit;
            pendingFees += performanceFee;

            // Pay harvester reward
            if (harvesterReward > 0) {
                vault.asset.safeTransfer(msg.sender, harvesterReward);
            }

            // Compound if enabled
            if (strategy.autoCompound && netProfit > 0) {
                _depositToStrategy(strategyId, netProfit);
                emit Compounded(strategyId, netProfit);
            } else {
                vault.totalAssets += netProfit;
            }

            // Record harvest
            harvestHistory.push(HarvestRecord({
                timestamp: block.timestamp,
                strategyId: strategyId,
                profitBefore: balanceBefore,
                profitAfter: vault.asset.balanceOf(address(this)),
                feeCollected: performanceFee,
                harvester: msg.sender,
                gasUsed: 0 // Would need to track gas
            }));

            emit Harvested(strategyId, profit, performanceFee, msg.sender);
        }
    }

    /**
     * @notice Harvests all active strategies
     */
    function harvestAll() external nonReentrant onlyRole(KEEPER_ROLE) notEmergency {
        for (uint256 i = 0; i < activeStrategies.length; i++) {
            uint256 strategyId = activeStrategies[i];
            Strategy storage strategy = strategies[strategyId];

            if (strategy.status != StrategyStatus.Active) continue;
            if (!strategy.autoHarvest) continue;
            if (block.timestamp < strategy.lastHarvest + minHarvestInterval) continue;

            // Harvest
            try this.harvest(strategyId) {} catch {}
        }
    }

    // ============================================
    // REBALANCING
    // ============================================

    /**
     * @notice Rebalances strategies according to allocations
     */
    function rebalance() 
        external 
        nonReentrant 
        onlyRole(STRATEGIST_ROLE) 
        notEmergency 
    {
        uint256 totalValue = _getTotalValue();
        if (totalValue == 0) return;

        uint256[] memory targetAmounts = new uint256[](activeStrategies.length);
        uint256[] memory currentAmounts = new uint256[](activeStrategies.length);
        int256[] memory deltas = new int256[](activeStrategies.length);

        // Calculate targets and deltas
        for (uint256 i = 0; i < activeStrategies.length; i++) {
            uint256 strategyId = activeStrategies[i];
            Strategy storage strategy = strategies[strategyId];

            if (strategy.status != StrategyStatus.Active) continue;

            targetAmounts[i] = (totalValue * strategy.allocation) / BPS;
            currentAmounts[i] = strategy.currentBalance;
            deltas[i] = int256(targetAmounts[i]) - int256(currentAmounts[i]);
        }

        // First pass: withdrawals
        for (uint256 i = 0; i < activeStrategies.length; i++) {
            if (deltas[i] < 0) {
                uint256 withdrawAmount = uint256(-deltas[i]);
                _withdrawFromStrategy(activeStrategies[i], withdrawAmount);
            }
        }

        // Second pass: deposits
        for (uint256 i = 0; i < activeStrategies.length; i++) {
            if (deltas[i] > 0) {
                uint256 depositAmount = uint256(deltas[i]);
                uint256 available = vault.asset.balanceOf(address(this));
                if (depositAmount > available) {
                    depositAmount = available;
                }
                if (depositAmount > 0) {
                    _depositToStrategy(activeStrategies[i], depositAmount);
                }
            }
        }

        emit Rebalanced(new uint256[](0), new uint256[](0), new uint256[](0));
    }

    /**
     * @notice Manual rebalance between two strategies
     */
    function rebalanceBetween(
        uint256 fromStrategyId,
        uint256 toStrategyId,
        uint256 amount
    ) 
        external 
        nonReentrant 
        onlyRole(STRATEGIST_ROLE)
        notEmergency
        strategyExists(fromStrategyId)
        strategyExists(toStrategyId)
    {
        if (amount == 0) revert ZeroAmount();

        // Withdraw from source
        _withdrawFromStrategy(fromStrategyId, amount);

        // Deposit to target
        _depositToStrategy(toStrategyId, amount);
    }

    // ============================================
    // INTERNAL FUNCTIONS
    // ============================================

    /**
     * @notice Deploys assets to strategies based on allocations
     */
    function _deployToStrategies(uint256 amount) internal {
        if (activeStrategyCount == 0) return;

        uint256 totalAllocation = _getTotalAllocation();
        if (totalAllocation == 0) return;

        for (uint256 i = 0; i < activeStrategies.length; i++) {
            uint256 strategyId = activeStrategies[i];
            Strategy storage strategy = strategies[strategyId];

            if (strategy.status != StrategyStatus.Active) continue;
            if (strategy.allocation == 0) continue;

            uint256 deployAmount = (amount * strategy.allocation) / totalAllocation;
            if (deployAmount > 0) {
                _depositToStrategy(strategyId, deployAmount);
            }
        }
    }

    /**
     * @notice Withdraws from strategies
     */
    function _withdrawFromStrategies(uint256 amount) internal {
        uint256 remaining = amount;

        // First try to withdraw from strategies with above-target balance
        for (uint256 i = 0; i < activeStrategies.length && remaining > 0; i++) {
            uint256 strategyId = activeStrategies[i];
            Strategy storage strategy = strategies[strategyId];

            if (strategy.currentBalance == 0) continue;

            uint256 withdrawAmount = remaining > strategy.currentBalance 
                ? strategy.currentBalance 
                : remaining;
            
            _withdrawFromStrategy(strategyId, withdrawAmount);
            remaining -= withdrawAmount;
        }
    }

    /**
     * @notice Deposits to a specific strategy
     */
    function _depositToStrategy(uint256 strategyId, uint256 amount) internal {
        Strategy storage strategy = strategies[strategyId];

        vault.asset.approve(strategy.strategyAddress, amount);
        
        (bool success,) = strategy.strategyAddress.call(
            abi.encodeWithSignature("deposit(uint256)", amount)
        );

        if (success) {
            strategy.totalDeposited += amount;
            strategy.currentBalance += amount;
        }
    }

    /**
     * @notice Withdraws from a specific strategy
     */
    function _withdrawFromStrategy(uint256 strategyId, uint256 amount) internal {
        Strategy storage strategy = strategies[strategyId];

        (bool success,) = strategy.strategyAddress.call(
            abi.encodeWithSignature("withdraw(uint256)", amount)
        );

        if (success) {
            strategy.totalWithdrawn += amount;
            strategy.currentBalance = strategy.currentBalance > amount 
                ? strategy.currentBalance - amount 
                : 0;
        }
    }

    /**
     * @notice Gets total value across all strategies
     */
    function _getTotalValue() internal view returns (uint256 total) {
        total = vault.asset.balanceOf(address(this));

        for (uint256 i = 0; i < activeStrategies.length; i++) {
            total += strategies[activeStrategies[i]].currentBalance;
        }
    }

    /**
     * @notice Gets total allocation
     */
    function _getTotalAllocation() internal view returns (uint256 total) {
        for (uint256 i = 0; i < activeStrategies.length; i++) {
            Strategy storage strategy = strategies[activeStrategies[i]];
            if (strategy.status == StrategyStatus.Active) {
                total += strategy.allocation;
            }
        }
    }

    /**
     * @notice Converts assets to shares
     */
    function _convertToShares(uint256 assets) internal view returns (uint256) {
        if (vault.totalShares == 0 || vault.totalAssets == 0) {
            return assets;
        }
        return (assets * vault.totalShares) / vault.totalAssets;
    }

    /**
     * @notice Converts shares to assets
     */
    function _convertToAssets(uint256 shares) internal view returns (uint256) {
        if (vault.totalShares == 0) {
            return shares;
        }
        return (shares * vault.totalAssets) / vault.totalShares;
    }

    // ============================================
    // FEE COLLECTION
    // ============================================

    /**
     * @notice Collects management fees
     */
    function collectManagementFees() external onlyRole(STRATEGIST_ROLE) {
        uint256 timeSinceLastCollection = block.timestamp - vault.lastFeeCollection;
        uint256 managementFee = (vault.totalAssets * vault.managementFee * timeSinceLastCollection) 
            / (BPS * 365 days);

        pendingFees += managementFee;
        vault.lastFeeCollection = block.timestamp;
    }

    /**
     * @notice Withdraws collected fees
     */
    function withdrawFees() external onlyRole(DEFAULT_ADMIN_ROLE) {
        uint256 fees = pendingFees;
        pendingFees = 0;
        totalFeesCollected += fees;

        vault.asset.safeTransfer(treasury, fees);

        emit FeesCollected(0, fees, treasury);
    }

    // ============================================
    // EMERGENCY FUNCTIONS
    // ============================================

    /**
     * @notice Activates emergency mode
     */
    function activateEmergencyMode() external onlyRole(GUARDIAN_ROLE) {
        emergencyMode = true;
        vault.status = VaultStatus.Emergency;
        
        // Withdraw from all strategies
        for (uint256 i = 0; i < activeStrategies.length; i++) {
            uint256 strategyId = activeStrategies[i];
            Strategy storage strategy = strategies[strategyId];
            
            if (strategy.currentBalance > 0) {
                strategy.status = StrategyStatus.Emergency;
                _withdrawFromStrategy(strategyId, strategy.currentBalance);
                
                emit EmergencyWithdrawn(strategyId, strategy.currentBalance);
            }
        }

        emit EmergencyModeActivated(msg.sender);
    }

    /**
     * @notice Deactivates emergency mode
     */
    function deactivateEmergencyMode() external onlyRole(DEFAULT_ADMIN_ROLE) {
        emergencyMode = false;
        vault.status = VaultStatus.Open;

        emit EmergencyModeDeactivated(msg.sender);
    }

    /**
     * @notice Emergency withdraw from specific strategy
     */
    function emergencyWithdrawFromStrategy(uint256 strategyId) 
        external 
        onlyRole(GUARDIAN_ROLE) 
        strategyExists(strategyId) 
    {
        Strategy storage strategy = strategies[strategyId];
        uint256 amount = strategy.currentBalance;

        strategy.status = StrategyStatus.Emergency;
        _withdrawFromStrategy(strategyId, amount);

        emit EmergencyWithdrawn(strategyId, amount);
    }

    // ============================================
    // ADMIN FUNCTIONS
    // ============================================

    /**
     * @notice Updates vault fees
     */
    function updateFees(
        uint256 performanceFee,
        uint256 managementFee,
        uint256 withdrawalFee
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (performanceFee > MAX_PERFORMANCE_FEE) revert InvalidFee(performanceFee, MAX_PERFORMANCE_FEE);
        if (managementFee > MAX_MANAGEMENT_FEE) revert InvalidFee(managementFee, MAX_MANAGEMENT_FEE);
        if (withdrawalFee > MAX_WITHDRAWAL_FEE) revert InvalidFee(withdrawalFee, MAX_WITHDRAWAL_FEE);

        vault.performanceFee = performanceFee;
        vault.managementFee = managementFee;
        vault.withdrawalFee = withdrawalFee;
    }

    /**
     * @notice Updates harvest reward
     */
    function updateHarvestReward(uint256 newReward) external onlyRole(DEFAULT_ADMIN_ROLE) {
        require(newReward <= 500, "Max 5%"); // 5% max
        harvestReward = newReward;
    }

    /**
     * @notice Updates deposit limit
     */
    function updateDepositLimit(uint256 newLimit) external onlyRole(DEFAULT_ADMIN_ROLE) {
        vault.depositLimit = newLimit;
    }

    /**
     * @notice Updates treasury
     */
    function updateTreasury(address newTreasury) external onlyRole(DEFAULT_ADMIN_ROLE) {
        treasury = newTreasury;
    }

    /**
     * @notice Pauses vault
     */
    function pause() external onlyRole(GUARDIAN_ROLE) {
        _pause();
    }

    /**
     * @notice Unpauses vault
     */
    function unpause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        _unpause();
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Gets total assets under management
     */
    function totalAssets() external view returns (uint256) {
        return _getTotalValue();
    }

    /**
     * @notice Converts assets to shares (view)
     */
    function convertToShares(uint256 assets) external view returns (uint256) {
        return _convertToShares(assets);
    }

    /**
     * @notice Converts shares to assets (view)
     */
    function convertToAssets(uint256 shares) external view returns (uint256) {
        return _convertToAssets(shares);
    }

    /**
     * @notice Gets user balance
     */
    function balanceOf(address user) external view returns (uint256) {
        return userDeposits[user].shares;
    }

    /**
     * @notice Gets all active strategy IDs
     */
    function getActiveStrategies() external view returns (uint256[] memory) {
        return activeStrategies;
    }

    /**
     * @notice Gets strategy details
     */
    function getStrategy(uint256 strategyId) external view returns (Strategy memory) {
        return strategies[strategyId];
    }

    /**
     * @notice Gets harvest history length
     */
    function getHarvestHistoryLength() external view returns (uint256) {
        return harvestHistory.length;
    }

    /**
     * @notice Gets harvest record
     */
    function getHarvestRecord(uint256 index) external view returns (HarvestRecord memory) {
        return harvestHistory[index];
    }

    /**
     * @notice Gets APY estimate
     */
    function getAPY() external view returns (uint256) {
        if (harvestHistory.length == 0) return 0;

        // Calculate average profit from last 10 harvests
        uint256 count = harvestHistory.length > 10 ? 10 : harvestHistory.length;
        uint256 totalHarvested;
        uint256 timeSpan;

        for (uint256 i = harvestHistory.length - count; i < harvestHistory.length; i++) {
            totalHarvested += harvestHistory[i].profitAfter - harvestHistory[i].profitBefore;
        }

        if (count > 1) {
            timeSpan = harvestHistory[harvestHistory.length - 1].timestamp 
                - harvestHistory[harvestHistory.length - count].timestamp;
        } else {
            timeSpan = 1 days;
        }

        if (timeSpan == 0 || vault.totalAssets == 0) return 0;

        // APY = (profit / timeSpan) * 365 days / totalAssets
        return (totalHarvested * 365 days * BPS) / (timeSpan * vault.totalAssets);
    }
}
