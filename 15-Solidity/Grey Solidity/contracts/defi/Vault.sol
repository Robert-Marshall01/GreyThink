// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/token/ERC20/extensions/IERC20Permit.sol";
import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/Pausable.sol";

/**
 * @title Vault
 * @author Grey Solidity Project
 * @notice Multi-strategy vault with deposits, permits, and shares
 * @dev ERC-4626 style vault with yield generation
 */
contract Vault is AccessControl, ReentrancyGuard, Pausable {
    using SafeERC20 for IERC20;

    /// @notice Role for strategy management
    bytes32 public constant STRATEGIST_ROLE = keccak256("STRATEGIST_ROLE");

    /// @notice Deposit info structure
    struct DepositInfo {
        uint256 shares;
        uint256 depositTime;
        uint256 lastClaimTime;
    }

    /// @notice The underlying asset
    IERC20 public immutable asset;

    /// @notice Total shares issued
    uint256 public totalShares;

    /// @notice Total assets under management
    uint256 public totalAssets;

    /// @notice Performance fee in basis points
    uint256 public performanceFee;

    /// @notice Management fee in basis points (annual)
    uint256 public managementFee;

    /// @notice Deposit cap (0 = unlimited)
    uint256 public depositCap;

    /// @notice Minimum deposit
    uint256 public minDeposit;

    /// @notice Withdrawal delay (0 = instant)
    uint256 public withdrawalDelay;

    /// @notice Mapping of user deposits
    mapping(address => DepositInfo) public deposits;

    /// @notice Mapping of pending withdrawals
    mapping(address => uint256) public pendingWithdrawals;

    /// @notice Mapping of withdrawal unlock time
    mapping(address => uint256) public withdrawalUnlockTime;

    /// @notice Fee recipient
    address public feeRecipient;

    /// @notice Last fee collection time
    uint256 public lastFeeCollection;

    /// @notice Accumulated fees
    uint256 public accumulatedFees;

    // ============ Events ============

    event Deposited(address indexed user, uint256 assets, uint256 shares);
    event WithdrawalRequested(address indexed user, uint256 shares, uint256 unlockTime);
    event Withdrawn(address indexed user, uint256 assets, uint256 shares);
    event YieldHarvested(uint256 amount);
    event FeesCollected(address indexed recipient, uint256 amount);
    event DepositCapUpdated(uint256 oldCap, uint256 newCap);
    event FeesUpdated(uint256 performanceFee, uint256 managementFee);

    // ============ Errors ============

    error InsufficientBalance(uint256 requested, uint256 available);
    error DepositCapExceeded(uint256 deposit, uint256 cap);
    error BelowMinimumDeposit(uint256 deposit, uint256 min);
    error WithdrawalNotReady(uint256 unlockTime);
    error NoPendingWithdrawal();
    error ZeroAmount();
    error ZeroAddress();
    error NoShares();

    /**
     * @notice Initializes the vault
     * @param asset_ The underlying asset
     * @param feeRecipient_ The fee recipient
     * @param performanceFee_ Performance fee (basis points)
     * @param managementFee_ Management fee (basis points, annual)
     */
    constructor(
        address asset_,
        address feeRecipient_,
        uint256 performanceFee_,
        uint256 managementFee_
    ) {
        asset = IERC20(asset_);
        feeRecipient = feeRecipient_;
        performanceFee = performanceFee_;
        managementFee = managementFee_;
        minDeposit = 1e18;  // 1 token minimum
        lastFeeCollection = block.timestamp;

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(STRATEGIST_ROLE, msg.sender);
    }

    // ============ Deposit Functions ============

    /**
     * @notice Deposits assets into the vault
     * @param assets Amount of assets to deposit
     * @return shares Amount of shares received
     */
    function deposit(uint256 assets) external nonReentrant whenNotPaused returns (uint256 shares) {
        if (assets == 0) revert ZeroAmount();
        if (assets < minDeposit) revert BelowMinimumDeposit(assets, minDeposit);
        if (depositCap > 0 && totalAssets + assets > depositCap) {
            revert DepositCapExceeded(assets, depositCap - totalAssets);
        }

        shares = _convertToShares(assets);
        if (shares == 0) revert NoShares();

        asset.safeTransferFrom(msg.sender, address(this), assets);

        _updateDeposit(msg.sender, shares);
        totalAssets += assets;

        emit Deposited(msg.sender, assets, shares);
    }

    /**
     * @notice Deposits with permit
     * @param assets Amount of assets
     * @param deadline Permit deadline
     * @param v Signature v
     * @param r Signature r
     * @param s Signature s
     * @return shares Amount of shares received
     */
    function depositWithPermit(
        uint256 assets,
        uint256 deadline,
        uint8 v,
        bytes32 r,
        bytes32 s
    ) external nonReentrant whenNotPaused returns (uint256 shares) {
        IERC20Permit(address(asset)).permit(
            msg.sender,
            address(this),
            assets,
            deadline,
            v,
            r,
            s
        );

        if (assets == 0) revert ZeroAmount();
        if (assets < minDeposit) revert BelowMinimumDeposit(assets, minDeposit);
        if (depositCap > 0 && totalAssets + assets > depositCap) {
            revert DepositCapExceeded(assets, depositCap - totalAssets);
        }

        shares = _convertToShares(assets);
        if (shares == 0) revert NoShares();

        asset.safeTransferFrom(msg.sender, address(this), assets);

        _updateDeposit(msg.sender, shares);
        totalAssets += assets;

        emit Deposited(msg.sender, assets, shares);
    }

    // ============ Withdrawal Functions ============

    /**
     * @notice Requests a withdrawal
     * @param shares Amount of shares to withdraw
     */
    function requestWithdrawal(uint256 shares) external nonReentrant whenNotPaused {
        DepositInfo storage info = deposits[msg.sender];
        if (shares == 0) revert ZeroAmount();
        if (shares > info.shares) revert InsufficientBalance(shares, info.shares);

        info.shares -= shares;
        totalShares -= shares;

        pendingWithdrawals[msg.sender] += shares;
        withdrawalUnlockTime[msg.sender] = block.timestamp + withdrawalDelay;

        emit WithdrawalRequested(msg.sender, shares, withdrawalUnlockTime[msg.sender]);
    }

    /**
     * @notice Completes a pending withdrawal
     * @return assets Amount of assets received
     */
    function withdraw() external nonReentrant whenNotPaused returns (uint256 assets) {
        uint256 shares = pendingWithdrawals[msg.sender];
        if (shares == 0) revert NoPendingWithdrawal();
        if (block.timestamp < withdrawalUnlockTime[msg.sender]) {
            revert WithdrawalNotReady(withdrawalUnlockTime[msg.sender]);
        }

        assets = _convertToAssets(shares);
        
        delete pendingWithdrawals[msg.sender];
        delete withdrawalUnlockTime[msg.sender];

        totalAssets -= assets;

        asset.safeTransfer(msg.sender, assets);

        emit Withdrawn(msg.sender, assets, shares);
    }

    /**
     * @notice Instant withdrawal (if delay is 0)
     * @param shares Amount of shares to withdraw
     * @return assets Amount of assets received
     */
    function instantWithdraw(uint256 shares) external nonReentrant whenNotPaused returns (uint256 assets) {
        require(withdrawalDelay == 0, "Vault: delayed withdrawals only");

        DepositInfo storage info = deposits[msg.sender];
        if (shares == 0) revert ZeroAmount();
        if (shares > info.shares) revert InsufficientBalance(shares, info.shares);

        assets = _convertToAssets(shares);

        info.shares -= shares;
        totalShares -= shares;
        totalAssets -= assets;

        asset.safeTransfer(msg.sender, assets);

        emit Withdrawn(msg.sender, assets, shares);
    }

    // ============ View Functions ============

    /**
     * @notice Converts assets to shares
     * @param assets Amount of assets
     * @return shares Amount of shares
     */
    function convertToShares(uint256 assets) external view returns (uint256) {
        return _convertToShares(assets);
    }

    /**
     * @notice Converts shares to assets
     * @param shares Amount of shares
     * @return assets Amount of assets
     */
    function convertToAssets(uint256 shares) external view returns (uint256) {
        return _convertToAssets(shares);
    }

    /**
     * @notice Returns the balance of a user
     * @param user The user address
     * @return shares User's share balance
     * @return assets User's asset value
     */
    function balanceOf(address user) external view returns (uint256 shares, uint256 assets) {
        shares = deposits[user].shares;
        assets = _convertToAssets(shares);
    }

    /**
     * @notice Returns the share price
     * @return The price per share (1e18 = 1:1)
     */
    function sharePrice() external view returns (uint256) {
        if (totalShares == 0) return 1e18;
        return (totalAssets * 1e18) / totalShares;
    }

    /**
     * @notice Returns available deposit capacity
     * @return The remaining capacity
     */
    function availableCapacity() external view returns (uint256) {
        if (depositCap == 0) return type(uint256).max;
        return depositCap > totalAssets ? depositCap - totalAssets : 0;
    }

    // ============ Strategy Functions ============

    /**
     * @notice Reports yield from strategy
     * @param profit The profit amount
     */
    function reportProfit(uint256 profit) external onlyRole(STRATEGIST_ROLE) {
        // Take performance fee
        uint256 fee = (profit * performanceFee) / 10000;
        accumulatedFees += fee;

        totalAssets += (profit - fee);

        emit YieldHarvested(profit);
    }

    /**
     * @notice Reports loss from strategy
     * @param loss The loss amount
     */
    function reportLoss(uint256 loss) external onlyRole(STRATEGIST_ROLE) {
        if (loss > totalAssets) {
            totalAssets = 0;
        } else {
            totalAssets -= loss;
        }
    }

    // ============ Admin Functions ============

    /**
     * @notice Collects accumulated fees
     */
    function collectFees() external onlyRole(DEFAULT_ADMIN_ROLE) nonReentrant {
        // Calculate management fee
        uint256 timePassed = block.timestamp - lastFeeCollection;
        uint256 mgmtFee = (totalAssets * managementFee * timePassed) / (10000 * 365 days);
        
        uint256 totalFees = accumulatedFees + mgmtFee;
        if (totalFees > totalAssets) {
            totalFees = totalAssets;
        }

        accumulatedFees = 0;
        lastFeeCollection = block.timestamp;
        totalAssets -= totalFees;

        asset.safeTransfer(feeRecipient, totalFees);

        emit FeesCollected(feeRecipient, totalFees);
    }

    /**
     * @notice Sets deposit cap
     * @param newCap The new cap
     */
    function setDepositCap(uint256 newCap) external onlyRole(DEFAULT_ADMIN_ROLE) {
        uint256 oldCap = depositCap;
        depositCap = newCap;
        emit DepositCapUpdated(oldCap, newCap);
    }

    /**
     * @notice Sets minimum deposit
     * @param newMin The new minimum
     */
    function setMinDeposit(uint256 newMin) external onlyRole(DEFAULT_ADMIN_ROLE) {
        minDeposit = newMin;
    }

    /**
     * @notice Sets withdrawal delay
     * @param delay The delay in seconds
     */
    function setWithdrawalDelay(uint256 delay) external onlyRole(DEFAULT_ADMIN_ROLE) {
        require(delay <= 7 days, "Vault: delay too long");
        withdrawalDelay = delay;
    }

    /**
     * @notice Updates fees
     * @param newPerformanceFee New performance fee
     * @param newManagementFee New management fee
     */
    function setFees(
        uint256 newPerformanceFee,
        uint256 newManagementFee
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        require(newPerformanceFee <= 3000, "Vault: perf fee too high");  // Max 30%
        require(newManagementFee <= 500, "Vault: mgmt fee too high");    // Max 5%

        performanceFee = newPerformanceFee;
        managementFee = newManagementFee;

        emit FeesUpdated(newPerformanceFee, newManagementFee);
    }

    /**
     * @notice Sets fee recipient
     * @param newRecipient The new recipient
     */
    function setFeeRecipient(address newRecipient) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (newRecipient == address(0)) revert ZeroAddress();
        feeRecipient = newRecipient;
    }

    /**
     * @notice Pauses the vault
     */
    function pause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        _pause();
    }

    /**
     * @notice Unpauses the vault
     */
    function unpause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        _unpause();
    }

    // ============ Internal Functions ============

    function _convertToShares(uint256 assets) private view returns (uint256) {
        if (totalShares == 0 || totalAssets == 0) {
            return assets;
        }
        return (assets * totalShares) / totalAssets;
    }

    function _convertToAssets(uint256 shares) private view returns (uint256) {
        if (totalShares == 0) {
            return shares;
        }
        return (shares * totalAssets) / totalShares;
    }

    function _updateDeposit(address user, uint256 shares) private {
        DepositInfo storage info = deposits[user];
        info.shares += shares;
        info.depositTime = block.timestamp;
        info.lastClaimTime = block.timestamp;
        totalShares += shares;
    }
}
