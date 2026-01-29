// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";

/**
 * @title FlashLoanProvider
 * @notice Production-grade flash loan facility with multi-asset support
 * @dev Implements EIP-3156 compatible flash loans with extensions
 * 
 * Features:
 * - Single and batch flash loans
 * - Configurable per-asset fees
 * - Flash loan receiver callback validation
 * - Liquidity provider rewards
 * - Usage statistics and analytics
 * - Rate limiting per address
 * - Whitelist/blacklist support
 * - Emergency pause functionality
 */
contract FlashLoanProvider is AccessControl, ReentrancyGuard {
    using SafeERC20 for IERC20;

    // ============================================
    // ROLES
    // ============================================

    bytes32 public constant OPERATOR_ROLE = keccak256("OPERATOR_ROLE");
    bytes32 public constant FEE_MANAGER_ROLE = keccak256("FEE_MANAGER_ROLE");
    bytes32 public constant LIQUIDITY_MANAGER_ROLE = keccak256("LIQUIDITY_MANAGER_ROLE");

    // ============================================
    // TYPES
    // ============================================

    /// @notice Asset configuration
    struct AssetConfig {
        bool enabled;
        uint256 feeBps;             // Fee in basis points
        uint256 maxLoanAmount;      // Maximum single loan
        uint256 totalLiquidity;     // Total deposited liquidity
        uint256 availableLiquidity; // Currently available
        uint256 totalFeesCollected;
        uint256 totalLoansIssued;
        uint256 loanCount;
    }

    /// @notice Liquidity provider info
    struct LiquidityProvider {
        uint256 depositedAmount;
        uint256 lastRewardClaim;
        uint256 pendingRewards;
        uint256 rewardDebt;
    }

    /// @notice Flash loan request
    struct FlashLoanRequest {
        address token;
        uint256 amount;
    }

    /// @notice Rate limit info
    struct RateLimitInfo {
        uint256 lastLoanTime;
        uint256 loanCount;
        uint256 totalBorrowed;
    }

    /// @notice Flash loan execution result
    struct FlashLoanResult {
        bool success;
        uint256 amountBorrowed;
        uint256 fee;
        bytes returnData;
    }

    // ============================================
    // CONSTANTS
    // ============================================

    uint256 public constant MAX_FEE_BPS = 1000;      // 10% max fee
    uint256 public constant DEFAULT_FEE_BPS = 9;     // 0.09% default (like Aave)
    bytes32 public constant CALLBACK_SUCCESS = keccak256("ERC3156FlashBorrower.onFlashLoan");
    uint256 public constant PRECISION = 1e18;

    // ============================================
    // STATE
    // ============================================

    /// @notice Asset configurations
    mapping(address => AssetConfig) public assetConfigs;
    address[] public supportedAssets;

    /// @notice Liquidity providers: asset => provider => info
    mapping(address => mapping(address => LiquidityProvider)) public liquidityProviders;

    /// @notice Accumulated rewards per share (scaled by PRECISION)
    mapping(address => uint256) public accRewardPerShare;

    /// @notice Rate limiting: borrower => asset => info
    mapping(address => mapping(address => RateLimitInfo)) public rateLimits;

    /// @notice Whitelist (if empty, anyone can borrow)
    mapping(address => bool) public whitelist;
    bool public whitelistEnabled;

    /// @notice Blacklist
    mapping(address => bool) public blacklist;

    /// @notice Fee recipient
    address public feeRecipient;

    /// @notice Protocol fee share (basis points of collected fees)
    uint256 public protocolFeeShareBps = 1000; // 10% of fees go to protocol

    /// @notice Pause state
    bool public paused;

    /// @notice Rate limit settings
    uint256 public rateLimitPeriod = 1 hours;
    uint256 public maxLoansPerPeriod = 10;
    uint256 public maxBorrowPerPeriod; // 0 = unlimited

    // ============================================
    // EVENTS
    // ============================================

    event FlashLoan(
        address indexed borrower,
        address indexed token,
        uint256 amount,
        uint256 fee,
        address receiver
    );

    event BatchFlashLoan(
        address indexed borrower,
        uint256 numAssets,
        uint256 totalFees
    );

    event LiquidityDeposited(
        address indexed provider,
        address indexed token,
        uint256 amount
    );

    event LiquidityWithdrawn(
        address indexed provider,
        address indexed token,
        uint256 amount
    );

    event RewardsClaimed(
        address indexed provider,
        address indexed token,
        uint256 amount
    );

    event AssetConfigured(
        address indexed token,
        bool enabled,
        uint256 feeBps,
        uint256 maxLoanAmount
    );

    event FeeCollected(
        address indexed token,
        uint256 amount,
        uint256 protocolShare
    );

    // ============================================
    // ERRORS
    // ============================================

    error Paused();
    error AssetNotSupported();
    error AssetDisabled();
    error InsufficientLiquidity();
    error ExceedsMaxLoanAmount();
    error InvalidCallback();
    error LoanNotRepaid();
    error RateLimitExceeded();
    error Blacklisted();
    error NotWhitelisted();
    error InvalidFee();
    error NothingToWithdraw();
    error NoRewardsToClaim();
    error ArrayLengthMismatch();

    // ============================================
    // MODIFIERS
    // ============================================

    modifier whenNotPaused() {
        if (paused) revert Paused();
        _;
    }

    modifier notBlacklisted(address account) {
        if (blacklist[account]) revert Blacklisted();
        _;
    }

    modifier onlyWhitelisted(address account) {
        if (whitelistEnabled && !whitelist[account]) revert NotWhitelisted();
        _;
    }

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(address _feeRecipient) {
        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(OPERATOR_ROLE, msg.sender);
        _grantRole(FEE_MANAGER_ROLE, msg.sender);
        _grantRole(LIQUIDITY_MANAGER_ROLE, msg.sender);

        feeRecipient = _feeRecipient;
    }

    // ============================================
    // FLASH LOAN FUNCTIONS
    // ============================================

    /**
     * @notice Execute a flash loan (EIP-3156 compatible)
     * @param receiver The receiver contract
     * @param token The token to borrow
     * @param amount The amount to borrow
     * @param data Arbitrary data to pass to receiver
     */
    function flashLoan(
        address receiver,
        address token,
        uint256 amount,
        bytes calldata data
    ) external nonReentrant whenNotPaused notBlacklisted(msg.sender) onlyWhitelisted(msg.sender) returns (bool) {
        AssetConfig storage config = assetConfigs[token];

        if (!config.enabled) revert AssetDisabled();
        if (amount > config.availableLiquidity) revert InsufficientLiquidity();
        if (config.maxLoanAmount > 0 && amount > config.maxLoanAmount) revert ExceedsMaxLoanAmount();

        // Check rate limits
        _checkRateLimit(msg.sender, token, amount);

        uint256 fee = _calculateFee(token, amount);
        uint256 balanceBefore = IERC20(token).balanceOf(address(this));

        // Update state before external call
        config.availableLiquidity -= amount;

        // Transfer tokens to receiver
        IERC20(token).safeTransfer(receiver, amount);

        // Call receiver callback
        bytes32 callbackResult = IFlashLoanReceiver(receiver).onFlashLoan(
            msg.sender,
            token,
            amount,
            fee,
            data
        );

        if (callbackResult != CALLBACK_SUCCESS) revert InvalidCallback();

        // Verify repayment
        uint256 balanceAfter = IERC20(token).balanceOf(address(this));
        if (balanceAfter < balanceBefore + fee) revert LoanNotRepaid();

        // Update state
        config.availableLiquidity += amount + fee;
        config.totalLiquidity += fee;
        config.totalFeesCollected += fee;
        config.totalLoansIssued += amount;
        config.loanCount++;

        // Distribute fees
        _distributeFees(token, fee);

        // Update rate limit tracking
        _updateRateLimit(msg.sender, token, amount);

        emit FlashLoan(msg.sender, token, amount, fee, receiver);

        return true;
    }

    /**
     * @notice Execute batch flash loan (multiple assets)
     */
    function batchFlashLoan(
        address receiver,
        address[] calldata tokens,
        uint256[] calldata amounts,
        bytes calldata data
    ) external nonReentrant whenNotPaused notBlacklisted(msg.sender) onlyWhitelisted(msg.sender) returns (bool) {
        if (tokens.length != amounts.length) revert ArrayLengthMismatch();

        uint256[] memory fees = new uint256[](tokens.length);
        uint256[] memory balancesBefore = new uint256[](tokens.length);
        uint256 totalFees = 0;

        // Validate and transfer all tokens
        for (uint256 i = 0; i < tokens.length; i++) {
            AssetConfig storage config = assetConfigs[tokens[i]];

            if (!config.enabled) revert AssetDisabled();
            if (amounts[i] > config.availableLiquidity) revert InsufficientLiquidity();

            fees[i] = _calculateFee(tokens[i], amounts[i]);
            totalFees += fees[i];
            balancesBefore[i] = IERC20(tokens[i]).balanceOf(address(this));

            config.availableLiquidity -= amounts[i];
            IERC20(tokens[i]).safeTransfer(receiver, amounts[i]);
        }

        // Call receiver callback
        bytes32 callbackResult = IFlashLoanReceiver(receiver).onBatchFlashLoan(
            msg.sender,
            tokens,
            amounts,
            fees,
            data
        );

        if (callbackResult != CALLBACK_SUCCESS) revert InvalidCallback();

        // Verify all repayments
        for (uint256 i = 0; i < tokens.length; i++) {
            uint256 balanceAfter = IERC20(tokens[i]).balanceOf(address(this));
            if (balanceAfter < balancesBefore[i] + fees[i]) revert LoanNotRepaid();

            AssetConfig storage config = assetConfigs[tokens[i]];
            config.availableLiquidity += amounts[i] + fees[i];
            config.totalLiquidity += fees[i];
            config.totalFeesCollected += fees[i];
            config.totalLoansIssued += amounts[i];
            config.loanCount++;

            _distributeFees(tokens[i], fees[i]);
        }

        emit BatchFlashLoan(msg.sender, tokens.length, totalFees);

        return true;
    }

    /**
     * @notice Calculate flash loan fee
     */
    function _calculateFee(address token, uint256 amount) internal view returns (uint256) {
        AssetConfig memory config = assetConfigs[token];
        return (amount * config.feeBps) / 10000;
    }

    /**
     * @notice Distribute fees to LPs and protocol
     */
    function _distributeFees(address token, uint256 fee) internal {
        uint256 protocolShare = (fee * protocolFeeShareBps) / 10000;
        uint256 lpShare = fee - protocolShare;

        AssetConfig memory config = assetConfigs[token];

        if (config.totalLiquidity > 0 && lpShare > 0) {
            // Add to accumulated rewards per share
            accRewardPerShare[token] += (lpShare * PRECISION) / config.totalLiquidity;
        }

        if (protocolShare > 0) {
            IERC20(token).safeTransfer(feeRecipient, protocolShare);
        }

        emit FeeCollected(token, fee, protocolShare);
    }

    /**
     * @notice Check rate limits
     */
    function _checkRateLimit(address borrower, address token, uint256 amount) internal view {
        RateLimitInfo memory info = rateLimits[borrower][token];

        if (block.timestamp < info.lastLoanTime + rateLimitPeriod) {
            if (info.loanCount >= maxLoansPerPeriod) revert RateLimitExceeded();
            if (maxBorrowPerPeriod > 0 && info.totalBorrowed + amount > maxBorrowPerPeriod) {
                revert RateLimitExceeded();
            }
        }
    }

    /**
     * @notice Update rate limit tracking
     */
    function _updateRateLimit(address borrower, address token, uint256 amount) internal {
        RateLimitInfo storage info = rateLimits[borrower][token];

        if (block.timestamp >= info.lastLoanTime + rateLimitPeriod) {
            // Reset for new period
            info.loanCount = 1;
            info.totalBorrowed = amount;
        } else {
            info.loanCount++;
            info.totalBorrowed += amount;
        }

        info.lastLoanTime = block.timestamp;
    }

    // ============================================
    // LIQUIDITY PROVIDER FUNCTIONS
    // ============================================

    /**
     * @notice Deposit liquidity for flash loans
     */
    function deposit(address token, uint256 amount) external nonReentrant whenNotPaused {
        AssetConfig storage config = assetConfigs[token];
        if (!config.enabled) revert AssetDisabled();

        LiquidityProvider storage lp = liquidityProviders[token][msg.sender];

        // Update rewards before changing deposit
        _updateRewards(token, msg.sender);

        IERC20(token).safeTransferFrom(msg.sender, address(this), amount);

        lp.depositedAmount += amount;
        config.totalLiquidity += amount;
        config.availableLiquidity += amount;

        // Update reward debt
        lp.rewardDebt = (lp.depositedAmount * accRewardPerShare[token]) / PRECISION;

        emit LiquidityDeposited(msg.sender, token, amount);
    }

    /**
     * @notice Withdraw liquidity
     */
    function withdraw(address token, uint256 amount) external nonReentrant {
        AssetConfig storage config = assetConfigs[token];
        LiquidityProvider storage lp = liquidityProviders[token][msg.sender];

        if (lp.depositedAmount < amount) revert NothingToWithdraw();
        if (config.availableLiquidity < amount) revert InsufficientLiquidity();

        // Claim pending rewards first
        _updateRewards(token, msg.sender);
        _claimRewards(token, msg.sender);

        lp.depositedAmount -= amount;
        config.totalLiquidity -= amount;
        config.availableLiquidity -= amount;

        // Update reward debt
        lp.rewardDebt = (lp.depositedAmount * accRewardPerShare[token]) / PRECISION;

        IERC20(token).safeTransfer(msg.sender, amount);

        emit LiquidityWithdrawn(msg.sender, token, amount);
    }

    /**
     * @notice Claim accumulated rewards
     */
    function claimRewards(address token) external nonReentrant {
        _updateRewards(token, msg.sender);
        _claimRewards(token, msg.sender);
    }

    /**
     * @notice Update pending rewards for LP
     */
    function _updateRewards(address token, address provider) internal {
        LiquidityProvider storage lp = liquidityProviders[token][provider];

        if (lp.depositedAmount > 0) {
            uint256 accumulatedReward = (lp.depositedAmount * accRewardPerShare[token]) / PRECISION;
            lp.pendingRewards += accumulatedReward - lp.rewardDebt;
        }
    }

    /**
     * @notice Claim rewards internal
     */
    function _claimRewards(address token, address provider) internal {
        LiquidityProvider storage lp = liquidityProviders[token][provider];

        uint256 rewards = lp.pendingRewards;
        if (rewards == 0) return;

        lp.pendingRewards = 0;
        lp.lastRewardClaim = block.timestamp;
        lp.rewardDebt = (lp.depositedAmount * accRewardPerShare[token]) / PRECISION;

        // Rewards are paid in the same token
        IERC20(token).safeTransfer(provider, rewards);

        emit RewardsClaimed(provider, token, rewards);
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Get maximum flash loan amount for token
     */
    function maxFlashLoan(address token) external view returns (uint256) {
        AssetConfig memory config = assetConfigs[token];
        if (!config.enabled) return 0;

        uint256 maxFromLiquidity = config.availableLiquidity;
        uint256 maxFromConfig = config.maxLoanAmount;

        if (maxFromConfig == 0) return maxFromLiquidity;
        return maxFromLiquidity < maxFromConfig ? maxFromLiquidity : maxFromConfig;
    }

    /**
     * @notice Get flash loan fee for amount
     */
    function flashFee(address token, uint256 amount) external view returns (uint256) {
        if (!assetConfigs[token].enabled) return 0;
        return _calculateFee(token, amount);
    }

    /**
     * @notice Get pending rewards for LP
     */
    function pendingRewards(address token, address provider) external view returns (uint256) {
        LiquidityProvider memory lp = liquidityProviders[token][provider];

        if (lp.depositedAmount == 0) return lp.pendingRewards;

        uint256 accumulatedReward = (lp.depositedAmount * accRewardPerShare[token]) / PRECISION;
        return lp.pendingRewards + accumulatedReward - lp.rewardDebt;
    }

    /**
     * @notice Get LP info
     */
    function getLPInfo(address token, address provider) external view returns (
        uint256 deposited,
        uint256 pending,
        uint256 lastClaim
    ) {
        LiquidityProvider memory lp = liquidityProviders[token][provider];
        return (lp.depositedAmount, lp.pendingRewards, lp.lastRewardClaim);
    }

    /**
     * @notice Get asset config
     */
    function getAssetConfig(address token) external view returns (AssetConfig memory) {
        return assetConfigs[token];
    }

    /**
     * @notice Get all supported assets
     */
    function getSupportedAssets() external view returns (address[] memory) {
        return supportedAssets;
    }

    // ============================================
    // ADMIN FUNCTIONS
    // ============================================

    /**
     * @notice Configure an asset for flash loans
     */
    function configureAsset(
        address token,
        bool enabled,
        uint256 feeBps,
        uint256 maxLoanAmount
    ) external onlyRole(OPERATOR_ROLE) {
        if (feeBps > MAX_FEE_BPS) revert InvalidFee();

        AssetConfig storage config = assetConfigs[token];

        if (!config.enabled && enabled) {
            supportedAssets.push(token);
        }

        config.enabled = enabled;
        config.feeBps = feeBps > 0 ? feeBps : DEFAULT_FEE_BPS;
        config.maxLoanAmount = maxLoanAmount;

        emit AssetConfigured(token, enabled, config.feeBps, maxLoanAmount);
    }

    function setFeeRecipient(address _feeRecipient) external onlyRole(FEE_MANAGER_ROLE) {
        feeRecipient = _feeRecipient;
    }

    function setProtocolFeeShare(uint256 _sharesBps) external onlyRole(FEE_MANAGER_ROLE) {
        require(_sharesBps <= 5000, "Max 50%");
        protocolFeeShareBps = _sharesBps;
    }

    function setRateLimitSettings(
        uint256 _period,
        uint256 _maxLoans,
        uint256 _maxBorrow
    ) external onlyRole(OPERATOR_ROLE) {
        rateLimitPeriod = _period;
        maxLoansPerPeriod = _maxLoans;
        maxBorrowPerPeriod = _maxBorrow;
    }

    function setWhitelistEnabled(bool _enabled) external onlyRole(OPERATOR_ROLE) {
        whitelistEnabled = _enabled;
    }

    function updateWhitelist(address account, bool whitelisted) external onlyRole(OPERATOR_ROLE) {
        whitelist[account] = whitelisted;
    }

    function updateBlacklist(address account, bool blacklisted) external onlyRole(OPERATOR_ROLE) {
        blacklist[account] = blacklisted;
    }

    function pause() external onlyRole(OPERATOR_ROLE) {
        paused = true;
    }

    function unpause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        paused = false;
    }
}

/**
 * @title IFlashLoanReceiver
 * @notice Interface for flash loan receiver contracts
 */
interface IFlashLoanReceiver {
    function onFlashLoan(
        address initiator,
        address token,
        uint256 amount,
        uint256 fee,
        bytes calldata data
    ) external returns (bytes32);

    function onBatchFlashLoan(
        address initiator,
        address[] calldata tokens,
        uint256[] calldata amounts,
        uint256[] calldata fees,
        bytes calldata data
    ) external returns (bytes32);
}
