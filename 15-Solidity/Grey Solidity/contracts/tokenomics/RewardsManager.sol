// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/utils/math/Math.sol";

/**
 * @title RewardsManager
 * @notice Multi-pool rewards distribution with boosting and multipliers
 * @dev Manages staking rewards across multiple pools with advanced features
 *
 * Features:
 * - Multiple reward tokens per pool
 * - Time-locked staking with boost multipliers
 * - NFT boost integration
 * - Referral rewards
 * - Compounding functionality
 */
contract RewardsManager is AccessControl, ReentrancyGuard {
    using SafeERC20 for IERC20;

    // ============ Constants ============

    bytes32 public constant POOL_MANAGER_ROLE = keccak256("POOL_MANAGER_ROLE");
    bytes32 public constant REWARDS_ROLE = keccak256("REWARDS_ROLE");
    
    uint256 public constant PRECISION = 1e18;
    uint256 public constant MAX_BOOST = 3e18; // 3x max boost
    uint256 public constant BASIS_POINTS = 10000;

    // ============ Structs ============

    struct PoolInfo {
        address stakingToken;
        uint256 totalStaked;
        uint256 totalBoostedStaked;
        uint256 depositFee; // Basis points
        uint256 withdrawFee;
        uint256 lockDuration; // Minimum lock
        bool active;
    }

    struct RewardInfo {
        address token;
        uint256 rewardRate; // Per second
        uint256 periodFinish;
        uint256 lastUpdateTime;
        uint256 rewardPerTokenStored;
        uint256 totalDistributed;
    }

    struct UserInfo {
        uint256 amount;
        uint256 boostedAmount;
        uint256 depositTime;
        uint256 unlockTime;
        uint256 lockDuration;
        mapping(address => uint256) rewardPerTokenPaid;
        mapping(address => uint256) rewards;
        address referrer;
        uint256 totalReferred;
    }

    struct LockBoost {
        uint256 duration;
        uint256 multiplier; // 1e18 = 1x
    }

    // ============ State Variables ============

    /// @notice Pool configurations
    mapping(uint256 => PoolInfo) public pools;
    uint256 public poolCount;

    /// @notice Reward tokens per pool
    mapping(uint256 => address[]) public poolRewardTokens;
    mapping(uint256 => mapping(address => RewardInfo)) public rewardInfo;
    mapping(uint256 => mapping(address => bool)) public isRewardToken;

    /// @notice User stakes per pool
    mapping(uint256 => mapping(address => UserInfo)) internal userInfo;

    /// @notice Lock boost tiers
    LockBoost[] public lockBoosts;

    /// @notice NFT boost contract
    address public nftBoostContract;
    mapping(address => uint256) public nftBoostMultiplier; // Per NFT collection

    /// @notice Referral configuration
    uint256 public referralRewardRate = 500; // 5% of referee rewards
    mapping(address => uint256) public referralRewards;
    mapping(address => address[]) public referrals;

    /// @notice Fee collector
    address public feeCollector;

    /// @notice Pause status
    bool public paused;

    // ============ Events ============

    event PoolCreated(uint256 indexed poolId, address stakingToken);
    event PoolUpdated(uint256 indexed poolId);
    event RewardTokenAdded(uint256 indexed poolId, address token, uint256 rate, uint256 duration);
    event Deposited(uint256 indexed poolId, address indexed user, uint256 amount, uint256 lockDuration);
    event Withdrawn(uint256 indexed poolId, address indexed user, uint256 amount);
    event RewardClaimed(uint256 indexed poolId, address indexed user, address token, uint256 amount);
    event RewardCompounded(uint256 indexed poolId, address indexed user, address token, uint256 amount);
    event ReferralSet(address indexed user, address indexed referrer);
    event ReferralRewardPaid(address indexed referrer, address indexed referee, uint256 amount);
    event LockExtended(uint256 indexed poolId, address indexed user, uint256 newUnlockTime);
    event EmergencyWithdraw(uint256 indexed poolId, address indexed user, uint256 amount);

    // ============ Errors ============

    error PoolDoesNotExist();
    error PoolNotActive();
    error InsufficientBalance();
    error LockNotExpired();
    error InvalidDuration();
    error AlreadyHasReferrer();
    error CannotReferSelf();
    error ContractPaused();
    error ZeroAmount();
    error InvalidFee();
    error NotNFTContract();

    // ============ Modifiers ============

    modifier whenNotPaused() {
        if (paused) revert ContractPaused();
        _;
    }

    modifier poolExists(uint256 poolId) {
        if (poolId >= poolCount) revert PoolDoesNotExist();
        _;
    }

    modifier updateReward(uint256 poolId, address account) {
        address[] memory tokens = poolRewardTokens[poolId];
        for (uint256 i = 0; i < tokens.length; i++) {
            RewardInfo storage info = rewardInfo[poolId][tokens[i]];
            info.rewardPerTokenStored = rewardPerToken(poolId, tokens[i]);
            info.lastUpdateTime = lastTimeRewardApplicable(poolId, tokens[i]);
            
            if (account != address(0)) {
                UserInfo storage user = userInfo[poolId][account];
                user.rewards[tokens[i]] = earned(poolId, account, tokens[i]);
                user.rewardPerTokenPaid[tokens[i]] = info.rewardPerTokenStored;
            }
        }
        _;
    }

    // ============ Constructor ============

    constructor(address _admin, address _feeCollector) {
        require(_admin != address(0), "Invalid admin");
        
        feeCollector = _feeCollector;
        
        _grantRole(DEFAULT_ADMIN_ROLE, _admin);
        _grantRole(POOL_MANAGER_ROLE, _admin);
        _grantRole(REWARDS_ROLE, _admin);

        // Initialize default lock boosts
        lockBoosts.push(LockBoost({duration: 0, multiplier: 1e18}));           // No lock: 1x
        lockBoosts.push(LockBoost({duration: 7 days, multiplier: 1.1e18}));    // 1 week: 1.1x
        lockBoosts.push(LockBoost({duration: 30 days, multiplier: 1.25e18}));  // 1 month: 1.25x
        lockBoosts.push(LockBoost({duration: 90 days, multiplier: 1.5e18}));   // 3 months: 1.5x
        lockBoosts.push(LockBoost({duration: 180 days, multiplier: 2e18}));    // 6 months: 2x
        lockBoosts.push(LockBoost({duration: 365 days, multiplier: 3e18}));    // 1 year: 3x
    }

    // ============ Pool Management ============

    /**
     * @notice Create a new staking pool
     * @param stakingToken Token to stake
     * @param depositFee Deposit fee in basis points
     * @param withdrawFee Withdrawal fee in basis points
     * @param minLockDuration Minimum lock duration
     */
    function createPool(
        address stakingToken,
        uint256 depositFee,
        uint256 withdrawFee,
        uint256 minLockDuration
    ) external onlyRole(POOL_MANAGER_ROLE) returns (uint256 poolId) {
        if (depositFee > 1000 || withdrawFee > 1000) revert InvalidFee(); // Max 10%
        
        poolId = poolCount++;
        
        pools[poolId] = PoolInfo({
            stakingToken: stakingToken,
            totalStaked: 0,
            totalBoostedStaked: 0,
            depositFee: depositFee,
            withdrawFee: withdrawFee,
            lockDuration: minLockDuration,
            active: true
        });
        
        emit PoolCreated(poolId, stakingToken);
    }

    /**
     * @notice Add reward token to pool
     * @param poolId Pool ID
     * @param token Reward token address
     * @param rewardRate Rewards per second
     * @param duration Reward duration
     */
    function addRewardToken(
        uint256 poolId,
        address token,
        uint256 rewardRate,
        uint256 duration
    ) external poolExists(poolId) onlyRole(REWARDS_ROLE) {
        if (!isRewardToken[poolId][token]) {
            poolRewardTokens[poolId].push(token);
            isRewardToken[poolId][token] = true;
        }
        
        RewardInfo storage info = rewardInfo[poolId][token];
        
        if (block.timestamp >= info.periodFinish) {
            info.rewardRate = rewardRate;
        } else {
            uint256 remaining = info.periodFinish - block.timestamp;
            uint256 leftover = remaining * info.rewardRate;
            info.rewardRate = (leftover + rewardRate * duration) / duration;
        }
        
        info.token = token;
        info.lastUpdateTime = block.timestamp;
        info.periodFinish = block.timestamp + duration;
        
        emit RewardTokenAdded(poolId, token, rewardRate, duration);
    }

    /**
     * @notice Notify new rewards
     * @param poolId Pool ID
     * @param token Reward token
     * @param amount Amount of rewards
     * @param duration Duration to distribute over
     */
    function notifyRewardAmount(
        uint256 poolId,
        address token,
        uint256 amount,
        uint256 duration
    ) external poolExists(poolId) onlyRole(REWARDS_ROLE) updateReward(poolId, address(0)) {
        IERC20(token).safeTransferFrom(msg.sender, address(this), amount);
        
        if (!isRewardToken[poolId][token]) {
            poolRewardTokens[poolId].push(token);
            isRewardToken[poolId][token] = true;
        }
        
        RewardInfo storage info = rewardInfo[poolId][token];
        
        if (block.timestamp >= info.periodFinish) {
            info.rewardRate = amount / duration;
        } else {
            uint256 remaining = info.periodFinish - block.timestamp;
            uint256 leftover = remaining * info.rewardRate;
            info.rewardRate = (leftover + amount) / duration;
        }
        
        info.token = token;
        info.lastUpdateTime = block.timestamp;
        info.periodFinish = block.timestamp + duration;
    }

    // ============ Staking Functions ============

    /**
     * @notice Deposit tokens to pool
     * @param poolId Pool ID
     * @param amount Amount to deposit
     * @param lockDuration Lock duration for boost
     * @param referrer Optional referrer address
     */
    function deposit(
        uint256 poolId,
        uint256 amount,
        uint256 lockDuration,
        address referrer
    ) external nonReentrant whenNotPaused poolExists(poolId) updateReward(poolId, msg.sender) {
        if (amount == 0) revert ZeroAmount();
        
        PoolInfo storage pool = pools[poolId];
        if (!pool.active) revert PoolNotActive();
        if (lockDuration < pool.lockDuration) revert InvalidDuration();
        
        UserInfo storage user = userInfo[poolId][msg.sender];
        
        // Handle referral
        if (referrer != address(0) && user.referrer == address(0)) {
            _setReferrer(msg.sender, referrer);
        }
        
        // Transfer tokens
        uint256 balanceBefore = IERC20(pool.stakingToken).balanceOf(address(this));
        IERC20(pool.stakingToken).safeTransferFrom(msg.sender, address(this), amount);
        uint256 received = IERC20(pool.stakingToken).balanceOf(address(this)) - balanceBefore;
        
        // Apply deposit fee
        uint256 feeAmount = (received * pool.depositFee) / BASIS_POINTS;
        if (feeAmount > 0 && feeCollector != address(0)) {
            IERC20(pool.stakingToken).safeTransfer(feeCollector, feeAmount);
        }
        uint256 depositAmount = received - feeAmount;
        
        // Calculate boost
        uint256 boostMultiplier = _getBoostMultiplier(lockDuration, msg.sender);
        uint256 boostedAmount = (depositAmount * boostMultiplier) / PRECISION;
        
        // Update user info
        if (user.amount > 0) {
            // Recalculate boost for total
            pool.totalBoostedStaked -= user.boostedAmount;
        }
        
        user.amount += depositAmount;
        user.boostedAmount = (user.amount * boostMultiplier) / PRECISION;
        user.depositTime = block.timestamp;
        
        // Set unlock time to max of current and new
        uint256 newUnlockTime = block.timestamp + lockDuration;
        if (newUnlockTime > user.unlockTime) {
            user.unlockTime = newUnlockTime;
            user.lockDuration = lockDuration;
        }
        
        // Update pool totals
        pool.totalStaked += depositAmount;
        pool.totalBoostedStaked += user.boostedAmount;
        
        emit Deposited(poolId, msg.sender, depositAmount, lockDuration);
    }

    /**
     * @notice Withdraw staked tokens
     * @param poolId Pool ID
     * @param amount Amount to withdraw
     */
    function withdraw(
        uint256 poolId,
        uint256 amount
    ) external nonReentrant poolExists(poolId) updateReward(poolId, msg.sender) {
        if (amount == 0) revert ZeroAmount();
        
        PoolInfo storage pool = pools[poolId];
        UserInfo storage user = userInfo[poolId][msg.sender];
        
        if (user.amount < amount) revert InsufficientBalance();
        if (block.timestamp < user.unlockTime) revert LockNotExpired();
        
        // Update totals
        uint256 boostedToRemove = (amount * user.boostedAmount) / user.amount;
        pool.totalStaked -= amount;
        pool.totalBoostedStaked -= boostedToRemove;
        
        user.amount -= amount;
        user.boostedAmount -= boostedToRemove;
        
        // Apply withdrawal fee
        uint256 feeAmount = (amount * pool.withdrawFee) / BASIS_POINTS;
        if (feeAmount > 0 && feeCollector != address(0)) {
            IERC20(pool.stakingToken).safeTransfer(feeCollector, feeAmount);
        }
        uint256 withdrawAmount = amount - feeAmount;
        
        IERC20(pool.stakingToken).safeTransfer(msg.sender, withdrawAmount);
        
        emit Withdrawn(poolId, msg.sender, withdrawAmount);
    }

    /**
     * @notice Claim all rewards
     * @param poolId Pool ID
     */
    function claim(uint256 poolId) external nonReentrant poolExists(poolId) updateReward(poolId, msg.sender) {
        UserInfo storage user = userInfo[poolId][msg.sender];
        address[] memory tokens = poolRewardTokens[poolId];
        
        for (uint256 i = 0; i < tokens.length; i++) {
            uint256 reward = user.rewards[tokens[i]];
            if (reward > 0) {
                user.rewards[tokens[i]] = 0;
                
                // Pay referral
                if (user.referrer != address(0)) {
                    uint256 referralAmount = (reward * referralRewardRate) / BASIS_POINTS;
                    if (referralAmount > 0) {
                        referralRewards[user.referrer] += referralAmount;
                        emit ReferralRewardPaid(user.referrer, msg.sender, referralAmount);
                    }
                }
                
                IERC20(tokens[i]).safeTransfer(msg.sender, reward);
                rewardInfo[poolId][tokens[i]].totalDistributed += reward;
                
                emit RewardClaimed(poolId, msg.sender, tokens[i], reward);
            }
        }
    }

    /**
     * @notice Compound rewards back into stake (if reward token = staking token)
     * @param poolId Pool ID
     */
    function compound(uint256 poolId) external nonReentrant poolExists(poolId) updateReward(poolId, msg.sender) {
        PoolInfo storage pool = pools[poolId];
        UserInfo storage user = userInfo[poolId][msg.sender];
        
        uint256 compoundAmount = 0;
        address[] memory tokens = poolRewardTokens[poolId];
        
        for (uint256 i = 0; i < tokens.length; i++) {
            if (tokens[i] == pool.stakingToken) {
                uint256 reward = user.rewards[tokens[i]];
                if (reward > 0) {
                    user.rewards[tokens[i]] = 0;
                    compoundAmount += reward;
                    
                    emit RewardCompounded(poolId, msg.sender, tokens[i], reward);
                }
            }
        }
        
        if (compoundAmount > 0) {
            // Add to stake without fee
            uint256 boostMultiplier = _getBoostMultiplier(user.lockDuration, msg.sender);
            uint256 boostedAmount = (compoundAmount * boostMultiplier) / PRECISION;
            
            pool.totalBoostedStaked -= user.boostedAmount;
            user.amount += compoundAmount;
            user.boostedAmount = (user.amount * boostMultiplier) / PRECISION;
            pool.totalStaked += compoundAmount;
            pool.totalBoostedStaked += user.boostedAmount;
        }
    }

    /**
     * @notice Extend lock duration to increase boost
     * @param poolId Pool ID
     * @param newLockDuration New lock duration
     */
    function extendLock(
        uint256 poolId,
        uint256 newLockDuration
    ) external nonReentrant poolExists(poolId) updateReward(poolId, msg.sender) {
        UserInfo storage user = userInfo[poolId][msg.sender];
        PoolInfo storage pool = pools[poolId];
        
        if (user.amount == 0) revert InsufficientBalance();
        if (newLockDuration <= user.lockDuration) revert InvalidDuration();
        
        // Update boost
        pool.totalBoostedStaked -= user.boostedAmount;
        
        user.lockDuration = newLockDuration;
        user.unlockTime = block.timestamp + newLockDuration;
        
        uint256 boostMultiplier = _getBoostMultiplier(newLockDuration, msg.sender);
        user.boostedAmount = (user.amount * boostMultiplier) / PRECISION;
        
        pool.totalBoostedStaked += user.boostedAmount;
        
        emit LockExtended(poolId, msg.sender, user.unlockTime);
    }

    /**
     * @notice Emergency withdraw without caring about rewards
     * @param poolId Pool ID
     */
    function emergencyWithdraw(uint256 poolId) external nonReentrant poolExists(poolId) {
        PoolInfo storage pool = pools[poolId];
        UserInfo storage user = userInfo[poolId][msg.sender];
        
        uint256 amount = user.amount;
        if (amount == 0) revert InsufficientBalance();
        
        // Update totals
        pool.totalStaked -= amount;
        pool.totalBoostedStaked -= user.boostedAmount;
        
        user.amount = 0;
        user.boostedAmount = 0;
        
        // 10% emergency withdrawal penalty
        uint256 penalty = amount / 10;
        if (feeCollector != address(0)) {
            IERC20(pool.stakingToken).safeTransfer(feeCollector, penalty);
        }
        
        IERC20(pool.stakingToken).safeTransfer(msg.sender, amount - penalty);
        
        emit EmergencyWithdraw(poolId, msg.sender, amount - penalty);
    }

    // ============ Referral Functions ============

    function _setReferrer(address user, address referrer) internal {
        if (referrer == user) revert CannotReferSelf();
        if (userInfo[0][user].referrer != address(0)) revert AlreadyHasReferrer();
        
        // Store referrer in pool 0 as global
        userInfo[0][user].referrer = referrer;
        referrals[referrer].push(user);
        userInfo[0][referrer].totalReferred++;
        
        emit ReferralSet(user, referrer);
    }

    /**
     * @notice Claim accumulated referral rewards
     * @param token Reward token
     */
    function claimReferralRewards(address token) external nonReentrant {
        uint256 amount = referralRewards[msg.sender];
        if (amount == 0) revert ZeroAmount();
        
        referralRewards[msg.sender] = 0;
        IERC20(token).safeTransfer(msg.sender, amount);
    }

    // ============ View Functions ============

    function lastTimeRewardApplicable(uint256 poolId, address token) public view returns (uint256) {
        RewardInfo storage info = rewardInfo[poolId][token];
        return Math.min(block.timestamp, info.periodFinish);
    }

    function rewardPerToken(uint256 poolId, address token) public view returns (uint256) {
        PoolInfo storage pool = pools[poolId];
        RewardInfo storage info = rewardInfo[poolId][token];
        
        if (pool.totalBoostedStaked == 0) {
            return info.rewardPerTokenStored;
        }
        
        return info.rewardPerTokenStored + (
            (lastTimeRewardApplicable(poolId, token) - info.lastUpdateTime) *
            info.rewardRate *
            PRECISION /
            pool.totalBoostedStaked
        );
    }

    function earned(uint256 poolId, address account, address token) public view returns (uint256) {
        UserInfo storage user = userInfo[poolId][account];
        return (
            user.boostedAmount *
            (rewardPerToken(poolId, token) - user.rewardPerTokenPaid[token]) /
            PRECISION
        ) + user.rewards[token];
    }

    function getUserInfo(uint256 poolId, address account) external view returns (
        uint256 amount,
        uint256 boostedAmount,
        uint256 depositTime,
        uint256 unlockTime,
        uint256 lockDuration,
        address referrer
    ) {
        UserInfo storage user = userInfo[poolId][account];
        return (
            user.amount,
            user.boostedAmount,
            user.depositTime,
            user.unlockTime,
            user.lockDuration,
            userInfo[0][account].referrer
        );
    }

    function getPoolRewardTokens(uint256 poolId) external view returns (address[] memory) {
        return poolRewardTokens[poolId];
    }

    function _getBoostMultiplier(uint256 lockDuration, address user) internal view returns (uint256 multiplier) {
        // Base multiplier from lock duration
        multiplier = PRECISION;
        for (uint256 i = lockBoosts.length; i > 0; i--) {
            if (lockDuration >= lockBoosts[i - 1].duration) {
                multiplier = lockBoosts[i - 1].multiplier;
                break;
            }
        }
        
        // NFT boost if applicable
        if (nftBoostContract != address(0)) {
            (bool success, bytes memory data) = nftBoostContract.staticcall(
                abi.encodeWithSignature("getBoost(address)", user)
            );
            if (success && data.length >= 32) {
                uint256 nftBoost = abi.decode(data, (uint256));
                multiplier = (multiplier * (PRECISION + nftBoost)) / PRECISION;
            }
        }
        
        // Cap at max boost
        if (multiplier > MAX_BOOST) {
            multiplier = MAX_BOOST;
        }
    }

    // ============ Admin Functions ============

    function setPoolActive(uint256 poolId, bool active) external onlyRole(POOL_MANAGER_ROLE) poolExists(poolId) {
        pools[poolId].active = active;
        emit PoolUpdated(poolId);
    }

    function setPoolFees(
        uint256 poolId,
        uint256 depositFee,
        uint256 withdrawFee
    ) external onlyRole(POOL_MANAGER_ROLE) poolExists(poolId) {
        if (depositFee > 1000 || withdrawFee > 1000) revert InvalidFee();
        pools[poolId].depositFee = depositFee;
        pools[poolId].withdrawFee = withdrawFee;
        emit PoolUpdated(poolId);
    }

    function setNFTBoostContract(address _nftBoostContract) external onlyRole(DEFAULT_ADMIN_ROLE) {
        nftBoostContract = _nftBoostContract;
    }

    function setReferralRate(uint256 rate) external onlyRole(DEFAULT_ADMIN_ROLE) {
        require(rate <= 2000, "Rate too high"); // Max 20%
        referralRewardRate = rate;
    }

    function setFeeCollector(address _collector) external onlyRole(DEFAULT_ADMIN_ROLE) {
        feeCollector = _collector;
    }

    function setPaused(bool _paused) external onlyRole(DEFAULT_ADMIN_ROLE) {
        paused = _paused;
    }

    function updateLockBoosts(LockBoost[] calldata _boosts) external onlyRole(DEFAULT_ADMIN_ROLE) {
        delete lockBoosts;
        for (uint256 i = 0; i < _boosts.length; i++) {
            lockBoosts.push(_boosts[i]);
        }
    }

    function recoverTokens(address token, uint256 amount) external onlyRole(DEFAULT_ADMIN_ROLE) {
        IERC20(token).safeTransfer(msg.sender, amount);
    }
}
