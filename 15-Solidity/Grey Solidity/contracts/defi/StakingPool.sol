// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/Pausable.sol";

/**
 * @title StakingPool
 * @author Grey Solidity Project
 * @notice Flexible staking pool with rewards and lock periods
 * @dev Supports multiple reward tokens, lock periods, and boost multipliers
 */
contract StakingPool is AccessControl, ReentrancyGuard, Pausable {
    using SafeERC20 for IERC20;

    /// @notice Role for managing pool parameters
    bytes32 public constant POOL_ADMIN_ROLE = keccak256("POOL_ADMIN_ROLE");

    /// @notice Role for distributing rewards
    bytes32 public constant REWARD_DISTRIBUTOR_ROLE = keccak256("REWARD_DISTRIBUTOR_ROLE");

    /// @notice Stake info structure
    struct StakeInfo {
        uint256 amount;
        uint256 rewardDebt;
        uint256 lockEnd;
        uint256 boostMultiplier;  // 1e18 = 1x
        uint256 stakedAt;
    }

    /// @notice Pool configuration
    struct PoolConfig {
        uint256 minStake;
        uint256 maxStake;
        uint256 lockDuration;
        uint256 earlyUnstakePenalty;  // basis points (100 = 1%)
        bool emergencyWithdrawEnabled;
    }

    /// @notice Staking token
    IERC20 public immutable stakingToken;

    /// @notice Reward token
    IERC20 public immutable rewardToken;

    /// @notice Total staked amount
    uint256 public totalStaked;

    /// @notice Accumulated reward per share (scaled by 1e18)
    uint256 public accRewardPerShare;

    /// @notice Last reward update timestamp
    uint256 public lastRewardTime;

    /// @notice Reward rate per second
    uint256 public rewardRate;

    /// @notice Pool configuration
    PoolConfig public config;

    /// @notice Mapping of user stakes
    mapping(address => StakeInfo) public stakes;

    /// @notice Mapping of user to pending penalty
    mapping(address => uint256) public pendingPenalties;

    /// @notice Total penalties collected
    uint256 public totalPenalties;

    // ============ Events ============

    event Staked(address indexed user, uint256 amount, uint256 lockEnd);
    event Unstaked(address indexed user, uint256 amount, uint256 penalty);
    event RewardsClaimed(address indexed user, uint256 amount);
    event RewardRateUpdated(uint256 oldRate, uint256 newRate);
    event ConfigUpdated(PoolConfig config);
    event BoostApplied(address indexed user, uint256 multiplier);
    event EmergencyWithdraw(address indexed user, uint256 amount);
    event RewardsAdded(uint256 amount, uint256 duration);
    event PenaltiesWithdrawn(address indexed to, uint256 amount);

    // ============ Errors ============

    error InsufficientStake(uint256 amount, uint256 min);
    error ExceedsMaxStake(uint256 amount, uint256 max);
    error StillLocked(uint256 lockEnd, uint256 currentTime);
    error NothingStaked();
    error NothingToClaim();
    error EmergencyWithdrawDisabled();
    error ZeroAmount();
    error InvalidConfig();

    /**
     * @notice Initializes the staking pool
     * @param stakingToken_ The token to stake
     * @param rewardToken_ The reward token
     * @param minStake_ Minimum stake amount
     * @param maxStake_ Maximum stake amount
     * @param lockDuration_ Lock duration in seconds
     */
    constructor(
        address stakingToken_,
        address rewardToken_,
        uint256 minStake_,
        uint256 maxStake_,
        uint256 lockDuration_
    ) {
        stakingToken = IERC20(stakingToken_);
        rewardToken = IERC20(rewardToken_);

        config = PoolConfig({
            minStake: minStake_,
            maxStake: maxStake_,
            lockDuration: lockDuration_,
            earlyUnstakePenalty: 1000,  // 10%
            emergencyWithdrawEnabled: false
        });

        lastRewardTime = block.timestamp;

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(POOL_ADMIN_ROLE, msg.sender);
        _grantRole(REWARD_DISTRIBUTOR_ROLE, msg.sender);
    }

    // ============ Staking Functions ============

    /**
     * @notice Stakes tokens
     * @param amount Amount to stake
     */
    function stake(uint256 amount) external nonReentrant whenNotPaused {
        if (amount == 0) revert ZeroAmount();
        
        StakeInfo storage userStake = stakes[msg.sender];
        uint256 newTotal = userStake.amount + amount;

        if (newTotal < config.minStake) {
            revert InsufficientStake(newTotal, config.minStake);
        }
        if (config.maxStake > 0 && newTotal > config.maxStake) {
            revert ExceedsMaxStake(newTotal, config.maxStake);
        }

        _updatePool();
        
        // Claim pending rewards first
        if (userStake.amount > 0) {
            uint256 pending = _calculatePending(msg.sender);
            if (pending > 0) {
                _safeRewardTransfer(msg.sender, pending);
                emit RewardsClaimed(msg.sender, pending);
            }
        }

        stakingToken.safeTransferFrom(msg.sender, address(this), amount);

        userStake.amount = newTotal;
        userStake.rewardDebt = (newTotal * accRewardPerShare) / 1e18;
        userStake.lockEnd = block.timestamp + config.lockDuration;
        
        if (userStake.stakedAt == 0) {
            userStake.stakedAt = block.timestamp;
            userStake.boostMultiplier = 1e18;  // 1x default
        }

        totalStaked += amount;

        emit Staked(msg.sender, amount, userStake.lockEnd);
    }

    /**
     * @notice Unstakes tokens
     * @param amount Amount to unstake
     */
    function unstake(uint256 amount) external nonReentrant whenNotPaused {
        StakeInfo storage userStake = stakes[msg.sender];
        
        if (userStake.amount == 0) revert NothingStaked();
        if (amount == 0) revert ZeroAmount();
        if (amount > userStake.amount) {
            amount = userStake.amount;
        }

        _updatePool();

        // Claim pending rewards
        uint256 pending = _calculatePending(msg.sender);
        if (pending > 0) {
            _safeRewardTransfer(msg.sender, pending);
            emit RewardsClaimed(msg.sender, pending);
        }

        // Calculate penalty if still locked
        uint256 penalty = 0;
        if (block.timestamp < userStake.lockEnd) {
            penalty = (amount * config.earlyUnstakePenalty) / 10000;
            totalPenalties += penalty;
        }

        userStake.amount -= amount;
        userStake.rewardDebt = (userStake.amount * accRewardPerShare) / 1e18;
        totalStaked -= amount;

        if (userStake.amount == 0) {
            userStake.stakedAt = 0;
            userStake.boostMultiplier = 0;
            userStake.lockEnd = 0;
        }

        stakingToken.safeTransfer(msg.sender, amount - penalty);

        emit Unstaked(msg.sender, amount, penalty);
    }

    /**
     * @notice Claims pending rewards
     */
    function claim() external nonReentrant whenNotPaused {
        StakeInfo storage userStake = stakes[msg.sender];
        if (userStake.amount == 0) revert NothingStaked();

        _updatePool();

        uint256 pending = _calculatePending(msg.sender);
        if (pending == 0) revert NothingToClaim();

        userStake.rewardDebt = (userStake.amount * accRewardPerShare) / 1e18;

        _safeRewardTransfer(msg.sender, pending);

        emit RewardsClaimed(msg.sender, pending);
    }

    /**
     * @notice Emergency withdrawal without rewards
     */
    function emergencyWithdraw() external nonReentrant {
        if (!config.emergencyWithdrawEnabled) revert EmergencyWithdrawDisabled();

        StakeInfo storage userStake = stakes[msg.sender];
        uint256 amount = userStake.amount;
        
        if (amount == 0) revert NothingStaked();

        totalStaked -= amount;
        
        delete stakes[msg.sender];

        stakingToken.safeTransfer(msg.sender, amount);

        emit EmergencyWithdraw(msg.sender, amount);
    }

    // ============ View Functions ============

    /**
     * @notice Returns pending rewards for a user
     * @param user The user address
     * @return The pending reward amount
     */
    function pendingRewards(address user) external view returns (uint256) {
        StakeInfo storage userStake = stakes[user];
        if (userStake.amount == 0) {
            return 0;
        }

        uint256 accReward = accRewardPerShare;
        if (block.timestamp > lastRewardTime && totalStaked > 0) {
            uint256 elapsed = block.timestamp - lastRewardTime;
            uint256 reward = elapsed * rewardRate;
            accReward += (reward * 1e18) / totalStaked;
        }

        return _calculatePendingWithAcc(user, accReward);
    }

    /**
     * @notice Returns stake info for a user
     * @param user The user address
     * @return The stake info struct
     */
    function getStakeInfo(address user) external view returns (StakeInfo memory) {
        return stakes[user];
    }

    /**
     * @notice Returns time until lock expires
     * @param user The user address
     * @return Seconds until unlocked (0 if already unlocked)
     */
    function timeUntilUnlock(address user) external view returns (uint256) {
        StakeInfo storage userStake = stakes[user];
        if (block.timestamp >= userStake.lockEnd) {
            return 0;
        }
        return userStake.lockEnd - block.timestamp;
    }

    /**
     * @notice Returns effective staked amount with boost
     * @param user The user address
     * @return The boosted amount
     */
    function effectiveStake(address user) external view returns (uint256) {
        StakeInfo storage userStake = stakes[user];
        return (userStake.amount * userStake.boostMultiplier) / 1e18;
    }

    // ============ Admin Functions ============

    /**
     * @notice Adds rewards to the pool
     * @param amount The reward amount
     * @param duration The distribution duration
     */
    function addRewards(
        uint256 amount,
        uint256 duration
    ) external onlyRole(REWARD_DISTRIBUTOR_ROLE) {
        require(duration > 0, "StakingPool: zero duration");
        
        _updatePool();

        rewardToken.safeTransferFrom(msg.sender, address(this), amount);
        rewardRate = amount / duration;

        emit RewardsAdded(amount, duration);
    }

    /**
     * @notice Sets the reward rate
     * @param newRate The new rate per second
     */
    function setRewardRate(uint256 newRate) external onlyRole(POOL_ADMIN_ROLE) {
        _updatePool();
        
        uint256 oldRate = rewardRate;
        rewardRate = newRate;

        emit RewardRateUpdated(oldRate, newRate);
    }

    /**
     * @notice Updates pool configuration
     * @param newConfig The new configuration
     */
    function setConfig(PoolConfig calldata newConfig) external onlyRole(POOL_ADMIN_ROLE) {
        if (newConfig.maxStake > 0 && newConfig.maxStake < newConfig.minStake) {
            revert InvalidConfig();
        }
        if (newConfig.earlyUnstakePenalty > 5000) {  // Max 50% penalty
            revert InvalidConfig();
        }

        config = newConfig;
        emit ConfigUpdated(newConfig);
    }

    /**
     * @notice Applies a boost multiplier to a user
     * @param user The user address
     * @param multiplier The boost multiplier (1e18 = 1x)
     */
    function applyBoost(
        address user,
        uint256 multiplier
    ) external onlyRole(POOL_ADMIN_ROLE) {
        require(multiplier >= 1e18 && multiplier <= 3e18, "StakingPool: invalid multiplier");
        
        _updatePool();
        
        StakeInfo storage userStake = stakes[user];
        if (userStake.amount > 0) {
            // Claim pending first
            uint256 pending = _calculatePending(user);
            if (pending > 0) {
                _safeRewardTransfer(user, pending);
                emit RewardsClaimed(user, pending);
            }
            userStake.rewardDebt = (userStake.amount * accRewardPerShare) / 1e18;
        }

        userStake.boostMultiplier = multiplier;
        emit BoostApplied(user, multiplier);
    }

    /**
     * @notice Withdraws collected penalties
     * @param to The recipient
     * @param amount The amount
     */
    function withdrawPenalties(
        address to,
        uint256 amount
    ) external onlyRole(DEFAULT_ADMIN_ROLE) nonReentrant {
        require(amount <= totalPenalties, "StakingPool: exceeds penalties");
        
        totalPenalties -= amount;
        stakingToken.safeTransfer(to, amount);

        emit PenaltiesWithdrawn(to, amount);
    }

    /**
     * @notice Pauses the pool
     */
    function pause() external onlyRole(POOL_ADMIN_ROLE) {
        _pause();
    }

    /**
     * @notice Unpauses the pool
     */
    function unpause() external onlyRole(POOL_ADMIN_ROLE) {
        _unpause();
    }

    // ============ Internal Functions ============

    /**
     * @dev Updates pool rewards
     */
    function _updatePool() private {
        if (block.timestamp <= lastRewardTime) {
            return;
        }

        if (totalStaked == 0) {
            lastRewardTime = block.timestamp;
            return;
        }

        uint256 elapsed = block.timestamp - lastRewardTime;
        uint256 reward = elapsed * rewardRate;
        accRewardPerShare += (reward * 1e18) / totalStaked;
        lastRewardTime = block.timestamp;
    }

    /**
     * @dev Calculates pending rewards
     */
    function _calculatePending(address user) private view returns (uint256) {
        return _calculatePendingWithAcc(user, accRewardPerShare);
    }

    /**
     * @dev Calculates pending with custom acc
     */
    function _calculatePendingWithAcc(address user, uint256 acc) private view returns (uint256) {
        StakeInfo storage userStake = stakes[user];
        uint256 effectiveAmount = (userStake.amount * userStake.boostMultiplier) / 1e18;
        uint256 accumulated = (effectiveAmount * acc) / 1e18;
        
        // Adjust reward debt by boost
        uint256 adjustedDebt = (userStake.rewardDebt * userStake.boostMultiplier) / 1e18;
        
        return accumulated > adjustedDebt ? accumulated - adjustedDebt : 0;
    }

    /**
     * @dev Safe reward transfer
     */
    function _safeRewardTransfer(address to, uint256 amount) private {
        uint256 balance = rewardToken.balanceOf(address(this));
        if (amount > balance) {
            amount = balance;
        }
        if (amount > 0) {
            rewardToken.safeTransfer(to, amount);
        }
    }
}
