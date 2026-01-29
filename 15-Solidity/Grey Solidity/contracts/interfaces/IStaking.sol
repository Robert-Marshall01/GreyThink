// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

/**
 * @title IStaking
 * @notice Interface for staking pool contracts
 * @dev Supports staking, unstaking, and reward distribution
 */
interface IStaking {
    /**
     * @notice Stake info structure
     */
    struct StakeInfo {
        uint256 amount;
        uint256 rewardDebt;
        uint256 lastStakeTime;
        uint256 lockEndTime;
    }

    /**
     * @notice Pool info structure
     */
    struct PoolInfo {
        address stakingToken;
        address rewardToken;
        uint256 totalStaked;
        uint256 rewardPerSecond;
        uint256 accRewardPerShare;
        uint256 lastRewardTime;
        uint256 startTime;
        uint256 endTime;
        uint256 lockDuration;
        bool active;
    }

    /**
     * @notice Returns the staking token address
     * @return The staking token
     */
    function stakingToken() external view returns (address);

    /**
     * @notice Returns the reward token address
     * @return The reward token
     */
    function rewardToken() external view returns (address);

    /**
     * @notice Returns total staked amount
     * @return The total staked
     */
    function totalStaked() external view returns (uint256);

    /**
     * @notice Deposits tokens into the staking pool
     * @param amount Amount to stake
     */
    function stake(uint256 amount) external;

    /**
     * @notice Deposits tokens with a lock duration
     * @param amount Amount to stake
     * @param lockDuration Lock duration in seconds
     */
    function stakeWithLock(uint256 amount, uint256 lockDuration) external;

    /**
     * @notice Withdraws staked tokens
     * @param amount Amount to withdraw
     */
    function unstake(uint256 amount) external;

    /**
     * @notice Emergency withdraw without caring about rewards
     */
    function emergencyWithdraw() external;

    /**
     * @notice Claims pending rewards
     */
    function claimRewards() external;

    /**
     * @notice Claims and restakes rewards
     */
    function compound() external;

    /**
     * @notice Returns pending rewards for a user
     * @param user The user address
     * @return The pending reward amount
     */
    function pendingRewards(address user) external view returns (uint256);

    /**
     * @notice Returns stake info for a user
     * @param user The user address
     * @return The stake info
     */
    function getStakeInfo(address user) external view returns (StakeInfo memory);

    /**
     * @notice Returns pool information
     * @return The pool info
     */
    function getPoolInfo() external view returns (PoolInfo memory);

    /**
     * @notice Updates reward rate
     * @param rewardPerSecond New reward rate
     */
    function setRewardRate(uint256 rewardPerSecond) external;

    /**
     * @notice Adds rewards to the pool
     * @param amount Amount to add
     */
    function addRewards(uint256 amount) external;

    /**
     * @notice Sets the lock duration
     * @param duration The lock duration in seconds
     */
    function setLockDuration(uint256 duration) external;

    /**
     * @notice Pauses staking
     */
    function pause() external;

    /**
     * @notice Unpauses staking
     */
    function unpause() external;

    /**
     * @notice Returns the APR
     * @return The annual percentage rate
     */
    function getAPR() external view returns (uint256);

    /**
     * @notice Returns the APY
     * @return The annual percentage yield
     */
    function getAPY() external view returns (uint256);

    // Events
    event Staked(address indexed user, uint256 amount, uint256 lockDuration);
    event Unstaked(address indexed user, uint256 amount);
    event RewardsClaimed(address indexed user, uint256 amount);
    event Compounded(address indexed user, uint256 amount);
    event EmergencyWithdraw(address indexed user, uint256 amount);
    event RewardRateUpdated(uint256 oldRate, uint256 newRate);
    event RewardsAdded(uint256 amount);
    event LockDurationUpdated(uint256 oldDuration, uint256 newDuration);
    event PoolUpdated(uint256 accRewardPerShare, uint256 lastRewardTime);
}
