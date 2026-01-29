const { expect } = require("chai");
const { ethers } = require("hardhat");
const { time } = require("@nomicfoundation/hardhat-network-helpers");

describe("StakingPool", function () {
  let stakingPool;
  let stakingToken;
  let rewardToken;
  let owner, user1, user2;

  const MIN_STAKE = ethers.parseEther("10");
  const MAX_STAKE = ethers.parseEther("10000");
  const LOCK_DURATION = 7 * 24 * 60 * 60; // 7 days

  beforeEach(async function () {
    [owner, user1, user2] = await ethers.getSigners();

    // Deploy mock tokens
    const MockERC20 = await ethers.getContractFactory("MockERC20");
    stakingToken = await MockERC20.deploy("Staking Token", "STK", 18);
    rewardToken = await MockERC20.deploy("Reward Token", "RWD", 18);

    // Deploy staking pool
    const StakingPool = await ethers.getContractFactory("StakingPool");
    stakingPool = await StakingPool.deploy(
      await stakingToken.getAddress(),
      await rewardToken.getAddress(),
      MIN_STAKE,
      MAX_STAKE,
      LOCK_DURATION
    );

    // Mint tokens to users
    await stakingToken.mint(user1.address, ethers.parseEther("100000"));
    await stakingToken.mint(user2.address, ethers.parseEther("100000"));

    // Approve staking pool
    await stakingToken.connect(user1).approve(await stakingPool.getAddress(), ethers.MaxUint256);
    await stakingToken.connect(user2).approve(await stakingPool.getAddress(), ethers.MaxUint256);

    // Add rewards
    await rewardToken.mint(owner.address, ethers.parseEther("1000000"));
    await rewardToken.approve(await stakingPool.getAddress(), ethers.MaxUint256);
  });

  describe("Deployment", function () {
    it("Should set correct staking token", async function () {
      expect(await stakingPool.stakingToken()).to.equal(await stakingToken.getAddress());
    });

    it("Should set correct reward token", async function () {
      expect(await stakingPool.rewardToken()).to.equal(await rewardToken.getAddress());
    });

    it("Should set correct config", async function () {
      const config = await stakingPool.config();
      expect(config.minStake).to.equal(MIN_STAKE);
      expect(config.maxStake).to.equal(MAX_STAKE);
      expect(config.lockDuration).to.equal(LOCK_DURATION);
    });
  });

  describe("Staking", function () {
    it("Should allow staking above minimum", async function () {
      const amount = ethers.parseEther("100");
      await stakingPool.connect(user1).stake(amount);
      
      const stakeInfo = await stakingPool.stakes(user1.address);
      expect(stakeInfo.amount).to.equal(amount);
    });

    it("Should update total staked", async function () {
      const amount = ethers.parseEther("100");
      await stakingPool.connect(user1).stake(amount);
      expect(await stakingPool.totalStaked()).to.equal(amount);
    });

    it("Should set lock end time", async function () {
      const amount = ethers.parseEther("100");
      await stakingPool.connect(user1).stake(amount);
      
      const stakeInfo = await stakingPool.stakes(user1.address);
      const expectedLockEnd = (await time.latest()) + LOCK_DURATION;
      expect(stakeInfo.lockEnd).to.be.closeTo(expectedLockEnd, 5);
    });

    it("Should reject stake below minimum", async function () {
      const amount = ethers.parseEther("5"); // Below minimum
      await expect(
        stakingPool.connect(user1).stake(amount)
      ).to.be.revertedWithCustomError(stakingPool, "InsufficientStake");
    });

    it("Should reject stake above maximum", async function () {
      const amount = ethers.parseEther("20000"); // Above maximum
      await expect(
        stakingPool.connect(user1).stake(amount)
      ).to.be.revertedWithCustomError(stakingPool, "ExceedsMaxStake");
    });

    it("Should emit Staked event", async function () {
      const amount = ethers.parseEther("100");
      await expect(stakingPool.connect(user1).stake(amount))
        .to.emit(stakingPool, "Staked");
    });
  });

  describe("Unstaking", function () {
    beforeEach(async function () {
      await stakingPool.connect(user1).stake(ethers.parseEther("100"));
    });

    it("Should allow unstaking with penalty during lock", async function () {
      const unstakeAmount = ethers.parseEther("50");
      await stakingPool.connect(user1).unstake(unstakeAmount);
      
      const stakeInfo = await stakingPool.stakes(user1.address);
      expect(stakeInfo.amount).to.equal(ethers.parseEther("50"));
    });

    it("Should apply penalty during lock period", async function () {
      const initialBalance = await stakingToken.balanceOf(user1.address);
      const unstakeAmount = ethers.parseEther("50");
      
      await stakingPool.connect(user1).unstake(unstakeAmount);
      
      const finalBalance = await stakingToken.balanceOf(user1.address);
      const received = finalBalance - initialBalance;
      
      // 10% penalty = 5 ETH
      const expectedReceived = ethers.parseEther("45");
      expect(received).to.equal(expectedReceived);
    });

    it("Should not apply penalty after lock expires", async function () {
      await time.increase(LOCK_DURATION + 1);
      
      const initialBalance = await stakingToken.balanceOf(user1.address);
      const unstakeAmount = ethers.parseEther("50");
      
      await stakingPool.connect(user1).unstake(unstakeAmount);
      
      const finalBalance = await stakingToken.balanceOf(user1.address);
      expect(finalBalance - initialBalance).to.equal(unstakeAmount);
    });

    it("Should revert if nothing staked", async function () {
      await expect(
        stakingPool.connect(user2).unstake(ethers.parseEther("10"))
      ).to.be.revertedWithCustomError(stakingPool, "NothingStaked");
    });
  });

  describe("Rewards", function () {
    beforeEach(async function () {
      // Add rewards (100 tokens over 100 seconds = 1 token/second)
      await stakingPool.addRewards(ethers.parseEther("100"), 100);
    });

    it("Should accumulate rewards", async function () {
      await stakingPool.connect(user1).stake(ethers.parseEther("100"));
      
      await time.increase(50);
      
      const pending = await stakingPool.pendingRewards(user1.address);
      expect(pending).to.be.gt(0);
    });

    it("Should allow claiming rewards", async function () {
      await stakingPool.connect(user1).stake(ethers.parseEther("100"));
      
      await time.increase(50);
      
      await stakingPool.connect(user1).claim();
      
      const rewardBalance = await rewardToken.balanceOf(user1.address);
      expect(rewardBalance).to.be.gt(0);
    });

    it("Should distribute rewards proportionally", async function () {
      await stakingPool.connect(user1).stake(ethers.parseEther("100"));
      await stakingPool.connect(user2).stake(ethers.parseEther("100"));
      
      await time.increase(100);
      
      const pending1 = await stakingPool.pendingRewards(user1.address);
      const pending2 = await stakingPool.pendingRewards(user2.address);
      
      // Should be approximately equal
      expect(pending1).to.be.closeTo(pending2, ethers.parseEther("1"));
    });
  });

  describe("Admin", function () {
    it("Should allow setting reward rate", async function () {
      await stakingPool.setRewardRate(ethers.parseEther("1"));
      expect(await stakingPool.rewardRate()).to.equal(ethers.parseEther("1"));
    });

    it("Should allow pausing", async function () {
      await stakingPool.pause();
      expect(await stakingPool.paused()).to.be.true;
    });

    it("Should prevent staking when paused", async function () {
      await stakingPool.pause();
      await expect(
        stakingPool.connect(user1).stake(ethers.parseEther("100"))
      ).to.be.revertedWithCustomError(stakingPool, "EnforcedPause");
    });
  });
});
