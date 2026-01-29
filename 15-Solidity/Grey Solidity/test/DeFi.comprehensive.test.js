const { expect } = require("chai");
const { ethers } = require("hardhat");
const { time, loadFixture } = require("@nomicfoundation/hardhat-network-helpers");

/**
 * @title Comprehensive DeFi Module Tests
 * @notice Tests for Staking, Liquidity Pool, Vault, and Lending
 */
describe("DeFi Modules - Comprehensive Tests", function () {
  // ============================================
  // STAKING POOL FIXTURES
  // ============================================

  async function deployStakingPoolFixture() {
    const [owner, user1, user2, user3, rewardDistributor] = await ethers.getSigners();

    // Deploy mock tokens
    const MockERC20 = await ethers.getContractFactory("MockERC20");
    const stakingToken = await MockERC20.deploy("Staking Token", "STK", 18);
    const rewardToken = await MockERC20.deploy("Reward Token", "RWD", 18);

    // Deploy staking pool
    const StakingPool = await ethers.getContractFactory("StakingPool");
    const stakingPool = await StakingPool.deploy(
      await stakingToken.getAddress(),
      await rewardToken.getAddress(),
      ethers.parseEther("0.1"),     // 10% APR
      7 * 24 * 60 * 60,             // 7 days lock
      ethers.parseEther("100"),     // min stake
      ethers.parseEther("1000000")  // max stake
    );

    // Mint tokens
    await stakingToken.mint(user1.address, ethers.parseEther("100000"));
    await stakingToken.mint(user2.address, ethers.parseEther("100000"));
    await stakingToken.mint(user3.address, ethers.parseEther("100000"));
    await rewardToken.mint(await stakingPool.getAddress(), ethers.parseEther("1000000"));

    // Approve
    await stakingToken.connect(user1).approve(await stakingPool.getAddress(), ethers.MaxUint256);
    await stakingToken.connect(user2).approve(await stakingPool.getAddress(), ethers.MaxUint256);
    await stakingToken.connect(user3).approve(await stakingPool.getAddress(), ethers.MaxUint256);

    return {
      stakingPool,
      stakingToken,
      rewardToken,
      owner,
      user1,
      user2,
      user3,
      rewardDistributor
    };
  }

  // ============================================
  // STAKING POOL TESTS
  // ============================================

  describe("StakingPool", function () {
    describe("Deployment", function () {
      it("Should set correct staking token", async function () {
        const { stakingPool, stakingToken } = await loadFixture(deployStakingPoolFixture);
        expect(await stakingPool.stakingToken()).to.equal(await stakingToken.getAddress());
      });

      it("Should set correct reward token", async function () {
        const { stakingPool, rewardToken } = await loadFixture(deployStakingPoolFixture);
        expect(await stakingPool.rewardToken()).to.equal(await rewardToken.getAddress());
      });

      it("Should set correct reward rate", async function () {
        const { stakingPool } = await loadFixture(deployStakingPoolFixture);
        expect(await stakingPool.rewardRate()).to.equal(ethers.parseEther("0.1"));
      });

      it("Should start with zero total staked", async function () {
        const { stakingPool } = await loadFixture(deployStakingPoolFixture);
        expect(await stakingPool.totalStaked()).to.equal(0);
      });
    });

    describe("Staking", function () {
      it("Should allow staking above minimum", async function () {
        const { stakingPool, user1 } = await loadFixture(deployStakingPoolFixture);
        const amount = ethers.parseEther("1000");

        await stakingPool.connect(user1).stake(amount);
        expect(await stakingPool.stakedBalance(user1.address)).to.equal(amount);
      });

      it("Should emit Staked event", async function () {
        const { stakingPool, user1 } = await loadFixture(deployStakingPoolFixture);
        const amount = ethers.parseEther("1000");

        await expect(stakingPool.connect(user1).stake(amount))
          .to.emit(stakingPool, "Staked")
          .withArgs(user1.address, amount);
      });

      it("Should update total staked", async function () {
        const { stakingPool, user1, user2 } = await loadFixture(deployStakingPoolFixture);

        await stakingPool.connect(user1).stake(ethers.parseEther("1000"));
        await stakingPool.connect(user2).stake(ethers.parseEther("2000"));

        expect(await stakingPool.totalStaked()).to.equal(ethers.parseEther("3000"));
      });

      it("Should reject stake below minimum", async function () {
        const { stakingPool, user1 } = await loadFixture(deployStakingPoolFixture);

        await expect(stakingPool.connect(user1).stake(ethers.parseEther("50")))
          .to.be.revertedWithCustomError(stakingPool, "StakeBelowMinimum");
      });

      it("Should reject stake above maximum", async function () {
        const { stakingPool, user1 } = await loadFixture(deployStakingPoolFixture);

        await expect(stakingPool.connect(user1).stake(ethers.parseEther("2000000")))
          .to.be.revertedWithCustomError(stakingPool, "StakeExceedsMaximum");
      });

      it("Should allow multiple stakes", async function () {
        const { stakingPool, user1 } = await loadFixture(deployStakingPoolFixture);

        await stakingPool.connect(user1).stake(ethers.parseEther("1000"));
        await stakingPool.connect(user1).stake(ethers.parseEther("2000"));

        expect(await stakingPool.stakedBalance(user1.address)).to.equal(ethers.parseEther("3000"));
      });
    });

    describe("Unstaking", function () {
      it("Should allow unstaking after lock period", async function () {
        const { stakingPool, user1 } = await loadFixture(deployStakingPoolFixture);
        const amount = ethers.parseEther("1000");

        await stakingPool.connect(user1).stake(amount);
        
        // Fast forward past lock period
        await time.increase(7 * 24 * 60 * 60 + 1);

        await stakingPool.connect(user1).unstake(amount);
        expect(await stakingPool.stakedBalance(user1.address)).to.equal(0);
      });

      it("Should apply penalty for early unstake", async function () {
        const { stakingPool, stakingToken, user1 } = await loadFixture(deployStakingPoolFixture);
        const amount = ethers.parseEther("1000");
        const initialBalance = await stakingToken.balanceOf(user1.address);

        await stakingPool.connect(user1).stake(amount);
        
        // Fast forward partial lock period
        await time.increase(3 * 24 * 60 * 60);

        await stakingPool.connect(user1).unstake(amount);

        // Should receive less than staked due to penalty
        const finalBalance = await stakingToken.balanceOf(user1.address);
        expect(finalBalance).to.be.lt(initialBalance);
      });

      it("Should emit Unstaked event", async function () {
        const { stakingPool, user1 } = await loadFixture(deployStakingPoolFixture);
        const amount = ethers.parseEther("1000");

        await stakingPool.connect(user1).stake(amount);
        await time.increase(7 * 24 * 60 * 60 + 1);

        await expect(stakingPool.connect(user1).unstake(amount))
          .to.emit(stakingPool, "Unstaked");
      });

      it("Should reject unstake exceeding staked amount", async function () {
        const { stakingPool, user1 } = await loadFixture(deployStakingPoolFixture);

        await stakingPool.connect(user1).stake(ethers.parseEther("1000"));
        await time.increase(7 * 24 * 60 * 60 + 1);

        await expect(stakingPool.connect(user1).unstake(ethers.parseEther("2000")))
          .to.be.revertedWithCustomError(stakingPool, "InsufficientStakedBalance");
      });
    });

    describe("Rewards", function () {
      it("Should accumulate rewards over time", async function () {
        const { stakingPool, user1 } = await loadFixture(deployStakingPoolFixture);

        await stakingPool.connect(user1).stake(ethers.parseEther("1000"));
        
        // Fast forward 30 days
        await time.increase(30 * 24 * 60 * 60);

        const pending = await stakingPool.pendingRewards(user1.address);
        expect(pending).to.be.gt(0);
      });

      it("Should allow claiming rewards", async function () {
        const { stakingPool, rewardToken, user1 } = await loadFixture(deployStakingPoolFixture);

        await stakingPool.connect(user1).stake(ethers.parseEther("1000"));
        await time.increase(30 * 24 * 60 * 60);

        const initialBalance = await rewardToken.balanceOf(user1.address);
        await stakingPool.connect(user1).claimRewards();
        const finalBalance = await rewardToken.balanceOf(user1.address);

        expect(finalBalance).to.be.gt(initialBalance);
      });

      it("Should emit RewardClaimed event", async function () {
        const { stakingPool, user1 } = await loadFixture(deployStakingPoolFixture);

        await stakingPool.connect(user1).stake(ethers.parseEther("1000"));
        await time.increase(30 * 24 * 60 * 60);

        await expect(stakingPool.connect(user1).claimRewards())
          .to.emit(stakingPool, "RewardClaimed");
      });

      it("Should reset pending rewards after claim", async function () {
        const { stakingPool, user1 } = await loadFixture(deployStakingPoolFixture);

        await stakingPool.connect(user1).stake(ethers.parseEther("1000"));
        await time.increase(30 * 24 * 60 * 60);

        await stakingPool.connect(user1).claimRewards();
        
        // Should be nearly zero (might have tiny amount from block time)
        const pending = await stakingPool.pendingRewards(user1.address);
        expect(pending).to.be.lt(ethers.parseEther("0.01"));
      });

      it("Should distribute rewards proportionally", async function () {
        const { stakingPool, user1, user2 } = await loadFixture(deployStakingPoolFixture);

        await stakingPool.connect(user1).stake(ethers.parseEther("1000"));
        await stakingPool.connect(user2).stake(ethers.parseEther("2000"));
        
        await time.increase(30 * 24 * 60 * 60);

        const pending1 = await stakingPool.pendingRewards(user1.address);
        const pending2 = await stakingPool.pendingRewards(user2.address);

        // User2 should have approximately 2x rewards
        const ratio = Number(pending2) / Number(pending1);
        expect(ratio).to.be.closeTo(2, 0.1);
      });
    });

    describe("Boost Multipliers", function () {
      it("Should apply boost multiplier to rewards", async function () {
        const { stakingPool, owner, user1 } = await loadFixture(deployStakingPoolFixture);

        await stakingPool.connect(user1).stake(ethers.parseEther("1000"));
        await stakingPool.connect(owner).setBoostMultiplier(user1.address, 15000); // 1.5x

        await time.increase(30 * 24 * 60 * 60);

        const pending = await stakingPool.pendingRewards(user1.address);
        // Should have boosted rewards
        expect(pending).to.be.gt(0);
      });
    });
  });

  // ============================================
  // LIQUIDITY POOL FIXTURES
  // ============================================

  async function deployLiquidityPoolFixture() {
    const [owner, user1, user2, feeRecipient] = await ethers.getSigners();

    const MockERC20 = await ethers.getContractFactory("MockERC20");
    const tokenA = await MockERC20.deploy("Token A", "TKA", 18);
    const tokenB = await MockERC20.deploy("Token B", "TKB", 18);

    const LiquidityPool = await ethers.getContractFactory("LiquidityPool");
    const pool = await LiquidityPool.deploy(
      await tokenA.getAddress(),
      await tokenB.getAddress(),
      30, // 0.3% fee
      feeRecipient.address
    );

    // Mint tokens
    await tokenA.mint(user1.address, ethers.parseEther("1000000"));
    await tokenB.mint(user1.address, ethers.parseEther("1000000"));
    await tokenA.mint(user2.address, ethers.parseEther("1000000"));
    await tokenB.mint(user2.address, ethers.parseEther("1000000"));

    // Approve
    await tokenA.connect(user1).approve(await pool.getAddress(), ethers.MaxUint256);
    await tokenB.connect(user1).approve(await pool.getAddress(), ethers.MaxUint256);
    await tokenA.connect(user2).approve(await pool.getAddress(), ethers.MaxUint256);
    await tokenB.connect(user2).approve(await pool.getAddress(), ethers.MaxUint256);

    return { pool, tokenA, tokenB, owner, user1, user2, feeRecipient };
  }

  // ============================================
  // LIQUIDITY POOL TESTS
  // ============================================

  describe("LiquidityPool", function () {
    describe("Adding Liquidity", function () {
      it("Should add initial liquidity", async function () {
        const { pool, user1 } = await loadFixture(deployLiquidityPoolFixture);

        await pool.connect(user1).addLiquidity(
          ethers.parseEther("10000"),
          ethers.parseEther("10000"),
          0
        );

        expect(await pool.totalSupply()).to.be.gt(0);
      });

      it("Should mint LP tokens to provider", async function () {
        const { pool, user1 } = await loadFixture(deployLiquidityPoolFixture);

        await pool.connect(user1).addLiquidity(
          ethers.parseEther("10000"),
          ethers.parseEther("10000"),
          0
        );

        expect(await pool.balanceOf(user1.address)).to.be.gt(0);
      });

      it("Should emit LiquidityAdded event", async function () {
        const { pool, user1 } = await loadFixture(deployLiquidityPoolFixture);

        await expect(pool.connect(user1).addLiquidity(
          ethers.parseEther("10000"),
          ethers.parseEther("10000"),
          0
        )).to.emit(pool, "LiquidityAdded");
      });

      it("Should update reserves", async function () {
        const { pool, user1 } = await loadFixture(deployLiquidityPoolFixture);
        const amount = ethers.parseEther("10000");

        await pool.connect(user1).addLiquidity(amount, amount, 0);

        expect(await pool.reserveA()).to.equal(amount);
        expect(await pool.reserveB()).to.equal(amount);
      });

      it("Should reject slippage exceeded", async function () {
        const { pool, user1 } = await loadFixture(deployLiquidityPoolFixture);

        await pool.connect(user1).addLiquidity(
          ethers.parseEther("10000"),
          ethers.parseEther("10000"),
          0
        );

        // Second add with high min liquidity
        await expect(pool.connect(user1).addLiquidity(
          ethers.parseEther("1000"),
          ethers.parseEther("1000"),
          ethers.parseEther("10000") // Too high
        )).to.be.revertedWithCustomError(pool, "SlippageExceeded");
      });
    });

    describe("Removing Liquidity", function () {
      async function addedLiquidityFixture() {
        const fixture = await loadFixture(deployLiquidityPoolFixture);
        const { pool, user1 } = fixture;

        await pool.connect(user1).addLiquidity(
          ethers.parseEther("10000"),
          ethers.parseEther("10000"),
          0
        );

        return fixture;
      }

      it("Should remove liquidity", async function () {
        const { pool, user1 } = await addedLiquidityFixture();
        const lpBalance = await pool.balanceOf(user1.address);

        await pool.connect(user1).removeLiquidity(lpBalance, 0, 0);

        expect(await pool.balanceOf(user1.address)).to.equal(0);
      });

      it("Should return tokens to provider", async function () {
        const { pool, tokenA, tokenB, user1 } = await addedLiquidityFixture();
        const lpBalance = await pool.balanceOf(user1.address);
        const initialA = await tokenA.balanceOf(user1.address);
        const initialB = await tokenB.balanceOf(user1.address);

        await pool.connect(user1).removeLiquidity(lpBalance, 0, 0);

        expect(await tokenA.balanceOf(user1.address)).to.be.gt(initialA);
        expect(await tokenB.balanceOf(user1.address)).to.be.gt(initialB);
      });

      it("Should burn LP tokens", async function () {
        const { pool, user1 } = await addedLiquidityFixture();
        const lpBalance = await pool.balanceOf(user1.address);
        const totalBefore = await pool.totalSupply();

        await pool.connect(user1).removeLiquidity(lpBalance / 2n, 0, 0);

        expect(await pool.totalSupply()).to.be.lt(totalBefore);
      });

      it("Should emit LiquidityRemoved event", async function () {
        const { pool, user1 } = await addedLiquidityFixture();
        const lpBalance = await pool.balanceOf(user1.address);

        await expect(pool.connect(user1).removeLiquidity(lpBalance, 0, 0))
          .to.emit(pool, "LiquidityRemoved");
      });
    });

    describe("Swapping", function () {
      async function poolWithLiquidityFixture() {
        const fixture = await loadFixture(deployLiquidityPoolFixture);
        const { pool, user1 } = fixture;

        await pool.connect(user1).addLiquidity(
          ethers.parseEther("100000"),
          ethers.parseEther("100000"),
          0
        );

        return fixture;
      }

      it("Should swap A for B", async function () {
        const { pool, tokenB, user2 } = await poolWithLiquidityFixture();
        const initialB = await tokenB.balanceOf(user2.address);

        await pool.connect(user2).swapAForB(ethers.parseEther("1000"), 0);

        expect(await tokenB.balanceOf(user2.address)).to.be.gt(initialB);
      });

      it("Should swap B for A", async function () {
        const { pool, tokenA, user2 } = await poolWithLiquidityFixture();
        const initialA = await tokenA.balanceOf(user2.address);

        await pool.connect(user2).swapBForA(ethers.parseEther("1000"), 0);

        expect(await tokenA.balanceOf(user2.address)).to.be.gt(initialA);
      });

      it("Should apply swap fee", async function () {
        const { pool, tokenB, user2 } = await poolWithLiquidityFixture();

        const amountOut = await pool.getAmountOut(ethers.parseEther("1000"), true);
        
        // With 0.3% fee, should get less than 1000
        expect(amountOut).to.be.lt(ethers.parseEther("1000"));
      });

      it("Should emit Swap event", async function () {
        const { pool, user2 } = await poolWithLiquidityFixture();

        await expect(pool.connect(user2).swapAForB(ethers.parseEther("1000"), 0))
          .to.emit(pool, "Swap");
      });

      it("Should update reserves after swap", async function () {
        const { pool, user2 } = await poolWithLiquidityFixture();
        const reserveBBefore = await pool.reserveB();

        await pool.connect(user2).swapAForB(ethers.parseEther("1000"), 0);

        expect(await pool.reserveB()).to.be.lt(reserveBBefore);
      });

      it("Should reject swap with slippage exceeded", async function () {
        const { pool, user2 } = await poolWithLiquidityFixture();

        await expect(pool.connect(user2).swapAForB(
          ethers.parseEther("1000"),
          ethers.parseEther("1000") // Too high min out
        )).to.be.revertedWithCustomError(pool, "SlippageExceeded");
      });

      it("Should maintain constant product (approximately)", async function () {
        const { pool, user2 } = await poolWithLiquidityFixture();
        
        const k1 = (await pool.reserveA()) * (await pool.reserveB());

        await pool.connect(user2).swapAForB(ethers.parseEther("1000"), 0);

        const k2 = (await pool.reserveA()) * (await pool.reserveB());

        // K should stay same or increase (due to fees)
        expect(k2).to.be.gte(k1);
      });
    });

    describe("Price Queries", function () {
      async function poolWithLiquidityFixture() {
        const fixture = await loadFixture(deployLiquidityPoolFixture);
        const { pool, user1 } = fixture;

        await pool.connect(user1).addLiquidity(
          ethers.parseEther("100000"),
          ethers.parseEther("50000"),
          0
        );

        return fixture;
      }

      it("Should return correct price A to B", async function () {
        const { pool } = await poolWithLiquidityFixture();
        
        const price = await pool.getPrice(true);
        // With 2:1 ratio, price should be ~0.5
        expect(price).to.be.closeTo(ethers.parseEther("0.5"), ethers.parseEther("0.01"));
      });

      it("Should return correct price B to A", async function () {
        const { pool } = await poolWithLiquidityFixture();
        
        const price = await pool.getPrice(false);
        // With 2:1 ratio, price should be ~2
        expect(price).to.be.closeTo(ethers.parseEther("2"), ethers.parseEther("0.01"));
      });

      it("Should update price after swap", async function () {
        const { pool, user2 } = await poolWithLiquidityFixture();
        
        const priceBefore = await pool.getPrice(true);

        await pool.connect(user2).swapAForB(ethers.parseEther("10000"), 0);

        const priceAfter = await pool.getPrice(true);

        expect(priceAfter).to.not.equal(priceBefore);
      });
    });
  });

  // ============================================
  // VAULT TESTS
  // ============================================

  describe("Vault", function () {
    async function deployVaultFixture() {
      const [owner, user1, user2] = await ethers.getSigners();

      const MockERC20 = await ethers.getContractFactory("MockERC20");
      const asset = await MockERC20.deploy("Vault Asset", "VAST", 18);

      const Vault = await ethers.getContractFactory("Vault");
      const vault = await Vault.deploy(
        await asset.getAddress(),
        "Vault Shares",
        "vSHR",
        100,  // 1% performance fee
        50    // 0.5% management fee
      );

      await asset.mint(user1.address, ethers.parseEther("100000"));
      await asset.mint(user2.address, ethers.parseEther("100000"));
      await asset.connect(user1).approve(await vault.getAddress(), ethers.MaxUint256);
      await asset.connect(user2).approve(await vault.getAddress(), ethers.MaxUint256);

      return { vault, asset, owner, user1, user2 };
    }

    describe("Deposits", function () {
      it("Should accept deposits", async function () {
        const { vault, user1 } = await loadFixture(deployVaultFixture);

        await vault.connect(user1).deposit(ethers.parseEther("1000"));
        expect(await vault.totalAssets()).to.equal(ethers.parseEther("1000"));
      });

      it("Should mint shares to depositor", async function () {
        const { vault, user1 } = await loadFixture(deployVaultFixture);

        await vault.connect(user1).deposit(ethers.parseEther("1000"));
        expect(await vault.balanceOf(user1.address)).to.be.gt(0);
      });

      it("Should emit Deposit event", async function () {
        const { vault, user1 } = await loadFixture(deployVaultFixture);

        await expect(vault.connect(user1).deposit(ethers.parseEther("1000")))
          .to.emit(vault, "Deposit");
      });

      it("Should give 1:1 shares on first deposit", async function () {
        const { vault, user1 } = await loadFixture(deployVaultFixture);
        const amount = ethers.parseEther("1000");

        await vault.connect(user1).deposit(amount);
        expect(await vault.balanceOf(user1.address)).to.equal(amount);
      });
    });

    describe("Withdrawals", function () {
      async function vaultWithDepositsFixture() {
        const fixture = await loadFixture(deployVaultFixture);
        const { vault, user1 } = fixture;

        await vault.connect(user1).deposit(ethers.parseEther("10000"));

        return fixture;
      }

      it("Should allow withdrawals", async function () {
        const { vault, user1 } = await vaultWithDepositsFixture();
        const shares = await vault.balanceOf(user1.address);

        await vault.connect(user1).withdraw(shares);
        expect(await vault.balanceOf(user1.address)).to.equal(0);
      });

      it("Should return assets", async function () {
        const { vault, asset, user1 } = await vaultWithDepositsFixture();
        const shares = await vault.balanceOf(user1.address);
        const initialBalance = await asset.balanceOf(user1.address);

        await vault.connect(user1).withdraw(shares);

        expect(await asset.balanceOf(user1.address)).to.be.gt(initialBalance);
      });

      it("Should emit Withdraw event", async function () {
        const { vault, user1 } = await vaultWithDepositsFixture();
        const shares = await vault.balanceOf(user1.address);

        await expect(vault.connect(user1).withdraw(shares))
          .to.emit(vault, "Withdraw");
      });
    });

    describe("Share Calculations", function () {
      it("Should preview deposit correctly", async function () {
        const { vault, user1 } = await loadFixture(deployVaultFixture);
        const amount = ethers.parseEther("1000");

        const preview = await vault.previewDeposit(amount);
        await vault.connect(user1).deposit(amount);

        expect(await vault.balanceOf(user1.address)).to.equal(preview);
      });

      it("Should preview withdraw correctly", async function () {
        const { vault, user1 } = await loadFixture(deployVaultFixture);
        await vault.connect(user1).deposit(ethers.parseEther("1000"));

        const shares = await vault.balanceOf(user1.address);
        const preview = await vault.previewWithdraw(shares);

        expect(preview).to.be.gt(0);
      });

      it("Should convert assets to shares", async function () {
        const { vault, user1 } = await loadFixture(deployVaultFixture);
        await vault.connect(user1).deposit(ethers.parseEther("1000"));

        const shares = await vault.convertToShares(ethers.parseEther("100"));
        expect(shares).to.be.gt(0);
      });

      it("Should convert shares to assets", async function () {
        const { vault, user1 } = await loadFixture(deployVaultFixture);
        await vault.connect(user1).deposit(ethers.parseEther("1000"));

        const assets = await vault.convertToAssets(ethers.parseEther("100"));
        expect(assets).to.be.gt(0);
      });
    });

    describe("Yield", function () {
      it("Should increase share value when yield added", async function () {
        const { vault, asset, user1 } = await loadFixture(deployVaultFixture);
        await vault.connect(user1).deposit(ethers.parseEther("1000"));

        const valuePerShareBefore = await vault.convertToAssets(ethers.parseEther("1"));

        // Simulate yield
        await asset.mint(await vault.getAddress(), ethers.parseEther("100"));

        const valuePerShareAfter = await vault.convertToAssets(ethers.parseEther("1"));

        expect(valuePerShareAfter).to.be.gt(valuePerShareBefore);
      });
    });
  });

  // ============================================
  // INTEGRATION TESTS
  // ============================================

  describe("DeFi Integration", function () {
    it("Should handle complex multi-user staking scenarios", async function () {
      const { stakingPool, user1, user2, user3 } = await loadFixture(deployStakingPoolFixture);

      // Multiple users stake
      await stakingPool.connect(user1).stake(ethers.parseEther("10000"));
      await stakingPool.connect(user2).stake(ethers.parseEther("20000"));
      await stakingPool.connect(user3).stake(ethers.parseEther("30000"));

      // Time passes
      await time.increase(30 * 24 * 60 * 60);

      // All claim
      await stakingPool.connect(user1).claimRewards();
      await stakingPool.connect(user2).claimRewards();
      await stakingPool.connect(user3).claimRewards();

      // Verify total staked
      expect(await stakingPool.totalStaked()).to.equal(ethers.parseEther("60000"));
    });

    it("Should handle repeated add/remove liquidity", async function () {
      const { pool, user1 } = await loadFixture(deployLiquidityPoolFixture);
      const amount = ethers.parseEther("10000");

      for (let i = 0; i < 5; i++) {
        await pool.connect(user1).addLiquidity(amount, amount, 0);
        const balance = await pool.balanceOf(user1.address);
        await pool.connect(user1).removeLiquidity(balance / 2n, 0, 0);
      }

      expect(await pool.totalSupply()).to.be.gt(0);
    });

    it("Should handle high-frequency swaps", async function () {
      const fixture = await loadFixture(deployLiquidityPoolFixture);
      const { pool, user1, user2 } = fixture;

      await pool.connect(user1).addLiquidity(
        ethers.parseEther("100000"),
        ethers.parseEther("100000"),
        0
      );

      // Rapid swaps
      for (let i = 0; i < 10; i++) {
        await pool.connect(user2).swapAForB(ethers.parseEther("100"), 0);
        await pool.connect(user2).swapBForA(ethers.parseEther("90"), 0);
      }

      // Pool should still be functional
      const reserveA = await pool.reserveA();
      const reserveB = await pool.reserveB();
      expect(reserveA).to.be.gt(0);
      expect(reserveB).to.be.gt(0);
    });
  });
});
