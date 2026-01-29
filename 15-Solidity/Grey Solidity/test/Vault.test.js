const { expect } = require("chai");
const { ethers } = require("hardhat");
const { time } = require("@nomicfoundation/hardhat-network-helpers");

describe("Vault", function () {
  let vault;
  let token;
  let owner, user1, user2;

  const PERFORMANCE_FEE = 100; // 1%
  const MANAGEMENT_FEE = 50; // 0.5%
  const DEPOSIT_AMOUNT = ethers.parseEther("1000");

  beforeEach(async function () {
    [owner, user1, user2] = await ethers.getSigners();

    // Deploy mock token
    const MockERC20 = await ethers.getContractFactory("MockERC20");
    token = await MockERC20.deploy("Test Token", "TEST", 18);

    // Deploy vault
    const Vault = await ethers.getContractFactory("Vault");
    vault = await Vault.deploy(
      await token.getAddress(),
      "Grey Vault Shares",
      "gGREY",
      PERFORMANCE_FEE,
      MANAGEMENT_FEE
    );

    // Mint and approve tokens
    await token.mint(user1.address, ethers.parseEther("100000"));
    await token.mint(user2.address, ethers.parseEther("100000"));
    await token.connect(user1).approve(await vault.getAddress(), ethers.MaxUint256);
    await token.connect(user2).approve(await vault.getAddress(), ethers.MaxUint256);
  });

  describe("Deployment", function () {
    it("Should set correct name and symbol", async function () {
      expect(await vault.name()).to.equal("Grey Vault Shares");
      expect(await vault.symbol()).to.equal("gGREY");
    });

    it("Should set correct asset", async function () {
      expect(await vault.asset()).to.equal(await token.getAddress());
    });

    it("Should set correct fees", async function () {
      expect(await vault.performanceFee()).to.equal(PERFORMANCE_FEE);
      expect(await vault.managementFee()).to.equal(MANAGEMENT_FEE);
    });

    it("Should start with zero total assets", async function () {
      expect(await vault.totalAssets()).to.equal(0);
    });
  });

  describe("Deposits", function () {
    it("Should accept deposits", async function () {
      await vault.connect(user1).deposit(DEPOSIT_AMOUNT);
      expect(await vault.totalAssets()).to.equal(DEPOSIT_AMOUNT);
    });

    it("Should mint shares to depositor", async function () {
      await vault.connect(user1).deposit(DEPOSIT_AMOUNT);
      expect(await vault.balanceOf(user1.address)).to.be.gt(0);
    });

    it("Should emit Deposit event", async function () {
      await expect(vault.connect(user1).deposit(DEPOSIT_AMOUNT))
        .to.emit(vault, "Deposit");
    });

    it("Should transfer tokens to vault", async function () {
      const vaultAddress = await vault.getAddress();
      const initialBalance = await token.balanceOf(vaultAddress);

      await vault.connect(user1).deposit(DEPOSIT_AMOUNT);

      const finalBalance = await token.balanceOf(vaultAddress);
      expect(finalBalance - initialBalance).to.equal(DEPOSIT_AMOUNT);
    });

    it("Should give 1:1 shares on first deposit", async function () {
      await vault.connect(user1).deposit(DEPOSIT_AMOUNT);
      expect(await vault.balanceOf(user1.address)).to.equal(DEPOSIT_AMOUNT);
    });

    it("Should maintain proportional shares for multiple depositors", async function () {
      await vault.connect(user1).deposit(DEPOSIT_AMOUNT);
      await vault.connect(user2).deposit(DEPOSIT_AMOUNT);

      const user1Shares = await vault.balanceOf(user1.address);
      const user2Shares = await vault.balanceOf(user2.address);

      expect(user1Shares).to.equal(user2Shares);
    });
  });

  describe("Withdrawals", function () {
    beforeEach(async function () {
      await vault.connect(user1).deposit(DEPOSIT_AMOUNT);
    });

    it("Should allow withdrawals", async function () {
      const shares = await vault.balanceOf(user1.address);
      await vault.connect(user1).withdraw(shares);
      expect(await vault.balanceOf(user1.address)).to.equal(0);
    });

    it("Should return assets to user", async function () {
      const initialBalance = await token.balanceOf(user1.address);
      const shares = await vault.balanceOf(user1.address);

      await vault.connect(user1).withdraw(shares);

      const finalBalance = await token.balanceOf(user1.address);
      expect(finalBalance).to.be.gt(initialBalance);
    });

    it("Should emit Withdraw event", async function () {
      const shares = await vault.balanceOf(user1.address);
      await expect(vault.connect(user1).withdraw(shares))
        .to.emit(vault, "Withdraw");
    });

    it("Should reject withdrawal exceeding balance", async function () {
      const shares = await vault.balanceOf(user1.address);
      await expect(
        vault.connect(user1).withdraw(shares * 2n)
      ).to.be.reverted;
    });

    it("Should reject zero withdrawal", async function () {
      await expect(
        vault.connect(user1).withdraw(0)
      ).to.be.revertedWithCustomError(vault, "ZeroAmount");
    });
  });

  describe("Share Conversion", function () {
    beforeEach(async function () {
      await vault.connect(user1).deposit(DEPOSIT_AMOUNT);
    });

    it("Should correctly convert assets to shares", async function () {
      const assets = ethers.parseEther("100");
      const shares = await vault.convertToShares(assets);
      expect(shares).to.be.gt(0);
    });

    it("Should correctly convert shares to assets", async function () {
      const shares = ethers.parseEther("100");
      const assets = await vault.convertToAssets(shares);
      expect(assets).to.be.gt(0);
    });

    it("Should preview deposit correctly", async function () {
      const assets = ethers.parseEther("100");
      const previewedShares = await vault.previewDeposit(assets);
      
      await vault.connect(user2).deposit(assets);
      const actualShares = await vault.balanceOf(user2.address);
      
      expect(previewedShares).to.equal(actualShares);
    });

    it("Should preview withdraw correctly", async function () {
      const shares = await vault.balanceOf(user1.address);
      const previewedAssets = await vault.previewWithdraw(shares);
      
      const initialBalance = await token.balanceOf(user1.address);
      await vault.connect(user1).withdraw(shares);
      const finalBalance = await token.balanceOf(user1.address);
      
      const actualAssets = finalBalance - initialBalance;
      expect(previewedAssets).to.be.closeTo(actualAssets, ethers.parseEther("0.01"));
    });
  });

  describe("Yield Generation", function () {
    beforeEach(async function () {
      await vault.connect(user1).deposit(DEPOSIT_AMOUNT);
    });

    it("Should increase share value when assets added", async function () {
      const sharesBefore = await vault.convertToAssets(ethers.parseEther("1"));

      // Simulate yield by adding tokens directly
      await token.mint(await vault.getAddress(), ethers.parseEther("100"));

      const sharesAfter = await vault.convertToAssets(ethers.parseEther("1"));
      expect(sharesAfter).to.be.gt(sharesBefore);
    });

    it("Should distribute yield proportionally", async function () {
      await vault.connect(user2).deposit(DEPOSIT_AMOUNT);

      // Simulate yield
      await token.mint(await vault.getAddress(), ethers.parseEther("200"));

      const user1Assets = await vault.convertToAssets(await vault.balanceOf(user1.address));
      const user2Assets = await vault.convertToAssets(await vault.balanceOf(user2.address));

      // Both should have approximately equal share of yield
      expect(user1Assets).to.be.closeTo(user2Assets, ethers.parseEther("1"));
    });
  });

  describe("Fee Calculations", function () {
    it("Should calculate performance fee correctly", async function () {
      await vault.connect(user1).deposit(DEPOSIT_AMOUNT);
      
      // Simulate significant yield
      await token.mint(await vault.getAddress(), ethers.parseEther("500"));

      const shares = await vault.balanceOf(user1.address);
      const expectedAssets = await vault.convertToAssets(shares);

      // User should receive less than total assets due to fees
      expect(expectedAssets).to.be.lt(DEPOSIT_AMOUNT + ethers.parseEther("500"));
    });
  });

  describe("Admin Functions", function () {
    it("Should allow owner to update performance fee", async function () {
      await vault.setPerformanceFee(200);
      expect(await vault.performanceFee()).to.equal(200);
    });

    it("Should reject fee above maximum", async function () {
      await expect(vault.setPerformanceFee(3000))
        .to.be.revertedWith("Vault: fee too high");
    });

    it("Should allow owner to update management fee", async function () {
      await vault.setManagementFee(100);
      expect(await vault.managementFee()).to.equal(100);
    });

    it("Should reject non-owner fee updates", async function () {
      await expect(
        vault.connect(user1).setPerformanceFee(200)
      ).to.be.revertedWithCustomError(vault, "OwnableUnauthorizedAccount");
    });

    it("Should allow pausing", async function () {
      await vault.pause();
      expect(await vault.paused()).to.be.true;
    });

    it("Should prevent deposits when paused", async function () {
      await vault.pause();
      await expect(
        vault.connect(user1).deposit(DEPOSIT_AMOUNT)
      ).to.be.revertedWithCustomError(vault, "EnforcedPause");
    });

    it("Should allow withdrawals when paused", async function () {
      await vault.connect(user1).deposit(DEPOSIT_AMOUNT);
      await vault.pause();

      const shares = await vault.balanceOf(user1.address);
      // Withdrawals should still work when paused for user safety
      await expect(vault.connect(user1).withdraw(shares)).to.not.be.reverted;
    });
  });

  describe("Edge Cases", function () {
    it("Should handle very small deposits", async function () {
      const smallAmount = 1000n; // 1000 wei
      await token.mint(user1.address, smallAmount);

      await vault.connect(user1).deposit(smallAmount);
      expect(await vault.balanceOf(user1.address)).to.be.gt(0);
    });

    it("Should handle very large deposits", async function () {
      const largeAmount = ethers.parseEther("1000000000"); // 1 billion
      await token.mint(user1.address, largeAmount);

      await vault.connect(user1).deposit(largeAmount);
      expect(await vault.totalAssets()).to.equal(largeAmount);
    });

    it("Should handle withdrawal of all shares", async function () {
      await vault.connect(user1).deposit(DEPOSIT_AMOUNT);

      const allShares = await vault.balanceOf(user1.address);
      await vault.connect(user1).withdraw(allShares);

      expect(await vault.balanceOf(user1.address)).to.equal(0);
    });

    it("Should handle multiple deposits and withdrawals", async function () {
      // Multiple deposits
      await vault.connect(user1).deposit(ethers.parseEther("100"));
      await vault.connect(user1).deposit(ethers.parseEther("200"));
      await vault.connect(user1).deposit(ethers.parseEther("300"));

      // Partial withdrawal
      await vault.connect(user1).withdraw(ethers.parseEther("100"));

      const remainingShares = await vault.balanceOf(user1.address);
      expect(remainingShares).to.be.gt(0);

      // Full withdrawal
      await vault.connect(user1).withdraw(remainingShares);
      expect(await vault.balanceOf(user1.address)).to.equal(0);
    });
  });
});
