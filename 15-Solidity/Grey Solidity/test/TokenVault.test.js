const { expect } = require("chai");
const { ethers } = require("hardhat");

describe("TokenVault", function () {
  let accessControl;
  let tokenVault;
  let mockToken;
  let owner;
  let user1;
  let user2;
  let nonUser;
  let ADMIN_ROLE;
  let USER_ROLE;

  const INITIAL_SUPPLY = ethers.parseEther("1000000");
  const DEPOSIT_AMOUNT = ethers.parseEther("1000");

  beforeEach(async function () {
    [owner, user1, user2, nonUser] = await ethers.getSigners();

    // Deploy AccessControlManager
    const AccessControlManager = await ethers.getContractFactory("AccessControlManager");
    accessControl = await AccessControlManager.deploy();
    await accessControl.waitForDeployment();

    ADMIN_ROLE = await accessControl.ADMIN_ROLE();
    USER_ROLE = await accessControl.USER_ROLE();

    // Grant USER_ROLE to user1 and user2
    await accessControl.grantRole(USER_ROLE, user1.address);
    await accessControl.grantRole(USER_ROLE, user2.address);

    // Deploy TokenVault
    const TokenVault = await ethers.getContractFactory("TokenVault");
    tokenVault = await TokenVault.deploy(await accessControl.getAddress());
    await tokenVault.waitForDeployment();

    // Deploy MockERC20
    const MockERC20 = await ethers.getContractFactory("MockERC20");
    mockToken = await MockERC20.deploy("Mock Token", "MTK", 18);
    await mockToken.waitForDeployment();

    // Mint tokens to users
    await mockToken.mint(user1.address, INITIAL_SUPPLY);
    await mockToken.mint(user2.address, INITIAL_SUPPLY);
    await mockToken.mint(owner.address, INITIAL_SUPPLY);
  });

  describe("Deployment", function () {
    it("Should set correct access control reference", async function () {
      expect(await tokenVault.accessControl()).to.equal(await accessControl.getAddress());
    });

    it("Should revert with zero address for access control", async function () {
      const TokenVault = await ethers.getContractFactory("TokenVault");
      await expect(TokenVault.deploy(ethers.ZeroAddress))
        .to.be.revertedWith("TokenVault: invalid access control address");
    });
  });

  describe("Deposits", function () {
    beforeEach(async function () {
      // Approve vault to spend tokens
      await mockToken.connect(user1).approve(await tokenVault.getAddress(), INITIAL_SUPPLY);
    });

    it("Should allow user to deposit tokens", async function () {
      await expect(tokenVault.connect(user1).deposit(await mockToken.getAddress(), DEPOSIT_AMOUNT))
        .to.emit(tokenVault, "Deposited")
        .withArgs(await mockToken.getAddress(), user1.address, DEPOSIT_AMOUNT);

      expect(await tokenVault.balanceOf(await mockToken.getAddress(), user1.address))
        .to.equal(DEPOSIT_AMOUNT);
      expect(await tokenVault.totalDeposited(await mockToken.getAddress()))
        .to.equal(DEPOSIT_AMOUNT);
    });

    it("Should allow admin to deposit tokens", async function () {
      await mockToken.connect(owner).approve(await tokenVault.getAddress(), DEPOSIT_AMOUNT);
      
      await expect(tokenVault.connect(owner).deposit(await mockToken.getAddress(), DEPOSIT_AMOUNT))
        .to.emit(tokenVault, "Deposited");
    });

    it("Should revert when non-user tries to deposit", async function () {
      await mockToken.mint(nonUser.address, DEPOSIT_AMOUNT);
      await mockToken.connect(nonUser).approve(await tokenVault.getAddress(), DEPOSIT_AMOUNT);

      await expect(
        tokenVault.connect(nonUser).deposit(await mockToken.getAddress(), DEPOSIT_AMOUNT)
      ).to.be.revertedWith("TokenVault: caller is not a user or admin");
    });

    it("Should revert with zero amount", async function () {
      await expect(
        tokenVault.connect(user1).deposit(await mockToken.getAddress(), 0)
      ).to.be.revertedWithCustomError(tokenVault, "ZeroAmount");
    });

    it("Should revert with zero token address", async function () {
      await expect(
        tokenVault.connect(user1).deposit(ethers.ZeroAddress, DEPOSIT_AMOUNT)
      ).to.be.revertedWithCustomError(tokenVault, "InvalidToken");
    });

    it("Should track multiple deposits from same user", async function () {
      await tokenVault.connect(user1).deposit(await mockToken.getAddress(), DEPOSIT_AMOUNT);
      await tokenVault.connect(user1).deposit(await mockToken.getAddress(), DEPOSIT_AMOUNT);

      expect(await tokenVault.balanceOf(await mockToken.getAddress(), user1.address))
        .to.equal(DEPOSIT_AMOUNT * 2n);
    });

    it("Should track supported tokens", async function () {
      await tokenVault.connect(user1).deposit(await mockToken.getAddress(), DEPOSIT_AMOUNT);

      const supportedTokens = await tokenVault.getSupportedTokens();
      expect(supportedTokens.length).to.equal(1);
      expect(supportedTokens[0]).to.equal(await mockToken.getAddress());
    });
  });

  describe("Withdrawals", function () {
    beforeEach(async function () {
      await mockToken.connect(user1).approve(await tokenVault.getAddress(), INITIAL_SUPPLY);
      await tokenVault.connect(user1).deposit(await mockToken.getAddress(), DEPOSIT_AMOUNT);
    });

    it("Should allow user to withdraw their tokens", async function () {
      const withdrawAmount = ethers.parseEther("500");
      const balanceBefore = await mockToken.balanceOf(user1.address);

      await expect(tokenVault.connect(user1).withdraw(await mockToken.getAddress(), withdrawAmount))
        .to.emit(tokenVault, "Withdrawn")
        .withArgs(await mockToken.getAddress(), user1.address, withdrawAmount);

      expect(await tokenVault.balanceOf(await mockToken.getAddress(), user1.address))
        .to.equal(DEPOSIT_AMOUNT - withdrawAmount);
      expect(await mockToken.balanceOf(user1.address))
        .to.equal(balanceBefore + withdrawAmount);
    });

    it("Should revert when withdrawing more than balance", async function () {
      const excessAmount = DEPOSIT_AMOUNT + ethers.parseEther("1");

      await expect(
        tokenVault.connect(user1).withdraw(await mockToken.getAddress(), excessAmount)
      ).to.be.revertedWithCustomError(tokenVault, "InsufficientBalance");
    });

    it("Should revert when non-user tries to withdraw", async function () {
      await expect(
        tokenVault.connect(nonUser).withdraw(await mockToken.getAddress(), DEPOSIT_AMOUNT)
      ).to.be.revertedWith("TokenVault: caller is not a user or admin");
    });

    it("Should revert with zero amount", async function () {
      await expect(
        tokenVault.connect(user1).withdraw(await mockToken.getAddress(), 0)
      ).to.be.revertedWithCustomError(tokenVault, "ZeroAmount");
    });

    it("Should allow full withdrawal", async function () {
      await tokenVault.connect(user1).withdraw(await mockToken.getAddress(), DEPOSIT_AMOUNT);

      expect(await tokenVault.balanceOf(await mockToken.getAddress(), user1.address))
        .to.equal(0);
      expect(await tokenVault.totalDeposited(await mockToken.getAddress()))
        .to.equal(0);
    });
  });

  describe("Admin Sweep", function () {
    beforeEach(async function () {
      await mockToken.connect(user1).approve(await tokenVault.getAddress(), INITIAL_SUPPLY);
      await mockToken.connect(user2).approve(await tokenVault.getAddress(), INITIAL_SUPPLY);
      
      await tokenVault.connect(user1).deposit(await mockToken.getAddress(), DEPOSIT_AMOUNT);
      await tokenVault.connect(user2).deposit(await mockToken.getAddress(), DEPOSIT_AMOUNT);
    });

    it("Should allow admin to sweep all tokens", async function () {
      const recipient = owner.address;
      const totalAmount = DEPOSIT_AMOUNT * 2n;

      await expect(tokenVault.connect(owner).sweep(await mockToken.getAddress(), recipient))
        .to.emit(tokenVault, "Swept")
        .withArgs(await mockToken.getAddress(), recipient, totalAmount);

      expect(await tokenVault.vaultBalance(await mockToken.getAddress())).to.equal(0);
      expect(await tokenVault.totalDeposited(await mockToken.getAddress())).to.equal(0);
    });

    it("Should revert when non-admin tries to sweep", async function () {
      await expect(
        tokenVault.connect(user1).sweep(await mockToken.getAddress(), user1.address)
      ).to.be.revertedWith("TokenVault: caller is not an admin");
    });

    it("Should revert with zero recipient address", async function () {
      await expect(
        tokenVault.connect(owner).sweep(await mockToken.getAddress(), ethers.ZeroAddress)
      ).to.be.revertedWithCustomError(tokenVault, "InvalidRecipient");
    });

    it("Should revert when vault has no balance", async function () {
      // First sweep everything
      await tokenVault.connect(owner).sweep(await mockToken.getAddress(), owner.address);

      // Try to sweep again
      await expect(
        tokenVault.connect(owner).sweep(await mockToken.getAddress(), owner.address)
      ).to.be.revertedWithCustomError(tokenVault, "ZeroAmount");
    });
  });

  describe("View Functions", function () {
    beforeEach(async function () {
      await mockToken.connect(user1).approve(await tokenVault.getAddress(), INITIAL_SUPPLY);
      await tokenVault.connect(user1).deposit(await mockToken.getAddress(), DEPOSIT_AMOUNT);
    });

    it("Should return correct vault balance", async function () {
      expect(await tokenVault.vaultBalance(await mockToken.getAddress()))
        .to.equal(DEPOSIT_AMOUNT);
    });

    it("Should return correct user balance", async function () {
      expect(await tokenVault.balanceOf(await mockToken.getAddress(), user1.address))
        .to.equal(DEPOSIT_AMOUNT);
    });

    it("Should return correct total deposited", async function () {
      expect(await tokenVault.totalDeposited(await mockToken.getAddress()))
        .to.equal(DEPOSIT_AMOUNT);
    });
  });

  describe("Multiple Tokens", function () {
    let secondToken;

    beforeEach(async function () {
      const MockERC20 = await ethers.getContractFactory("MockERC20");
      secondToken = await MockERC20.deploy("Second Token", "STK", 18);
      await secondToken.waitForDeployment();

      await secondToken.mint(user1.address, INITIAL_SUPPLY);
      
      await mockToken.connect(user1).approve(await tokenVault.getAddress(), INITIAL_SUPPLY);
      await secondToken.connect(user1).approve(await tokenVault.getAddress(), INITIAL_SUPPLY);
    });

    it("Should track multiple tokens independently", async function () {
      await tokenVault.connect(user1).deposit(await mockToken.getAddress(), DEPOSIT_AMOUNT);
      await tokenVault.connect(user1).deposit(await secondToken.getAddress(), DEPOSIT_AMOUNT * 2n);

      expect(await tokenVault.balanceOf(await mockToken.getAddress(), user1.address))
        .to.equal(DEPOSIT_AMOUNT);
      expect(await tokenVault.balanceOf(await secondToken.getAddress(), user1.address))
        .to.equal(DEPOSIT_AMOUNT * 2n);

      const supportedTokens = await tokenVault.getSupportedTokens();
      expect(supportedTokens.length).to.equal(2);
    });
  });
});
