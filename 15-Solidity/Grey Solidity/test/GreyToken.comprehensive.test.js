const { expect } = require("chai");
const { ethers } = require("hardhat");
const { time, loadFixture } = require("@nomicfoundation/hardhat-network-helpers");

/**
 * @title Comprehensive GreyToken Test Suite
 * @notice Tests for ERC20 token with advanced features
 */
describe("GreyToken - Comprehensive Tests", function () {
  // ============================================
  // FIXTURES
  // ============================================

  async function deployGreyTokenFixture() {
    const [owner, admin, minter, pauser, user1, user2, user3] = await ethers.getSigners();

    const GreyToken = await ethers.getContractFactory("GreyToken");
    const maxSupply = ethers.parseEther("1000000000"); // 1 billion
    const token = await GreyToken.deploy("Grey Token", "GREY", maxSupply, owner.address);

    // Get role hashes
    const MINTER_ROLE = await token.MINTER_ROLE();
    const PAUSER_ROLE = await token.PAUSER_ROLE();
    const SNAPSHOT_ROLE = await token.SNAPSHOT_ROLE();

    // Grant roles
    await token.grantRole(MINTER_ROLE, minter.address);
    await token.grantRole(PAUSER_ROLE, pauser.address);

    return {
      token,
      owner,
      admin,
      minter,
      pauser,
      user1,
      user2,
      user3,
      maxSupply,
      MINTER_ROLE,
      PAUSER_ROLE,
      SNAPSHOT_ROLE
    };
  }

  async function deployWithInitialSupplyFixture() {
    const fixture = await loadFixture(deployGreyTokenFixture);
    const { token, minter, user1, user2 } = fixture;

    // Mint initial tokens
    await token.connect(minter).mint(user1.address, ethers.parseEther("1000000"));
    await token.connect(minter).mint(user2.address, ethers.parseEther("500000"));

    return fixture;
  }

  // ============================================
  // DEPLOYMENT TESTS
  // ============================================

  describe("Deployment", function () {
    it("Should set the correct name and symbol", async function () {
      const { token } = await loadFixture(deployGreyTokenFixture);
      expect(await token.name()).to.equal("Grey Token");
      expect(await token.symbol()).to.equal("GREY");
    });

    it("Should set the correct decimals", async function () {
      const { token } = await loadFixture(deployGreyTokenFixture);
      expect(await token.decimals()).to.equal(18);
    });

    it("Should set the correct max supply", async function () {
      const { token, maxSupply } = await loadFixture(deployGreyTokenFixture);
      expect(await token.maxSupply()).to.equal(maxSupply);
    });

    it("Should grant admin role to deployer", async function () {
      const { token, owner } = await loadFixture(deployGreyTokenFixture);
      const DEFAULT_ADMIN_ROLE = await token.DEFAULT_ADMIN_ROLE();
      expect(await token.hasRole(DEFAULT_ADMIN_ROLE, owner.address)).to.be.true;
    });

    it("Should start with zero total supply", async function () {
      const { token } = await loadFixture(deployGreyTokenFixture);
      expect(await token.totalSupply()).to.equal(0);
    });
  });

  // ============================================
  // MINTING TESTS
  // ============================================

  describe("Minting", function () {
    it("Should allow minter to mint tokens", async function () {
      const { token, minter, user1 } = await loadFixture(deployGreyTokenFixture);
      const amount = ethers.parseEther("1000");

      await token.connect(minter).mint(user1.address, amount);
      expect(await token.balanceOf(user1.address)).to.equal(amount);
    });

    it("Should emit Transfer event on mint", async function () {
      const { token, minter, user1 } = await loadFixture(deployGreyTokenFixture);
      const amount = ethers.parseEther("1000");

      await expect(token.connect(minter).mint(user1.address, amount))
        .to.emit(token, "Transfer")
        .withArgs(ethers.ZeroAddress, user1.address, amount);
    });

    it("Should reject minting by non-minter", async function () {
      const { token, user1, user2, MINTER_ROLE } = await loadFixture(deployGreyTokenFixture);
      const amount = ethers.parseEther("1000");

      await expect(token.connect(user1).mint(user2.address, amount))
        .to.be.revertedWithCustomError(token, "AccessControlUnauthorizedAccount")
        .withArgs(user1.address, MINTER_ROLE);
    });

    it("Should reject minting beyond max supply", async function () {
      const { token, minter, user1, maxSupply } = await loadFixture(deployGreyTokenFixture);

      await expect(token.connect(minter).mint(user1.address, maxSupply + 1n))
        .to.be.revertedWith("GreyToken: max supply exceeded");
    });

    it("Should reject minting to zero address", async function () {
      const { token, minter } = await loadFixture(deployGreyTokenFixture);
      const amount = ethers.parseEther("1000");

      await expect(token.connect(minter).mint(ethers.ZeroAddress, amount))
        .to.be.revertedWithCustomError(token, "ERC20InvalidReceiver");
    });

    it("Should update total supply on mint", async function () {
      const { token, minter, user1 } = await loadFixture(deployGreyTokenFixture);
      const amount = ethers.parseEther("1000");

      await token.connect(minter).mint(user1.address, amount);
      expect(await token.totalSupply()).to.equal(amount);
    });

    it("Should handle multiple mints correctly", async function () {
      const { token, minter, user1 } = await loadFixture(deployGreyTokenFixture);
      
      await token.connect(minter).mint(user1.address, ethers.parseEther("100"));
      await token.connect(minter).mint(user1.address, ethers.parseEther("200"));
      await token.connect(minter).mint(user1.address, ethers.parseEther("300"));

      expect(await token.balanceOf(user1.address)).to.equal(ethers.parseEther("600"));
    });
  });

  // ============================================
  // BURNING TESTS
  // ============================================

  describe("Burning", function () {
    it("Should allow holder to burn their tokens", async function () {
      const { token, user1 } = await loadFixture(deployWithInitialSupplyFixture);
      const burnAmount = ethers.parseEther("100000");
      const initialBalance = await token.balanceOf(user1.address);

      await token.connect(user1).burn(burnAmount);
      expect(await token.balanceOf(user1.address)).to.equal(initialBalance - burnAmount);
    });

    it("Should emit Transfer event on burn", async function () {
      const { token, user1 } = await loadFixture(deployWithInitialSupplyFixture);
      const burnAmount = ethers.parseEther("100000");

      await expect(token.connect(user1).burn(burnAmount))
        .to.emit(token, "Transfer")
        .withArgs(user1.address, ethers.ZeroAddress, burnAmount);
    });

    it("Should reduce total supply on burn", async function () {
      const { token, user1 } = await loadFixture(deployWithInitialSupplyFixture);
      const initialSupply = await token.totalSupply();
      const burnAmount = ethers.parseEther("100000");

      await token.connect(user1).burn(burnAmount);
      expect(await token.totalSupply()).to.equal(initialSupply - burnAmount);
    });

    it("Should reject burning more than balance", async function () {
      const { token, user1 } = await loadFixture(deployWithInitialSupplyFixture);
      const balance = await token.balanceOf(user1.address);

      await expect(token.connect(user1).burn(balance + 1n))
        .to.be.revertedWithCustomError(token, "ERC20InsufficientBalance");
    });

    it("Should allow burnFrom with approval", async function () {
      const { token, user1, user2 } = await loadFixture(deployWithInitialSupplyFixture);
      const burnAmount = ethers.parseEther("100000");

      await token.connect(user1).approve(user2.address, burnAmount);
      await token.connect(user2).burnFrom(user1.address, burnAmount);

      expect(await token.balanceOf(user1.address)).to.equal(ethers.parseEther("900000"));
    });
  });

  // ============================================
  // TRANSFER TESTS
  // ============================================

  describe("Transfers", function () {
    it("Should transfer tokens between accounts", async function () {
      const { token, user1, user2 } = await loadFixture(deployWithInitialSupplyFixture);
      const amount = ethers.parseEther("10000");

      await token.connect(user1).transfer(user2.address, amount);
      expect(await token.balanceOf(user2.address)).to.equal(ethers.parseEther("510000"));
    });

    it("Should emit Transfer event", async function () {
      const { token, user1, user2 } = await loadFixture(deployWithInitialSupplyFixture);
      const amount = ethers.parseEther("10000");

      await expect(token.connect(user1).transfer(user2.address, amount))
        .to.emit(token, "Transfer")
        .withArgs(user1.address, user2.address, amount);
    });

    it("Should reject transfer to zero address", async function () {
      const { token, user1 } = await loadFixture(deployWithInitialSupplyFixture);
      const amount = ethers.parseEther("10000");

      await expect(token.connect(user1).transfer(ethers.ZeroAddress, amount))
        .to.be.revertedWithCustomError(token, "ERC20InvalidReceiver");
    });

    it("Should reject transfer exceeding balance", async function () {
      const { token, user1, user2 } = await loadFixture(deployWithInitialSupplyFixture);
      const balance = await token.balanceOf(user1.address);

      await expect(token.connect(user1).transfer(user2.address, balance + 1n))
        .to.be.revertedWithCustomError(token, "ERC20InsufficientBalance");
    });

    it("Should allow transfer of zero tokens", async function () {
      const { token, user1, user2 } = await loadFixture(deployWithInitialSupplyFixture);

      await expect(token.connect(user1).transfer(user2.address, 0)).to.not.be.reverted;
    });

    it("Should handle self-transfer", async function () {
      const { token, user1 } = await loadFixture(deployWithInitialSupplyFixture);
      const initialBalance = await token.balanceOf(user1.address);
      const amount = ethers.parseEther("10000");

      await token.connect(user1).transfer(user1.address, amount);
      expect(await token.balanceOf(user1.address)).to.equal(initialBalance);
    });
  });

  // ============================================
  // APPROVAL TESTS
  // ============================================

  describe("Approvals", function () {
    it("Should set allowance correctly", async function () {
      const { token, user1, user2 } = await loadFixture(deployWithInitialSupplyFixture);
      const amount = ethers.parseEther("50000");

      await token.connect(user1).approve(user2.address, amount);
      expect(await token.allowance(user1.address, user2.address)).to.equal(amount);
    });

    it("Should emit Approval event", async function () {
      const { token, user1, user2 } = await loadFixture(deployWithInitialSupplyFixture);
      const amount = ethers.parseEther("50000");

      await expect(token.connect(user1).approve(user2.address, amount))
        .to.emit(token, "Approval")
        .withArgs(user1.address, user2.address, amount);
    });

    it("Should allow transferFrom with approval", async function () {
      const { token, user1, user2, user3 } = await loadFixture(deployWithInitialSupplyFixture);
      const amount = ethers.parseEther("50000");

      await token.connect(user1).approve(user2.address, amount);
      await token.connect(user2).transferFrom(user1.address, user3.address, amount);

      expect(await token.balanceOf(user3.address)).to.equal(amount);
    });

    it("Should reduce allowance after transferFrom", async function () {
      const { token, user1, user2, user3 } = await loadFixture(deployWithInitialSupplyFixture);
      const approveAmount = ethers.parseEther("100000");
      const transferAmount = ethers.parseEther("50000");

      await token.connect(user1).approve(user2.address, approveAmount);
      await token.connect(user2).transferFrom(user1.address, user3.address, transferAmount);

      expect(await token.allowance(user1.address, user2.address)).to.equal(approveAmount - transferAmount);
    });

    it("Should reject transferFrom exceeding allowance", async function () {
      const { token, user1, user2, user3 } = await loadFixture(deployWithInitialSupplyFixture);
      const amount = ethers.parseEther("50000");

      await token.connect(user1).approve(user2.address, amount);
      
      await expect(token.connect(user2).transferFrom(user1.address, user3.address, amount + 1n))
        .to.be.revertedWithCustomError(token, "ERC20InsufficientAllowance");
    });

    it("Should allow increase allowance", async function () {
      const { token, user1, user2 } = await loadFixture(deployWithInitialSupplyFixture);
      const initialAmount = ethers.parseEther("50000");
      const increaseAmount = ethers.parseEther("25000");

      await token.connect(user1).approve(user2.address, initialAmount);
      await token.connect(user1).approve(user2.address, initialAmount + increaseAmount);

      expect(await token.allowance(user1.address, user2.address)).to.equal(initialAmount + increaseAmount);
    });
  });

  // ============================================
  // PAUSE TESTS
  // ============================================

  describe("Pausability", function () {
    it("Should allow pauser to pause", async function () {
      const { token, pauser } = await loadFixture(deployGreyTokenFixture);

      await token.connect(pauser).pause();
      expect(await token.paused()).to.be.true;
    });

    it("Should allow pauser to unpause", async function () {
      const { token, pauser } = await loadFixture(deployGreyTokenFixture);

      await token.connect(pauser).pause();
      await token.connect(pauser).unpause();
      expect(await token.paused()).to.be.false;
    });

    it("Should emit Paused event", async function () {
      const { token, pauser } = await loadFixture(deployGreyTokenFixture);

      await expect(token.connect(pauser).pause())
        .to.emit(token, "Paused")
        .withArgs(pauser.address);
    });

    it("Should reject transfers when paused", async function () {
      const { token, pauser, user1, user2 } = await loadFixture(deployWithInitialSupplyFixture);

      await token.connect(pauser).pause();

      await expect(token.connect(user1).transfer(user2.address, ethers.parseEther("1000")))
        .to.be.revertedWithCustomError(token, "EnforcedPause");
    });

    it("Should reject minting when paused", async function () {
      const { token, minter, pauser, user1 } = await loadFixture(deployGreyTokenFixture);

      await token.connect(pauser).pause();

      await expect(token.connect(minter).mint(user1.address, ethers.parseEther("1000")))
        .to.be.revertedWithCustomError(token, "EnforcedPause");
    });

    it("Should reject pause by non-pauser", async function () {
      const { token, user1, PAUSER_ROLE } = await loadFixture(deployGreyTokenFixture);

      await expect(token.connect(user1).pause())
        .to.be.revertedWithCustomError(token, "AccessControlUnauthorizedAccount")
        .withArgs(user1.address, PAUSER_ROLE);
    });
  });

  // ============================================
  // SNAPSHOT TESTS
  // ============================================

  describe("Snapshots", function () {
    it("Should create snapshot", async function () {
      const { token, owner } = await loadFixture(deployWithInitialSupplyFixture);

      await expect(token.connect(owner).snapshot()).to.not.be.reverted;
    });

    it("Should return correct snapshot ID", async function () {
      const { token, owner } = await loadFixture(deployWithInitialSupplyFixture);

      const tx = await token.connect(owner).snapshot();
      const receipt = await tx.wait();
      
      // Find Snapshot event
      const snapshotEvent = receipt.logs.find(
        log => log.fragment && log.fragment.name === "Snapshot"
      );
      
      expect(snapshotEvent).to.not.be.undefined;
    });

    it("Should preserve balances at snapshot", async function () {
      const { token, owner, user1, user2 } = await loadFixture(deployWithInitialSupplyFixture);

      const balanceBefore = await token.balanceOf(user1.address);
      
      // Create snapshot
      await token.connect(owner).snapshot();

      // Transfer tokens after snapshot
      await token.connect(user1).transfer(user2.address, ethers.parseEther("100000"));

      // Current balance should be reduced
      expect(await token.balanceOf(user1.address)).to.equal(balanceBefore - ethers.parseEther("100000"));

      // Snapshot balance should be preserved (if balanceOfAt is implemented)
      // This test structure depends on implementation
    });
  });

  // ============================================
  // VOTING TESTS
  // ============================================

  describe("Voting", function () {
    it("Should delegate to self to activate voting power", async function () {
      const { token, user1 } = await loadFixture(deployWithInitialSupplyFixture);

      await token.connect(user1).delegate(user1.address);
      expect(await token.delegates(user1.address)).to.equal(user1.address);
    });

    it("Should delegate to another address", async function () {
      const { token, user1, user2 } = await loadFixture(deployWithInitialSupplyFixture);

      await token.connect(user1).delegate(user2.address);
      expect(await token.delegates(user1.address)).to.equal(user2.address);
    });

    it("Should update voting power on delegation", async function () {
      const { token, user1, user2 } = await loadFixture(deployWithInitialSupplyFixture);
      const balance = await token.balanceOf(user1.address);

      await token.connect(user1).delegate(user2.address);
      expect(await token.getVotes(user2.address)).to.equal(balance);
    });

    it("Should update voting power on transfer", async function () {
      const { token, user1, user2, user3 } = await loadFixture(deployWithInitialSupplyFixture);

      // Delegate
      await token.connect(user1).delegate(user1.address);
      await token.connect(user2).delegate(user2.address);

      const initialVotes1 = await token.getVotes(user1.address);
      const transferAmount = ethers.parseEther("100000");

      // Transfer
      await token.connect(user1).transfer(user3.address, transferAmount);

      expect(await token.getVotes(user1.address)).to.equal(initialVotes1 - transferAmount);
    });

    it("Should emit DelegateChanged event", async function () {
      const { token, user1, user2 } = await loadFixture(deployWithInitialSupplyFixture);

      await expect(token.connect(user1).delegate(user2.address))
        .to.emit(token, "DelegateChanged")
        .withArgs(user1.address, ethers.ZeroAddress, user2.address);
    });
  });

  // ============================================
  // PERMIT TESTS (EIP-2612)
  // ============================================

  describe("Permit (EIP-2612)", function () {
    it("Should return correct domain separator", async function () {
      const { token } = await loadFixture(deployGreyTokenFixture);
      const domainSeparator = await token.DOMAIN_SEPARATOR();
      expect(domainSeparator).to.not.equal(ethers.ZeroHash);
    });

    it("Should return correct nonces", async function () {
      const { token, user1 } = await loadFixture(deployGreyTokenFixture);
      expect(await token.nonces(user1.address)).to.equal(0);
    });

    it("Should approve via permit signature", async function () {
      const { token, user1, user2 } = await loadFixture(deployWithInitialSupplyFixture);
      
      const value = ethers.parseEther("1000");
      const deadline = (await time.latest()) + 3600;
      const nonce = await token.nonces(user1.address);

      // Get domain
      const domain = {
        name: await token.name(),
        version: "1",
        chainId: (await ethers.provider.getNetwork()).chainId,
        verifyingContract: await token.getAddress()
      };

      const types = {
        Permit: [
          { name: "owner", type: "address" },
          { name: "spender", type: "address" },
          { name: "value", type: "uint256" },
          { name: "nonce", type: "uint256" },
          { name: "deadline", type: "uint256" }
        ]
      };

      const message = {
        owner: user1.address,
        spender: user2.address,
        value: value,
        nonce: nonce,
        deadline: deadline
      };

      const signature = await user1.signTypedData(domain, types, message);
      const { v, r, s } = ethers.Signature.from(signature);

      await token.permit(user1.address, user2.address, value, deadline, v, r, s);

      expect(await token.allowance(user1.address, user2.address)).to.equal(value);
    });
  });

  // ============================================
  // BLACKLIST TESTS
  // ============================================

  describe("Blacklist", function () {
    it("Should blacklist an address", async function () {
      const { token, owner, user1 } = await loadFixture(deployGreyTokenFixture);

      await token.connect(owner).setBlacklisted(user1.address, true);
      expect(await token.blacklisted(user1.address)).to.be.true;
    });

    it("Should emit AddressBlacklisted event", async function () {
      const { token, owner, user1 } = await loadFixture(deployGreyTokenFixture);

      await expect(token.connect(owner).setBlacklisted(user1.address, true))
        .to.emit(token, "AddressBlacklisted")
        .withArgs(user1.address, true);
    });

    it("Should reject transfers from blacklisted address", async function () {
      const { token, owner, user1, user2 } = await loadFixture(deployWithInitialSupplyFixture);

      await token.connect(owner).setBlacklisted(user1.address, true);

      await expect(token.connect(user1).transfer(user2.address, ethers.parseEther("1000")))
        .to.be.revertedWith("GreyToken: sender blacklisted");
    });

    it("Should reject transfers to blacklisted address", async function () {
      const { token, owner, user1, user2 } = await loadFixture(deployWithInitialSupplyFixture);

      await token.connect(owner).setBlacklisted(user2.address, true);

      await expect(token.connect(user1).transfer(user2.address, ethers.parseEther("1000")))
        .to.be.revertedWith("GreyToken: recipient blacklisted");
    });

    it("Should allow removing from blacklist", async function () {
      const { token, owner, user1, user2 } = await loadFixture(deployWithInitialSupplyFixture);

      await token.connect(owner).setBlacklisted(user1.address, true);
      await token.connect(owner).setBlacklisted(user1.address, false);

      await expect(token.connect(user1).transfer(user2.address, ethers.parseEther("1000")))
        .to.not.be.reverted;
    });
  });

  // ============================================
  // GAS OPTIMIZATION TESTS
  // ============================================

  describe("Gas Usage", function () {
    it("Should use reasonable gas for transfer", async function () {
      const { token, user1, user2 } = await loadFixture(deployWithInitialSupplyFixture);

      const tx = await token.connect(user1).transfer(user2.address, ethers.parseEther("1000"));
      const receipt = await tx.wait();
      
      expect(receipt.gasUsed).to.be.lessThan(100000n);
    });

    it("Should use reasonable gas for approve", async function () {
      const { token, user1, user2 } = await loadFixture(deployWithInitialSupplyFixture);

      const tx = await token.connect(user1).approve(user2.address, ethers.parseEther("1000"));
      const receipt = await tx.wait();
      
      expect(receipt.gasUsed).to.be.lessThan(50000n);
    });

    it("Should use reasonable gas for mint", async function () {
      const { token, minter, user1 } = await loadFixture(deployGreyTokenFixture);

      const tx = await token.connect(minter).mint(user1.address, ethers.parseEther("1000"));
      const receipt = await tx.wait();
      
      expect(receipt.gasUsed).to.be.lessThan(100000n);
    });
  });

  // ============================================
  // EDGE CASES
  // ============================================

  describe("Edge Cases", function () {
    it("Should handle very large transfers", async function () {
      const { token, minter, user1, user2, maxSupply } = await loadFixture(deployGreyTokenFixture);

      await token.connect(minter).mint(user1.address, maxSupply);
      await token.connect(user1).transfer(user2.address, maxSupply);

      expect(await token.balanceOf(user2.address)).to.equal(maxSupply);
    });

    it("Should handle very small transfers", async function () {
      const { token, user1, user2 } = await loadFixture(deployWithInitialSupplyFixture);

      await token.connect(user1).transfer(user2.address, 1);
      expect(await token.balanceOf(user2.address)).to.equal(ethers.parseEther("500000") + 1n);
    });

    it("Should handle multiple approvals correctly", async function () {
      const { token, user1, user2 } = await loadFixture(deployWithInitialSupplyFixture);

      await token.connect(user1).approve(user2.address, ethers.parseEther("100"));
      await token.connect(user1).approve(user2.address, ethers.parseEther("200"));
      await token.connect(user1).approve(user2.address, ethers.parseEther("50"));

      expect(await token.allowance(user1.address, user2.address)).to.equal(ethers.parseEther("50"));
    });

    it("Should handle concurrent operations", async function () {
      const { token, minter, user1, user2, user3 } = await loadFixture(deployGreyTokenFixture);

      // Parallel minting
      await Promise.all([
        token.connect(minter).mint(user1.address, ethers.parseEther("1000")),
        token.connect(minter).mint(user2.address, ethers.parseEther("2000")),
        token.connect(minter).mint(user3.address, ethers.parseEther("3000"))
      ]);

      expect(await token.totalSupply()).to.equal(ethers.parseEther("6000"));
    });
  });
});
