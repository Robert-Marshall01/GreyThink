const { expect } = require("chai");
const { ethers } = require("hardhat");
const { time } = require("@nomicfoundation/hardhat-network-helpers");

describe("GreyToken", function () {
  let token;
  let owner, minter, pauser, user1, user2;

  const NAME = "Grey Token";
  const SYMBOL = "GREY";
  const MAX_SUPPLY = ethers.parseEther("1000000000"); // 1 billion

  beforeEach(async function () {
    [owner, minter, pauser, user1, user2] = await ethers.getSigners();

    const GreyToken = await ethers.getContractFactory("GreyToken");
    token = await GreyToken.deploy(NAME, SYMBOL, MAX_SUPPLY);
    await token.waitForDeployment();
  });

  describe("Deployment", function () {
    it("Should set correct name and symbol", async function () {
      expect(await token.name()).to.equal(NAME);
      expect(await token.symbol()).to.equal(SYMBOL);
    });

    it("Should set correct max supply", async function () {
      expect(await token.maxSupply()).to.equal(MAX_SUPPLY);
    });

    it("Should grant admin role to deployer", async function () {
      const ADMIN_ROLE = await token.DEFAULT_ADMIN_ROLE();
      expect(await token.hasRole(ADMIN_ROLE, owner.address)).to.be.true;
    });

    it("Should have zero total supply initially", async function () {
      expect(await token.totalSupply()).to.equal(0);
    });
  });

  describe("Minting", function () {
    it("Should allow minter to mint tokens", async function () {
      const amount = ethers.parseEther("1000");
      await token.mint(user1.address, amount);
      expect(await token.balanceOf(user1.address)).to.equal(amount);
    });

    it("Should not allow minting beyond max supply", async function () {
      await expect(
        token.mint(user1.address, MAX_SUPPLY + 1n)
      ).to.be.revertedWithCustomError(token, "MaxSupplyExceeded");
    });

    it("Should not allow non-minter to mint", async function () {
      await expect(
        token.connect(user1).mint(user2.address, ethers.parseEther("100"))
      ).to.be.reverted;
    });

    it("Should emit Transfer event on mint", async function () {
      const amount = ethers.parseEther("1000");
      await expect(token.mint(user1.address, amount))
        .to.emit(token, "Transfer")
        .withArgs(ethers.ZeroAddress, user1.address, amount);
    });
  });

  describe("Burning", function () {
    beforeEach(async function () {
      await token.mint(user1.address, ethers.parseEther("1000"));
    });

    it("Should allow holder to burn tokens", async function () {
      const burnAmount = ethers.parseEther("500");
      await token.connect(user1).burn(burnAmount);
      expect(await token.balanceOf(user1.address)).to.equal(ethers.parseEther("500"));
    });

    it("Should allow approved spender to burn from holder", async function () {
      const burnAmount = ethers.parseEther("300");
      await token.connect(user1).approve(owner.address, burnAmount);
      await token.burnFrom(user1.address, burnAmount);
      expect(await token.balanceOf(user1.address)).to.equal(ethers.parseEther("700"));
    });
  });

  describe("Pausing", function () {
    beforeEach(async function () {
      await token.mint(user1.address, ethers.parseEther("1000"));
    });

    it("Should allow pauser to pause", async function () {
      await token.pause();
      expect(await token.paused()).to.be.true;
    });

    it("Should prevent transfers when paused", async function () {
      await token.pause();
      await expect(
        token.connect(user1).transfer(user2.address, ethers.parseEther("100"))
      ).to.be.revertedWithCustomError(token, "EnforcedPause");
    });

    it("Should allow transfers after unpause", async function () {
      await token.pause();
      await token.unpause();
      await token.connect(user1).transfer(user2.address, ethers.parseEther("100"));
      expect(await token.balanceOf(user2.address)).to.equal(ethers.parseEther("100"));
    });
  });

  describe("Snapshots", function () {
    beforeEach(async function () {
      await token.mint(user1.address, ethers.parseEther("1000"));
    });

    it("Should create snapshots", async function () {
      await token.snapshot();
      const snapshotId = await token.getCurrentSnapshotId();
      expect(snapshotId).to.equal(1);
    });

    it("Should track balances at snapshot", async function () {
      const initialBalance = ethers.parseEther("1000");
      await token.snapshot();
      
      // Transfer after snapshot
      await token.connect(user1).transfer(user2.address, ethers.parseEther("500"));
      
      // Check snapshot balance
      expect(await token.balanceOfAt(user1.address, 1)).to.equal(initialBalance);
      expect(await token.balanceOf(user1.address)).to.equal(ethers.parseEther("500"));
    });
  });

  describe("Voting", function () {
    beforeEach(async function () {
      await token.mint(user1.address, ethers.parseEther("1000"));
    });

    it("Should allow delegation", async function () {
      await token.connect(user1).delegate(user1.address);
      expect(await token.delegates(user1.address)).to.equal(user1.address);
    });

    it("Should track voting power after delegation", async function () {
      await token.connect(user1).delegate(user1.address);
      expect(await token.getVotes(user1.address)).to.equal(ethers.parseEther("1000"));
    });

    it("Should allow delegating to another address", async function () {
      await token.connect(user1).delegate(user2.address);
      expect(await token.getVotes(user2.address)).to.equal(ethers.parseEther("1000"));
      expect(await token.getVotes(user1.address)).to.equal(0);
    });
  });

  describe("Permit", function () {
    it("Should support permit", async function () {
      const amount = ethers.parseEther("100");
      const deadline = (await time.latest()) + 3600;
      
      const domain = {
        name: NAME,
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

      const nonce = await token.nonces(owner.address);

      const value = {
        owner: owner.address,
        spender: user1.address,
        value: amount,
        nonce: nonce,
        deadline: deadline
      };

      const signature = await owner.signTypedData(domain, types, value);
      const { v, r, s } = ethers.Signature.from(signature);

      await token.permit(owner.address, user1.address, amount, deadline, v, r, s);
      expect(await token.allowance(owner.address, user1.address)).to.equal(amount);
    });
  });
});
