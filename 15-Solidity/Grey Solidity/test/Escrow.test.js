const { expect } = require("chai");
const { ethers } = require("hardhat");
const { time } = require("@nomicfoundation/hardhat-network-helpers");

describe("Escrow", function () {
  let escrow;
  let token;
  let nft;
  let owner, party1, party2, arbiter;

  const DEPOSIT_AMOUNT = ethers.parseEther("10");

  beforeEach(async function () {
    [owner, party1, party2, arbiter] = await ethers.getSigners();

    // Deploy mock tokens
    const MockERC20 = await ethers.getContractFactory("MockERC20");
    token = await MockERC20.deploy("Test Token", "TEST", 18);

    const GreyNFT = await ethers.getContractFactory("GreyNFT");
    nft = await GreyNFT.deploy(
      "Test NFT",
      "TNFT",
      "https://api.test.com/",
      owner.address,
      500
    );

    // Deploy Escrow
    const Escrow = await ethers.getContractFactory("Escrow");
    escrow = await Escrow.deploy();

    // Setup tokens
    await token.mint(party1.address, ethers.parseEther("1000"));
    await token.mint(party2.address, ethers.parseEther("1000"));
    await token.connect(party1).approve(await escrow.getAddress(), ethers.MaxUint256);
    await token.connect(party2).approve(await escrow.getAddress(), ethers.MaxUint256);

    // Mint NFT
    await nft.mint(party1.address, "test.json");
    await nft.connect(party1).setApprovalForAll(await escrow.getAddress(), true);

    // Setup arbiter
    await escrow.addArbiter(arbiter.address);
  });

  describe("Deployment", function () {
    it("Should set correct owner", async function () {
      expect(await escrow.owner()).to.equal(owner.address);
    });

    it("Should have deal counter at 0", async function () {
      expect(await escrow.dealCounter()).to.equal(0);
    });

    it("Should register arbiter", async function () {
      expect(await escrow.isArbiter(arbiter.address)).to.be.true;
    });
  });

  describe("Creating Deals", function () {
    it("Should create ETH deal", async function () {
      const deadline = (await time.latest()) + 86400;

      const tx = await escrow.connect(party1).createDeal(
        party2.address,
        ethers.ZeroAddress,
        0, // AssetType.ETH
        0,
        DEPOSIT_AMOUNT,
        deadline,
        "Test deal"
      );

      await expect(tx).to.emit(escrow, "DealCreated").withArgs(0, party1.address, party2.address);
    });

    it("Should create ERC20 deal", async function () {
      const deadline = (await time.latest()) + 86400;

      await escrow.connect(party1).createDeal(
        party2.address,
        await token.getAddress(),
        1, // AssetType.ERC20
        0,
        DEPOSIT_AMOUNT,
        deadline,
        "Token deal"
      );

      const deal = await escrow.getDeal(0);
      expect(deal.assetType).to.equal(1);
      expect(deal.amount).to.equal(DEPOSIT_AMOUNT);
    });

    it("Should create NFT deal", async function () {
      const deadline = (await time.latest()) + 86400;

      await escrow.connect(party1).createDeal(
        party2.address,
        await nft.getAddress(),
        2, // AssetType.ERC721
        0, // Token ID
        1,
        deadline,
        "NFT deal"
      );

      const deal = await escrow.getDeal(0);
      expect(deal.assetType).to.equal(2);
      expect(deal.tokenId).to.equal(0);
    });

    it("Should reject deal with past deadline", async function () {
      const pastDeadline = (await time.latest()) - 1;

      await expect(
        escrow.connect(party1).createDeal(
          party2.address,
          ethers.ZeroAddress,
          0,
          0,
          DEPOSIT_AMOUNT,
          pastDeadline,
          "Bad deal"
        )
      ).to.be.revertedWithCustomError(escrow, "DeadlinePassed");
    });

    it("Should reject zero amount", async function () {
      const deadline = (await time.latest()) + 86400;

      await expect(
        escrow.connect(party1).createDeal(
          party2.address,
          ethers.ZeroAddress,
          0,
          0,
          0,
          deadline,
          "Zero deal"
        )
      ).to.be.revertedWithCustomError(escrow, "ZeroAmount");
    });
  });

  describe("Deposits", function () {
    let dealId;
    let deadline;

    beforeEach(async function () {
      deadline = (await time.latest()) + 86400;

      await escrow.connect(party1).createDeal(
        party2.address,
        ethers.ZeroAddress,
        0,
        0,
        DEPOSIT_AMOUNT,
        deadline,
        "ETH deal"
      );
      dealId = 0;
    });

    it("Should deposit ETH", async function () {
      await escrow.connect(party1).deposit(dealId, { value: DEPOSIT_AMOUNT });

      const deal = await escrow.getDeal(dealId);
      expect(deal.deposited).to.equal(DEPOSIT_AMOUNT);
    });

    it("Should emit Deposited event", async function () {
      await expect(
        escrow.connect(party1).deposit(dealId, { value: DEPOSIT_AMOUNT })
      ).to.emit(escrow, "Deposited").withArgs(dealId, party1.address, DEPOSIT_AMOUNT);
    });

    it("Should reject deposit from non-party", async function () {
      await expect(
        escrow.connect(arbiter).deposit(dealId, { value: DEPOSIT_AMOUNT })
      ).to.be.revertedWithCustomError(escrow, "NotPartyToDealt");
    });

    it("Should reject insufficient deposit", async function () {
      await expect(
        escrow.connect(party1).deposit(dealId, { value: DEPOSIT_AMOUNT / 2n })
      ).to.be.revertedWithCustomError(escrow, "InsufficientDeposit");
    });
  });

  describe("ERC20 Deposits", function () {
    let dealId;

    beforeEach(async function () {
      const deadline = (await time.latest()) + 86400;

      await escrow.connect(party1).createDeal(
        party2.address,
        await token.getAddress(),
        1, // ERC20
        0,
        DEPOSIT_AMOUNT,
        deadline,
        "Token deal"
      );
      dealId = 0;
    });

    it("Should deposit tokens", async function () {
      await escrow.connect(party1).deposit(dealId);

      const deal = await escrow.getDeal(dealId);
      expect(deal.deposited).to.equal(DEPOSIT_AMOUNT);
    });

    it("Should transfer tokens to escrow", async function () {
      const escrowAddress = await escrow.getAddress();
      const initialBalance = await token.balanceOf(escrowAddress);

      await escrow.connect(party1).deposit(dealId);

      const finalBalance = await token.balanceOf(escrowAddress);
      expect(finalBalance - initialBalance).to.equal(DEPOSIT_AMOUNT);
    });
  });

  describe("NFT Deposits", function () {
    let dealId;

    beforeEach(async function () {
      const deadline = (await time.latest()) + 86400;

      await escrow.connect(party1).createDeal(
        party2.address,
        await nft.getAddress(),
        2, // ERC721
        0,
        1,
        deadline,
        "NFT deal"
      );
      dealId = 0;
    });

    it("Should deposit NFT", async function () {
      await escrow.connect(party1).deposit(dealId);
      expect(await nft.ownerOf(0)).to.equal(await escrow.getAddress());
    });
  });

  describe("Release", function () {
    let dealId;

    beforeEach(async function () {
      const deadline = (await time.latest()) + 86400;

      await escrow.connect(party1).createDeal(
        party2.address,
        ethers.ZeroAddress,
        0,
        0,
        DEPOSIT_AMOUNT,
        deadline,
        "ETH deal"
      );
      dealId = 0;

      await escrow.connect(party1).deposit(dealId, { value: DEPOSIT_AMOUNT });
    });

    it("Should release funds to counterparty", async function () {
      const initialBalance = await ethers.provider.getBalance(party2.address);

      await escrow.connect(party1).release(dealId);

      const finalBalance = await ethers.provider.getBalance(party2.address);
      expect(finalBalance - initialBalance).to.equal(DEPOSIT_AMOUNT);
    });

    it("Should update deal status", async function () {
      await escrow.connect(party1).release(dealId);

      const deal = await escrow.getDeal(dealId);
      expect(deal.status).to.equal(2); // Released
    });

    it("Should emit Released event", async function () {
      await expect(escrow.connect(party1).release(dealId))
        .to.emit(escrow, "Released")
        .withArgs(dealId, party2.address, DEPOSIT_AMOUNT);
    });

    it("Should reject release from non-depositor", async function () {
      await expect(
        escrow.connect(party2).release(dealId)
      ).to.be.revertedWithCustomError(escrow, "NotDepositor");
    });
  });

  describe("Disputes", function () {
    let dealId;

    beforeEach(async function () {
      const deadline = (await time.latest()) + 86400;

      await escrow.connect(party1).createDeal(
        party2.address,
        ethers.ZeroAddress,
        0,
        0,
        DEPOSIT_AMOUNT,
        deadline,
        "ETH deal"
      );
      dealId = 0;

      await escrow.connect(party1).deposit(dealId, { value: DEPOSIT_AMOUNT });
    });

    it("Should open dispute", async function () {
      await escrow.connect(party1).openDispute(dealId, "Issue with delivery");

      const deal = await escrow.getDeal(dealId);
      expect(deal.status).to.equal(3); // Disputed
    });

    it("Should emit DisputeOpened event", async function () {
      await expect(escrow.connect(party1).openDispute(dealId, "Issue"))
        .to.emit(escrow, "DisputeOpened")
        .withArgs(dealId, party1.address, "Issue");
    });

    it("Should reject dispute from non-party", async function () {
      await expect(
        escrow.connect(arbiter).openDispute(dealId, "Not my deal")
      ).to.be.revertedWithCustomError(escrow, "NotPartyToDealt");
    });
  });

  describe("Dispute Resolution", function () {
    let dealId;

    beforeEach(async function () {
      const deadline = (await time.latest()) + 86400;

      await escrow.connect(party1).createDeal(
        party2.address,
        ethers.ZeroAddress,
        0,
        0,
        DEPOSIT_AMOUNT,
        deadline,
        "ETH deal"
      );
      dealId = 0;

      await escrow.connect(party1).deposit(dealId, { value: DEPOSIT_AMOUNT });
      await escrow.connect(party1).openDispute(dealId, "Issue");
    });

    it("Should resolve in favor of party1", async function () {
      const initialBalance = await ethers.provider.getBalance(party1.address);

      await escrow.connect(arbiter).resolveDispute(dealId, party1.address, "Party1 wins");

      const finalBalance = await ethers.provider.getBalance(party1.address);
      expect(finalBalance - initialBalance).to.equal(DEPOSIT_AMOUNT);
    });

    it("Should resolve in favor of party2", async function () {
      const initialBalance = await ethers.provider.getBalance(party2.address);

      await escrow.connect(arbiter).resolveDispute(dealId, party2.address, "Party2 wins");

      const finalBalance = await ethers.provider.getBalance(party2.address);
      expect(finalBalance - initialBalance).to.equal(DEPOSIT_AMOUNT);
    });

    it("Should update status to Resolved", async function () {
      await escrow.connect(arbiter).resolveDispute(dealId, party1.address, "Resolved");

      const deal = await escrow.getDeal(dealId);
      expect(deal.status).to.equal(4); // Resolved
    });

    it("Should reject resolution from non-arbiter", async function () {
      await expect(
        escrow.connect(owner).resolveDispute(dealId, party1.address, "Hack")
      ).to.be.revertedWithCustomError(escrow, "NotArbiter");
    });

    it("Should reject resolution for non-disputed deal", async function () {
      // Create new non-disputed deal
      const deadline = (await time.latest()) + 86400;
      await escrow.connect(party1).createDeal(
        party2.address,
        ethers.ZeroAddress,
        0,
        0,
        DEPOSIT_AMOUNT,
        deadline,
        "Clean deal"
      );

      await expect(
        escrow.connect(arbiter).resolveDispute(1, party1.address, "Can't resolve")
      ).to.be.revertedWithCustomError(escrow, "NotDisputed");
    });
  });

  describe("Refunds", function () {
    let dealId;

    beforeEach(async function () {
      const deadline = (await time.latest()) + 86400;

      await escrow.connect(party1).createDeal(
        party2.address,
        ethers.ZeroAddress,
        0,
        0,
        DEPOSIT_AMOUNT,
        deadline,
        "ETH deal"
      );
      dealId = 0;

      await escrow.connect(party1).deposit(dealId, { value: DEPOSIT_AMOUNT });
    });

    it("Should refund after deadline", async function () {
      await time.increase(86401); // Past deadline

      const initialBalance = await ethers.provider.getBalance(party1.address);

      const tx = await escrow.connect(party1).requestRefund(dealId);
      const receipt = await tx.wait();
      const gasUsed = receipt.gasUsed * receipt.gasPrice;

      const finalBalance = await ethers.provider.getBalance(party1.address);
      expect(finalBalance - initialBalance + gasUsed).to.equal(DEPOSIT_AMOUNT);
    });

    it("Should reject refund before deadline", async function () {
      await expect(
        escrow.connect(party1).requestRefund(dealId)
      ).to.be.revertedWithCustomError(escrow, "DeadlineNotPassed");
    });
  });

  describe("Admin Functions", function () {
    it("Should add arbiter", async function () {
      const newArbiter = ethers.Wallet.createRandom();
      await escrow.addArbiter(newArbiter.address);
      expect(await escrow.isArbiter(newArbiter.address)).to.be.true;
    });

    it("Should remove arbiter", async function () {
      await escrow.removeArbiter(arbiter.address);
      expect(await escrow.isArbiter(arbiter.address)).to.be.false;
    });

    it("Should not allow non-owner to add arbiter", async function () {
      await expect(
        escrow.connect(party1).addArbiter(party2.address)
      ).to.be.revertedWithCustomError(escrow, "OwnableUnauthorizedAccount");
    });

    it("Should pause contract", async function () {
      await escrow.pause();
      expect(await escrow.paused()).to.be.true;
    });

    it("Should prevent operations when paused", async function () {
      await escrow.pause();
      const deadline = (await time.latest()) + 86400;

      await expect(
        escrow.connect(party1).createDeal(
          party2.address,
          ethers.ZeroAddress,
          0,
          0,
          DEPOSIT_AMOUNT,
          deadline,
          "Paused deal"
        )
      ).to.be.revertedWithCustomError(escrow, "EnforcedPause");
    });
  });
});
