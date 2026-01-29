const { expect } = require("chai");
const { ethers } = require("hardhat");
const { time, loadFixture } = require("@nomicfoundation/hardhat-network-helpers");

/**
 * @title Comprehensive Marketplace & Escrow Tests
 * @notice Tests for NFTMarketplace, Escrow, and trading systems
 */
describe("Marketplace Modules - Comprehensive Tests", function () {
  // ============================================
  // FIXTURES
  // ============================================

  async function deployMarketplaceFixture() {
    const [owner, seller, buyer, buyer2, feeRecipient, royaltyRecipient] = await ethers.getSigners();

    // Deploy NFT contract
    const GreyNFT = await ethers.getContractFactory("GreyNFT");
    const nft = await GreyNFT.deploy(
      "Grey NFT",
      "GNFT",
      "https://api.grey.com/nft/",
      10000,           // max supply
      500,             // 5% royalty
      royaltyRecipient.address
    );

    // Deploy mock payment token
    const MockERC20 = await ethers.getContractFactory("MockERC20");
    const paymentToken = await MockERC20.deploy("Payment Token", "PAY", 18);

    // Deploy marketplace
    const NFTMarketplace = await ethers.getContractFactory("NFTMarketplace");
    const marketplace = await NFTMarketplace.deploy(
      250,             // 2.5% marketplace fee
      feeRecipient.address
    );

    // Deploy escrow
    const Escrow = await ethers.getContractFactory("Escrow");
    const escrow = await Escrow.deploy(
      86400,           // 1 day dispute period
      feeRecipient.address,
      100              // 1% escrow fee
    );

    // Mint NFTs to seller
    await nft.mint(seller.address);
    await nft.mint(seller.address);
    await nft.mint(seller.address);
    await nft.mint(seller.address);
    await nft.mint(seller.address);

    // Give buyers payment tokens
    await paymentToken.mint(buyer.address, ethers.parseEther("100000"));
    await paymentToken.mint(buyer2.address, ethers.parseEther("100000"));

    // Approve marketplace
    await nft.connect(seller).setApprovalForAll(await marketplace.getAddress(), true);
    await paymentToken.connect(buyer).approve(await marketplace.getAddress(), ethers.MaxUint256);
    await paymentToken.connect(buyer2).approve(await marketplace.getAddress(), ethers.MaxUint256);

    return {
      nft,
      paymentToken,
      marketplace,
      escrow,
      owner,
      seller,
      buyer,
      buyer2,
      feeRecipient,
      royaltyRecipient
    };
  }

  // ============================================
  // NFT MARKETPLACE TESTS
  // ============================================

  describe("NFTMarketplace", function () {
    describe("Listing", function () {
      it("Should create fixed price listing", async function () {
        const { marketplace, nft, seller } = await loadFixture(deployMarketplaceFixture);

        await marketplace.connect(seller).createListing(
          await nft.getAddress(),
          1,
          ethers.parseEther("1"),
          ethers.ZeroAddress,  // ETH payment
          0,                   // Fixed price
          0                    // No deadline
        );

        const listing = await marketplace.getListing(1);
        expect(listing.seller).to.equal(seller.address);
        expect(listing.price).to.equal(ethers.parseEther("1"));
      });

      it("Should create auction listing", async function () {
        const { marketplace, nft, seller } = await loadFixture(deployMarketplaceFixture);

        const deadline = (await time.latest()) + 86400;

        await marketplace.connect(seller).createListing(
          await nft.getAddress(),
          1,
          ethers.parseEther("0.5"),  // Starting price
          ethers.ZeroAddress,
          1,                         // Auction type
          deadline
        );

        const listing = await marketplace.getListing(1);
        expect(listing.listingType).to.equal(1);
      });

      it("Should emit ListingCreated event", async function () {
        const { marketplace, nft, seller } = await loadFixture(deployMarketplaceFixture);

        await expect(marketplace.connect(seller).createListing(
          await nft.getAddress(),
          1,
          ethers.parseEther("1"),
          ethers.ZeroAddress,
          0,
          0
        )).to.emit(marketplace, "ListingCreated");
      });

      it("Should reject listing without NFT ownership", async function () {
        const { marketplace, nft, buyer } = await loadFixture(deployMarketplaceFixture);

        await expect(marketplace.connect(buyer).createListing(
          await nft.getAddress(),
          1,
          ethers.parseEther("1"),
          ethers.ZeroAddress,
          0,
          0
        )).to.be.revertedWithCustomError(marketplace, "NotTokenOwner");
      });

      it("Should reject listing without approval", async function () {
        const { marketplace, nft, seller } = await loadFixture(deployMarketplaceFixture);

        await nft.connect(seller).setApprovalForAll(await marketplace.getAddress(), false);

        await expect(marketplace.connect(seller).createListing(
          await nft.getAddress(),
          1,
          ethers.parseEther("1"),
          ethers.ZeroAddress,
          0,
          0
        )).to.be.revertedWithCustomError(marketplace, "NotApproved");
      });

      it("Should reject zero price", async function () {
        const { marketplace, nft, seller } = await loadFixture(deployMarketplaceFixture);

        await expect(marketplace.connect(seller).createListing(
          await nft.getAddress(),
          1,
          0,
          ethers.ZeroAddress,
          0,
          0
        )).to.be.revertedWithCustomError(marketplace, "InvalidPrice");
      });
    });

    describe("Buying", function () {
      async function listedNFTFixture() {
        const fixture = await loadFixture(deployMarketplaceFixture);
        const { marketplace, nft, seller } = fixture;

        await marketplace.connect(seller).createListing(
          await nft.getAddress(),
          1,
          ethers.parseEther("1"),
          ethers.ZeroAddress,
          0,
          0
        );

        return fixture;
      }

      it("Should allow purchase with ETH", async function () {
        const { marketplace, nft, buyer } = await listedNFTFixture();

        await marketplace.connect(buyer).buy(1, { value: ethers.parseEther("1") });

        expect(await nft.ownerOf(1)).to.equal(buyer.address);
      });

      it("Should transfer payment to seller", async function () {
        const { marketplace, seller, buyer } = await listedNFTFixture();

        const balanceBefore = await ethers.provider.getBalance(seller.address);
        await marketplace.connect(buyer).buy(1, { value: ethers.parseEther("1") });
        const balanceAfter = await ethers.provider.getBalance(seller.address);

        // Seller receives price - fees
        expect(balanceAfter).to.be.gt(balanceBefore);
      });

      it("Should collect marketplace fee", async function () {
        const { marketplace, feeRecipient, buyer } = await listedNFTFixture();

        const balanceBefore = await ethers.provider.getBalance(feeRecipient.address);
        await marketplace.connect(buyer).buy(1, { value: ethers.parseEther("1") });
        const balanceAfter = await ethers.provider.getBalance(feeRecipient.address);

        expect(balanceAfter).to.be.gt(balanceBefore);
      });

      it("Should emit Sale event", async function () {
        const { marketplace, buyer } = await listedNFTFixture();

        await expect(marketplace.connect(buyer).buy(1, { value: ethers.parseEther("1") }))
          .to.emit(marketplace, "Sale");
      });

      it("Should reject insufficient payment", async function () {
        const { marketplace, buyer } = await listedNFTFixture();

        await expect(marketplace.connect(buyer).buy(1, { value: ethers.parseEther("0.5") }))
          .to.be.revertedWithCustomError(marketplace, "InsufficientPayment");
      });

      it("Should reject buying non-existent listing", async function () {
        const { marketplace, buyer } = await listedNFTFixture();

        await expect(marketplace.connect(buyer).buy(999, { value: ethers.parseEther("1") }))
          .to.be.revertedWithCustomError(marketplace, "ListingNotFound");
      });
    });

    describe("Cancellation", function () {
      async function listedNFTFixture() {
        const fixture = await loadFixture(deployMarketplaceFixture);
        const { marketplace, nft, seller } = fixture;

        await marketplace.connect(seller).createListing(
          await nft.getAddress(),
          1,
          ethers.parseEther("1"),
          ethers.ZeroAddress,
          0,
          0
        );

        return fixture;
      }

      it("Should allow seller to cancel", async function () {
        const { marketplace, seller } = await listedNFTFixture();

        await marketplace.connect(seller).cancelListing(1);

        const listing = await marketplace.getListing(1);
        expect(listing.active).to.be.false;
      });

      it("Should emit ListingCancelled event", async function () {
        const { marketplace, seller } = await listedNFTFixture();

        await expect(marketplace.connect(seller).cancelListing(1))
          .to.emit(marketplace, "ListingCancelled");
      });

      it("Should reject cancellation by non-seller", async function () {
        const { marketplace, buyer } = await listedNFTFixture();

        await expect(marketplace.connect(buyer).cancelListing(1))
          .to.be.revertedWithCustomError(marketplace, "NotSeller");
      });
    });

    describe("Auctions", function () {
      async function auctionListingFixture() {
        const fixture = await loadFixture(deployMarketplaceFixture);
        const { marketplace, nft, seller } = fixture;

        const deadline = (await time.latest()) + 86400;

        await marketplace.connect(seller).createListing(
          await nft.getAddress(),
          1,
          ethers.parseEther("0.5"),
          ethers.ZeroAddress,
          1,
          deadline
        );

        return { ...fixture, deadline };
      }

      it("Should accept bids", async function () {
        const { marketplace, buyer } = await auctionListingFixture();

        await marketplace.connect(buyer).placeBid(1, { value: ethers.parseEther("0.6") });

        const listing = await marketplace.getListing(1);
        expect(listing.highestBidder).to.equal(buyer.address);
      });

      it("Should reject bid below current", async function () {
        const { marketplace, buyer, buyer2 } = await auctionListingFixture();

        await marketplace.connect(buyer).placeBid(1, { value: ethers.parseEther("0.6") });

        await expect(marketplace.connect(buyer2).placeBid(1, { value: ethers.parseEther("0.55") }))
          .to.be.revertedWithCustomError(marketplace, "BidTooLow");
      });

      it("Should allow settling after deadline", async function () {
        const { marketplace, nft, buyer } = await auctionListingFixture();

        await marketplace.connect(buyer).placeBid(1, { value: ethers.parseEther("0.6") });
        
        await time.increase(86401);

        await marketplace.settleAuction(1);

        expect(await nft.ownerOf(1)).to.equal(buyer.address);
      });

      it("Should reject settling before deadline", async function () {
        const { marketplace, buyer } = await auctionListingFixture();

        await marketplace.connect(buyer).placeBid(1, { value: ethers.parseEther("0.6") });

        await expect(marketplace.settleAuction(1))
          .to.be.revertedWithCustomError(marketplace, "AuctionNotEnded");
      });

      it("Should refund outbid bidder", async function () {
        const { marketplace, buyer, buyer2 } = await auctionListingFixture();

        await marketplace.connect(buyer).placeBid(1, { value: ethers.parseEther("0.6") });

        const balanceBefore = await ethers.provider.getBalance(buyer.address);
        await marketplace.connect(buyer2).placeBid(1, { value: ethers.parseEther("0.7") });
        const balanceAfter = await ethers.provider.getBalance(buyer.address);

        expect(balanceAfter).to.be.gt(balanceBefore);
      });
    });

    describe("Offers", function () {
      it("Should create offer", async function () {
        const { marketplace, nft, buyer } = await loadFixture(deployMarketplaceFixture);

        await marketplace.connect(buyer).makeOffer(
          await nft.getAddress(),
          1,
          ethers.ZeroAddress,
          86400,
          { value: ethers.parseEther("0.5") }
        );

        const offer = await marketplace.getOffer(1);
        expect(offer.offerer).to.equal(buyer.address);
      });

      it("Should allow seller to accept offer", async function () {
        const { marketplace, nft, seller, buyer } = await loadFixture(deployMarketplaceFixture);

        await marketplace.connect(buyer).makeOffer(
          await nft.getAddress(),
          1,
          ethers.ZeroAddress,
          86400,
          { value: ethers.parseEther("0.5") }
        );

        await marketplace.connect(seller).acceptOffer(1);

        expect(await nft.ownerOf(1)).to.equal(buyer.address);
      });

      it("Should allow offerer to cancel", async function () {
        const { marketplace, nft, buyer } = await loadFixture(deployMarketplaceFixture);

        await marketplace.connect(buyer).makeOffer(
          await nft.getAddress(),
          1,
          ethers.ZeroAddress,
          86400,
          { value: ethers.parseEther("0.5") }
        );

        const balanceBefore = await ethers.provider.getBalance(buyer.address);
        await marketplace.connect(buyer).cancelOffer(1);
        const balanceAfter = await ethers.provider.getBalance(buyer.address);

        expect(balanceAfter).to.be.gt(balanceBefore);
      });
    });

    describe("ERC20 Payments", function () {
      async function listedWithERC20Fixture() {
        const fixture = await loadFixture(deployMarketplaceFixture);
        const { marketplace, nft, paymentToken, seller, buyer } = fixture;

        await marketplace.connect(seller).createListing(
          await nft.getAddress(),
          1,
          ethers.parseEther("100"),
          await paymentToken.getAddress(),
          0,
          0
        );

        return fixture;
      }

      it("Should accept ERC20 payment", async function () {
        const { marketplace, nft, buyer } = await listedWithERC20Fixture();

        await marketplace.connect(buyer).buy(1);

        expect(await nft.ownerOf(1)).to.equal(buyer.address);
      });

      it("Should transfer ERC20 to seller", async function () {
        const { marketplace, paymentToken, seller, buyer } = await listedWithERC20Fixture();

        const balanceBefore = await paymentToken.balanceOf(seller.address);
        await marketplace.connect(buyer).buy(1);
        const balanceAfter = await paymentToken.balanceOf(seller.address);

        expect(balanceAfter).to.be.gt(balanceBefore);
      });
    });
  });

  // ============================================
  // ESCROW TESTS
  // ============================================

  describe("Escrow", function () {
    describe("Creating Escrows", function () {
      it("Should create escrow with ETH", async function () {
        const { escrow, seller, buyer } = await loadFixture(deployMarketplaceFixture);

        await escrow.connect(buyer).createEscrow(
          seller.address,
          ethers.ZeroAddress,
          0,
          86400,
          "Purchase of service",
          { value: ethers.parseEther("1") }
        );

        const escrowData = await escrow.getEscrow(1);
        expect(escrowData.buyer).to.equal(buyer.address);
        expect(escrowData.seller).to.equal(seller.address);
      });

      it("Should create escrow with ERC20", async function () {
        const { escrow, paymentToken, seller, buyer } = await loadFixture(deployMarketplaceFixture);

        await paymentToken.connect(buyer).approve(await escrow.getAddress(), ethers.MaxUint256);

        await escrow.connect(buyer).createEscrow(
          seller.address,
          await paymentToken.getAddress(),
          ethers.parseEther("100"),
          86400,
          "Token purchase"
        );

        const escrowData = await escrow.getEscrow(1);
        expect(escrowData.amount).to.equal(ethers.parseEther("100"));
      });

      it("Should emit EscrowCreated event", async function () {
        const { escrow, seller, buyer } = await loadFixture(deployMarketplaceFixture);

        await expect(escrow.connect(buyer).createEscrow(
          seller.address,
          ethers.ZeroAddress,
          0,
          86400,
          "Test",
          { value: ethers.parseEther("1") }
        )).to.emit(escrow, "EscrowCreated");
      });
    });

    describe("Releases", function () {
      async function activeEscrowFixture() {
        const fixture = await loadFixture(deployMarketplaceFixture);
        const { escrow, seller, buyer } = fixture;

        await escrow.connect(buyer).createEscrow(
          seller.address,
          ethers.ZeroAddress,
          0,
          86400,
          "Test escrow",
          { value: ethers.parseEther("1") }
        );

        return fixture;
      }

      it("Should allow buyer to release", async function () {
        const { escrow, seller, buyer } = await activeEscrowFixture();

        const balanceBefore = await ethers.provider.getBalance(seller.address);
        await escrow.connect(buyer).release(1);
        const balanceAfter = await ethers.provider.getBalance(seller.address);

        expect(balanceAfter).to.be.gt(balanceBefore);
      });

      it("Should emit EscrowReleased event", async function () {
        const { escrow, buyer } = await activeEscrowFixture();

        await expect(escrow.connect(buyer).release(1))
          .to.emit(escrow, "EscrowReleased");
      });

      it("Should reject release by non-buyer", async function () {
        const { escrow, seller } = await activeEscrowFixture();

        await expect(escrow.connect(seller).release(1))
          .to.be.revertedWithCustomError(escrow, "NotBuyer");
      });

      it("Should reject double release", async function () {
        const { escrow, buyer } = await activeEscrowFixture();

        await escrow.connect(buyer).release(1);

        await expect(escrow.connect(buyer).release(1))
          .to.be.revertedWithCustomError(escrow, "EscrowNotActive");
      });
    });

    describe("Refunds", function () {
      async function activeEscrowFixture() {
        const fixture = await loadFixture(deployMarketplaceFixture);
        const { escrow, seller, buyer } = fixture;

        await escrow.connect(buyer).createEscrow(
          seller.address,
          ethers.ZeroAddress,
          0,
          86400,
          "Test escrow",
          { value: ethers.parseEther("1") }
        );

        return fixture;
      }

      it("Should allow seller to refund", async function () {
        const { escrow, seller, buyer } = await activeEscrowFixture();

        const balanceBefore = await ethers.provider.getBalance(buyer.address);
        await escrow.connect(seller).refund(1);
        const balanceAfter = await ethers.provider.getBalance(buyer.address);

        expect(balanceAfter).to.be.gt(balanceBefore);
      });

      it("Should emit EscrowRefunded event", async function () {
        const { escrow, seller } = await activeEscrowFixture();

        await expect(escrow.connect(seller).refund(1))
          .to.emit(escrow, "EscrowRefunded");
      });

      it("Should reject refund by non-seller", async function () {
        const { escrow, buyer } = await activeEscrowFixture();

        await expect(escrow.connect(buyer).refund(1))
          .to.be.revertedWithCustomError(escrow, "NotSeller");
      });
    });

    describe("Disputes", function () {
      async function activeEscrowFixture() {
        const fixture = await loadFixture(deployMarketplaceFixture);
        const { escrow, seller, buyer } = fixture;

        await escrow.connect(buyer).createEscrow(
          seller.address,
          ethers.ZeroAddress,
          0,
          86400,
          "Test escrow",
          { value: ethers.parseEther("1") }
        );

        return fixture;
      }

      it("Should allow buyer to dispute", async function () {
        const { escrow, buyer } = await activeEscrowFixture();

        await escrow.connect(buyer).dispute(1, "Goods not received");

        const escrowData = await escrow.getEscrow(1);
        expect(escrowData.disputed).to.be.true;
      });

      it("Should allow seller to dispute", async function () {
        const { escrow, seller } = await activeEscrowFixture();

        await escrow.connect(seller).dispute(1, "Buyer not paying for extras");

        const escrowData = await escrow.getEscrow(1);
        expect(escrowData.disputed).to.be.true;
      });

      it("Should emit EscrowDisputed event", async function () {
        const { escrow, buyer } = await activeEscrowFixture();

        await expect(escrow.connect(buyer).dispute(1, "Issue"))
          .to.emit(escrow, "EscrowDisputed");
      });

      it("Should allow admin to resolve dispute", async function () {
        const { escrow, owner, buyer } = await activeEscrowFixture();

        await escrow.connect(buyer).dispute(1, "Issue");

        await escrow.connect(owner).resolveDispute(1, buyer.address, ethers.parseEther("1"));

        const escrowData = await escrow.getEscrow(1);
        expect(escrowData.resolved).to.be.true;
      });
    });

    describe("Expiration", function () {
      async function activeEscrowFixture() {
        const fixture = await loadFixture(deployMarketplaceFixture);
        const { escrow, seller, buyer } = fixture;

        await escrow.connect(buyer).createEscrow(
          seller.address,
          ethers.ZeroAddress,
          0,
          86400,
          "Test escrow",
          { value: ethers.parseEther("1") }
        );

        return fixture;
      }

      it("Should auto-release after deadline if not disputed", async function () {
        const { escrow, seller, buyer } = await activeEscrowFixture();

        await time.increase(86401);

        // Seller can claim after expiry
        const balanceBefore = await ethers.provider.getBalance(seller.address);
        await escrow.connect(seller).claimExpired(1);
        const balanceAfter = await ethers.provider.getBalance(seller.address);

        expect(balanceAfter).to.be.gt(balanceBefore);
      });

      it("Should reject claim before expiry", async function () {
        const { escrow, seller } = await activeEscrowFixture();

        await expect(escrow.connect(seller).claimExpired(1))
          .to.be.revertedWithCustomError(escrow, "EscrowNotExpired");
      });
    });
  });

  // ============================================
  // MARKETPLACE INTEGRATION TESTS
  // ============================================

  describe("Marketplace Integration", function () {
    it("Should handle full listing to purchase flow", async function () {
      const { marketplace, nft, seller, buyer, feeRecipient, royaltyRecipient } = 
        await loadFixture(deployMarketplaceFixture);

      // List
      await marketplace.connect(seller).createListing(
        await nft.getAddress(),
        1,
        ethers.parseEther("10"),
        ethers.ZeroAddress,
        0,
        0
      );

      // Buy
      const sellerBalanceBefore = await ethers.provider.getBalance(seller.address);
      const feeBalanceBefore = await ethers.provider.getBalance(feeRecipient.address);

      await marketplace.connect(buyer).buy(1, { value: ethers.parseEther("10") });

      // Verify transfers
      expect(await nft.ownerOf(1)).to.equal(buyer.address);
      expect(await ethers.provider.getBalance(seller.address)).to.be.gt(sellerBalanceBefore);
      expect(await ethers.provider.getBalance(feeRecipient.address)).to.be.gt(feeBalanceBefore);
    });

    it("Should handle auction with multiple bidders", async function () {
      const { marketplace, nft, seller, buyer, buyer2 } = await loadFixture(deployMarketplaceFixture);

      const deadline = (await time.latest()) + 86400;

      await marketplace.connect(seller).createListing(
        await nft.getAddress(),
        1,
        ethers.parseEther("1"),
        ethers.ZeroAddress,
        1,
        deadline
      );

      // Bidding war
      await marketplace.connect(buyer).placeBid(1, { value: ethers.parseEther("1.1") });
      await marketplace.connect(buyer2).placeBid(1, { value: ethers.parseEther("1.5") });
      await marketplace.connect(buyer).placeBid(1, { value: ethers.parseEther("2") });

      await time.increase(86401);
      await marketplace.settleAuction(1);

      expect(await nft.ownerOf(1)).to.equal(buyer.address);
    });

    it("Should handle escrow with ERC20 tokens", async function () {
      const { escrow, paymentToken, seller, buyer } = await loadFixture(deployMarketplaceFixture);

      await paymentToken.connect(buyer).approve(await escrow.getAddress(), ethers.MaxUint256);

      await escrow.connect(buyer).createEscrow(
        seller.address,
        await paymentToken.getAddress(),
        ethers.parseEther("500"),
        86400,
        "Large purchase"
      );

      const sellerBalanceBefore = await paymentToken.balanceOf(seller.address);
      await escrow.connect(buyer).release(1);
      const sellerBalanceAfter = await paymentToken.balanceOf(seller.address);

      expect(sellerBalanceAfter).to.be.gt(sellerBalanceBefore);
    });

    it("Should handle multiple concurrent listings", async function () {
      const { marketplace, nft, seller, buyer, buyer2 } = await loadFixture(deployMarketplaceFixture);

      // Create multiple listings
      for (let i = 1; i <= 5; i++) {
        await marketplace.connect(seller).createListing(
          await nft.getAddress(),
          i,
          ethers.parseEther(String(i)),
          ethers.ZeroAddress,
          0,
          0
        );
      }

      // Buy different NFTs
      await marketplace.connect(buyer).buy(1, { value: ethers.parseEther("1") });
      await marketplace.connect(buyer2).buy(3, { value: ethers.parseEther("3") });

      expect(await nft.ownerOf(1)).to.equal(buyer.address);
      expect(await nft.ownerOf(3)).to.equal(buyer2.address);
      expect(await nft.ownerOf(2)).to.equal(seller.address);  // Still owned by seller
    });
  });
});
