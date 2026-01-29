const { expect } = require("chai");
const { ethers } = require("hardhat");
const { time } = require("@nomicfoundation/hardhat-network-helpers");

describe("NFTMarketplace", function () {
  let marketplace;
  let nft;
  let paymentToken;
  let owner, seller, buyer, feeRecipient;

  const PLATFORM_FEE = 250; // 2.5%
  const LISTING_PRICE = ethers.parseEther("1");
  const LISTING_DURATION = 7 * 24 * 60 * 60; // 7 days

  beforeEach(async function () {
    [owner, seller, buyer, feeRecipient] = await ethers.getSigners();

    // Deploy mock tokens
    const MockERC20 = await ethers.getContractFactory("MockERC20");
    paymentToken = await MockERC20.deploy("Payment Token", "PAY", 18);

    // Deploy NFT
    const GreyNFT = await ethers.getContractFactory("GreyNFT");
    nft = await GreyNFT.deploy(
      "Grey NFT",
      "GNFT",
      "https://api.grey.com/nft/",
      owner.address, // Royalty recipient
      500 // 5% royalty
    );

    // Deploy marketplace
    const NFTMarketplace = await ethers.getContractFactory("NFTMarketplace");
    marketplace = await NFTMarketplace.deploy(PLATFORM_FEE, feeRecipient.address);

    // Setup
    await marketplace.setCollection(await nft.getAddress(), true);
    await marketplace.setPaymentToken(await paymentToken.getAddress(), true);

    // Mint NFTs to seller
    await nft.mint(seller.address, "token1.json");
    await nft.mint(seller.address, "token2.json");
    await nft.mint(seller.address, "token3.json");

    // Approve marketplace
    await nft.connect(seller).setApprovalForAll(await marketplace.getAddress(), true);

    // Mint payment tokens to buyer
    await paymentToken.mint(buyer.address, ethers.parseEther("1000"));
    await paymentToken.connect(buyer).approve(await marketplace.getAddress(), ethers.MaxUint256);
  });

  describe("Deployment", function () {
    it("Should set correct platform fee", async function () {
      expect(await marketplace.platformFee()).to.equal(PLATFORM_FEE);
    });

    it("Should set correct fee recipient", async function () {
      expect(await marketplace.feeRecipient()).to.equal(feeRecipient.address);
    });

    it("Should approve ETH as payment token", async function () {
      expect(await marketplace.approvedPaymentTokens(ethers.ZeroAddress)).to.be.true;
    });
  });

  describe("Fixed Price Listings", function () {
    it("Should create a listing", async function () {
      const tx = await marketplace.connect(seller).createListing(
        await nft.getAddress(),
        0, // Token ID
        ethers.ZeroAddress, // ETH payment
        LISTING_PRICE,
        LISTING_DURATION
      );

      await expect(tx).to.emit(marketplace, "Listed");
    });

    it("Should transfer NFT to marketplace", async function () {
      await marketplace.connect(seller).createListing(
        await nft.getAddress(),
        0,
        ethers.ZeroAddress,
        LISTING_PRICE,
        LISTING_DURATION
      );

      expect(await nft.ownerOf(0)).to.equal(await marketplace.getAddress());
    });

    it("Should set correct listing data", async function () {
      await marketplace.connect(seller).createListing(
        await nft.getAddress(),
        0,
        ethers.ZeroAddress,
        LISTING_PRICE,
        LISTING_DURATION
      );

      const listing = await marketplace.getListing(0);
      expect(listing.seller).to.equal(seller.address);
      expect(listing.price).to.equal(LISTING_PRICE);
      expect(listing.listingType).to.equal(0); // FixedPrice
    });

    it("Should reject listing of unapproved collection", async function () {
      const MockERC20 = await ethers.getContractFactory("MockERC20");
      const fakeNft = await MockERC20.deploy("Fake", "F", 18);

      await expect(
        marketplace.connect(seller).createListing(
          await fakeNft.getAddress(),
          0,
          ethers.ZeroAddress,
          LISTING_PRICE,
          LISTING_DURATION
        )
      ).to.be.revertedWithCustomError(marketplace, "CollectionNotApproved");
    });
  });

  describe("Buying with ETH", function () {
    beforeEach(async function () {
      await marketplace.connect(seller).createListing(
        await nft.getAddress(),
        0,
        ethers.ZeroAddress,
        LISTING_PRICE,
        LISTING_DURATION
      );
    });

    it("Should complete purchase", async function () {
      await marketplace.connect(buyer).buyWithETH(0, { value: LISTING_PRICE });
      expect(await nft.ownerOf(0)).to.equal(buyer.address);
    });

    it("Should emit Sale event", async function () {
      await expect(
        marketplace.connect(buyer).buyWithETH(0, { value: LISTING_PRICE })
      ).to.emit(marketplace, "Sale");
    });

    it("Should pay seller after fees", async function () {
      const initialBalance = await ethers.provider.getBalance(seller.address);

      await marketplace.connect(buyer).buyWithETH(0, { value: LISTING_PRICE });

      const finalBalance = await ethers.provider.getBalance(seller.address);
      const received = finalBalance - initialBalance;

      // Seller should receive less than full price (due to platform fee + royalty)
      expect(received).to.be.gt(0);
      expect(received).to.be.lt(LISTING_PRICE);
    });

    it("Should pay platform fee", async function () {
      const initialBalance = await ethers.provider.getBalance(feeRecipient.address);

      await marketplace.connect(buyer).buyWithETH(0, { value: LISTING_PRICE });

      const finalBalance = await ethers.provider.getBalance(feeRecipient.address);
      const received = finalBalance - initialBalance;

      const expectedFee = (LISTING_PRICE * BigInt(PLATFORM_FEE)) / 10000n;
      expect(received).to.equal(expectedFee);
    });

    it("Should update listing status", async function () {
      await marketplace.connect(buyer).buyWithETH(0, { value: LISTING_PRICE });

      const listing = await marketplace.getListing(0);
      expect(listing.status).to.equal(1); // Sold
    });

    it("Should reject insufficient payment", async function () {
      await expect(
        marketplace.connect(buyer).buyWithETH(0, { value: LISTING_PRICE / 2n })
      ).to.be.revertedWithCustomError(marketplace, "InsufficientPayment");
    });

    it("Should refund excess payment", async function () {
      const excessAmount = LISTING_PRICE * 2n;
      const initialBalance = await ethers.provider.getBalance(buyer.address);

      const tx = await marketplace.connect(buyer).buyWithETH(0, { value: excessAmount });
      const receipt = await tx.wait();
      const gasUsed = receipt.gasUsed * receipt.gasPrice;

      const finalBalance = await ethers.provider.getBalance(buyer.address);
      const spent = initialBalance - finalBalance - gasUsed;

      // Should have only spent the listing price (approximately)
      expect(spent).to.be.closeTo(LISTING_PRICE, ethers.parseEther("0.01"));
    });
  });

  describe("Buying with Tokens", function () {
    beforeEach(async function () {
      await marketplace.connect(seller).createListing(
        await nft.getAddress(),
        0,
        await paymentToken.getAddress(),
        LISTING_PRICE,
        LISTING_DURATION
      );
    });

    it("Should complete token purchase", async function () {
      await marketplace.connect(buyer).buyWithToken(0);
      expect(await nft.ownerOf(0)).to.equal(buyer.address);
    });

    it("Should transfer tokens from buyer", async function () {
      const initialBalance = await paymentToken.balanceOf(buyer.address);

      await marketplace.connect(buyer).buyWithToken(0);

      const finalBalance = await paymentToken.balanceOf(buyer.address);
      expect(initialBalance - finalBalance).to.equal(LISTING_PRICE);
    });

    it("Should pay seller in tokens", async function () {
      const initialBalance = await paymentToken.balanceOf(seller.address);

      await marketplace.connect(buyer).buyWithToken(0);

      const finalBalance = await paymentToken.balanceOf(seller.address);
      expect(finalBalance).to.be.gt(initialBalance);
    });
  });

  describe("Auctions", function () {
    const RESERVE_PRICE = ethers.parseEther("0.5");
    const AUCTION_DURATION = 24 * 60 * 60; // 1 day

    beforeEach(async function () {
      await marketplace.connect(seller).createAuction(
        await nft.getAddress(),
        1, // Token ID
        ethers.ZeroAddress,
        RESERVE_PRICE,
        ethers.parseEther("0.1"), // Min bid increment
        AUCTION_DURATION
      );
    });

    it("Should create auction listing", async function () {
      const listing = await marketplace.getListing(0);
      expect(listing.listingType).to.equal(1); // Auction
    });

    it("Should set auction data", async function () {
      const auction = await marketplace.getAuction(0);
      expect(auction.reservePrice).to.equal(RESERVE_PRICE);
      expect(auction.highestBid).to.equal(0);
    });

    it("Should accept bids at or above reserve", async function () {
      await marketplace.connect(buyer).placeBid(0, 0, { value: RESERVE_PRICE });

      const auction = await marketplace.getAuction(0);
      expect(auction.highestBid).to.equal(RESERVE_PRICE);
      expect(auction.highestBidder).to.equal(buyer.address);
    });

    it("Should reject bids below reserve", async function () {
      await expect(
        marketplace.connect(buyer).placeBid(0, 0, { value: RESERVE_PRICE / 2n })
      ).to.be.revertedWithCustomError(marketplace, "BidTooLow");
    });

    it("Should refund previous bidder", async function () {
      await marketplace.connect(buyer).placeBid(0, 0, { value: RESERVE_PRICE });

      const initialBalance = await ethers.provider.getBalance(buyer.address);

      // Second bidder outbids
      await marketplace.connect(owner).placeBid(0, 0, { value: ethers.parseEther("1") });

      const finalBalance = await ethers.provider.getBalance(buyer.address);
      expect(finalBalance - initialBalance).to.equal(RESERVE_PRICE);
    });

    it("Should settle auction to highest bidder", async function () {
      await marketplace.connect(buyer).placeBid(0, 0, { value: ethers.parseEther("1") });

      // Fast forward past auction end
      await time.increase(AUCTION_DURATION + 1);

      await marketplace.settleAuction(0);

      expect(await nft.ownerOf(1)).to.equal(buyer.address);
    });

    it("Should return NFT to seller if no bids", async function () {
      // Fast forward
      await time.increase(AUCTION_DURATION + 1);

      await marketplace.settleAuction(0);

      expect(await nft.ownerOf(1)).to.equal(seller.address);
    });

    it("Should not settle before auction ends", async function () {
      await expect(
        marketplace.settleAuction(0)
      ).to.be.revertedWithCustomError(marketplace, "AuctionNotEnded");
    });
  });

  describe("Listing Cancellation", function () {
    beforeEach(async function () {
      await marketplace.connect(seller).createListing(
        await nft.getAddress(),
        0,
        ethers.ZeroAddress,
        LISTING_PRICE,
        LISTING_DURATION
      );
    });

    it("Should allow seller to cancel", async function () {
      await marketplace.connect(seller).cancelListing(0);

      const listing = await marketplace.getListing(0);
      expect(listing.status).to.equal(3); // Cancelled
    });

    it("Should return NFT to seller", async function () {
      await marketplace.connect(seller).cancelListing(0);
      expect(await nft.ownerOf(0)).to.equal(seller.address);
    });

    it("Should not allow non-seller to cancel", async function () {
      await expect(
        marketplace.connect(buyer).cancelListing(0)
      ).to.be.revertedWithCustomError(marketplace, "NotSeller");
    });

    it("Should not cancel auction with bids", async function () {
      // Create auction
      await marketplace.connect(seller).createAuction(
        await nft.getAddress(),
        1,
        ethers.ZeroAddress,
        RESERVE_PRICE,
        ethers.parseEther("0.1"),
        LISTING_DURATION
      );

      // Place bid
      await marketplace.connect(buyer).placeBid(1, 0, { value: RESERVE_PRICE });

      await expect(
        marketplace.connect(seller).cancelListing(1)
      ).to.be.reverted;
    });
  });

  describe("Admin Functions", function () {
    it("Should allow updating platform fee", async function () {
      await marketplace.setPlatformFee(500);
      expect(await marketplace.platformFee()).to.equal(500);
    });

    it("Should reject fee above maximum", async function () {
      await expect(marketplace.setPlatformFee(2000))
        .to.be.revertedWith("NFTMarketplace: fee too high");
    });

    it("Should allow approving collections", async function () {
      const MockERC20 = await ethers.getContractFactory("MockERC20");
      const newCollection = await MockERC20.deploy("New", "NEW", 18);

      await marketplace.setCollection(await newCollection.getAddress(), true);
      expect(await marketplace.approvedCollections(await newCollection.getAddress())).to.be.true;
    });

    it("Should allow pausing", async function () {
      await marketplace.pause();
      expect(await marketplace.paused()).to.be.true;
    });

    it("Should prevent operations when paused", async function () {
      await marketplace.pause();

      await expect(
        marketplace.connect(seller).createListing(
          await nft.getAddress(),
          0,
          ethers.ZeroAddress,
          LISTING_PRICE,
          LISTING_DURATION
        )
      ).to.be.revertedWithCustomError(marketplace, "EnforcedPause");
    });
  });
});

const RESERVE_PRICE = ethers.parseEther("0.5");
