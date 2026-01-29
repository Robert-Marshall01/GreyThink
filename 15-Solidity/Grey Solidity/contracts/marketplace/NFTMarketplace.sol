// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/token/ERC721/IERC721.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/Pausable.sol";
import "@openzeppelin/contracts/interfaces/IERC2981.sol";

/**
 * @title NFTMarketplace
 * @author Grey Solidity Project
 * @notice Full-featured NFT marketplace with listings, bids, and auctions
 * @dev Supports ERC721 with royalties (EIP-2981), multiple payment tokens
 */
contract NFTMarketplace is AccessControl, ReentrancyGuard, Pausable {
    using SafeERC20 for IERC20;

    /// @notice Role for fee management
    bytes32 public constant FEE_MANAGER_ROLE = keccak256("FEE_MANAGER_ROLE");

    /// @notice Listing types
    enum ListingType {
        FixedPrice,
        Auction
    }

    /// @notice Listing status
    enum ListingStatus {
        Active,
        Sold,
        Cancelled,
        Expired
    }

    /// @notice Listing structure
    struct Listing {
        address seller;
        address nftContract;
        uint256 tokenId;
        address paymentToken;      // address(0) = ETH
        uint256 price;
        uint256 startTime;
        uint256 endTime;
        ListingType listingType;
        ListingStatus status;
    }

    /// @notice Bid structure
    struct Bid {
        address bidder;
        uint256 amount;
        uint256 timestamp;
        bool active;
    }

    /// @notice Auction structure
    struct Auction {
        uint256 highestBid;
        address highestBidder;
        uint256 reservePrice;
        uint256 minBidIncrement;
        bool settled;
    }

    /// @notice Platform fee in basis points
    uint256 public platformFee;

    /// @notice Fee recipient
    address public feeRecipient;

    /// @notice Mapping of listing ID to listing
    mapping(uint256 => Listing) public listings;

    /// @notice Mapping of listing ID to auction data
    mapping(uint256 => Auction) public auctions;

    /// @notice Mapping from listing ID to bids
    mapping(uint256 => Bid[]) public listingBids;

    /// @notice Mapping of (user, listing) to bid amount
    mapping(address => mapping(uint256 => uint256)) public userBids;

    /// @notice Next listing ID
    uint256 public nextListingId;

    /// @notice Approved payment tokens
    mapping(address => bool) public approvedPaymentTokens;

    /// @notice Approved NFT collections
    mapping(address => bool) public approvedCollections;

    /// @notice User listing IDs
    mapping(address => uint256[]) public userListings;

    /// @notice Collection listing IDs
    mapping(address => uint256[]) public collectionListings;

    // ============ Events ============

    event Listed(
        uint256 indexed listingId,
        address indexed seller,
        address indexed nftContract,
        uint256 tokenId,
        uint256 price,
        ListingType listingType
    );

    event Sale(
        uint256 indexed listingId,
        address indexed seller,
        address indexed buyer,
        uint256 price,
        uint256 royalty,
        uint256 platformFeeAmount
    );

    event BidPlaced(
        uint256 indexed listingId,
        address indexed bidder,
        uint256 amount
    );

    event BidWithdrawn(
        uint256 indexed listingId,
        address indexed bidder,
        uint256 amount
    );

    event AuctionSettled(
        uint256 indexed listingId,
        address indexed winner,
        uint256 amount
    );

    event ListingCancelled(uint256 indexed listingId);
    event PlatformFeeUpdated(uint256 oldFee, uint256 newFee);
    event PaymentTokenUpdated(address indexed token, bool approved);
    event CollectionUpdated(address indexed collection, bool approved);

    // ============ Errors ============

    error NotSeller();
    error NotBidder();
    error ListingNotActive();
    error ListingExpired();
    error ListingNotExpired();
    error InsufficientPayment(uint256 required, uint256 provided);
    error InvalidPrice();
    error InvalidDuration();
    error CollectionNotApproved(address collection);
    error PaymentTokenNotApproved(address token);
    error BidTooLow(uint256 minimum, uint256 provided);
    error AuctionNotEnded();
    error AuctionAlreadySettled();
    error NoBidsToWithdraw();
    error CannotWithdrawWinningBid();
    error NotAuction();
    error TransferFailed();

    /**
     * @notice Initializes the marketplace
     * @param platformFee_ Initial platform fee (basis points)
     * @param feeRecipient_ Fee recipient address
     */
    constructor(uint256 platformFee_, address feeRecipient_) {
        require(platformFee_ <= 1000, "NFTMarketplace: fee too high");
        platformFee = platformFee_;
        feeRecipient = feeRecipient_;

        // ETH is always approved
        approvedPaymentTokens[address(0)] = true;

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(FEE_MANAGER_ROLE, msg.sender);
    }

    // ============ Listing Functions ============

    /**
     * @notice Creates a fixed price listing
     * @param nftContract NFT contract address
     * @param tokenId Token ID
     * @param paymentToken Payment token (address(0) = ETH)
     * @param price Listing price
     * @param duration Listing duration in seconds
     * @return listingId The listing ID
     */
    function createListing(
        address nftContract,
        uint256 tokenId,
        address paymentToken,
        uint256 price,
        uint256 duration
    ) external whenNotPaused returns (uint256 listingId) {
        if (!approvedCollections[nftContract]) {
            revert CollectionNotApproved(nftContract);
        }
        if (!approvedPaymentTokens[paymentToken]) {
            revert PaymentTokenNotApproved(paymentToken);
        }
        if (price == 0) revert InvalidPrice();
        if (duration == 0 || duration > 365 days) revert InvalidDuration();

        // Transfer NFT to marketplace
        IERC721(nftContract).transferFrom(msg.sender, address(this), tokenId);

        listingId = nextListingId++;

        listings[listingId] = Listing({
            seller: msg.sender,
            nftContract: nftContract,
            tokenId: tokenId,
            paymentToken: paymentToken,
            price: price,
            startTime: block.timestamp,
            endTime: block.timestamp + duration,
            listingType: ListingType.FixedPrice,
            status: ListingStatus.Active
        });

        userListings[msg.sender].push(listingId);
        collectionListings[nftContract].push(listingId);

        emit Listed(listingId, msg.sender, nftContract, tokenId, price, ListingType.FixedPrice);
    }

    /**
     * @notice Creates an auction listing
     * @param nftContract NFT contract address
     * @param tokenId Token ID
     * @param paymentToken Payment token
     * @param reservePrice Reserve price
     * @param minBidIncrement Minimum bid increment
     * @param duration Auction duration
     * @return listingId The listing ID
     */
    function createAuction(
        address nftContract,
        uint256 tokenId,
        address paymentToken,
        uint256 reservePrice,
        uint256 minBidIncrement,
        uint256 duration
    ) external whenNotPaused returns (uint256 listingId) {
        if (!approvedCollections[nftContract]) {
            revert CollectionNotApproved(nftContract);
        }
        if (!approvedPaymentTokens[paymentToken]) {
            revert PaymentTokenNotApproved(paymentToken);
        }
        if (duration == 0 || duration > 30 days) revert InvalidDuration();

        IERC721(nftContract).transferFrom(msg.sender, address(this), tokenId);

        listingId = nextListingId++;

        listings[listingId] = Listing({
            seller: msg.sender,
            nftContract: nftContract,
            tokenId: tokenId,
            paymentToken: paymentToken,
            price: reservePrice,
            startTime: block.timestamp,
            endTime: block.timestamp + duration,
            listingType: ListingType.Auction,
            status: ListingStatus.Active
        });

        auctions[listingId] = Auction({
            highestBid: 0,
            highestBidder: address(0),
            reservePrice: reservePrice,
            minBidIncrement: minBidIncrement > 0 ? minBidIncrement : reservePrice / 20,
            settled: false
        });

        userListings[msg.sender].push(listingId);
        collectionListings[nftContract].push(listingId);

        emit Listed(listingId, msg.sender, nftContract, tokenId, reservePrice, ListingType.Auction);
    }

    /**
     * @notice Buys a fixed-price listing with ETH
     * @param listingId The listing ID
     */
    function buyWithETH(uint256 listingId) external payable nonReentrant whenNotPaused {
        Listing storage listing = listings[listingId];
        
        _validateListing(listing);
        if (listing.listingType != ListingType.FixedPrice) revert NotAuction();
        if (listing.paymentToken != address(0)) {
            revert PaymentTokenNotApproved(address(0));
        }
        if (msg.value < listing.price) {
            revert InsufficientPayment(listing.price, msg.value);
        }

        listing.status = ListingStatus.Sold;

        (uint256 sellerAmount, uint256 royaltyAmount, uint256 feeAmount) = _calculatePayouts(
            listing.nftContract,
            listing.tokenId,
            listing.price
        );

        // Transfer NFT to buyer
        IERC721(listing.nftContract).transferFrom(address(this), msg.sender, listing.tokenId);

        // Pay royalty
        address royaltyRecipient = _getRoyaltyRecipient(listing.nftContract, listing.tokenId);
        if (royaltyAmount > 0 && royaltyRecipient != address(0)) {
            (bool royaltySuccess, ) = royaltyRecipient.call{value: royaltyAmount}("");
            if (!royaltySuccess) revert TransferFailed();
        }

        // Pay platform fee
        if (feeAmount > 0) {
            (bool feeSuccess, ) = feeRecipient.call{value: feeAmount}("");
            if (!feeSuccess) revert TransferFailed();
        }

        // Pay seller
        (bool sellerSuccess, ) = listing.seller.call{value: sellerAmount}("");
        if (!sellerSuccess) revert TransferFailed();

        // Refund excess
        if (msg.value > listing.price) {
            (bool refundSuccess, ) = msg.sender.call{value: msg.value - listing.price}("");
            if (!refundSuccess) revert TransferFailed();
        }

        emit Sale(listingId, listing.seller, msg.sender, listing.price, royaltyAmount, feeAmount);
    }

    /**
     * @notice Buys a fixed-price listing with tokens
     * @param listingId The listing ID
     */
    function buyWithToken(uint256 listingId) external nonReentrant whenNotPaused {
        Listing storage listing = listings[listingId];
        
        _validateListing(listing);
        if (listing.listingType != ListingType.FixedPrice) revert NotAuction();
        if (listing.paymentToken == address(0)) {
            revert PaymentTokenNotApproved(listing.paymentToken);
        }

        listing.status = ListingStatus.Sold;

        (uint256 sellerAmount, uint256 royaltyAmount, uint256 feeAmount) = _calculatePayouts(
            listing.nftContract,
            listing.tokenId,
            listing.price
        );

        IERC20 token = IERC20(listing.paymentToken);

        // Transfer payment from buyer
        token.safeTransferFrom(msg.sender, address(this), listing.price);

        // Transfer NFT to buyer
        IERC721(listing.nftContract).transferFrom(address(this), msg.sender, listing.tokenId);

        // Pay royalty
        address royaltyRecipient = _getRoyaltyRecipient(listing.nftContract, listing.tokenId);
        if (royaltyAmount > 0 && royaltyRecipient != address(0)) {
            token.safeTransfer(royaltyRecipient, royaltyAmount);
        }

        // Pay platform fee
        if (feeAmount > 0) {
            token.safeTransfer(feeRecipient, feeAmount);
        }

        // Pay seller
        token.safeTransfer(listing.seller, sellerAmount);

        emit Sale(listingId, listing.seller, msg.sender, listing.price, royaltyAmount, feeAmount);
    }

    // ============ Auction Functions ============

    /**
     * @notice Places a bid on an auction
     * @param listingId The listing ID
     * @param amount Bid amount (for token payments)
     */
    function placeBid(uint256 listingId, uint256 amount) external payable nonReentrant whenNotPaused {
        Listing storage listing = listings[listingId];
        Auction storage auction = auctions[listingId];

        _validateListing(listing);
        if (listing.listingType != ListingType.Auction) revert NotAuction();

        uint256 bidAmount = listing.paymentToken == address(0) ? msg.value : amount;
        uint256 minBid = auction.highestBid == 0 
            ? auction.reservePrice 
            : auction.highestBid + auction.minBidIncrement;

        if (bidAmount < minBid) {
            revert BidTooLow(minBid, bidAmount);
        }

        // Handle token payment
        if (listing.paymentToken != address(0)) {
            IERC20(listing.paymentToken).safeTransferFrom(msg.sender, address(this), bidAmount);
        }

        // Refund previous highest bidder
        if (auction.highestBidder != address(0)) {
            _refundBid(listing.paymentToken, auction.highestBidder, auction.highestBid);
        }

        auction.highestBid = bidAmount;
        auction.highestBidder = msg.sender;

        listingBids[listingId].push(Bid({
            bidder: msg.sender,
            amount: bidAmount,
            timestamp: block.timestamp,
            active: true
        }));

        userBids[msg.sender][listingId] = bidAmount;

        emit BidPlaced(listingId, msg.sender, bidAmount);
    }

    /**
     * @notice Settles a completed auction
     * @param listingId The listing ID
     */
    function settleAuction(uint256 listingId) external nonReentrant {
        Listing storage listing = listings[listingId];
        Auction storage auction = auctions[listingId];

        if (listing.status != ListingStatus.Active) revert ListingNotActive();
        if (listing.listingType != ListingType.Auction) revert NotAuction();
        if (block.timestamp < listing.endTime) revert AuctionNotEnded();
        if (auction.settled) revert AuctionAlreadySettled();

        auction.settled = true;
        listing.status = ListingStatus.Sold;

        if (auction.highestBidder == address(0)) {
            // No bids, return NFT to seller
            IERC721(listing.nftContract).transferFrom(address(this), listing.seller, listing.tokenId);
            listing.status = ListingStatus.Expired;
            emit ListingCancelled(listingId);
            return;
        }

        (uint256 sellerAmount, uint256 royaltyAmount, uint256 feeAmount) = _calculatePayouts(
            listing.nftContract,
            listing.tokenId,
            auction.highestBid
        );

        // Transfer NFT to winner
        IERC721(listing.nftContract).transferFrom(address(this), auction.highestBidder, listing.tokenId);

        // Distribute funds
        if (listing.paymentToken == address(0)) {
            address royaltyRecipient = _getRoyaltyRecipient(listing.nftContract, listing.tokenId);
            if (royaltyAmount > 0 && royaltyRecipient != address(0)) {
                (bool s1, ) = royaltyRecipient.call{value: royaltyAmount}("");
                require(s1, "Royalty transfer failed");
            }
            if (feeAmount > 0) {
                (bool s2, ) = feeRecipient.call{value: feeAmount}("");
                require(s2, "Fee transfer failed");
            }
            (bool s3, ) = listing.seller.call{value: sellerAmount}("");
            require(s3, "Seller transfer failed");
        } else {
            IERC20 token = IERC20(listing.paymentToken);
            address royaltyRecipient = _getRoyaltyRecipient(listing.nftContract, listing.tokenId);
            if (royaltyAmount > 0 && royaltyRecipient != address(0)) {
                token.safeTransfer(royaltyRecipient, royaltyAmount);
            }
            if (feeAmount > 0) {
                token.safeTransfer(feeRecipient, feeAmount);
            }
            token.safeTransfer(listing.seller, sellerAmount);
        }

        emit AuctionSettled(listingId, auction.highestBidder, auction.highestBid);
    }

    /**
     * @notice Cancels a listing
     * @param listingId The listing ID
     */
    function cancelListing(uint256 listingId) external nonReentrant {
        Listing storage listing = listings[listingId];
        Auction storage auction = auctions[listingId];

        if (listing.seller != msg.sender) revert NotSeller();
        if (listing.status != ListingStatus.Active) revert ListingNotActive();

        // For auctions, can't cancel if there are bids
        if (listing.listingType == ListingType.Auction && auction.highestBidder != address(0)) {
            revert("Cannot cancel auction with bids");
        }

        listing.status = ListingStatus.Cancelled;

        // Return NFT
        IERC721(listing.nftContract).transferFrom(address(this), listing.seller, listing.tokenId);

        emit ListingCancelled(listingId);
    }

    // ============ View Functions ============

    /**
     * @notice Returns listing details
     * @param listingId The listing ID
     * @return The listing struct
     */
    function getListing(uint256 listingId) external view returns (Listing memory) {
        return listings[listingId];
    }

    /**
     * @notice Returns auction details
     * @param listingId The listing ID
     * @return The auction struct
     */
    function getAuction(uint256 listingId) external view returns (Auction memory) {
        return auctions[listingId];
    }

    /**
     * @notice Returns bids for a listing
     * @param listingId The listing ID
     * @return Array of bids
     */
    function getBids(uint256 listingId) external view returns (Bid[] memory) {
        return listingBids[listingId];
    }

    /**
     * @notice Returns user's listings
     * @param user The user address
     * @return Array of listing IDs
     */
    function getUserListings(address user) external view returns (uint256[] memory) {
        return userListings[user];
    }

    /**
     * @notice Returns collection listings
     * @param collection The collection address
     * @return Array of listing IDs
     */
    function getCollectionListings(address collection) external view returns (uint256[] memory) {
        return collectionListings[collection];
    }

    // ============ Admin Functions ============

    /**
     * @notice Sets the platform fee
     * @param newFee New fee in basis points
     */
    function setPlatformFee(uint256 newFee) external onlyRole(FEE_MANAGER_ROLE) {
        require(newFee <= 1000, "NFTMarketplace: fee too high");
        uint256 oldFee = platformFee;
        platformFee = newFee;
        emit PlatformFeeUpdated(oldFee, newFee);
    }

    /**
     * @notice Sets the fee recipient
     * @param newRecipient New recipient address
     */
    function setFeeRecipient(address newRecipient) external onlyRole(DEFAULT_ADMIN_ROLE) {
        require(newRecipient != address(0), "NFTMarketplace: zero address");
        feeRecipient = newRecipient;
    }

    /**
     * @notice Approves a payment token
     * @param token Token address
     * @param approved Approval status
     */
    function setPaymentToken(address token, bool approved) external onlyRole(DEFAULT_ADMIN_ROLE) {
        approvedPaymentTokens[token] = approved;
        emit PaymentTokenUpdated(token, approved);
    }

    /**
     * @notice Approves an NFT collection
     * @param collection Collection address
     * @param approved Approval status
     */
    function setCollection(address collection, bool approved) external onlyRole(DEFAULT_ADMIN_ROLE) {
        approvedCollections[collection] = approved;
        emit CollectionUpdated(collection, approved);
    }

    /**
     * @notice Pauses the marketplace
     */
    function pause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        _pause();
    }

    /**
     * @notice Unpauses the marketplace
     */
    function unpause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        _unpause();
    }

    // ============ Internal Functions ============

    function _validateListing(Listing storage listing) private view {
        if (listing.status != ListingStatus.Active) revert ListingNotActive();
        if (block.timestamp > listing.endTime) revert ListingExpired();
    }

    function _calculatePayouts(
        address nftContract,
        uint256 tokenId,
        uint256 salePrice
    ) private view returns (uint256 sellerAmount, uint256 royaltyAmount, uint256 feeAmount) {
        // Platform fee
        feeAmount = (salePrice * platformFee) / 10000;

        // Royalty (EIP-2981)
        try IERC2981(nftContract).royaltyInfo(tokenId, salePrice) returns (
            address,
            uint256 royalty
        ) {
            royaltyAmount = royalty;
        } catch {
            royaltyAmount = 0;
        }

        // Seller gets the rest
        sellerAmount = salePrice - feeAmount - royaltyAmount;
    }

    function _getRoyaltyRecipient(
        address nftContract,
        uint256 tokenId
    ) private view returns (address) {
        try IERC2981(nftContract).royaltyInfo(tokenId, 0) returns (
            address recipient,
            uint256
        ) {
            return recipient;
        } catch {
            return address(0);
        }
    }

    function _refundBid(address token, address bidder, uint256 amount) private {
        if (token == address(0)) {
            (bool success, ) = bidder.call{value: amount}("");
            require(success, "Refund failed");
        } else {
            IERC20(token).safeTransfer(bidder, amount);
        }
    }

    // Receive ETH
    receive() external payable {}
}
