// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

/**
 * @title IMarketplace
 * @notice Interface for NFT marketplace contracts
 * @dev Supports listings, bids, auctions, and trades
 */
interface IMarketplace {
    /// @notice Listing type enumeration
    enum ListingType {
        FixedPrice,
        Auction,
        DutchAuction
    }

    /// @notice Listing status enumeration
    enum ListingStatus {
        Active,
        Sold,
        Cancelled,
        Expired
    }

    /// @notice Bid status enumeration
    enum BidStatus {
        Active,
        Accepted,
        Rejected,
        Cancelled,
        Expired
    }

    /**
     * @notice Listing structure
     */
    struct Listing {
        uint256 listingId;
        address nftContract;
        uint256 tokenId;
        address seller;
        address paymentToken;
        uint256 price;
        uint256 startPrice;
        uint256 endPrice;
        uint256 startTime;
        uint256 endTime;
        ListingType listingType;
        ListingStatus status;
        uint256 quantity;
        bool is1155;
    }

    /**
     * @notice Bid structure
     */
    struct Bid {
        uint256 bidId;
        uint256 listingId;
        address bidder;
        uint256 amount;
        uint256 timestamp;
        uint256 expiresAt;
        BidStatus status;
    }

    /**
     * @notice Creates a fixed price listing
     * @param nftContract The NFT contract address
     * @param tokenId The token ID
     * @param paymentToken The payment token address (address(0) for ETH)
     * @param price The listing price
     * @param duration The listing duration in seconds
     * @param quantity The quantity (for ERC1155)
     * @return The listing ID
     */
    function createListing(
        address nftContract,
        uint256 tokenId,
        address paymentToken,
        uint256 price,
        uint256 duration,
        uint256 quantity
    ) external returns (uint256);

    /**
     * @notice Creates an auction listing
     * @param nftContract The NFT contract address
     * @param tokenId The token ID
     * @param paymentToken The payment token address
     * @param startPrice The starting price
     * @param reservePrice The reserve price (minimum accepted)
     * @param duration The auction duration in seconds
     * @return The listing ID
     */
    function createAuction(
        address nftContract,
        uint256 tokenId,
        address paymentToken,
        uint256 startPrice,
        uint256 reservePrice,
        uint256 duration
    ) external returns (uint256);

    /**
     * @notice Creates a Dutch auction listing
     * @param nftContract The NFT contract address
     * @param tokenId The token ID
     * @param paymentToken The payment token address
     * @param startPrice The starting price
     * @param endPrice The ending price
     * @param duration The auction duration
     * @return The listing ID
     */
    function createDutchAuction(
        address nftContract,
        uint256 tokenId,
        address paymentToken,
        uint256 startPrice,
        uint256 endPrice,
        uint256 duration
    ) external returns (uint256);

    /**
     * @notice Buys a fixed price listing
     * @param listingId The listing ID
     * @param quantity The quantity to buy (for ERC1155)
     */
    function buy(uint256 listingId, uint256 quantity) external payable;

    /**
     * @notice Places a bid on an auction
     * @param listingId The listing ID
     * @param amount The bid amount
     * @param expiresIn The bid expiry duration in seconds
     * @return The bid ID
     */
    function placeBid(uint256 listingId, uint256 amount, uint256 expiresIn) external payable returns (uint256);

    /**
     * @notice Accepts a bid
     * @param bidId The bid ID
     */
    function acceptBid(uint256 bidId) external;

    /**
     * @notice Cancels a bid
     * @param bidId The bid ID
     */
    function cancelBid(uint256 bidId) external;

    /**
     * @notice Cancels a listing
     * @param listingId The listing ID
     */
    function cancelListing(uint256 listingId) external;

    /**
     * @notice Finalizes an ended auction
     * @param listingId The listing ID
     */
    function finalizeAuction(uint256 listingId) external;

    /**
     * @notice Updates listing price
     * @param listingId The listing ID
     * @param newPrice The new price
     */
    function updateListingPrice(uint256 listingId, uint256 newPrice) external;

    /**
     * @notice Gets a listing by ID
     * @param listingId The listing ID
     * @return The listing struct
     */
    function getListing(uint256 listingId) external view returns (Listing memory);

    /**
     * @notice Gets a bid by ID
     * @param bidId The bid ID
     * @return The bid struct
     */
    function getBid(uint256 bidId) external view returns (Bid memory);

    /**
     * @notice Gets all active listings
     * @return Array of listing IDs
     */
    function getActiveListings() external view returns (uint256[] memory);

    /**
     * @notice Gets listings by seller
     * @param seller The seller address
     * @return Array of listing IDs
     */
    function getListingsBySeller(address seller) external view returns (uint256[] memory);

    /**
     * @notice Gets bids for a listing
     * @param listingId The listing ID
     * @return Array of bid IDs
     */
    function getBidsForListing(uint256 listingId) external view returns (uint256[] memory);

    /**
     * @notice Gets the highest bid for a listing
     * @param listingId The listing ID
     * @return The highest bid struct
     */
    function getHighestBid(uint256 listingId) external view returns (Bid memory);

    /**
     * @notice Gets current Dutch auction price
     * @param listingId The listing ID
     * @return The current price
     */
    function getCurrentDutchAuctionPrice(uint256 listingId) external view returns (uint256);

    /**
     * @notice Returns the marketplace fee basis points
     * @return Fee in basis points (e.g., 250 = 2.5%)
     */
    function marketplaceFee() external view returns (uint256);

    /**
     * @notice Sets the marketplace fee
     * @param fee The new fee in basis points
     */
    function setMarketplaceFee(uint256 fee) external;

    /**
     * @notice Returns the fee recipient address
     * @return The fee recipient
     */
    function feeRecipient() external view returns (address);

    /**
     * @notice Sets the fee recipient
     * @param recipient The new fee recipient
     */
    function setFeeRecipient(address recipient) external;

    // Events
    event ListingCreated(
        uint256 indexed listingId,
        address indexed seller,
        address indexed nftContract,
        uint256 tokenId,
        uint256 price,
        ListingType listingType
    );
    event ListingUpdated(uint256 indexed listingId, uint256 oldPrice, uint256 newPrice);
    event ListingCancelled(uint256 indexed listingId);
    event ListingSold(
        uint256 indexed listingId,
        address indexed buyer,
        uint256 price,
        uint256 quantity
    );
    event BidPlaced(uint256 indexed bidId, uint256 indexed listingId, address indexed bidder, uint256 amount);
    event BidAccepted(uint256 indexed bidId, uint256 indexed listingId);
    event BidCancelled(uint256 indexed bidId);
    event BidRejected(uint256 indexed bidId);
    event AuctionFinalized(uint256 indexed listingId, address indexed winner, uint256 finalPrice);
    event MarketplaceFeeUpdated(uint256 oldFee, uint256 newFee);
    event FeeRecipientUpdated(address oldRecipient, address newRecipient);
}
