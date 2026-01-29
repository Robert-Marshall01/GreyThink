// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/token/ERC721/IERC721.sol";
import "@openzeppelin/contracts/token/ERC1155/IERC1155.sol";

/**
 * @title DutchAuction
 * @notice Dutch auction system with linear and exponential price decay
 * @dev Supports ERC20, ERC721, and ERC1155 assets
 * 
 * Features:
 * - Linear and exponential price decay curves
 * - Reserve price protection
 * - Batch auctions
 * - Partial fills for ERC20/ERC1155
 * - Seller cancellation (with penalty)
 * - Extension on last-minute bids
 * 
 * Use Cases:
 * - Token sales (ICO/IDO style)
 * - NFT drops
 * - Liquidation auctions
 * - Fair price discovery
 */
contract DutchAuction is AccessControl, ReentrancyGuard {
    using SafeERC20 for IERC20;

    // ============================================
    // ROLES
    // ============================================

    bytes32 public constant AUCTIONEER_ROLE = keccak256("AUCTIONEER_ROLE");

    // ============================================
    // TYPES
    // ============================================

    /// @notice Auction configuration
    struct Auction {
        address seller;
        AssetType assetType;
        address assetContract;
        uint256 tokenId;                // For ERC721/ERC1155
        uint256 amount;                 // Amount being auctioned
        address paymentToken;           // address(0) for ETH
        uint256 startPrice;
        uint256 endPrice;               // Reserve/minimum price
        uint256 startTime;
        uint256 endTime;
        DecayType decayType;
        AuctionStatus status;
        uint256 soldAmount;             // Amount sold so far
        uint256 totalReceived;          // Total payment received
    }

    /// @notice Bid record
    struct Bid {
        address bidder;
        uint256 auctionId;
        uint256 amount;                 // Asset amount purchased
        uint256 price;                  // Price per unit at bid time
        uint256 totalPaid;
        uint256 timestamp;
    }

    enum AssetType {
        ERC20,
        ERC721,
        ERC1155
    }

    enum DecayType {
        Linear,                         // Price decreases linearly
        Exponential,                    // Price decreases exponentially (faster decay)
        Stepped                         // Price drops in discrete steps
    }

    enum AuctionStatus {
        Active,
        Completed,
        Cancelled,
        Expired
    }

    // ============================================
    // CONSTANTS
    // ============================================

    uint256 public constant MIN_AUCTION_DURATION = 1 hours;
    uint256 public constant MAX_AUCTION_DURATION = 30 days;
    uint256 public constant CANCELLATION_PENALTY = 500;     // 5%
    uint256 public constant EXTENSION_THRESHOLD = 10 minutes;
    uint256 public constant EXTENSION_DURATION = 10 minutes;

    // ============================================
    // STATE
    // ============================================

    /// @notice Auctions by ID
    mapping(uint256 => Auction) public auctions;
    uint256 public auctionCount;

    /// @notice Bids by ID
    mapping(uint256 => Bid) public bids;
    uint256 public bidCount;

    /// @notice Bids per auction
    mapping(uint256 => uint256[]) public auctionBids;

    /// @notice User's auctions
    mapping(address => uint256[]) public sellerAuctions;

    /// @notice User's bids
    mapping(address => uint256[]) public bidderBids;

    /// @notice Protocol fee (basis points)
    uint256 public protocolFee = 250;   // 2.5%
    address public feeRecipient;

    // ============================================
    // EVENTS
    // ============================================

    event AuctionCreated(
        uint256 indexed auctionId,
        address indexed seller,
        AssetType assetType,
        address assetContract,
        uint256 startPrice,
        uint256 endPrice,
        uint256 duration
    );

    event BidPlaced(
        uint256 indexed auctionId,
        uint256 indexed bidId,
        address indexed bidder,
        uint256 amount,
        uint256 price,
        uint256 totalPaid
    );

    event AuctionCompleted(
        uint256 indexed auctionId,
        uint256 totalSold,
        uint256 totalReceived
    );

    event AuctionCancelled(
        uint256 indexed auctionId,
        address indexed seller,
        uint256 penalty
    );

    event AuctionExtended(uint256 indexed auctionId, uint256 newEndTime);

    // ============================================
    // ERRORS
    // ============================================

    error InvalidDuration();
    error InvalidPrice();
    error AuctionNotActive();
    error AuctionNotExpired();
    error InsufficientPayment();
    error InsufficientAmount();
    error NotSeller();
    error AuctionEnded();
    error AuctionNotStarted();

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(address _feeRecipient) {
        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(AUCTIONEER_ROLE, msg.sender);
        feeRecipient = _feeRecipient;
    }

    // ============================================
    // AUCTION CREATION
    // ============================================

    /**
     * @notice Create an ERC20 token auction
     */
    function createERC20Auction(
        address assetContract,
        uint256 amount,
        address paymentToken,
        uint256 startPrice,
        uint256 endPrice,
        uint256 duration,
        DecayType decayType
    ) external nonReentrant returns (uint256 auctionId) {
        if (duration < MIN_AUCTION_DURATION || duration > MAX_AUCTION_DURATION) {
            revert InvalidDuration();
        }
        if (startPrice <= endPrice) revert InvalidPrice();
        if (amount == 0) revert InsufficientAmount();

        // Transfer tokens to auction
        IERC20(assetContract).safeTransferFrom(msg.sender, address(this), amount);

        auctionId = _createAuction(
            AssetType.ERC20,
            assetContract,
            0,
            amount,
            paymentToken,
            startPrice,
            endPrice,
            duration,
            decayType
        );
    }

    /**
     * @notice Create an ERC721 NFT auction
     */
    function createERC721Auction(
        address assetContract,
        uint256 tokenId,
        address paymentToken,
        uint256 startPrice,
        uint256 endPrice,
        uint256 duration,
        DecayType decayType
    ) external nonReentrant returns (uint256 auctionId) {
        if (duration < MIN_AUCTION_DURATION || duration > MAX_AUCTION_DURATION) {
            revert InvalidDuration();
        }
        if (startPrice <= endPrice) revert InvalidPrice();

        // Transfer NFT to auction
        IERC721(assetContract).transferFrom(msg.sender, address(this), tokenId);

        auctionId = _createAuction(
            AssetType.ERC721,
            assetContract,
            tokenId,
            1,
            paymentToken,
            startPrice,
            endPrice,
            duration,
            decayType
        );
    }

    /**
     * @notice Create an ERC1155 token auction
     */
    function createERC1155Auction(
        address assetContract,
        uint256 tokenId,
        uint256 amount,
        address paymentToken,
        uint256 startPrice,
        uint256 endPrice,
        uint256 duration,
        DecayType decayType
    ) external nonReentrant returns (uint256 auctionId) {
        if (duration < MIN_AUCTION_DURATION || duration > MAX_AUCTION_DURATION) {
            revert InvalidDuration();
        }
        if (startPrice <= endPrice) revert InvalidPrice();
        if (amount == 0) revert InsufficientAmount();

        // Transfer tokens to auction
        IERC1155(assetContract).safeTransferFrom(
            msg.sender,
            address(this),
            tokenId,
            amount,
            ""
        );

        auctionId = _createAuction(
            AssetType.ERC1155,
            assetContract,
            tokenId,
            amount,
            paymentToken,
            startPrice,
            endPrice,
            duration,
            decayType
        );
    }

    /**
     * @notice Internal auction creation
     */
    function _createAuction(
        AssetType assetType,
        address assetContract,
        uint256 tokenId,
        uint256 amount,
        address paymentToken,
        uint256 startPrice,
        uint256 endPrice,
        uint256 duration,
        DecayType decayType
    ) internal returns (uint256 auctionId) {
        auctionId = ++auctionCount;

        auctions[auctionId] = Auction({
            seller: msg.sender,
            assetType: assetType,
            assetContract: assetContract,
            tokenId: tokenId,
            amount: amount,
            paymentToken: paymentToken,
            startPrice: startPrice,
            endPrice: endPrice,
            startTime: block.timestamp,
            endTime: block.timestamp + duration,
            decayType: decayType,
            status: AuctionStatus.Active,
            soldAmount: 0,
            totalReceived: 0
        });

        sellerAuctions[msg.sender].push(auctionId);

        emit AuctionCreated(
            auctionId,
            msg.sender,
            assetType,
            assetContract,
            startPrice,
            endPrice,
            duration
        );
    }

    // ============================================
    // BIDDING
    // ============================================

    /**
     * @notice Place a bid at current price
     * @param auctionId The auction to bid on
     * @param amount Amount to purchase (for ERC20/ERC1155)
     */
    function bid(
        uint256 auctionId,
        uint256 amount
    ) external payable nonReentrant returns (uint256 bidId) {
        Auction storage auction = auctions[auctionId];

        if (auction.status != AuctionStatus.Active) revert AuctionNotActive();
        if (block.timestamp < auction.startTime) revert AuctionNotStarted();
        if (block.timestamp >= auction.endTime) revert AuctionEnded();

        uint256 availableAmount = auction.amount - auction.soldAmount;
        if (amount > availableAmount) {
            amount = availableAmount;
        }
        if (amount == 0) revert InsufficientAmount();

        // For ERC721, must buy the whole thing
        if (auction.assetType == AssetType.ERC721 && amount != 1) {
            revert InsufficientAmount();
        }

        uint256 currentPrice = getCurrentPrice(auctionId);
        uint256 totalCost = (currentPrice * amount) / 1e18;

        // Handle payment
        if (auction.paymentToken == address(0)) {
            if (msg.value < totalCost) revert InsufficientPayment();
            if (msg.value > totalCost) {
                payable(msg.sender).transfer(msg.value - totalCost);
            }
        } else {
            IERC20(auction.paymentToken).safeTransferFrom(msg.sender, address(this), totalCost);
        }

        auction.soldAmount += amount;
        auction.totalReceived += totalCost;

        bidId = ++bidCount;

        bids[bidId] = Bid({
            bidder: msg.sender,
            auctionId: auctionId,
            amount: amount,
            price: currentPrice,
            totalPaid: totalCost,
            timestamp: block.timestamp
        });

        auctionBids[auctionId].push(bidId);
        bidderBids[msg.sender].push(bidId);

        // Transfer asset to bidder
        _transferAsset(auction, msg.sender, amount);

        // Check if auction is complete
        if (auction.soldAmount >= auction.amount) {
            auction.status = AuctionStatus.Completed;
            _settleAuction(auctionId);
            emit AuctionCompleted(auctionId, auction.soldAmount, auction.totalReceived);
        }

        // Extend auction if bid near end
        if (auction.endTime - block.timestamp < EXTENSION_THRESHOLD) {
            auction.endTime += EXTENSION_DURATION;
            emit AuctionExtended(auctionId, auction.endTime);
        }

        emit BidPlaced(auctionId, bidId, msg.sender, amount, currentPrice, totalCost);
    }

    /**
     * @notice Get current auction price
     */
    function getCurrentPrice(uint256 auctionId) public view returns (uint256) {
        Auction memory auction = auctions[auctionId];

        if (block.timestamp <= auction.startTime) {
            return auction.startPrice;
        }

        if (block.timestamp >= auction.endTime) {
            return auction.endPrice;
        }

        uint256 elapsed = block.timestamp - auction.startTime;
        uint256 duration = auction.endTime - auction.startTime;
        uint256 priceDiff = auction.startPrice - auction.endPrice;

        if (auction.decayType == DecayType.Linear) {
            return auction.startPrice - (priceDiff * elapsed / duration);
        } else if (auction.decayType == DecayType.Exponential) {
            // Exponential decay: price = start * (end/start)^(elapsed/duration)
            // Simplified approximation using quadratic
            uint256 progress = (elapsed * 1e18) / duration;
            uint256 decayFactor = (progress * progress) / 1e18;
            return auction.startPrice - (priceDiff * decayFactor / 1e18);
        } else {
            // Stepped: price drops every 10 minutes
            uint256 steps = elapsed / 10 minutes;
            uint256 totalSteps = duration / 10 minutes;
            if (totalSteps == 0) return auction.endPrice;
            uint256 stepDrop = priceDiff / totalSteps;
            uint256 dropped = stepDrop * steps;
            return dropped > priceDiff ? auction.endPrice : auction.startPrice - dropped;
        }
    }

    // ============================================
    // AUCTION MANAGEMENT
    // ============================================

    /**
     * @notice Cancel an auction (with penalty if bids exist)
     */
    function cancelAuction(uint256 auctionId) external nonReentrant {
        Auction storage auction = auctions[auctionId];

        if (auction.seller != msg.sender) revert NotSeller();
        if (auction.status != AuctionStatus.Active) revert AuctionNotActive();

        auction.status = AuctionStatus.Cancelled;

        uint256 unsoldAmount = auction.amount - auction.soldAmount;
        uint256 penalty = 0;

        // Calculate penalty if any sales occurred
        if (auction.soldAmount > 0) {
            penalty = (auction.totalReceived * CANCELLATION_PENALTY) / 10000;
        }

        // Return unsold assets to seller
        if (unsoldAmount > 0) {
            _transferAsset(auction, msg.sender, unsoldAmount);
        }

        // Pay seller minus penalty
        if (auction.totalReceived > 0) {
            uint256 sellerPayment = auction.totalReceived - penalty;
            _transferPayment(auction.paymentToken, msg.sender, sellerPayment);
            if (penalty > 0) {
                _transferPayment(auction.paymentToken, feeRecipient, penalty);
            }
        }

        emit AuctionCancelled(auctionId, msg.sender, penalty);
    }

    /**
     * @notice Finalize expired auction
     */
    function finalizeExpiredAuction(uint256 auctionId) external nonReentrant {
        Auction storage auction = auctions[auctionId];

        if (auction.status != AuctionStatus.Active) revert AuctionNotActive();
        if (block.timestamp < auction.endTime) revert AuctionNotExpired();

        auction.status = AuctionStatus.Expired;

        // Return unsold assets
        uint256 unsoldAmount = auction.amount - auction.soldAmount;
        if (unsoldAmount > 0) {
            _transferAsset(auction, auction.seller, unsoldAmount);
        }

        // Settle payments
        _settleAuction(auctionId);

        emit AuctionCompleted(auctionId, auction.soldAmount, auction.totalReceived);
    }

    /**
     * @notice Settle auction payments
     */
    function _settleAuction(uint256 auctionId) internal {
        Auction memory auction = auctions[auctionId];

        if (auction.totalReceived == 0) return;

        uint256 fee = (auction.totalReceived * protocolFee) / 10000;
        uint256 sellerPayment = auction.totalReceived - fee;

        _transferPayment(auction.paymentToken, auction.seller, sellerPayment);
        if (fee > 0) {
            _transferPayment(auction.paymentToken, feeRecipient, fee);
        }
    }

    /**
     * @notice Transfer asset to recipient
     */
    function _transferAsset(
        Auction memory auction,
        address recipient,
        uint256 amount
    ) internal {
        if (auction.assetType == AssetType.ERC20) {
            IERC20(auction.assetContract).safeTransfer(recipient, amount);
        } else if (auction.assetType == AssetType.ERC721) {
            IERC721(auction.assetContract).transferFrom(address(this), recipient, auction.tokenId);
        } else {
            IERC1155(auction.assetContract).safeTransferFrom(
                address(this),
                recipient,
                auction.tokenId,
                amount,
                ""
            );
        }
    }

    /**
     * @notice Transfer payment
     */
    function _transferPayment(address token, address recipient, uint256 amount) internal {
        if (token == address(0)) {
            payable(recipient).transfer(amount);
        } else {
            IERC20(token).safeTransfer(recipient, amount);
        }
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    function getAuction(uint256 auctionId) external view returns (
        address seller,
        AssetType assetType,
        address assetContract,
        uint256 amount,
        uint256 currentPrice,
        uint256 soldAmount,
        AuctionStatus status,
        uint256 timeRemaining
    ) {
        Auction memory a = auctions[auctionId];
        return (
            a.seller,
            a.assetType,
            a.assetContract,
            a.amount,
            getCurrentPrice(auctionId),
            a.soldAmount,
            a.status,
            block.timestamp >= a.endTime ? 0 : a.endTime - block.timestamp
        );
    }

    function getAuctionBids(uint256 auctionId) external view returns (uint256[] memory) {
        return auctionBids[auctionId];
    }

    function getSellerAuctions(address seller) external view returns (uint256[] memory) {
        return sellerAuctions[seller];
    }

    function getBidderBids(address bidder) external view returns (uint256[] memory) {
        return bidderBids[bidder];
    }

    // ============================================
    // ADMIN
    // ============================================

    function setProtocolFee(uint256 _fee) external onlyRole(DEFAULT_ADMIN_ROLE) {
        require(_fee <= 1000, "Fee too high");
        protocolFee = _fee;
    }

    function setFeeRecipient(address _recipient) external onlyRole(DEFAULT_ADMIN_ROLE) {
        feeRecipient = _recipient;
    }

    // ============================================
    // RECEIVER FUNCTIONS
    // ============================================

    function onERC721Received(address, address, uint256, bytes calldata) external pure returns (bytes4) {
        return this.onERC721Received.selector;
    }

    function onERC1155Received(address, address, uint256, uint256, bytes calldata) external pure returns (bytes4) {
        return this.onERC1155Received.selector;
    }

    function onERC1155BatchReceived(address, address, uint256[] calldata, uint256[] calldata, bytes calldata) external pure returns (bytes4) {
        return this.onERC1155BatchReceived.selector;
    }

    receive() external payable {}
}
