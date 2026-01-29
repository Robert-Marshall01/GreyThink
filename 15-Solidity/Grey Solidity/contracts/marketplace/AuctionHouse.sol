// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/token/ERC721/IERC721.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";

/**
 * @title AuctionHouse
 * @notice Advanced auction system supporting multiple auction types
 * @dev Implements English, Dutch, and sealed-bid auctions
 * 
 * Features:
 * - English auctions (ascending price)
 * - Dutch auctions (descending price)
 * - Sealed-bid auctions with commit-reveal
 * - Reserve prices and minimum bid increments
 * - Auction extensions on late bids
 * - Multi-asset support (NFTs, ERC20 bundles)
 * - Fee distribution and royalty handling
 * - Escrow integration for secure settlement
 */
contract AuctionHouse is AccessControl, ReentrancyGuard {
    using SafeERC20 for IERC20;

    // ============================================
    // ROLES
    // ============================================

    bytes32 public constant OPERATOR_ROLE = keccak256("OPERATOR_ROLE");
    bytes32 public constant FEE_MANAGER_ROLE = keccak256("FEE_MANAGER_ROLE");

    // ============================================
    // TYPES
    // ============================================

    enum AuctionType {
        English,        // Ascending price auction
        Dutch,          // Descending price auction
        SealedBid       // First-price sealed-bid auction
    }

    enum AuctionStatus {
        Created,
        Active,
        Ended,
        Settled,
        Cancelled
    }

    struct Auction {
        uint256 auctionId;
        address seller;
        AuctionType auctionType;
        AuctionStatus status;
        
        // Asset info
        address nftContract;
        uint256 tokenId;
        
        // Payment
        address paymentToken;       // address(0) for ETH
        
        // Pricing
        uint256 startPrice;
        uint256 reservePrice;
        uint256 buyNowPrice;        // Instant purchase price (0 = disabled)
        uint256 minBidIncrement;
        
        // Dutch auction specific
        uint256 endPrice;           // Final price for Dutch auction
        uint256 priceDecrement;     // Per-second price decrease
        
        // Timing
        uint256 startTime;
        uint256 endTime;
        uint256 extensionThreshold; // Seconds before end to trigger extension
        uint256 extensionDuration;  // Extension duration in seconds
        
        // Current state
        address highestBidder;
        uint256 highestBid;
        uint256 bidCount;
        
        // Fees
        uint256 platformFee;        // Basis points
        address royaltyRecipient;
        uint256 royaltyFee;         // Basis points
    }

    struct Bid {
        address bidder;
        uint256 amount;
        uint256 timestamp;
        bool withdrawn;
    }

    struct SealedBid {
        bytes32 commitment;
        uint256 revealedAmount;
        bool revealed;
        uint256 commitTime;
    }

    // ============================================
    // CONSTANTS
    // ============================================

    uint256 public constant MAX_PLATFORM_FEE = 1000; // 10%
    uint256 public constant MAX_ROYALTY_FEE = 2500;  // 25%
    uint256 public constant MIN_AUCTION_DURATION = 1 hours;
    uint256 public constant MAX_AUCTION_DURATION = 30 days;
    uint256 public constant SEALED_BID_REVEAL_PERIOD = 1 hours;

    // ============================================
    // STATE
    // ============================================

    /// @notice Auctions by ID
    mapping(uint256 => Auction) public auctions;
    uint256 public auctionCounter;

    /// @notice Bids for each auction: auctionId => bidder => Bid
    mapping(uint256 => mapping(address => Bid)) public bids;

    /// @notice Bid history for English auctions
    mapping(uint256 => Bid[]) public bidHistory;

    /// @notice Sealed bids: auctionId => bidder => SealedBid
    mapping(uint256 => mapping(address => SealedBid)) public sealedBids;

    /// @notice Sealed bid participants
    mapping(uint256 => address[]) public sealedBidParticipants;

    /// @notice Platform fee recipient
    address public feeRecipient;

    /// @notice Default platform fee (basis points)
    uint256 public defaultPlatformFee = 250; // 2.5%

    /// @notice Paused state
    bool public paused;

    /// @notice Active auctions by seller
    mapping(address => uint256[]) public sellerAuctions;

    /// @notice Pending withdrawals
    mapping(address => mapping(address => uint256)) public pendingWithdrawals;

    // ============================================
    // EVENTS
    // ============================================

    event AuctionCreated(
        uint256 indexed auctionId,
        address indexed seller,
        address nftContract,
        uint256 tokenId,
        AuctionType auctionType,
        uint256 startPrice,
        uint256 endTime
    );

    event AuctionStarted(uint256 indexed auctionId, uint256 startTime);

    event BidPlaced(
        uint256 indexed auctionId,
        address indexed bidder,
        uint256 amount,
        uint256 timestamp
    );

    event BidWithdrawn(
        uint256 indexed auctionId,
        address indexed bidder,
        uint256 amount
    );

    event SealedBidCommitted(
        uint256 indexed auctionId,
        address indexed bidder,
        bytes32 commitment
    );

    event SealedBidRevealed(
        uint256 indexed auctionId,
        address indexed bidder,
        uint256 amount
    );

    event AuctionExtended(
        uint256 indexed auctionId,
        uint256 newEndTime
    );

    event AuctionEnded(
        uint256 indexed auctionId,
        address indexed winner,
        uint256 winningBid
    );

    event AuctionSettled(
        uint256 indexed auctionId,
        address indexed winner,
        uint256 finalPrice,
        uint256 platformFee,
        uint256 royaltyFee
    );

    event AuctionCancelled(uint256 indexed auctionId, address indexed seller);

    event BuyNowExecuted(
        uint256 indexed auctionId,
        address indexed buyer,
        uint256 price
    );

    // ============================================
    // ERRORS
    // ============================================

    error Paused();
    error AuctionNotFound();
    error AuctionNotActive();
    error AuctionStillActive();
    error AuctionAlreadySettled();
    error InvalidAuctionType();
    error InvalidDuration();
    error InvalidPrice();
    error BidTooLow();
    error CannotBidOnOwnAuction();
    error NoBidsToWithdraw();
    error InvalidCommitment();
    error AlreadyRevealed();
    error RevealPeriodNotStarted();
    error RevealPeriodEnded();
    error TransferFailed();
    error Unauthorized();
    error NoBuyNowPrice();
    error HasBids();

    // ============================================
    // MODIFIERS
    // ============================================

    modifier whenNotPaused() {
        if (paused) revert Paused();
        _;
    }

    modifier auctionExists(uint256 auctionId) {
        if (auctions[auctionId].seller == address(0)) revert AuctionNotFound();
        _;
    }

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(address _feeRecipient) {
        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(OPERATOR_ROLE, msg.sender);
        _grantRole(FEE_MANAGER_ROLE, msg.sender);
        
        feeRecipient = _feeRecipient;
    }

    // ============================================
    // AUCTION CREATION
    // ============================================

    /**
     * @notice Create an English (ascending) auction
     */
    function createEnglishAuction(
        address nftContract,
        uint256 tokenId,
        address paymentToken,
        uint256 startPrice,
        uint256 reservePrice,
        uint256 buyNowPrice,
        uint256 minBidIncrement,
        uint256 duration,
        uint256 extensionThreshold,
        uint256 extensionDuration,
        address royaltyRecipient,
        uint256 royaltyFee
    ) external whenNotPaused nonReentrant returns (uint256 auctionId) {
        if (duration < MIN_AUCTION_DURATION || duration > MAX_AUCTION_DURATION) {
            revert InvalidDuration();
        }
        if (reservePrice > 0 && startPrice > reservePrice) revert InvalidPrice();
        if (royaltyFee > MAX_ROYALTY_FEE) revert InvalidPrice();

        // Transfer NFT to auction house
        IERC721(nftContract).transferFrom(msg.sender, address(this), tokenId);

        auctionId = ++auctionCounter;

        auctions[auctionId] = Auction({
            auctionId: auctionId,
            seller: msg.sender,
            auctionType: AuctionType.English,
            status: AuctionStatus.Active,
            nftContract: nftContract,
            tokenId: tokenId,
            paymentToken: paymentToken,
            startPrice: startPrice,
            reservePrice: reservePrice,
            buyNowPrice: buyNowPrice,
            minBidIncrement: minBidIncrement,
            endPrice: 0,
            priceDecrement: 0,
            startTime: block.timestamp,
            endTime: block.timestamp + duration,
            extensionThreshold: extensionThreshold,
            extensionDuration: extensionDuration,
            highestBidder: address(0),
            highestBid: 0,
            bidCount: 0,
            platformFee: defaultPlatformFee,
            royaltyRecipient: royaltyRecipient,
            royaltyFee: royaltyFee
        });

        sellerAuctions[msg.sender].push(auctionId);

        emit AuctionCreated(
            auctionId,
            msg.sender,
            nftContract,
            tokenId,
            AuctionType.English,
            startPrice,
            block.timestamp + duration
        );
    }

    /**
     * @notice Create a Dutch (descending) auction
     */
    function createDutchAuction(
        address nftContract,
        uint256 tokenId,
        address paymentToken,
        uint256 startPrice,
        uint256 endPrice,
        uint256 duration,
        address royaltyRecipient,
        uint256 royaltyFee
    ) external whenNotPaused nonReentrant returns (uint256 auctionId) {
        if (duration < MIN_AUCTION_DURATION || duration > MAX_AUCTION_DURATION) {
            revert InvalidDuration();
        }
        if (startPrice <= endPrice) revert InvalidPrice();

        IERC721(nftContract).transferFrom(msg.sender, address(this), tokenId);

        auctionId = ++auctionCounter;
        uint256 priceDecrement = (startPrice - endPrice) / duration;

        auctions[auctionId] = Auction({
            auctionId: auctionId,
            seller: msg.sender,
            auctionType: AuctionType.Dutch,
            status: AuctionStatus.Active,
            nftContract: nftContract,
            tokenId: tokenId,
            paymentToken: paymentToken,
            startPrice: startPrice,
            reservePrice: endPrice,
            buyNowPrice: 0,
            minBidIncrement: 0,
            endPrice: endPrice,
            priceDecrement: priceDecrement,
            startTime: block.timestamp,
            endTime: block.timestamp + duration,
            extensionThreshold: 0,
            extensionDuration: 0,
            highestBidder: address(0),
            highestBid: 0,
            bidCount: 0,
            platformFee: defaultPlatformFee,
            royaltyRecipient: royaltyRecipient,
            royaltyFee: royaltyFee
        });

        sellerAuctions[msg.sender].push(auctionId);

        emit AuctionCreated(
            auctionId,
            msg.sender,
            nftContract,
            tokenId,
            AuctionType.Dutch,
            startPrice,
            block.timestamp + duration
        );
    }

    /**
     * @notice Create a sealed-bid auction
     */
    function createSealedBidAuction(
        address nftContract,
        uint256 tokenId,
        address paymentToken,
        uint256 reservePrice,
        uint256 duration,
        address royaltyRecipient,
        uint256 royaltyFee
    ) external whenNotPaused nonReentrant returns (uint256 auctionId) {
        if (duration < MIN_AUCTION_DURATION || duration > MAX_AUCTION_DURATION) {
            revert InvalidDuration();
        }

        IERC721(nftContract).transferFrom(msg.sender, address(this), tokenId);

        auctionId = ++auctionCounter;

        auctions[auctionId] = Auction({
            auctionId: auctionId,
            seller: msg.sender,
            auctionType: AuctionType.SealedBid,
            status: AuctionStatus.Active,
            nftContract: nftContract,
            tokenId: tokenId,
            paymentToken: paymentToken,
            startPrice: 0,
            reservePrice: reservePrice,
            buyNowPrice: 0,
            minBidIncrement: 0,
            endPrice: 0,
            priceDecrement: 0,
            startTime: block.timestamp,
            endTime: block.timestamp + duration,
            extensionThreshold: 0,
            extensionDuration: 0,
            highestBidder: address(0),
            highestBid: 0,
            bidCount: 0,
            platformFee: defaultPlatformFee,
            royaltyRecipient: royaltyRecipient,
            royaltyFee: royaltyFee
        });

        sellerAuctions[msg.sender].push(auctionId);

        emit AuctionCreated(
            auctionId,
            msg.sender,
            nftContract,
            tokenId,
            AuctionType.SealedBid,
            0,
            block.timestamp + duration
        );
    }

    // ============================================
    // BIDDING FUNCTIONS
    // ============================================

    /**
     * @notice Place bid on English auction
     */
    function placeBid(uint256 auctionId) external payable whenNotPaused nonReentrant auctionExists(auctionId) {
        Auction storage auction = auctions[auctionId];

        if (auction.status != AuctionStatus.Active) revert AuctionNotActive();
        if (block.timestamp >= auction.endTime) revert AuctionNotActive();
        if (auction.auctionType != AuctionType.English) revert InvalidAuctionType();
        if (msg.sender == auction.seller) revert CannotBidOnOwnAuction();
        if (auction.paymentToken != address(0)) revert InvalidAuctionType();

        uint256 minBid = auction.highestBid == 0 
            ? auction.startPrice 
            : auction.highestBid + auction.minBidIncrement;

        if (msg.value < minBid) revert BidTooLow();

        // Refund previous highest bidder
        if (auction.highestBidder != address(0)) {
            pendingWithdrawals[auction.highestBidder][address(0)] += auction.highestBid;
        }

        // Update auction
        auction.highestBidder = msg.sender;
        auction.highestBid = msg.value;
        auction.bidCount++;

        // Record bid
        bids[auctionId][msg.sender] = Bid({
            bidder: msg.sender,
            amount: msg.value,
            timestamp: block.timestamp,
            withdrawn: false
        });

        bidHistory[auctionId].push(Bid({
            bidder: msg.sender,
            amount: msg.value,
            timestamp: block.timestamp,
            withdrawn: false
        }));

        // Check for extension
        if (auction.extensionThreshold > 0 && 
            auction.endTime - block.timestamp <= auction.extensionThreshold) {
            auction.endTime += auction.extensionDuration;
            emit AuctionExtended(auctionId, auction.endTime);
        }

        emit BidPlaced(auctionId, msg.sender, msg.value, block.timestamp);
    }

    /**
     * @notice Place bid with ERC20 token
     */
    function placeBidWithToken(
        uint256 auctionId,
        uint256 amount
    ) external whenNotPaused nonReentrant auctionExists(auctionId) {
        Auction storage auction = auctions[auctionId];

        if (auction.status != AuctionStatus.Active) revert AuctionNotActive();
        if (block.timestamp >= auction.endTime) revert AuctionNotActive();
        if (auction.auctionType != AuctionType.English) revert InvalidAuctionType();
        if (msg.sender == auction.seller) revert CannotBidOnOwnAuction();
        if (auction.paymentToken == address(0)) revert InvalidAuctionType();

        uint256 minBid = auction.highestBid == 0 
            ? auction.startPrice 
            : auction.highestBid + auction.minBidIncrement;

        if (amount < minBid) revert BidTooLow();

        // Transfer tokens
        IERC20(auction.paymentToken).safeTransferFrom(msg.sender, address(this), amount);

        // Refund previous highest bidder
        if (auction.highestBidder != address(0)) {
            pendingWithdrawals[auction.highestBidder][auction.paymentToken] += auction.highestBid;
        }

        auction.highestBidder = msg.sender;
        auction.highestBid = amount;
        auction.bidCount++;

        bids[auctionId][msg.sender] = Bid({
            bidder: msg.sender,
            amount: amount,
            timestamp: block.timestamp,
            withdrawn: false
        });

        bidHistory[auctionId].push(Bid({
            bidder: msg.sender,
            amount: amount,
            timestamp: block.timestamp,
            withdrawn: false
        }));

        if (auction.extensionThreshold > 0 && 
            auction.endTime - block.timestamp <= auction.extensionThreshold) {
            auction.endTime += auction.extensionDuration;
            emit AuctionExtended(auctionId, auction.endTime);
        }

        emit BidPlaced(auctionId, msg.sender, amount, block.timestamp);
    }

    /**
     * @notice Buy instantly at Dutch auction current price
     */
    function buyDutchAuction(uint256 auctionId) external payable whenNotPaused nonReentrant auctionExists(auctionId) {
        Auction storage auction = auctions[auctionId];

        if (auction.status != AuctionStatus.Active) revert AuctionNotActive();
        if (auction.auctionType != AuctionType.Dutch) revert InvalidAuctionType();
        if (block.timestamp >= auction.endTime) revert AuctionNotActive();
        if (auction.paymentToken != address(0)) revert InvalidAuctionType();

        uint256 currentPrice = getDutchAuctionPrice(auctionId);
        if (msg.value < currentPrice) revert BidTooLow();

        auction.highestBidder = msg.sender;
        auction.highestBid = currentPrice;
        auction.status = AuctionStatus.Ended;

        // Refund excess
        if (msg.value > currentPrice) {
            (bool success, ) = msg.sender.call{value: msg.value - currentPrice}("");
            if (!success) revert TransferFailed();
        }

        emit AuctionEnded(auctionId, msg.sender, currentPrice);

        // Auto-settle
        _settle(auctionId);
    }

    /**
     * @notice Commit sealed bid
     */
    function commitSealedBid(
        uint256 auctionId,
        bytes32 commitment
    ) external payable whenNotPaused nonReentrant auctionExists(auctionId) {
        Auction storage auction = auctions[auctionId];

        if (auction.status != AuctionStatus.Active) revert AuctionNotActive();
        if (auction.auctionType != AuctionType.SealedBid) revert InvalidAuctionType();
        if (block.timestamp >= auction.endTime) revert AuctionNotActive();
        if (msg.sender == auction.seller) revert CannotBidOnOwnAuction();

        // Deposit must cover potential bid
        sealedBids[auctionId][msg.sender] = SealedBid({
            commitment: commitment,
            revealedAmount: 0,
            revealed: false,
            commitTime: block.timestamp
        });

        sealedBidParticipants[auctionId].push(msg.sender);
        auction.bidCount++;

        emit SealedBidCommitted(auctionId, msg.sender, commitment);
    }

    /**
     * @notice Reveal sealed bid
     */
    function revealSealedBid(
        uint256 auctionId,
        uint256 amount,
        bytes32 salt
    ) external payable whenNotPaused nonReentrant auctionExists(auctionId) {
        Auction storage auction = auctions[auctionId];

        if (auction.auctionType != AuctionType.SealedBid) revert InvalidAuctionType();
        if (block.timestamp < auction.endTime) revert RevealPeriodNotStarted();
        if (block.timestamp > auction.endTime + SEALED_BID_REVEAL_PERIOD) {
            revert RevealPeriodEnded();
        }

        SealedBid storage bid = sealedBids[auctionId][msg.sender];
        if (bid.revealed) revert AlreadyRevealed();

        // Verify commitment
        bytes32 expectedCommitment = keccak256(abi.encodePacked(msg.sender, amount, salt));
        if (bid.commitment != expectedCommitment) revert InvalidCommitment();

        // Verify payment
        if (auction.paymentToken == address(0)) {
            if (msg.value < amount) revert BidTooLow();
        } else {
            IERC20(auction.paymentToken).safeTransferFrom(msg.sender, address(this), amount);
        }

        bid.revealed = true;
        bid.revealedAmount = amount;

        // Update highest bid if applicable
        if (amount > auction.highestBid && amount >= auction.reservePrice) {
            // Refund previous highest
            if (auction.highestBidder != address(0)) {
                pendingWithdrawals[auction.highestBidder][auction.paymentToken] += auction.highestBid;
            }
            auction.highestBidder = msg.sender;
            auction.highestBid = amount;
        } else {
            // Refund this bid
            pendingWithdrawals[msg.sender][auction.paymentToken] += amount;
        }

        emit SealedBidRevealed(auctionId, msg.sender, amount);
    }

    /**
     * @notice Buy now at fixed price (English auctions only)
     */
    function buyNow(uint256 auctionId) external payable whenNotPaused nonReentrant auctionExists(auctionId) {
        Auction storage auction = auctions[auctionId];

        if (auction.status != AuctionStatus.Active) revert AuctionNotActive();
        if (auction.buyNowPrice == 0) revert NoBuyNowPrice();
        if (auction.paymentToken != address(0)) revert InvalidAuctionType();
        if (msg.value < auction.buyNowPrice) revert BidTooLow();

        // Refund previous highest bidder
        if (auction.highestBidder != address(0)) {
            pendingWithdrawals[auction.highestBidder][address(0)] += auction.highestBid;
        }

        auction.highestBidder = msg.sender;
        auction.highestBid = auction.buyNowPrice;
        auction.status = AuctionStatus.Ended;

        // Refund excess
        if (msg.value > auction.buyNowPrice) {
            (bool success, ) = msg.sender.call{value: msg.value - auction.buyNowPrice}("");
            if (!success) revert TransferFailed();
        }

        emit BuyNowExecuted(auctionId, msg.sender, auction.buyNowPrice);

        _settle(auctionId);
    }

    // ============================================
    // AUCTION SETTLEMENT
    // ============================================

    /**
     * @notice End and settle an auction
     */
    function settleAuction(uint256 auctionId) external nonReentrant auctionExists(auctionId) {
        Auction storage auction = auctions[auctionId];

        if (auction.status == AuctionStatus.Settled) revert AuctionAlreadySettled();
        if (auction.status == AuctionStatus.Cancelled) revert AuctionNotFound();

        // For sealed bid, wait for reveal period
        if (auction.auctionType == AuctionType.SealedBid) {
            if (block.timestamp < auction.endTime + SEALED_BID_REVEAL_PERIOD) {
                revert AuctionStillActive();
            }
        } else if (block.timestamp < auction.endTime && auction.status != AuctionStatus.Ended) {
            revert AuctionStillActive();
        }

        auction.status = AuctionStatus.Ended;

        _settle(auctionId);
    }

    /**
     * @notice Internal settlement logic
     */
    function _settle(uint256 auctionId) internal {
        Auction storage auction = auctions[auctionId];

        bool hasValidBid = auction.highestBidder != address(0) && 
                           auction.highestBid >= auction.reservePrice;

        if (hasValidBid) {
            // Calculate fees
            uint256 platformFeeAmount = (auction.highestBid * auction.platformFee) / 10000;
            uint256 royaltyAmount = (auction.highestBid * auction.royaltyFee) / 10000;
            uint256 sellerAmount = auction.highestBid - platformFeeAmount - royaltyAmount;

            // Transfer NFT to winner
            IERC721(auction.nftContract).transferFrom(
                address(this),
                auction.highestBidder,
                auction.tokenId
            );

            // Distribute funds
            if (auction.paymentToken == address(0)) {
                (bool success1, ) = feeRecipient.call{value: platformFeeAmount}("");
                if (!success1) revert TransferFailed();

                if (royaltyAmount > 0 && auction.royaltyRecipient != address(0)) {
                    (bool success2, ) = auction.royaltyRecipient.call{value: royaltyAmount}("");
                    if (!success2) revert TransferFailed();
                }

                (bool success3, ) = auction.seller.call{value: sellerAmount}("");
                if (!success3) revert TransferFailed();
            } else {
                IERC20 token = IERC20(auction.paymentToken);
                token.safeTransfer(feeRecipient, platformFeeAmount);
                
                if (royaltyAmount > 0 && auction.royaltyRecipient != address(0)) {
                    token.safeTransfer(auction.royaltyRecipient, royaltyAmount);
                }
                
                token.safeTransfer(auction.seller, sellerAmount);
            }

            emit AuctionSettled(
                auctionId,
                auction.highestBidder,
                auction.highestBid,
                platformFeeAmount,
                royaltyAmount
            );
        } else {
            // Return NFT to seller
            IERC721(auction.nftContract).transferFrom(
                address(this),
                auction.seller,
                auction.tokenId
            );

            emit AuctionEnded(auctionId, address(0), 0);
        }

        auction.status = AuctionStatus.Settled;
    }

    /**
     * @notice Cancel auction (seller only, no bids)
     */
    function cancelAuction(uint256 auctionId) external nonReentrant auctionExists(auctionId) {
        Auction storage auction = auctions[auctionId];

        if (auction.seller != msg.sender) revert Unauthorized();
        if (auction.status != AuctionStatus.Active && auction.status != AuctionStatus.Created) {
            revert AuctionNotActive();
        }
        if (auction.bidCount > 0) revert HasBids();

        auction.status = AuctionStatus.Cancelled;

        // Return NFT
        IERC721(auction.nftContract).transferFrom(
            address(this),
            auction.seller,
            auction.tokenId
        );

        emit AuctionCancelled(auctionId, msg.sender);
    }

    // ============================================
    // WITHDRAWAL
    // ============================================

    /**
     * @notice Withdraw pending funds
     */
    function withdraw(address token) external nonReentrant {
        uint256 amount = pendingWithdrawals[msg.sender][token];
        if (amount == 0) revert NoBidsToWithdraw();

        pendingWithdrawals[msg.sender][token] = 0;

        if (token == address(0)) {
            (bool success, ) = msg.sender.call{value: amount}("");
            if (!success) revert TransferFailed();
        } else {
            IERC20(token).safeTransfer(msg.sender, amount);
        }

        emit BidWithdrawn(0, msg.sender, amount);
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    function getDutchAuctionPrice(uint256 auctionId) public view returns (uint256) {
        Auction memory auction = auctions[auctionId];
        
        if (auction.auctionType != AuctionType.Dutch) return 0;
        if (block.timestamp >= auction.endTime) return auction.endPrice;
        
        uint256 elapsed = block.timestamp - auction.startTime;
        uint256 priceDrop = auction.priceDecrement * elapsed;
        
        if (priceDrop >= auction.startPrice - auction.endPrice) {
            return auction.endPrice;
        }
        
        return auction.startPrice - priceDrop;
    }

    function getAuction(uint256 auctionId) external view returns (Auction memory) {
        return auctions[auctionId];
    }

    function getBidHistory(uint256 auctionId) external view returns (Bid[] memory) {
        return bidHistory[auctionId];
    }

    function getSellerAuctions(address seller) external view returns (uint256[] memory) {
        return sellerAuctions[seller];
    }

    // ============================================
    // ADMIN FUNCTIONS
    // ============================================

    function setFeeRecipient(address _feeRecipient) external onlyRole(FEE_MANAGER_ROLE) {
        feeRecipient = _feeRecipient;
    }

    function setDefaultPlatformFee(uint256 _fee) external onlyRole(FEE_MANAGER_ROLE) {
        require(_fee <= MAX_PLATFORM_FEE, "Fee too high");
        defaultPlatformFee = _fee;
    }

    function pause() external onlyRole(OPERATOR_ROLE) {
        paused = true;
    }

    function unpause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        paused = false;
    }

    receive() external payable {}
}
