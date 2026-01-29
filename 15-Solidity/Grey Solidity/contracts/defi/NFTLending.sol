// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/token/ERC721/IERC721.sol";
import "@openzeppelin/contracts/token/ERC1155/IERC1155.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/Pausable.sol";
import "@openzeppelin/contracts/utils/math/Math.sol";

/**
 * @title NFTLending
 * @author Grey Protocol Team
 * @notice NFT-backed lending protocol with floor price oracles
 * @dev Implements NFT collateralized loans with dynamic LTV
 * 
 * Features:
 * - NFT as collateral for loans
 * - Floor price oracle integration
 * - Dynamic LTV based on collection metrics
 * - Loan refinancing
 * - Auction-based liquidations
 * - Interest accrual
 * - Multiple loan terms
 * - Collection whitelisting
 */
contract NFTLending is AccessControl, ReentrancyGuard, Pausable {
    using SafeERC20 for IERC20;
    using Math for uint256;

    // ============================================
    // CONSTANTS & ROLES
    // ============================================

    bytes32 public constant LENDING_ADMIN_ROLE = keccak256("LENDING_ADMIN_ROLE");
    bytes32 public constant ORACLE_ROLE = keccak256("ORACLE_ROLE");
    bytes32 public constant LIQUIDATOR_ROLE = keccak256("LIQUIDATOR_ROLE");

    uint256 public constant PRECISION = 1e18;
    uint256 public constant BPS = 10000;
    uint256 public constant SECONDS_PER_YEAR = 31536000;
    uint256 public constant MIN_LOAN_DURATION = 1 days;
    uint256 public constant MAX_LOAN_DURATION = 365 days;
    uint256 public constant AUCTION_DURATION = 24 hours;
    uint256 public constant GRACE_PERIOD = 24 hours;

    // ============================================
    // ENUMS
    // ============================================

    enum LoanStatus {
        None,
        Active,
        Repaid,
        Defaulted,
        Liquidating,
        Liquidated
    }

    enum NFTType {
        ERC721,
        ERC1155
    }

    enum AuctionStatus {
        None,
        Active,
        Ended,
        Claimed
    }

    // ============================================
    // STRUCTS
    // ============================================

    /**
     * @notice Collection configuration
     */
    struct CollectionConfig {
        address collection;
        NFTType nftType;
        address oracle;
        uint256 maxLTV;                 // Max LTV in BPS
        uint256 liquidationThreshold;   // Liquidation threshold in BPS
        uint256 liquidationPenalty;     // Penalty in BPS
        uint256 interestRate;           // Annual interest rate in BPS
        uint256 minLoanAmount;
        uint256 maxLoanAmount;
        uint256 minDuration;
        uint256 maxDuration;
        bool isActive;
        bool isPaused;
        uint256 totalLoans;
        uint256 activeLoans;
        uint256 totalBorrowed;
        uint256 totalRepaid;
        uint256 totalDefaulted;
    }

    /**
     * @notice Floor price data
     */
    struct FloorPrice {
        uint256 price;
        uint256 timestamp;
        uint256 volatility;        // 30-day volatility in BPS
        uint256 volume24h;
        uint256 salesCount24h;
    }

    /**
     * @notice Loan data
     */
    struct Loan {
        uint256 loanId;
        address borrower;
        address collection;
        uint256 tokenId;
        uint256 amount1155;        // For ERC1155
        uint256 principal;
        uint256 interestRate;
        uint256 startTime;
        uint256 duration;
        uint256 dueDate;
        uint256 lastInterestAccrual;
        uint256 accruedInterest;
        uint256 floorPriceAtLoan;
        LoanStatus status;
    }

    /**
     * @notice Loan offer (for P2P lending)
     */
    struct LoanOffer {
        uint256 offerId;
        address lender;
        address collection;
        uint256 principal;
        uint256 interestRate;
        uint256 duration;
        uint256 expiry;
        bool isActive;
        uint256 maxLTV;
    }

    /**
     * @notice Liquidation auction
     */
    struct Auction {
        uint256 auctionId;
        uint256 loanId;
        address collection;
        uint256 tokenId;
        uint256 startPrice;
        uint256 currentBid;
        address currentBidder;
        uint256 startTime;
        uint256 endTime;
        AuctionStatus status;
        uint256 debtToCover;
        uint256 minBidIncrement;
    }

    /**
     * @notice Interest rate tier
     */
    struct InterestTier {
        uint256 utilizationThreshold;
        uint256 rate;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Lending asset (e.g., WETH)
    IERC20 public lendingAsset;

    /// @notice Collection configurations
    mapping(address => CollectionConfig) public collections;

    /// @notice Collection floor prices
    mapping(address => FloorPrice) public floorPrices;

    /// @notice Active collections list
    address[] public activeCollections;

    /// @notice Loans
    mapping(uint256 => Loan) public loans;

    /// @notice Loan counter
    uint256 public loanCounter;

    /// @notice User loans
    mapping(address => uint256[]) public userLoans;

    /// @notice NFT to loan mapping
    mapping(address => mapping(uint256 => uint256)) public nftToLoan;

    /// @notice Loan offers
    mapping(uint256 => LoanOffer) public loanOffers;

    /// @notice Offer counter
    uint256 public offerCounter;

    /// @notice User offers
    mapping(address => uint256[]) public userOffers;

    /// @notice Auctions
    mapping(uint256 => Auction) public auctions;

    /// @notice Auction counter
    uint256 public auctionCounter;

    /// @notice Loan to auction mapping
    mapping(uint256 => uint256) public loanToAuction;

    /// @notice Interest rate tiers
    InterestTier[] public interestTiers;

    /// @notice Protocol fee (in BPS)
    uint256 public protocolFee = 500; // 5%

    /// @notice Treasury
    address public treasury;

    /// @notice Total liquidity provided
    uint256 public totalLiquidity;

    /// @notice Total borrowed
    uint256 public totalBorrowed;

    /// @notice Total interest collected
    uint256 public totalInterestCollected;

    /// @notice Protocol earnings
    uint256 public protocolEarnings;

    // ============================================
    // EVENTS
    // ============================================

    event CollectionAdded(
        address indexed collection,
        uint256 maxLTV,
        uint256 interestRate
    );

    event CollectionUpdated(
        address indexed collection,
        uint256 maxLTV,
        uint256 liquidationThreshold
    );

    event FloorPriceUpdated(
        address indexed collection,
        uint256 price,
        uint256 volatility
    );

    event LoanCreated(
        uint256 indexed loanId,
        address indexed borrower,
        address indexed collection,
        uint256 tokenId,
        uint256 principal,
        uint256 duration
    );

    event LoanRepaid(
        uint256 indexed loanId,
        address indexed borrower,
        uint256 principal,
        uint256 interest
    );

    event LoanRefinanced(
        uint256 indexed oldLoanId,
        uint256 indexed newLoanId,
        uint256 newPrincipal,
        uint256 newDuration
    );

    event LoanDefaulted(
        uint256 indexed loanId,
        address indexed borrower,
        uint256 debtOwed
    );

    event AuctionStarted(
        uint256 indexed auctionId,
        uint256 indexed loanId,
        uint256 startPrice
    );

    event BidPlaced(
        uint256 indexed auctionId,
        address indexed bidder,
        uint256 amount
    );

    event AuctionEnded(
        uint256 indexed auctionId,
        address indexed winner,
        uint256 winningBid
    );

    event LoanOfferCreated(
        uint256 indexed offerId,
        address indexed lender,
        address indexed collection,
        uint256 principal
    );

    event LoanOfferAccepted(
        uint256 indexed offerId,
        uint256 indexed loanId,
        address indexed borrower
    );

    event LoanOfferCancelled(uint256 indexed offerId);

    event LiquidityDeposited(
        address indexed depositor,
        uint256 amount
    );

    event LiquidityWithdrawn(
        address indexed withdrawer,
        uint256 amount
    );

    // ============================================
    // ERRORS
    // ============================================

    error CollectionNotActive(address collection);
    error CollectionPaused(address collection);
    error LoanNotFound(uint256 loanId);
    error LoanNotActive(uint256 loanId);
    error NotBorrower(uint256 loanId, address caller);
    error NotNFTOwner(address collection, uint256 tokenId);
    error LTVTooHigh(uint256 requested, uint256 max);
    error DurationOutOfRange(uint256 requested, uint256 min, uint256 max);
    error AmountOutOfRange(uint256 requested, uint256 min, uint256 max);
    error InsufficientLiquidity(uint256 available, uint256 requested);
    error LoanNotDefaulted(uint256 loanId);
    error AuctionNotActive(uint256 auctionId);
    error BidTooLow(uint256 minBid, uint256 bid);
    error AuctionNotEnded(uint256 auctionId);
    error NotWinner(uint256 auctionId, address caller);
    error OfferNotActive(uint256 offerId);
    error OfferExpired(uint256 offerId);
    error WrongCollection(address expected, address provided);
    error NFTAlreadyCollateralized(address collection, uint256 tokenId);
    error InvalidFloorPrice();
    error GracePeriodNotExpired(uint256 expiresAt);
    error ZeroAmount();

    // ============================================
    // MODIFIERS
    // ============================================

    modifier collectionActive(address collection) {
        if (!collections[collection].isActive) revert CollectionNotActive(collection);
        if (collections[collection].isPaused) revert CollectionPaused(collection);
        _;
    }

    modifier loanExists(uint256 loanId) {
        if (loans[loanId].borrower == address(0)) revert LoanNotFound(loanId);
        _;
    }

    modifier loanActive(uint256 loanId) {
        if (loans[loanId].status != LoanStatus.Active) revert LoanNotActive(loanId);
        _;
    }

    modifier onlyBorrower(uint256 loanId) {
        if (loans[loanId].borrower != msg.sender) revert NotBorrower(loanId, msg.sender);
        _;
    }

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(address _lendingAsset, address _treasury) {
        lendingAsset = IERC20(_lendingAsset);
        treasury = _treasury;

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(LENDING_ADMIN_ROLE, msg.sender);
        _grantRole(ORACLE_ROLE, msg.sender);

        // Default interest tiers
        interestTiers.push(InterestTier(2000, 500));   // 0-20% util: 5%
        interestTiers.push(InterestTier(5000, 1000));  // 20-50% util: 10%
        interestTiers.push(InterestTier(8000, 2000));  // 50-80% util: 20%
        interestTiers.push(InterestTier(10000, 5000)); // 80-100% util: 50%
    }

    // ============================================
    // COLLECTION MANAGEMENT
    // ============================================

    /**
     * @notice Adds a new collection
     */
    function addCollection(
        address collection,
        NFTType nftType,
        address oracle,
        uint256 maxLTV,
        uint256 liquidationThreshold,
        uint256 liquidationPenalty,
        uint256 interestRate,
        uint256 minLoanAmount,
        uint256 maxLoanAmount,
        uint256 minDuration,
        uint256 maxDuration
    ) external onlyRole(LENDING_ADMIN_ROLE) {
        require(collection != address(0), "Invalid collection");
        require(maxLTV < liquidationThreshold, "LTV >= threshold");
        require(liquidationThreshold <= BPS, "Invalid threshold");

        collections[collection] = CollectionConfig({
            collection: collection,
            nftType: nftType,
            oracle: oracle,
            maxLTV: maxLTV,
            liquidationThreshold: liquidationThreshold,
            liquidationPenalty: liquidationPenalty,
            interestRate: interestRate,
            minLoanAmount: minLoanAmount,
            maxLoanAmount: maxLoanAmount,
            minDuration: minDuration > MIN_LOAN_DURATION ? minDuration : MIN_LOAN_DURATION,
            maxDuration: maxDuration < MAX_LOAN_DURATION ? maxDuration : MAX_LOAN_DURATION,
            isActive: true,
            isPaused: false,
            totalLoans: 0,
            activeLoans: 0,
            totalBorrowed: 0,
            totalRepaid: 0,
            totalDefaulted: 0
        });

        activeCollections.push(collection);

        emit CollectionAdded(collection, maxLTV, interestRate);
    }

    /**
     * @notice Updates collection parameters
     */
    function updateCollection(
        address collection,
        uint256 maxLTV,
        uint256 liquidationThreshold,
        uint256 interestRate,
        uint256 maxLoanAmount
    ) external onlyRole(LENDING_ADMIN_ROLE) {
        CollectionConfig storage config = collections[collection];
        require(config.isActive, "Collection not found");

        config.maxLTV = maxLTV;
        config.liquidationThreshold = liquidationThreshold;
        config.interestRate = interestRate;
        config.maxLoanAmount = maxLoanAmount;

        emit CollectionUpdated(collection, maxLTV, liquidationThreshold);
    }

    /**
     * @notice Updates floor price
     */
    function updateFloorPrice(
        address collection,
        uint256 price,
        uint256 volatility,
        uint256 volume24h,
        uint256 salesCount24h
    ) external onlyRole(ORACLE_ROLE) {
        if (price == 0) revert InvalidFloorPrice();

        floorPrices[collection] = FloorPrice({
            price: price,
            timestamp: block.timestamp,
            volatility: volatility,
            volume24h: volume24h,
            salesCount24h: salesCount24h
        });

        emit FloorPriceUpdated(collection, price, volatility);
    }

    /**
     * @notice Pauses a collection
     */
    function pauseCollection(address collection) external onlyRole(LENDING_ADMIN_ROLE) {
        collections[collection].isPaused = true;
    }

    /**
     * @notice Unpauses a collection
     */
    function unpauseCollection(address collection) external onlyRole(LENDING_ADMIN_ROLE) {
        collections[collection].isPaused = false;
    }

    // ============================================
    // LIQUIDITY MANAGEMENT
    // ============================================

    /**
     * @notice Deposits liquidity
     */
    function depositLiquidity(uint256 amount) external nonReentrant whenNotPaused {
        if (amount == 0) revert ZeroAmount();

        lendingAsset.safeTransferFrom(msg.sender, address(this), amount);
        totalLiquidity += amount;

        emit LiquidityDeposited(msg.sender, amount);
    }

    /**
     * @notice Withdraws liquidity
     */
    function withdrawLiquidity(uint256 amount) external nonReentrant onlyRole(DEFAULT_ADMIN_ROLE) {
        uint256 available = totalLiquidity - totalBorrowed;
        if (amount > available) revert InsufficientLiquidity(available, amount);

        totalLiquidity -= amount;
        lendingAsset.safeTransfer(msg.sender, amount);

        emit LiquidityWithdrawn(msg.sender, amount);
    }

    // ============================================
    // BORROWING
    // ============================================

    /**
     * @notice Borrow parameters struct
     */
    struct BorrowParams {
        address collection;
        uint256 tokenId;
        uint256 amount1155;
        uint256 principal;
        uint256 duration;
    }

    /**
     * @notice Creates a loan with NFT collateral
     */
    function borrow(
        address collection,
        uint256 tokenId,
        uint256 amount1155,
        uint256 principal,
        uint256 duration
    ) 
        external 
        nonReentrant 
        whenNotPaused 
        collectionActive(collection)
        returns (uint256 loanId) 
    {
        BorrowParams memory params = BorrowParams(collection, tokenId, amount1155, principal, duration);
        return _executeBorrow(params);
    }

    function _executeBorrow(BorrowParams memory params) internal returns (uint256 loanId) {
        if (params.principal == 0) revert ZeroAmount();

        CollectionConfig storage config = collections[params.collection];

        _validateBorrowParams(params, config);
        _transferNFTIn(params.collection, params.tokenId, params.amount1155, config.nftType);

        loanId = _createLoan(params, config);
        _updateBorrowState(params.collection, params.principal, config);

        lendingAsset.safeTransfer(msg.sender, params.principal);

        emit LoanCreated(loanId, msg.sender, params.collection, params.tokenId, params.principal, params.duration);
    }

    function _validateBorrowParams(BorrowParams memory params, CollectionConfig storage config) internal view {
        if (params.duration < config.minDuration || params.duration > config.maxDuration) {
            revert DurationOutOfRange(params.duration, config.minDuration, config.maxDuration);
        }
        if (params.principal < config.minLoanAmount || params.principal > config.maxLoanAmount) {
            revert AmountOutOfRange(params.principal, config.minLoanAmount, config.maxLoanAmount);
        }
        if (nftToLoan[params.collection][params.tokenId] != 0) {
            revert NFTAlreadyCollateralized(params.collection, params.tokenId);
        }
        uint256 collateralValue = _getCollateralValue(params.collection, params.tokenId, params.amount1155);
        uint256 maxLoan = (collateralValue * config.maxLTV) / BPS;
        if (params.principal > maxLoan) {
            revert LTVTooHigh((params.principal * BPS) / collateralValue, config.maxLTV);
        }
        uint256 available = totalLiquidity - totalBorrowed;
        if (params.principal > available) {
            revert InsufficientLiquidity(available, params.principal);
        }
    }

    function _transferNFTIn(address collection, uint256 tokenId, uint256 amount1155, NFTType nftType) internal {
        if (nftType == NFTType.ERC721) {
            IERC721(collection).transferFrom(msg.sender, address(this), tokenId);
        } else {
            IERC1155(collection).safeTransferFrom(msg.sender, address(this), tokenId, amount1155, "");
        }
    }

    function _createLoan(BorrowParams memory params, CollectionConfig storage config) internal returns (uint256 loanId) {
        loanId = ++loanCounter;
        loans[loanId] = Loan({
            loanId: loanId,
            borrower: msg.sender,
            collection: params.collection,
            tokenId: params.tokenId,
            amount1155: params.amount1155,
            principal: params.principal,
            interestRate: _calculateInterestRate(params.collection),
            startTime: block.timestamp,
            duration: params.duration,
            dueDate: block.timestamp + params.duration,
            lastInterestAccrual: block.timestamp,
            accruedInterest: 0,
            floorPriceAtLoan: floorPrices[params.collection].price,
            status: LoanStatus.Active
        });
        nftToLoan[params.collection][params.tokenId] = loanId;
        userLoans[msg.sender].push(loanId);
    }

    function _updateBorrowState(address collection, uint256 principal, CollectionConfig storage config) internal {
        totalBorrowed += principal;
        config.totalLoans++;
        config.activeLoans++;
        config.totalBorrowed += principal;
    }

    /**
     * @notice Repays a loan
     */
    function repay(uint256 loanId) 
        external 
        nonReentrant 
        loanExists(loanId) 
        loanActive(loanId) 
    {
        Loan storage loan = loans[loanId];

        // Accrue interest
        _accrueInterest(loanId);

        uint256 totalOwed = loan.principal + loan.accruedInterest;

        // Transfer payment
        lendingAsset.safeTransferFrom(msg.sender, address(this), totalOwed);

        // Update state
        loan.status = LoanStatus.Repaid;
        totalBorrowed -= loan.principal;
        totalInterestCollected += loan.accruedInterest;

        CollectionConfig storage config = collections[loan.collection];
        config.activeLoans--;
        config.totalRepaid += loan.principal;

        // Calculate protocol fee
        uint256 protocolShare = (loan.accruedInterest * protocolFee) / BPS;
        protocolEarnings += protocolShare;

        // Return NFT
        if (config.nftType == NFTType.ERC721) {
            IERC721(loan.collection).transferFrom(address(this), loan.borrower, loan.tokenId);
        } else {
            IERC1155(loan.collection).safeTransferFrom(
                address(this), 
                loan.borrower, 
                loan.tokenId, 
                loan.amount1155, 
                ""
            );
        }

        delete nftToLoan[loan.collection][loan.tokenId];

        emit LoanRepaid(loanId, loan.borrower, loan.principal, loan.accruedInterest);
    }

    /**
     * @notice Refinances a loan with new terms
     */
    function refinance(
        uint256 loanId,
        uint256 newPrincipal,
        uint256 newDuration
    ) 
        external 
        nonReentrant 
        loanExists(loanId) 
        loanActive(loanId)
        onlyBorrower(loanId)
        returns (uint256 newLoanId)
    {
        Loan storage oldLoan = loans[loanId];
        _accrueInterest(loanId);
        
        uint256 oldDebt = oldLoan.principal + oldLoan.accruedInterest;
        _validateRefinance(oldLoan, newPrincipal);
        _handleRefinancePrincipal(oldLoan.principal, newPrincipal, oldDebt);
        
        newLoanId = _executeRefinance(loanId, oldLoan, newPrincipal, newDuration);
        
        emit LoanRefinanced(loanId, newLoanId, newPrincipal, newDuration);
    }

    function _validateRefinance(Loan storage oldLoan, uint256 newPrincipal) internal view {
        CollectionConfig storage config = collections[oldLoan.collection];
        uint256 collateralValue = _getCollateralValue(oldLoan.collection, oldLoan.tokenId, oldLoan.amount1155);
        uint256 maxLoan = (collateralValue * config.maxLTV) / BPS;
        if (newPrincipal > maxLoan) {
            revert LTVTooHigh((newPrincipal * BPS) / collateralValue, config.maxLTV);
        }
    }

    function _handleRefinancePrincipal(uint256 oldPrincipal, uint256 newPrincipal, uint256 oldDebt) internal {
        if (newPrincipal > oldDebt) {
            uint256 additional = newPrincipal - oldDebt;
            uint256 available = totalLiquidity - totalBorrowed;
            if (additional > available) {
                revert InsufficientLiquidity(available, additional);
            }
            lendingAsset.safeTransfer(msg.sender, additional);
            totalBorrowed += additional;
        } else if (newPrincipal < oldDebt) {
            uint256 payment = oldDebt - newPrincipal;
            lendingAsset.safeTransferFrom(msg.sender, address(this), payment);
            totalBorrowed -= oldPrincipal - newPrincipal;
        }
    }

    function _executeRefinance(
        uint256 oldLoanId,
        Loan storage oldLoan,
        uint256 newPrincipal,
        uint256 newDuration
    ) internal returns (uint256 newLoanId) {
        CollectionConfig storage config = collections[oldLoan.collection];
        
        oldLoan.status = LoanStatus.Repaid;
        config.activeLoans--;
        delete nftToLoan[oldLoan.collection][oldLoan.tokenId];

        newLoanId = ++loanCounter;
        loans[newLoanId] = Loan({
            loanId: newLoanId,
            borrower: msg.sender,
            collection: oldLoan.collection,
            tokenId: oldLoan.tokenId,
            amount1155: oldLoan.amount1155,
            principal: newPrincipal,
            interestRate: _calculateInterestRate(oldLoan.collection),
            startTime: block.timestamp,
            duration: newDuration,
            dueDate: block.timestamp + newDuration,
            lastInterestAccrual: block.timestamp,
            accruedInterest: 0,
            floorPriceAtLoan: floorPrices[oldLoan.collection].price,
            status: LoanStatus.Active
        });

        nftToLoan[oldLoan.collection][oldLoan.tokenId] = newLoanId;
        userLoans[msg.sender].push(newLoanId);
        config.activeLoans++;
        config.totalLoans++;
    }

    // ============================================
    // LIQUIDATION
    // ============================================

    /**
     * @notice Starts liquidation auction for defaulted loan
     */
    function startLiquidation(uint256 loanId) 
        external 
        nonReentrant 
        onlyRole(LIQUIDATOR_ROLE)
        loanExists(loanId)
        loanActive(loanId)
    {
        Loan storage loan = loans[loanId];
        if (!_isLoanDefaulted(loanId)) revert LoanNotDefaulted(loanId);
        if (block.timestamp < loan.dueDate + GRACE_PERIOD) {
            revert GracePeriodNotExpired(loan.dueDate + GRACE_PERIOD);
        }

        _accrueInterest(loanId);
        loan.status = LoanStatus.Liquidating;

        uint256 auctionId = _createAuction(loanId, loan);
        
        emit LoanDefaulted(loanId, loan.borrower, loan.principal + loan.accruedInterest);
        emit AuctionStarted(auctionId, loanId, auctions[auctionId].startPrice);
    }

    function _createAuction(uint256 loanId, Loan storage loan) internal returns (uint256 auctionId) {
        CollectionConfig storage config = collections[loan.collection];
        uint256 collateralValue = _getCollateralValue(loan.collection, loan.tokenId, loan.amount1155);
        uint256 debtOwed = loan.principal + loan.accruedInterest;

        auctionId = ++auctionCounter;
        auctions[auctionId] = Auction({
            auctionId: auctionId,
            loanId: loanId,
            collection: loan.collection,
            tokenId: loan.tokenId,
            startPrice: (collateralValue * (BPS - config.liquidationPenalty)) / BPS,
            currentBid: 0,
            currentBidder: address(0),
            startTime: block.timestamp,
            endTime: block.timestamp + AUCTION_DURATION,
            status: AuctionStatus.Active,
            debtToCover: debtOwed,
            minBidIncrement: collateralValue / 100
        });
        loanToAuction[loanId] = auctionId;
    }

    /**
     * @notice Places a bid on auction
     */
    function placeBid(uint256 auctionId, uint256 amount) 
        external 
        nonReentrant 
        whenNotPaused 
    {
        Auction storage auction = auctions[auctionId];
        
        if (auction.status != AuctionStatus.Active) revert AuctionNotActive(auctionId);
        if (block.timestamp > auction.endTime) revert AuctionNotActive(auctionId);

        uint256 minBid = auction.currentBid == 0 
            ? auction.startPrice 
            : auction.currentBid + auction.minBidIncrement;

        if (amount < minBid) revert BidTooLow(minBid, amount);

        // Refund previous bidder
        if (auction.currentBidder != address(0)) {
            lendingAsset.safeTransfer(auction.currentBidder, auction.currentBid);
        }

        // Accept new bid
        lendingAsset.safeTransferFrom(msg.sender, address(this), amount);

        auction.currentBid = amount;
        auction.currentBidder = msg.sender;

        // Extend auction if bid in last 10 minutes
        if (auction.endTime - block.timestamp < 10 minutes) {
            auction.endTime = block.timestamp + 10 minutes;
        }

        emit BidPlaced(auctionId, msg.sender, amount);
    }

    /**
     * @notice Ends auction and distributes proceeds
     */
    function endAuction(uint256 auctionId) external nonReentrant {
        Auction storage auction = auctions[auctionId];
        
        if (auction.status != AuctionStatus.Active) revert AuctionNotActive(auctionId);
        if (block.timestamp < auction.endTime) revert AuctionNotEnded(auctionId);

        Loan storage loan = loans[auction.loanId];
        auction.status = AuctionStatus.Ended;
        loan.status = LoanStatus.Liquidated;

        if (auction.currentBidder != address(0)) {
            _handleAuctionWithWinner(auction, loan);
        } else {
            _handleAuctionNoWinner(auction, loan);
        }

        CollectionConfig storage config = collections[loan.collection];
        config.activeLoans--;
        delete nftToLoan[loan.collection][loan.tokenId];
    }

    function _handleAuctionWithWinner(Auction storage auction, Loan storage loan) internal {
        CollectionConfig storage config = collections[loan.collection];
        uint256 proceeds = auction.currentBid;
        
        totalBorrowed -= loan.principal;
        
        if (proceeds > auction.debtToCover) {
            uint256 surplus = proceeds - auction.debtToCover;
            lendingAsset.safeTransfer(loan.borrower, surplus);
        }

        _transferNFTOut(loan.collection, auction.currentBidder, loan.tokenId, loan.amount1155, config.nftType);
        emit AuctionEnded(auction.auctionId, auction.currentBidder, auction.currentBid);
    }

    function _handleAuctionNoWinner(Auction storage auction, Loan storage loan) internal {
        CollectionConfig storage config = collections[loan.collection];
        _transferNFTOut(loan.collection, treasury, loan.tokenId, loan.amount1155, config.nftType);
        config.totalDefaulted += loan.principal;
        totalBorrowed -= loan.principal;
        emit AuctionEnded(auction.auctionId, treasury, 0);
    }

    function _transferNFTOut(address collection, address to, uint256 tokenId, uint256 amount1155, NFTType nftType) internal {
        if (nftType == NFTType.ERC721) {
            IERC721(collection).transferFrom(address(this), to, tokenId);
        } else {
            IERC1155(collection).safeTransferFrom(address(this), to, tokenId, amount1155, "");
        }
    }

    // ============================================
    // P2P LOAN OFFERS
    // ============================================

    /**
     * @notice Creates a loan offer
     */
    function createOffer(
        address collection,
        uint256 principal,
        uint256 interestRate,
        uint256 duration,
        uint256 expiry,
        uint256 maxLTV
    ) 
        external 
        nonReentrant 
        collectionActive(collection)
        returns (uint256 offerId)
    {
        if (principal == 0) revert ZeroAmount();

        // Lock funds
        lendingAsset.safeTransferFrom(msg.sender, address(this), principal);

        offerId = ++offerCounter;

        loanOffers[offerId] = LoanOffer({
            offerId: offerId,
            lender: msg.sender,
            collection: collection,
            principal: principal,
            interestRate: interestRate,
            duration: duration,
            expiry: expiry,
            isActive: true,
            maxLTV: maxLTV
        });

        userOffers[msg.sender].push(offerId);

        emit LoanOfferCreated(offerId, msg.sender, collection, principal);
    }

    /**
     * @notice Accepts a loan offer
     */
    function acceptOffer(
        uint256 offerId,
        uint256 tokenId,
        uint256 amount1155
    ) 
        external 
        nonReentrant 
        whenNotPaused
        returns (uint256 loanId)
    {
        LoanOffer storage offer = loanOffers[offerId];
        
        if (!offer.isActive) revert OfferNotActive(offerId);
        if (block.timestamp > offer.expiry) revert OfferExpired(offerId);

        _validateOfferCollateral(offer, tokenId, amount1155);
        
        CollectionConfig storage config = collections[offer.collection];
        _transferNFTIn(offer.collection, tokenId, amount1155, config.nftType);

        loanId = _createLoanFromOffer(offer, tokenId, amount1155);
        offer.isActive = false;
        lendingAsset.safeTransfer(msg.sender, offer.principal);

        config.totalLoans++;
        config.activeLoans++;

        emit LoanOfferAccepted(offerId, loanId, msg.sender);
    }

    function _validateOfferCollateral(LoanOffer storage offer, uint256 tokenId, uint256 amount1155) internal view {
        uint256 collateralValue = _getCollateralValue(offer.collection, tokenId, amount1155);
        uint256 requiredCollateral = (offer.principal * BPS) / offer.maxLTV;
        if (collateralValue < requiredCollateral) {
            revert LTVTooHigh((offer.principal * BPS) / collateralValue, offer.maxLTV);
        }
    }

    function _createLoanFromOffer(
        LoanOffer storage offer,
        uint256 tokenId,
        uint256 amount1155
    ) internal returns (uint256 loanId) {
        loanId = ++loanCounter;
        loans[loanId] = Loan({
            loanId: loanId,
            borrower: msg.sender,
            collection: offer.collection,
            tokenId: tokenId,
            amount1155: amount1155,
            principal: offer.principal,
            interestRate: offer.interestRate,
            startTime: block.timestamp,
            duration: offer.duration,
            dueDate: block.timestamp + offer.duration,
            lastInterestAccrual: block.timestamp,
            accruedInterest: 0,
            floorPriceAtLoan: floorPrices[offer.collection].price,
            status: LoanStatus.Active
        });
        nftToLoan[offer.collection][tokenId] = loanId;
        userLoans[msg.sender].push(loanId);
    }

    /**
     * @notice Cancels a loan offer
     */
    function cancelOffer(uint256 offerId) external nonReentrant {
        LoanOffer storage offer = loanOffers[offerId];
        
        require(offer.lender == msg.sender, "Not offer owner");
        require(offer.isActive, "Offer not active");

        offer.isActive = false;

        // Refund locked funds
        lendingAsset.safeTransfer(msg.sender, offer.principal);

        emit LoanOfferCancelled(offerId);
    }

    // ============================================
    // INTERNAL FUNCTIONS
    // ============================================

    /**
     * @notice Accrues interest on a loan
     */
    function _accrueInterest(uint256 loanId) internal {
        Loan storage loan = loans[loanId];

        uint256 timeElapsed = block.timestamp - loan.lastInterestAccrual;
        if (timeElapsed == 0) return;

        uint256 interest = (loan.principal * loan.interestRate * timeElapsed) 
            / (BPS * SECONDS_PER_YEAR);

        loan.accruedInterest += interest;
        loan.lastInterestAccrual = block.timestamp;
    }

    /**
     * @notice Gets collateral value
     */
    function _getCollateralValue(
        address collection,
        uint256 tokenId,
        uint256 amount1155
    ) internal view returns (uint256) {
        FloorPrice storage floor = floorPrices[collection];
        CollectionConfig storage config = collections[collection];

        uint256 baseValue = floor.price;

        // Adjust for volatility
        if (floor.volatility > 2000) { // > 20% volatility
            baseValue = (baseValue * (BPS - floor.volatility / 10)) / BPS;
        }

        if (config.nftType == NFTType.ERC1155) {
            return baseValue * amount1155;
        }

        return baseValue;
    }

    /**
     * @notice Calculates current interest rate based on utilization
     */
    function _calculateInterestRate(address collection) internal view returns (uint256) {
        CollectionConfig storage config = collections[collection];

        uint256 utilization = totalLiquidity > 0 
            ? (totalBorrowed * BPS) / totalLiquidity 
            : 0;

        for (uint256 i = 0; i < interestTiers.length; i++) {
            if (utilization <= interestTiers[i].utilizationThreshold) {
                return interestTiers[i].rate > config.interestRate 
                    ? interestTiers[i].rate 
                    : config.interestRate;
            }
        }

        return config.interestRate;
    }

    /**
     * @notice Checks if loan is defaulted
     */
    function _isLoanDefaulted(uint256 loanId) internal view returns (bool) {
        Loan storage loan = loans[loanId];

        // Past due date
        if (block.timestamp > loan.dueDate) {
            return true;
        }

        // Below liquidation threshold
        uint256 currentValue = _getCollateralValue(
            loan.collection, 
            loan.tokenId, 
            loan.amount1155
        );
        
        _accrueInterestView(loanId);
        uint256 debt = loan.principal + loan.accruedInterest;
        uint256 ltv = (debt * BPS) / currentValue;

        CollectionConfig storage config = collections[loan.collection];
        return ltv >= config.liquidationThreshold;
    }

    function _accrueInterestView(uint256 loanId) internal view returns (uint256) {
        Loan storage loan = loans[loanId];
        uint256 timeElapsed = block.timestamp - loan.lastInterestAccrual;
        return loan.accruedInterest + 
            (loan.principal * loan.interestRate * timeElapsed) / (BPS * SECONDS_PER_YEAR);
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Gets loan health factor
     */
    function getLoanHealth(uint256 loanId) external view returns (uint256) {
        Loan storage loan = loans[loanId];
        if (loan.status != LoanStatus.Active) return 0;

        uint256 collateralValue = _getCollateralValue(
            loan.collection, 
            loan.tokenId, 
            loan.amount1155
        );
        
        uint256 debt = loan.principal + _accrueInterestView(loanId);
        CollectionConfig storage config = collections[loan.collection];

        return (collateralValue * config.liquidationThreshold) / debt;
    }

    /**
     * @notice Gets total debt for a loan
     */
    function getTotalDebt(uint256 loanId) external view returns (uint256) {
        Loan storage loan = loans[loanId];
        return loan.principal + _accrueInterestView(loanId);
    }

    /**
     * @notice Gets user's active loans
     */
    function getUserActiveLoans(address user) external view returns (uint256[] memory) {
        uint256[] storage allLoans = userLoans[user];
        uint256 count = 0;
        
        for (uint256 i = 0; i < allLoans.length; i++) {
            if (loans[allLoans[i]].status == LoanStatus.Active) {
                count++;
            }
        }

        uint256[] memory activeLoans = new uint256[](count);
        uint256 index = 0;
        
        for (uint256 i = 0; i < allLoans.length; i++) {
            if (loans[allLoans[i]].status == LoanStatus.Active) {
                activeLoans[index++] = allLoans[i];
            }
        }

        return activeLoans;
    }

    /**
     * @notice Gets utilization rate
     */
    function getUtilizationRate() external view returns (uint256) {
        if (totalLiquidity == 0) return 0;
        return (totalBorrowed * BPS) / totalLiquidity;
    }

    /**
     * @notice Gets available liquidity
     */
    function getAvailableLiquidity() external view returns (uint256) {
        return totalLiquidity - totalBorrowed;
    }

    /**
     * @notice Gets active collections
     */
    function getActiveCollections() external view returns (address[] memory) {
        return activeCollections;
    }

    // ============================================
    // ADMIN FUNCTIONS
    // ============================================

    /**
     * @notice Updates protocol fee
     */
    function setProtocolFee(uint256 newFee) external onlyRole(DEFAULT_ADMIN_ROLE) {
        require(newFee <= 2000, "Fee too high"); // Max 20%
        protocolFee = newFee;
    }

    /**
     * @notice Withdraws protocol earnings
     */
    function withdrawEarnings() external onlyRole(DEFAULT_ADMIN_ROLE) {
        uint256 earnings = protocolEarnings;
        protocolEarnings = 0;
        lendingAsset.safeTransfer(treasury, earnings);
    }

    /**
     * @notice Pauses contract
     */
    function pause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        _pause();
    }

    /**
     * @notice Unpauses contract
     */
    function unpause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        _unpause();
    }

    /**
     * @notice ERC1155 receiver
     */
    function onERC1155Received(
        address,
        address,
        uint256,
        uint256,
        bytes calldata
    ) external pure returns (bytes4) {
        return this.onERC1155Received.selector;
    }

    function onERC1155BatchReceived(
        address,
        address,
        uint256[] calldata,
        uint256[] calldata,
        bytes calldata
    ) external pure returns (bytes4) {
        return this.onERC1155BatchReceived.selector;
    }
}
