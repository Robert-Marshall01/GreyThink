// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/token/ERC721/IERC721.sol";
import "@openzeppelin/contracts/token/ERC1155/IERC1155.sol";
import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/Pausable.sol";

/**
 * @title Escrow
 * @author Grey Solidity Project
 * @notice Multi-asset escrow with disputes and arbitration
 * @dev Supports ETH, ERC20, ERC721, and ERC1155
 */
contract Escrow is AccessControl, ReentrancyGuard, Pausable {
    using SafeERC20 for IERC20;

    /// @notice Role for arbitration
    bytes32 public constant ARBITER_ROLE = keccak256("ARBITER_ROLE");

    /// @notice Escrow status
    enum Status {
        Created,
        Funded,
        Completed,
        Refunded,
        Disputed,
        Resolved
    }

    /// @notice Asset types
    enum AssetType {
        ETH,
        ERC20,
        ERC721,
        ERC1155
    }

    /// @notice Asset info
    struct Asset {
        AssetType assetType;
        address contractAddress;
        uint256 tokenId;        // For NFTs
        uint256 amount;
    }

    /// @notice Escrow deal structure
    struct Deal {
        address buyer;
        address seller;
        Asset[] buyerAssets;     // What buyer deposits
        Asset[] sellerAssets;    // What seller deposits
        uint256 deadline;
        Status status;
        bool buyerApproved;
        bool sellerApproved;
        uint256 createdAt;
        string description;
    }

    /// @notice Dispute structure
    struct Dispute {
        address initiator;
        string reason;
        uint256 timestamp;
        bool resolved;
        address winner;
        string resolution;
    }

    /// @notice Mapping of deal ID to deal
    mapping(uint256 => Deal) public deals;

    /// @notice Mapping of deal ID to dispute
    mapping(uint256 => Dispute) public disputes;

    /// @notice Next deal ID
    uint256 public nextDealId;

    /// @notice User's deals
    mapping(address => uint256[]) public userDeals;

    /// @notice Fee in basis points
    uint256 public escrowFee;

    /// @notice Fee recipient
    address public feeRecipient;

    /// @notice Collected ETH fees
    uint256 public collectedEthFees;

    /// @notice Collected token fees
    mapping(address => uint256) public collectedTokenFees;

    // ============ Events ============

    event DealCreated(
        uint256 indexed dealId,
        address indexed buyer,
        address indexed seller,
        uint256 deadline
    );

    event AssetDeposited(
        uint256 indexed dealId,
        address indexed depositor,
        AssetType assetType,
        address contractAddress,
        uint256 amount
    );

    event DealApproved(uint256 indexed dealId, address indexed approver);
    event DealCompleted(uint256 indexed dealId);
    event DealRefunded(uint256 indexed dealId);

    event DisputeRaised(
        uint256 indexed dealId,
        address indexed initiator,
        string reason
    );

    event DisputeResolved(
        uint256 indexed dealId,
        address indexed winner,
        string resolution
    );

    event FeeUpdated(uint256 oldFee, uint256 newFee);
    event FeesWithdrawn(address indexed recipient, uint256 ethAmount);

    // ============ Errors ============

    error NotParty();
    error NotBuyer();
    error NotSeller();
    error DealExpired();
    error DealNotExpired();
    error InvalidStatus(Status current, Status required);
    error AlreadyApproved();
    error AlreadyDisputed();
    error DisputeNotRaised();
    error InsufficientDeposit();
    error ZeroAddress();
    error EmptyAssets();
    error TransferFailed();

    /**
     * @notice Initializes the escrow contract
     * @param fee_ Initial fee in basis points
     * @param feeRecipient_ Fee recipient address
     */
    constructor(uint256 fee_, address feeRecipient_) {
        require(fee_ <= 500, "Escrow: fee too high");  // Max 5%
        escrowFee = fee_;
        feeRecipient = feeRecipient_;

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(ARBITER_ROLE, msg.sender);
    }

    // ============ Deal Creation ============

    /**
     * @notice Creates a new escrow deal
     * @param seller The seller address
     * @param buyerAssets Assets the buyer will deposit
     * @param sellerAssets Assets the seller will deposit
     * @param deadline Deal deadline
     * @param description Deal description
     * @return dealId The deal ID
     */
    function createDeal(
        address seller,
        Asset[] calldata buyerAssets,
        Asset[] calldata sellerAssets,
        uint256 deadline,
        string calldata description
    ) external whenNotPaused returns (uint256 dealId) {
        if (seller == address(0)) revert ZeroAddress();
        if (buyerAssets.length == 0 && sellerAssets.length == 0) revert EmptyAssets();
        require(deadline > block.timestamp, "Escrow: invalid deadline");

        dealId = nextDealId++;

        Deal storage deal = deals[dealId];
        deal.buyer = msg.sender;
        deal.seller = seller;
        deal.deadline = deadline;
        deal.status = Status.Created;
        deal.createdAt = block.timestamp;
        deal.description = description;

        // Copy assets
        for (uint256 i = 0; i < buyerAssets.length; i++) {
            deal.buyerAssets.push(buyerAssets[i]);
        }
        for (uint256 i = 0; i < sellerAssets.length; i++) {
            deal.sellerAssets.push(sellerAssets[i]);
        }

        userDeals[msg.sender].push(dealId);
        userDeals[seller].push(dealId);

        emit DealCreated(dealId, msg.sender, seller, deadline);
    }

    // ============ Deposit Functions ============

    /**
     * @notice Deposits ETH for a deal
     * @param dealId The deal ID
     */
    function depositETH(uint256 dealId) external payable nonReentrant whenNotPaused {
        Deal storage deal = deals[dealId];
        _validateDeposit(deal);

        bool isBuyer = msg.sender == deal.buyer;
        Asset[] storage assets = isBuyer ? deal.buyerAssets : deal.sellerAssets;

        uint256 required = 0;
        for (uint256 i = 0; i < assets.length; i++) {
            if (assets[i].assetType == AssetType.ETH) {
                required += assets[i].amount;
            }
        }

        if (msg.value < required) revert InsufficientDeposit();

        emit AssetDeposited(dealId, msg.sender, AssetType.ETH, address(0), msg.value);

        _checkAndUpdateStatus(dealId);
    }

    /**
     * @notice Deposits ERC20 tokens for a deal
     * @param dealId The deal ID
     * @param token Token address
     * @param amount Amount to deposit
     */
    function depositERC20(
        uint256 dealId,
        address token,
        uint256 amount
    ) external nonReentrant whenNotPaused {
        Deal storage deal = deals[dealId];
        _validateDeposit(deal);

        IERC20(token).safeTransferFrom(msg.sender, address(this), amount);

        emit AssetDeposited(dealId, msg.sender, AssetType.ERC20, token, amount);

        _checkAndUpdateStatus(dealId);
    }

    /**
     * @notice Deposits an NFT for a deal
     * @param dealId The deal ID
     * @param nftContract NFT contract address
     * @param tokenId Token ID
     */
    function depositNFT(
        uint256 dealId,
        address nftContract,
        uint256 tokenId
    ) external nonReentrant whenNotPaused {
        Deal storage deal = deals[dealId];
        _validateDeposit(deal);

        IERC721(nftContract).transferFrom(msg.sender, address(this), tokenId);

        emit AssetDeposited(dealId, msg.sender, AssetType.ERC721, nftContract, tokenId);

        _checkAndUpdateStatus(dealId);
    }

    /**
     * @notice Deposits ERC1155 tokens for a deal
     * @param dealId The deal ID
     * @param collection Collection address
     * @param tokenId Token ID
     * @param amount Amount
     */
    function depositERC1155(
        uint256 dealId,
        address collection,
        uint256 tokenId,
        uint256 amount
    ) external nonReentrant whenNotPaused {
        Deal storage deal = deals[dealId];
        _validateDeposit(deal);

        IERC1155(collection).safeTransferFrom(msg.sender, address(this), tokenId, amount, "");

        emit AssetDeposited(dealId, msg.sender, AssetType.ERC1155, collection, amount);

        _checkAndUpdateStatus(dealId);
    }

    // ============ Approval & Completion ============

    /**
     * @notice Approves the deal (both parties must approve)
     * @param dealId The deal ID
     */
    function approveDeal(uint256 dealId) external nonReentrant whenNotPaused {
        Deal storage deal = deals[dealId];
        
        if (deal.status != Status.Funded) {
            revert InvalidStatus(deal.status, Status.Funded);
        }
        if (msg.sender != deal.buyer && msg.sender != deal.seller) {
            revert NotParty();
        }
        if (block.timestamp > deal.deadline) revert DealExpired();

        if (msg.sender == deal.buyer) {
            if (deal.buyerApproved) revert AlreadyApproved();
            deal.buyerApproved = true;
        } else {
            if (deal.sellerApproved) revert AlreadyApproved();
            deal.sellerApproved = true;
        }

        emit DealApproved(dealId, msg.sender);

        // If both approved, complete the deal
        if (deal.buyerApproved && deal.sellerApproved) {
            _completeDeal(dealId);
        }
    }

    /**
     * @notice Refunds a deal (only before funding or after expiry)
     * @param dealId The deal ID
     */
    function refundDeal(uint256 dealId) external nonReentrant {
        Deal storage deal = deals[dealId];
        
        if (msg.sender != deal.buyer && msg.sender != deal.seller) {
            revert NotParty();
        }

        // Can refund if: not funded yet, or expired without dispute
        if (deal.status == Status.Funded && block.timestamp <= deal.deadline) {
            revert DealNotExpired();
        }
        if (deal.status == Status.Disputed) {
            revert InvalidStatus(deal.status, Status.Funded);
        }
        if (deal.status == Status.Completed || deal.status == Status.Refunded) {
            revert InvalidStatus(deal.status, Status.Funded);
        }

        deal.status = Status.Refunded;

        // Return assets to original depositors
        _refundAssets(deal.buyer, deal.buyerAssets);
        _refundAssets(deal.seller, deal.sellerAssets);

        emit DealRefunded(dealId);
    }

    // ============ Dispute Functions ============

    /**
     * @notice Raises a dispute
     * @param dealId The deal ID
     * @param reason Dispute reason
     */
    function raiseDispute(uint256 dealId, string calldata reason) external {
        Deal storage deal = deals[dealId];
        
        if (msg.sender != deal.buyer && msg.sender != deal.seller) {
            revert NotParty();
        }
        if (deal.status != Status.Funded) {
            revert InvalidStatus(deal.status, Status.Funded);
        }
        if (disputes[dealId].timestamp > 0) revert AlreadyDisputed();

        deal.status = Status.Disputed;

        disputes[dealId] = Dispute({
            initiator: msg.sender,
            reason: reason,
            timestamp: block.timestamp,
            resolved: false,
            winner: address(0),
            resolution: ""
        });

        emit DisputeRaised(dealId, msg.sender, reason);
    }

    /**
     * @notice Resolves a dispute (arbiter only)
     * @param dealId The deal ID
     * @param winner The winning party
     * @param resolution Resolution description
     */
    function resolveDispute(
        uint256 dealId,
        address winner,
        string calldata resolution
    ) external onlyRole(ARBITER_ROLE) nonReentrant {
        Deal storage deal = deals[dealId];
        Dispute storage dispute = disputes[dealId];

        if (deal.status != Status.Disputed) {
            revert InvalidStatus(deal.status, Status.Disputed);
        }
        if (winner != deal.buyer && winner != deal.seller) {
            revert NotParty();
        }

        deal.status = Status.Resolved;
        dispute.resolved = true;
        dispute.winner = winner;
        dispute.resolution = resolution;

        // Transfer all assets to winner
        _transferAssets(winner, deal.buyerAssets);
        _transferAssets(winner, deal.sellerAssets);

        emit DisputeResolved(dealId, winner, resolution);
    }

    // ============ View Functions ============

    /**
     * @notice Returns deal info
     * @param dealId The deal ID
     * @return buyer The buyer address
     * @return seller The seller address
     * @return deadline The deadline
     * @return status The status
     * @return buyerApproved Buyer approval status
     * @return sellerApproved Seller approval status
     */
    function getDealInfo(uint256 dealId) external view returns (
        address buyer,
        address seller,
        uint256 deadline,
        Status status,
        bool buyerApproved,
        bool sellerApproved
    ) {
        Deal storage deal = deals[dealId];
        return (
            deal.buyer,
            deal.seller,
            deal.deadline,
            deal.status,
            deal.buyerApproved,
            deal.sellerApproved
        );
    }

    /**
     * @notice Returns buyer's assets for a deal
     * @param dealId The deal ID
     * @return Array of assets
     */
    function getBuyerAssets(uint256 dealId) external view returns (Asset[] memory) {
        return deals[dealId].buyerAssets;
    }

    /**
     * @notice Returns seller's assets for a deal
     * @param dealId The deal ID
     * @return Array of assets
     */
    function getSellerAssets(uint256 dealId) external view returns (Asset[] memory) {
        return deals[dealId].sellerAssets;
    }

    /**
     * @notice Returns dispute info
     * @param dealId The deal ID
     * @return The dispute struct
     */
    function getDispute(uint256 dealId) external view returns (Dispute memory) {
        return disputes[dealId];
    }

    /**
     * @notice Returns user's deals
     * @param user The user address
     * @return Array of deal IDs
     */
    function getUserDeals(address user) external view returns (uint256[] memory) {
        return userDeals[user];
    }

    // ============ Admin Functions ============

    /**
     * @notice Sets the escrow fee
     * @param newFee New fee in basis points
     */
    function setFee(uint256 newFee) external onlyRole(DEFAULT_ADMIN_ROLE) {
        require(newFee <= 500, "Escrow: fee too high");
        uint256 oldFee = escrowFee;
        escrowFee = newFee;
        emit FeeUpdated(oldFee, newFee);
    }

    /**
     * @notice Withdraws collected fees
     */
    function withdrawFees() external onlyRole(DEFAULT_ADMIN_ROLE) nonReentrant {
        uint256 ethFees = collectedEthFees;
        collectedEthFees = 0;

        if (ethFees > 0) {
            (bool success, ) = feeRecipient.call{value: ethFees}("");
            if (!success) revert TransferFailed();
        }

        emit FeesWithdrawn(feeRecipient, ethFees);
    }

    /**
     * @notice Pauses the escrow
     */
    function pause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        _pause();
    }

    /**
     * @notice Unpauses the escrow
     */
    function unpause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        _unpause();
    }

    // ============ Internal Functions ============

    function _validateDeposit(Deal storage deal) private view {
        if (deal.status != Status.Created && deal.status != Status.Funded) {
            revert InvalidStatus(deal.status, Status.Created);
        }
        if (msg.sender != deal.buyer && msg.sender != deal.seller) {
            revert NotParty();
        }
        if (block.timestamp > deal.deadline) revert DealExpired();
    }

    function _checkAndUpdateStatus(uint256 dealId) private {
        Deal storage deal = deals[dealId];
        if (deal.status == Status.Created) {
            deal.status = Status.Funded;
        }
    }

    function _completeDeal(uint256 dealId) private {
        Deal storage deal = deals[dealId];
        deal.status = Status.Completed;

        // Calculate and collect fees
        // Transfer buyer assets to seller
        _transferAssetsWithFee(deal.seller, deal.buyerAssets);
        // Transfer seller assets to buyer
        _transferAssetsWithFee(deal.buyer, deal.sellerAssets);

        emit DealCompleted(dealId);
    }

    function _transferAssets(address to, Asset[] storage assets) private {
        for (uint256 i = 0; i < assets.length; i++) {
            Asset storage asset = assets[i];
            _transferAsset(to, asset);
        }
    }

    function _transferAssetsWithFee(address to, Asset[] storage assets) private {
        for (uint256 i = 0; i < assets.length; i++) {
            Asset storage asset = assets[i];
            
            if (asset.assetType == AssetType.ETH) {
                uint256 fee = (asset.amount * escrowFee) / 10000;
                uint256 netAmount = asset.amount - fee;
                collectedEthFees += fee;
                
                (bool success, ) = to.call{value: netAmount}("");
                require(success, "ETH transfer failed");
            } else if (asset.assetType == AssetType.ERC20) {
                uint256 fee = (asset.amount * escrowFee) / 10000;
                uint256 netAmount = asset.amount - fee;
                collectedTokenFees[asset.contractAddress] += fee;
                
                IERC20(asset.contractAddress).safeTransfer(to, netAmount);
            } else {
                _transferAsset(to, asset);
            }
        }
    }

    function _transferAsset(address to, Asset storage asset) private {
        if (asset.assetType == AssetType.ETH) {
            (bool success, ) = to.call{value: asset.amount}("");
            require(success, "ETH transfer failed");
        } else if (asset.assetType == AssetType.ERC20) {
            IERC20(asset.contractAddress).safeTransfer(to, asset.amount);
        } else if (asset.assetType == AssetType.ERC721) {
            IERC721(asset.contractAddress).transferFrom(address(this), to, asset.tokenId);
        } else if (asset.assetType == AssetType.ERC1155) {
            IERC1155(asset.contractAddress).safeTransferFrom(
                address(this),
                to,
                asset.tokenId,
                asset.amount,
                ""
            );
        }
    }

    function _refundAssets(address to, Asset[] storage assets) private {
        for (uint256 i = 0; i < assets.length; i++) {
            _transferAsset(to, assets[i]);
        }
    }

    // ============ Receiver Functions ============

    receive() external payable {}

    function onERC721Received(
        address,
        address,
        uint256,
        bytes calldata
    ) external pure returns (bytes4) {
        return this.onERC721Received.selector;
    }

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
