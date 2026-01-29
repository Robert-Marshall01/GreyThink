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
 * @title Treasury
 * @author Grey Solidity Project
 * @notice Multi-asset treasury for DAO governance
 * @dev Supports ETH, ERC20, ERC721, and ERC1155 with spending limits
 */
contract Treasury is AccessControl, ReentrancyGuard, Pausable {
    using SafeERC20 for IERC20;

    /// @notice Role for executing withdrawals
    bytes32 public constant TREASURER_ROLE = keccak256("TREASURER_ROLE");

    /// @notice Role for emergency operations
    bytes32 public constant EMERGENCY_ROLE = keccak256("EMERGENCY_ROLE");

    /// @notice Spending limit structure
    struct SpendingLimit {
        uint256 maxAmount;
        uint256 spent;
        uint256 resetTime;
        uint256 period;
    }

    /// @notice Asset types
    enum AssetType {
        ETH,
        ERC20,
        ERC721,
        ERC1155
    }

    /// @notice Mapping of token address to spending limit
    mapping(address => SpendingLimit) private _spendingLimits;

    /// @notice ETH spending limit (address(0))
    SpendingLimit private _ethSpendingLimit;

    /// @notice Whitelisted tokens
    mapping(address => bool) public whitelistedTokens;

    /// @notice Approved NFT collections
    mapping(address => bool) public approvedCollections;

    /// @notice Total ETH deposited
    uint256 public totalEthDeposited;

    /// @notice Total ETH withdrawn
    uint256 public totalEthWithdrawn;

    // ============ Events ============

    event EthDeposited(address indexed from, uint256 amount);
    event EthWithdrawn(address indexed to, uint256 amount, string reason);
    event TokenDeposited(address indexed token, address indexed from, uint256 amount);
    event TokenWithdrawn(address indexed token, address indexed to, uint256 amount, string reason);
    event NFTDeposited(address indexed collection, address indexed from, uint256 tokenId);
    event NFTWithdrawn(address indexed collection, address indexed to, uint256 tokenId);
    event ERC1155Deposited(address indexed collection, address indexed from, uint256 id, uint256 amount);
    event ERC1155Withdrawn(address indexed collection, address indexed to, uint256 id, uint256 amount);
    event SpendingLimitSet(address indexed token, uint256 maxAmount, uint256 period);
    event TokenWhitelisted(address indexed token, bool status);
    event CollectionApproved(address indexed collection, bool status);

    // ============ Errors ============

    error InsufficientBalance(uint256 requested, uint256 available);
    error SpendingLimitExceeded(uint256 requested, uint256 remaining);
    error TokenNotWhitelisted(address token);
    error CollectionNotApproved(address collection);
    error ZeroAmount();
    error ZeroAddress();
    error TransferFailed();

    /**
     * @notice Initializes the treasury
     */
    constructor() {
        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(TREASURER_ROLE, msg.sender);
        _grantRole(EMERGENCY_ROLE, msg.sender);
    }

    // ============ Receive ETH ============

    /**
     * @notice Receives ETH deposits
     */
    receive() external payable {
        totalEthDeposited += msg.value;
        emit EthDeposited(msg.sender, msg.value);
    }

    /**
     * @notice Fallback for ETH sent with data
     */
    fallback() external payable {
        totalEthDeposited += msg.value;
        emit EthDeposited(msg.sender, msg.value);
    }

    // ============ ETH Operations ============

    /**
     * @notice Deposits ETH
     */
    function depositEth() external payable {
        if (msg.value == 0) revert ZeroAmount();
        totalEthDeposited += msg.value;
        emit EthDeposited(msg.sender, msg.value);
    }

    /**
     * @notice Withdraws ETH
     * @param to Recipient address
     * @param amount Amount to withdraw
     * @param reason Reason for withdrawal
     */
    function withdrawEth(
        address to,
        uint256 amount,
        string calldata reason
    ) external onlyRole(TREASURER_ROLE) nonReentrant whenNotPaused {
        if (to == address(0)) revert ZeroAddress();
        if (amount == 0) revert ZeroAmount();
        if (address(this).balance < amount) {
            revert InsufficientBalance(amount, address(this).balance);
        }

        _checkAndUpdateSpendingLimit(address(0), amount);

        totalEthWithdrawn += amount;

        (bool success, ) = to.call{value: amount}("");
        if (!success) revert TransferFailed();

        emit EthWithdrawn(to, amount, reason);
    }

    // ============ ERC20 Operations ============

    /**
     * @notice Deposits ERC20 tokens
     * @param token Token address
     * @param amount Amount to deposit
     */
    function depositToken(address token, uint256 amount) external nonReentrant {
        if (token == address(0)) revert ZeroAddress();
        if (amount == 0) revert ZeroAmount();

        IERC20(token).safeTransferFrom(msg.sender, address(this), amount);
        emit TokenDeposited(token, msg.sender, amount);
    }

    /**
     * @notice Withdraws ERC20 tokens
     * @param token Token address
     * @param to Recipient address
     * @param amount Amount to withdraw
     * @param reason Reason for withdrawal
     */
    function withdrawToken(
        address token,
        address to,
        uint256 amount,
        string calldata reason
    ) external onlyRole(TREASURER_ROLE) nonReentrant whenNotPaused {
        if (token == address(0)) revert ZeroAddress();
        if (to == address(0)) revert ZeroAddress();
        if (amount == 0) revert ZeroAmount();
        if (!whitelistedTokens[token]) revert TokenNotWhitelisted(token);

        uint256 balance = IERC20(token).balanceOf(address(this));
        if (balance < amount) {
            revert InsufficientBalance(amount, balance);
        }

        _checkAndUpdateSpendingLimit(token, amount);

        IERC20(token).safeTransfer(to, amount);
        emit TokenWithdrawn(token, to, amount, reason);
    }

    // ============ NFT Operations ============

    /**
     * @notice Deposits an NFT
     * @param collection NFT collection address
     * @param tokenId Token ID
     */
    function depositNFT(address collection, uint256 tokenId) external nonReentrant {
        if (collection == address(0)) revert ZeroAddress();
        
        IERC721(collection).transferFrom(msg.sender, address(this), tokenId);
        emit NFTDeposited(collection, msg.sender, tokenId);
    }

    /**
     * @notice Withdraws an NFT
     * @param collection NFT collection address
     * @param to Recipient address
     * @param tokenId Token ID
     */
    function withdrawNFT(
        address collection,
        address to,
        uint256 tokenId
    ) external onlyRole(TREASURER_ROLE) nonReentrant whenNotPaused {
        if (collection == address(0)) revert ZeroAddress();
        if (to == address(0)) revert ZeroAddress();
        if (!approvedCollections[collection]) revert CollectionNotApproved(collection);

        IERC721(collection).transferFrom(address(this), to, tokenId);
        emit NFTWithdrawn(collection, to, tokenId);
    }

    // ============ ERC1155 Operations ============

    /**
     * @notice Deposits ERC1155 tokens
     * @param collection Collection address
     * @param id Token ID
     * @param amount Amount
     * @param data Additional data
     */
    function depositERC1155(
        address collection,
        uint256 id,
        uint256 amount,
        bytes calldata data
    ) external nonReentrant {
        if (collection == address(0)) revert ZeroAddress();
        if (amount == 0) revert ZeroAmount();

        IERC1155(collection).safeTransferFrom(msg.sender, address(this), id, amount, data);
        emit ERC1155Deposited(collection, msg.sender, id, amount);
    }

    /**
     * @notice Withdraws ERC1155 tokens
     * @param collection Collection address
     * @param to Recipient address
     * @param id Token ID
     * @param amount Amount
     * @param data Additional data
     */
    function withdrawERC1155(
        address collection,
        address to,
        uint256 id,
        uint256 amount,
        bytes calldata data
    ) external onlyRole(TREASURER_ROLE) nonReentrant whenNotPaused {
        if (collection == address(0)) revert ZeroAddress();
        if (to == address(0)) revert ZeroAddress();
        if (amount == 0) revert ZeroAmount();
        if (!approvedCollections[collection]) revert CollectionNotApproved(collection);

        IERC1155(collection).safeTransferFrom(address(this), to, id, amount, data);
        emit ERC1155Withdrawn(collection, to, id, amount);
    }

    // ============ Spending Limits ============

    /**
     * @notice Sets spending limit for a token
     * @param token Token address (address(0) for ETH)
     * @param maxAmount Maximum amount per period
     * @param period Period duration in seconds
     */
    function setSpendingLimit(
        address token,
        uint256 maxAmount,
        uint256 period
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (token == address(0)) {
            _ethSpendingLimit = SpendingLimit({
                maxAmount: maxAmount,
                spent: 0,
                resetTime: block.timestamp + period,
                period: period
            });
        } else {
            _spendingLimits[token] = SpendingLimit({
                maxAmount: maxAmount,
                spent: 0,
                resetTime: block.timestamp + period,
                period: period
            });
        }

        emit SpendingLimitSet(token, maxAmount, period);
    }

    /**
     * @notice Gets spending limit for a token
     * @param token Token address
     * @return The spending limit struct
     */
    function getSpendingLimit(address token) external view returns (SpendingLimit memory) {
        if (token == address(0)) {
            return _ethSpendingLimit;
        }
        return _spendingLimits[token];
    }

    /**
     * @notice Gets remaining spending allowance
     * @param token Token address
     * @return The remaining allowance
     */
    function remainingAllowance(address token) public view returns (uint256) {
        SpendingLimit storage limit = token == address(0) ? _ethSpendingLimit : _spendingLimits[token];
        
        if (limit.maxAmount == 0) {
            return type(uint256).max;
        }

        if (block.timestamp >= limit.resetTime) {
            return limit.maxAmount;
        }

        return limit.maxAmount > limit.spent ? limit.maxAmount - limit.spent : 0;
    }

    // ============ Whitelist Management ============

    /**
     * @notice Whitelists a token
     * @param token Token address
     * @param status Whitelist status
     */
    function setTokenWhitelist(
        address token,
        bool status
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        whitelistedTokens[token] = status;
        emit TokenWhitelisted(token, status);
    }

    /**
     * @notice Approves an NFT collection
     * @param collection Collection address
     * @param status Approval status
     */
    function setCollectionApproval(
        address collection,
        bool status
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        approvedCollections[collection] = status;
        emit CollectionApproved(collection, status);
    }

    /**
     * @notice Batch whitelist tokens
     * @param tokens Array of token addresses
     * @param statuses Array of statuses
     */
    function batchSetTokenWhitelist(
        address[] calldata tokens,
        bool[] calldata statuses
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        require(tokens.length == statuses.length, "Treasury: length mismatch");
        
        for (uint256 i = 0; i < tokens.length; i++) {
            whitelistedTokens[tokens[i]] = statuses[i];
            emit TokenWhitelisted(tokens[i], statuses[i]);
        }
    }

    // ============ View Functions ============

    /**
     * @notice Returns ETH balance
     * @return The balance
     */
    function ethBalance() external view returns (uint256) {
        return address(this).balance;
    }

    /**
     * @notice Returns ERC20 balance
     * @param token Token address
     * @return The balance
     */
    function tokenBalance(address token) external view returns (uint256) {
        return IERC20(token).balanceOf(address(this));
    }

    /**
     * @notice Returns NFT ownership
     * @param collection Collection address
     * @param tokenId Token ID
     * @return True if owned
     */
    function ownsNFT(address collection, uint256 tokenId) external view returns (bool) {
        try IERC721(collection).ownerOf(tokenId) returns (address owner) {
            return owner == address(this);
        } catch {
            return false;
        }
    }

    /**
     * @notice Returns ERC1155 balance
     * @param collection Collection address
     * @param id Token ID
     * @return The balance
     */
    function erc1155Balance(address collection, uint256 id) external view returns (uint256) {
        return IERC1155(collection).balanceOf(address(this), id);
    }

    // ============ Emergency Functions ============

    /**
     * @notice Pauses the treasury
     */
    function pause() external onlyRole(EMERGENCY_ROLE) {
        _pause();
    }

    /**
     * @notice Unpauses the treasury
     */
    function unpause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        _unpause();
    }

    /**
     * @notice Emergency rescue of stuck tokens
     * @param token Token address
     * @param to Recipient
     * @param amount Amount
     */
    function emergencyRescue(
        address token,
        address to,
        uint256 amount
    ) external onlyRole(EMERGENCY_ROLE) nonReentrant {
        if (token == address(0)) {
            (bool success, ) = to.call{value: amount}("");
            require(success, "Treasury: ETH transfer failed");
        } else {
            IERC20(token).safeTransfer(to, amount);
        }
    }

    // ============ Internal Functions ============

    /**
     * @dev Checks and updates spending limit
     */
    function _checkAndUpdateSpendingLimit(address token, uint256 amount) private {
        SpendingLimit storage limit = token == address(0) ? _ethSpendingLimit : _spendingLimits[token];

        // No limit set
        if (limit.maxAmount == 0) {
            return;
        }

        // Reset if period expired
        if (block.timestamp >= limit.resetTime) {
            limit.spent = 0;
            limit.resetTime = block.timestamp + limit.period;
        }

        uint256 remaining = limit.maxAmount - limit.spent;
        if (amount > remaining) {
            revert SpendingLimitExceeded(amount, remaining);
        }

        limit.spent += amount;
    }

    // ============ ERC721/1155 Receiver ============

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

    function supportsInterface(bytes4 interfaceId) public view override returns (bool) {
        return
            interfaceId == type(IERC721).interfaceId ||
            interfaceId == type(IERC1155).interfaceId ||
            super.supportsInterface(interfaceId);
    }
}
