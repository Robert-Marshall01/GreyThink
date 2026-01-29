// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

/**
 * @title ITreasury
 * @notice Interface for treasury contracts
 * @dev Manages protocol funds and withdrawals
 */
interface ITreasury {
    /**
     * @notice Transaction structure
     */
    struct Transaction {
        uint256 txId;
        address token;
        address recipient;
        uint256 amount;
        string reason;
        uint256 timestamp;
        address executor;
    }

    /**
     * @notice Deposits ETH into the treasury
     */
    function depositETH() external payable;

    /**
     * @notice Deposits ERC20 tokens into the treasury
     * @param token The token address
     * @param amount The amount to deposit
     */
    function depositToken(address token, uint256 amount) external;

    /**
     * @notice Withdraws ETH from the treasury
     * @param recipient The recipient address
     * @param amount The amount to withdraw
     * @param reason The withdrawal reason
     * @return The transaction ID
     */
    function withdrawETH(
        address payable recipient,
        uint256 amount,
        string calldata reason
    ) external returns (uint256);

    /**
     * @notice Withdraws ERC20 tokens from the treasury
     * @param token The token address
     * @param recipient The recipient address
     * @param amount The amount to withdraw
     * @param reason The withdrawal reason
     * @return The transaction ID
     */
    function withdrawToken(
        address token,
        address recipient,
        uint256 amount,
        string calldata reason
    ) external returns (uint256);

    /**
     * @notice Returns the ETH balance
     * @return The ETH balance
     */
    function ethBalance() external view returns (uint256);

    /**
     * @notice Returns the token balance
     * @param token The token address
     * @return The token balance
     */
    function tokenBalance(address token) external view returns (uint256);

    /**
     * @notice Returns all supported tokens
     * @return Array of token addresses
     */
    function getSupportedTokens() external view returns (address[] memory);

    /**
     * @notice Gets transaction by ID
     * @param txId The transaction ID
     * @return The transaction
     */
    function getTransaction(uint256 txId) external view returns (Transaction memory);

    /**
     * @notice Gets all transactions
     * @return Array of transactions
     */
    function getAllTransactions() external view returns (Transaction[] memory);

    /**
     * @notice Gets transactions by recipient
     * @param recipient The recipient address
     * @return Array of transaction IDs
     */
    function getTransactionsByRecipient(address recipient) external view returns (uint256[] memory);

    /**
     * @notice Sets spending limit for a token
     * @param token The token address (address(0) for ETH)
     * @param limit The daily spending limit
     */
    function setSpendingLimit(address token, uint256 limit) external;

    /**
     * @notice Gets spending limit for a token
     * @param token The token address
     * @return The spending limit
     */
    function getSpendingLimit(address token) external view returns (uint256);

    /**
     * @notice Gets amount spent today for a token
     * @param token The token address
     * @return The spent amount
     */
    function getSpentToday(address token) external view returns (uint256);

    /**
     * @notice Pauses the treasury
     */
    function pause() external;

    /**
     * @notice Unpauses the treasury
     */
    function unpause() external;

    // Events
    event ETHDeposited(address indexed sender, uint256 amount);
    event TokenDeposited(address indexed token, address indexed sender, uint256 amount);
    event ETHWithdrawn(uint256 indexed txId, address indexed recipient, uint256 amount, string reason);
    event TokenWithdrawn(uint256 indexed txId, address indexed token, address indexed recipient, uint256 amount, string reason);
    event SpendingLimitUpdated(address indexed token, uint256 oldLimit, uint256 newLimit);
}
