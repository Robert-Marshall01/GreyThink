// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

/**
 * @title IVault
 * @notice Interface for token vault contracts
 * @dev Manages deposits and withdrawals of ERC20 tokens
 */
interface IVault {
    /**
     * @notice Deposit info structure
     */
    struct DepositInfo {
        uint256 amount;
        uint256 depositTime;
        uint256 lastInteractionTime;
    }

    /**
     * @notice Deposits tokens into the vault
     * @param token The token address
     * @param amount The amount to deposit
     */
    function deposit(address token, uint256 amount) external;

    /**
     * @notice Deposits tokens with permit
     * @param token The token address
     * @param amount The amount to deposit
     * @param deadline The permit deadline
     * @param v Signature v
     * @param r Signature r
     * @param s Signature s
     */
    function depositWithPermit(
        address token,
        uint256 amount,
        uint256 deadline,
        uint8 v,
        bytes32 r,
        bytes32 s
    ) external;

    /**
     * @notice Withdraws tokens from the vault
     * @param token The token address
     * @param amount The amount to withdraw
     */
    function withdraw(address token, uint256 amount) external;

    /**
     * @notice Withdraws all tokens of a type
     * @param token The token address
     */
    function withdrawAll(address token) external;

    /**
     * @notice Emergency withdraws all tokens
     */
    function emergencyWithdrawAll() external;

    /**
     * @notice Gets user balance for a token
     * @param token The token address
     * @param user The user address
     * @return The balance
     */
    function balanceOf(address token, address user) external view returns (uint256);

    /**
     * @notice Gets user deposit info for a token
     * @param token The token address
     * @param user The user address
     * @return The deposit info
     */
    function getDepositInfo(address token, address user) external view returns (DepositInfo memory);

    /**
     * @notice Gets total deposited for a token
     * @param token The token address
     * @return The total deposited
     */
    function totalDeposited(address token) external view returns (uint256);

    /**
     * @notice Gets vault balance for a token
     * @param token The token address
     * @return The vault balance
     */
    function vaultBalance(address token) external view returns (uint256);

    /**
     * @notice Gets all tokens deposited by a user
     * @param user The user address
     * @return Array of token addresses
     */
    function getUserTokens(address user) external view returns (address[] memory);

    /**
     * @notice Gets all supported tokens
     * @return Array of token addresses
     */
    function getSupportedTokens() external view returns (address[] memory);

    /**
     * @notice Checks if a token is supported
     * @param token The token address
     * @return True if supported
     */
    function isTokenSupported(address token) external view returns (bool);

    /**
     * @notice Adds support for a token
     * @param token The token address
     */
    function addSupportedToken(address token) external;

    /**
     * @notice Removes support for a token
     * @param token The token address
     */
    function removeSupportedToken(address token) external;

    /**
     * @notice Sweeps stuck tokens to admin
     * @param token The token address
     * @param recipient The recipient address
     */
    function sweep(address token, address recipient) external;

    /**
     * @notice Pauses the vault
     */
    function pause() external;

    /**
     * @notice Unpauses the vault
     */
    function unpause() external;

    // Events
    event Deposited(address indexed token, address indexed user, uint256 amount);
    event Withdrawn(address indexed token, address indexed user, uint256 amount);
    event EmergencyWithdraw(address indexed user, address[] tokens, uint256[] amounts);
    event TokenAdded(address indexed token);
    event TokenRemoved(address indexed token);
    event Swept(address indexed token, address indexed recipient, uint256 amount);
}
