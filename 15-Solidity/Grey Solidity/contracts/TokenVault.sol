// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "./AccessControlManager.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";

/**
 * @title TokenVault
 * @author Grey Solidity Project
 * @notice A secure vault for holding and managing ERC20 tokens
 * @dev Integrates with AccessControlManager for permission control
 */
contract TokenVault is ReentrancyGuard {
    using SafeERC20 for IERC20;

    /// @notice Reference to the access control contract
    AccessControlManager public immutable accessControl;

    /// @notice Mapping of token address => user address => balance
    mapping(address => mapping(address => uint256)) private _balances;

    /// @notice Mapping of token address => total deposited
    mapping(address => uint256) private _totalDeposited;

    /// @notice Array of all tokens that have been deposited
    address[] private _supportedTokens;

    /// @notice Mapping to track if a token has been added to supportedTokens
    mapping(address => bool) private _tokenExists;

    /// @notice Emitted when tokens are deposited
    /// @param token The token address
    /// @param depositor The address that deposited
    /// @param amount The amount deposited
    event Deposited(address indexed token, address indexed depositor, uint256 amount);

    /// @notice Emitted when tokens are withdrawn
    /// @param token The token address
    /// @param recipient The address that received the tokens
    /// @param amount The amount withdrawn
    event Withdrawn(address indexed token, address indexed recipient, uint256 amount);

    /// @notice Emitted when an admin sweeps tokens
    /// @param token The token address
    /// @param recipient The address that received the tokens
    /// @param amount The amount swept
    event Swept(address indexed token, address indexed recipient, uint256 amount);

    /// @notice Error thrown when deposit amount is zero
    error ZeroAmount();

    /// @notice Error thrown when token address is invalid
    error InvalidToken();

    /// @notice Error thrown when withdrawal exceeds balance
    error InsufficientBalance(uint256 requested, uint256 available);

    /// @notice Error thrown when recipient address is invalid
    error InvalidRecipient();

    /// @notice Restricts access to admins only
    modifier onlyAdmin() {
        require(
            accessControl.hasRole(accessControl.ADMIN_ROLE(), msg.sender),
            "TokenVault: caller is not an admin"
        );
        _;
    }

    /// @notice Restricts access to users with USER_ROLE or ADMIN_ROLE
    modifier onlyUser() {
        bool isUser = accessControl.hasRole(accessControl.USER_ROLE(), msg.sender);
        bool isAdmin = accessControl.hasRole(accessControl.ADMIN_ROLE(), msg.sender);
        require(isUser || isAdmin, "TokenVault: caller is not a user or admin");
        _;
    }

    /**
     * @notice Initializes the TokenVault with an AccessControlManager reference
     * @param _accessControl Address of the AccessControlManager contract
     */
    constructor(address _accessControl) {
        require(_accessControl != address(0), "TokenVault: invalid access control address");
        accessControl = AccessControlManager(_accessControl);
    }

    /**
     * @notice Deposits ERC20 tokens into the vault
     * @dev Caller must have approved this contract to spend the tokens
     * @param token The ERC20 token address
     * @param amount The amount to deposit
     */
    function deposit(address token, uint256 amount) external nonReentrant onlyUser {
        if (token == address(0)) {
            revert InvalidToken();
        }
        if (amount == 0) {
            revert ZeroAmount();
        }

        // Track new tokens
        if (!_tokenExists[token]) {
            _tokenExists[token] = true;
            _supportedTokens.push(token);
        }

        _balances[token][msg.sender] += amount;
        _totalDeposited[token] += amount;

        IERC20(token).safeTransferFrom(msg.sender, address(this), amount);

        emit Deposited(token, msg.sender, amount);
    }

    /**
     * @notice Withdraws ERC20 tokens from the vault
     * @dev Caller can only withdraw their own deposited tokens
     * @param token The ERC20 token address
     * @param amount The amount to withdraw
     */
    function withdraw(address token, uint256 amount) external nonReentrant onlyUser {
        if (token == address(0)) {
            revert InvalidToken();
        }
        if (amount == 0) {
            revert ZeroAmount();
        }

        uint256 userBalance = _balances[token][msg.sender];
        if (amount > userBalance) {
            revert InsufficientBalance(amount, userBalance);
        }

        _balances[token][msg.sender] -= amount;
        _totalDeposited[token] -= amount;

        IERC20(token).safeTransfer(msg.sender, amount);

        emit Withdrawn(token, msg.sender, amount);
    }

    /**
     * @notice Admin function to sweep tokens to a recipient
     * @dev Only admins can sweep, and it transfers the entire contract balance
     * @param token The ERC20 token address
     * @param recipient The address to receive the tokens
     */
    function sweep(address token, address recipient) external nonReentrant onlyAdmin {
        if (token == address(0)) {
            revert InvalidToken();
        }
        if (recipient == address(0)) {
            revert InvalidRecipient();
        }

        uint256 balance = IERC20(token).balanceOf(address(this));
        if (balance == 0) {
            revert ZeroAmount();
        }

        // Reset all user balances for this token (emergency action)
        _totalDeposited[token] = 0;

        IERC20(token).safeTransfer(recipient, balance);

        emit Swept(token, recipient, balance);
    }

    /**
     * @notice Gets the balance of a user for a specific token
     * @param token The ERC20 token address
     * @param user The user address
     * @return The user's balance
     */
    function balanceOf(address token, address user) external view returns (uint256) {
        return _balances[token][user];
    }

    /**
     * @notice Gets the total amount deposited for a specific token
     * @param token The ERC20 token address
     * @return The total deposited amount
     */
    function totalDeposited(address token) external view returns (uint256) {
        return _totalDeposited[token];
    }

    /**
     * @notice Gets the actual token balance held by the vault
     * @param token The ERC20 token address
     * @return The vault's token balance
     */
    function vaultBalance(address token) external view returns (uint256) {
        return IERC20(token).balanceOf(address(this));
    }

    /**
     * @notice Gets all tokens that have been deposited into the vault
     * @return Array of token addresses
     */
    function getSupportedTokens() external view returns (address[] memory) {
        return _supportedTokens;
    }
}
