// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/token/ERC20/IERC20.sol";

/**
 * @title IERC20Extended
 * @notice Extended ERC20 interface with additional functionality
 * @dev Includes permit, mint, burn, pause, and snapshot capabilities
 */
interface IERC20Extended is IERC20 {
    /**
     * @notice Returns the number of decimals used for token amounts
     * @return The number of decimals
     */
    function decimals() external view returns (uint8);

    /**
     * @notice Returns the name of the token
     * @return The token name
     */
    function name() external view returns (string memory);

    /**
     * @notice Returns the symbol of the token
     * @return The token symbol
     */
    function symbol() external view returns (string memory);

    /**
     * @notice Mints new tokens to an address
     * @param to The recipient address
     * @param amount The amount to mint
     */
    function mint(address to, uint256 amount) external;

    /**
     * @notice Burns tokens from an address
     * @param from The address to burn from
     * @param amount The amount to burn
     */
    function burn(address from, uint256 amount) external;

    /**
     * @notice Pauses all token transfers
     */
    function pause() external;

    /**
     * @notice Unpauses token transfers
     */
    function unpause() external;

    /**
     * @notice Returns whether the contract is paused
     * @return True if paused
     */
    function paused() external view returns (bool);

    /**
     * @notice Creates a new snapshot and returns its ID
     * @return The snapshot ID
     */
    function snapshot() external returns (uint256);

    /**
     * @notice Returns the balance at a specific snapshot
     * @param account The account to query
     * @param snapshotId The snapshot ID
     * @return The balance at that snapshot
     */
    function balanceOfAt(address account, uint256 snapshotId) external view returns (uint256);

    /**
     * @notice Returns the total supply at a specific snapshot
     * @param snapshotId The snapshot ID
     * @return The total supply at that snapshot
     */
    function totalSupplyAt(uint256 snapshotId) external view returns (uint256);

    /**
     * @notice EIP-2612 permit function
     * @param owner The token owner
     * @param spender The spender address
     * @param value The allowance value
     * @param deadline The permit deadline
     * @param v Signature v
     * @param r Signature r
     * @param s Signature s
     */
    function permit(
        address owner,
        address spender,
        uint256 value,
        uint256 deadline,
        uint8 v,
        bytes32 r,
        bytes32 s
    ) external;

    /**
     * @notice Returns the current nonce for an address
     * @param owner The address to query
     * @return The current nonce
     */
    function nonces(address owner) external view returns (uint256);

    /**
     * @notice Returns the domain separator
     * @return The domain separator hash
     */
    function DOMAIN_SEPARATOR() external view returns (bytes32);

    // Events
    event Snapshot(uint256 id);
    event Paused(address account);
    event Unpaused(address account);
}
