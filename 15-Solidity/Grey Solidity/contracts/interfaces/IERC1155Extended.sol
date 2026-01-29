// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/token/ERC1155/IERC1155.sol";

/**
 * @title IERC1155Extended
 * @notice Extended ERC1155 interface with additional functionality
 * @dev Includes supply tracking, minting, and burning
 */
interface IERC1155Extended is IERC1155 {
    /**
     * @notice Returns the URI for a token type
     * @param id The token type ID
     * @return The metadata URI
     */
    function uri(uint256 id) external view returns (string memory);

    /**
     * @notice Sets the URI for a token type
     * @param id The token type ID
     * @param newUri The new URI
     */
    function setURI(uint256 id, string calldata newUri) external;

    /**
     * @notice Sets the base URI for all tokens
     * @param newBaseUri The new base URI
     */
    function setBaseURI(string calldata newBaseUri) external;

    /**
     * @notice Mints tokens to an address
     * @param to The recipient address
     * @param id The token type ID
     * @param amount The amount to mint
     * @param data Additional data
     */
    function mint(address to, uint256 id, uint256 amount, bytes calldata data) external;

    /**
     * @notice Batch mints multiple token types
     * @param to The recipient address
     * @param ids Array of token type IDs
     * @param amounts Array of amounts
     * @param data Additional data
     */
    function mintBatch(
        address to,
        uint256[] calldata ids,
        uint256[] calldata amounts,
        bytes calldata data
    ) external;

    /**
     * @notice Burns tokens from an address
     * @param from The address to burn from
     * @param id The token type ID
     * @param amount The amount to burn
     */
    function burn(address from, uint256 id, uint256 amount) external;

    /**
     * @notice Batch burns multiple token types
     * @param from The address to burn from
     * @param ids Array of token type IDs
     * @param amounts Array of amounts
     */
    function burnBatch(address from, uint256[] calldata ids, uint256[] calldata amounts) external;

    /**
     * @notice Returns the total supply of a token type
     * @param id The token type ID
     * @return The total supply
     */
    function totalSupply(uint256 id) external view returns (uint256);

    /**
     * @notice Checks if a token type exists
     * @param id The token type ID
     * @return True if the token type exists
     */
    function exists(uint256 id) external view returns (bool);

    /**
     * @notice Returns the max supply for a token type
     * @param id The token type ID
     * @return The max supply (0 = unlimited)
     */
    function maxSupply(uint256 id) external view returns (uint256);

    /**
     * @notice Sets the max supply for a token type
     * @param id The token type ID
     * @param supply The max supply
     */
    function setMaxSupply(uint256 id, uint256 supply) external;

    /**
     * @notice Creates a new token type
     * @param id The token type ID
     * @param initialSupply The initial supply to mint
     * @param maxSupply_ The max supply (0 = unlimited)
     * @param tokenUri The token URI
     * @param data Additional data
     */
    function create(
        uint256 id,
        uint256 initialSupply,
        uint256 maxSupply_,
        string calldata tokenUri,
        bytes calldata data
    ) external;

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

    // Events
    event TokenCreated(uint256 indexed id, uint256 initialSupply, uint256 maxSupply, string uri);
    event MaxSupplyUpdated(uint256 indexed id, uint256 oldMaxSupply, uint256 newMaxSupply);
    event URIUpdated(uint256 indexed id, string oldUri, string newUri);
}
