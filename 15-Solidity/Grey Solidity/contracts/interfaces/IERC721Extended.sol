// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/token/ERC721/IERC721.sol";
import "@openzeppelin/contracts/interfaces/IERC2981.sol";

/**
 * @title IERC721Extended
 * @notice Extended ERC721 interface with royalties and batch operations
 * @dev Includes EIP-2981 royalty support and batch minting
 */
interface IERC721Extended is IERC721, IERC2981 {
    /**
     * @notice Returns the name of the token collection
     * @return The collection name
     */
    function name() external view returns (string memory);

    /**
     * @notice Returns the symbol of the token collection
     * @return The collection symbol
     */
    function symbol() external view returns (string memory);

    /**
     * @notice Returns the URI for a token's metadata
     * @param tokenId The token ID
     * @return The metadata URI
     */
    function tokenURI(uint256 tokenId) external view returns (string memory);

    /**
     * @notice Returns the base URI for all tokens
     * @return The base URI
     */
    function baseURI() external view returns (string memory);

    /**
     * @notice Mints a new token to an address
     * @param to The recipient address
     * @param tokenId The token ID to mint
     */
    function mint(address to, uint256 tokenId) external;

    /**
     * @notice Safely mints a new token with data
     * @param to The recipient address
     * @param tokenId The token ID to mint
     * @param data Additional data to pass to receiver
     */
    function safeMint(address to, uint256 tokenId, bytes calldata data) external;

    /**
     * @notice Batch mints multiple tokens
     * @param to The recipient address
     * @param tokenIds Array of token IDs to mint
     */
    function batchMint(address to, uint256[] calldata tokenIds) external;

    /**
     * @notice Burns a token
     * @param tokenId The token ID to burn
     */
    function burn(uint256 tokenId) external;

    /**
     * @notice Sets the base URI for token metadata
     * @param baseURI_ The new base URI
     */
    function setBaseURI(string calldata baseURI_) external;

    /**
     * @notice Sets the URI for a specific token
     * @param tokenId The token ID
     * @param tokenURI_ The token URI
     */
    function setTokenURI(uint256 tokenId, string calldata tokenURI_) external;

    /**
     * @notice Sets the default royalty info
     * @param receiver The royalty recipient
     * @param feeNumerator The royalty fee (basis points)
     */
    function setDefaultRoyalty(address receiver, uint96 feeNumerator) external;

    /**
     * @notice Sets royalty info for a specific token
     * @param tokenId The token ID
     * @param receiver The royalty recipient
     * @param feeNumerator The royalty fee (basis points)
     */
    function setTokenRoyalty(uint256 tokenId, address receiver, uint96 feeNumerator) external;

    /**
     * @notice Returns the total number of tokens minted
     * @return The total supply
     */
    function totalSupply() external view returns (uint256);

    /**
     * @notice Returns the token ID at a given index
     * @param index The index
     * @return The token ID
     */
    function tokenByIndex(uint256 index) external view returns (uint256);

    /**
     * @notice Returns the token ID at a given index for an owner
     * @param owner The owner address
     * @param index The index
     * @return The token ID
     */
    function tokenOfOwnerByIndex(address owner, uint256 index) external view returns (uint256);

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
    event BaseURIUpdated(string oldBaseURI, string newBaseURI);
    event TokenURIUpdated(uint256 indexed tokenId, string tokenURI);
    event RoyaltyUpdated(uint256 indexed tokenId, address receiver, uint96 feeNumerator);
    event BatchMinted(address indexed to, uint256[] tokenIds);
}
