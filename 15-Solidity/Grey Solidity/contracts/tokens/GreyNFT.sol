// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/token/ERC721/ERC721.sol";
import "@openzeppelin/contracts/token/ERC721/extensions/ERC721Enumerable.sol";
import "@openzeppelin/contracts/token/ERC721/extensions/ERC721URIStorage.sol";
import "@openzeppelin/contracts/token/ERC721/extensions/ERC721Pausable.sol";
import "@openzeppelin/contracts/token/ERC721/extensions/ERC721Royalty.sol";
import "@openzeppelin/contracts/token/ERC721/extensions/ERC721Burnable.sol";
import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";

/**
 * @title GreyNFT
 * @author Grey Solidity Project
 * @notice Full-featured ERC721 NFT with royalties and batch operations
 * @dev Implements EIP-2981 royalties, enumerable, pausable, burnable
 */
contract GreyNFT is 
    ERC721,
    ERC721Enumerable,
    ERC721URIStorage,
    ERC721Pausable,
    ERC721Royalty,
    ERC721Burnable,
    AccessControl,
    ReentrancyGuard 
{
    /// @notice Role for minting NFTs
    bytes32 public constant MINTER_ROLE = keccak256("MINTER_ROLE");
    
    /// @notice Role for pausing operations
    bytes32 public constant PAUSER_ROLE = keccak256("PAUSER_ROLE");
    
    /// @notice Role for royalty management
    bytes32 public constant ROYALTY_ADMIN_ROLE = keccak256("ROYALTY_ADMIN_ROLE");

    /// @notice Maximum supply (0 = unlimited)
    uint256 public immutable maxSupply;

    /// @notice Base URI for metadata
    string private _baseTokenURI;

    /// @notice Contract-level metadata URI
    string public contractURI;

    /// @notice Token ID counter
    uint256 private _tokenIdCounter;

    /// @notice Maximum batch size
    uint256 public constant MAX_BATCH_SIZE = 100;

    /// @notice Mapping from token ID to creation timestamp
    mapping(uint256 => uint256) private _tokenCreationTime;

    /// @notice Mapping from token ID to creator
    mapping(uint256 => address) private _tokenCreators;

    /// @notice Emitted when base URI is updated
    event BaseURIUpdated(string oldURI, string newURI);

    /// @notice Emitted when contract URI is updated
    event ContractURIUpdated(string oldURI, string newURI);

    /// @notice Emitted when tokens are batch minted
    event BatchMinted(address indexed to, uint256[] tokenIds);

    /// @notice Emitted when default royalty is set
    event DefaultRoyaltySet(address indexed receiver, uint96 feeNumerator);

    /// @notice Emitted when token royalty is set
    event TokenRoyaltySet(uint256 indexed tokenId, address indexed receiver, uint96 feeNumerator);

    /// @notice Error for max supply exceeded
    error MaxSupplyExceeded();

    /// @notice Error for batch size exceeded
    error BatchSizeExceeded();

    /// @notice Error for invalid token
    error InvalidToken();

    /**
     * @notice Initializes the NFT collection
     * @param name_ The collection name
     * @param symbol_ The collection symbol
     * @param baseURI_ The base URI for metadata
     * @param maxSupply_ The maximum supply (0 = unlimited)
     * @param royaltyReceiver The default royalty receiver
     * @param royaltyFee The default royalty fee in basis points
     */
    constructor(
        string memory name_,
        string memory symbol_,
        string memory baseURI_,
        uint256 maxSupply_,
        address royaltyReceiver,
        uint96 royaltyFee
    ) ERC721(name_, symbol_) {
        maxSupply = maxSupply_;
        _baseTokenURI = baseURI_;

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(MINTER_ROLE, msg.sender);
        _grantRole(PAUSER_ROLE, msg.sender);
        _grantRole(ROYALTY_ADMIN_ROLE, msg.sender);

        if (royaltyReceiver != address(0)) {
            _setDefaultRoyalty(royaltyReceiver, royaltyFee);
            emit DefaultRoyaltySet(royaltyReceiver, royaltyFee);
        }
    }

    // ============ Minting ============

    /**
     * @notice Mints a new NFT
     * @param to The recipient address
     * @return The new token ID
     */
    function mint(address to) external onlyRole(MINTER_ROLE) nonReentrant returns (uint256) {
        return _mintSingle(to, "");
    }

    /**
     * @notice Mints a new NFT with custom URI
     * @param to The recipient address
     * @param tokenURI_ The token URI
     * @return The new token ID
     */
    function mintWithURI(
        address to,
        string calldata tokenURI_
    ) external onlyRole(MINTER_ROLE) nonReentrant returns (uint256) {
        return _mintSingle(to, tokenURI_);
    }

    /**
     * @notice Safe mints a new NFT
     * @param to The recipient address
     * @param data Additional data for receiver
     * @return The new token ID
     */
    function safeMint(
        address to,
        bytes calldata data
    ) external onlyRole(MINTER_ROLE) nonReentrant returns (uint256) {
        // Use _mintSingle which handles all safety checks
        return _mintSingle(to, "");
    }

    /**
     * @notice Batch mints multiple NFTs
     * @param to The recipient address
     * @param quantity The number to mint
     * @return tokenIds The minted token IDs
     */
    function batchMint(
        address to,
        uint256 quantity
    ) external onlyRole(MINTER_ROLE) nonReentrant returns (uint256[] memory tokenIds) {
        if (quantity > MAX_BATCH_SIZE) {
            revert BatchSizeExceeded();
        }
        if (maxSupply > 0 && _tokenIdCounter + quantity > maxSupply) {
            revert MaxSupplyExceeded();
        }

        tokenIds = new uint256[](quantity);

        for (uint256 i = 0; i < quantity; i++) {
            _tokenIdCounter++;
            uint256 tokenId = _tokenIdCounter;
            
            _safeMint(to, tokenId);
            _tokenCreationTime[tokenId] = block.timestamp;
            _tokenCreators[tokenId] = msg.sender;
            
            tokenIds[i] = tokenId;
        }

        emit BatchMinted(to, tokenIds);
    }

    /**
     * @notice Batch mints with custom URIs
     * @param to The recipient address
     * @param tokenURIs Array of token URIs
     * @return tokenIds The minted token IDs
     */
    function batchMintWithURIs(
        address to,
        string[] calldata tokenURIs
    ) external onlyRole(MINTER_ROLE) nonReentrant returns (uint256[] memory tokenIds) {
        uint256 quantity = tokenURIs.length;
        
        if (quantity > MAX_BATCH_SIZE) {
            revert BatchSizeExceeded();
        }
        if (maxSupply > 0 && _tokenIdCounter + quantity > maxSupply) {
            revert MaxSupplyExceeded();
        }

        tokenIds = new uint256[](quantity);

        for (uint256 i = 0; i < quantity; i++) {
            _tokenIdCounter++;
            uint256 tokenId = _tokenIdCounter;
            
            _safeMint(to, tokenId);
            if (bytes(tokenURIs[i]).length > 0) {
                _setTokenURI(tokenId, tokenURIs[i]);
            }
            _tokenCreationTime[tokenId] = block.timestamp;
            _tokenCreators[tokenId] = msg.sender;
            
            tokenIds[i] = tokenId;
        }

        emit BatchMinted(to, tokenIds);
    }

    // ============ URI Management ============

    /**
     * @notice Sets the base URI
     * @param baseURI_ The new base URI
     */
    function setBaseURI(string calldata baseURI_) external onlyRole(DEFAULT_ADMIN_ROLE) {
        string memory oldURI = _baseTokenURI;
        _baseTokenURI = baseURI_;
        emit BaseURIUpdated(oldURI, baseURI_);
    }

    /**
     * @notice Sets the token URI for a specific token
     * @param tokenId The token ID
     * @param tokenURI_ The new token URI
     */
    function setTokenURI(
        uint256 tokenId,
        string calldata tokenURI_
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (_ownerOf(tokenId) == address(0)) {
            revert InvalidToken();
        }
        _setTokenURI(tokenId, tokenURI_);
    }

    /**
     * @notice Sets the contract URI
     * @param contractURI_ The new contract URI
     */
    function setContractURI(string calldata contractURI_) external onlyRole(DEFAULT_ADMIN_ROLE) {
        string memory oldURI = contractURI;
        contractURI = contractURI_;
        emit ContractURIUpdated(oldURI, contractURI_);
    }

    // ============ Royalties ============

    /**
     * @notice Sets the default royalty
     * @param receiver The royalty receiver
     * @param feeNumerator The fee in basis points
     */
    function setDefaultRoyalty(
        address receiver,
        uint96 feeNumerator
    ) external onlyRole(ROYALTY_ADMIN_ROLE) {
        _setDefaultRoyalty(receiver, feeNumerator);
        emit DefaultRoyaltySet(receiver, feeNumerator);
    }

    /**
     * @notice Sets royalty for a specific token
     * @param tokenId The token ID
     * @param receiver The royalty receiver
     * @param feeNumerator The fee in basis points
     */
    function setTokenRoyalty(
        uint256 tokenId,
        address receiver,
        uint96 feeNumerator
    ) external onlyRole(ROYALTY_ADMIN_ROLE) {
        _setTokenRoyalty(tokenId, receiver, feeNumerator);
        emit TokenRoyaltySet(tokenId, receiver, feeNumerator);
    }

    /**
     * @notice Deletes default royalty
     */
    function deleteDefaultRoyalty() external onlyRole(ROYALTY_ADMIN_ROLE) {
        _deleteDefaultRoyalty();
    }

    /**
     * @notice Resets token royalty to default
     * @param tokenId The token ID
     */
    function resetTokenRoyalty(uint256 tokenId) external onlyRole(ROYALTY_ADMIN_ROLE) {
        _resetTokenRoyalty(tokenId);
    }

    // ============ Pausing ============

    /**
     * @notice Pauses all transfers
     */
    function pause() external onlyRole(PAUSER_ROLE) {
        _pause();
    }

    /**
     * @notice Unpauses transfers
     */
    function unpause() external onlyRole(PAUSER_ROLE) {
        _unpause();
    }

    // ============ View Functions ============

    /**
     * @notice Returns the base URI
     * @return The base URI
     */
    function baseURI() external view returns (string memory) {
        return _baseTokenURI;
    }

    /**
     * @notice Returns the current token ID counter
     * @return The counter value
     */
    function currentTokenId() external view returns (uint256) {
        return _tokenIdCounter;
    }

    /**
     * @notice Returns remaining mintable supply
     * @return The remaining supply
     */
    function remainingSupply() external view returns (uint256) {
        if (maxSupply == 0) {
            return type(uint256).max;
        }
        return maxSupply - _tokenIdCounter;
    }

    /**
     * @notice Returns token creation time
     * @param tokenId The token ID
     * @return The creation timestamp
     */
    function getTokenCreationTime(uint256 tokenId) external view returns (uint256) {
        return _tokenCreationTime[tokenId];
    }

    /**
     * @notice Returns token creator
     * @param tokenId The token ID
     * @return The creator address
     */
    function getTokenCreator(uint256 tokenId) external view returns (address) {
        return _tokenCreators[tokenId];
    }

    /**
     * @notice Returns all tokens owned by an address
     * @param owner The owner address
     * @return Array of token IDs
     */
    function tokensOfOwner(address owner) external view returns (uint256[] memory) {
        uint256 balance = balanceOf(owner);
        uint256[] memory tokens = new uint256[](balance);
        
        for (uint256 i = 0; i < balance; i++) {
            tokens[i] = tokenOfOwnerByIndex(owner, i);
        }
        
        return tokens;
    }

    // ============ Internal Functions ============

    /**
     * @dev Internal mint function
     */
    function _mintSingle(address to, string memory tokenURI_) private returns (uint256) {
        if (maxSupply > 0 && _tokenIdCounter >= maxSupply) {
            revert MaxSupplyExceeded();
        }

        _tokenIdCounter++;
        uint256 tokenId = _tokenIdCounter;

        _safeMint(to, tokenId);
        
        if (bytes(tokenURI_).length > 0) {
            _setTokenURI(tokenId, tokenURI_);
        }

        _tokenCreationTime[tokenId] = block.timestamp;
        _tokenCreators[tokenId] = msg.sender;

        return tokenId;
    }

    /**
     * @dev Base URI override
     */
    function _baseURI() internal view override returns (string memory) {
        return _baseTokenURI;
    }

    /**
     * @dev Update hook override
     */
    function _update(
        address to,
        uint256 tokenId,
        address auth
    ) internal override(ERC721, ERC721Enumerable, ERC721Pausable) returns (address) {
        return super._update(to, tokenId, auth);
    }

    /**
     * @dev Increase balance override
     */
    function _increaseBalance(
        address account,
        uint128 value
    ) internal override(ERC721, ERC721Enumerable) {
        super._increaseBalance(account, value);
    }

    /**
     * @dev Token URI override
     */
    function tokenURI(
        uint256 tokenId
    ) public view override(ERC721, ERC721URIStorage) returns (string memory) {
        return super.tokenURI(tokenId);
    }

    /**
     * @dev Supports interface override
     */
    function supportsInterface(
        bytes4 interfaceId
    ) public view override(ERC721, ERC721Enumerable, ERC721URIStorage, ERC721Royalty, AccessControl) returns (bool) {
        return super.supportsInterface(interfaceId);
    }
}
