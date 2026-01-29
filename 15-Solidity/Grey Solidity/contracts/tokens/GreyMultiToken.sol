// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/token/ERC1155/ERC1155.sol";
import "@openzeppelin/contracts/token/ERC1155/extensions/ERC1155Pausable.sol";
import "@openzeppelin/contracts/token/ERC1155/extensions/ERC1155Burnable.sol";
import "@openzeppelin/contracts/token/ERC1155/extensions/ERC1155Supply.sol";
import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/Strings.sol";

/**
 * @title GreyMultiToken
 * @author Grey Solidity Project
 * @notice Full-featured ERC1155 multi-token with supply tracking
 * @dev Implements pausable, burnable, supply tracking, and custom URIs
 */
contract GreyMultiToken is 
    ERC1155,
    ERC1155Pausable,
    ERC1155Burnable,
    ERC1155Supply,
    AccessControl,
    ReentrancyGuard 
{
    using Strings for uint256;

    /// @notice Role for minting tokens
    bytes32 public constant MINTER_ROLE = keccak256("MINTER_ROLE");
    
    /// @notice Role for pausing operations
    bytes32 public constant PAUSER_ROLE = keccak256("PAUSER_ROLE");
    
    /// @notice Role for URI management
    bytes32 public constant URI_SETTER_ROLE = keccak256("URI_SETTER_ROLE");

    /// @notice Token type info structure
    struct TokenType {
        uint256 maxSupply;
        string uri;
        bool created;
        bool transferable;
        uint256 createdAt;
    }

    /// @notice Collection name
    string public name;

    /// @notice Collection symbol
    string public symbol;

    /// @notice Base URI for tokens without custom URI
    string private _baseURI;

    /// @notice Mapping from token ID to token type info
    mapping(uint256 => TokenType) private _tokenTypes;

    /// @notice Array of all token type IDs
    uint256[] private _allTokenIds;

    /// @notice Contract-level metadata URI
    string public contractURI;

    /// @notice Emitted when a token type is created
    event TokenTypeCreated(uint256 indexed id, uint256 maxSupply, string uri, bool transferable);

    /// @notice Emitted when token URI is updated
    event URIUpdated(uint256 indexed id, string oldUri, string newUri);

    /// @notice Emitted when base URI is updated
    event BaseURIUpdated(string oldUri, string newUri);

    /// @notice Emitted when max supply is updated
    event MaxSupplyUpdated(uint256 indexed id, uint256 oldMaxSupply, uint256 newMaxSupply);

    /// @notice Emitted when transferability is updated
    event TransferabilityUpdated(uint256 indexed id, bool transferable);

    /// @notice Error for token not existing
    error TokenDoesNotExist(uint256 id);

    /// @notice Error for token already existing
    error TokenAlreadyExists(uint256 id);

    /// @notice Error for max supply exceeded
    error MaxSupplyExceeded(uint256 id, uint256 requested, uint256 available);

    /// @notice Error for non-transferable token
    error TokenNotTransferable(uint256 id);

    /**
     * @notice Initializes the multi-token collection
     * @param name_ The collection name
     * @param symbol_ The collection symbol
     * @param baseURI_ The base URI for metadata
     */
    constructor(
        string memory name_,
        string memory symbol_,
        string memory baseURI_
    ) ERC1155(baseURI_) {
        name = name_;
        symbol = symbol_;
        _baseURI = baseURI_;

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(MINTER_ROLE, msg.sender);
        _grantRole(PAUSER_ROLE, msg.sender);
        _grantRole(URI_SETTER_ROLE, msg.sender);
    }

    // ============ Token Type Management ============

    /**
     * @notice Creates a new token type
     * @param id The token type ID
     * @param maxSupply_ The max supply (0 = unlimited)
     * @param tokenUri The token URI (empty = use base URI)
     * @param transferable_ Whether tokens are transferable
     */
    function createTokenType(
        uint256 id,
        uint256 maxSupply_,
        string calldata tokenUri,
        bool transferable_
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (_tokenTypes[id].created) {
            revert TokenAlreadyExists(id);
        }

        _tokenTypes[id] = TokenType({
            maxSupply: maxSupply_,
            uri: tokenUri,
            created: true,
            transferable: transferable_,
            createdAt: block.timestamp
        });

        _allTokenIds.push(id);

        emit TokenTypeCreated(id, maxSupply_, tokenUri, transferable_);
    }

    /**
     * @notice Creates and mints a new token type
     * @param id The token type ID
     * @param initialSupply The initial supply to mint
     * @param maxSupply_ The max supply (0 = unlimited)
     * @param to The recipient address
     * @param tokenUri The token URI
     * @param transferable_ Whether tokens are transferable
     * @param data Additional data
     */
    function createAndMint(
        uint256 id,
        uint256 initialSupply,
        uint256 maxSupply_,
        address to,
        string calldata tokenUri,
        bool transferable_,
        bytes calldata data
    ) external onlyRole(MINTER_ROLE) nonReentrant {
        if (_tokenTypes[id].created) {
            revert TokenAlreadyExists(id);
        }
        if (maxSupply_ > 0 && initialSupply > maxSupply_) {
            revert MaxSupplyExceeded(id, initialSupply, maxSupply_);
        }

        _tokenTypes[id] = TokenType({
            maxSupply: maxSupply_,
            uri: tokenUri,
            created: true,
            transferable: transferable_,
            createdAt: block.timestamp
        });

        _allTokenIds.push(id);

        emit TokenTypeCreated(id, maxSupply_, tokenUri, transferable_);

        if (initialSupply > 0) {
            _mint(to, id, initialSupply, data);
        }
    }

    /**
     * @notice Updates max supply for a token type
     * @param id The token type ID
     * @param newMaxSupply The new max supply
     */
    function setMaxSupply(
        uint256 id,
        uint256 newMaxSupply
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (!_tokenTypes[id].created) {
            revert TokenDoesNotExist(id);
        }
        
        uint256 oldMaxSupply = _tokenTypes[id].maxSupply;
        require(
            newMaxSupply >= totalSupply(id) || newMaxSupply == 0,
            "GreyMultiToken: new max < current supply"
        );
        
        _tokenTypes[id].maxSupply = newMaxSupply;
        emit MaxSupplyUpdated(id, oldMaxSupply, newMaxSupply);
    }

    /**
     * @notice Updates transferability for a token type
     * @param id The token type ID
     * @param transferable_ New transferability status
     */
    function setTransferable(
        uint256 id,
        bool transferable_
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (!_tokenTypes[id].created) {
            revert TokenDoesNotExist(id);
        }
        
        _tokenTypes[id].transferable = transferable_;
        emit TransferabilityUpdated(id, transferable_);
    }

    // ============ Minting ============

    /**
     * @notice Mints tokens
     * @param to The recipient address
     * @param id The token type ID
     * @param amount The amount to mint
     * @param data Additional data
     */
    function mint(
        address to,
        uint256 id,
        uint256 amount,
        bytes calldata data
    ) external onlyRole(MINTER_ROLE) nonReentrant {
        _validateMint(id, amount);
        _mint(to, id, amount, data);
    }

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
    ) external onlyRole(MINTER_ROLE) nonReentrant {
        for (uint256 i = 0; i < ids.length; i++) {
            _validateMint(ids[i], amounts[i]);
        }
        _mintBatch(to, ids, amounts, data);
    }

    /**
     * @notice Mints to multiple recipients
     * @param recipients Array of recipient addresses
     * @param id The token type ID
     * @param amounts Array of amounts
     * @param data Additional data
     */
    function mintToMultiple(
        address[] calldata recipients,
        uint256 id,
        uint256[] calldata amounts,
        bytes calldata data
    ) external onlyRole(MINTER_ROLE) nonReentrant {
        require(recipients.length == amounts.length, "GreyMultiToken: length mismatch");
        
        uint256 totalAmount;
        for (uint256 i = 0; i < amounts.length; i++) {
            totalAmount += amounts[i];
        }
        _validateMint(id, totalAmount);

        for (uint256 i = 0; i < recipients.length; i++) {
            _mint(recipients[i], id, amounts[i], data);
        }
    }

    // ============ URI Management ============

    /**
     * @notice Returns the URI for a token type
     * @param id The token type ID
     * @return The URI
     */
    function uri(uint256 id) public view override returns (string memory) {
        if (!_tokenTypes[id].created) {
            return string(abi.encodePacked(_baseURI, id.toString(), ".json"));
        }
        
        if (bytes(_tokenTypes[id].uri).length > 0) {
            return _tokenTypes[id].uri;
        }
        
        return string(abi.encodePacked(_baseURI, id.toString(), ".json"));
    }

    /**
     * @notice Sets the URI for a token type
     * @param id The token type ID
     * @param newUri The new URI
     */
    function setURI(uint256 id, string calldata newUri) external onlyRole(URI_SETTER_ROLE) {
        if (!_tokenTypes[id].created) {
            revert TokenDoesNotExist(id);
        }
        
        string memory oldUri = _tokenTypes[id].uri;
        _tokenTypes[id].uri = newUri;
        emit URIUpdated(id, oldUri, newUri);
    }

    /**
     * @notice Sets the base URI
     * @param newBaseUri The new base URI
     */
    function setBaseURI(string calldata newBaseUri) external onlyRole(URI_SETTER_ROLE) {
        string memory oldUri = _baseURI;
        _baseURI = newBaseUri;
        emit BaseURIUpdated(oldUri, newBaseUri);
    }

    /**
     * @notice Sets the contract URI
     * @param newContractUri The new contract URI
     */
    function setContractURI(string calldata newContractUri) external onlyRole(DEFAULT_ADMIN_ROLE) {
        contractURI = newContractUri;
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
     * @notice Returns token type info
     * @param id The token type ID
     * @return The token type struct
     */
    function getTokenType(uint256 id) external view returns (TokenType memory) {
        return _tokenTypes[id];
    }

    /**
     * @notice Returns all token type IDs
     * @return Array of token type IDs
     */
    function getAllTokenIds() external view returns (uint256[] memory) {
        return _allTokenIds;
    }

    /**
     * @notice Returns the number of token types
     * @return The count
     */
    function getTokenTypeCount() external view returns (uint256) {
        return _allTokenIds.length;
    }

    /**
     * @notice Checks if a token type exists
     * @param id The token type ID
     * @return True if exists
     */
    function exists(uint256 id) public view override returns (bool) {
        return _tokenTypes[id].created;
    }

    /**
     * @notice Returns max supply for a token type
     * @param id The token type ID
     * @return The max supply (0 = unlimited)
     */
    function maxSupply(uint256 id) external view returns (uint256) {
        return _tokenTypes[id].maxSupply;
    }

    /**
     * @notice Returns remaining mintable supply
     * @param id The token type ID
     * @return The remaining supply
     */
    function remainingSupply(uint256 id) external view returns (uint256) {
        uint256 max = _tokenTypes[id].maxSupply;
        if (max == 0) {
            return type(uint256).max;
        }
        return max - totalSupply(id);
    }

    /**
     * @notice Checks if a token type is transferable
     * @param id The token type ID
     * @return True if transferable
     */
    function isTransferable(uint256 id) external view returns (bool) {
        return _tokenTypes[id].transferable;
    }

    /**
     * @notice Returns balances for multiple accounts and token types
     * @param accounts Array of account addresses
     * @param ids Array of token type IDs
     * @return Array of balances
     */
    function balanceOfBatch(
        address[] memory accounts,
        uint256[] memory ids
    ) public view override returns (uint256[] memory) {
        return super.balanceOfBatch(accounts, ids);
    }

    // ============ Internal Functions ============

    /**
     * @dev Validates a mint operation
     */
    function _validateMint(uint256 id, uint256 amount) private view {
        if (!_tokenTypes[id].created) {
            revert TokenDoesNotExist(id);
        }
        
        uint256 max = _tokenTypes[id].maxSupply;
        if (max > 0) {
            uint256 currentSupply = totalSupply(id);
            if (currentSupply + amount > max) {
                revert MaxSupplyExceeded(id, amount, max - currentSupply);
            }
        }
    }

    /**
     * @dev Update hook - checks transferability
     */
    function _update(
        address from,
        address to,
        uint256[] memory ids,
        uint256[] memory values
    ) internal override(ERC1155, ERC1155Pausable, ERC1155Supply) {
        // Skip transferability check for mints and burns
        if (from != address(0) && to != address(0)) {
            for (uint256 i = 0; i < ids.length; i++) {
                if (_tokenTypes[ids[i]].created && !_tokenTypes[ids[i]].transferable) {
                    revert TokenNotTransferable(ids[i]);
                }
            }
        }
        
        super._update(from, to, ids, values);
    }

    /**
     * @dev Supports interface override
     */
    function supportsInterface(
        bytes4 interfaceId
    ) public view override(ERC1155, AccessControl) returns (bool) {
        return super.supportsInterface(interfaceId);
    }
}
