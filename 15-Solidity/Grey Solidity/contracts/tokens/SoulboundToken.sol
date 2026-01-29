// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts-upgradeable/token/ERC721/ERC721Upgradeable.sol";
import "@openzeppelin/contracts-upgradeable/token/ERC721/extensions/ERC721EnumerableUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/token/ERC721/extensions/ERC721URIStorageUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/access/AccessControlUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/PausableUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/Initializable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/UUPSUpgradeable.sol";
import "@openzeppelin/contracts/utils/Strings.sol";

/**
 * @title SoulboundToken
 * @author Grey Protocol Team
 * @notice Non-transferable ERC721 token representing identity, achievements, or credentials
 * @dev Implements EIP-5192 Minimal Soulbound Interface
 * 
 * Features:
 * - Non-transferable by default (can be locked/unlocked per token)
 * - Revocable by authorized issuers
 * - Metadata support with multiple schemas
 * - Expiration support for time-limited credentials
 * - Batch minting and revoking
 * - On-chain attributes storage
 * - Recovery mechanism for lost wallets
 */
contract SoulboundToken is 
    Initializable,
    ERC721Upgradeable,
    ERC721EnumerableUpgradeable,
    ERC721URIStorageUpgradeable,
    AccessControlUpgradeable,
    PausableUpgradeable,
    UUPSUpgradeable
{
    using Strings for uint256;

    // ============================================
    // CONSTANTS & ROLES
    // ============================================

    /// @notice Role for minting tokens
    bytes32 public constant MINTER_ROLE = keccak256("MINTER_ROLE");
    
    /// @notice Role for revoking tokens
    bytes32 public constant REVOKER_ROLE = keccak256("REVOKER_ROLE");
    
    /// @notice Role for updating metadata
    bytes32 public constant METADATA_ROLE = keccak256("METADATA_ROLE");
    
    /// @notice Role for recovery operations
    bytes32 public constant RECOVERY_ROLE = keccak256("RECOVERY_ROLE");
    
    /// @notice Role for upgrading contract
    bytes32 public constant UPGRADER_ROLE = keccak256("UPGRADER_ROLE");

    // ============================================
    // ENUMS
    // ============================================

    /**
     * @notice Token categories
     */
    enum TokenCategory {
        IDENTITY,           // Identity verification
        CREDENTIAL,         // Professional credentials
        ACHIEVEMENT,        // Achievements/badges
        MEMBERSHIP,         // Memberships
        ATTESTATION,        // Attestations from others
        CERTIFICATION,      // Certifications
        REPUTATION,         // Reputation scores
        CUSTOM              // Custom category
    }

    /**
     * @notice Token status
     */
    enum TokenStatus {
        ACTIVE,
        EXPIRED,
        REVOKED,
        SUSPENDED
    }

    // ============================================
    // STRUCTS
    // ============================================

    /**
     * @notice Token metadata structure
     * @param category Category of the token
     * @param status Current status
     * @param issuer Address that issued the token
     * @param issuedAt When the token was issued
     * @param expiresAt When the token expires (0 = never)
     * @param isTransferable Whether the token can be transferred
     * @param schemaId Schema identifier for metadata
     * @param attributeCount Number of on-chain attributes
     */
    struct TokenMetadata {
        TokenCategory category;
        TokenStatus status;
        address issuer;
        uint256 issuedAt;
        uint256 expiresAt;
        bool isTransferable;
        bytes32 schemaId;
        uint256 attributeCount;
    }

    /**
     * @notice On-chain attribute
     * @param key Attribute key
     * @param value Attribute value
     * @param dataType Type of the value (string, uint, bool, etc.)
     * @param isPublic Whether the attribute is publicly visible
     */
    struct Attribute {
        string key;
        bytes value;
        string dataType;
        bool isPublic;
    }

    /**
     * @notice Input struct for batch attribute setting (reduces stack depth)
     */
    struct AttributeInput {
        string key;
        bytes value;
        string dataType;
        bool isPublic;
    }

    /**
     * @notice Recovery request
     * @param oldOwner Current token owner
     * @param newOwner Requested new owner
     * @param tokenId Token to recover
     * @param requestedAt When recovery was requested
     * @param approvedAt When recovery was approved (0 if pending)
     * @param approvals Number of approvals received
     * @param requiredApprovals Number of approvals needed
     */
    struct RecoveryRequest {
        address oldOwner;
        address newOwner;
        uint256 tokenId;
        uint256 requestedAt;
        uint256 approvedAt;
        uint256 approvals;
        uint256 requiredApprovals;
        mapping(address => bool) hasApproved;
    }

    /**
     * @notice Mint request for batch operations
     */
    struct MintRequest {
        address to;
        TokenCategory category;
        string uri;
        uint256 expiresAt;
        bytes32 schemaId;
        bool transferable;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Counter for token IDs
    uint256 private _tokenIdCounter;

    /// @notice Base URI for metadata
    string public baseURI;

    /// @notice Mapping from token ID to metadata
    mapping(uint256 => TokenMetadata) public tokenMetadata;

    /// @notice Mapping from token ID to attributes (key => attribute)
    mapping(uint256 => mapping(bytes32 => Attribute)) public tokenAttributes;

    /// @notice Mapping from token ID to attribute keys
    mapping(uint256 => bytes32[]) public tokenAttributeKeys;

    /// @notice Recovery requests
    mapping(uint256 => RecoveryRequest) public recoveryRequests;

    /// @notice Recovery request counter
    uint256 public recoveryRequestCounter;

    /// @notice Default expiration period (0 = never)
    uint256 public defaultExpirationPeriod;

    /// @notice Whether global transfers are allowed (overrides per-token setting)
    bool public globalTransfersAllowed;

    /// @notice Mapping of locked tokens (EIP-5192)
    mapping(uint256 => bool) private _locked;

    /// @notice Schema registry (schemaId => schema URI)
    mapping(bytes32 => string) public schemas;

    /// @notice Token count per category
    mapping(TokenCategory => uint256) public categoryTokenCount;

    /// @notice Token count per issuer
    mapping(address => uint256) public issuerTokenCount;

    /// @notice Storage gap for upgrades
    uint256[45] private __gap;

    // ============================================
    // EVENTS
    // ============================================

    /// @notice EIP-5192: Emitted when token locked status changes
    event Locked(uint256 indexed tokenId);
    
    /// @notice EIP-5192: Emitted when token unlocked
    event Unlocked(uint256 indexed tokenId);

    event TokenIssued(
        uint256 indexed tokenId,
        address indexed to,
        address indexed issuer,
        TokenCategory category,
        uint256 expiresAt
    );

    event TokenRevoked(
        uint256 indexed tokenId,
        address indexed holder,
        address indexed revoker,
        string reason
    );

    event TokenExpired(uint256 indexed tokenId, address indexed holder);

    event TokenSuspended(uint256 indexed tokenId, address indexed suspender, string reason);

    event TokenReinstated(uint256 indexed tokenId, address indexed reinstater);

    event AttributeSet(
        uint256 indexed tokenId,
        bytes32 indexed key,
        string dataType,
        bool isPublic
    );

    event AttributeRemoved(uint256 indexed tokenId, bytes32 indexed key);

    event RecoveryRequested(
        uint256 indexed requestId,
        uint256 indexed tokenId,
        address indexed oldOwner,
        address newOwner
    );

    event RecoveryApproved(uint256 indexed requestId, address indexed approver);

    event RecoveryExecuted(
        uint256 indexed requestId,
        uint256 indexed tokenId,
        address indexed newOwner
    );

    event SchemaRegistered(bytes32 indexed schemaId, string schemaURI);

    // ============================================
    // ERRORS
    // ============================================

    error TokenLocked(uint256 tokenId);
    error TokenNotTransferable(uint256 tokenId);
    error TokenExpiredError(uint256 tokenId);
    error TokenNotActive(uint256 tokenId);
    error InvalidExpiration();
    error InvalidCategory();
    error AttributeNotFound(bytes32 key);
    error RecoveryNotFound(uint256 requestId);
    error RecoveryAlreadyApproved();
    error InsufficientApprovals();
    error RecoveryExpired();
    error InvalidRecoveryRequest();
    error SchemaNotFound(bytes32 schemaId);
    error ZeroAddress();
    error AlreadyIssued(address holder);

    // ============================================
    // MODIFIERS
    // ============================================

    /**
     * @notice Ensures token is active and not expired
     */
    modifier tokenActive(uint256 tokenId) {
        TokenMetadata storage meta = tokenMetadata[tokenId];
        if (meta.status == TokenStatus.REVOKED) revert TokenNotActive(tokenId);
        if (meta.status == TokenStatus.SUSPENDED) revert TokenNotActive(tokenId);
        if (meta.expiresAt > 0 && block.timestamp > meta.expiresAt) {
            revert TokenExpiredError(tokenId);
        }
        _;
    }

    // ============================================
    // INITIALIZER
    // ============================================

    /**
     * @notice Initializes the Soulbound Token contract
     * @param name Token name
     * @param symbol Token symbol
     * @param baseURI_ Base URI for token metadata
     * @param admin Admin address
     */
    function initialize(
        string memory name,
        string memory symbol,
        string memory baseURI_,
        address admin
    ) public initializer {
        if (admin == address(0)) revert ZeroAddress();

        __ERC721_init(name, symbol);
        __ERC721Enumerable_init();
        __ERC721URIStorage_init();
        __AccessControl_init();
        __Pausable_init();
        __UUPSUpgradeable_init();

        baseURI = baseURI_;
        globalTransfersAllowed = false;

        _grantRole(DEFAULT_ADMIN_ROLE, admin);
        _grantRole(MINTER_ROLE, admin);
        _grantRole(REVOKER_ROLE, admin);
        _grantRole(METADATA_ROLE, admin);
        _grantRole(RECOVERY_ROLE, admin);
        _grantRole(UPGRADER_ROLE, admin);
    }

    // ============================================
    // MINTING
    // ============================================

    /**
     * @notice Issues a new soulbound token
     * @param to Recipient address
     * @param category Token category
     * @param uri Token URI
     * @param expiresAt Expiration timestamp (0 = never)
     * @param schemaId Schema identifier
     * @return tokenId The minted token ID
     */
    function issue(
        address to,
        TokenCategory category,
        string calldata uri,
        uint256 expiresAt,
        bytes32 schemaId
    )
        external
        onlyRole(MINTER_ROLE)
        whenNotPaused
        returns (uint256 tokenId)
    {
        if (to == address(0)) revert ZeroAddress();
        if (expiresAt > 0 && expiresAt <= block.timestamp) revert InvalidExpiration();

        tokenId = _tokenIdCounter++;
        
        _safeMint(to, tokenId);
        _setTokenURI(tokenId, uri);

        tokenMetadata[tokenId] = TokenMetadata({
            category: category,
            status: TokenStatus.ACTIVE,
            issuer: msg.sender,
            issuedAt: block.timestamp,
            expiresAt: expiresAt,
            isTransferable: false,
            schemaId: schemaId,
            attributeCount: 0
        });

        // Lock by default (soulbound)
        _locked[tokenId] = true;

        categoryTokenCount[category]++;
        issuerTokenCount[msg.sender]++;

        emit TokenIssued(tokenId, to, msg.sender, category, expiresAt);
        emit Locked(tokenId);
    }

    /**
     * @notice Batch issue tokens
     * @param requests Array of mint requests
     * @return tokenIds Array of minted token IDs
     */
    function batchIssue(
        MintRequest[] calldata requests
    )
        external
        onlyRole(MINTER_ROLE)
        whenNotPaused
        returns (uint256[] memory tokenIds)
    {
        uint256 count = requests.length;
        tokenIds = new uint256[](count);

        for (uint256 i = 0; i < count; i++) {
            MintRequest calldata req = requests[i];
            if (req.to == address(0)) revert ZeroAddress();
            if (req.expiresAt > 0 && req.expiresAt <= block.timestamp) revert InvalidExpiration();

            uint256 tokenId = _tokenIdCounter++;
            tokenIds[i] = tokenId;

            _safeMint(req.to, tokenId);
            _setTokenURI(tokenId, req.uri);

            tokenMetadata[tokenId] = TokenMetadata({
                category: req.category,
                status: TokenStatus.ACTIVE,
                issuer: msg.sender,
                issuedAt: block.timestamp,
                expiresAt: req.expiresAt,
                isTransferable: req.transferable,
                schemaId: req.schemaId,
                attributeCount: 0
            });

            if (!req.transferable) {
                _locked[tokenId] = true;
                emit Locked(tokenId);
            }

            categoryTokenCount[req.category]++;
            issuerTokenCount[msg.sender]++;

            emit TokenIssued(tokenId, req.to, msg.sender, req.category, req.expiresAt);
        }
    }

    // ============================================
    // REVOCATION & STATUS
    // ============================================

    /**
     * @notice Revokes a token
     * @param tokenId Token to revoke
     * @param reason Reason for revocation
     */
    function revoke(
        uint256 tokenId,
        string calldata reason
    ) external onlyRole(REVOKER_ROLE) {
        address holder = ownerOf(tokenId);
        
        tokenMetadata[tokenId].status = TokenStatus.REVOKED;
        
        emit TokenRevoked(tokenId, holder, msg.sender, reason);
    }

    /**
     * @notice Batch revoke tokens
     * @param tokenIds Array of token IDs to revoke
     * @param reason Reason for revocation
     */
    function batchRevoke(
        uint256[] calldata tokenIds,
        string calldata reason
    ) external onlyRole(REVOKER_ROLE) {
        for (uint256 i = 0; i < tokenIds.length; i++) {
            address holder = ownerOf(tokenIds[i]);
            tokenMetadata[tokenIds[i]].status = TokenStatus.REVOKED;
            emit TokenRevoked(tokenIds[i], holder, msg.sender, reason);
        }
    }

    /**
     * @notice Suspends a token temporarily
     * @param tokenId Token to suspend
     * @param reason Reason for suspension
     */
    function suspend(
        uint256 tokenId,
        string calldata reason
    ) external onlyRole(REVOKER_ROLE) {
        tokenMetadata[tokenId].status = TokenStatus.SUSPENDED;
        emit TokenSuspended(tokenId, msg.sender, reason);
    }

    /**
     * @notice Reinstates a suspended token
     * @param tokenId Token to reinstate
     */
    function reinstate(uint256 tokenId) external onlyRole(REVOKER_ROLE) {
        TokenMetadata storage meta = tokenMetadata[tokenId];
        require(meta.status == TokenStatus.SUSPENDED, "Not suspended");
        
        meta.status = TokenStatus.ACTIVE;
        emit TokenReinstated(tokenId, msg.sender);
    }

    // ============================================
    // ATTRIBUTES
    // ============================================

    /**
     * @notice Sets an attribute on a token
     * @param tokenId Token ID
     * @param key Attribute key
     * @param value Attribute value (encoded)
     * @param dataType Type of the value
     * @param isPublic Whether visible to everyone
     */
    function setAttribute(
        uint256 tokenId,
        string calldata key,
        bytes calldata value,
        string calldata dataType,
        bool isPublic
    ) external onlyRole(METADATA_ROLE) tokenActive(tokenId) {
        bytes32 keyHash = keccak256(bytes(key));
        
        // Check if new attribute
        if (tokenAttributes[tokenId][keyHash].value.length == 0) {
            tokenAttributeKeys[tokenId].push(keyHash);
            tokenMetadata[tokenId].attributeCount++;
        }

        tokenAttributes[tokenId][keyHash] = Attribute({
            key: key,
            value: value,
            dataType: dataType,
            isPublic: isPublic
        });

        emit AttributeSet(tokenId, keyHash, dataType, isPublic);
    }

    /**
     * @notice Batch set attributes using struct array
     * @param tokenId Token ID
     * @param attributes Array of AttributeInput structs
     */
    function batchSetAttributes(
        uint256 tokenId,
        AttributeInput[] calldata attributes
    ) external onlyRole(METADATA_ROLE) tokenActive(tokenId) {
        uint256 len = attributes.length;
        for (uint256 i = 0; i < len; i++) {
            AttributeInput calldata attr = attributes[i];
            bytes32 keyHash = keccak256(bytes(attr.key));
            
            if (tokenAttributes[tokenId][keyHash].value.length == 0) {
                tokenAttributeKeys[tokenId].push(keyHash);
                tokenMetadata[tokenId].attributeCount++;
            }

            tokenAttributes[tokenId][keyHash] = Attribute({
                key: attr.key,
                value: attr.value,
                dataType: attr.dataType,
                isPublic: attr.isPublic
            });

            emit AttributeSet(tokenId, keyHash, attr.dataType, attr.isPublic);
        }
    }

    /**
     * @notice Removes an attribute
     * @param tokenId Token ID
     * @param key Attribute key to remove
     */
    function removeAttribute(
        uint256 tokenId,
        string calldata key
    ) external onlyRole(METADATA_ROLE) {
        bytes32 keyHash = keccak256(bytes(key));
        if (tokenAttributes[tokenId][keyHash].value.length == 0) {
            revert AttributeNotFound(keyHash);
        }

        delete tokenAttributes[tokenId][keyHash];
        tokenMetadata[tokenId].attributeCount--;

        emit AttributeRemoved(tokenId, keyHash);
    }

    /**
     * @notice Gets an attribute value
     * @param tokenId Token ID
     * @param key Attribute key
     * @return attribute The attribute data
     */
    function getAttribute(
        uint256 tokenId,
        string calldata key
    ) external view returns (Attribute memory attribute) {
        bytes32 keyHash = keccak256(bytes(key));
        attribute = tokenAttributes[tokenId][keyHash];
        
        // Check visibility
        if (!attribute.isPublic && msg.sender != ownerOf(tokenId)) {
            if (!hasRole(METADATA_ROLE, msg.sender)) {
                return Attribute("", "", "", false);
            }
        }
    }

    /**
     * @notice Gets all public attribute keys for a token
     * @param tokenId Token ID
     * @return keys Array of attribute keys
     */
    function getPublicAttributeKeys(uint256 tokenId) external view returns (string[] memory keys) {
        bytes32[] memory keyHashes = tokenAttributeKeys[tokenId];
        uint256 publicCount = 0;

        // Count public attributes
        for (uint256 i = 0; i < keyHashes.length; i++) {
            if (tokenAttributes[tokenId][keyHashes[i]].isPublic) {
                publicCount++;
            }
        }

        // Build array
        keys = new string[](publicCount);
        uint256 index = 0;
        for (uint256 i = 0; i < keyHashes.length; i++) {
            if (tokenAttributes[tokenId][keyHashes[i]].isPublic) {
                keys[index++] = tokenAttributes[tokenId][keyHashes[i]].key;
            }
        }
    }

    // ============================================
    // RECOVERY
    // ============================================

    /**
     * @notice Initiates a recovery request for a lost wallet
     * @param tokenId Token to recover
     * @param newOwner New owner address
     * @param requiredApprovals Number of approvals needed
     * @return requestId The recovery request ID
     */
    function initiateRecovery(
        uint256 tokenId,
        address newOwner,
        uint256 requiredApprovals
    )
        external
        onlyRole(RECOVERY_ROLE)
        returns (uint256 requestId)
    {
        if (newOwner == address(0)) revert ZeroAddress();
        address oldOwner = ownerOf(tokenId);
        if (newOwner == oldOwner) revert InvalidRecoveryRequest();

        requestId = recoveryRequestCounter++;
        
        RecoveryRequest storage request = recoveryRequests[requestId];
        request.oldOwner = oldOwner;
        request.newOwner = newOwner;
        request.tokenId = tokenId;
        request.requestedAt = block.timestamp;
        request.requiredApprovals = requiredApprovals;

        emit RecoveryRequested(requestId, tokenId, oldOwner, newOwner);
    }

    /**
     * @notice Approves a recovery request
     * @param requestId Recovery request ID
     */
    function approveRecovery(uint256 requestId) external onlyRole(RECOVERY_ROLE) {
        RecoveryRequest storage request = recoveryRequests[requestId];
        if (request.requestedAt == 0) revert RecoveryNotFound(requestId);
        if (request.hasApproved[msg.sender]) revert RecoveryAlreadyApproved();
        
        // Check not expired (7 days)
        if (block.timestamp > request.requestedAt + 7 days) revert RecoveryExpired();

        request.hasApproved[msg.sender] = true;
        request.approvals++;

        emit RecoveryApproved(requestId, msg.sender);
    }

    /**
     * @notice Executes a recovery after sufficient approvals
     * @param requestId Recovery request ID
     */
    function executeRecovery(uint256 requestId) external onlyRole(RECOVERY_ROLE) {
        RecoveryRequest storage request = recoveryRequests[requestId];
        if (request.requestedAt == 0) revert RecoveryNotFound(requestId);
        if (request.approvals < request.requiredApprovals) revert InsufficientApprovals();
        if (request.approvedAt > 0) revert("Already executed");

        request.approvedAt = block.timestamp;

        // Transfer token (bypassing soulbound restriction)
        _update(request.newOwner, request.tokenId, address(0));

        emit RecoveryExecuted(requestId, request.tokenId, request.newOwner);
    }

    // ============================================
    // SCHEMA MANAGEMENT
    // ============================================

    /**
     * @notice Registers a new metadata schema
     * @param schemaId Schema identifier
     * @param schemaURI URI pointing to schema definition
     */
    function registerSchema(
        bytes32 schemaId,
        string calldata schemaURI
    ) external onlyRole(METADATA_ROLE) {
        schemas[schemaId] = schemaURI;
        emit SchemaRegistered(schemaId, schemaURI);
    }

    /**
     * @notice Gets schema URI
     * @param schemaId Schema identifier
     * @return Schema URI
     */
    function getSchema(bytes32 schemaId) external view returns (string memory) {
        return schemas[schemaId];
    }

    // ============================================
    // LOCK/UNLOCK (EIP-5192)
    // ============================================

    /**
     * @notice Checks if a token is locked
     * @param tokenId Token to check
     * @return True if locked
     */
    function locked(uint256 tokenId) external view returns (bool) {
        return _locked[tokenId];
    }

    /**
     * @notice Unlocks a token (makes it transferable)
     * @param tokenId Token to unlock
     */
    function unlock(uint256 tokenId) external onlyRole(DEFAULT_ADMIN_ROLE) {
        require(_locked[tokenId], "Not locked");
        _locked[tokenId] = false;
        tokenMetadata[tokenId].isTransferable = true;
        emit Unlocked(tokenId);
    }

    /**
     * @notice Locks a token (makes it non-transferable)
     * @param tokenId Token to lock
     */
    function lock(uint256 tokenId) external onlyRole(DEFAULT_ADMIN_ROLE) {
        require(!_locked[tokenId], "Already locked");
        _locked[tokenId] = true;
        tokenMetadata[tokenId].isTransferable = false;
        emit Locked(tokenId);
    }

    // ============================================
    // TRANSFER RESTRICTIONS
    // ============================================

    /**
     * @notice Override to enforce soulbound property
     */
    function _update(
        address to,
        uint256 tokenId,
        address auth
    ) internal override(ERC721Upgradeable, ERC721EnumerableUpgradeable) returns (address) {
        address from = _ownerOf(tokenId);
        
        // Allow minting and burning
        if (from != address(0) && to != address(0)) {
            // Check if transfers are allowed
            if (_locked[tokenId] && !globalTransfersAllowed) {
                // Only allow transfer during recovery
                if (auth != address(0)) {
                    revert TokenLocked(tokenId);
                }
            }
        }

        return super._update(to, tokenId, auth);
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Gets full token information
     * @param tokenId Token ID
     * @return metadata Token metadata
     */
    function getTokenInfo(uint256 tokenId) external view returns (TokenMetadata memory metadata) {
        return tokenMetadata[tokenId];
    }

    /**
     * @notice Checks if a token is valid (not expired or revoked)
     * @param tokenId Token ID
     * @return True if valid
     */
    function isValid(uint256 tokenId) external view returns (bool) {
        TokenMetadata storage meta = tokenMetadata[tokenId];
        
        if (meta.status != TokenStatus.ACTIVE) return false;
        if (meta.expiresAt > 0 && block.timestamp > meta.expiresAt) return false;
        
        return true;
    }

    /**
     * @notice Gets tokens by category for a holder
     * @param holder Address to query
     * @param category Category to filter
     * @return tokenIds Array of token IDs
     */
    function getTokensByCategory(
        address holder,
        TokenCategory category
    ) external view returns (uint256[] memory tokenIds) {
        uint256 balance = balanceOf(holder);
        uint256[] memory temp = new uint256[](balance);
        uint256 count = 0;

        for (uint256 i = 0; i < balance; i++) {
            uint256 tokenId = tokenOfOwnerByIndex(holder, i);
            if (tokenMetadata[tokenId].category == category) {
                temp[count++] = tokenId;
            }
        }

        tokenIds = new uint256[](count);
        for (uint256 i = 0; i < count; i++) {
            tokenIds[i] = temp[i];
        }
    }

    // ============================================
    // ADMIN
    // ============================================

    /**
     * @notice Sets base URI
     * @param newBaseURI New base URI
     */
    function setBaseURI(string calldata newBaseURI) external onlyRole(METADATA_ROLE) {
        baseURI = newBaseURI;
    }

    /**
     * @notice Sets global transfer permission
     * @param allowed Whether transfers are globally allowed
     */
    function setGlobalTransfersAllowed(bool allowed) external onlyRole(DEFAULT_ADMIN_ROLE) {
        globalTransfersAllowed = allowed;
    }

    /**
     * @notice Pauses the contract
     */
    function pause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        _pause();
    }

    /**
     * @notice Unpauses the contract
     */
    function unpause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        _unpause();
    }

    // ============================================
    // OVERRIDES
    // ============================================

    function _baseURI() internal view override returns (string memory) {
        return baseURI;
    }

    function tokenURI(uint256 tokenId) public view override(ERC721Upgradeable, ERC721URIStorageUpgradeable) returns (string memory) {
        return super.tokenURI(tokenId);
    }

    function _increaseBalance(address account, uint128 value) internal override(ERC721Upgradeable, ERC721EnumerableUpgradeable) {
        super._increaseBalance(account, value);
    }

    function supportsInterface(bytes4 interfaceId) public view override(ERC721Upgradeable, ERC721EnumerableUpgradeable, ERC721URIStorageUpgradeable, AccessControlUpgradeable) returns (bool) {
        // EIP-5192 interface ID
        if (interfaceId == 0xb45a3c0e) return true;
        return super.supportsInterface(interfaceId);
    }

    function _authorizeUpgrade(address newImplementation) internal override onlyRole(UPGRADER_ROLE) {}
}
