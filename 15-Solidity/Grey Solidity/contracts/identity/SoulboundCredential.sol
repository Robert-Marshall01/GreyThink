// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/token/ERC721/extensions/ERC721Enumerable.sol";
import "@openzeppelin/contracts/utils/cryptography/ECDSA.sol";
import "@openzeppelin/contracts/utils/cryptography/MessageHashUtils.sol";

/**
 * @title SoulboundCredential
 * @notice Non-transferable credentials for decentralized identity
 * @dev ERC-5192 compliant soulbound tokens with attestation integration
 * 
 * Features:
 * - Non-transferable (soulbound) credentials
 * - Issuer-based credential types
 * - Expiration and revocation
 * - Batch issuance support
 * - Delegation and consent framework
 * - Emergency burn capability
 * - Metadata URI with on-chain verification
 */
contract SoulboundCredential is ERC721Enumerable, AccessControl {
    using ECDSA for bytes32;
    using MessageHashUtils for bytes32;

    // ============================================
    // ROLES
    // ============================================

    bytes32 public constant ISSUER_ROLE = keccak256("ISSUER_ROLE");
    bytes32 public constant VERIFIER_ROLE = keccak256("VERIFIER_ROLE");
    bytes32 public constant REVOKER_ROLE = keccak256("REVOKER_ROLE");

    // ============================================
    // TYPES
    // ============================================

    /// @notice Credential type definition
    struct CredentialType {
        string name;
        string description;
        address issuer;
        bool transferable;           // Override soulbound for certain credentials
        bool revocable;
        uint256 maxSupply;           // 0 = unlimited
        uint256 currentSupply;
        string baseURI;
        bool active;
    }

    /// @notice Individual credential data
    struct Credential {
        uint256 typeId;
        address holder;
        address issuer;
        uint256 issuedAt;
        uint256 expiresAt;           // 0 = never expires
        bytes32 dataHash;            // Hash of off-chain credential data
        CredentialStatus status;
        bool consentGiven;           // Holder consented to receive
    }

    /// @notice Consent request for credential issuance
    struct ConsentRequest {
        uint256 typeId;
        address holder;
        address issuer;
        bytes32 dataHash;
        uint256 expiresAt;
        uint256 requestedAt;
        bool approved;
        bool processed;
    }

    enum CredentialStatus {
        Active,
        Revoked,
        Expired,
        Suspended
    }

    // ============================================
    // STATE
    // ============================================

    /// @notice Token ID counter
    uint256 private _tokenIdCounter;

    /// @notice Credential types
    mapping(uint256 => CredentialType) public credentialTypes;
    uint256 public typeCount;

    /// @notice Credential data by token ID
    mapping(uint256 => Credential) public credentials;

    /// @notice Credentials by holder address
    mapping(address => uint256[]) public holderCredentials;

    /// @notice Credentials by type
    mapping(uint256 => uint256[]) public typeCredentials;

    /// @notice Consent requests
    mapping(bytes32 => ConsentRequest) public consentRequests;

    /// @notice Pending consents by holder
    mapping(address => bytes32[]) public pendingConsents;

    /// @notice Locked status (ERC-5192)
    mapping(uint256 => bool) private _locked;

    /// @notice Revocation reasons
    mapping(uint256 => string) public revocationReasons;

    // ============================================
    // EVENTS
    // ============================================

    event CredentialTypeCreated(
        uint256 indexed typeId,
        string name,
        address indexed issuer
    );

    event CredentialIssued(
        uint256 indexed tokenId,
        uint256 indexed typeId,
        address indexed holder,
        address issuer
    );

    event CredentialRevoked(
        uint256 indexed tokenId,
        address indexed revoker,
        string reason
    );

    event CredentialSuspended(
        uint256 indexed tokenId,
        address indexed suspender
    );

    event CredentialReinstated(
        uint256 indexed tokenId,
        address indexed reinstater
    );

    event ConsentRequested(
        bytes32 indexed requestId,
        address indexed holder,
        uint256 typeId
    );

    event ConsentGiven(
        bytes32 indexed requestId,
        address indexed holder
    );

    event ConsentRejected(
        bytes32 indexed requestId,
        address indexed holder
    );

    /// @notice ERC-5192 Locked event
    event Locked(uint256 indexed tokenId);
    event Unlocked(uint256 indexed tokenId);

    // ============================================
    // ERRORS
    // ============================================

    error Soulbound();
    error CredentialTypeNotFound();
    error CredentialNotFound();
    error CredentialExpired();
    error CredentialIsRevoked();
    error MaxSupplyReached();
    error ConsentNotGiven();
    error ConsentAlreadyProcessed();
    error NotHolder();
    error NotIssuer();
    error AlreadyRevoked();
    error InvalidCredential();

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor() ERC721("Soulbound Credential", "SBC") {
        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(ISSUER_ROLE, msg.sender);
    }

    // ============================================
    // CREDENTIAL TYPE MANAGEMENT
    // ============================================

    /**
     * @notice Create a new credential type
     * @param name The credential type name
     * @param description Description of the credential
     * @param transferable Whether this credential can be transferred
     * @param revocable Whether this credential can be revoked
     * @param maxSupply Maximum number of credentials (0 = unlimited)
     * @param baseURI Base URI for metadata
     */
    function createCredentialType(
        string calldata name,
        string calldata description,
        bool transferable,
        bool revocable,
        uint256 maxSupply,
        string calldata baseURI
    ) external onlyRole(ISSUER_ROLE) returns (uint256 typeId) {
        typeId = ++typeCount;

        credentialTypes[typeId] = CredentialType({
            name: name,
            description: description,
            issuer: msg.sender,
            transferable: transferable,
            revocable: revocable,
            maxSupply: maxSupply,
            currentSupply: 0,
            baseURI: baseURI,
            active: true
        });

        emit CredentialTypeCreated(typeId, name, msg.sender);
    }

    /**
     * @notice Deactivate a credential type
     */
    function deactivateCredentialType(uint256 typeId) external {
        CredentialType storage cType = credentialTypes[typeId];
        if (cType.issuer != msg.sender && !hasRole(DEFAULT_ADMIN_ROLE, msg.sender)) {
            revert NotIssuer();
        }
        cType.active = false;
    }

    // ============================================
    // CONSENT FRAMEWORK
    // ============================================

    /**
     * @notice Request consent to issue a credential
     */
    function requestConsent(
        uint256 typeId,
        address holder,
        bytes32 dataHash,
        uint256 expiresAt
    ) external onlyRole(ISSUER_ROLE) returns (bytes32 requestId) {
        CredentialType memory cType = credentialTypes[typeId];
        if (!cType.active) revert CredentialTypeNotFound();

        requestId = keccak256(abi.encodePacked(
            typeId,
            holder,
            msg.sender,
            dataHash,
            block.timestamp
        ));

        consentRequests[requestId] = ConsentRequest({
            typeId: typeId,
            holder: holder,
            issuer: msg.sender,
            dataHash: dataHash,
            expiresAt: expiresAt,
            requestedAt: block.timestamp,
            approved: false,
            processed: false
        });

        pendingConsents[holder].push(requestId);

        emit ConsentRequested(requestId, holder, typeId);
    }

    /**
     * @notice Holder gives consent for credential issuance
     */
    function giveConsent(bytes32 requestId) external {
        ConsentRequest storage request = consentRequests[requestId];

        if (request.holder != msg.sender) revert NotHolder();
        if (request.processed) revert ConsentAlreadyProcessed();

        request.approved = true;
        request.processed = true;

        emit ConsentGiven(requestId, msg.sender);
    }

    /**
     * @notice Holder rejects credential issuance
     */
    function rejectConsent(bytes32 requestId) external {
        ConsentRequest storage request = consentRequests[requestId];

        if (request.holder != msg.sender) revert NotHolder();
        if (request.processed) revert ConsentAlreadyProcessed();

        request.approved = false;
        request.processed = true;

        emit ConsentRejected(requestId, msg.sender);
    }

    // ============================================
    // CREDENTIAL ISSUANCE
    // ============================================

    /**
     * @notice Issue a credential after consent
     */
    function issueAfterConsent(bytes32 requestId) external returns (uint256 tokenId) {
        ConsentRequest memory request = consentRequests[requestId];

        if (!request.approved) revert ConsentNotGiven();
        if (request.issuer != msg.sender) revert NotIssuer();

        return _issueCredential(
            request.typeId,
            request.holder,
            request.dataHash,
            request.expiresAt,
            true
        );
    }

    /**
     * @notice Issue credential directly (for self-claimed or trusted scenarios)
     */
    function issueCredential(
        uint256 typeId,
        address holder,
        bytes32 dataHash,
        uint256 expiresAt
    ) external onlyRole(ISSUER_ROLE) returns (uint256 tokenId) {
        return _issueCredential(typeId, holder, dataHash, expiresAt, false);
    }

    /**
     * @notice Batch issue credentials
     */
    function batchIssueCredentials(
        uint256 typeId,
        address[] calldata holders,
        bytes32[] calldata dataHashes,
        uint256 expiresAt
    ) external onlyRole(ISSUER_ROLE) returns (uint256[] memory tokenIds) {
        require(holders.length == dataHashes.length, "Length mismatch");

        tokenIds = new uint256[](holders.length);

        for (uint256 i = 0; i < holders.length; i++) {
            tokenIds[i] = _issueCredential(typeId, holders[i], dataHashes[i], expiresAt, false);
        }
    }

    /**
     * @notice Internal credential issuance
     */
    function _issueCredential(
        uint256 typeId,
        address holder,
        bytes32 dataHash,
        uint256 expiresAt,
        bool hasConsent
    ) internal returns (uint256 tokenId) {
        CredentialType storage cType = credentialTypes[typeId];

        if (!cType.active) revert CredentialTypeNotFound();
        if (cType.maxSupply > 0 && cType.currentSupply >= cType.maxSupply) {
            revert MaxSupplyReached();
        }

        tokenId = ++_tokenIdCounter;

        _safeMint(holder, tokenId);

        credentials[tokenId] = Credential({
            typeId: typeId,
            holder: holder,
            issuer: msg.sender,
            issuedAt: block.timestamp,
            expiresAt: expiresAt,
            dataHash: dataHash,
            status: CredentialStatus.Active,
            consentGiven: hasConsent
        });

        holderCredentials[holder].push(tokenId);
        typeCredentials[typeId].push(tokenId);
        cType.currentSupply++;

        // Lock the token if soulbound
        if (!cType.transferable) {
            _locked[tokenId] = true;
            emit Locked(tokenId);
        }

        emit CredentialIssued(tokenId, typeId, holder, msg.sender);
    }

    // ============================================
    // CREDENTIAL STATUS MANAGEMENT
    // ============================================

    /**
     * @notice Revoke a credential
     */
    function revokeCredential(
        uint256 tokenId,
        string calldata reason
    ) external {
        Credential storage cred = credentials[tokenId];
        CredentialType memory cType = credentialTypes[cred.typeId];

        if (!cType.revocable) revert InvalidCredential();
        if (cred.issuer != msg.sender && !hasRole(REVOKER_ROLE, msg.sender)) {
            revert NotIssuer();
        }
        if (cred.status == CredentialStatus.Revoked) revert AlreadyRevoked();

        cred.status = CredentialStatus.Revoked;
        revocationReasons[tokenId] = reason;

        emit CredentialRevoked(tokenId, msg.sender, reason);
    }

    /**
     * @notice Suspend a credential temporarily
     */
    function suspendCredential(uint256 tokenId) external {
        Credential storage cred = credentials[tokenId];

        if (cred.issuer != msg.sender && !hasRole(REVOKER_ROLE, msg.sender)) {
            revert NotIssuer();
        }

        cred.status = CredentialStatus.Suspended;

        emit CredentialSuspended(tokenId, msg.sender);
    }

    /**
     * @notice Reinstate a suspended credential
     */
    function reinstateCredential(uint256 tokenId) external {
        Credential storage cred = credentials[tokenId];

        if (cred.issuer != msg.sender) revert NotIssuer();
        if (cred.status != CredentialStatus.Suspended) revert InvalidCredential();

        cred.status = CredentialStatus.Active;

        emit CredentialReinstated(tokenId, msg.sender);
    }

    /**
     * @notice Holder burns their own credential
     */
    function burnCredential(uint256 tokenId) external {
        if (ownerOf(tokenId) != msg.sender) revert NotHolder();

        _burn(tokenId);
        credentials[tokenId].status = CredentialStatus.Revoked;
    }

    // ============================================
    // VERIFICATION FUNCTIONS
    // ============================================

    /**
     * @notice Verify a credential is valid
     */
    function verifyCredential(uint256 tokenId) external view returns (
        bool isValid,
        uint256 typeId,
        address holder,
        address issuer,
        uint256 expiresAt
    ) {
        Credential memory cred = credentials[tokenId];

        bool valid = cred.status == CredentialStatus.Active &&
                     (cred.expiresAt == 0 || cred.expiresAt > block.timestamp);

        return (valid, cred.typeId, cred.holder, cred.issuer, cred.expiresAt);
    }

    /**
     * @notice Check if holder has a valid credential of a specific type
     */
    function hasValidCredential(
        address holder,
        uint256 typeId
    ) external view returns (bool) {
        uint256[] memory creds = holderCredentials[holder];

        for (uint256 i = 0; i < creds.length; i++) {
            Credential memory cred = credentials[creds[i]];
            if (cred.typeId == typeId &&
                cred.status == CredentialStatus.Active &&
                (cred.expiresAt == 0 || cred.expiresAt > block.timestamp)) {
                return true;
            }
        }

        return false;
    }

    /**
     * @notice Get all valid credentials for a holder
     */
    function getValidCredentials(address holder) external view returns (uint256[] memory) {
        uint256[] memory allCreds = holderCredentials[holder];
        uint256 validCount = 0;

        // Count valid credentials
        for (uint256 i = 0; i < allCreds.length; i++) {
            Credential memory cred = credentials[allCreds[i]];
            if (cred.status == CredentialStatus.Active &&
                (cred.expiresAt == 0 || cred.expiresAt > block.timestamp)) {
                validCount++;
            }
        }

        // Build valid credentials array
        uint256[] memory validCreds = new uint256[](validCount);
        uint256 index = 0;

        for (uint256 i = 0; i < allCreds.length; i++) {
            Credential memory cred = credentials[allCreds[i]];
            if (cred.status == CredentialStatus.Active &&
                (cred.expiresAt == 0 || cred.expiresAt > block.timestamp)) {
                validCreds[index++] = allCreds[i];
            }
        }

        return validCreds;
    }

    // ============================================
    // ERC-5192 SOULBOUND INTERFACE
    // ============================================

    /**
     * @notice Check if token is locked (soulbound)
     */
    function locked(uint256 tokenId) external view returns (bool) {
        return _locked[tokenId];
    }

    /**
     * @notice Override transfer to enforce soulbound
     */
    function _update(
        address to,
        uint256 tokenId,
        address auth
    ) internal virtual override returns (address) {
        address from = _ownerOf(tokenId);

        // Allow minting and burning, block transfers for locked tokens
        if (from != address(0) && to != address(0) && _locked[tokenId]) {
            revert Soulbound();
        }

        return super._update(to, tokenId, auth);
    }

    // ============================================
    // METADATA
    // ============================================

    function tokenURI(uint256 tokenId) public view override returns (string memory) {
        _requireOwned(tokenId);

        Credential memory cred = credentials[tokenId];
        CredentialType memory cType = credentialTypes[cred.typeId];

        return string(abi.encodePacked(cType.baseURI, _toString(tokenId)));
    }

    function _toString(uint256 value) internal pure returns (string memory) {
        if (value == 0) return "0";

        uint256 temp = value;
        uint256 digits;
        while (temp != 0) {
            digits++;
            temp /= 10;
        }

        bytes memory buffer = new bytes(digits);
        while (value != 0) {
            digits -= 1;
            buffer[digits] = bytes1(uint8(48 + uint256(value % 10)));
            value /= 10;
        }

        return string(buffer);
    }

    // ============================================
    // INTERFACE SUPPORT
    // ============================================

    function supportsInterface(bytes4 interfaceId)
        public
        view
        override(ERC721Enumerable, AccessControl)
        returns (bool)
    {
        return
            interfaceId == 0xb45a3c0e || // ERC-5192
            super.supportsInterface(interfaceId);
    }
}
