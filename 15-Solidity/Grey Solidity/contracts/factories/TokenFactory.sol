// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/proxy/Clones.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/Pausable.sol";

/**
 * @title TokenFactory
 * @author Grey Protocol Team
 * @notice Factory contract for deploying various token types using minimal proxy cloning
 * @dev Uses OpenZeppelin's Clones library for gas-efficient deployments
 * 
 * Features:
 * - Deploy ERC20, ERC721, ERC1155 tokens via cloning
 * - Configurable deployment fees
 * - Template management for different token implementations
 * - Event tracking for all deployments
 * - Access control for template management
 */
contract TokenFactory is AccessControl, ReentrancyGuard, Pausable {
    using Clones for address;

    // ============================================
    // CONSTANTS & ROLES
    // ============================================

    /// @notice Role for managing token templates
    bytes32 public constant TEMPLATE_ADMIN_ROLE = keccak256("TEMPLATE_ADMIN_ROLE");
    
    /// @notice Role for pausing the factory
    bytes32 public constant PAUSER_ROLE = keccak256("PAUSER_ROLE");
    
    /// @notice Role for fee management
    bytes32 public constant FEE_MANAGER_ROLE = keccak256("FEE_MANAGER_ROLE");
    
    /// @notice Maximum deployment fee (10 ETH)
    uint256 public constant MAX_DEPLOYMENT_FEE = 10 ether;
    
    /// @notice Minimum deployment fee
    uint256 public constant MIN_DEPLOYMENT_FEE = 0.001 ether;

    // ============================================
    // ENUMS
    // ============================================

    /**
     * @notice Types of tokens that can be deployed
     */
    enum TokenType {
        ERC20_STANDARD,
        ERC20_MINTABLE,
        ERC20_BURNABLE,
        ERC20_PAUSABLE,
        ERC20_VOTES,
        ERC20_PERMIT,
        ERC20_SNAPSHOT,
        ERC20_CAPPED,
        ERC20_FLASH_MINT,
        ERC721_STANDARD,
        ERC721_ENUMERABLE,
        ERC721_ROYALTY,
        ERC721_URI_STORAGE,
        ERC721_VOTES,
        ERC721_CONSECUTIVE,
        ERC1155_STANDARD,
        ERC1155_SUPPLY,
        ERC1155_BURNABLE,
        ERC1155_PAUSABLE,
        SOULBOUND_TOKEN,
        MULTI_TOKEN_HYBRID
    }

    // ============================================
    // STRUCTS
    // ============================================

    /**
     * @notice Template information for token deployment
     * @param implementation Address of the implementation contract
     * @param isActive Whether the template is currently active
     * @param deploymentFee Fee required to deploy this template
     * @param totalDeployments Number of times this template has been deployed
     * @param minInitialSupply Minimum initial supply (for fungible tokens)
     * @param maxInitialSupply Maximum initial supply (for fungible tokens)
     * @param createdAt Timestamp when template was added
     * @param lastDeployedAt Timestamp of last deployment
     */
    struct TokenTemplate {
        address implementation;
        bool isActive;
        uint256 deploymentFee;
        uint256 totalDeployments;
        uint256 minInitialSupply;
        uint256 maxInitialSupply;
        uint256 createdAt;
        uint256 lastDeployedAt;
    }

    /**
     * @notice Deployment record for tracking created tokens
     * @param tokenAddress Address of the deployed token
     * @param tokenType Type of deployed token
     * @param deployer Address that deployed the token
     * @param deployedAt Timestamp of deployment
     * @param initData Initialization data hash
     * @param feePaid Fee paid for deployment
     */
    struct DeploymentRecord {
        address tokenAddress;
        TokenType tokenType;
        address deployer;
        uint256 deployedAt;
        bytes32 initData;
        uint256 feePaid;
    }

    /**
     * @notice Configuration for ERC20 token initialization
     */
    struct ERC20Config {
        string name;
        string symbol;
        uint8 decimals;
        uint256 initialSupply;
        uint256 maxSupply;
        address owner;
        bool mintable;
        bool burnable;
        bool pausable;
        address[] initialHolders;
        uint256[] initialAmounts;
    }

    /**
     * @notice Configuration for ERC721 token initialization
     */
    struct ERC721Config {
        string name;
        string symbol;
        string baseURI;
        address owner;
        uint256 maxSupply;
        uint96 royaltyBps;
        address royaltyReceiver;
        bool enumerable;
        bool pausable;
    }

    /**
     * @notice Configuration for ERC1155 token initialization
     */
    struct ERC1155Config {
        string uri;
        address owner;
        bool pausable;
        bool supplyTracking;
        uint256[] initialTokenIds;
        uint256[] initialSupplies;
        address[] initialReceivers;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Mapping from token type to template
    mapping(TokenType => TokenTemplate) public templates;
    
    /// @notice Array of all deployed token addresses
    address[] public allDeployedTokens;
    
    /// @notice Mapping from token address to deployment record
    mapping(address => DeploymentRecord) public deploymentRecords;
    
    /// @notice Mapping from deployer to their deployed tokens
    mapping(address => address[]) public deployerTokens;
    
    /// @notice Total fees collected by the factory
    uint256 public totalFeesCollected;
    
    /// @notice Fee recipient address
    address public feeRecipient;
    
    /// @notice Default deployment fee when template fee is 0
    uint256 public defaultDeploymentFee;
    
    /// @notice Discount for bulk deployments (basis points)
    uint256 public bulkDiscountBps;
    
    /// @notice Minimum deployments for bulk discount
    uint256 public bulkDiscountThreshold;
    
    /// @notice Mapping of whitelisted deployers (no fee)
    mapping(address => bool) public whitelistedDeployers;
    
    /// @notice Deployment counter for deterministic addressing
    uint256 public deploymentNonce;

    // ============================================
    // EVENTS
    // ============================================

    /**
     * @notice Emitted when a new token is deployed
     * @param tokenAddress Address of the deployed token
     * @param tokenType Type of the deployed token
     * @param deployer Address that deployed the token
     * @param feePaid Fee paid for deployment
     */
    event TokenDeployed(
        address indexed tokenAddress,
        TokenType indexed tokenType,
        address indexed deployer,
        uint256 feePaid
    );

    /**
     * @notice Emitted when a template is added or updated
     * @param tokenType Type of the token template
     * @param implementation Address of the implementation
     * @param deploymentFee Fee for deploying this template
     */
    event TemplateUpdated(
        TokenType indexed tokenType,
        address indexed implementation,
        uint256 deploymentFee
    );

    /**
     * @notice Emitted when a template is activated or deactivated
     * @param tokenType Type of the token template
     * @param isActive New active status
     */
    event TemplateStatusChanged(TokenType indexed tokenType, bool isActive);

    /**
     * @notice Emitted when fees are withdrawn
     * @param recipient Address receiving the fees
     * @param amount Amount withdrawn
     */
    event FeesWithdrawn(address indexed recipient, uint256 amount);

    /**
     * @notice Emitted when fee recipient is updated
     * @param oldRecipient Previous fee recipient
     * @param newRecipient New fee recipient
     */
    event FeeRecipientUpdated(address indexed oldRecipient, address indexed newRecipient);

    /**
     * @notice Emitted when default deployment fee is updated
     * @param oldFee Previous default fee
     * @param newFee New default fee
     */
    event DefaultFeeUpdated(uint256 oldFee, uint256 newFee);

    /**
     * @notice Emitted when a deployer is whitelisted/removed
     * @param deployer Address of the deployer
     * @param status Whitelist status
     */
    event DeployerWhitelistUpdated(address indexed deployer, bool status);

    /**
     * @notice Emitted when bulk discount parameters are updated
     * @param discountBps New discount in basis points
     * @param threshold New threshold for bulk discount
     */
    event BulkDiscountUpdated(uint256 discountBps, uint256 threshold);

    // ============================================
    // ERRORS
    // ============================================

    /// @notice Template does not exist or is inactive
    error TemplateNotActive(TokenType tokenType);
    
    /// @notice Insufficient fee provided
    error InsufficientFee(uint256 required, uint256 provided);
    
    /// @notice Invalid implementation address
    error InvalidImplementation();
    
    /// @notice Invalid fee amount
    error InvalidFee(uint256 fee);
    
    /// @notice Invalid configuration
    error InvalidConfig(string reason);
    
    /// @notice Deployment failed
    error DeploymentFailed();
    
    /// @notice Initialization failed
    error InitializationFailed();
    
    /// @notice Invalid recipient
    error InvalidRecipient();
    
    /// @notice No fees to withdraw
    error NoFeesToWithdraw();
    
    /// @notice Arrays length mismatch
    error ArrayLengthMismatch();
    
    /// @notice Supply exceeds maximum
    error SupplyExceedsMax(uint256 requested, uint256 max);
    
    /// @notice Supply below minimum
    error SupplyBelowMin(uint256 requested, uint256 min);

    // ============================================
    // MODIFIERS
    // ============================================

    /**
     * @notice Ensures template exists and is active
     * @param tokenType Type of token template
     */
    modifier templateActive(TokenType tokenType) {
        if (!templates[tokenType].isActive || templates[tokenType].implementation == address(0)) {
            revert TemplateNotActive(tokenType);
        }
        _;
    }

    // ============================================
    // CONSTRUCTOR
    // ============================================

    /**
     * @notice Initializes the TokenFactory
     * @param _feeRecipient Address to receive deployment fees
     * @param _defaultFee Default deployment fee
     */
    constructor(address _feeRecipient, uint256 _defaultFee) {
        if (_feeRecipient == address(0)) revert InvalidRecipient();
        if (_defaultFee > MAX_DEPLOYMENT_FEE) revert InvalidFee(_defaultFee);

        feeRecipient = _feeRecipient;
        defaultDeploymentFee = _defaultFee;
        bulkDiscountBps = 1000; // 10% discount
        bulkDiscountThreshold = 5;

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(TEMPLATE_ADMIN_ROLE, msg.sender);
        _grantRole(PAUSER_ROLE, msg.sender);
        _grantRole(FEE_MANAGER_ROLE, msg.sender);
    }

    // ============================================
    // TEMPLATE MANAGEMENT
    // ============================================

    /**
     * @notice Adds or updates a token template
     * @param tokenType Type of the token
     * @param implementation Address of the implementation contract
     * @param deploymentFee Fee for deploying this template
     * @param minSupply Minimum initial supply
     * @param maxSupply Maximum initial supply
     */
    function setTemplate(
        TokenType tokenType,
        address implementation,
        uint256 deploymentFee,
        uint256 minSupply,
        uint256 maxSupply
    ) external onlyRole(TEMPLATE_ADMIN_ROLE) {
        if (implementation == address(0)) revert InvalidImplementation();
        if (deploymentFee > MAX_DEPLOYMENT_FEE) revert InvalidFee(deploymentFee);
        if (maxSupply > 0 && minSupply > maxSupply) {
            revert InvalidConfig("min > max supply");
        }

        TokenTemplate storage template = templates[tokenType];
        template.implementation = implementation;
        template.deploymentFee = deploymentFee;
        template.minInitialSupply = minSupply;
        template.maxInitialSupply = maxSupply;
        
        if (template.createdAt == 0) {
            template.createdAt = block.timestamp;
            template.isActive = true;
        }

        emit TemplateUpdated(tokenType, implementation, deploymentFee);
    }

    /**
     * @notice Activates or deactivates a template
     * @param tokenType Type of the token
     * @param isActive New active status
     */
    function setTemplateStatus(
        TokenType tokenType,
        bool isActive
    ) external onlyRole(TEMPLATE_ADMIN_ROLE) {
        templates[tokenType].isActive = isActive;
        emit TemplateStatusChanged(tokenType, isActive);
    }

    /**
     * @notice Batch updates multiple templates
     * @param tokenTypes Array of token types
     * @param implementations Array of implementation addresses
     * @param fees Array of deployment fees
     */
    function batchSetTemplates(
        TokenType[] calldata tokenTypes,
        address[] calldata implementations,
        uint256[] calldata fees
    ) external onlyRole(TEMPLATE_ADMIN_ROLE) {
        uint256 length = tokenTypes.length;
        if (length != implementations.length || length != fees.length) {
            revert ArrayLengthMismatch();
        }

        for (uint256 i = 0; i < length; i++) {
            if (implementations[i] == address(0)) revert InvalidImplementation();
            if (fees[i] > MAX_DEPLOYMENT_FEE) revert InvalidFee(fees[i]);

            TokenTemplate storage template = templates[tokenTypes[i]];
            template.implementation = implementations[i];
            template.deploymentFee = fees[i];
            
            if (template.createdAt == 0) {
                template.createdAt = block.timestamp;
                template.isActive = true;
            }

            emit TemplateUpdated(tokenTypes[i], implementations[i], fees[i]);
        }
    }

    // ============================================
    // ERC20 DEPLOYMENT
    // ============================================

    /**
     * @notice Deploys a new ERC20 token using the clone pattern
     * @param tokenType Type of ERC20 to deploy
     * @param config Configuration for the token
     * @return tokenAddress Address of the deployed token
     */
    function deployERC20(
        TokenType tokenType,
        ERC20Config calldata config
    ) 
        external 
        payable 
        nonReentrant 
        whenNotPaused 
        templateActive(tokenType) 
        returns (address tokenAddress) 
    {
        // Validate token type is ERC20
        if (uint8(tokenType) > uint8(TokenType.ERC20_FLASH_MINT)) {
            revert InvalidConfig("Not an ERC20 type");
        }

        // Validate configuration
        _validateERC20Config(tokenType, config);

        // Calculate and collect fee
        uint256 fee = _collectDeploymentFee(tokenType);

        // Clone the implementation
        tokenAddress = templates[tokenType].implementation.clone();
        if (tokenAddress == address(0)) revert DeploymentFailed();

        // Initialize the token
        bytes memory initData = _buildERC20InitData(config);
        (bool success,) = tokenAddress.call(initData);
        if (!success) revert InitializationFailed();

        // Record deployment
        _recordDeployment(tokenAddress, tokenType, fee, keccak256(initData));

        emit TokenDeployed(tokenAddress, tokenType, msg.sender, fee);
    }

    /**
     * @notice Deploys ERC20 with deterministic address using create2
     * @param tokenType Type of ERC20 to deploy
     * @param config Configuration for the token
     * @param salt Salt for deterministic deployment
     * @return tokenAddress Address of the deployed token
     */
    function deployERC20Deterministic(
        TokenType tokenType,
        ERC20Config calldata config,
        bytes32 salt
    )
        external
        payable
        nonReentrant
        whenNotPaused
        templateActive(tokenType)
        returns (address tokenAddress)
    {
        // Validate token type is ERC20
        if (uint8(tokenType) > uint8(TokenType.ERC20_FLASH_MINT)) {
            revert InvalidConfig("Not an ERC20 type");
        }

        // Validate configuration
        _validateERC20Config(tokenType, config);

        // Calculate and collect fee
        uint256 fee = _collectDeploymentFee(tokenType);

        // Clone with deterministic address
        bytes32 finalSalt = keccak256(abi.encodePacked(msg.sender, salt, deploymentNonce++));
        tokenAddress = templates[tokenType].implementation.cloneDeterministic(finalSalt);
        if (tokenAddress == address(0)) revert DeploymentFailed();

        // Initialize the token
        bytes memory initData = _buildERC20InitData(config);
        (bool success,) = tokenAddress.call(initData);
        if (!success) revert InitializationFailed();

        // Record deployment
        _recordDeployment(tokenAddress, tokenType, fee, keccak256(initData));

        emit TokenDeployed(tokenAddress, tokenType, msg.sender, fee);
    }

    /**
     * @notice Validates ERC20 configuration
     * @param tokenType Type of ERC20
     * @param config Configuration to validate
     */
    function _validateERC20Config(
        TokenType tokenType,
        ERC20Config calldata config
    ) internal view {
        if (bytes(config.name).length == 0) revert InvalidConfig("Empty name");
        if (bytes(config.symbol).length == 0) revert InvalidConfig("Empty symbol");
        if (config.owner == address(0)) revert InvalidConfig("Zero owner");
        if (config.decimals > 18) revert InvalidConfig("Decimals > 18");

        TokenTemplate storage template = templates[tokenType];
        if (template.maxInitialSupply > 0 && config.initialSupply > template.maxInitialSupply) {
            revert SupplyExceedsMax(config.initialSupply, template.maxInitialSupply);
        }
        if (config.initialSupply < template.minInitialSupply) {
            revert SupplyBelowMin(config.initialSupply, template.minInitialSupply);
        }

        if (config.initialHolders.length != config.initialAmounts.length) {
            revert ArrayLengthMismatch();
        }
    }

    /**
     * @notice Builds initialization data for ERC20
     * @param config Configuration data
     * @return initData Encoded initialization call
     */
    function _buildERC20InitData(
        ERC20Config calldata config
    ) internal pure returns (bytes memory) {
        return abi.encodeWithSignature(
            "initialize(string,string,uint8,uint256,uint256,address,bool,bool,bool,address[],uint256[])",
            config.name,
            config.symbol,
            config.decimals,
            config.initialSupply,
            config.maxSupply,
            config.owner,
            config.mintable,
            config.burnable,
            config.pausable,
            config.initialHolders,
            config.initialAmounts
        );
    }

    // ============================================
    // ERC721 DEPLOYMENT
    // ============================================

    /**
     * @notice Deploys a new ERC721 token
     * @param tokenType Type of ERC721 to deploy
     * @param config Configuration for the token
     * @return tokenAddress Address of the deployed token
     */
    function deployERC721(
        TokenType tokenType,
        ERC721Config calldata config
    )
        external
        payable
        nonReentrant
        whenNotPaused
        templateActive(tokenType)
        returns (address tokenAddress)
    {
        // Validate token type is ERC721
        if (
            uint8(tokenType) < uint8(TokenType.ERC721_STANDARD) ||
            uint8(tokenType) > uint8(TokenType.ERC721_CONSECUTIVE)
        ) {
            revert InvalidConfig("Not an ERC721 type");
        }

        // Validate configuration
        _validateERC721Config(config);

        // Calculate and collect fee
        uint256 fee = _collectDeploymentFee(tokenType);

        // Clone the implementation
        tokenAddress = templates[tokenType].implementation.clone();
        if (tokenAddress == address(0)) revert DeploymentFailed();

        // Initialize the token
        bytes memory initData = _buildERC721InitData(config);
        (bool success,) = tokenAddress.call(initData);
        if (!success) revert InitializationFailed();

        // Record deployment
        _recordDeployment(tokenAddress, tokenType, fee, keccak256(initData));

        emit TokenDeployed(tokenAddress, tokenType, msg.sender, fee);
    }

    /**
     * @notice Validates ERC721 configuration
     * @param config Configuration to validate
     */
    function _validateERC721Config(ERC721Config calldata config) internal pure {
        if (bytes(config.name).length == 0) revert InvalidConfig("Empty name");
        if (bytes(config.symbol).length == 0) revert InvalidConfig("Empty symbol");
        if (config.owner == address(0)) revert InvalidConfig("Zero owner");
        if (config.royaltyBps > 10000) revert InvalidConfig("Royalty > 100%");
        if (config.royaltyBps > 0 && config.royaltyReceiver == address(0)) {
            revert InvalidConfig("Zero royalty receiver");
        }
    }

    /**
     * @notice Builds initialization data for ERC721
     * @param config Configuration data
     * @return initData Encoded initialization call
     */
    function _buildERC721InitData(
        ERC721Config calldata config
    ) internal pure returns (bytes memory) {
        return abi.encodeWithSignature(
            "initialize(string,string,string,address,uint256,uint96,address,bool,bool)",
            config.name,
            config.symbol,
            config.baseURI,
            config.owner,
            config.maxSupply,
            config.royaltyBps,
            config.royaltyReceiver,
            config.enumerable,
            config.pausable
        );
    }

    // ============================================
    // ERC1155 DEPLOYMENT
    // ============================================

    /**
     * @notice Deploys a new ERC1155 token
     * @param tokenType Type of ERC1155 to deploy
     * @param config Configuration for the token
     * @return tokenAddress Address of the deployed token
     */
    function deployERC1155(
        TokenType tokenType,
        ERC1155Config calldata config
    )
        external
        payable
        nonReentrant
        whenNotPaused
        templateActive(tokenType)
        returns (address tokenAddress)
    {
        // Validate token type is ERC1155
        if (
            uint8(tokenType) < uint8(TokenType.ERC1155_STANDARD) ||
            uint8(tokenType) > uint8(TokenType.ERC1155_PAUSABLE)
        ) {
            revert InvalidConfig("Not an ERC1155 type");
        }

        // Validate configuration
        _validateERC1155Config(config);

        // Calculate and collect fee
        uint256 fee = _collectDeploymentFee(tokenType);

        // Clone the implementation
        tokenAddress = templates[tokenType].implementation.clone();
        if (tokenAddress == address(0)) revert DeploymentFailed();

        // Initialize the token
        bytes memory initData = _buildERC1155InitData(config);
        (bool success,) = tokenAddress.call(initData);
        if (!success) revert InitializationFailed();

        // Record deployment
        _recordDeployment(tokenAddress, tokenType, fee, keccak256(initData));

        emit TokenDeployed(tokenAddress, tokenType, msg.sender, fee);
    }

    /**
     * @notice Validates ERC1155 configuration
     * @param config Configuration to validate
     */
    function _validateERC1155Config(ERC1155Config calldata config) internal pure {
        if (bytes(config.uri).length == 0) revert InvalidConfig("Empty URI");
        if (config.owner == address(0)) revert InvalidConfig("Zero owner");
        
        if (config.initialTokenIds.length != config.initialSupplies.length) {
            revert ArrayLengthMismatch();
        }
        if (config.initialReceivers.length > 0 && 
            config.initialReceivers.length != config.initialTokenIds.length) {
            revert ArrayLengthMismatch();
        }
    }

    /**
     * @notice Builds initialization data for ERC1155
     * @param config Configuration data
     * @return initData Encoded initialization call
     */
    function _buildERC1155InitData(
        ERC1155Config calldata config
    ) internal pure returns (bytes memory) {
        return abi.encodeWithSignature(
            "initialize(string,address,bool,bool,uint256[],uint256[],address[])",
            config.uri,
            config.owner,
            config.pausable,
            config.supplyTracking,
            config.initialTokenIds,
            config.initialSupplies,
            config.initialReceivers
        );
    }

    // ============================================
    // SOULBOUND TOKEN DEPLOYMENT
    // ============================================

    /**
     * @notice Deploys a new Soulbound Token (non-transferable)
     * @param name Token name
     * @param symbol Token symbol
     * @param baseURI Base URI for metadata
     * @param owner Owner of the token contract
     * @return tokenAddress Address of the deployed token
     */
    function deploySoulboundToken(
        string calldata name,
        string calldata symbol,
        string calldata baseURI,
        address owner
    )
        external
        payable
        nonReentrant
        whenNotPaused
        templateActive(TokenType.SOULBOUND_TOKEN)
        returns (address tokenAddress)
    {
        if (bytes(name).length == 0) revert InvalidConfig("Empty name");
        if (bytes(symbol).length == 0) revert InvalidConfig("Empty symbol");
        if (owner == address(0)) revert InvalidConfig("Zero owner");

        // Calculate and collect fee
        uint256 fee = _collectDeploymentFee(TokenType.SOULBOUND_TOKEN);

        // Clone the implementation
        tokenAddress = templates[TokenType.SOULBOUND_TOKEN].implementation.clone();
        if (tokenAddress == address(0)) revert DeploymentFailed();

        // Initialize the token
        bytes memory initData = abi.encodeWithSignature(
            "initialize(string,string,string,address)",
            name,
            symbol,
            baseURI,
            owner
        );
        (bool success,) = tokenAddress.call(initData);
        if (!success) revert InitializationFailed();

        // Record deployment
        _recordDeployment(tokenAddress, TokenType.SOULBOUND_TOKEN, fee, keccak256(initData));

        emit TokenDeployed(tokenAddress, TokenType.SOULBOUND_TOKEN, msg.sender, fee);
    }

    // ============================================
    // FEE MANAGEMENT
    // ============================================

    /**
     * @notice Collects deployment fee from sender
     * @param tokenType Type of token being deployed
     * @return fee Amount of fee collected
     */
    function _collectDeploymentFee(TokenType tokenType) internal returns (uint256 fee) {
        // Whitelisted deployers don't pay fees
        if (whitelistedDeployers[msg.sender]) {
            return 0;
        }

        // Get template fee or use default
        fee = templates[tokenType].deploymentFee;
        if (fee == 0) {
            fee = defaultDeploymentFee;
        }

        // Apply bulk discount if applicable
        if (deployerTokens[msg.sender].length >= bulkDiscountThreshold) {
            fee = fee * (10000 - bulkDiscountBps) / 10000;
        }

        if (msg.value < fee) {
            revert InsufficientFee(fee, msg.value);
        }

        totalFeesCollected += fee;

        // Refund excess
        if (msg.value > fee) {
            (bool success,) = msg.sender.call{value: msg.value - fee}("");
            require(success, "Refund failed");
        }
    }

    /**
     * @notice Withdraws collected fees
     */
    function withdrawFees() external onlyRole(FEE_MANAGER_ROLE) nonReentrant {
        uint256 balance = address(this).balance;
        if (balance == 0) revert NoFeesToWithdraw();

        (bool success,) = feeRecipient.call{value: balance}("");
        require(success, "Transfer failed");

        emit FeesWithdrawn(feeRecipient, balance);
    }

    /**
     * @notice Updates the fee recipient
     * @param newRecipient New fee recipient address
     */
    function setFeeRecipient(address newRecipient) external onlyRole(FEE_MANAGER_ROLE) {
        if (newRecipient == address(0)) revert InvalidRecipient();
        
        address oldRecipient = feeRecipient;
        feeRecipient = newRecipient;
        
        emit FeeRecipientUpdated(oldRecipient, newRecipient);
    }

    /**
     * @notice Updates the default deployment fee
     * @param newFee New default fee
     */
    function setDefaultFee(uint256 newFee) external onlyRole(FEE_MANAGER_ROLE) {
        if (newFee > MAX_DEPLOYMENT_FEE) revert InvalidFee(newFee);
        
        uint256 oldFee = defaultDeploymentFee;
        defaultDeploymentFee = newFee;
        
        emit DefaultFeeUpdated(oldFee, newFee);
    }

    /**
     * @notice Updates bulk discount parameters
     * @param discountBps Discount in basis points (max 5000 = 50%)
     * @param threshold Minimum deployments for discount
     */
    function setBulkDiscount(
        uint256 discountBps,
        uint256 threshold
    ) external onlyRole(FEE_MANAGER_ROLE) {
        require(discountBps <= 5000, "Discount too high");
        require(threshold > 0, "Threshold must be > 0");
        
        bulkDiscountBps = discountBps;
        bulkDiscountThreshold = threshold;
        
        emit BulkDiscountUpdated(discountBps, threshold);
    }

    /**
     * @notice Adds or removes deployer from whitelist
     * @param deployer Address to update
     * @param status New whitelist status
     */
    function setDeployerWhitelist(
        address deployer,
        bool status
    ) external onlyRole(FEE_MANAGER_ROLE) {
        whitelistedDeployers[deployer] = status;
        emit DeployerWhitelistUpdated(deployer, status);
    }

    // ============================================
    // INTERNAL HELPERS
    // ============================================

    /**
     * @notice Records a deployment
     * @param tokenAddress Address of deployed token
     * @param tokenType Type of token
     * @param feePaid Fee paid for deployment
     * @param initDataHash Hash of initialization data
     */
    function _recordDeployment(
        address tokenAddress,
        TokenType tokenType,
        uint256 feePaid,
        bytes32 initDataHash
    ) internal {
        deploymentRecords[tokenAddress] = DeploymentRecord({
            tokenAddress: tokenAddress,
            tokenType: tokenType,
            deployer: msg.sender,
            deployedAt: block.timestamp,
            initData: initDataHash,
            feePaid: feePaid
        });

        allDeployedTokens.push(tokenAddress);
        deployerTokens[msg.sender].push(tokenAddress);
        
        templates[tokenType].totalDeployments++;
        templates[tokenType].lastDeployedAt = block.timestamp;
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Returns all tokens deployed by a specific address
     * @param deployer Address to query
     * @return Array of token addresses
     */
    function getDeployerTokens(address deployer) external view returns (address[] memory) {
        return deployerTokens[deployer];
    }

    /**
     * @notice Returns the total number of deployed tokens
     * @return Total count
     */
    function totalDeployedTokens() external view returns (uint256) {
        return allDeployedTokens.length;
    }

    /**
     * @notice Returns template information
     * @param tokenType Type to query
     * @return Template data
     */
    function getTemplate(TokenType tokenType) external view returns (TokenTemplate memory) {
        return templates[tokenType];
    }

    /**
     * @notice Predicts the address of a deterministic deployment
     * @param implementation Implementation address
     * @param salt Salt for deployment
     * @return Predicted address
     */
    function predictDeterministicAddress(
        address implementation,
        bytes32 salt
    ) external view returns (address) {
        bytes32 finalSalt = keccak256(abi.encodePacked(msg.sender, salt, deploymentNonce));
        return implementation.predictDeterministicAddress(finalSalt, address(this));
    }

    /**
     * @notice Calculates deployment fee for a token type
     * @param tokenType Type of token
     * @param deployer Address of deployer
     * @return fee Required fee
     */
    function calculateDeploymentFee(
        TokenType tokenType,
        address deployer
    ) external view returns (uint256 fee) {
        if (whitelistedDeployers[deployer]) {
            return 0;
        }

        fee = templates[tokenType].deploymentFee;
        if (fee == 0) {
            fee = defaultDeploymentFee;
        }

        if (deployerTokens[deployer].length >= bulkDiscountThreshold) {
            fee = fee * (10000 - bulkDiscountBps) / 10000;
        }
    }

    /**
     * @notice Returns paginated list of deployed tokens
     * @param offset Starting index
     * @param limit Maximum results
     * @return tokens Array of token addresses
     */
    function getDeployedTokensPaginated(
        uint256 offset,
        uint256 limit
    ) external view returns (address[] memory tokens) {
        uint256 total = allDeployedTokens.length;
        if (offset >= total) {
            return new address[](0);
        }

        uint256 end = offset + limit;
        if (end > total) {
            end = total;
        }

        tokens = new address[](end - offset);
        for (uint256 i = offset; i < end; i++) {
            tokens[i - offset] = allDeployedTokens[i];
        }
    }

    // ============================================
    // PAUSE FUNCTIONALITY
    // ============================================

    /**
     * @notice Pauses the factory
     */
    function pause() external onlyRole(PAUSER_ROLE) {
        _pause();
    }

    /**
     * @notice Unpauses the factory
     */
    function unpause() external onlyRole(PAUSER_ROLE) {
        _unpause();
    }

    // ============================================
    // RECEIVE
    // ============================================

    /**
     * @notice Allows factory to receive ETH
     */
    receive() external payable {}
}
