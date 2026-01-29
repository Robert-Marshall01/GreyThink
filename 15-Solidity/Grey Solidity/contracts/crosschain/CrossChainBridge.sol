// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts-upgradeable/access/AccessControlUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/PausableUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/ReentrancyGuardUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/Initializable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/UUPSUpgradeable.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/utils/cryptography/ECDSA.sol";
import "@openzeppelin/contracts/utils/cryptography/MessageHashUtils.sol";

/**
 * @title CrossChainBridge
 * @author Grey Protocol Team
 * @notice Cross-chain bridge supporting lock-and-mint / burn-and-unlock patterns
 * @dev Implements a multi-signature validator system for cross-chain message verification
 * 
 * Features:
 * - Lock-and-mint for native tokens
 * - Burn-and-unlock for wrapped tokens
 * - Multi-validator signature verification
 * - Rate limiting and daily limits
 * - Fee collection system
 * - Emergency pause functionality
 * - Nonce tracking for replay protection
 * - Batch operations support
 */
contract CrossChainBridge is 
    Initializable,
    AccessControlUpgradeable,
    PausableUpgradeable,
    ReentrancyGuardUpgradeable,
    UUPSUpgradeable
{
    using SafeERC20 for IERC20;
    using ECDSA for bytes32;
    using MessageHashUtils for bytes32;

    // ============================================
    // CONSTANTS & ROLES
    // ============================================

    bytes32 public constant VALIDATOR_ROLE = keccak256("VALIDATOR_ROLE");
    bytes32 public constant OPERATOR_ROLE = keccak256("OPERATOR_ROLE");
    bytes32 public constant FEE_MANAGER_ROLE = keccak256("FEE_MANAGER_ROLE");
    bytes32 public constant PAUSER_ROLE = keccak256("PAUSER_ROLE");
    bytes32 public constant UPGRADER_ROLE = keccak256("UPGRADER_ROLE");

    /// @notice Basis points denominator
    uint256 public constant BPS = 10000;
    
    /// @notice Maximum bridge fee (5%)
    uint256 public constant MAX_FEE = 500;
    
    /// @notice Maximum validators
    uint256 public constant MAX_VALIDATORS = 50;
    
    /// @notice Signature validity period
    uint256 public constant SIGNATURE_VALIDITY = 1 hours;

    // ============================================
    // ENUMS
    // ============================================

    /**
     * @notice Bridge operation types
     */
    enum OperationType {
        LOCK,           // Lock native tokens
        UNLOCK,         // Unlock native tokens
        MINT,           // Mint wrapped tokens
        BURN            // Burn wrapped tokens
    }

    /**
     * @notice Transfer status
     */
    enum TransferStatus {
        PENDING,
        COMPLETED,
        REFUNDED,
        CANCELLED
    }

    // ============================================
    // STRUCTS
    // ============================================

    /**
     * @notice Token configuration for bridging
     * @param isSupported Whether token is supported
     * @param isNative Whether this is the native chain for the token
     * @param wrappedToken Address of wrapped version (on destination chains)
     * @param minAmount Minimum bridge amount
     * @param maxAmount Maximum bridge amount per tx
     * @param dailyLimit Daily bridge limit
     * @param bridgeFee Fee in basis points
     * @param totalBridged Total amount bridged
     * @param dailyBridged Amount bridged today
     * @param lastDayReset Timestamp of last daily reset
     */
    struct TokenConfig {
        bool isSupported;
        bool isNative;
        address wrappedToken;
        uint256 minAmount;
        uint256 maxAmount;
        uint256 dailyLimit;
        uint256 bridgeFee;
        uint256 totalBridged;
        uint256 dailyBridged;
        uint256 lastDayReset;
    }

    /**
     * @notice Chain configuration
     * @param chainId Chain ID
     * @param isSupported Whether chain is supported
     * @param bridgeAddress Bridge contract address on that chain
     * @param requiredConfirmations Required block confirmations
     * @param gasLimit Gas limit for execution
     */
    struct ChainConfig {
        uint256 chainId;
        bool isSupported;
        address bridgeAddress;
        uint256 requiredConfirmations;
        uint256 gasLimit;
    }

    /**
     * @notice Bridge transfer request
     * @param id Unique transfer ID
     * @param sender Sender address
     * @param recipient Recipient address on destination
     * @param token Token address
     * @param amount Amount to bridge
     * @param fee Fee amount
     * @param sourceChain Source chain ID
     * @param destChain Destination chain ID
     * @param nonce Transfer nonce
     * @param timestamp Creation timestamp
     * @param deadline Execution deadline
     * @param status Transfer status
     * @param operationType Type of operation
     */
    struct BridgeTransfer {
        bytes32 id;
        address sender;
        address recipient;
        address token;
        uint256 amount;
        uint256 fee;
        uint256 sourceChain;
        uint256 destChain;
        uint256 nonce;
        uint256 timestamp;
        uint256 deadline;
        TransferStatus status;
        OperationType operationType;
    }

    /**
     * @notice Validator signature
     */
    struct ValidatorSignature {
        address validator;
        bytes signature;
        uint256 timestamp;
    }

    /**
     * @notice Message for cross-chain verification
     */
    struct BridgeMessage {
        bytes32 transferId;
        address sender;
        address recipient;
        address token;
        uint256 amount;
        uint256 sourceChain;
        uint256 destChain;
        uint256 nonce;
        uint256 deadline;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Current chain ID
    uint256 public chainId;

    /// @notice Token configurations
    mapping(address => TokenConfig) public tokenConfigs;
    
    /// @notice Supported tokens list
    address[] public supportedTokens;
    
    /// @notice Chain configurations
    mapping(uint256 => ChainConfig) public chainConfigs;
    
    /// @notice Supported chains list
    uint256[] public supportedChains;
    
    /// @notice Transfer records by ID
    mapping(bytes32 => BridgeTransfer) public transfers;
    
    /// @notice User transfer history
    mapping(address => bytes32[]) public userTransfers;
    
    /// @notice Processed transfer IDs (from source chain)
    mapping(bytes32 => bool) public processedTransfers;
    
    /// @notice User nonces for replay protection
    mapping(address => uint256) public userNonces;
    
    /// @notice Validator addresses
    address[] public validators;
    
    /// @notice Required signatures for verification
    uint256 public requiredSignatures;
    
    /// @notice Is validator mapping
    mapping(address => bool) public isValidator;
    
    /// @notice Transfer ID counter
    uint256 public transferCounter;
    
    /// @notice Total fees collected per token
    mapping(address => uint256) public collectedFees;
    
    /// @notice Fee recipient
    address public feeRecipient;
    
    /// @notice Emergency shutdown flag
    bool public emergencyShutdown;

    /// @notice Domain separator for EIP-712
    bytes32 public DOMAIN_SEPARATOR;

    /// @notice Storage gap
    uint256[40] private __gap;

    // ============================================
    // EVENTS
    // ============================================

    event TokenLocked(
        bytes32 indexed transferId,
        address indexed sender,
        address indexed token,
        uint256 amount,
        uint256 destChain,
        address recipient
    );

    event TokenUnlocked(
        bytes32 indexed transferId,
        address indexed recipient,
        address indexed token,
        uint256 amount,
        uint256 sourceChain
    );

    event TokenMinted(
        bytes32 indexed transferId,
        address indexed recipient,
        address indexed token,
        uint256 amount,
        uint256 sourceChain
    );

    event TokenBurned(
        bytes32 indexed transferId,
        address indexed sender,
        address indexed token,
        uint256 amount,
        uint256 destChain,
        address recipient
    );

    event TransferCompleted(bytes32 indexed transferId, address indexed recipient, uint256 amount);
    event TransferRefunded(bytes32 indexed transferId, address indexed sender, uint256 amount);
    
    event TokenConfigUpdated(
        address indexed token,
        bool isSupported,
        bool isNative,
        uint256 minAmount,
        uint256 maxAmount
    );
    
    event ChainConfigUpdated(
        uint256 indexed chainId,
        bool isSupported,
        address bridgeAddress
    );
    
    event ValidatorAdded(address indexed validator);
    event ValidatorRemoved(address indexed validator);
    event RequiredSignaturesUpdated(uint256 oldValue, uint256 newValue);
    
    event FeesWithdrawn(address indexed token, address indexed recipient, uint256 amount);
    event EmergencyShutdown(bool enabled);

    // ============================================
    // ERRORS
    // ============================================

    error TokenNotSupported(address token);
    error ChainNotSupported(uint256 chainId);
    error AmountTooLow(uint256 amount, uint256 minimum);
    error AmountTooHigh(uint256 amount, uint256 maximum);
    error DailyLimitExceeded(uint256 amount, uint256 remaining);
    error TransferAlreadyProcessed(bytes32 transferId);
    error TransferNotFound(bytes32 transferId);
    error TransferExpired(bytes32 transferId);
    error InvalidRecipient();
    error InvalidAmount();
    error InvalidSignature();
    error InsufficientSignatures(uint256 provided, uint256 required);
    error SignatureExpired();
    error DuplicateValidator();
    error NotValidator(address account);
    error ValidatorLimitReached();
    error EmergencyShutdownActive();
    error InvalidChainId();
    error SameChain();
    error TransferFailed();
    error NotNativeToken();
    error ZeroAddress();

    // ============================================
    // MODIFIERS
    // ============================================

    modifier notShutdown() {
        if (emergencyShutdown) revert EmergencyShutdownActive();
        _;
    }

    modifier tokenSupported(address token) {
        if (!tokenConfigs[token].isSupported) revert TokenNotSupported(token);
        _;
    }

    modifier chainSupported(uint256 _chainId) {
        if (!chainConfigs[_chainId].isSupported) revert ChainNotSupported(_chainId);
        _;
    }

    // ============================================
    // INITIALIZER
    // ============================================

    /**
     * @notice Initializes the bridge
     * @param admin Admin address
     * @param _feeRecipient Fee recipient address
     * @param _requiredSignatures Number of required validator signatures
     */
    function initialize(
        address admin,
        address _feeRecipient,
        uint256 _requiredSignatures
    ) public initializer {
        if (admin == address(0)) revert ZeroAddress();
        if (_feeRecipient == address(0)) revert ZeroAddress();

        __AccessControl_init();
        __Pausable_init();
        __ReentrancyGuard_init();
        __UUPSUpgradeable_init();

        chainId = block.chainid;
        feeRecipient = _feeRecipient;
        requiredSignatures = _requiredSignatures;

        DOMAIN_SEPARATOR = keccak256(
            abi.encode(
                keccak256("EIP712Domain(string name,string version,uint256 chainId,address verifyingContract)"),
                keccak256("CrossChainBridge"),
                keccak256("1"),
                block.chainid,
                address(this)
            )
        );

        _grantRole(DEFAULT_ADMIN_ROLE, admin);
        _grantRole(OPERATOR_ROLE, admin);
        _grantRole(FEE_MANAGER_ROLE, admin);
        _grantRole(PAUSER_ROLE, admin);
        _grantRole(UPGRADER_ROLE, admin);
    }

    // ============================================
    // LOCK & MINT (Native Chain -> Destination)
    // ============================================

    /**
     * @notice Locks tokens on native chain for bridging
     * @param token Token to lock
     * @param amount Amount to lock
     * @param destChain Destination chain ID
     * @param recipient Recipient on destination chain
     * @param deadline Transaction deadline
     * @return transferId The transfer ID
     */
    function lock(
        address token,
        uint256 amount,
        uint256 destChain,
        address recipient,
        uint256 deadline
    )
        external
        nonReentrant
        whenNotPaused
        notShutdown
        tokenSupported(token)
        chainSupported(destChain)
        returns (bytes32 transferId)
    {
        if (recipient == address(0)) revert InvalidRecipient();
        if (destChain == chainId) revert SameChain();
        if (block.timestamp > deadline) revert TransferExpired(bytes32(0));

        TokenConfig storage config = tokenConfigs[token];
        if (!config.isNative) revert NotNativeToken();

        // Validate amount
        _validateAmount(token, amount);

        // Calculate fee
        uint256 fee = (amount * config.bridgeFee) / BPS;
        uint256 netAmount = amount - fee;

        // Generate transfer ID
        uint256 nonce = userNonces[msg.sender]++;
        transferId = _generateTransferId(msg.sender, recipient, token, amount, destChain, nonce);

        // Transfer tokens to bridge
        IERC20(token).safeTransferFrom(msg.sender, address(this), amount);

        // Record transfer
        transfers[transferId] = BridgeTransfer({
            id: transferId,
            sender: msg.sender,
            recipient: recipient,
            token: token,
            amount: netAmount,
            fee: fee,
            sourceChain: chainId,
            destChain: destChain,
            nonce: nonce,
            timestamp: block.timestamp,
            deadline: deadline,
            status: TransferStatus.PENDING,
            operationType: OperationType.LOCK
        });

        userTransfers[msg.sender].push(transferId);
        collectedFees[token] += fee;

        // Update daily volume
        _updateDailyVolume(token, amount);

        emit TokenLocked(transferId, msg.sender, token, netAmount, destChain, recipient);
    }

    /**
     * @notice Mints wrapped tokens on destination chain
     * @param message Bridge message
     * @param signatures Validator signatures
     */
    function mint(
        BridgeMessage calldata message,
        ValidatorSignature[] calldata signatures
    )
        external
        nonReentrant
        whenNotPaused
        notShutdown
    {
        // Verify not already processed
        if (processedTransfers[message.transferId]) {
            revert TransferAlreadyProcessed(message.transferId);
        }

        // Verify deadline
        if (block.timestamp > message.deadline) {
            revert TransferExpired(message.transferId);
        }

        // Verify chain
        if (message.destChain != chainId) revert InvalidChainId();

        // Verify token is supported and has wrapped version
        TokenConfig storage config = tokenConfigs[message.token];
        if (!config.isSupported) revert TokenNotSupported(message.token);
        if (config.isNative) revert NotNativeToken(); // Should be wrapped on dest chain

        // Verify signatures
        _verifySignatures(message, signatures);

        // Mark as processed
        processedTransfers[message.transferId] = true;

        // Mint wrapped tokens
        address wrappedToken = config.wrappedToken;
        
        // Call mint on wrapped token (assumes IMintable interface)
        (bool success,) = wrappedToken.call(
            abi.encodeWithSignature("mint(address,uint256)", message.recipient, message.amount)
        );
        if (!success) revert TransferFailed();

        emit TokenMinted(message.transferId, message.recipient, wrappedToken, message.amount, message.sourceChain);
        emit TransferCompleted(message.transferId, message.recipient, message.amount);
    }

    // ============================================
    // BURN & UNLOCK (Destination Chain -> Native)
    // ============================================

    /**
     * @notice Burns wrapped tokens for unlocking on native chain
     * @param token Wrapped token to burn
     * @param amount Amount to burn
     * @param destChain Destination (native) chain ID
     * @param recipient Recipient on native chain
     * @param deadline Transaction deadline
     * @return transferId The transfer ID
     */
    function burn(
        address token,
        uint256 amount,
        uint256 destChain,
        address recipient,
        uint256 deadline
    )
        external
        nonReentrant
        whenNotPaused
        notShutdown
        tokenSupported(token)
        chainSupported(destChain)
        returns (bytes32 transferId)
    {
        if (recipient == address(0)) revert InvalidRecipient();
        if (destChain == chainId) revert SameChain();
        if (block.timestamp > deadline) revert TransferExpired(bytes32(0));

        TokenConfig storage config = tokenConfigs[token];
        
        // Validate amount
        _validateAmount(token, amount);

        // Calculate fee
        uint256 fee = (amount * config.bridgeFee) / BPS;
        uint256 netAmount = amount - fee;

        // Generate transfer ID
        uint256 nonce = userNonces[msg.sender]++;
        transferId = _generateTransferId(msg.sender, recipient, token, amount, destChain, nonce);

        // Burn tokens (assumes IBurnable interface)
        (bool success,) = token.call(
            abi.encodeWithSignature("burnFrom(address,uint256)", msg.sender, amount)
        );
        if (!success) {
            // Fallback: transfer and burn
            IERC20(token).safeTransferFrom(msg.sender, address(this), amount);
            (success,) = token.call(abi.encodeWithSignature("burn(uint256)", amount - fee));
            if (!success) revert TransferFailed();
        }

        // Record transfer
        transfers[transferId] = BridgeTransfer({
            id: transferId,
            sender: msg.sender,
            recipient: recipient,
            token: token,
            amount: netAmount,
            fee: fee,
            sourceChain: chainId,
            destChain: destChain,
            nonce: nonce,
            timestamp: block.timestamp,
            deadline: deadline,
            status: TransferStatus.PENDING,
            operationType: OperationType.BURN
        });

        userTransfers[msg.sender].push(transferId);
        collectedFees[token] += fee;

        emit TokenBurned(transferId, msg.sender, token, netAmount, destChain, recipient);
    }

    /**
     * @notice Unlocks tokens on native chain
     * @param message Bridge message
     * @param signatures Validator signatures
     */
    function unlock(
        BridgeMessage calldata message,
        ValidatorSignature[] calldata signatures
    )
        external
        nonReentrant
        whenNotPaused
        notShutdown
    {
        // Verify not already processed
        if (processedTransfers[message.transferId]) {
            revert TransferAlreadyProcessed(message.transferId);
        }

        // Verify deadline
        if (block.timestamp > message.deadline) {
            revert TransferExpired(message.transferId);
        }

        // Verify chain
        if (message.destChain != chainId) revert InvalidChainId();

        // Verify token is native here
        TokenConfig storage config = tokenConfigs[message.token];
        if (!config.isSupported) revert TokenNotSupported(message.token);
        if (!config.isNative) revert NotNativeToken();

        // Verify signatures
        _verifySignatures(message, signatures);

        // Mark as processed
        processedTransfers[message.transferId] = true;

        // Transfer locked tokens to recipient
        IERC20(message.token).safeTransfer(message.recipient, message.amount);

        emit TokenUnlocked(message.transferId, message.recipient, message.token, message.amount, message.sourceChain);
        emit TransferCompleted(message.transferId, message.recipient, message.amount);
    }

    // ============================================
    // SIGNATURE VERIFICATION
    // ============================================

    /**
     * @notice Verifies validator signatures
     * @param message Bridge message
     * @param signatures Array of validator signatures
     */
    function _verifySignatures(
        BridgeMessage calldata message,
        ValidatorSignature[] calldata signatures
    ) internal view {
        if (signatures.length < requiredSignatures) {
            revert InsufficientSignatures(signatures.length, requiredSignatures);
        }

        // Create message hash
        bytes32 messageHash = keccak256(abi.encode(
            message.transferId,
            message.sender,
            message.recipient,
            message.token,
            message.amount,
            message.sourceChain,
            message.destChain,
            message.nonce,
            message.deadline
        ));

        bytes32 ethSignedHash = messageHash.toEthSignedMessageHash();

        address[] memory seenValidators = new address[](signatures.length);
        uint256 validCount = 0;

        for (uint256 i = 0; i < signatures.length; i++) {
            ValidatorSignature calldata sig = signatures[i];
            
            // Verify timestamp
            if (block.timestamp > sig.timestamp + SIGNATURE_VALIDITY) {
                revert SignatureExpired();
            }

            // Recover signer
            address signer = ethSignedHash.recover(sig.signature);
            
            // Verify is validator
            if (!isValidator[signer]) revert NotValidator(signer);
            
            // Check for duplicates
            for (uint256 j = 0; j < validCount; j++) {
                if (seenValidators[j] == signer) revert DuplicateValidator();
            }
            
            seenValidators[validCount++] = signer;
        }

        if (validCount < requiredSignatures) {
            revert InsufficientSignatures(validCount, requiredSignatures);
        }
    }

    // ============================================
    // HELPER FUNCTIONS
    // ============================================

    /**
     * @notice Generates a unique transfer ID
     */
    function _generateTransferId(
        address sender,
        address recipient,
        address token,
        uint256 amount,
        uint256 destChain,
        uint256 nonce
    ) internal returns (bytes32) {
        return keccak256(abi.encodePacked(
            sender,
            recipient,
            token,
            amount,
            chainId,
            destChain,
            nonce,
            block.timestamp,
            transferCounter++
        ));
    }

    /**
     * @notice Validates bridge amount
     */
    function _validateAmount(address token, uint256 amount) internal view {
        TokenConfig storage config = tokenConfigs[token];
        
        if (amount < config.minAmount) {
            revert AmountTooLow(amount, config.minAmount);
        }
        if (amount > config.maxAmount) {
            revert AmountTooHigh(amount, config.maxAmount);
        }

        // Check daily limit
        uint256 remaining = _getRemainingDailyLimit(token);
        if (amount > remaining) {
            revert DailyLimitExceeded(amount, remaining);
        }
    }

    /**
     * @notice Updates daily volume tracking
     */
    function _updateDailyVolume(address token, uint256 amount) internal {
        TokenConfig storage config = tokenConfigs[token];
        
        // Reset if new day
        if (block.timestamp >= config.lastDayReset + 1 days) {
            config.dailyBridged = 0;
            config.lastDayReset = block.timestamp - (block.timestamp % 1 days);
        }
        
        config.dailyBridged += amount;
        config.totalBridged += amount;
    }

    /**
     * @notice Gets remaining daily limit
     */
    function _getRemainingDailyLimit(address token) internal view returns (uint256) {
        TokenConfig storage config = tokenConfigs[token];
        
        // Check if new day
        if (block.timestamp >= config.lastDayReset + 1 days) {
            return config.dailyLimit;
        }
        
        if (config.dailyBridged >= config.dailyLimit) {
            return 0;
        }
        
        return config.dailyLimit - config.dailyBridged;
    }

    // ============================================
    // TOKEN CONFIGURATION
    // ============================================

    /**
     * @notice Adds or updates token configuration
     */
    function setTokenConfig(
        address token,
        bool isSupported,
        bool isNative,
        address wrappedToken,
        uint256 minAmount,
        uint256 maxAmount,
        uint256 dailyLimit,
        uint256 bridgeFee
    ) external onlyRole(OPERATOR_ROLE) {
        if (token == address(0)) revert ZeroAddress();
        if (bridgeFee > MAX_FEE) revert InvalidAmount();
        if (minAmount > maxAmount) revert InvalidAmount();

        bool isNew = !tokenConfigs[token].isSupported;

        tokenConfigs[token] = TokenConfig({
            isSupported: isSupported,
            isNative: isNative,
            wrappedToken: wrappedToken,
            minAmount: minAmount,
            maxAmount: maxAmount,
            dailyLimit: dailyLimit,
            bridgeFee: bridgeFee,
            totalBridged: tokenConfigs[token].totalBridged,
            dailyBridged: tokenConfigs[token].dailyBridged,
            lastDayReset: tokenConfigs[token].lastDayReset
        });

        if (isNew && isSupported) {
            supportedTokens.push(token);
        }

        emit TokenConfigUpdated(token, isSupported, isNative, minAmount, maxAmount);
    }

    /**
     * @notice Adds or updates chain configuration
     */
    function setChainConfig(
        uint256 _chainId,
        bool isSupported,
        address bridgeAddress,
        uint256 requiredConfirmations,
        uint256 gasLimit
    ) external onlyRole(OPERATOR_ROLE) {
        if (bridgeAddress == address(0) && isSupported) revert ZeroAddress();

        bool isNew = !chainConfigs[_chainId].isSupported;

        chainConfigs[_chainId] = ChainConfig({
            chainId: _chainId,
            isSupported: isSupported,
            bridgeAddress: bridgeAddress,
            requiredConfirmations: requiredConfirmations,
            gasLimit: gasLimit
        });

        if (isNew && isSupported) {
            supportedChains.push(_chainId);
        }

        emit ChainConfigUpdated(_chainId, isSupported, bridgeAddress);
    }

    // ============================================
    // VALIDATOR MANAGEMENT
    // ============================================

    /**
     * @notice Adds a validator
     */
    function addValidator(address validator) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (validator == address(0)) revert ZeroAddress();
        if (isValidator[validator]) revert DuplicateValidator();
        if (validators.length >= MAX_VALIDATORS) revert ValidatorLimitReached();

        isValidator[validator] = true;
        validators.push(validator);
        _grantRole(VALIDATOR_ROLE, validator);

        emit ValidatorAdded(validator);
    }

    /**
     * @notice Removes a validator
     */
    function removeValidator(address validator) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (!isValidator[validator]) revert NotValidator(validator);

        isValidator[validator] = false;
        
        // Remove from array
        for (uint256 i = 0; i < validators.length; i++) {
            if (validators[i] == validator) {
                validators[i] = validators[validators.length - 1];
                validators.pop();
                break;
            }
        }

        _revokeRole(VALIDATOR_ROLE, validator);

        emit ValidatorRemoved(validator);
    }

    /**
     * @notice Updates required signatures
     */
    function setRequiredSignatures(uint256 _required) external onlyRole(DEFAULT_ADMIN_ROLE) {
        require(_required > 0 && _required <= validators.length, "Invalid requirement");
        
        uint256 oldValue = requiredSignatures;
        requiredSignatures = _required;
        
        emit RequiredSignaturesUpdated(oldValue, _required);
    }

    // ============================================
    // FEE MANAGEMENT
    // ============================================

    /**
     * @notice Withdraws collected fees
     */
    function withdrawFees(address token) external onlyRole(FEE_MANAGER_ROLE) {
        uint256 amount = collectedFees[token];
        if (amount == 0) revert InvalidAmount();
        
        collectedFees[token] = 0;
        IERC20(token).safeTransfer(feeRecipient, amount);
        
        emit FeesWithdrawn(token, feeRecipient, amount);
    }

    /**
     * @notice Sets fee recipient
     */
    function setFeeRecipient(address _feeRecipient) external onlyRole(FEE_MANAGER_ROLE) {
        if (_feeRecipient == address(0)) revert ZeroAddress();
        feeRecipient = _feeRecipient;
    }

    // ============================================
    // EMERGENCY
    // ============================================

    /**
     * @notice Emergency shutdown
     */
    function setEmergencyShutdown(bool enabled) external onlyRole(DEFAULT_ADMIN_ROLE) {
        emergencyShutdown = enabled;
        if (enabled) {
            _pause();
        }
        emit EmergencyShutdown(enabled);
    }

    /**
     * @notice Refunds a pending transfer
     */
    function refundTransfer(bytes32 transferId) external onlyRole(OPERATOR_ROLE) {
        BridgeTransfer storage transfer = transfers[transferId];
        if (transfer.id == bytes32(0)) revert TransferNotFound(transferId);
        if (transfer.status != TransferStatus.PENDING) revert InvalidAmount();
        if (transfer.operationType != OperationType.LOCK) revert InvalidAmount();

        transfer.status = TransferStatus.REFUNDED;

        // Refund including fee
        IERC20(transfer.token).safeTransfer(transfer.sender, transfer.amount + transfer.fee);
        collectedFees[transfer.token] -= transfer.fee;

        emit TransferRefunded(transferId, transfer.sender, transfer.amount + transfer.fee);
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    function getSupportedTokens() external view returns (address[] memory) {
        return supportedTokens;
    }

    function getSupportedChains() external view returns (uint256[] memory) {
        return supportedChains;
    }

    function getValidators() external view returns (address[] memory) {
        return validators;
    }

    function getUserTransfers(address user) external view returns (bytes32[] memory) {
        return userTransfers[user];
    }

    function getTransfer(bytes32 transferId) external view returns (BridgeTransfer memory) {
        return transfers[transferId];
    }

    function getRemainingDailyLimit(address token) external view returns (uint256) {
        return _getRemainingDailyLimit(token);
    }

    // ============================================
    // ADMIN
    // ============================================

    function pause() external onlyRole(PAUSER_ROLE) { _pause(); }
    function unpause() external onlyRole(PAUSER_ROLE) { _unpause(); }
    function _authorizeUpgrade(address) internal override onlyRole(UPGRADER_ROLE) {}
}
