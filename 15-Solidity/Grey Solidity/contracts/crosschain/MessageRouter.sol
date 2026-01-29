// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts-upgradeable/access/AccessControlUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/PausableUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/ReentrancyGuardUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/Initializable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/UUPSUpgradeable.sol";
import "@openzeppelin/contracts/utils/cryptography/ECDSA.sol";
import "@openzeppelin/contracts/utils/cryptography/MessageHashUtils.sol";

/**
 * @title MessageRouter
 * @author Grey Protocol Team
 * @notice Cross-chain message routing protocol for arbitrary data transmission
 * @dev Implements a generalized message passing system between chains
 * 
 * Features:
 * - Arbitrary message passing between chains
 * - Multiple message types (broadcast, targeted, callback)
 * - Gas estimation and fee calculation
 * - Message ordering and sequencing
 * - Retry mechanisms for failed messages
 * - Callback confirmation system
 * - Message batching for efficiency
 * - Adapter pattern for multiple protocols
 */
contract MessageRouter is
    Initializable,
    AccessControlUpgradeable,
    PausableUpgradeable,
    ReentrancyGuardUpgradeable,
    UUPSUpgradeable
{
    using ECDSA for bytes32;
    using MessageHashUtils for bytes32;

    // ============================================
    // CONSTANTS & ROLES
    // ============================================

    bytes32 public constant RELAYER_ROLE = keccak256("RELAYER_ROLE");
    bytes32 public constant OPERATOR_ROLE = keccak256("OPERATOR_ROLE");
    bytes32 public constant FEE_MANAGER_ROLE = keccak256("FEE_MANAGER_ROLE");
    bytes32 public constant UPGRADER_ROLE = keccak256("UPGRADER_ROLE");

    /// @notice Maximum message size (64KB)
    uint256 public constant MAX_MESSAGE_SIZE = 65536;
    
    /// @notice Maximum retry count
    uint256 public constant MAX_RETRIES = 5;
    
    /// @notice Message expiry period
    uint256 public constant MESSAGE_EXPIRY = 7 days;
    
    /// @notice Callback timeout
    uint256 public constant CALLBACK_TIMEOUT = 1 hours;

    // ============================================
    // ENUMS
    // ============================================

    /**
     * @notice Message types
     */
    enum MessageType {
        STANDARD,           // One-way message
        CALLBACK,           // Expects response
        BROADCAST,          // Multi-chain broadcast
        BATCH,              // Batched messages
        ORDERED             // Strictly ordered
    }

    /**
     * @notice Message status
     */
    enum MessageStatus {
        PENDING,
        SENT,
        DELIVERED,
        EXECUTED,
        FAILED,
        EXPIRED,
        CALLBACK_PENDING,
        CALLBACK_RECEIVED
    }

    /**
     * @notice Adapter types for different protocols
     */
    enum AdapterType {
        NATIVE,             // Native bridge
        LAYER_ZERO,         // LayerZero
        AXELAR,             // Axelar
        WORMHOLE,           // Wormhole
        CCIP,               // Chainlink CCIP
        CUSTOM              // Custom adapter
    }

    // ============================================
    // STRUCTS
    // ============================================

    /**
     * @notice Cross-chain message
     * @param id Unique message ID
     * @param sender Sender address
     * @param receiver Receiver address on destination
     * @param sourceChain Source chain ID
     * @param destChain Destination chain ID
     * @param nonce Message nonce
     * @param messageType Type of message
     * @param payload Message payload
     * @param gasLimit Gas limit for execution
     * @param fee Fee paid
     * @param timestamp Creation timestamp
     * @param deadline Execution deadline
     * @param status Message status
     * @param retries Retry count
     * @param callbackId Callback message ID (if applicable)
     */
    struct Message {
        bytes32 id;
        address sender;
        address receiver;
        uint256 sourceChain;
        uint256 destChain;
        uint256 nonce;
        MessageType messageType;
        bytes payload;
        uint256 gasLimit;
        uint256 fee;
        uint256 timestamp;
        uint256 deadline;
        MessageStatus status;
        uint256 retries;
        bytes32 callbackId;
    }

    /**
     * @notice Message receipt for confirmations
     */
    struct MessageReceipt {
        bytes32 messageId;
        uint256 sourceChain;
        uint256 destChain;
        bool success;
        bytes returnData;
        uint256 gasUsed;
        uint256 timestamp;
    }

    /**
     * @notice Chain routing configuration
     * @param chainId Chain ID
     * @param isActive Whether routing is active
     * @param router Router address on destination
     * @param adapter Preferred adapter type
     * @param baseFee Base fee for messages
     * @param gasPrice Estimated gas price
     * @param maxGasLimit Maximum gas limit
     * @param batchSize Maximum batch size
     * @param totalMessages Total messages sent
     * @param failedMessages Failed message count
     */
    struct ChainRoute {
        uint256 chainId;
        bool isActive;
        address router;
        AdapterType adapter;
        uint256 baseFee;
        uint256 gasPrice;
        uint256 maxGasLimit;
        uint256 batchSize;
        uint256 totalMessages;
        uint256 failedMessages;
    }

    /**
     * @notice Messaging adapter configuration
     */
    struct AdapterConfig {
        AdapterType adapterType;
        address adapterAddress;
        bool isActive;
        uint256 feeMultiplier;
        uint256 successRate;
        uint256 avgLatency;
    }

    /**
     * @notice Batch message request
     */
    struct BatchMessageRequest {
        address receiver;
        uint256 destChain;
        bytes payload;
        uint256 gasLimit;
    }

    /**
     * @notice Message filter for queries
     */
    struct MessageFilter {
        address sender;
        address receiver;
        uint256 sourceChain;
        uint256 destChain;
        MessageStatus status;
        uint256 fromTimestamp;
        uint256 toTimestamp;
    }

    /**
     * @notice Gas estimation result
     */
    struct GasEstimate {
        uint256 baseGas;
        uint256 payloadGas;
        uint256 totalGas;
        uint256 estimatedFee;
        uint256 maxFee;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Current chain ID
    uint256 public chainId;

    /// @notice Message counter for generating IDs
    uint256 public messageCounter;

    /// @notice User nonces
    mapping(address => uint256) public userNonces;

    /// @notice Messages by ID
    mapping(bytes32 => Message) public messages;

    /// @notice User sent messages
    mapping(address => bytes32[]) public sentMessages;

    /// @notice User received messages
    mapping(address => bytes32[]) public receivedMessages;

    /// @notice Processed message IDs (from other chains)
    mapping(bytes32 => bool) public processedMessages;

    /// @notice Message receipts
    mapping(bytes32 => MessageReceipt) public receipts;

    /// @notice Callback message mapping (original -> callback)
    mapping(bytes32 => bytes32) public callbackMessages;

    /// @notice Chain routes
    mapping(uint256 => ChainRoute) public chainRoutes;

    /// @notice Supported chain IDs
    uint256[] public supportedChains;

    /// @notice Adapter configurations
    mapping(AdapterType => AdapterConfig) public adapters;

    /// @notice Active adapters
    AdapterType[] public activeAdapters;

    /// @notice Ordered message sequences per sender per chain
    mapping(address => mapping(uint256 => uint256)) public messageSequences;

    /// @notice Fee collector address
    address public feeCollector;

    /// @notice Total fees collected
    uint256 public totalFeesCollected;

    /// @notice Relayer rewards pool
    uint256 public relayerRewardsPool;

    /// @notice Registered receivers (contracts that can receive messages)
    mapping(address => bool) public registeredReceivers;

    /// @notice Storage gap
    uint256[40] private __gap;

    // ============================================
    // EVENTS
    // ============================================

    event MessageSent(
        bytes32 indexed messageId,
        address indexed sender,
        address indexed receiver,
        uint256 destChain,
        MessageType messageType,
        uint256 fee
    );

    event MessageReceived(
        bytes32 indexed messageId,
        address indexed sender,
        address indexed receiver,
        uint256 sourceChain,
        bool success
    );

    event MessageExecuted(
        bytes32 indexed messageId,
        address indexed receiver,
        bool success,
        bytes returnData
    );

    event MessageFailed(
        bytes32 indexed messageId,
        address indexed receiver,
        uint256 retries,
        bytes reason
    );

    event MessageRetried(bytes32 indexed messageId, uint256 retryCount);
    event MessageExpired(bytes32 indexed messageId);
    
    event CallbackReceived(
        bytes32 indexed originalMessageId,
        bytes32 indexed callbackMessageId,
        bool success
    );

    event BatchMessageSent(
        bytes32[] messageIds,
        address indexed sender,
        uint256 destChain,
        uint256 totalFee
    );

    event ChainRouteUpdated(
        uint256 indexed chainId,
        bool isActive,
        address router,
        AdapterType adapter
    );

    event AdapterUpdated(AdapterType indexed adapterType, address adapterAddress, bool isActive);
    event ReceiverRegistered(address indexed receiver, bool registered);
    event FeesWithdrawn(address indexed recipient, uint256 amount);

    // ============================================
    // ERRORS
    // ============================================

    error MessageTooLarge(uint256 size, uint256 maxSize);
    error ChainNotSupported(uint256 chainId);
    error InvalidReceiver();
    error InsufficientFee(uint256 provided, uint256 required);
    error MessageNotFound(bytes32 messageId);
    error MessageAlreadyProcessed(bytes32 messageId);
    error MessageExpiredError(bytes32 messageId);
    error MaxRetriesExceeded(bytes32 messageId);
    error ExecutionFailed(bytes returnData);
    error AdapterNotActive(AdapterType adapterType);
    error UnauthorizedRelayer();
    error InvalidSequence(uint256 expected, uint256 received);
    error ReceiverNotRegistered(address receiver);
    error SameChain();
    error ZeroAddress();
    error InvalidPayload();
    error CallbackTimeout();

    // ============================================
    // MODIFIERS
    // ============================================

    modifier validChain(uint256 _chainId) {
        if (!chainRoutes[_chainId].isActive) revert ChainNotSupported(_chainId);
        _;
    }

    modifier onlyRelayer() {
        if (!hasRole(RELAYER_ROLE, msg.sender)) revert UnauthorizedRelayer();
        _;
    }

    // ============================================
    // INITIALIZER
    // ============================================

    /**
     * @notice Initializes the message router
     * @param admin Admin address
     * @param _feeCollector Fee collector address
     */
    function initialize(
        address admin,
        address _feeCollector
    ) public initializer {
        if (admin == address(0)) revert ZeroAddress();
        if (_feeCollector == address(0)) revert ZeroAddress();

        __AccessControl_init();
        __Pausable_init();
        __ReentrancyGuard_init();
        __UUPSUpgradeable_init();

        chainId = block.chainid;
        feeCollector = _feeCollector;

        _grantRole(DEFAULT_ADMIN_ROLE, admin);
        _grantRole(OPERATOR_ROLE, admin);
        _grantRole(FEE_MANAGER_ROLE, admin);
        _grantRole(UPGRADER_ROLE, admin);
    }

    // ============================================
    // SEND MESSAGES
    // ============================================

    /**
     * @notice Sends a cross-chain message
     * @param receiver Receiver address on destination
     * @param destChain Destination chain ID
     * @param payload Message payload
     * @param gasLimit Gas limit for execution
     * @param messageType Type of message
     * @return messageId The message ID
     */
    function sendMessage(
        address receiver,
        uint256 destChain,
        bytes calldata payload,
        uint256 gasLimit,
        MessageType messageType
    )
        external
        payable
        nonReentrant
        whenNotPaused
        validChain(destChain)
        returns (bytes32 messageId)
    {
        if (receiver == address(0)) revert InvalidReceiver();
        if (destChain == chainId) revert SameChain();
        if (payload.length > MAX_MESSAGE_SIZE) {
            revert MessageTooLarge(payload.length, MAX_MESSAGE_SIZE);
        }

        ChainRoute storage route = chainRoutes[destChain];
        if (gasLimit > route.maxGasLimit) {
            gasLimit = route.maxGasLimit;
        }

        // Calculate fee
        uint256 requiredFee = calculateFee(destChain, payload.length, gasLimit);
        if (msg.value < requiredFee) {
            revert InsufficientFee(msg.value, requiredFee);
        }

        // Generate message ID
        uint256 nonce = userNonces[msg.sender]++;
        messageId = _generateMessageId(msg.sender, receiver, destChain, nonce);

        // Create message
        messages[messageId] = Message({
            id: messageId,
            sender: msg.sender,
            receiver: receiver,
            sourceChain: chainId,
            destChain: destChain,
            nonce: nonce,
            messageType: messageType,
            payload: payload,
            gasLimit: gasLimit,
            fee: msg.value,
            timestamp: block.timestamp,
            deadline: block.timestamp + MESSAGE_EXPIRY,
            status: MessageStatus.SENT,
            retries: 0,
            callbackId: bytes32(0)
        });

        sentMessages[msg.sender].push(messageId);
        route.totalMessages++;
        totalFeesCollected += msg.value;

        // Handle ordered messages
        if (messageType == MessageType.ORDERED) {
            messageSequences[msg.sender][destChain]++;
        }

        emit MessageSent(messageId, msg.sender, receiver, destChain, messageType, msg.value);

        // Refund excess
        if (msg.value > requiredFee) {
            payable(msg.sender).transfer(msg.value - requiredFee);
        }
    }

    /**
     * @notice Sends a batch of messages
     * @param requests Array of batch message requests
     * @return messageIds Array of message IDs
     */
    function sendBatchMessages(
        BatchMessageRequest[] calldata requests
    )
        external
        payable
        nonReentrant
        whenNotPaused
        returns (bytes32[] memory messageIds)
    {
        uint256 len = requests.length;
        messageIds = new bytes32[](len);
        uint256 totalRequiredFee = 0;

        // First pass: validate and calculate fees
        for (uint256 i = 0; i < len; i++) {
            BatchMessageRequest calldata req = requests[i];
            if (!chainRoutes[req.destChain].isActive) revert ChainNotSupported(req.destChain);
            if (req.receiver == address(0)) revert InvalidReceiver();
            if (req.payload.length > MAX_MESSAGE_SIZE) {
                revert MessageTooLarge(req.payload.length, MAX_MESSAGE_SIZE);
            }
            totalRequiredFee += calculateFee(req.destChain, req.payload.length, req.gasLimit);
        }

        if (msg.value < totalRequiredFee) {
            revert InsufficientFee(msg.value, totalRequiredFee);
        }

        // Second pass: create messages
        for (uint256 i = 0; i < len; i++) {
            BatchMessageRequest calldata req = requests[i];
            uint256 nonce = userNonces[msg.sender]++;
            bytes32 messageId = _generateMessageId(msg.sender, req.receiver, req.destChain, nonce);
            
            uint256 fee = calculateFee(req.destChain, req.payload.length, req.gasLimit);

            messages[messageId] = Message({
                id: messageId,
                sender: msg.sender,
                receiver: req.receiver,
                sourceChain: chainId,
                destChain: req.destChain,
                nonce: nonce,
                messageType: MessageType.BATCH,
                payload: req.payload,
                gasLimit: req.gasLimit,
                fee: fee,
                timestamp: block.timestamp,
                deadline: block.timestamp + MESSAGE_EXPIRY,
                status: MessageStatus.SENT,
                retries: 0,
                callbackId: bytes32(0)
            });

            sentMessages[msg.sender].push(messageId);
            chainRoutes[req.destChain].totalMessages++;
            messageIds[i] = messageId;
        }

        totalFeesCollected += totalRequiredFee;

        emit BatchMessageSent(messageIds, msg.sender, requests[0].destChain, totalRequiredFee);

        // Refund excess
        if (msg.value > totalRequiredFee) {
            payable(msg.sender).transfer(msg.value - totalRequiredFee);
        }
    }

    // ============================================
    // RECEIVE & EXECUTE MESSAGES
    // ============================================

    /**
     * @notice Receives and executes a message from another chain
     * @param messageId Original message ID
     * @param sender Original sender
     * @param receiver Receiver on this chain
     * @param sourceChain Source chain ID
     * @param payload Message payload
     * @param gasLimit Gas limit for execution
     * @param messageType Message type
     */
    function receiveMessage(
        bytes32 messageId,
        address sender,
        address receiver,
        uint256 sourceChain,
        bytes calldata payload,
        uint256 gasLimit,
        MessageType messageType
    )
        external
        onlyRelayer
        nonReentrant
        whenNotPaused
    {
        if (processedMessages[messageId]) {
            revert MessageAlreadyProcessed(messageId);
        }

        processedMessages[messageId] = true;
        receivedMessages[receiver].push(messageId);

        // Execute message
        bool success;
        bytes memory returnData;

        if (registeredReceivers[receiver]) {
            // Call receiver with message data
            (success, returnData) = receiver.call{gas: gasLimit}(
                abi.encodeWithSignature(
                    "onMessageReceived(bytes32,address,uint256,bytes)",
                    messageId,
                    sender,
                    sourceChain,
                    payload
                )
            );
        } else {
            // Just emit event for unregistered receivers
            success = true;
            returnData = "";
        }

        // Store receipt
        receipts[messageId] = MessageReceipt({
            messageId: messageId,
            sourceChain: sourceChain,
            destChain: chainId,
            success: success,
            returnData: returnData,
            gasUsed: gasLimit, // Simplified
            timestamp: block.timestamp
        });

        emit MessageReceived(messageId, sender, receiver, sourceChain, success);
        emit MessageExecuted(messageId, receiver, success, returnData);

        // Handle callback if needed
        if (messageType == MessageType.CALLBACK && success) {
            // Send callback confirmation
            _sendCallback(messageId, sourceChain, sender, returnData);
        }
    }

    /**
     * @notice Retries a failed message
     * @param messageId Message ID to retry
     */
    function retryMessage(bytes32 messageId) external onlyRelayer {
        Message storage message = messages[messageId];
        if (message.id == bytes32(0)) revert MessageNotFound(messageId);
        if (message.status != MessageStatus.FAILED) revert InvalidPayload();
        if (message.retries >= MAX_RETRIES) revert MaxRetriesExceeded(messageId);
        if (block.timestamp > message.deadline) revert MessageExpiredError(messageId);

        message.retries++;
        message.status = MessageStatus.PENDING;

        emit MessageRetried(messageId, message.retries);
    }

    /**
     * @notice Marks a message as failed
     * @param messageId Message ID
     * @param reason Failure reason
     */
    function markMessageFailed(
        bytes32 messageId,
        bytes calldata reason
    ) external onlyRelayer {
        Message storage message = messages[messageId];
        if (message.id == bytes32(0)) revert MessageNotFound(messageId);

        message.status = MessageStatus.FAILED;
        chainRoutes[message.destChain].failedMessages++;

        emit MessageFailed(messageId, message.receiver, message.retries, reason);
    }

    // ============================================
    // CALLBACKS
    // ============================================

    /**
     * @notice Internal function to send callback
     */
    function _sendCallback(
        bytes32 originalMessageId,
        uint256 destChain,
        address recipient,
        bytes memory returnData
    ) internal {
        bytes32 callbackId = keccak256(abi.encodePacked(
            "CALLBACK",
            originalMessageId,
            block.timestamp
        ));

        callbackMessages[originalMessageId] = callbackId;

        // Note: In production, this would trigger actual cross-chain message
        emit CallbackReceived(originalMessageId, callbackId, true);
    }

    /**
     * @notice Receives a callback message
     */
    function receiveCallback(
        bytes32 originalMessageId,
        bytes32 callbackId,
        bool success,
        bytes calldata returnData
    ) external onlyRelayer {
        Message storage message = messages[originalMessageId];
        if (message.id == bytes32(0)) revert MessageNotFound(originalMessageId);
        if (message.messageType != MessageType.CALLBACK) revert InvalidPayload();

        message.status = success ? MessageStatus.CALLBACK_RECEIVED : MessageStatus.FAILED;
        message.callbackId = callbackId;

        receipts[callbackId] = MessageReceipt({
            messageId: callbackId,
            sourceChain: message.destChain,
            destChain: chainId,
            success: success,
            returnData: returnData,
            gasUsed: 0,
            timestamp: block.timestamp
        });

        emit CallbackReceived(originalMessageId, callbackId, success);
    }

    // ============================================
    // FEE CALCULATION
    // ============================================

    /**
     * @notice Calculates message fee
     * @param destChain Destination chain
     * @param payloadSize Payload size in bytes
     * @param gasLimit Gas limit
     * @return fee Required fee
     */
    function calculateFee(
        uint256 destChain,
        uint256 payloadSize,
        uint256 gasLimit
    ) public view returns (uint256 fee) {
        ChainRoute storage route = chainRoutes[destChain];
        
        // Base fee + gas cost + payload cost
        uint256 gasCost = gasLimit * route.gasPrice;
        uint256 payloadCost = (payloadSize / 32 + 1) * 1000; // Cost per 32-byte word
        
        fee = route.baseFee + gasCost + payloadCost;

        // Apply adapter multiplier
        AdapterConfig storage adapter = adapters[route.adapter];
        if (adapter.isActive) {
            fee = (fee * adapter.feeMultiplier) / 100;
        }
    }

    /**
     * @notice Estimates gas for a message
     * @param destChain Destination chain
     * @param payloadSize Payload size
     * @param gasLimit Requested gas limit
     * @return estimate Gas estimation result
     */
    function estimateGas(
        uint256 destChain,
        uint256 payloadSize,
        uint256 gasLimit
    ) external view returns (GasEstimate memory estimate) {
        ChainRoute storage route = chainRoutes[destChain];
        
        uint256 baseGas = 21000; // Base transaction gas
        uint256 payloadGas = (payloadSize / 32 + 1) * 100; // Gas per word
        
        estimate = GasEstimate({
            baseGas: baseGas,
            payloadGas: payloadGas,
            totalGas: baseGas + payloadGas + gasLimit,
            estimatedFee: calculateFee(destChain, payloadSize, gasLimit),
            maxFee: calculateFee(destChain, payloadSize, route.maxGasLimit)
        });
    }

    // ============================================
    // CONFIGURATION
    // ============================================

    /**
     * @notice Sets chain route configuration
     */
    function setChainRoute(
        uint256 _chainId,
        bool isActive,
        address router,
        AdapterType adapter,
        uint256 baseFee,
        uint256 gasPrice,
        uint256 maxGasLimit,
        uint256 batchSize
    ) external onlyRole(OPERATOR_ROLE) {
        bool isNew = !chainRoutes[_chainId].isActive && isActive;

        chainRoutes[_chainId] = ChainRoute({
            chainId: _chainId,
            isActive: isActive,
            router: router,
            adapter: adapter,
            baseFee: baseFee,
            gasPrice: gasPrice,
            maxGasLimit: maxGasLimit,
            batchSize: batchSize,
            totalMessages: chainRoutes[_chainId].totalMessages,
            failedMessages: chainRoutes[_chainId].failedMessages
        });

        if (isNew) {
            supportedChains.push(_chainId);
        }

        emit ChainRouteUpdated(_chainId, isActive, router, adapter);
    }

    /**
     * @notice Sets adapter configuration
     */
    function setAdapter(
        AdapterType adapterType,
        address adapterAddress,
        bool isActive,
        uint256 feeMultiplier
    ) external onlyRole(OPERATOR_ROLE) {
        bool isNew = !adapters[adapterType].isActive && isActive;

        adapters[adapterType] = AdapterConfig({
            adapterType: adapterType,
            adapterAddress: adapterAddress,
            isActive: isActive,
            feeMultiplier: feeMultiplier,
            successRate: adapters[adapterType].successRate,
            avgLatency: adapters[adapterType].avgLatency
        });

        if (isNew) {
            activeAdapters.push(adapterType);
        }

        emit AdapterUpdated(adapterType, adapterAddress, isActive);
    }

    /**
     * @notice Registers a receiver contract
     */
    function registerReceiver(address receiver, bool registered) external onlyRole(OPERATOR_ROLE) {
        registeredReceivers[receiver] = registered;
        emit ReceiverRegistered(receiver, registered);
    }

    // ============================================
    // HELPER FUNCTIONS
    // ============================================

    /**
     * @notice Generates a unique message ID
     */
    function _generateMessageId(
        address sender,
        address receiver,
        uint256 destChain,
        uint256 nonce
    ) internal returns (bytes32) {
        return keccak256(abi.encodePacked(
            sender,
            receiver,
            chainId,
            destChain,
            nonce,
            block.timestamp,
            messageCounter++
        ));
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    function getMessage(bytes32 messageId) external view returns (Message memory) {
        return messages[messageId];
    }

    function getReceipt(bytes32 messageId) external view returns (MessageReceipt memory) {
        return receipts[messageId];
    }

    function getSentMessages(address sender) external view returns (bytes32[] memory) {
        return sentMessages[sender];
    }

    function getReceivedMessages(address receiver) external view returns (bytes32[] memory) {
        return receivedMessages[receiver];
    }

    function getSupportedChains() external view returns (uint256[] memory) {
        return supportedChains;
    }

    function getActiveAdapters() external view returns (AdapterType[] memory) {
        return activeAdapters;
    }

    function isMessageProcessed(bytes32 messageId) external view returns (bool) {
        return processedMessages[messageId];
    }

    // ============================================
    // FEE MANAGEMENT
    // ============================================

    /**
     * @notice Withdraws collected fees
     */
    function withdrawFees(address recipient, uint256 amount) external onlyRole(FEE_MANAGER_ROLE) {
        if (recipient == address(0)) revert ZeroAddress();
        if (amount > address(this).balance) {
            amount = address(this).balance;
        }

        payable(recipient).transfer(amount);
        emit FeesWithdrawn(recipient, amount);
    }

    /**
     * @notice Sets fee collector
     */
    function setFeeCollector(address _feeCollector) external onlyRole(FEE_MANAGER_ROLE) {
        if (_feeCollector == address(0)) revert ZeroAddress();
        feeCollector = _feeCollector;
    }

    // ============================================
    // ADMIN
    // ============================================

    function pause() external onlyRole(OPERATOR_ROLE) { _pause(); }
    function unpause() external onlyRole(OPERATOR_ROLE) { _unpause(); }
    function _authorizeUpgrade(address) internal override onlyRole(UPGRADER_ROLE) {}

    /// @notice Receive ETH
    receive() external payable {}
}
