// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

/**
 * @title ICrossChainReceiver
 * @author Grey Protocol Team
 * @notice Interface for contracts that receive cross-chain messages
 */
interface ICrossChainReceiver {
    /**
     * @notice Called when a cross-chain message is received
     * @param messageId Unique message identifier
     * @param sender Original sender on source chain
     * @param sourceChain Source chain ID
     * @param payload Message payload
     * @return success Whether execution was successful
     * @return returnData Any return data
     */
    function onMessageReceived(
        bytes32 messageId,
        address sender,
        uint256 sourceChain,
        bytes calldata payload
    ) external returns (bool success, bytes memory returnData);
}

/**
 * @title ICrossChainBridge
 * @author Grey Protocol Team
 * @notice Interface for the cross-chain bridge
 */
interface ICrossChainBridge {
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

    struct ValidatorSignature {
        address validator;
        bytes signature;
        uint256 timestamp;
    }

    function lock(
        address token,
        uint256 amount,
        uint256 destChain,
        address recipient,
        uint256 deadline
    ) external returns (bytes32 transferId);

    function mint(
        BridgeMessage calldata message,
        ValidatorSignature[] calldata signatures
    ) external;

    function burn(
        address token,
        uint256 amount,
        uint256 destChain,
        address recipient,
        uint256 deadline
    ) external returns (bytes32 transferId);

    function unlock(
        BridgeMessage calldata message,
        ValidatorSignature[] calldata signatures
    ) external;

    function getRemainingDailyLimit(address token) external view returns (uint256);
}

/**
 * @title IMessageRouter
 * @author Grey Protocol Team
 * @notice Interface for the message router
 */
interface IMessageRouter {
    enum MessageType {
        STANDARD,
        CALLBACK,
        BROADCAST,
        BATCH,
        ORDERED
    }

    struct BatchMessageRequest {
        address receiver;
        uint256 destChain;
        bytes payload;
        uint256 gasLimit;
    }

    struct GasEstimate {
        uint256 baseGas;
        uint256 payloadGas;
        uint256 totalGas;
        uint256 estimatedFee;
        uint256 maxFee;
    }

    function sendMessage(
        address receiver,
        uint256 destChain,
        bytes calldata payload,
        uint256 gasLimit,
        MessageType messageType
    ) external payable returns (bytes32 messageId);

    function sendBatchMessages(
        BatchMessageRequest[] calldata requests
    ) external payable returns (bytes32[] memory messageIds);

    function calculateFee(
        uint256 destChain,
        uint256 payloadSize,
        uint256 gasLimit
    ) external view returns (uint256 fee);

    function estimateGas(
        uint256 destChain,
        uint256 payloadSize,
        uint256 gasLimit
    ) external view returns (GasEstimate memory estimate);

    function isMessageProcessed(bytes32 messageId) external view returns (bool);
}

/**
 * @title IWrappedToken
 * @author Grey Protocol Team
 * @notice Interface for wrapped tokens
 */
interface IWrappedToken {
    function mint(address to, uint256 amount) external;
    function burn(uint256 amount) external;
    function burnFrom(address account, uint256 amount) external;
    function originalToken() external view returns (address);
    function originalChainId() external view returns (uint256);
}
