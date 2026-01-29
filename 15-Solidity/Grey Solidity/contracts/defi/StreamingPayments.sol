// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";

/**
 * @title StreamingPayments
 * @notice Real-time continuous payment streaming protocol
 * @dev Implements Sablier-style streaming with advanced features
 * 
 * Features:
 * - Linear and cliff-based streaming
 * - Multi-token support
 * - Stream delegation and transfer
 * - Batch operations
 * - Emergency cancellation with refunds
 * - Stream composability (chained streams)
 * 
 * Use Cases:
 * - Salary streaming for DAOs
 * - Vesting schedules
 * - Subscription payments
 * - Continuous service billing
 */
contract StreamingPayments is AccessControl, ReentrancyGuard {
    using SafeERC20 for IERC20;

    // ============================================
    // ROLES
    // ============================================

    bytes32 public constant OPERATOR_ROLE = keccak256("OPERATOR_ROLE");

    // ============================================
    // TYPES
    // ============================================

    /// @notice Stream configuration
    struct Stream {
        address sender;
        address recipient;
        address token;
        uint256 depositAmount;          // Total amount deposited
        uint256 withdrawnAmount;        // Amount already withdrawn
        uint256 startTime;
        uint256 stopTime;
        uint256 cliffTime;              // Cliff period (0 = no cliff)
        uint256 ratePerSecond;          // Tokens per second
        StreamType streamType;
        StreamStatus status;
        bool cancelable;
        bool transferable;
    }

    /// @notice Batch stream creation params
    struct StreamParams {
        address recipient;
        address token;
        uint256 depositAmount;
        uint256 startTime;
        uint256 stopTime;
        uint256 cliffTime;
        StreamType streamType;
        bool cancelable;
        bool transferable;
    }

    enum StreamType {
        Linear,                         // Constant rate
        Cliff,                          // Nothing until cliff, then linear
        Stepped,                        // Periodic unlocks
        Exponential                     // Accelerating rate
    }

    enum StreamStatus {
        Active,
        Completed,
        Cancelled,
        Paused
    }

    // ============================================
    // CONSTANTS
    // ============================================

    uint256 public constant MIN_STREAM_DURATION = 1 hours;
    uint256 public constant MAX_STREAMS_PER_BATCH = 100;

    // ============================================
    // STATE
    // ============================================

    /// @notice Streams by ID
    mapping(uint256 => Stream) public streams;
    uint256 public streamCount;

    /// @notice Streams sent by address
    mapping(address => uint256[]) public senderStreams;

    /// @notice Streams received by address
    mapping(address => uint256[]) public recipientStreams;

    /// @notice Stream delegates (who can withdraw on behalf)
    mapping(uint256 => address) public streamDelegates;

    /// @notice Protocol fee (basis points)
    uint256 public protocolFee = 25;        // 0.25%
    address public feeRecipient;

    /// @notice Paused state
    bool public paused;

    // ============================================
    // EVENTS
    // ============================================

    event StreamCreated(
        uint256 indexed streamId,
        address indexed sender,
        address indexed recipient,
        address token,
        uint256 depositAmount,
        uint256 startTime,
        uint256 stopTime
    );

    event Withdrawal(
        uint256 indexed streamId,
        address indexed recipient,
        uint256 amount
    );

    event StreamCancelled(
        uint256 indexed streamId,
        address indexed sender,
        address indexed recipient,
        uint256 senderRefund,
        uint256 recipientBalance
    );

    event StreamTransferred(
        uint256 indexed streamId,
        address indexed oldRecipient,
        address indexed newRecipient
    );

    event StreamDelegated(
        uint256 indexed streamId,
        address indexed delegate
    );

    event StreamPaused(uint256 indexed streamId);
    event StreamResumed(uint256 indexed streamId);

    // ============================================
    // ERRORS
    // ============================================

    error Paused();
    error InvalidRecipient();
    error InvalidDuration();
    error InvalidAmount();
    error InsufficientBalance();
    error StreamNotActive();
    error NotStreamSender();
    error NotStreamRecipient();
    error NotAuthorized();
    error StreamNotCancelable();
    error StreamNotTransferable();
    error CliffNotReached();
    error BatchTooLarge();

    // ============================================
    // MODIFIERS
    // ============================================

    modifier whenNotPaused() {
        if (paused) revert Paused();
        _;
    }

    modifier streamExists(uint256 streamId) {
        if (streams[streamId].sender == address(0)) revert StreamNotActive();
        _;
    }

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(address _feeRecipient) {
        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(OPERATOR_ROLE, msg.sender);
        feeRecipient = _feeRecipient;
    }

    // ============================================
    // STREAM CREATION
    // ============================================

    /**
     * @notice Create a new payment stream
     * @param recipient The recipient of the stream
     * @param token The token to stream
     * @param depositAmount Total amount to stream
     * @param startTime When streaming starts
     * @param stopTime When streaming ends
     * @param cliffTime Cliff timestamp (0 = no cliff)
     * @param streamType Type of stream (linear, cliff, etc.)
     * @param cancelable Whether sender can cancel
     * @param transferable Whether recipient can transfer
     */
    function createStream(
        address recipient,
        address token,
        uint256 depositAmount,
        uint256 startTime,
        uint256 stopTime,
        uint256 cliffTime,
        StreamType streamType,
        bool cancelable,
        bool transferable
    ) external whenNotPaused nonReentrant returns (uint256 streamId) {
        if (recipient == address(0) || recipient == msg.sender) revert InvalidRecipient();
        if (startTime >= stopTime) revert InvalidDuration();
        if (stopTime - startTime < MIN_STREAM_DURATION) revert InvalidDuration();
        if (depositAmount == 0) revert InvalidAmount();
        if (cliffTime != 0 && (cliffTime < startTime || cliffTime > stopTime)) revert InvalidDuration();

        // Calculate rate per second
        uint256 duration = stopTime - startTime;
        uint256 ratePerSecond = depositAmount / duration;
        
        // Adjust deposit to be evenly divisible
        uint256 adjustedDeposit = ratePerSecond * duration;

        // Transfer tokens (with fee)
        uint256 fee = (adjustedDeposit * protocolFee) / 10000;
        IERC20(token).safeTransferFrom(msg.sender, address(this), adjustedDeposit + fee);
        if (fee > 0) {
            IERC20(token).safeTransfer(feeRecipient, fee);
        }

        streamId = ++streamCount;

        streams[streamId] = Stream({
            sender: msg.sender,
            recipient: recipient,
            token: token,
            depositAmount: adjustedDeposit,
            withdrawnAmount: 0,
            startTime: startTime,
            stopTime: stopTime,
            cliffTime: cliffTime,
            ratePerSecond: ratePerSecond,
            streamType: streamType,
            status: StreamStatus.Active,
            cancelable: cancelable,
            transferable: transferable
        });

        senderStreams[msg.sender].push(streamId);
        recipientStreams[recipient].push(streamId);

        emit StreamCreated(
            streamId,
            msg.sender,
            recipient,
            token,
            adjustedDeposit,
            startTime,
            stopTime
        );
    }

    /**
     * @notice Create multiple streams in one transaction
     */
    function createStreamBatch(
        StreamParams[] calldata params
    ) external whenNotPaused nonReentrant returns (uint256[] memory streamIds) {
        if (params.length > MAX_STREAMS_PER_BATCH) revert BatchTooLarge();

        streamIds = new uint256[](params.length);

        for (uint256 i = 0; i < params.length; i++) {
            StreamParams calldata p = params[i];

            if (p.recipient == address(0) || p.recipient == msg.sender) revert InvalidRecipient();
            if (p.startTime >= p.stopTime) revert InvalidDuration();
            if (p.depositAmount == 0) revert InvalidAmount();

            uint256 duration = p.stopTime - p.startTime;
            uint256 ratePerSecond = p.depositAmount / duration;
            uint256 adjustedDeposit = ratePerSecond * duration;

            uint256 fee = (adjustedDeposit * protocolFee) / 10000;
            IERC20(p.token).safeTransferFrom(msg.sender, address(this), adjustedDeposit + fee);
            if (fee > 0) {
                IERC20(p.token).safeTransfer(feeRecipient, fee);
            }

            uint256 streamId = ++streamCount;
            streamIds[i] = streamId;

            streams[streamId] = Stream({
                sender: msg.sender,
                recipient: p.recipient,
                token: p.token,
                depositAmount: adjustedDeposit,
                withdrawnAmount: 0,
                startTime: p.startTime,
                stopTime: p.stopTime,
                cliffTime: p.cliffTime,
                ratePerSecond: ratePerSecond,
                streamType: p.streamType,
                status: StreamStatus.Active,
                cancelable: p.cancelable,
                transferable: p.transferable
            });

            senderStreams[msg.sender].push(streamId);
            recipientStreams[p.recipient].push(streamId);

            emit StreamCreated(
                streamId,
                msg.sender,
                p.recipient,
                p.token,
                adjustedDeposit,
                p.startTime,
                p.stopTime
            );
        }
    }

    // ============================================
    // WITHDRAWAL
    // ============================================

    /**
     * @notice Withdraw from a stream
     * @param streamId The stream to withdraw from
     * @param amount Amount to withdraw (0 = max available)
     */
    function withdraw(
        uint256 streamId,
        uint256 amount
    ) external nonReentrant streamExists(streamId) returns (uint256 withdrawn) {
        Stream storage stream = streams[streamId];

        if (stream.status != StreamStatus.Active) revert StreamNotActive();

        bool authorized = msg.sender == stream.recipient ||
                          msg.sender == streamDelegates[streamId];
        if (!authorized) revert NotAuthorized();

        uint256 available = balanceOf(streamId);
        if (available == 0) revert InsufficientBalance();

        withdrawn = amount == 0 ? available : (amount > available ? available : amount);

        stream.withdrawnAmount += withdrawn;

        // Check if stream is complete
        if (stream.withdrawnAmount >= stream.depositAmount) {
            stream.status = StreamStatus.Completed;
        }

        IERC20(stream.token).safeTransfer(stream.recipient, withdrawn);

        emit Withdrawal(streamId, stream.recipient, withdrawn);
    }

    /**
     * @notice Withdraw from multiple streams
     */
    function withdrawBatch(
        uint256[] calldata streamIds
    ) external nonReentrant returns (uint256 totalWithdrawn) {
        for (uint256 i = 0; i < streamIds.length; i++) {
            Stream storage stream = streams[streamIds[i]];

            if (stream.status != StreamStatus.Active) continue;
            if (stream.recipient != msg.sender && streamDelegates[streamIds[i]] != msg.sender) continue;

            uint256 available = balanceOf(streamIds[i]);
            if (available == 0) continue;

            stream.withdrawnAmount += available;
            totalWithdrawn += available;

            if (stream.withdrawnAmount >= stream.depositAmount) {
                stream.status = StreamStatus.Completed;
            }

            IERC20(stream.token).safeTransfer(stream.recipient, available);

            emit Withdrawal(streamIds[i], stream.recipient, available);
        }
    }

    // ============================================
    // STREAM MANAGEMENT
    // ============================================

    /**
     * @notice Cancel a stream and refund remaining balance
     */
    function cancelStream(
        uint256 streamId
    ) external nonReentrant streamExists(streamId) returns (
        uint256 senderRefund,
        uint256 recipientBalance
    ) {
        Stream storage stream = streams[streamId];

        if (stream.status != StreamStatus.Active) revert StreamNotActive();
        if (!stream.cancelable) revert StreamNotCancelable();
        if (msg.sender != stream.sender) revert NotStreamSender();

        recipientBalance = balanceOf(streamId);
        senderRefund = stream.depositAmount - stream.withdrawnAmount - recipientBalance;

        stream.status = StreamStatus.Cancelled;

        if (recipientBalance > 0) {
            IERC20(stream.token).safeTransfer(stream.recipient, recipientBalance);
        }
        if (senderRefund > 0) {
            IERC20(stream.token).safeTransfer(stream.sender, senderRefund);
        }

        emit StreamCancelled(streamId, stream.sender, stream.recipient, senderRefund, recipientBalance);
    }

    /**
     * @notice Transfer stream to a new recipient
     */
    function transferStream(
        uint256 streamId,
        address newRecipient
    ) external streamExists(streamId) {
        Stream storage stream = streams[streamId];

        if (stream.status != StreamStatus.Active) revert StreamNotActive();
        if (!stream.transferable) revert StreamNotTransferable();
        if (msg.sender != stream.recipient) revert NotStreamRecipient();
        if (newRecipient == address(0) || newRecipient == stream.sender) revert InvalidRecipient();

        address oldRecipient = stream.recipient;
        stream.recipient = newRecipient;

        recipientStreams[newRecipient].push(streamId);

        emit StreamTransferred(streamId, oldRecipient, newRecipient);
    }

    /**
     * @notice Delegate withdrawal rights
     */
    function delegateStream(
        uint256 streamId,
        address delegate
    ) external streamExists(streamId) {
        Stream storage stream = streams[streamId];

        if (msg.sender != stream.recipient) revert NotStreamRecipient();

        streamDelegates[streamId] = delegate;

        emit StreamDelegated(streamId, delegate);
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Calculate withdrawable balance for a stream
     */
    function balanceOf(uint256 streamId) public view returns (uint256) {
        Stream memory stream = streams[streamId];

        if (stream.status != StreamStatus.Active) return 0;
        if (block.timestamp < stream.startTime) return 0;

        // Check cliff
        if (stream.cliffTime != 0 && block.timestamp < stream.cliffTime) {
            return 0;
        }

        uint256 elapsed;
        if (block.timestamp >= stream.stopTime) {
            elapsed = stream.stopTime - stream.startTime;
        } else {
            elapsed = block.timestamp - stream.startTime;
        }

        uint256 streamed = _calculateStreamed(stream, elapsed);
        return streamed > stream.withdrawnAmount ? streamed - stream.withdrawnAmount : 0;
    }

    /**
     * @notice Calculate total streamed amount based on stream type
     */
    function _calculateStreamed(
        Stream memory stream,
        uint256 elapsed
    ) internal pure returns (uint256) {
        if (stream.streamType == StreamType.Linear || stream.streamType == StreamType.Cliff) {
            return stream.ratePerSecond * elapsed;
        } else if (stream.streamType == StreamType.Stepped) {
            // Weekly unlocks
            uint256 weeks_ = elapsed / 1 weeks;
            uint256 totalWeeks = (stream.stopTime - stream.startTime) / 1 weeks;
            if (totalWeeks == 0) return stream.depositAmount;
            return (stream.depositAmount * weeks_) / totalWeeks;
        } else {
            // Exponential: more tokens unlock as time passes
            uint256 duration = stream.stopTime - stream.startTime;
            // Using simple quadratic curve: (elapsed/duration)^2 * deposit
            return (stream.depositAmount * elapsed * elapsed) / (duration * duration);
        }
    }

    /**
     * @notice Get stream details
     */
    function getStream(uint256 streamId) external view returns (
        address sender,
        address recipient,
        address token,
        uint256 depositAmount,
        uint256 withdrawnAmount,
        uint256 startTime,
        uint256 stopTime,
        uint256 ratePerSecond,
        StreamStatus status
    ) {
        Stream memory s = streams[streamId];
        return (
            s.sender,
            s.recipient,
            s.token,
            s.depositAmount,
            s.withdrawnAmount,
            s.startTime,
            s.stopTime,
            s.ratePerSecond,
            s.status
        );
    }

    /**
     * @notice Get streams by sender
     */
    function getSenderStreams(address sender) external view returns (uint256[] memory) {
        return senderStreams[sender];
    }

    /**
     * @notice Get streams by recipient
     */
    function getRecipientStreams(address recipient) external view returns (uint256[] memory) {
        return recipientStreams[recipient];
    }

    /**
     * @notice Calculate total withdrawable across streams
     */
    function getTotalWithdrawable(
        address recipient
    ) external view returns (uint256 total) {
        uint256[] memory streamIds = recipientStreams[recipient];
        for (uint256 i = 0; i < streamIds.length; i++) {
            total += balanceOf(streamIds[i]);
        }
    }

    // ============================================
    // ADMIN
    // ============================================

    function setProtocolFee(uint256 _fee) external onlyRole(DEFAULT_ADMIN_ROLE) {
        require(_fee <= 500, "Fee too high"); // Max 5%
        protocolFee = _fee;
    }

    function setFeeRecipient(address _recipient) external onlyRole(DEFAULT_ADMIN_ROLE) {
        feeRecipient = _recipient;
    }

    function setPaused(bool _paused) external onlyRole(OPERATOR_ROLE) {
        paused = _paused;
    }
}
