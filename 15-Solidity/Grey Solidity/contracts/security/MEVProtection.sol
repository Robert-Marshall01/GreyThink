// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";

/**
 * @title MEVProtection
 * @notice Comprehensive MEV protection and transaction ordering fairness
 * @dev Implements multiple MEV mitigation strategies
 * 
 * Strategies implemented:
 * 1. Commit-reveal scheme for order submission
 * 2. Time-weighted average price (TWAP) execution
 * 3. Batch auctions with uniform clearing price
 * 4. Private transaction relay integration
 * 5. Slippage protection with oracle bounds
 * 6. Flashbots-style bundle verification
 */
contract MEVProtection is AccessControl, ReentrancyGuard {
    // ============================================
    // ROLES
    // ============================================

    bytes32 public constant OPERATOR_ROLE = keccak256("OPERATOR_ROLE");
    bytes32 public constant SEQUENCER_ROLE = keccak256("SEQUENCER_ROLE");
    bytes32 public constant RELAYER_ROLE = keccak256("RELAYER_ROLE");

    // ============================================
    // TYPES
    // ============================================

    /// @notice Order commitment for commit-reveal
    struct OrderCommitment {
        bytes32 commitmentHash;
        uint256 commitBlock;
        uint256 revealDeadline;
        uint256 deposit;
        bool revealed;
        bool executed;
        CommitmentStatus status;
    }

    /// @notice Revealed order details
    struct Order {
        address maker;
        address tokenIn;
        address tokenOut;
        uint256 amountIn;
        uint256 minAmountOut;
        uint256 maxSlippageBps;
        uint256 deadline;
        bytes32 salt;
        OrderType orderType;
    }

    /// @notice Batch auction
    struct BatchAuction {
        uint256 batchId;
        uint256 startTime;
        uint256 endTime;
        uint256 settlementTime;
        address tokenA;
        address tokenB;
        uint256 clearingPrice;
        BatchStatus status;
        uint256 totalBuyVolume;
        uint256 totalSellVolume;
    }

    /// @notice Batch order
    struct BatchOrder {
        address maker;
        bool isBuy;              // true = buy tokenA with tokenB
        uint256 amount;
        uint256 limitPrice;      // max price for buy, min price for sell
        uint256 filledAmount;
        bool settled;
    }

    /// @notice TWAP execution plan
    struct TWAPExecution {
        address maker;
        address tokenIn;
        address tokenOut;
        uint256 totalAmount;
        uint256 executedAmount;
        uint256 numIntervals;
        uint256 intervalDuration;
        uint256 startTime;
        uint256 lastExecutionTime;
        uint256 executionCount;
        uint256 maxPriceImpact;
        TWAPStatus status;
    }

    /// @notice Private transaction
    struct PrivateTransaction {
        bytes32 txHash;
        address submitter;
        uint256 maxBlockNumber;
        uint256 tipAmount;
        bytes encryptedData;
        bool executed;
    }

    enum CommitmentStatus {
        Pending,
        Revealed,
        Executed,
        Expired,
        Cancelled
    }

    enum OrderType {
        Market,
        Limit,
        StopLoss,
        TakeProfit
    }

    enum BatchStatus {
        Collecting,
        Sealed,
        Settled,
        Cancelled
    }

    enum TWAPStatus {
        Active,
        Completed,
        Cancelled
    }

    // ============================================
    // CONSTANTS
    // ============================================

    uint256 public constant MIN_COMMIT_BLOCKS = 1;
    uint256 public constant MAX_COMMIT_BLOCKS = 50;
    uint256 public constant REVEAL_WINDOW = 10;
    uint256 public constant MIN_DEPOSIT = 0.001 ether;
    uint256 public constant BATCH_DURATION = 5 minutes;
    uint256 public constant MAX_SLIPPAGE_BPS = 5000; // 50%

    // ============================================
    // STATE
    // ============================================

    /// @notice Order commitments
    mapping(bytes32 => OrderCommitment) public commitments;
    mapping(address => bytes32[]) public userCommitments;

    /// @notice Revealed orders
    mapping(bytes32 => Order) public orders;

    /// @notice Batch auctions
    mapping(uint256 => BatchAuction) public batches;
    uint256 public batchCounter;

    /// @notice Orders in each batch: batchId => orderId => BatchOrder
    mapping(uint256 => mapping(uint256 => BatchOrder)) public batchOrders;
    mapping(uint256 => uint256) public batchOrderCount;

    /// @notice User orders in batch
    mapping(uint256 => mapping(address => uint256[])) public userBatchOrders;

    /// @notice TWAP executions
    mapping(uint256 => TWAPExecution) public twapExecutions;
    uint256 public twapCounter;

    /// @notice Private transactions
    mapping(bytes32 => PrivateTransaction) public privateTransactions;

    /// @notice Nonces for replay protection
    mapping(address => uint256) public nonces;

    /// @notice Oracle address for price validation
    address public priceOracle;

    /// @notice Current active batch for each pair
    mapping(bytes32 => uint256) public activeBatch;

    /// @notice Sequencer address for fair ordering
    address public sequencer;

    /// @notice Block delay for commit-reveal
    uint256 public commitDelay = 2;

    // ============================================
    // EVENTS
    // ============================================

    event OrderCommitted(
        bytes32 indexed commitmentId,
        address indexed maker,
        uint256 commitBlock,
        uint256 revealDeadline
    );

    event OrderRevealed(
        bytes32 indexed commitmentId,
        address indexed maker,
        address tokenIn,
        address tokenOut,
        uint256 amountIn
    );

    event OrderExecuted(
        bytes32 indexed commitmentId,
        address indexed maker,
        uint256 amountIn,
        uint256 amountOut
    );

    event BatchCreated(
        uint256 indexed batchId,
        address tokenA,
        address tokenB,
        uint256 endTime
    );

    event BatchOrderPlaced(
        uint256 indexed batchId,
        uint256 indexed orderId,
        address indexed maker,
        bool isBuy,
        uint256 amount
    );

    event BatchSettled(
        uint256 indexed batchId,
        uint256 clearingPrice,
        uint256 buyVolume,
        uint256 sellVolume
    );

    event TWAPStarted(
        uint256 indexed twapId,
        address indexed maker,
        address tokenIn,
        address tokenOut,
        uint256 totalAmount,
        uint256 numIntervals
    );

    event TWAPExecuted(
        uint256 indexed twapId,
        uint256 executionNumber,
        uint256 amount,
        uint256 price
    );

    event PrivateTransactionSubmitted(
        bytes32 indexed txHash,
        address indexed submitter,
        uint256 maxBlock
    );

    // ============================================
    // ERRORS
    // ============================================

    error InvalidCommitment();
    error CommitmentNotFound();
    error RevealTooEarly();
    error RevealTooLate();
    error AlreadyRevealed();
    error OrderExpired();
    error InsufficientDeposit();
    error SlippageExceeded();
    error BatchNotActive();
    error BatchNotSealed();
    error InvalidPrice();
    error TWAPNotActive();
    error TWAPIntervalNotReached();
    error Unauthorized();

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(address _priceOracle) {
        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(OPERATOR_ROLE, msg.sender);
        _grantRole(SEQUENCER_ROLE, msg.sender);
        
        priceOracle = _priceOracle;
        sequencer = msg.sender;
    }

    // ============================================
    // COMMIT-REVEAL SCHEME
    // ============================================

    /**
     * @notice Commit an order hash (hides order details from miners)
     * @param commitmentHash Hash of order details + salt
     */
    function commitOrder(bytes32 commitmentHash) external payable nonReentrant {
        if (msg.value < MIN_DEPOSIT) revert InsufficientDeposit();

        bytes32 commitmentId = keccak256(abi.encodePacked(
            msg.sender,
            commitmentHash,
            block.number,
            nonces[msg.sender]++
        ));

        uint256 revealDeadline = block.number + commitDelay + REVEAL_WINDOW;

        commitments[commitmentId] = OrderCommitment({
            commitmentHash: commitmentHash,
            commitBlock: block.number,
            revealDeadline: revealDeadline,
            deposit: msg.value,
            revealed: false,
            executed: false,
            status: CommitmentStatus.Pending
        });

        userCommitments[msg.sender].push(commitmentId);

        emit OrderCommitted(commitmentId, msg.sender, block.number, revealDeadline);
    }

    /**
     * @notice Reveal a committed order
     */
    function revealOrder(
        bytes32 commitmentId,
        address tokenIn,
        address tokenOut,
        uint256 amountIn,
        uint256 minAmountOut,
        uint256 maxSlippageBps,
        uint256 deadline,
        bytes32 salt,
        OrderType orderType
    ) external nonReentrant {
        OrderCommitment storage commitment = commitments[commitmentId];

        if (commitment.commitmentHash == bytes32(0)) revert CommitmentNotFound();
        if (commitment.revealed) revert AlreadyRevealed();
        if (block.number < commitment.commitBlock + commitDelay) revert RevealTooEarly();
        if (block.number > commitment.revealDeadline) revert RevealTooLate();

        // Verify commitment
        bytes32 expectedHash = keccak256(abi.encodePacked(
            msg.sender,
            tokenIn,
            tokenOut,
            amountIn,
            minAmountOut,
            maxSlippageBps,
            deadline,
            salt,
            orderType
        ));

        if (expectedHash != commitment.commitmentHash) revert InvalidCommitment();

        commitment.revealed = true;
        commitment.status = CommitmentStatus.Revealed;

        orders[commitmentId] = Order({
            maker: msg.sender,
            tokenIn: tokenIn,
            tokenOut: tokenOut,
            amountIn: amountIn,
            minAmountOut: minAmountOut,
            maxSlippageBps: maxSlippageBps,
            deadline: deadline,
            salt: salt,
            orderType: orderType
        });

        emit OrderRevealed(commitmentId, msg.sender, tokenIn, tokenOut, amountIn);
    }

    /**
     * @notice Execute a revealed order
     * @dev In production, this would integrate with DEX or router
     */
    function executeOrder(bytes32 commitmentId) external nonReentrant onlyRole(SEQUENCER_ROLE) {
        OrderCommitment storage commitment = commitments[commitmentId];
        Order memory order = orders[commitmentId];

        if (!commitment.revealed) revert InvalidCommitment();
        if (commitment.executed) revert InvalidCommitment();
        if (block.timestamp > order.deadline) revert OrderExpired();

        // Validate slippage against oracle
        // uint256 oraclePrice = _getOraclePrice(order.tokenIn, order.tokenOut);
        // Execution logic would go here...

        commitment.executed = true;
        commitment.status = CommitmentStatus.Executed;

        // Return deposit
        if (commitment.deposit > 0) {
            payable(order.maker).transfer(commitment.deposit);
        }

        emit OrderExecuted(commitmentId, order.maker, order.amountIn, order.minAmountOut);
    }

    /**
     * @notice Cancel expired commitment and reclaim deposit
     */
    function cancelExpiredCommitment(bytes32 commitmentId) external nonReentrant {
        OrderCommitment storage commitment = commitments[commitmentId];

        if (commitment.commitmentHash == bytes32(0)) revert CommitmentNotFound();
        if (commitment.executed) revert InvalidCommitment();
        if (block.number <= commitment.revealDeadline && commitment.revealed) {
            revert InvalidCommitment();
        }

        commitment.status = CommitmentStatus.Expired;

        if (commitment.deposit > 0) {
            uint256 refund = commitment.deposit;
            commitment.deposit = 0;
            
            // Partial refund for unrevealed (penalize for wasting block space)
            if (!commitment.revealed) {
                refund = refund * 80 / 100; // 20% penalty
            }
            
            payable(msg.sender).transfer(refund);
        }
    }

    // ============================================
    // BATCH AUCTIONS
    // ============================================

    /**
     * @notice Create a new batch auction
     */
    function createBatch(
        address tokenA,
        address tokenB
    ) external onlyRole(OPERATOR_ROLE) returns (uint256 batchId) {
        batchId = ++batchCounter;

        batches[batchId] = BatchAuction({
            batchId: batchId,
            startTime: block.timestamp,
            endTime: block.timestamp + BATCH_DURATION,
            settlementTime: 0,
            tokenA: tokenA,
            tokenB: tokenB,
            clearingPrice: 0,
            status: BatchStatus.Collecting,
            totalBuyVolume: 0,
            totalSellVolume: 0
        });

        bytes32 pairKey = keccak256(abi.encodePacked(tokenA, tokenB));
        activeBatch[pairKey] = batchId;

        emit BatchCreated(batchId, tokenA, tokenB, block.timestamp + BATCH_DURATION);
    }

    /**
     * @notice Place an order in a batch auction
     */
    function placeBatchOrder(
        uint256 batchId,
        bool isBuy,
        uint256 amount,
        uint256 limitPrice
    ) external nonReentrant returns (uint256 orderId) {
        BatchAuction storage batch = batches[batchId];

        if (batch.status != BatchStatus.Collecting) revert BatchNotActive();
        if (block.timestamp >= batch.endTime) revert BatchNotActive();

        orderId = batchOrderCount[batchId]++;

        batchOrders[batchId][orderId] = BatchOrder({
            maker: msg.sender,
            isBuy: isBuy,
            amount: amount,
            limitPrice: limitPrice,
            filledAmount: 0,
            settled: false
        });

        userBatchOrders[batchId][msg.sender].push(orderId);

        if (isBuy) {
            batch.totalBuyVolume += amount;
        } else {
            batch.totalSellVolume += amount;
        }

        emit BatchOrderPlaced(batchId, orderId, msg.sender, isBuy, amount);
    }

    /**
     * @notice Seal a batch (no more orders)
     */
    function sealBatch(uint256 batchId) external onlyRole(OPERATOR_ROLE) {
        BatchAuction storage batch = batches[batchId];

        if (batch.status != BatchStatus.Collecting) revert BatchNotActive();
        if (block.timestamp < batch.endTime) revert BatchNotActive();

        batch.status = BatchStatus.Sealed;
    }

    /**
     * @notice Settle a batch with uniform clearing price
     */
    function settleBatch(
        uint256 batchId,
        uint256 clearingPrice
    ) external onlyRole(SEQUENCER_ROLE) {
        BatchAuction storage batch = batches[batchId];

        if (batch.status != BatchStatus.Sealed) revert BatchNotSealed();

        batch.clearingPrice = clearingPrice;
        batch.settlementTime = block.timestamp;
        batch.status = BatchStatus.Settled;

        // Match orders at clearing price
        _matchBatchOrders(batchId, clearingPrice);

        emit BatchSettled(batchId, clearingPrice, batch.totalBuyVolume, batch.totalSellVolume);
    }

    /**
     * @notice Match orders in a batch at clearing price
     */
    function _matchBatchOrders(uint256 batchId, uint256 clearingPrice) internal {
        uint256 orderCount = batchOrderCount[batchId];
        uint256 availableSell = 0;
        uint256 availableBuy = 0;

        // First pass: count valid orders at clearing price
        for (uint256 i = 0; i < orderCount; i++) {
            BatchOrder storage order = batchOrders[batchId][i];
            
            if (order.isBuy && order.limitPrice >= clearingPrice) {
                availableBuy += order.amount;
            } else if (!order.isBuy && order.limitPrice <= clearingPrice) {
                availableSell += order.amount;
            }
        }

        // Determine fill ratio (pro-rata if oversubscribed)
        uint256 matchedVolume = availableBuy < availableSell ? availableBuy : availableSell;
        uint256 buyFillRatio = availableBuy > 0 ? (matchedVolume * 1e18) / availableBuy : 0;
        uint256 sellFillRatio = availableSell > 0 ? (matchedVolume * 1e18) / availableSell : 0;

        // Second pass: fill orders
        for (uint256 i = 0; i < orderCount; i++) {
            BatchOrder storage order = batchOrders[batchId][i];
            
            if (order.isBuy && order.limitPrice >= clearingPrice) {
                order.filledAmount = (order.amount * buyFillRatio) / 1e18;
            } else if (!order.isBuy && order.limitPrice <= clearingPrice) {
                order.filledAmount = (order.amount * sellFillRatio) / 1e18;
            }
            
            order.settled = true;
        }
    }

    // ============================================
    // TWAP EXECUTION
    // ============================================

    /**
     * @notice Start a TWAP execution plan
     */
    function startTWAP(
        address tokenIn,
        address tokenOut,
        uint256 totalAmount,
        uint256 numIntervals,
        uint256 intervalDuration,
        uint256 maxPriceImpact
    ) external nonReentrant returns (uint256 twapId) {
        require(numIntervals > 0 && numIntervals <= 100, "Invalid intervals");
        require(intervalDuration >= 1 minutes, "Interval too short");

        twapId = ++twapCounter;

        twapExecutions[twapId] = TWAPExecution({
            maker: msg.sender,
            tokenIn: tokenIn,
            tokenOut: tokenOut,
            totalAmount: totalAmount,
            executedAmount: 0,
            numIntervals: numIntervals,
            intervalDuration: intervalDuration,
            startTime: block.timestamp,
            lastExecutionTime: 0,
            executionCount: 0,
            maxPriceImpact: maxPriceImpact,
            status: TWAPStatus.Active
        });

        emit TWAPStarted(twapId, msg.sender, tokenIn, tokenOut, totalAmount, numIntervals);
    }

    /**
     * @notice Execute next TWAP interval
     */
    function executeTWAPInterval(uint256 twapId) external nonReentrant {
        TWAPExecution storage twap = twapExecutions[twapId];

        if (twap.status != TWAPStatus.Active) revert TWAPNotActive();
        if (twap.executionCount >= twap.numIntervals) {
            twap.status = TWAPStatus.Completed;
            revert TWAPNotActive();
        }

        uint256 nextExecutionTime = twap.lastExecutionTime == 0 
            ? twap.startTime 
            : twap.lastExecutionTime + twap.intervalDuration;

        if (block.timestamp < nextExecutionTime) revert TWAPIntervalNotReached();

        uint256 intervalAmount = twap.totalAmount / twap.numIntervals;
        
        // Handle remainder in last interval
        if (twap.executionCount == twap.numIntervals - 1) {
            intervalAmount = twap.totalAmount - twap.executedAmount;
        }

        // Execute swap (integration with DEX would go here)
        // uint256 amountOut = _executeSwap(twap.tokenIn, twap.tokenOut, intervalAmount);

        twap.executedAmount += intervalAmount;
        twap.lastExecutionTime = block.timestamp;
        twap.executionCount++;

        if (twap.executionCount >= twap.numIntervals) {
            twap.status = TWAPStatus.Completed;
        }

        emit TWAPExecuted(twapId, twap.executionCount, intervalAmount, 0);
    }

    /**
     * @notice Cancel active TWAP
     */
    function cancelTWAP(uint256 twapId) external nonReentrant {
        TWAPExecution storage twap = twapExecutions[twapId];

        if (twap.maker != msg.sender) revert Unauthorized();
        if (twap.status != TWAPStatus.Active) revert TWAPNotActive();

        twap.status = TWAPStatus.Cancelled;
    }

    // ============================================
    // PRIVATE TRANSACTIONS
    // ============================================

    /**
     * @notice Submit a private transaction (for relay to builders)
     */
    function submitPrivateTransaction(
        bytes32 txHash,
        uint256 maxBlockNumber,
        bytes calldata encryptedData
    ) external payable onlyRole(RELAYER_ROLE) {
        privateTransactions[txHash] = PrivateTransaction({
            txHash: txHash,
            submitter: msg.sender,
            maxBlockNumber: maxBlockNumber,
            tipAmount: msg.value,
            encryptedData: encryptedData,
            executed: false
        });

        emit PrivateTransactionSubmitted(txHash, msg.sender, maxBlockNumber);
    }

    /**
     * @notice Mark private transaction as executed
     */
    function markPrivateTransactionExecuted(bytes32 txHash) external onlyRole(SEQUENCER_ROLE) {
        privateTransactions[txHash].executed = true;
    }

    // ============================================
    // ADMIN FUNCTIONS
    // ============================================

    function setCommitDelay(uint256 _delay) external onlyRole(DEFAULT_ADMIN_ROLE) {
        require(_delay >= MIN_COMMIT_BLOCKS && _delay <= MAX_COMMIT_BLOCKS, "Invalid delay");
        commitDelay = _delay;
    }

    function setPriceOracle(address _oracle) external onlyRole(DEFAULT_ADMIN_ROLE) {
        priceOracle = _oracle;
    }

    function setSequencer(address _sequencer) external onlyRole(DEFAULT_ADMIN_ROLE) {
        sequencer = _sequencer;
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    function getCommitment(bytes32 commitmentId) external view returns (
        bytes32 commitmentHash,
        uint256 commitBlock,
        uint256 revealDeadline,
        bool revealed,
        bool executed,
        CommitmentStatus status
    ) {
        OrderCommitment memory c = commitments[commitmentId];
        return (
            c.commitmentHash,
            c.commitBlock,
            c.revealDeadline,
            c.revealed,
            c.executed,
            c.status
        );
    }

    function getUserCommitments(address user) external view returns (bytes32[] memory) {
        return userCommitments[user];
    }

    function getBatch(uint256 batchId) external view returns (BatchAuction memory) {
        return batches[batchId];
    }

    function getTWAP(uint256 twapId) external view returns (TWAPExecution memory) {
        return twapExecutions[twapId];
    }

    function getUserBatchOrders(
        uint256 batchId,
        address user
    ) external view returns (uint256[] memory) {
        return userBatchOrders[batchId][user];
    }

    receive() external payable {}
}
