// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/utils/cryptography/ECDSA.sol";
import "@openzeppelin/contracts/utils/cryptography/EIP712.sol";

/**
 * @title LimitOrderBook
 * @notice Off-chain order book with on-chain settlement
 * @dev Gasless limit orders using EIP-712 signatures with matching engine
 *
 * Architecture:
 * - Orders are signed off-chain using EIP-712
 * - Relayers/makers submit matched orders for on-chain settlement
 * - Partial fills supported
 * - MEV protection via order hash commitment
 *
 * Order Types:
 * - Limit orders (maker + taker amounts fixed)
 * - Fill-or-kill (must fill entirely or revert)
 * - Good-til-cancelled (persists until cancelled)
 * - Immediate-or-cancel (fill what's possible, cancel rest)
 */
contract LimitOrderBook is AccessControl, ReentrancyGuard, EIP712 {
    using SafeERC20 for IERC20;
    using ECDSA for bytes32;

    // ============ Constants ============

    bytes32 public constant RELAYER_ROLE = keccak256("RELAYER_ROLE");
    bytes32 public constant FEE_MANAGER_ROLE = keccak256("FEE_MANAGER_ROLE");

    bytes32 public constant ORDER_TYPEHASH = keccak256(
        "Order(address maker,address taker,address makerToken,address takerToken,uint256 makerAmount,uint256 takerAmount,uint256 expiry,uint256 salt,uint8 orderType)"
    );

    uint256 public constant BASIS_POINTS = 10000;
    uint256 public constant MAX_FEE = 100; // 1%

    // ============ Enums ============

    enum OrderType {
        Limit,
        FillOrKill,
        ImmediateOrCancel
    }

    enum OrderStatus {
        Invalid,
        Fillable,
        Filled,
        Cancelled,
        Expired
    }

    // ============ Structs ============

    struct Order {
        address maker;
        address taker;          // 0x0 = any taker
        address makerToken;
        address takerToken;
        uint256 makerAmount;
        uint256 takerAmount;
        uint256 expiry;
        uint256 salt;
        OrderType orderType;
    }

    struct OrderInfo {
        bytes32 orderHash;
        OrderStatus status;
        uint256 filledAmount;
    }

    struct FillResult {
        uint256 makerTokenFilledAmount;
        uint256 takerTokenFilledAmount;
        uint256 makerFeePaid;
        uint256 takerFeePaid;
    }

    // ============ State Variables ============

    /// @notice Filled amounts per order hash
    mapping(bytes32 => uint256) public orderFilled;

    /// @notice Cancelled orders
    mapping(bytes32 => bool) public orderCancelled;

    /// @notice User cancelled all orders before this timestamp
    mapping(address => uint256) public userCancelTimestamp;

    /// @notice Fee rates (in basis points)
    uint256 public makerFee;
    uint256 public takerFee;

    /// @notice Fee recipient
    address public feeRecipient;

    /// @notice Allowed trading pairs
    mapping(address => mapping(address => bool)) public allowedPairs;
    bool public pairRestrictionEnabled;

    /// @notice Order commitment for MEV protection
    mapping(bytes32 => uint256) public orderCommitments;
    uint256 public commitmentDelay = 1; // blocks

    /// @notice Protocol pause
    bool public paused;

    // ============ Events ============

    event OrderFilled(
        bytes32 indexed orderHash,
        address indexed maker,
        address indexed taker,
        address makerToken,
        address takerToken,
        uint256 makerAmount,
        uint256 takerAmount
    );
    
    event OrderCancelled(bytes32 indexed orderHash, address indexed maker);
    event AllOrdersCancelled(address indexed maker, uint256 timestamp);
    event OrderCommitted(bytes32 indexed commitment, address indexed relayer);
    event FeesUpdated(uint256 makerFee, uint256 takerFee);
    event PairStatusUpdated(address indexed tokenA, address indexed tokenB, bool allowed);

    // ============ Errors ============

    error OrderExpired();
    error OrderCancelledError();
    error OrderNotFillable();
    error InvalidSignature();
    error InvalidTaker();
    error InsufficientFillAmount();
    error ExcessiveFillAmount();
    error PairNotAllowed();
    error CommitmentRequired();
    error CommitmentTooRecent();
    error ContractPaused();
    error InvalidFee();
    error ZeroAmount();

    // ============ Constructor ============

    constructor(
        address _admin,
        address _feeRecipient
    ) EIP712("LimitOrderBook", "1") {
        require(_admin != address(0), "Invalid admin");
        
        feeRecipient = _feeRecipient;
        
        _grantRole(DEFAULT_ADMIN_ROLE, _admin);
        _grantRole(RELAYER_ROLE, _admin);
        _grantRole(FEE_MANAGER_ROLE, _admin);
    }

    // ============ Order Functions ============

    /**
     * @notice Fill a signed order
     * @param order Order struct
     * @param signature EIP-712 signature
     * @param takerFillAmount Amount of taker tokens to fill
     */
    function fillOrder(
        Order calldata order,
        bytes calldata signature,
        uint256 takerFillAmount
    ) external nonReentrant returns (FillResult memory result) {
        if (paused) revert ContractPaused();
        if (takerFillAmount == 0) revert ZeroAmount();
        
        // Verify order
        OrderInfo memory info = getOrderInfo(order);
        if (info.status != OrderStatus.Fillable) revert OrderNotFillable();
        
        // Verify signature
        bytes32 digest = _hashTypedDataV4(_hashOrder(order));
        address signer = digest.recover(signature);
        if (signer != order.maker) revert InvalidSignature();
        
        // Verify taker
        if (order.taker != address(0) && order.taker != msg.sender) {
            revert InvalidTaker();
        }
        
        // Check pair allowance
        if (pairRestrictionEnabled && !allowedPairs[order.makerToken][order.takerToken]) {
            revert PairNotAllowed();
        }
        
        // Calculate fill amounts
        uint256 remainingTakerAmount = order.takerAmount - orderFilled[info.orderHash];
        
        uint256 actualTakerFill;
        if (order.orderType == OrderType.FillOrKill) {
            if (takerFillAmount < order.takerAmount) revert InsufficientFillAmount();
            actualTakerFill = order.takerAmount;
        } else if (order.orderType == OrderType.ImmediateOrCancel) {
            actualTakerFill = takerFillAmount > remainingTakerAmount ? remainingTakerAmount : takerFillAmount;
        } else {
            if (takerFillAmount > remainingTakerAmount) revert ExcessiveFillAmount();
            actualTakerFill = takerFillAmount;
        }
        
        // Calculate maker fill proportionally
        uint256 actualMakerFill = (order.makerAmount * actualTakerFill) / order.takerAmount;
        
        // Update filled amount
        orderFilled[info.orderHash] += actualTakerFill;
        
        // Calculate fees
        result.makerFeePaid = (actualMakerFill * makerFee) / BASIS_POINTS;
        result.takerFeePaid = (actualTakerFill * takerFee) / BASIS_POINTS;
        result.makerTokenFilledAmount = actualMakerFill;
        result.takerTokenFilledAmount = actualTakerFill;
        
        // Execute transfers
        _settleOrder(order, actualMakerFill, actualTakerFill, result.makerFeePaid, result.takerFeePaid);
        
        emit OrderFilled(
            info.orderHash,
            order.maker,
            msg.sender,
            order.makerToken,
            order.takerToken,
            actualMakerFill,
            actualTakerFill
        );
    }

    /**
     * @notice Fill multiple orders in batch
     * @param orders Array of orders
     * @param signatures Array of signatures
     * @param takerFillAmounts Array of fill amounts
     */
    function batchFillOrders(
        Order[] calldata orders,
        bytes[] calldata signatures,
        uint256[] calldata takerFillAmounts
    ) external nonReentrant returns (FillResult[] memory results) {
        if (paused) revert ContractPaused();
        require(orders.length == signatures.length && orders.length == takerFillAmounts.length, "Length mismatch");
        
        results = new FillResult[](orders.length);
        
        for (uint256 i = 0; i < orders.length; i++) {
            // Try to fill, don't revert on individual failures
            try this.fillOrderInternal(orders[i], signatures[i], takerFillAmounts[i], msg.sender) returns (FillResult memory r) {
                results[i] = r;
            } catch {
                // Leave result as zero
            }
        }
    }

    /**
     * @notice Fill with commitment for MEV protection
     * @param commitment Hash of (orderHash, taker, fillAmount, salt)
     */
    function commitOrder(bytes32 commitment) external {
        orderCommitments[commitment] = block.number;
        emit OrderCommitted(commitment, msg.sender);
    }

    /**
     * @notice Fill order with prior commitment
     * @param order Order struct
     * @param signature EIP-712 signature
     * @param takerFillAmount Fill amount
     * @param salt Salt used in commitment
     */
    function fillOrderWithCommitment(
        Order calldata order,
        bytes calldata signature,
        uint256 takerFillAmount,
        bytes32 salt
    ) external nonReentrant returns (FillResult memory result) {
        bytes32 orderHash = _hashOrder(order);
        bytes32 commitment = keccak256(abi.encodePacked(orderHash, msg.sender, takerFillAmount, salt));
        
        uint256 commitBlock = orderCommitments[commitment];
        if (commitBlock == 0) revert CommitmentRequired();
        if (block.number < commitBlock + commitmentDelay) revert CommitmentTooRecent();
        
        // Clear commitment
        delete orderCommitments[commitment];
        
        // Fill order through internal function
        return this.fillOrderInternal(order, signature, takerFillAmount, msg.sender);
    }

    /**
     * @notice Internal fill function (called by batch and commitment fills)
     */
    function fillOrderInternal(
        Order calldata order,
        bytes calldata signature,
        uint256 takerFillAmount,
        address taker
    ) external returns (FillResult memory result) {
        require(msg.sender == address(this), "Internal only");
        
        if (paused) revert ContractPaused();
        if (takerFillAmount == 0) revert ZeroAmount();
        
        OrderInfo memory info = getOrderInfo(order);
        if (info.status != OrderStatus.Fillable) revert OrderNotFillable();
        
        bytes32 digest = _hashTypedDataV4(_hashOrder(order));
        address signer = digest.recover(signature);
        if (signer != order.maker) revert InvalidSignature();
        
        if (order.taker != address(0) && order.taker != taker) {
            revert InvalidTaker();
        }
        
        uint256 remainingTakerAmount = order.takerAmount - orderFilled[info.orderHash];
        uint256 actualTakerFill = takerFillAmount > remainingTakerAmount ? remainingTakerAmount : takerFillAmount;
        uint256 actualMakerFill = (order.makerAmount * actualTakerFill) / order.takerAmount;
        
        orderFilled[info.orderHash] += actualTakerFill;
        
        result.makerFeePaid = (actualMakerFill * makerFee) / BASIS_POINTS;
        result.takerFeePaid = (actualTakerFill * takerFee) / BASIS_POINTS;
        result.makerTokenFilledAmount = actualMakerFill;
        result.takerTokenFilledAmount = actualTakerFill;
        
        _settleOrderFrom(order, actualMakerFill, actualTakerFill, result.makerFeePaid, result.takerFeePaid, taker);
        
        emit OrderFilled(
            info.orderHash,
            order.maker,
            taker,
            order.makerToken,
            order.takerToken,
            actualMakerFill,
            actualTakerFill
        );
    }

    // ============ Cancellation ============

    /**
     * @notice Cancel a specific order
     * @param order Order to cancel
     */
    function cancelOrder(Order calldata order) external {
        require(msg.sender == order.maker, "Not maker");
        
        bytes32 orderHash = getOrderHash(order);
        orderCancelled[orderHash] = true;
        
        emit OrderCancelled(orderHash, msg.sender);
    }

    /**
     * @notice Cancel multiple orders
     * @param orders Orders to cancel
     */
    function batchCancelOrders(Order[] calldata orders) external {
        for (uint256 i = 0; i < orders.length; i++) {
            require(msg.sender == orders[i].maker, "Not maker");
            
            bytes32 orderHash = getOrderHash(orders[i]);
            orderCancelled[orderHash] = true;
            
            emit OrderCancelled(orderHash, msg.sender);
        }
    }

    /**
     * @notice Cancel all orders before current timestamp
     */
    function cancelAllOrders() external {
        userCancelTimestamp[msg.sender] = block.timestamp;
        emit AllOrdersCancelled(msg.sender, block.timestamp);
    }

    // ============ Internal Functions ============

    function _settleOrder(
        Order calldata order,
        uint256 makerAmount,
        uint256 takerAmount,
        uint256 makerFee_,
        uint256 takerFee_
    ) internal {
        _settleOrderFrom(order, makerAmount, takerAmount, makerFee_, takerFee_, msg.sender);
    }

    function _settleOrderFrom(
        Order calldata order,
        uint256 makerAmount,
        uint256 takerAmount,
        uint256 makerFee_,
        uint256 takerFee_,
        address taker
    ) internal {
        // Taker sends takerToken to maker (minus fee)
        IERC20(order.takerToken).safeTransferFrom(taker, order.maker, takerAmount - takerFee_);
        
        // Maker sends makerToken to taker (minus fee)
        IERC20(order.makerToken).safeTransferFrom(order.maker, taker, makerAmount - makerFee_);
        
        // Collect fees
        if (feeRecipient != address(0)) {
            if (makerFee_ > 0) {
                IERC20(order.makerToken).safeTransferFrom(order.maker, feeRecipient, makerFee_);
            }
            if (takerFee_ > 0) {
                IERC20(order.takerToken).safeTransferFrom(taker, feeRecipient, takerFee_);
            }
        }
    }

    function _hashOrder(Order calldata order) internal pure returns (bytes32) {
        return keccak256(abi.encode(
            ORDER_TYPEHASH,
            order.maker,
            order.taker,
            order.makerToken,
            order.takerToken,
            order.makerAmount,
            order.takerAmount,
            order.expiry,
            order.salt,
            uint8(order.orderType)
        ));
    }

    // ============ View Functions ============

    /**
     * @notice Get order hash
     * @param order Order struct
     */
    function getOrderHash(Order calldata order) public view returns (bytes32) {
        return _hashTypedDataV4(_hashOrder(order));
    }

    /**
     * @notice Get order info
     * @param order Order struct
     */
    function getOrderInfo(Order calldata order) public view returns (OrderInfo memory info) {
        info.orderHash = getOrderHash(order);
        info.filledAmount = orderFilled[info.orderHash];
        
        if (orderCancelled[info.orderHash]) {
            info.status = OrderStatus.Cancelled;
        } else if (block.timestamp >= order.expiry) {
            info.status = OrderStatus.Expired;
        } else if (order.salt < userCancelTimestamp[order.maker]) {
            info.status = OrderStatus.Cancelled;
        } else if (info.filledAmount >= order.takerAmount) {
            info.status = OrderStatus.Filled;
        } else {
            info.status = OrderStatus.Fillable;
        }
    }

    /**
     * @notice Get remaining fillable amount
     * @param order Order struct
     */
    function getRemainingFillableAmount(Order calldata order) external view returns (uint256) {
        OrderInfo memory info = getOrderInfo(order);
        if (info.status != OrderStatus.Fillable) return 0;
        return order.takerAmount - info.filledAmount;
    }

    /**
     * @notice Verify order signature
     * @param order Order struct
     * @param signature Signature bytes
     */
    function isValidSignature(
        Order calldata order,
        bytes calldata signature
    ) external view returns (bool) {
        bytes32 digest = _hashTypedDataV4(_hashOrder(order));
        return digest.recover(signature) == order.maker;
    }

    /**
     * @notice Get domain separator for EIP-712
     */
    function DOMAIN_SEPARATOR() external view returns (bytes32) {
        return _domainSeparatorV4();
    }

    // ============ Admin Functions ============

    function setFees(uint256 _makerFee, uint256 _takerFee) external onlyRole(FEE_MANAGER_ROLE) {
        if (_makerFee > MAX_FEE || _takerFee > MAX_FEE) revert InvalidFee();
        makerFee = _makerFee;
        takerFee = _takerFee;
        emit FeesUpdated(_makerFee, _takerFee);
    }

    function setFeeRecipient(address _recipient) external onlyRole(DEFAULT_ADMIN_ROLE) {
        feeRecipient = _recipient;
    }

    function setPairAllowed(address tokenA, address tokenB, bool allowed) external onlyRole(DEFAULT_ADMIN_ROLE) {
        allowedPairs[tokenA][tokenB] = allowed;
        allowedPairs[tokenB][tokenA] = allowed;
        emit PairStatusUpdated(tokenA, tokenB, allowed);
    }

    function setPairRestriction(bool enabled) external onlyRole(DEFAULT_ADMIN_ROLE) {
        pairRestrictionEnabled = enabled;
    }

    function setCommitmentDelay(uint256 delay) external onlyRole(DEFAULT_ADMIN_ROLE) {
        commitmentDelay = delay;
    }

    function setPaused(bool _paused) external onlyRole(DEFAULT_ADMIN_ROLE) {
        paused = _paused;
    }
}

/**
 * @title OrderMatcher
 * @notice Matches orders from the order book
 * @dev Used by relayers to match compatible orders
 */
contract OrderMatcher is AccessControl {
    using ECDSA for bytes32;

    bytes32 public constant MATCHER_ROLE = keccak256("MATCHER_ROLE");

    LimitOrderBook public immutable orderBook;

    event OrdersMatched(
        bytes32 indexed makerOrderHash,
        bytes32 indexed takerOrderHash,
        uint256 amount
    );

    constructor(address _orderBook, address _admin) {
        orderBook = LimitOrderBook(_orderBook);
        _grantRole(DEFAULT_ADMIN_ROLE, _admin);
        _grantRole(MATCHER_ROLE, _admin);
    }

    /**
     * @notice Match two compatible orders
     * @param makerOrder The maker order
     * @param takerOrder The taker order (opposite side)
     * @param makerSignature Maker's signature
     * @param takerSignature Taker's signature
     * @param fillAmount Amount to fill
     */
    function matchOrders(
        LimitOrderBook.Order calldata makerOrder,
        LimitOrderBook.Order calldata takerOrder,
        bytes calldata makerSignature,
        bytes calldata takerSignature,
        uint256 fillAmount
    ) external onlyRole(MATCHER_ROLE) {
        // Verify orders are compatible
        require(makerOrder.makerToken == takerOrder.takerToken, "Token mismatch");
        require(makerOrder.takerToken == takerOrder.makerToken, "Token mismatch");
        
        // Verify price compatibility (maker price <= taker price)
        // makerPrice = takerAmount / makerAmount
        // takerPrice = makerAmount / takerAmount
        uint256 makerPrice = (makerOrder.takerAmount * 1e18) / makerOrder.makerAmount;
        uint256 takerPrice = (takerOrder.makerAmount * 1e18) / takerOrder.takerAmount;
        require(makerPrice <= takerPrice, "Price mismatch");
        
        // Fill both orders
        orderBook.fillOrder(makerOrder, makerSignature, fillAmount);
        
        // Calculate equivalent amount for taker order
        uint256 takerFillAmount = (fillAmount * takerOrder.takerAmount) / takerOrder.makerAmount;
        orderBook.fillOrder(takerOrder, takerSignature, takerFillAmount);
        
        emit OrdersMatched(
            orderBook.getOrderHash(makerOrder),
            orderBook.getOrderHash(takerOrder),
            fillAmount
        );
    }
}
