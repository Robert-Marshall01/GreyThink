// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/Pausable.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";

/**
 * @title OrderBookExchange
 * @author Grey Team
 * @notice Central Limit Order Book (CLOB) DEX with price-time priority matching
 * @dev Implements professional-grade order book with limit/market orders,
 *      partial fills, maker/taker fees, and self-trade prevention
 * 
 * Architecture inspired by dYdX, Serum, and traditional exchange matching engines:
 * - Red-black tree-like price level ordering (implemented via sorted linked list)
 * - Price-time priority (FIFO at each price level)
 * - Immediate-or-Cancel (IOC) and Good-Till-Cancelled (GTC) order types
 * - Post-only orders for market makers
 * - Fill-or-Kill (FOK) orders
 */
contract OrderBookExchange is AccessControl, ReentrancyGuard, Pausable {
    using SafeERC20 for IERC20;

    // ============ Roles ============
    bytes32 public constant OPERATOR_ROLE = keccak256("OPERATOR_ROLE");
    bytes32 public constant FEE_MANAGER_ROLE = keccak256("FEE_MANAGER_ROLE");
    bytes32 public constant MARKET_MAKER_ROLE = keccak256("MARKET_MAKER_ROLE");

    // ============ Enums ============
    enum OrderSide { BUY, SELL }
    enum OrderType { LIMIT, MARKET }
    enum TimeInForce { GTC, IOC, FOK, POST_ONLY }
    enum OrderStatus { OPEN, PARTIALLY_FILLED, FILLED, CANCELLED, EXPIRED }

    // ============ Structs ============
    
    /**
     * @notice Trading pair configuration
     */
    struct TradingPair {
        address baseToken;      // Token being traded (e.g., WETH)
        address quoteToken;     // Quote currency (e.g., USDC)
        uint256 minOrderSize;   // Minimum order size in base token
        uint256 tickSize;       // Price increment (in quote token decimals)
        uint256 lotSize;        // Quantity increment (in base token decimals)
        uint256 makerFee;       // Fee for makers (basis points)
        uint256 takerFee;       // Fee for takers (basis points)
        bool isActive;
        uint256 lastTradePrice;
        uint256 volume24h;
        uint256 lastVolumeReset;
    }

    /**
     * @notice Individual order in the book
     */
    struct Order {
        uint256 id;
        bytes32 pairId;
        address trader;
        OrderSide side;
        OrderType orderType;
        TimeInForce timeInForce;
        uint256 price;          // Price in quote token per base token (scaled by PRICE_PRECISION)
        uint256 quantity;       // Original quantity
        uint256 filledQuantity; // Amount filled so far
        uint256 timestamp;
        uint256 expiry;         // 0 = no expiry
        OrderStatus status;
        uint256 nextOrderId;    // Linked list pointer
        uint256 prevOrderId;    // Linked list pointer
    }

    /**
     * @notice Price level in the order book
     */
    struct PriceLevel {
        uint256 price;
        uint256 totalQuantity;  // Sum of all order quantities at this price
        uint256 orderCount;
        uint256 headOrderId;    // First order (oldest) at this price
        uint256 tailOrderId;    // Last order (newest) at this price
        uint256 nextPriceLevel; // For price level linked list
        uint256 prevPriceLevel;
    }

    /**
     * @notice Trade execution record
     */
    struct Trade {
        uint256 id;
        bytes32 pairId;
        uint256 makerOrderId;
        uint256 takerOrderId;
        address maker;
        address taker;
        uint256 price;
        uint256 quantity;
        uint256 makerFee;
        uint256 takerFee;
        uint256 timestamp;
        bool makerIsBuyer;
    }

    /**
     * @notice User balance tracking
     */
    struct UserBalance {
        uint256 available;      // Available for trading
        uint256 locked;         // Locked in open orders
        uint256 totalDeposited;
        uint256 totalWithdrawn;
    }

    // ============ Constants ============
    uint256 public constant PRICE_PRECISION = 1e18;
    uint256 public constant FEE_DENOMINATOR = 10000;
    uint256 public constant MAX_FEE = 100; // 1%
    uint256 public constant MAX_ORDERS_PER_MATCH = 50;
    uint256 public constant VOLUME_RESET_PERIOD = 24 hours;

    // ============ State Variables ============
    
    // Order tracking
    uint256 public nextOrderId = 1;
    uint256 public nextTradeId = 1;
    mapping(uint256 => Order) public orders;
    mapping(address => uint256[]) public userOrders;
    
    // Trading pairs
    mapping(bytes32 => TradingPair) public tradingPairs;
    bytes32[] public pairIds;
    
    // Order book structure: pairId => side => price => PriceLevel
    mapping(bytes32 => mapping(OrderSide => mapping(uint256 => PriceLevel))) public priceLevels;
    mapping(bytes32 => mapping(OrderSide => uint256)) public bestPrice; // Best bid/ask
    
    // User balances: token => user => balance
    mapping(address => mapping(address => UserBalance)) public balances;
    
    // Trade history
    mapping(uint256 => Trade) public trades;
    mapping(bytes32 => uint256[]) public pairTrades;
    
    // Fee collection
    address public feeRecipient;
    mapping(address => uint256) public collectedFees;
    
    // Self-trade prevention
    mapping(address => bytes32) public selfTradePreventionGroup;
    
    // Market maker rebates
    mapping(address => uint256) public makerRebates; // Negative fees for makers

    // ============ Events ============
    event TradingPairCreated(
        bytes32 indexed pairId,
        address indexed baseToken,
        address indexed quoteToken,
        uint256 tickSize,
        uint256 lotSize
    );
    
    event Deposit(
        address indexed user,
        address indexed token,
        uint256 amount
    );
    
    event Withdrawal(
        address indexed user,
        address indexed token,
        uint256 amount
    );
    
    event OrderPlaced(
        uint256 indexed orderId,
        bytes32 indexed pairId,
        address indexed trader,
        OrderSide side,
        OrderType orderType,
        uint256 price,
        uint256 quantity
    );
    
    event OrderCancelled(
        uint256 indexed orderId,
        address indexed trader,
        uint256 unfilledQuantity
    );
    
    event OrderFilled(
        uint256 indexed orderId,
        uint256 filledQuantity,
        uint256 remainingQuantity
    );
    
    event TradeExecuted(
        uint256 indexed tradeId,
        bytes32 indexed pairId,
        uint256 indexed makerOrderId,
        uint256 takerOrderId,
        uint256 price,
        uint256 quantity
    );
    
    event PriceLevelUpdated(
        bytes32 indexed pairId,
        OrderSide indexed side,
        uint256 price,
        uint256 totalQuantity
    );

    // ============ Errors ============
    error InvalidPair();
    error InvalidPrice();
    error InvalidQuantity();
    error InsufficientBalance();
    error OrderNotFound();
    error NotOrderOwner();
    error OrderNotCancellable();
    error PairAlreadyExists();
    error PairNotActive();
    error BelowMinOrderSize();
    error InvalidTickSize();
    error InvalidLotSize();
    error PostOnlyWouldTake();
    error FillOrKillNotFilled();
    error OrderExpired();
    error SelfTradeNotAllowed();
    error InvalidFee();
    error WithdrawalFailed();

    // ============ Constructor ============
    constructor(address _feeRecipient) {
        require(_feeRecipient != address(0), "Invalid fee recipient");
        
        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(OPERATOR_ROLE, msg.sender);
        _grantRole(FEE_MANAGER_ROLE, msg.sender);
        
        feeRecipient = _feeRecipient;
    }

    // ============ Deposit/Withdraw ============
    
    /**
     * @notice Deposit tokens for trading
     * @param token Token address
     * @param amount Amount to deposit
     */
    function deposit(address token, uint256 amount) external nonReentrant {
        require(amount > 0, "Amount must be > 0");
        
        IERC20(token).safeTransferFrom(msg.sender, address(this), amount);
        
        balances[token][msg.sender].available += amount;
        balances[token][msg.sender].totalDeposited += amount;
        
        emit Deposit(msg.sender, token, amount);
    }

    /**
     * @notice Withdraw tokens
     * @param token Token address
     * @param amount Amount to withdraw
     */
    function withdraw(address token, uint256 amount) external nonReentrant {
        UserBalance storage balance = balances[token][msg.sender];
        
        if (amount > balance.available) revert InsufficientBalance();
        
        balance.available -= amount;
        balance.totalWithdrawn += amount;
        
        IERC20(token).safeTransfer(msg.sender, amount);
        
        emit Withdrawal(msg.sender, token, amount);
    }

    // ============ Trading Pair Management ============
    
    /**
     * @notice Create a new trading pair
     */
    function createTradingPair(
        address baseToken,
        address quoteToken,
        uint256 minOrderSize,
        uint256 tickSize,
        uint256 lotSize,
        uint256 makerFee,
        uint256 takerFee
    ) external onlyRole(OPERATOR_ROLE) {
        bytes32 pairId = keccak256(abi.encodePacked(baseToken, quoteToken));
        
        if (tradingPairs[pairId].baseToken != address(0)) revert PairAlreadyExists();
        if (makerFee > MAX_FEE || takerFee > MAX_FEE) revert InvalidFee();
        
        tradingPairs[pairId] = TradingPair({
            baseToken: baseToken,
            quoteToken: quoteToken,
            minOrderSize: minOrderSize,
            tickSize: tickSize,
            lotSize: lotSize,
            makerFee: makerFee,
            takerFee: takerFee,
            isActive: true,
            lastTradePrice: 0,
            volume24h: 0,
            lastVolumeReset: block.timestamp
        });
        
        pairIds.push(pairId);
        
        emit TradingPairCreated(pairId, baseToken, quoteToken, tickSize, lotSize);
    }

    // ============ Order Placement ============
    
    /**
     * @notice Place a limit order
     * @param pairId Trading pair identifier
     * @param side Buy or Sell
     * @param price Limit price (scaled by PRICE_PRECISION)
     * @param quantity Order quantity in base token
     * @param timeInForce Order time in force
     * @param expiry Order expiry timestamp (0 for no expiry)
     */
    function placeLimitOrder(
        bytes32 pairId,
        OrderSide side,
        uint256 price,
        uint256 quantity,
        TimeInForce timeInForce,
        uint256 expiry
    ) external nonReentrant whenNotPaused returns (uint256 orderId) {
        TradingPair storage pair = tradingPairs[pairId];
        if (pair.baseToken == address(0) || !pair.isActive) revert PairNotActive();
        
        // Validate price and quantity
        if (price == 0 || price % pair.tickSize != 0) revert InvalidTickSize();
        if (quantity < pair.minOrderSize || quantity % pair.lotSize != 0) revert InvalidLotSize();
        
        // Calculate required balance
        uint256 requiredBalance = _calculateRequiredBalance(pair, side, price, quantity);
        address lockToken = side == OrderSide.BUY ? pair.quoteToken : pair.baseToken;
        
        if (balances[lockToken][msg.sender].available < requiredBalance) {
            revert InsufficientBalance();
        }
        
        // Lock funds
        balances[lockToken][msg.sender].available -= requiredBalance;
        balances[lockToken][msg.sender].locked += requiredBalance;
        
        // Create order
        orderId = nextOrderId++;
        orders[orderId] = Order({
            id: orderId,
            pairId: pairId,
            trader: msg.sender,
            side: side,
            orderType: OrderType.LIMIT,
            timeInForce: timeInForce,
            price: price,
            quantity: quantity,
            filledQuantity: 0,
            timestamp: block.timestamp,
            expiry: expiry,
            status: OrderStatus.OPEN,
            nextOrderId: 0,
            prevOrderId: 0
        });
        
        userOrders[msg.sender].push(orderId);
        
        emit OrderPlaced(orderId, pairId, msg.sender, side, OrderType.LIMIT, price, quantity);
        
        // Match order
        uint256 filledQuantity = _matchOrder(orderId);
        
        // Handle time in force
        Order storage order = orders[orderId];
        uint256 remainingQuantity = order.quantity - order.filledQuantity;
        
        if (timeInForce == TimeInForce.IOC) {
            // Cancel any unfilled portion
            if (remainingQuantity > 0) {
                _cancelOrder(orderId);
            }
        } else if (timeInForce == TimeInForce.FOK) {
            // Must be fully filled
            if (order.filledQuantity < order.quantity) {
                revert FillOrKillNotFilled();
            }
        } else if (timeInForce == TimeInForce.POST_ONLY) {
            // Should not have matched
            if (filledQuantity > 0) {
                revert PostOnlyWouldTake();
            }
            _addToBook(orderId);
        } else {
            // GTC - add remaining to book
            if (remainingQuantity > 0 && order.status == OrderStatus.OPEN) {
                _addToBook(orderId);
            }
        }
        
        return orderId;
    }

    /**
     * @notice Place a market order
     */
    function placeMarketOrder(
        bytes32 pairId,
        OrderSide side,
        uint256 quantity
    ) external nonReentrant whenNotPaused returns (uint256 orderId) {
        TradingPair storage pair = tradingPairs[pairId];
        if (pair.baseToken == address(0) || !pair.isActive) revert PairNotActive();
        
        if (quantity < pair.minOrderSize || quantity % pair.lotSize != 0) revert InvalidLotSize();
        
        // For market orders, we need to estimate the required balance
        // Use a very high/low price estimate
        uint256 estimatedPrice = side == OrderSide.BUY ? type(uint256).max / 2 : 1;
        uint256 requiredBalance = _calculateRequiredBalance(pair, side, estimatedPrice, quantity);
        address lockToken = side == OrderSide.BUY ? pair.quoteToken : pair.baseToken;
        
        if (balances[lockToken][msg.sender].available < requiredBalance) {
            revert InsufficientBalance();
        }
        
        // Lock all available for market orders (will refund unused)
        uint256 actualLock = side == OrderSide.BUY 
            ? balances[lockToken][msg.sender].available 
            : quantity;
            
        balances[lockToken][msg.sender].available -= actualLock;
        balances[lockToken][msg.sender].locked += actualLock;
        
        // Create order
        orderId = nextOrderId++;
        orders[orderId] = Order({
            id: orderId,
            pairId: pairId,
            trader: msg.sender,
            side: side,
            orderType: OrderType.MARKET,
            timeInForce: TimeInForce.IOC,
            price: 0, // Market order has no limit price
            quantity: quantity,
            filledQuantity: 0,
            timestamp: block.timestamp,
            expiry: 0,
            status: OrderStatus.OPEN,
            nextOrderId: 0,
            prevOrderId: 0
        });
        
        userOrders[msg.sender].push(orderId);
        
        emit OrderPlaced(orderId, pairId, msg.sender, side, OrderType.MARKET, 0, quantity);
        
        // Match order
        _matchOrder(orderId);
        
        // Cancel any unfilled portion (IOC behavior)
        Order storage order = orders[orderId];
        if (order.filledQuantity < order.quantity) {
            _cancelOrder(orderId);
        }
        
        return orderId;
    }

    /**
     * @notice Cancel an open order
     */
    function cancelOrder(uint256 orderId) external nonReentrant {
        Order storage order = orders[orderId];
        
        if (order.id == 0) revert OrderNotFound();
        if (order.trader != msg.sender) revert NotOrderOwner();
        if (order.status != OrderStatus.OPEN && order.status != OrderStatus.PARTIALLY_FILLED) {
            revert OrderNotCancellable();
        }
        
        _removeFromBook(orderId);
        _cancelOrder(orderId);
    }

    /**
     * @notice Cancel multiple orders
     */
    function cancelOrders(uint256[] calldata orderIds) external nonReentrant {
        for (uint256 i = 0; i < orderIds.length; i++) {
            Order storage order = orders[orderIds[i]];
            
            if (order.id == 0 || order.trader != msg.sender) continue;
            if (order.status != OrderStatus.OPEN && order.status != OrderStatus.PARTIALLY_FILLED) continue;
            
            _removeFromBook(orderIds[i]);
            _cancelOrder(orderIds[i]);
        }
    }

    // ============ Internal Matching Engine ============
    
    /**
     * @notice Match an incoming order against the book
     */
    function _matchOrder(uint256 takerOrderId) internal returns (uint256 totalFilled) {
        Order storage takerOrder = orders[takerOrderId];
        TradingPair storage pair = tradingPairs[takerOrder.pairId];
        
        // Determine the opposing side to match against
        OrderSide opposingSide = takerOrder.side == OrderSide.BUY ? OrderSide.SELL : OrderSide.BUY;
        
        uint256 remainingQuantity = takerOrder.quantity - takerOrder.filledQuantity;
        uint256 matchCount = 0;
        
        while (remainingQuantity > 0 && matchCount < MAX_ORDERS_PER_MATCH) {
            uint256 bestOpposingPrice = bestPrice[takerOrder.pairId][opposingSide];
            if (bestOpposingPrice == 0) break;
            
            // Check if price is acceptable
            bool priceAcceptable = takerOrder.orderType == OrderType.MARKET ||
                (takerOrder.side == OrderSide.BUY && takerOrder.price >= bestOpposingPrice) ||
                (takerOrder.side == OrderSide.SELL && takerOrder.price <= bestOpposingPrice);
            
            if (!priceAcceptable) break;
            
            PriceLevel storage level = priceLevels[takerOrder.pairId][opposingSide][bestOpposingPrice];
            uint256 makerOrderId = level.headOrderId;
            
            while (makerOrderId != 0 && remainingQuantity > 0 && matchCount < MAX_ORDERS_PER_MATCH) {
                Order storage makerOrder = orders[makerOrderId];
                uint256 nextMakerOrderId = makerOrder.nextOrderId;
                
                // Skip if same trader (self-trade prevention)
                if (_isSelfTrade(takerOrder.trader, makerOrder.trader)) {
                    makerOrderId = nextMakerOrderId;
                    continue;
                }
                
                // Check expiry
                if (makerOrder.expiry != 0 && makerOrder.expiry < block.timestamp) {
                    _removeFromBook(makerOrderId);
                    _cancelOrder(makerOrderId);
                    makerOrderId = nextMakerOrderId;
                    continue;
                }
                
                // Calculate fill quantity
                uint256 makerRemaining = makerOrder.quantity - makerOrder.filledQuantity;
                uint256 fillQuantity = remainingQuantity < makerRemaining ? remainingQuantity : makerRemaining;
                
                // Execute trade
                _executeTrade(
                    takerOrderId,
                    makerOrderId,
                    bestOpposingPrice,
                    fillQuantity
                );
                
                remainingQuantity -= fillQuantity;
                totalFilled += fillQuantity;
                matchCount++;
                
                // Update maker order
                makerOrder.filledQuantity += fillQuantity;
                if (makerOrder.filledQuantity >= makerOrder.quantity) {
                    makerOrder.status = OrderStatus.FILLED;
                    _removeFromBook(makerOrderId);
                } else {
                    makerOrder.status = OrderStatus.PARTIALLY_FILLED;
                }
                
                emit OrderFilled(makerOrderId, fillQuantity, makerOrder.quantity - makerOrder.filledQuantity);
                
                makerOrderId = nextMakerOrderId;
            }
            
            // Move to next price level if current is empty
            if (level.totalQuantity == 0) {
                _moveToBestPrice(takerOrder.pairId, opposingSide);
            }
        }
        
        // Update taker order
        takerOrder.filledQuantity += totalFilled;
        if (takerOrder.filledQuantity >= takerOrder.quantity) {
            takerOrder.status = OrderStatus.FILLED;
        } else if (takerOrder.filledQuantity > 0) {
            takerOrder.status = OrderStatus.PARTIALLY_FILLED;
        }
        
        if (totalFilled > 0) {
            emit OrderFilled(takerOrderId, totalFilled, takerOrder.quantity - takerOrder.filledQuantity);
        }
        
        return totalFilled;
    }

    /**
     * @notice Execute a trade between maker and taker
     */
    function _executeTrade(
        uint256 takerOrderId,
        uint256 makerOrderId,
        uint256 price,
        uint256 quantity
    ) internal {
        Order storage takerOrder = orders[takerOrderId];
        Order storage makerOrder = orders[makerOrderId];
        TradingPair storage pair = tradingPairs[takerOrder.pairId];
        
        // Calculate quote amount
        uint256 quoteAmount = (quantity * price) / PRICE_PRECISION;
        
        // Calculate fees
        uint256 makerFee = hasRole(MARKET_MAKER_ROLE, makerOrder.trader) 
            ? 0 
            : (quoteAmount * pair.makerFee) / FEE_DENOMINATOR;
        uint256 takerFee = (quoteAmount * pair.takerFee) / FEE_DENOMINATOR;
        
        // Apply maker rebates
        if (makerRebates[makerOrder.trader] > 0) {
            uint256 rebate = (quoteAmount * makerRebates[makerOrder.trader]) / FEE_DENOMINATOR;
            makerFee = rebate > makerFee ? 0 : makerFee - rebate;
        }
        
        // Settle balances
        if (takerOrder.side == OrderSide.BUY) {
            // Taker buys base with quote
            // Unlock and deduct quote from taker
            balances[pair.quoteToken][takerOrder.trader].locked -= quoteAmount + takerFee;
            
            // Credit base to taker
            balances[pair.baseToken][takerOrder.trader].available += quantity;
            
            // Unlock and deduct base from maker
            balances[pair.baseToken][makerOrder.trader].locked -= quantity;
            
            // Credit quote to maker (minus fee)
            balances[pair.quoteToken][makerOrder.trader].available += quoteAmount - makerFee;
        } else {
            // Taker sells base for quote
            // Unlock and deduct base from taker
            balances[pair.baseToken][takerOrder.trader].locked -= quantity;
            
            // Credit quote to taker (minus fee)
            balances[pair.quoteToken][takerOrder.trader].available += quoteAmount - takerFee;
            
            // Unlock and deduct quote from maker
            balances[pair.quoteToken][makerOrder.trader].locked -= quoteAmount;
            
            // Credit base to maker
            balances[pair.baseToken][makerOrder.trader].available += quantity - makerFee;
        }
        
        // Collect fees
        collectedFees[pair.quoteToken] += makerFee + takerFee;
        
        // Record trade
        uint256 tradeId = nextTradeId++;
        trades[tradeId] = Trade({
            id: tradeId,
            pairId: takerOrder.pairId,
            makerOrderId: makerOrderId,
            takerOrderId: takerOrderId,
            maker: makerOrder.trader,
            taker: takerOrder.trader,
            price: price,
            quantity: quantity,
            makerFee: makerFee,
            takerFee: takerFee,
            timestamp: block.timestamp,
            makerIsBuyer: makerOrder.side == OrderSide.BUY
        });
        
        pairTrades[takerOrder.pairId].push(tradeId);
        
        // Update pair stats
        pair.lastTradePrice = price;
        _updateVolume(pair, quantity);
        
        emit TradeExecuted(tradeId, takerOrder.pairId, makerOrderId, takerOrderId, price, quantity);
    }

    // ============ Order Book Management ============
    
    /**
     * @notice Add order to the book at its price level
     */
    function _addToBook(uint256 orderId) internal {
        Order storage order = orders[orderId];
        PriceLevel storage level = priceLevels[order.pairId][order.side][order.price];
        
        uint256 remainingQuantity = order.quantity - order.filledQuantity;
        
        if (level.price == 0) {
            // New price level
            level.price = order.price;
            level.headOrderId = orderId;
            level.tailOrderId = orderId;
            level.totalQuantity = remainingQuantity;
            level.orderCount = 1;
            
            // Insert into price level linked list
            _insertPriceLevel(order.pairId, order.side, order.price);
        } else {
            // Add to existing level
            orders[level.tailOrderId].nextOrderId = orderId;
            order.prevOrderId = level.tailOrderId;
            level.tailOrderId = orderId;
            level.totalQuantity += remainingQuantity;
            level.orderCount++;
        }
        
        // Update best price
        _updateBestPrice(order.pairId, order.side, order.price);
        
        emit PriceLevelUpdated(order.pairId, order.side, order.price, level.totalQuantity);
    }

    /**
     * @notice Remove order from the book
     */
    function _removeFromBook(uint256 orderId) internal {
        Order storage order = orders[orderId];
        if (order.price == 0) return;
        
        PriceLevel storage level = priceLevels[order.pairId][order.side][order.price];
        
        uint256 remainingQuantity = order.quantity - order.filledQuantity;
        
        // Update linked list
        if (order.prevOrderId != 0) {
            orders[order.prevOrderId].nextOrderId = order.nextOrderId;
        } else {
            level.headOrderId = order.nextOrderId;
        }
        
        if (order.nextOrderId != 0) {
            orders[order.nextOrderId].prevOrderId = order.prevOrderId;
        } else {
            level.tailOrderId = order.prevOrderId;
        }
        
        level.totalQuantity -= remainingQuantity;
        level.orderCount--;
        
        // Clean up if level is empty
        if (level.orderCount == 0) {
            _removePriceLevel(order.pairId, order.side, order.price);
        }
        
        // Clear order pointers
        order.prevOrderId = 0;
        order.nextOrderId = 0;
        
        emit PriceLevelUpdated(order.pairId, order.side, order.price, level.totalQuantity);
    }

    /**
     * @notice Insert a new price level in sorted order
     */
    function _insertPriceLevel(bytes32 pairId, OrderSide side, uint256 price) internal {
        uint256 currentBest = bestPrice[pairId][side];
        
        if (currentBest == 0) {
            bestPrice[pairId][side] = price;
            return;
        }
        
        // For bids, best = highest price
        // For asks, best = lowest price
        bool isBetter = side == OrderSide.BUY 
            ? price > currentBest 
            : price < currentBest;
        
        if (isBetter) {
            priceLevels[pairId][side][price].nextPriceLevel = currentBest;
            priceLevels[pairId][side][currentBest].prevPriceLevel = price;
            bestPrice[pairId][side] = price;
        } else {
            // Find insertion point
            uint256 current = currentBest;
            uint256 next = priceLevels[pairId][side][current].nextPriceLevel;
            
            while (next != 0) {
                bool shouldInsert = side == OrderSide.BUY 
                    ? price > next 
                    : price < next;
                
                if (shouldInsert) break;
                
                current = next;
                next = priceLevels[pairId][side][current].nextPriceLevel;
            }
            
            priceLevels[pairId][side][price].prevPriceLevel = current;
            priceLevels[pairId][side][price].nextPriceLevel = next;
            priceLevels[pairId][side][current].nextPriceLevel = price;
            
            if (next != 0) {
                priceLevels[pairId][side][next].prevPriceLevel = price;
            }
        }
    }

    /**
     * @notice Remove a price level from the linked list
     */
    function _removePriceLevel(bytes32 pairId, OrderSide side, uint256 price) internal {
        PriceLevel storage level = priceLevels[pairId][side][price];
        
        if (level.prevPriceLevel != 0) {
            priceLevels[pairId][side][level.prevPriceLevel].nextPriceLevel = level.nextPriceLevel;
        } else {
            bestPrice[pairId][side] = level.nextPriceLevel;
        }
        
        if (level.nextPriceLevel != 0) {
            priceLevels[pairId][side][level.nextPriceLevel].prevPriceLevel = level.prevPriceLevel;
        }
        
        // Clear level
        delete priceLevels[pairId][side][price];
    }

    /**
     * @notice Update best price after order activity
     */
    function _updateBestPrice(bytes32 pairId, OrderSide side, uint256 price) internal {
        uint256 currentBest = bestPrice[pairId][side];
        
        if (currentBest == 0) {
            bestPrice[pairId][side] = price;
        } else {
            bool isBetter = side == OrderSide.BUY 
                ? price > currentBest 
                : price < currentBest;
            
            if (isBetter) {
                bestPrice[pairId][side] = price;
            }
        }
    }

    /**
     * @notice Move to next best price when current level is exhausted
     */
    function _moveToBestPrice(bytes32 pairId, OrderSide side) internal {
        uint256 current = bestPrice[pairId][side];
        if (current == 0) return;
        
        bestPrice[pairId][side] = priceLevels[pairId][side][current].nextPriceLevel;
    }

    // ============ Helper Functions ============
    
    function _cancelOrder(uint256 orderId) internal {
        Order storage order = orders[orderId];
        TradingPair storage pair = tradingPairs[order.pairId];
        
        uint256 remainingQuantity = order.quantity - order.filledQuantity;
        if (remainingQuantity == 0) {
            order.status = OrderStatus.FILLED;
            return;
        }
        
        // Calculate and unlock remaining funds
        uint256 unlockAmount;
        address unlockToken;
        
        if (order.side == OrderSide.BUY) {
            unlockAmount = (remainingQuantity * order.price) / PRICE_PRECISION;
            unlockToken = pair.quoteToken;
        } else {
            unlockAmount = remainingQuantity;
            unlockToken = pair.baseToken;
        }
        
        balances[unlockToken][order.trader].locked -= unlockAmount;
        balances[unlockToken][order.trader].available += unlockAmount;
        
        order.status = OrderStatus.CANCELLED;
        
        emit OrderCancelled(orderId, order.trader, remainingQuantity);
    }

    function _calculateRequiredBalance(
        TradingPair storage pair,
        OrderSide side,
        uint256 price,
        uint256 quantity
    ) internal view returns (uint256) {
        if (side == OrderSide.BUY) {
            // Need quote tokens to buy base tokens
            uint256 quoteAmount = (quantity * price) / PRICE_PRECISION;
            uint256 maxFee = (quoteAmount * pair.takerFee) / FEE_DENOMINATOR;
            return quoteAmount + maxFee;
        } else {
            // Need base tokens to sell
            return quantity;
        }
    }

    function _isSelfTrade(address trader1, address trader2) internal view returns (bool) {
        if (trader1 == trader2) return true;
        
        bytes32 group1 = selfTradePreventionGroup[trader1];
        bytes32 group2 = selfTradePreventionGroup[trader2];
        
        return group1 != bytes32(0) && group1 == group2;
    }

    function _updateVolume(TradingPair storage pair, uint256 quantity) internal {
        if (block.timestamp >= pair.lastVolumeReset + VOLUME_RESET_PERIOD) {
            pair.volume24h = quantity;
            pair.lastVolumeReset = block.timestamp;
        } else {
            pair.volume24h += quantity;
        }
    }

    // ============ View Functions ============
    
    /**
     * @notice Get order book depth at a specific price
     */
    function getOrderBookDepth(
        bytes32 pairId,
        OrderSide side,
        uint256 levels
    ) external view returns (
        uint256[] memory prices,
        uint256[] memory quantities
    ) {
        prices = new uint256[](levels);
        quantities = new uint256[](levels);
        
        uint256 currentPrice = bestPrice[pairId][side];
        uint256 i = 0;
        
        while (currentPrice != 0 && i < levels) {
            PriceLevel storage level = priceLevels[pairId][side][currentPrice];
            prices[i] = currentPrice;
            quantities[i] = level.totalQuantity;
            currentPrice = level.nextPriceLevel;
            i++;
        }
    }

    /**
     * @notice Get bid-ask spread
     */
    function getSpread(bytes32 pairId) external view returns (
        uint256 bestBid,
        uint256 bestAsk,
        uint256 spreadBps
    ) {
        bestBid = bestPrice[pairId][OrderSide.BUY];
        bestAsk = bestPrice[pairId][OrderSide.SELL];
        
        if (bestBid > 0 && bestAsk > 0) {
            spreadBps = ((bestAsk - bestBid) * FEE_DENOMINATOR) / bestBid;
        }
    }

    /**
     * @notice Get user's open orders
     */
    function getUserOpenOrders(address user) external view returns (uint256[] memory openOrders) {
        uint256[] memory allOrders = userOrders[user];
        uint256 count = 0;
        
        for (uint256 i = 0; i < allOrders.length; i++) {
            Order storage order = orders[allOrders[i]];
            if (order.status == OrderStatus.OPEN || order.status == OrderStatus.PARTIALLY_FILLED) {
                count++;
            }
        }
        
        openOrders = new uint256[](count);
        uint256 j = 0;
        
        for (uint256 i = 0; i < allOrders.length; i++) {
            Order storage order = orders[allOrders[i]];
            if (order.status == OrderStatus.OPEN || order.status == OrderStatus.PARTIALLY_FILLED) {
                openOrders[j] = allOrders[i];
                j++;
            }
        }
    }

    /**
     * @notice Get user balance for a token
     */
    function getUserBalance(address user, address token) external view returns (
        uint256 available,
        uint256 locked,
        uint256 total
    ) {
        UserBalance storage balance = balances[token][user];
        available = balance.available;
        locked = balance.locked;
        total = available + locked;
    }

    /**
     * @notice Get recent trades for a pair
     */
    function getRecentTrades(bytes32 pairId, uint256 count) external view returns (Trade[] memory) {
        uint256[] storage tradeIds = pairTrades[pairId];
        uint256 length = tradeIds.length < count ? tradeIds.length : count;
        Trade[] memory result = new Trade[](length);
        
        for (uint256 i = 0; i < length; i++) {
            result[i] = trades[tradeIds[tradeIds.length - 1 - i]];
        }
        
        return result;
    }

    // ============ Admin Functions ============
    
    /**
     * @notice Set fee recipient
     */
    function setFeeRecipient(address _feeRecipient) external onlyRole(FEE_MANAGER_ROLE) {
        require(_feeRecipient != address(0), "Invalid recipient");
        feeRecipient = _feeRecipient;
    }

    /**
     * @notice Update pair fees
     */
    function updatePairFees(
        bytes32 pairId,
        uint256 makerFee,
        uint256 takerFee
    ) external onlyRole(FEE_MANAGER_ROLE) {
        if (makerFee > MAX_FEE || takerFee > MAX_FEE) revert InvalidFee();
        
        TradingPair storage pair = tradingPairs[pairId];
        pair.makerFee = makerFee;
        pair.takerFee = takerFee;
    }

    /**
     * @notice Set maker rebate for a trader
     */
    function setMakerRebate(address trader, uint256 rebateBps) external onlyRole(FEE_MANAGER_ROLE) {
        require(rebateBps <= 50, "Rebate too high"); // Max 0.5%
        makerRebates[trader] = rebateBps;
    }

    /**
     * @notice Set self-trade prevention group
     */
    function setSelfTradeGroup(address trader, bytes32 groupId) external onlyRole(OPERATOR_ROLE) {
        selfTradePreventionGroup[trader] = groupId;
    }

    /**
     * @notice Withdraw collected fees
     */
    function withdrawFees(address token) external onlyRole(FEE_MANAGER_ROLE) {
        uint256 amount = collectedFees[token];
        collectedFees[token] = 0;
        IERC20(token).safeTransfer(feeRecipient, amount);
    }

    /**
     * @notice Toggle pair active status
     */
    function setPairStatus(bytes32 pairId, bool isActive) external onlyRole(OPERATOR_ROLE) {
        tradingPairs[pairId].isActive = isActive;
    }

    /**
     * @notice Pause trading
     */
    function pause() external onlyRole(OPERATOR_ROLE) {
        _pause();
    }

    /**
     * @notice Unpause trading
     */
    function unpause() external onlyRole(OPERATOR_ROLE) {
        _unpause();
    }
}
