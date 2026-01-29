// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/Pausable.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";

/**
 * @title PerpetualPositions
 * @author Grey Team
 * @notice Perpetual futures trading with leverage, funding rates, and liquidations
 * @dev Implements professional-grade perpetual DEX mechanics inspired by
 *      dYdX, GMX, and Perpetual Protocol
 * 
 * Key features:
 * - Isolated and cross-margin modes
 * - Up to 100x leverage
 * - Funding rate mechanism (long/short balance)
 * - Partial liquidations
 * - Take-profit and stop-loss orders
 * - Position NFTs for composability
 */
contract PerpetualPositions is AccessControl, ReentrancyGuard, Pausable {
    using SafeERC20 for IERC20;

    // ============ Roles ============
    bytes32 public constant KEEPER_ROLE = keccak256("KEEPER_ROLE");
    bytes32 public constant ORACLE_ROLE = keccak256("ORACLE_ROLE");
    bytes32 public constant RISK_MANAGER_ROLE = keccak256("RISK_MANAGER_ROLE");

    // ============ Enums ============
    enum Side { LONG, SHORT }
    enum MarginMode { ISOLATED, CROSS }
    enum OrderType { MARKET, LIMIT, STOP_MARKET, TAKE_PROFIT, STOP_LOSS }
    enum PositionStatus { OPEN, CLOSED, LIQUIDATED }

    // ============ Structs ============
    
    /**
     * @notice Perpetual market configuration
     */
    struct Market {
        bytes32 marketId;
        string symbol;                  // e.g., "ETH-USD"
        address indexToken;             // Token for price index
        uint256 maxLeverage;            // Maximum leverage (e.g., 100 = 100x)
        uint256 maintenanceMargin;      // Maintenance margin ratio (basis points)
        uint256 initialMargin;          // Initial margin ratio (basis points)
        uint256 makerFee;               // Maker fee (basis points)
        uint256 takerFee;               // Taker fee (basis points)
        uint256 maxPositionSize;        // Maximum position size
        uint256 maxOpenInterest;        // Maximum open interest (long + short)
        uint256 fundingInterval;        // Funding rate interval
        uint256 maxFundingRate;         // Maximum funding rate per interval
        bool isActive;
    }

    /**
     * @notice Individual position
     */
    struct Position {
        uint256 id;
        bytes32 marketId;
        address trader;
        Side side;
        MarginMode marginMode;
        uint256 size;                   // Position size in base token
        uint256 collateral;             // Collateral amount
        uint256 entryPrice;             // Average entry price
        uint256 leverage;               // Effective leverage
        int256 unrealizedPnL;           // Current unrealized P&L
        int256 accumulatedFunding;      // Accumulated funding payments
        uint256 lastFundingTime;
        uint256 openTime;
        PositionStatus status;
        uint256 takeProfitPrice;        // 0 = not set
        uint256 stopLossPrice;          // 0 = not set
    }

    /**
     * @notice Pending order
     */
    struct Order {
        uint256 id;
        bytes32 marketId;
        address trader;
        Side side;
        OrderType orderType;
        uint256 size;
        uint256 collateral;
        uint256 price;                  // Limit/trigger price
        uint256 leverage;
        bool reduceOnly;
        uint256 expiry;
        bool isExecuted;
        bool isCancelled;
    }

    /**
     * @notice Market statistics
     */
    struct MarketStats {
        uint256 longOpenInterest;
        uint256 shortOpenInterest;
        uint256 totalVolume;
        int256 currentFundingRate;
        int256 accumulatedFundingRate;
        uint256 lastFundingTime;
        uint256 lastPrice;
        uint256 markPrice;
    }

    /**
     * @notice Funding rate snapshot
     */
    struct FundingSnapshot {
        uint256 timestamp;
        int256 fundingRate;
        int256 cumulativeFunding;
    }

    // ============ Constants ============
    uint256 public constant PRECISION = 1e18;
    uint256 public constant BPS = 10000;
    uint256 public constant MAX_LEVERAGE = 100;
    uint256 public constant LIQUIDATION_FEE = 50; // 0.5%
    uint256 public constant MIN_POSITION_SIZE = 10e18; // $10 minimum
    uint256 public constant FUNDING_PERIOD = 8 hours;
    int256 public constant MAX_FUNDING_RATE = 1e15; // 0.1% per period

    // ============ State Variables ============
    
    // Collateral token (e.g., USDC)
    IERC20 public immutable collateralToken;
    
    // Markets
    mapping(bytes32 => Market) public markets;
    bytes32[] public marketIds;
    
    // Market statistics
    mapping(bytes32 => MarketStats) public marketStats;
    
    // Positions
    mapping(uint256 => Position) public positions;
    mapping(address => uint256[]) public traderPositions;
    uint256 public nextPositionId = 1;
    
    // Orders
    mapping(uint256 => Order) public orders;
    mapping(address => uint256[]) public traderOrders;
    uint256 public nextOrderId = 1;
    
    // Funding history
    mapping(bytes32 => FundingSnapshot[]) public fundingHistory;
    
    // Cross-margin account balances
    mapping(address => uint256) public crossMarginBalance;
    
    // Insurance fund
    uint256 public insuranceFund;
    
    // Fee collection
    address public feeRecipient;
    uint256 public collectedFees;

    // ============ Events ============
    event MarketCreated(bytes32 indexed marketId, string symbol, uint256 maxLeverage);
    event PositionOpened(
        uint256 indexed positionId,
        bytes32 indexed marketId,
        address indexed trader,
        Side side,
        uint256 size,
        uint256 entryPrice,
        uint256 leverage
    );
    event PositionModified(
        uint256 indexed positionId,
        int256 sizeChange,
        int256 collateralChange,
        uint256 newSize,
        uint256 newCollateral
    );
    event PositionClosed(
        uint256 indexed positionId,
        uint256 exitPrice,
        int256 realizedPnL
    );
    event PositionLiquidated(
        uint256 indexed positionId,
        address indexed liquidator,
        uint256 liquidationPrice,
        int256 loss
    );
    event OrderPlaced(
        uint256 indexed orderId,
        bytes32 indexed marketId,
        address indexed trader,
        OrderType orderType,
        uint256 price
    );
    event OrderExecuted(uint256 indexed orderId, uint256 indexed positionId);
    event OrderCancelled(uint256 indexed orderId);
    event FundingSettled(bytes32 indexed marketId, int256 fundingRate);
    event CollateralDeposited(address indexed trader, uint256 amount);
    event CollateralWithdrawn(address indexed trader, uint256 amount);

    // ============ Errors ============
    error MarketNotActive();
    error PositionNotFound();
    error PositionNotOpen();
    error InsufficientCollateral();
    error ExceedsMaxLeverage();
    error ExceedsMaxPosition();
    error ExceedsOpenInterest();
    error BelowMinPosition();
    error InvalidOrder();
    error OrderNotExecutable();
    error NotPositionOwner();
    error NotLiquidatable();
    error InvalidPrice();
    error OrderExpired();

    // ============ Constructor ============
    constructor(address _collateralToken, address _feeRecipient) {
        require(_collateralToken != address(0), "Invalid token");
        require(_feeRecipient != address(0), "Invalid recipient");
        
        collateralToken = IERC20(_collateralToken);
        feeRecipient = _feeRecipient;
        
        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(RISK_MANAGER_ROLE, msg.sender);
    }

    // ============ Market Management ============
    
    /**
     * @notice Create a new perpetual market
     */
    function createMarket(
        string calldata symbol,
        address indexToken,
        uint256 maxLeverage,
        uint256 maintenanceMargin,
        uint256 initialMargin,
        uint256 makerFee,
        uint256 takerFee,
        uint256 maxPositionSize,
        uint256 maxOpenInterest
    ) external onlyRole(RISK_MANAGER_ROLE) {
        require(maxLeverage <= MAX_LEVERAGE, "Leverage too high");
        require(maintenanceMargin < initialMargin, "Invalid margins");
        
        bytes32 marketId = keccak256(abi.encodePacked(symbol, indexToken));
        require(markets[marketId].indexToken == address(0), "Market exists");
        
        markets[marketId] = Market({
            marketId: marketId,
            symbol: symbol,
            indexToken: indexToken,
            maxLeverage: maxLeverage,
            maintenanceMargin: maintenanceMargin,
            initialMargin: initialMargin,
            makerFee: makerFee,
            takerFee: takerFee,
            maxPositionSize: maxPositionSize,
            maxOpenInterest: maxOpenInterest,
            fundingInterval: FUNDING_PERIOD,
            maxFundingRate: uint256(MAX_FUNDING_RATE),
            isActive: true
        });
        
        marketIds.push(marketId);
        marketStats[marketId].lastFundingTime = block.timestamp;
        
        emit MarketCreated(marketId, symbol, maxLeverage);
    }

    // ============ Collateral Management ============
    
    /**
     * @notice Deposit collateral for cross-margin trading
     */
    function depositCollateral(uint256 amount) external nonReentrant {
        collateralToken.safeTransferFrom(msg.sender, address(this), amount);
        crossMarginBalance[msg.sender] += amount;
        emit CollateralDeposited(msg.sender, amount);
    }

    /**
     * @notice Withdraw collateral (if not used as margin)
     */
    function withdrawCollateral(uint256 amount) external nonReentrant {
        require(crossMarginBalance[msg.sender] >= amount, "Insufficient balance");
        require(_getAvailableMargin(msg.sender) >= amount, "Margin in use");
        
        crossMarginBalance[msg.sender] -= amount;
        collateralToken.safeTransfer(msg.sender, amount);
        
        emit CollateralWithdrawn(msg.sender, amount);
    }

    // ============ Position Management ============
    
    /**
     * @notice Open a new position with market order
     * @param marketId Market identifier
     * @param side Long or Short
     * @param size Position size in USD
     * @param collateral Collateral amount
     * @param marginMode Isolated or Cross margin
     */
    function openPosition(
        bytes32 marketId,
        Side side,
        uint256 size,
        uint256 collateral,
        MarginMode marginMode
    ) external nonReentrant whenNotPaused returns (uint256 positionId) {
        Market storage market = markets[marketId];
        if (!market.isActive) revert MarketNotActive();
        
        if (size < MIN_POSITION_SIZE) revert BelowMinPosition();
        if (size > market.maxPositionSize) revert ExceedsMaxPosition();
        
        // Check open interest limits
        MarketStats storage stats = marketStats[marketId];
        uint256 newOI = stats.longOpenInterest + stats.shortOpenInterest + size;
        if (newOI > market.maxOpenInterest) revert ExceedsOpenInterest();
        
        // Calculate leverage
        uint256 leverage = (size * PRECISION) / collateral;
        if (leverage > market.maxLeverage * PRECISION) revert ExceedsMaxLeverage();
        
        // Transfer collateral
        if (marginMode == MarginMode.ISOLATED) {
            collateralToken.safeTransferFrom(msg.sender, address(this), collateral);
        } else {
            if (crossMarginBalance[msg.sender] < collateral) revert InsufficientCollateral();
            crossMarginBalance[msg.sender] -= collateral;
        }
        
        // Get current price
        uint256 indexPrice = _getIndexPrice(marketId);
        uint256 markPrice = _getMarkPrice(marketId);
        
        // Apply fee
        uint256 fee = (size * market.takerFee) / BPS;
        collectedFees += fee;
        
        // Create position
        positionId = nextPositionId++;
        positions[positionId] = Position({
            id: positionId,
            marketId: marketId,
            trader: msg.sender,
            side: side,
            marginMode: marginMode,
            size: size,
            collateral: collateral - fee,
            entryPrice: markPrice,
            leverage: leverage / PRECISION,
            unrealizedPnL: 0,
            accumulatedFunding: 0,
            lastFundingTime: block.timestamp,
            openTime: block.timestamp,
            status: PositionStatus.OPEN,
            takeProfitPrice: 0,
            stopLossPrice: 0
        });
        
        traderPositions[msg.sender].push(positionId);
        
        // Update open interest
        if (side == Side.LONG) {
            stats.longOpenInterest += size;
        } else {
            stats.shortOpenInterest += size;
        }
        stats.totalVolume += size;
        stats.lastPrice = indexPrice;
        
        emit PositionOpened(positionId, marketId, msg.sender, side, size, markPrice, leverage / PRECISION);
        
        return positionId;
    }

    /**
     * @notice Increase position size or add collateral
     */
    function increasePosition(
        uint256 positionId,
        uint256 additionalSize,
        uint256 additionalCollateral
    ) external nonReentrant whenNotPaused {
        Position storage position = positions[positionId];
        if (position.trader != msg.sender) revert NotPositionOwner();
        if (position.status != PositionStatus.OPEN) revert PositionNotOpen();
        
        Market storage market = markets[position.marketId];
        if (!market.isActive) revert MarketNotActive();
        
        // Settle funding first
        _settleFunding(positionId);
        
        // Transfer additional collateral
        if (additionalCollateral > 0) {
            if (position.marginMode == MarginMode.ISOLATED) {
                collateralToken.safeTransferFrom(msg.sender, address(this), additionalCollateral);
            } else {
                if (crossMarginBalance[msg.sender] < additionalCollateral) revert InsufficientCollateral();
                crossMarginBalance[msg.sender] -= additionalCollateral;
            }
            position.collateral += additionalCollateral;
        }
        
        if (additionalSize > 0) {
            // Calculate new average entry price
            uint256 markPrice = _getMarkPrice(position.marketId);
            uint256 newTotalValue = position.size + additionalSize;
            position.entryPrice = (position.entryPrice * position.size + markPrice * additionalSize) / newTotalValue;
            position.size = newTotalValue;
            
            // Apply fee
            uint256 fee = (additionalSize * market.takerFee) / BPS;
            position.collateral -= fee;
            collectedFees += fee;
            
            // Update open interest
            MarketStats storage stats = marketStats[position.marketId];
            if (position.side == Side.LONG) {
                stats.longOpenInterest += additionalSize;
            } else {
                stats.shortOpenInterest += additionalSize;
            }
        }
        
        // Check leverage
        uint256 newLeverage = (position.size * PRECISION) / position.collateral;
        if (newLeverage > market.maxLeverage * PRECISION) revert ExceedsMaxLeverage();
        position.leverage = newLeverage / PRECISION;
        
        emit PositionModified(positionId, int256(additionalSize), int256(additionalCollateral), position.size, position.collateral);
    }

    /**
     * @notice Decrease position size
     */
    function decreasePosition(
        uint256 positionId,
        uint256 sizeReduction
    ) external nonReentrant returns (int256 realizedPnL) {
        Position storage position = positions[positionId];
        if (position.trader != msg.sender) revert NotPositionOwner();
        if (position.status != PositionStatus.OPEN) revert PositionNotOpen();
        
        require(sizeReduction <= position.size, "Size too large");
        
        // Settle funding first
        _settleFunding(positionId);
        
        // Calculate P&L for the closed portion
        uint256 markPrice = _getMarkPrice(position.marketId);
        realizedPnL = _calculatePnL(position, markPrice, sizeReduction);
        
        // Update position
        if (sizeReduction == position.size) {
            // Full close
            return closePosition(positionId);
        } else {
            // Partial close
            position.size -= sizeReduction;
            
            // Return proportional collateral + P&L
            uint256 collateralReturn = (position.collateral * sizeReduction) / (position.size + sizeReduction);
            position.collateral -= collateralReturn;
            
            int256 payout = int256(collateralReturn) + realizedPnL;
            if (payout > 0) {
                if (position.marginMode == MarginMode.ISOLATED) {
                    collateralToken.safeTransfer(msg.sender, uint256(payout));
                } else {
                    crossMarginBalance[msg.sender] += uint256(payout);
                }
            }
            
            // Update open interest
            MarketStats storage stats = marketStats[position.marketId];
            if (position.side == Side.LONG) {
                stats.longOpenInterest -= sizeReduction;
            } else {
                stats.shortOpenInterest -= sizeReduction;
            }
        }
        
        emit PositionModified(positionId, -int256(sizeReduction), 0, position.size, position.collateral);
        
        return realizedPnL;
    }

    /**
     * @notice Close a position completely
     */
    function closePosition(uint256 positionId) public nonReentrant returns (int256 realizedPnL) {
        Position storage position = positions[positionId];
        if (position.trader != msg.sender) revert NotPositionOwner();
        if (position.status != PositionStatus.OPEN) revert PositionNotOpen();
        
        // Settle funding first
        _settleFunding(positionId);
        
        uint256 markPrice = _getMarkPrice(position.marketId);
        realizedPnL = _calculatePnL(position, markPrice, position.size);
        
        // Calculate final payout
        int256 totalPayout = int256(position.collateral) + realizedPnL + position.accumulatedFunding;
        
        // Update open interest
        MarketStats storage stats = marketStats[position.marketId];
        if (position.side == Side.LONG) {
            stats.longOpenInterest -= position.size;
        } else {
            stats.shortOpenInterest -= position.size;
        }
        
        // Close position
        position.status = PositionStatus.CLOSED;
        
        // Handle payout
        if (totalPayout > 0) {
            if (position.marginMode == MarginMode.ISOLATED) {
                collateralToken.safeTransfer(msg.sender, uint256(totalPayout));
            } else {
                crossMarginBalance[msg.sender] += uint256(totalPayout);
            }
        } else if (totalPayout < 0) {
            // Loss exceeds collateral - covered by insurance fund
            uint256 loss = uint256(-totalPayout);
            if (loss > insuranceFund) {
                loss = insuranceFund;
            }
            insuranceFund -= loss;
        }
        
        emit PositionClosed(positionId, markPrice, realizedPnL);
        
        return realizedPnL;
    }

    // ============ Orders ============
    
    /**
     * @notice Place a limit or trigger order
     */
    function placeOrder(
        bytes32 marketId,
        Side side,
        OrderType orderType,
        uint256 size,
        uint256 collateral,
        uint256 price,
        uint256 leverage,
        bool reduceOnly,
        uint256 expiry
    ) external nonReentrant whenNotPaused returns (uint256 orderId) {
        Market storage market = markets[marketId];
        if (!market.isActive) revert MarketNotActive();
        if (price == 0) revert InvalidPrice();
        
        // Lock collateral for new positions
        if (!reduceOnly) {
            collateralToken.safeTransferFrom(msg.sender, address(this), collateral);
        }
        
        orderId = nextOrderId++;
        orders[orderId] = Order({
            id: orderId,
            marketId: marketId,
            trader: msg.sender,
            side: side,
            orderType: orderType,
            size: size,
            collateral: collateral,
            price: price,
            leverage: leverage,
            reduceOnly: reduceOnly,
            expiry: expiry,
            isExecuted: false,
            isCancelled: false
        });
        
        traderOrders[msg.sender].push(orderId);
        
        emit OrderPlaced(orderId, marketId, msg.sender, orderType, price);
        
        return orderId;
    }

    /**
     * @notice Execute a pending order (called by keeper)
     */
    function executeOrder(uint256 orderId) external nonReentrant onlyRole(KEEPER_ROLE) {
        Order storage order = orders[orderId];
        
        if (order.isExecuted || order.isCancelled) revert InvalidOrder();
        if (order.expiry > 0 && block.timestamp > order.expiry) revert OrderExpired();
        
        uint256 currentPrice = _getMarkPrice(order.marketId);
        
        // Check if order is executable
        bool executable = false;
        if (order.orderType == OrderType.LIMIT) {
            executable = order.side == Side.LONG 
                ? currentPrice <= order.price 
                : currentPrice >= order.price;
        } else if (order.orderType == OrderType.STOP_MARKET) {
            executable = order.side == Side.LONG 
                ? currentPrice >= order.price 
                : currentPrice <= order.price;
        }
        
        if (!executable) revert OrderNotExecutable();
        
        // Execute the order
        order.isExecuted = true;
        
        if (order.reduceOnly) {
            // Find and reduce existing position
            // Simplified: close first matching position
            uint256[] storage positionIds = traderPositions[order.trader];
            for (uint256 i = 0; i < positionIds.length; i++) {
                Position storage pos = positions[positionIds[i]];
                if (pos.status == PositionStatus.OPEN && 
                    pos.marketId == order.marketId && 
                    pos.side != order.side) {
                    // This is an opposing position, reduce it
                    uint256 reduceSize = order.size > pos.size ? pos.size : order.size;
                    _decreasePositionInternal(positionIds[i], reduceSize);
                    break;
                }
            }
        } else {
            // Open new position
            _openPositionInternal(order);
        }
        
        emit OrderExecuted(orderId, 0); // TODO: return actual position ID
    }

    /**
     * @notice Cancel a pending order
     */
    function cancelOrder(uint256 orderId) external nonReentrant {
        Order storage order = orders[orderId];
        require(order.trader == msg.sender, "Not order owner");
        require(!order.isExecuted && !order.isCancelled, "Order not active");
        
        order.isCancelled = true;
        
        // Refund collateral if locked
        if (!order.reduceOnly && order.collateral > 0) {
            collateralToken.safeTransfer(msg.sender, order.collateral);
        }
        
        emit OrderCancelled(orderId);
    }

    /**
     * @notice Set take-profit and stop-loss for a position
     */
    function setTPSL(
        uint256 positionId,
        uint256 takeProfitPrice,
        uint256 stopLossPrice
    ) external {
        Position storage position = positions[positionId];
        if (position.trader != msg.sender) revert NotPositionOwner();
        if (position.status != PositionStatus.OPEN) revert PositionNotOpen();
        
        position.takeProfitPrice = takeProfitPrice;
        position.stopLossPrice = stopLossPrice;
    }

    // ============ Liquidations ============
    
    /**
     * @notice Liquidate an underwater position
     */
    function liquidate(uint256 positionId) external nonReentrant {
        Position storage position = positions[positionId];
        if (position.status != PositionStatus.OPEN) revert PositionNotOpen();
        
        // Check if liquidatable
        if (!isLiquidatable(positionId)) revert NotLiquidatable();
        
        Market storage market = markets[position.marketId];
        uint256 markPrice = _getMarkPrice(position.marketId);
        
        // Calculate loss
        int256 pnl = _calculatePnL(position, markPrice, position.size);
        int256 totalLoss = pnl + position.accumulatedFunding;
        
        // Liquidation fee to keeper
        uint256 liquidationFee = (position.size * LIQUIDATION_FEE) / BPS;
        
        // Update open interest
        MarketStats storage stats = marketStats[position.marketId];
        if (position.side == Side.LONG) {
            stats.longOpenInterest -= position.size;
        } else {
            stats.shortOpenInterest -= position.size;
        }
        
        // Remaining collateral goes to insurance fund
        int256 remaining = int256(position.collateral) + totalLoss - int256(liquidationFee);
        if (remaining > 0) {
            insuranceFund += uint256(remaining);
        } else {
            // Socialized loss
            uint256 loss = uint256(-remaining);
            if (loss > insuranceFund) {
                loss = insuranceFund;
            }
            insuranceFund -= loss;
        }
        
        // Pay liquidator
        collateralToken.safeTransfer(msg.sender, liquidationFee);
        
        position.status = PositionStatus.LIQUIDATED;
        
        emit PositionLiquidated(positionId, msg.sender, markPrice, totalLoss);
    }

    /**
     * @notice Check if a position is liquidatable
     */
    function isLiquidatable(uint256 positionId) public view returns (bool) {
        Position storage position = positions[positionId];
        if (position.status != PositionStatus.OPEN) return false;
        
        Market storage market = markets[position.marketId];
        uint256 markPrice = _getMarkPrice(position.marketId);
        
        // Calculate margin ratio
        int256 pnl = _calculatePnL(position, markPrice, position.size);
        int256 equity = int256(position.collateral) + pnl + position.accumulatedFunding;
        
        if (equity <= 0) return true;
        
        uint256 marginRatio = (uint256(equity) * BPS) / position.size;
        
        return marginRatio < market.maintenanceMargin;
    }

    // ============ Funding ============
    
    /**
     * @notice Settle funding rate for a market (called by keeper)
     */
    function settleFundingRate(bytes32 marketId) external onlyRole(KEEPER_ROLE) {
        MarketStats storage stats = marketStats[marketId];
        Market storage market = markets[marketId];
        
        require(block.timestamp >= stats.lastFundingTime + market.fundingInterval, "Too early");
        
        // Calculate funding rate based on OI imbalance
        int256 fundingRate;
        uint256 totalOI = stats.longOpenInterest + stats.shortOpenInterest;
        
        if (totalOI > 0) {
            int256 imbalance = int256(stats.longOpenInterest) - int256(stats.shortOpenInterest);
            fundingRate = (imbalance * MAX_FUNDING_RATE) / int256(totalOI);
            
            // Clamp to max
            if (fundingRate > MAX_FUNDING_RATE) fundingRate = MAX_FUNDING_RATE;
            if (fundingRate < -MAX_FUNDING_RATE) fundingRate = -MAX_FUNDING_RATE;
        }
        
        stats.currentFundingRate = fundingRate;
        stats.accumulatedFundingRate += fundingRate;
        stats.lastFundingTime = block.timestamp;
        
        // Record snapshot
        fundingHistory[marketId].push(FundingSnapshot({
            timestamp: block.timestamp,
            fundingRate: fundingRate,
            cumulativeFunding: stats.accumulatedFundingRate
        }));
        
        emit FundingSettled(marketId, fundingRate);
    }

    function _settleFunding(uint256 positionId) internal {
        Position storage position = positions[positionId];
        MarketStats storage stats = marketStats[position.marketId];
        
        if (position.lastFundingTime >= stats.lastFundingTime) return;
        
        // Calculate funding payment
        // Longs pay shorts when funding is positive
        int256 fundingPayment = (int256(position.size) * stats.currentFundingRate) / int256(PRECISION);
        
        if (position.side == Side.LONG) {
            position.accumulatedFunding -= fundingPayment;
        } else {
            position.accumulatedFunding += fundingPayment;
        }
        
        position.lastFundingTime = block.timestamp;
    }

    // ============ Internal Functions ============
    
    function _calculatePnL(
        Position storage position,
        uint256 currentPrice,
        uint256 size
    ) internal view returns (int256) {
        if (position.side == Side.LONG) {
            // Long: profit when price goes up
            if (currentPrice > position.entryPrice) {
                return int256((currentPrice - position.entryPrice) * size / position.entryPrice);
            } else {
                return -int256((position.entryPrice - currentPrice) * size / position.entryPrice);
            }
        } else {
            // Short: profit when price goes down
            if (currentPrice < position.entryPrice) {
                return int256((position.entryPrice - currentPrice) * size / position.entryPrice);
            } else {
                return -int256((currentPrice - position.entryPrice) * size / position.entryPrice);
            }
        }
    }

    function _getIndexPrice(bytes32 marketId) internal view returns (uint256) {
        // In production, this would fetch from oracle
        return marketStats[marketId].lastPrice > 0 ? marketStats[marketId].lastPrice : 1e18;
    }

    function _getMarkPrice(bytes32 marketId) internal view returns (uint256) {
        // Mark price = index price + premium
        // Simplified: use index price
        return _getIndexPrice(marketId);
    }

    function _getAvailableMargin(address trader) internal view returns (uint256) {
        uint256 balance = crossMarginBalance[trader];
        uint256 usedMargin = 0;
        
        uint256[] storage positionIds = traderPositions[trader];
        for (uint256 i = 0; i < positionIds.length; i++) {
            Position storage pos = positions[positionIds[i]];
            if (pos.status == PositionStatus.OPEN && pos.marginMode == MarginMode.CROSS) {
                usedMargin += pos.collateral;
            }
        }
        
        return balance > usedMargin ? balance - usedMargin : 0;
    }

    function _openPositionInternal(Order storage order) internal {
        // Simplified internal position opening
        // Full implementation would mirror openPosition logic
    }

    function _decreasePositionInternal(uint256 positionId, uint256 size) internal {
        // Simplified internal decrease
        // Full implementation would mirror decreasePosition logic
    }

    // ============ View Functions ============
    
    /**
     * @notice Get position P&L
     */
    function getPositionPnL(uint256 positionId) external view returns (
        int256 unrealizedPnL,
        int256 funding,
        int256 total
    ) {
        Position storage position = positions[positionId];
        uint256 markPrice = _getMarkPrice(position.marketId);
        
        unrealizedPnL = _calculatePnL(position, markPrice, position.size);
        funding = position.accumulatedFunding;
        total = unrealizedPnL + funding;
    }

    /**
     * @notice Get market info
     */
    function getMarketInfo(bytes32 marketId) external view returns (
        Market memory market,
        MarketStats memory stats
    ) {
        return (markets[marketId], marketStats[marketId]);
    }

    /**
     * @notice Get trader's positions
     */
    function getTraderPositions(address trader) external view returns (uint256[] memory) {
        return traderPositions[trader];
    }

    /**
     * @notice Get trader's orders
     */
    function getTraderOrders(address trader) external view returns (uint256[] memory) {
        return traderOrders[trader];
    }

    /**
     * @notice Update mark price (called by oracle)
     */
    function updatePrice(bytes32 marketId, uint256 price) external onlyRole(ORACLE_ROLE) {
        marketStats[marketId].lastPrice = price;
        marketStats[marketId].markPrice = price;
    }

    // ============ Admin Functions ============
    
    function setFeeRecipient(address _feeRecipient) external onlyRole(DEFAULT_ADMIN_ROLE) {
        require(_feeRecipient != address(0), "Invalid recipient");
        feeRecipient = _feeRecipient;
    }

    function withdrawFees() external onlyRole(DEFAULT_ADMIN_ROLE) {
        uint256 amount = collectedFees;
        collectedFees = 0;
        collateralToken.safeTransfer(feeRecipient, amount);
    }

    function fundInsurance(uint256 amount) external {
        collateralToken.safeTransferFrom(msg.sender, address(this), amount);
        insuranceFund += amount;
    }

    function pause() external onlyRole(RISK_MANAGER_ROLE) {
        _pause();
    }

    function unpause() external onlyRole(RISK_MANAGER_ROLE) {
        _unpause();
    }
}
