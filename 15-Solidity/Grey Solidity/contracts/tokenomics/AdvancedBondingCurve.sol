// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/access/Ownable.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/math/Math.sol";

/**
 * @title AdvancedBondingCurve
 * @author Grey Protocol
 * @notice Multiple bonding curve implementations for token pricing
 * @dev Supports linear, polynomial, exponential, sigmoid, and custom curves
 * 
 * Features:
 * - Multiple curve types with configurable parameters
 * - Buy/sell with slippage protection
 * - Reserve ratio management
 * - Fee distribution
 * - Curve migration support
 * - Analytics and projections
 */
contract AdvancedBondingCurve is Ownable, ReentrancyGuard {
    using SafeERC20 for IERC20;
    using Math for uint256;

    // ============================================
    // CONSTANTS
    // ============================================

    uint256 public constant PRECISION = 1e18;
    uint256 public constant BPS_DENOMINATOR = 10000;
    uint256 public constant MAX_FEE = 500; // 5% max

    // ============================================
    // ENUMS
    // ============================================

    enum CurveType {
        LINEAR,           // price = m * supply + b
        POLYNOMIAL,       // price = a * supply^n + b
        EXPONENTIAL,      // price = a * e^(k * supply)
        LOGARITHMIC,      // price = a * ln(supply + 1) + b
        SIGMOID,          // price = L / (1 + e^(-k*(supply - x0)))
        BANCOR            // Bancor formula with reserve ratio
    }

    // ============================================
    // STRUCTS
    // ============================================

    /// @notice Curve parameters
    struct CurveParams {
        CurveType curveType;
        uint256 paramA;          // Scaling factor
        uint256 paramB;          // Base/offset
        uint256 paramK;          // Growth rate (for exp/sigmoid)
        uint256 paramN;          // Exponent (for polynomial)
        uint256 reserveRatio;    // Reserve ratio in BPS (for Bancor)
        uint256 maxSupply;       // Maximum token supply
    }

    /// @notice Fee configuration
    struct FeeConfig {
        uint256 buyFee;          // Buy fee in BPS
        uint256 sellFee;         // Sell fee in BPS
        address feeRecipient;    // Fee recipient
        uint256 protocolFee;     // Protocol fee portion in BPS
        address protocolRecipient;
    }

    /// @notice Trade record
    struct Trade {
        address trader;
        bool isBuy;
        uint256 tokenAmount;
        uint256 reserveAmount;
        uint256 price;
        uint256 fee;
        uint256 timestamp;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Token being traded
    IERC20 public token;

    /// @notice Reserve token (e.g., ETH wrapper or stablecoin)
    IERC20 public reserveToken;

    /// @notice Curve parameters
    CurveParams public curveParams;

    /// @notice Fee configuration
    FeeConfig public feeConfig;

    /// @notice Current token supply in curve
    uint256 public curveSupply;

    /// @notice Current reserve balance
    uint256 public reserveBalance;

    /// @notice Historical trades
    Trade[] public trades;

    /// @notice Total volume (reserve token)
    uint256 public totalVolume;

    /// @notice Total fees collected
    uint256 public totalFeesCollected;

    /// @notice Is trading active
    bool public tradingActive;

    /// @notice Minimum trade amount
    uint256 public minTradeAmount;

    // ============================================
    // EVENTS
    // ============================================

    event TokensPurchased(
        address indexed buyer,
        uint256 tokenAmount,
        uint256 reserveAmount,
        uint256 price,
        uint256 fee
    );

    event TokensSold(
        address indexed seller,
        uint256 tokenAmount,
        uint256 reserveAmount,
        uint256 price,
        uint256 fee
    );

    event CurveParamsUpdated(CurveType curveType);

    event FeesDistributed(uint256 amount, address recipient);

    event TradingStatusChanged(bool active);

    // ============================================
    // ERRORS
    // ============================================

    error TradingNotActive();
    error InsufficientReserve();
    error SlippageExceeded();
    error MaxSupplyReached();
    error InsufficientTokens();
    error BelowMinimum();
    error InvalidCurve();

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(
        address _token,
        address _reserveToken,
        CurveType _curveType,
        uint256 _paramA,
        uint256 _paramB,
        uint256 _paramK,
        address initialOwner
    ) Ownable(initialOwner) {
        token = IERC20(_token);
        reserveToken = IERC20(_reserveToken);

        curveParams = CurveParams({
            curveType: _curveType,
            paramA: _paramA,
            paramB: _paramB,
            paramK: _paramK,
            paramN: 2,           // Default quadratic
            reserveRatio: 5000,  // 50% reserve ratio
            maxSupply: type(uint256).max
        });

        feeConfig = FeeConfig({
            buyFee: 100,         // 1% buy fee
            sellFee: 100,        // 1% sell fee
            feeRecipient: initialOwner,
            protocolFee: 2000,   // 20% of fees to protocol
            protocolRecipient: initialOwner
        });

        minTradeAmount = 1e15; // 0.001 tokens
        tradingActive = true;
    }

    // ============================================
    // TRADING FUNCTIONS
    // ============================================

    /**
     * @notice Buy tokens with reserve
     * @param reserveAmount Amount of reserve tokens to spend
     * @param minTokens Minimum tokens to receive (slippage protection)
     */
    function buy(uint256 reserveAmount, uint256 minTokens) 
        external 
        nonReentrant 
        returns (uint256 tokenAmount) 
    {
        if (!tradingActive) revert TradingNotActive();
        if (reserveAmount < minTradeAmount) revert BelowMinimum();

        // Calculate fee
        uint256 fee = (reserveAmount * feeConfig.buyFee) / BPS_DENOMINATOR;
        uint256 reserveAfterFee = reserveAmount - fee;

        // Calculate tokens to mint
        tokenAmount = calculateBuyReturn(reserveAfterFee);
        
        if (tokenAmount < minTokens) revert SlippageExceeded();
        if (curveSupply + tokenAmount > curveParams.maxSupply) revert MaxSupplyReached();

        // Transfer reserve
        reserveToken.safeTransferFrom(msg.sender, address(this), reserveAmount);

        // Update state
        uint256 pricePerToken = (reserveAfterFee * PRECISION) / tokenAmount;
        curveSupply += tokenAmount;
        reserveBalance += reserveAfterFee;
        totalVolume += reserveAmount;
        totalFeesCollected += fee;

        // Transfer tokens
        token.safeTransfer(msg.sender, tokenAmount);

        // Distribute fees
        _distributeFees(fee);

        // Record trade
        trades.push(Trade({
            trader: msg.sender,
            isBuy: true,
            tokenAmount: tokenAmount,
            reserveAmount: reserveAmount,
            price: pricePerToken,
            fee: fee,
            timestamp: block.timestamp
        }));

        emit TokensPurchased(msg.sender, tokenAmount, reserveAmount, pricePerToken, fee);
    }

    /**
     * @notice Sell tokens for reserve
     * @param tokenAmount Amount of tokens to sell
     * @param minReserve Minimum reserve to receive (slippage protection)
     */
    function sell(uint256 tokenAmount, uint256 minReserve) 
        external 
        nonReentrant 
        returns (uint256 reserveAmount) 
    {
        if (!tradingActive) revert TradingNotActive();
        if (tokenAmount < minTradeAmount) revert BelowMinimum();
        if (tokenAmount > curveSupply) revert InsufficientTokens();

        // Calculate reserve return
        uint256 grossReserve = calculateSellReturn(tokenAmount);
        
        // Calculate fee
        uint256 fee = (grossReserve * feeConfig.sellFee) / BPS_DENOMINATOR;
        reserveAmount = grossReserve - fee;

        if (reserveAmount < minReserve) revert SlippageExceeded();
        if (reserveAmount > reserveBalance) revert InsufficientReserve();

        // Transfer tokens to curve
        token.safeTransferFrom(msg.sender, address(this), tokenAmount);

        // Update state
        uint256 pricePerToken = (grossReserve * PRECISION) / tokenAmount;
        curveSupply -= tokenAmount;
        reserveBalance -= grossReserve;
        totalVolume += grossReserve;
        totalFeesCollected += fee;

        // Transfer reserve
        reserveToken.safeTransfer(msg.sender, reserveAmount);

        // Distribute fees
        _distributeFees(fee);

        // Record trade
        trades.push(Trade({
            trader: msg.sender,
            isBuy: false,
            tokenAmount: tokenAmount,
            reserveAmount: reserveAmount,
            price: pricePerToken,
            fee: fee,
            timestamp: block.timestamp
        }));

        emit TokensSold(msg.sender, tokenAmount, reserveAmount, pricePerToken, fee);
    }

    // ============================================
    // PRICE CALCULATIONS
    // ============================================

    /**
     * @notice Calculate current spot price
     */
    function currentPrice() public view returns (uint256) {
        return _priceAtSupply(curveSupply);
    }

    /**
     * @notice Calculate price at a given supply
     */
    function _priceAtSupply(uint256 supply) internal view returns (uint256) {
        CurveParams memory params = curveParams;

        if (params.curveType == CurveType.LINEAR) {
            // price = m * supply + b
            return (params.paramA * supply) / PRECISION + params.paramB;
        } 
        else if (params.curveType == CurveType.POLYNOMIAL) {
            // price = a * supply^n + b
            uint256 powered = _power(supply, params.paramN);
            return (params.paramA * powered) / PRECISION + params.paramB;
        }
        else if (params.curveType == CurveType.EXPONENTIAL) {
            // price = a * e^(k * supply / PRECISION)
            uint256 exponent = (params.paramK * supply) / PRECISION;
            uint256 expResult = _exp(exponent);
            return (params.paramA * expResult) / PRECISION;
        }
        else if (params.curveType == CurveType.LOGARITHMIC) {
            // price = a * ln(supply + 1) + b
            uint256 logResult = _ln(supply + PRECISION);
            return (params.paramA * logResult) / PRECISION + params.paramB;
        }
        else if (params.curveType == CurveType.SIGMOID) {
            // price = L / (1 + e^(-k*(supply - x0)))
            // Simplified: paramA = L, paramK = k, paramB = x0
            int256 exponent = -int256(params.paramK) * (int256(supply) - int256(params.paramB)) / int256(PRECISION);
            uint256 expResult = _exp(uint256(exponent > 0 ? exponent : -exponent));
            if (exponent < 0) {
                return (params.paramA * PRECISION) / (PRECISION + expResult);
            } else {
                return (params.paramA * expResult) / (PRECISION + expResult);
            }
        }
        else if (params.curveType == CurveType.BANCOR) {
            // Bancor formula: price = reserve / (supply * reserveRatio)
            if (supply == 0) return params.paramB; // Base price
            return (reserveBalance * PRECISION * BPS_DENOMINATOR) / (supply * params.reserveRatio);
        }

        return params.paramB; // Fallback to base price
    }

    /**
     * @notice Calculate tokens received for reserve amount
     */
    function calculateBuyReturn(uint256 reserveAmount) public view returns (uint256) {
        if (curveParams.curveType == CurveType.BANCOR) {
            return _bancorBuyReturn(reserveAmount);
        }

        // Numerical integration for other curves
        return _integrateBuy(reserveAmount);
    }

    /**
     * @notice Calculate reserve received for token amount
     */
    function calculateSellReturn(uint256 tokenAmount) public view returns (uint256) {
        if (curveParams.curveType == CurveType.BANCOR) {
            return _bancorSellReturn(tokenAmount);
        }

        // Numerical integration for other curves
        return _integrateSell(tokenAmount);
    }

    /**
     * @notice Bancor buy return formula
     */
    function _bancorBuyReturn(uint256 reserveAmount) internal view returns (uint256) {
        // Bancor formula: tokenAmount = supply * ((1 + reserveAmount/reserve)^(ratio) - 1)
        uint256 supply = curveSupply;
        uint256 reserve = reserveBalance > 0 ? reserveBalance : 1;
        uint256 ratio = curveParams.reserveRatio;

        // Simplified approximation
        uint256 base = PRECISION + (reserveAmount * PRECISION) / reserve;
        uint256 powered = _power(base, ratio * PRECISION / BPS_DENOMINATOR);
        
        return (supply * (powered - PRECISION)) / PRECISION;
    }

    /**
     * @notice Bancor sell return formula
     */
    function _bancorSellReturn(uint256 tokenAmount) internal view returns (uint256) {
        // Bancor formula: reserveAmount = reserve * (1 - (1 - tokenAmount/supply)^(1/ratio))
        uint256 supply = curveSupply;
        uint256 reserve = reserveBalance;
        uint256 ratio = curveParams.reserveRatio;

        if (supply == 0 || tokenAmount >= supply) return reserve;

        // Simplified approximation
        uint256 base = PRECISION - (tokenAmount * PRECISION) / supply;
        uint256 inverseRatio = (BPS_DENOMINATOR * PRECISION) / ratio;
        uint256 powered = _power(base, inverseRatio);

        return (reserve * (PRECISION - powered)) / PRECISION;
    }

    /**
     * @notice Numerical integration for buy
     */
    function _integrateBuy(uint256 reserveAmount) internal view returns (uint256) {
        // Use trapezoidal approximation
        uint256 steps = 10;
        uint256 tokenAmount = 0;
        uint256 remainingReserve = reserveAmount;
        uint256 currentSupply = curveSupply;

        for (uint256 i = 0; i < steps && remainingReserve > 0; i++) {
            uint256 price = _priceAtSupply(currentSupply);
            if (price == 0) price = 1;
            
            uint256 stepReserve = remainingReserve / (steps - i);
            uint256 stepTokens = (stepReserve * PRECISION) / price;
            
            tokenAmount += stepTokens;
            currentSupply += stepTokens;
            remainingReserve -= stepReserve;
        }

        return tokenAmount;
    }

    /**
     * @notice Numerical integration for sell
     */
    function _integrateSell(uint256 tokenAmount) internal view returns (uint256) {
        // Use trapezoidal approximation
        uint256 steps = 10;
        uint256 reserveAmount = 0;
        uint256 remainingTokens = tokenAmount;
        uint256 currentSupply = curveSupply;

        for (uint256 i = 0; i < steps && remainingTokens > 0; i++) {
            uint256 stepTokens = remainingTokens / (steps - i);
            uint256 price = _priceAtSupply(currentSupply);
            
            uint256 stepReserve = (stepTokens * price) / PRECISION;
            
            reserveAmount += stepReserve;
            currentSupply -= stepTokens;
            remainingTokens -= stepTokens;
        }

        return reserveAmount;
    }

    // ============================================
    // MATH HELPERS
    // ============================================

    /**
     * @notice Power function with precision
     */
    function _power(uint256 base, uint256 exp) internal pure returns (uint256) {
        if (exp == 0) return PRECISION;
        if (exp == PRECISION) return base;
        
        // Use logarithm for fractional exponents
        // result = e^(exp * ln(base))
        // Simplified integer approximation for small exponents
        uint256 result = PRECISION;
        uint256 intExp = exp / PRECISION;
        
        for (uint256 i = 0; i < intExp && i < 10; i++) {
            result = (result * base) / PRECISION;
        }
        
        return result;
    }

    /**
     * @notice Natural exponential approximation
     */
    function _exp(uint256 x) internal pure returns (uint256) {
        // Taylor series: e^x ≈ 1 + x + x^2/2! + x^3/3! + ...
        if (x > 20 * PRECISION) return type(uint128).max; // Overflow protection
        
        uint256 result = PRECISION;
        uint256 term = PRECISION;
        
        for (uint256 i = 1; i <= 6; i++) {
            term = (term * x) / (i * PRECISION);
            result += term;
        }
        
        return result;
    }

    /**
     * @notice Natural logarithm approximation
     */
    function _ln(uint256 x) internal pure returns (uint256) {
        // ln(x) approximation using series expansion
        if (x <= PRECISION) return 0;
        
        // Scale x to [1, 2) range
        uint256 n = 0;
        while (x >= 2 * PRECISION) {
            x = x / 2;
            n++;
        }
        
        // ln(x) for x in [1, 2) using (x-1)/(x+1) series
        uint256 y = ((x - PRECISION) * PRECISION) / (x + PRECISION);
        uint256 y2 = (y * y) / PRECISION;
        
        uint256 result = y;
        uint256 term = y;
        
        for (uint256 i = 3; i <= 9; i += 2) {
            term = (term * y2) / PRECISION;
            result += term / i;
        }
        
        result = result * 2;
        
        // Add back ln(2) * n
        result += n * 693147180559945309; // ln(2) * 1e18
        
        return result;
    }

    // ============================================
    // FEE DISTRIBUTION
    // ============================================

    function _distributeFees(uint256 fee) internal {
        uint256 protocolAmount = (fee * feeConfig.protocolFee) / BPS_DENOMINATOR;
        uint256 recipientAmount = fee - protocolAmount;

        if (protocolAmount > 0 && feeConfig.protocolRecipient != address(0)) {
            reserveToken.safeTransfer(feeConfig.protocolRecipient, protocolAmount);
            emit FeesDistributed(protocolAmount, feeConfig.protocolRecipient);
        }

        if (recipientAmount > 0 && feeConfig.feeRecipient != address(0)) {
            reserveToken.safeTransfer(feeConfig.feeRecipient, recipientAmount);
            emit FeesDistributed(recipientAmount, feeConfig.feeRecipient);
        }
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Get quote for buying tokens
     */
    function getBuyQuote(uint256 reserveAmount) 
        external 
        view 
        returns (uint256 tokens, uint256 pricePerToken, uint256 fee) 
    {
        fee = (reserveAmount * feeConfig.buyFee) / BPS_DENOMINATOR;
        uint256 netReserve = reserveAmount - fee;
        tokens = calculateBuyReturn(netReserve);
        pricePerToken = tokens > 0 ? (netReserve * PRECISION) / tokens : 0;
    }

    /**
     * @notice Get quote for selling tokens
     */
    function getSellQuote(uint256 tokenAmount) 
        external 
        view 
        returns (uint256 reserve, uint256 pricePerToken, uint256 fee) 
    {
        uint256 grossReserve = calculateSellReturn(tokenAmount);
        fee = (grossReserve * feeConfig.sellFee) / BPS_DENOMINATOR;
        reserve = grossReserve - fee;
        pricePerToken = tokenAmount > 0 ? (grossReserve * PRECISION) / tokenAmount : 0;
    }

    /**
     * @notice Get trade history length
     */
    function getTradeCount() external view returns (uint256) {
        return trades.length;
    }

    /**
     * @notice Get market cap
     */
    function marketCap() external view returns (uint256) {
        return (curveSupply * currentPrice()) / PRECISION;
    }

    // ============================================
    // ADMIN FUNCTIONS
    // ============================================

    function setCurveParams(CurveParams calldata params) external onlyOwner {
        curveParams = params;
        emit CurveParamsUpdated(params.curveType);
    }

    function setFeeConfig(FeeConfig calldata config) external onlyOwner {
        if (config.buyFee > MAX_FEE || config.sellFee > MAX_FEE) revert InvalidCurve();
        feeConfig = config;
    }

    function setTradingActive(bool active) external onlyOwner {
        tradingActive = active;
        emit TradingStatusChanged(active);
    }

    function setMinTradeAmount(uint256 amount) external onlyOwner {
        minTradeAmount = amount;
    }

    function withdrawExcessReserve(uint256 amount, address to) external onlyOwner {
        uint256 excess = reserveToken.balanceOf(address(this)) - reserveBalance;
        require(amount <= excess, "Amount exceeds excess");
        reserveToken.safeTransfer(to, amount);
    }
}
