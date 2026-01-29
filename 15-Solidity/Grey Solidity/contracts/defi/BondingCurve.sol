// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/Pausable.sol";
import "@openzeppelin/contracts/utils/math/Math.sol";

/**
 * @title BondingCurve
 * @author Grey Protocol Team
 * @notice Advanced bonding curve implementation for token sales
 * @dev Implements multiple curve types with advanced pricing
 * 
 * Features:
 * - Multiple curve types (linear, exponential, logarithmic, sigmoid)
 * - Buy and sell with slippage protection
 * - Reserve ratio management
 * - Vesting on purchases
 * - Anti-bot measures
 * - Graduated pricing
 * - Tax/fee system
 * - Emergency controls
 */
contract BondingCurve is AccessControl, ReentrancyGuard, Pausable {
    using SafeERC20 for IERC20;
    using Math for uint256;

    // ============================================
    // CONSTANTS & ROLES
    // ============================================

    bytes32 public constant CURVE_ADMIN_ROLE = keccak256("CURVE_ADMIN_ROLE");
    bytes32 public constant FEE_MANAGER_ROLE = keccak256("FEE_MANAGER_ROLE");

    uint256 public constant PRECISION = 1e18;
    uint256 public constant BPS = 10000;
    uint256 public constant MAX_FEE = 1000; // 10% max
    uint256 public constant MAX_RESERVE_RATIO = 1000000; // 100% in PPM

    // ============================================
    // ENUMS
    // ============================================

    enum CurveType {
        Linear,
        Exponential,
        Logarithmic,
        Sigmoid,
        Polynomial,
        Custom
    }

    enum SalePhase {
        NotStarted,
        Whitelist,
        Public,
        Graduated,
        Closed,
        Emergency
    }

    // ============================================
    // STRUCTS
    // ============================================

    /**
     * @notice Curve parameters
     */
    struct CurveParams {
        CurveType curveType;
        uint256 slope;              // For linear curves
        uint256 exponent;           // For exponential/polynomial
        uint256 initialPrice;       // Starting price
        uint256 maxPrice;           // Price cap
        uint256 virtualBalance;     // Virtual balance for bootstrapping
        uint256 virtualSupply;      // Virtual supply for bootstrapping
        uint256 reserveRatio;       // Reserve ratio in PPM (1000000 = 100%)
    }

    /**
     * @notice Sale configuration
     */
    struct SaleConfig {
        uint256 hardCap;
        uint256 softCap;
        uint256 minPurchase;
        uint256 maxPurchase;
        uint256 maxPerWallet;
        uint256 startTime;
        uint256 endTime;
        uint256 whitelistDuration;
        bool vestingEnabled;
        uint256 vestingDuration;
        uint256 cliffDuration;
    }

    /**
     * @notice Fee configuration
     */
    struct FeeConfig {
        uint256 buyFee;           // Fee on buys in BPS
        uint256 sellFee;          // Fee on sells in BPS
        uint256 referralReward;   // Referral reward in BPS
        address feeRecipient;
        bool dynamicFees;
    }

    /**
     * @notice User purchase record
     */
    struct PurchaseRecord {
        uint256 totalPurchased;
        uint256 totalSpent;
        uint256 averagePrice;
        uint256 firstPurchase;
        uint256 lastPurchase;
        uint256 vestingStart;
        uint256 vestedAmount;
        uint256 claimedAmount;
        address referrer;
    }

    /**
     * @notice Vesting schedule
     */
    struct VestingSchedule {
        uint256 totalAmount;
        uint256 releasedAmount;
        uint256 startTime;
        uint256 cliffEnd;
        uint256 endTime;
    }

    /**
     * @notice Price checkpoint for TWAP
     */
    struct PriceCheckpoint {
        uint256 timestamp;
        uint256 price;
        uint256 supply;
        uint256 reserve;
    }

    /**
     * @notice Graduation milestone
     */
    struct GraduationMilestone {
        uint256 supplyThreshold;
        uint256 reserveThreshold;
        bool achieved;
        uint256 achievedAt;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Token being sold
    IERC20 public token;

    /// @notice Reserve token (e.g., ETH wrapper or stablecoin)
    IERC20 public reserveToken;

    /// @notice Curve parameters
    CurveParams public curveParams;

    /// @notice Sale configuration
    SaleConfig public saleConfig;

    /// @notice Fee configuration
    FeeConfig public feeConfig;

    /// @notice Current sale phase
    SalePhase public salePhase;

    /// @notice Total reserve balance
    uint256 public reserveBalance;

    /// @notice Total token supply in curve
    uint256 public tokenSupply;

    /// @notice Total tokens sold
    uint256 public totalSold;

    /// @notice Total reserve collected
    uint256 public totalRaised;

    /// @notice User purchase records
    mapping(address => PurchaseRecord) public purchases;

    /// @notice User vesting schedules
    mapping(address => VestingSchedule) public vestingSchedules;

    /// @notice Whitelist
    mapping(address => bool) public whitelist;

    /// @notice Whitelist allocations
    mapping(address => uint256) public whitelistAllocations;

    /// @notice Price history
    PriceCheckpoint[] public priceHistory;

    /// @notice Graduation milestones
    GraduationMilestone[] public milestones;

    /// @notice Current milestone index
    uint256 public currentMilestone;

    /// @notice Cooldown period between purchases
    uint256 public purchaseCooldown = 1 minutes;

    /// @notice Last purchase time per user
    mapping(address => uint256) public lastPurchaseTime;

    /// @notice Is graduated (moved to DEX)
    bool public graduated;

    /// @notice DEX pair address after graduation
    address public dexPair;

    // ============================================
    // EVENTS
    // ============================================

    event CurveCreated(
        address indexed token,
        address indexed reserveToken,
        CurveType curveType
    );

    event TokensPurchased(
        address indexed buyer,
        uint256 reserveIn,
        uint256 tokensOut,
        uint256 price,
        uint256 fee
    );

    event TokensSold(
        address indexed seller,
        uint256 tokensIn,
        uint256 reserveOut,
        uint256 price,
        uint256 fee
    );

    event VestingClaimed(
        address indexed user,
        uint256 amount
    );

    event PhaseChanged(
        SalePhase oldPhase,
        SalePhase newPhase
    );

    event MilestoneAchieved(
        uint256 indexed milestoneId,
        uint256 supply,
        uint256 reserve
    );

    event Graduated(
        address indexed dexPair,
        uint256 liquidity
    );

    event WhitelistUpdated(
        address indexed user,
        bool status,
        uint256 allocation
    );

    event ReferralReward(
        address indexed referrer,
        address indexed buyer,
        uint256 reward
    );

    event FeesCollected(
        uint256 amount,
        address indexed recipient
    );

    // ============================================
    // ERRORS
    // ============================================

    error SaleNotStarted();
    error SaleEnded();
    error NotWhitelisted();
    error InsufficientAllocation(uint256 available, uint256 requested);
    error BelowMinPurchase(uint256 min, uint256 amount);
    error ExceedsMaxPurchase(uint256 max, uint256 amount);
    error ExceedsWalletLimit(uint256 limit, uint256 current);
    error HardCapReached();
    error InsufficientReserve(uint256 available, uint256 requested);
    error InsufficientTokens(uint256 available, uint256 requested);
    error SlippageExceeded(uint256 expected, uint256 actual);
    error CooldownActive(uint256 nextPurchaseTime);
    error VestingNotStarted();
    error NothingToClaim();
    error AlreadyGraduated();
    error NotGraduated();
    error InvalidCurveParams();
    error InvalidPhase();
    error EmergencyMode();
    error ZeroAmount();

    // ============================================
    // MODIFIERS
    // ============================================

    modifier onlyDuringPhase(SalePhase phase) {
        if (salePhase != phase) revert InvalidPhase();
        _;
    }

    modifier notEmergency() {
        if (salePhase == SalePhase.Emergency) revert EmergencyMode();
        _;
    }

    modifier notGraduated() {
        if (graduated) revert AlreadyGraduated();
        _;
    }

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(
        address _token,
        address _reserveToken,
        CurveParams memory _curveParams,
        SaleConfig memory _saleConfig,
        FeeConfig memory _feeConfig
    ) {
        require(_token != address(0), "Invalid token");
        require(_curveParams.reserveRatio <= MAX_RESERVE_RATIO, "Invalid reserve ratio");
        require(_feeConfig.buyFee <= MAX_FEE, "Buy fee too high");
        require(_feeConfig.sellFee <= MAX_FEE, "Sell fee too high");

        token = IERC20(_token);
        reserveToken = IERC20(_reserveToken);
        curveParams = _curveParams;
        saleConfig = _saleConfig;
        feeConfig = _feeConfig;

        salePhase = SalePhase.NotStarted;

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(CURVE_ADMIN_ROLE, msg.sender);
        _grantRole(FEE_MANAGER_ROLE, msg.sender);

        emit CurveCreated(_token, _reserveToken, _curveParams.curveType);
    }

    // ============================================
    // BUY FUNCTIONS
    // ============================================

    /**
     * @notice Buys tokens with reserve
     * @param reserveAmount Amount of reserve to spend
     * @param minTokens Minimum tokens expected (slippage protection)
     * @param referrer Optional referrer address
     */
    function buy(
        uint256 reserveAmount,
        uint256 minTokens,
        address referrer
    ) 
        external 
        nonReentrant 
        whenNotPaused 
        notEmergency 
        notGraduated
        returns (uint256 tokensOut) 
    {
        if (reserveAmount == 0) revert ZeroAmount();
        
        _checkSaleActive();
        _checkWhitelist(msg.sender, reserveAmount);
        _checkCooldown(msg.sender);
        _checkPurchaseLimits(msg.sender, reserveAmount);

        // Check hard cap
        if (saleConfig.hardCap > 0 && totalRaised + reserveAmount > saleConfig.hardCap) {
            revert HardCapReached();
        }

        // Calculate tokens out
        uint256 fee = (reserveAmount * feeConfig.buyFee) / BPS;
        uint256 netReserve = reserveAmount - fee;
        
        tokensOut = _calculatePurchaseReturn(netReserve);

        if (tokensOut < minTokens) {
            revert SlippageExceeded(minTokens, tokensOut);
        }

        // Check token availability
        uint256 available = token.balanceOf(address(this)) - _getTotalVested();
        if (tokensOut > available) {
            revert InsufficientTokens(available, tokensOut);
        }

        // Update state
        reserveBalance += netReserve;
        tokenSupply += tokensOut;
        totalSold += tokensOut;
        totalRaised += reserveAmount;

        // Update user record
        PurchaseRecord storage record = purchases[msg.sender];
        record.totalPurchased += tokensOut;
        record.totalSpent += reserveAmount;
        record.averagePrice = record.totalSpent * PRECISION / record.totalPurchased;
        if (record.firstPurchase == 0) {
            record.firstPurchase = block.timestamp;
        }
        record.lastPurchase = block.timestamp;
        lastPurchaseTime[msg.sender] = block.timestamp;

        // Handle referral
        if (referrer != address(0) && referrer != msg.sender && feeConfig.referralReward > 0) {
            uint256 referralReward = (fee * feeConfig.referralReward) / BPS;
            fee -= referralReward;
            record.referrer = referrer;
            
            purchases[referrer].totalPurchased += referralReward;
            emit ReferralReward(referrer, msg.sender, referralReward);
        }

        // Transfer reserve
        reserveToken.safeTransferFrom(msg.sender, address(this), reserveAmount);

        // Handle vesting or direct transfer
        if (saleConfig.vestingEnabled) {
            _createVesting(msg.sender, tokensOut);
        } else {
            token.safeTransfer(msg.sender, tokensOut);
        }

        // Record price checkpoint
        _recordPrice();

        // Check milestones
        _checkMilestones();

        // Send fees
        if (fee > 0 && feeConfig.feeRecipient != address(0)) {
            reserveToken.safeTransfer(feeConfig.feeRecipient, fee);
            emit FeesCollected(fee, feeConfig.feeRecipient);
        }

        emit TokensPurchased(msg.sender, reserveAmount, tokensOut, getCurrentPrice(), fee);
    }

    /**
     * @notice Buys tokens with ETH
     */
    function buyWithETH(
        uint256 minTokens,
        address referrer
    ) 
        external 
        payable 
        nonReentrant 
        whenNotPaused 
        notEmergency 
        notGraduated
        returns (uint256 tokensOut) 
    {
        require(address(reserveToken) == address(0), "Not ETH curve");
        uint256 reserveAmount = msg.value;
        
        if (reserveAmount == 0) revert ZeroAmount();
        
        _checkSaleActive();
        _checkWhitelist(msg.sender, reserveAmount);
        _checkCooldown(msg.sender);
        _checkPurchaseLimits(msg.sender, reserveAmount);

        if (saleConfig.hardCap > 0 && totalRaised + reserveAmount > saleConfig.hardCap) {
            revert HardCapReached();
        }

        uint256 fee = (reserveAmount * feeConfig.buyFee) / BPS;
        uint256 netReserve = reserveAmount - fee;
        
        tokensOut = _calculatePurchaseReturn(netReserve);

        if (tokensOut < minTokens) {
            revert SlippageExceeded(minTokens, tokensOut);
        }

        uint256 available = token.balanceOf(address(this)) - _getTotalVested();
        if (tokensOut > available) {
            revert InsufficientTokens(available, tokensOut);
        }

        reserveBalance += netReserve;
        tokenSupply += tokensOut;
        totalSold += tokensOut;
        totalRaised += reserveAmount;

        PurchaseRecord storage record = purchases[msg.sender];
        record.totalPurchased += tokensOut;
        record.totalSpent += reserveAmount;
        record.averagePrice = record.totalSpent * PRECISION / record.totalPurchased;
        if (record.firstPurchase == 0) {
            record.firstPurchase = block.timestamp;
        }
        record.lastPurchase = block.timestamp;
        lastPurchaseTime[msg.sender] = block.timestamp;

        if (referrer != address(0) && referrer != msg.sender && feeConfig.referralReward > 0) {
            uint256 referralReward = (fee * feeConfig.referralReward) / BPS;
            fee -= referralReward;
            record.referrer = referrer;
            (bool success,) = referrer.call{value: referralReward}("");
            if (success) {
                emit ReferralReward(referrer, msg.sender, referralReward);
            }
        }

        if (saleConfig.vestingEnabled) {
            _createVesting(msg.sender, tokensOut);
        } else {
            token.safeTransfer(msg.sender, tokensOut);
        }

        _recordPrice();
        _checkMilestones();

        if (fee > 0 && feeConfig.feeRecipient != address(0)) {
            (bool sent,) = feeConfig.feeRecipient.call{value: fee}("");
            require(sent, "Fee transfer failed");
            emit FeesCollected(fee, feeConfig.feeRecipient);
        }

        emit TokensPurchased(msg.sender, reserveAmount, tokensOut, getCurrentPrice(), fee);
    }

    // ============================================
    // SELL FUNCTIONS
    // ============================================

    /**
     * @notice Sells tokens back to the curve
     * @param tokenAmount Amount of tokens to sell
     * @param minReserve Minimum reserve expected
     */
    function sell(
        uint256 tokenAmount,
        uint256 minReserve
    ) 
        external 
        nonReentrant 
        whenNotPaused 
        notEmergency 
        notGraduated
        returns (uint256 reserveOut) 
    {
        if (tokenAmount == 0) revert ZeroAmount();

        // Calculate reserve out
        reserveOut = _calculateSaleReturn(tokenAmount);

        uint256 fee = (reserveOut * feeConfig.sellFee) / BPS;
        uint256 netReserve = reserveOut - fee;

        if (netReserve < minReserve) {
            revert SlippageExceeded(minReserve, netReserve);
        }

        if (reserveOut > reserveBalance) {
            revert InsufficientReserve(reserveBalance, reserveOut);
        }

        // Update state
        reserveBalance -= reserveOut;
        tokenSupply -= tokenAmount;

        // Transfer tokens from seller
        token.safeTransferFrom(msg.sender, address(this), tokenAmount);

        // Transfer reserve to seller
        if (address(reserveToken) == address(0)) {
            (bool sent,) = msg.sender.call{value: netReserve}("");
            require(sent, "ETH transfer failed");
        } else {
            reserveToken.safeTransfer(msg.sender, netReserve);
        }

        // Record price
        _recordPrice();

        // Send fees
        if (fee > 0 && feeConfig.feeRecipient != address(0)) {
            if (address(reserveToken) == address(0)) {
                (bool sent,) = feeConfig.feeRecipient.call{value: fee}("");
                require(sent, "Fee transfer failed");
            } else {
                reserveToken.safeTransfer(feeConfig.feeRecipient, fee);
            }
            emit FeesCollected(fee, feeConfig.feeRecipient);
        }

        emit TokensSold(msg.sender, tokenAmount, netReserve, getCurrentPrice(), fee);
    }

    // ============================================
    // VESTING FUNCTIONS
    // ============================================

    /**
     * @notice Claims vested tokens
     */
    function claimVested() external nonReentrant returns (uint256 amount) {
        VestingSchedule storage schedule = vestingSchedules[msg.sender];
        
        if (schedule.startTime == 0) revert VestingNotStarted();
        if (block.timestamp < schedule.cliffEnd) revert VestingNotStarted();

        amount = _getVestedAmount(msg.sender) - schedule.releasedAmount;
        if (amount == 0) revert NothingToClaim();

        schedule.releasedAmount += amount;
        purchases[msg.sender].claimedAmount += amount;

        token.safeTransfer(msg.sender, amount);

        emit VestingClaimed(msg.sender, amount);
    }

    /**
     * @notice Creates vesting schedule for user
     */
    function _createVesting(address user, uint256 amount) internal {
        VestingSchedule storage schedule = vestingSchedules[user];

        if (schedule.startTime == 0) {
            schedule.startTime = block.timestamp;
            schedule.cliffEnd = block.timestamp + saleConfig.cliffDuration;
            schedule.endTime = block.timestamp + saleConfig.vestingDuration;
        }

        schedule.totalAmount += amount;
        purchases[user].vestedAmount += amount;
    }

    /**
     * @notice Gets vested amount for user
     */
    function _getVestedAmount(address user) internal view returns (uint256) {
        VestingSchedule storage schedule = vestingSchedules[user];

        if (schedule.startTime == 0) return 0;
        if (block.timestamp < schedule.cliffEnd) return 0;
        if (block.timestamp >= schedule.endTime) return schedule.totalAmount;

        uint256 vestingDuration = schedule.endTime - schedule.cliffEnd;
        uint256 timePassed = block.timestamp - schedule.cliffEnd;

        return (schedule.totalAmount * timePassed) / vestingDuration;
    }

    /**
     * @notice Gets total vested tokens
     */
    function _getTotalVested() internal view returns (uint256) {
        // Would iterate users or track globally
        return 0;
    }

    // ============================================
    // CURVE CALCULATIONS
    // ============================================

    /**
     * @notice Calculates tokens received for reserve input
     */
    function _calculatePurchaseReturn(uint256 reserveAmount) internal view returns (uint256) {
        uint256 supply = tokenSupply + curveParams.virtualSupply;
        uint256 reserve = reserveBalance + curveParams.virtualBalance;

        if (curveParams.curveType == CurveType.Linear) {
            return _calculateLinearPurchase(reserveAmount, supply);
        } else if (curveParams.curveType == CurveType.Exponential) {
            return _calculateExponentialPurchase(reserveAmount, supply, reserve);
        } else if (curveParams.curveType == CurveType.Logarithmic) {
            return _calculateLogarithmicPurchase(reserveAmount, supply);
        } else if (curveParams.curveType == CurveType.Sigmoid) {
            return _calculateSigmoidPurchase(reserveAmount, supply);
        } else if (curveParams.curveType == CurveType.Polynomial) {
            return _calculatePolynomialPurchase(reserveAmount, supply);
        }

        // Bancor formula for custom
        return _calculateBancorPurchase(reserveAmount, supply, reserve);
    }

    /**
     * @notice Calculates reserve received for token input
     */
    function _calculateSaleReturn(uint256 tokenAmount) internal view returns (uint256) {
        uint256 supply = tokenSupply + curveParams.virtualSupply;
        uint256 reserve = reserveBalance + curveParams.virtualBalance;

        if (curveParams.curveType == CurveType.Linear) {
            return _calculateLinearSale(tokenAmount, supply);
        } else if (curveParams.curveType == CurveType.Exponential) {
            return _calculateExponentialSale(tokenAmount, supply, reserve);
        } else if (curveParams.curveType == CurveType.Logarithmic) {
            return _calculateLogarithmicSale(tokenAmount, supply);
        } else if (curveParams.curveType == CurveType.Sigmoid) {
            return _calculateSigmoidSale(tokenAmount, supply);
        } else if (curveParams.curveType == CurveType.Polynomial) {
            return _calculatePolynomialSale(tokenAmount, supply);
        }

        return _calculateBancorSale(tokenAmount, supply, reserve);
    }

    /**
     * @notice Linear curve: price = initialPrice + slope * supply
     */
    function _calculateLinearPurchase(uint256 reserveAmount, uint256 supply) internal view returns (uint256) {
        // Solve for tokens: reserve = initialPrice * tokens + (slope * tokens^2) / 2
        // Using quadratic formula
        uint256 a = curveParams.slope / 2;
        uint256 b = curveParams.initialPrice + (curveParams.slope * supply);
        uint256 c = reserveAmount;

        if (a == 0) {
            return (c * PRECISION) / b;
        }

        // tokens = (-b + sqrt(b^2 + 4ac)) / (2a)
        uint256 discriminant = b * b + 4 * a * c * PRECISION;
        uint256 sqrtDisc = Math.sqrt(discriminant);
        
        return ((sqrtDisc - b) * PRECISION) / (2 * a);
    }

    function _calculateLinearSale(uint256 tokenAmount, uint256 supply) internal view returns (uint256) {
        uint256 newSupply = supply - tokenAmount;
        uint256 avgPrice = curveParams.initialPrice + 
            (curveParams.slope * (supply + newSupply)) / 2;
        return (tokenAmount * avgPrice) / PRECISION;
    }

    /**
     * @notice Exponential curve: price = initialPrice * (1 + rate)^supply
     */
    function _calculateExponentialPurchase(
        uint256 reserveAmount, 
        uint256 supply, 
        uint256 reserve
    ) internal view returns (uint256) {
        // Simplified: using reserve ratio
        uint256 ratio = curveParams.reserveRatio;
        
        // tokens = supply * ((1 + deposit/reserve)^ratio - 1)
        if (reserve == 0) {
            return reserveAmount * PRECISION / curveParams.initialPrice;
        }

        uint256 baseN = reserve + reserveAmount;
        uint256 result = (supply * _power(baseN, reserve, ratio)) / PRECISION;
        return result - supply;
    }

    function _calculateExponentialSale(
        uint256 tokenAmount, 
        uint256 supply, 
        uint256 reserve
    ) internal view returns (uint256) {
        uint256 ratio = curveParams.reserveRatio;
        
        if (supply == 0) return 0;

        uint256 baseN = supply - tokenAmount;
        uint256 result = reserve - (reserve * _power(baseN, supply, ratio)) / PRECISION;
        return result;
    }

    /**
     * @notice Logarithmic curve: price = initialPrice * log(1 + supply/k)
     */
    function _calculateLogarithmicPurchase(uint256 reserveAmount, uint256 supply) internal view returns (uint256) {
        // Simplified logarithmic approximation
        uint256 k = curveParams.exponent; // Scale factor
        
        // Using linear approximation for simplicity
        uint256 price = curveParams.initialPrice + (curveParams.slope * _log2(supply + k)) / PRECISION;
        return (reserveAmount * PRECISION) / price;
    }

    function _calculateLogarithmicSale(uint256 tokenAmount, uint256 supply) internal view returns (uint256) {
        uint256 k = curveParams.exponent;
        uint256 price = curveParams.initialPrice + (curveParams.slope * _log2(supply + k)) / PRECISION;
        return (tokenAmount * price) / PRECISION;
    }

    /**
     * @notice Sigmoid curve: price = maxPrice / (1 + e^(-k*(supply - midpoint)))
     */
    function _calculateSigmoidPurchase(uint256 reserveAmount, uint256 supply) internal view returns (uint256) {
        uint256 price = _getSigmoidPrice(supply);
        return (reserveAmount * PRECISION) / price;
    }

    function _calculateSigmoidSale(uint256 tokenAmount, uint256 supply) internal view returns (uint256) {
        uint256 price = _getSigmoidPrice(supply);
        return (tokenAmount * price) / PRECISION;
    }

    function _getSigmoidPrice(uint256 supply) internal view returns (uint256) {
        // Simplified sigmoid using linear interpolation
        uint256 midpoint = curveParams.exponent; // Midpoint supply
        
        if (supply < midpoint / 2) {
            return curveParams.initialPrice + (curveParams.maxPrice - curveParams.initialPrice) * supply / midpoint;
        } else if (supply < midpoint * 2) {
            return (curveParams.initialPrice + curveParams.maxPrice) / 2 + 
                   (curveParams.maxPrice - curveParams.initialPrice) * (supply - midpoint / 2) / (midpoint * 3);
        } else {
            return curveParams.maxPrice;
        }
    }

    /**
     * @notice Polynomial curve: price = initialPrice + slope * supply^exponent
     */
    function _calculatePolynomialPurchase(uint256 reserveAmount, uint256 supply) internal view returns (uint256) {
        uint256 price = _getPolynomialPrice(supply);
        return (reserveAmount * PRECISION) / price;
    }

    function _calculatePolynomialSale(uint256 tokenAmount, uint256 supply) internal view returns (uint256) {
        uint256 price = _getPolynomialPrice(supply);
        return (tokenAmount * price) / PRECISION;
    }

    function _getPolynomialPrice(uint256 supply) internal view returns (uint256) {
        uint256 powered = _pow(supply, curveParams.exponent);
        uint256 price = curveParams.initialPrice + (curveParams.slope * powered) / PRECISION;
        return price > curveParams.maxPrice ? curveParams.maxPrice : price;
    }

    /**
     * @notice Bancor formula for custom curves
     */
    function _calculateBancorPurchase(
        uint256 reserveAmount, 
        uint256 supply, 
        uint256 reserve
    ) internal view returns (uint256) {
        uint256 ratio = curveParams.reserveRatio;
        
        if (reserve == 0 || supply == 0) {
            return reserveAmount * PRECISION / curveParams.initialPrice;
        }

        // Return = supply * ((1 + depositAmount / reserveBalance) ^ connectorWeight - 1)
        uint256 temp = PRECISION + (reserveAmount * PRECISION / reserve);
        uint256 result = supply * (_power(temp, PRECISION, ratio) - PRECISION);
        return result / PRECISION;
    }

    function _calculateBancorSale(
        uint256 tokenAmount, 
        uint256 supply, 
        uint256 reserve
    ) internal view returns (uint256) {
        uint256 ratio = curveParams.reserveRatio;
        
        if (supply == 0) return 0;

        // Return = reserveBalance * (1 - (1 - _sellAmount / _supply) ^ (1 / connectorWeight))
        uint256 temp = PRECISION - (tokenAmount * PRECISION / supply);
        uint256 invertedRatio = (PRECISION * PRECISION) / ratio;
        uint256 result = reserve * (PRECISION - _power(temp, PRECISION, invertedRatio));
        return result / PRECISION;
    }

    // ============================================
    // MATH HELPERS
    // ============================================

    function _power(uint256 base, uint256 denominator, uint256 exp) internal pure returns (uint256) {
        if (exp == 0) return PRECISION;
        if (base == 0) return 0;
        
        // Simplified power calculation
        uint256 result = PRECISION;
        uint256 basePow = (base * PRECISION) / denominator;
        
        uint256 n = exp / PRECISION;
        for (uint256 i = 0; i < n && i < 256; i++) {
            result = (result * basePow) / PRECISION;
        }
        
        return result;
    }

    function _pow(uint256 base, uint256 exp) internal pure returns (uint256) {
        if (exp == 0) return PRECISION;
        if (base == 0) return 0;
        
        uint256 result = PRECISION;
        for (uint256 i = 0; i < exp && i < 256; i++) {
            result = (result * base) / PRECISION;
        }
        return result;
    }

    function _log2(uint256 x) internal pure returns (uint256) {
        if (x <= PRECISION) return 0;
        
        uint256 result = 0;
        uint256 temp = x;
        
        while (temp > PRECISION) {
            temp = temp / 2;
            result += PRECISION;
        }
        
        return result;
    }

    // ============================================
    // INTERNAL HELPERS
    // ============================================

    function _checkSaleActive() internal view {
        if (salePhase == SalePhase.NotStarted) {
            if (block.timestamp < saleConfig.startTime) revert SaleNotStarted();
        }
        if (salePhase == SalePhase.Closed) revert SaleEnded();
        if (block.timestamp > saleConfig.endTime && saleConfig.endTime > 0) revert SaleEnded();
    }

    function _checkWhitelist(address user, uint256 amount) internal view {
        if (salePhase == SalePhase.Whitelist) {
            if (!whitelist[user]) revert NotWhitelisted();
            if (whitelistAllocations[user] > 0) {
                uint256 spent = purchases[user].totalSpent;
                if (spent + amount > whitelistAllocations[user]) {
                    revert InsufficientAllocation(
                        whitelistAllocations[user] - spent,
                        amount
                    );
                }
            }
        }
    }

    function _checkCooldown(address user) internal view {
        if (purchaseCooldown > 0) {
            if (block.timestamp < lastPurchaseTime[user] + purchaseCooldown) {
                revert CooldownActive(lastPurchaseTime[user] + purchaseCooldown);
            }
        }
    }

    function _checkPurchaseLimits(address user, uint256 amount) internal view {
        if (amount < saleConfig.minPurchase) {
            revert BelowMinPurchase(saleConfig.minPurchase, amount);
        }
        if (saleConfig.maxPurchase > 0 && amount > saleConfig.maxPurchase) {
            revert ExceedsMaxPurchase(saleConfig.maxPurchase, amount);
        }
        if (saleConfig.maxPerWallet > 0) {
            uint256 total = purchases[user].totalSpent + amount;
            if (total > saleConfig.maxPerWallet) {
                revert ExceedsWalletLimit(saleConfig.maxPerWallet, total);
            }
        }
    }

    function _recordPrice() internal {
        priceHistory.push(PriceCheckpoint({
            timestamp: block.timestamp,
            price: getCurrentPrice(),
            supply: tokenSupply,
            reserve: reserveBalance
        }));
    }

    function _checkMilestones() internal {
        while (currentMilestone < milestones.length) {
            GraduationMilestone storage milestone = milestones[currentMilestone];
            
            if (tokenSupply >= milestone.supplyThreshold && 
                reserveBalance >= milestone.reserveThreshold) {
                milestone.achieved = true;
                milestone.achievedAt = block.timestamp;
                
                emit MilestoneAchieved(currentMilestone, tokenSupply, reserveBalance);
                currentMilestone++;
            } else {
                break;
            }
        }
    }

    // ============================================
    // ADMIN FUNCTIONS
    // ============================================

    /**
     * @notice Sets sale phase
     */
    function setPhase(SalePhase newPhase) external onlyRole(CURVE_ADMIN_ROLE) {
        SalePhase oldPhase = salePhase;
        salePhase = newPhase;
        emit PhaseChanged(oldPhase, newPhase);
    }

    /**
     * @notice Updates whitelist
     */
    function updateWhitelist(
        address[] calldata users,
        bool[] calldata statuses,
        uint256[] calldata allocations
    ) external onlyRole(CURVE_ADMIN_ROLE) {
        require(users.length == statuses.length && users.length == allocations.length, "Length mismatch");

        for (uint256 i = 0; i < users.length; i++) {
            whitelist[users[i]] = statuses[i];
            whitelistAllocations[users[i]] = allocations[i];
            emit WhitelistUpdated(users[i], statuses[i], allocations[i]);
        }
    }

    /**
     * @notice Adds graduation milestone
     */
    function addMilestone(
        uint256 supplyThreshold,
        uint256 reserveThreshold
    ) external onlyRole(CURVE_ADMIN_ROLE) {
        milestones.push(GraduationMilestone({
            supplyThreshold: supplyThreshold,
            reserveThreshold: reserveThreshold,
            achieved: false,
            achievedAt: 0
        }));
    }

    /**
     * @notice Updates fees
     */
    function updateFees(
        uint256 buyFee,
        uint256 sellFee,
        uint256 referralReward
    ) external onlyRole(FEE_MANAGER_ROLE) {
        require(buyFee <= MAX_FEE, "Buy fee too high");
        require(sellFee <= MAX_FEE, "Sell fee too high");

        feeConfig.buyFee = buyFee;
        feeConfig.sellFee = sellFee;
        feeConfig.referralReward = referralReward;
    }

    /**
     * @notice Pauses the curve
     */
    function pause() external onlyRole(CURVE_ADMIN_ROLE) {
        _pause();
    }

    /**
     * @notice Unpauses the curve
     */
    function unpause() external onlyRole(CURVE_ADMIN_ROLE) {
        _unpause();
    }

    /**
     * @notice Emergency stop
     */
    function emergencyStop() external onlyRole(DEFAULT_ADMIN_ROLE) {
        salePhase = SalePhase.Emergency;
        emit PhaseChanged(salePhase, SalePhase.Emergency);
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Gets current token price
     */
    function getCurrentPrice() public view returns (uint256) {
        uint256 supply = tokenSupply + curveParams.virtualSupply;
        
        if (curveParams.curveType == CurveType.Linear) {
            return curveParams.initialPrice + (curveParams.slope * supply) / PRECISION;
        } else if (curveParams.curveType == CurveType.Polynomial) {
            return _getPolynomialPrice(supply);
        } else if (curveParams.curveType == CurveType.Sigmoid) {
            return _getSigmoidPrice(supply);
        } else {
            // Bancor/Exponential
            uint256 reserve = reserveBalance + curveParams.virtualBalance;
            if (supply == 0) return curveParams.initialPrice;
            return (reserve * MAX_RESERVE_RATIO) / (supply * curveParams.reserveRatio);
        }
    }

    /**
     * @notice Gets expected tokens for reserve amount
     */
    function getExpectedTokens(uint256 reserveAmount) external view returns (uint256) {
        uint256 fee = (reserveAmount * feeConfig.buyFee) / BPS;
        return _calculatePurchaseReturn(reserveAmount - fee);
    }

    /**
     * @notice Gets expected reserve for token amount
     */
    function getExpectedReserve(uint256 tokenAmount) external view returns (uint256) {
        uint256 gross = _calculateSaleReturn(tokenAmount);
        uint256 fee = (gross * feeConfig.sellFee) / BPS;
        return gross - fee;
    }

    /**
     * @notice Gets price history length
     */
    function getPriceHistoryLength() external view returns (uint256) {
        return priceHistory.length;
    }

    /**
     * @notice Gets TWAP for period
     */
    function getTWAP(uint256 period) external view returns (uint256) {
        if (priceHistory.length == 0) return getCurrentPrice();
        
        uint256 startTime = block.timestamp - period;
        uint256 sum = 0;
        uint256 count = 0;
        
        for (uint256 i = priceHistory.length; i > 0; i--) {
            PriceCheckpoint storage cp = priceHistory[i - 1];
            if (cp.timestamp < startTime) break;
            sum += cp.price;
            count++;
        }
        
        return count > 0 ? sum / count : getCurrentPrice();
    }

    /**
     * @notice Gets vested amount for user
     */
    function getVestedAmount(address user) external view returns (uint256) {
        return _getVestedAmount(user);
    }

    /**
     * @notice Gets claimable amount for user
     */
    function getClaimableAmount(address user) external view returns (uint256) {
        VestingSchedule storage schedule = vestingSchedules[user];
        return _getVestedAmount(user) - schedule.releasedAmount;
    }

    /**
     * @notice Gets milestone count
     */
    function getMilestoneCount() external view returns (uint256) {
        return milestones.length;
    }

    receive() external payable {}
}
