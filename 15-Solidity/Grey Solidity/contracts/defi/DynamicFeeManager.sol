// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";

/**
 * @title DynamicFeeManager
 * @author Grey Team
 * @notice Adaptive fee mechanism for AMMs based on market conditions
 * @dev Implements dynamic fee adjustments based on volatility, volume,
 *      liquidity depth, and market conditions inspired by Uniswap v3,
 *      Curve, and Trader Joe's Liquidity Book
 * 
 * Key mechanisms:
 * - Volatility-based fee tiers (higher vol = higher fees)
 * - Volume-weighted fee discounts
 * - Time-weighted average fee calculation
 * - Liquidity concentration bonuses
 * - MEV-resistant fee spikes
 */
contract DynamicFeeManager is AccessControl, ReentrancyGuard {
    // ============ Roles ============
    bytes32 public constant POOL_ROLE = keccak256("POOL_ROLE");
    bytes32 public constant ORACLE_ROLE = keccak256("ORACLE_ROLE");
    bytes32 public constant GOVERNOR_ROLE = keccak256("GOVERNOR_ROLE");

    // ============ Structs ============
    
    /**
     * @notice Fee configuration for a pool
     */
    struct PoolFeeConfig {
        uint256 baseFee;                // Base fee in basis points
        uint256 minFee;                 // Minimum fee floor
        uint256 maxFee;                 // Maximum fee ceiling
        uint256 volatilityMultiplier;  // Multiplier for vol adjustment
        uint256 volumeDiscountRate;     // Volume discount rate
        uint256 liquidityBonus;         // Bonus for deep liquidity
        bool isActive;
    }

    /**
     * @notice Volatility tracking
     */
    struct VolatilityData {
        uint256 lastPrice;
        uint256 lastUpdateTime;
        uint256 priceVariance;          // Sum of squared returns
        uint256 sampleCount;
        uint256 ewmaVolatility;         // Exponentially weighted moving average
        uint256[] recentPrices;         // Circular buffer
        uint256 priceIndex;
    }

    /**
     * @notice Volume tracking
     */
    struct VolumeData {
        uint256 cumulativeVolume;
        uint256 volume24h;
        uint256 volumeLastPeriod;
        uint256 periodStartTime;
        uint256[] hourlyVolumes;
        uint256 hourIndex;
    }

    /**
     * @notice Fee tier based on conditions
     */
    struct FeeTier {
        uint256 minVolatility;          // Minimum volatility for this tier
        uint256 maxVolatility;          // Maximum volatility for this tier
        uint256 feeMultiplier;          // Fee multiplier (100 = 1x)
        uint256 minLiquidity;           // Minimum liquidity requirement
    }

    /**
     * @notice User fee discount
     */
    struct UserDiscount {
        uint256 cumulativeVolume;       // User's total volume
        uint256 discountTier;           // Current discount tier
        uint256 lastTradeTime;
        uint256 loyaltyPoints;
    }

    /**
     * @notice Time-weighted average fee
     */
    struct TWAFObservation {
        uint256 timestamp;
        uint256 cumulativeFee;
        uint256 fee;
    }

    // ============ Constants ============
    uint256 public constant PRECISION = 1e18;
    uint256 public constant BPS = 10000;
    uint256 public constant MAX_FEE = 1000;             // 10% max
    uint256 public constant MIN_FEE = 1;                // 0.01% min
    uint256 public constant EWMA_DECAY = 94;            // ~6% decay per sample (94/100)
    uint256 public constant VOLATILITY_WINDOW = 24;     // Hours for volatility calc
    uint256 public constant VOLUME_PERIOD = 1 hours;
    uint256 public constant MAX_PRICE_SAMPLES = 24;
    uint256 public constant FEE_SPIKE_COOLDOWN = 5 minutes;

    // ============ State Variables ============
    
    // Pool configurations
    mapping(address => PoolFeeConfig) public poolConfigs;
    address[] public registeredPools;
    
    // Volatility tracking per pool
    mapping(address => VolatilityData) public volatilityData;
    
    // Volume tracking per pool
    mapping(address => VolumeData) public volumeData;
    
    // Fee tiers
    FeeTier[] public feeTiers;
    
    // User discounts
    mapping(address => mapping(address => UserDiscount)) public userDiscounts; // pool => user => discount
    
    // Volume discount thresholds
    uint256[] public volumeDiscountThresholds;
    uint256[] public volumeDiscountRates;
    
    // TWAF observations
    mapping(address => TWAFObservation[]) public twafObservations;
    uint256 public constant MAX_TWAF_OBSERVATIONS = 100;
    
    // Fee spike protection
    mapping(address => uint256) public lastFeeUpdate;
    mapping(address => uint256) public previousFee;

    // Global settings
    uint256 public globalFeeMultiplier = 100; // 100 = 1x
    bool public dynamicFeesEnabled = true;

    // ============ Events ============
    event PoolRegistered(address indexed pool, uint256 baseFee, uint256 minFee, uint256 maxFee);
    event PoolConfigUpdated(address indexed pool);
    event FeeCalculated(address indexed pool, uint256 baseFee, uint256 dynamicFee, uint256 finalFee);
    event VolatilityUpdated(address indexed pool, uint256 newVolatility);
    event VolumeUpdated(address indexed pool, uint256 volume24h);
    event FeeTierAdded(uint256 tierId, uint256 minVol, uint256 maxVol, uint256 multiplier);
    event UserDiscountApplied(address indexed pool, address indexed user, uint256 discount);
    event GlobalMultiplierUpdated(uint256 oldMultiplier, uint256 newMultiplier);

    // ============ Errors ============
    error PoolNotRegistered();
    error PoolAlreadyRegistered();
    error InvalidFeeRange();
    error InvalidTier();
    error FeeSpikeProtection();
    error DynamicFeesDisabled();

    // ============ Constructor ============
    constructor() {
        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(GOVERNOR_ROLE, msg.sender);
        
        // Initialize default fee tiers
        _initializeDefaultTiers();
        
        // Initialize volume discount thresholds
        _initializeVolumeDiscounts();
    }

    function _initializeDefaultTiers() internal {
        // Low volatility: normal fees
        feeTiers.push(FeeTier({
            minVolatility: 0,
            maxVolatility: 2e16,   // 2% daily
            feeMultiplier: 100,    // 1x
            minLiquidity: 0
        }));
        
        // Medium volatility: slightly higher fees
        feeTiers.push(FeeTier({
            minVolatility: 2e16,
            maxVolatility: 5e16,   // 5% daily
            feeMultiplier: 150,    // 1.5x
            minLiquidity: 0
        }));
        
        // High volatility: higher fees
        feeTiers.push(FeeTier({
            minVolatility: 5e16,
            maxVolatility: 10e16,  // 10% daily
            feeMultiplier: 200,    // 2x
            minLiquidity: 0
        }));
        
        // Extreme volatility: maximum fees
        feeTiers.push(FeeTier({
            minVolatility: 10e16,
            maxVolatility: type(uint256).max,
            feeMultiplier: 300,    // 3x
            minLiquidity: 0
        }));
    }

    function _initializeVolumeDiscounts() internal {
        // Volume thresholds and corresponding discount rates
        volumeDiscountThresholds.push(100_000e18);   // $100k
        volumeDiscountRates.push(100);                // 1% discount
        
        volumeDiscountThresholds.push(1_000_000e18); // $1M
        volumeDiscountRates.push(250);                // 2.5% discount
        
        volumeDiscountThresholds.push(10_000_000e18);// $10M
        volumeDiscountRates.push(500);                // 5% discount
        
        volumeDiscountThresholds.push(100_000_000e18);// $100M
        volumeDiscountRates.push(1000);               // 10% discount
    }

    // ============ Pool Registration ============
    
    /**
     * @notice Register a new pool for dynamic fees
     */
    function registerPool(
        address pool,
        uint256 baseFee,
        uint256 minFee,
        uint256 maxFee,
        uint256 volatilityMultiplier
    ) external onlyRole(GOVERNOR_ROLE) {
        if (poolConfigs[pool].isActive) revert PoolAlreadyRegistered();
        if (minFee > baseFee || baseFee > maxFee) revert InvalidFeeRange();
        if (maxFee > MAX_FEE) revert InvalidFeeRange();
        
        poolConfigs[pool] = PoolFeeConfig({
            baseFee: baseFee,
            minFee: minFee,
            maxFee: maxFee,
            volatilityMultiplier: volatilityMultiplier,
            volumeDiscountRate: 100, // 1x
            liquidityBonus: 0,
            isActive: true
        });
        
        registeredPools.push(pool);
        
        // Initialize volatility tracking
        volatilityData[pool].recentPrices = new uint256[](MAX_PRICE_SAMPLES);
        
        // Initialize volume tracking
        volumeData[pool].hourlyVolumes = new uint256[](24);
        volumeData[pool].periodStartTime = block.timestamp;
        
        emit PoolRegistered(pool, baseFee, minFee, maxFee);
    }

    /**
     * @notice Update pool configuration
     */
    function updatePoolConfig(
        address pool,
        uint256 baseFee,
        uint256 minFee,
        uint256 maxFee,
        uint256 volatilityMultiplier,
        uint256 volumeDiscountRate,
        uint256 liquidityBonus
    ) external onlyRole(GOVERNOR_ROLE) {
        if (!poolConfigs[pool].isActive) revert PoolNotRegistered();
        if (minFee > baseFee || baseFee > maxFee) revert InvalidFeeRange();
        
        PoolFeeConfig storage config = poolConfigs[pool];
        config.baseFee = baseFee;
        config.minFee = minFee;
        config.maxFee = maxFee;
        config.volatilityMultiplier = volatilityMultiplier;
        config.volumeDiscountRate = volumeDiscountRate;
        config.liquidityBonus = liquidityBonus;
        
        emit PoolConfigUpdated(pool);
    }

    // ============ Fee Calculation ============
    
    /**
     * @notice Calculate the current dynamic fee for a pool
     * @param pool Pool address
     * @param tradeAmount Trade amount for volume discount calculation
     * @param user User address for loyalty discount
     * @return fee The calculated fee in basis points
     */
    function calculateFee(
        address pool,
        uint256 tradeAmount,
        address user
    ) external view returns (uint256 fee) {
        if (!dynamicFeesEnabled) {
            return poolConfigs[pool].baseFee;
        }
        
        PoolFeeConfig storage config = poolConfigs[pool];
        if (!config.isActive) revert PoolNotRegistered();
        
        // Start with base fee
        fee = config.baseFee;
        
        // Apply volatility adjustment
        fee = _applyVolatilityAdjustment(pool, fee, config);
        
        // Apply volume discount
        fee = _applyVolumeDiscount(pool, user, fee, tradeAmount);
        
        // Apply global multiplier
        fee = (fee * globalFeeMultiplier) / 100;
        
        // Enforce min/max bounds
        if (fee < config.minFee) fee = config.minFee;
        if (fee > config.maxFee) fee = config.maxFee;
        
        return fee;
    }

    /**
     * @notice Get fee with spike protection
     */
    function getFeeWithSpikeProtection(
        address pool,
        uint256 tradeAmount,
        address user
    ) external returns (uint256 fee) {
        fee = this.calculateFee(pool, tradeAmount, user);
        
        // Spike protection: limit rate of change
        if (block.timestamp < lastFeeUpdate[pool] + FEE_SPIKE_COOLDOWN) {
            uint256 maxIncrease = (previousFee[pool] * 120) / 100; // Max 20% increase
            if (fee > maxIncrease) {
                fee = maxIncrease;
            }
        }
        
        previousFee[pool] = fee;
        lastFeeUpdate[pool] = block.timestamp;
        
        emit FeeCalculated(pool, poolConfigs[pool].baseFee, fee, fee);
        
        return fee;
    }

    function _applyVolatilityAdjustment(
        address pool,
        uint256 baseFee,
        PoolFeeConfig storage config
    ) internal view returns (uint256) {
        uint256 volatility = volatilityData[pool].ewmaVolatility;
        
        // Find applicable tier
        uint256 multiplier = 100;
        for (uint256 i = 0; i < feeTiers.length; i++) {
            if (volatility >= feeTiers[i].minVolatility && 
                volatility < feeTiers[i].maxVolatility) {
                multiplier = feeTiers[i].feeMultiplier;
                break;
            }
        }
        
        // Apply pool-specific volatility multiplier
        multiplier = (multiplier * config.volatilityMultiplier) / 100;
        
        return (baseFee * multiplier) / 100;
    }

    function _applyVolumeDiscount(
        address pool,
        address user,
        uint256 fee,
        uint256 /* tradeAmount */
    ) internal view returns (uint256) {
        UserDiscount storage discount = userDiscounts[pool][user];
        
        // Find applicable discount tier
        uint256 discountRate = 0;
        for (uint256 i = volumeDiscountThresholds.length; i > 0; i--) {
            if (discount.cumulativeVolume >= volumeDiscountThresholds[i - 1]) {
                discountRate = volumeDiscountRates[i - 1];
                break;
            }
        }
        
        // Apply loyalty bonus
        if (discount.loyaltyPoints > 0) {
            discountRate += discount.loyaltyPoints / 100; // 1% per 100 loyalty points
        }
        
        // Cap discount at 20%
        if (discountRate > 2000) discountRate = 2000;
        
        return (fee * (BPS - discountRate)) / BPS;
    }

    // ============ Data Updates ============
    
    /**
     * @notice Update price data for volatility calculation (called by pool/oracle)
     */
    function updatePrice(address pool, uint256 price) external onlyRole(ORACLE_ROLE) {
        if (!poolConfigs[pool].isActive) revert PoolNotRegistered();
        
        VolatilityData storage vol = volatilityData[pool];
        
        if (vol.lastPrice > 0) {
            // Calculate return (log approximation)
            uint256 return_;
            if (price > vol.lastPrice) {
                return_ = ((price - vol.lastPrice) * PRECISION) / vol.lastPrice;
            } else {
                return_ = ((vol.lastPrice - price) * PRECISION) / vol.lastPrice;
            }
            
            // Update EWMA volatility
            vol.ewmaVolatility = (vol.ewmaVolatility * EWMA_DECAY + return_ * (100 - EWMA_DECAY)) / 100;
            
            // Store in circular buffer
            vol.recentPrices[vol.priceIndex] = price;
            vol.priceIndex = (vol.priceIndex + 1) % MAX_PRICE_SAMPLES;
            vol.sampleCount++;
        }
        
        vol.lastPrice = price;
        vol.lastUpdateTime = block.timestamp;
        
        emit VolatilityUpdated(pool, vol.ewmaVolatility);
    }

    /**
     * @notice Record trade volume (called by pool)
     */
    function recordVolume(
        address pool,
        address user,
        uint256 amount
    ) external onlyRole(POOL_ROLE) {
        if (!poolConfigs[pool].isActive) revert PoolNotRegistered();
        
        VolumeData storage vol = volumeData[pool];
        
        // Update hourly bucket
        uint256 currentHour = (block.timestamp / 1 hours) % 24;
        if (currentHour != vol.hourIndex) {
            // Moving to new hour, shift and reset
            vol.hourlyVolumes[currentHour] = 0;
            vol.hourIndex = currentHour;
        }
        vol.hourlyVolumes[currentHour] += amount;
        
        // Calculate 24h volume
        vol.volume24h = 0;
        for (uint256 i = 0; i < 24; i++) {
            vol.volume24h += vol.hourlyVolumes[i];
        }
        
        vol.cumulativeVolume += amount;
        
        // Update user discount data
        UserDiscount storage discount = userDiscounts[pool][user];
        discount.cumulativeVolume += amount;
        
        // Award loyalty points (1 point per 1000 USD equivalent)
        discount.loyaltyPoints += amount / 1000e18;
        discount.lastTradeTime = block.timestamp;
        
        // Update TWAF observation
        _recordTWAFObservation(pool);
        
        emit VolumeUpdated(pool, vol.volume24h);
    }

    function _recordTWAFObservation(address pool) internal {
        TWAFObservation[] storage observations = twafObservations[pool];
        uint256 currentFee = poolConfigs[pool].baseFee;
        
        if (observations.length == 0) {
            observations.push(TWAFObservation({
                timestamp: block.timestamp,
                cumulativeFee: currentFee,
                fee: currentFee
            }));
            return;
        }
        
        TWAFObservation storage last = observations[observations.length - 1];
        uint256 timeElapsed = block.timestamp - last.timestamp;
        uint256 cumulativeFee = last.cumulativeFee + (last.fee * timeElapsed);
        
        if (observations.length >= MAX_TWAF_OBSERVATIONS) {
            // Shift observations
            for (uint256 i = 0; i < observations.length - 1; i++) {
                observations[i] = observations[i + 1];
            }
            observations[observations.length - 1] = TWAFObservation({
                timestamp: block.timestamp,
                cumulativeFee: cumulativeFee,
                fee: currentFee
            });
        } else {
            observations.push(TWAFObservation({
                timestamp: block.timestamp,
                cumulativeFee: cumulativeFee,
                fee: currentFee
            }));
        }
    }

    // ============ Fee Tier Management ============
    
    /**
     * @notice Add a new fee tier
     */
    function addFeeTier(
        uint256 minVolatility,
        uint256 maxVolatility,
        uint256 feeMultiplier,
        uint256 minLiquidity
    ) external onlyRole(GOVERNOR_ROLE) {
        require(minVolatility < maxVolatility, "Invalid range");
        require(feeMultiplier > 0 && feeMultiplier <= 1000, "Invalid multiplier");
        
        feeTiers.push(FeeTier({
            minVolatility: minVolatility,
            maxVolatility: maxVolatility,
            feeMultiplier: feeMultiplier,
            minLiquidity: minLiquidity
        }));
        
        emit FeeTierAdded(feeTiers.length - 1, minVolatility, maxVolatility, feeMultiplier);
    }

    /**
     * @notice Update an existing fee tier
     */
    function updateFeeTier(
        uint256 tierId,
        uint256 minVolatility,
        uint256 maxVolatility,
        uint256 feeMultiplier,
        uint256 minLiquidity
    ) external onlyRole(GOVERNOR_ROLE) {
        if (tierId >= feeTiers.length) revert InvalidTier();
        
        feeTiers[tierId] = FeeTier({
            minVolatility: minVolatility,
            maxVolatility: maxVolatility,
            feeMultiplier: feeMultiplier,
            minLiquidity: minLiquidity
        });
    }

    // ============ View Functions ============
    
    /**
     * @notice Get current volatility for a pool
     */
    function getVolatility(address pool) external view returns (uint256) {
        return volatilityData[pool].ewmaVolatility;
    }

    /**
     * @notice Get 24h volume for a pool
     */
    function getVolume24h(address pool) external view returns (uint256) {
        return volumeData[pool].volume24h;
    }

    /**
     * @notice Get user discount info
     */
    function getUserDiscountInfo(
        address pool,
        address user
    ) external view returns (
        uint256 cumulativeVolume,
        uint256 discountTier,
        uint256 loyaltyPoints,
        uint256 effectiveDiscount
    ) {
        UserDiscount storage discount = userDiscounts[pool][user];
        cumulativeVolume = discount.cumulativeVolume;
        loyaltyPoints = discount.loyaltyPoints;
        
        // Calculate discount tier
        for (uint256 i = volumeDiscountThresholds.length; i > 0; i--) {
            if (cumulativeVolume >= volumeDiscountThresholds[i - 1]) {
                discountTier = i;
                effectiveDiscount = volumeDiscountRates[i - 1];
                break;
            }
        }
        
        // Add loyalty bonus
        effectiveDiscount += loyaltyPoints / 100;
        if (effectiveDiscount > 2000) effectiveDiscount = 2000;
    }

    /**
     * @notice Calculate Time-Weighted Average Fee
     */
    function getTWAF(
        address pool,
        uint256 period
    ) external view returns (uint256) {
        TWAFObservation[] storage observations = twafObservations[pool];
        if (observations.length == 0) return poolConfigs[pool].baseFee;
        
        uint256 targetTime = block.timestamp - period;
        
        // Find observation at or before target time
        TWAFObservation storage startObs = observations[0];
        for (uint256 i = observations.length; i > 0; i--) {
            if (observations[i - 1].timestamp <= targetTime) {
                startObs = observations[i - 1];
                break;
            }
        }
        
        TWAFObservation storage endObs = observations[observations.length - 1];
        
        uint256 timeElapsed = endObs.timestamp - startObs.timestamp;
        if (timeElapsed == 0) return endObs.fee;
        
        uint256 cumulativeDiff = endObs.cumulativeFee - startObs.cumulativeFee;
        return cumulativeDiff / timeElapsed;
    }

    /**
     * @notice Get all fee tiers
     */
    function getFeeTiers() external view returns (FeeTier[] memory) {
        return feeTiers;
    }

    /**
     * @notice Get pool fee breakdown
     */
    function getFeeBreakdown(
        address pool,
        address user,
        uint256 tradeAmount
    ) external view returns (
        uint256 baseFee,
        uint256 volatilityAdjustedFee,
        uint256 volumeDiscountedFee,
        uint256 finalFee
    ) {
        PoolFeeConfig storage config = poolConfigs[pool];
        if (!config.isActive) revert PoolNotRegistered();
        
        baseFee = config.baseFee;
        volatilityAdjustedFee = _applyVolatilityAdjustment(pool, baseFee, config);
        volumeDiscountedFee = _applyVolumeDiscount(pool, user, volatilityAdjustedFee, tradeAmount);
        finalFee = (volumeDiscountedFee * globalFeeMultiplier) / 100;
        
        if (finalFee < config.minFee) finalFee = config.minFee;
        if (finalFee > config.maxFee) finalFee = config.maxFee;
    }

    // ============ Admin Functions ============
    
    /**
     * @notice Set global fee multiplier
     */
    function setGlobalMultiplier(uint256 multiplier) external onlyRole(GOVERNOR_ROLE) {
        require(multiplier >= 50 && multiplier <= 300, "Invalid multiplier");
        uint256 old = globalFeeMultiplier;
        globalFeeMultiplier = multiplier;
        emit GlobalMultiplierUpdated(old, multiplier);
    }

    /**
     * @notice Toggle dynamic fees
     */
    function setDynamicFeesEnabled(bool enabled) external onlyRole(GOVERNOR_ROLE) {
        dynamicFeesEnabled = enabled;
    }

    /**
     * @notice Update volume discount thresholds
     */
    function updateVolumeDiscounts(
        uint256[] calldata thresholds,
        uint256[] calldata rates
    ) external onlyRole(GOVERNOR_ROLE) {
        require(thresholds.length == rates.length, "Length mismatch");
        delete volumeDiscountThresholds;
        delete volumeDiscountRates;
        
        for (uint256 i = 0; i < thresholds.length; i++) {
            volumeDiscountThresholds.push(thresholds[i]);
            volumeDiscountRates.push(rates[i]);
        }
    }

    /**
     * @notice Deactivate a pool
     */
    function deactivatePool(address pool) external onlyRole(GOVERNOR_ROLE) {
        poolConfigs[pool].isActive = false;
    }

    /**
     * @notice Get all registered pools
     */
    function getRegisteredPools() external view returns (address[] memory) {
        return registeredPools;
    }
}
