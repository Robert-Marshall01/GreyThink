// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts-upgradeable/access/AccessControlUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/PausableUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/ReentrancyGuardUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/Initializable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/UUPSUpgradeable.sol";
import "@openzeppelin/contracts/utils/math/Math.sol";

/**
 * @title TWAPOracle
 * @author Grey Protocol Team
 * @notice Time-Weighted Average Price (TWAP) Oracle implementation
 * @dev Calculates TWAP over configurable time windows using cumulative price observations
 * 
 * Features:
 * - Configurable observation windows (1 min to 24 hours)
 * - Multiple price feed support
 * - Cumulative price tracking
 * - Stale data detection
 * - Price deviation checks
 * - Heartbeat monitoring
 * - Historical data queries
 * - Multi-pair support
 */
contract TWAPOracle is
    Initializable,
    AccessControlUpgradeable,
    PausableUpgradeable,
    ReentrancyGuardUpgradeable,
    UUPSUpgradeable
{
    using Math for uint256;

    // ============================================
    // CONSTANTS & ROLES
    // ============================================

    bytes32 public constant UPDATER_ROLE = keccak256("UPDATER_ROLE");
    bytes32 public constant OPERATOR_ROLE = keccak256("OPERATOR_ROLE");
    bytes32 public constant UPGRADER_ROLE = keccak256("UPGRADER_ROLE");

    /// @notice Maximum observations per pair
    uint256 public constant MAX_OBSERVATIONS = 8640; // 24 hours at 10-second intervals
    
    /// @notice Minimum observation gap
    uint256 public constant MIN_OBSERVATION_GAP = 10 seconds;
    
    /// @notice Maximum staleness
    uint256 public constant MAX_STALENESS = 1 hours;
    
    /// @notice Price precision
    uint256 public constant PRICE_PRECISION = 1e18;

    // ============================================
    // STRUCTS
    // ============================================

    /**
     * @notice Price observation
     * @param timestamp Observation timestamp
     * @param price Spot price at observation
     * @param cumulativePrice Cumulative price up to this point
     * @param liquidity Available liquidity
     */
    struct Observation {
        uint32 timestamp;
        uint224 price;
        uint256 cumulativePrice;
        uint256 liquidity;
    }

    /**
     * @notice Trading pair configuration
     * @param token0 First token address
     * @param token1 Second token address
     * @param decimals0 Decimals for token0
     * @param decimals1 Decimals for token1
     * @param isActive Whether pair is active
     * @param observationLength Number of stored observations
     * @param observationIndex Current observation index
     * @param lastUpdateTime Last update timestamp
     * @param minLiquidity Minimum required liquidity
     * @param maxDeviation Maximum price deviation (in BPS)
     */
    struct PairConfig {
        address token0;
        address token1;
        uint8 decimals0;
        uint8 decimals1;
        bool isActive;
        uint16 observationLength;
        uint16 observationIndex;
        uint32 lastUpdateTime;
        uint256 minLiquidity;
        uint256 maxDeviation;
    }

    /**
     * @notice TWAP configuration
     * @param shortWindow Short TWAP window (e.g., 5 minutes)
     * @param mediumWindow Medium TWAP window (e.g., 30 minutes)
     * @param longWindow Long TWAP window (e.g., 24 hours)
     * @param useGeometricMean Whether to use geometric mean
     */
    struct TWAPConfig {
        uint32 shortWindow;
        uint32 mediumWindow;
        uint32 longWindow;
        bool useGeometricMean;
    }

    /**
     * @notice TWAP result
     * @param shortTWAP Short window TWAP
     * @param mediumTWAP Medium window TWAP
     * @param longTWAP Long window TWAP
     * @param spotPrice Current spot price
     * @param timestamp Calculation timestamp
     * @param observationsUsed Number of observations used
     */
    struct TWAPResult {
        uint256 shortTWAP;
        uint256 mediumTWAP;
        uint256 longTWAP;
        uint256 spotPrice;
        uint256 timestamp;
        uint256 observationsUsed;
    }

    /**
     * @notice Price range
     */
    struct PriceRange {
        uint256 low;
        uint256 high;
        uint256 average;
        uint256 volatility;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Pair configurations by pair ID
    mapping(bytes32 => PairConfig) public pairs;

    /// @notice Observations by pair ID
    mapping(bytes32 => Observation[]) public observations;

    /// @notice TWAP configuration
    TWAPConfig public twapConfig;

    /// @notice All pair IDs
    bytes32[] public pairIds;

    /// @notice Token to pair mapping
    mapping(address => mapping(address => bytes32)) public tokenPairs;

    /// @notice Heartbeat tracking
    mapping(bytes32 => uint256) public lastHeartbeat;

    /// @notice Total observations recorded
    uint256 public totalObservations;

    /// @notice Storage gap
    uint256[40] private __gap;

    // ============================================
    // EVENTS
    // ============================================

    event ObservationRecorded(
        bytes32 indexed pairId,
        uint256 price,
        uint256 cumulativePrice,
        uint256 liquidity,
        uint256 timestamp
    );

    event PairAdded(
        bytes32 indexed pairId,
        address token0,
        address token1,
        uint16 observationLength
    );

    event PairUpdated(bytes32 indexed pairId, bool isActive);
    event TWAPConfigUpdated(uint32 shortWindow, uint32 mediumWindow, uint32 longWindow);
    event HeartbeatReceived(bytes32 indexed pairId, uint256 timestamp);

    // ============================================
    // ERRORS
    // ============================================

    error PairNotFound(bytes32 pairId);
    error PairNotActive(bytes32 pairId);
    error InsufficientObservations(uint256 available, uint256 required);
    error StalePrice(uint256 lastUpdate, uint256 maxAge);
    error PriceDeviationExceeded(uint256 deviation, uint256 maxDeviation);
    error InsufficientLiquidity(uint256 liquidity, uint256 minimum);
    error InvalidPrice();
    error InvalidWindow();
    error ObservationTooSoon(uint256 timeSinceLastUpdate);
    error ZeroAddress();
    error PairExists();
    error InvalidDecimals();

    // ============================================
    // INITIALIZER
    // ============================================

    /**
     * @notice Initializes the TWAP oracle
     * @param admin Admin address
     */
    function initialize(address admin) public initializer {
        if (admin == address(0)) revert ZeroAddress();

        __AccessControl_init();
        __Pausable_init();
        __ReentrancyGuard_init();
        __UUPSUpgradeable_init();

        // Default TWAP windows
        twapConfig = TWAPConfig({
            shortWindow: 5 minutes,
            mediumWindow: 30 minutes,
            longWindow: 24 hours,
            useGeometricMean: false
        });

        _grantRole(DEFAULT_ADMIN_ROLE, admin);
        _grantRole(UPDATER_ROLE, admin);
        _grantRole(OPERATOR_ROLE, admin);
        _grantRole(UPGRADER_ROLE, admin);
    }

    // ============================================
    // PAIR MANAGEMENT
    // ============================================

    /**
     * @notice Adds a new trading pair
     * @param token0 First token
     * @param token1 Second token
     * @param decimals0 Token0 decimals
     * @param decimals1 Token1 decimals
     * @param observationLength Number of observations to store
     * @param minLiquidity Minimum liquidity requirement
     * @param maxDeviation Maximum price deviation in BPS
     * @return pairId The pair identifier
     */
    function addPair(
        address token0,
        address token1,
        uint8 decimals0,
        uint8 decimals1,
        uint16 observationLength,
        uint256 minLiquidity,
        uint256 maxDeviation
    ) external onlyRole(OPERATOR_ROLE) returns (bytes32 pairId) {
        if (token0 == address(0) || token1 == address(0)) revert ZeroAddress();
        if (decimals0 > 18 || decimals1 > 18) revert InvalidDecimals();
        
        // Sort tokens
        if (token0 > token1) {
            (token0, token1) = (token1, token0);
            (decimals0, decimals1) = (decimals1, decimals0);
        }

        pairId = keccak256(abi.encodePacked(token0, token1));
        
        if (pairs[pairId].token0 != address(0)) revert PairExists();

        uint16 obsLen = observationLength > 0 ? observationLength : uint16(MAX_OBSERVATIONS);
        if (obsLen > MAX_OBSERVATIONS) {
            obsLen = uint16(MAX_OBSERVATIONS);
        }

        pairs[pairId] = PairConfig({
            token0: token0,
            token1: token1,
            decimals0: decimals0,
            decimals1: decimals1,
            isActive: true,
            observationLength: obsLen,
            observationIndex: 0,
            lastUpdateTime: 0,
            minLiquidity: minLiquidity,
            maxDeviation: maxDeviation
        });

        // Initialize observation array
        Observation[] storage obs = observations[pairId];
        for (uint256 i = 0; i < obsLen; i++) {
            obs.push(Observation({
                timestamp: 0,
                price: 0,
                cumulativePrice: 0,
                liquidity: 0
            }));
        }

        pairIds.push(pairId);
        tokenPairs[token0][token1] = pairId;
        tokenPairs[token1][token0] = pairId;

        emit PairAdded(pairId, token0, token1, obsLen);
    }

    /**
     * @notice Updates pair configuration
     */
    function setPairActive(bytes32 pairId, bool isActive) external onlyRole(OPERATOR_ROLE) {
        if (pairs[pairId].token0 == address(0)) revert PairNotFound(pairId);
        pairs[pairId].isActive = isActive;
        emit PairUpdated(pairId, isActive);
    }

    /**
     * @notice Updates pair limits
     */
    function setPairLimits(
        bytes32 pairId,
        uint256 minLiquidity,
        uint256 maxDeviation
    ) external onlyRole(OPERATOR_ROLE) {
        if (pairs[pairId].token0 == address(0)) revert PairNotFound(pairId);
        pairs[pairId].minLiquidity = minLiquidity;
        pairs[pairId].maxDeviation = maxDeviation;
    }

    // ============================================
    // OBSERVATION RECORDING
    // ============================================

    /**
     * @notice Records a price observation
     * @param pairId Pair identifier
     * @param price Current spot price (scaled by PRICE_PRECISION)
     * @param liquidity Available liquidity
     */
    function recordObservation(
        bytes32 pairId,
        uint256 price,
        uint256 liquidity
    ) external onlyRole(UPDATER_ROLE) whenNotPaused {
        PairConfig storage pair = pairs[pairId];
        if (pair.token0 == address(0)) revert PairNotFound(pairId);
        if (!pair.isActive) revert PairNotActive(pairId);
        if (price == 0) revert InvalidPrice();

        // Check observation gap
        uint256 timeSinceLast = block.timestamp - pair.lastUpdateTime;
        if (pair.lastUpdateTime > 0 && timeSinceLast < MIN_OBSERVATION_GAP) {
            revert ObservationTooSoon(timeSinceLast);
        }

        // Check liquidity
        if (liquidity < pair.minLiquidity) {
            revert InsufficientLiquidity(liquidity, pair.minLiquidity);
        }

        // Get last observation for cumulative calculation
        Observation[] storage obs = observations[pairId];
        uint256 lastIndex = pair.observationIndex;
        Observation storage lastObs = obs[lastIndex];

        // Calculate cumulative price
        uint256 cumulativePrice;
        if (lastObs.timestamp > 0) {
            uint256 timeElapsed = block.timestamp - lastObs.timestamp;
            cumulativePrice = lastObs.cumulativePrice + (lastObs.price * timeElapsed);

            // Check price deviation
            if (pair.maxDeviation > 0) {
                uint256 deviation = _calculateDeviation(lastObs.price, price);
                if (deviation > pair.maxDeviation) {
                    revert PriceDeviationExceeded(deviation, pair.maxDeviation);
                }
            }
        } else {
            cumulativePrice = 0;
        }

        // Advance index
        uint256 newIndex = (lastIndex + 1) % pair.observationLength;
        pair.observationIndex = uint16(newIndex);
        pair.lastUpdateTime = uint32(block.timestamp);

        // Store observation
        obs[newIndex] = Observation({
            timestamp: uint32(block.timestamp),
            price: uint224(price),
            cumulativePrice: cumulativePrice,
            liquidity: liquidity
        });

        totalObservations++;
        lastHeartbeat[pairId] = block.timestamp;

        emit ObservationRecorded(pairId, price, cumulativePrice, liquidity, block.timestamp);
        emit HeartbeatReceived(pairId, block.timestamp);
    }

    /**
     * @notice Batch record observations
     */
    function recordObservations(
        bytes32[] calldata pairIdArray,
        uint256[] calldata prices,
        uint256[] calldata liquidities
    ) external onlyRole(UPDATER_ROLE) whenNotPaused {
        require(pairIdArray.length == prices.length && prices.length == liquidities.length, "Length mismatch");

        for (uint256 i = 0; i < pairIdArray.length; i++) {
            try this.recordObservation(pairIdArray[i], prices[i], liquidities[i]) {
                // Success
            } catch {
                // Skip failed observations
            }
        }
    }

    // ============================================
    // TWAP CALCULATION
    // ============================================

    /**
     * @notice Calculates TWAP for a pair
     * @param pairId Pair identifier
     * @return result TWAP result with short, medium, long windows
     */
    function getTWAP(bytes32 pairId) external view returns (TWAPResult memory result) {
        PairConfig storage pair = pairs[pairId];
        if (pair.token0 == address(0)) revert PairNotFound(pairId);
        if (!pair.isActive) revert PairNotActive(pairId);

        // Check staleness
        if (block.timestamp - pair.lastUpdateTime > MAX_STALENESS) {
            revert StalePrice(pair.lastUpdateTime, MAX_STALENESS);
        }

        Observation[] storage obs = observations[pairId];
        uint256 currentIndex = pair.observationIndex;

        // Get current spot price
        result.spotPrice = obs[currentIndex].price;
        result.timestamp = block.timestamp;

        // Calculate TWAPs for each window
        (result.shortTWAP, ) = _calculateTWAPForWindow(obs, pair, twapConfig.shortWindow);
        (result.mediumTWAP, ) = _calculateTWAPForWindow(obs, pair, twapConfig.mediumWindow);
        (result.longTWAP, result.observationsUsed) = _calculateTWAPForWindow(obs, pair, twapConfig.longWindow);
    }

    /**
     * @notice Calculates TWAP for a specific time window
     * @param pairId Pair identifier
     * @param window Time window in seconds
     * @return twap Time-weighted average price
     */
    function getTWAPForWindow(
        bytes32 pairId,
        uint32 window
    ) external view returns (uint256 twap) {
        PairConfig storage pair = pairs[pairId];
        if (pair.token0 == address(0)) revert PairNotFound(pairId);
        if (window == 0) revert InvalidWindow();

        Observation[] storage obs = observations[pairId];
        (twap, ) = _calculateTWAPForWindow(obs, pair, window);
    }

    /**
     * @notice Internal TWAP calculation
     */
    function _calculateTWAPForWindow(
        Observation[] storage obs,
        PairConfig storage pair,
        uint32 window
    ) internal view returns (uint256 twap, uint256 observationsUsed) {
        if (pair.lastUpdateTime == 0) {
            return (0, 0);
        }

        uint256 targetTime = block.timestamp - window;
        uint256 currentIndex = pair.observationIndex;
        
        Observation storage newestObs = obs[currentIndex];
        
        // Find the oldest observation within the window
        uint256 oldestValidIndex = currentIndex;
        uint256 count = 0;

        for (uint256 i = 0; i < pair.observationLength; i++) {
            uint256 checkIndex = (currentIndex + pair.observationLength - i) % pair.observationLength;
            Observation storage checkObs = obs[checkIndex];
            
            if (checkObs.timestamp == 0) break;
            if (checkObs.timestamp < targetTime) break;
            
            oldestValidIndex = checkIndex;
            count++;
        }

        if (count < 2) {
            // Not enough observations, return spot price
            return (newestObs.price, count);
        }

        Observation storage oldestObs = obs[oldestValidIndex];

        // Calculate TWAP using cumulative prices
        uint256 timeElapsed = newestObs.timestamp - oldestObs.timestamp;
        if (timeElapsed == 0) {
            return (newestObs.price, count);
        }

        // Include time from last observation to now
        uint256 totalCumulativeChange = newestObs.cumulativePrice - oldestObs.cumulativePrice;
        totalCumulativeChange += newestObs.price * (block.timestamp - newestObs.timestamp);

        uint256 totalTimeElapsed = block.timestamp - oldestObs.timestamp;
        twap = totalCumulativeChange / totalTimeElapsed;
        observationsUsed = count;
    }

    /**
     * @notice Gets price range for a pair
     */
    function getPriceRange(
        bytes32 pairId,
        uint32 window
    ) external view returns (PriceRange memory range) {
        PairConfig storage pair = pairs[pairId];
        if (pair.token0 == address(0)) revert PairNotFound(pairId);

        Observation[] storage obs = observations[pairId];
        uint256 targetTime = block.timestamp - window;
        uint256 currentIndex = pair.observationIndex;

        uint256 minPrice = type(uint256).max;
        uint256 maxPrice = 0;
        uint256 sumPrice = 0;
        uint256 count = 0;

        for (uint256 i = 0; i < pair.observationLength; i++) {
            uint256 checkIndex = (currentIndex + pair.observationLength - i) % pair.observationLength;
            Observation storage checkObs = obs[checkIndex];
            
            if (checkObs.timestamp == 0) break;
            if (checkObs.timestamp < targetTime) break;
            
            uint256 price = checkObs.price;
            if (price < minPrice) minPrice = price;
            if (price > maxPrice) maxPrice = price;
            sumPrice += price;
            count++;
        }

        if (count > 0) {
            range.low = minPrice;
            range.high = maxPrice;
            range.average = sumPrice / count;
            // Simplified volatility: (high - low) / average * 10000 (basis points)
            if (range.average > 0) {
                range.volatility = ((maxPrice - minPrice) * 10000) / range.average;
            }
        }
    }

    // ============================================
    // HELPER FUNCTIONS
    // ============================================

    /**
     * @notice Calculates price deviation
     */
    function _calculateDeviation(uint256 oldPrice, uint256 newPrice) internal pure returns (uint256) {
        if (oldPrice == 0) return 0;
        
        uint256 diff = oldPrice > newPrice ? oldPrice - newPrice : newPrice - oldPrice;
        return (diff * 10000) / oldPrice; // Returns deviation in basis points
    }

    // ============================================
    // CONFIGURATION
    // ============================================

    /**
     * @notice Updates TWAP windows
     */
    function setTWAPConfig(
        uint32 shortWindow,
        uint32 mediumWindow,
        uint32 longWindow,
        bool useGeometricMean
    ) external onlyRole(OPERATOR_ROLE) {
        if (shortWindow == 0 || mediumWindow == 0 || longWindow == 0) revert InvalidWindow();
        
        twapConfig = TWAPConfig({
            shortWindow: shortWindow,
            mediumWindow: mediumWindow,
            longWindow: longWindow,
            useGeometricMean: useGeometricMean
        });

        emit TWAPConfigUpdated(shortWindow, mediumWindow, longWindow);
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    function getPairId(address token0, address token1) external view returns (bytes32) {
        return tokenPairs[token0][token1];
    }

    function getPairConfig(bytes32 pairId) external view returns (PairConfig memory) {
        return pairs[pairId];
    }

    function getAllPairs() external view returns (bytes32[] memory) {
        return pairIds;
    }

    function getLatestObservation(bytes32 pairId) external view returns (Observation memory) {
        PairConfig storage pair = pairs[pairId];
        return observations[pairId][pair.observationIndex];
    }

    function getObservations(
        bytes32 pairId,
        uint256 count
    ) external view returns (Observation[] memory result) {
        PairConfig storage pair = pairs[pairId];
        Observation[] storage obs = observations[pairId];
        
        uint256 len = count > pair.observationLength ? pair.observationLength : count;
        result = new Observation[](len);
        
        uint256 currentIndex = pair.observationIndex;
        for (uint256 i = 0; i < len; i++) {
            uint256 idx = (currentIndex + pair.observationLength - i) % pair.observationLength;
            result[i] = obs[idx];
        }
    }

    function isStale(bytes32 pairId) external view returns (bool) {
        return block.timestamp - pairs[pairId].lastUpdateTime > MAX_STALENESS;
    }

    function getSpotPrice(bytes32 pairId) external view returns (uint256) {
        PairConfig storage pair = pairs[pairId];
        return observations[pairId][pair.observationIndex].price;
    }

    // ============================================
    // ADMIN
    // ============================================

    function pause() external onlyRole(OPERATOR_ROLE) { _pause(); }
    function unpause() external onlyRole(OPERATOR_ROLE) { _unpause(); }
    function _authorizeUpgrade(address) internal override onlyRole(UPGRADER_ROLE) {}
}
