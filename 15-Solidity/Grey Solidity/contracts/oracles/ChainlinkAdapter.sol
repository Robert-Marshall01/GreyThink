// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts-upgradeable/access/AccessControlUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/PausableUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/Initializable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/UUPSUpgradeable.sol";

/**
 * @notice Chainlink Aggregator interface
 */
interface AggregatorV3Interface {
    function decimals() external view returns (uint8);
    function description() external view returns (string memory);
    function version() external view returns (uint256);
    function getRoundData(uint80 _roundId)
        external
        view
        returns (
            uint80 roundId,
            int256 answer,
            uint256 startedAt,
            uint256 updatedAt,
            uint80 answeredInRound
        );
    function latestRoundData()
        external
        view
        returns (
            uint80 roundId,
            int256 answer,
            uint256 startedAt,
            uint256 updatedAt,
            uint80 answeredInRound
        );
}

/**
 * @title ChainlinkAdapter
 * @author Grey Protocol Team
 * @notice Adapter for integrating Chainlink price feeds
 * @dev Wraps Chainlink aggregator interface for standardized price access
 * 
 * Features:
 * - Multiple feed support
 * - Staleness detection
 * - Price deviation bounds
 * - Fallback feed support
 * - Round ID tracking
 * - Historical price queries
 * - Heartbeat monitoring
 */
contract ChainlinkAdapter is
    Initializable,
    AccessControlUpgradeable,
    PausableUpgradeable,
    UUPSUpgradeable
{
    // ============================================
    // CONSTANTS & ROLES
    // ============================================

    bytes32 public constant OPERATOR_ROLE = keccak256("OPERATOR_ROLE");
    bytes32 public constant UPGRADER_ROLE = keccak256("UPGRADER_ROLE");

    /// @notice Maximum staleness for prices
    uint256 public constant MAX_STALENESS = 1 hours;
    
    /// @notice Maximum deviation for price sanity (50%)
    uint256 public constant MAX_DEVIATION = 5000;
    
    /// @notice Price precision
    uint256 public constant PRICE_PRECISION = 1e18;

    // ============================================
    // STRUCTS
    // ============================================

    /**
     * @notice Feed configuration
     * @param aggregator Chainlink aggregator address
     * @param fallbackAggregator Fallback aggregator
     * @param isActive Whether feed is active
     * @param decimals Feed decimals
     * @param heartbeat Maximum seconds between updates
     * @param lowerBound Minimum acceptable price
     * @param upperBound Maximum acceptable price
     * @param description Feed description
     * @param lastPrice Last retrieved price
     * @param lastUpdate Last update timestamp
     * @param lastRoundId Last round ID
     */
    struct FeedConfig {
        address aggregator;
        address fallbackAggregator;
        bool isActive;
        uint8 decimals;
        uint256 heartbeat;
        uint256 lowerBound;
        uint256 upperBound;
        string description;
        uint256 lastPrice;
        uint256 lastUpdate;
        uint80 lastRoundId;
    }

    /**
     * @notice Price data result
     */
    struct PriceData {
        uint256 price;
        uint256 timestamp;
        uint80 roundId;
        uint8 decimals;
        bool isValid;
        bool usedFallback;
    }

    /**
     * @notice Round data
     */
    struct RoundData {
        uint80 roundId;
        int256 answer;
        uint256 startedAt;
        uint256 updatedAt;
        uint80 answeredInRound;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Feed configurations by asset
    mapping(address => FeedConfig) public feeds;

    /// @notice All configured assets
    address[] public assets;

    /// @notice Asset pairs for derived prices
    mapping(address => mapping(address => address[])) public pricePaths;

    /// @notice Base currency (e.g., USD quote)
    address public baseCurrency;

    /// @notice Number of active feeds
    uint256 public activeFeedCount;

    /// @notice Storage gap
    uint256[40] private __gap;

    // ============================================
    // EVENTS
    // ============================================

    event FeedConfigured(
        address indexed asset,
        address aggregator,
        address fallbackAggregator,
        uint8 decimals
    );

    event FeedUpdated(address indexed asset, bool isActive);
    event PriceRetrieved(address indexed asset, uint256 price, uint256 timestamp, uint80 roundId);
    event FallbackUsed(address indexed asset, address fallbackAggregator);
    event PriceBoundsUpdated(address indexed asset, uint256 lowerBound, uint256 upperBound);

    // ============================================
    // ERRORS
    // ============================================

    error FeedNotFound(address asset);
    error FeedNotActive(address asset);
    error StalePrice(uint256 lastUpdate, uint256 maxAge);
    error InvalidPrice(int256 price);
    error PriceOutOfBounds(uint256 price, uint256 lower, uint256 upper);
    error FallbackFailed(address asset);
    error InvalidRound(uint80 roundId);
    error ZeroAddress();
    error FeedExists(address asset);
    error InvalidBounds();
    error NoPath(address from, address to);

    // ============================================
    // INITIALIZER
    // ============================================

    /**
     * @notice Initializes the adapter
     * @param admin Admin address
     * @param _baseCurrency Base currency address (e.g., USD = address(840))
     */
    function initialize(
        address admin,
        address _baseCurrency
    ) public initializer {
        if (admin == address(0)) revert ZeroAddress();

        __AccessControl_init();
        __Pausable_init();
        __UUPSUpgradeable_init();

        baseCurrency = _baseCurrency;

        _grantRole(DEFAULT_ADMIN_ROLE, admin);
        _grantRole(OPERATOR_ROLE, admin);
        _grantRole(UPGRADER_ROLE, admin);
    }

    // ============================================
    // FEED CONFIGURATION
    // ============================================

    /**
     * @notice Configures a Chainlink price feed
     * @param asset Asset address
     * @param aggregator Chainlink aggregator address
     * @param fallbackAggregator Optional fallback aggregator
     * @param heartbeat Maximum seconds between updates
     * @param lowerBound Minimum acceptable price
     * @param upperBound Maximum acceptable price
     */
    function configureFeed(
        address asset,
        address aggregator,
        address fallbackAggregator,
        uint256 heartbeat,
        uint256 lowerBound,
        uint256 upperBound
    ) external onlyRole(OPERATOR_ROLE) {
        if (asset == address(0)) revert ZeroAddress();
        if (aggregator == address(0)) revert ZeroAddress();
        if (upperBound > 0 && lowerBound >= upperBound) revert InvalidBounds();

        bool isNew = feeds[asset].aggregator == address(0);

        // Get feed metadata
        AggregatorV3Interface agg = AggregatorV3Interface(aggregator);
        uint8 decimals = agg.decimals();
        string memory desc = agg.description();

        feeds[asset] = FeedConfig({
            aggregator: aggregator,
            fallbackAggregator: fallbackAggregator,
            isActive: true,
            decimals: decimals,
            heartbeat: heartbeat > 0 ? heartbeat : MAX_STALENESS,
            lowerBound: lowerBound,
            upperBound: upperBound,
            description: desc,
            lastPrice: 0,
            lastUpdate: 0,
            lastRoundId: 0
        });

        if (isNew) {
            assets.push(asset);
            activeFeedCount++;
        }

        emit FeedConfigured(asset, aggregator, fallbackAggregator, decimals);
    }

    /**
     * @notice Updates feed bounds
     */
    function setPriceBounds(
        address asset,
        uint256 lowerBound,
        uint256 upperBound
    ) external onlyRole(OPERATOR_ROLE) {
        if (feeds[asset].aggregator == address(0)) revert FeedNotFound(asset);
        if (upperBound > 0 && lowerBound >= upperBound) revert InvalidBounds();

        feeds[asset].lowerBound = lowerBound;
        feeds[asset].upperBound = upperBound;

        emit PriceBoundsUpdated(asset, lowerBound, upperBound);
    }

    /**
     * @notice Sets feed active status
     */
    function setFeedActive(address asset, bool isActive) external onlyRole(OPERATOR_ROLE) {
        if (feeds[asset].aggregator == address(0)) revert FeedNotFound(asset);
        
        if (feeds[asset].isActive && !isActive) {
            activeFeedCount--;
        } else if (!feeds[asset].isActive && isActive) {
            activeFeedCount++;
        }
        
        feeds[asset].isActive = isActive;
        emit FeedUpdated(asset, isActive);
    }

    /**
     * @notice Configures a price path for derived prices
     */
    function setPricePath(
        address from,
        address to,
        address[] calldata path
    ) external onlyRole(OPERATOR_ROLE) {
        pricePaths[from][to] = path;
    }

    // ============================================
    // PRICE RETRIEVAL
    // ============================================

    /**
     * @notice Gets the latest price for an asset
     * @param asset Asset address
     * @return priceData Complete price data
     */
    function getLatestPrice(address asset) public view returns (PriceData memory priceData) {
        FeedConfig storage config = feeds[asset];
        if (config.aggregator == address(0)) revert FeedNotFound(asset);
        if (!config.isActive) revert FeedNotActive(asset);

        // Try primary aggregator
        try AggregatorV3Interface(config.aggregator).latestRoundData() returns (
            uint80 roundId,
            int256 answer,
            uint256 /* startedAt */,
            uint256 updatedAt,
            uint80 answeredInRound
        ) {
            // Validate
            if (answer <= 0) revert InvalidPrice(answer);
            if (answeredInRound < roundId) revert InvalidRound(roundId);
            if (block.timestamp - updatedAt > config.heartbeat) {
                // Try fallback
                if (config.fallbackAggregator != address(0)) {
                    return _getFallbackPrice(asset, config);
                }
                revert StalePrice(updatedAt, config.heartbeat);
            }

            uint256 normalizedPrice = _normalizePrice(uint256(answer), config.decimals);
            
            // Check bounds
            if (config.lowerBound > 0 && normalizedPrice < config.lowerBound) {
                revert PriceOutOfBounds(normalizedPrice, config.lowerBound, config.upperBound);
            }
            if (config.upperBound > 0 && normalizedPrice > config.upperBound) {
                revert PriceOutOfBounds(normalizedPrice, config.lowerBound, config.upperBound);
            }

            priceData = PriceData({
                price: normalizedPrice,
                timestamp: updatedAt,
                roundId: roundId,
                decimals: config.decimals,
                isValid: true,
                usedFallback: false
            });

        } catch {
            // Try fallback
            if (config.fallbackAggregator != address(0)) {
                return _getFallbackPrice(asset, config);
            }
            revert FallbackFailed(asset);
        }
    }

    /**
     * @notice Gets price from fallback aggregator
     */
    function _getFallbackPrice(
        address asset,
        FeedConfig storage config
    ) internal view returns (PriceData memory priceData) {
        try AggregatorV3Interface(config.fallbackAggregator).latestRoundData() returns (
            uint80 roundId,
            int256 answer,
            uint256 /* startedAt */,
            uint256 updatedAt,
            uint80 answeredInRound
        ) {
            if (answer <= 0) revert InvalidPrice(answer);
            if (answeredInRound < roundId) revert InvalidRound(roundId);
            if (block.timestamp - updatedAt > config.heartbeat) {
                revert StalePrice(updatedAt, config.heartbeat);
            }

            uint8 fallbackDecimals = AggregatorV3Interface(config.fallbackAggregator).decimals();
            uint256 normalizedPrice = _normalizePrice(uint256(answer), fallbackDecimals);

            priceData = PriceData({
                price: normalizedPrice,
                timestamp: updatedAt,
                roundId: roundId,
                decimals: fallbackDecimals,
                isValid: true,
                usedFallback: true
            });

        } catch {
            revert FallbackFailed(asset);
        }
    }

    /**
     * @notice Gets historical price at a specific round
     * @param asset Asset address
     * @param roundId Round ID to query
     */
    function getHistoricalPrice(
        address asset,
        uint80 roundId
    ) external view returns (RoundData memory roundData) {
        FeedConfig storage config = feeds[asset];
        if (config.aggregator == address(0)) revert FeedNotFound(asset);

        (
            uint80 id,
            int256 answer,
            uint256 startedAt,
            uint256 updatedAt,
            uint80 answeredInRound
        ) = AggregatorV3Interface(config.aggregator).getRoundData(roundId);

        roundData = RoundData({
            roundId: id,
            answer: answer,
            startedAt: startedAt,
            updatedAt: updatedAt,
            answeredInRound: answeredInRound
        });
    }

    /**
     * @notice Gets derived price between two assets via path
     * @param from Source asset
     * @param to Destination asset
     * @return price Derived price normalized to PRICE_PRECISION
     */
    function getDerivedPrice(
        address from,
        address to
    ) external view returns (uint256 price) {
        address[] storage path = pricePaths[from][to];
        if (path.length == 0) revert NoPath(from, to);

        price = PRICE_PRECISION;

        for (uint256 i = 0; i < path.length; i++) {
            PriceData memory priceData = getLatestPrice(path[i]);
            price = (price * priceData.price) / PRICE_PRECISION;
        }
    }

    // ============================================
    // HELPER FUNCTIONS
    // ============================================

    /**
     * @notice Normalizes price to standard precision
     */
    function _normalizePrice(
        uint256 price,
        uint8 decimals
    ) internal pure returns (uint256) {
        if (decimals < 18) {
            return price * (10 ** (18 - decimals));
        } else if (decimals > 18) {
            return price / (10 ** (decimals - 18));
        }
        return price;
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    function getFeed(address asset) external view returns (FeedConfig memory) {
        return feeds[asset];
    }

    function getAllAssets() external view returns (address[] memory) {
        return assets;
    }

    function isStale(address asset) external view returns (bool) {
        FeedConfig storage config = feeds[asset];
        try AggregatorV3Interface(config.aggregator).latestRoundData() returns (
            uint80,
            int256,
            uint256,
            uint256 updatedAt,
            uint80
        ) {
            return block.timestamp - updatedAt > config.heartbeat;
        } catch {
            return true;
        }
    }

    function getDecimals(address asset) external view returns (uint8) {
        return feeds[asset].decimals;
    }

    /**
     * @notice Gets prices for multiple assets
     */
    function getPrices(address[] calldata assetList) external view returns (PriceData[] memory prices) {
        prices = new PriceData[](assetList.length);
        for (uint256 i = 0; i < assetList.length; i++) {
            try this.getLatestPrice(assetList[i]) returns (PriceData memory data) {
                prices[i] = data;
            } catch {
                prices[i] = PriceData({
                    price: 0,
                    timestamp: 0,
                    roundId: 0,
                    decimals: 0,
                    isValid: false,
                    usedFallback: false
                });
            }
        }
    }

    // ============================================
    // ADMIN
    // ============================================

    function pause() external onlyRole(OPERATOR_ROLE) { _pause(); }
    function unpause() external onlyRole(OPERATOR_ROLE) { _unpause(); }
    function _authorizeUpgrade(address) internal override onlyRole(UPGRADER_ROLE) {}
}
