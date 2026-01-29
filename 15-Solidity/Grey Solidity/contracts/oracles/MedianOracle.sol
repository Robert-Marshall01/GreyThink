// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts-upgradeable/access/AccessControlUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/PausableUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/ReentrancyGuardUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/Initializable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/UUPSUpgradeable.sol";
import "@openzeppelin/contracts/utils/math/Math.sol";

/**
 * @title MedianOracle
 * @author Grey Protocol Team
 * @notice Multi-source median oracle with outlier detection
 * @dev Aggregates prices from multiple data sources and returns the median
 * 
 * Features:
 * - Multiple data sources per price feed
 * - Median calculation for outlier resistance
 * - Source weighting and reputation
 * - Staleness detection per source
 * - Price deviation bounds
 * - Heartbeat monitoring
 * - Historical medians storage
 * - Consensus mechanism
 */
contract MedianOracle is
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

    bytes32 public constant REPORTER_ROLE = keccak256("REPORTER_ROLE");
    bytes32 public constant OPERATOR_ROLE = keccak256("OPERATOR_ROLE");
    bytes32 public constant UPGRADER_ROLE = keccak256("UPGRADER_ROLE");

    /// @notice Maximum sources per feed
    uint256 public constant MAX_SOURCES = 20;
    
    /// @notice Minimum sources for valid median
    uint256 public constant MIN_SOURCES = 3;
    
    /// @notice Maximum staleness
    uint256 public constant MAX_STALENESS = 1 hours;
    
    /// @notice Price precision
    uint256 public constant PRICE_PRECISION = 1e18;
    
    /// @notice Maximum history entries
    uint256 public constant MAX_HISTORY = 100;

    // ============================================
    // STRUCTS
    // ============================================

    /**
     * @notice Data source configuration
     * @param source Source address
     * @param isActive Whether source is active
     * @param weight Source weight (100 = normal, >100 = higher weight)
     * @param reputation Reputation score (0-1000)
     * @param lastReport Last report timestamp
     * @param lastPrice Last reported price
     * @param totalReports Total reports submitted
     * @param accurateReports Reports within accuracy threshold
     */
    struct DataSource {
        address source;
        bool isActive;
        uint256 weight;
        uint256 reputation;
        uint256 lastReport;
        uint256 lastPrice;
        uint256 totalReports;
        uint256 accurateReports;
    }

    /**
     * @notice Price feed configuration
     * @param feedId Feed identifier
     * @param description Feed description
     * @param isActive Whether feed is active
     * @param decimals Price decimals
     * @param heartbeat Maximum time between updates
     * @param deviationThreshold Max deviation from median (BPS)
     * @param minSources Minimum sources for valid price
     * @param sourceCount Number of active sources
     * @param lastMedian Last calculated median
     * @param lastUpdate Last median update time
     */
    struct PriceFeed {
        bytes32 feedId;
        string description;
        bool isActive;
        uint8 decimals;
        uint256 heartbeat;
        uint256 deviationThreshold;
        uint256 minSources;
        uint256 sourceCount;
        uint256 lastMedian;
        uint256 lastUpdate;
    }

    /**
     * @notice Price report from a source
     */
    struct PriceReport {
        address source;
        uint256 price;
        uint256 timestamp;
        uint256 confidence;
    }

    /**
     * @notice Median calculation result
     */
    struct MedianResult {
        uint256 median;
        uint256 mean;
        uint256 standardDeviation;
        uint256 validSources;
        uint256 timestamp;
        bool isValid;
    }

    /**
     * @notice Historical median entry
     */
    struct HistoryEntry {
        uint256 median;
        uint256 validSources;
        uint256 timestamp;
    }

    /**
     * @notice Aggregation statistics
     */
    struct AggregationStats {
        uint256 totalReports;
        uint256 validReports;
        uint256 outlierCount;
        uint256 avgDeviation;
        uint256 maxDeviation;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Price feeds by ID
    mapping(bytes32 => PriceFeed) public feeds;

    /// @notice All feed IDs
    bytes32[] public feedIds;

    /// @notice Data sources by feed and source address
    mapping(bytes32 => mapping(address => DataSource)) public sources;

    /// @notice Source addresses per feed
    mapping(bytes32 => address[]) public feedSources;

    /// @notice Latest reports per feed per source
    mapping(bytes32 => mapping(address => PriceReport)) public latestReports;

    /// @notice Median history per feed
    mapping(bytes32 => HistoryEntry[]) public medianHistory;

    /// @notice Global source registry
    mapping(address => bool) public registeredSources;

    /// @notice Total feeds
    uint256 public totalFeeds;

    /// @notice Total active sources
    uint256 public totalActiveSources;

    /// @notice Storage gap
    uint256[40] private __gap;

    // ============================================
    // EVENTS
    // ============================================

    event PriceReported(
        bytes32 indexed feedId,
        address indexed source,
        uint256 price,
        uint256 confidence,
        uint256 timestamp
    );

    event MedianUpdated(
        bytes32 indexed feedId,
        uint256 median,
        uint256 validSources,
        uint256 timestamp
    );

    event SourceAdded(
        bytes32 indexed feedId,
        address indexed source,
        uint256 weight
    );

    event SourceRemoved(bytes32 indexed feedId, address indexed source);
    event SourceReputationUpdated(bytes32 indexed feedId, address indexed source, uint256 reputation);
    event FeedCreated(bytes32 indexed feedId, string description, uint8 decimals);
    event FeedUpdated(bytes32 indexed feedId, bool isActive);
    event OutlierDetected(bytes32 indexed feedId, address indexed source, uint256 price, uint256 median);

    // ============================================
    // ERRORS
    // ============================================

    error FeedNotFound(bytes32 feedId);
    error FeedNotActive(bytes32 feedId);
    error SourceNotFound(address source);
    error SourceNotActive(address source);
    error TooManySources(uint256 count, uint256 max);
    error NotEnoughSources(uint256 count, uint256 min);
    error StalePrice(uint256 lastUpdate, uint256 maxAge);
    error PriceDeviationExceeded(uint256 deviation, uint256 threshold);
    error InvalidPrice();
    error InvalidConfidence();
    error ReportTooSoon(uint256 elapsed, uint256 minimum);
    error ZeroAddress();
    error FeedExists(bytes32 feedId);
    error SourceExists(address source);

    // ============================================
    // INITIALIZER
    // ============================================

    /**
     * @notice Initializes the median oracle
     * @param admin Admin address
     */
    function initialize(address admin) public initializer {
        if (admin == address(0)) revert ZeroAddress();

        __AccessControl_init();
        __Pausable_init();
        __ReentrancyGuard_init();
        __UUPSUpgradeable_init();

        _grantRole(DEFAULT_ADMIN_ROLE, admin);
        _grantRole(OPERATOR_ROLE, admin);
        _grantRole(UPGRADER_ROLE, admin);
    }

    // ============================================
    // FEED MANAGEMENT
    // ============================================

    /**
     * @notice Creates a new price feed
     * @param feedId Unique feed identifier
     * @param description Feed description
     * @param decimals Price decimals
     * @param heartbeat Maximum seconds between updates
     * @param deviationThreshold Max deviation in BPS
     * @param minSources Minimum sources required
     */
    function createFeed(
        bytes32 feedId,
        string calldata description,
        uint8 decimals,
        uint256 heartbeat,
        uint256 deviationThreshold,
        uint256 minSources
    ) external onlyRole(OPERATOR_ROLE) {
        if (feeds[feedId].feedId != bytes32(0)) revert FeedExists(feedId);
        if (minSources < MIN_SOURCES) minSources = MIN_SOURCES;

        feeds[feedId] = PriceFeed({
            feedId: feedId,
            description: description,
            isActive: true,
            decimals: decimals,
            heartbeat: heartbeat,
            deviationThreshold: deviationThreshold,
            minSources: minSources,
            sourceCount: 0,
            lastMedian: 0,
            lastUpdate: 0
        });

        feedIds.push(feedId);
        totalFeeds++;

        emit FeedCreated(feedId, description, decimals);
    }

    /**
     * @notice Updates feed status
     */
    function setFeedActive(bytes32 feedId, bool isActive) external onlyRole(OPERATOR_ROLE) {
        if (feeds[feedId].feedId == bytes32(0)) revert FeedNotFound(feedId);
        feeds[feedId].isActive = isActive;
        emit FeedUpdated(feedId, isActive);
    }

    /**
     * @notice Updates feed parameters
     */
    function updateFeed(
        bytes32 feedId,
        uint256 heartbeat,
        uint256 deviationThreshold,
        uint256 minSources
    ) external onlyRole(OPERATOR_ROLE) {
        PriceFeed storage feed = feeds[feedId];
        if (feed.feedId == bytes32(0)) revert FeedNotFound(feedId);

        feed.heartbeat = heartbeat;
        feed.deviationThreshold = deviationThreshold;
        feed.minSources = minSources;
    }

    // ============================================
    // SOURCE MANAGEMENT
    // ============================================

    /**
     * @notice Adds a data source to a feed
     * @param feedId Feed identifier
     * @param source Source address
     * @param weight Source weight
     */
    function addSource(
        bytes32 feedId,
        address source,
        uint256 weight
    ) external onlyRole(OPERATOR_ROLE) {
        if (feeds[feedId].feedId == bytes32(0)) revert FeedNotFound(feedId);
        if (source == address(0)) revert ZeroAddress();
        if (sources[feedId][source].source != address(0)) revert SourceExists(source);
        if (feedSources[feedId].length >= MAX_SOURCES) {
            revert TooManySources(feedSources[feedId].length, MAX_SOURCES);
        }

        sources[feedId][source] = DataSource({
            source: source,
            isActive: true,
            weight: weight > 0 ? weight : 100,
            reputation: 500, // Start at 50%
            lastReport: 0,
            lastPrice: 0,
            totalReports: 0,
            accurateReports: 0
        });

        feedSources[feedId].push(source);
        feeds[feedId].sourceCount++;
        
        if (!registeredSources[source]) {
            registeredSources[source] = true;
            totalActiveSources++;
        }

        _grantRole(REPORTER_ROLE, source);

        emit SourceAdded(feedId, source, weight);
    }

    /**
     * @notice Removes a data source
     */
    function removeSource(bytes32 feedId, address source) external onlyRole(OPERATOR_ROLE) {
        if (sources[feedId][source].source == address(0)) revert SourceNotFound(source);

        sources[feedId][source].isActive = false;
        feeds[feedId].sourceCount--;

        emit SourceRemoved(feedId, source);
    }

    /**
     * @notice Updates source weight
     */
    function setSourceWeight(
        bytes32 feedId,
        address source,
        uint256 weight
    ) external onlyRole(OPERATOR_ROLE) {
        if (sources[feedId][source].source == address(0)) revert SourceNotFound(source);
        sources[feedId][source].weight = weight;
    }

    // ============================================
    // PRICE REPORTING
    // ============================================

    /**
     * @notice Reports a price from a data source
     * @param feedId Feed identifier
     * @param price Price value
     * @param confidence Confidence level (0-100)
     */
    function reportPrice(
        bytes32 feedId,
        uint256 price,
        uint256 confidence
    ) external onlyRole(REPORTER_ROLE) whenNotPaused {
        PriceFeed storage feed = feeds[feedId];
        if (feed.feedId == bytes32(0)) revert FeedNotFound(feedId);
        if (!feed.isActive) revert FeedNotActive(feedId);

        DataSource storage source = sources[feedId][msg.sender];
        if (source.source == address(0)) revert SourceNotFound(msg.sender);
        if (!source.isActive) revert SourceNotActive(msg.sender);

        if (price == 0) revert InvalidPrice();
        if (confidence > 100) revert InvalidConfidence();

        // Store report
        latestReports[feedId][msg.sender] = PriceReport({
            source: msg.sender,
            price: price,
            timestamp: block.timestamp,
            confidence: confidence
        });

        source.lastReport = block.timestamp;
        source.lastPrice = price;
        source.totalReports++;

        emit PriceReported(feedId, msg.sender, price, confidence, block.timestamp);

        // Update median if enough sources have reported
        _tryUpdateMedian(feedId);
    }

    /**
     * @notice Batch report prices
     */
    function batchReportPrices(
        bytes32[] calldata feedIdArray,
        uint256[] calldata prices,
        uint256[] calldata confidences
    ) external onlyRole(REPORTER_ROLE) whenNotPaused {
        require(feedIdArray.length == prices.length && prices.length == confidences.length, "Length mismatch");

        for (uint256 i = 0; i < feedIdArray.length; i++) {
            PriceFeed storage feed = feeds[feedIdArray[i]];
            if (feed.feedId == bytes32(0) || !feed.isActive) continue;

            DataSource storage source = sources[feedIdArray[i]][msg.sender];
            if (source.source == address(0) || !source.isActive) continue;
            if (prices[i] == 0 || confidences[i] > 100) continue;

            latestReports[feedIdArray[i]][msg.sender] = PriceReport({
                source: msg.sender,
                price: prices[i],
                timestamp: block.timestamp,
                confidence: confidences[i]
            });

            source.lastReport = block.timestamp;
            source.lastPrice = prices[i];
            source.totalReports++;

            emit PriceReported(feedIdArray[i], msg.sender, prices[i], confidences[i], block.timestamp);
        }
    }

    // ============================================
    // MEDIAN CALCULATION
    // ============================================

    /**
     * @notice Attempts to update the median price
     */
    function _tryUpdateMedian(bytes32 feedId) internal {
        MedianResult memory result = _calculateMedian(feedId);
        
        if (result.isValid) {
            PriceFeed storage feed = feeds[feedId];
            feed.lastMedian = result.median;
            feed.lastUpdate = block.timestamp;

            // Update history
            _addToHistory(feedId, result.median, result.validSources);

            // Update source reputations
            _updateSourceReputations(feedId, result.median);

            emit MedianUpdated(feedId, result.median, result.validSources, block.timestamp);
        }
    }

    /**
     * @notice Calculates the median price from all valid sources
     */
    function _calculateMedian(bytes32 feedId) internal view returns (MedianResult memory result) {
        PriceFeed storage feed = feeds[feedId];
        address[] storage sourceAddrs = feedSources[feedId];
        
        // Collect valid prices
        uint256[] memory validPrices = new uint256[](sourceAddrs.length);
        uint256[] memory weights = new uint256[](sourceAddrs.length);
        uint256 validCount = 0;
        uint256 totalWeight = 0;

        for (uint256 i = 0; i < sourceAddrs.length; i++) {
            address sourceAddr = sourceAddrs[i];
            DataSource storage source = sources[feedId][sourceAddr];
            PriceReport storage report = latestReports[feedId][sourceAddr];

            if (!source.isActive) continue;
            if (report.timestamp == 0) continue;
            if (block.timestamp - report.timestamp > feed.heartbeat) continue;

            validPrices[validCount] = report.price;
            weights[validCount] = source.weight;
            totalWeight += source.weight;
            validCount++;
        }

        result.validSources = validCount;
        result.timestamp = block.timestamp;

        if (validCount < feed.minSources) {
            result.isValid = false;
            return result;
        }

        // Sort prices (simple bubble sort for small arrays)
        for (uint256 i = 0; i < validCount - 1; i++) {
            for (uint256 j = 0; j < validCount - i - 1; j++) {
                if (validPrices[j] > validPrices[j + 1]) {
                    (validPrices[j], validPrices[j + 1]) = (validPrices[j + 1], validPrices[j]);
                    (weights[j], weights[j + 1]) = (weights[j + 1], weights[j]);
                }
            }
        }

        // Calculate median
        if (validCount % 2 == 0) {
            uint256 mid1 = validPrices[validCount / 2 - 1];
            uint256 mid2 = validPrices[validCount / 2];
            result.median = (mid1 + mid2) / 2;
        } else {
            result.median = validPrices[validCount / 2];
        }

        // Calculate mean
        uint256 sum = 0;
        for (uint256 i = 0; i < validCount; i++) {
            sum += validPrices[i];
        }
        result.mean = sum / validCount;

        // Calculate standard deviation
        uint256 varianceSum = 0;
        for (uint256 i = 0; i < validCount; i++) {
            uint256 diff = validPrices[i] > result.mean 
                ? validPrices[i] - result.mean 
                : result.mean - validPrices[i];
            varianceSum += diff * diff;
        }
        result.standardDeviation = Math.sqrt(varianceSum / validCount);

        result.isValid = true;
    }

    /**
     * @notice Updates source reputations based on accuracy
     */
    function _updateSourceReputations(bytes32 feedId, uint256 median) internal {
        PriceFeed storage feed = feeds[feedId];
        address[] storage sourceAddrs = feedSources[feedId];

        for (uint256 i = 0; i < sourceAddrs.length; i++) {
            address sourceAddr = sourceAddrs[i];
            DataSource storage source = sources[feedId][sourceAddr];
            PriceReport storage report = latestReports[feedId][sourceAddr];

            if (!source.isActive || report.timestamp == 0) continue;

            // Calculate deviation from median
            uint256 deviation = report.price > median
                ? ((report.price - median) * 10000) / median
                : ((median - report.price) * 10000) / median;

            // Check if outlier
            if (deviation > feed.deviationThreshold) {
                emit OutlierDetected(feedId, sourceAddr, report.price, median);
                // Decrease reputation
                if (source.reputation > 10) {
                    source.reputation -= 10;
                }
            } else {
                source.accurateReports++;
                // Increase reputation (slowly)
                if (source.reputation < 990) {
                    source.reputation += 1;
                }
            }

            emit SourceReputationUpdated(feedId, sourceAddr, source.reputation);
        }
    }

    /**
     * @notice Adds entry to median history
     */
    function _addToHistory(bytes32 feedId, uint256 median, uint256 validSources) internal {
        HistoryEntry[] storage history = medianHistory[feedId];
        
        if (history.length >= MAX_HISTORY) {
            // Shift array
            for (uint256 i = 0; i < history.length - 1; i++) {
                history[i] = history[i + 1];
            }
            history[history.length - 1] = HistoryEntry({
                median: median,
                validSources: validSources,
                timestamp: block.timestamp
            });
        } else {
            history.push(HistoryEntry({
                median: median,
                validSources: validSources,
                timestamp: block.timestamp
            }));
        }
    }

    // ============================================
    // QUERY FUNCTIONS
    // ============================================

    /**
     * @notice Gets the latest median price
     * @param feedId Feed identifier
     * @return median Median price
     * @return timestamp Last update timestamp
     * @return isValid Whether price is valid
     */
    function getLatestMedian(bytes32 feedId) external view returns (
        uint256 median,
        uint256 timestamp,
        bool isValid
    ) {
        PriceFeed storage feed = feeds[feedId];
        if (feed.feedId == bytes32(0)) revert FeedNotFound(feedId);

        median = feed.lastMedian;
        timestamp = feed.lastUpdate;
        isValid = feed.isActive && 
                  feed.lastUpdate > 0 && 
                  block.timestamp - feed.lastUpdate <= feed.heartbeat;
    }

    /**
     * @notice Gets full median result
     */
    function getMedianResult(bytes32 feedId) external view returns (MedianResult memory) {
        return _calculateMedian(feedId);
    }

    /**
     * @notice Gets aggregation statistics
     */
    function getAggregationStats(bytes32 feedId) external view returns (AggregationStats memory stats) {
        PriceFeed storage feed = feeds[feedId];
        address[] storage sourceAddrs = feedSources[feedId];
        
        uint256 lastMedian = feed.lastMedian;

        for (uint256 i = 0; i < sourceAddrs.length; i++) {
            address sourceAddr = sourceAddrs[i];
            DataSource storage source = sources[feedId][sourceAddr];
            PriceReport storage report = latestReports[feedId][sourceAddr];

            if (!source.isActive) continue;
            
            stats.totalReports++;

            if (report.timestamp > 0 && block.timestamp - report.timestamp <= feed.heartbeat) {
                stats.validReports++;

                if (lastMedian > 0) {
                    uint256 deviation = report.price > lastMedian
                        ? ((report.price - lastMedian) * 10000) / lastMedian
                        : ((lastMedian - report.price) * 10000) / lastMedian;

                    stats.avgDeviation += deviation;
                    if (deviation > stats.maxDeviation) {
                        stats.maxDeviation = deviation;
                    }
                    if (deviation > feed.deviationThreshold) {
                        stats.outlierCount++;
                    }
                }
            }
        }

        if (stats.validReports > 0) {
            stats.avgDeviation /= stats.validReports;
        }
    }

    /**
     * @notice Gets median history
     */
    function getMedianHistory(
        bytes32 feedId,
        uint256 count
    ) external view returns (HistoryEntry[] memory history) {
        HistoryEntry[] storage fullHistory = medianHistory[feedId];
        uint256 len = count > fullHistory.length ? fullHistory.length : count;
        
        history = new HistoryEntry[](len);
        uint256 startIdx = fullHistory.length - len;
        
        for (uint256 i = 0; i < len; i++) {
            history[i] = fullHistory[startIdx + i];
        }
    }

    /**
     * @notice Gets all reports for a feed
     */
    function getAllReports(bytes32 feedId) external view returns (PriceReport[] memory reports) {
        address[] storage sourceAddrs = feedSources[feedId];
        reports = new PriceReport[](sourceAddrs.length);
        
        for (uint256 i = 0; i < sourceAddrs.length; i++) {
            reports[i] = latestReports[feedId][sourceAddrs[i]];
        }
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    function getFeed(bytes32 feedId) external view returns (PriceFeed memory) {
        return feeds[feedId];
    }

    function getAllFeeds() external view returns (bytes32[] memory) {
        return feedIds;
    }

    function getSource(bytes32 feedId, address source) external view returns (DataSource memory) {
        return sources[feedId][source];
    }

    function getFeedSources(bytes32 feedId) external view returns (address[] memory) {
        return feedSources[feedId];
    }

    function getLatestReport(bytes32 feedId, address source) external view returns (PriceReport memory) {
        return latestReports[feedId][source];
    }

    function isStale(bytes32 feedId) external view returns (bool) {
        PriceFeed storage feed = feeds[feedId];
        return block.timestamp - feed.lastUpdate > feed.heartbeat;
    }

    // ============================================
    // ADMIN
    // ============================================

    function pause() external onlyRole(OPERATOR_ROLE) { _pause(); }
    function unpause() external onlyRole(OPERATOR_ROLE) { _unpause(); }
    function _authorizeUpgrade(address) internal override onlyRole(UPGRADER_ROLE) {}
}
