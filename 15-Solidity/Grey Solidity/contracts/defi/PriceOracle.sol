// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/Pausable.sol";

/**
 * @title PriceOracle
 * @author Grey Solidity Project
 * @notice Mock price oracle with aggregation and staleness checks
 * @dev Implements a Chainlink-compatible interface for testing
 */
contract PriceOracle is AccessControl, Pausable {
    /// @notice Role for price feed updates
    bytes32 public constant ORACLE_ROLE = keccak256("ORACLE_ROLE");

    /// @notice Price data structure
    struct PriceData {
        int256 price;
        uint256 timestamp;
        uint80 roundId;
        uint8 decimals;
    }

    /// @notice Asset configuration
    struct AssetConfig {
        bool isSupported;
        uint256 heartbeat;      // Max staleness in seconds
        int256 minPrice;        // Minimum valid price
        int256 maxPrice;        // Maximum valid price
        string description;
    }

    /// @notice Mapping of asset to latest price data
    mapping(address => PriceData) private _latestPrices;

    /// @notice Mapping of asset to historical prices by round
    mapping(address => mapping(uint80 => PriceData)) private _roundPrices;

    /// @notice Mapping of asset to configuration
    mapping(address => AssetConfig) private _assetConfigs;

    /// @notice Array of supported assets
    address[] private _supportedAssets;

    /// @notice Default heartbeat (24 hours)
    uint256 public constant DEFAULT_HEARTBEAT = 86400;

    // ============ Events ============

    event PriceUpdated(
        address indexed asset,
        int256 price,
        uint256 timestamp,
        uint80 roundId
    );

    event AssetAdded(
        address indexed asset,
        uint8 decimals,
        string description
    );

    event AssetRemoved(address indexed asset);
    event HeartbeatUpdated(address indexed asset, uint256 heartbeat);
    event PriceBoundsUpdated(address indexed asset, int256 minPrice, int256 maxPrice);

    // ============ Errors ============

    error AssetNotSupported(address asset);
    error StalePrice(address asset, uint256 timestamp, uint256 heartbeat);
    error PriceOutOfBounds(address asset, int256 price, int256 min, int256 max);
    error InvalidPrice();
    error InvalidRound(uint80 roundId);
    error AssetAlreadySupported(address asset);

    /**
     * @notice Initializes the oracle
     */
    constructor() {
        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(ORACLE_ROLE, msg.sender);
    }

    // ============ Price Update Functions ============

    /**
     * @notice Updates the price for an asset
     * @param asset The asset address
     * @param price The new price
     */
    function updatePrice(
        address asset,
        int256 price
    ) external onlyRole(ORACLE_ROLE) whenNotPaused {
        AssetConfig storage config = _assetConfigs[asset];
        if (!config.isSupported) revert AssetNotSupported(asset);
        if (price <= 0) revert InvalidPrice();

        // Check bounds
        if (config.minPrice > 0 && price < config.minPrice) {
            revert PriceOutOfBounds(asset, price, config.minPrice, config.maxPrice);
        }
        if (config.maxPrice > 0 && price > config.maxPrice) {
            revert PriceOutOfBounds(asset, price, config.minPrice, config.maxPrice);
        }

        PriceData storage latest = _latestPrices[asset];
        uint80 newRoundId = latest.roundId + 1;

        PriceData memory newPrice = PriceData({
            price: price,
            timestamp: block.timestamp,
            roundId: newRoundId,
            decimals: latest.decimals
        });

        _latestPrices[asset] = newPrice;
        _roundPrices[asset][newRoundId] = newPrice;

        emit PriceUpdated(asset, price, block.timestamp, newRoundId);
    }

    /**
     * @notice Batch updates multiple prices
     * @param assets Array of asset addresses
     * @param prices Array of prices
     */
    function batchUpdatePrices(
        address[] calldata assets,
        int256[] calldata prices
    ) external onlyRole(ORACLE_ROLE) whenNotPaused {
        require(assets.length == prices.length, "PriceOracle: length mismatch");

        for (uint256 i = 0; i < assets.length; i++) {
            AssetConfig storage config = _assetConfigs[assets[i]];
            if (!config.isSupported) continue;
            if (prices[i] <= 0) continue;

            if (config.minPrice > 0 && prices[i] < config.minPrice) continue;
            if (config.maxPrice > 0 && prices[i] > config.maxPrice) continue;

            PriceData storage latest = _latestPrices[assets[i]];
            uint80 newRoundId = latest.roundId + 1;

            PriceData memory newPrice = PriceData({
                price: prices[i],
                timestamp: block.timestamp,
                roundId: newRoundId,
                decimals: latest.decimals
            });

            _latestPrices[assets[i]] = newPrice;
            _roundPrices[assets[i]][newRoundId] = newPrice;

            emit PriceUpdated(assets[i], prices[i], block.timestamp, newRoundId);
        }
    }

    // ============ Chainlink-Compatible Interface ============

    /**
     * @notice Returns the latest price for an asset
     * @param asset The asset address
     * @return roundId The round ID
     * @return answer The price
     * @return startedAt Timestamp (same as updatedAt)
     * @return updatedAt The update timestamp
     * @return answeredInRound The round ID
     */
    function latestRoundData(address asset) external view returns (
        uint80 roundId,
        int256 answer,
        uint256 startedAt,
        uint256 updatedAt,
        uint80 answeredInRound
    ) {
        PriceData storage data = _latestPrices[asset];
        AssetConfig storage config = _assetConfigs[asset];

        if (!config.isSupported) revert AssetNotSupported(asset);

        uint256 heartbeat = config.heartbeat > 0 ? config.heartbeat : DEFAULT_HEARTBEAT;
        if (block.timestamp - data.timestamp > heartbeat) {
            revert StalePrice(asset, data.timestamp, heartbeat);
        }

        return (
            data.roundId,
            data.price,
            data.timestamp,
            data.timestamp,
            data.roundId
        );
    }

    /**
     * @notice Returns price for a specific round
     * @param asset The asset address
     * @param _roundId The round ID
     * @return roundId The round ID
     * @return answer The price
     * @return startedAt Timestamp
     * @return updatedAt Timestamp
     * @return answeredInRound The round ID
     */
    function getRoundData(
        address asset,
        uint80 _roundId
    ) external view returns (
        uint80 roundId,
        int256 answer,
        uint256 startedAt,
        uint256 updatedAt,
        uint80 answeredInRound
    ) {
        PriceData storage data = _roundPrices[asset][_roundId];
        if (data.timestamp == 0) revert InvalidRound(_roundId);

        return (
            data.roundId,
            data.price,
            data.timestamp,
            data.timestamp,
            data.roundId
        );
    }

    /**
     * @notice Returns the number of decimals
     * @param asset The asset address
     * @return The decimals
     */
    function decimals(address asset) external view returns (uint8) {
        return _latestPrices[asset].decimals;
    }

    /**
     * @notice Returns the asset description
     * @param asset The asset address
     * @return The description
     */
    function description(address asset) external view returns (string memory) {
        return _assetConfigs[asset].description;
    }

    // ============ View Functions ============

    /**
     * @notice Returns the latest price (simple)
     * @param asset The asset address
     * @return The price
     */
    function getPrice(address asset) external view returns (int256) {
        PriceData storage data = _latestPrices[asset];
        AssetConfig storage config = _assetConfigs[asset];

        if (!config.isSupported) revert AssetNotSupported(asset);

        uint256 heartbeat = config.heartbeat > 0 ? config.heartbeat : DEFAULT_HEARTBEAT;
        if (block.timestamp - data.timestamp > heartbeat) {
            revert StalePrice(asset, data.timestamp, heartbeat);
        }

        return data.price;
    }

    /**
     * @notice Returns the price without staleness check
     * @param asset The asset address
     * @return price The price
     * @return timestamp The timestamp
     */
    function getPriceUnsafe(address asset) external view returns (int256 price, uint256 timestamp) {
        PriceData storage data = _latestPrices[asset];
        return (data.price, data.timestamp);
    }

    /**
     * @notice Checks if a price is stale
     * @param asset The asset address
     * @return True if stale
     */
    function isStale(address asset) external view returns (bool) {
        PriceData storage data = _latestPrices[asset];
        AssetConfig storage config = _assetConfigs[asset];
        
        uint256 heartbeat = config.heartbeat > 0 ? config.heartbeat : DEFAULT_HEARTBEAT;
        return block.timestamp - data.timestamp > heartbeat;
    }

    /**
     * @notice Returns all supported assets
     * @return Array of asset addresses
     */
    function getSupportedAssets() external view returns (address[] memory) {
        return _supportedAssets;
    }

    /**
     * @notice Returns asset configuration
     * @param asset The asset address
     * @return The configuration
     */
    function getAssetConfig(address asset) external view returns (AssetConfig memory) {
        return _assetConfigs[asset];
    }

    /**
     * @notice Checks if an asset is supported
     * @param asset The asset address
     * @return True if supported
     */
    function isAssetSupported(address asset) external view returns (bool) {
        return _assetConfigs[asset].isSupported;
    }

    // ============ Admin Functions ============

    /**
     * @notice Adds a supported asset
     * @param asset The asset address
     * @param assetDecimals The price decimals
     * @param heartbeat The max staleness
     * @param minPrice The minimum price
     * @param maxPrice The maximum price
     * @param desc The description
     */
    function addAsset(
        address asset,
        uint8 assetDecimals,
        uint256 heartbeat,
        int256 minPrice,
        int256 maxPrice,
        string calldata desc
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (_assetConfigs[asset].isSupported) {
            revert AssetAlreadySupported(asset);
        }

        _assetConfigs[asset] = AssetConfig({
            isSupported: true,
            heartbeat: heartbeat > 0 ? heartbeat : DEFAULT_HEARTBEAT,
            minPrice: minPrice,
            maxPrice: maxPrice,
            description: desc
        });

        _latestPrices[asset] = PriceData({
            price: 0,
            timestamp: 0,
            roundId: 0,
            decimals: assetDecimals
        });

        _supportedAssets.push(asset);

        emit AssetAdded(asset, assetDecimals, desc);
    }

    /**
     * @notice Removes a supported asset
     * @param asset The asset address
     */
    function removeAsset(address asset) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (!_assetConfigs[asset].isSupported) {
            revert AssetNotSupported(asset);
        }

        delete _assetConfigs[asset];
        delete _latestPrices[asset];

        // Remove from array
        for (uint256 i = 0; i < _supportedAssets.length; i++) {
            if (_supportedAssets[i] == asset) {
                _supportedAssets[i] = _supportedAssets[_supportedAssets.length - 1];
                _supportedAssets.pop();
                break;
            }
        }

        emit AssetRemoved(asset);
    }

    /**
     * @notice Updates asset heartbeat
     * @param asset The asset address
     * @param heartbeat The new heartbeat
     */
    function setHeartbeat(
        address asset,
        uint256 heartbeat
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (!_assetConfigs[asset].isSupported) {
            revert AssetNotSupported(asset);
        }

        _assetConfigs[asset].heartbeat = heartbeat;
        emit HeartbeatUpdated(asset, heartbeat);
    }

    /**
     * @notice Updates price bounds
     * @param asset The asset address
     * @param minPrice The minimum price
     * @param maxPrice The maximum price
     */
    function setPriceBounds(
        address asset,
        int256 minPrice,
        int256 maxPrice
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (!_assetConfigs[asset].isSupported) {
            revert AssetNotSupported(asset);
        }

        _assetConfigs[asset].minPrice = minPrice;
        _assetConfigs[asset].maxPrice = maxPrice;
        emit PriceBoundsUpdated(asset, minPrice, maxPrice);
    }

    /**
     * @notice Pauses the oracle
     */
    function pause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        _pause();
    }

    /**
     * @notice Unpauses the oracle
     */
    function unpause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        _unpause();
    }
}
