// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

/**
 * @title IOracle
 * @notice Interface for price oracle contracts
 * @dev Provides price data for various assets
 */
interface IOracle {
    /**
     * @notice Price data structure
     */
    struct PriceData {
        uint256 price;
        uint256 timestamp;
        uint8 decimals;
        bool valid;
    }

    /**
     * @notice Returns the latest price for an asset
     * @param asset The asset address
     * @return The price data
     */
    function getLatestPrice(address asset) external view returns (PriceData memory);

    /**
     * @notice Returns the latest price as a uint256
     * @param asset The asset address
     * @return The price
     */
    function getPrice(address asset) external view returns (uint256);

    /**
     * @notice Returns the latest round data for an asset
     * @param asset The asset address
     * @return roundId The round ID
     * @return answer The price
     * @return startedAt The round start timestamp
     * @return updatedAt The update timestamp
     * @return answeredInRound The round in which the answer was computed
     */
    function latestRoundData(address asset)
        external
        view
        returns (
            uint80 roundId,
            int256 answer,
            uint256 startedAt,
            uint256 updatedAt,
            uint80 answeredInRound
        );

    /**
     * @notice Returns the decimals for an asset's price
     * @param asset The asset address
     * @return The number of decimals
     */
    function decimals(address asset) external view returns (uint8);

    /**
     * @notice Sets the price for an asset (admin only)
     * @param asset The asset address
     * @param price The new price
     */
    function setPrice(address asset, uint256 price) external;

    /**
     * @notice Sets the price with custom decimals
     * @param asset The asset address
     * @param price The new price
     * @param priceDecimals The price decimals
     */
    function setPriceWithDecimals(address asset, uint256 price, uint8 priceDecimals) external;

    /**
     * @notice Marks an asset's price as stale/invalid
     * @param asset The asset address
     */
    function invalidatePrice(address asset) external;

    /**
     * @notice Checks if an asset's price is valid
     * @param asset The asset address
     * @return True if valid
     */
    function isPriceValid(address asset) external view returns (bool);

    /**
     * @notice Returns the staleness threshold
     * @return The threshold in seconds
     */
    function stalenessThreshold() external view returns (uint256);

    /**
     * @notice Sets the staleness threshold
     * @param threshold The new threshold in seconds
     */
    function setStalenessThreshold(uint256 threshold) external;

    /**
     * @notice Returns all supported assets
     * @return Array of asset addresses
     */
    function getSupportedAssets() external view returns (address[] memory);

    /**
     * @notice Checks if an asset is supported
     * @param asset The asset address
     * @return True if supported
     */
    function isAssetSupported(address asset) external view returns (bool);

    /**
     * @notice Adds support for a new asset
     * @param asset The asset address
     * @param initialPrice The initial price
     * @param priceDecimals The price decimals
     */
    function addAsset(address asset, uint256 initialPrice, uint8 priceDecimals) external;

    /**
     * @notice Removes support for an asset
     * @param asset The asset address
     */
    function removeAsset(address asset) external;

    // Events
    event PriceUpdated(address indexed asset, uint256 oldPrice, uint256 newPrice, uint256 timestamp);
    event PriceInvalidated(address indexed asset, uint256 timestamp);
    event AssetAdded(address indexed asset, uint256 initialPrice, uint8 decimals);
    event AssetRemoved(address indexed asset);
    event StalenessThresholdUpdated(uint256 oldThreshold, uint256 newThreshold);
}
