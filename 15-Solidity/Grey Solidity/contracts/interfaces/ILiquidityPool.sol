// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

/**
 * @title ILiquidityPool
 * @notice Interface for AMM liquidity pool contracts
 * @dev Implements constant product market maker (x*y=k)
 */
interface ILiquidityPool {
    /**
     * @notice Pool info structure
     */
    struct PoolInfo {
        address token0;
        address token1;
        uint256 reserve0;
        uint256 reserve1;
        uint256 totalLiquidity;
        uint256 fee; // basis points
        uint256 kLast; // reserve0 * reserve1
    }

    /**
     * @notice Liquidity position structure
     */
    struct LiquidityPosition {
        uint256 liquidity;
        uint256 token0Owed;
        uint256 token1Owed;
    }

    /**
     * @notice Returns the first token address
     * @return The token0 address
     */
    function token0() external view returns (address);

    /**
     * @notice Returns the second token address
     * @return The token1 address
     */
    function token1() external view returns (address);

    /**
     * @notice Returns the current reserves
     * @return reserve0 The reserve of token0
     * @return reserve1 The reserve of token1
     * @return blockTimestampLast The last update timestamp
     */
    function getReserves() external view returns (uint256 reserve0, uint256 reserve1, uint32 blockTimestampLast);

    /**
     * @notice Adds liquidity to the pool
     * @param amount0Desired Desired amount of token0
     * @param amount1Desired Desired amount of token1
     * @param amount0Min Minimum amount of token0
     * @param amount1Min Minimum amount of token1
     * @param to The recipient of LP tokens
     * @param deadline Transaction deadline
     * @return amount0 Actual amount of token0 added
     * @return amount1 Actual amount of token1 added
     * @return liquidity The liquidity tokens minted
     */
    function addLiquidity(
        uint256 amount0Desired,
        uint256 amount1Desired,
        uint256 amount0Min,
        uint256 amount1Min,
        address to,
        uint256 deadline
    ) external returns (uint256 amount0, uint256 amount1, uint256 liquidity);

    /**
     * @notice Removes liquidity from the pool
     * @param liquidity The liquidity to remove
     * @param amount0Min Minimum amount of token0
     * @param amount1Min Minimum amount of token1
     * @param to The recipient
     * @param deadline Transaction deadline
     * @return amount0 The amount of token0 received
     * @return amount1 The amount of token1 received
     */
    function removeLiquidity(
        uint256 liquidity,
        uint256 amount0Min,
        uint256 amount1Min,
        address to,
        uint256 deadline
    ) external returns (uint256 amount0, uint256 amount1);

    /**
     * @notice Swaps tokens
     * @param amount0Out The amount of token0 to receive
     * @param amount1Out The amount of token1 to receive
     * @param to The recipient
     * @param data Flash swap callback data
     */
    function swap(uint256 amount0Out, uint256 amount1Out, address to, bytes calldata data) external;

    /**
     * @notice Swaps exact input for output
     * @param tokenIn The input token
     * @param amountIn The input amount
     * @param amountOutMin The minimum output amount
     * @param to The recipient
     * @param deadline Transaction deadline
     * @return amountOut The output amount
     */
    function swapExactIn(
        address tokenIn,
        uint256 amountIn,
        uint256 amountOutMin,
        address to,
        uint256 deadline
    ) external returns (uint256 amountOut);

    /**
     * @notice Swaps input for exact output
     * @param tokenOut The output token
     * @param amountOut The output amount
     * @param amountInMax The maximum input amount
     * @param to The recipient
     * @param deadline Transaction deadline
     * @return amountIn The input amount
     */
    function swapExactOut(
        address tokenOut,
        uint256 amountOut,
        uint256 amountInMax,
        address to,
        uint256 deadline
    ) external returns (uint256 amountIn);

    /**
     * @notice Gets the output amount for a given input
     * @param tokenIn The input token
     * @param amountIn The input amount
     * @return The output amount
     */
    function getAmountOut(address tokenIn, uint256 amountIn) external view returns (uint256);

    /**
     * @notice Gets the input amount for a given output
     * @param tokenOut The output token
     * @param amountOut The output amount
     * @return The input amount
     */
    function getAmountIn(address tokenOut, uint256 amountOut) external view returns (uint256);

    /**
     * @notice Returns liquidity balance
     * @param owner The owner address
     * @return The liquidity balance
     */
    function liquidityOf(address owner) external view returns (uint256);

    /**
     * @notice Returns total liquidity
     * @return The total liquidity
     */
    function totalLiquidity() external view returns (uint256);

    /**
     * @notice Returns pool info
     * @return The pool info
     */
    function getPoolInfo() external view returns (PoolInfo memory);

    /**
     * @notice Returns liquidity position
     * @param owner The owner address
     * @return The position
     */
    function getPosition(address owner) external view returns (LiquidityPosition memory);

    /**
     * @notice Returns the swap fee
     * @return The fee in basis points
     */
    function swapFee() external view returns (uint256);

    /**
     * @notice Sets the swap fee
     * @param fee The new fee in basis points
     */
    function setSwapFee(uint256 fee) external;

    /**
     * @notice Syncs reserves with actual balances
     */
    function sync() external;

    /**
     * @notice Skims excess tokens to an address
     * @param to The recipient
     */
    function skim(address to) external;

    /**
     * @notice Returns the current price
     * @return The price of token0 in terms of token1
     */
    function getPrice() external view returns (uint256);

    /**
     * @notice Returns the time-weighted average price
     * @param periodSeconds The averaging period
     * @return The TWAP
     */
    function getTWAP(uint256 periodSeconds) external view returns (uint256);

    // Events
    event LiquidityAdded(
        address indexed provider,
        uint256 amount0,
        uint256 amount1,
        uint256 liquidity
    );
    event LiquidityRemoved(
        address indexed provider,
        uint256 amount0,
        uint256 amount1,
        uint256 liquidity
    );
    event Swap(
        address indexed sender,
        uint256 amount0In,
        uint256 amount1In,
        uint256 amount0Out,
        uint256 amount1Out,
        address indexed to
    );
    event Sync(uint256 reserve0, uint256 reserve1);
    event SwapFeeUpdated(uint256 oldFee, uint256 newFee);
}
