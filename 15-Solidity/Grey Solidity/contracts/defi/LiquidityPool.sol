// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/token/ERC20/ERC20.sol";
import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/Pausable.sol";
import "../libraries/MathUtils.sol";

/**
 * @title LiquidityPool
 * @author Grey Solidity Project
 * @notice Constant product AMM (x*y=k) with LP tokens and fees
 * @dev Implements Uniswap V2-style automated market maker
 */
contract LiquidityPool is ERC20, AccessControl, ReentrancyGuard, Pausable {
    using SafeERC20 for IERC20;

    /// @notice Role for fee management
    bytes32 public constant FEE_MANAGER_ROLE = keccak256("FEE_MANAGER_ROLE");

    /// @notice Token A in the pair
    IERC20 public immutable tokenA;

    /// @notice Token B in the pair
    IERC20 public immutable tokenB;

    /// @notice Reserve of token A
    uint256 public reserveA;

    /// @notice Reserve of token B
    uint256 public reserveB;

    /// @notice Swap fee in basis points (30 = 0.3%)
    uint256 public swapFee;

    /// @notice Protocol fee share of swap fee (0-5000 = 0-50%)
    uint256 public protocolFeeShare;

    /// @notice Accumulated protocol fees for token A
    uint256 public protocolFeesA;

    /// @notice Accumulated protocol fees for token B
    uint256 public protocolFeesB;

    /// @notice Last recorded K value
    uint256 public lastK;

    /// @notice Minimum liquidity burned on first deposit
    uint256 public constant MINIMUM_LIQUIDITY = 1000;

    /// @notice TWAP oracle support
    uint256 public priceACumulativeLast;
    uint256 public priceBCumulativeLast;
    uint32 public blockTimestampLast;

    // ============ Events ============

    event LiquidityAdded(
        address indexed provider,
        uint256 amountA,
        uint256 amountB,
        uint256 liquidity
    );

    event LiquidityRemoved(
        address indexed provider,
        uint256 amountA,
        uint256 amountB,
        uint256 liquidity
    );

    event Swap(
        address indexed user,
        address indexed tokenIn,
        uint256 amountIn,
        uint256 amountOut,
        uint256 fee
    );

    event SwapFeeUpdated(uint256 oldFee, uint256 newFee);
    event ProtocolFeeShareUpdated(uint256 oldShare, uint256 newShare);
    event ProtocolFeesWithdrawn(address indexed to, uint256 amountA, uint256 amountB);
    event Sync(uint256 reserveA, uint256 reserveB);

    // ============ Errors ============

    error InsufficientLiquidity();
    error InsufficientInputAmount();
    error InsufficientOutputAmount();
    error InsufficientLiquidityMinted();
    error InsufficientLiquidityBurned();
    error InvalidToken();
    error InvalidK();
    error SlippageExceeded(uint256 expected, uint256 actual);
    error ZeroAddress();

    /**
     * @notice Initializes the liquidity pool
     * @param tokenA_ Address of token A
     * @param tokenB_ Address of token B
     * @param name_ LP token name
     * @param symbol_ LP token symbol
     * @param swapFee_ Initial swap fee in basis points
     */
    constructor(
        address tokenA_,
        address tokenB_,
        string memory name_,
        string memory symbol_,
        uint256 swapFee_
    ) ERC20(name_, symbol_) {
        require(tokenA_ != tokenB_, "LiquidityPool: identical tokens");
        require(tokenA_ != address(0) && tokenB_ != address(0), "LiquidityPool: zero address");

        tokenA = IERC20(tokenA_);
        tokenB = IERC20(tokenB_);
        swapFee = swapFee_;
        protocolFeeShare = 1667;  // ~16.67% of swap fee (like Uniswap)

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(FEE_MANAGER_ROLE, msg.sender);
    }

    // ============ Liquidity Functions ============

    /**
     * @notice Adds liquidity to the pool
     * @param amountADesired Desired amount of token A
     * @param amountBDesired Desired amount of token B
     * @param amountAMin Minimum amount of token A
     * @param amountBMin Minimum amount of token B
     * @param to Recipient of LP tokens
     * @return amountA Actual amount of token A added
     * @return amountB Actual amount of token B added
     * @return liquidity LP tokens minted
     */
    function addLiquidity(
        uint256 amountADesired,
        uint256 amountBDesired,
        uint256 amountAMin,
        uint256 amountBMin,
        address to
    ) external nonReentrant whenNotPaused returns (
        uint256 amountA,
        uint256 amountB,
        uint256 liquidity
    ) {
        if (to == address(0)) revert ZeroAddress();

        (amountA, amountB) = _calculateLiquidityAmounts(
            amountADesired,
            amountBDesired,
            amountAMin,
            amountBMin
        );

        tokenA.safeTransferFrom(msg.sender, address(this), amountA);
        tokenB.safeTransferFrom(msg.sender, address(this), amountB);

        liquidity = _mint(to, amountA, amountB);

        emit LiquidityAdded(to, amountA, amountB, liquidity);
    }

    /**
     * @notice Removes liquidity from the pool
     * @param liquidity Amount of LP tokens to burn
     * @param amountAMin Minimum amount of token A to receive
     * @param amountBMin Minimum amount of token B to receive
     * @param to Recipient of tokens
     * @return amountA Amount of token A returned
     * @return amountB Amount of token B returned
     */
    function removeLiquidity(
        uint256 liquidity,
        uint256 amountAMin,
        uint256 amountBMin,
        address to
    ) external nonReentrant whenNotPaused returns (uint256 amountA, uint256 amountB) {
        if (to == address(0)) revert ZeroAddress();

        // Transfer LP tokens to this contract
        _transfer(msg.sender, address(this), liquidity);

        uint256 totalSupply_ = totalSupply();
        amountA = (liquidity * reserveA) / totalSupply_;
        amountB = (liquidity * reserveB) / totalSupply_;

        if (amountA < amountAMin) {
            revert SlippageExceeded(amountAMin, amountA);
        }
        if (amountB < amountBMin) {
            revert SlippageExceeded(amountBMin, amountB);
        }

        // Burn LP tokens
        _burn(address(this), liquidity);

        tokenA.safeTransfer(to, amountA);
        tokenB.safeTransfer(to, amountB);

        _update();

        emit LiquidityRemoved(to, amountA, amountB, liquidity);
    }

    // ============ Swap Functions ============

    /**
     * @notice Swaps tokens
     * @param tokenIn The input token address
     * @param amountIn The input amount
     * @param amountOutMin Minimum output amount
     * @param to Recipient of output tokens
     * @return amountOut The output amount
     */
    function swap(
        address tokenIn,
        uint256 amountIn,
        uint256 amountOutMin,
        address to
    ) external nonReentrant whenNotPaused returns (uint256 amountOut) {
        if (to == address(0)) revert ZeroAddress();
        if (amountIn == 0) revert InsufficientInputAmount();
        if (tokenIn != address(tokenA) && tokenIn != address(tokenB)) {
            revert InvalidToken();
        }

        bool isTokenA = tokenIn == address(tokenA);
        (IERC20 tokenInContract, IERC20 tokenOutContract) = isTokenA
            ? (tokenA, tokenB)
            : (tokenB, tokenA);
        (uint256 reserveIn, uint256 reserveOut) = isTokenA
            ? (reserveA, reserveB)
            : (reserveB, reserveA);

        // Transfer in
        tokenInContract.safeTransferFrom(msg.sender, address(this), amountIn);

        // Calculate output with fee
        uint256 amountInWithFee = amountIn * (10000 - swapFee);
        amountOut = (amountInWithFee * reserveOut) / (reserveIn * 10000 + amountInWithFee);

        if (amountOut < amountOutMin) {
            revert SlippageExceeded(amountOutMin, amountOut);
        }
        if (amountOut == 0) revert InsufficientOutputAmount();

        // Calculate protocol fee
        uint256 fee = (amountIn * swapFee) / 10000;
        uint256 protocolFee = (fee * protocolFeeShare) / 10000;

        if (isTokenA) {
            protocolFeesA += protocolFee;
        } else {
            protocolFeesB += protocolFee;
        }

        // Transfer out
        tokenOutContract.safeTransfer(to, amountOut);

        _update();

        // Verify K
        uint256 newK = reserveA * reserveB;
        if (newK < lastK) revert InvalidK();

        emit Swap(msg.sender, tokenIn, amountIn, amountOut, fee);
    }

    /**
     * @notice Gets the output amount for a swap
     * @param tokenIn The input token
     * @param amountIn The input amount
     * @return amountOut The output amount
     */
    function getAmountOut(
        address tokenIn,
        uint256 amountIn
    ) external view returns (uint256 amountOut) {
        if (amountIn == 0) return 0;

        bool isTokenA = tokenIn == address(tokenA);
        (uint256 reserveIn, uint256 reserveOut) = isTokenA
            ? (reserveA, reserveB)
            : (reserveB, reserveA);

        uint256 amountInWithFee = amountIn * (10000 - swapFee);
        amountOut = (amountInWithFee * reserveOut) / (reserveIn * 10000 + amountInWithFee);
    }

    /**
     * @notice Gets the input amount required for a desired output
     * @param tokenOut The output token
     * @param amountOut The desired output amount
     * @return amountIn The required input amount
     */
    function getAmountIn(
        address tokenOut,
        uint256 amountOut
    ) external view returns (uint256 amountIn) {
        if (amountOut == 0) return 0;

        bool isTokenA = tokenOut == address(tokenA);
        (uint256 reserveIn, uint256 reserveOut) = isTokenA
            ? (reserveB, reserveA)
            : (reserveA, reserveB);

        if (amountOut >= reserveOut) return type(uint256).max;

        amountIn = (reserveIn * amountOut * 10000) / ((reserveOut - amountOut) * (10000 - swapFee)) + 1;
    }

    // ============ View Functions ============

    /**
     * @notice Returns current reserves
     * @return _reserveA Reserve of token A
     * @return _reserveB Reserve of token B
     * @return _blockTimestampLast Last update timestamp
     */
    function getReserves() external view returns (
        uint256 _reserveA,
        uint256 _reserveB,
        uint32 _blockTimestampLast
    ) {
        return (reserveA, reserveB, blockTimestampLast);
    }

    /**
     * @notice Returns the current price (B per A)
     * @return price The price scaled by 1e18
     */
    function getPrice() external view returns (uint256 price) {
        if (reserveA == 0) return 0;
        return (reserveB * 1e18) / reserveA;
    }

    /**
     * @notice Returns pool token addresses
     * @return token0 Token A address
     * @return token1 Token B address
     */
    function getTokens() external view returns (address token0, address token1) {
        return (address(tokenA), address(tokenB));
    }

    /**
     * @notice Calculates LP tokens for adding liquidity
     * @param amountA Amount of token A
     * @param amountB Amount of token B
     * @return liquidity The LP tokens to receive
     */
    function quoteAddLiquidity(
        uint256 amountA,
        uint256 amountB
    ) external view returns (uint256 liquidity) {
        uint256 totalSupply_ = totalSupply();
        
        if (totalSupply_ == 0) {
            liquidity = MathUtils.sqrt(amountA * amountB) - MINIMUM_LIQUIDITY;
        } else {
            uint256 liquidityA = (amountA * totalSupply_) / reserveA;
            uint256 liquidityB = (amountB * totalSupply_) / reserveB;
            liquidity = liquidityA < liquidityB ? liquidityA : liquidityB;
        }
    }

    /**
     * @notice Calculates tokens received for removing liquidity
     * @param liquidity Amount of LP tokens
     * @return amountA Token A to receive
     * @return amountB Token B to receive
     */
    function quoteRemoveLiquidity(
        uint256 liquidity
    ) external view returns (uint256 amountA, uint256 amountB) {
        uint256 totalSupply_ = totalSupply();
        if (totalSupply_ == 0) return (0, 0);
        
        amountA = (liquidity * reserveA) / totalSupply_;
        amountB = (liquidity * reserveB) / totalSupply_;
    }

    // ============ Admin Functions ============

    /**
     * @notice Sets the swap fee
     * @param newFee The new fee in basis points
     */
    function setSwapFee(uint256 newFee) external onlyRole(FEE_MANAGER_ROLE) {
        require(newFee <= 1000, "LiquidityPool: fee too high");  // Max 10%
        
        uint256 oldFee = swapFee;
        swapFee = newFee;

        emit SwapFeeUpdated(oldFee, newFee);
    }

    /**
     * @notice Sets the protocol fee share
     * @param newShare The new share in basis points
     */
    function setProtocolFeeShare(uint256 newShare) external onlyRole(FEE_MANAGER_ROLE) {
        require(newShare <= 5000, "LiquidityPool: share too high");  // Max 50%
        
        uint256 oldShare = protocolFeeShare;
        protocolFeeShare = newShare;

        emit ProtocolFeeShareUpdated(oldShare, newShare);
    }

    /**
     * @notice Withdraws accumulated protocol fees
     * @param to Recipient of fees
     */
    function withdrawProtocolFees(address to) external onlyRole(FEE_MANAGER_ROLE) nonReentrant {
        if (to == address(0)) revert ZeroAddress();

        uint256 feesA = protocolFeesA;
        uint256 feesB = protocolFeesB;

        protocolFeesA = 0;
        protocolFeesB = 0;

        if (feesA > 0) {
            tokenA.safeTransfer(to, feesA);
        }
        if (feesB > 0) {
            tokenB.safeTransfer(to, feesB);
        }

        emit ProtocolFeesWithdrawn(to, feesA, feesB);
    }

    /**
     * @notice Syncs reserves with actual balances
     */
    function sync() external nonReentrant {
        _update();
        emit Sync(reserveA, reserveB);
    }

    /**
     * @notice Pauses the pool
     */
    function pause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        _pause();
    }

    /**
     * @notice Unpauses the pool
     */
    function unpause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        _unpause();
    }

    // ============ Internal Functions ============

    /**
     * @dev Calculates optimal liquidity amounts
     */
    function _calculateLiquidityAmounts(
        uint256 amountADesired,
        uint256 amountBDesired,
        uint256 amountAMin,
        uint256 amountBMin
    ) private view returns (uint256 amountA, uint256 amountB) {
        if (reserveA == 0 && reserveB == 0) {
            return (amountADesired, amountBDesired);
        }

        uint256 amountBOptimal = (amountADesired * reserveB) / reserveA;
        if (amountBOptimal <= amountBDesired) {
            if (amountBOptimal < amountBMin) {
                revert SlippageExceeded(amountBMin, amountBOptimal);
            }
            return (amountADesired, amountBOptimal);
        }

        uint256 amountAOptimal = (amountBDesired * reserveA) / reserveB;
        if (amountAOptimal < amountAMin) {
            revert SlippageExceeded(amountAMin, amountAOptimal);
        }
        return (amountAOptimal, amountBDesired);
    }

    /**
     * @dev Mints LP tokens
     */
    function _mint(
        address to,
        uint256 amountA,
        uint256 amountB
    ) private returns (uint256 liquidity) {
        uint256 totalSupply_ = totalSupply();

        if (totalSupply_ == 0) {
            liquidity = MathUtils.sqrt(amountA * amountB) - MINIMUM_LIQUIDITY;
            _mint(address(0xdead), MINIMUM_LIQUIDITY);  // Lock minimum liquidity
        } else {
            uint256 liquidityA = (amountA * totalSupply_) / reserveA;
            uint256 liquidityB = (amountB * totalSupply_) / reserveB;
            liquidity = liquidityA < liquidityB ? liquidityA : liquidityB;
        }

        if (liquidity == 0) revert InsufficientLiquidityMinted();

        _mint(to, liquidity);
        _update();
    }

    /**
     * @dev Updates reserves and TWAP accumulators
     */
    function _update() private {
        uint256 balanceA = tokenA.balanceOf(address(this)) - protocolFeesA;
        uint256 balanceB = tokenB.balanceOf(address(this)) - protocolFeesB;

        uint32 blockTimestamp = uint32(block.timestamp);
        uint32 timeElapsed;
        unchecked {
            timeElapsed = blockTimestamp - blockTimestampLast;
        }

        // Update TWAP accumulators
        if (timeElapsed > 0 && reserveA > 0 && reserveB > 0) {
            unchecked {
                priceACumulativeLast += (reserveB * 1e18 / reserveA) * timeElapsed;
                priceBCumulativeLast += (reserveA * 1e18 / reserveB) * timeElapsed;
            }
        }

        reserveA = balanceA;
        reserveB = balanceB;
        lastK = balanceA * balanceB;
        blockTimestampLast = blockTimestamp;
    }
}
