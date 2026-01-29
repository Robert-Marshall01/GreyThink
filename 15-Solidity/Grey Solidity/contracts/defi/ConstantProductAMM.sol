// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/token/ERC20/ERC20.sol";
import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/math/Math.sol";

/**
 * @title ConstantProductAMM
 * @notice Uniswap V2 style constant product AMM (x * y = k)
 * @dev Implements the classic automated market maker with liquidity provider tokens
 *
 * Features:
 * - Constant product formula (x * y = k)
 * - LP token minting/burning
 * - Flash swap support
 * - Protocol fee extraction
 * - TWAP oracle accumulator
 * - Skim and sync functions
 * - MEV-resistant price updates
 */
contract ConstantProductAMM is ERC20, AccessControl, ReentrancyGuard {
    using SafeERC20 for IERC20;

    // ============ Constants ============

    bytes32 public constant FEE_MANAGER_ROLE = keccak256("FEE_MANAGER_ROLE");
    
    uint256 public constant MINIMUM_LIQUIDITY = 1000;
    uint256 public constant FEE_DENOMINATOR = 10000;
    uint256 public constant MAX_FEE = 100; // 1% max
    uint256 private constant Q112 = 2**112;

    // ============ State Variables ============

    address public immutable token0;
    address public immutable token1;
    address public immutable factory;

    uint112 private reserve0;
    uint112 private reserve1;
    uint32 private blockTimestampLast;

    /// @notice Cumulative price for TWAP oracle
    uint256 public price0CumulativeLast;
    uint256 public price1CumulativeLast;

    /// @notice K value after last liquidity event
    uint256 public kLast;

    /// @notice Swap fee (in basis points, e.g., 30 = 0.3%)
    uint256 public swapFee = 30;

    /// @notice Protocol fee share (1/n of swap fee goes to protocol)
    uint256 public protocolFeeDivisor = 6; // 1/6 of 0.3% = 0.05%

    /// @notice Protocol fee recipient
    address public feeRecipient;

    /// @notice Flash swap fee
    uint256 public flashFee = 9; // 0.09%

    // ============ Events ============

    event Mint(address indexed sender, uint256 amount0, uint256 amount1);
    event Burn(address indexed sender, uint256 amount0, uint256 amount1, address indexed to);
    event Swap(
        address indexed sender,
        uint256 amount0In,
        uint256 amount1In,
        uint256 amount0Out,
        uint256 amount1Out,
        address indexed to
    );
    event Sync(uint112 reserve0, uint112 reserve1);
    event ProtocolFeeCollected(address indexed token, uint256 amount);

    // ============ Errors ============

    error InsufficientLiquidity();
    error InsufficientLiquidityMinted();
    error InsufficientLiquidityBurned();
    error InsufficientInputAmount();
    error InsufficientOutputAmount();
    error InvalidK();
    error InvalidTo();
    error Overflow();
    error Locked();
    error IdenticalAddresses();
    error ZeroAddress();

    // ============ Constructor ============

    constructor(
        address _token0,
        address _token1,
        address _feeRecipient,
        address _admin
    ) ERC20("Grey LP Token", "GLP") {
        require(_token0 != _token1, "Identical addresses");
        require(_token0 != address(0) && _token1 != address(0), "Zero address");
        
        // Sort tokens
        (token0, token1) = _token0 < _token1 ? (_token0, _token1) : (_token1, _token0);
        factory = msg.sender;
        feeRecipient = _feeRecipient;

        _grantRole(DEFAULT_ADMIN_ROLE, _admin);
        _grantRole(FEE_MANAGER_ROLE, _admin);
    }

    // ============ View Functions ============

    /**
     * @notice Get current reserves
     */
    function getReserves() public view returns (
        uint112 _reserve0,
        uint112 _reserve1,
        uint32 _blockTimestampLast
    ) {
        return (reserve0, reserve1, blockTimestampLast);
    }

    /**
     * @notice Calculate output amount for a swap
     * @param amountIn Input amount
     * @param reserveIn Input reserve
     * @param reserveOut Output reserve
     */
    function getAmountOut(
        uint256 amountIn,
        uint256 reserveIn,
        uint256 reserveOut
    ) public view returns (uint256 amountOut) {
        require(amountIn > 0, "Insufficient input");
        require(reserveIn > 0 && reserveOut > 0, "Insufficient liquidity");

        uint256 amountInWithFee = amountIn * (FEE_DENOMINATOR - swapFee);
        uint256 numerator = amountInWithFee * reserveOut;
        uint256 denominator = reserveIn * FEE_DENOMINATOR + amountInWithFee;
        amountOut = numerator / denominator;
    }

    /**
     * @notice Calculate input amount for a desired output
     */
    function getAmountIn(
        uint256 amountOut,
        uint256 reserveIn,
        uint256 reserveOut
    ) public view returns (uint256 amountIn) {
        require(amountOut > 0, "Insufficient output");
        require(reserveIn > 0 && reserveOut > 0, "Insufficient liquidity");

        uint256 numerator = reserveIn * amountOut * FEE_DENOMINATOR;
        uint256 denominator = (reserveOut - amountOut) * (FEE_DENOMINATOR - swapFee);
        amountIn = (numerator / denominator) + 1;
    }

    /**
     * @notice Quote amount of token1 for token0
     */
    function quote(
        uint256 amountA,
        uint256 reserveA,
        uint256 reserveB
    ) public pure returns (uint256 amountB) {
        require(amountA > 0, "Insufficient amount");
        require(reserveA > 0 && reserveB > 0, "Insufficient liquidity");
        amountB = (amountA * reserveB) / reserveA;
    }

    // ============ Liquidity Functions ============

    /**
     * @notice Add liquidity to the pool
     * @param to Recipient of LP tokens
     * @return liquidity Amount of LP tokens minted
     */
    function mint(address to) external nonReentrant returns (uint256 liquidity) {
        (uint112 _reserve0, uint112 _reserve1,) = getReserves();
        uint256 balance0 = IERC20(token0).balanceOf(address(this));
        uint256 balance1 = IERC20(token1).balanceOf(address(this));
        uint256 amount0 = balance0 - _reserve0;
        uint256 amount1 = balance1 - _reserve1;

        // Mint protocol fee if applicable
        bool feeOn = _mintProtocolFee(_reserve0, _reserve1);
        uint256 _totalSupply = totalSupply();

        if (_totalSupply == 0) {
            liquidity = Math.sqrt(amount0 * amount1) - MINIMUM_LIQUIDITY;
            _mint(address(1), MINIMUM_LIQUIDITY); // Permanently lock first MINIMUM_LIQUIDITY
        } else {
            liquidity = Math.min(
                (amount0 * _totalSupply) / _reserve0,
                (amount1 * _totalSupply) / _reserve1
            );
        }

        if (liquidity == 0) revert InsufficientLiquidityMinted();
        _mint(to, liquidity);

        _update(balance0, balance1, _reserve0, _reserve1);
        if (feeOn) kLast = uint256(reserve0) * reserve1;

        emit Mint(msg.sender, amount0, amount1);
    }

    /**
     * @notice Remove liquidity from the pool
     * @param to Recipient of underlying tokens
     * @return amount0 Amount of token0 returned
     * @return amount1 Amount of token1 returned
     */
    function burn(address to) external nonReentrant returns (uint256 amount0, uint256 amount1) {
        (uint112 _reserve0, uint112 _reserve1,) = getReserves();
        address _token0 = token0;
        address _token1 = token1;
        uint256 balance0 = IERC20(_token0).balanceOf(address(this));
        uint256 balance1 = IERC20(_token1).balanceOf(address(this));
        uint256 liquidity = balanceOf(address(this));

        bool feeOn = _mintProtocolFee(_reserve0, _reserve1);
        uint256 _totalSupply = totalSupply();

        amount0 = (liquidity * balance0) / _totalSupply;
        amount1 = (liquidity * balance1) / _totalSupply;

        if (amount0 == 0 || amount1 == 0) revert InsufficientLiquidityBurned();

        _burn(address(this), liquidity);
        IERC20(_token0).safeTransfer(to, amount0);
        IERC20(_token1).safeTransfer(to, amount1);

        balance0 = IERC20(_token0).balanceOf(address(this));
        balance1 = IERC20(_token1).balanceOf(address(this));

        _update(balance0, balance1, _reserve0, _reserve1);
        if (feeOn) kLast = uint256(reserve0) * reserve1;

        emit Burn(msg.sender, amount0, amount1, to);
    }

    // ============ Swap Functions ============

    /**
     * @notice Swap tokens
     * @param amount0Out Amount of token0 to receive
     * @param amount1Out Amount of token1 to receive
     * @param to Recipient address
     * @param data Callback data for flash swaps
     */
    function swap(
        uint256 amount0Out,
        uint256 amount1Out,
        address to,
        bytes calldata data
    ) external nonReentrant {
        if (amount0Out == 0 && amount1Out == 0) revert InsufficientOutputAmount();
        if (to == token0 || to == token1) revert InvalidTo();

        (uint112 _reserve0, uint112 _reserve1,) = getReserves();
        if (amount0Out >= _reserve0 || amount1Out >= _reserve1) revert InsufficientLiquidity();

        uint256 balance0;
        uint256 balance1;
        {
            address _token0 = token0;
            address _token1 = token1;

            if (amount0Out > 0) IERC20(_token0).safeTransfer(to, amount0Out);
            if (amount1Out > 0) IERC20(_token1).safeTransfer(to, amount1Out);

            // Flash swap callback
            if (data.length > 0) {
                IFlashSwapCallback(to).flashSwapCallback(
                    msg.sender,
                    amount0Out,
                    amount1Out,
                    data
                );
            }

            balance0 = IERC20(_token0).balanceOf(address(this));
            balance1 = IERC20(_token1).balanceOf(address(this));
        }

        uint256 amount0In = balance0 > _reserve0 - amount0Out 
            ? balance0 - (_reserve0 - amount0Out) : 0;
        uint256 amount1In = balance1 > _reserve1 - amount1Out 
            ? balance1 - (_reserve1 - amount1Out) : 0;

        if (amount0In == 0 && amount1In == 0) revert InsufficientInputAmount();

        // Verify K (accounting for fees)
        {
            uint256 balance0Adjusted = balance0 * FEE_DENOMINATOR - amount0In * swapFee;
            uint256 balance1Adjusted = balance1 * FEE_DENOMINATOR - amount1In * swapFee;
            
            if (balance0Adjusted * balance1Adjusted < uint256(_reserve0) * _reserve1 * FEE_DENOMINATOR ** 2) {
                revert InvalidK();
            }
        }

        _update(balance0, balance1, _reserve0, _reserve1);

        emit Swap(msg.sender, amount0In, amount1In, amount0Out, amount1Out, to);
    }

    /**
     * @notice Convenient swap function with exact input
     * @param tokenIn Input token address
     * @param amountIn Input amount
     * @param minAmountOut Minimum output amount
     * @param to Recipient
     */
    function swapExactIn(
        address tokenIn,
        uint256 amountIn,
        uint256 minAmountOut,
        address to
    ) external nonReentrant returns (uint256 amountOut) {
        (uint112 _reserve0, uint112 _reserve1,) = getReserves();
        
        bool isToken0 = tokenIn == token0;
        (uint256 reserveIn, uint256 reserveOut) = isToken0 
            ? (uint256(_reserve0), uint256(_reserve1))
            : (uint256(_reserve1), uint256(_reserve0));

        IERC20(tokenIn).safeTransferFrom(msg.sender, address(this), amountIn);
        
        amountOut = getAmountOut(amountIn, reserveIn, reserveOut);
        require(amountOut >= minAmountOut, "Insufficient output");

        (uint256 amount0Out, uint256 amount1Out) = isToken0 
            ? (uint256(0), amountOut)
            : (amountOut, uint256(0));

        address tokenOut = isToken0 ? token1 : token0;
        IERC20(tokenOut).safeTransfer(to, amountOut);

        uint256 balance0 = IERC20(token0).balanceOf(address(this));
        uint256 balance1 = IERC20(token1).balanceOf(address(this));
        _update(balance0, balance1, _reserve0, _reserve1);

        emit Swap(msg.sender, isToken0 ? amountIn : 0, isToken0 ? 0 : amountIn, amount0Out, amount1Out, to);
    }

    // ============ Sync/Skim Functions ============

    /**
     * @notice Force reserves to match balances
     */
    function sync() external nonReentrant {
        _update(
            IERC20(token0).balanceOf(address(this)),
            IERC20(token1).balanceOf(address(this)),
            reserve0,
            reserve1
        );
    }

    /**
     * @notice Skim excess tokens to recipient
     */
    function skim(address to) external nonReentrant {
        address _token0 = token0;
        address _token1 = token1;
        IERC20(_token0).safeTransfer(to, IERC20(_token0).balanceOf(address(this)) - reserve0);
        IERC20(_token1).safeTransfer(to, IERC20(_token1).balanceOf(address(this)) - reserve1);
    }

    // ============ Internal Functions ============

    function _update(
        uint256 balance0,
        uint256 balance1,
        uint112 _reserve0,
        uint112 _reserve1
    ) private {
        if (balance0 > type(uint112).max || balance1 > type(uint112).max) revert Overflow();

        uint32 blockTimestamp = uint32(block.timestamp % 2**32);
        uint32 timeElapsed;
        unchecked {
            timeElapsed = blockTimestamp - blockTimestampLast;
        }

        // Update price accumulators for TWAP
        if (timeElapsed > 0 && _reserve0 != 0 && _reserve1 != 0) {
            unchecked {
                price0CumulativeLast += uint256(_reserve1) * Q112 / _reserve0 * timeElapsed;
                price1CumulativeLast += uint256(_reserve0) * Q112 / _reserve1 * timeElapsed;
            }
        }

        reserve0 = uint112(balance0);
        reserve1 = uint112(balance1);
        blockTimestampLast = blockTimestamp;

        emit Sync(reserve0, reserve1);
    }

    function _mintProtocolFee(uint112 _reserve0, uint112 _reserve1) private returns (bool feeOn) {
        address _feeRecipient = feeRecipient;
        feeOn = _feeRecipient != address(0);
        uint256 _kLast = kLast;

        if (feeOn) {
            if (_kLast != 0) {
                uint256 rootK = Math.sqrt(uint256(_reserve0) * _reserve1);
                uint256 rootKLast = Math.sqrt(_kLast);

                if (rootK > rootKLast) {
                    uint256 numerator = totalSupply() * (rootK - rootKLast);
                    uint256 denominator = rootK * protocolFeeDivisor + rootKLast;
                    uint256 liquidity = numerator / denominator;

                    if (liquidity > 0) {
                        _mint(_feeRecipient, liquidity);
                    }
                }
            }
        } else if (_kLast != 0) {
            kLast = 0;
        }
    }

    // ============ Admin Functions ============

    function setSwapFee(uint256 _fee) external onlyRole(FEE_MANAGER_ROLE) {
        require(_fee <= MAX_FEE, "Fee too high");
        swapFee = _fee;
    }

    function setProtocolFeeDivisor(uint256 _divisor) external onlyRole(FEE_MANAGER_ROLE) {
        require(_divisor >= 1, "Invalid divisor");
        protocolFeeDivisor = _divisor;
    }

    function setFeeRecipient(address _recipient) external onlyRole(DEFAULT_ADMIN_ROLE) {
        feeRecipient = _recipient;
    }
}

/**
 * @title IFlashSwapCallback
 * @notice Interface for flash swap callbacks
 */
interface IFlashSwapCallback {
    function flashSwapCallback(
        address sender,
        uint256 amount0,
        uint256 amount1,
        bytes calldata data
    ) external;
}
