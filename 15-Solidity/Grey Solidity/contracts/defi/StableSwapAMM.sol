// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/token/ERC20/ERC20.sol";
import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/math/Math.sol";

/**
 * @title StableSwapAMM
 * @notice Curve-style StableSwap AMM for pegged assets
 * @dev Implements the StableSwap invariant: A*n^n * sum(x_i) + D = A*D*n^n + D^(n+1) / (n^n * prod(x_i))
 *
 * Features:
 * - StableSwap invariant for low-slippage stablecoin swaps
 * - Dynamic amplification coefficient (A)
 * - Multi-asset pools (2-4 tokens)
 * - Admin fee extraction
 * - Virtual price tracking
 * - Ramping A parameter safely
 */
contract StableSwapAMM is ERC20, AccessControl, ReentrancyGuard {
    using SafeERC20 for IERC20;

    // ============ Constants ============

    bytes32 public constant FEE_MANAGER_ROLE = keccak256("FEE_MANAGER_ROLE");
    bytes32 public constant PARAMETER_ADMIN_ROLE = keccak256("PARAMETER_ADMIN_ROLE");

    uint256 public constant MAX_COINS = 4;
    uint256 public constant FEE_DENOMINATOR = 10**10;
    uint256 public constant PRECISION = 10**18;
    uint256 public constant A_PRECISION = 100;
    
    uint256 public constant MAX_A = 10**6;
    uint256 public constant MAX_A_CHANGE = 10;
    uint256 public constant MIN_RAMP_TIME = 1 days;
    uint256 public constant MAX_FEE = 5 * 10**9; // 50%
    uint256 public constant MAX_ADMIN_FEE = 10**10; // 100% of fee

    // ============ State Variables ============

    /// @notice Number of tokens in pool
    uint256 public immutable nCoins;

    /// @notice Pool tokens (ordered)
    address[] public coins;

    /// @notice Token decimal multipliers for normalization
    uint256[] public precisionMultipliers;

    /// @notice Token balances (normalized)
    uint256[] public balances;

    /// @notice Current amplification coefficient
    uint256 public initialA;
    uint256 public futureA;
    uint256 public initialATime;
    uint256 public futureATime;

    /// @notice Swap fee (in fee denominator units)
    uint256 public fee = 4000000; // 0.04%

    /// @notice Admin fee (percentage of swap fee)
    uint256 public adminFee = 5000000000; // 50%

    /// @notice Accumulated admin fees per token
    uint256[] public adminBalances;

    /// @notice Initial virtual price (for tracking)
    uint256 public initialVirtualPrice;

    /// @notice Kill switch
    bool public isKilled;

    // ============ Events ============

    event TokenExchange(
        address indexed buyer,
        uint256 soldId,
        uint256 tokensSold,
        uint256 boughtId,
        uint256 tokensBought
    );
    event AddLiquidity(
        address indexed provider,
        uint256[] tokenAmounts,
        uint256[] fees,
        uint256 invariant,
        uint256 lpTokenSupply
    );
    event RemoveLiquidity(
        address indexed provider,
        uint256[] tokenAmounts,
        uint256[] fees,
        uint256 lpTokenSupply
    );
    event RemoveLiquidityOne(
        address indexed provider,
        uint256 tokenAmount,
        uint256 coinAmount,
        uint256 coinIndex
    );
    event RemoveLiquidityImbalance(
        address indexed provider,
        uint256[] tokenAmounts,
        uint256[] fees,
        uint256 invariant,
        uint256 lpTokenSupply
    );
    event RampA(uint256 oldA, uint256 newA, uint256 initialTime, uint256 futureTime);
    event StopRampA(uint256 A, uint256 t);
    event CommitNewFee(uint256 newFee, uint256 newAdminFee);
    event NewFee(uint256 fee, uint256 adminFee);
    event AdminFeeCollected(address indexed token, uint256 amount);

    // ============ Errors ============

    error PoolKilled();
    error InvalidCoinIndex();
    error InvalidAmounts();
    error SlippageExceeded();
    error InvariantNotMet();
    error RampInProgress();
    error InsufficientRampTime();
    error AValueOutOfRange();
    error FeeOutOfRange();
    error ZeroAmount();
    error SameCoin();

    // ============ Constructor ============

    constructor(
        string memory _name,
        string memory _symbol,
        address[] memory _coins,
        uint256 _A,
        uint256 _fee,
        address _admin
    ) ERC20(_name, _symbol) {
        require(_coins.length >= 2 && _coins.length <= MAX_COINS, "Invalid coin count");
        require(_A <= MAX_A, "A too high");
        require(_fee <= MAX_FEE, "Fee too high");

        nCoins = _coins.length;
        coins = _coins;
        
        // Initialize arrays
        balances = new uint256[](_coins.length);
        adminBalances = new uint256[](_coins.length);
        precisionMultipliers = new uint256[](_coins.length);

        // Set precision multipliers based on decimals
        for (uint256 i = 0; i < _coins.length; i++) {
            uint8 coinDecimals = _getDecimals(_coins[i]);
            precisionMultipliers[i] = 10 ** (18 - coinDecimals);
        }

        initialA = _A * A_PRECISION;
        futureA = _A * A_PRECISION;
        fee = _fee;

        _grantRole(DEFAULT_ADMIN_ROLE, _admin);
        _grantRole(FEE_MANAGER_ROLE, _admin);
        _grantRole(PARAMETER_ADMIN_ROLE, _admin);
    }

    // ============ View Functions ============

    /**
     * @notice Get current amplification coefficient
     */
    function getA() public view returns (uint256) {
        uint256 t1 = futureATime;
        uint256 A1 = futureA;

        if (block.timestamp < t1) {
            uint256 t0 = initialATime;
            uint256 A0 = initialA;

            if (A1 > A0) {
                return A0 + (A1 - A0) * (block.timestamp - t0) / (t1 - t0);
            } else {
                return A0 - (A0 - A1) * (block.timestamp - t0) / (t1 - t0);
            }
        }
        return A1;
    }

    /**
     * @notice Get A in precise units
     */
    function getAPrecise() external view returns (uint256) {
        return getA();
    }

    /**
     * @notice Calculate the current virtual price
     * @return Virtual price in 1e18
     */
    function getVirtualPrice() external view returns (uint256) {
        uint256 D = _getD(_xp(), getA());
        uint256 supply = totalSupply();
        if (supply == 0) return PRECISION;
        return D * PRECISION / supply;
    }

    /**
     * @notice Calculate swap output
     */
    function getExchangeAmount(
        uint256 i,
        uint256 j,
        uint256 dx
    ) external view returns (uint256) {
        uint256[] memory xp = _xp();
        uint256 x = xp[i] + dx * precisionMultipliers[i];
        uint256 y = _getY(i, j, x, xp, getA());
        uint256 dy = xp[j] - y - 1;
        uint256 feeAmount = dy * fee / FEE_DENOMINATOR;
        return (dy - feeAmount) / precisionMultipliers[j];
    }

    /**
     * @notice Calculate LP tokens for adding liquidity
     */
    function calcTokenAmount(
        uint256[] calldata amounts,
        bool isDeposit
    ) external view returns (uint256) {
        uint256 amp = getA();
        uint256[] memory xp = _xp();
        uint256 D0 = _getD(xp, amp);

        for (uint256 i = 0; i < nCoins; i++) {
            if (isDeposit) {
                xp[i] += amounts[i] * precisionMultipliers[i];
            } else {
                xp[i] -= amounts[i] * precisionMultipliers[i];
            }
        }

        uint256 D1 = _getD(xp, amp);
        uint256 supply = totalSupply();

        if (supply == 0) {
            return D1;
        }

        uint256 diff = isDeposit ? D1 - D0 : D0 - D1;
        return diff * supply / D0;
    }

    /**
     * @notice Calculate withdrawal amount for single token
     */
    function calcWithdrawOneCoin(
        uint256 tokenAmount,
        uint256 i
    ) external view returns (uint256) {
        (uint256 dy,) = _calcWithdrawOneCoin(tokenAmount, i);
        return dy;
    }

    // ============ Liquidity Functions ============

    /**
     * @notice Add liquidity to the pool
     * @param amounts Token amounts to add
     * @param minMintAmount Minimum LP tokens to receive
     * @return LP tokens minted
     */
    function addLiquidity(
        uint256[] calldata amounts,
        uint256 minMintAmount
    ) external nonReentrant returns (uint256) {
        if (isKilled) revert PoolKilled();
        if (amounts.length != nCoins) revert InvalidAmounts();

        uint256 amp = getA();
        uint256[] memory xp = _xp();
        uint256 D0 = totalSupply() == 0 ? 0 : _getD(xp, amp);

        // Transfer tokens and update balances
        uint256[] memory newBalances = new uint256[](nCoins);
        for (uint256 i = 0; i < nCoins; i++) {
            if (totalSupply() == 0 && amounts[i] == 0) revert ZeroAmount();
            
            if (amounts[i] > 0) {
                IERC20(coins[i]).safeTransferFrom(msg.sender, address(this), amounts[i]);
            }
            newBalances[i] = balances[i] + amounts[i];
        }

        // Calculate new D
        for (uint256 i = 0; i < nCoins; i++) {
            xp[i] = newBalances[i] * precisionMultipliers[i];
        }
        uint256 D1 = _getD(xp, amp);
        if (D1 <= D0) revert InvariantNotMet();

        // Calculate fees
        uint256[] memory fees = new uint256[](nCoins);
        uint256 mintAmount;

        if (totalSupply() > 0) {
            uint256 feePerToken = fee * nCoins / (4 * (nCoins - 1));
            
            for (uint256 i = 0; i < nCoins; i++) {
                uint256 idealBalance = D1 * balances[i] / D0;
                uint256 difference = idealBalance > newBalances[i] 
                    ? idealBalance - newBalances[i] 
                    : newBalances[i] - idealBalance;
                fees[i] = feePerToken * difference / FEE_DENOMINATOR;
                adminBalances[i] += fees[i] * adminFee / FEE_DENOMINATOR;
                newBalances[i] -= fees[i];
            }

            // Recalculate D with fees
            for (uint256 i = 0; i < nCoins; i++) {
                xp[i] = newBalances[i] * precisionMultipliers[i];
            }
            uint256 D2 = _getD(xp, amp);
            mintAmount = totalSupply() * (D2 - D0) / D0;
        } else {
            mintAmount = D1;
            initialVirtualPrice = PRECISION;
        }

        if (mintAmount < minMintAmount) revert SlippageExceeded();

        // Update balances
        for (uint256 i = 0; i < nCoins; i++) {
            balances[i] = newBalances[i];
        }

        _mint(msg.sender, mintAmount);

        emit AddLiquidity(msg.sender, amounts, fees, D1, totalSupply());
        return mintAmount;
    }

    /**
     * @notice Remove liquidity proportionally
     * @param amount LP tokens to burn
     * @param minAmounts Minimum amounts to receive
     * @return amounts Tokens received
     */
    function removeLiquidity(
        uint256 amount,
        uint256[] calldata minAmounts
    ) external nonReentrant returns (uint256[] memory amounts) {
        if (minAmounts.length != nCoins) revert InvalidAmounts();

        uint256 supply = totalSupply();
        amounts = new uint256[](nCoins);
        uint256[] memory fees = new uint256[](nCoins);

        for (uint256 i = 0; i < nCoins; i++) {
            amounts[i] = balances[i] * amount / supply;
            if (amounts[i] < minAmounts[i]) revert SlippageExceeded();
            balances[i] -= amounts[i];
            IERC20(coins[i]).safeTransfer(msg.sender, amounts[i]);
        }

        _burn(msg.sender, amount);

        emit RemoveLiquidity(msg.sender, amounts, fees, totalSupply());
    }

    /**
     * @notice Remove liquidity for a single token
     * @param tokenAmount LP tokens to burn
     * @param i Token index
     * @param minAmount Minimum tokens to receive
     */
    function removeLiquidityOneCoin(
        uint256 tokenAmount,
        uint256 i,
        uint256 minAmount
    ) external nonReentrant returns (uint256) {
        if (i >= nCoins) revert InvalidCoinIndex();

        (uint256 dy, uint256 dyFee) = _calcWithdrawOneCoin(tokenAmount, i);
        if (dy < minAmount) revert SlippageExceeded();

        balances[i] -= (dy + dyFee * adminFee / FEE_DENOMINATOR);
        adminBalances[i] += dyFee * adminFee / FEE_DENOMINATOR;

        _burn(msg.sender, tokenAmount);
        IERC20(coins[i]).safeTransfer(msg.sender, dy);

        emit RemoveLiquidityOne(msg.sender, tokenAmount, dy, i);
        return dy;
    }

    // ============ Swap Functions ============

    /**
     * @notice Swap tokens
     * @param i Input token index
     * @param j Output token index
     * @param dx Input amount
     * @param minDy Minimum output amount
     */
    function exchange(
        uint256 i,
        uint256 j,
        uint256 dx,
        uint256 minDy
    ) external nonReentrant returns (uint256) {
        if (isKilled) revert PoolKilled();
        if (i == j) revert SameCoin();
        if (i >= nCoins || j >= nCoins) revert InvalidCoinIndex();

        uint256 amp = getA();
        uint256[] memory xp = _xp();

        // Transfer input
        IERC20(coins[i]).safeTransferFrom(msg.sender, address(this), dx);
        
        uint256 x = xp[i] + dx * precisionMultipliers[i];
        uint256 y = _getY(i, j, x, xp, amp);
        uint256 dy = xp[j] - y - 1;
        uint256 dyFee = dy * fee / FEE_DENOMINATOR;

        dy = (dy - dyFee) / precisionMultipliers[j];
        if (dy < minDy) revert SlippageExceeded();

        // Update admin fees
        uint256 dyAdminFee = dyFee * adminFee / FEE_DENOMINATOR / precisionMultipliers[j];
        adminBalances[j] += dyAdminFee;

        // Update balances
        balances[i] += dx;
        balances[j] -= dy + dyAdminFee;

        IERC20(coins[j]).safeTransfer(msg.sender, dy);

        emit TokenExchange(msg.sender, i, dx, j, dy);
        return dy;
    }

    // ============ Internal Functions ============

    function _xp() internal view returns (uint256[] memory xp) {
        xp = new uint256[](nCoins);
        for (uint256 i = 0; i < nCoins; i++) {
            xp[i] = balances[i] * precisionMultipliers[i];
        }
    }

    /**
     * @notice Calculate D (invariant)
     * @dev Newton's method: D^(n+1) / (n^n * prod(x_i)) + A*n^n*sum(x_i) - A*n^n*D - D = 0
     */
    function _getD(uint256[] memory xp, uint256 amp) internal view returns (uint256) {
        uint256 S = 0;
        for (uint256 i = 0; i < nCoins; i++) {
            S += xp[i];
        }
        if (S == 0) return 0;

        uint256 D = S;
        uint256 Ann = amp * nCoins;

        for (uint256 iter = 0; iter < 255; iter++) {
            uint256 D_P = D;
            for (uint256 i = 0; i < nCoins; i++) {
                D_P = D_P * D / (xp[i] * nCoins);
            }
            uint256 Dprev = D;
            D = (Ann * S / A_PRECISION + D_P * nCoins) * D / 
                ((Ann - A_PRECISION) * D / A_PRECISION + (nCoins + 1) * D_P);

            if (D > Dprev) {
                if (D - Dprev <= 1) return D;
            } else {
                if (Dprev - D <= 1) return D;
            }
        }
        revert InvariantNotMet();
    }

    /**
     * @notice Calculate y given x
     * @dev Solve for y using Newton's method
     */
    function _getY(
        uint256 i,
        uint256 j,
        uint256 x,
        uint256[] memory xp,
        uint256 amp
    ) internal view returns (uint256) {
        uint256 D = _getD(xp, amp);
        uint256 Ann = amp * nCoins;
        uint256 c = D;
        uint256 S = 0;

        for (uint256 k = 0; k < nCoins; k++) {
            uint256 _x;
            if (k == i) {
                _x = x;
            } else if (k == j) {
                continue;
            } else {
                _x = xp[k];
            }
            S += _x;
            c = c * D / (_x * nCoins);
        }

        c = c * D * A_PRECISION / (Ann * nCoins);
        uint256 b = S + D * A_PRECISION / Ann;
        uint256 y = D;

        for (uint256 iter = 0; iter < 255; iter++) {
            uint256 yPrev = y;
            y = (y * y + c) / (2 * y + b - D);

            if (y > yPrev) {
                if (y - yPrev <= 1) return y;
            } else {
                if (yPrev - y <= 1) return y;
            }
        }
        revert InvariantNotMet();
    }

    function _calcWithdrawOneCoin(
        uint256 tokenAmount,
        uint256 i
    ) internal view returns (uint256, uint256) {
        uint256 amp = getA();
        uint256[] memory xp = _xp();
        uint256 D0 = _getD(xp, amp);
        uint256 D1 = D0 - tokenAmount * D0 / totalSupply();
        uint256 newY = _getYD(i, xp, D1, amp);
        
        uint256 feePerToken = fee * nCoins / (4 * (nCoins - 1));
        uint256[] memory xpReduced = new uint256[](nCoins);

        for (uint256 k = 0; k < nCoins; k++) {
            uint256 dxExpected = k == i 
                ? xp[k] * D1 / D0 - newY 
                : xp[k] - xp[k] * D1 / D0;
            xpReduced[k] = xp[k] - feePerToken * dxExpected / FEE_DENOMINATOR;
        }

        uint256 dy = xpReduced[i] - _getYD(i, xpReduced, D1, amp);
        dy = (dy - 1) / precisionMultipliers[i];
        uint256 dyFee = (xp[i] - newY) / precisionMultipliers[i] - dy;

        return (dy, dyFee);
    }

    function _getYD(
        uint256 i,
        uint256[] memory xp,
        uint256 D,
        uint256 amp
    ) internal view returns (uint256) {
        uint256 Ann = amp * nCoins;
        uint256 c = D;
        uint256 S = 0;

        for (uint256 k = 0; k < nCoins; k++) {
            if (k == i) continue;
            S += xp[k];
            c = c * D / (xp[k] * nCoins);
        }

        c = c * D * A_PRECISION / (Ann * nCoins);
        uint256 b = S + D * A_PRECISION / Ann;
        uint256 y = D;

        for (uint256 iter = 0; iter < 255; iter++) {
            uint256 yPrev = y;
            y = (y * y + c) / (2 * y + b - D);

            if (y > yPrev) {
                if (y - yPrev <= 1) return y;
            } else {
                if (yPrev - y <= 1) return y;
            }
        }
        revert InvariantNotMet();
    }

    function _getDecimals(address token) internal view returns (uint8) {
        (bool success, bytes memory data) = token.staticcall(
            abi.encodeWithSignature("decimals()")
        );
        if (success && data.length >= 32) {
            return abi.decode(data, (uint8));
        }
        return 18;
    }

    // ============ Admin Functions ============

    /**
     * @notice Ramp A parameter
     */
    function rampA(uint256 futureA_, uint256 futureTime) external onlyRole(PARAMETER_ADMIN_ROLE) {
        if (block.timestamp < initialATime + MIN_RAMP_TIME) revert RampInProgress();
        if (futureTime < block.timestamp + MIN_RAMP_TIME) revert InsufficientRampTime();

        uint256 _initialA = getA();
        uint256 _futureA = futureA_ * A_PRECISION;

        if (_futureA == 0 || _futureA > MAX_A * A_PRECISION) revert AValueOutOfRange();
        if (
            (_futureA >= _initialA && _futureA > _initialA * MAX_A_CHANGE) ||
            (_futureA < _initialA && _futureA * MAX_A_CHANGE < _initialA)
        ) revert AValueOutOfRange();

        initialA = _initialA;
        futureA = _futureA;
        initialATime = block.timestamp;
        futureATime = futureTime;

        emit RampA(_initialA, _futureA, block.timestamp, futureTime);
    }

    /**
     * @notice Stop ramping A
     */
    function stopRampA() external onlyRole(PARAMETER_ADMIN_ROLE) {
        uint256 currentA = getA();
        initialA = currentA;
        futureA = currentA;
        initialATime = block.timestamp;
        futureATime = block.timestamp;

        emit StopRampA(currentA, block.timestamp);
    }

    /**
     * @notice Set fees
     */
    function setFees(uint256 _fee, uint256 _adminFee) external onlyRole(FEE_MANAGER_ROLE) {
        if (_fee > MAX_FEE) revert FeeOutOfRange();
        if (_adminFee > MAX_ADMIN_FEE) revert FeeOutOfRange();
        fee = _fee;
        adminFee = _adminFee;
        emit NewFee(_fee, _adminFee);
    }

    /**
     * @notice Withdraw admin fees
     */
    function withdrawAdminFees() external onlyRole(DEFAULT_ADMIN_ROLE) {
        for (uint256 i = 0; i < nCoins; i++) {
            uint256 amount = adminBalances[i];
            if (amount > 0) {
                adminBalances[i] = 0;
                IERC20(coins[i]).safeTransfer(msg.sender, amount);
                emit AdminFeeCollected(coins[i], amount);
            }
        }
    }

    /**
     * @notice Kill pool (emergency)
     */
    function killPool() external onlyRole(DEFAULT_ADMIN_ROLE) {
        isKilled = true;
    }

    /**
     * @notice Unkill pool
     */
    function unkillPool() external onlyRole(DEFAULT_ADMIN_ROLE) {
        isKilled = false;
    }
}
