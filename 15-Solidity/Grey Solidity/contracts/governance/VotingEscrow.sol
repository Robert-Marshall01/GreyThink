// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts-upgradeable/access/AccessControlUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/ReentrancyGuardUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/PausableUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/Initializable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/UUPSUpgradeable.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";

/**
 * @title VotingEscrow
 * @author Grey Protocol Team
 * @notice Vote-escrowed token mechanism (ve-token) for governance participation
 * @dev Users lock tokens for a period to receive voting power
 * 
 * Features:
 * - Token locking with time-weighted voting power
 * - Linear decay of voting power over time
 * - Lock extensions
 * - Early withdrawal with penalty
 * - Delegation capability
 * - Voting power snapshots
 * - Multiple lock positions per user
 */
contract VotingEscrow is
    Initializable,
    AccessControlUpgradeable,
    ReentrancyGuardUpgradeable,
    PausableUpgradeable,
    UUPSUpgradeable
{
    using SafeERC20 for IERC20;

    // ============================================
    // CONSTANTS & ROLES
    // ============================================

    bytes32 public constant ADMIN_ROLE = keccak256("ADMIN_ROLE");
    bytes32 public constant UPGRADER_ROLE = keccak256("UPGRADER_ROLE");

    /// @notice Maximum lock time (4 years)
    uint256 public constant MAX_LOCK_TIME = 4 * 365 days;

    /// @notice Minimum lock time (1 week)
    uint256 public constant MIN_LOCK_TIME = 7 days;

    /// @notice Early withdrawal penalty (20%)
    uint256 public constant EARLY_WITHDRAW_PENALTY = 2000;

    /// @notice Precision for calculations
    uint256 public constant PRECISION = 1e18;

    /// @notice Basis points denominator
    uint256 public constant BPS = 10000;

    // ============================================
    // STRUCTS
    // ============================================

    /**
     * @notice Lock position
     * @param id Position ID
     * @param amount Locked amount
     * @param start Lock start time
     * @param end Lock end time
     * @param initialPower Initial voting power
     * @param delegate Delegation address
     */
    struct Lock {
        uint256 id;
        uint256 amount;
        uint256 start;
        uint256 end;
        uint256 initialPower;
        address delegate;
    }

    /**
     * @notice Point for vote tracking
     */
    struct Point {
        int128 bias;
        int128 slope;
        uint256 timestamp;
        uint256 block;
    }

    /**
     * @notice User lock summary
     */
    struct UserLockSummary {
        uint256 totalLocked;
        uint256 totalVotingPower;
        uint256 lockCount;
        uint256 earliestUnlock;
        uint256 latestUnlock;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Token to lock
    IERC20 public token;

    /// @notice Name of the veToken
    string public name;

    /// @notice Symbol of the veToken
    string public symbol;

    /// @notice Decimals (same as underlying)
    uint8 public decimals;

    /// @notice Total supply of veToken
    uint256 public totalSupply;

    /// @notice Total locked tokens
    uint256 public totalLocked;

    /// @notice Lock counter for IDs
    uint256 public lockCounter;

    /// @notice User locks by ID
    mapping(address => mapping(uint256 => Lock)) public userLocks;

    /// @notice User lock IDs
    mapping(address => uint256[]) public userLockIds;

    /// @notice User voting power
    mapping(address => uint256) public votingPower;

    /// @notice Delegated voting power
    mapping(address => uint256) public delegatedPower;

    /// @notice User epoch for point tracking
    mapping(address => uint256) public userPointEpoch;

    /// @notice User points history
    mapping(address => mapping(uint256 => Point)) public userPointHistory;

    /// @notice Global epoch
    uint256 public epoch;

    /// @notice Global point history
    mapping(uint256 => Point) public pointHistory;

    /// @notice Slope changes at timestamps
    mapping(uint256 => int128) public slopeChanges;

    /// @notice Penalty collector
    address public penaltyCollector;

    /// @notice Total penalties collected
    uint256 public totalPenalties;

    /// @notice Storage gap
    uint256[40] private __gap;

    // ============================================
    // EVENTS
    // ============================================

    event TokenLocked(
        address indexed user,
        uint256 indexed lockId,
        uint256 amount,
        uint256 lockEnd,
        uint256 votingPower
    );

    event LockExtended(
        address indexed user,
        uint256 indexed lockId,
        uint256 newEnd,
        uint256 newVotingPower
    );

    event LockIncreased(
        address indexed user,
        uint256 indexed lockId,
        uint256 addedAmount,
        uint256 newVotingPower
    );

    event Withdrawn(
        address indexed user,
        uint256 indexed lockId,
        uint256 amount,
        bool early,
        uint256 penalty
    );

    event DelegateChanged(
        address indexed user,
        uint256 indexed lockId,
        address indexed delegate
    );

    event PenaltyCollectorUpdated(address indexed collector);

    // ============================================
    // ERRORS
    // ============================================

    error InvalidAmount();
    error InvalidLockTime(uint256 time);
    error LockNotExpired(uint256 end);
    error LockExpired(uint256 end);
    error LockNotFound(uint256 lockId);
    error NotOwner();
    error CannotDelegate();
    error ZeroAddress();
    error LockTooShort(uint256 duration, uint256 minimum);
    error LockTooLong(uint256 duration, uint256 maximum);

    // ============================================
    // INITIALIZER
    // ============================================

    /**
     * @notice Initializes the voting escrow
     * @param _token Token to lock
     * @param _name veToken name
     * @param _symbol veToken symbol
     * @param admin Admin address
     */
    function initialize(
        address _token,
        string memory _name,
        string memory _symbol,
        address admin
    ) public initializer {
        if (_token == address(0)) revert ZeroAddress();
        if (admin == address(0)) revert ZeroAddress();

        __AccessControl_init();
        __ReentrancyGuard_init();
        __Pausable_init();
        __UUPSUpgradeable_init();

        token = IERC20(_token);
        name = _name;
        symbol = _symbol;
        decimals = 18;
        penaltyCollector = admin;

        _grantRole(DEFAULT_ADMIN_ROLE, admin);
        _grantRole(ADMIN_ROLE, admin);
        _grantRole(UPGRADER_ROLE, admin);

        // Initialize point history
        pointHistory[0] = Point({
            bias: 0,
            slope: 0,
            timestamp: block.timestamp,
            block: block.number
        });
    }

    // ============================================
    // LOCKING
    // ============================================

    /**
     * @notice Creates a new lock
     * @param amount Amount to lock
     * @param lockDuration Lock duration in seconds
     * @return lockId The lock ID
     */
    function createLock(
        uint256 amount,
        uint256 lockDuration
    ) external nonReentrant whenNotPaused returns (uint256 lockId) {
        if (amount == 0) revert InvalidAmount();
        if (lockDuration < MIN_LOCK_TIME) {
            revert LockTooShort(lockDuration, MIN_LOCK_TIME);
        }
        if (lockDuration > MAX_LOCK_TIME) {
            revert LockTooLong(lockDuration, MAX_LOCK_TIME);
        }

        // Transfer tokens
        token.safeTransferFrom(msg.sender, address(this), amount);

        // Calculate lock end (rounded to week)
        uint256 unlockTime = ((block.timestamp + lockDuration) / 1 weeks) * 1 weeks;
        
        // Calculate voting power (linear based on lock time)
        uint256 power = _calculatePower(amount, unlockTime - block.timestamp);

        lockId = ++lockCounter;

        userLocks[msg.sender][lockId] = Lock({
            id: lockId,
            amount: amount,
            start: block.timestamp,
            end: unlockTime,
            initialPower: power,
            delegate: msg.sender
        });

        userLockIds[msg.sender].push(lockId);
        totalLocked += amount;
        
        _updateVotingPower(msg.sender, power, true);
        _checkpoint(msg.sender, amount, unlockTime);

        emit TokenLocked(msg.sender, lockId, amount, unlockTime, power);
    }

    /**
     * @notice Increases lock amount
     * @param lockId Lock ID
     * @param amount Additional amount
     */
    function increaseLock(
        uint256 lockId,
        uint256 amount
    ) external nonReentrant whenNotPaused {
        if (amount == 0) revert InvalidAmount();

        Lock storage lock = userLocks[msg.sender][lockId];
        if (lock.id == 0) revert LockNotFound(lockId);
        if (block.timestamp >= lock.end) revert LockExpired(lock.end);

        // Transfer tokens
        token.safeTransferFrom(msg.sender, address(this), amount);

        // Recalculate voting power
        uint256 oldPower = _calculatePower(lock.amount, lock.end - block.timestamp);
        lock.amount += amount;
        uint256 newPower = _calculatePower(lock.amount, lock.end - block.timestamp);

        totalLocked += amount;
        _updateVotingPower(lock.delegate, newPower - oldPower, true);

        emit LockIncreased(msg.sender, lockId, amount, newPower);
    }

    /**
     * @notice Extends lock duration
     * @param lockId Lock ID
     * @param newDuration New total duration from now
     */
    function extendLock(
        uint256 lockId,
        uint256 newDuration
    ) external nonReentrant whenNotPaused {
        Lock storage lock = userLocks[msg.sender][lockId];
        if (lock.id == 0) revert LockNotFound(lockId);
        
        // Can extend even if expired
        uint256 newEnd = ((block.timestamp + newDuration) / 1 weeks) * 1 weeks;
        if (newEnd <= lock.end) revert InvalidLockTime(newDuration);
        if (newDuration > MAX_LOCK_TIME) {
            revert LockTooLong(newDuration, MAX_LOCK_TIME);
        }

        // Recalculate voting power
        uint256 oldPower = _currentPower(lock);
        lock.end = newEnd;
        lock.start = block.timestamp;
        uint256 newPower = _calculatePower(lock.amount, lock.end - block.timestamp);
        lock.initialPower = newPower;

        if (newPower > oldPower) {
            _updateVotingPower(lock.delegate, newPower - oldPower, true);
        }

        _checkpoint(msg.sender, lock.amount, newEnd);

        emit LockExtended(msg.sender, lockId, newEnd, newPower);
    }

    // ============================================
    // WITHDRAWAL
    // ============================================

    /**
     * @notice Withdraws tokens from an expired lock
     * @param lockId Lock ID
     */
    function withdraw(uint256 lockId) external nonReentrant {
        Lock storage lock = userLocks[msg.sender][lockId];
        if (lock.id == 0) revert LockNotFound(lockId);
        if (block.timestamp < lock.end) revert LockNotExpired(lock.end);

        uint256 amount = lock.amount;
        address delegate = lock.delegate;

        // Clear voting power
        uint256 remainingPower = _currentPower(lock);
        if (remainingPower > 0) {
            _updateVotingPower(delegate, remainingPower, false);
        }

        // Clear lock
        totalLocked -= amount;
        delete userLocks[msg.sender][lockId];

        // Transfer tokens
        token.safeTransfer(msg.sender, amount);

        emit Withdrawn(msg.sender, lockId, amount, false, 0);
    }

    /**
     * @notice Early withdrawal with penalty
     * @param lockId Lock ID
     */
    function withdrawEarly(uint256 lockId) external nonReentrant {
        Lock storage lock = userLocks[msg.sender][lockId];
        if (lock.id == 0) revert LockNotFound(lockId);
        if (block.timestamp >= lock.end) {
            // Just use normal withdraw
            revert LockExpired(lock.end);
        }

        uint256 amount = lock.amount;
        address delegate = lock.delegate;

        // Calculate penalty based on remaining time
        uint256 remainingTime = lock.end - block.timestamp;
        uint256 maxTime = MAX_LOCK_TIME;
        uint256 penaltyRate = (remainingTime * EARLY_WITHDRAW_PENALTY) / maxTime;
        if (penaltyRate > EARLY_WITHDRAW_PENALTY) {
            penaltyRate = EARLY_WITHDRAW_PENALTY;
        }
        
        uint256 penalty = (amount * penaltyRate) / BPS;
        uint256 withdrawAmount = amount - penalty;

        // Clear voting power
        uint256 remainingPower = _currentPower(lock);
        _updateVotingPower(delegate, remainingPower, false);

        // Clear lock
        totalLocked -= amount;
        totalPenalties += penalty;
        delete userLocks[msg.sender][lockId];

        // Transfer tokens
        token.safeTransfer(msg.sender, withdrawAmount);
        if (penalty > 0 && penaltyCollector != address(0)) {
            token.safeTransfer(penaltyCollector, penalty);
        }

        emit Withdrawn(msg.sender, lockId, withdrawAmount, true, penalty);
    }

    // ============================================
    // DELEGATION
    // ============================================

    /**
     * @notice Delegates voting power of a lock
     * @param lockId Lock ID
     * @param newDelegate New delegate address
     */
    function delegate(uint256 lockId, address newDelegate) external {
        if (newDelegate == address(0)) revert ZeroAddress();

        Lock storage lock = userLocks[msg.sender][lockId];
        if (lock.id == 0) revert LockNotFound(lockId);
        if (block.timestamp >= lock.end) revert LockExpired(lock.end);

        address oldDelegate = lock.delegate;
        if (oldDelegate == newDelegate) return;

        uint256 power = _currentPower(lock);
        
        // Move power
        _updateVotingPower(oldDelegate, power, false);
        _updateVotingPower(newDelegate, power, true);
        
        lock.delegate = newDelegate;

        emit DelegateChanged(msg.sender, lockId, newDelegate);
    }

    // ============================================
    // INTERNAL FUNCTIONS
    // ============================================

    /**
     * @notice Calculates voting power based on amount and time
     */
    function _calculatePower(
        uint256 amount,
        uint256 duration
    ) internal pure returns (uint256) {
        // Linear: full power for MAX_LOCK_TIME
        return (amount * duration) / MAX_LOCK_TIME;
    }

    /**
     * @notice Calculates current power of a lock (with decay)
     */
    function _currentPower(Lock storage lock) internal view returns (uint256) {
        if (block.timestamp >= lock.end) return 0;
        uint256 remaining = lock.end - block.timestamp;
        return _calculatePower(lock.amount, remaining);
    }

    /**
     * @notice Updates voting power
     */
    function _updateVotingPower(
        address user,
        uint256 amount,
        bool add
    ) internal {
        if (add) {
            votingPower[user] += amount;
            totalSupply += amount;
        } else {
            if (votingPower[user] >= amount) {
                votingPower[user] -= amount;
            } else {
                votingPower[user] = 0;
            }
            if (totalSupply >= amount) {
                totalSupply -= amount;
            } else {
                totalSupply = 0;
            }
        }
    }

    /**
     * @notice Records a checkpoint for historical queries
     */
    function _checkpoint(
        address user,
        uint256 amount,
        uint256 unlockTime
    ) internal {
        Point memory uOld = Point(0, 0, 0, 0);
        Point memory uNew = Point(0, 0, 0, 0);
        
        int128 slope = int128(int256(amount / MAX_LOCK_TIME));
        int128 bias = slope * int128(int256(unlockTime - block.timestamp));

        uNew.slope = slope;
        uNew.bias = bias;
        uNew.timestamp = block.timestamp;
        uNew.block = block.number;

        uint256 userEpoch = ++userPointEpoch[user];
        userPointHistory[user][userEpoch] = uNew;

        // Update global
        epoch++;
        pointHistory[epoch] = Point({
            bias: pointHistory[epoch - 1].bias + uNew.bias - uOld.bias,
            slope: pointHistory[epoch - 1].slope + uNew.slope - uOld.slope,
            timestamp: block.timestamp,
            block: block.number
        });

        // Schedule slope change
        slopeChanges[unlockTime] += slope;
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Gets user balance (current voting power)
     */
    function balanceOf(address user) external view returns (uint256) {
        return votingPower[user];
    }

    /**
     * @notice Gets balance at a past timestamp
     */
    function balanceOfAt(address user, uint256 timestamp) external view returns (uint256) {
        uint256 epoch_ = userPointEpoch[user];
        if (epoch_ == 0) return 0;

        // Binary search for the right epoch
        uint256 min = 0;
        uint256 max = epoch_;
        
        for (uint256 i = 0; i < 128; i++) {
            if (min >= max) break;
            uint256 mid = (min + max + 1) / 2;
            if (userPointHistory[user][mid].timestamp <= timestamp) {
                min = mid;
            } else {
                max = mid - 1;
            }
        }

        Point memory point = userPointHistory[user][min];
        int128 bias = point.bias - point.slope * int128(int256(timestamp - point.timestamp));
        if (bias < 0) bias = 0;
        
        return uint256(int256(bias));
    }

    /**
     * @notice Gets user lock summary
     */
    function getUserLockSummary(address user) external view returns (UserLockSummary memory summary) {
        uint256[] storage lockIds = userLockIds[user];
        uint256 earliest = type(uint256).max;
        uint256 latest = 0;

        for (uint256 i = 0; i < lockIds.length; i++) {
            Lock storage lock = userLocks[user][lockIds[i]];
            if (lock.id == 0) continue;
            
            summary.totalLocked += lock.amount;
            summary.totalVotingPower += _currentPower(lock);
            summary.lockCount++;
            
            if (lock.end < earliest) earliest = lock.end;
            if (lock.end > latest) latest = lock.end;
        }

        summary.earliestUnlock = earliest == type(uint256).max ? 0 : earliest;
        summary.latestUnlock = latest;
    }

    /**
     * @notice Gets lock details
     */
    function getLock(address user, uint256 lockId) external view returns (Lock memory) {
        return userLocks[user][lockId];
    }

    /**
     * @notice Gets all user lock IDs
     */
    function getUserLockIds(address user) external view returns (uint256[] memory) {
        return userLockIds[user];
    }

    /**
     * @notice Gets total supply at timestamp
     */
    function totalSupplyAt(uint256 timestamp) external view returns (uint256) {
        uint256 epoch_ = epoch;
        
        // Binary search
        uint256 min = 0;
        uint256 max = epoch_;
        
        for (uint256 i = 0; i < 128; i++) {
            if (min >= max) break;
            uint256 mid = (min + max + 1) / 2;
            if (pointHistory[mid].timestamp <= timestamp) {
                min = mid;
            } else {
                max = mid - 1;
            }
        }

        Point memory point = pointHistory[min];
        int128 bias = point.bias - point.slope * int128(int256(timestamp - point.timestamp));
        if (bias < 0) bias = 0;
        
        return uint256(int256(bias));
    }

    // ============================================
    // ADMIN
    // ============================================

    /**
     * @notice Sets penalty collector
     */
    function setPenaltyCollector(address collector) external onlyRole(ADMIN_ROLE) {
        penaltyCollector = collector;
        emit PenaltyCollectorUpdated(collector);
    }

    function pause() external onlyRole(ADMIN_ROLE) { _pause(); }
    function unpause() external onlyRole(ADMIN_ROLE) { _unpause(); }
    function _authorizeUpgrade(address) internal override onlyRole(UPGRADER_ROLE) {}
}
