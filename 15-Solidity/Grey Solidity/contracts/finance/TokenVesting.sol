// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/Pausable.sol";

/**
 * @title TokenVesting
 * @author Grey Solidity Project
 * @notice Manages token vesting schedules with linear and cliff vesting
 * @dev Supports multiple beneficiaries, revocable schedules, and multi-token
 */
contract TokenVesting is AccessControl, ReentrancyGuard, Pausable {
    using SafeERC20 for IERC20;

    /// @notice Role for creating vesting schedules
    bytes32 public constant VESTING_ADMIN_ROLE = keccak256("VESTING_ADMIN_ROLE");

    /// @notice Vesting schedule types
    enum VestingType {
        Linear,      // Linear vesting over duration
        Cliff,       // All at cliff
        LinearCliff  // Cliff then linear
    }

    /// @notice Vesting schedule structure
    struct VestingSchedule {
        address beneficiary;
        address token;
        uint256 totalAmount;
        uint256 released;
        uint256 start;
        uint256 cliff;
        uint256 duration;
        VestingType vestingType;
        bool revocable;
        bool revoked;
    }

    /// @notice Mapping from schedule ID to schedule
    mapping(bytes32 => VestingSchedule) private _schedules;

    /// @notice Mapping from beneficiary to their schedule IDs
    mapping(address => bytes32[]) private _beneficiarySchedules;

    /// @notice Mapping from token to total vested amount
    mapping(address => uint256) private _totalVestedAmount;

    /// @notice Array of all schedule IDs
    bytes32[] private _allScheduleIds;

    /// @notice Schedule counter for ID generation
    uint256 private _scheduleCounter;

    /// @notice Emitted when a schedule is created
    event ScheduleCreated(
        bytes32 indexed scheduleId,
        address indexed beneficiary,
        address indexed token,
        uint256 totalAmount,
        uint256 start,
        uint256 cliff,
        uint256 duration,
        VestingType vestingType,
        bool revocable
    );

    /// @notice Emitted when tokens are released
    event TokensReleased(
        bytes32 indexed scheduleId,
        address indexed beneficiary,
        address indexed token,
        uint256 amount
    );

    /// @notice Emitted when a schedule is revoked
    event ScheduleRevoked(
        bytes32 indexed scheduleId,
        address indexed beneficiary,
        uint256 unvested
    );

    /// @notice Emitted when beneficiary is changed
    event BeneficiaryChanged(
        bytes32 indexed scheduleId,
        address indexed oldBeneficiary,
        address indexed newBeneficiary
    );

    /// @notice Error for invalid schedule
    error InvalidSchedule(bytes32 scheduleId);

    /// @notice Error for schedule not revocable
    error NotRevocable(bytes32 scheduleId);

    /// @notice Error for schedule already revoked
    error AlreadyRevoked(bytes32 scheduleId);

    /// @notice Error for unauthorized access
    error Unauthorized();

    /// @notice Error for invalid parameters
    error InvalidParameters(string reason);

    /// @notice Error for nothing to release
    error NothingToRelease();

    /**
     * @notice Initializes the vesting contract
     */
    constructor() {
        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(VESTING_ADMIN_ROLE, msg.sender);
    }

    // ============ Schedule Creation ============

    /**
     * @notice Creates a vesting schedule
     * @param beneficiary The beneficiary address
     * @param token The token address
     * @param totalAmount The total amount to vest
     * @param start The start timestamp
     * @param cliff The cliff timestamp
     * @param duration The vesting duration
     * @param vestingType The type of vesting
     * @param revocable Whether the schedule can be revoked
     * @return scheduleId The schedule ID
     */
    function createSchedule(
        address beneficiary,
        address token,
        uint256 totalAmount,
        uint256 start,
        uint256 cliff,
        uint256 duration,
        VestingType vestingType,
        bool revocable
    ) external onlyRole(VESTING_ADMIN_ROLE) whenNotPaused returns (bytes32 scheduleId) {
        if (beneficiary == address(0)) {
            revert InvalidParameters("zero beneficiary");
        }
        if (token == address(0)) {
            revert InvalidParameters("zero token");
        }
        if (totalAmount == 0) {
            revert InvalidParameters("zero amount");
        }
        if (duration == 0 && vestingType != VestingType.Cliff) {
            revert InvalidParameters("zero duration");
        }
        if (cliff < start) {
            revert InvalidParameters("cliff before start");
        }
        if (vestingType != VestingType.Cliff && cliff > start + duration) {
            revert InvalidParameters("cliff after end");
        }

        // Transfer tokens to this contract
        IERC20(token).safeTransferFrom(msg.sender, address(this), totalAmount);

        scheduleId = _computeScheduleId(beneficiary, _scheduleCounter++);

        _schedules[scheduleId] = VestingSchedule({
            beneficiary: beneficiary,
            token: token,
            totalAmount: totalAmount,
            released: 0,
            start: start,
            cliff: cliff,
            duration: duration,
            vestingType: vestingType,
            revocable: revocable,
            revoked: false
        });

        _beneficiarySchedules[beneficiary].push(scheduleId);
        _allScheduleIds.push(scheduleId);
        _totalVestedAmount[token] += totalAmount;

        emit ScheduleCreated(
            scheduleId,
            beneficiary,
            token,
            totalAmount,
            start,
            cliff,
            duration,
            vestingType,
            revocable
        );
    }

    /**
     * @notice Creates multiple vesting schedules
     * @param beneficiaries Array of beneficiary addresses
     * @param token The token address
     * @param amounts Array of amounts
     * @param start The start timestamp
     * @param cliff The cliff timestamp
     * @param duration The vesting duration
     * @param vestingType The type of vesting
     * @param revocable Whether schedules can be revoked
     * @return scheduleIds Array of schedule IDs
     */
    function createBatchSchedules(
        address[] calldata beneficiaries,
        address token,
        uint256[] calldata amounts,
        uint256 start,
        uint256 cliff,
        uint256 duration,
        VestingType vestingType,
        bool revocable
    ) external onlyRole(VESTING_ADMIN_ROLE) whenNotPaused returns (bytes32[] memory scheduleIds) {
        if (beneficiaries.length != amounts.length) {
            revert InvalidParameters("length mismatch");
        }

        scheduleIds = new bytes32[](beneficiaries.length);

        uint256 totalAmount;
        for (uint256 i = 0; i < amounts.length; i++) {
            totalAmount += amounts[i];
        }

        IERC20(token).safeTransferFrom(msg.sender, address(this), totalAmount);

        for (uint256 i = 0; i < beneficiaries.length; i++) {
            bytes32 scheduleId = _computeScheduleId(beneficiaries[i], _scheduleCounter++);

            _schedules[scheduleId] = VestingSchedule({
                beneficiary: beneficiaries[i],
                token: token,
                totalAmount: amounts[i],
                released: 0,
                start: start,
                cliff: cliff,
                duration: duration,
                vestingType: vestingType,
                revocable: revocable,
                revoked: false
            });

            _beneficiarySchedules[beneficiaries[i]].push(scheduleId);
            _allScheduleIds.push(scheduleId);
            scheduleIds[i] = scheduleId;

            emit ScheduleCreated(
                scheduleId,
                beneficiaries[i],
                token,
                amounts[i],
                start,
                cliff,
                duration,
                vestingType,
                revocable
            );
        }

        _totalVestedAmount[token] += totalAmount;
    }

    // ============ Token Release ============

    /**
     * @notice Releases vested tokens for a schedule
     * @param scheduleId The schedule ID
     */
    function release(bytes32 scheduleId) external nonReentrant whenNotPaused {
        VestingSchedule storage schedule = _schedules[scheduleId];
        
        if (schedule.beneficiary == address(0)) {
            revert InvalidSchedule(scheduleId);
        }
        if (msg.sender != schedule.beneficiary) {
            revert Unauthorized();
        }

        uint256 releasable = _computeReleasable(schedule);
        if (releasable == 0) {
            revert NothingToRelease();
        }

        schedule.released += releasable;
        _totalVestedAmount[schedule.token] -= releasable;

        IERC20(schedule.token).safeTransfer(schedule.beneficiary, releasable);

        emit TokensReleased(scheduleId, schedule.beneficiary, schedule.token, releasable);
    }

    /**
     * @notice Releases vested tokens from multiple schedules
     * @param scheduleIds Array of schedule IDs
     */
    function releaseBatch(bytes32[] calldata scheduleIds) external nonReentrant whenNotPaused {
        for (uint256 i = 0; i < scheduleIds.length; i++) {
            VestingSchedule storage schedule = _schedules[scheduleIds[i]];
            
            if (schedule.beneficiary != msg.sender) {
                continue;
            }

            uint256 releasable = _computeReleasable(schedule);
            if (releasable == 0) {
                continue;
            }

            schedule.released += releasable;
            _totalVestedAmount[schedule.token] -= releasable;

            IERC20(schedule.token).safeTransfer(schedule.beneficiary, releasable);

            emit TokensReleased(scheduleIds[i], schedule.beneficiary, schedule.token, releasable);
        }
    }

    // ============ Schedule Management ============

    /**
     * @notice Revokes a vesting schedule
     * @param scheduleId The schedule ID
     */
    function revoke(bytes32 scheduleId) external onlyRole(VESTING_ADMIN_ROLE) nonReentrant {
        VestingSchedule storage schedule = _schedules[scheduleId];
        
        if (schedule.beneficiary == address(0)) {
            revert InvalidSchedule(scheduleId);
        }
        if (!schedule.revocable) {
            revert NotRevocable(scheduleId);
        }
        if (schedule.revoked) {
            revert AlreadyRevoked(scheduleId);
        }

        // Release any vested tokens first
        uint256 releasable = _computeReleasable(schedule);
        if (releasable > 0) {
            schedule.released += releasable;
            IERC20(schedule.token).safeTransfer(schedule.beneficiary, releasable);
            emit TokensReleased(scheduleId, schedule.beneficiary, schedule.token, releasable);
        }

        // Calculate unvested and return to admin
        uint256 unvested = schedule.totalAmount - schedule.released;
        schedule.revoked = true;
        _totalVestedAmount[schedule.token] -= unvested;

        if (unvested > 0) {
            IERC20(schedule.token).safeTransfer(msg.sender, unvested);
        }

        emit ScheduleRevoked(scheduleId, schedule.beneficiary, unvested);
    }

    /**
     * @notice Changes the beneficiary of a schedule
     * @param scheduleId The schedule ID
     * @param newBeneficiary The new beneficiary
     */
    function changeBeneficiary(
        bytes32 scheduleId,
        address newBeneficiary
    ) external nonReentrant {
        VestingSchedule storage schedule = _schedules[scheduleId];
        
        if (schedule.beneficiary == address(0)) {
            revert InvalidSchedule(scheduleId);
        }
        if (msg.sender != schedule.beneficiary) {
            revert Unauthorized();
        }
        if (newBeneficiary == address(0)) {
            revert InvalidParameters("zero beneficiary");
        }

        address oldBeneficiary = schedule.beneficiary;
        schedule.beneficiary = newBeneficiary;

        // Update beneficiary schedules mapping
        _beneficiarySchedules[newBeneficiary].push(scheduleId);

        emit BeneficiaryChanged(scheduleId, oldBeneficiary, newBeneficiary);
    }

    // ============ View Functions ============

    /**
     * @notice Returns a vesting schedule
     * @param scheduleId The schedule ID
     * @return The schedule struct
     */
    function getSchedule(bytes32 scheduleId) external view returns (VestingSchedule memory) {
        return _schedules[scheduleId];
    }

    /**
     * @notice Returns all schedule IDs for a beneficiary
     * @param beneficiary The beneficiary address
     * @return Array of schedule IDs
     */
    function getSchedulesByBeneficiary(address beneficiary) external view returns (bytes32[] memory) {
        return _beneficiarySchedules[beneficiary];
    }

    /**
     * @notice Returns the total number of schedules
     * @return The count
     */
    function getScheduleCount() external view returns (uint256) {
        return _allScheduleIds.length;
    }

    /**
     * @notice Returns vested amount for a schedule
     * @param scheduleId The schedule ID
     * @return The vested amount
     */
    function vestedAmount(bytes32 scheduleId) public view returns (uint256) {
        VestingSchedule storage schedule = _schedules[scheduleId];
        return _computeVested(schedule);
    }

    /**
     * @notice Returns releasable amount for a schedule
     * @param scheduleId The schedule ID
     * @return The releasable amount
     */
    function releasableAmount(bytes32 scheduleId) public view returns (uint256) {
        VestingSchedule storage schedule = _schedules[scheduleId];
        return _computeReleasable(schedule);
    }

    /**
     * @notice Returns total vested amount for a token
     * @param token The token address
     * @return The total amount
     */
    function totalVested(address token) external view returns (uint256) {
        return _totalVestedAmount[token];
    }

    /**
     * @notice Computes schedule ID
     * @param beneficiary The beneficiary
     * @param index The schedule index
     * @return The schedule ID
     */
    function computeScheduleId(address beneficiary, uint256 index) external pure returns (bytes32) {
        return _computeScheduleId(beneficiary, index);
    }

    // ============ Admin Functions ============

    /**
     * @notice Pauses the contract
     */
    function pause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        _pause();
    }

    /**
     * @notice Unpauses the contract
     */
    function unpause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        _unpause();
    }

    /**
     * @notice Emergency withdrawal (only unallocated tokens)
     * @param token The token address
     * @param amount The amount to withdraw
     * @param to The recipient
     */
    function emergencyWithdraw(
        address token,
        uint256 amount,
        address to
    ) external onlyRole(DEFAULT_ADMIN_ROLE) nonReentrant {
        uint256 balance = IERC20(token).balanceOf(address(this));
        uint256 allocated = _totalVestedAmount[token];
        uint256 unallocated = balance > allocated ? balance - allocated : 0;
        
        require(amount <= unallocated, "TokenVesting: exceeds unallocated");
        
        IERC20(token).safeTransfer(to, amount);
    }

    // ============ Internal Functions ============

    /**
     * @dev Computes schedule ID
     */
    function _computeScheduleId(address beneficiary, uint256 index) private pure returns (bytes32) {
        return keccak256(abi.encodePacked(beneficiary, index));
    }

    /**
     * @dev Computes vested amount
     */
    function _computeVested(VestingSchedule storage schedule) private view returns (uint256) {
        if (schedule.revoked) {
            return schedule.released;
        }

        if (block.timestamp < schedule.cliff) {
            return 0;
        }

        if (schedule.vestingType == VestingType.Cliff) {
            return schedule.totalAmount;
        }

        if (block.timestamp >= schedule.start + schedule.duration) {
            return schedule.totalAmount;
        }

        uint256 elapsed = block.timestamp - schedule.start;
        return (schedule.totalAmount * elapsed) / schedule.duration;
    }

    /**
     * @dev Computes releasable amount
     */
    function _computeReleasable(VestingSchedule storage schedule) private view returns (uint256) {
        return _computeVested(schedule) - schedule.released;
    }
}
