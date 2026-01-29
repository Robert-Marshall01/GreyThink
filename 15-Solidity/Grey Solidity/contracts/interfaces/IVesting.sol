// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

/**
 * @title IVesting
 * @notice Interface for token vesting contracts
 * @dev Supports linear and cliff vesting schedules
 */
interface IVesting {
    /// @notice Vesting type enumeration
    enum VestingType {
        Linear,
        Cliff,
        LinearWithCliff
    }

    /**
     * @notice Vesting schedule structure
     */
    struct VestingSchedule {
        uint256 scheduleId;
        address beneficiary;
        address token;
        uint256 totalAmount;
        uint256 releasedAmount;
        uint256 startTime;
        uint256 cliffDuration;
        uint256 vestingDuration;
        VestingType vestingType;
        bool revocable;
        bool revoked;
    }

    /**
     * @notice Creates a vesting schedule
     * @param beneficiary The beneficiary address
     * @param token The token address
     * @param totalAmount The total vesting amount
     * @param startTime The vesting start time
     * @param cliffDuration The cliff duration in seconds
     * @param vestingDuration The total vesting duration
     * @param vestingType The vesting type
     * @param revocable Whether the schedule is revocable
     * @return The schedule ID
     */
    function createVestingSchedule(
        address beneficiary,
        address token,
        uint256 totalAmount,
        uint256 startTime,
        uint256 cliffDuration,
        uint256 vestingDuration,
        VestingType vestingType,
        bool revocable
    ) external returns (uint256);

    /**
     * @notice Releases vested tokens
     * @param scheduleId The schedule ID
     */
    function release(uint256 scheduleId) external;

    /**
     * @notice Revokes a vesting schedule
     * @param scheduleId The schedule ID
     */
    function revoke(uint256 scheduleId) external;

    /**
     * @notice Returns vested amount for a schedule
     * @param scheduleId The schedule ID
     * @return The vested amount
     */
    function vestedAmount(uint256 scheduleId) external view returns (uint256);

    /**
     * @notice Returns releasable amount for a schedule
     * @param scheduleId The schedule ID
     * @return The releasable amount
     */
    function releasableAmount(uint256 scheduleId) external view returns (uint256);

    /**
     * @notice Returns remaining amount for a schedule
     * @param scheduleId The schedule ID
     * @return The remaining amount
     */
    function remainingAmount(uint256 scheduleId) external view returns (uint256);

    /**
     * @notice Gets a vesting schedule by ID
     * @param scheduleId The schedule ID
     * @return The vesting schedule
     */
    function getVestingSchedule(uint256 scheduleId) external view returns (VestingSchedule memory);

    /**
     * @notice Gets all schedules for a beneficiary
     * @param beneficiary The beneficiary address
     * @return Array of schedule IDs
     */
    function getSchedulesByBeneficiary(address beneficiary) external view returns (uint256[] memory);

    /**
     * @notice Gets total vested amount for a beneficiary
     * @param beneficiary The beneficiary address
     * @param token The token address
     * @return The total vested amount
     */
    function getTotalVested(address beneficiary, address token) external view returns (uint256);

    /**
     * @notice Gets total releasable amount for a beneficiary
     * @param beneficiary The beneficiary address
     * @param token The token address
     * @return The total releasable amount
     */
    function getTotalReleasable(address beneficiary, address token) external view returns (uint256);

    /**
     * @notice Returns the number of vesting schedules
     * @return The count
     */
    function getVestingScheduleCount() external view returns (uint256);

    // Events
    event VestingScheduleCreated(
        uint256 indexed scheduleId,
        address indexed beneficiary,
        address indexed token,
        uint256 totalAmount,
        uint256 startTime,
        uint256 vestingDuration,
        VestingType vestingType
    );
    event TokensReleased(uint256 indexed scheduleId, address indexed beneficiary, uint256 amount);
    event VestingRevoked(uint256 indexed scheduleId, uint256 unreleasedAmount);
}
