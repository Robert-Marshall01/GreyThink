// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

/**
 * @title TimeUtils
 * @author Grey Solidity Project
 * @notice Time-related utility functions
 * @dev Provides constants and calculations for time operations
 */
library TimeUtils {
    /// @notice Seconds in a minute
    uint256 public constant MINUTE = 60;
    
    /// @notice Seconds in an hour
    uint256 public constant HOUR = 3600;
    
    /// @notice Seconds in a day
    uint256 public constant DAY = 86400;
    
    /// @notice Seconds in a week
    uint256 public constant WEEK = 604800;
    
    /// @notice Seconds in a month (30 days)
    uint256 public constant MONTH = 2592000;
    
    /// @notice Seconds in a year (365 days)
    uint256 public constant YEAR = 31536000;

    /**
     * @notice Returns the current block timestamp
     * @return The current timestamp
     */
    function now_() internal view returns (uint256) {
        return block.timestamp;
    }

    /**
     * @notice Converts minutes to seconds
     * @param _minutes The number of minutes
     * @return The seconds
     */
    function minutesToSeconds(uint256 _minutes) internal pure returns (uint256) {
        return _minutes * MINUTE;
    }

    /**
     * @notice Converts hours to seconds
     * @param _hours The number of hours
     * @return The seconds
     */
    function hoursToSeconds(uint256 _hours) internal pure returns (uint256) {
        return _hours * HOUR;
    }

    /**
     * @notice Converts days to seconds
     * @param days_ The number of days
     * @return The seconds
     */
    function daysToSeconds(uint256 days_) internal pure returns (uint256) {
        return days_ * DAY;
    }

    /**
     * @notice Converts weeks to seconds
     * @param weeks_ The number of weeks
     * @return The seconds
     */
    function weeksToSeconds(uint256 weeks_) internal pure returns (uint256) {
        return weeks_ * WEEK;
    }

    /**
     * @notice Converts months to seconds
     * @param _months The number of months
     * @return The seconds
     */
    function monthsToSeconds(uint256 _months) internal pure returns (uint256) {
        return _months * MONTH;
    }

    /**
     * @notice Converts years to seconds
     * @param _years The number of years
     * @return The seconds
     */
    function yearsToSeconds(uint256 _years) internal pure returns (uint256) {
        return _years * YEAR;
    }

    /**
     * @notice Converts seconds to minutes
     * @param seconds_ The number of seconds
     * @return The minutes (rounded down)
     */
    function secondsToMinutes(uint256 seconds_) internal pure returns (uint256) {
        return seconds_ / MINUTE;
    }

    /**
     * @notice Converts seconds to hours
     * @param seconds_ The number of seconds
     * @return The hours (rounded down)
     */
    function secondsToHours(uint256 seconds_) internal pure returns (uint256) {
        return seconds_ / HOUR;
    }

    /**
     * @notice Converts seconds to days
     * @param seconds_ The number of seconds
     * @return The days (rounded down)
     */
    function secondsToDays(uint256 seconds_) internal pure returns (uint256) {
        return seconds_ / DAY;
    }

    /**
     * @notice Converts seconds to weeks
     * @param seconds_ The number of seconds
     * @return The weeks (rounded down)
     */
    function secondsToWeeks(uint256 seconds_) internal pure returns (uint256) {
        return seconds_ / WEEK;
    }

    /**
     * @notice Checks if a timestamp is in the past
     * @param timestamp The timestamp to check
     * @return True if in the past
     */
    function isPast(uint256 timestamp) internal view returns (bool) {
        return timestamp < block.timestamp;
    }

    /**
     * @notice Checks if a timestamp is in the future
     * @param timestamp The timestamp to check
     * @return True if in the future
     */
    function isFuture(uint256 timestamp) internal view returns (bool) {
        return timestamp > block.timestamp;
    }

    /**
     * @notice Checks if current time is between start and end
     * @param start The start timestamp
     * @param end The end timestamp
     * @return True if within range
     */
    function isWithinRange(uint256 start, uint256 end) internal view returns (bool) {
        return block.timestamp >= start && block.timestamp <= end;
    }

    /**
     * @notice Returns time elapsed since a timestamp
     * @param timestamp The past timestamp
     * @return The elapsed time in seconds
     */
    function elapsed(uint256 timestamp) internal view returns (uint256) {
        if (timestamp >= block.timestamp) return 0;
        return block.timestamp - timestamp;
    }

    /**
     * @notice Returns time remaining until a timestamp
     * @param timestamp The future timestamp
     * @return The remaining time in seconds
     */
    function remaining(uint256 timestamp) internal view returns (uint256) {
        if (timestamp <= block.timestamp) return 0;
        return timestamp - block.timestamp;
    }

    /**
     * @notice Returns the duration between two timestamps
     * @param start The start timestamp
     * @param end The end timestamp
     * @return The duration in seconds
     */
    function duration(uint256 start, uint256 end) internal pure returns (uint256) {
        require(end >= start, "TimeUtils: end before start");
        return end - start;
    }

    /**
     * @notice Adds duration to a timestamp
     * @param timestamp The base timestamp
     * @param duration_ The duration to add
     * @return The new timestamp
     */
    function add(uint256 timestamp, uint256 duration_) internal pure returns (uint256) {
        return timestamp + duration_;
    }

    /**
     * @notice Subtracts duration from a timestamp
     * @param timestamp The base timestamp
     * @param duration_ The duration to subtract
     * @return The new timestamp
     */
    function sub(uint256 timestamp, uint256 duration_) internal pure returns (uint256) {
        require(timestamp >= duration_, "TimeUtils: underflow");
        return timestamp - duration_;
    }

    /**
     * @notice Gets the start of the current day
     * @return The timestamp of the start of today
     */
    function startOfDay() internal view returns (uint256) {
        return (block.timestamp / DAY) * DAY;
    }

    /**
     * @notice Gets the start of the day for a timestamp
     * @param timestamp The timestamp
     * @return The start of that day
     */
    function startOfDayFor(uint256 timestamp) internal pure returns (uint256) {
        return (timestamp / DAY) * DAY;
    }

    /**
     * @notice Gets the start of the current week
     * @return The timestamp of the start of this week
     */
    function startOfWeek() internal view returns (uint256) {
        return (block.timestamp / WEEK) * WEEK;
    }

    /**
     * @notice Gets the start of the current month
     * @return The timestamp of the start of this month
     */
    function startOfMonth() internal view returns (uint256) {
        return (block.timestamp / MONTH) * MONTH;
    }

    /**
     * @notice Calculates progress percentage through a period
     * @param start The period start
     * @param end The period end
     * @return Progress in basis points (0-10000)
     */
    function progressBps(uint256 start, uint256 end) internal view returns (uint256) {
        if (block.timestamp <= start) return 0;
        if (block.timestamp >= end) return 10000;
        
        uint256 elapsed_ = block.timestamp - start;
        uint256 total = end - start;
        
        return (elapsed_ * 10000) / total;
    }

    /**
     * @notice Checks if enough time has passed
     * @param timestamp The starting timestamp
     * @param duration_ The required duration
     * @return True if duration has passed
     */
    function hasElapsed(uint256 timestamp, uint256 duration_) internal view returns (bool) {
        return block.timestamp >= timestamp + duration_;
    }

    /**
     * @notice Returns the day number since Unix epoch
     * @param timestamp The timestamp
     * @return The day number
     */
    function dayNumber(uint256 timestamp) internal pure returns (uint256) {
        return timestamp / DAY;
    }

    /**
     * @notice Returns the week number since Unix epoch
     * @param timestamp The timestamp
     * @return The week number
     */
    function weekNumber(uint256 timestamp) internal pure returns (uint256) {
        return timestamp / WEEK;
    }

    /**
     * @notice Returns the current day number
     * @return The current day number
     */
    function currentDayNumber() internal view returns (uint256) {
        return block.timestamp / DAY;
    }

    /**
     * @notice Creates a deadline timestamp
     * @param duration_ The duration from now
     * @return The deadline timestamp
     */
    function deadline(uint256 duration_) internal view returns (uint256) {
        return block.timestamp + duration_;
    }

    /**
     * @notice Checks if a deadline has passed
     * @param deadline_ The deadline timestamp
     * @return True if expired
     */
    function isExpired(uint256 deadline_) internal view returns (bool) {
        return block.timestamp > deadline_;
    }

    /**
     * @notice Checks if a deadline is still valid
     * @param deadline_ The deadline timestamp
     * @return True if still valid
     */
    function isValid(uint256 deadline_) internal view returns (bool) {
        return block.timestamp <= deadline_;
    }
}
