//! Grey SDK Error Codes
//!
//! Standard error codes used throughout the Grey SDK.

const std = @import("std");

/// Standard error codes for Grey SDK operations.
pub const ErrorCode = enum(u32) {
    /// Authentication required or token invalid
    unauthorized = 401,
    /// Access denied to the requested resource
    forbidden = 403,
    /// Requested resource was not found
    not_found = 404,
    /// Request validation failed
    validation_error = 422,
    /// Network connectivity issue
    network_error = 1001,
    /// Request timed out
    timeout = 1002,
    /// Server returned an error
    server_error = 500,
    /// Unknown or unexpected error
    unknown = 0,

    /// Convert from HTTP status code
    pub fn fromHttpStatus(status: u32) ErrorCode {
        return switch (status) {
            401 => .unauthorized,
            403 => .forbidden,
            404 => .not_found,
            400, 422 => .validation_error,
            408 => .timeout,
            500...599 => .server_error,
            else => .unknown,
        };
    }

    /// Convert from gRPC status code
    pub fn fromGrpcStatus(status: u32) ErrorCode {
        return switch (status) {
            1 => .unknown, // CANCELLED
            2 => .unknown, // UNKNOWN
            3 => .validation_error, // INVALID_ARGUMENT
            4 => .timeout, // DEADLINE_EXCEEDED
            5 => .not_found, // NOT_FOUND
            6 => .validation_error, // ALREADY_EXISTS
            7 => .forbidden, // PERMISSION_DENIED
            8 => .server_error, // RESOURCE_EXHAUSTED
            9 => .validation_error, // FAILED_PRECONDITION
            10 => .unknown, // ABORTED
            11 => .validation_error, // OUT_OF_RANGE
            12 => .unknown, // UNIMPLEMENTED
            13 => .server_error, // INTERNAL
            14 => .network_error, // UNAVAILABLE
            15 => .server_error, // DATA_LOSS
            16 => .unauthorized, // UNAUTHENTICATED
            else => .unknown,
        };
    }

    /// Check if this error is retryable
    pub fn isRetryable(self: ErrorCode) bool {
        return switch (self) {
            .network_error, .timeout, .server_error => true,
            else => false,
        };
    }

    /// Get human-readable message for error code
    pub fn message(self: ErrorCode) []const u8 {
        return switch (self) {
            .unauthorized => "Authentication required or token invalid",
            .forbidden => "Access denied to the requested resource",
            .not_found => "Requested resource was not found",
            .validation_error => "Request validation failed",
            .network_error => "Network connectivity issue",
            .timeout => "Request timed out",
            .server_error => "Server returned an error",
            .unknown => "Unknown or unexpected error",
        };
    }

    /// Get string representation
    pub fn toString(self: ErrorCode) []const u8 {
        return switch (self) {
            .unauthorized => "unauthorized",
            .forbidden => "forbidden",
            .not_found => "not_found",
            .validation_error => "validation_error",
            .network_error => "network_error",
            .timeout => "timeout",
            .server_error => "server_error",
            .unknown => "unknown",
        };
    }
};

test "ErrorCode.fromHttpStatus" {
    try std.testing.expectEqual(ErrorCode.unauthorized, ErrorCode.fromHttpStatus(401));
    try std.testing.expectEqual(ErrorCode.forbidden, ErrorCode.fromHttpStatus(403));
    try std.testing.expectEqual(ErrorCode.not_found, ErrorCode.fromHttpStatus(404));
    try std.testing.expectEqual(ErrorCode.server_error, ErrorCode.fromHttpStatus(500));
}

test "ErrorCode.fromGrpcStatus" {
    try std.testing.expectEqual(ErrorCode.unauthorized, ErrorCode.fromGrpcStatus(16));
    try std.testing.expectEqual(ErrorCode.forbidden, ErrorCode.fromGrpcStatus(7));
    try std.testing.expectEqual(ErrorCode.not_found, ErrorCode.fromGrpcStatus(5));
}

test "ErrorCode.isRetryable" {
    try std.testing.expect(ErrorCode.network_error.isRetryable());
    try std.testing.expect(ErrorCode.timeout.isRetryable());
    try std.testing.expect(!ErrorCode.unauthorized.isRetryable());
}
