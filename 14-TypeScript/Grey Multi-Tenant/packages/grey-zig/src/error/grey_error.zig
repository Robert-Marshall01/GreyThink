//! Grey SDK Error Type
//!
//! Normalized error type containing code, message, and details.

const std = @import("std");
const codes = @import("codes.zig");

pub const ErrorCode = codes.ErrorCode;

/// Normalized error type for Grey SDK operations.
pub const GreyError = struct {
    /// The error code
    code: ErrorCode,
    /// Human-readable error message
    message: []const u8,
    /// Optional additional error details
    details: ?[]const u8,
    /// Allocator used for dynamic strings (if any)
    allocator: ?std.mem.Allocator,

    const Self = @This();

    /// Create a new error with all components
    pub fn init(code: ErrorCode, message: []const u8, details: ?[]const u8) Self {
        return .{
            .code = code,
            .message = message,
            .details = details,
            .allocator = null,
        };
    }

    /// Create error with allocator for dynamic strings
    pub fn initOwned(
        allocator: std.mem.Allocator,
        code: ErrorCode,
        message: []const u8,
        details: ?[]const u8,
    ) !Self {
        const owned_message = try allocator.dupe(u8, message);
        const owned_details = if (details) |d| try allocator.dupe(u8, d) else null;
        return .{
            .code = code,
            .message = owned_message,
            .details = owned_details,
            .allocator = allocator,
        };
    }

    /// Free owned memory
    pub fn deinit(self: *Self) void {
        if (self.allocator) |alloc| {
            alloc.free(self.message);
            if (self.details) |d| {
                alloc.free(d);
            }
        }
    }

    /// Check if this error is retryable
    pub fn isRetryable(self: Self) bool {
        return self.code.isRetryable();
    }

    /// Get string representation of the error code
    pub fn codeString(self: Self) []const u8 {
        return self.code.toString();
    }

    // Factory methods

    pub fn unauthorized(details: ?[]const u8) Self {
        return init(.unauthorized, ErrorCode.unauthorized.message(), details);
    }

    pub fn forbidden(details: ?[]const u8) Self {
        return init(.forbidden, ErrorCode.forbidden.message(), details);
    }

    pub fn notFound(details: ?[]const u8) Self {
        return init(.not_found, ErrorCode.not_found.message(), details);
    }

    pub fn validationError(message: []const u8, details: ?[]const u8) Self {
        return init(.validation_error, message, details);
    }

    pub fn networkError(details: ?[]const u8) Self {
        return init(.network_error, ErrorCode.network_error.message(), details);
    }

    pub fn timeout(details: ?[]const u8) Self {
        return init(.timeout, ErrorCode.timeout.message(), details);
    }

    pub fn serverError(details: ?[]const u8) Self {
        return init(.server_error, ErrorCode.server_error.message(), details);
    }

    pub fn fromHttpStatus(status: u32, message: ?[]const u8, details: ?[]const u8) Self {
        const code = ErrorCode.fromHttpStatus(status);
        return init(code, message orelse code.message(), details);
    }

    pub fn fromGrpcStatus(status: u32, message: ?[]const u8, details: ?[]const u8) Self {
        const code = ErrorCode.fromGrpcStatus(status);
        return init(code, message orelse code.message(), details);
    }

    /// Format error for display
    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("GreyError[{s}]: {s}", .{ self.codeString(), self.message });
        if (self.details) |d| {
            try writer.print(" - {s}", .{d});
        }
    }
};

test "GreyError factory methods" {
    const err = GreyError.unauthorized("Invalid token");
    try std.testing.expectEqual(ErrorCode.unauthorized, err.code);
    try std.testing.expect(err.isRetryable() == false);
}

test "GreyError fromGrpcStatus" {
    const err = GreyError.fromGrpcStatus(16, null, "Session expired");
    try std.testing.expectEqual(ErrorCode.unauthorized, err.code);
    try std.testing.expectEqualStrings("Session expired", err.details.?);
}
