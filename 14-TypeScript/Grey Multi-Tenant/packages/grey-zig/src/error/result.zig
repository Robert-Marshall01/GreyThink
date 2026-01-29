//! Grey SDK Result Type
//!
//! A tagged union representing either success with a value or failure with an error.

const std = @import("std");
const grey_error = @import("grey_error.zig");

pub const GreyError = grey_error.GreyError;
pub const ErrorCode = grey_error.ErrorCode;

/// Result type representing success or failure.
pub fn Result(comptime T: type) type {
    return union(enum) {
        ok: T,
        err: GreyError,

        const Self = @This();

        /// Create a success result
        pub fn success(value: T) Self {
            return .{ .ok = value };
        }

        /// Create a failure result
        pub fn failure(error_value: GreyError) Self {
            return .{ .err = error_value };
        }

        /// Create a failure result with components
        pub fn failWith(code: ErrorCode, message: []const u8, details: ?[]const u8) Self {
            return .{ .err = GreyError.init(code, message, details) };
        }

        /// Check if result is success
        pub fn isOk(self: Self) bool {
            return self == .ok;
        }

        /// Check if result is failure
        pub fn isErr(self: Self) bool {
            return self == .err;
        }

        /// Get the success value or return a default
        pub fn getOr(self: Self, default: T) T {
            return switch (self) {
                .ok => |v| v,
                .err => default,
            };
        }

        /// Get the success value or null
        pub fn getOrNull(self: Self) ?T {
            return switch (self) {
                .ok => |v| v,
                .err => null,
            };
        }

        /// Map the success value to a new type
        pub fn map(self: Self, comptime U: type, f: fn (T) U) Result(U) {
            return switch (self) {
                .ok => |v| Result(U).success(f(v)),
                .err => |e| Result(U).failure(e),
            };
        }

        /// Unwrap the success value, panicking on error
        pub fn unwrap(self: Self) T {
            return switch (self) {
                .ok => |v| v,
                .err => |e| std.debug.panic("Unwrap on error: {s}", .{e.message}),
            };
        }

        /// Unwrap the error, panicking on success
        pub fn unwrapErr(self: Self) GreyError {
            return switch (self) {
                .ok => std.debug.panic("unwrapErr called on success", .{}),
                .err => |e| e,
            };
        }
    };
}

/// Result type for operations that don't return a value
pub const VoidResult = Result(void);

test "Result success" {
    const result = Result(i32).success(42);
    try std.testing.expect(result.isOk());
    try std.testing.expectEqual(@as(i32, 42), result.unwrap());
}

test "Result failure" {
    const result = Result(i32).failure(GreyError.unauthorized(null));
    try std.testing.expect(result.isErr());
    try std.testing.expectEqual(@as(i32, 0), result.getOr(0));
}

test "Result getOrNull" {
    const success = Result(i32).success(42);
    const failure = Result(i32).failure(GreyError.notFound(null));

    try std.testing.expectEqual(@as(?i32, 42), success.getOrNull());
    try std.testing.expectEqual(@as(?i32, null), failure.getOrNull());
}
