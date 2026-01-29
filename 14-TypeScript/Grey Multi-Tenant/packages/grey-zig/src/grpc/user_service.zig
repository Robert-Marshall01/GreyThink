//! gRPC User Service
//!
//! gRPC service bindings for user operations.

const std = @import("std");
const grpc = @import("client.zig");

pub const Channel = grpc.Channel;
pub const Result = grpc.Result;
pub const GreyError = grpc.GreyError;

/// Get user request
pub const GetUserRequest = struct {
    user_id: []const u8,

    pub fn toJson(self: GetUserRequest, allocator: std.mem.Allocator) ![]u8 {
        return std.fmt.allocPrint(
            allocator,
            \\{{"user_id":"{s}"}}
        ,
            .{self.user_id},
        );
    }
};

/// User response
pub const UserResponse = struct {
    user_id: []const u8,
    email: []const u8,
    display_name: ?[]const u8 = null,
    tenant_id: ?[]const u8 = null,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *UserResponse) void {
        self.allocator.free(self.user_id);
        self.allocator.free(self.email);
        if (self.display_name) |dn| self.allocator.free(dn);
        if (self.tenant_id) |tid| self.allocator.free(tid);
    }
};

/// Empty request for getCurrentUser
pub const Empty = struct {};

/// User service client
pub const UserService = struct {
    channel: *Channel,
    allocator: std.mem.Allocator,

    const Self = @This();
    const service_name = "grey.user.v1.UserService";

    pub fn init(channel: *Channel, allocator: std.mem.Allocator) Self {
        return .{
            .channel = channel,
            .allocator = allocator,
        };
    }

    /// Get the current authenticated user
    pub fn getCurrentUser(self: Self) Result(UserResponse) {
        return grpc.unaryCall(
            UserResponse,
            self.channel,
            service_name ++ "/GetCurrentUser",
            "{}",
        );
    }

    /// Get a user by ID
    pub fn getUser(self: Self, request: GetUserRequest) Result(UserResponse) {
        const request_data = request.toJson(self.allocator) catch |err| {
            return Result(UserResponse).failure(
                GreyError.validationError("Failed to serialize request", @errorName(err)),
            );
        };
        defer self.allocator.free(request_data);

        return grpc.unaryCall(
            UserResponse,
            self.channel,
            service_name ++ "/GetUser",
            request_data,
        );
    }
};

test "GetUserRequest toJson" {
    const request = GetUserRequest{ .user_id = "user-123" };

    const json = try request.toJson(std.testing.allocator);
    defer std.testing.allocator.free(json);

    try std.testing.expect(std.mem.indexOf(u8, json, "user-123") != null);
}
