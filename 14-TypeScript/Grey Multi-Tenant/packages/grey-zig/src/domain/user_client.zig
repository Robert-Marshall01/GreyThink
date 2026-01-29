//! User Client
//!
//! Domain client for user operations with validation.

const std = @import("std");
const grpc = @import("../grpc/client.zig");
const user_service = @import("../grpc/user_service.zig");
const result_mod = @import("../error/result.zig");
const error_mod = @import("../error/grey_error.zig");

pub const Result = result_mod.Result;
pub const GreyError = error_mod.GreyError;
pub const Channel = grpc.Channel;

pub const UserResponse = user_service.UserResponse;
pub const GetUserRequest = user_service.GetUserRequest;

/// User domain model (convenience alias)
pub const User = struct {
    user_id: []const u8,
    email: []const u8,
    display_name: ?[]const u8 = null,
    tenant_id: ?[]const u8 = null,
    allocator: std.mem.Allocator,

    /// Create from UserResponse
    pub fn fromResponse(response: UserResponse) User {
        return .{
            .user_id = response.user_id,
            .email = response.email,
            .display_name = response.display_name,
            .tenant_id = response.tenant_id,
            .allocator = response.allocator,
        };
    }

    pub fn deinit(self: *User) void {
        self.allocator.free(self.user_id);
        self.allocator.free(self.email);
        if (self.display_name) |dn| self.allocator.free(dn);
        if (self.tenant_id) |tid| self.allocator.free(tid);
    }
};

/// User client with validation
pub const UserClient = struct {
    service: user_service.UserService,
    channel: *Channel,
    allocator: std.mem.Allocator,

    const Self = @This();

    /// Create a new user client
    pub fn init(channel: *Channel, allocator: std.mem.Allocator) Self {
        return .{
            .service = user_service.UserService.init(channel, allocator),
            .channel = channel,
            .allocator = allocator,
        };
    }

    /// Get the current authenticated user
    pub fn getCurrentUser(self: Self) Result(User) {
        // Check if authenticated
        if (self.channel.auth_token == null) {
            return Result(User).failure(
                GreyError.unauthorized("Authentication required"),
            );
        }

        const result = self.service.getCurrentUser();

        return result.map(User, User.fromResponse);
    }

    /// Get a user by ID
    pub fn getUser(self: Self, user_id: []const u8) Result(User) {
        // Check if authenticated
        if (self.channel.auth_token == null) {
            return Result(User).failure(
                GreyError.unauthorized("Authentication required"),
            );
        }

        // Validate user ID
        if (user_id.len == 0) {
            return Result(User).failure(
                GreyError.validationError("user_id", "User ID is required"),
            );
        }

        const request = GetUserRequest{ .user_id = user_id };
        const result = self.service.getUser(request);

        return result.map(User, User.fromResponse);
    }
};

test "UserClient validation" {
    var channel = try Channel.init(grpc.Options.local(std.testing.allocator));
    defer channel.deinit();

    const client = UserClient.init(&channel, std.testing.allocator);

    // Should fail when not authenticated
    const result = client.getCurrentUser();
    try std.testing.expect(result.isErr());
}
