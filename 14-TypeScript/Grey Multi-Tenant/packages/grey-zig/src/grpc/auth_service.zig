//! gRPC Auth Service
//!
//! gRPC service bindings for authentication operations.

const std = @import("std");
const grpc = @import("client.zig");

pub const Channel = grpc.Channel;
pub const Result = grpc.Result;
pub const GreyError = grpc.GreyError;

/// Login request
pub const LoginRequest = struct {
    email: []const u8,
    password: []const u8,
    tenant_id: ?[]const u8 = null,

    /// Serialize to JSON (simplified - would be protobuf in production)
    pub fn toJson(self: LoginRequest, allocator: std.mem.Allocator) ![]u8 {
        if (self.tenant_id) |tid| {
            return std.fmt.allocPrint(
                allocator,
                \\{{"email":"{s}","password":"{s}","tenant_id":"{s}"}}
            ,
                .{ self.email, self.password, tid },
            );
        } else {
            return std.fmt.allocPrint(
                allocator,
                \\{{"email":"{s}","password":"{s}"}}
            ,
                .{ self.email, self.password },
            );
        }
    }
};

/// Login response
pub const LoginResponse = struct {
    access_token: []const u8,
    refresh_token: []const u8,
    expires_in: u64,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *LoginResponse) void {
        self.allocator.free(self.access_token);
        self.allocator.free(self.refresh_token);
    }
};

/// Refresh request
pub const RefreshRequest = struct {
    refresh_token: []const u8,

    pub fn toJson(self: RefreshRequest, allocator: std.mem.Allocator) ![]u8 {
        return std.fmt.allocPrint(
            allocator,
            \\{{"refresh_token":"{s}"}}
        ,
            .{self.refresh_token},
        );
    }
};

/// Empty request/response for logout
pub const Empty = struct {};

/// Auth service client
pub const AuthService = struct {
    channel: *Channel,
    allocator: std.mem.Allocator,

    const Self = @This();
    const service_name = "grey.auth.v1.AuthService";

    pub fn init(channel: *Channel, allocator: std.mem.Allocator) Self {
        return .{
            .channel = channel,
            .allocator = allocator,
        };
    }

    /// Login with credentials
    pub fn login(self: Self, request: LoginRequest) Result(LoginResponse) {
        const request_data = request.toJson(self.allocator) catch |err| {
            return Result(LoginResponse).failure(
                GreyError.validationError("Failed to serialize request", @errorName(err)),
            );
        };
        defer self.allocator.free(request_data);

        return grpc.unaryCall(
            LoginResponse,
            self.channel,
            service_name ++ "/Login",
            request_data,
        );
    }

    /// Logout current session
    pub fn logout(self: Self) Result(Empty) {
        return grpc.unaryCall(
            Empty,
            self.channel,
            service_name ++ "/Logout",
            "{}",
        );
    }

    /// Refresh authentication token
    pub fn refresh(self: Self, request: RefreshRequest) Result(LoginResponse) {
        const request_data = request.toJson(self.allocator) catch |err| {
            return Result(LoginResponse).failure(
                GreyError.validationError("Failed to serialize request", @errorName(err)),
            );
        };
        defer self.allocator.free(request_data);

        return grpc.unaryCall(
            LoginResponse,
            self.channel,
            service_name ++ "/Refresh",
            request_data,
        );
    }
};

test "LoginRequest toJson" {
    const request = LoginRequest{
        .email = "test@example.com",
        .password = "password123",
    };

    const json = try request.toJson(std.testing.allocator);
    defer std.testing.allocator.free(json);

    try std.testing.expect(std.mem.indexOf(u8, json, "test@example.com") != null);
}
