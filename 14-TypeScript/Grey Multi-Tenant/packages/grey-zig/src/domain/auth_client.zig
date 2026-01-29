//! Auth Client
//!
//! Domain client for authentication with validation.

const std = @import("std");
const grpc = @import("../grpc/client.zig");
const auth_service = @import("../grpc/auth_service.zig");
const result_mod = @import("../error/result.zig");
const error_mod = @import("../error/grey_error.zig");

pub const Result = result_mod.Result;
pub const VoidResult = result_mod.VoidResult;
pub const GreyError = error_mod.GreyError;
pub const Channel = grpc.Channel;

pub const LoginRequest = auth_service.LoginRequest;
pub const LoginResponse = auth_service.LoginResponse;
pub const RefreshRequest = auth_service.RefreshRequest;

/// Validation error details
const ValidationError = struct {
    field: []const u8,
    message: []const u8,
};

/// Auth client with validation
pub const AuthClient = struct {
    service: auth_service.AuthService,
    channel: *Channel,
    allocator: std.mem.Allocator,

    const Self = @This();

    /// Email validation regex pattern (simplified)
    fn isValidEmail(email: []const u8) bool {
        // Simple validation: contains @ and at least one char before and after
        const at_pos = std.mem.indexOf(u8, email, "@") orelse return false;
        return at_pos > 0 and at_pos < email.len - 1;
    }

    /// Create a new auth client
    pub fn init(channel: *Channel, allocator: std.mem.Allocator) Self {
        return .{
            .service = auth_service.AuthService.init(channel, allocator),
            .channel = channel,
            .allocator = allocator,
        };
    }

    /// Login with email and password
    pub fn login(self: Self, email: []const u8, password: []const u8) Result(LoginResponse) {
        return self.loginWithTenant(email, password, null);
    }

    /// Login with email, password, and optional tenant
    pub fn loginWithTenant(
        self: Self,
        email: []const u8,
        password: []const u8,
        tenant_id: ?[]const u8,
    ) Result(LoginResponse) {
        // Validate email
        if (email.len == 0) {
            return Result(LoginResponse).failure(
                GreyError.validationError("email", "Email is required"),
            );
        }

        if (!isValidEmail(email)) {
            return Result(LoginResponse).failure(
                GreyError.validationError("email", "Invalid email format"),
            );
        }

        // Validate password
        if (password.len == 0) {
            return Result(LoginResponse).failure(
                GreyError.validationError("password", "Password is required"),
            );
        }

        if (password.len < 8) {
            return Result(LoginResponse).failure(
                GreyError.validationError("password", "Password must be at least 8 characters"),
            );
        }

        // Make request
        const request = LoginRequest{
            .email = email,
            .password = password,
            .tenant_id = tenant_id,
        };

        const result = self.service.login(request);

        // Update channel auth token on success
        if (result.isOk()) {
            if (result.getOrNull()) |response| {
                self.channel.setAuthToken(response.access_token);
            }
        }

        return result;
    }

    /// Logout current session
    pub fn logout(self: Self) VoidResult {
        const result = self.service.logout();

        // Clear auth token regardless of result
        self.channel.clearAuthToken();

        if (result.isOk()) {
            return VoidResult.success({});
        } else {
            return VoidResult.failure(result.unwrapErr());
        }
    }

    /// Refresh authentication token
    pub fn refresh(self: Self, refresh_token: []const u8) Result(LoginResponse) {
        // Validate refresh token
        if (refresh_token.len == 0) {
            return Result(LoginResponse).failure(
                GreyError.validationError("refresh_token", "Refresh token is required"),
            );
        }

        const request = RefreshRequest{
            .refresh_token = refresh_token,
        };

        const result = self.service.refresh(request);

        // Update channel auth token on success
        if (result.isOk()) {
            if (result.getOrNull()) |response| {
                self.channel.setAuthToken(response.access_token);
            }
        }

        return result;
    }
};

test "AuthClient email validation" {
    try std.testing.expect(AuthClient.isValidEmail("test@example.com"));
    try std.testing.expect(AuthClient.isValidEmail("a@b"));
    try std.testing.expect(!AuthClient.isValidEmail(""));
    try std.testing.expect(!AuthClient.isValidEmail("@example.com"));
    try std.testing.expect(!AuthClient.isValidEmail("test@"));
    try std.testing.expect(!AuthClient.isValidEmail("testexample.com"));
}
