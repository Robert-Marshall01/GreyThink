//! Grey SDK Configuration Options
//!
//! Configuration for the Grey SDK client.

const std = @import("std");

/// Configuration options for the Grey SDK client.
pub const Options = struct {
    /// API host
    host: []const u8 = "localhost",
    /// API port
    port: u16 = 50051,
    /// Whether to use TLS
    use_tls: bool = false,
    /// Request timeout in milliseconds
    timeout_ms: u32 = 30000,
    /// Authentication token
    auth_token: ?[]const u8 = null,
    /// Custom metadata headers
    metadata: ?std.StringHashMap([]const u8) = null,
    /// Allocator for dynamic data
    allocator: std.mem.Allocator,

    const Self = @This();

    /// Create default options for local development
    pub fn local(allocator: std.mem.Allocator) Self {
        return .{ .allocator = allocator };
    }

    /// Create options for local development with custom port
    pub fn localWithPort(allocator: std.mem.Allocator, port: u16) Self {
        return .{
            .allocator = allocator,
            .port = port,
        };
    }

    /// Create options for production
    pub fn production(allocator: std.mem.Allocator, host: []const u8) Self {
        return .{
            .allocator = allocator,
            .host = host,
            .port = 443,
            .use_tls = true,
        };
    }

    /// Create options for production with custom port
    pub fn productionWithPort(allocator: std.mem.Allocator, host: []const u8, port: u16) Self {
        return .{
            .allocator = allocator,
            .host = host,
            .port = port,
            .use_tls = true,
        };
    }

    /// Get the gRPC endpoint string
    pub fn endpoint(self: Self) ![]u8 {
        return std.fmt.allocPrint(self.allocator, "{s}:{d}", .{ self.host, self.port });
    }

    /// Create a new options with auth token set
    pub fn withAuthToken(self: Self, token: []const u8) Self {
        var new = self;
        new.auth_token = token;
        return new;
    }

    /// Create a new options with timeout set
    pub fn withTimeout(self: Self, timeout_ms: u32) Self {
        var new = self;
        new.timeout_ms = timeout_ms;
        return new;
    }

    /// Add a metadata header
    pub fn withMetadata(self: *Self, key: []const u8, value: []const u8) !void {
        if (self.metadata == null) {
            self.metadata = std.StringHashMap([]const u8).init(self.allocator);
        }
        try self.metadata.?.put(key, value);
    }

    /// Clean up resources
    pub fn deinit(self: *Self) void {
        if (self.metadata) |*m| {
            m.deinit();
        }
    }
};

test "Options local" {
    var options = Options.local(std.testing.allocator);
    defer options.deinit();

    try std.testing.expectEqualStrings("localhost", options.host);
    try std.testing.expectEqual(@as(u16, 50051), options.port);
    try std.testing.expectEqual(false, options.use_tls);
}

test "Options production" {
    var options = Options.production(std.testing.allocator, "api.grey.io");
    defer options.deinit();

    try std.testing.expectEqualStrings("api.grey.io", options.host);
    try std.testing.expectEqual(@as(u16, 443), options.port);
    try std.testing.expectEqual(true, options.use_tls);
}

test "Options endpoint" {
    var options = Options.local(std.testing.allocator);
    defer options.deinit();

    const ep = try options.endpoint();
    defer options.allocator.free(ep);

    try std.testing.expectEqualStrings("localhost:50051", ep);
}
