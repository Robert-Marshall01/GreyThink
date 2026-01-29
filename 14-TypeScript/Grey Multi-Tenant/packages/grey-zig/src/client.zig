//! Grey Multi-Tenant SDK Client
//!
//! Main facade for the Grey Multi-Tenant SDK providing unified access
//! to all domain clients.

const std = @import("std");
const options_mod = @import("config/options.zig");
const grpc = @import("grpc/client.zig");
const auth_client = @import("domain/auth_client.zig");
const user_client = @import("domain/user_client.zig");
const projects_client = @import("domain/projects_client.zig");
const query_client = @import("domain/query_client.zig");
const mutation_client = @import("domain/mutation_client.zig");

pub const Options = options_mod.Options;
pub const Channel = grpc.Channel;

pub const AuthClient = auth_client.AuthClient;
pub const UserClient = user_client.UserClient;
pub const ProjectsClient = projects_client.ProjectsClient;
pub const QueryClient = query_client.QueryClient;
pub const MutationClient = mutation_client.MutationClient;

/// Grey Multi-Tenant SDK Client
///
/// Provides unified access to all Grey Multi-Tenant services.
pub const GreyClient = struct {
    channel: Channel,
    allocator: std.mem.Allocator,

    // Domain clients
    auth: AuthClient,
    user: UserClient,
    projects: ProjectsClient,
    query: QueryClient,
    mutation: MutationClient,

    const Self = @This();

    /// Create a new Grey client with options
    pub fn init(allocator: std.mem.Allocator, opts: Options) !Self {
        var channel = try Channel.init(opts);

        return .{
            .channel = channel,
            .allocator = allocator,
            .auth = AuthClient.init(&channel, allocator),
            .user = UserClient.init(&channel, allocator),
            .projects = ProjectsClient.init(&channel, allocator),
            .query = QueryClient.init(&channel, allocator),
            .mutation = MutationClient.init(&channel, allocator),
        };
    }

    /// Create a new Grey client for local development
    pub fn local(allocator: std.mem.Allocator) !Self {
        return init(allocator, Options.local(allocator));
    }

    /// Create a new Grey client for production
    pub fn production(allocator: std.mem.Allocator, host: []const u8) !Self {
        return init(allocator, Options.production(allocator, host));
    }

    /// Close the client and release resources
    pub fn deinit(self: *Self) void {
        self.channel.deinit();
    }

    /// Set authentication token
    pub fn setAuthToken(self: *Self, token: []const u8) void {
        self.channel.setAuthToken(token);
    }

    /// Clear authentication token
    pub fn clearAuthToken(self: *Self) void {
        self.channel.clearAuthToken();
    }

    /// Check if the client is authenticated
    pub fn isAuthenticated(self: Self) bool {
        return self.channel.auth_token != null;
    }
};

/// Create a new Grey client with default local options
pub fn createClient(allocator: std.mem.Allocator) !GreyClient {
    return GreyClient.local(allocator);
}

/// Create a new Grey client with custom options
pub fn createClientWithOptions(allocator: std.mem.Allocator, opts: Options) !GreyClient {
    return GreyClient.init(allocator, opts);
}

test "GreyClient local" {
    var client = try GreyClient.local(std.testing.allocator);
    defer client.deinit();

    try std.testing.expect(!client.isAuthenticated());

    client.setAuthToken("test-token");
    try std.testing.expect(client.isAuthenticated());

    client.clearAuthToken();
    try std.testing.expect(!client.isAuthenticated());
}
