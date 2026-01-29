//! Query Client
//!
//! Domain client for query operations with validation.

const std = @import("std");
const grpc = @import("../grpc/client.zig");
const query_service = @import("../grpc/query_service.zig");
const result_mod = @import("../error/result.zig");
const error_mod = @import("../error/grey_error.zig");

pub const Result = result_mod.Result;
pub const GreyError = error_mod.GreyError;
pub const Channel = grpc.Channel;

pub const QueryRequest = query_service.QueryRequest;
pub const QueryResponse = query_service.QueryResponse;

/// Query builder for fluent API
pub const QueryBuilder = struct {
    query_name: []const u8,
    parameters: std.StringHashMap([]const u8),
    tenant_id: ?[]const u8 = null,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, query_name: []const u8) Self {
        return .{
            .query_name = query_name,
            .parameters = std.StringHashMap([]const u8).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.parameters.deinit();
    }

    /// Add a parameter
    pub fn param(self: *Self, key: []const u8, value: []const u8) *Self {
        self.parameters.put(key, value) catch {};
        return self;
    }

    /// Set tenant ID
    pub fn forTenant(self: *Self, tenant_id: []const u8) *Self {
        self.tenant_id = tenant_id;
        return self;
    }

    /// Build the request
    pub fn build(self: Self) QueryRequest {
        return .{
            .query_name = self.query_name,
            .parameters = self.parameters,
            .tenant_id = self.tenant_id,
        };
    }
};

/// Query client with validation
pub const QueryClient = struct {
    service: query_service.QueryService,
    channel: *Channel,
    allocator: std.mem.Allocator,

    const Self = @This();

    /// Maximum query name length
    const max_query_name_length: usize = 255;

    /// Create a new query client
    pub fn init(channel: *Channel, allocator: std.mem.Allocator) Self {
        return .{
            .service = query_service.QueryService.init(channel, allocator),
            .channel = channel,
            .allocator = allocator,
        };
    }

    /// Create a query builder
    pub fn queryBuilder(self: Self, query_name: []const u8) QueryBuilder {
        return QueryBuilder.init(self.allocator, query_name);
    }

    /// Execute a simple query by name
    pub fn query(self: Self, query_name: []const u8) Result(QueryResponse) {
        var request = QueryRequest.init(self.allocator, query_name);
        defer request.deinit();

        return self.executeQuery(request);
    }

    /// Execute a query with parameters
    pub fn queryWithParams(
        self: Self,
        query_name: []const u8,
        params: std.StringHashMap([]const u8),
    ) Result(QueryResponse) {
        const request = QueryRequest{
            .query_name = query_name,
            .parameters = params,
            .tenant_id = null,
        };

        return self.executeQuery(request);
    }

    /// Execute a query request
    pub fn executeQuery(self: Self, request: QueryRequest) Result(QueryResponse) {
        // Check if authenticated
        if (self.channel.auth_token == null) {
            return Result(QueryResponse).failure(
                GreyError.unauthorized("Authentication required"),
            );
        }

        // Validate query name
        if (request.query_name.len == 0) {
            return Result(QueryResponse).failure(
                GreyError.validationError("query_name", "Query name is required"),
            );
        }

        if (request.query_name.len > max_query_name_length) {
            return Result(QueryResponse).failure(
                GreyError.validationError("query_name", "Query name too long"),
            );
        }

        return self.service.query(request);
    }
};

test "QueryBuilder fluent API" {
    var builder = QueryBuilder.init(std.testing.allocator, "getStats");
    defer builder.deinit();

    _ = builder.param("project_id", "proj-123").param("format", "json");

    const request = builder.build();
    try std.testing.expectEqualStrings("getStats", request.query_name);
}
