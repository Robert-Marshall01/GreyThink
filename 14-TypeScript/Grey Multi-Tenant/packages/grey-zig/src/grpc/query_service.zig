//! gRPC Query Service
//!
//! gRPC service bindings for query operations.

const std = @import("std");
const grpc = @import("client.zig");

pub const Channel = grpc.Channel;
pub const Result = grpc.Result;
pub const GreyError = grpc.GreyError;

/// Query request
pub const QueryRequest = struct {
    query_name: []const u8,
    parameters: std.StringHashMap([]const u8),
    tenant_id: ?[]const u8 = null,

    pub fn init(allocator: std.mem.Allocator, query_name: []const u8) QueryRequest {
        return .{
            .query_name = query_name,
            .parameters = std.StringHashMap([]const u8).init(allocator),
        };
    }

    pub fn deinit(self: *QueryRequest) void {
        self.parameters.deinit();
    }

    pub fn setParam(self: *QueryRequest, key: []const u8, value: []const u8) !void {
        try self.parameters.put(key, value);
    }

    pub fn toJson(self: QueryRequest, allocator: std.mem.Allocator) ![]u8 {
        var parts = std.ArrayList(u8).init(allocator);
        defer parts.deinit();

        try parts.appendSlice("{\"query_name\":\"");
        try parts.appendSlice(self.query_name);
        try parts.appendSlice("\",\"parameters\":{");

        var first = true;
        var iter = self.parameters.iterator();
        while (iter.next()) |entry| {
            if (!first) {
                try parts.appendSlice(",");
            }
            try parts.appendSlice("\"");
            try parts.appendSlice(entry.key_ptr.*);
            try parts.appendSlice("\":\"");
            try parts.appendSlice(entry.value_ptr.*);
            try parts.appendSlice("\"");
            first = false;
        }

        try parts.appendSlice("}");

        if (self.tenant_id) |tid| {
            try parts.appendSlice(",\"tenant_id\":\"");
            try parts.appendSlice(tid);
            try parts.appendSlice("\"");
        }

        try parts.appendSlice("}");

        return parts.toOwnedSlice();
    }
};

/// Query response
pub const QueryResponse = struct {
    data: []const u8, // Raw JSON data
    metadata: ?[]const u8 = null,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *QueryResponse) void {
        self.allocator.free(self.data);
        if (self.metadata) |m| self.allocator.free(m);
    }
};

/// Query service client
pub const QueryService = struct {
    channel: *Channel,
    allocator: std.mem.Allocator,

    const Self = @This();
    const service_name = "grey.query.v1.QueryService";

    pub fn init(channel: *Channel, allocator: std.mem.Allocator) Self {
        return .{
            .channel = channel,
            .allocator = allocator,
        };
    }

    /// Execute a query
    pub fn query(self: Self, request: QueryRequest) Result(QueryResponse) {
        const request_data = request.toJson(self.allocator) catch |err| {
            return Result(QueryResponse).failure(
                GreyError.validationError("Failed to serialize request", @errorName(err)),
            );
        };
        defer self.allocator.free(request_data);

        return grpc.unaryCall(
            QueryResponse,
            self.channel,
            service_name ++ "/Query",
            request_data,
        );
    }
};

test "QueryRequest toJson" {
    var request = QueryRequest.init(std.testing.allocator, "getStats");
    defer request.deinit();

    try request.setParam("project_id", "proj-123");

    const json = try request.toJson(std.testing.allocator);
    defer std.testing.allocator.free(json);

    try std.testing.expect(std.mem.indexOf(u8, json, "getStats") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "proj-123") != null);
}
