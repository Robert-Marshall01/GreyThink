//! gRPC Mutation Service
//!
//! gRPC service bindings for mutation operations.

const std = @import("std");
const grpc = @import("client.zig");

pub const Channel = grpc.Channel;
pub const Result = grpc.Result;
pub const GreyError = grpc.GreyError;

/// Mutation request
pub const MutationRequest = struct {
    mutation_name: []const u8,
    parameters: std.StringHashMap([]const u8),
    tenant_id: ?[]const u8 = null,

    pub fn init(allocator: std.mem.Allocator, mutation_name: []const u8) MutationRequest {
        return .{
            .mutation_name = mutation_name,
            .parameters = std.StringHashMap([]const u8).init(allocator),
        };
    }

    pub fn deinit(self: *MutationRequest) void {
        self.parameters.deinit();
    }

    pub fn setParam(self: *MutationRequest, key: []const u8, value: []const u8) !void {
        try self.parameters.put(key, value);
    }

    pub fn toJson(self: MutationRequest, allocator: std.mem.Allocator) ![]u8 {
        var parts = std.ArrayList(u8).init(allocator);
        defer parts.deinit();

        try parts.appendSlice("{\"mutation_name\":\"");
        try parts.appendSlice(self.mutation_name);
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

/// Mutation response
pub const MutationResponse = struct {
    success: bool,
    message: ?[]const u8 = null,
    data: ?[]const u8 = null,
    metadata: ?[]const u8 = null,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *MutationResponse) void {
        if (self.message) |m| self.allocator.free(m);
        if (self.data) |d| self.allocator.free(d);
        if (self.metadata) |meta| self.allocator.free(meta);
    }
};

/// Mutation service client
pub const MutationService = struct {
    channel: *Channel,
    allocator: std.mem.Allocator,

    const Self = @This();
    const service_name = "grey.mutation.v1.MutationService";

    pub fn init(channel: *Channel, allocator: std.mem.Allocator) Self {
        return .{
            .channel = channel,
            .allocator = allocator,
        };
    }

    /// Execute a mutation
    pub fn mutate(self: Self, request: MutationRequest) Result(MutationResponse) {
        const request_data = request.toJson(self.allocator) catch |err| {
            return Result(MutationResponse).failure(
                GreyError.validationError("Failed to serialize request", @errorName(err)),
            );
        };
        defer self.allocator.free(request_data);

        return grpc.unaryCall(
            MutationResponse,
            self.channel,
            service_name ++ "/Mutate",
            request_data,
        );
    }
};

test "MutationRequest toJson" {
    var request = MutationRequest.init(std.testing.allocator, "updateStatus");
    defer request.deinit();

    try request.setParam("project_id", "proj-123");
    try request.setParam("status", "active");

    const json = try request.toJson(std.testing.allocator);
    defer std.testing.allocator.free(json);

    try std.testing.expect(std.mem.indexOf(u8, json, "updateStatus") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "proj-123") != null);
}
