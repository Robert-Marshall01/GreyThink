//! Mutation Client
//!
//! Domain client for mutation operations with validation.

const std = @import("std");
const grpc = @import("../grpc/client.zig");
const mutation_service = @import("../grpc/mutation_service.zig");
const result_mod = @import("../error/result.zig");
const error_mod = @import("../error/grey_error.zig");

pub const Result = result_mod.Result;
pub const GreyError = error_mod.GreyError;
pub const Channel = grpc.Channel;

pub const MutationRequest = mutation_service.MutationRequest;
pub const MutationResponse = mutation_service.MutationResponse;

/// Mutation builder for fluent API
pub const MutationBuilder = struct {
    mutation_name: []const u8,
    parameters: std.StringHashMap([]const u8),
    tenant_id: ?[]const u8 = null,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, mutation_name: []const u8) Self {
        return .{
            .mutation_name = mutation_name,
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
    pub fn build(self: Self) MutationRequest {
        return .{
            .mutation_name = self.mutation_name,
            .parameters = self.parameters,
            .tenant_id = self.tenant_id,
        };
    }
};

/// Mutation client with validation
pub const MutationClient = struct {
    service: mutation_service.MutationService,
    channel: *Channel,
    allocator: std.mem.Allocator,

    const Self = @This();

    /// Maximum mutation name length
    const max_mutation_name_length: usize = 255;

    /// Create a new mutation client
    pub fn init(channel: *Channel, allocator: std.mem.Allocator) Self {
        return .{
            .service = mutation_service.MutationService.init(channel, allocator),
            .channel = channel,
            .allocator = allocator,
        };
    }

    /// Create a mutation builder
    pub fn mutationBuilder(self: Self, mutation_name: []const u8) MutationBuilder {
        return MutationBuilder.init(self.allocator, mutation_name);
    }

    /// Execute a simple mutation by name
    pub fn mutate(self: Self, mutation_name: []const u8) Result(MutationResponse) {
        var request = MutationRequest.init(self.allocator, mutation_name);
        defer request.deinit();

        return self.executeMutation(request);
    }

    /// Execute a mutation with parameters
    pub fn mutateWithParams(
        self: Self,
        mutation_name: []const u8,
        params: std.StringHashMap([]const u8),
    ) Result(MutationResponse) {
        const request = MutationRequest{
            .mutation_name = mutation_name,
            .parameters = params,
            .tenant_id = null,
        };

        return self.executeMutation(request);
    }

    /// Execute a mutation request
    pub fn executeMutation(self: Self, request: MutationRequest) Result(MutationResponse) {
        // Check if authenticated
        if (self.channel.auth_token == null) {
            return Result(MutationResponse).failure(
                GreyError.unauthorized("Authentication required"),
            );
        }

        // Validate mutation name
        if (request.mutation_name.len == 0) {
            return Result(MutationResponse).failure(
                GreyError.validationError("mutation_name", "Mutation name is required"),
            );
        }

        if (request.mutation_name.len > max_mutation_name_length) {
            return Result(MutationResponse).failure(
                GreyError.validationError("mutation_name", "Mutation name too long"),
            );
        }

        return self.service.mutate(request);
    }
};

test "MutationBuilder fluent API" {
    var builder = MutationBuilder.init(std.testing.allocator, "updateStatus");
    defer builder.deinit();

    _ = builder.param("project_id", "proj-123").param("status", "active").forTenant("tenant-1");

    const request = builder.build();
    try std.testing.expectEqualStrings("updateStatus", request.mutation_name);
    try std.testing.expectEqualStrings("tenant-1", request.tenant_id.?);
}
