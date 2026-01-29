//! gRPC Projects Service
//!
//! gRPC service bindings for project operations.

const std = @import("std");
const grpc = @import("client.zig");

pub const Channel = grpc.Channel;
pub const Result = grpc.Result;
pub const GreyError = grpc.GreyError;

/// Project data
pub const Project = struct {
    project_id: []const u8,
    name: []const u8,
    description: ?[]const u8 = null,
    tenant_id: ?[]const u8 = null,
    created_at: ?[]const u8 = null,
    updated_at: ?[]const u8 = null,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *Project) void {
        self.allocator.free(self.project_id);
        self.allocator.free(self.name);
        if (self.description) |d| self.allocator.free(d);
        if (self.tenant_id) |tid| self.allocator.free(tid);
        if (self.created_at) |ca| self.allocator.free(ca);
        if (self.updated_at) |ua| self.allocator.free(ua);
    }
};

/// List projects request
pub const ListProjectsRequest = struct {
    limit: u32 = 10,
    offset: u32 = 0,
    tenant_id: ?[]const u8 = null,

    pub fn toJson(self: ListProjectsRequest, allocator: std.mem.Allocator) ![]u8 {
        if (self.tenant_id) |tid| {
            return std.fmt.allocPrint(
                allocator,
                \\{{"limit":{d},"offset":{d},"tenant_id":"{s}"}}
            ,
                .{ self.limit, self.offset, tid },
            );
        } else {
            return std.fmt.allocPrint(
                allocator,
                \\{{"limit":{d},"offset":{d}}}
            ,
                .{ self.limit, self.offset },
            );
        }
    }
};

/// List projects response
pub const ListProjectsResponse = struct {
    projects: []Project,
    total: u32,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *ListProjectsResponse) void {
        for (self.projects) |*p| {
            p.deinit();
        }
        self.allocator.free(self.projects);
    }
};

/// Create project request
pub const CreateProjectRequest = struct {
    name: []const u8,
    description: ?[]const u8 = null,
    tenant_id: ?[]const u8 = null,

    pub fn toJson(self: CreateProjectRequest, allocator: std.mem.Allocator) ![]u8 {
        var parts = std.ArrayList(u8).init(allocator);
        defer parts.deinit();

        try parts.appendSlice("{\"name\":\"");
        try parts.appendSlice(self.name);
        try parts.appendSlice("\"");

        if (self.description) |desc| {
            try parts.appendSlice(",\"description\":\"");
            try parts.appendSlice(desc);
            try parts.appendSlice("\"");
        }

        if (self.tenant_id) |tid| {
            try parts.appendSlice(",\"tenant_id\":\"");
            try parts.appendSlice(tid);
            try parts.appendSlice("\"");
        }

        try parts.appendSlice("}");

        return parts.toOwnedSlice();
    }
};

/// Get project request
pub const GetProjectRequest = struct {
    project_id: []const u8,

    pub fn toJson(self: GetProjectRequest, allocator: std.mem.Allocator) ![]u8 {
        return std.fmt.allocPrint(
            allocator,
            \\{{"project_id":"{s}"}}
        ,
            .{self.project_id},
        );
    }
};

/// Projects service client
pub const ProjectsService = struct {
    channel: *Channel,
    allocator: std.mem.Allocator,

    const Self = @This();
    const service_name = "grey.projects.v1.ProjectsService";

    pub fn init(channel: *Channel, allocator: std.mem.Allocator) Self {
        return .{
            .channel = channel,
            .allocator = allocator,
        };
    }

    /// List projects with pagination
    pub fn listProjects(self: Self, request: ListProjectsRequest) Result(ListProjectsResponse) {
        const request_data = request.toJson(self.allocator) catch |err| {
            return Result(ListProjectsResponse).failure(
                GreyError.validationError("Failed to serialize request", @errorName(err)),
            );
        };
        defer self.allocator.free(request_data);

        return grpc.unaryCall(
            ListProjectsResponse,
            self.channel,
            service_name ++ "/ListProjects",
            request_data,
        );
    }

    /// Create a new project
    pub fn createProject(self: Self, request: CreateProjectRequest) Result(Project) {
        const request_data = request.toJson(self.allocator) catch |err| {
            return Result(Project).failure(
                GreyError.validationError("Failed to serialize request", @errorName(err)),
            );
        };
        defer self.allocator.free(request_data);

        return grpc.unaryCall(
            Project,
            self.channel,
            service_name ++ "/CreateProject",
            request_data,
        );
    }

    /// Get a project by ID
    pub fn getProject(self: Self, request: GetProjectRequest) Result(Project) {
        const request_data = request.toJson(self.allocator) catch |err| {
            return Result(Project).failure(
                GreyError.validationError("Failed to serialize request", @errorName(err)),
            );
        };
        defer self.allocator.free(request_data);

        return grpc.unaryCall(
            Project,
            self.channel,
            service_name ++ "/GetProject",
            request_data,
        );
    }
};

test "ListProjectsRequest toJson" {
    const request = ListProjectsRequest{ .limit = 20, .offset = 10 };

    const json = try request.toJson(std.testing.allocator);
    defer std.testing.allocator.free(json);

    try std.testing.expect(std.mem.indexOf(u8, json, "20") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "10") != null);
}

test "CreateProjectRequest toJson" {
    const request = CreateProjectRequest{
        .name = "Test Project",
        .description = "A test project",
    };

    const json = try request.toJson(std.testing.allocator);
    defer std.testing.allocator.free(json);

    try std.testing.expect(std.mem.indexOf(u8, json, "Test Project") != null);
}
