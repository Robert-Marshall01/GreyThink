//! Projects Client
//!
//! Domain client for project operations with validation.

const std = @import("std");
const grpc = @import("../grpc/client.zig");
const projects_service = @import("../grpc/projects_service.zig");
const result_mod = @import("../error/result.zig");
const error_mod = @import("../error/grey_error.zig");

pub const Result = result_mod.Result;
pub const GreyError = error_mod.GreyError;
pub const Channel = grpc.Channel;

pub const Project = projects_service.Project;
pub const ListProjectsRequest = projects_service.ListProjectsRequest;
pub const ListProjectsResponse = projects_service.ListProjectsResponse;
pub const CreateProjectRequest = projects_service.CreateProjectRequest;

/// Pagination options
pub const PaginationOptions = struct {
    limit: u32 = 10,
    offset: u32 = 0,
};

/// Projects client with validation
pub const ProjectsClient = struct {
    service: projects_service.ProjectsService,
    channel: *Channel,
    allocator: std.mem.Allocator,

    const Self = @This();

    /// Maximum allowed limit for pagination
    const max_limit: u32 = 100;
    /// Maximum allowed project name length
    const max_name_length: usize = 255;

    /// Create a new projects client
    pub fn init(channel: *Channel, allocator: std.mem.Allocator) Self {
        return .{
            .service = projects_service.ProjectsService.init(channel, allocator),
            .channel = channel,
            .allocator = allocator,
        };
    }

    /// List projects with default pagination
    pub fn listProjects(self: Self) Result(ListProjectsResponse) {
        return self.listProjectsWithOptions(.{});
    }

    /// List projects with pagination options
    pub fn listProjectsWithOptions(self: Self, options: PaginationOptions) Result(ListProjectsResponse) {
        return self.listProjectsForTenant(options, null);
    }

    /// List projects for a specific tenant
    pub fn listProjectsForTenant(
        self: Self,
        options: PaginationOptions,
        tenant_id: ?[]const u8,
    ) Result(ListProjectsResponse) {
        // Check if authenticated
        if (self.channel.auth_token == null) {
            return Result(ListProjectsResponse).failure(
                GreyError.unauthorized("Authentication required"),
            );
        }

        // Validate pagination
        const validated_limit = @min(options.limit, max_limit);

        const request = ListProjectsRequest{
            .limit = validated_limit,
            .offset = options.offset,
            .tenant_id = tenant_id,
        };

        return self.service.listProjects(request);
    }

    /// Create a new project
    pub fn createProject(self: Self, name: []const u8, description: ?[]const u8) Result(Project) {
        return self.createProjectForTenant(name, description, null);
    }

    /// Create a new project for a specific tenant
    pub fn createProjectForTenant(
        self: Self,
        name: []const u8,
        description: ?[]const u8,
        tenant_id: ?[]const u8,
    ) Result(Project) {
        // Check if authenticated
        if (self.channel.auth_token == null) {
            return Result(Project).failure(
                GreyError.unauthorized("Authentication required"),
            );
        }

        // Validate name
        if (name.len == 0) {
            return Result(Project).failure(
                GreyError.validationError("name", "Project name is required"),
            );
        }

        if (name.len > max_name_length) {
            return Result(Project).failure(
                GreyError.validationError("name", "Project name too long"),
            );
        }

        // Check for whitespace-only name
        const trimmed = std.mem.trim(u8, name, " \t\n\r");
        if (trimmed.len == 0) {
            return Result(Project).failure(
                GreyError.validationError("name", "Project name cannot be blank"),
            );
        }

        const request = CreateProjectRequest{
            .name = name,
            .description = description,
            .tenant_id = tenant_id,
        };

        return self.service.createProject(request);
    }

    /// Get a project by ID
    pub fn getProject(self: Self, project_id: []const u8) Result(Project) {
        // Check if authenticated
        if (self.channel.auth_token == null) {
            return Result(Project).failure(
                GreyError.unauthorized("Authentication required"),
            );
        }

        // Validate project ID
        if (project_id.len == 0) {
            return Result(Project).failure(
                GreyError.validationError("project_id", "Project ID is required"),
            );
        }

        const request = projects_service.GetProjectRequest{
            .project_id = project_id,
        };

        return self.service.getProject(request);
    }
};

test "ProjectsClient pagination validation" {
    var channel = try Channel.init(grpc.Options.local(std.testing.allocator));
    defer channel.deinit();

    _ = ProjectsClient.init(&channel, std.testing.allocator);

    // Verify max limit constant
    try std.testing.expectEqual(@as(u32, 100), ProjectsClient.max_limit);
}
