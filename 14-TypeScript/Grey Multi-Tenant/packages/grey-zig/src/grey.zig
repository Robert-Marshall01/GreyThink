//! Grey Multi-Tenant SDK for Zig
//!
//! A complete SDK for integrating with Grey Multi-Tenant services using
//! gRPC transport via C interop.
//!
//! ## Quick Start
//!
//! ```zig
//! const grey = @import("grey");
//!
//! pub fn main() !void {
//!     var gpa = std.heap.GeneralPurposeAllocator(.{}){};
//!     defer _ = gpa.deinit();
//!     const allocator = gpa.allocator();
//!
//!     // Create client
//!     var client = try grey.createClient(allocator);
//!     defer client.deinit();
//!
//!     // Login
//!     const login_result = client.auth.login("user@example.com", "password123");
//!     if (login_result.isOk()) {
//!         std.debug.print("Logged in successfully!\n", .{});
//!     }
//! }
//! ```

const std = @import("std");

// Error types
pub const error_codes = @import("error/codes.zig");
pub const grey_error = @import("error/grey_error.zig");
pub const result = @import("error/result.zig");

pub const ErrorCode = error_codes.ErrorCode;
pub const GreyError = grey_error.GreyError;
pub const Result = result.Result;
pub const VoidResult = result.VoidResult;

// Configuration
pub const options = @import("config/options.zig");
pub const Options = options.Options;

// gRPC layer
pub const grpc_client = @import("grpc/client.zig");
pub const auth_service = @import("grpc/auth_service.zig");
pub const user_service = @import("grpc/user_service.zig");
pub const projects_service = @import("grpc/projects_service.zig");
pub const query_service = @import("grpc/query_service.zig");
pub const mutation_service = @import("grpc/mutation_service.zig");

pub const Channel = grpc_client.Channel;

// Domain clients
pub const auth_client = @import("domain/auth_client.zig");
pub const user_client = @import("domain/user_client.zig");
pub const projects_client = @import("domain/projects_client.zig");
pub const query_client = @import("domain/query_client.zig");
pub const mutation_client = @import("domain/mutation_client.zig");

pub const AuthClient = auth_client.AuthClient;
pub const UserClient = user_client.UserClient;
pub const ProjectsClient = projects_client.ProjectsClient;
pub const QueryClient = query_client.QueryClient;
pub const MutationClient = mutation_client.MutationClient;

// Main client
pub const client = @import("client.zig");
pub const GreyClient = client.GreyClient;

/// Create a new Grey client with default local options
pub const createClient = client.createClient;

/// Create a new Grey client with custom options
pub const createClientWithOptions = client.createClientWithOptions;

// Version info
pub const version = "0.0.1";
pub const version_major: u32 = 0;
pub const version_minor: u32 = 0;
pub const version_patch: u32 = 1;

test {
    // Run all tests
    std.testing.refAllDecls(@This());
}
