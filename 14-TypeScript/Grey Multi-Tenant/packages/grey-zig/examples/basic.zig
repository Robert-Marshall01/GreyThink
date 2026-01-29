//! Basic Example
//!
//! Demonstrates basic usage of the Grey Multi-Tenant SDK.

const std = @import("std");
const grey = @import("grey");

pub fn main() !void {
    // Use general purpose allocator
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    std.debug.print("Grey Multi-Tenant SDK for Zig\n", .{});
    std.debug.print("Version: {s}\n\n", .{grey.version});

    // Create client with local development options
    var client = try grey.createClient(allocator);
    defer client.deinit();

    std.debug.print("Client created (authenticated: {})\n", .{client.isAuthenticated()});

    // Attempt login
    std.debug.print("\nAttempting login...\n", .{});
    const login_result = client.auth.login("user@example.com", "password123");

    if (login_result.isOk()) {
        std.debug.print("Login successful!\n", .{});

        // Get current user
        const user_result = client.user.getCurrentUser();
        if (user_result.isOk()) {
            if (user_result.getOrNull()) |user| {
                std.debug.print("Current user: {s}\n", .{user.email});
            }
        }

        // List projects
        const projects_result = client.projects.listProjects();
        if (projects_result.isOk()) {
            if (projects_result.getOrNull()) |response| {
                std.debug.print("Found {d} projects\n", .{response.total});
            }
        }

        // Logout
        _ = client.auth.logout();
        std.debug.print("Logged out\n", .{});
    } else {
        const err = login_result.unwrapErr();
        std.debug.print("Login failed: {s} - {s}\n", .{
            err.code.toString(),
            err.message,
        });
    }
}
