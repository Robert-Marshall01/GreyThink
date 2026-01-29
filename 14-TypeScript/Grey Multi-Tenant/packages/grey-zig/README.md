# Grey Multi-Tenant SDK for Zig

A complete SDK for integrating with Grey Multi-Tenant services using gRPC transport via C interop.

## Features

- **gRPC Transport**: Uses gRPC via C interop for efficient communication
- **Idiomatic Zig**: Tagged unions, comptime generics, and proper error handling
- **Type-Safe Results**: `Result(T)` type for explicit error handling
- **Domain Clients**: Auth, User, Projects, Query, Mutation
- **Validation**: Input validation before API calls
- **Allocator-Aware**: Full control over memory allocation

## Installation

Add to your `build.zig.zon`:

```zig
.dependencies = .{
    .@"grey-zig" = .{
        .url = "https://github.com/grey-systems/grey-zig/archive/v0.0.1.tar.gz",
        .hash = "...",
    },
},
```

Then in your `build.zig`:

```zig
const grey_dep = b.dependency("grey-zig", .{
    .target = target,
    .optimize = optimize,
});

exe.root_module.addImport("grey", grey_dep.module("grey"));
```

## Quick Start

```zig
const std = @import("std");
const grey = @import("grey");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Create client
    var client = try grey.createClient(allocator);
    defer client.deinit();

    // Login
    const login_result = client.auth.login("user@example.com", "password123");
    
    if (login_result.isOk()) {
        std.debug.print("Logged in successfully!\n", .{});
        
        // Get current user
        const user_result = client.user.getCurrentUser();
        if (user_result.isOk()) {
            if (user_result.getOrNull()) |user| {
                std.debug.print("Welcome, {s}!\n", .{user.email});
            }
        }
        
        // Logout
        _ = client.auth.logout();
    } else {
        const err = login_result.unwrapErr();
        std.debug.print("Login failed: {s}\n", .{err.message});
    }
}
```

## Configuration

```zig
const grey = @import("grey");

// Local development
const local_opts = grey.Options.local(allocator);

// Production
const prod_opts = grey.Options.production(allocator, "api.grey.systems");

// Custom configuration
var custom_opts = grey.Options{
    .allocator = allocator,
    .host = "custom.grey.local",
    .port = 9090,
    .use_tls = true,
    .timeout_ms = 30000,
};

// With auth token
custom_opts = custom_opts.withAuthToken("your-token");

var client = try grey.createClientWithOptions(allocator, custom_opts);
defer client.deinit();
```

## Domain Clients

### Auth

```zig
// Login
const result = client.auth.login("user@example.com", "password123");
if (result.isOk()) {
    // Token is automatically set on the client
}

// Login with tenant
const result2 = client.auth.loginWithTenant("user@example.com", "password", "tenant-id");

// Refresh token
const refresh_result = client.auth.refresh("refresh-token");

// Logout
_ = client.auth.logout();
```

### User

```zig
// Get current user
const result = client.user.getCurrentUser();
if (result.isOk()) {
    if (result.getOrNull()) |user| {
        std.debug.print("User: {s}", .{user.email});
    }
}

// Get user by ID
const result2 = client.user.getUser("user-123");
```

### Projects

```zig
// List projects
const result = client.projects.listProjects();

// With pagination
const result2 = client.projects.listProjectsWithOptions(.{
    .limit = 20,
    .offset = 0,
});

// Create project
const result3 = client.projects.createProject("My Project", "Description");

// Get project
const result4 = client.projects.getProject("project-123");
```

### Query

```zig
// Simple query
const result = client.query.query("getStats");

// Query with builder
var builder = client.query.queryBuilder("getStats");
defer builder.deinit();

_ = builder.param("project_id", "proj-123");
const request = builder.build();
const result2 = client.query.executeQuery(request);
```

### Mutation

```zig
// Simple mutation
const result = client.mutation.mutate("updateStatus");

// Mutation with builder
var builder = client.mutation.mutationBuilder("updateProject");
defer builder.deinit();

_ = builder
    .param("project_id", "proj-123")
    .param("name", "New Name")
    .forTenant("tenant-1");
const request = builder.build();
const result2 = client.mutation.executeMutation(request);
```

## Error Handling

```zig
const grey = @import("grey");

const result = client.auth.login("user@example.com", "wrong-password");

switch (result) {
    .ok => |response| {
        std.debug.print("Token: {s}\n", .{response.access_token});
    },
    .err => |e| {
        std.debug.print("Error: {s}\n", .{e.code.toString()});
        std.debug.print("Message: {s}\n", .{e.message});
        
        if (e.code.isRetryable()) {
            std.debug.print("This error is retryable\n", .{});
        }
    },
}

// Or use convenience methods
if (result.isOk()) {
    const response = result.getOrNull().?;
    // ...
}

// Get with default
const token = result.map([]const u8, struct {
    fn get(r: anytype) []const u8 {
        return r.access_token;
    }
}.get).getOr("no-token");
```

## Error Codes

| Code | Description |
|------|-------------|
| `unauthorized` | Authentication required or token expired |
| `forbidden` | Access denied |
| `not_found` | Resource not found |
| `validation_error` | Invalid input data |
| `network_error` | Network connectivity issue |
| `timeout` | Request timed out |
| `server_error` | Internal server error |
| `unknown` | Unknown error |

## Building

```bash
# Build library
zig build

# Run tests
zig build test

# Generate docs
zig build docs

# Run example
zig build example
```

## Requirements

- Zig 0.11.0 or later
- grpc-c library (for production use)

## gRPC C Interop

This SDK uses C interop to communicate with gRPC services. You'll need the grpc-c library installed:

```bash
# Ubuntu/Debian
sudo apt-get install libgrpc-dev

# macOS
brew install grpc

# Windows (vcpkg)
vcpkg install grpc
```

Link the library in your build.zig:

```zig
exe.linkSystemLibrary("grpc");
exe.linkLibC();
```

## Project Structure

```
grey-zig/
├── build.zig           # Zig build configuration
├── build.zig.zon       # Package manifest
├── src/
│   ├── grey.zig        # Main module entry
│   ├── client.zig      # GreyClient facade
│   ├── error/
│   │   ├── codes.zig       # Error codes enum
│   │   ├── grey_error.zig  # Error type
│   │   └── result.zig      # Result type
│   ├── config/
│   │   └── options.zig     # Configuration options
│   ├── grpc/
│   │   ├── client.zig           # gRPC channel
│   │   ├── auth_service.zig     # Auth gRPC service
│   │   ├── user_service.zig     # User gRPC service
│   │   ├── projects_service.zig # Projects gRPC service
│   │   ├── query_service.zig    # Query gRPC service
│   │   └── mutation_service.zig # Mutation gRPC service
│   └── domain/
│       ├── auth_client.zig      # Auth domain client
│       ├── user_client.zig      # User domain client
│       ├── projects_client.zig  # Projects domain client
│       ├── query_client.zig     # Query domain client
│       └── mutation_client.zig  # Mutation domain client
├── examples/
│   └── basic.zig       # Basic usage example
└── README.md
```

## License

MIT License - see LICENSE file for details.
