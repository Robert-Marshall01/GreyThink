# Grey Multi-Tenant SDK for Mojo

A complete SDK for integrating with Grey Multi-Tenant services using gRPC transport via Python interop.

## Features

- **gRPC Transport**: Uses Python gRPC stubs through Mojo's Python interop
- **Idiomatic Mojo**: Structs, value semantics, and type-safe results
- **Type-Safe Results**: `Result[T]` type for explicit error handling
- **Domain Clients**: Auth, User, Projects, Query, Mutation
- **Validation**: Input validation before API calls
- **Builder Pattern**: Fluent API for queries and mutations

## Requirements

- Mojo 24.1 or later
- Python 3.10+ with grpcio installed
- Generated Python gRPC stubs (`grey_pb2.py`, `grey_pb2_grpc.py`)

## Installation

1. Add the package to your Mojo project
2. Install Python dependencies:

```bash
pip install grpcio grpcio-tools
```

3. Generate Python gRPC stubs from your proto files:

```bash
python -m grpc_tools.protoc -I. --python_out=. --grpc_python_out=. grey.proto
```

## Quick Start

```mojo
from grey import create_client

fn main() raises:
    # Create client for local development
    var client = create_client()
    
    # Login
    let login_result = client.auth.login("user@example.com", "password123")
    
    if login_result.is_ok():
        print("Logged in successfully!")
        
        # Get current user
        let user_result = client.user.get_current_user()
        if user_result.is_ok():
            let user = user_result.unwrap()
            print("Welcome, " + user.email)
        
        # List projects
        let projects_result = client.projects.list_projects()
        if projects_result.is_ok():
            let response = projects_result.unwrap()
            print("Found " + str(response.total) + " projects")
        
        # Logout
        _ = client.auth.logout()
    else:
        let error = login_result.error()
        print("Login failed: " + error.message)
```

## Configuration

```mojo
from grey import Options, create_client_with_options

fn main() raises:
    # Local development
    let local_opts = Options.local()
    
    # Production
    let prod_opts = Options.production("api.grey.systems")
    
    # Custom configuration
    var custom_opts = Options(
        host="custom.grey.local",
        port=9090,
        use_tls=True,
        timeout_ms=60000,
    )
    
    # With auth token
    custom_opts = custom_opts.with_auth_token("your-token")
    
    # Create client
    var client = create_client_with_options(custom_opts)
```

## Domain Clients

### Auth

```mojo
# Login
let result = client.auth.login("user@example.com", "password123")
if result.is_ok():
    let response = result.unwrap()
    print("Token: " + response.access_token)

# Login with tenant
let result2 = client.auth.login_with_tenant("user@example.com", "password", "tenant-id")

# Refresh token
let refresh_result = client.auth.refresh("refresh-token")

# Logout
_ = client.auth.logout()
```

### User

```mojo
# Get current user
let result = client.user.get_current_user()
if result.is_ok():
    let user = result.unwrap()
    print("User: " + user.email)

# Get user by ID
let result2 = client.user.get_user("user-123")
```

### Projects

```mojo
# List projects
let result = client.projects.list_projects()

# With pagination
from grey import PaginationOptions

let result2 = client.projects.list_projects_with_options(
    PaginationOptions(limit=20, offset=0)
)

# Create project
let result3 = client.projects.create_project("My Project", "Description")

# Get project
let result4 = client.projects.get_project("project-123")
```

### Query

```mojo
# Simple query
let result = client.query.query("getStats")

# Query with builder
var builder = client.query.query_builder("getStats")
_ = builder.param("project_id", "proj-123")
let request = builder.build()
let result2 = client.query.execute_query(request)
```

### Mutation

```mojo
# Simple mutation
let result = client.mutation.mutate("updateStatus")

# Mutation with builder
var builder = client.mutation.mutation_builder("updateProject")
_ = builder.param("project_id", "proj-123")
_ = builder.param("name", "New Name")
_ = builder.for_tenant("tenant-1")
let request = builder.build()
let result2 = client.mutation.execute_mutation(request)
```

## Error Handling

```mojo
from grey import ErrorCode

let result = client.auth.login("user@example.com", "wrong-password")

if result.is_err():
    let error = result.error()
    print("Error code: " + error.code.to_string())
    print("Message: " + error.message)
    
    if error.is_retryable():
        print("This error is retryable")
    
    # Check specific error types
    if error.code == ErrorCode.UNAUTHORIZED:
        print("Invalid credentials")
    elif error.code == ErrorCode.NETWORK_ERROR:
        print("Network issue, please retry")

# Use unwrap_or for defaults
let token = result.unwrap_or(LoginResponse()).access_token
```

## Error Codes

| Code | Description |
|------|-------------|
| `UNAUTHORIZED` | Authentication required or token expired |
| `FORBIDDEN` | Access denied |
| `NOT_FOUND` | Resource not found |
| `VALIDATION_ERROR` | Invalid input data |
| `NETWORK_ERROR` | Network connectivity issue |
| `TIMEOUT` | Request timed out |
| `SERVER_ERROR` | Internal server error |
| `UNKNOWN` | Unknown error |

## Project Structure

```
grey-mojo/
├── src/
│   ├── grey.mojo           # Main module entry
│   ├── client.mojo         # GreyClient facade
│   ├── error/
│   │   ├── codes.mojo          # Error codes
│   │   ├── grey_error.mojo     # Error type
│   │   └── result.mojo         # Result type
│   ├── config/
│   │   └── options.mojo        # Configuration options
│   ├── grpc/
│   │   ├── client.mojo             # gRPC channel
│   │   ├── auth_service.mojo       # Auth gRPC service
│   │   ├── user_service.mojo       # User gRPC service
│   │   ├── projects_service.mojo   # Projects gRPC service
│   │   ├── query_service.mojo      # Query gRPC service
│   │   └── mutation_service.mojo   # Mutation gRPC service
│   └── domain/
│       ├── auth_client.mojo        # Auth domain client
│       ├── user_client.mojo        # User domain client
│       ├── projects_client.mojo    # Projects domain client
│       ├── query_client.mojo       # Query domain client
│       └── mutation_client.mojo    # Mutation domain client
└── README.md
```

## Python Interop

This SDK uses Mojo's Python interop to call Python gRPC stubs. Ensure your Python environment has:

1. `grpcio` - gRPC Python library
2. `grey_pb2.py` - Generated protobuf messages
3. `grey_pb2_grpc.py` - Generated gRPC stubs

The Python modules should be importable from your Mojo environment.

## Building

```bash
# Build package
mojo build src/grey.mojo -o grey.mojopkg

# Run tests
mojo test src/
```

## License

MIT License - see LICENSE file for details.
