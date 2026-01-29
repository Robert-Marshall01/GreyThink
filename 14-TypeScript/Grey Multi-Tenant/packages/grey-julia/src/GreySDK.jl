"""
    GreySDK

A Julia SDK for the Grey Multi-Tenant platform.

# Usage

```julia
using GreySDK

# Create a client for local development
client = GreySDK.create_client(GreySDK.local_options(8080))

# Create a client for production
client = GreySDK.create_client(GreySDK.production_options("api.grey.com"))

# Login
result = GreySDK.login(client, "user", "password")
if GreySDK.is_ok(result)
    println("Logged in successfully")
end

# List projects
result = GreySDK.list_projects(client)
if GreySDK.is_ok(result)
    projects = result.data
end

# Execute a query
result = GreySDK.query(client, "{ users { id name } }")
```
"""
module GreySDK

# Export main types and functions
export GreyClient, create_client,
       local_options, production_options,
       is_ok, is_err, unwrap, unwrap_or,
       # Auth
       login, logout, refresh,
       # User
       get_user, get_current_user,
       # Projects
       list_projects, get_project, create_project, update_project, delete_project,
       # Query
       query, batch_query,
       # Mutation
       mutate, batch_mutate

# Include all modules in dependency order
include("ErrorCodes.jl")
include("GreyError.jl")
include("Results.jl")
include("Options.jl")
include("HttpClient.jl")
include("AuthClient.jl")
include("UserClient.jl")
include("ProjectsClient.jl")
include("QueryClient.jl")
include("MutationClient.jl")

# Re-export from submodules
using .ErrorCodes
using .GreyError
using .Results
using .Options
using .HttpClient
using .AuthClient
using .UserClient
using .ProjectsClient
using .QueryClient
using .MutationClient

# Type alias for the client
const GreyClient = HttpClient.Client

"""
    create_client(options::GreyOptions) -> GreyClient

Create a new Grey SDK client with the given options.
"""
function create_client(options::Options.GreyOptions)::GreyClient
    HttpClient.Client(options)
end

# Re-export Options factories
local_options = Options.local_options
production_options = Options.production_options

# Re-export Result utilities
is_ok = Results.is_ok
is_err = Results.is_err
unwrap = Results.unwrap
unwrap_or = Results.unwrap_or

# Re-export Auth functions
"""Login with username and password."""
function login(client::GreyClient, username::String, password::String)
    AuthClient.login(client, username, password)
end

"""Logout the current user."""
function logout(client::GreyClient)
    AuthClient.logout(client)
end

"""Refresh the access token."""
function refresh(client::GreyClient, refresh_token::String)
    AuthClient.refresh(client, refresh_token)
end

# Re-export User functions
"""Get a user by ID."""
function get_user(client::GreyClient, user_id::String)
    UserClient.get_user(client, user_id)
end

"""Get the current authenticated user."""
function get_current_user(client::GreyClient)
    UserClient.get_current_user(client)
end

# Re-export Projects functions
"""List all projects with optional pagination."""
function list_projects(client::GreyClient; kwargs...)
    ProjectsClient.list_projects(client; kwargs...)
end

"""Get a project by ID."""
function get_project(client::GreyClient, project_id::String)
    ProjectsClient.get_project(client, project_id)
end

"""Create a new project."""
function create_project(client::GreyClient, name::String; kwargs...)
    ProjectsClient.create_project(client, name; kwargs...)
end

"""Update an existing project."""
function update_project(client::GreyClient, project_id::String; kwargs...)
    ProjectsClient.update_project(client, project_id; kwargs...)
end

"""Delete a project by ID."""
function delete_project(client::GreyClient, project_id::String)
    ProjectsClient.delete_project(client, project_id)
end

# Re-export Query functions
"""Execute a GraphQL-style query."""
function query(client::GreyClient, query_string::String; kwargs...)
    QueryClient.query(client, query_string; kwargs...)
end

"""Execute multiple queries in a batch."""
function batch_query(client::GreyClient, queries::Vector)
    QueryClient.batch_query(client, queries)
end

# Re-export Mutation functions
"""Execute a GraphQL-style mutation."""
function mutate(client::GreyClient, mutation_string::String; kwargs...)
    MutationClient.mutate(client, mutation_string; kwargs...)
end

"""Execute multiple mutations in a batch."""
function batch_mutate(client::GreyClient, mutations::Vector)
    MutationClient.batch_mutate(client, mutations)
end

end # module
