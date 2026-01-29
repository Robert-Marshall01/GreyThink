# GreySDK.jl

Julia client SDK for the Grey Multi-Tenant API.

## Installation

```julia
using Pkg
Pkg.add(url="https://github.com/grey/grey-julia")
```

## Requirements

- Julia 1.6+
- HTTP.jl
- JSON3.jl

## Usage

```julia
using GreySDK

# Create client for local development
client = GreyClient(Options.local(8080))

# Or for production
client = GreyClient(Options.production("api.grey.com"))

# Authenticate
result = login(client.auth, email="user@example.com", password="secret")

if is_ok(result)
    tokens = result.data
    println("Access token: ", tokens.access_token)
else
    error = result.error
    println("Error: ", error.message)
end

# List projects
projects_result = list_projects(client.projects)

if is_ok(projects_result)
    for project in projects_result.data.projects
        println("Project: ", project.name)
    end
end

# Create a project
create_result = create_project(client.projects, name="New Project", description="A description")

# Execute a query
query_result = query(client.query_client, query_name="getUsers", variables=Dict("limit" => 10))

# Execute a mutation
mutation_result = mutate(client.mutation_client, 
    mutation_name="updateUser", 
    input=Dict("id" => "123", "name" => "New Name")
)

# Logout
logout(client.auth)
```

## Error Handling

All methods return a `Result{T}` with either data or error:

```julia
result = login(client.auth, email=email, password=password)

if is_ok(result)
    data = result.data
    # Handle success
else
    error = result.error
    # error has: code, message, details
    println("Error code: ", error.code)
    println("Message: ", error.message)
end

# Chaining with map/then
final = result |> 
    r -> map_result(r, d -> d.access_token) |>
    r -> then_result(r, token -> Result(ok=true, data="Token: $token"))
```

## Error Codes

- `:unauthorized` - Authentication required (401)
- `:forbidden` - Permission denied (403)
- `:not_found` - Resource not found (404)
- `:validation_error` - Invalid input (400, 422)
- `:network_error` - Connection failed
- `:timeout` - Request timed out
- `:server_error` - Server error (5xx)
- `:unknown` - Unknown error

## License

MIT
