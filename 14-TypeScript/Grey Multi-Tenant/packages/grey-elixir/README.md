# Grey Multi-Tenant SDK - Elixir

Elixir SDK for the Grey Multi-Tenant platform using gRPC transport.

## Installation

Add the dependency to your `mix.exs`:

```elixir
def deps do
  [
    {:grey_sdk, "~> 0.1.0"}
  ]
end
```

Then run:

```bash
mix deps.get
```

## Configuration

Configure the SDK in your `config/config.exs`:

```elixir
config :grey_sdk,
  host: "api.grey.example.com",
  port: 443,
  use_tls: true,
  timeout_ms: 30_000
```

## Usage

```elixir
# Create client
{:ok, client} = GreySdk.Client.start_link(
  host: "api.grey.example.com",
  port: 443
)

# Login
case GreySdk.Auth.login(client, "user@example.com", "password") do
  {:ok, auth} ->
    IO.puts("Logged in: #{auth.access_token}")

    # Fetch user
    case GreySdk.User.get_user(client) do
      {:ok, user} ->
        IO.puts("User: #{user.name}")

      {:error, %GreySdk.Error{code: code, message: message}} ->
        IO.puts("Error: [#{code}] #{message}")
    end

  {:error, %GreySdk.Error{} = error} ->
    IO.puts("Login failed: [#{error.code}] #{error.message}")
end

# Cleanup
GreySdk.Client.stop(client)
```

## Pattern Matching with Results

```elixir
with {:ok, _auth} <- GreySdk.Auth.login(client, email, password),
     {:ok, user} <- GreySdk.User.get_user(client),
     {:ok, projects} <- GreySdk.Projects.list_projects(client) do
  {:ok, %{user: user, projects: projects}}
else
  {:error, %GreySdk.Error{} = error} ->
    {:error, error}
end
```

## Features

- **gRPC Transport**: Fast, type-safe binary protocol
- **OTP Compliant**: GenServer-based client with supervision support
- **Pattern Matching**: Idiomatic `{:ok, result}` / `{:error, reason}` tuples
- **Error Normalization**: Consistent `%GreySdk.Error{}` across all operations
- **Domain Modules**: Auth, User, Projects, Query, Mutation

## API

### GreySdk.Auth

- `login(client, email, password)` - `{:ok, AuthData.t()} | {:error, Error.t()}`
- `logout(client)` - `:ok | {:error, Error.t()}`
- `refresh(client, refresh_token)` - `{:ok, AuthData.t()} | {:error, Error.t()}`

### GreySdk.User

- `get_user(client, opts \\ [])` - `{:ok, User.t()} | {:error, Error.t()}`

### GreySdk.Projects

- `list_projects(client, opts \\ [])` - `{:ok, ProjectsData.t()} | {:error, Error.t()}`
- `create_project(client, name, opts \\ [])` - `{:ok, Project.t()} | {:error, Error.t()}`

### GreySdk.Query

- `query(client, endpoint, opts \\ [])` - `{:ok, QueryData.t()} | {:error, Error.t()}`

### GreySdk.Mutation

- `mutate(client, endpoint, opts \\ [])` - `{:ok, MutationData.t()} | {:error, Error.t()}`

## License

MIT
