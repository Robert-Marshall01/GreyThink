defmodule GreySdk do
  @moduledoc """
  Grey Multi-Tenant SDK for Elixir.

  A Elixir SDK for the Grey Multi-Tenant platform using gRPC transport.

  ## Quick Start

      # Start a client
      {:ok, client} = GreySdk.Client.start_link(host: "api.grey.example.com")

      # Login
      {:ok, auth} = GreySdk.Auth.login(client, "user@example.com", "password")

      # Fetch user
      {:ok, user} = GreySdk.User.get_user(client)
      IO.puts("Hello, \#{user.name}!")

      # List projects
      {:ok, %{projects: projects}} = GreySdk.Projects.list_projects(client)
      Enum.each(projects, &IO.puts(&1.name))

      # Cleanup
      GreySdk.Client.stop(client)

  ## Error Handling

  All operations return `{:ok, result}` or `{:error, %GreySdk.Error{}}`:

      case GreySdk.Auth.login(client, email, password) do
        {:ok, auth} ->
          IO.puts("Logged in!")

        {:error, %GreySdk.Error{code: "VALIDATION_ERROR", message: msg}} ->
          IO.puts("Invalid input: \#{msg}")

        {:error, %GreySdk.Error{code: code, message: msg}} ->
          IO.puts("Error [\#{code}]: \#{msg}")
      end

  ## With Pattern Matching

      with {:ok, _auth} <- GreySdk.Auth.login(client, email, password),
           {:ok, user} <- GreySdk.User.get_user(client),
           {:ok, projects} <- GreySdk.Projects.list_projects(client) do
        {:ok, %{user: user, projects: projects}}
      end

  ## Modules

  - `GreySdk.Client` - Main client GenServer
  - `GreySdk.Auth` - Authentication operations
  - `GreySdk.User` - User operations
  - `GreySdk.Projects` - Project operations
  - `GreySdk.Query` - Generic query operations
  - `GreySdk.Mutation` - Generic mutation operations
  - `GreySdk.Error` - Error struct
  - `GreySdk.Config.Options` - Configuration options
  """

  # Convenience aliases for domain modules
  defdelegate login(client, email, password), to: GreySdk.Client
  defdelegate logout(client), to: GreySdk.Client
  defdelegate refresh(client, refresh_token), to: GreySdk.Client
  defdelegate authenticated?(client), to: GreySdk.Client
  defdelegate get_user(client, opts \\ []), to: GreySdk.Client
  defdelegate list_projects(client, opts \\ []), to: GreySdk.Client
  defdelegate create_project(client, name, opts \\ []), to: GreySdk.Client
  defdelegate query(client, endpoint, opts \\ []), to: GreySdk.Client
  defdelegate mutate(client, endpoint, opts \\ []), to: GreySdk.Client
end
