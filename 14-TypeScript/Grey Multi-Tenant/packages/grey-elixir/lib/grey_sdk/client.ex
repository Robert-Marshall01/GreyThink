defmodule GreySdk.Client do
  @moduledoc """
  Main entry point for the Grey SDK.

  This module provides a GenServer-based client that manages the gRPC
  connection and provides access to all domain operations.

  ## Usage

      # Start the client
      {:ok, client} = GreySdk.Client.start_link(host: "api.grey.example.com")

      # Login
      {:ok, auth} = GreySdk.Client.login(client, "user@example.com", "password")

      # Use domain operations
      {:ok, user} = GreySdk.Client.get_user(client)
      {:ok, projects} = GreySdk.Client.list_projects(client)

      # Stop the client
      GreySdk.Client.stop(client)

  ## Supervision

  The client can be started under a supervisor:

      children = [
        {GreySdk.Client, host: "api.grey.example.com", name: MyApp.GreyClient}
      ]

      Supervisor.start_link(children, strategy: :one_for_one)

  Then use with the registered name:

      GreySdk.Client.login(MyApp.GreyClient, email, password)
  """

  use GenServer

  alias GreySdk.Config.Options
  alias GreySdk.Domain.Auth
  alias GreySdk.Domain.Mutation
  alias GreySdk.Domain.Projects
  alias GreySdk.Domain.Query
  alias GreySdk.Domain.User
  alias GreySdk.Grpc.Channel
  alias GreySdk.Grpc.Types

  @type client :: GenServer.server()

  # Client API

  @doc """
  Starts the Grey SDK client.

  ## Options

  - `:host` - The Grey API host (required)
  - `:port` - The port (default: 443)
  - `:use_tls` - Whether to use TLS (default: true)
  - `:timeout_ms` - Request timeout in ms (default: 30000)
  - `:name` - Optional GenServer name for registration

  ## Examples

      {:ok, client} = GreySdk.Client.start_link(host: "api.grey.example.com")

      # With a registered name
      {:ok, _} = GreySdk.Client.start_link(
        host: "api.grey.example.com",
        name: MyApp.GreyClient
      )
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts) when is_list(opts) do
    {name, opts} = Keyword.pop(opts, :name)
    options = Options.new(opts)

    gen_opts = if name, do: [name: name], else: []
    GenServer.start_link(__MODULE__, options, gen_opts)
  end

  @doc """
  Stops the client.
  """
  @spec stop(client()) :: :ok
  def stop(client) do
    GenServer.stop(client, :normal)
  end

  # Auth operations

  @doc """
  Authenticates a user with email and password.

  See `GreySdk.Domain.Auth.login/3` for details.
  """
  @spec login(client(), String.t(), String.t()) ::
          {:ok, Types.auth_data()} | {:error, GreySdk.Error.t()}
  def login(client, email, password) do
    GenServer.call(client, {:auth, :login, [email, password]})
  end

  @doc """
  Logs out the current user.

  See `GreySdk.Domain.Auth.logout/1` for details.
  """
  @spec logout(client()) :: :ok | {:error, GreySdk.Error.t()}
  def logout(client) do
    GenServer.call(client, {:auth, :logout, []})
  end

  @doc """
  Refreshes the authentication tokens.

  See `GreySdk.Domain.Auth.refresh/2` for details.
  """
  @spec refresh(client(), String.t()) ::
          {:ok, Types.auth_data()} | {:error, GreySdk.Error.t()}
  def refresh(client, refresh_token) do
    GenServer.call(client, {:auth, :refresh, [refresh_token]})
  end

  @doc """
  Checks if the client is authenticated.
  """
  @spec authenticated?(client()) :: boolean()
  def authenticated?(client) do
    GenServer.call(client, :authenticated?)
  end

  # User operations

  @doc """
  Gets the current authenticated user.

  See `GreySdk.Domain.User.get_user/2` for details.
  """
  @spec get_user(client(), keyword()) :: {:ok, Types.user()} | {:error, GreySdk.Error.t()}
  def get_user(client, opts \\ []) do
    GenServer.call(client, {:user, :get_user, [opts]})
  end

  # Projects operations

  @doc """
  Lists projects with pagination.

  See `GreySdk.Domain.Projects.list_projects/2` for details.
  """
  @spec list_projects(client(), keyword()) ::
          {:ok, Types.projects_data()} | {:error, GreySdk.Error.t()}
  def list_projects(client, opts \\ []) do
    GenServer.call(client, {:projects, :list_projects, [opts]})
  end

  @doc """
  Creates a new project.

  See `GreySdk.Domain.Projects.create_project/3` for details.
  """
  @spec create_project(client(), String.t(), keyword()) ::
          {:ok, Types.project()} | {:error, GreySdk.Error.t()}
  def create_project(client, name, opts \\ []) do
    GenServer.call(client, {:projects, :create_project, [name, opts]})
  end

  # Query operations

  @doc """
  Executes a query against the specified endpoint.

  See `GreySdk.Domain.Query.query/3` for details.
  """
  @spec query(client(), String.t(), keyword()) ::
          {:ok, Types.query_data()} | {:error, GreySdk.Error.t()}
  def query(client, endpoint, opts \\ []) do
    GenServer.call(client, {:query, :query, [endpoint, opts]})
  end

  # Mutation operations

  @doc """
  Executes a mutation against the specified endpoint.

  See `GreySdk.Domain.Mutation.mutate/3` for details.
  """
  @spec mutate(client(), String.t(), keyword()) ::
          {:ok, Types.mutation_data()} | {:error, GreySdk.Error.t()}
  def mutate(client, endpoint, opts \\ []) do
    GenServer.call(client, {:mutation, :mutate, [endpoint, opts]})
  end

  # Server Callbacks

  @impl true
  def init(%Options{} = options) do
    {:ok, channel} = Channel.start_link(options)
    {:ok, %{channel: channel, options: options}}
  end

  @impl true
  def handle_call({:auth, :login, [email, password]}, _from, state) do
    result = Auth.login(state.channel, email, password)
    {:reply, result, state}
  end

  def handle_call({:auth, :logout, []}, _from, state) do
    result = Auth.logout(state.channel)
    {:reply, result, state}
  end

  def handle_call({:auth, :refresh, [refresh_token]}, _from, state) do
    result = Auth.refresh(state.channel, refresh_token)
    {:reply, result, state}
  end

  def handle_call(:authenticated?, _from, state) do
    result = Auth.authenticated?(state.channel)
    {:reply, result, state}
  end

  def handle_call({:user, :get_user, [opts]}, _from, state) do
    result = User.get_user(state.channel, opts)
    {:reply, result, state}
  end

  def handle_call({:projects, :list_projects, [opts]}, _from, state) do
    result = Projects.list_projects(state.channel, opts)
    {:reply, result, state}
  end

  def handle_call({:projects, :create_project, [name, opts]}, _from, state) do
    result = Projects.create_project(state.channel, name, opts)
    {:reply, result, state}
  end

  def handle_call({:query, :query, [endpoint, opts]}, _from, state) do
    result = Query.query(state.channel, endpoint, opts)
    {:reply, result, state}
  end

  def handle_call({:mutation, :mutate, [endpoint, opts]}, _from, state) do
    result = Mutation.mutate(state.channel, endpoint, opts)
    {:reply, result, state}
  end

  @impl true
  def terminate(_reason, state) do
    Channel.shutdown(state.channel)
    :ok
  rescue
    _ -> :ok
  end
end
