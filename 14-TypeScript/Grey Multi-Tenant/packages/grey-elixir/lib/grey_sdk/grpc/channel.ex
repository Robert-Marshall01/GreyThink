defmodule GreySdk.Grpc.Channel do
  @moduledoc """
  Manages the gRPC channel connection for Grey SDK.

  This GenServer maintains the gRPC channel, handles authentication
  state, and provides metadata for authenticated requests.
  """

  use GenServer

  alias GreySdk.Config.Options

  @type state :: %{
          options: Options.t(),
          channel: GRPC.Channel.t() | nil,
          access_token: String.t() | nil
        }

  # Client API

  @doc """
  Starts the channel manager.
  """
  @spec start_link(Options.t()) :: GenServer.on_start()
  def start_link(%Options{} = options) do
    GenServer.start_link(__MODULE__, options)
  end

  @doc """
  Gets the gRPC channel.
  """
  @spec get_channel(GenServer.server()) :: {:ok, GRPC.Channel.t()} | {:error, term()}
  def get_channel(server) do
    GenServer.call(server, :get_channel)
  end

  @doc """
  Sets the access token for authenticated requests.
  """
  @spec set_access_token(GenServer.server(), String.t() | nil) :: :ok
  def set_access_token(server, token) do
    GenServer.cast(server, {:set_access_token, token})
  end

  @doc """
  Gets the current access token.
  """
  @spec get_access_token(GenServer.server()) :: String.t() | nil
  def get_access_token(server) do
    GenServer.call(server, :get_access_token)
  end

  @doc """
  Checks if the client is authenticated.
  """
  @spec authenticated?(GenServer.server()) :: boolean()
  def authenticated?(server) do
    GenServer.call(server, :authenticated?)
  end

  @doc """
  Gets metadata with authorization header.
  """
  @spec auth_metadata(GenServer.server()) :: map()
  def auth_metadata(server) do
    GenServer.call(server, :auth_metadata)
  end

  @doc """
  Gets the request timeout.
  """
  @spec timeout(GenServer.server()) :: non_neg_integer()
  def timeout(server) do
    GenServer.call(server, :timeout)
  end

  @doc """
  Shuts down the channel.
  """
  @spec shutdown(GenServer.server()) :: :ok
  def shutdown(server) do
    GenServer.stop(server, :normal)
  end

  # Server Callbacks

  @impl true
  def init(%Options{} = options) do
    state = %{
      options: options,
      channel: nil,
      access_token: nil
    }

    {:ok, state, {:continue, :connect}}
  end

  @impl true
  def handle_continue(:connect, state) do
    case connect(state.options) do
      {:ok, channel} ->
        {:noreply, %{state | channel: channel}}

      {:error, reason} ->
        # Log error but don't crash - will retry on next request
        require Logger
        Logger.warning("Failed to connect to Grey API: #{inspect(reason)}")
        {:noreply, state}
    end
  end

  @impl true
  def handle_call(:get_channel, _from, %{channel: nil} = state) do
    case connect(state.options) do
      {:ok, channel} ->
        {:reply, {:ok, channel}, %{state | channel: channel}}

      {:error, _} = error ->
        {:reply, error, state}
    end
  end

  def handle_call(:get_channel, _from, %{channel: channel} = state) do
    {:reply, {:ok, channel}, state}
  end

  def handle_call(:get_access_token, _from, state) do
    {:reply, state.access_token, state}
  end

  def handle_call(:authenticated?, _from, state) do
    {:reply, state.access_token != nil, state}
  end

  def handle_call(:auth_metadata, _from, state) do
    metadata =
      case state.access_token do
        nil -> %{}
        token -> %{"authorization" => "Bearer #{token}"}
      end

    {:reply, metadata, state}
  end

  def handle_call(:timeout, _from, state) do
    {:reply, state.options.timeout_ms, state}
  end

  @impl true
  def handle_cast({:set_access_token, token}, state) do
    {:noreply, %{state | access_token: token}}
  end

  @impl true
  def terminate(_reason, %{channel: nil}), do: :ok

  def terminate(_reason, %{channel: channel}) do
    GRPC.Stub.disconnect(channel)
    :ok
  rescue
    _ -> :ok
  end

  # Private functions

  defp connect(%Options{} = options) do
    endpoint = Options.endpoint(options)

    cred =
      if options.use_tls do
        GRPC.Credential.new(ssl: [])
      else
        nil
      end

    case GRPC.Stub.connect(endpoint, cred: cred) do
      {:ok, channel} -> {:ok, channel}
      {:error, reason} -> {:error, reason}
    end
  rescue
    e -> {:error, e}
  end
end
