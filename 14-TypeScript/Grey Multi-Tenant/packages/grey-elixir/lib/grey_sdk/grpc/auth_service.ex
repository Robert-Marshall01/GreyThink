defmodule GreySdk.Grpc.AuthService do
  @moduledoc """
  gRPC service for authentication operations.

  This is a placeholder stub. In a real implementation, this would
  use generated protobuf client stubs from .proto files.
  """

  alias GreySdk.Grpc.Channel
  alias GreySdk.Grpc.Types

  @doc """
  Performs login via gRPC.
  """
  @spec login(GenServer.server(), String.t(), String.t()) ::
          {:ok, Types.auth_data()} | {:error, term()}
  def login(channel_server, email, password) do
    # In a real implementation:
    # with {:ok, channel} <- Channel.get_channel(channel_server),
    #      request = AuthProto.LoginRequest.new(email: email, password: password),
    #      {:ok, response} <- AuthProto.Stub.login(channel, request) do
    #   {:ok, Types.auth_data(response.access_token, response.refresh_token, response.expires_in)}
    # end

    simulate_grpc_call(channel_server, "auth.Login", fn ->
      Types.auth_data(
        "stub_access_token",
        "stub_refresh_token",
        3600
      )
    end)
  end

  @doc """
  Performs logout via gRPC.
  """
  @spec logout(GenServer.server()) :: :ok | {:error, term()}
  def logout(channel_server) do
    simulate_grpc_call(channel_server, "auth.Logout", fn -> :ok end)
  end

  @doc """
  Refreshes authentication tokens via gRPC.
  """
  @spec refresh(GenServer.server(), String.t()) ::
          {:ok, Types.auth_data()} | {:error, term()}
  def refresh(channel_server, refresh_token) do
    simulate_grpc_call(channel_server, "auth.Refresh", fn ->
      Types.auth_data(
        "stub_new_access_token",
        "stub_new_refresh_token",
        3600
      )
    end)
  end

  # Simulates a gRPC call for stub implementation
  defp simulate_grpc_call(channel_server, _method, result_fn) do
    with {:ok, _channel} <- Channel.get_channel(channel_server) do
      # Simulate network latency
      Process.sleep(1)
      {:ok, result_fn.()}
    end
  end
end
