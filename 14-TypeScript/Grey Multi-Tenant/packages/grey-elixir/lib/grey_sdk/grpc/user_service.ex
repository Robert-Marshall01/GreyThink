defmodule GreySdk.Grpc.UserService do
  @moduledoc """
  gRPC service for user operations.

  This is a placeholder stub. In a real implementation, this would
  use generated protobuf client stubs from .proto files.
  """

  alias GreySdk.Error
  alias GreySdk.Grpc.Channel
  alias GreySdk.Grpc.Types

  @doc """
  Gets the current user via gRPC.
  """
  @spec get_user(GenServer.server(), keyword()) :: {:ok, Types.user()} | {:error, term()}
  def get_user(channel_server, opts \\ []) do
    _force_refresh = Keyword.get(opts, :force_refresh, false)

    simulate_authenticated_grpc_call(channel_server, "user.GetUser", fn ->
      Types.user(
        "stub_user_id",
        "user@example.com",
        "Stub User"
      )
    end)
  end

  # Simulates an authenticated gRPC call
  defp simulate_authenticated_grpc_call(channel_server, _method, result_fn) do
    if Channel.authenticated?(channel_server) do
      with {:ok, _channel} <- Channel.get_channel(channel_server) do
        Process.sleep(1)
        {:ok, result_fn.()}
      end
    else
      {:error, Error.unauthorized()}
    end
  end
end
