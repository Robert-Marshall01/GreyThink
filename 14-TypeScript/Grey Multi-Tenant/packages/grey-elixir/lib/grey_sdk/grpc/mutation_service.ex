defmodule GreySdk.Grpc.MutationService do
  @moduledoc """
  gRPC service for generic mutation operations.

  This is a placeholder stub. In a real implementation, this would
  use generated protobuf client stubs from .proto files.
  """

  alias GreySdk.Error
  alias GreySdk.Grpc.Channel
  alias GreySdk.Grpc.Types

  @doc """
  Executes a mutation via gRPC.
  """
  @spec mutate(GenServer.server(), String.t(), keyword()) ::
          {:ok, Types.mutation_data()} | {:error, term()}
  def mutate(channel_server, endpoint, opts \\ []) do
    method = Keyword.get(opts, :method, "POST")
    body = Keyword.get(opts, :body)

    simulate_authenticated_grpc_call(channel_server, "mutation.Mutate", fn ->
      Types.mutation_data(
        true,
        %{
          endpoint: endpoint,
          method: method,
          received: body,
          stub: true
        }
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
