defmodule GreySdk.Domain.Query do
  @moduledoc """
  Domain module for generic query operations.

  Provides a flexible query interface for custom endpoints.
  """

  alias GreySdk.Error
  alias GreySdk.Grpc.Channel
  alias GreySdk.Grpc.QueryService
  alias GreySdk.Grpc.Types

  @doc """
  Executes a query against the specified endpoint.

  ## Parameters

  - `channel_server` - The channel GenServer pid
  - `endpoint` - The query endpoint path
  - `opts` - Additional options
    - `:params` - Query parameters map (default: %{})
    - `:require_auth` - Whether authentication is required (default: true)

  ## Returns

  - `{:ok, query_data}` on success
  - `{:error, %GreySdk.Error{}}` on failure

  ## Examples

      case GreySdk.Domain.Query.query(channel, "/api/data", params: %{filter: "active"}) do
        {:ok, %{data: data}} ->
          IO.inspect(data)

        {:error, error} ->
          IO.puts("Error: \#{error.message}")
      end
  """
  @spec query(GenServer.server(), String.t(), keyword()) ::
          {:ok, Types.query_data()} | {:error, Error.t()}
  def query(channel_server, endpoint, opts \\ []) do
    require_auth = Keyword.get(opts, :require_auth, true)

    with :ok <- validate_endpoint(endpoint),
         :ok <- validate_auth_if_required(channel_server, require_auth),
         {:ok, query_data} <- QueryService.query(channel_server, endpoint, opts) do
      {:ok, query_data}
    else
      {:error, %Error{} = error} -> {:error, error}
      {:error, reason} -> {:error, Error.from_any(reason)}
    end
  end

  # Validation helpers

  defp validate_endpoint(nil), do: {:error, Error.validation("Endpoint is required")}
  defp validate_endpoint(""), do: {:error, Error.validation("Endpoint is required")}
  defp validate_endpoint(endpoint) when is_binary(endpoint), do: :ok
  defp validate_endpoint(_), do: {:error, Error.validation("Endpoint must be a string")}

  defp validate_auth_if_required(channel_server, true) do
    if Channel.authenticated?(channel_server) do
      :ok
    else
      {:error, Error.unauthorized()}
    end
  end

  defp validate_auth_if_required(_channel_server, false), do: :ok
end
