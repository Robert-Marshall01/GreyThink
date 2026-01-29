defmodule GreySdk.Domain.Mutation do
  @moduledoc """
  Domain module for generic mutation operations.

  Provides a flexible mutation interface for custom endpoints.
  """

  alias GreySdk.Error
  alias GreySdk.Grpc.Channel
  alias GreySdk.Grpc.MutationService
  alias GreySdk.Grpc.Types

  @valid_methods ~w(POST PUT PATCH DELETE)

  @doc """
  Executes a mutation against the specified endpoint.

  ## Parameters

  - `channel_server` - The channel GenServer pid
  - `endpoint` - The mutation endpoint path
  - `opts` - Additional options
    - `:method` - HTTP method equivalent (default: "POST")
    - `:body` - Request body

  ## Returns

  - `{:ok, mutation_data}` on success
  - `{:error, %GreySdk.Error{}}` if not authenticated or on failure

  ## Examples

      case GreySdk.Domain.Mutation.mutate(channel, "/api/items", method: "POST", body: %{name: "Item"}) do
        {:ok, %{success: true, data: data}} ->
          IO.puts("Created: \#{inspect(data)}")

        {:ok, %{success: false}} ->
          IO.puts("Mutation failed")

        {:error, error} ->
          IO.puts("Error: \#{error.message}")
      end
  """
  @spec mutate(GenServer.server(), String.t(), keyword()) ::
          {:ok, Types.mutation_data()} | {:error, Error.t()}
  def mutate(channel_server, endpoint, opts \\ []) do
    method = opts |> Keyword.get(:method, "POST") |> String.upcase()

    with :ok <- validate_endpoint(endpoint),
         :ok <- validate_method(method),
         :ok <- validate_authenticated(channel_server),
         {:ok, mutation_data} <- MutationService.mutate(channel_server, endpoint, opts) do
      {:ok, mutation_data}
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

  defp validate_method(method) when method in @valid_methods, do: :ok

  defp validate_method(_method) do
    {:error, Error.validation("Method must be one of: #{Enum.join(@valid_methods, ", ")}")}
  end

  defp validate_authenticated(channel_server) do
    if Channel.authenticated?(channel_server) do
      :ok
    else
      {:error, Error.unauthorized()}
    end
  end
end
