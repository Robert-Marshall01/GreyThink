defmodule GreySdk.Domain.User do
  @moduledoc """
  Domain module for user operations.

  Provides user retrieval with optional caching.
  """

  alias GreySdk.Error
  alias GreySdk.Grpc.Channel
  alias GreySdk.Grpc.Types
  alias GreySdk.Grpc.UserService

  @doc """
  Gets the current authenticated user.

  ## Options

  - `:force_refresh` - Whether to bypass cache and fetch fresh data (default: false)

  ## Returns

  - `{:ok, user}` on success
  - `{:error, %GreySdk.Error{}}` if not authenticated or on failure

  ## Examples

      case GreySdk.Domain.User.get_user(channel) do
        {:ok, user} ->
          IO.puts("User: \#{user.name} <\#{user.email}>")

        {:error, %GreySdk.Error{code: "UNAUTHORIZED"}} ->
          IO.puts("Not logged in")

        {:error, error} ->
          IO.puts("Error: \#{error.message}")
      end
  """
  @spec get_user(GenServer.server(), keyword()) :: {:ok, Types.user()} | {:error, Error.t()}
  def get_user(channel_server, opts \\ []) do
    if not Channel.authenticated?(channel_server) do
      {:error, Error.unauthorized()}
    else
      case UserService.get_user(channel_server, opts) do
        {:ok, user} -> {:ok, user}
        {:error, %Error{} = error} -> {:error, error}
        {:error, reason} -> {:error, Error.from_any(reason)}
      end
    end
  end
end
