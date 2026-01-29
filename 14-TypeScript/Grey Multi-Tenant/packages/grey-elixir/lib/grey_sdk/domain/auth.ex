defmodule GreySdk.Domain.Auth do
  @moduledoc """
  Domain module for authentication operations.

  Provides login, logout, and token refresh functionality with
  input validation and error normalization.
  """

  alias GreySdk.Error
  alias GreySdk.Grpc.AuthService
  alias GreySdk.Grpc.Channel
  alias GreySdk.Grpc.Types

  @doc """
  Authenticates a user with email and password.

  On success, stores the access token for subsequent authenticated requests.

  ## Parameters

  - `channel_server` - The channel GenServer pid
  - `email` - The user's email address
  - `password` - The user's password

  ## Returns

  - `{:ok, auth_data}` on success
  - `{:error, %GreySdk.Error{}}` on failure

  ## Examples

      case GreySdk.Domain.Auth.login(channel, "user@example.com", "password") do
        {:ok, auth} ->
          IO.puts("Logged in with token: \#{auth.access_token}")

        {:error, %GreySdk.Error{code: "VALIDATION_ERROR"} = error} ->
          IO.puts("Invalid input: \#{error.message}")

        {:error, %GreySdk.Error{} = error} ->
          IO.puts("Login failed: \#{error.message}")
      end
  """
  @spec login(GenServer.server(), String.t(), String.t()) ::
          {:ok, Types.auth_data()} | {:error, Error.t()}
  def login(channel_server, email, password) do
    with :ok <- validate_email(email),
         :ok <- validate_password(password),
         {:ok, auth_data} <- AuthService.login(channel_server, email, password) do
      # Store the access token
      Channel.set_access_token(channel_server, auth_data.access_token)
      {:ok, auth_data}
    else
      {:error, %Error{} = error} -> {:error, error}
      {:error, reason} -> {:error, Error.from_any(reason)}
    end
  end

  @doc """
  Logs out the current user.

  Clears the stored access token.

  ## Returns

  - `:ok` on success
  - `{:error, %GreySdk.Error{}}` on failure
  """
  @spec logout(GenServer.server()) :: :ok | {:error, Error.t()}
  def logout(channel_server) do
    case AuthService.logout(channel_server) do
      {:ok, :ok} ->
        Channel.set_access_token(channel_server, nil)
        :ok

      {:error, reason} ->
        # Always clear token even on error
        Channel.set_access_token(channel_server, nil)
        {:error, Error.from_any(reason)}
    end
  end

  @doc """
  Refreshes the authentication tokens.

  ## Parameters

  - `channel_server` - The channel GenServer pid
  - `refresh_token` - The refresh token

  ## Returns

  - `{:ok, auth_data}` on success
  - `{:error, %GreySdk.Error{}}` on failure
  """
  @spec refresh(GenServer.server(), String.t()) ::
          {:ok, Types.auth_data()} | {:error, Error.t()}
  def refresh(channel_server, refresh_token) do
    with :ok <- validate_refresh_token(refresh_token),
         {:ok, auth_data} <- AuthService.refresh(channel_server, refresh_token) do
      Channel.set_access_token(channel_server, auth_data.access_token)
      {:ok, auth_data}
    else
      {:error, %Error{} = error} -> {:error, error}
      {:error, reason} -> {:error, Error.from_any(reason)}
    end
  end

  @doc """
  Checks if the client is authenticated.
  """
  @spec authenticated?(GenServer.server()) :: boolean()
  def authenticated?(channel_server) do
    Channel.authenticated?(channel_server)
  end

  # Validation helpers

  defp validate_email(nil), do: {:error, Error.validation("Email is required")}
  defp validate_email(""), do: {:error, Error.validation("Email is required")}
  defp validate_email(email) when is_binary(email), do: :ok
  defp validate_email(_), do: {:error, Error.validation("Email must be a string")}

  defp validate_password(nil), do: {:error, Error.validation("Password is required")}
  defp validate_password(""), do: {:error, Error.validation("Password is required")}
  defp validate_password(password) when is_binary(password), do: :ok
  defp validate_password(_), do: {:error, Error.validation("Password must be a string")}

  defp validate_refresh_token(nil), do: {:error, Error.validation("Refresh token is required")}
  defp validate_refresh_token(""), do: {:error, Error.validation("Refresh token is required")}
  defp validate_refresh_token(token) when is_binary(token), do: :ok
  defp validate_refresh_token(_), do: {:error, Error.validation("Refresh token must be a string")}
end
