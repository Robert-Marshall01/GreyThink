defmodule GreySdk.Error do
  @moduledoc """
  Normalized error structure for all Grey SDK operations.

  All errors from the Grey SDK are normalized into this structure,
  providing consistent error handling across all operations.

  ## Fields

  - `code` - Error code identifying the type of error
  - `message` - Human-readable error message
  - `details` - Optional map with additional error details

  ## Common Error Codes

  - `"UNAUTHORIZED"` - Authentication required or failed
  - `"FORBIDDEN"` - Permission denied
  - `"NOT_FOUND"` - Resource not found
  - `"VALIDATION_ERROR"` - Invalid input
  - `"NETWORK_ERROR"` - Network connectivity issue
  - `"TIMEOUT"` - Request timed out
  - `"SERVER_ERROR"` - Internal server error
  - `"UNKNOWN"` - Unknown error

  ## Examples

      case GreySdk.Auth.login(client, email, password) do
        {:ok, auth} ->
          {:ok, auth}

        {:error, %GreySdk.Error{code: "UNAUTHORIZED"} = error} ->
          Logger.warning("Auth failed: \#{error.message}")
          {:error, :unauthorized}

        {:error, %GreySdk.Error{code: code, message: message}} ->
          Logger.error("Error [\#{code}]: \#{message}")
          {:error, :unknown}
      end
  """

  @type t :: %__MODULE__{
          code: String.t(),
          message: String.t(),
          details: map() | nil
        }

  @enforce_keys [:code, :message]
  defstruct [:code, :message, :details]

  # Common error codes
  @unauthorized "UNAUTHORIZED"
  @forbidden "FORBIDDEN"
  @not_found "NOT_FOUND"
  @validation_error "VALIDATION_ERROR"
  @network_error "NETWORK_ERROR"
  @timeout "TIMEOUT"
  @server_error "SERVER_ERROR"
  @unknown "UNKNOWN"

  @doc "Returns the UNAUTHORIZED error code."
  def unauthorized_code, do: @unauthorized

  @doc "Returns the FORBIDDEN error code."
  def forbidden_code, do: @forbidden

  @doc "Returns the NOT_FOUND error code."
  def not_found_code, do: @not_found

  @doc "Returns the VALIDATION_ERROR error code."
  def validation_error_code, do: @validation_error

  @doc "Returns the NETWORK_ERROR error code."
  def network_error_code, do: @network_error

  @doc "Returns the TIMEOUT error code."
  def timeout_code, do: @timeout

  @doc "Returns the SERVER_ERROR error code."
  def server_error_code, do: @server_error

  @doc "Returns the UNKNOWN error code."
  def unknown_code, do: @unknown

  @doc """
  Creates an unauthorized error.

  ## Examples

      iex> GreySdk.Error.unauthorized()
      %GreySdk.Error{code: "UNAUTHORIZED", message: "Not authenticated"}

      iex> GreySdk.Error.unauthorized("Invalid credentials")
      %GreySdk.Error{code: "UNAUTHORIZED", message: "Invalid credentials"}
  """
  @spec unauthorized(String.t()) :: t()
  def unauthorized(message \\ "Not authenticated") do
    %__MODULE__{code: @unauthorized, message: message}
  end

  @doc """
  Creates a validation error.

  ## Examples

      iex> GreySdk.Error.validation("Email is required")
      %GreySdk.Error{code: "VALIDATION_ERROR", message: "Email is required"}
  """
  @spec validation(String.t()) :: t()
  def validation(message) do
    %__MODULE__{code: @validation_error, message: message}
  end

  @doc """
  Creates a not found error.
  """
  @spec not_found(String.t()) :: t()
  def not_found(message \\ "Resource not found") do
    %__MODULE__{code: @not_found, message: message}
  end

  @doc """
  Creates a network error.
  """
  @spec network(String.t()) :: t()
  def network(message \\ "Network error") do
    %__MODULE__{code: @network_error, message: message}
  end

  @doc """
  Creates a timeout error.
  """
  @spec timeout(String.t()) :: t()
  def timeout(message \\ "Request timed out") do
    %__MODULE__{code: @timeout, message: message}
  end

  @doc """
  Creates an unknown error.
  """
  @spec unknown(String.t()) :: t()
  def unknown(message \\ "Unknown error") do
    %__MODULE__{code: @unknown, message: message}
  end

  @doc """
  Creates a GreyError from a gRPC error.

  Maps gRPC status codes to Grey error codes.
  """
  @spec from_grpc_error(GRPC.RPCError.t()) :: t()
  def from_grpc_error(%GRPC.RPCError{status: status, message: message}) do
    code = grpc_status_to_code(status)
    %__MODULE__{code: code, message: message || "gRPC error", details: %{grpc_status: status}}
  end

  def from_grpc_error(error) when is_exception(error) do
    %__MODULE__{code: @unknown, message: Exception.message(error)}
  end

  def from_grpc_error(error) do
    %__MODULE__{code: @unknown, message: inspect(error)}
  end

  @doc """
  Creates a GreyError from any term.
  """
  @spec from_any(any()) :: t()
  def from_any(%__MODULE__{} = error), do: error
  def from_any(%GRPC.RPCError{} = error), do: from_grpc_error(error)
  def from_any(error) when is_exception(error), do: %__MODULE__{code: @unknown, message: Exception.message(error)}
  def from_any(error) when is_binary(error), do: %__MODULE__{code: @unknown, message: error}
  def from_any(error), do: %__MODULE__{code: @unknown, message: inspect(error)}

  # Maps gRPC status codes to Grey error codes
  defp grpc_status_to_code(status) do
    case status do
      :unauthenticated -> @unauthorized
      :permission_denied -> @forbidden
      :not_found -> @not_found
      :invalid_argument -> @validation_error
      :failed_precondition -> @validation_error
      :unavailable -> @network_error
      :deadline_exceeded -> @timeout
      :internal -> @server_error
      :unknown -> @unknown
      _ -> @unknown
    end
  end
end
