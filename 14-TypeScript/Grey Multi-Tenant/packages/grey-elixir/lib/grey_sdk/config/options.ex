defmodule GreySdk.Config.Options do
  @moduledoc """
  Configuration options for the Grey SDK client.

  ## Options

  - `:host` - The host address of the Grey API server (required)
  - `:port` - The port to connect to (default: 443)
  - `:use_tls` - Whether to use TLS for the connection (default: true)
  - `:timeout_ms` - Request timeout in milliseconds (default: 30000)
  - `:metadata` - Additional metadata to include in requests

  ## Examples

      # Production configuration
      options = GreySdk.Config.Options.new(
        host: "api.grey.example.com",
        port: 443,
        use_tls: true
      )

      # Local development
      options = GreySdk.Config.Options.local(port: 50051)
  """

  @type t :: %__MODULE__{
          host: String.t(),
          port: non_neg_integer(),
          use_tls: boolean(),
          timeout_ms: non_neg_integer(),
          metadata: map()
        }

  @enforce_keys [:host]
  defstruct [
    :host,
    port: 443,
    use_tls: true,
    timeout_ms: 30_000,
    metadata: %{}
  ]

  @doc """
  Creates new options with the given configuration.

  ## Examples

      iex> GreySdk.Config.Options.new(host: "api.grey.example.com")
      %GreySdk.Config.Options{host: "api.grey.example.com", port: 443, use_tls: true}
  """
  @spec new(keyword()) :: t()
  def new(opts) when is_list(opts) do
    host = Keyword.fetch!(opts, :host)

    if host == "" or is_nil(host) do
      raise ArgumentError, "host is required and cannot be empty"
    end

    port = Keyword.get(opts, :port, 443)
    use_tls = Keyword.get(opts, :use_tls, true)
    timeout_ms = Keyword.get(opts, :timeout_ms, 30_000)
    metadata = Keyword.get(opts, :metadata, %{})

    validate_port!(port)
    validate_timeout!(timeout_ms)

    %__MODULE__{
      host: host,
      port: port,
      use_tls: use_tls,
      timeout_ms: timeout_ms,
      metadata: metadata
    }
  end

  @doc """
  Creates options for local development.

  ## Examples

      iex> GreySdk.Config.Options.local()
      %GreySdk.Config.Options{host: "localhost", port: 50051, use_tls: false}
  """
  @spec local(keyword()) :: t()
  def local(opts \\ []) do
    new(
      Keyword.merge(
        [host: "localhost", port: 50051, use_tls: false],
        opts
      )
    )
  end

  @doc """
  Creates options for production.

  ## Examples

      iex> GreySdk.Config.Options.production("api.grey.example.com")
      %GreySdk.Config.Options{host: "api.grey.example.com", port: 443, use_tls: true}
  """
  @spec production(String.t(), keyword()) :: t()
  def production(host, opts \\ []) do
    new(
      Keyword.merge(
        [host: host, port: 443, use_tls: true],
        opts
      )
    )
  end

  @doc """
  Returns the timeout as a duration in milliseconds.
  """
  @spec timeout(t()) :: non_neg_integer()
  def timeout(%__MODULE__{timeout_ms: timeout_ms}), do: timeout_ms

  @doc """
  Returns the full endpoint address.
  """
  @spec endpoint(t()) :: String.t()
  def endpoint(%__MODULE__{host: host, port: port}) do
    "#{host}:#{port}"
  end

  defp validate_port!(port) when is_integer(port) and port > 0 and port <= 65535, do: :ok
  defp validate_port!(port), do: raise(ArgumentError, "port must be between 1 and 65535, got: #{inspect(port)}")

  defp validate_timeout!(timeout) when is_integer(timeout) and timeout > 0, do: :ok
  defp validate_timeout!(timeout), do: raise(ArgumentError, "timeout_ms must be positive, got: #{inspect(timeout)}")
end
