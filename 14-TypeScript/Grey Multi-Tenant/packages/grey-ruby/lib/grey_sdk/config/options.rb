# frozen_string_literal: true

module GreySdk
  module Config
    # Configuration options for the Grey SDK client
    class Options
      DEFAULT_TIMEOUT = 30
      DEFAULT_PORT = 443

      attr_reader :host, :port, :use_tls, :timeout, :headers

      # @param host [String] Server hostname
      # @param port [Integer] Server port
      # @param use_tls [Boolean] Whether to use HTTPS
      # @param timeout [Integer] Request timeout in seconds
      # @param headers [Hash] Additional headers
      def initialize(host:, port: DEFAULT_PORT, use_tls: true, timeout: DEFAULT_TIMEOUT, headers: {})
        @host = host
        @port = port
        @use_tls = use_tls
        @timeout = timeout
        @headers = headers
      end

      # Creates options for local development
      # @param port [Integer] Local server port
      # @return [Options]
      def self.local(port = 8080)
        new(
          host: "localhost",
          port: port,
          use_tls: false,
          timeout: DEFAULT_TIMEOUT
        )
      end

      # Creates options for production with TLS
      # @param host [String] Production server hostname
      # @param port [Integer] Server port
      # @return [Options]
      def self.production(host, port = 443)
        new(
          host: host,
          port: port,
          use_tls: true,
          timeout: DEFAULT_TIMEOUT
        )
      end

      # Returns the base URL for HTTP requests
      # @return [String]
      def base_url
        scheme = @use_tls ? "https" : "http"
        "#{scheme}://#{@host}:#{@port}"
      end

      # Returns new options with a different timeout
      # @param timeout [Integer] New timeout in seconds
      # @return [Options]
      def with_timeout(timeout)
        Options.new(
          host: @host,
          port: @port,
          use_tls: @use_tls,
          timeout: timeout,
          headers: @headers
        )
      end

      # Returns new options with additional headers merged
      # @param new_headers [Hash] Headers to merge
      # @return [Options]
      def with_headers(new_headers)
        Options.new(
          host: @host,
          port: @port,
          use_tls: @use_tls,
          timeout: @timeout,
          headers: @headers.merge(new_headers)
        )
      end

      # Converts options to a hash
      # @return [Hash]
      def to_h
        {
          host: @host,
          port: @port,
          use_tls: @use_tls,
          timeout: @timeout,
          headers: @headers
        }
      end
    end
  end
end
