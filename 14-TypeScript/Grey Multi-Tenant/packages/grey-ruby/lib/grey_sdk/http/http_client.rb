# frozen_string_literal: true

require "net/http"
require "uri"
require "json"

module GreySdk
  module Http
    # Shared HTTP client for making requests to the Grey API
    class HttpClient
      attr_reader :options, :access_token

      # @param options [GreySdk::Config::Options] Client options
      def initialize(options)
        @options = options
        @access_token = nil
      end

      # Sets the access token for authenticated requests
      # @param token [String, nil] Access token
      def access_token=(token)
        @access_token = token
      end

      # Makes a GET request
      # @param path [String] Request path
      # @param params [Hash] Query parameters
      # @return [GreySdk::Error::Result]
      def get(path, params = {})
        uri = build_uri(path, params)
        request = Net::HTTP::Get.new(uri)
        execute(uri, request)
      end

      # Makes a POST request
      # @param path [String] Request path
      # @param body [Hash] Request body
      # @return [GreySdk::Error::Result]
      def post(path, body = {})
        uri = build_uri(path)
        request = Net::HTTP::Post.new(uri)
        request.body = body.to_json
        execute(uri, request)
      end

      # Makes a PUT request
      # @param path [String] Request path
      # @param body [Hash] Request body
      # @return [GreySdk::Error::Result]
      def put(path, body = {})
        uri = build_uri(path)
        request = Net::HTTP::Put.new(uri)
        request.body = body.to_json
        execute(uri, request)
      end

      # Makes a PATCH request
      # @param path [String] Request path
      # @param body [Hash] Request body
      # @return [GreySdk::Error::Result]
      def patch(path, body = {})
        uri = build_uri(path)
        request = Net::HTTP::Patch.new(uri)
        request.body = body.to_json
        execute(uri, request)
      end

      # Makes a DELETE request
      # @param path [String] Request path
      # @param params [Hash] Query parameters
      # @return [GreySdk::Error::Result]
      def delete(path, params = {})
        uri = build_uri(path, params)
        request = Net::HTTP::Delete.new(uri)
        execute(uri, request)
      end

      private

      # Builds a full URI from path and optional query parameters
      # @param path [String] Request path
      # @param params [Hash] Query parameters
      # @return [URI]
      def build_uri(path, params = {})
        uri = URI.join(@options.base_url, path)
        uri.query = URI.encode_www_form(params) unless params.empty?
        uri
      end

      # Applies default headers to a request
      # @param request [Net::HTTPRequest] The request
      def apply_headers(request)
        request["Content-Type"] = "application/json"
        request["Accept"] = "application/json"

        @options.headers.each do |key, value|
          request[key.to_s] = value
        end

        request["Authorization"] = "Bearer #{@access_token}" if @access_token
      end

      # Creates an HTTP connection with proper settings
      # @param uri [URI] Request URI
      # @return [Net::HTTP]
      def create_connection(uri)
        http = Net::HTTP.new(uri.host, uri.port)
        http.use_ssl = @options.use_tls
        http.open_timeout = @options.timeout
        http.read_timeout = @options.timeout
        http.verify_mode = OpenSSL::SSL::VERIFY_PEER if @options.use_tls
        http
      end

      # Executes an HTTP request and returns a Result
      # @param uri [URI] Request URI
      # @param request [Net::HTTPRequest] The request
      # @return [GreySdk::Error::Result]
      def execute(uri, request)
        apply_headers(request)
        http = create_connection(uri)

        response = http.request(request)
        handle_response(response)
      rescue Errno::ECONNREFUSED, Errno::ECONNRESET, Errno::EHOSTUNREACH => e
        Error::Result.err(Error::GreyError.network(e.message))
      rescue Net::OpenTimeout, Net::ReadTimeout => e
        Error::Result.err(Error::GreyError.timeout(e.message))
      rescue JSON::ParserError => e
        Error::Result.err(Error::GreyError.validation("Invalid JSON: #{e.message}"))
      rescue StandardError => e
        Error::Result.err(Error::GreyError.unknown(e.message))
      end

      # Handles an HTTP response and returns a Result
      # @param response [Net::HTTPResponse] The response
      # @return [GreySdk::Error::Result]
      def handle_response(response)
        body = parse_body(response.body)
        status = response.code.to_i

        if status >= 200 && status < 300
          Error::Result.ok(body)
        else
          error = Error::GreyError.from_http_response(status, body)
          Error::Result.err(error)
        end
      end

      # Parses a response body as JSON
      # @param body [String, nil] Response body
      # @return [Hash, nil]
      def parse_body(body)
        return nil if body.nil? || body.empty?

        JSON.parse(body)
      rescue JSON::ParserError
        { "raw" => body }
      end
    end
  end
end
