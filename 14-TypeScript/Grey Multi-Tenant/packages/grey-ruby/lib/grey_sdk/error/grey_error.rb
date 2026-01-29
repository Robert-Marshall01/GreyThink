# frozen_string_literal: true

require "json"

module GreySdk
  module Error
    # Represents a normalized error in the Grey SDK
    class GreyError
      attr_reader :code, :message, :details

      # @param code [Symbol] Error code
      # @param message [String] Error message
      # @param details [Hash, nil] Additional error details
      def initialize(code, message, details = nil)
        @code = ErrorCode.normalize(code)
        @message = message.to_s
        @details = details
      end

      # Creates an unauthorized error
      # @param message [String, nil] Custom message
      # @return [GreyError]
      def self.unauthorized(message = nil)
        new(ErrorCode::UNAUTHORIZED, message || "Authentication required")
      end

      # Creates a forbidden error
      # @param message [String, nil] Custom message
      # @return [GreyError]
      def self.forbidden(message = nil)
        new(ErrorCode::FORBIDDEN, message || "Permission denied")
      end

      # Creates a not found error
      # @param message [String, nil] Custom message
      # @return [GreyError]
      def self.not_found(message = nil)
        new(ErrorCode::NOT_FOUND, message || "Resource not found")
      end

      # Creates a validation error
      # @param message [String] Error message
      # @param details [Hash, nil] Validation details
      # @return [GreyError]
      def self.validation(message, details = nil)
        new(ErrorCode::VALIDATION_ERROR, message, details)
      end

      # Creates a network error
      # @param message [String, nil] Custom message
      # @return [GreyError]
      def self.network(message = nil)
        new(ErrorCode::NETWORK_ERROR, message || "Network error occurred")
      end

      # Creates a timeout error
      # @param message [String, nil] Custom message
      # @return [GreyError]
      def self.timeout(message = nil)
        new(ErrorCode::TIMEOUT, message || "Request timed out")
      end

      # Creates a server error
      # @param message [String, nil] Custom message
      # @return [GreyError]
      def self.server(message = nil)
        new(ErrorCode::SERVER_ERROR, message || "Server error occurred")
      end

      # Creates an unknown error
      # @param message [String, nil] Custom message
      # @return [GreyError]
      def self.unknown(message = nil)
        new(ErrorCode::UNKNOWN, message || "An unknown error occurred")
      end

      # Creates a GreyError from an HTTP response
      # @param status [Integer] HTTP status code
      # @param body [Hash, nil] Response body
      # @return [GreyError]
      def self.from_http_response(status, body = nil)
        code = ErrorCode.from_http_status(status)
        message = body&.dig("message") || body&.dig("error") || "HTTP error #{status}"
        details = body&.dig("details")

        new(code, message, details)
      end

      # Creates a GreyError from an exception
      # @param exception [Exception] The exception
      # @return [GreyError]
      def self.from_exception(exception)
        case exception
        when Errno::ECONNREFUSED, Errno::ECONNRESET, Errno::EHOSTUNREACH
          network(exception.message)
        when Net::OpenTimeout, Net::ReadTimeout
          timeout(exception.message)
        when JSON::ParserError
          validation("Invalid JSON response: #{exception.message}")
        else
          unknown(exception.message)
        end
      end

      # Converts the error to a hash
      # @return [Hash]
      def to_h
        {
          code: @code,
          message: @message,
          details: @details
        }
      end

      # Converts the error to JSON
      # @return [String]
      def to_json(*args)
        to_h.to_json(*args)
      end

      # String representation
      # @return [String]
      def to_s
        "GreyError[#{@code}]: #{@message}"
      end

      # Inspect representation
      # @return [String]
      def inspect
        "#<GreySdk::Error::GreyError code=#{@code.inspect} message=#{@message.inspect}>"
      end
    end
  end
end
