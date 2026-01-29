# frozen_string_literal: true

module GreySdk
  module Error
    # Error codes used throughout the SDK
    module ErrorCode
      UNAUTHORIZED = :unauthorized
      FORBIDDEN = :forbidden
      NOT_FOUND = :not_found
      VALIDATION_ERROR = :validation_error
      NETWORK_ERROR = :network_error
      TIMEOUT = :timeout
      SERVER_ERROR = :server_error
      UNKNOWN = :unknown

      ALL = [
        UNAUTHORIZED,
        FORBIDDEN,
        NOT_FOUND,
        VALIDATION_ERROR,
        NETWORK_ERROR,
        TIMEOUT,
        SERVER_ERROR,
        UNKNOWN
      ].freeze

      class << self
        # Converts an HTTP status code to an error code
        # @param status [Integer] HTTP status code
        # @return [Symbol] Error code
        def from_http_status(status)
          case status
          when 401 then UNAUTHORIZED
          when 403 then FORBIDDEN
          when 404 then NOT_FOUND
          when 400, 422 then VALIDATION_ERROR
          when 408, 504 then TIMEOUT
          when 500..599 then SERVER_ERROR
          else UNKNOWN
          end
        end

        # Checks if a code is valid
        # @param code [Symbol] Error code to check
        # @return [Boolean]
        def valid?(code)
          ALL.include?(code)
        end

        # Normalizes a code to a valid error code
        # @param code [Symbol, String] Code to normalize
        # @return [Symbol] Valid error code
        def normalize(code)
          sym = code.to_sym
          valid?(sym) ? sym : UNKNOWN
        end
      end
    end
  end
end
