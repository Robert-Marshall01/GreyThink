# frozen_string_literal: true

module GreySdk
  module Domain
    # Authentication client for login, logout, and token refresh
    class AuthClient
      # @param http_client [GreySdk::Http::HttpClient] The HTTP client
      def initialize(http_client)
        @http = http_client
      end

      # Authenticates a user with email and password
      # @param email [String] User email
      # @param password [String] User password
      # @return [GreySdk::Error::Result] Result containing tokens or error
      def login(email:, password:)
        return validation_error("Email is required") if email.nil? || email.empty?
        return validation_error("Password is required") if password.nil? || password.empty?

        @http.post("/api/v1/auth/login", {
          email: email,
          password: password
        }).then do |data|
          # Store the access token for subsequent requests
          @http.access_token = data["access_token"] if data["access_token"]
          Error::Result.ok(data)
        end
      end

      # Logs out the current user
      # @return [GreySdk::Error::Result] Result indicating success or error
      def logout
        result = @http.post("/api/v1/auth/logout")
        @http.access_token = nil if result.ok?
        result
      end

      # Refreshes the current access token
      # @param refresh_token [String] The refresh token
      # @return [GreySdk::Error::Result] Result containing new tokens or error
      def refresh(refresh_token:)
        return validation_error("Refresh token is required") if refresh_token.nil? || refresh_token.empty?

        @http.post("/api/v1/auth/refresh", {
          refresh_token: refresh_token
        }).then do |data|
          @http.access_token = data["access_token"] if data["access_token"]
          Error::Result.ok(data)
        end
      end

      # Sets the access token manually
      # @param token [String, nil] Access token
      def set_token(token)
        @http.access_token = token
      end

      private

      def validation_error(message)
        Error::Result.err(Error::GreyError.validation(message))
      end
    end
  end
end
