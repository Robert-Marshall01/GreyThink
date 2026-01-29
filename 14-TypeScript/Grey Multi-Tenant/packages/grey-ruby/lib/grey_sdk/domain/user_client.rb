# frozen_string_literal: true

module GreySdk
  module Domain
    # User client for user-related operations
    class UserClient
      # @param http_client [GreySdk::Http::HttpClient] The HTTP client
      def initialize(http_client)
        @http = http_client
      end

      # Retrieves a user by ID
      # @param user_id [String] The user ID
      # @return [GreySdk::Error::Result] Result containing user data or error
      def get_user(user_id:)
        return validation_error("User ID is required") if user_id.nil? || user_id.to_s.empty?

        @http.get("/api/v1/users/#{user_id}")
      end

      # Retrieves the current authenticated user
      # @return [GreySdk::Error::Result] Result containing user data or error
      def get_current_user
        @http.get("/api/v1/users/me")
      end

      # Updates a user's profile
      # @param user_id [String] The user ID
      # @param data [Hash] User data to update
      # @return [GreySdk::Error::Result] Result containing updated user or error
      def update_user(user_id:, data:)
        return validation_error("User ID is required") if user_id.nil? || user_id.to_s.empty?
        return validation_error("Update data is required") if data.nil? || data.empty?

        @http.patch("/api/v1/users/#{user_id}", data)
      end

      private

      def validation_error(message)
        Error::Result.err(Error::GreyError.validation(message))
      end
    end
  end
end
