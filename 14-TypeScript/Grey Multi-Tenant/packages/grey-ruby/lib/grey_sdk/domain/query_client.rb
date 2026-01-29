# frozen_string_literal: true

module GreySdk
  module Domain
    # Query client for executing read operations
    class QueryClient
      # @param http_client [GreySdk::Http::HttpClient] The HTTP client
      def initialize(http_client)
        @http = http_client
      end

      # Executes a query operation
      # @param query_name [String] The name of the query to execute
      # @param variables [Hash, nil] Query variables
      # @return [GreySdk::Error::Result] Result containing query result or error
      def query(query_name:, variables: nil)
        return validation_error("Query name is required") if query_name.nil? || query_name.empty?

        body = { query: query_name }
        body[:variables] = variables if variables

        @http.post("/api/v1/query", body)
      end

      # Executes a batch of queries
      # @param queries [Array<Hash>] Array of query objects with :query_name and :variables
      # @return [GreySdk::Error::Result] Result containing array of results or error
      def batch_query(queries:)
        return validation_error("Queries array is required") if queries.nil? || !queries.is_a?(Array)
        return validation_error("At least one query is required") if queries.empty?

        body = {
          queries: queries.map do |q|
            {
              query: q[:query_name],
              variables: q[:variables]
            }
          end
        }

        @http.post("/api/v1/query/batch", body)
      end

      # Executes a named query with pagination
      # @param query_name [String] The name of the query
      # @param variables [Hash, nil] Query variables
      # @param page [Integer] Page number
      # @param per_page [Integer] Items per page
      # @return [GreySdk::Error::Result] Result containing paginated result or error
      def paginated_query(query_name:, variables: nil, page: 1, per_page: 20)
        return validation_error("Query name is required") if query_name.nil? || query_name.empty?

        body = {
          query: query_name,
          variables: variables || {},
          pagination: {
            page: page,
            per_page: per_page
          }
        }

        @http.post("/api/v1/query", body)
      end

      private

      def validation_error(message)
        Error::Result.err(Error::GreyError.validation(message))
      end
    end
  end
end
