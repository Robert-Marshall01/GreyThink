# frozen_string_literal: true

module GreySdk
  module Domain
    # Mutation client for executing write operations
    class MutationClient
      # @param http_client [GreySdk::Http::HttpClient] The HTTP client
      def initialize(http_client)
        @http = http_client
      end

      # Executes a mutation operation
      # @param mutation_name [String] The name of the mutation to execute
      # @param input [Hash] Mutation input data
      # @return [GreySdk::Error::Result] Result containing mutation result or error
      def mutate(mutation_name:, input:)
        return validation_error("Mutation name is required") if mutation_name.nil? || mutation_name.empty?
        return validation_error("Input data is required") if input.nil?

        body = {
          mutation: mutation_name,
          input: input
        }

        @http.post("/api/v1/mutate", body)
      end

      # Executes a batch of mutations
      # @param mutations [Array<Hash>] Array of mutation objects with :mutation_name and :input
      # @return [GreySdk::Error::Result] Result containing array of results or error
      def batch_mutate(mutations:)
        return validation_error("Mutations array is required") if mutations.nil? || !mutations.is_a?(Array)
        return validation_error("At least one mutation is required") if mutations.empty?

        body = {
          mutations: mutations.map do |m|
            {
              mutation: m[:mutation_name],
              input: m[:input]
            }
          end
        }

        @http.post("/api/v1/mutate/batch", body)
      end

      # Executes a mutation within a transaction
      # @param transaction_id [String] Transaction ID
      # @param mutation_name [String] Mutation name
      # @param input [Hash] Mutation input
      # @return [GreySdk::Error::Result] Result containing mutation result or error
      def transactional_mutate(transaction_id:, mutation_name:, input:)
        return validation_error("Transaction ID is required") if transaction_id.nil? || transaction_id.empty?
        return validation_error("Mutation name is required") if mutation_name.nil? || mutation_name.empty?
        return validation_error("Input data is required") if input.nil?

        body = {
          transaction_id: transaction_id,
          mutation: mutation_name,
          input: input
        }

        @http.post("/api/v1/mutate/transactional", body)
      end

      private

      def validation_error(message)
        Error::Result.err(Error::GreyError.validation(message))
      end
    end
  end
end
