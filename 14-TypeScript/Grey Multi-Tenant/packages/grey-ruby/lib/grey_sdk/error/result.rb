# frozen_string_literal: true

module GreySdk
  module Error
    # Represents the result of an operation that can succeed or fail
    class Result
      attr_reader :data, :error

      # @param ok [Boolean] Whether the result is successful
      # @param data [Object, nil] The success data
      # @param error [GreyError, nil] The error
      def initialize(ok:, data: nil, error: nil)
        @ok = ok
        @data = data
        @error = error
      end

      # Creates a successful result
      # @param data [Object] The success data
      # @return [Result]
      def self.ok(data)
        new(ok: true, data: data)
      end

      # Creates a failed result
      # @param error [GreyError] The error
      # @return [Result]
      def self.err(error)
        error = GreyError.new(ErrorCode::UNKNOWN, error.to_s) unless error.is_a?(GreyError)
        new(ok: false, error: error)
      end

      # Returns true if the result is successful
      # @return [Boolean]
      def ok?
        @ok
      end

      # Returns true if the result is an error
      # @return [Boolean]
      def err?
        !@ok
      end

      # Gets the data or raises an exception if failed
      # @return [Object]
      # @raise [RuntimeError] If the result is an error
      def unwrap!
        raise "Unwrap failed: #{@error&.message || 'Unknown error'}" unless ok?

        @data
      end

      # Gets the data or returns a default value if failed
      # @param default [Object] Default value
      # @return [Object]
      def unwrap_or(default)
        ok? ? @data : default
      end

      # Maps the data if successful
      # @yield [data] Block to transform the data
      # @return [Result]
      def map(&block)
        return self unless ok?

        begin
          Result.ok(block.call(@data))
        rescue StandardError => e
          Result.err(GreyError.from_exception(e))
        end
      end

      # Chains another operation if successful
      # @yield [data] Block that returns a Result
      # @return [Result]
      def then(&block)
        return self unless ok?

        begin
          block.call(@data)
        rescue StandardError => e
          Result.err(GreyError.from_exception(e))
        end
      end

      # Handles an error if failed
      # @yield [error] Block that returns a Result
      # @return [Result]
      def catch(&block)
        return self if ok?

        begin
          block.call(@error)
        rescue StandardError => e
          Result.err(GreyError.from_exception(e))
        end
      end

      # Converts the result to a hash
      # @return [Hash]
      def to_h
        if ok?
          { ok: true, data: @data }
        else
          { ok: false, error: @error&.to_h }
        end
      end

      # String representation
      # @return [String]
      def to_s
        if ok?
          "Result::Ok(#{@data.inspect})"
        else
          "Result::Err(#{@error})"
        end
      end

      # Inspect representation
      # @return [String]
      def inspect
        "#<GreySdk::Error::Result #{to_s}>"
      end
    end
  end
end
