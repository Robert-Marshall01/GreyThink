# frozen_string_literal: true

require "spec_helper"

RSpec.describe GreySdk::Error::GreyError do
  describe ".new" do
    it "creates an error with code, message, and details" do
      error = described_class.new(:unauthorized, "Access denied", { user_id: 123 })

      expect(error.code).to eq(:unauthorized)
      expect(error.message).to eq("Access denied")
      expect(error.details).to eq({ user_id: 123 })
    end

    it "normalizes invalid error codes to :unknown" do
      error = described_class.new(:invalid_code, "Some error")

      expect(error.code).to eq(:unknown)
    end
  end

  describe "factory methods" do
    it ".unauthorized creates an unauthorized error" do
      error = described_class.unauthorized

      expect(error.code).to eq(:unauthorized)
      expect(error.message).to eq("Authentication required")
    end

    it ".forbidden creates a forbidden error" do
      error = described_class.forbidden("Custom forbidden message")

      expect(error.code).to eq(:forbidden)
      expect(error.message).to eq("Custom forbidden message")
    end

    it ".not_found creates a not found error" do
      error = described_class.not_found

      expect(error.code).to eq(:not_found)
    end

    it ".validation creates a validation error with details" do
      error = described_class.validation("Invalid input", { field: "email" })

      expect(error.code).to eq(:validation_error)
      expect(error.details).to eq({ field: "email" })
    end

    it ".network creates a network error" do
      error = described_class.network

      expect(error.code).to eq(:network_error)
    end

    it ".timeout creates a timeout error" do
      error = described_class.timeout

      expect(error.code).to eq(:timeout)
    end

    it ".server creates a server error" do
      error = described_class.server

      expect(error.code).to eq(:server_error)
    end
  end

  describe ".from_http_response" do
    it "creates an error from HTTP status and body" do
      error = described_class.from_http_response(404, { "message" => "User not found" })

      expect(error.code).to eq(:not_found)
      expect(error.message).to eq("User not found")
    end

    it "handles nil body" do
      error = described_class.from_http_response(500, nil)

      expect(error.code).to eq(:server_error)
      expect(error.message).to eq("HTTP error 500")
    end
  end

  describe "#to_h" do
    it "converts error to hash" do
      error = described_class.new(:forbidden, "Not allowed", { reason: "quota" })
      hash = error.to_h

      expect(hash).to eq({
        code: :forbidden,
        message: "Not allowed",
        details: { reason: "quota" }
      })
    end
  end

  describe "#to_s" do
    it "returns a string representation" do
      error = described_class.new(:not_found, "Resource missing")

      expect(error.to_s).to eq("GreyError[not_found]: Resource missing")
    end
  end
end
