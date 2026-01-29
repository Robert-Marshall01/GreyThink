# frozen_string_literal: true

require "spec_helper"

RSpec.describe GreySdk::Error::ErrorCode do
  describe ".from_http_status" do
    it "returns :unauthorized for 401" do
      expect(described_class.from_http_status(401)).to eq(:unauthorized)
    end

    it "returns :forbidden for 403" do
      expect(described_class.from_http_status(403)).to eq(:forbidden)
    end

    it "returns :not_found for 404" do
      expect(described_class.from_http_status(404)).to eq(:not_found)
    end

    it "returns :validation_error for 400" do
      expect(described_class.from_http_status(400)).to eq(:validation_error)
    end

    it "returns :validation_error for 422" do
      expect(described_class.from_http_status(422)).to eq(:validation_error)
    end

    it "returns :timeout for 408" do
      expect(described_class.from_http_status(408)).to eq(:timeout)
    end

    it "returns :server_error for 500" do
      expect(described_class.from_http_status(500)).to eq(:server_error)
    end

    it "returns :unknown for unrecognized status" do
      expect(described_class.from_http_status(418)).to eq(:unknown)
    end
  end

  describe ".valid?" do
    it "returns true for valid error codes" do
      expect(described_class.valid?(:unauthorized)).to be true
      expect(described_class.valid?(:forbidden)).to be true
      expect(described_class.valid?(:not_found)).to be true
    end

    it "returns false for invalid error codes" do
      expect(described_class.valid?(:invalid_code)).to be false
    end
  end

  describe ".normalize" do
    it "returns the same code if valid" do
      expect(described_class.normalize(:forbidden)).to eq(:forbidden)
    end

    it "returns :unknown for invalid codes" do
      expect(described_class.normalize(:invalid)).to eq(:unknown)
    end

    it "converts strings to symbols" do
      expect(described_class.normalize("forbidden")).to eq(:forbidden)
    end
  end
end
