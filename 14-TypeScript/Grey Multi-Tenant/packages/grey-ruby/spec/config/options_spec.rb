# frozen_string_literal: true

require "spec_helper"

RSpec.describe GreySdk::Config::Options do
  describe ".new" do
    it "creates options with required parameters" do
      options = described_class.new(host: "api.example.com")

      expect(options.host).to eq("api.example.com")
      expect(options.port).to eq(443)
      expect(options.use_tls).to be true
      expect(options.timeout).to eq(30)
      expect(options.headers).to eq({})
    end

    it "creates options with all parameters" do
      options = described_class.new(
        host: "localhost",
        port: 8080,
        use_tls: false,
        timeout: 60,
        headers: { "X-Custom" => "value" }
      )

      expect(options.host).to eq("localhost")
      expect(options.port).to eq(8080)
      expect(options.use_tls).to be false
      expect(options.timeout).to eq(60)
      expect(options.headers).to eq({ "X-Custom" => "value" })
    end
  end

  describe ".local" do
    it "creates options for local development" do
      options = described_class.local(3000)

      expect(options.host).to eq("localhost")
      expect(options.port).to eq(3000)
      expect(options.use_tls).to be false
    end

    it "uses default port 8080" do
      options = described_class.local

      expect(options.port).to eq(8080)
    end
  end

  describe ".production" do
    it "creates options for production" do
      options = described_class.production("api.grey.com")

      expect(options.host).to eq("api.grey.com")
      expect(options.port).to eq(443)
      expect(options.use_tls).to be true
    end

    it "allows custom port" do
      options = described_class.production("api.grey.com", 8443)

      expect(options.port).to eq(8443)
    end
  end

  describe "#base_url" do
    it "returns https URL for TLS" do
      options = described_class.production("api.grey.com")

      expect(options.base_url).to eq("https://api.grey.com:443")
    end

    it "returns http URL without TLS" do
      options = described_class.local(8080)

      expect(options.base_url).to eq("http://localhost:8080")
    end
  end

  describe "#with_timeout" do
    it "returns new options with different timeout" do
      original = described_class.local
      modified = original.with_timeout(120)

      expect(original.timeout).to eq(30)
      expect(modified.timeout).to eq(120)
      expect(modified.host).to eq(original.host)
    end
  end

  describe "#with_headers" do
    it "returns new options with merged headers" do
      original = described_class.new(host: "api.grey.com", headers: { "X-A" => "a" })
      modified = original.with_headers({ "X-B" => "b" })

      expect(original.headers).to eq({ "X-A" => "a" })
      expect(modified.headers).to eq({ "X-A" => "a", "X-B" => "b" })
    end
  end

  describe "#to_h" do
    it "converts options to hash" do
      options = described_class.local

      expect(options.to_h).to include(
        host: "localhost",
        port: 8080,
        use_tls: false
      )
    end
  end
end
