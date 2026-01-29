# frozen_string_literal: true

require "spec_helper"

RSpec.describe GreySdk::Client do
  let(:options) { GreySdk::Config::Options.local(8080) }
  let(:client) { described_class.new(options) }

  describe ".new" do
    it "creates a client with domain clients" do
      expect(client.auth).to be_a(GreySdk::Domain::AuthClient)
      expect(client.user).to be_a(GreySdk::Domain::UserClient)
      expect(client.projects).to be_a(GreySdk::Domain::ProjectsClient)
      expect(client.query).to be_a(GreySdk::Domain::QueryClient)
      expect(client.mutation).to be_a(GreySdk::Domain::MutationClient)
    end
  end

  describe ".local" do
    it "creates a client for local development" do
      local_client = described_class.local(3000)

      expect(local_client.options.host).to eq("localhost")
      expect(local_client.options.port).to eq(3000)
    end
  end

  describe ".production" do
    it "creates a client for production" do
      prod_client = described_class.production("api.grey.com")

      expect(prod_client.options.host).to eq("api.grey.com")
      expect(prod_client.options.use_tls).to be true
    end
  end

  describe "#authenticated?" do
    it "returns false when no token" do
      expect(client.authenticated?).to be false
    end

    it "returns true after login" do
      stub_request(:post, "http://localhost:8080/api/v1/auth/login")
        .to_return(
          status: 200,
          body: { access_token: "test-token", refresh_token: "refresh" }.to_json,
          headers: { "Content-Type" => "application/json" }
        )

      client.auth.login(email: "user@example.com", password: "password")

      expect(client.authenticated?).to be true
    end
  end

  describe "#clear_auth" do
    it "clears authentication" do
      stub_request(:post, "http://localhost:8080/api/v1/auth/login")
        .to_return(
          status: 200,
          body: { access_token: "test-token" }.to_json,
          headers: { "Content-Type" => "application/json" }
        )

      client.auth.login(email: "user@example.com", password: "password")
      expect(client.authenticated?).to be true

      client.clear_auth
      expect(client.authenticated?).to be false
    end
  end
end
