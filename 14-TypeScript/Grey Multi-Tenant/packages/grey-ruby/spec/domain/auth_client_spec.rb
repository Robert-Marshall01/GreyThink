# frozen_string_literal: true

require "spec_helper"

RSpec.describe GreySdk::Domain::AuthClient do
  let(:options) { GreySdk::Config::Options.local(8080) }
  let(:http_client) { GreySdk::Http::HttpClient.new(options) }
  let(:auth_client) { described_class.new(http_client) }

  describe "#login" do
    it "authenticates with email and password" do
      stub_request(:post, "http://localhost:8080/api/v1/auth/login")
        .with(body: { email: "user@example.com", password: "secret" }.to_json)
        .to_return(
          status: 200,
          body: { access_token: "tok123", refresh_token: "ref456" }.to_json,
          headers: { "Content-Type" => "application/json" }
        )

      result = auth_client.login(email: "user@example.com", password: "secret")

      expect(result.ok?).to be true
      expect(result.data["access_token"]).to eq("tok123")
    end

    it "returns validation error for missing email" do
      result = auth_client.login(email: nil, password: "secret")

      expect(result.err?).to be true
      expect(result.error.code).to eq(:validation_error)
    end

    it "returns validation error for missing password" do
      result = auth_client.login(email: "user@example.com", password: "")

      expect(result.err?).to be true
      expect(result.error.code).to eq(:validation_error)
    end

    it "handles unauthorized response" do
      stub_request(:post, "http://localhost:8080/api/v1/auth/login")
        .to_return(
          status: 401,
          body: { message: "Invalid credentials" }.to_json,
          headers: { "Content-Type" => "application/json" }
        )

      result = auth_client.login(email: "user@example.com", password: "wrong")

      expect(result.err?).to be true
      expect(result.error.code).to eq(:unauthorized)
    end
  end

  describe "#logout" do
    it "logs out the user" do
      stub_request(:post, "http://localhost:8080/api/v1/auth/logout")
        .to_return(
          status: 200,
          body: { success: true }.to_json,
          headers: { "Content-Type" => "application/json" }
        )

      result = auth_client.logout

      expect(result.ok?).to be true
    end
  end

  describe "#refresh" do
    it "refreshes the access token" do
      stub_request(:post, "http://localhost:8080/api/v1/auth/refresh")
        .with(body: { refresh_token: "ref456" }.to_json)
        .to_return(
          status: 200,
          body: { access_token: "newtok", refresh_token: "newref" }.to_json,
          headers: { "Content-Type" => "application/json" }
        )

      result = auth_client.refresh(refresh_token: "ref456")

      expect(result.ok?).to be true
      expect(result.data["access_token"]).to eq("newtok")
    end

    it "returns validation error for missing refresh token" do
      result = auth_client.refresh(refresh_token: nil)

      expect(result.err?).to be true
      expect(result.error.code).to eq(:validation_error)
    end
  end
end
