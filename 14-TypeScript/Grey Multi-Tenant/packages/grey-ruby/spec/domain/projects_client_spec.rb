# frozen_string_literal: true

require "spec_helper"

RSpec.describe GreySdk::Domain::ProjectsClient do
  let(:options) { GreySdk::Config::Options.local(8080) }
  let(:http_client) { GreySdk::Http::HttpClient.new(options) }
  let(:projects_client) { described_class.new(http_client) }

  describe "#list_projects" do
    it "lists all projects" do
      stub_request(:get, "http://localhost:8080/api/v1/projects")
        .to_return(
          status: 200,
          body: { projects: [{ id: "1", name: "Project 1" }] }.to_json,
          headers: { "Content-Type" => "application/json" }
        )

      result = projects_client.list_projects

      expect(result.ok?).to be true
      expect(result.data["projects"]).to have(1).item
    end

    it "supports pagination" do
      stub_request(:get, "http://localhost:8080/api/v1/projects?page=2&per_page=10")
        .to_return(
          status: 200,
          body: { projects: [] }.to_json,
          headers: { "Content-Type" => "application/json" }
        )

      result = projects_client.list_projects(page: 2, per_page: 10)

      expect(result.ok?).to be true
    end
  end

  describe "#create_project" do
    it "creates a new project" do
      stub_request(:post, "http://localhost:8080/api/v1/projects")
        .with(body: { name: "New Project" }.to_json)
        .to_return(
          status: 201,
          body: { id: "123", name: "New Project" }.to_json,
          headers: { "Content-Type" => "application/json" }
        )

      result = projects_client.create_project(name: "New Project")

      expect(result.ok?).to be true
      expect(result.data["id"]).to eq("123")
    end

    it "includes description and metadata when provided" do
      stub_request(:post, "http://localhost:8080/api/v1/projects")
        .with(body: { name: "Project", description: "A description", metadata: { key: "value" } }.to_json)
        .to_return(
          status: 201,
          body: { id: "123" }.to_json,
          headers: { "Content-Type" => "application/json" }
        )

      result = projects_client.create_project(
        name: "Project",
        description: "A description",
        metadata: { key: "value" }
      )

      expect(result.ok?).to be true
    end

    it "returns validation error for missing name" do
      result = projects_client.create_project(name: "")

      expect(result.err?).to be true
      expect(result.error.code).to eq(:validation_error)
    end
  end

  describe "#get_project" do
    it "retrieves a project by ID" do
      stub_request(:get, "http://localhost:8080/api/v1/projects/abc123")
        .to_return(
          status: 200,
          body: { id: "abc123", name: "My Project" }.to_json,
          headers: { "Content-Type" => "application/json" }
        )

      result = projects_client.get_project(project_id: "abc123")

      expect(result.ok?).to be true
      expect(result.data["name"]).to eq("My Project")
    end

    it "returns not found for missing project" do
      stub_request(:get, "http://localhost:8080/api/v1/projects/missing")
        .to_return(
          status: 404,
          body: { message: "Project not found" }.to_json,
          headers: { "Content-Type" => "application/json" }
        )

      result = projects_client.get_project(project_id: "missing")

      expect(result.err?).to be true
      expect(result.error.code).to eq(:not_found)
    end
  end

  describe "#update_project" do
    it "updates a project" do
      stub_request(:patch, "http://localhost:8080/api/v1/projects/abc123")
        .with(body: { name: "Updated Name" }.to_json)
        .to_return(
          status: 200,
          body: { id: "abc123", name: "Updated Name" }.to_json,
          headers: { "Content-Type" => "application/json" }
        )

      result = projects_client.update_project(project_id: "abc123", data: { name: "Updated Name" })

      expect(result.ok?).to be true
    end
  end

  describe "#delete_project" do
    it "deletes a project" do
      stub_request(:delete, "http://localhost:8080/api/v1/projects/abc123")
        .to_return(
          status: 204,
          body: nil,
          headers: {}
        )

      result = projects_client.delete_project(project_id: "abc123")

      expect(result.ok?).to be true
    end
  end
end
