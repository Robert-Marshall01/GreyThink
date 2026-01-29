# frozen_string_literal: true

module GreySdk
  module Domain
    # Projects client for project-related operations
    class ProjectsClient
      # @param http_client [GreySdk::Http::HttpClient] The HTTP client
      def initialize(http_client)
        @http = http_client
      end

      # Lists all projects for the current tenant
      # @param page [Integer, nil] Page number for pagination
      # @param per_page [Integer, nil] Number of items per page
      # @return [GreySdk::Error::Result] Result containing projects list or error
      def list_projects(page: nil, per_page: nil)
        params = {}
        params[:page] = page if page
        params[:per_page] = per_page if per_page

        @http.get("/api/v1/projects", params)
      end

      # Creates a new project
      # @param name [String] Project name
      # @param description [String, nil] Project description
      # @param metadata [Hash, nil] Additional project metadata
      # @return [GreySdk::Error::Result] Result containing created project or error
      def create_project(name:, description: nil, metadata: nil)
        return validation_error("Project name is required") if name.nil? || name.empty?

        body = { name: name }
        body[:description] = description if description
        body[:metadata] = metadata if metadata

        @http.post("/api/v1/projects", body)
      end

      # Retrieves a project by ID
      # @param project_id [String] The project ID
      # @return [GreySdk::Error::Result] Result containing project data or error
      def get_project(project_id:)
        return validation_error("Project ID is required") if project_id.nil? || project_id.to_s.empty?

        @http.get("/api/v1/projects/#{project_id}")
      end

      # Updates a project
      # @param project_id [String] The project ID
      # @param data [Hash] Project data to update
      # @return [GreySdk::Error::Result] Result containing updated project or error
      def update_project(project_id:, data:)
        return validation_error("Project ID is required") if project_id.nil? || project_id.to_s.empty?
        return validation_error("Update data is required") if data.nil? || data.empty?

        @http.patch("/api/v1/projects/#{project_id}", data)
      end

      # Deletes a project
      # @param project_id [String] The project ID
      # @return [GreySdk::Error::Result] Result indicating success or error
      def delete_project(project_id:)
        return validation_error("Project ID is required") if project_id.nil? || project_id.to_s.empty?

        @http.delete("/api/v1/projects/#{project_id}")
      end

      private

      def validation_error(message)
        Error::Result.err(Error::GreyError.validation(message))
      end
    end
  end
end
