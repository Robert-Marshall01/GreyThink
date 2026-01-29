# frozen_string_literal: true

module GreySdk
  # Main client facade for the Grey Multi-Tenant SDK
  #
  # @example Basic usage
  #   client = GreySdk::Client.new(GreySdk::Config::Options.local(8080))
  #   result = client.auth.login(email: "user@example.com", password: "secret")
  #   if result.ok?
  #     puts "Logged in: #{result.data}"
  #   else
  #     puts "Error: #{result.error.message}"
  #   end
  #
  # @example Production usage
  #   client = GreySdk::Client.new(GreySdk::Config::Options.production("api.grey.com"))
  #   client.auth.set_token("existing-access-token")
  #   projects = client.projects.list_projects
  #
  class Client
    attr_reader :auth, :user, :projects, :query, :mutation

    # Creates a new Grey SDK client
    # @param options [GreySdk::Config::Options] Client configuration options
    def initialize(options)
      @options = options
      @http_client = Http::HttpClient.new(options)

      # Initialize domain clients
      @auth = Domain::AuthClient.new(@http_client)
      @user = Domain::UserClient.new(@http_client)
      @projects = Domain::ProjectsClient.new(@http_client)
      @query = Domain::QueryClient.new(@http_client)
      @mutation = Domain::MutationClient.new(@http_client)
    end

    # Creates a client configured for local development
    # @param port [Integer] Local server port (default: 8080)
    # @return [GreySdk::Client]
    def self.local(port = 8080)
      new(Config::Options.local(port))
    end

    # Creates a client configured for production
    # @param host [String] Production server hostname
    # @param port [Integer] Server port (default: 443)
    # @return [GreySdk::Client]
    def self.production(host, port = 443)
      new(Config::Options.production(host, port))
    end

    # Returns the current options
    # @return [GreySdk::Config::Options]
    def options
      @options
    end

    # Returns true if authenticated (has access token)
    # @return [Boolean]
    def authenticated?
      !@http_client.access_token.nil?
    end

    # Clears the current authentication
    def clear_auth
      @http_client.access_token = nil
    end
  end
end
