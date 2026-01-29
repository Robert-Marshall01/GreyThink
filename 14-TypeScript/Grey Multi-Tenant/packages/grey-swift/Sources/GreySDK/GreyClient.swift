// =============================================================================
// Grey Multi-Tenant SDK - Swift
// GreyClient.swift
//
// Main client façade providing access to all domain clients.
// =============================================================================

import Foundation

/// Main client façade for the Grey Multi-Tenant SDK.
/// Provides access to all domain clients.
///
/// Usage:
/// ```swift
/// let options = GreyOptions(host: "api.grey.example.com")
/// let client = try GreyClient(options: options)
///
/// // Login
/// let auth = try await client.auth.login(email: "user@example.com", password: "password")
///
/// // Fetch user
/// let user = try await client.user.getUser()
///
/// // List projects
/// let projects = try await client.projects.listProjects()
///
/// // Cleanup
/// try await client.shutdown()
/// ```
public final class GreyClient: Sendable {
    
    /// Configuration options.
    public let options: GreyOptions
    
    /// Shared gRPC channel.
    private let channel: GreyChannel
    
    // MARK: - gRPC Services (internal)
    
    private let authGRPCService: AuthGRPCService
    private let userGRPCService: UserGRPCService
    private let projectsGRPCService: ProjectsGRPCService
    private let queryGRPCService: QueryGRPCService
    private let mutationGRPCService: MutationGRPCService
    
    // MARK: - Domain Clients (public)
    
    /// Authentication client.
    /// Provides login, logout, and token refresh operations.
    public let auth: AuthClient
    
    /// User client.
    /// Provides user data operations.
    public let user: UserClient
    
    /// Projects client.
    /// Provides project list and creation operations.
    public let projects: ProjectsClient
    
    /// Query client.
    /// Provides generic query operations.
    public let query: QueryClient
    
    /// Mutation client.
    /// Provides generic mutation operations.
    public let mutation: MutationClient
    
    // MARK: - Initialization
    
    /// Create a new GreyClient.
    /// - Parameter options: Configuration options
    /// - Throws: If channel creation fails
    public init(options: GreyOptions) throws {
        self.options = options
        
        // Create channel
        self.channel = try GreyChannel(options: options)
        
        // Create gRPC services
        self.authGRPCService = AuthGRPCService(channel: channel)
        self.userGRPCService = UserGRPCService(channel: channel)
        self.projectsGRPCService = ProjectsGRPCService(channel: channel)
        self.queryGRPCService = QueryGRPCService(channel: channel)
        self.mutationGRPCService = MutationGRPCService(channel: channel)
        
        // Create domain clients
        self.auth = AuthClient(grpcService: authGRPCService)
        self.user = UserClient(grpcService: userGRPCService, authClient: auth)
        self.projects = ProjectsClient(grpcService: projectsGRPCService, authClient: auth)
        self.query = QueryClient(grpcService: queryGRPCService, authClient: auth)
        self.mutation = MutationClient(grpcService: mutationGRPCService, authClient: auth)
    }
    
    // MARK: - Properties
    
    /// Whether the client is connected (channel is active).
    public var isConnected: Bool {
        channel.isActive
    }
    
    /// Whether the client is authenticated.
    public var isAuthenticated: Bool {
        auth.isAuthenticated
    }
    
    // MARK: - Lifecycle
    
    /// Shutdown the client and release resources.
    /// Should be called when the client is no longer needed.
    public func shutdown() async throws {
        auth.clearTokens()
        user.clearCache()
        try await channel.shutdown()
    }
    
    /// Restore authentication from existing tokens.
    /// Useful when restoring session from persisted storage.
    /// - Parameters:
    ///   - accessToken: The access token
    ///   - refreshToken: The refresh token
    public func restoreAuth(accessToken: String, refreshToken: String) {
        auth.setTokens(accessToken: accessToken, refreshToken: refreshToken)
    }
    
    // MARK: - Factory Methods
    
    /// Create a client for local development.
    /// - Parameter port: The local server port (default: 50051)
    /// - Returns: GreyClient configured for local development
    /// - Throws: If channel creation fails
    public static func forLocalDev(port: Int = 50051) throws -> GreyClient {
        try GreyClient(options: .forLocalDev(port: port))
    }
    
    /// Create a client with builder pattern.
    /// - Parameter configure: Configuration closure
    /// - Returns: GreyClient with configured options
    /// - Throws: If channel creation fails
    public static func create(_ configure: (GreyOptions.Builder) -> Void) throws -> GreyClient {
        let builder = GreyOptions.builder()
        configure(builder)
        return try GreyClient(options: builder.build())
    }
}
