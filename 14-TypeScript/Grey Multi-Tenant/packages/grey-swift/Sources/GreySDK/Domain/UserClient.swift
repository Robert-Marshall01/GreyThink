// =============================================================================
// Grey Multi-Tenant SDK - Swift
// Domain/UserClient.swift
//
// Domain client for user operations.
// Wraps the gRPC service with a clean domain API.
// =============================================================================

import Foundation

// MARK: - Domain Models

/// User data exposed to consumers.
public struct User: Sendable {
    public let id: String
    public let email: String
    public let name: String?
    public let avatarUrl: String?
    public let createdAt: Int64
    public let updatedAt: Int64
    
    public init(
        id: String,
        email: String,
        name: String?,
        avatarUrl: String?,
        createdAt: Int64,
        updatedAt: Int64
    ) {
        self.id = id
        self.email = email
        self.name = name
        self.avatarUrl = avatarUrl
        self.createdAt = createdAt
        self.updatedAt = updatedAt
    }
}

// MARK: - Client

/// Domain client for user operations.
/// All methods are async functions that throw GreyError on failure.
public final class UserClient: @unchecked Sendable {
    
    private let grpcService: UserGRPCService
    private let authClient: AuthClient
    
    /// Cached user data.
    private var _cachedUser: User?
    
    /// Lock for thread-safe cache access.
    private let lock = NSLock()
    
    // MARK: - Initialization
    
    internal init(grpcService: UserGRPCService, authClient: AuthClient) {
        self.grpcService = grpcService
        self.authClient = authClient
    }
    
    // MARK: - Properties
    
    /// The cached user, if available.
    public var cachedUser: User? {
        lock.lock()
        defer { lock.unlock() }
        return _cachedUser
    }
    
    // MARK: - Operations
    
    /// Get the current user.
    /// Requires authentication.
    /// - Returns: User on success
    /// - Throws: GreyError on failure
    public func getUser() async throws -> User {
        guard let accessToken = authClient.accessToken else {
            throw GreyError.unauthenticated()
        }
        
        let response = try await grpcService.getUser(accessToken: accessToken)
        
        let user = User(
            id: response.id,
            email: response.email,
            name: response.name,
            avatarUrl: response.avatarUrl,
            createdAt: response.createdAt,
            updatedAt: response.updatedAt
        )
        
        lock.lock()
        _cachedUser = user
        lock.unlock()
        
        return user
    }
    
    /// Get the current user, using cache if available.
    /// - Parameter forceRefresh: Force a fresh fetch from server
    /// - Returns: User on success
    /// - Throws: GreyError on failure
    public func getUser(forceRefresh: Bool) async throws -> User {
        if !forceRefresh, let cached = cachedUser {
            return cached
        }
        return try await getUser()
    }
    
    /// Clear the cached user.
    public func clearCache() {
        lock.lock()
        defer { lock.unlock() }
        _cachedUser = nil
    }
}
