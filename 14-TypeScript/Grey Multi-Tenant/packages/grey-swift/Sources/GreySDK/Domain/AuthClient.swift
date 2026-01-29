// =============================================================================
// Grey Multi-Tenant SDK - Swift
// Domain/AuthClient.swift
//
// Domain client for authentication operations.
// Wraps the gRPC service with a clean domain API.
// =============================================================================

import Foundation

// MARK: - Domain Models

/// Authentication data exposed to consumers.
public struct AuthData: Sendable {
    public let accessToken: String
    public let refreshToken: String
    public let expiresIn: Int64
    public let userId: String
    
    public init(accessToken: String, refreshToken: String, expiresIn: Int64, userId: String) {
        self.accessToken = accessToken
        self.refreshToken = refreshToken
        self.expiresIn = expiresIn
        self.userId = userId
    }
}

// MARK: - Client

/// Domain client for authentication operations.
/// All methods are async functions that throw GreyError on failure.
public final class AuthClient: @unchecked Sendable {
    
    private let grpcService: AuthGRPCService
    
    /// Internal storage for access token.
    private var _accessToken: String?
    
    /// Internal storage for refresh token.
    private var _refreshToken: String?
    
    /// Lock for thread-safe token access.
    private let lock = NSLock()
    
    // MARK: - Initialization
    
    internal init(grpcService: AuthGRPCService) {
        self.grpcService = grpcService
    }
    
    // MARK: - Properties
    
    /// The current access token, if authenticated.
    public var accessToken: String? {
        lock.lock()
        defer { lock.unlock() }
        return _accessToken
    }
    
    /// The current refresh token, if authenticated.
    public var refreshToken: String? {
        lock.lock()
        defer { lock.unlock() }
        return _refreshToken
    }
    
    /// Whether the client is currently authenticated.
    public var isAuthenticated: Bool {
        accessToken != nil
    }
    
    // MARK: - Operations
    
    /// Login with email and password.
    /// - Parameters:
    ///   - email: User email
    ///   - password: User password
    /// - Returns: AuthData on success
    /// - Throws: GreyError on failure
    public func login(email: String, password: String) async throws -> AuthData {
        guard !email.isEmpty else {
            throw GreyError.invalidArgument("Email is required")
        }
        guard !password.isEmpty else {
            throw GreyError.invalidArgument("Password is required")
        }
        
        let response = try await grpcService.login(email: email, password: password)
        
        setTokens(accessToken: response.accessToken, refreshToken: response.refreshToken)
        
        return AuthData(
            accessToken: response.accessToken,
            refreshToken: response.refreshToken,
            expiresIn: response.expiresIn,
            userId: response.userId
        )
    }
    
    /// Logout the current user.
    /// Clears local authentication state.
    /// - Throws: GreyError on failure
    public func logout() async throws {
        guard let token = accessToken else {
            // Already logged out
            return
        }
        
        do {
            try await grpcService.logout(accessToken: token)
        } catch {
            // Clear tokens regardless of server response
        }
        
        clearTokens()
    }
    
    /// Refresh the access token using the stored refresh token.
    /// - Returns: New AuthData on success
    /// - Throws: GreyError on failure
    public func refresh() async throws -> AuthData {
        guard let token = refreshToken else {
            throw GreyError.unauthenticated("No refresh token available")
        }
        
        return try await refresh(refreshToken: token)
    }
    
    /// Refresh the access token with an explicit token.
    /// - Parameter refreshToken: The refresh token to use
    /// - Returns: New AuthData on success
    /// - Throws: GreyError on failure
    public func refresh(refreshToken: String) async throws -> AuthData {
        guard !refreshToken.isEmpty else {
            throw GreyError.invalidArgument("Refresh token is required")
        }
        
        let response = try await grpcService.refresh(refreshToken: refreshToken)
        
        setTokens(accessToken: response.accessToken, refreshToken: response.refreshToken)
        
        return AuthData(
            accessToken: response.accessToken,
            refreshToken: response.refreshToken,
            expiresIn: response.expiresIn,
            userId: response.userId
        )
    }
    
    // MARK: - Token Management
    
    /// Set tokens directly (e.g., from persisted storage).
    public func setTokens(accessToken: String?, refreshToken: String?) {
        lock.lock()
        defer { lock.unlock() }
        _accessToken = accessToken
        _refreshToken = refreshToken
    }
    
    /// Clear all tokens.
    public func clearTokens() {
        lock.lock()
        defer { lock.unlock() }
        _accessToken = nil
        _refreshToken = nil
    }
}
