// =============================================================================
// Grey Multi-Tenant SDK - Swift
// Domain/QueryClient.swift
//
// Domain client for generic query operations.
// Wraps the gRPC service with a clean domain API.
// =============================================================================

import Foundation

// MARK: - Domain Models

/// Query result data exposed to consumers.
public struct QueryData: Sendable {
    public let data: Any?
    public let metadata: [String: Any]?
    
    public init(data: Any?, metadata: [String: Any]?) {
        self.data = data
        self.metadata = metadata
    }
}

// MARK: - Client

/// Domain client for generic query operations.
/// All methods are async functions that throw GreyError on failure.
public final class QueryClient: Sendable {
    
    private let grpcService: QueryGRPCService
    private let authClient: AuthClient
    
    // MARK: - Initialization
    
    internal init(grpcService: QueryGRPCService, authClient: AuthClient) {
        self.grpcService = grpcService
        self.authClient = authClient
    }
    
    // MARK: - Operations
    
    /// Execute a generic query.
    /// May or may not require authentication depending on the endpoint.
    /// - Parameters:
    ///   - endpoint: The query endpoint
    ///   - params: Optional query parameters
    ///   - requireAuth: Whether authentication is required (default: true)
    /// - Returns: QueryData on success
    /// - Throws: GreyError on failure
    public func query(
        endpoint: String,
        params: [String: String]? = nil,
        requireAuth: Bool = true
    ) async throws -> QueryData {
        guard !endpoint.isEmpty else {
            throw GreyError.invalidArgument("Endpoint is required")
        }
        
        let accessToken: String?
        if requireAuth {
            guard let token = authClient.accessToken else {
                throw GreyError.unauthenticated()
            }
            accessToken = token
        } else {
            accessToken = authClient.accessToken
        }
        
        let response = try await grpcService.query(
            accessToken: accessToken,
            endpoint: endpoint,
            params: params
        )
        
        return QueryData(
            data: response.data,
            metadata: response.metadata
        )
    }
    
    /// Execute an authenticated query.
    /// Shorthand for query(endpoint:params:requireAuth:) with requireAuth = true.
    /// - Parameters:
    ///   - endpoint: The query endpoint
    ///   - params: Optional query parameters
    /// - Returns: QueryData on success
    /// - Throws: GreyError on failure
    public func authenticatedQuery(
        endpoint: String,
        params: [String: String]? = nil
    ) async throws -> QueryData {
        try await query(endpoint: endpoint, params: params, requireAuth: true)
    }
    
    /// Execute a public query (no authentication required).
    /// Shorthand for query(endpoint:params:requireAuth:) with requireAuth = false.
    /// - Parameters:
    ///   - endpoint: The query endpoint
    ///   - params: Optional query parameters
    /// - Returns: QueryData on success
    /// - Throws: GreyError on failure
    public func publicQuery(
        endpoint: String,
        params: [String: String]? = nil
    ) async throws -> QueryData {
        try await query(endpoint: endpoint, params: params, requireAuth: false)
    }
}
