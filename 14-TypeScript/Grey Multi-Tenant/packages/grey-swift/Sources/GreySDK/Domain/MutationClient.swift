// =============================================================================
// Grey Multi-Tenant SDK - Swift
// Domain/MutationClient.swift
//
// Domain client for generic mutation operations.
// Wraps the gRPC service with a clean domain API.
// =============================================================================

import Foundation

// MARK: - Domain Models

/// Mutation result data exposed to consumers.
public struct MutationData: Sendable {
    public let data: Any?
    public let metadata: [String: Any]?
    
    public init(data: Any?, metadata: [String: Any]?) {
        self.data = data
        self.metadata = metadata
    }
}

// MARK: - Client

/// Domain client for generic mutation operations.
/// All methods are async functions that throw GreyError on failure.
public final class MutationClient: Sendable {
    
    private let grpcService: MutationGRPCService
    private let authClient: AuthClient
    
    /// Valid HTTP methods for mutations.
    private static let validMethods: Set<String> = ["POST", "PUT", "PATCH", "DELETE"]
    
    // MARK: - Initialization
    
    internal init(grpcService: MutationGRPCService, authClient: AuthClient) {
        self.grpcService = grpcService
        self.authClient = authClient
    }
    
    // MARK: - Operations
    
    /// Execute a generic mutation.
    /// Requires authentication.
    /// - Parameters:
    ///   - endpoint: The mutation endpoint
    ///   - method: HTTP method (POST, PUT, PATCH, DELETE)
    ///   - body: Optional request body
    /// - Returns: MutationData on success
    /// - Throws: GreyError on failure
    public func mutate(
        endpoint: String,
        method: String = "POST",
        body: Any? = nil
    ) async throws -> MutationData {
        guard !endpoint.isEmpty else {
            throw GreyError.invalidArgument("Endpoint is required")
        }
        
        let normalizedMethod = method.uppercased()
        guard Self.validMethods.contains(normalizedMethod) else {
            throw GreyError.invalidArgument("Invalid method: \(method). Must be one of: \(Self.validMethods)")
        }
        
        guard let accessToken = authClient.accessToken else {
            throw GreyError.unauthenticated()
        }
        
        let response = try await grpcService.mutate(
            accessToken: accessToken,
            endpoint: endpoint,
            method: normalizedMethod,
            body: body
        )
        
        return MutationData(
            data: response.data,
            metadata: response.metadata
        )
    }
    
    /// Execute a POST mutation.
    /// Shorthand for mutate(endpoint:method:body:) with method = "POST".
    /// - Parameters:
    ///   - endpoint: The mutation endpoint
    ///   - body: Optional request body
    /// - Returns: MutationData on success
    /// - Throws: GreyError on failure
    public func post(endpoint: String, body: Any? = nil) async throws -> MutationData {
        try await mutate(endpoint: endpoint, method: "POST", body: body)
    }
    
    /// Execute a PUT mutation.
    /// Shorthand for mutate(endpoint:method:body:) with method = "PUT".
    /// - Parameters:
    ///   - endpoint: The mutation endpoint
    ///   - body: Optional request body
    /// - Returns: MutationData on success
    /// - Throws: GreyError on failure
    public func put(endpoint: String, body: Any? = nil) async throws -> MutationData {
        try await mutate(endpoint: endpoint, method: "PUT", body: body)
    }
    
    /// Execute a PATCH mutation.
    /// Shorthand for mutate(endpoint:method:body:) with method = "PATCH".
    /// - Parameters:
    ///   - endpoint: The mutation endpoint
    ///   - body: Optional request body
    /// - Returns: MutationData on success
    /// - Throws: GreyError on failure
    public func patch(endpoint: String, body: Any? = nil) async throws -> MutationData {
        try await mutate(endpoint: endpoint, method: "PATCH", body: body)
    }
    
    /// Execute a DELETE mutation.
    /// Shorthand for mutate(endpoint:method:body:) with method = "DELETE".
    /// - Parameter endpoint: The mutation endpoint
    /// - Returns: MutationData on success
    /// - Throws: GreyError on failure
    public func delete(endpoint: String) async throws -> MutationData {
        try await mutate(endpoint: endpoint, method: "DELETE", body: nil)
    }
}
