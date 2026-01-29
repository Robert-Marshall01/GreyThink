// =============================================================================
// Grey Multi-Tenant SDK - Swift
// GRPC/UserGRPCService.swift
//
// gRPC service stub for user operations.
// =============================================================================

import Foundation
import GRPC

// MARK: - Response Models

/// User response data.
public struct UserResponse: Sendable {
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

// MARK: - Service

/// gRPC service stub for user operations.
/// Wraps the Grey User gRPC service.
public final class UserGRPCService: Sendable {
    
    private let channel: GreyChannel
    
    // MARK: - Initialization
    
    public init(channel: GreyChannel) {
        self.channel = channel
    }
    
    // MARK: - Operations
    
    /// Get the current user.
    /// - Parameter accessToken: Current access token
    /// - Returns: UserResponse on success
    /// - Throws: GreyError on failure
    public func getUser(accessToken: String) async throws -> UserResponse {
        do {
            let grpcChannel = channel.getChannel()
            let options = channel.callOptions(accessToken: accessToken)
            
            // Note: In production, call actual gRPC stub
            // let client = User_UserServiceAsyncClient(channel: grpcChannel)
            // let response = try await client.getUser(User_GetUserRequest(), callOptions: options)
            
            return UserResponse(
                id: "",
                email: "",
                name: nil,
                avatarUrl: nil,
                createdAt: 0,
                updatedAt: 0
            )
        } catch let error as GRPCStatus {
            throw GreyError.fromGRPCStatus(error)
        } catch {
            throw GreyError.fromError(error)
        }
    }
}
