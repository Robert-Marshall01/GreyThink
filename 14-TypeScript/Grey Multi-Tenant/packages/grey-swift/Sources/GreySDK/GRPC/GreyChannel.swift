// =============================================================================
// Grey Multi-Tenant SDK - Swift
// GRPC/GreyChannel.swift
//
// Shared gRPC channel management.
// =============================================================================

import Foundation
import GRPC
import NIO
import NIOSSL
import NIOTransportServices

/// Manages the gRPC channel for Grey SDK.
public final class GreyChannel: Sendable {
    
    /// The event loop group for async operations.
    private let group: EventLoopGroup
    
    /// The gRPC channel.
    private let channel: GRPCChannel
    
    /// Configuration options.
    private let options: GreyOptions
    
    // MARK: - Initialization
    
    /// Create a new GreyChannel.
    /// - Parameter options: Configuration options
    /// - Throws: If channel creation fails
    public init(options: GreyOptions) throws {
        self.options = options
        
        // Create event loop group
        #if canImport(Network)
        self.group = NIOTSEventLoopGroup()
        #else
        self.group = MultiThreadedEventLoopGroup(numberOfThreads: 1)
        #endif
        
        // Build channel
        let builder: ClientConnection.Builder
        
        if options.useTLS {
            builder = ClientConnection.usingTLSBackedByNIOSSL(on: group)
        } else {
            builder = ClientConnection.insecure(group: group)
        }
        
        self.channel = builder
            .withConnectionTimeout(minimum: .seconds(Int64(options.timeoutSeconds)))
            .connect(host: options.host, port: options.port)
    }
    
    // MARK: - Channel Access
    
    /// Get the underlying gRPC channel.
    public func getChannel() -> GRPCChannel {
        channel
    }
    
    /// Create call options with optional authorization.
    /// - Parameter accessToken: Optional access token
    /// - Returns: CallOptions with timeout and optional auth header
    public func callOptions(accessToken: String? = nil) -> CallOptions {
        var options = CallOptions(
            timeLimit: .timeout(.seconds(Int64(self.options.timeoutSeconds)))
        )
        
        if let token = accessToken ?? self.options.accessToken {
            options.customMetadata.add(name: "authorization", value: "Bearer \(token)")
        }
        
        return options
    }
    
    // MARK: - Lifecycle
    
    /// Shutdown the channel and release resources.
    public func shutdown() async throws {
        try await channel.close().get()
        try await group.shutdownGracefully()
    }
    
    /// Check if the channel is active.
    public var isActive: Bool {
        // Channel connectivity check
        true // Simplified - in production, check actual state
    }
}
