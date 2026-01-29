// =============================================================================
// Grey Multi-Tenant SDK - Swift
// Config/GreyOptions.swift
//
// Configuration options for the Grey SDK.
// =============================================================================

import Foundation

/// Configuration options for the Grey SDK.
public struct GreyOptions: Sendable {
    
    /// The gRPC server host.
    public let host: String
    
    /// The gRPC server port.
    public let port: Int
    
    /// Whether to use TLS.
    public let useTLS: Bool
    
    /// Request timeout in seconds.
    public let timeoutSeconds: TimeInterval
    
    /// Optional access token for authenticated requests.
    public var accessToken: String?
    
    // MARK: - Initializers
    
    public init(
        host: String,
        port: Int = 443,
        useTLS: Bool = true,
        timeoutSeconds: TimeInterval = 30,
        accessToken: String? = nil
    ) {
        self.host = host
        self.port = port
        self.useTLS = useTLS
        self.timeoutSeconds = timeoutSeconds
        self.accessToken = accessToken
    }
    
    // MARK: - Factory Methods
    
    /// Create options for local development.
    public static func forLocalDev(port: Int = 50051) -> GreyOptions {
        GreyOptions(
            host: "localhost",
            port: port,
            useTLS: false
        )
    }
    
    // MARK: - Mutating
    
    /// Create a copy with a new access token.
    public func withAccessToken(_ token: String?) -> GreyOptions {
        var copy = self
        copy.accessToken = token
        return copy
    }
}

// MARK: - Builder

extension GreyOptions {
    
    /// Builder for GreyOptions.
    public class Builder {
        private var host: String = "localhost"
        private var port: Int = 443
        private var useTLS: Bool = true
        private var timeoutSeconds: TimeInterval = 30
        private var accessToken: String?
        
        public init() {}
        
        @discardableResult
        public func host(_ host: String) -> Builder {
            self.host = host
            return self
        }
        
        @discardableResult
        public func port(_ port: Int) -> Builder {
            self.port = port
            return self
        }
        
        @discardableResult
        public func useTLS(_ useTLS: Bool) -> Builder {
            self.useTLS = useTLS
            return self
        }
        
        @discardableResult
        public func timeoutSeconds(_ timeout: TimeInterval) -> Builder {
            self.timeoutSeconds = timeout
            return self
        }
        
        @discardableResult
        public func accessToken(_ token: String?) -> Builder {
            self.accessToken = token
            return self
        }
        
        public func build() -> GreyOptions {
            GreyOptions(
                host: host,
                port: port,
                useTLS: useTLS,
                timeoutSeconds: timeoutSeconds,
                accessToken: accessToken
            )
        }
    }
    
    /// Create a new builder.
    public static func builder() -> Builder {
        Builder()
    }
}
