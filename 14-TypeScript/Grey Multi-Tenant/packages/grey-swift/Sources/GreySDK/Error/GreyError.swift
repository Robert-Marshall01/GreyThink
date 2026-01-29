// =============================================================================
// Grey Multi-Tenant SDK - Swift
// Error/GreyError.swift
//
// Normalized error shape for all Grey operations.
// =============================================================================

import Foundation
import GRPC

/// Normalized error shape for all Grey operations.
/// All domain actions normalize errors into this type.
public struct GreyError: Error, LocalizedError, Sendable {
    
    /// Error code (e.g., "UNAUTHENTICATED", "NOT_FOUND", "INTERNAL")
    public let code: String
    
    /// Human-readable error message
    public let message: String
    
    /// Optional additional error details
    public let details: [String: Any]?
    
    /// Localized description for Error protocol
    public var errorDescription: String? {
        "[\(code)] \(message)"
    }
    
    // MARK: - Initializers
    
    public init(code: String, message: String, details: [String: Any]? = nil) {
        self.code = code
        self.message = message
        self.details = details
    }
    
    // MARK: - Factory Methods
    
    /// Create a GreyError from a gRPC status.
    public static func fromGRPCStatus(_ status: GRPCStatus) -> GreyError {
        GreyError(
            code: status.code.description,
            message: status.message ?? "Unknown error",
            details: ["grpcCode": status.code.rawValue]
        )
    }
    
    /// Create a GreyError from any Error.
    public static func fromError(_ error: Error) -> GreyError {
        if let greyError = error as? GreyError {
            return greyError
        }
        
        if let grpcStatus = error as? GRPCStatus {
            return fromGRPCStatus(grpcStatus)
        }
        
        return GreyError(
            code: "UNKNOWN",
            message: error.localizedDescription,
            details: ["errorType": String(describing: type(of: error))]
        )
    }
    
    /// Create an UNAUTHENTICATED error.
    public static func unauthenticated(_ message: String = "Not authenticated") -> GreyError {
        GreyError(code: "UNAUTHENTICATED", message: message)
    }
    
    /// Create a NOT_FOUND error.
    public static func notFound(_ message: String = "Resource not found") -> GreyError {
        GreyError(code: "NOT_FOUND", message: message)
    }
    
    /// Create an INVALID_ARGUMENT error.
    public static func invalidArgument(_ message: String = "Invalid argument") -> GreyError {
        GreyError(code: "INVALID_ARGUMENT", message: message)
    }
    
    /// Create an INTERNAL error.
    public static func `internal`(_ message: String = "Internal error") -> GreyError {
        GreyError(code: "INTERNAL", message: message)
    }
    
    /// Create a CANCELLED error.
    public static func cancelled(_ message: String = "Operation cancelled") -> GreyError {
        GreyError(code: "CANCELLED", message: message)
    }
}

// MARK: - GreyResult

/// Result wrapper for Grey operations.
/// Provides convenient methods for handling success and failure cases.
public enum GreyResult<T: Sendable>: Sendable {
    case success(T)
    case failure(GreyError)
    
    /// Whether the result is successful.
    public var isSuccess: Bool {
        if case .success = self { return true }
        return false
    }
    
    /// Whether the result is a failure.
    public var isFailure: Bool {
        if case .failure = self { return true }
        return false
    }
    
    /// Get the data if successful, nil otherwise.
    public var data: T? {
        if case .success(let value) = self { return value }
        return nil
    }
    
    /// Get the error if failed, nil otherwise.
    public var error: GreyError? {
        if case .failure(let error) = self { return error }
        return nil
    }
    
    /// Map the success value.
    public func map<U: Sendable>(_ transform: (T) -> U) -> GreyResult<U> {
        switch self {
        case .success(let value):
            return .success(transform(value))
        case .failure(let error):
            return .failure(error)
        }
    }
    
    /// Get the value or throw the error.
    public func get() throws -> T {
        switch self {
        case .success(let value):
            return value
        case .failure(let error):
            throw error
        }
    }
    
    /// Get the value or return a default.
    public func getOrDefault(_ defaultValue: T) -> T {
        switch self {
        case .success(let value):
            return value
        case .failure:
            return defaultValue
        }
    }
}

// MARK: - Result Extension

extension Result where Failure == Error {
    /// Convert to GreyResult.
    public func toGreyResult<T: Sendable>() -> GreyResult<T> where Success == T {
        switch self {
        case .success(let value):
            return .success(value)
        case .failure(let error):
            return .failure(GreyError.fromError(error))
        }
    }
}
