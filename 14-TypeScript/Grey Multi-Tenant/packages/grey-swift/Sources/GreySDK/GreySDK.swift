// =============================================================================
// Grey Multi-Tenant SDK - Swift
// GreySDK.swift (Entrypoint)
//
// Main entrypoint and public API exports for the Grey SDK.
// =============================================================================

import Foundation

// =============================================================================
// Public API Exports
// =============================================================================

// This file serves as the main module entrypoint.
// All public types are exported from their respective files.

// MARK: - Configuration
// - GreyOptions
// - GreyOptions.Builder

// MARK: - Error Handling
// - GreyError
// - GreyResult

// MARK: - Main Client
// - GreyClient

// MARK: - Domain Clients
// - AuthClient
// - UserClient
// - ProjectsClient
// - QueryClient
// - MutationClient

// MARK: - Domain Models
// - AuthData
// - User
// - Project
// - Pagination
// - ProjectsData
// - QueryData
// - MutationData

// =============================================================================
// SDK Info
// =============================================================================

/// Grey Multi-Tenant SDK information.
public enum GreySDK {
    
    /// SDK version.
    public static let version = "0.1.0"
    
    /// SDK name.
    public static let name = "GreySDK"
    
    /// Create a new Grey client.
    /// - Parameters:
    ///   - host: The gRPC server host
    ///   - port: The gRPC server port (default: 443)
    ///   - useTLS: Whether to use TLS (default: true)
    /// - Returns: GreyClient instance
    /// - Throws: If channel creation fails
    public static func client(
        host: String,
        port: Int = 443,
        useTLS: Bool = true
    ) throws -> GreyClient {
        try GreyClient(options: GreyOptions(
            host: host,
            port: port,
            useTLS: useTLS
        ))
    }
    
    /// Create a client for local development.
    /// - Parameter port: The local server port (default: 50051)
    /// - Returns: GreyClient configured for local development
    /// - Throws: If channel creation fails
    public static func localClient(port: Int = 50051) throws -> GreyClient {
        try GreyClient.forLocalDev(port: port)
    }
}

// =============================================================================
// Usage Examples (Documentation)
// =============================================================================

/*
 Quick Start:
 
 ```swift
 import GreySDK
 
 // Create client
 let grey = try GreySDK.client(host: "api.grey.example.com")
 
 // Login
 do {
     let auth = try await grey.auth.login(email: "user@example.com", password: "password")
     print("Logged in as: \(auth.userId)")
 } catch let error as GreyError {
     print("Login failed: [\(error.code)] \(error.message)")
 }
 
 // Fetch user
 let user = try await grey.user.getUser()
 print("User: \(user.name ?? "Unknown")")
 
 // List projects
 let projectsData = try await grey.projects.listProjects()
 for project in projectsData.projects {
     print("Project: \(project.name)")
 }
 
 // Create project
 let newProject = try await grey.projects.createProject(
     name: "My Project",
     description: "A new project"
 )
 
 // Generic query
 let queryResult = try await grey.query.query(
     endpoint: "/custom/endpoint",
     params: ["key": "value"]
 )
 
 // Generic mutation
 let mutationResult = try await grey.mutation.post(
     endpoint: "/custom/action",
     body: ["data": "value"]
 )
 
 // Cleanup
 try await grey.shutdown()
 ```
 
 Builder Pattern:
 
 ```swift
 let grey = try GreyClient.create { builder in
     builder.host("api.grey.example.com")
            .port(443)
            .useTLS(true)
            .timeoutSeconds(30)
 }
 ```
 
 Local Development:
 
 ```swift
 let grey = try GreySDK.localClient(port: 50051)
 ```
 
 Session Restoration:
 
 ```swift
 // Load tokens from Keychain
 if let accessToken = loadAccessToken(),
    let refreshToken = loadRefreshToken() {
     grey.restoreAuth(accessToken: accessToken, refreshToken: refreshToken)
 }
 ```
 */
