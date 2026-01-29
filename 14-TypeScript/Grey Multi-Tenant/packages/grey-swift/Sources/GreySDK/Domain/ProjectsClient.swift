// =============================================================================
// Grey Multi-Tenant SDK - Swift
// Domain/ProjectsClient.swift
//
// Domain client for projects operations.
// Wraps the gRPC service with a clean domain API.
// =============================================================================

import Foundation

// MARK: - Domain Models

/// Project data exposed to consumers.
public struct Project: Sendable {
    public let id: String
    public let name: String
    public let description: String?
    public let ownerId: String
    public let createdAt: Int64
    public let updatedAt: Int64
    
    public init(
        id: String,
        name: String,
        description: String?,
        ownerId: String,
        createdAt: Int64,
        updatedAt: Int64
    ) {
        self.id = id
        self.name = name
        self.description = description
        self.ownerId = ownerId
        self.createdAt = createdAt
        self.updatedAt = updatedAt
    }
}

/// Pagination info exposed to consumers.
public struct Pagination: Sendable {
    public let page: Int
    public let pageSize: Int
    public let totalPages: Int
    public let totalItems: Int
    
    public init(page: Int, pageSize: Int, totalPages: Int, totalItems: Int) {
        self.page = page
        self.pageSize = pageSize
        self.totalPages = totalPages
        self.totalItems = totalItems
    }
}

/// Projects list data exposed to consumers.
public struct ProjectsData: Sendable {
    public let projects: [Project]
    public let pagination: Pagination
    
    public init(projects: [Project], pagination: Pagination) {
        self.projects = projects
        self.pagination = pagination
    }
}

// MARK: - Client

/// Domain client for projects operations.
/// All methods are async functions that throw GreyError on failure.
public final class ProjectsClient: Sendable {
    
    private let grpcService: ProjectsGRPCService
    private let authClient: AuthClient
    
    // MARK: - Initialization
    
    internal init(grpcService: ProjectsGRPCService, authClient: AuthClient) {
        self.grpcService = grpcService
        self.authClient = authClient
    }
    
    // MARK: - Operations
    
    /// List projects with pagination.
    /// Requires authentication.
    /// - Parameters:
    ///   - page: Page number (1-indexed, default: 1)
    ///   - pageSize: Items per page (default: 20)
    /// - Returns: ProjectsData on success
    /// - Throws: GreyError on failure
    public func listProjects(page: Int = 1, pageSize: Int = 20) async throws -> ProjectsData {
        guard let accessToken = authClient.accessToken else {
            throw GreyError.unauthenticated()
        }
        
        guard page >= 1 else {
            throw GreyError.invalidArgument("Page must be >= 1")
        }
        
        guard pageSize >= 1 && pageSize <= 100 else {
            throw GreyError.invalidArgument("Page size must be between 1 and 100")
        }
        
        let response = try await grpcService.listProjects(
            accessToken: accessToken,
            page: page,
            pageSize: pageSize
        )
        
        return ProjectsData(
            projects: response.projects.map { project in
                Project(
                    id: project.id,
                    name: project.name,
                    description: project.description,
                    ownerId: project.ownerId,
                    createdAt: project.createdAt,
                    updatedAt: project.updatedAt
                )
            },
            pagination: Pagination(
                page: response.pagination.page,
                pageSize: response.pagination.pageSize,
                totalPages: response.pagination.totalPages,
                totalItems: response.pagination.totalItems
            )
        )
    }
    
    /// Create a new project.
    /// Requires authentication.
    /// - Parameters:
    ///   - name: Project name
    ///   - description: Optional project description
    /// - Returns: Created Project on success
    /// - Throws: GreyError on failure
    public func createProject(name: String, description: String? = nil) async throws -> Project {
        guard let accessToken = authClient.accessToken else {
            throw GreyError.unauthenticated()
        }
        
        guard !name.isEmpty else {
            throw GreyError.invalidArgument("Project name is required")
        }
        
        let response = try await grpcService.createProject(
            accessToken: accessToken,
            name: name,
            description: description
        )
        
        return Project(
            id: response.id,
            name: response.name,
            description: response.description,
            ownerId: response.ownerId,
            createdAt: response.createdAt,
            updatedAt: response.updatedAt
        )
    }
}
