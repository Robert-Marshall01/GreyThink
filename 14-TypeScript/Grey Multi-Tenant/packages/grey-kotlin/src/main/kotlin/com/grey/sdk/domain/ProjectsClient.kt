// =============================================================================
// Grey Multi-Tenant SDK - Kotlin
// domain/ProjectsClient.kt
//
// Domain client for projects operations.
// Wraps the gRPC service with a clean domain API.
// =============================================================================

package com.grey.sdk.domain

import com.grey.sdk.error.GreyError
import com.grey.sdk.error.GreyResult
import com.grey.sdk.grpc.PaginationResponse
import com.grey.sdk.grpc.ProjectResponse
import com.grey.sdk.grpc.ProjectsGrpcService
import com.grey.sdk.grpc.ProjectsListResponse

/**
 * Project data exposed to consumers.
 */
data class Project(
    val id: String,
    val name: String,
    val description: String?,
    val ownerId: String,
    val createdAt: Long,
    val updatedAt: Long
)

/**
 * Pagination info exposed to consumers.
 */
data class Pagination(
    val page: Int,
    val pageSize: Int,
    val totalPages: Int,
    val totalItems: Int
)

/**
 * Projects list data exposed to consumers.
 */
data class ProjectsData(
    val projects: List<Project>,
    val pagination: Pagination
)

/**
 * Domain client for projects operations.
 * All methods are suspend functions that return GreyResult.
 *
 * @property grpcService The underlying gRPC service
 * @property authClient The auth client for obtaining access tokens
 */
class ProjectsClient internal constructor(
    private val grpcService: ProjectsGrpcService,
    private val authClient: AuthClient
) {
    /**
     * List projects with pagination.
     * Requires authentication.
     *
     * @param page Page number (1-indexed, default: 1)
     * @param pageSize Items per page (default: 20)
     * @return GreyResult containing ProjectsData or error
     */
    suspend fun listProjects(page: Int = 1, pageSize: Int = 20): GreyResult<ProjectsData> {
        val accessToken = authClient.accessToken
            ?: return GreyResult.failure(GreyError.unauthenticated())

        if (page < 1) {
            return GreyResult.failure(GreyError.invalidArgument("Page must be >= 1"))
        }
        if (pageSize < 1 || pageSize > 100) {
            return GreyResult.failure(GreyError.invalidArgument("Page size must be between 1 and 100"))
        }

        return grpcService.listProjects(accessToken, page, pageSize)
            .map { response -> response.toProjectsData() }
    }

    /**
     * Create a new project.
     * Requires authentication.
     *
     * @param name Project name
     * @param description Optional project description
     * @return GreyResult containing created Project or error
     */
    suspend fun createProject(name: String, description: String? = null): GreyResult<Project> {
        val accessToken = authClient.accessToken
            ?: return GreyResult.failure(GreyError.unauthenticated())

        if (name.isBlank()) {
            return GreyResult.failure(GreyError.invalidArgument("Project name is required"))
        }

        return grpcService.createProject(accessToken, name, description)
            .map { response -> response.toProject() }
    }

    /**
     * Extension to convert gRPC response to domain model.
     */
    private fun ProjectsListResponse.toProjectsData(): ProjectsData = ProjectsData(
        projects = projects.map { it.toProject() },
        pagination = pagination.toPagination()
    )

    /**
     * Extension to convert gRPC response to domain model.
     */
    private fun ProjectResponse.toProject(): Project = Project(
        id = id,
        name = name,
        description = description,
        ownerId = ownerId,
        createdAt = createdAt,
        updatedAt = updatedAt
    )

    /**
     * Extension to convert gRPC response to domain model.
     */
    private fun PaginationResponse.toPagination(): Pagination = Pagination(
        page = page,
        pageSize = pageSize,
        totalPages = totalPages,
        totalItems = totalItems
    )
}
