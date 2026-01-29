// =============================================================================
// Grey Multi-Tenant SDK - Kotlin
// Grey.kt (Entrypoint)
//
// Main entrypoint and convenience exports for the Grey SDK.
// =============================================================================

@file:JvmName("Grey")

package com.grey.sdk

// =============================================================================
// Re-exports for convenience
// =============================================================================

// Main client
// export: GreyClient

// Configuration
// export: GreyOptions, GreyOptions.Builder

// Error handling
// export: GreyError, GreyResult, GreyException

// Domain models
// export: AuthData, User, Project, Pagination, ProjectsData, QueryData, MutationData

// Domain clients
// export: AuthClient, UserClient, ProjectsClient, QueryClient, MutationClient

/**
 * Grey Multi-Tenant SDK for Kotlin.
 *
 * Quick Start:
 * ```kotlin
 * import com.grey.sdk.GreyClient
 * import com.grey.sdk.config.GreyOptions
 *
 * // Create client
 * val grey = GreyClient(GreyOptions(
 *     host = "api.grey.example.com",
 *     port = 443,
 *     useTls = true
 * ))
 *
 * // Login
 * val loginResult = grey.auth.login("user@example.com", "password")
 * loginResult.onSuccess { auth ->
 *     println("Logged in as: ${auth.userId}")
 * }.onFailure { error ->
 *     println("Login failed: ${error.message}")
 * }
 *
 * // Fetch user
 * val userResult = grey.user.getUser()
 * userResult.onSuccess { user ->
 *     println("User: ${user.name}")
 * }
 *
 * // List projects
 * val projectsResult = grey.projects.listProjects()
 * projectsResult.onSuccess { data ->
 *     data.projects.forEach { project ->
 *         println("Project: ${project.name}")
 *     }
 * }
 *
 * // Create project
 * val createResult = grey.projects.createProject(
 *     name = "My Project",
 *     description = "A new project"
 * )
 *
 * // Generic query
 * val queryResult = grey.query.query("/custom/endpoint", mapOf("key" to "value"))
 *
 * // Generic mutation
 * val mutationResult = grey.mutation.post("/custom/action", mapOf("data" to "value"))
 *
 * // Cleanup
 * grey.shutdown()
 * ```
 *
 * Builder Pattern:
 * ```kotlin
 * val grey = GreyClient.create {
 *     host("api.grey.example.com")
 *     port(443)
 *     useTls(true)
 *     timeout(30, TimeUnit.SECONDS)
 * }
 * ```
 *
 * Local Development:
 * ```kotlin
 * val grey = GreyClient.forLocalDev(port = 50051)
 * ```
 */
object Grey {
    /**
     * SDK version.
     */
    const val VERSION = "0.1.0"

    /**
     * Create a new Grey client.
     *
     * @param host The gRPC server host
     * @param port The gRPC server port (default: 443)
     * @param useTls Whether to use TLS (default: true)
     * @return GreyClient instance
     */
    @JvmStatic
    @JvmOverloads
    fun client(
        host: String,
        port: Int = 443,
        useTls: Boolean = true
    ): GreyClient {
        return GreyClient(
            com.grey.sdk.config.GreyOptions(
                host = host,
                port = port,
                useTls = useTls
            )
        )
    }

    /**
     * Create a client for local development.
     *
     * @param port The local server port (default: 50051)
     * @return GreyClient configured for local development
     */
    @JvmStatic
    @JvmOverloads
    fun localClient(port: Int = 50051): GreyClient {
        return GreyClient.forLocalDev(port)
    }
}
