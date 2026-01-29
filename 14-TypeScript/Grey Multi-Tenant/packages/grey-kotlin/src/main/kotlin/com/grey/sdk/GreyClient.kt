// =============================================================================
// Grey Multi-Tenant SDK - Kotlin
// GreyClient.kt
//
// Main client façade providing access to all domain clients.
// =============================================================================

package com.grey.sdk

import com.grey.sdk.config.GreyOptions
import com.grey.sdk.domain.AuthClient
import com.grey.sdk.domain.MutationClient
import com.grey.sdk.domain.ProjectsClient
import com.grey.sdk.domain.QueryClient
import com.grey.sdk.domain.UserClient
import com.grey.sdk.grpc.AuthGrpcService
import com.grey.sdk.grpc.GreyChannel
import com.grey.sdk.grpc.MutationGrpcService
import com.grey.sdk.grpc.ProjectsGrpcService
import com.grey.sdk.grpc.QueryGrpcService
import com.grey.sdk.grpc.UserGrpcService

/**
 * Main client façade for the Grey Multi-Tenant SDK.
 * Provides access to all domain clients.
 *
 * Usage:
 * ```kotlin
 * val client = GreyClient(GreyOptions(host = "api.grey.example.com"))
 *
 * // Login
 * client.auth.login("user@example.com", "password")
 *
 * // Fetch user
 * val user = client.user.getUser()
 *
 * // List projects
 * val projects = client.projects.listProjects()
 *
 * // Cleanup
 * client.shutdown()
 * ```
 *
 * @property options Configuration options
 */
class GreyClient(private val options: GreyOptions) {

    /**
     * Shared gRPC channel.
     */
    private val channel = GreyChannel(options)

    // ==========================================================================
    // gRPC Services (internal)
    // ==========================================================================

    private val authGrpcService = AuthGrpcService(channel, options)
    private val userGrpcService = UserGrpcService(channel, options)
    private val projectsGrpcService = ProjectsGrpcService(channel, options)
    private val queryGrpcService = QueryGrpcService(channel, options)
    private val mutationGrpcService = MutationGrpcService(channel, options)

    // ==========================================================================
    // Domain Clients (public)
    // ==========================================================================

    /**
     * Authentication client.
     * Provides login, logout, and token refresh operations.
     */
    val auth: AuthClient = AuthClient(authGrpcService)

    /**
     * User client.
     * Provides user data operations.
     */
    val user: UserClient = UserClient(userGrpcService, auth)

    /**
     * Projects client.
     * Provides project list and creation operations.
     */
    val projects: ProjectsClient = ProjectsClient(projectsGrpcService, auth)

    /**
     * Query client.
     * Provides generic query operations.
     */
    val query: QueryClient = QueryClient(queryGrpcService, auth)

    /**
     * Mutation client.
     * Provides generic mutation operations.
     */
    val mutation: MutationClient = MutationClient(mutationGrpcService, auth)

    // ==========================================================================
    // Client Management
    // ==========================================================================

    /**
     * Whether the client is connected (channel is active).
     */
    val isConnected: Boolean
        get() = channel.isActive

    /**
     * Whether the client is authenticated.
     */
    val isAuthenticated: Boolean
        get() = auth.isAuthenticated

    /**
     * Shutdown the client and release resources.
     * Should be called when the client is no longer needed.
     */
    fun shutdown() {
        auth.clearTokens()
        user.clearCache()
        channel.shutdown()
    }

    /**
     * Restore authentication from existing tokens.
     * Useful when restoring session from persisted storage.
     *
     * @param accessToken The access token
     * @param refreshToken The refresh token
     */
    fun restoreAuth(accessToken: String, refreshToken: String) {
        auth.setTokens(accessToken, refreshToken)
    }

    companion object {
        /**
         * Create a client for local development.
         *
         * @param port The local server port (default: 50051)
         * @return GreyClient configured for local development
         */
        fun forLocalDev(port: Int = 50051): GreyClient {
            return GreyClient(GreyOptions.forLocalDev(port))
        }

        /**
         * Create a client with builder pattern.
         *
         * @param block Configuration block
         * @return GreyClient with configured options
         */
        fun create(block: GreyOptions.Builder.() -> Unit): GreyClient {
            val builder = GreyOptions.builder()
            builder.block()
            return GreyClient(builder.build())
        }
    }
}
