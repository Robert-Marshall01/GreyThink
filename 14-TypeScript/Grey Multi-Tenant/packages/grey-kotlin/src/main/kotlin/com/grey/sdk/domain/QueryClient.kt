// =============================================================================
// Grey Multi-Tenant SDK - Kotlin
// domain/QueryClient.kt
//
// Domain client for generic query operations.
// Wraps the gRPC service with a clean domain API.
// =============================================================================

package com.grey.sdk.domain

import com.grey.sdk.error.GreyError
import com.grey.sdk.error.GreyResult
import com.grey.sdk.grpc.QueryGrpcService
import com.grey.sdk.grpc.QueryResponse

/**
 * Query result data exposed to consumers.
 */
data class QueryData(
    val data: Any?,
    val metadata: Map<String, Any>? = null
)

/**
 * Domain client for generic query operations.
 * All methods are suspend functions that return GreyResult.
 *
 * @property grpcService The underlying gRPC service
 * @property authClient The auth client for obtaining access tokens
 */
class QueryClient internal constructor(
    private val grpcService: QueryGrpcService,
    private val authClient: AuthClient
) {
    /**
     * Execute a generic query.
     * May or may not require authentication depending on the endpoint.
     *
     * @param endpoint The query endpoint
     * @param params Optional query parameters
     * @param requireAuth Whether authentication is required (default: true)
     * @return GreyResult containing QueryData or error
     */
    suspend fun query(
        endpoint: String,
        params: Map<String, String>? = null,
        requireAuth: Boolean = true
    ): GreyResult<QueryData> {
        if (endpoint.isBlank()) {
            return GreyResult.failure(GreyError.invalidArgument("Endpoint is required"))
        }

        val accessToken = if (requireAuth) {
            authClient.accessToken
                ?: return GreyResult.failure(GreyError.unauthenticated())
        } else {
            authClient.accessToken
        }

        return grpcService.query(accessToken, endpoint, params)
            .map { response -> response.toQueryData() }
    }

    /**
     * Execute an authenticated query.
     * Shorthand for query(endpoint, params, requireAuth = true).
     *
     * @param endpoint The query endpoint
     * @param params Optional query parameters
     * @return GreyResult containing QueryData or error
     */
    suspend fun authenticatedQuery(
        endpoint: String,
        params: Map<String, String>? = null
    ): GreyResult<QueryData> = query(endpoint, params, requireAuth = true)

    /**
     * Execute a public query (no authentication required).
     * Shorthand for query(endpoint, params, requireAuth = false).
     *
     * @param endpoint The query endpoint
     * @param params Optional query parameters
     * @return GreyResult containing QueryData or error
     */
    suspend fun publicQuery(
        endpoint: String,
        params: Map<String, String>? = null
    ): GreyResult<QueryData> = query(endpoint, params, requireAuth = false)

    /**
     * Extension to convert gRPC response to domain model.
     */
    private fun QueryResponse.toQueryData(): QueryData = QueryData(
        data = data,
        metadata = metadata
    )
}
