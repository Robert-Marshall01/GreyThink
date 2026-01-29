// =============================================================================
// Grey Multi-Tenant SDK - Kotlin
// domain/MutationClient.kt
//
// Domain client for generic mutation operations.
// Wraps the gRPC service with a clean domain API.
// =============================================================================

package com.grey.sdk.domain

import com.grey.sdk.error.GreyError
import com.grey.sdk.error.GreyResult
import com.grey.sdk.grpc.MutationGrpcService
import com.grey.sdk.grpc.MutationResponse

/**
 * Mutation result data exposed to consumers.
 */
data class MutationData(
    val data: Any?,
    val metadata: Map<String, Any>? = null
)

/**
 * Domain client for generic mutation operations.
 * All methods are suspend functions that return GreyResult.
 *
 * @property grpcService The underlying gRPC service
 * @property authClient The auth client for obtaining access tokens
 */
class MutationClient internal constructor(
    private val grpcService: MutationGrpcService,
    private val authClient: AuthClient
) {
    /**
     * Execute a generic mutation.
     * Requires authentication.
     *
     * @param endpoint The mutation endpoint
     * @param method HTTP method (POST, PUT, PATCH, DELETE)
     * @param body Optional request body
     * @return GreyResult containing MutationData or error
     */
    suspend fun mutate(
        endpoint: String,
        method: String = "POST",
        body: Any? = null
    ): GreyResult<MutationData> {
        if (endpoint.isBlank()) {
            return GreyResult.failure(GreyError.invalidArgument("Endpoint is required"))
        }

        val validMethods = setOf("POST", "PUT", "PATCH", "DELETE")
        val normalizedMethod = method.uppercase()
        if (normalizedMethod !in validMethods) {
            return GreyResult.failure(
                GreyError.invalidArgument("Invalid method: $method. Must be one of: $validMethods")
            )
        }

        val accessToken = authClient.accessToken
            ?: return GreyResult.failure(GreyError.unauthenticated())

        return grpcService.mutate(accessToken, endpoint, normalizedMethod, body)
            .map { response -> response.toMutationData() }
    }

    /**
     * Execute a POST mutation.
     * Shorthand for mutate(endpoint, "POST", body).
     *
     * @param endpoint The mutation endpoint
     * @param body Optional request body
     * @return GreyResult containing MutationData or error
     */
    suspend fun post(endpoint: String, body: Any? = null): GreyResult<MutationData> =
        mutate(endpoint, "POST", body)

    /**
     * Execute a PUT mutation.
     * Shorthand for mutate(endpoint, "PUT", body).
     *
     * @param endpoint The mutation endpoint
     * @param body Optional request body
     * @return GreyResult containing MutationData or error
     */
    suspend fun put(endpoint: String, body: Any? = null): GreyResult<MutationData> =
        mutate(endpoint, "PUT", body)

    /**
     * Execute a PATCH mutation.
     * Shorthand for mutate(endpoint, "PATCH", body).
     *
     * @param endpoint The mutation endpoint
     * @param body Optional request body
     * @return GreyResult containing MutationData or error
     */
    suspend fun patch(endpoint: String, body: Any? = null): GreyResult<MutationData> =
        mutate(endpoint, "PATCH", body)

    /**
     * Execute a DELETE mutation.
     * Shorthand for mutate(endpoint, "DELETE", null).
     *
     * @param endpoint The mutation endpoint
     * @return GreyResult containing MutationData or error
     */
    suspend fun delete(endpoint: String): GreyResult<MutationData> =
        mutate(endpoint, "DELETE", null)

    /**
     * Extension to convert gRPC response to domain model.
     */
    private fun MutationResponse.toMutationData(): MutationData = MutationData(
        data = data,
        metadata = metadata
    )
}
