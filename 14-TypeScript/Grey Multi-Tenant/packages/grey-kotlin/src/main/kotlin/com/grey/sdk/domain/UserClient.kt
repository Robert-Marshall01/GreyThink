// =============================================================================
// Grey Multi-Tenant SDK - Kotlin
// domain/UserClient.kt
//
// Domain client for user operations.
// Wraps the gRPC service with a clean domain API.
// =============================================================================

package com.grey.sdk.domain

import com.grey.sdk.error.GreyError
import com.grey.sdk.error.GreyResult
import com.grey.sdk.grpc.UserGrpcService
import com.grey.sdk.grpc.UserResponse

/**
 * User data exposed to consumers.
 */
data class User(
    val id: String,
    val email: String,
    val name: String?,
    val avatarUrl: String?,
    val createdAt: Long,
    val updatedAt: Long
)

/**
 * Domain client for user operations.
 * All methods are suspend functions that return GreyResult.
 *
 * @property grpcService The underlying gRPC service
 * @property authClient The auth client for obtaining access tokens
 */
class UserClient internal constructor(
    private val grpcService: UserGrpcService,
    private val authClient: AuthClient
) {
    /**
     * Cached user data.
     */
    @Volatile
    private var _cachedUser: User? = null

    /**
     * The cached user, if available.
     */
    val cachedUser: User?
        get() = _cachedUser

    /**
     * Get the current user.
     * Requires authentication.
     *
     * @return GreyResult containing User or error
     */
    suspend fun getUser(): GreyResult<User> {
        val accessToken = authClient.accessToken
            ?: return GreyResult.failure(GreyError.unauthenticated())

        return grpcService.getUser(accessToken)
            .map { response ->
                val user = response.toUser()
                _cachedUser = user
                user
            }
    }

    /**
     * Get the current user, using cache if available.
     *
     * @param forceRefresh Force a fresh fetch from server
     * @return GreyResult containing User or error
     */
    suspend fun getUser(forceRefresh: Boolean): GreyResult<User> {
        if (!forceRefresh && _cachedUser != null) {
            return GreyResult.success(_cachedUser!!)
        }
        return getUser()
    }

    /**
     * Clear the cached user.
     */
    fun clearCache() {
        _cachedUser = null
    }

    /**
     * Extension to convert gRPC response to domain model.
     */
    private fun UserResponse.toUser(): User = User(
        id = id,
        email = email,
        name = name,
        avatarUrl = avatarUrl,
        createdAt = createdAt,
        updatedAt = updatedAt
    )
}
