// =============================================================================
// Grey Multi-Tenant SDK - Kotlin
// domain/AuthClient.kt
//
// Domain client for authentication operations.
// Wraps the gRPC service with a clean domain API.
// =============================================================================

package com.grey.sdk.domain

import com.grey.sdk.error.GreyError
import com.grey.sdk.error.GreyResult
import com.grey.sdk.grpc.AuthGrpcService
import com.grey.sdk.grpc.AuthResponse

/**
 * Authentication data exposed to consumers.
 */
data class AuthData(
    val accessToken: String,
    val refreshToken: String,
    val expiresIn: Long,
    val userId: String
)

/**
 * Domain client for authentication operations.
 * All methods are suspend functions that return GreyResult.
 *
 * @property grpcService The underlying gRPC service
 */
class AuthClient internal constructor(
    private val grpcService: AuthGrpcService
) {
    /**
     * Internal state for access token.
     * Used to maintain authentication state between calls.
     */
    @Volatile
    private var _accessToken: String? = null

    /**
     * Internal state for refresh token.
     */
    @Volatile
    private var _refreshToken: String? = null

    /**
     * The current access token, if authenticated.
     */
    val accessToken: String?
        get() = _accessToken

    /**
     * Whether the client is currently authenticated.
     */
    val isAuthenticated: Boolean
        get() = _accessToken != null

    /**
     * Login with email and password.
     *
     * @param email User email
     * @param password User password
     * @return GreyResult containing AuthData or error
     */
    suspend fun login(email: String, password: String): GreyResult<AuthData> {
        if (email.isBlank()) {
            return GreyResult.failure(GreyError.invalidArgument("Email is required"))
        }
        if (password.isBlank()) {
            return GreyResult.failure(GreyError.invalidArgument("Password is required"))
        }

        return grpcService.login(email, password)
            .map { response ->
                _accessToken = response.accessToken
                _refreshToken = response.refreshToken
                response.toAuthData()
            }
    }

    /**
     * Logout the current user.
     * Clears local authentication state.
     *
     * @return GreyResult indicating success or error
     */
    suspend fun logout(): GreyResult<Unit> {
        val token = _accessToken
        if (token == null) {
            // Already logged out, consider it a success
            return GreyResult.success(Unit)
        }

        val result = grpcService.logout(token)

        // Clear local state regardless of server response
        _accessToken = null
        _refreshToken = null

        return result
    }

    /**
     * Refresh the access token.
     *
     * @return GreyResult containing new AuthData or error
     */
    suspend fun refresh(): GreyResult<AuthData> {
        val token = _refreshToken
            ?: return GreyResult.failure(GreyError.unauthenticated("No refresh token available"))

        return grpcService.refresh(token)
            .map { response ->
                _accessToken = response.accessToken
                _refreshToken = response.refreshToken
                response.toAuthData()
            }
    }

    /**
     * Refresh with an explicit token.
     *
     * @param refreshToken The refresh token to use
     * @return GreyResult containing new AuthData or error
     */
    suspend fun refresh(refreshToken: String): GreyResult<AuthData> {
        if (refreshToken.isBlank()) {
            return GreyResult.failure(GreyError.invalidArgument("Refresh token is required"))
        }

        return grpcService.refresh(refreshToken)
            .map { response ->
                _accessToken = response.accessToken
                _refreshToken = response.refreshToken
                response.toAuthData()
            }
    }

    /**
     * Set tokens directly (e.g., from persisted storage).
     */
    fun setTokens(accessToken: String?, refreshToken: String?) {
        _accessToken = accessToken
        _refreshToken = refreshToken
    }

    /**
     * Clear all tokens.
     */
    fun clearTokens() {
        _accessToken = null
        _refreshToken = null
    }

    /**
     * Extension to convert gRPC response to domain model.
     */
    private fun AuthResponse.toAuthData(): AuthData = AuthData(
        accessToken = accessToken,
        refreshToken = refreshToken,
        expiresIn = expiresIn,
        userId = userId
    )
}
