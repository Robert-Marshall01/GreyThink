// =============================================================================
// Grey Multi-Tenant SDK - Kotlin
// grpc/UserGrpcService.kt
//
// gRPC service stub for user operations.
// =============================================================================

package com.grey.sdk.grpc

import com.grey.sdk.config.GreyOptions
import com.grey.sdk.error.GreyError
import com.grey.sdk.error.GreyResult
import io.grpc.StatusException
import io.grpc.StatusRuntimeException
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext

/**
 * User data model.
 */
data class UserResponse(
    val id: String,
    val email: String,
    val name: String?,
    val avatarUrl: String?,
    val createdAt: Long,
    val updatedAt: Long
)

/**
 * gRPC service stub for user operations.
 * Wraps the Grey User gRPC service.
 */
class UserGrpcService(
    private val channel: GreyChannel,
    private val options: GreyOptions
) {
    /**
     * Get the current user.
     *
     * @param accessToken Current access token
     * @return GreyResult containing User or error
     */
    suspend fun getUser(accessToken: String): GreyResult<UserResponse> {
        return withContext(Dispatchers.IO) {
            try {
                val grpcChannel = channel.getChannel()
                
                // Note: In production, call actual gRPC stub with auth metadata
                // val metadata = channel.createAuthMetadata(accessToken)
                // val stub = UserServiceGrpcKt.UserServiceCoroutineStub(grpcChannel)
                //     .withInterceptors(MetadataUtils.newAttachHeadersInterceptor(metadata))
                // val response = stub.getUser(getUserRequest {})
                
                // Simulated response - replace with actual gRPC call
                GreyResult.success(
                    UserResponse(
                        id = "", // From gRPC response
                        email = "",
                        name = null,
                        avatarUrl = null,
                        createdAt = 0,
                        updatedAt = 0
                    )
                )
            } catch (e: StatusException) {
                GreyResult.failure(GreyError.fromGrpcStatus(e))
            } catch (e: StatusRuntimeException) {
                GreyResult.failure(GreyError.fromGrpcStatusRuntime(e))
            } catch (e: Throwable) {
                GreyResult.failure(GreyError.fromException(e))
            }
        }
    }
}
