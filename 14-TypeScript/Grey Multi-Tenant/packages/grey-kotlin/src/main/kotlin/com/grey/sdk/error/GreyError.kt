// =============================================================================
// Grey Multi-Tenant SDK - Kotlin
// error/GreyError.kt
//
// Normalized error shape for all Grey operations.
// =============================================================================

package com.grey.sdk.error

import io.grpc.Status
import io.grpc.StatusException
import io.grpc.StatusRuntimeException

/**
 * Normalized error shape for all Grey operations.
 * All domain actions normalize errors into this type.
 *
 * @property code Error code (e.g., "UNAUTHENTICATED", "NOT_FOUND", "INTERNAL")
 * @property message Human-readable error message
 * @property details Optional additional error details
 */
data class GreyError(
    val code: String,
    val message: String,
    val details: Map<String, Any>? = null
) {
    companion object {
        /**
         * Create a GreyError from a gRPC StatusException.
         */
        fun fromGrpcStatus(e: StatusException): GreyError {
            return GreyError(
                code = e.status.code.name,
                message = e.status.description ?: e.message ?: "Unknown error",
                details = mapOf("grpcStatus" to e.status.code.value())
            )
        }

        /**
         * Create a GreyError from a gRPC StatusRuntimeException.
         */
        fun fromGrpcStatusRuntime(e: StatusRuntimeException): GreyError {
            return GreyError(
                code = e.status.code.name,
                message = e.status.description ?: e.message ?: "Unknown error",
                details = mapOf("grpcStatus" to e.status.code.value())
            )
        }

        /**
         * Create a GreyError from a generic exception.
         */
        fun fromException(e: Throwable): GreyError {
            return when (e) {
                is StatusException -> fromGrpcStatus(e)
                is StatusRuntimeException -> fromGrpcStatusRuntime(e)
                else -> GreyError(
                    code = "UNKNOWN",
                    message = e.message ?: "Unknown error",
                    details = mapOf("exceptionType" to (e::class.simpleName ?: "Unknown"))
                )
            }
        }

        /**
         * Create a GreyError from a gRPC Status.
         */
        fun fromStatus(status: Status, message: String? = null): GreyError {
            return GreyError(
                code = status.code.name,
                message = message ?: status.description ?: "Unknown error",
                details = mapOf("grpcStatus" to status.code.value())
            )
        }

        /**
         * Create an UNAUTHENTICATED error.
         */
        fun unauthenticated(message: String = "Not authenticated"): GreyError {
            return GreyError(
                code = "UNAUTHENTICATED",
                message = message
            )
        }

        /**
         * Create a NOT_FOUND error.
         */
        fun notFound(message: String = "Resource not found"): GreyError {
            return GreyError(
                code = "NOT_FOUND",
                message = message
            )
        }

        /**
         * Create an INVALID_ARGUMENT error.
         */
        fun invalidArgument(message: String = "Invalid argument"): GreyError {
            return GreyError(
                code = "INVALID_ARGUMENT",
                message = message
            )
        }

        /**
         * Create an INTERNAL error.
         */
        fun internal(message: String = "Internal error"): GreyError {
            return GreyError(
                code = "INTERNAL",
                message = message
            )
        }
    }
}

/**
 * Result wrapper for Grey operations.
 * Contains either data or an error, never both.
 */
sealed class GreyResult<out T> {
    /**
     * Successful result containing data.
     */
    data class Success<T>(val data: T) : GreyResult<T>()

    /**
     * Failed result containing an error.
     */
    data class Failure(val error: GreyError) : GreyResult<Nothing>()

    /**
     * Whether the result is successful.
     */
    val isSuccess: Boolean
        get() = this is Success

    /**
     * Whether the result is a failure.
     */
    val isFailure: Boolean
        get() = this is Failure

    /**
     * Get the data if successful, null otherwise.
     */
    val data: T?
        get() = (this as? Success)?.data

    /**
     * Get the error if failed, null otherwise.
     */
    val error: GreyError?
        get() = (this as? Failure)?.error

    /**
     * Map the data if successful.
     */
    inline fun <R> map(transform: (T) -> R): GreyResult<R> {
        return when (this) {
            is Success -> Success(transform(data))
            is Failure -> this
        }
    }

    /**
     * Execute action if successful.
     */
    inline fun onSuccess(action: (T) -> Unit): GreyResult<T> {
        if (this is Success) {
            action(data)
        }
        return this
    }

    /**
     * Execute action if failed.
     */
    inline fun onFailure(action: (GreyError) -> Unit): GreyResult<T> {
        if (this is Failure) {
            action(error)
        }
        return this
    }

    /**
     * Get data or throw exception.
     */
    fun getOrThrow(): T {
        return when (this) {
            is Success -> data
            is Failure -> throw GreyException(error)
        }
    }

    /**
     * Get data or return default value.
     */
    fun getOrDefault(default: @UnsafeVariance T): T {
        return when (this) {
            is Success -> data
            is Failure -> default
        }
    }

    /**
     * Get data or compute default value.
     */
    inline fun getOrElse(default: (GreyError) -> @UnsafeVariance T): T {
        return when (this) {
            is Success -> data
            is Failure -> default(error)
        }
    }

    companion object {
        /**
         * Create a successful result.
         */
        fun <T> success(data: T): GreyResult<T> = Success(data)

        /**
         * Create a failed result.
         */
        fun failure(error: GreyError): GreyResult<Nothing> = Failure(error)

        /**
         * Create a failed result from an exception.
         */
        fun failure(e: Throwable): GreyResult<Nothing> = Failure(GreyError.fromException(e))

        /**
         * Wrap a suspend function call in a GreyResult.
         */
        suspend inline fun <T> catching(block: () -> T): GreyResult<T> {
            return try {
                Success(block())
            } catch (e: Throwable) {
                Failure(GreyError.fromException(e))
            }
        }
    }
}

/**
 * Exception wrapper for GreyError.
 */
class GreyException(val greyError: GreyError) : Exception(greyError.message) {
    override val message: String
        get() = "[${greyError.code}] ${greyError.message}"
}
