"""Grey SDK Error Codes.

Standardized error codes for the Grey Multi-Tenant SDK.
"""


@value
struct ErrorCode:
    """Enumeration of error codes."""

    var value: Int

    # Error code constants
    alias UNAUTHORIZED = ErrorCode(1)
    alias FORBIDDEN = ErrorCode(2)
    alias NOT_FOUND = ErrorCode(3)
    alias VALIDATION_ERROR = ErrorCode(4)
    alias NETWORK_ERROR = ErrorCode(5)
    alias TIMEOUT = ErrorCode(6)
    alias SERVER_ERROR = ErrorCode(7)
    alias UNKNOWN = ErrorCode(0)

    fn __init__(inout self, value: Int):
        self.value = value

    fn __eq__(self, other: ErrorCode) -> Bool:
        return self.value == other.value

    fn __ne__(self, other: ErrorCode) -> Bool:
        return self.value != other.value

    fn from_http_status(status: Int) -> ErrorCode:
        """Convert HTTP status code to ErrorCode."""
        if status == 401:
            return ErrorCode.UNAUTHORIZED
        elif status == 403:
            return ErrorCode.FORBIDDEN
        elif status == 404:
            return ErrorCode.NOT_FOUND
        elif status == 400 or status == 422:
            return ErrorCode.VALIDATION_ERROR
        elif status == 408 or status == 504:
            return ErrorCode.TIMEOUT
        elif status >= 500:
            return ErrorCode.SERVER_ERROR
        else:
            return ErrorCode.UNKNOWN

    fn from_grpc_status(status: Int) -> ErrorCode:
        """Convert gRPC status code to ErrorCode."""
        if status == 16:  # UNAUTHENTICATED
            return ErrorCode.UNAUTHORIZED
        elif status == 7:  # PERMISSION_DENIED
            return ErrorCode.FORBIDDEN
        elif status == 5:  # NOT_FOUND
            return ErrorCode.NOT_FOUND
        elif status == 3:  # INVALID_ARGUMENT
            return ErrorCode.VALIDATION_ERROR
        elif status == 4:  # DEADLINE_EXCEEDED
            return ErrorCode.TIMEOUT
        elif status == 14:  # UNAVAILABLE
            return ErrorCode.NETWORK_ERROR
        elif status == 13:  # INTERNAL
            return ErrorCode.SERVER_ERROR
        else:
            return ErrorCode.UNKNOWN

    fn is_retryable(self) -> Bool:
        """Check if this error type is retryable."""
        return (
            self == ErrorCode.NETWORK_ERROR
            or self == ErrorCode.TIMEOUT
            or self == ErrorCode.SERVER_ERROR
        )

    fn to_string(self) -> String:
        """Get string representation of the error code."""
        if self == ErrorCode.UNAUTHORIZED:
            return "unauthorized"
        elif self == ErrorCode.FORBIDDEN:
            return "forbidden"
        elif self == ErrorCode.NOT_FOUND:
            return "not_found"
        elif self == ErrorCode.VALIDATION_ERROR:
            return "validation_error"
        elif self == ErrorCode.NETWORK_ERROR:
            return "network_error"
        elif self == ErrorCode.TIMEOUT:
            return "timeout"
        elif self == ErrorCode.SERVER_ERROR:
            return "server_error"
        else:
            return "unknown"

    fn message(self) -> String:
        """Get default message for the error code."""
        if self == ErrorCode.UNAUTHORIZED:
            return "Authentication required"
        elif self == ErrorCode.FORBIDDEN:
            return "Access denied"
        elif self == ErrorCode.NOT_FOUND:
            return "Resource not found"
        elif self == ErrorCode.VALIDATION_ERROR:
            return "Validation failed"
        elif self == ErrorCode.NETWORK_ERROR:
            return "Network error"
        elif self == ErrorCode.TIMEOUT:
            return "Request timed out"
        elif self == ErrorCode.SERVER_ERROR:
            return "Internal server error"
        else:
            return "Unknown error"
