"""Grey SDK Error Type.

Normalized error type for all SDK operations.
"""

from .codes import ErrorCode


@value
struct GreyError:
    """Normalized error type for Grey SDK operations.
    
    Contains a code, message, and optional details for debugging.
    """

    var code: ErrorCode
    var message: String
    var details: String

    fn __init__(inout self, code: ErrorCode, message: String, details: String = ""):
        self.code = code
        self.message = message
        self.details = details

    fn __str__(self) -> String:
        if len(self.details) > 0:
            return self.code.to_string() + ": " + self.message + " (" + self.details + ")"
        return self.code.to_string() + ": " + self.message

    fn is_retryable(self) -> Bool:
        """Check if this error is retryable."""
        return self.code.is_retryable()

    # Factory methods for common errors

    @staticmethod
    fn unauthorized(message: String = "Authentication required") -> GreyError:
        """Create an unauthorized error."""
        return GreyError(ErrorCode.UNAUTHORIZED, message)

    @staticmethod
    fn forbidden(message: String = "Access denied") -> GreyError:
        """Create a forbidden error."""
        return GreyError(ErrorCode.FORBIDDEN, message)

    @staticmethod
    fn not_found(message: String = "Resource not found") -> GreyError:
        """Create a not found error."""
        return GreyError(ErrorCode.NOT_FOUND, message)

    @staticmethod
    fn validation_error(field: String, message: String) -> GreyError:
        """Create a validation error."""
        return GreyError(ErrorCode.VALIDATION_ERROR, message, field)

    @staticmethod
    fn network_error(message: String = "Network error") -> GreyError:
        """Create a network error."""
        return GreyError(ErrorCode.NETWORK_ERROR, message)

    @staticmethod
    fn timeout(message: String = "Request timed out") -> GreyError:
        """Create a timeout error."""
        return GreyError(ErrorCode.TIMEOUT, message)

    @staticmethod
    fn server_error(message: String = "Internal server error") -> GreyError:
        """Create a server error."""
        return GreyError(ErrorCode.SERVER_ERROR, message)

    @staticmethod
    fn from_http_status(status: Int, message: String, details: String = "") -> GreyError:
        """Create an error from HTTP status code."""
        return GreyError(ErrorCode.from_http_status(status), message, details)

    @staticmethod
    fn from_grpc_status(status: Int, message: String, details: String = "") -> GreyError:
        """Create an error from gRPC status code."""
        return GreyError(ErrorCode.from_grpc_status(status), message, details)
