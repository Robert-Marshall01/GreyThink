"""Grey SDK Result Type.

Type-safe result wrapper for operations that can fail.
"""

from .grey_error import GreyError
from .codes import ErrorCode


@value
struct Result[T: CollectionElement]:
    """Result type that can contain either a success value or an error.
    
    Provides a type-safe way to handle operations that can fail.
    """

    var _value: T
    var _error: GreyError
    var _is_ok: Bool

    fn __init__(inout self, value: T):
        """Create a success result."""
        self._value = value
        self._error = GreyError(ErrorCode.UNKNOWN, "")
        self._is_ok = True

    fn __init__(inout self, error: GreyError):
        """Create an error result."""
        self._value = T()  # Default value
        self._error = error
        self._is_ok = False

    @staticmethod
    fn ok(value: T) -> Result[T]:
        """Create a success result."""
        return Result[T](value)

    @staticmethod
    fn err(error: GreyError) -> Result[T]:
        """Create an error result."""
        return Result[T](error)

    fn is_ok(self) -> Bool:
        """Check if this is a success result."""
        return self._is_ok

    fn is_err(self) -> Bool:
        """Check if this is an error result."""
        return not self._is_ok

    fn unwrap(self) raises -> T:
        """Get the success value or raise an error."""
        if self._is_ok:
            return self._value
        raise Error("Result is an error: " + str(self._error))

    fn unwrap_or(self, default: T) -> T:
        """Get the success value or return a default."""
        if self._is_ok:
            return self._value
        return default

    fn unwrap_err(self) raises -> GreyError:
        """Get the error or raise if success."""
        if not self._is_ok:
            return self._error
        raise Error("Result is not an error")

    fn error(self) -> GreyError:
        """Get the error (undefined if is_ok)."""
        return self._error

    fn value(self) -> T:
        """Get the value (undefined if is_err)."""
        return self._value


@value
struct VoidResult:
    """Result type for operations that don't return a value."""

    var _error: GreyError
    var _is_ok: Bool

    fn __init__(inout self):
        """Create a success result."""
        self._error = GreyError(ErrorCode.UNKNOWN, "")
        self._is_ok = True

    fn __init__(inout self, error: GreyError):
        """Create an error result."""
        self._error = error
        self._is_ok = False

    @staticmethod
    fn ok() -> VoidResult:
        """Create a success result."""
        return VoidResult()

    @staticmethod
    fn err(error: GreyError) -> VoidResult:
        """Create an error result."""
        return VoidResult(error)

    fn is_ok(self) -> Bool:
        """Check if this is a success result."""
        return self._is_ok

    fn is_err(self) -> Bool:
        """Check if this is an error result."""
        return not self._is_ok

    fn unwrap_err(self) raises -> GreyError:
        """Get the error or raise if success."""
        if not self._is_ok:
            return self._error
        raise Error("Result is not an error")

    fn error(self) -> GreyError:
        """Get the error (undefined if is_ok)."""
        return self._error
