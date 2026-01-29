"""Grey SDK gRPC Client.

Base gRPC client functionality using Python interop.
"""

from python import Python, PythonObject
from ..config.options import Options
from ..error.grey_error import GreyError
from ..error.codes import ErrorCode


struct GrpcChannel:
    """gRPC Channel wrapper using Python interop."""

    var _channel: PythonObject
    var _options: Options
    var _auth_token: String
    var _grpc: PythonObject

    fn __init__(inout self, options: Options) raises:
        """Create a new gRPC channel."""
        self._options = options
        self._auth_token = options.auth_token
        
        # Import Python gRPC library
        self._grpc = Python.import_module("grpc")
        
        # Create channel
        let target = options.grpc_target()
        if options.use_tls:
            let credentials = self._grpc.ssl_channel_credentials()
            self._channel = self._grpc.secure_channel(target, credentials)
        else:
            self._channel = self._grpc.insecure_channel(target)

    fn __del__(owned self):
        """Close the channel."""
        try:
            _ = self._channel.close()
        except:
            pass

    fn set_auth_token(inout self, token: String):
        """Set the authentication token."""
        self._auth_token = token

    fn clear_auth_token(inout self):
        """Clear the authentication token."""
        self._auth_token = ""

    fn has_auth_token(self) -> Bool:
        """Check if an auth token is set."""
        return len(self._auth_token) > 0

    fn get_metadata(self) raises -> PythonObject:
        """Get metadata for gRPC calls including auth header."""
        let metadata = Python.list()
        
        if len(self._auth_token) > 0:
            let auth_tuple = Python.tuple()
            # Would be: [("authorization", "Bearer " + token)]
            _ = metadata.append(("authorization", "Bearer " + self._auth_token))
        
        return metadata

    fn channel(self) -> PythonObject:
        """Get the underlying Python gRPC channel."""
        return self._channel


fn grpc_error_to_grey_error(grpc_error: PythonObject) raises -> GreyError:
    """Convert a Python gRPC error to a GreyError."""
    let code = int(grpc_error.code().value[0])
    let message = str(grpc_error.details())
    return GreyError.from_grpc_status(code, message)


fn handle_grpc_call[T: CollectionElement](
    call_fn: fn() raises -> T
) raises -> T:
    """Execute a gRPC call and handle errors."""
    try:
        return call_fn()
    except e:
        # Re-raise as we can't easily distinguish error types in Mojo
        raise e
