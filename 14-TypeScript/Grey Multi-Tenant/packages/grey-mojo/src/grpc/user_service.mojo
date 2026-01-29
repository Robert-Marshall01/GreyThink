"""Grey SDK User gRPC Service.

gRPC service bindings for user operations.
"""

from python import Python, PythonObject
from .client import GrpcChannel, grpc_error_to_grey_error
from ..error.grey_error import GreyError
from ..error.result import Result
from ..error.codes import ErrorCode


@value
struct GetUserRequest:
    """Get user request data."""
    var user_id: String

    fn __init__(inout self, user_id: String):
        self.user_id = user_id

    fn to_python(self) raises -> PythonObject:
        """Convert to Python dict for gRPC."""
        let py = Python.import_module("builtins")
        let request = py.dict()
        request["user_id"] = self.user_id
        return request


@value
struct UserResponse:
    """User response data."""
    var user_id: String
    var email: String
    var display_name: String
    var tenant_id: String

    fn __init__(
        inout self,
        user_id: String = "",
        email: String = "",
        display_name: String = "",
        tenant_id: String = "",
    ):
        self.user_id = user_id
        self.email = email
        self.display_name = display_name
        self.tenant_id = tenant_id

    @staticmethod
    fn from_python(response: PythonObject) raises -> UserResponse:
        """Create from Python gRPC response."""
        var display_name = ""
        var tenant_id = ""
        
        # Handle optional fields
        try:
            display_name = str(response.display_name)
        except:
            pass
        
        try:
            tenant_id = str(response.tenant_id)
        except:
            pass
        
        return UserResponse(
            str(response.user_id),
            str(response.email),
            display_name,
            tenant_id,
        )


struct UserService:
    """gRPC User service client."""

    var _channel: GrpcChannel
    var _stub: PythonObject

    fn __init__(inout self, channel: GrpcChannel) raises:
        """Create a new User service client."""
        self._channel = channel
        
        # Import the generated Python gRPC stub
        let grpc_stubs = Python.import_module("grey_pb2_grpc")
        self._stub = grpc_stubs.UserServiceStub(channel.channel())

    fn get_current_user(self) -> Result[UserResponse]:
        """Get the current authenticated user."""
        try:
            let py = Python.import_module("builtins")
            let empty = py.dict()
            let metadata = self._channel.get_metadata()
            let response = self._stub.GetCurrentUser(empty, metadata=metadata)
            return Result[UserResponse].ok(UserResponse.from_python(response))
        except e:
            return Result[UserResponse].err(
                GreyError.server_error("GetCurrentUser failed: " + str(e))
            )

    fn get_user(self, request: GetUserRequest) -> Result[UserResponse]:
        """Get a user by ID."""
        try:
            let py_request = request.to_python()
            let metadata = self._channel.get_metadata()
            let response = self._stub.GetUser(py_request, metadata=metadata)
            return Result[UserResponse].ok(UserResponse.from_python(response))
        except e:
            return Result[UserResponse].err(
                GreyError.server_error("GetUser failed: " + str(e))
            )
