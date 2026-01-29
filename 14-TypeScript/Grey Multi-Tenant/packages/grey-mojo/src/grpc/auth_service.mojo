"""Grey SDK Auth gRPC Service.

gRPC service bindings for authentication operations.
"""

from python import Python, PythonObject
from .client import GrpcChannel, grpc_error_to_grey_error
from ..error.grey_error import GreyError
from ..error.result import Result, VoidResult
from ..error.codes import ErrorCode


@value
struct LoginRequest:
    """Login request data."""
    var email: String
    var password: String
    var tenant_id: String

    fn __init__(inout self, email: String, password: String, tenant_id: String = ""):
        self.email = email
        self.password = password
        self.tenant_id = tenant_id

    fn to_python(self) raises -> PythonObject:
        """Convert to Python dict for gRPC."""
        let py = Python.import_module("builtins")
        let request = py.dict()
        request["email"] = self.email
        request["password"] = self.password
        if len(self.tenant_id) > 0:
            request["tenant_id"] = self.tenant_id
        return request


@value
struct LoginResponse:
    """Login response data."""
    var access_token: String
    var refresh_token: String
    var expires_in: Int

    fn __init__(inout self, access_token: String = "", refresh_token: String = "", expires_in: Int = 0):
        self.access_token = access_token
        self.refresh_token = refresh_token
        self.expires_in = expires_in

    @staticmethod
    fn from_python(response: PythonObject) raises -> LoginResponse:
        """Create from Python gRPC response."""
        return LoginResponse(
            str(response.access_token),
            str(response.refresh_token),
            int(response.expires_in),
        )


@value
struct RefreshRequest:
    """Refresh token request data."""
    var refresh_token: String

    fn __init__(inout self, refresh_token: String):
        self.refresh_token = refresh_token

    fn to_python(self) raises -> PythonObject:
        """Convert to Python dict for gRPC."""
        let py = Python.import_module("builtins")
        let request = py.dict()
        request["refresh_token"] = self.refresh_token
        return request


struct AuthService:
    """gRPC Auth service client."""

    var _channel: GrpcChannel
    var _stub: PythonObject

    fn __init__(inout self, channel: GrpcChannel) raises:
        """Create a new Auth service client."""
        self._channel = channel
        
        # Import the generated Python gRPC stub
        # In production, this would be: grey_pb2_grpc.AuthServiceStub
        let grpc_stubs = Python.import_module("grey_pb2_grpc")
        self._stub = grpc_stubs.AuthServiceStub(channel.channel())

    fn login(self, request: LoginRequest) -> Result[LoginResponse]:
        """Login with credentials."""
        try:
            let py_request = request.to_python()
            let metadata = self._channel.get_metadata()
            let response = self._stub.Login(py_request, metadata=metadata)
            return Result[LoginResponse].ok(LoginResponse.from_python(response))
        except e:
            return Result[LoginResponse].err(
                GreyError.server_error("Login failed: " + str(e))
            )

    fn logout(self) -> VoidResult:
        """Logout current session."""
        try:
            let py = Python.import_module("builtins")
            let empty = py.dict()
            let metadata = self._channel.get_metadata()
            _ = self._stub.Logout(empty, metadata=metadata)
            return VoidResult.ok()
        except e:
            return VoidResult.err(
                GreyError.server_error("Logout failed: " + str(e))
            )

    fn refresh(self, request: RefreshRequest) -> Result[LoginResponse]:
        """Refresh authentication token."""
        try:
            let py_request = request.to_python()
            let metadata = self._channel.get_metadata()
            let response = self._stub.Refresh(py_request, metadata=metadata)
            return Result[LoginResponse].ok(LoginResponse.from_python(response))
        except e:
            return Result[LoginResponse].err(
                GreyError.server_error("Refresh failed: " + str(e))
            )
