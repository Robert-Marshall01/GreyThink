"""Grey SDK User Domain Client.

Domain client for user operations with validation.
"""

from ..grpc.client import GrpcChannel
from ..grpc.user_service import (
    UserService,
    GetUserRequest,
    UserResponse,
)
from ..error.grey_error import GreyError
from ..error.result import Result
from ..error.codes import ErrorCode


@value
struct User:
    """User domain model."""
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
    fn from_response(response: UserResponse) -> User:
        """Create from UserResponse."""
        return User(
            response.user_id,
            response.email,
            response.display_name,
            response.tenant_id,
        )


struct UserClient:
    """Domain client for user operations with validation."""

    var _service: UserService
    var _channel: GrpcChannel

    fn __init__(inout self, channel: GrpcChannel) raises:
        """Create a new user client."""
        self._channel = channel
        self._service = UserService(channel)

    fn get_current_user(self) -> Result[User]:
        """Get the current authenticated user."""
        # Check if authenticated
        if not self._channel.has_auth_token():
            return Result[User].err(
                GreyError.unauthorized("Authentication required")
            )
        
        let result = self._service.get_current_user()
        
        if result.is_ok():
            try:
                let response = result.unwrap()
                return Result[User].ok(User.from_response(response))
            except e:
                return Result[User].err(
                    GreyError.server_error("Failed to parse response: " + str(e))
                )
        
        return Result[User].err(result.error())

    fn get_user(self, user_id: String) -> Result[User]:
        """Get a user by ID."""
        # Check if authenticated
        if not self._channel.has_auth_token():
            return Result[User].err(
                GreyError.unauthorized("Authentication required")
            )
        
        # Validate user ID
        if len(user_id) == 0:
            return Result[User].err(
                GreyError.validation_error("user_id", "User ID is required")
            )
        
        let request = GetUserRequest(user_id)
        let result = self._service.get_user(request)
        
        if result.is_ok():
            try:
                let response = result.unwrap()
                return Result[User].ok(User.from_response(response))
            except e:
                return Result[User].err(
                    GreyError.server_error("Failed to parse response: " + str(e))
                )
        
        return Result[User].err(result.error())
