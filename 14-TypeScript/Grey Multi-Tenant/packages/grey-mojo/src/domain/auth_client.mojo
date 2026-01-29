"""Grey SDK Auth Domain Client.

Domain client for authentication with validation.
"""

from ..grpc.client import GrpcChannel
from ..grpc.auth_service import (
    AuthService,
    LoginRequest,
    LoginResponse,
    RefreshRequest,
)
from ..error.grey_error import GreyError
from ..error.result import Result, VoidResult
from ..error.codes import ErrorCode


fn is_valid_email(email: String) -> Bool:
    """Simple email validation."""
    if len(email) == 0:
        return False
    
    # Find @ position
    var at_pos: Int = -1
    for i in range(len(email)):
        if email[i] == "@":
            at_pos = i
            break
    
    if at_pos < 0:
        return False
    
    # Must have chars before and after @
    return at_pos > 0 and at_pos < len(email) - 1


struct AuthClient:
    """Domain client for authentication with validation."""

    var _service: AuthService
    var _channel: GrpcChannel

    fn __init__(inout self, channel: GrpcChannel) raises:
        """Create a new auth client."""
        self._channel = channel
        self._service = AuthService(channel)

    fn login(inout self, email: String, password: String) -> Result[LoginResponse]:
        """Login with email and password."""
        return self.login_with_tenant(email, password, "")

    fn login_with_tenant(
        inout self,
        email: String,
        password: String,
        tenant_id: String,
    ) -> Result[LoginResponse]:
        """Login with email, password, and optional tenant."""
        # Validate email
        if len(email) == 0:
            return Result[LoginResponse].err(
                GreyError.validation_error("email", "Email is required")
            )
        
        if not is_valid_email(email):
            return Result[LoginResponse].err(
                GreyError.validation_error("email", "Invalid email format")
            )
        
        # Validate password
        if len(password) == 0:
            return Result[LoginResponse].err(
                GreyError.validation_error("password", "Password is required")
            )
        
        if len(password) < 8:
            return Result[LoginResponse].err(
                GreyError.validation_error("password", "Password must be at least 8 characters")
            )
        
        # Make request
        let request = LoginRequest(email, password, tenant_id)
        let result = self._service.login(request)
        
        # Update channel auth token on success
        if result.is_ok():
            try:
                let response = result.unwrap()
                self._channel.set_auth_token(response.access_token)
            except:
                pass
        
        return result

    fn logout(inout self) -> VoidResult:
        """Logout current session."""
        let result = self._service.logout()
        
        # Clear auth token regardless of result
        self._channel.clear_auth_token()
        
        return result

    fn refresh(inout self, refresh_token: String) -> Result[LoginResponse]:
        """Refresh authentication token."""
        # Validate refresh token
        if len(refresh_token) == 0:
            return Result[LoginResponse].err(
                GreyError.validation_error("refresh_token", "Refresh token is required")
            )
        
        let request = RefreshRequest(refresh_token)
        let result = self._service.refresh(request)
        
        # Update channel auth token on success
        if result.is_ok():
            try:
                let response = result.unwrap()
                self._channel.set_auth_token(response.access_token)
            except:
                pass
        
        return result
