"""Grey Multi-Tenant SDK for Mojo.

A complete SDK for integrating with Grey Multi-Tenant services using
gRPC transport via Python interop.

Example:
    ```mojo
    from grey import create_client
    
    fn main() raises:
        var client = create_client()
        
        # Login
        let result = client.auth.login("user@example.com", "password123")
        if result.is_ok():
            print("Logged in successfully!")
            
            # Get current user
            let user_result = client.user.get_current_user()
            if user_result.is_ok():
                let user = user_result.unwrap()
                print("Welcome, " + user.email)
    ```
"""

# Version info
alias VERSION = "0.0.1"
alias VERSION_MAJOR = 0
alias VERSION_MINOR = 0
alias VERSION_PATCH = 1

# Error types
from .error.codes import ErrorCode
from .error.grey_error import GreyError
from .error.result import Result, VoidResult

# Configuration
from .config.options import Options

# gRPC layer
from .grpc.client import GrpcChannel
from .grpc.auth_service import (
    AuthService,
    LoginRequest,
    LoginResponse,
    RefreshRequest,
)
from .grpc.user_service import (
    UserService,
    GetUserRequest,
    UserResponse,
)
from .grpc.projects_service import (
    ProjectsService,
    Project,
    ListProjectsRequest,
    ListProjectsResponse,
    CreateProjectRequest,
)
from .grpc.query_service import (
    QueryService,
    QueryRequest,
    QueryResponse,
)
from .grpc.mutation_service import (
    MutationService,
    MutationRequest,
    MutationResponse,
)

# Domain clients
from .domain.auth_client import AuthClient
from .domain.user_client import UserClient, User
from .domain.projects_client import ProjectsClient, PaginationOptions
from .domain.query_client import QueryClient, QueryBuilder
from .domain.mutation_client import MutationClient, MutationBuilder

# Main client
from .client import GreyClient, create_client, create_client_with_options
