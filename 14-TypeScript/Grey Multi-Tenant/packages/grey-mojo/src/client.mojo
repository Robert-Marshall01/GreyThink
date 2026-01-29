"""Grey Multi-Tenant SDK Client.

Main facade for the Grey Multi-Tenant SDK providing unified access
to all domain clients.
"""

from .config.options import Options
from .grpc.client import GrpcChannel
from .domain.auth_client import AuthClient
from .domain.user_client import UserClient
from .domain.projects_client import ProjectsClient
from .domain.query_client import QueryClient
from .domain.mutation_client import MutationClient
from .error.grey_error import GreyError
from .error.result import Result, VoidResult
from .error.codes import ErrorCode


struct GreyClient:
    """Grey Multi-Tenant SDK Client.
    
    Provides unified access to all Grey Multi-Tenant services.
    """

    var _channel: GrpcChannel
    var _options: Options
    var auth: AuthClient
    var user: UserClient
    var projects: ProjectsClient
    var query: QueryClient
    var mutation: MutationClient

    fn __init__(inout self, options: Options) raises:
        """Create a new Grey client with options."""
        self._options = options
        self._channel = GrpcChannel(options)
        self.auth = AuthClient(self._channel)
        self.user = UserClient(self._channel)
        self.projects = ProjectsClient(self._channel)
        self.query = QueryClient(self._channel)
        self.mutation = MutationClient(self._channel)

    @staticmethod
    fn local() raises -> GreyClient:
        """Create a client for local development."""
        return GreyClient(Options.local())

    @staticmethod
    fn production(host: String) raises -> GreyClient:
        """Create a client for production."""
        return GreyClient(Options.production(host))

    fn set_auth_token(inout self, token: String):
        """Set authentication token."""
        self._channel.set_auth_token(token)

    fn clear_auth_token(inout self):
        """Clear authentication token."""
        self._channel.clear_auth_token()

    fn is_authenticated(self) -> Bool:
        """Check if the client is authenticated."""
        return self._channel.has_auth_token()

    fn options(self) -> Options:
        """Get the client options."""
        return self._options


fn create_client() raises -> GreyClient:
    """Create a new Grey client with default local options."""
    return GreyClient.local()


fn create_client_with_options(options: Options) raises -> GreyClient:
    """Create a new Grey client with custom options."""
    return GreyClient(options)
