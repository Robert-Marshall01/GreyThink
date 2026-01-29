"""Grey SDK Query Domain Client.

Domain client for query operations with validation.
"""

from ..grpc.client import GrpcChannel
from ..grpc.query_service import (
    QueryService,
    QueryRequest,
    QueryResponse,
)
from ..error.grey_error import GreyError
from ..error.result import Result
from ..error.codes import ErrorCode


struct QueryBuilder:
    """Builder for query requests."""

    var query_name: String
    var parameters: Dict[String, String]
    var tenant_id: String

    fn __init__(inout self, query_name: String):
        self.query_name = query_name
        self.parameters = Dict[String, String]()
        self.tenant_id = ""

    fn param(inout self, key: String, value: String) -> Self:
        """Add a parameter."""
        self.parameters[key] = value
        return self

    fn for_tenant(inout self, tenant_id: String) -> Self:
        """Set tenant ID."""
        self.tenant_id = tenant_id
        return self

    fn build(self) -> QueryRequest:
        """Build the query request."""
        return QueryRequest(self.query_name, self.parameters, self.tenant_id)


struct QueryClient:
    """Domain client for query operations with validation."""

    var _service: QueryService
    var _channel: GrpcChannel

    alias MAX_QUERY_NAME_LENGTH: Int = 255

    fn __init__(inout self, channel: GrpcChannel) raises:
        """Create a new query client."""
        self._channel = channel
        self._service = QueryService(channel)

    fn query_builder(self, query_name: String) -> QueryBuilder:
        """Create a query builder."""
        return QueryBuilder(query_name)

    fn query(self, query_name: String) -> Result[QueryResponse]:
        """Execute a simple query by name."""
        let request = QueryRequest(query_name)
        return self.execute_query(request)

    fn query_with_params(
        self,
        query_name: String,
        params: Dict[String, String],
    ) -> Result[QueryResponse]:
        """Execute a query with parameters."""
        let request = QueryRequest(query_name, params)
        return self.execute_query(request)

    fn execute_query(self, request: QueryRequest) -> Result[QueryResponse]:
        """Execute a query request."""
        # Check if authenticated
        if not self._channel.has_auth_token():
            return Result[QueryResponse].err(
                GreyError.unauthorized("Authentication required")
            )
        
        # Validate query name
        if len(request.query_name) == 0:
            return Result[QueryResponse].err(
                GreyError.validation_error("query_name", "Query name is required")
            )
        
        if len(request.query_name) > Self.MAX_QUERY_NAME_LENGTH:
            return Result[QueryResponse].err(
                GreyError.validation_error("query_name", "Query name too long")
            )
        
        return self._service.query(request)
