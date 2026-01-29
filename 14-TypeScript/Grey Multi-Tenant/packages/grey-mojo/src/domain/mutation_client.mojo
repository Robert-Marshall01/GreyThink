"""Grey SDK Mutation Domain Client.

Domain client for mutation operations with validation.
"""

from ..grpc.client import GrpcChannel
from ..grpc.mutation_service import (
    MutationService,
    MutationRequest,
    MutationResponse,
)
from ..error.grey_error import GreyError
from ..error.result import Result
from ..error.codes import ErrorCode


struct MutationBuilder:
    """Builder for mutation requests."""

    var mutation_name: String
    var parameters: Dict[String, String]
    var tenant_id: String

    fn __init__(inout self, mutation_name: String):
        self.mutation_name = mutation_name
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

    fn build(self) -> MutationRequest:
        """Build the mutation request."""
        return MutationRequest(self.mutation_name, self.parameters, self.tenant_id)


struct MutationClient:
    """Domain client for mutation operations with validation."""

    var _service: MutationService
    var _channel: GrpcChannel

    alias MAX_MUTATION_NAME_LENGTH: Int = 255

    fn __init__(inout self, channel: GrpcChannel) raises:
        """Create a new mutation client."""
        self._channel = channel
        self._service = MutationService(channel)

    fn mutation_builder(self, mutation_name: String) -> MutationBuilder:
        """Create a mutation builder."""
        return MutationBuilder(mutation_name)

    fn mutate(self, mutation_name: String) -> Result[MutationResponse]:
        """Execute a simple mutation by name."""
        let request = MutationRequest(mutation_name)
        return self.execute_mutation(request)

    fn mutate_with_params(
        self,
        mutation_name: String,
        params: Dict[String, String],
    ) -> Result[MutationResponse]:
        """Execute a mutation with parameters."""
        let request = MutationRequest(mutation_name, params)
        return self.execute_mutation(request)

    fn execute_mutation(self, request: MutationRequest) -> Result[MutationResponse]:
        """Execute a mutation request."""
        # Check if authenticated
        if not self._channel.has_auth_token():
            return Result[MutationResponse].err(
                GreyError.unauthorized("Authentication required")
            )
        
        # Validate mutation name
        if len(request.mutation_name) == 0:
            return Result[MutationResponse].err(
                GreyError.validation_error("mutation_name", "Mutation name is required")
            )
        
        if len(request.mutation_name) > Self.MAX_MUTATION_NAME_LENGTH:
            return Result[MutationResponse].err(
                GreyError.validation_error("mutation_name", "Mutation name too long")
            )
        
        return self._service.mutate(request)
