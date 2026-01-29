"""Grey SDK Mutation gRPC Service.

gRPC service bindings for mutation operations.
"""

from python import Python, PythonObject
from .client import GrpcChannel, grpc_error_to_grey_error
from ..error.grey_error import GreyError
from ..error.result import Result
from ..error.codes import ErrorCode


@value
struct MutationRequest:
    """Mutation request data."""
    var mutation_name: String
    var parameters: Dict[String, String]
    var tenant_id: String

    fn __init__(
        inout self,
        mutation_name: String,
        parameters: Dict[String, String] = Dict[String, String](),
        tenant_id: String = "",
    ):
        self.mutation_name = mutation_name
        self.parameters = parameters
        self.tenant_id = tenant_id

    fn to_python(self) raises -> PythonObject:
        """Convert to Python dict for gRPC."""
        let py = Python.import_module("builtins")
        let request = py.dict()
        request["mutation_name"] = self.mutation_name
        
        # Convert parameters
        let py_params = py.dict()
        for key in self.parameters:
            py_params[key] = self.parameters[key]
        request["parameters"] = py_params
        
        if len(self.tenant_id) > 0:
            request["tenant_id"] = self.tenant_id
        
        return request


@value
struct MutationResponse:
    """Mutation response data."""
    var success: Bool
    var message: String
    var data: String
    var metadata: String

    fn __init__(
        inout self,
        success: Bool = False,
        message: String = "",
        data: String = "",
        metadata: String = "",
    ):
        self.success = success
        self.message = message
        self.data = data
        self.metadata = metadata

    @staticmethod
    fn from_python(response: PythonObject) raises -> MutationResponse:
        """Create from Python gRPC response."""
        var message = ""
        var data = ""
        var metadata = ""
        
        try:
            message = str(response.message)
        except:
            pass
        try:
            data = str(response.data)
        except:
            pass
        try:
            metadata = str(response.metadata)
        except:
            pass
        
        return MutationResponse(
            bool(response.success),
            message,
            data,
            metadata,
        )


struct MutationService:
    """gRPC Mutation service client."""

    var _channel: GrpcChannel
    var _stub: PythonObject

    fn __init__(inout self, channel: GrpcChannel) raises:
        """Create a new Mutation service client."""
        self._channel = channel
        
        # Import the generated Python gRPC stub
        let grpc_stubs = Python.import_module("grey_pb2_grpc")
        self._stub = grpc_stubs.MutationServiceStub(channel.channel())

    fn mutate(self, request: MutationRequest) -> Result[MutationResponse]:
        """Execute a mutation."""
        try:
            let py_request = request.to_python()
            let metadata = self._channel.get_metadata()
            let response = self._stub.Mutate(py_request, metadata=metadata)
            return Result[MutationResponse].ok(MutationResponse.from_python(response))
        except e:
            return Result[MutationResponse].err(
                GreyError.server_error("Mutation failed: " + str(e))
            )
