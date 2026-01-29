"""Grey SDK Query gRPC Service.

gRPC service bindings for query operations.
"""

from python import Python, PythonObject
from .client import GrpcChannel, grpc_error_to_grey_error
from ..error.grey_error import GreyError
from ..error.result import Result
from ..error.codes import ErrorCode


@value
struct QueryRequest:
    """Query request data."""
    var query_name: String
    var parameters: Dict[String, String]
    var tenant_id: String

    fn __init__(
        inout self,
        query_name: String,
        parameters: Dict[String, String] = Dict[String, String](),
        tenant_id: String = "",
    ):
        self.query_name = query_name
        self.parameters = parameters
        self.tenant_id = tenant_id

    fn to_python(self) raises -> PythonObject:
        """Convert to Python dict for gRPC."""
        let py = Python.import_module("builtins")
        let request = py.dict()
        request["query_name"] = self.query_name
        
        # Convert parameters
        let py_params = py.dict()
        for key in self.parameters:
            py_params[key] = self.parameters[key]
        request["parameters"] = py_params
        
        if len(self.tenant_id) > 0:
            request["tenant_id"] = self.tenant_id
        
        return request


@value
struct QueryResponse:
    """Query response data."""
    var data: String
    var metadata: String

    fn __init__(inout self, data: String = "", metadata: String = ""):
        self.data = data
        self.metadata = metadata

    @staticmethod
    fn from_python(response: PythonObject) raises -> QueryResponse:
        """Create from Python gRPC response."""
        var metadata = ""
        try:
            metadata = str(response.metadata)
        except:
            pass
        
        return QueryResponse(
            str(response.data),
            metadata,
        )


struct QueryService:
    """gRPC Query service client."""

    var _channel: GrpcChannel
    var _stub: PythonObject

    fn __init__(inout self, channel: GrpcChannel) raises:
        """Create a new Query service client."""
        self._channel = channel
        
        # Import the generated Python gRPC stub
        let grpc_stubs = Python.import_module("grey_pb2_grpc")
        self._stub = grpc_stubs.QueryServiceStub(channel.channel())

    fn query(self, request: QueryRequest) -> Result[QueryResponse]:
        """Execute a query."""
        try:
            let py_request = request.to_python()
            let metadata = self._channel.get_metadata()
            let response = self._stub.Query(py_request, metadata=metadata)
            return Result[QueryResponse].ok(QueryResponse.from_python(response))
        except e:
            return Result[QueryResponse].err(
                GreyError.server_error("Query failed: " + str(e))
            )
