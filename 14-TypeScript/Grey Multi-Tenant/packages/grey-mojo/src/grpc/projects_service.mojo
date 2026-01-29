"""Grey SDK Projects gRPC Service.

gRPC service bindings for project operations.
"""

from python import Python, PythonObject
from .client import GrpcChannel, grpc_error_to_grey_error
from ..error.grey_error import GreyError
from ..error.result import Result
from ..error.codes import ErrorCode


@value
struct Project:
    """Project data."""
    var project_id: String
    var name: String
    var description: String
    var tenant_id: String
    var created_at: String
    var updated_at: String

    fn __init__(
        inout self,
        project_id: String = "",
        name: String = "",
        description: String = "",
        tenant_id: String = "",
        created_at: String = "",
        updated_at: String = "",
    ):
        self.project_id = project_id
        self.name = name
        self.description = description
        self.tenant_id = tenant_id
        self.created_at = created_at
        self.updated_at = updated_at

    @staticmethod
    fn from_python(response: PythonObject) raises -> Project:
        """Create from Python gRPC response."""
        var description = ""
        var tenant_id = ""
        var created_at = ""
        var updated_at = ""
        
        try:
            description = str(response.description)
        except:
            pass
        try:
            tenant_id = str(response.tenant_id)
        except:
            pass
        try:
            created_at = str(response.created_at)
        except:
            pass
        try:
            updated_at = str(response.updated_at)
        except:
            pass
        
        return Project(
            str(response.project_id),
            str(response.name),
            description,
            tenant_id,
            created_at,
            updated_at,
        )


@value
struct ListProjectsRequest:
    """List projects request data."""
    var limit: Int
    var offset: Int
    var tenant_id: String

    fn __init__(inout self, limit: Int = 10, offset: Int = 0, tenant_id: String = ""):
        self.limit = limit
        self.offset = offset
        self.tenant_id = tenant_id

    fn to_python(self) raises -> PythonObject:
        """Convert to Python dict for gRPC."""
        let py = Python.import_module("builtins")
        let request = py.dict()
        request["limit"] = self.limit
        request["offset"] = self.offset
        if len(self.tenant_id) > 0:
            request["tenant_id"] = self.tenant_id
        return request


@value
struct ListProjectsResponse:
    """List projects response data."""
    var projects: List[Project]
    var total: Int

    fn __init__(inout self, projects: List[Project] = List[Project](), total: Int = 0):
        self.projects = projects
        self.total = total

    @staticmethod
    fn from_python(response: PythonObject) raises -> ListProjectsResponse:
        """Create from Python gRPC response."""
        var projects = List[Project]()
        let py_projects = response.projects
        let count = int(Python.import_module("builtins").len(py_projects))
        
        for i in range(count):
            let py_project = py_projects[i]
            projects.append(Project.from_python(py_project))
        
        return ListProjectsResponse(projects, int(response.total))


@value
struct CreateProjectRequest:
    """Create project request data."""
    var name: String
    var description: String
    var tenant_id: String

    fn __init__(inout self, name: String, description: String = "", tenant_id: String = ""):
        self.name = name
        self.description = description
        self.tenant_id = tenant_id

    fn to_python(self) raises -> PythonObject:
        """Convert to Python dict for gRPC."""
        let py = Python.import_module("builtins")
        let request = py.dict()
        request["name"] = self.name
        if len(self.description) > 0:
            request["description"] = self.description
        if len(self.tenant_id) > 0:
            request["tenant_id"] = self.tenant_id
        return request


struct ProjectsService:
    """gRPC Projects service client."""

    var _channel: GrpcChannel
    var _stub: PythonObject

    fn __init__(inout self, channel: GrpcChannel) raises:
        """Create a new Projects service client."""
        self._channel = channel
        
        # Import the generated Python gRPC stub
        let grpc_stubs = Python.import_module("grey_pb2_grpc")
        self._stub = grpc_stubs.ProjectsServiceStub(channel.channel())

    fn list_projects(self, request: ListProjectsRequest) -> Result[ListProjectsResponse]:
        """List projects with pagination."""
        try:
            let py_request = request.to_python()
            let metadata = self._channel.get_metadata()
            let response = self._stub.ListProjects(py_request, metadata=metadata)
            return Result[ListProjectsResponse].ok(ListProjectsResponse.from_python(response))
        except e:
            return Result[ListProjectsResponse].err(
                GreyError.server_error("ListProjects failed: " + str(e))
            )

    fn create_project(self, request: CreateProjectRequest) -> Result[Project]:
        """Create a new project."""
        try:
            let py_request = request.to_python()
            let metadata = self._channel.get_metadata()
            let response = self._stub.CreateProject(py_request, metadata=metadata)
            return Result[Project].ok(Project.from_python(response))
        except e:
            return Result[Project].err(
                GreyError.server_error("CreateProject failed: " + str(e))
            )

    fn get_project(self, project_id: String) -> Result[Project]:
        """Get a project by ID."""
        try:
            let py = Python.import_module("builtins")
            let request = py.dict()
            request["project_id"] = project_id
            let metadata = self._channel.get_metadata()
            let response = self._stub.GetProject(request, metadata=metadata)
            return Result[Project].ok(Project.from_python(response))
        except e:
            return Result[Project].err(
                GreyError.server_error("GetProject failed: " + str(e))
            )
