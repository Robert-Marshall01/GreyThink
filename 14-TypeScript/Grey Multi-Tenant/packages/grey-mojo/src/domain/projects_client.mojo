"""Grey SDK Projects Domain Client.

Domain client for project operations with validation.
"""

from ..grpc.client import GrpcChannel
from ..grpc.projects_service import (
    ProjectsService,
    Project,
    ListProjectsRequest,
    ListProjectsResponse,
    CreateProjectRequest,
)
from ..error.grey_error import GreyError
from ..error.result import Result
from ..error.codes import ErrorCode


@value
struct PaginationOptions:
    """Pagination options for list operations."""
    var limit: Int
    var offset: Int

    fn __init__(inout self, limit: Int = 10, offset: Int = 0):
        self.limit = limit
        self.offset = offset


struct ProjectsClient:
    """Domain client for project operations with validation."""

    var _service: ProjectsService
    var _channel: GrpcChannel

    alias MAX_LIMIT: Int = 100
    alias MAX_NAME_LENGTH: Int = 255

    fn __init__(inout self, channel: GrpcChannel) raises:
        """Create a new projects client."""
        self._channel = channel
        self._service = ProjectsService(channel)

    fn list_projects(self) -> Result[ListProjectsResponse]:
        """List projects with default pagination."""
        return self.list_projects_with_options(PaginationOptions())

    fn list_projects_with_options(self, options: PaginationOptions) -> Result[ListProjectsResponse]:
        """List projects with pagination options."""
        return self.list_projects_for_tenant(options, "")

    fn list_projects_for_tenant(
        self,
        options: PaginationOptions,
        tenant_id: String,
    ) -> Result[ListProjectsResponse]:
        """List projects for a specific tenant."""
        # Check if authenticated
        if not self._channel.has_auth_token():
            return Result[ListProjectsResponse].err(
                GreyError.unauthorized("Authentication required")
            )
        
        # Validate and cap limit
        var validated_limit = options.limit
        if validated_limit > Self.MAX_LIMIT:
            validated_limit = Self.MAX_LIMIT
        if validated_limit < 1:
            validated_limit = 10
        
        let request = ListProjectsRequest(validated_limit, options.offset, tenant_id)
        return self._service.list_projects(request)

    fn create_project(self, name: String, description: String = "") -> Result[Project]:
        """Create a new project."""
        return self.create_project_for_tenant(name, description, "")

    fn create_project_for_tenant(
        self,
        name: String,
        description: String,
        tenant_id: String,
    ) -> Result[Project]:
        """Create a new project for a specific tenant."""
        # Check if authenticated
        if not self._channel.has_auth_token():
            return Result[Project].err(
                GreyError.unauthorized("Authentication required")
            )
        
        # Validate name
        if len(name) == 0:
            return Result[Project].err(
                GreyError.validation_error("name", "Project name is required")
            )
        
        if len(name) > Self.MAX_NAME_LENGTH:
            return Result[Project].err(
                GreyError.validation_error("name", "Project name too long")
            )
        
        # Check for whitespace-only name
        var is_blank = True
        for i in range(len(name)):
            if name[i] != " " and name[i] != "\t" and name[i] != "\n":
                is_blank = False
                break
        
        if is_blank:
            return Result[Project].err(
                GreyError.validation_error("name", "Project name cannot be blank")
            )
        
        let request = CreateProjectRequest(name, description, tenant_id)
        return self._service.create_project(request)

    fn get_project(self, project_id: String) -> Result[Project]:
        """Get a project by ID."""
        # Check if authenticated
        if not self._channel.has_auth_token():
            return Result[Project].err(
                GreyError.unauthorized("Authentication required")
            )
        
        # Validate project ID
        if len(project_id) == 0:
            return Result[Project].err(
                GreyError.validation_error("project_id", "Project ID is required")
            )
        
        return self._service.get_project(project_id)
