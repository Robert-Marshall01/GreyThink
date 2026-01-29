' Grey SDK for Visual Basic - Projects Client
' Project operations: listProjects, createProject, etc.

Imports Grey.Errors
Imports Grey.Http

Namespace Grey.Domain

    ''' <summary>
    ''' Represents a project in the system.
    ''' </summary>
    Public Class Project
        ''' <summary>Gets or sets the project ID.</summary>
        Public Property Id As String

        ''' <summary>Gets or sets the project name.</summary>
        Public Property Name As String

        ''' <summary>Gets or sets the project description.</summary>
        Public Property Description As String

        ''' <summary>Gets or sets the owner user ID.</summary>
        Public Property OwnerId As String

        ''' <summary>Gets or sets the tenant ID.</summary>
        Public Property TenantId As String

        ''' <summary>Gets or sets when the project was created.</summary>
        Public Property CreatedAt As DateTime

        ''' <summary>Gets or sets when the project was last updated.</summary>
        Public Property UpdatedAt As DateTime

        ''' <summary>Gets or sets project metadata.</summary>
        Public Property Metadata As Dictionary(Of String, Object)
    End Class

    ''' <summary>
    ''' Paginated list of projects.
    ''' </summary>
    Public Class ProjectList
        ''' <summary>Gets or sets the list of projects.</summary>
        Public Property Items As List(Of Project)

        ''' <summary>Gets or sets the total count.</summary>
        Public Property Total As Integer

        ''' <summary>Gets or sets the current page.</summary>
        Public Property Page As Integer

        ''' <summary>Gets or sets the page size.</summary>
        Public Property PerPage As Integer

        ''' <summary>Gets or sets whether there are more pages.</summary>
        Public Property HasMore As Boolean
    End Class

    ''' <summary>
    ''' Options for listing projects.
    ''' </summary>
    Public Class ListProjectsOptions
        ''' <summary>Gets or sets the page number (1-based).</summary>
        Public Property Page As Integer = 1

        ''' <summary>Gets or sets the number of items per page.</summary>
        Public Property PerPage As Integer = 20

        ''' <summary>Gets or sets a search filter.</summary>
        Public Property Search As String

        ''' <summary>Gets or sets the field to sort by.</summary>
        Public Property SortBy As String

        ''' <summary>Gets or sets whether to sort descending.</summary>
        Public Property SortDescending As Boolean = False
    End Class

    ''' <summary>
    ''' Data for creating a project.
    ''' </summary>
    Public Class CreateProjectData
        ''' <summary>Gets or sets the project name.</summary>
        Public Property Name As String

        ''' <summary>Gets or sets the project description.</summary>
        Public Property Description As String

        ''' <summary>Gets or sets project metadata.</summary>
        Public Property Metadata As Dictionary(Of String, Object)
    End Class

    ''' <summary>
    ''' Data for updating a project.
    ''' </summary>
    Public Class UpdateProjectData
        ''' <summary>Gets or sets the project name.</summary>
        Public Property Name As String

        ''' <summary>Gets or sets the project description.</summary>
        Public Property Description As String

        ''' <summary>Gets or sets project metadata.</summary>
        Public Property Metadata As Dictionary(Of String, Object)
    End Class

    ''' <summary>
    ''' Client for project operations.
    ''' </summary>
    Public Class ProjectsClient

        Private ReadOnly _httpClient As GreyHttpClient

        ''' <summary>
        ''' Creates a new ProjectsClient instance.
        ''' </summary>
        ''' <param name="httpClient">The HTTP client to use.</param>
        Public Sub New(httpClient As GreyHttpClient)
            _httpClient = httpClient
        End Sub

        ''' <summary>
        ''' Lists projects with optional filtering and pagination.
        ''' </summary>
        ''' <param name="options">List options.</param>
        ''' <returns>Result containing the project list on success.</returns>
        Public Async Function ListProjectsAsync(Optional options As ListProjectsOptions = Nothing) As Task(Of Result(Of ProjectList))
            If options Is Nothing Then
                options = New ListProjectsOptions()
            End If

            ' Validate pagination
            If options.Page < 1 Then
                Return Result(Of ProjectList).Fail(
                    GreyError.ValidationError("Page must be at least 1"))
            End If

            If options.PerPage < 1 OrElse options.PerPage > 100 Then
                Return Result(Of ProjectList).Fail(
                    GreyError.ValidationError("PerPage must be between 1 and 100"))
            End If

            ' Build query string
            Dim queryParams As New List(Of String)()
            queryParams.Add($"page={options.Page}")
            queryParams.Add($"perPage={options.PerPage}")

            If Not String.IsNullOrWhiteSpace(options.Search) Then
                queryParams.Add($"search={Uri.EscapeDataString(options.Search)}")
            End If

            If Not String.IsNullOrWhiteSpace(options.SortBy) Then
                queryParams.Add($"sortBy={Uri.EscapeDataString(options.SortBy)}")
                queryParams.Add($"sortDesc={options.SortDescending.ToString().ToLower()}")
            End If

            Dim queryString = String.Join("&", queryParams)
            Return Await _httpClient.GetAsync(Of ProjectList)($"/api/projects?{queryString}")
        End Function

        ''' <summary>
        ''' Gets a project by ID.
        ''' </summary>
        ''' <param name="projectId">The project ID.</param>
        ''' <returns>Result containing the project on success.</returns>
        Public Async Function GetProjectAsync(projectId As String) As Task(Of Result(Of Project))
            If String.IsNullOrWhiteSpace(projectId) Then
                Return Result(Of Project).Fail(
                    GreyError.ValidationError("Project ID is required"))
            End If

            Return Await _httpClient.GetAsync(Of Project)($"/api/projects/{Uri.EscapeDataString(projectId)}")
        End Function

        ''' <summary>
        ''' Creates a new project.
        ''' </summary>
        ''' <param name="data">The project data.</param>
        ''' <returns>Result containing the created project on success.</returns>
        Public Async Function CreateProjectAsync(data As CreateProjectData) As Task(Of Result(Of Project))
            If data Is Nothing Then
                Return Result(Of Project).Fail(
                    GreyError.ValidationError("Project data is required"))
            End If

            If String.IsNullOrWhiteSpace(data.Name) Then
                Return Result(Of Project).Fail(
                    GreyError.ValidationError("Project name is required"))
            End If

            Return Await _httpClient.PostAsync(Of Project)("/api/projects", data)
        End Function

        ''' <summary>
        ''' Updates an existing project.
        ''' </summary>
        ''' <param name="projectId">The project ID.</param>
        ''' <param name="data">The update data.</param>
        ''' <returns>Result containing the updated project on success.</returns>
        Public Async Function UpdateProjectAsync(projectId As String, data As UpdateProjectData) As Task(Of Result(Of Project))
            If String.IsNullOrWhiteSpace(projectId) Then
                Return Result(Of Project).Fail(
                    GreyError.ValidationError("Project ID is required"))
            End If

            If data Is Nothing Then
                Return Result(Of Project).Fail(
                    GreyError.ValidationError("Update data is required"))
            End If

            Return Await _httpClient.PatchAsync(Of Project)($"/api/projects/{Uri.EscapeDataString(projectId)}", data)
        End Function

        ''' <summary>
        ''' Deletes a project.
        ''' </summary>
        ''' <param name="projectId">The project ID.</param>
        ''' <returns>Result indicating success or failure.</returns>
        Public Async Function DeleteProjectAsync(projectId As String) As Task(Of Result(Of Boolean))
            If String.IsNullOrWhiteSpace(projectId) Then
                Return Result(Of Boolean).Fail(
                    GreyError.ValidationError("Project ID is required"))
            End If

            Return Await _httpClient.DeleteAsync($"/api/projects/{Uri.EscapeDataString(projectId)}")
        End Function

    End Class

End Namespace
