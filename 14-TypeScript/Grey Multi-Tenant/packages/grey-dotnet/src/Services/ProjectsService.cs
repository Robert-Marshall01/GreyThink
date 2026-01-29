// =============================================================================
// Grey Multi-Tenant SDK - .NET
// Services/ProjectsService.cs
//
// Projects service wrapping adapter-core functions.
// Exposes: Data, Loading, Error, and domain actions.
// =============================================================================

using System.Net.Http.Json;
using Microsoft.Extensions.Options;

namespace Grey.Sdk.Services;

/// <summary>
/// Project data model.
/// </summary>
public sealed class Project
{
    public string? Id { get; set; }
    public string? Name { get; set; }
    public string? Description { get; set; }
    public string? OwnerId { get; set; }
    public DateTime? CreatedAt { get; set; }
    public DateTime? UpdatedAt { get; set; }
}

/// <summary>
/// Pagination info for list responses.
/// </summary>
public sealed class Pagination
{
    public int Page { get; set; }
    public int PageSize { get; set; }
    public int TotalPages { get; set; }
    public int TotalItems { get; set; }
}

/// <summary>
/// Projects list response.
/// </summary>
public sealed class ProjectsData
{
    public List<Project> Projects { get; set; } = new();
    public Pagination? Pagination { get; set; }
}

/// <summary>
/// Create project request.
/// </summary>
public sealed class CreateProjectRequest
{
    public required string Name { get; set; }
    public string? Description { get; set; }
}

/// <summary>
/// Projects service for Grey Multi-Tenant SDK.
/// Thread-safe, async, DI-friendly.
/// </summary>
public sealed class ProjectsService
{
    private readonly HttpClient _httpClient;
    private readonly GreyOptions _options;
    private readonly object _lock = new();

    private ProjectsData? _data;
    private bool _loading;
    private GreyError? _error;

    /// <summary>
    /// Current projects data.
    /// </summary>
    public ProjectsData? Data
    {
        get { lock (_lock) return _data; }
        private set { lock (_lock) _data = value; }
    }

    /// <summary>
    /// Whether an operation is in progress.
    /// </summary>
    public bool Loading
    {
        get { lock (_lock) return _loading; }
        private set { lock (_lock) _loading = value; }
    }

    /// <summary>
    /// Last error from an operation.
    /// </summary>
    public GreyError? Error
    {
        get { lock (_lock) return _error; }
        private set { lock (_lock) _error = value; }
    }

    /// <summary>
    /// Creates a new ProjectsService.
    /// </summary>
    public ProjectsService(HttpClient httpClient, IOptions<GreyOptions> options)
    {
        _httpClient = httpClient ?? throw new ArgumentNullException(nameof(httpClient));
        _options = options?.Value ?? throw new ArgumentNullException(nameof(options));
    }

    /// <summary>
    /// List projects with pagination.
    /// </summary>
    public async Task<ProjectsData?> ListProjectsAsync(int page = 1, int pageSize = 20, CancellationToken cancellationToken = default)
    {
        Loading = true;
        Error = null;

        try
        {
            var response = await _httpClient.GetAsync(
                $"{_options.ApiUrl}/projects?page={page}&pageSize={pageSize}",
                cancellationToken
            );

            if (!response.IsSuccessStatusCode)
            {
                var errorContent = await response.Content.ReadAsStringAsync(cancellationToken);
                Error = new GreyError
                {
                    Message = $"List projects failed: {response.ReasonPhrase}",
                    Status = (int)response.StatusCode,
                    Raw = errorContent,
                };
                return null;
            }

            var data = await response.Content.ReadFromJsonAsync<ProjectsData>(cancellationToken: cancellationToken);
            Data = data;
            return data;
        }
        catch (Exception ex)
        {
            Error = GreyError.FromException(ex);
            return null;
        }
        finally
        {
            Loading = false;
        }
    }

    /// <summary>
    /// Create a new project.
    /// </summary>
    public async Task<Project?> CreateProjectAsync(CreateProjectRequest request, CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(request);

        Loading = true;
        Error = null;

        try
        {
            var response = await _httpClient.PostAsJsonAsync(
                $"{_options.ApiUrl}/projects",
                request,
                cancellationToken
            );

            if (!response.IsSuccessStatusCode)
            {
                var errorContent = await response.Content.ReadAsStringAsync(cancellationToken);
                Error = new GreyError
                {
                    Message = $"Create project failed: {response.ReasonPhrase}",
                    Status = (int)response.StatusCode,
                    Raw = errorContent,
                };
                return null;
            }

            var project = await response.Content.ReadFromJsonAsync<Project>(cancellationToken: cancellationToken);
            return project;
        }
        catch (Exception ex)
        {
            Error = GreyError.FromException(ex);
            return null;
        }
        finally
        {
            Loading = false;
        }
    }
}
