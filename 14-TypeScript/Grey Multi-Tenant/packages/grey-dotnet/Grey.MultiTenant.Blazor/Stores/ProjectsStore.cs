using Grey.MultiTenant.Core.Abstractions;
using Grey.MultiTenant.Core.Models;
using Grey.MultiTenant.Core.Projects;

namespace Grey.MultiTenant.Blazor.Stores;

/// <summary>
/// Observable store for projects state in Blazor applications.
/// </summary>
public sealed class ProjectsStore : StoreBase, IDisposable
{
    private readonly IProjectsService _projectsService;
    private readonly SemaphoreSlim _operationLock = new(1, 1);

    private bool _isLoading;
    private IReadOnlyList<ProjectModel> _projects = Array.Empty<ProjectModel>();
    private int _total;
    private string? _error;

    public ProjectsStore(IProjectsService projectsService)
    {
        _projectsService = projectsService;
    }

    /// <summary>
    /// Whether a projects operation is in progress.
    /// </summary>
    public bool IsLoading
    {
        get => _isLoading;
        private set => SetProperty(ref _isLoading, value);
    }

    /// <summary>
    /// The current list of projects.
    /// </summary>
    public IReadOnlyList<ProjectModel> Projects
    {
        get => _projects;
        private set => SetProperty(ref _projects, value);
    }

    /// <summary>
    /// Total number of projects available.
    /// </summary>
    public int Total
    {
        get => _total;
        private set => SetProperty(ref _total, value);
    }

    /// <summary>
    /// The last error that occurred.
    /// </summary>
    public string? Error
    {
        get => _error;
        private set => SetProperty(ref _error, value);
    }

    /// <summary>
    /// Event raised when projects list changes.
    /// </summary>
    public event EventHandler<IReadOnlyList<ProjectModel>>? ProjectsChanged;

    /// <summary>
    /// Fetches projects with optional filtering.
    /// </summary>
    public async Task FetchProjectsAsync(ProjectListRequest? request = null)
    {
        await _operationLock.WaitAsync();
        try
        {
            IsLoading = true;
            Error = null;

            var result = await _projectsService.ListProjectsAsync(request);
            Projects = result.Projects;
            Total = result.Total;
            
            ProjectsChanged?.Invoke(this, Projects);
        }
        catch (Exception ex)
        {
            Error = NormalizeError(ex);
        }
        finally
        {
            IsLoading = false;
            _operationLock.Release();
        }
    }

    /// <summary>
    /// Gets a project by its ID.
    /// </summary>
    public async Task<ProjectModel?> GetProjectAsync(string projectId)
    {
        await _operationLock.WaitAsync();
        try
        {
            IsLoading = true;
            Error = null;

            return await _projectsService.GetProjectAsync(projectId);
        }
        catch (Exception ex)
        {
            Error = NormalizeError(ex);
            return null;
        }
        finally
        {
            IsLoading = false;
            _operationLock.Release();
        }
    }

    /// <summary>
    /// Creates a new project.
    /// </summary>
    public async Task<ProjectModel?> CreateProjectAsync(CreateProjectRequest request)
    {
        await _operationLock.WaitAsync();
        try
        {
            IsLoading = true;
            Error = null;

            var project = await _projectsService.CreateProjectAsync(request);
            
            // Add to local list
            Projects = new List<ProjectModel> { project }.Concat(Projects).ToList();
            Total++;
            
            ProjectsChanged?.Invoke(this, Projects);
            return project;
        }
        catch (Exception ex)
        {
            Error = NormalizeError(ex);
            return null;
        }
        finally
        {
            IsLoading = false;
            _operationLock.Release();
        }
    }

    /// <summary>
    /// Updates an existing project.
    /// </summary>
    public async Task<ProjectModel?> UpdateProjectAsync(UpdateProjectRequest request)
    {
        await _operationLock.WaitAsync();
        try
        {
            IsLoading = true;
            Error = null;

            var project = await _projectsService.UpdateProjectAsync(request);
            
            // Update in local list
            Projects = Projects.Select(p => p.Id == project.Id ? project : p).ToList();
            
            ProjectsChanged?.Invoke(this, Projects);
            return project;
        }
        catch (Exception ex)
        {
            Error = NormalizeError(ex);
            return null;
        }
        finally
        {
            IsLoading = false;
            _operationLock.Release();
        }
    }

    /// <summary>
    /// Deletes a project.
    /// </summary>
    public async Task<bool> DeleteProjectAsync(string projectId)
    {
        await _operationLock.WaitAsync();
        try
        {
            IsLoading = true;
            Error = null;

            await _projectsService.DeleteProjectAsync(projectId);
            
            // Remove from local list
            Projects = Projects.Where(p => p.Id != projectId).ToList();
            Total--;
            
            ProjectsChanged?.Invoke(this, Projects);
            return true;
        }
        catch (Exception ex)
        {
            Error = NormalizeError(ex);
            return false;
        }
        finally
        {
            IsLoading = false;
            _operationLock.Release();
        }
    }

    /// <summary>
    /// Clears any current error.
    /// </summary>
    public void ClearError()
    {
        Error = null;
    }

    public void Dispose()
    {
        _operationLock.Dispose();
    }
}
