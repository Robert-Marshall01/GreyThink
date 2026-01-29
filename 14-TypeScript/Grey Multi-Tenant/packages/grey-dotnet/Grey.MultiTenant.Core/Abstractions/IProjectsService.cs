using Grey.MultiTenant.Core.Models;
using Grey.MultiTenant.Core.Projects;

namespace Grey.MultiTenant.Core.Abstractions;

/// <summary>
/// Service interface for project operations.
/// </summary>
public interface IProjectsService
{
    /// <summary>
    /// Lists projects with optional filtering and pagination.
    /// </summary>
    Task<ProjectListResult> ListProjectsAsync(ProjectListRequest? request = null, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets a project by its ID.
    /// </summary>
    Task<ProjectModel> GetProjectAsync(string projectId, CancellationToken cancellationToken = default);

    /// <summary>
    /// Creates a new project.
    /// </summary>
    Task<ProjectModel> CreateProjectAsync(CreateProjectRequest request, CancellationToken cancellationToken = default);

    /// <summary>
    /// Updates an existing project.
    /// </summary>
    Task<ProjectModel> UpdateProjectAsync(UpdateProjectRequest request, CancellationToken cancellationToken = default);

    /// <summary>
    /// Deletes a project.
    /// </summary>
    Task DeleteProjectAsync(string projectId, CancellationToken cancellationToken = default);

    /// <summary>
    /// Deletes a project with full request options.
    /// </summary>
    Task DeleteProjectAsync(DeleteProjectRequest request, CancellationToken cancellationToken = default);
}
