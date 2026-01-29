using System.Text.Json.Serialization;

namespace Grey.MultiTenant.Core.Projects;

/// <summary>
/// Request model for listing projects.
/// </summary>
public sealed record ProjectListRequest
{
    [JsonPropertyName("tenantId")]
    public string? TenantId { get; init; }

    [JsonPropertyName("page")]
    public int? Page { get; init; }

    [JsonPropertyName("pageSize")]
    public int? PageSize { get; init; }

    [JsonPropertyName("search")]
    public string? Search { get; init; }

    [JsonPropertyName("status")]
    public string? Status { get; init; }

    [JsonPropertyName("sortBy")]
    public string? SortBy { get; init; }

    [JsonPropertyName("sortOrder")]
    public string? SortOrder { get; init; }
}

/// <summary>
/// Result model for project list.
/// </summary>
public sealed record ProjectListResult
{
    [JsonPropertyName("projects")]
    public required IReadOnlyList<Models.ProjectModel> Projects { get; init; }

    [JsonPropertyName("total")]
    public int Total { get; init; }

    [JsonPropertyName("page")]
    public int Page { get; init; }

    [JsonPropertyName("pageSize")]
    public int PageSize { get; init; }

    [JsonPropertyName("hasMore")]
    public bool HasMore { get; init; }
}

/// <summary>
/// Request model for creating a project.
/// </summary>
public sealed record CreateProjectRequest
{
    [JsonPropertyName("name")]
    public required string Name { get; init; }

    [JsonPropertyName("description")]
    public string? Description { get; init; }

    [JsonPropertyName("tenantId")]
    public string? TenantId { get; init; }

    [JsonPropertyName("metadata")]
    public Dictionary<string, object>? Metadata { get; init; }
}

/// <summary>
/// Request model for updating a project.
/// </summary>
public sealed record UpdateProjectRequest
{
    [JsonPropertyName("projectId")]
    public required string ProjectId { get; init; }

    [JsonPropertyName("name")]
    public string? Name { get; init; }

    [JsonPropertyName("description")]
    public string? Description { get; init; }

    [JsonPropertyName("status")]
    public string? Status { get; init; }

    [JsonPropertyName("metadata")]
    public Dictionary<string, object>? Metadata { get; init; }
}

/// <summary>
/// Request model for deleting a project.
/// </summary>
public sealed record DeleteProjectRequest
{
    [JsonPropertyName("projectId")]
    public required string ProjectId { get; init; }
}

/// <summary>
/// Represents the current projects state.
/// </summary>
public sealed record ProjectsState
{
    public IReadOnlyList<Models.ProjectModel> Projects { get; init; } = Array.Empty<Models.ProjectModel>();
    public int Total { get; init; }
    public bool IsLoading { get; init; }
    public string? Error { get; init; }
}
