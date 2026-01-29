using System.Net.Http.Json;
using System.Text;
using System.Text.Json;
using System.Web;
using Grey.MultiTenant.Core.Abstractions;
using Grey.MultiTenant.Core.Exceptions;
using Grey.MultiTenant.Core.Models;
using Grey.MultiTenant.Core.Projects;
using Microsoft.Extensions.Options;

namespace Grey.MultiTenant.Http;

/// <summary>
/// HTTP-based implementation of the projects service.
/// </summary>
public sealed class HttpProjectsService : IProjectsService
{
    private readonly HttpClient _httpClient;
    private readonly GreyMultiTenantOptions _options;
    private readonly JsonSerializerOptions _jsonOptions;

    public HttpProjectsService(
        HttpClient httpClient,
        IOptions<GreyMultiTenantOptions> options)
    {
        _httpClient = httpClient;
        _options = options.Value;
        _jsonOptions = new JsonSerializerOptions
        {
            PropertyNameCaseInsensitive = true,
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase
        };
    }

    public async Task<ProjectListResult> ListProjectsAsync(ProjectListRequest? request = null, CancellationToken cancellationToken = default)
    {
        try
        {
            var queryParams = new List<string>();
            
            if (request != null)
            {
                if (!string.IsNullOrEmpty(request.TenantId))
                    queryParams.Add($"tenantId={HttpUtility.UrlEncode(request.TenantId)}");
                if (request.Page.HasValue)
                    queryParams.Add($"page={request.Page}");
                if (request.PageSize.HasValue)
                    queryParams.Add($"pageSize={request.PageSize}");
                if (!string.IsNullOrEmpty(request.Search))
                    queryParams.Add($"search={HttpUtility.UrlEncode(request.Search)}");
                if (!string.IsNullOrEmpty(request.Status))
                    queryParams.Add($"status={HttpUtility.UrlEncode(request.Status)}");
                if (!string.IsNullOrEmpty(request.SortBy))
                    queryParams.Add($"sortBy={HttpUtility.UrlEncode(request.SortBy)}");
                if (!string.IsNullOrEmpty(request.SortOrder))
                    queryParams.Add($"sortOrder={HttpUtility.UrlEncode(request.SortOrder)}");
            }

            var url = "/projects" + (queryParams.Count > 0 ? "?" + string.Join("&", queryParams) : "");
            var response = await _httpClient.GetAsync(url, cancellationToken);
            await ErrorHandler.EnsureSuccessAsync(response, cancellationToken);

            return await response.Content.ReadFromJsonAsync<ProjectListResult>(_jsonOptions, cancellationToken)
                ?? throw new ApiException("Invalid projects list response");
        }
        catch (Exception ex) when (ex is not ApiException and not AuthException)
        {
            throw ErrorHandler.WrapException(ex);
        }
    }

    public async Task<ProjectModel> GetProjectAsync(string projectId, CancellationToken cancellationToken = default)
    {
        try
        {
            var response = await _httpClient.GetAsync($"/projects/{Uri.EscapeDataString(projectId)}", cancellationToken);
            await ErrorHandler.EnsureSuccessAsync(response, cancellationToken);

            return await response.Content.ReadFromJsonAsync<ProjectModel>(_jsonOptions, cancellationToken)
                ?? throw new ApiException("Invalid project response");
        }
        catch (Exception ex) when (ex is not ApiException and not AuthException)
        {
            throw ErrorHandler.WrapException(ex);
        }
    }

    public async Task<ProjectModel> CreateProjectAsync(CreateProjectRequest request, CancellationToken cancellationToken = default)
    {
        try
        {
            var content = new StringContent(
                JsonSerializer.Serialize(request, _jsonOptions),
                Encoding.UTF8,
                "application/json");

            var response = await _httpClient.PostAsync("/projects", content, cancellationToken);
            await ErrorHandler.EnsureSuccessAsync(response, cancellationToken);

            return await response.Content.ReadFromJsonAsync<ProjectModel>(_jsonOptions, cancellationToken)
                ?? throw new ApiException("Invalid project response");
        }
        catch (Exception ex) when (ex is not ApiException and not AuthException and not ValidationException)
        {
            throw ErrorHandler.WrapException(ex);
        }
    }

    public async Task<ProjectModel> UpdateProjectAsync(UpdateProjectRequest request, CancellationToken cancellationToken = default)
    {
        try
        {
            var content = new StringContent(
                JsonSerializer.Serialize(request, _jsonOptions),
                Encoding.UTF8,
                "application/json");

            var response = await _httpClient.PatchAsync($"/projects/{Uri.EscapeDataString(request.ProjectId)}", content, cancellationToken);
            await ErrorHandler.EnsureSuccessAsync(response, cancellationToken);

            return await response.Content.ReadFromJsonAsync<ProjectModel>(_jsonOptions, cancellationToken)
                ?? throw new ApiException("Invalid project response");
        }
        catch (Exception ex) when (ex is not ApiException and not AuthException and not ValidationException)
        {
            throw ErrorHandler.WrapException(ex);
        }
    }

    public async Task DeleteProjectAsync(string projectId, CancellationToken cancellationToken = default)
    {
        try
        {
            var response = await _httpClient.DeleteAsync($"/projects/{Uri.EscapeDataString(projectId)}", cancellationToken);
            await ErrorHandler.EnsureSuccessAsync(response, cancellationToken);
        }
        catch (Exception ex) when (ex is not ApiException and not AuthException)
        {
            throw ErrorHandler.WrapException(ex);
        }
    }

    public Task DeleteProjectAsync(DeleteProjectRequest request, CancellationToken cancellationToken = default)
    {
        return DeleteProjectAsync(request.ProjectId, cancellationToken);
    }
}
