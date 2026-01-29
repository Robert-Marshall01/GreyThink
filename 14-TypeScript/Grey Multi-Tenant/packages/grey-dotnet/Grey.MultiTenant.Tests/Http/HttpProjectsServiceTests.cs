using System.Net;
using System.Text;
using System.Text.Json;
using FluentAssertions;
using Grey.MultiTenant.Core.Models;
using Grey.MultiTenant.Core.Projects;
using Grey.MultiTenant.Http;
using Microsoft.Extensions.Options;
using Xunit;

namespace Grey.MultiTenant.Tests.Http;

public class HttpProjectsServiceTests
{
    private readonly GreyMultiTenantOptions _options;

    public HttpProjectsServiceTests()
    {
        _options = new GreyMultiTenantOptions { BaseUrl = "https://api.example.com" };
    }

    [Fact]
    public async Task ListProjectsAsync_ReturnsProjects()
    {
        // Arrange
        var response = new ProjectListResult
        {
            Projects = new List<ProjectModel>
            {
                new() { Id = "proj-1", Name = "Project 1", TenantId = "tenant-1", OwnerId = "user-1" },
                new() { Id = "proj-2", Name = "Project 2", TenantId = "tenant-1", OwnerId = "user-1" }
            },
            Total = 2,
            Page = 1,
            PageSize = 10
        };

        var handler = new MockHttpMessageHandler(async request =>
        {
            request.RequestUri!.PathAndQuery.Should().StartWith("/projects");
            request.Method.Should().Be(HttpMethod.Get);

            return new HttpResponseMessage(HttpStatusCode.OK)
            {
                Content = new StringContent(JsonSerializer.Serialize(response), Encoding.UTF8, "application/json")
            };
        });

        var httpClient = new HttpClient(handler) { BaseAddress = new Uri(_options.BaseUrl) };
        var service = new HttpProjectsService(httpClient, Options.Create(_options));

        // Act
        var result = await service.ListProjectsAsync();

        // Assert
        result.Projects.Should().HaveCount(2);
        result.Total.Should().Be(2);
    }

    [Fact]
    public async Task ListProjectsAsync_WithFilters_SendsQueryParams()
    {
        // Arrange
        var response = new ProjectListResult
        {
            Projects = new List<ProjectModel>(),
            Total = 0,
            Page = 2,
            PageSize = 5
        };

        string? capturedPath = null;
        var handler = new MockHttpMessageHandler(async request =>
        {
            capturedPath = request.RequestUri!.PathAndQuery;
            return new HttpResponseMessage(HttpStatusCode.OK)
            {
                Content = new StringContent(JsonSerializer.Serialize(response), Encoding.UTF8, "application/json")
            };
        });

        var httpClient = new HttpClient(handler) { BaseAddress = new Uri(_options.BaseUrl) };
        var service = new HttpProjectsService(httpClient, Options.Create(_options));

        // Act
        await service.ListProjectsAsync(new ProjectListRequest
        {
            Page = 2,
            PageSize = 5,
            Search = "test"
        });

        // Assert
        capturedPath.Should().Contain("page=2");
        capturedPath.Should().Contain("pageSize=5");
        capturedPath.Should().Contain("search=test");
    }

    [Fact]
    public async Task GetProjectAsync_ReturnsProject()
    {
        // Arrange
        var project = new ProjectModel
        {
            Id = "proj-123",
            Name = "Test Project",
            TenantId = "tenant-1",
            OwnerId = "user-1"
        };

        var handler = new MockHttpMessageHandler(async request =>
        {
            request.RequestUri!.PathAndQuery.Should().Be("/projects/proj-123");
            return new HttpResponseMessage(HttpStatusCode.OK)
            {
                Content = new StringContent(JsonSerializer.Serialize(project), Encoding.UTF8, "application/json")
            };
        });

        var httpClient = new HttpClient(handler) { BaseAddress = new Uri(_options.BaseUrl) };
        var service = new HttpProjectsService(httpClient, Options.Create(_options));

        // Act
        var result = await service.GetProjectAsync("proj-123");

        // Assert
        result.Id.Should().Be("proj-123");
        result.Name.Should().Be("Test Project");
    }

    [Fact]
    public async Task CreateProjectAsync_CreatesAndReturnsProject()
    {
        // Arrange
        var createdProject = new ProjectModel
        {
            Id = "proj-new",
            Name = "New Project",
            TenantId = "tenant-1",
            OwnerId = "user-1"
        };

        var handler = new MockHttpMessageHandler(async request =>
        {
            request.RequestUri!.PathAndQuery.Should().Be("/projects");
            request.Method.Should().Be(HttpMethod.Post);

            return new HttpResponseMessage(HttpStatusCode.Created)
            {
                Content = new StringContent(JsonSerializer.Serialize(createdProject), Encoding.UTF8, "application/json")
            };
        });

        var httpClient = new HttpClient(handler) { BaseAddress = new Uri(_options.BaseUrl) };
        var service = new HttpProjectsService(httpClient, Options.Create(_options));

        // Act
        var result = await service.CreateProjectAsync(new CreateProjectRequest { Name = "New Project" });

        // Assert
        result.Id.Should().Be("proj-new");
        result.Name.Should().Be("New Project");
    }

    [Fact]
    public async Task DeleteProjectAsync_DeletesProject()
    {
        // Arrange
        var handler = new MockHttpMessageHandler(async request =>
        {
            request.RequestUri!.PathAndQuery.Should().Be("/projects/proj-123");
            request.Method.Should().Be(HttpMethod.Delete);

            return new HttpResponseMessage(HttpStatusCode.NoContent);
        });

        var httpClient = new HttpClient(handler) { BaseAddress = new Uri(_options.BaseUrl) };
        var service = new HttpProjectsService(httpClient, Options.Create(_options));

        // Act
        await service.DeleteProjectAsync("proj-123");

        // Assert - No exception means success
    }
}
