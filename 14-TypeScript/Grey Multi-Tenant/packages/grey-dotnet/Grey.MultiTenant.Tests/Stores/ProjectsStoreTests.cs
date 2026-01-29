using FluentAssertions;
using Grey.MultiTenant.Blazor.Stores;
using Grey.MultiTenant.Core.Abstractions;
using Grey.MultiTenant.Core.Models;
using Grey.MultiTenant.Core.Projects;
using Moq;
using Xunit;

namespace Grey.MultiTenant.Tests.Stores;

public class ProjectsStoreTests
{
    private readonly Mock<IProjectsService> _projectsServiceMock;
    private readonly ProjectsStore _store;

    public ProjectsStoreTests()
    {
        _projectsServiceMock = new Mock<IProjectsService>();
        _store = new ProjectsStore(_projectsServiceMock.Object);
    }

    [Fact]
    public async Task FetchProjectsAsync_LoadsProjects()
    {
        // Arrange
        var projects = new List<ProjectModel>
        {
            new() { Id = "proj-1", Name = "Project 1", TenantId = "t1", OwnerId = "u1" },
            new() { Id = "proj-2", Name = "Project 2", TenantId = "t1", OwnerId = "u1" }
        };

        _projectsServiceMock
            .Setup(x => x.ListProjectsAsync(It.IsAny<ProjectListRequest>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(new ProjectListResult { Projects = projects, Total = 2 });

        // Act
        await _store.FetchProjectsAsync();

        // Assert
        _store.Projects.Should().HaveCount(2);
        _store.Total.Should().Be(2);
        _store.Error.Should().BeNull();
    }

    [Fact]
    public async Task CreateProjectAsync_AddsProjectToList()
    {
        // Arrange
        var newProject = new ProjectModel { Id = "proj-new", Name = "New Project", TenantId = "t1", OwnerId = "u1" };

        _projectsServiceMock
            .Setup(x => x.CreateProjectAsync(It.IsAny<CreateProjectRequest>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(newProject);

        // Initial state
        _store.Projects.Should().BeEmpty();

        // Act
        var result = await _store.CreateProjectAsync(new CreateProjectRequest { Name = "New Project" });

        // Assert
        result.Should().NotBeNull();
        result!.Id.Should().Be("proj-new");
        _store.Projects.Should().Contain(p => p.Id == "proj-new");
        _store.Total.Should().Be(1);
    }

    [Fact]
    public async Task UpdateProjectAsync_UpdatesProjectInList()
    {
        // Arrange - First load some projects
        var initialProjects = new List<ProjectModel>
        {
            new() { Id = "proj-1", Name = "Original Name", TenantId = "t1", OwnerId = "u1" }
        };

        _projectsServiceMock
            .Setup(x => x.ListProjectsAsync(It.IsAny<ProjectListRequest>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(new ProjectListResult { Projects = initialProjects, Total = 1 });

        await _store.FetchProjectsAsync();

        // Setup update
        var updatedProject = new ProjectModel { Id = "proj-1", Name = "Updated Name", TenantId = "t1", OwnerId = "u1" };
        _projectsServiceMock
            .Setup(x => x.UpdateProjectAsync(It.IsAny<UpdateProjectRequest>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(updatedProject);

        // Act
        var result = await _store.UpdateProjectAsync(new UpdateProjectRequest { ProjectId = "proj-1", Name = "Updated Name" });

        // Assert
        result.Should().NotBeNull();
        _store.Projects.Single().Name.Should().Be("Updated Name");
    }

    [Fact]
    public async Task DeleteProjectAsync_RemovesProjectFromList()
    {
        // Arrange - First load some projects
        var initialProjects = new List<ProjectModel>
        {
            new() { Id = "proj-1", Name = "Project 1", TenantId = "t1", OwnerId = "u1" },
            new() { Id = "proj-2", Name = "Project 2", TenantId = "t1", OwnerId = "u1" }
        };

        _projectsServiceMock
            .Setup(x => x.ListProjectsAsync(It.IsAny<ProjectListRequest>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(new ProjectListResult { Projects = initialProjects, Total = 2 });

        await _store.FetchProjectsAsync();

        _projectsServiceMock
            .Setup(x => x.DeleteProjectAsync(It.IsAny<string>(), It.IsAny<CancellationToken>()))
            .Returns(Task.CompletedTask);

        // Act
        var result = await _store.DeleteProjectAsync("proj-1");

        // Assert
        result.Should().BeTrue();
        _store.Projects.Should().HaveCount(1);
        _store.Projects.Should().NotContain(p => p.Id == "proj-1");
        _store.Total.Should().Be(1);
    }

    [Fact]
    public async Task FetchProjectsAsync_RaisesProjectsChangedEvent()
    {
        // Arrange
        var projects = new List<ProjectModel>
        {
            new() { Id = "proj-1", Name = "Project 1", TenantId = "t1", OwnerId = "u1" }
        };

        _projectsServiceMock
            .Setup(x => x.ListProjectsAsync(It.IsAny<ProjectListRequest>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(new ProjectListResult { Projects = projects, Total = 1 });

        IReadOnlyList<ProjectModel>? eventProjects = null;
        _store.ProjectsChanged += (sender, p) => eventProjects = p;

        // Act
        await _store.FetchProjectsAsync();

        // Assert
        eventProjects.Should().NotBeNull();
        eventProjects.Should().HaveCount(1);
    }
}
