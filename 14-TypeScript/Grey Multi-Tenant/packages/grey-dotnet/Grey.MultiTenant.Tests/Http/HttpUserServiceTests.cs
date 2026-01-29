using System.Net;
using System.Text;
using System.Text.Json;
using FluentAssertions;
using Grey.MultiTenant.Core.Exceptions;
using Grey.MultiTenant.Core.Models;
using Grey.MultiTenant.Core.User;
using Grey.MultiTenant.Http;
using Microsoft.Extensions.Options;
using Xunit;

namespace Grey.MultiTenant.Tests.Http;

public class HttpUserServiceTests
{
    private readonly GreyMultiTenantOptions _options;

    public HttpUserServiceTests()
    {
        _options = new GreyMultiTenantOptions { BaseUrl = "https://api.example.com" };
    }

    [Fact]
    public async Task GetUserAsync_ReturnsCurrentUser()
    {
        // Arrange
        var user = new UserModel
        {
            Id = "user-1",
            Email = "test@example.com",
            Name = "Test User"
        };

        var handler = new MockHttpMessageHandler(async request =>
        {
            request.RequestUri!.PathAndQuery.Should().Be("/users/me");
            request.Method.Should().Be(HttpMethod.Get);

            return new HttpResponseMessage(HttpStatusCode.OK)
            {
                Content = new StringContent(JsonSerializer.Serialize(user), Encoding.UTF8, "application/json")
            };
        });

        var httpClient = new HttpClient(handler) { BaseAddress = new Uri(_options.BaseUrl) };
        var service = new HttpUserService(httpClient, Options.Create(_options));

        // Act
        var result = await service.GetUserAsync();

        // Assert
        result.Should().NotBeNull();
        result.Id.Should().Be("user-1");
        result.Email.Should().Be("test@example.com");
    }

    [Fact]
    public async Task GetUserByIdAsync_ReturnsUser()
    {
        // Arrange
        var user = new UserModel
        {
            Id = "user-123",
            Email = "user123@example.com"
        };

        var handler = new MockHttpMessageHandler(async request =>
        {
            request.RequestUri!.PathAndQuery.Should().Be("/users/user-123");
            return new HttpResponseMessage(HttpStatusCode.OK)
            {
                Content = new StringContent(JsonSerializer.Serialize(user), Encoding.UTF8, "application/json")
            };
        });

        var httpClient = new HttpClient(handler) { BaseAddress = new Uri(_options.BaseUrl) };
        var service = new HttpUserService(httpClient, Options.Create(_options));

        // Act
        var result = await service.GetUserByIdAsync("user-123");

        // Assert
        result.Id.Should().Be("user-123");
    }

    [Fact]
    public async Task UpdateUserAsync_UpdatesAndReturnsUser()
    {
        // Arrange
        var updatedUser = new UserModel
        {
            Id = "user-1",
            Email = "test@example.com",
            Name = "Updated Name"
        };

        var handler = new MockHttpMessageHandler(async request =>
        {
            request.RequestUri!.PathAndQuery.Should().Be("/users/me");
            request.Method.Should().Be(HttpMethod.Patch);

            return new HttpResponseMessage(HttpStatusCode.OK)
            {
                Content = new StringContent(JsonSerializer.Serialize(updatedUser), Encoding.UTF8, "application/json")
            };
        });

        var httpClient = new HttpClient(handler) { BaseAddress = new Uri(_options.BaseUrl) };
        var service = new HttpUserService(httpClient, Options.Create(_options));

        // Act
        var result = await service.UpdateUserAsync(new UpdateUserRequest { Name = "Updated Name" });

        // Assert
        result.Name.Should().Be("Updated Name");
    }

    [Fact]
    public async Task GetUserAsync_WhenNotFound_ThrowsApiException()
    {
        // Arrange
        var handler = new MockHttpMessageHandler(async request =>
        {
            return new HttpResponseMessage(HttpStatusCode.NotFound)
            {
                Content = new StringContent(
                    JsonSerializer.Serialize(new { message = "User not found" }),
                    Encoding.UTF8,
                    "application/json")
            };
        });

        var httpClient = new HttpClient(handler) { BaseAddress = new Uri(_options.BaseUrl) };
        var service = new HttpUserService(httpClient, Options.Create(_options));

        // Act & Assert
        var exception = await Assert.ThrowsAsync<ApiException>(() => service.GetUserAsync());
        exception.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }
}
