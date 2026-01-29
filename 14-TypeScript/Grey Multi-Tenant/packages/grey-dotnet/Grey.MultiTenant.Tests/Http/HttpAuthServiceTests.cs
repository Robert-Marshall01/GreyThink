using System.Net;
using System.Text;
using System.Text.Json;
using FluentAssertions;
using Grey.MultiTenant.Core.Auth;
using Grey.MultiTenant.Core.Exceptions;
using Grey.MultiTenant.Core.Models;
using Grey.MultiTenant.Http;
using Microsoft.Extensions.Options;
using Xunit;

namespace Grey.MultiTenant.Tests.Http;

public class HttpAuthServiceTests
{
    private readonly MockTokenStorage _tokenStorage;
    private readonly GreyMultiTenantOptions _options;

    public HttpAuthServiceTests()
    {
        _tokenStorage = new MockTokenStorage();
        _options = new GreyMultiTenantOptions { BaseUrl = "https://api.example.com" };
    }

    [Fact]
    public async Task LoginAsync_WithValidCredentials_ReturnsLoginResult()
    {
        // Arrange
        var loginResponse = new LoginResult
        {
            User = new UserModel { Id = "user-1", Email = "test@example.com" },
            Tokens = new TokenModel { AccessToken = "access-token", RefreshToken = "refresh-token" }
        };

        var handler = new MockHttpMessageHandler(async request =>
        {
            request.RequestUri!.PathAndQuery.Should().Be("/auth/login");
            request.Method.Should().Be(HttpMethod.Post);

            return new HttpResponseMessage(HttpStatusCode.OK)
            {
                Content = new StringContent(JsonSerializer.Serialize(loginResponse), Encoding.UTF8, "application/json")
            };
        });

        var httpClient = new HttpClient(handler) { BaseAddress = new Uri(_options.BaseUrl) };
        var service = new HttpAuthService(httpClient, _tokenStorage, Options.Create(_options));

        // Act
        var result = await service.LoginAsync(new LoginRequest
        {
            Email = "test@example.com",
            Password = "password123"
        });

        // Assert
        result.Should().NotBeNull();
        result.User.Email.Should().Be("test@example.com");
        result.Tokens.AccessToken.Should().Be("access-token");
        _tokenStorage.AccessToken.Should().Be("access-token");
        _tokenStorage.RefreshToken.Should().Be("refresh-token");
    }

    [Fact]
    public async Task LoginAsync_WithInvalidCredentials_ThrowsAuthException()
    {
        // Arrange
        var handler = new MockHttpMessageHandler(async request =>
        {
            return new HttpResponseMessage(HttpStatusCode.Unauthorized)
            {
                Content = new StringContent(
                    JsonSerializer.Serialize(new { message = "Invalid credentials", code = "INVALID_CREDENTIALS" }),
                    Encoding.UTF8,
                    "application/json")
            };
        });

        var httpClient = new HttpClient(handler) { BaseAddress = new Uri(_options.BaseUrl) };
        var service = new HttpAuthService(httpClient, _tokenStorage, Options.Create(_options));

        // Act & Assert
        var exception = await Assert.ThrowsAsync<AuthException>(() =>
            service.LoginAsync(new LoginRequest
            {
                Email = "test@example.com",
                Password = "wrong-password"
            }));

        exception.ErrorType.Should().Be(AuthErrorType.InvalidCredentials);
    }

    [Fact]
    public async Task LogoutAsync_ClearsTokens()
    {
        // Arrange
        _tokenStorage.AccessToken = "access-token";
        _tokenStorage.RefreshToken = "refresh-token";

        var handler = new MockHttpMessageHandler(async request =>
        {
            return new HttpResponseMessage(HttpStatusCode.OK);
        });

        var httpClient = new HttpClient(handler) { BaseAddress = new Uri(_options.BaseUrl) };
        var service = new HttpAuthService(httpClient, _tokenStorage, Options.Create(_options));

        // Act
        await service.LogoutAsync();

        // Assert
        _tokenStorage.AccessToken.Should().BeNull();
        _tokenStorage.RefreshToken.Should().BeNull();
    }

    [Fact]
    public async Task RefreshAsync_WithValidRefreshToken_ReturnsNewTokens()
    {
        // Arrange
        _tokenStorage.RefreshToken = "old-refresh-token";

        var refreshResponse = new RefreshResult
        {
            Tokens = new TokenModel { AccessToken = "new-access-token", RefreshToken = "new-refresh-token" }
        };

        var handler = new MockHttpMessageHandler(async request =>
        {
            request.RequestUri!.PathAndQuery.Should().Be("/auth/refresh");
            return new HttpResponseMessage(HttpStatusCode.OK)
            {
                Content = new StringContent(JsonSerializer.Serialize(refreshResponse), Encoding.UTF8, "application/json")
            };
        });

        var httpClient = new HttpClient(handler) { BaseAddress = new Uri(_options.BaseUrl) };
        var service = new HttpAuthService(httpClient, _tokenStorage, Options.Create(_options));

        // Act
        var result = await service.RefreshAsync();

        // Assert
        result.Tokens.AccessToken.Should().Be("new-access-token");
        _tokenStorage.AccessToken.Should().Be("new-access-token");
        _tokenStorage.RefreshToken.Should().Be("new-refresh-token");
    }

    [Fact]
    public async Task RefreshAsync_WithoutRefreshToken_ThrowsAuthException()
    {
        // Arrange
        _tokenStorage.RefreshToken = null;

        var httpClient = new HttpClient() { BaseAddress = new Uri(_options.BaseUrl) };
        var service = new HttpAuthService(httpClient, _tokenStorage, Options.Create(_options));

        // Act & Assert
        var exception = await Assert.ThrowsAsync<AuthException>(() => service.RefreshAsync());
        exception.ErrorType.Should().Be(AuthErrorType.RefreshFailed);
    }

    [Fact]
    public async Task IsAuthenticatedAsync_WithValidToken_ReturnsTrue()
    {
        // Arrange - Create a valid JWT token that expires in the future
        var payload = Convert.ToBase64String(Encoding.UTF8.GetBytes(
            JsonSerializer.Serialize(new { exp = DateTimeOffset.UtcNow.AddHours(1).ToUnixTimeSeconds() })));
        _tokenStorage.AccessToken = $"header.{payload}.signature";

        var httpClient = new HttpClient() { BaseAddress = new Uri(_options.BaseUrl) };
        var service = new HttpAuthService(httpClient, _tokenStorage, Options.Create(_options));

        // Act
        var result = await service.IsAuthenticatedAsync();

        // Assert
        result.Should().BeTrue();
    }

    [Fact]
    public async Task IsAuthenticatedAsync_WithNoToken_ReturnsFalse()
    {
        // Arrange
        _tokenStorage.AccessToken = null;

        var httpClient = new HttpClient() { BaseAddress = new Uri(_options.BaseUrl) };
        var service = new HttpAuthService(httpClient, _tokenStorage, Options.Create(_options));

        // Act
        var result = await service.IsAuthenticatedAsync();

        // Assert
        result.Should().BeFalse();
    }
}
