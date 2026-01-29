using System.Net;
using System.Text.Json;
using Grey.MultiTenant.Core.Abstractions;
using Grey.MultiTenant.Core.Auth;
using Grey.MultiTenant.Core.Models;
using Grey.MultiTenant.Http;

namespace Grey.MultiTenant.Tests.Http;

/// <summary>
/// Mock HTTP message handler for testing.
/// </summary>
public class MockHttpMessageHandler : HttpMessageHandler
{
    private readonly Func<HttpRequestMessage, Task<HttpResponseMessage>> _sendAsync;

    public MockHttpMessageHandler(Func<HttpRequestMessage, Task<HttpResponseMessage>> sendAsync)
    {
        _sendAsync = sendAsync;
    }

    protected override Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancellationToken)
    {
        return _sendAsync(request);
    }
}

/// <summary>
/// Mock token storage for testing.
/// </summary>
public class MockTokenStorage : ITokenStorage
{
    public string? AccessToken { get; set; }
    public string? RefreshToken { get; set; }

    public Task<string?> GetAccessTokenAsync(CancellationToken cancellationToken = default) => Task.FromResult(AccessToken);
    public Task SetAccessTokenAsync(string token, CancellationToken cancellationToken = default) { AccessToken = token; return Task.CompletedTask; }
    public Task ClearAccessTokenAsync(CancellationToken cancellationToken = default) { AccessToken = null; return Task.CompletedTask; }
    public Task<string?> GetRefreshTokenAsync(CancellationToken cancellationToken = default) => Task.FromResult(RefreshToken);
    public Task SetRefreshTokenAsync(string token, CancellationToken cancellationToken = default) { RefreshToken = token; return Task.CompletedTask; }
    public Task ClearRefreshTokenAsync(CancellationToken cancellationToken = default) { RefreshToken = null; return Task.CompletedTask; }
    public Task ClearAllAsync(CancellationToken cancellationToken = default) { AccessToken = null; RefreshToken = null; return Task.CompletedTask; }
    public Task<bool> HasTokensAsync(CancellationToken cancellationToken = default) => Task.FromResult(!string.IsNullOrEmpty(AccessToken));
}
