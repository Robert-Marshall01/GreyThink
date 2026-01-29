using System.Net;
using System.Net.Http.Headers;
using Grey.MultiTenant.Core.Abstractions;
using Microsoft.Extensions.Options;

namespace Grey.MultiTenant.Http;

/// <summary>
/// Delegating handler that automatically injects authentication tokens into HTTP requests.
/// </summary>
public sealed class AuthenticatedHttpMessageHandler : DelegatingHandler
{
    private readonly ITokenStorage _tokenStorage;
    private readonly ITokenProvider? _tokenProvider;
    private readonly GreyMultiTenantOptions _options;
    private readonly SemaphoreSlim _refreshLock = new(1, 1);

    public AuthenticatedHttpMessageHandler(
        ITokenStorage tokenStorage,
        IOptions<GreyMultiTenantOptions> options,
        ITokenProvider? tokenProvider = null)
    {
        _tokenStorage = tokenStorage;
        _options = options.Value;
        _tokenProvider = tokenProvider;
    }

    protected override async Task<HttpResponseMessage> SendAsync(
        HttpRequestMessage request,
        CancellationToken cancellationToken)
    {
        // Skip auth for login/register endpoints
        var path = request.RequestUri?.PathAndQuery ?? "";
        if (IsAuthEndpoint(path))
        {
            return await base.SendAsync(request, cancellationToken);
        }

        // Add token if available
        var token = await GetTokenAsync(cancellationToken);
        if (!string.IsNullOrEmpty(token))
        {
            request.Headers.Authorization = new AuthenticationHeaderValue("Bearer", token);
        }

        // Add default headers
        foreach (var header in _options.DefaultHeaders)
        {
            if (!request.Headers.Contains(header.Key))
            {
                request.Headers.TryAddWithoutValidation(header.Key, header.Value);
            }
        }

        // Add tenant ID if configured
        if (!string.IsNullOrEmpty(_options.TenantId) && !request.Headers.Contains("X-Tenant-Id"))
        {
            request.Headers.TryAddWithoutValidation("X-Tenant-Id", _options.TenantId);
        }

        // Add API version if configured
        if (!string.IsNullOrEmpty(_options.ApiVersion) && !request.Headers.Contains("X-API-Version"))
        {
            request.Headers.TryAddWithoutValidation("X-API-Version", _options.ApiVersion);
        }

        var response = await base.SendAsync(request, cancellationToken);

        // Handle 401 with auto-refresh
        if (response.StatusCode == HttpStatusCode.Unauthorized && _options.AutoRefreshTokens && _tokenProvider != null)
        {
            var refreshedToken = await TryRefreshTokenAsync(cancellationToken);
            if (!string.IsNullOrEmpty(refreshedToken))
            {
                // Clone and retry the request with new token
                using var newRequest = await CloneRequestAsync(request, cancellationToken);
                newRequest.Headers.Authorization = new AuthenticationHeaderValue("Bearer", refreshedToken);
                
                response.Dispose();
                response = await base.SendAsync(newRequest, cancellationToken);
            }
        }

        return response;
    }

    private async Task<string?> GetTokenAsync(CancellationToken cancellationToken)
    {
        if (_tokenProvider != null)
        {
            return await _tokenProvider.GetTokenAsync(cancellationToken);
        }

        return await _tokenStorage.GetAccessTokenAsync(cancellationToken);
    }

    private async Task<string?> TryRefreshTokenAsync(CancellationToken cancellationToken)
    {
        if (_tokenProvider == null)
            return null;

        await _refreshLock.WaitAsync(cancellationToken);
        try
        {
            return await _tokenProvider.RefreshTokenAsync(cancellationToken);
        }
        catch
        {
            return null;
        }
        finally
        {
            _refreshLock.Release();
        }
    }

    private static bool IsAuthEndpoint(string path)
    {
        return path.Contains("/auth/login", StringComparison.OrdinalIgnoreCase)
            || path.Contains("/auth/register", StringComparison.OrdinalIgnoreCase)
            || path.Contains("/auth/refresh", StringComparison.OrdinalIgnoreCase);
    }

    private static async Task<HttpRequestMessage> CloneRequestAsync(
        HttpRequestMessage request,
        CancellationToken cancellationToken)
    {
        var clone = new HttpRequestMessage(request.Method, request.RequestUri);

        // Copy headers
        foreach (var header in request.Headers)
        {
            clone.Headers.TryAddWithoutValidation(header.Key, header.Value);
        }

        // Copy content if present
        if (request.Content != null)
        {
            var content = await request.Content.ReadAsByteArrayAsync(cancellationToken);
            clone.Content = new ByteArrayContent(content);
            
            foreach (var header in request.Content.Headers)
            {
                clone.Content.Headers.TryAddWithoutValidation(header.Key, header.Value);
            }
        }

        return clone;
    }
}
