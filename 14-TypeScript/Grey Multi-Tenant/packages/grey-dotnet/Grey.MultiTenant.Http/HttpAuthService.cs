using System.Net.Http.Json;
using System.Text;
using System.Text.Json;
using Grey.MultiTenant.Core.Abstractions;
using Grey.MultiTenant.Core.Auth;
using Grey.MultiTenant.Core.Exceptions;
using Grey.MultiTenant.Core.Models;
using Microsoft.Extensions.Options;

namespace Grey.MultiTenant.Http;

/// <summary>
/// HTTP-based implementation of the authentication service.
/// </summary>
public sealed class HttpAuthService : IAuthService
{
    private readonly HttpClient _httpClient;
    private readonly ITokenStorage _tokenStorage;
    private readonly GreyMultiTenantOptions _options;
    private readonly JsonSerializerOptions _jsonOptions;

    public HttpAuthService(
        HttpClient httpClient,
        ITokenStorage tokenStorage,
        IOptions<GreyMultiTenantOptions> options)
    {
        _httpClient = httpClient;
        _tokenStorage = tokenStorage;
        _options = options.Value;
        _jsonOptions = new JsonSerializerOptions
        {
            PropertyNameCaseInsensitive = true,
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase
        };
    }

    public async Task<LoginResult> LoginAsync(LoginRequest request, CancellationToken cancellationToken = default)
    {
        try
        {
            var content = new StringContent(
                JsonSerializer.Serialize(request, _jsonOptions),
                Encoding.UTF8,
                "application/json");

            var response = await _httpClient.PostAsync("/auth/login", content, cancellationToken);
            await ErrorHandler.EnsureSuccessAsync(response, cancellationToken);

            var result = await response.Content.ReadFromJsonAsync<LoginResult>(_jsonOptions, cancellationToken)
                ?? throw new ApiException("Invalid login response");

            // Store tokens
            await _tokenStorage.SetAccessTokenAsync(result.Tokens.AccessToken, cancellationToken);
            if (result.Tokens.RefreshToken != null)
            {
                await _tokenStorage.SetRefreshTokenAsync(result.Tokens.RefreshToken, cancellationToken);
            }

            return result;
        }
        catch (Exception ex) when (ex is not ApiException and not AuthException)
        {
            throw ErrorHandler.WrapException(ex);
        }
    }

    public async Task LogoutAsync(CancellationToken cancellationToken = default)
    {
        try
        {
            // Try to call logout endpoint
            try
            {
                var response = await _httpClient.PostAsync("/auth/logout", null, cancellationToken);
                // Don't throw on logout failure - continue to clear tokens
            }
            catch
            {
                // Ignore logout API errors
            }

            // Always clear local tokens
            await _tokenStorage.ClearAllAsync(cancellationToken);
        }
        catch (Exception ex) when (ex is not ApiException)
        {
            throw ErrorHandler.WrapException(ex);
        }
    }

    public async Task<RefreshResult> RefreshAsync(CancellationToken cancellationToken = default)
    {
        var refreshToken = await _tokenStorage.GetRefreshTokenAsync(cancellationToken);
        if (string.IsNullOrEmpty(refreshToken))
        {
            throw new AuthException("No refresh token available", AuthErrorType.RefreshFailed);
        }

        return await RefreshAsync(refreshToken, cancellationToken);
    }

    public async Task<RefreshResult> RefreshAsync(string refreshToken, CancellationToken cancellationToken = default)
    {
        try
        {
            var request = new { refreshToken };
            var content = new StringContent(
                JsonSerializer.Serialize(request, _jsonOptions),
                Encoding.UTF8,
                "application/json");

            var response = await _httpClient.PostAsync("/auth/refresh", content, cancellationToken);
            await ErrorHandler.EnsureSuccessAsync(response, cancellationToken);

            var result = await response.Content.ReadFromJsonAsync<RefreshResult>(_jsonOptions, cancellationToken)
                ?? throw new ApiException("Invalid refresh response");

            // Store new tokens
            await _tokenStorage.SetAccessTokenAsync(result.Tokens.AccessToken, cancellationToken);
            if (result.Tokens.RefreshToken != null)
            {
                await _tokenStorage.SetRefreshTokenAsync(result.Tokens.RefreshToken, cancellationToken);
            }

            return result;
        }
        catch (Exception ex) when (ex is not ApiException and not AuthException)
        {
            throw ErrorHandler.WrapException(ex);
        }
    }

    public async Task<LoginResult> RegisterAsync(RegisterRequest request, CancellationToken cancellationToken = default)
    {
        try
        {
            var content = new StringContent(
                JsonSerializer.Serialize(request, _jsonOptions),
                Encoding.UTF8,
                "application/json");

            var response = await _httpClient.PostAsync("/auth/register", content, cancellationToken);
            await ErrorHandler.EnsureSuccessAsync(response, cancellationToken);

            var result = await response.Content.ReadFromJsonAsync<LoginResult>(_jsonOptions, cancellationToken)
                ?? throw new ApiException("Invalid register response");

            // Store tokens
            await _tokenStorage.SetAccessTokenAsync(result.Tokens.AccessToken, cancellationToken);
            if (result.Tokens.RefreshToken != null)
            {
                await _tokenStorage.SetRefreshTokenAsync(result.Tokens.RefreshToken, cancellationToken);
            }

            return result;
        }
        catch (Exception ex) when (ex is not ApiException and not AuthException and not ValidationException)
        {
            throw ErrorHandler.WrapException(ex);
        }
    }

    public async Task<string?> GetAccessTokenAsync(CancellationToken cancellationToken = default)
    {
        return await _tokenStorage.GetAccessTokenAsync(cancellationToken);
    }

    public async Task<bool> IsAuthenticatedAsync(CancellationToken cancellationToken = default)
    {
        var token = await _tokenStorage.GetAccessTokenAsync(cancellationToken);
        if (string.IsNullOrEmpty(token))
            return false;

        var claims = ParseTokenClaims(token);
        if (claims?.ExpiresAt == null)
            return true; // Assume valid if no expiry

        return claims.ExpiresAt > DateTimeOffset.UtcNow;
    }

    public Task<TokenClaims?> GetTokenClaimsAsync(CancellationToken cancellationToken = default)
    {
        return Task.Run(async () =>
        {
            var token = await _tokenStorage.GetAccessTokenAsync(cancellationToken);
            return string.IsNullOrEmpty(token) ? null : ParseTokenClaims(token);
        }, cancellationToken);
    }

    private static TokenClaims? ParseTokenClaims(string token)
    {
        try
        {
            var parts = token.Split('.');
            if (parts.Length != 3)
                return null;

            var payload = parts[1];
            // Add padding if needed
            switch (payload.Length % 4)
            {
                case 2: payload += "=="; break;
                case 3: payload += "="; break;
            }

            var json = Encoding.UTF8.GetString(Convert.FromBase64String(
                payload.Replace('-', '+').Replace('_', '/')));
            
            var claims = JsonSerializer.Deserialize<Dictionary<string, JsonElement>>(json);
            if (claims == null)
                return null;

            return new TokenClaims
            {
                Subject = claims.TryGetValue("sub", out var sub) ? sub.GetString() : null,
                Email = claims.TryGetValue("email", out var email) ? email.GetString() : null,
                TenantId = claims.TryGetValue("tenant_id", out var tenant) ? tenant.GetString() : null,
                Role = claims.TryGetValue("role", out var role) ? role.GetString() : null,
                IssuedAt = claims.TryGetValue("iat", out var iat) 
                    ? DateTimeOffset.FromUnixTimeSeconds(iat.GetInt64()) 
                    : null,
                ExpiresAt = claims.TryGetValue("exp", out var exp) 
                    ? DateTimeOffset.FromUnixTimeSeconds(exp.GetInt64()) 
                    : null
            };
        }
        catch
        {
            return null;
        }
    }
}
