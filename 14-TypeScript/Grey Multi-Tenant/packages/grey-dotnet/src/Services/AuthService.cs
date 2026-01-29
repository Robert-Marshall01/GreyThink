// =============================================================================
// Grey Multi-Tenant SDK - .NET
// Services/AuthService.cs
//
// Authentication service wrapping adapter-core functions.
// Exposes: Data, Loading, Error, and domain actions.
// =============================================================================

using System.Net.Http.Json;
using System.Text.Json;
using Microsoft.Extensions.Options;

namespace Grey.Sdk.Services;

/// <summary>
/// Authentication data returned from login/refresh operations.
/// </summary>
public sealed class AuthData
{
    public string? AccessToken { get; set; }
    public string? RefreshToken { get; set; }
    public DateTime? ExpiresAt { get; set; }
    public UserData? User { get; set; }
}

/// <summary>
/// User data embedded in auth responses.
/// </summary>
public sealed class UserData
{
    public string? Id { get; set; }
    public string? Email { get; set; }
    public string? Name { get; set; }
}

/// <summary>
/// Login request parameters.
/// </summary>
public sealed class LoginRequest
{
    public required string Email { get; set; }
    public required string Password { get; set; }
}

/// <summary>
/// Refresh request parameters.
/// </summary>
public sealed class RefreshRequest
{
    public required string RefreshToken { get; set; }
}

/// <summary>
/// Authentication service for Grey Multi-Tenant SDK.
/// Thread-safe, async, DI-friendly.
/// </summary>
public sealed class AuthService
{
    private readonly HttpClient _httpClient;
    private readonly GreyOptions _options;
    private readonly object _lock = new();

    private AuthData? _data;
    private bool _loading;
    private GreyError? _error;

    /// <summary>
    /// Current authentication data.
    /// </summary>
    public AuthData? Data
    {
        get { lock (_lock) return _data; }
        private set { lock (_lock) _data = value; }
    }

    /// <summary>
    /// Whether an operation is in progress.
    /// </summary>
    public bool Loading
    {
        get { lock (_lock) return _loading; }
        private set { lock (_lock) _loading = value; }
    }

    /// <summary>
    /// Last error from an operation.
    /// </summary>
    public GreyError? Error
    {
        get { lock (_lock) return _error; }
        private set { lock (_lock) _error = value; }
    }

    /// <summary>
    /// Creates a new AuthService.
    /// </summary>
    public AuthService(HttpClient httpClient, IOptions<GreyOptions> options)
    {
        _httpClient = httpClient ?? throw new ArgumentNullException(nameof(httpClient));
        _options = options?.Value ?? throw new ArgumentNullException(nameof(options));
    }

    /// <summary>
    /// Authenticate with email and password.
    /// </summary>
    public async Task<AuthData?> LoginAsync(LoginRequest request, CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(request);

        Loading = true;
        Error = null;

        try
        {
            var response = await _httpClient.PostAsJsonAsync(
                $"{_options.ApiUrl}/auth/login",
                request,
                cancellationToken
            );

            if (!response.IsSuccessStatusCode)
            {
                var errorContent = await response.Content.ReadAsStringAsync(cancellationToken);
                Error = new GreyError
                {
                    Message = $"Login failed: {response.ReasonPhrase}",
                    Status = (int)response.StatusCode,
                    Raw = errorContent,
                };
                return null;
            }

            var data = await response.Content.ReadFromJsonAsync<AuthData>(cancellationToken: cancellationToken);
            Data = data;
            return data;
        }
        catch (Exception ex)
        {
            Error = GreyError.FromException(ex);
            return null;
        }
        finally
        {
            Loading = false;
        }
    }

    /// <summary>
    /// Log out the current user.
    /// </summary>
    public async Task LogoutAsync(CancellationToken cancellationToken = default)
    {
        Loading = true;
        Error = null;

        try
        {
            var response = await _httpClient.PostAsync(
                $"{_options.ApiUrl}/auth/logout",
                null,
                cancellationToken
            );

            if (!response.IsSuccessStatusCode)
            {
                var errorContent = await response.Content.ReadAsStringAsync(cancellationToken);
                Error = new GreyError
                {
                    Message = $"Logout failed: {response.ReasonPhrase}",
                    Status = (int)response.StatusCode,
                    Raw = errorContent,
                };
                return;
            }

            Data = null;
        }
        catch (Exception ex)
        {
            Error = GreyError.FromException(ex);
        }
        finally
        {
            Loading = false;
        }
    }

    /// <summary>
    /// Refresh the access token.
    /// </summary>
    public async Task<AuthData?> RefreshAsync(RefreshRequest request, CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(request);

        Loading = true;
        Error = null;

        try
        {
            var response = await _httpClient.PostAsJsonAsync(
                $"{_options.ApiUrl}/auth/refresh",
                request,
                cancellationToken
            );

            if (!response.IsSuccessStatusCode)
            {
                var errorContent = await response.Content.ReadAsStringAsync(cancellationToken);
                Error = new GreyError
                {
                    Message = $"Refresh failed: {response.ReasonPhrase}",
                    Status = (int)response.StatusCode,
                    Raw = errorContent,
                };
                return null;
            }

            var data = await response.Content.ReadFromJsonAsync<AuthData>(cancellationToken: cancellationToken);
            Data = data;
            return data;
        }
        catch (Exception ex)
        {
            Error = GreyError.FromException(ex);
            return null;
        }
        finally
        {
            Loading = false;
        }
    }
}
