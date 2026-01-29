// =============================================================================
// Grey Multi-Tenant SDK - .NET
// Services/UserService.cs
//
// User service wrapping adapter-core functions.
// Exposes: Data, Loading, Error, and domain actions.
// =============================================================================

using System.Net.Http.Json;
using Microsoft.Extensions.Options;

namespace Grey.Sdk.Services;

/// <summary>
/// User data model.
/// </summary>
public sealed class User
{
    public string? Id { get; set; }
    public string? Email { get; set; }
    public string? Name { get; set; }
    public string? AvatarUrl { get; set; }
    public DateTime? CreatedAt { get; set; }
    public DateTime? UpdatedAt { get; set; }
}

/// <summary>
/// User service for Grey Multi-Tenant SDK.
/// Thread-safe, async, DI-friendly.
/// </summary>
public sealed class UserService
{
    private readonly HttpClient _httpClient;
    private readonly GreyOptions _options;
    private readonly object _lock = new();

    private User? _data;
    private bool _loading;
    private GreyError? _error;

    /// <summary>
    /// Current user data.
    /// </summary>
    public User? Data
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
    /// Creates a new UserService.
    /// </summary>
    public UserService(HttpClient httpClient, IOptions<GreyOptions> options)
    {
        _httpClient = httpClient ?? throw new ArgumentNullException(nameof(httpClient));
        _options = options?.Value ?? throw new ArgumentNullException(nameof(options));
    }

    /// <summary>
    /// Fetch the current user.
    /// </summary>
    public async Task<User?> FetchUserAsync(CancellationToken cancellationToken = default)
    {
        Loading = true;
        Error = null;

        try
        {
            var response = await _httpClient.GetAsync(
                $"{_options.ApiUrl}/users/me",
                cancellationToken
            );

            if (!response.IsSuccessStatusCode)
            {
                var errorContent = await response.Content.ReadAsStringAsync(cancellationToken);
                Error = new GreyError
                {
                    Message = $"Fetch user failed: {response.ReasonPhrase}",
                    Status = (int)response.StatusCode,
                    Raw = errorContent,
                };
                return null;
            }

            var data = await response.Content.ReadFromJsonAsync<User>(cancellationToken: cancellationToken);
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
