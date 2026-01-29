namespace Grey.MultiTenant.Core.Abstractions;

/// <summary>
/// Interface for providing authentication tokens to HTTP clients.
/// </summary>
public interface ITokenProvider
{
    /// <summary>
    /// Gets the current access token for HTTP requests.
    /// </summary>
    Task<string?> GetTokenAsync(CancellationToken cancellationToken = default);

    /// <summary>
    /// Called when a token refresh is needed.
    /// </summary>
    Task<string?> RefreshTokenAsync(CancellationToken cancellationToken = default);

    /// <summary>
    /// Called when authentication is required (e.g., 401 response).
    /// </summary>
    event EventHandler? AuthenticationRequired;
}
