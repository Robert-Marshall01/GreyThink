using Grey.MultiTenant.Core.Auth;
using Grey.MultiTenant.Core.Models;

namespace Grey.MultiTenant.Core.Abstractions;

/// <summary>
/// Service interface for authentication operations.
/// </summary>
public interface IAuthService
{
    /// <summary>
    /// Authenticates a user with credentials.
    /// </summary>
    Task<LoginResult> LoginAsync(LoginRequest request, CancellationToken cancellationToken = default);

    /// <summary>
    /// Logs out the current user.
    /// </summary>
    Task LogoutAsync(CancellationToken cancellationToken = default);

    /// <summary>
    /// Refreshes the current access token.
    /// </summary>
    Task<RefreshResult> RefreshAsync(CancellationToken cancellationToken = default);

    /// <summary>
    /// Refreshes the access token using a specific refresh token.
    /// </summary>
    Task<RefreshResult> RefreshAsync(string refreshToken, CancellationToken cancellationToken = default);

    /// <summary>
    /// Registers a new user.
    /// </summary>
    Task<LoginResult> RegisterAsync(RegisterRequest request, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets the current access token.
    /// </summary>
    Task<string?> GetAccessTokenAsync(CancellationToken cancellationToken = default);

    /// <summary>
    /// Checks if the current session is valid.
    /// </summary>
    Task<bool> IsAuthenticatedAsync(CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets the current token claims.
    /// </summary>
    Task<TokenClaims?> GetTokenClaimsAsync(CancellationToken cancellationToken = default);
}
