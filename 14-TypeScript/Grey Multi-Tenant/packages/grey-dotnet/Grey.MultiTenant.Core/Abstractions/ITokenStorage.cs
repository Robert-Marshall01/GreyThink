namespace Grey.MultiTenant.Core.Abstractions;

/// <summary>
/// Interface for token storage operations.
/// </summary>
public interface ITokenStorage
{
    /// <summary>
    /// Gets the stored access token.
    /// </summary>
    Task<string?> GetAccessTokenAsync(CancellationToken cancellationToken = default);

    /// <summary>
    /// Sets the access token.
    /// </summary>
    Task SetAccessTokenAsync(string token, CancellationToken cancellationToken = default);

    /// <summary>
    /// Clears the access token.
    /// </summary>
    Task ClearAccessTokenAsync(CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets the stored refresh token.
    /// </summary>
    Task<string?> GetRefreshTokenAsync(CancellationToken cancellationToken = default);

    /// <summary>
    /// Sets the refresh token.
    /// </summary>
    Task SetRefreshTokenAsync(string token, CancellationToken cancellationToken = default);

    /// <summary>
    /// Clears the refresh token.
    /// </summary>
    Task ClearRefreshTokenAsync(CancellationToken cancellationToken = default);

    /// <summary>
    /// Clears all stored tokens.
    /// </summary>
    Task ClearAllAsync(CancellationToken cancellationToken = default);

    /// <summary>
    /// Checks if tokens are currently stored.
    /// </summary>
    Task<bool> HasTokensAsync(CancellationToken cancellationToken = default);
}
