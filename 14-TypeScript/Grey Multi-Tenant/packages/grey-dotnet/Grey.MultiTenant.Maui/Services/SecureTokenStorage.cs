using Grey.MultiTenant.Core.Abstractions;

namespace Grey.MultiTenant.Maui.Services;

/// <summary>
/// Secure token storage implementation using MAUI SecureStorage.
/// Provides encrypted storage for access and refresh tokens on mobile devices.
/// </summary>
public sealed class SecureTokenStorage : ITokenStorage
{
    private const string AccessTokenKey = "grey_access_token";
    private const string RefreshTokenKey = "grey_refresh_token";
    private readonly SemaphoreSlim _lock = new(1, 1);

    /// <summary>
    /// Gets the stored access token from secure storage.
    /// </summary>
    public async Task<string?> GetAccessTokenAsync(CancellationToken cancellationToken = default)
    {
        await _lock.WaitAsync(cancellationToken);
        try
        {
            return await GetSecureValueAsync(AccessTokenKey);
        }
        finally
        {
            _lock.Release();
        }
    }

    /// <summary>
    /// Stores the access token in secure storage.
    /// </summary>
    public async Task SetAccessTokenAsync(string token, CancellationToken cancellationToken = default)
    {
        await _lock.WaitAsync(cancellationToken);
        try
        {
            await SetSecureValueAsync(AccessTokenKey, token);
        }
        finally
        {
            _lock.Release();
        }
    }

    /// <summary>
    /// Removes the access token from secure storage.
    /// </summary>
    public async Task ClearAccessTokenAsync(CancellationToken cancellationToken = default)
    {
        await _lock.WaitAsync(cancellationToken);
        try
        {
            RemoveSecureValue(AccessTokenKey);
            await Task.CompletedTask;
        }
        finally
        {
            _lock.Release();
        }
    }

    /// <summary>
    /// Gets the stored refresh token from secure storage.
    /// </summary>
    public async Task<string?> GetRefreshTokenAsync(CancellationToken cancellationToken = default)
    {
        await _lock.WaitAsync(cancellationToken);
        try
        {
            return await GetSecureValueAsync(RefreshTokenKey);
        }
        finally
        {
            _lock.Release();
        }
    }

    /// <summary>
    /// Stores the refresh token in secure storage.
    /// </summary>
    public async Task SetRefreshTokenAsync(string token, CancellationToken cancellationToken = default)
    {
        await _lock.WaitAsync(cancellationToken);
        try
        {
            await SetSecureValueAsync(RefreshTokenKey, token);
        }
        finally
        {
            _lock.Release();
        }
    }

    /// <summary>
    /// Removes the refresh token from secure storage.
    /// </summary>
    public async Task ClearRefreshTokenAsync(CancellationToken cancellationToken = default)
    {
        await _lock.WaitAsync(cancellationToken);
        try
        {
            RemoveSecureValue(RefreshTokenKey);
            await Task.CompletedTask;
        }
        finally
        {
            _lock.Release();
        }
    }

    /// <summary>
    /// Removes all tokens from secure storage.
    /// </summary>
    public async Task ClearAllAsync(CancellationToken cancellationToken = default)
    {
        await _lock.WaitAsync(cancellationToken);
        try
        {
            RemoveSecureValue(AccessTokenKey);
            RemoveSecureValue(RefreshTokenKey);
            await Task.CompletedTask;
        }
        finally
        {
            _lock.Release();
        }
    }

    /// <summary>
    /// Checks if tokens are currently stored.
    /// </summary>
    public async Task<bool> HasTokensAsync(CancellationToken cancellationToken = default)
    {
        var token = await GetAccessTokenAsync(cancellationToken);
        return !string.IsNullOrEmpty(token);
    }

    private static async Task<string?> GetSecureValueAsync(string key)
    {
        try
        {
            return await Microsoft.Maui.Storage.SecureStorage.Default.GetAsync(key);
        }
        catch (Exception)
        {
            // SecureStorage may throw if not available or if there's an error
            // Return null in these cases
            return null;
        }
    }

    private static async Task SetSecureValueAsync(string key, string value)
    {
        try
        {
            await Microsoft.Maui.Storage.SecureStorage.Default.SetAsync(key, value);
        }
        catch (Exception)
        {
            // SecureStorage may throw if not available
            // Fail silently - consider logging in production
        }
    }

    private static void RemoveSecureValue(string key)
    {
        try
        {
            Microsoft.Maui.Storage.SecureStorage.Default.Remove(key);
        }
        catch (Exception)
        {
            // SecureStorage may throw if not available
            // Fail silently - consider logging in production
        }
    }
}
