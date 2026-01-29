using Grey.MultiTenant.Core.Abstractions;

namespace Grey.MultiTenant.Http;

/// <summary>
/// In-memory implementation of token storage.
/// Suitable for non-persistent scenarios or testing.
/// </summary>
public sealed class InMemoryTokenStorage : ITokenStorage
{
    private string? _accessToken;
    private string? _refreshToken;
    private readonly SemaphoreSlim _lock = new(1, 1);

    public async Task<string?> GetAccessTokenAsync(CancellationToken cancellationToken = default)
    {
        await _lock.WaitAsync(cancellationToken);
        try
        {
            return _accessToken;
        }
        finally
        {
            _lock.Release();
        }
    }

    public async Task SetAccessTokenAsync(string token, CancellationToken cancellationToken = default)
    {
        await _lock.WaitAsync(cancellationToken);
        try
        {
            _accessToken = token;
        }
        finally
        {
            _lock.Release();
        }
    }

    public async Task ClearAccessTokenAsync(CancellationToken cancellationToken = default)
    {
        await _lock.WaitAsync(cancellationToken);
        try
        {
            _accessToken = null;
        }
        finally
        {
            _lock.Release();
        }
    }

    public async Task<string?> GetRefreshTokenAsync(CancellationToken cancellationToken = default)
    {
        await _lock.WaitAsync(cancellationToken);
        try
        {
            return _refreshToken;
        }
        finally
        {
            _lock.Release();
        }
    }

    public async Task SetRefreshTokenAsync(string token, CancellationToken cancellationToken = default)
    {
        await _lock.WaitAsync(cancellationToken);
        try
        {
            _refreshToken = token;
        }
        finally
        {
            _lock.Release();
        }
    }

    public async Task ClearRefreshTokenAsync(CancellationToken cancellationToken = default)
    {
        await _lock.WaitAsync(cancellationToken);
        try
        {
            _refreshToken = null;
        }
        finally
        {
            _lock.Release();
        }
    }

    public async Task ClearAllAsync(CancellationToken cancellationToken = default)
    {
        await _lock.WaitAsync(cancellationToken);
        try
        {
            _accessToken = null;
            _refreshToken = null;
        }
        finally
        {
            _lock.Release();
        }
    }

    public async Task<bool> HasTokensAsync(CancellationToken cancellationToken = default)
    {
        await _lock.WaitAsync(cancellationToken);
        try
        {
            return !string.IsNullOrEmpty(_accessToken);
        }
        finally
        {
            _lock.Release();
        }
    }
}
