using Grey.MultiTenant.Core.Abstractions;
using Grey.MultiTenant.Core.Auth;
using Grey.MultiTenant.Core.Models;

namespace Grey.MultiTenant.Blazor.Stores;

/// <summary>
/// Observable store for authentication state in Blazor applications.
/// </summary>
public sealed class AuthStore : StoreBase, IDisposable
{
    private readonly IAuthService _authService;
    private readonly SemaphoreSlim _operationLock = new(1, 1);
    
    private bool _isLoading;
    private bool _isAuthenticated;
    private UserModel? _user;
    private TokenModel? _tokens;
    private string? _error;

    public AuthStore(IAuthService authService)
    {
        _authService = authService;
    }

    /// <summary>
    /// Whether an authentication operation is in progress.
    /// </summary>
    public bool IsLoading
    {
        get => _isLoading;
        private set => SetProperty(ref _isLoading, value);
    }

    /// <summary>
    /// Whether the user is currently authenticated.
    /// </summary>
    public bool IsAuthenticated
    {
        get => _isAuthenticated;
        private set => SetProperty(ref _isAuthenticated, value);
    }

    /// <summary>
    /// The current authenticated user.
    /// </summary>
    public UserModel? User
    {
        get => _user;
        private set => SetProperty(ref _user, value);
    }

    /// <summary>
    /// The current authentication tokens.
    /// </summary>
    public TokenModel? Tokens
    {
        get => _tokens;
        private set => SetProperty(ref _tokens, value);
    }

    /// <summary>
    /// The last error that occurred.
    /// </summary>
    public string? Error
    {
        get => _error;
        private set => SetProperty(ref _error, value);
    }

    /// <summary>
    /// Event raised when authentication state changes.
    /// </summary>
    public event EventHandler<AuthStateChangedEventArgs>? AuthStateChanged;

    /// <summary>
    /// Authenticates a user with credentials.
    /// </summary>
    public async Task<bool> LoginAsync(string email, string password, string? tenantId = null, bool rememberMe = false)
    {
        await _operationLock.WaitAsync();
        try
        {
            IsLoading = true;
            Error = null;

            var result = await _authService.LoginAsync(new LoginRequest
            {
                Email = email,
                Password = password,
                TenantId = tenantId,
                RememberMe = rememberMe
            });

            User = result.User;
            Tokens = result.Tokens;
            IsAuthenticated = true;

            RaiseAuthStateChanged(true);
            return true;
        }
        catch (Exception ex)
        {
            Error = NormalizeError(ex);
            return false;
        }
        finally
        {
            IsLoading = false;
            _operationLock.Release();
        }
    }

    /// <summary>
    /// Logs out the current user.
    /// </summary>
    public async Task LogoutAsync()
    {
        await _operationLock.WaitAsync();
        try
        {
            IsLoading = true;
            Error = null;

            await _authService.LogoutAsync();

            User = null;
            Tokens = null;
            IsAuthenticated = false;

            RaiseAuthStateChanged(false);
        }
        catch (Exception ex)
        {
            Error = NormalizeError(ex);
        }
        finally
        {
            IsLoading = false;
            _operationLock.Release();
        }
    }

    /// <summary>
    /// Refreshes the current access token.
    /// </summary>
    public async Task<bool> RefreshAsync()
    {
        await _operationLock.WaitAsync();
        try
        {
            Error = null;

            var result = await _authService.RefreshAsync();
            Tokens = result.Tokens;
            return true;
        }
        catch (Exception ex)
        {
            Error = NormalizeError(ex);
            
            // If refresh fails, log out
            User = null;
            Tokens = null;
            IsAuthenticated = false;
            RaiseAuthStateChanged(false);
            
            return false;
        }
        finally
        {
            _operationLock.Release();
        }
    }

    /// <summary>
    /// Checks and restores authentication state on initialization.
    /// </summary>
    public async Task InitializeAsync()
    {
        try
        {
            IsLoading = true;
            var isAuthenticated = await _authService.IsAuthenticatedAsync();
            IsAuthenticated = isAuthenticated;

            if (isAuthenticated)
            {
                RaiseAuthStateChanged(true);
            }
        }
        catch
        {
            IsAuthenticated = false;
        }
        finally
        {
            IsLoading = false;
        }
    }

    /// <summary>
    /// Clears any current error.
    /// </summary>
    public void ClearError()
    {
        Error = null;
    }

    private void RaiseAuthStateChanged(bool isAuthenticated)
    {
        AuthStateChanged?.Invoke(this, new AuthStateChangedEventArgs(isAuthenticated, User));
    }

    public void Dispose()
    {
        _operationLock.Dispose();
    }
}

/// <summary>
/// Event arguments for authentication state changes.
/// </summary>
public sealed class AuthStateChangedEventArgs : EventArgs
{
    public bool IsAuthenticated { get; }
    public UserModel? User { get; }

    public AuthStateChangedEventArgs(bool isAuthenticated, UserModel? user)
    {
        IsAuthenticated = isAuthenticated;
        User = user;
    }
}
