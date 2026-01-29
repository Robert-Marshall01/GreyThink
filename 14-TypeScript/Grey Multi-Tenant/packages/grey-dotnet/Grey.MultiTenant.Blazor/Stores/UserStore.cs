using Grey.MultiTenant.Core.Abstractions;
using Grey.MultiTenant.Core.Models;
using Grey.MultiTenant.Core.User;

namespace Grey.MultiTenant.Blazor.Stores;

/// <summary>
/// Observable store for user state in Blazor applications.
/// </summary>
public sealed class UserStore : StoreBase, IDisposable
{
    private readonly IUserService _userService;
    private readonly AuthStore _authStore;
    private readonly SemaphoreSlim _operationLock = new(1, 1);

    private bool _isLoading;
    private UserModel? _user;
    private string? _error;

    public UserStore(IUserService userService, AuthStore authStore)
    {
        _userService = userService;
        _authStore = authStore;

        // Subscribe to auth state changes
        _authStore.AuthStateChanged += OnAuthStateChanged;
    }

    /// <summary>
    /// Whether a user operation is in progress.
    /// </summary>
    public bool IsLoading
    {
        get => _isLoading;
        private set => SetProperty(ref _isLoading, value);
    }

    /// <summary>
    /// The current user.
    /// </summary>
    public UserModel? User
    {
        get => _user;
        private set => SetProperty(ref _user, value);
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
    /// Event raised when user data changes.
    /// </summary>
    public event EventHandler<UserModel?>? UserChanged;

    /// <summary>
    /// Fetches the current user's profile.
    /// </summary>
    public async Task FetchUserAsync()
    {
        if (!_authStore.IsAuthenticated)
        {
            User = null;
            return;
        }

        await _operationLock.WaitAsync();
        try
        {
            IsLoading = true;
            Error = null;

            User = await _userService.GetUserAsync();
            UserChanged?.Invoke(this, User);
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
    /// Gets a user by their ID.
    /// </summary>
    public async Task<UserModel?> GetUserByIdAsync(string userId)
    {
        await _operationLock.WaitAsync();
        try
        {
            IsLoading = true;
            Error = null;

            return await _userService.GetUserByIdAsync(userId);
        }
        catch (Exception ex)
        {
            Error = NormalizeError(ex);
            return null;
        }
        finally
        {
            IsLoading = false;
            _operationLock.Release();
        }
    }

    /// <summary>
    /// Updates the current user's profile.
    /// </summary>
    public async Task<bool> UpdateUserAsync(UpdateUserRequest request)
    {
        await _operationLock.WaitAsync();
        try
        {
            IsLoading = true;
            Error = null;

            User = await _userService.UpdateUserAsync(request);
            UserChanged?.Invoke(this, User);
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
    /// Clears any current error.
    /// </summary>
    public void ClearError()
    {
        Error = null;
    }

    private void OnAuthStateChanged(object? sender, AuthStateChangedEventArgs e)
    {
        if (e.IsAuthenticated)
        {
            // Fetch user on login
            _ = FetchUserAsync();
        }
        else
        {
            // Clear user on logout
            User = null;
            UserChanged?.Invoke(this, null);
        }
    }

    public void Dispose()
    {
        _authStore.AuthStateChanged -= OnAuthStateChanged;
        _operationLock.Dispose();
    }
}
