using Grey.MultiTenant.Core.Abstractions;
using Grey.MultiTenant.Core.Mutation;

namespace Grey.MultiTenant.Blazor.Stores;

/// <summary>
/// Observable store for mutation state in Blazor applications.
/// </summary>
public sealed class MutationStore : StoreBase, IDisposable
{
    private readonly IMutationService _mutationService;
    private readonly SemaphoreSlim _operationLock = new(1, 1);

    private bool _isMutating;
    private object? _data;
    private string? _error;
    private bool _isSuccess;
    private bool _isError;

    public MutationStore(IMutationService mutationService)
    {
        _mutationService = mutationService;
    }

    /// <summary>
    /// Whether a mutation is currently in progress.
    /// </summary>
    public bool IsMutating
    {
        get => _isMutating;
        private set => SetProperty(ref _isMutating, value);
    }

    /// <summary>
    /// The result data from the last mutation.
    /// </summary>
    public object? Data
    {
        get => _data;
        private set => SetProperty(ref _data, value);
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
    /// Whether the last mutation was successful.
    /// </summary>
    public bool IsSuccess
    {
        get => _isSuccess;
        private set => SetProperty(ref _isSuccess, value);
    }

    /// <summary>
    /// Whether the last mutation had an error.
    /// </summary>
    public bool IsError
    {
        get => _isError;
        private set => SetProperty(ref _isError, value);
    }

    /// <summary>
    /// Event raised when mutation completes.
    /// </summary>
    public event EventHandler<MutationCompletedEventArgs>? MutationCompleted;

    /// <summary>
    /// Executes a mutation.
    /// </summary>
    public async Task<MutationResult<T>> ExecuteAsync<T>(MutationRequest request, MutationOptions? options = null)
    {
        await _operationLock.WaitAsync();
        try
        {
            IsMutating = true;
            Error = null;
            IsSuccess = false;
            IsError = false;

            var result = await _mutationService.ExecuteMutationAsync<T>(request, options);
            
            Data = result.Data;
            IsSuccess = result.Success;
            IsError = !result.Success;
            
            if (!result.Success)
            {
                Error = result.Error;
            }

            MutationCompleted?.Invoke(this, new MutationCompletedEventArgs(result.Success, result.Data, result.Error));
            return result;
        }
        catch (Exception ex)
        {
            Error = NormalizeError(ex);
            IsError = true;
            MutationCompleted?.Invoke(this, new MutationCompletedEventArgs(false, null, Error));
            
            return new MutationResult<T>
            {
                Success = false,
                Error = Error
            };
        }
        finally
        {
            IsMutating = false;
            _operationLock.Release();
        }
    }

    /// <summary>
    /// Executes a POST request.
    /// </summary>
    public async Task<T?> PostAsync<T>(string endpoint, object? body = null)
    {
        await _operationLock.WaitAsync();
        try
        {
            IsMutating = true;
            Error = null;
            IsSuccess = false;
            IsError = false;

            var result = await _mutationService.PostAsync<T>(endpoint, body);
            Data = result;
            IsSuccess = true;
            
            MutationCompleted?.Invoke(this, new MutationCompletedEventArgs(true, result, null));
            return result;
        }
        catch (Exception ex)
        {
            Error = NormalizeError(ex);
            IsError = true;
            MutationCompleted?.Invoke(this, new MutationCompletedEventArgs(false, null, Error));
            return default;
        }
        finally
        {
            IsMutating = false;
            _operationLock.Release();
        }
    }

    /// <summary>
    /// Executes a PUT request.
    /// </summary>
    public async Task<T?> PutAsync<T>(string endpoint, object? body = null)
    {
        await _operationLock.WaitAsync();
        try
        {
            IsMutating = true;
            Error = null;
            IsSuccess = false;
            IsError = false;

            var result = await _mutationService.PutAsync<T>(endpoint, body);
            Data = result;
            IsSuccess = true;
            
            MutationCompleted?.Invoke(this, new MutationCompletedEventArgs(true, result, null));
            return result;
        }
        catch (Exception ex)
        {
            Error = NormalizeError(ex);
            IsError = true;
            MutationCompleted?.Invoke(this, new MutationCompletedEventArgs(false, null, Error));
            return default;
        }
        finally
        {
            IsMutating = false;
            _operationLock.Release();
        }
    }

    /// <summary>
    /// Executes a DELETE request.
    /// </summary>
    public async Task<bool> DeleteAsync(string endpoint)
    {
        await _operationLock.WaitAsync();
        try
        {
            IsMutating = true;
            Error = null;
            IsSuccess = false;
            IsError = false;

            await _mutationService.DeleteAsync(endpoint);
            IsSuccess = true;
            
            MutationCompleted?.Invoke(this, new MutationCompletedEventArgs(true, null, null));
            return true;
        }
        catch (Exception ex)
        {
            Error = NormalizeError(ex);
            IsError = true;
            MutationCompleted?.Invoke(this, new MutationCompletedEventArgs(false, null, Error));
            return false;
        }
        finally
        {
            IsMutating = false;
            _operationLock.Release();
        }
    }

    /// <summary>
    /// Resets the mutation state.
    /// </summary>
    public void Reset()
    {
        Data = null;
        Error = null;
        IsSuccess = false;
        IsError = false;
    }

    /// <summary>
    /// Clears any current error.
    /// </summary>
    public void ClearError()
    {
        Error = null;
        IsError = false;
    }

    public void Dispose()
    {
        _operationLock.Dispose();
    }
}

/// <summary>
/// Event arguments for mutation completion.
/// </summary>
public sealed class MutationCompletedEventArgs : EventArgs
{
    public bool Success { get; }
    public object? Data { get; }
    public string? Error { get; }

    public MutationCompletedEventArgs(bool success, object? data, string? error)
    {
        Success = success;
        Data = data;
        Error = error;
    }
}
