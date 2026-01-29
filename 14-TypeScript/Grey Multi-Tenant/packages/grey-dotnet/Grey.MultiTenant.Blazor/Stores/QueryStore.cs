using Grey.MultiTenant.Core.Abstractions;
using Grey.MultiTenant.Core.Query;

namespace Grey.MultiTenant.Blazor.Stores;

/// <summary>
/// Observable store for query state in Blazor applications.
/// </summary>
public sealed class QueryStore : StoreBase, IDisposable
{
    private readonly IQueryService _queryService;
    private readonly SemaphoreSlim _operationLock = new(1, 1);

    private bool _isLoading;
    private bool _isFetching;
    private object? _data;
    private string? _error;
    private DateTimeOffset? _lastFetchedAt;

    public QueryStore(IQueryService queryService)
    {
        _queryService = queryService;
    }

    /// <summary>
    /// Whether a query is currently loading (initial load).
    /// </summary>
    public bool IsLoading
    {
        get => _isLoading;
        private set => SetProperty(ref _isLoading, value);
    }

    /// <summary>
    /// Whether a query is currently fetching (refetch).
    /// </summary>
    public bool IsFetching
    {
        get => _isFetching;
        private set => SetProperty(ref _isFetching, value);
    }

    /// <summary>
    /// The current query data.
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
    /// When the data was last fetched.
    /// </summary>
    public DateTimeOffset? LastFetchedAt
    {
        get => _lastFetchedAt;
        private set => SetProperty(ref _lastFetchedAt, value);
    }

    /// <summary>
    /// Whether the query was successful.
    /// </summary>
    public bool IsSuccess => Data != null && Error == null;

    /// <summary>
    /// Whether the query has an error.
    /// </summary>
    public bool IsError => Error != null;

    /// <summary>
    /// Event raised when query data changes.
    /// </summary>
    public event EventHandler<object?>? DataChanged;

    /// <summary>
    /// Executes a query.
    /// </summary>
    public async Task<QueryResult<T>> ExecuteAsync<T>(QueryRequest request, QueryOptions? options = null)
    {
        var isInitialLoad = Data == null;
        
        await _operationLock.WaitAsync();
        try
        {
            if (isInitialLoad)
                IsLoading = true;
            else
                IsFetching = true;
            
            Error = null;

            var result = await _queryService.ExecuteQueryAsync<T>(request, options);
            
            if (result.Success)
            {
                Data = result.Data;
                LastFetchedAt = DateTimeOffset.UtcNow;
                DataChanged?.Invoke(this, Data);
            }
            else
            {
                Error = result.Error;
            }

            return result;
        }
        catch (Exception ex)
        {
            Error = NormalizeError(ex);
            return new QueryResult<T>
            {
                Success = false,
                Error = Error
            };
        }
        finally
        {
            IsLoading = false;
            IsFetching = false;
            _operationLock.Release();
        }
    }

    /// <summary>
    /// Executes a GET request.
    /// </summary>
    public async Task<T?> GetAsync<T>(string endpoint, Dictionary<string, string>? queryParams = null)
    {
        var isInitialLoad = Data == null;
        
        await _operationLock.WaitAsync();
        try
        {
            if (isInitialLoad)
                IsLoading = true;
            else
                IsFetching = true;
            
            Error = null;

            var result = await _queryService.GetAsync<T>(endpoint, queryParams);
            Data = result;
            LastFetchedAt = DateTimeOffset.UtcNow;
            DataChanged?.Invoke(this, Data);
            
            return result;
        }
        catch (Exception ex)
        {
            Error = NormalizeError(ex);
            return default;
        }
        finally
        {
            IsLoading = false;
            IsFetching = false;
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

    /// <summary>
    /// Clears the current data.
    /// </summary>
    public void Clear()
    {
        Data = null;
        Error = null;
        LastFetchedAt = null;
    }

    public void Dispose()
    {
        _operationLock.Dispose();
    }
}
