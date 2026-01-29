using Grey.MultiTenant.Core.Query;

namespace Grey.MultiTenant.Core.Abstractions;

/// <summary>
/// Service interface for query operations.
/// </summary>
public interface IQueryService
{
    /// <summary>
    /// Executes a query and returns the result.
    /// </summary>
    Task<QueryResult> ExecuteQueryAsync(QueryRequest request, QueryOptions? options = null, CancellationToken cancellationToken = default);

    /// <summary>
    /// Executes a typed query and returns the result.
    /// </summary>
    Task<QueryResult<T>> ExecuteQueryAsync<T>(QueryRequest request, QueryOptions? options = null, CancellationToken cancellationToken = default);

    /// <summary>
    /// Executes a GET request to the specified endpoint.
    /// </summary>
    Task<T> GetAsync<T>(string endpoint, Dictionary<string, string>? queryParams = null, CancellationToken cancellationToken = default);

    /// <summary>
    /// Executes a batch of queries in parallel.
    /// </summary>
    Task<IReadOnlyList<QueryResult>> BatchQueryAsync(IEnumerable<QueryRequest> requests, QueryOptions? options = null, CancellationToken cancellationToken = default);
}
