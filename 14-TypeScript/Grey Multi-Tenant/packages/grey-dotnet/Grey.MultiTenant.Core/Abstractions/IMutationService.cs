using Grey.MultiTenant.Core.Mutation;

namespace Grey.MultiTenant.Core.Abstractions;

/// <summary>
/// Service interface for mutation operations.
/// </summary>
public interface IMutationService
{
    /// <summary>
    /// Executes a mutation and returns the result.
    /// </summary>
    Task<MutationResult> ExecuteMutationAsync(MutationRequest request, MutationOptions? options = null, CancellationToken cancellationToken = default);

    /// <summary>
    /// Executes a typed mutation and returns the result.
    /// </summary>
    Task<MutationResult<T>> ExecuteMutationAsync<T>(MutationRequest request, MutationOptions? options = null, CancellationToken cancellationToken = default);

    /// <summary>
    /// Executes a POST request to the specified endpoint.
    /// </summary>
    Task<T> PostAsync<T>(string endpoint, object? body = null, CancellationToken cancellationToken = default);

    /// <summary>
    /// Executes a PUT request to the specified endpoint.
    /// </summary>
    Task<T> PutAsync<T>(string endpoint, object? body = null, CancellationToken cancellationToken = default);

    /// <summary>
    /// Executes a PATCH request to the specified endpoint.
    /// </summary>
    Task<T> PatchAsync<T>(string endpoint, object? body = null, CancellationToken cancellationToken = default);

    /// <summary>
    /// Executes a DELETE request to the specified endpoint.
    /// </summary>
    Task DeleteAsync(string endpoint, CancellationToken cancellationToken = default);

    /// <summary>
    /// Executes multiple mutations in sequence.
    /// </summary>
    Task<IReadOnlyList<MutationResult>> ExecuteSequentialAsync(IEnumerable<MutationRequest> requests, MutationOptions? options = null, CancellationToken cancellationToken = default);

    /// <summary>
    /// Executes multiple mutations in parallel.
    /// </summary>
    Task<IReadOnlyList<MutationResult>> ExecuteParallelAsync(IEnumerable<MutationRequest> requests, MutationOptions? options = null, CancellationToken cancellationToken = default);
}
