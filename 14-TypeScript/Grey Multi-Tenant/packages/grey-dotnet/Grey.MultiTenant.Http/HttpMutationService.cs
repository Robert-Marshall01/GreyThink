using System.Net.Http.Json;
using System.Text;
using System.Text.Json;
using Grey.MultiTenant.Core.Abstractions;
using Grey.MultiTenant.Core.Exceptions;
using Grey.MultiTenant.Core.Mutation;
using Microsoft.Extensions.Options;

namespace Grey.MultiTenant.Http;

/// <summary>
/// HTTP-based implementation of the mutation service.
/// </summary>
public sealed class HttpMutationService : IMutationService
{
    private readonly HttpClient _httpClient;
    private readonly GreyMultiTenantOptions _options;
    private readonly JsonSerializerOptions _jsonOptions;

    public HttpMutationService(
        HttpClient httpClient,
        IOptions<GreyMultiTenantOptions> options)
    {
        _httpClient = httpClient;
        _options = options.Value;
        _jsonOptions = new JsonSerializerOptions
        {
            PropertyNameCaseInsensitive = true,
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase
        };
    }

    public async Task<MutationResult> ExecuteMutationAsync(MutationRequest request, MutationOptions? options = null, CancellationToken cancellationToken = default)
    {
        return await ExecuteMutationAsync<object>(request, options, cancellationToken);
    }

    public async Task<MutationResult<T>> ExecuteMutationAsync<T>(MutationRequest request, MutationOptions? options = null, CancellationToken cancellationToken = default)
    {
        var effectiveOptions = options ?? new MutationOptions();
        var retryCount = effectiveOptions.RetryCount;
        var lastException = (Exception?)null;

        for (var attempt = 0; attempt <= retryCount; attempt++)
        {
            try
            {
                if (attempt > 0 && effectiveOptions.RetryDelay.HasValue)
                {
                    await Task.Delay(effectiveOptions.RetryDelay.Value, cancellationToken);
                }

                using var httpRequest = new HttpRequestMessage(new HttpMethod(request.Method), request.Endpoint);

                if (request.Body != null)
                {
                    httpRequest.Content = new StringContent(
                        JsonSerializer.Serialize(request.Body, _jsonOptions),
                        Encoding.UTF8,
                        "application/json");
                }

                if (request.Headers != null)
                {
                    foreach (var header in request.Headers)
                    {
                        httpRequest.Headers.TryAddWithoutValidation(header.Key, header.Value);
                    }
                }

                using var cts = effectiveOptions.Timeout.HasValue
                    ? CancellationTokenSource.CreateLinkedTokenSource(cancellationToken)
                    : null;

                if (cts != null)
                {
                    cts.CancelAfter(effectiveOptions.Timeout!.Value);
                }

                var response = await _httpClient.SendAsync(httpRequest, cts?.Token ?? cancellationToken);

                if (!response.IsSuccessStatusCode)
                {
                    if (effectiveOptions.ThrowOnError)
                    {
                        await ErrorHandler.EnsureSuccessAsync(response, cancellationToken);
                    }

                    var errorContent = await response.Content.ReadAsStringAsync(cancellationToken);
                    return new MutationResult<T>
                    {
                        Success = false,
                        Error = errorContent,
                        StatusCode = (int)response.StatusCode
                    };
                }

                // Handle empty responses (e.g., 204 No Content)
                if (response.Content.Headers.ContentLength == 0)
                {
                    return new MutationResult<T>
                    {
                        Success = true,
                        StatusCode = (int)response.StatusCode
                    };
                }

                var data = await response.Content.ReadFromJsonAsync<T>(_jsonOptions, cancellationToken);
                return new MutationResult<T>
                {
                    Data = data,
                    Success = true,
                    StatusCode = (int)response.StatusCode
                };
            }
            catch (Exception ex) when (ex is not OperationCanceledException || !cancellationToken.IsCancellationRequested)
            {
                lastException = ex;
                if (attempt >= retryCount)
                {
                    if (effectiveOptions.ThrowOnError)
                    {
                        throw ErrorHandler.WrapException(ex);
                    }

                    return new MutationResult<T>
                    {
                        Success = false,
                        Error = ex.Message,
                        StatusCode = 0
                    };
                }
            }
        }

        // Should not reach here
        throw lastException ?? new ApiException("Mutation failed");
    }

    public async Task<T> PostAsync<T>(string endpoint, object? body = null, CancellationToken cancellationToken = default)
    {
        try
        {
            var content = body != null
                ? new StringContent(JsonSerializer.Serialize(body, _jsonOptions), Encoding.UTF8, "application/json")
                : null;

            var response = await _httpClient.PostAsync(endpoint, content, cancellationToken);
            await ErrorHandler.EnsureSuccessAsync(response, cancellationToken);

            return await response.Content.ReadFromJsonAsync<T>(_jsonOptions, cancellationToken)
                ?? throw new ApiException("Invalid response");
        }
        catch (Exception ex) when (ex is not ApiException and not AuthException and not ValidationException)
        {
            throw ErrorHandler.WrapException(ex);
        }
    }

    public async Task<T> PutAsync<T>(string endpoint, object? body = null, CancellationToken cancellationToken = default)
    {
        try
        {
            var content = body != null
                ? new StringContent(JsonSerializer.Serialize(body, _jsonOptions), Encoding.UTF8, "application/json")
                : null;

            var response = await _httpClient.PutAsync(endpoint, content, cancellationToken);
            await ErrorHandler.EnsureSuccessAsync(response, cancellationToken);

            return await response.Content.ReadFromJsonAsync<T>(_jsonOptions, cancellationToken)
                ?? throw new ApiException("Invalid response");
        }
        catch (Exception ex) when (ex is not ApiException and not AuthException and not ValidationException)
        {
            throw ErrorHandler.WrapException(ex);
        }
    }

    public async Task<T> PatchAsync<T>(string endpoint, object? body = null, CancellationToken cancellationToken = default)
    {
        try
        {
            var content = body != null
                ? new StringContent(JsonSerializer.Serialize(body, _jsonOptions), Encoding.UTF8, "application/json")
                : null;

            var response = await _httpClient.PatchAsync(endpoint, content, cancellationToken);
            await ErrorHandler.EnsureSuccessAsync(response, cancellationToken);

            return await response.Content.ReadFromJsonAsync<T>(_jsonOptions, cancellationToken)
                ?? throw new ApiException("Invalid response");
        }
        catch (Exception ex) when (ex is not ApiException and not AuthException and not ValidationException)
        {
            throw ErrorHandler.WrapException(ex);
        }
    }

    public async Task DeleteAsync(string endpoint, CancellationToken cancellationToken = default)
    {
        try
        {
            var response = await _httpClient.DeleteAsync(endpoint, cancellationToken);
            await ErrorHandler.EnsureSuccessAsync(response, cancellationToken);
        }
        catch (Exception ex) when (ex is not ApiException and not AuthException)
        {
            throw ErrorHandler.WrapException(ex);
        }
    }

    public async Task<IReadOnlyList<MutationResult>> ExecuteSequentialAsync(IEnumerable<MutationRequest> requests, MutationOptions? options = null, CancellationToken cancellationToken = default)
    {
        var results = new List<MutationResult>();
        foreach (var request in requests)
        {
            var result = await ExecuteMutationAsync(request, options, cancellationToken);
            results.Add(result);
            
            // Stop on first error if throwing is disabled
            if (!result.Success && options?.ThrowOnError == false)
            {
                break;
            }
        }
        return results;
    }

    public async Task<IReadOnlyList<MutationResult>> ExecuteParallelAsync(IEnumerable<MutationRequest> requests, MutationOptions? options = null, CancellationToken cancellationToken = default)
    {
        var tasks = requests.Select(r => ExecuteMutationAsync(r, options, cancellationToken));
        var results = await Task.WhenAll(tasks);
        return results;
    }
}
