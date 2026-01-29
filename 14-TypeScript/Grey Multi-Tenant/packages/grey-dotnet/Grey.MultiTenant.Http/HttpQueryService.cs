using System.Net.Http.Json;
using System.Text.Json;
using System.Web;
using Grey.MultiTenant.Core.Abstractions;
using Grey.MultiTenant.Core.Exceptions;
using Grey.MultiTenant.Core.Query;
using Microsoft.Extensions.Options;

namespace Grey.MultiTenant.Http;

/// <summary>
/// HTTP-based implementation of the query service.
/// </summary>
public sealed class HttpQueryService : IQueryService
{
    private readonly HttpClient _httpClient;
    private readonly GreyMultiTenantOptions _options;
    private readonly JsonSerializerOptions _jsonOptions;

    public HttpQueryService(
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

    public async Task<QueryResult> ExecuteQueryAsync(QueryRequest request, QueryOptions? options = null, CancellationToken cancellationToken = default)
    {
        return await ExecuteQueryAsync<object>(request, options, cancellationToken);
    }

    public async Task<QueryResult<T>> ExecuteQueryAsync<T>(QueryRequest request, QueryOptions? options = null, CancellationToken cancellationToken = default)
    {
        var effectiveOptions = options ?? new QueryOptions();
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

                var url = BuildUrl(request.Endpoint, request.Params);
                using var httpRequest = new HttpRequestMessage(new HttpMethod(request.Method), url);

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
                    return new QueryResult<T>
                    {
                        Success = false,
                        Error = errorContent,
                        StatusCode = (int)response.StatusCode
                    };
                }

                var data = await response.Content.ReadFromJsonAsync<T>(_jsonOptions, cancellationToken);
                return new QueryResult<T>
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

                    return new QueryResult<T>
                    {
                        Success = false,
                        Error = ex.Message,
                        StatusCode = 0
                    };
                }
            }
        }

        // Should not reach here
        throw lastException ?? new ApiException("Query failed");
    }

    public async Task<T> GetAsync<T>(string endpoint, Dictionary<string, string>? queryParams = null, CancellationToken cancellationToken = default)
    {
        try
        {
            var url = BuildUrl(endpoint, queryParams);
            var response = await _httpClient.GetAsync(url, cancellationToken);
            await ErrorHandler.EnsureSuccessAsync(response, cancellationToken);

            return await response.Content.ReadFromJsonAsync<T>(_jsonOptions, cancellationToken)
                ?? throw new ApiException("Invalid response");
        }
        catch (Exception ex) when (ex is not ApiException and not AuthException)
        {
            throw ErrorHandler.WrapException(ex);
        }
    }

    public async Task<IReadOnlyList<QueryResult>> BatchQueryAsync(IEnumerable<QueryRequest> requests, QueryOptions? options = null, CancellationToken cancellationToken = default)
    {
        var tasks = requests.Select(r => ExecuteQueryAsync(r, options, cancellationToken));
        var results = await Task.WhenAll(tasks);
        return results;
    }

    private static string BuildUrl(string endpoint, Dictionary<string, string>? queryParams)
    {
        if (queryParams == null || queryParams.Count == 0)
            return endpoint;

        var query = string.Join("&", queryParams.Select(kvp => 
            $"{HttpUtility.UrlEncode(kvp.Key)}={HttpUtility.UrlEncode(kvp.Value)}"));

        return endpoint.Contains('?') ? $"{endpoint}&{query}" : $"{endpoint}?{query}";
    }
}
