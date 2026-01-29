// =============================================================================
// Grey Multi-Tenant SDK - .NET
// Services/QueryService.cs
//
// Generic query service wrapping adapter-core functions.
// Exposes: Data, Loading, Error, and domain actions.
// =============================================================================

using System.Net.Http.Json;
using System.Text.Json;
using Microsoft.Extensions.Options;

namespace Grey.Sdk.Services;

/// <summary>
/// Query request parameters.
/// </summary>
public sealed class QueryRequest
{
    /// <summary>
    /// The API endpoint to query.
    /// </summary>
    public required string Endpoint { get; set; }

    /// <summary>
    /// Optional query parameters.
    /// </summary>
    public Dictionary<string, string>? Params { get; set; }
}

/// <summary>
/// Generic query service for Grey Multi-Tenant SDK.
/// Thread-safe, async, DI-friendly.
/// </summary>
public sealed class QueryService
{
    private readonly HttpClient _httpClient;
    private readonly GreyOptions _options;
    private readonly object _lock = new();

    private JsonElement? _data;
    private bool _loading;
    private GreyError? _error;

    /// <summary>
    /// Current query result data.
    /// </summary>
    public JsonElement? Data
    {
        get { lock (_lock) return _data; }
        private set { lock (_lock) _data = value; }
    }

    /// <summary>
    /// Whether an operation is in progress.
    /// </summary>
    public bool Loading
    {
        get { lock (_lock) return _loading; }
        private set { lock (_lock) _loading = value; }
    }

    /// <summary>
    /// Last error from an operation.
    /// </summary>
    public GreyError? Error
    {
        get { lock (_lock) return _error; }
        private set { lock (_lock) _error = value; }
    }

    /// <summary>
    /// Creates a new QueryService.
    /// </summary>
    public QueryService(HttpClient httpClient, IOptions<GreyOptions> options)
    {
        _httpClient = httpClient ?? throw new ArgumentNullException(nameof(httpClient));
        _options = options?.Value ?? throw new ArgumentNullException(nameof(options));
    }

    /// <summary>
    /// Execute a generic query.
    /// </summary>
    public async Task<JsonElement?> ExecuteQueryAsync(QueryRequest request, CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(request);

        Loading = true;
        Error = null;

        try
        {
            var queryString = string.Empty;
            if (request.Params?.Count > 0)
            {
                var queryParams = string.Join("&", request.Params.Select(kv => $"{Uri.EscapeDataString(kv.Key)}={Uri.EscapeDataString(kv.Value)}"));
                queryString = $"?{queryParams}";
            }

            var response = await _httpClient.GetAsync(
                $"{_options.ApiUrl}/{request.Endpoint.TrimStart('/')}{queryString}",
                cancellationToken
            );

            if (!response.IsSuccessStatusCode)
            {
                var errorContent = await response.Content.ReadAsStringAsync(cancellationToken);
                Error = new GreyError
                {
                    Message = $"Query failed: {response.ReasonPhrase}",
                    Status = (int)response.StatusCode,
                    Raw = errorContent,
                };
                return null;
            }

            var json = await response.Content.ReadAsStringAsync(cancellationToken);
            var data = JsonSerializer.Deserialize<JsonElement>(json);
            Data = data;
            return data;
        }
        catch (Exception ex)
        {
            Error = GreyError.FromException(ex);
            return null;
        }
        finally
        {
            Loading = false;
        }
    }

    /// <summary>
    /// Execute a typed query.
    /// </summary>
    public async Task<T?> ExecuteQueryAsync<T>(QueryRequest request, CancellationToken cancellationToken = default) where T : class
    {
        ArgumentNullException.ThrowIfNull(request);

        Loading = true;
        Error = null;

        try
        {
            var queryString = string.Empty;
            if (request.Params?.Count > 0)
            {
                var queryParams = string.Join("&", request.Params.Select(kv => $"{Uri.EscapeDataString(kv.Key)}={Uri.EscapeDataString(kv.Value)}"));
                queryString = $"?{queryParams}";
            }

            var response = await _httpClient.GetAsync(
                $"{_options.ApiUrl}/{request.Endpoint.TrimStart('/')}{queryString}",
                cancellationToken
            );

            if (!response.IsSuccessStatusCode)
            {
                var errorContent = await response.Content.ReadAsStringAsync(cancellationToken);
                Error = new GreyError
                {
                    Message = $"Query failed: {response.ReasonPhrase}",
                    Status = (int)response.StatusCode,
                    Raw = errorContent,
                };
                return null;
            }

            var data = await response.Content.ReadFromJsonAsync<T>(cancellationToken: cancellationToken);
            return data;
        }
        catch (Exception ex)
        {
            Error = GreyError.FromException(ex);
            return null;
        }
        finally
        {
            Loading = false;
        }
    }
}
