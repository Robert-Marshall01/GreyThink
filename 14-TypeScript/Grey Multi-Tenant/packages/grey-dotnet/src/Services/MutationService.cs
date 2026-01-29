// =============================================================================
// Grey Multi-Tenant SDK - .NET
// Services/MutationService.cs
//
// Generic mutation service wrapping adapter-core functions.
// Exposes: Data, Loading, Error, and domain actions.
// =============================================================================

using System.Net.Http.Json;
using System.Text.Json;
using Microsoft.Extensions.Options;

namespace Grey.Sdk.Services;

/// <summary>
/// Mutation request parameters.
/// </summary>
public sealed class MutationRequest
{
    /// <summary>
    /// The API endpoint for the mutation.
    /// </summary>
    public required string Endpoint { get; set; }

    /// <summary>
    /// HTTP method (POST, PUT, PATCH, DELETE).
    /// </summary>
    public string Method { get; set; } = "POST";

    /// <summary>
    /// Request body (will be serialized to JSON).
    /// </summary>
    public object? Body { get; set; }
}

/// <summary>
/// Generic mutation service for Grey Multi-Tenant SDK.
/// Thread-safe, async, DI-friendly.
/// </summary>
public sealed class MutationService
{
    private readonly HttpClient _httpClient;
    private readonly GreyOptions _options;
    private readonly object _lock = new();

    private JsonElement? _data;
    private bool _loading;
    private GreyError? _error;

    /// <summary>
    /// Current mutation result data.
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
    /// Creates a new MutationService.
    /// </summary>
    public MutationService(HttpClient httpClient, IOptions<GreyOptions> options)
    {
        _httpClient = httpClient ?? throw new ArgumentNullException(nameof(httpClient));
        _options = options?.Value ?? throw new ArgumentNullException(nameof(options));
    }

    /// <summary>
    /// Execute a generic mutation.
    /// </summary>
    public async Task<JsonElement?> ExecuteMutationAsync(MutationRequest request, CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(request);

        Loading = true;
        Error = null;

        try
        {
            var url = $"{_options.ApiUrl}/{request.Endpoint.TrimStart('/')}";
            HttpResponseMessage response;

            switch (request.Method.ToUpperInvariant())
            {
                case "POST":
                    response = await _httpClient.PostAsJsonAsync(url, request.Body ?? new { }, cancellationToken);
                    break;
                case "PUT":
                    response = await _httpClient.PutAsJsonAsync(url, request.Body ?? new { }, cancellationToken);
                    break;
                case "PATCH":
                    var patchContent = JsonContent.Create(request.Body ?? new { });
                    response = await _httpClient.PatchAsync(url, patchContent, cancellationToken);
                    break;
                case "DELETE":
                    response = await _httpClient.DeleteAsync(url, cancellationToken);
                    break;
                default:
                    Error = GreyError.FromMessage($"Unsupported HTTP method: {request.Method}");
                    return null;
            }

            if (!response.IsSuccessStatusCode)
            {
                var errorContent = await response.Content.ReadAsStringAsync(cancellationToken);
                Error = new GreyError
                {
                    Message = $"Mutation failed: {response.ReasonPhrase}",
                    Status = (int)response.StatusCode,
                    Raw = errorContent,
                };
                return null;
            }

            var json = await response.Content.ReadAsStringAsync(cancellationToken);
            if (string.IsNullOrWhiteSpace(json))
            {
                Data = null;
                return null;
            }

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
    /// Execute a typed mutation.
    /// </summary>
    public async Task<T?> ExecuteMutationAsync<T>(MutationRequest request, CancellationToken cancellationToken = default) where T : class
    {
        ArgumentNullException.ThrowIfNull(request);

        Loading = true;
        Error = null;

        try
        {
            var url = $"{_options.ApiUrl}/{request.Endpoint.TrimStart('/')}";
            HttpResponseMessage response;

            switch (request.Method.ToUpperInvariant())
            {
                case "POST":
                    response = await _httpClient.PostAsJsonAsync(url, request.Body ?? new { }, cancellationToken);
                    break;
                case "PUT":
                    response = await _httpClient.PutAsJsonAsync(url, request.Body ?? new { }, cancellationToken);
                    break;
                case "PATCH":
                    var patchContent = JsonContent.Create(request.Body ?? new { });
                    response = await _httpClient.PatchAsync(url, patchContent, cancellationToken);
                    break;
                case "DELETE":
                    response = await _httpClient.DeleteAsync(url, cancellationToken);
                    break;
                default:
                    Error = GreyError.FromMessage($"Unsupported HTTP method: {request.Method}");
                    return null;
            }

            if (!response.IsSuccessStatusCode)
            {
                var errorContent = await response.Content.ReadAsStringAsync(cancellationToken);
                Error = new GreyError
                {
                    Message = $"Mutation failed: {response.ReasonPhrase}",
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
