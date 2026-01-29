using System.Text.Json.Serialization;

namespace Grey.MultiTenant.Core.Query;

/// <summary>
/// Request model for executing a query.
/// </summary>
public sealed record QueryRequest
{
    [JsonPropertyName("endpoint")]
    public required string Endpoint { get; init; }

    [JsonPropertyName("method")]
    public string Method { get; init; } = "GET";

    [JsonPropertyName("params")]
    public Dictionary<string, string>? Params { get; init; }

    [JsonPropertyName("headers")]
    public Dictionary<string, string>? Headers { get; init; }
}

/// <summary>
/// Result model for a query execution.
/// </summary>
public sealed record QueryResult<T>
{
    [JsonPropertyName("data")]
    public T? Data { get; init; }

    [JsonPropertyName("success")]
    public bool Success { get; init; }

    [JsonPropertyName("error")]
    public string? Error { get; init; }

    [JsonPropertyName("statusCode")]
    public int StatusCode { get; init; }
}

/// <summary>
/// Non-generic query result for dynamic scenarios.
/// </summary>
public sealed record QueryResult
{
    [JsonPropertyName("data")]
    public object? Data { get; init; }

    [JsonPropertyName("success")]
    public bool Success { get; init; }

    [JsonPropertyName("error")]
    public string? Error { get; init; }

    [JsonPropertyName("statusCode")]
    public int StatusCode { get; init; }
}

/// <summary>
/// Options for query execution.
/// </summary>
public sealed record QueryOptions
{
    public TimeSpan? Timeout { get; init; }
    public int RetryCount { get; init; } = 0;
    public TimeSpan? RetryDelay { get; init; }
    public bool ThrowOnError { get; init; } = true;
    public CancellationToken CancellationToken { get; init; }
}

/// <summary>
/// Represents the current query state.
/// </summary>
public sealed record QueryState<T>
{
    public T? Data { get; init; }
    public bool IsLoading { get; init; }
    public bool IsFetching { get; init; }
    public string? Error { get; init; }
    public bool IsSuccess { get; init; }
    public bool IsError { get; init; }
    public DateTimeOffset? LastFetchedAt { get; init; }
}
